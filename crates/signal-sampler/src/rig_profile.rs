//! GigPerformer-style profile/patch layer over [`GuitarRig`].
//!
//! A [`RigProfile`] (e.g. "Worship") is a list of [`RigPatch`]es (e.g. "Clean",
//! "Lead"), each a named tone realized as an ordered FX **chain** (drive → amp →
//! cab → …) plus patch-level input/output trims. [`ProfileRig`] loads a profile
//! by **pre-installing every patch's chain into the rig's resident bank**, so
//! switching patches mid-set is a single lock-free atomic — no reload, no
//! dropout.
//!
//! ## Level-matching across patches
//!
//! When [`ProfileRig::set_level_match`] is on, each patch's output trim is
//! auto-compensated from its amp model's NAM `loudness` metadata so "Clean" and
//! "Lead" land at a consistent perceived level. See [`ProfileRig::activate`].
//!
//! ## Relationship to the `signal-proto` Profile model
//!
//! Each NAM block is a [`signal_proto::block_kind::NamRef`] — the same reference
//! a proto `Block` with `BlockKind::Nam` carries. [`RigProfile::from_proto`]
//! converts a proto [`Profile`](signal_proto::profile::Profile) into a
//! `RigProfile` given a [`PatchResolver`] (which the `signal-live` resolve stack
//! implements); this layer stays repo-free so the standalone rig runs without
//! the storage stack.

use std::path::{Path, PathBuf};

use facet::Facet;

use crate::SamplerError;
use crate::rig::{GuitarRig, ModelId, RigBlock};

/// One patch in a rig profile: a named tone = an ordered FX chain + patch trims.
#[derive(Debug, Clone, Facet)]
pub struct RigPatch {
    /// Patch name shown on the switcher (e.g. "Clean", "Lead").
    pub name: String,
    /// Ordered FX chain (drive → amp → cab → …). Each block is a NAM model or a
    /// cabinet IR; see [`RigBlock`].
    pub chain: Vec<RigBlock>,
    /// Patch-level trim before the chain (dB).
    #[facet(default)]
    pub input_trim_db: f32,
    /// Patch-level trim after the chain (dB).
    #[facet(default)]
    pub output_trim_db: f32,
}

impl RigPatch {
    /// A patch that is a single NAM amp (no cab / extra blocks).
    pub fn amp(name: impl Into<String>, model_path: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            chain: vec![RigBlock::nam(model_path)],
            input_trim_db: 0.0,
            output_trim_db: 0.0,
        }
    }

    /// An empty-chain patch to build up with [`with_block`](Self::with_block).
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            chain: Vec::new(),
            input_trim_db: 0.0,
            output_trim_db: 0.0,
        }
    }

    #[must_use]
    pub fn with_block(mut self, block: RigBlock) -> Self {
        self.chain.push(block);
        self
    }

    #[must_use]
    pub fn with_trims(mut self, input_db: f32, output_db: f32) -> Self {
        self.input_trim_db = input_db;
        self.output_trim_db = output_db;
        self
    }
}

/// A named collection of patches — the standalone-rig analogue of a
/// `signal_proto::Profile`.
#[derive(Debug, Clone, Facet)]
pub struct RigProfile {
    pub name: String,
    pub patches: Vec<RigPatch>,
    /// Index of the patch to make active on load. Defaults to 0.
    #[facet(default)]
    pub default_patch: usize,
}

impl RigProfile {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), patches: Vec::new(), default_patch: 0 }
    }

    #[must_use]
    pub fn with_patch(mut self, patch: RigPatch) -> Self {
        self.patches.push(patch);
        self
    }

    /// Parse a profile from a `.styx` file (see `examples/worship.styx`).
    pub fn from_styx_file(path: &Path) -> Result<Self, SamplerError> {
        let text = std::fs::read_to_string(path)?;
        Self::from_styx_str(&text)
    }

    pub fn from_styx_str(text: &str) -> Result<Self, SamplerError> {
        facet_styx::from_str(text).map_err(|e| SamplerError::SpecParse(e.to_string()))
    }

    /// Convert a `signal_proto::Profile` into a `RigProfile`, using `resolver`
    /// to turn each patch into an ordered chain of realized blocks. NAM blocks
    /// map to [`RigBlock::nam`]; cabinet blocks with an IR path map to
    /// [`RigBlock::cab_ir`]. Non-NAM/non-cab blocks (hosted plugins, native DSP
    /// without a path) are skipped with a log — the standalone rig can't host
    /// them yet. A patch that resolves to an empty chain is kept (it will be
    /// "unavailable" at load time).
    pub fn from_proto<R: PatchResolver>(
        profile: &signal_proto::profile::Profile,
        resolver: &R,
    ) -> Result<Self, String> {
        use signal_proto::block_kind::BlockKind;

        let mut patches = Vec::with_capacity(profile.patches.len());
        let mut default_patch = 0;
        for (i, patch) in profile.patches.iter().enumerate() {
            if patch.id == profile.default_patch_id {
                default_patch = i;
            }
            let resolved = resolver.resolve_patch(patch)?;
            let mut chain = Vec::new();
            for rb in resolved {
                match rb.kind {
                    BlockKind::Nam(nam) => chain.push(RigBlock::nam(nam.model_path)),
                    BlockKind::HostedPlugin(href) => {
                        chain.push(RigBlock::plugin_with_state(href.path, href.state_b64))
                    }
                    BlockKind::Native => {
                        // A cabinet realized natively carries its IR path.
                        if let Some(ir) = rb.cab_ir_path {
                            chain.push(RigBlock::cab_ir(ir));
                        } else {
                            tracing::debug!(
                                patch = %patch.name,
                                "from_proto: skipping native block with no IR path"
                            );
                        }
                    }
                    other => {
                        tracing::warn!(
                            patch = %patch.name,
                            kind = other.tag(),
                            "from_proto: skipping block — not supported in the standalone rig yet"
                        );
                    }
                }
            }
            patches.push(RigPatch {
                name: patch.name.clone(),
                chain,
                input_trim_db: 0.0,
                output_trim_db: 0.0,
            });
        }
        Ok(Self { name: profile.name.clone(), patches, default_patch })
    }
}

/// Resolves a proto `Patch` into an ordered list of realized blocks. The
/// `signal-live` resolve stack (repos + `ResolveService`) implements this; the
/// standalone rig only needs this narrow surface, so it stays decoupled from
/// storage. Each [`ResolvedRigBlock`] is one block in chain order.
pub trait PatchResolver {
    fn resolve_patch(
        &self,
        patch: &signal_proto::profile::Patch,
    ) -> Result<Vec<ResolvedRigBlock>, String>;
}

/// One realized block from a [`PatchResolver`]: its `BlockKind` (Nam / Native /
/// HostedPlugin / …) plus, for a natively-realized cabinet, its IR path.
#[derive(Debug, Clone)]
pub struct ResolvedRigBlock {
    pub kind: signal_proto::block_kind::BlockKind,
    /// IR path for a native cabinet block, if any (extracted from the block's
    /// parameters by the resolver).
    pub cab_ir_path: Option<String>,
}

impl ResolvedRigBlock {
    pub fn nam(model_path: impl Into<String>) -> Self {
        Self {
            kind: signal_proto::block_kind::BlockKind::Nam(signal_proto::block_kind::NamRef {
                model_path: model_path.into(),
                model_id: None,
            }),
            cab_ir_path: None,
        }
    }

    pub fn cab_ir(ir_path: impl Into<String>) -> Self {
        Self {
            kind: signal_proto::block_kind::BlockKind::Native,
            cab_ir_path: Some(ir_path.into()),
        }
    }

    pub fn plugin(
        format: impl Into<String>,
        path: impl Into<String>,
        state_b64: Option<String>,
    ) -> Self {
        Self {
            kind: signal_proto::block_kind::BlockKind::HostedPlugin(
                signal_proto::block_kind::HostedPluginRef {
                    format: format.into(),
                    path: path.into(),
                    state_b64,
                },
            ),
            cab_ir_path: None,
        }
    }
}

/// Resolve a block path against the profile file's directory when relative.
fn resolve_path(path: &str, base_dir: Option<&Path>) -> PathBuf {
    let p = PathBuf::from(path);
    match base_dir {
        Some(dir) if p.is_relative() => dir.join(p),
        _ => p,
    }
}

/// A [`GuitarRig`] plus the active [`RigProfile`] — the live switcher.
pub struct ProfileRig {
    rig: GuitarRig,
    profile: Option<RigProfile>,
    /// `ModelId` for each patch, parallel to `profile.patches`.
    patch_ids: Vec<ModelId>,
    active: Option<usize>,
    /// Auto level-match patches from NAM loudness metadata.
    level_match: bool,
    /// Target loudness (dB) patches normalize toward when level-matching.
    target_loudness_db: f32,
}

impl ProfileRig {
    pub fn new(rig: GuitarRig) -> Self {
        Self {
            rig,
            profile: None,
            patch_ids: Vec::new(),
            active: None,
            level_match: false,
            target_loudness_db: -18.0,
        }
    }

    /// Open the default audio devices and wrap a fresh rig.
    pub fn open_default() -> eyre::Result<Self> {
        Ok(Self::new(GuitarRig::new()?))
    }

    /// Enable/disable loudness-based level matching across patches. Re-applies
    /// to the active patch immediately.
    pub fn set_level_match(&mut self, on: bool) {
        self.level_match = on;
        if let Some(i) = self.active {
            self.activate(i);
        }
    }

    pub fn is_level_matching(&self) -> bool {
        self.level_match
    }

    /// Target loudness (dB) patches normalize toward when level-matching.
    pub fn set_target_loudness_db(&mut self, db: f32) {
        self.target_loudness_db = db;
        if let Some(i) = self.active {
            self.activate(i);
        }
    }

    /// Load a profile: uninstall any previous chains, pre-install every patch's
    /// chain into the bank, then activate the default patch. `base_dir` resolves
    /// relative block paths (pass the profile file's directory).
    pub fn load_profile(
        &mut self,
        profile: RigProfile,
        base_dir: Option<&Path>,
    ) -> Result<(), String> {
        self.rig.clear();
        self.patch_ids.clear();
        self.active = None;

        let mut loaded = 0usize;
        let mut first_ok: Option<usize> = None;
        for (i, patch) in profile.patches.iter().enumerate() {
            // Resolve every block's path against the base dir.
            let blocks: Vec<RigBlock> = patch
                .chain
                .iter()
                .map(|b| {
                    let mut rb = b.clone();
                    rb.path = resolve_path(&b.path, base_dir).to_string_lossy().to_string();
                    rb
                })
                .collect();

            if blocks.is_empty() {
                self.patch_ids.push(MODEL_UNAVAILABLE);
                tracing::warn!(patch = %patch.name, "ProfileRig: patch has no blocks — skipping");
                continue;
            }
            match self.rig.install_chain(&blocks) {
                Ok(id) => {
                    self.patch_ids.push(id);
                    loaded += 1;
                    first_ok.get_or_insert(i);
                }
                Err(e) => {
                    self.patch_ids.push(MODEL_UNAVAILABLE);
                    tracing::warn!(
                        patch = %patch.name,
                        error = %e,
                        "ProfileRig: failed to build patch chain — skipping"
                    );
                }
            }
        }

        if loaded == 0 && !profile.patches.is_empty() {
            self.profile = Some(profile);
            return Err("no patch chains could be built".into());
        }

        let want = profile.default_patch;
        let start = if self.patch_ids.get(want).copied().unwrap_or(MODEL_UNAVAILABLE)
            != MODEL_UNAVAILABLE
        {
            want
        } else {
            first_ok.unwrap_or(0)
        };
        self.profile = Some(profile);
        self.activate(start);
        Ok(())
    }

    /// Convenience: load a profile from a `.styx` file.
    pub fn load_profile_file(&mut self, path: &Path) -> Result<(), String> {
        let profile = RigProfile::from_styx_file(path).map_err(|e| e.to_string())?;
        let base = path.parent();
        self.load_profile(profile, base)
    }

    /// Activate the patch at `index`. Returns false if out of range or its
    /// chain failed to build. Applies patch trims + (optional) level-match.
    pub fn activate(&mut self, index: usize) -> bool {
        let Some(profile) = &self.profile else { return false };
        let Some(patch) = profile.patches.get(index) else { return false };
        let Some(&id) = self.patch_ids.get(index) else { return false };
        if id == MODEL_UNAVAILABLE {
            return false;
        }

        let mut output_trim = patch.output_trim_db;
        if self.level_match {
            if let Some(loud) = self.rig.slot_info(id).and_then(|s| s.primary_loudness) {
                output_trim += self.target_loudness_db - loud as f32;
            }
        }
        self.rig.set_input_trim_db(patch.input_trim_db);
        self.rig.set_output_trim_db(output_trim);
        self.rig.set_active(Some(id));
        self.active = Some(index);
        true
    }

    /// Activate a patch by name (case-insensitive).
    pub fn activate_named(&mut self, name: &str) -> bool {
        let Some(idx) = self.profile.as_ref().and_then(|p| {
            p.patches.iter().position(|q| q.name.eq_ignore_ascii_case(name))
        }) else {
            return false;
        };
        self.activate(idx)
    }

    /// Step to the next loadable patch (wraps). Footswitch-style.
    pub fn next_patch(&mut self) -> bool {
        self.step(1)
    }

    pub fn prev_patch(&mut self) -> bool {
        self.step(-1)
    }

    fn step(&mut self, dir: i32) -> bool {
        let n = self.patch_ids.len();
        if n == 0 {
            return false;
        }
        let start = self.active.unwrap_or(0) as i32;
        for k in 1..=n as i32 {
            let idx = (start + dir * k).rem_euclid(n as i32) as usize;
            if self.patch_ids[idx] != MODEL_UNAVAILABLE && self.activate(idx) {
                return true;
            }
        }
        false
    }

    // ── Read-side accessors ──────────────────────────────────────────────

    pub fn profile_name(&self) -> Option<&str> {
        self.profile.as_ref().map(|p| p.name.as_str())
    }

    pub fn patches(&self) -> &[RigPatch] {
        self.profile.as_ref().map(|p| p.patches.as_slice()).unwrap_or(&[])
    }

    pub fn is_patch_available(&self, index: usize) -> bool {
        self.patch_ids
            .get(index)
            .copied()
            .map(|id| id != MODEL_UNAVAILABLE)
            .unwrap_or(false)
    }

    pub fn active_index(&self) -> Option<usize> {
        self.active
    }

    pub fn active_patch(&self) -> Option<&RigPatch> {
        let i = self.active?;
        self.profile.as_ref()?.patches.get(i)
    }

    pub fn rig(&self) -> &GuitarRig {
        &self.rig
    }

    pub fn rig_mut(&mut self) -> &mut GuitarRig {
        &mut self.rig
    }
}

/// Sentinel stored in `patch_ids` for a patch whose chain failed to build.
const MODEL_UNAVAILABLE: ModelId = ModelId::MAX;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_worship_profile_programmatically() {
        let profile = RigProfile::new("Worship")
            .with_patch(RigPatch::amp("Clean", "clean.nam"))
            .with_patch(
                RigPatch::new("Lead")
                    .with_block(RigBlock::nam("lead.nam"))
                    .with_block(RigBlock::cab_ir("v30.wav"))
                    .with_trims(3.0, -2.0),
            );
        assert_eq!(profile.patches.len(), 2);
        assert_eq!(profile.patches[0].chain.len(), 1);
        assert_eq!(profile.patches[1].chain.len(), 2);
        assert!(profile.patches[1].chain[1].is_cab_ir());
        assert_eq!(profile.patches[1].input_trim_db, 3.0);
    }

    #[test]
    fn parses_chain_profile_from_styx() {
        let text = r#"
            name Worship
            default_patch 0
            patches (
                {
                    name Clean
                    chain ( { kind nam, path amps/clean.nam } )
                }
                {
                    name Lead
                    chain (
                        { kind nam, path amps/drive.nam }
                        { kind nam, path amps/lead.nam }
                        { kind cab_ir, path cabs/v30.wav }
                    )
                    input_trim_db 3.0
                    output_trim_db -2.0
                }
            )
        "#;
        let profile = RigProfile::from_styx_str(text).expect("parse");
        assert_eq!(profile.patches.len(), 2);
        assert_eq!(profile.patches[1].chain.len(), 3);
        assert!(profile.patches[1].chain[2].is_cab_ir());
        assert_eq!(profile.patches[1].chain[0].path, "amps/drive.nam");
        assert_eq!(profile.patches[1].output_trim_db, -2.0);
    }

    #[test]
    fn parses_plugin_block_from_styx() {
        let text = r#"
            name Rig
            patches (
                {
                    name Lead
                    chain (
                        { kind nam, path amps/amp.nam }
                        { kind cab_ir, path cabs/v30.wav }
                        { kind plugin, path /usr/lib/clap/ValhallaDelay.clap }
                    )
                }
            )
        "#;
        let profile = RigProfile::from_styx_str(text).expect("parse");
        let chain = &profile.patches[0].chain;
        assert_eq!(chain.len(), 3);
        assert!(chain[2].is_plugin());
        assert_eq!(chain[2].path, "/usr/lib/clap/ValhallaDelay.clap");
        assert!(chain[2].state_b64.is_none());
    }

    #[test]
    fn from_proto_maps_hosted_plugin_blocks() {
        use signal_proto::profile::{Patch, PatchId, Profile, ProfileId};
        use signal_proto::rig::{RigId, RigSceneId};

        struct PluginResolver;
        impl PatchResolver for PluginResolver {
            fn resolve_patch(&self, _p: &Patch) -> Result<Vec<ResolvedRigBlock>, String> {
                Ok(vec![
                    ResolvedRigBlock::nam("amp.nam"),
                    ResolvedRigBlock::plugin("Clap", "/plugins/Reverb.clap", Some("c3RhdGU=".into())),
                ])
            }
        }

        let patch = Patch::from_rig_scene(PatchId::new(), "Lead", RigId::new(), RigSceneId::new());
        let profile = Profile::new(ProfileId::new(), "Rig", patch);
        let rig_profile = RigProfile::from_proto(&profile, &PluginResolver).expect("convert");
        let chain = &rig_profile.patches[0].chain;
        assert_eq!(chain.len(), 2);
        assert!(chain[0].is_nam());
        assert!(chain[1].is_plugin());
        assert_eq!(chain[1].path, "/plugins/Reverb.clap");
        assert_eq!(chain[1].state_b64.as_deref(), Some("c3RhdGU="));
    }

    #[test]
    fn shipped_worship_example_parses() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/worship.styx");
        let profile = RigProfile::from_styx_file(&path).expect("worship.styx should parse");
        assert_eq!(profile.name, "Worship");
        assert!(profile.patches.len() >= 2);
        assert_eq!(profile.patches[0].name, "Clean");
    }

    #[test]
    fn resolves_relative_paths_against_base_dir() {
        let base = Path::new("/profiles/worship");
        assert_eq!(
            resolve_path("amps/clean.nam", Some(base)),
            Path::new("/profiles/worship/amps/clean.nam")
        );
        assert_eq!(
            resolve_path("/models/clean.nam", Some(base)),
            Path::new("/models/clean.nam")
        );
    }

    // ── proto bridge ────────────────────────────────────────────────────

    /// A trivial in-memory resolver: maps each patch to a fixed chain by name.
    struct StubResolver;
    impl PatchResolver for StubResolver {
        fn resolve_patch(
            &self,
            patch: &signal_proto::profile::Patch,
        ) -> Result<Vec<ResolvedRigBlock>, String> {
            // Pretend every patch is amp + cab; name the model after the patch.
            Ok(vec![
                ResolvedRigBlock::nam(format!("{}.nam", patch.name.to_lowercase())),
                ResolvedRigBlock::cab_ir("v30.wav"),
            ])
        }
    }

    #[test]
    fn from_proto_maps_patches_to_chains() {
        use signal_proto::profile::{Patch, Profile};
        use signal_proto::rig::{RigId, RigSceneId};
        use signal_proto::profile::PatchId;

        let clean = Patch::from_rig_scene(PatchId::new(), "Clean", RigId::new(), RigSceneId::new());
        let lead_id = PatchId::new();
        let mut profile = Profile::new(signal_proto::profile::ProfileId::new(), "Worship", clean);
        profile.add_patch(Patch::from_rig_scene(
            lead_id.clone(),
            "Lead",
            RigId::new(),
            RigSceneId::new(),
        ));

        let rig_profile = RigProfile::from_proto(&profile, &StubResolver).expect("convert");
        assert_eq!(rig_profile.name, "Worship");
        assert_eq!(rig_profile.patches.len(), 2);
        // Default patch ("Clean") is index 0.
        assert_eq!(rig_profile.default_patch, 0);
        // Each patch resolved to amp(NAM) + cab(IR).
        let lead = &rig_profile.patches[1];
        assert_eq!(lead.name, "Lead");
        assert_eq!(lead.chain.len(), 2);
        assert!(lead.chain[0].is_nam());
        assert_eq!(lead.chain[0].path, "lead.nam");
        assert!(lead.chain[1].is_cab_ir());
    }
}
