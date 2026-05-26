//! The Ideal Full Session Template — the opinionated "golden session".
//!
//! This is the canonical full track/folder layout FastTrackStudio organizes
//! every session toward: the complete instrument-band folder tree, each band's
//! group-slot membership (the REAPER 128-slot partition), routing into buses,
//! and per-track defaults.
//!
//! It is **pure data**. Categories are expressed as string paths (top-level
//! first, e.g. `["Guitars", "Electric"]`) sourced from the canonical taxonomy
//! in [`music_catalog`] — there is no typed instrument enum here, so other
//! repos can describe layouts without coupling to a closed category set. The
//! scaffold / conform / audit *logic* that consumes this model lives in the
//! `dynamic-template` engine crate, not here.

use facet::Facet;
use music_catalog::groups::SLOT_BANDS;

/// The opinionated full-session template: the ideal track/folder layout a
/// session is organized toward.
#[derive(Clone, Debug, Facet)]
pub struct IdealFullSessionTemplate {
    /// Human name for this template, e.g. `"FTS Golden Session"`.
    pub name: String,
    /// Schema version, bumped when the layout's shape changes.
    pub version: u32,
    /// Top-level nodes (folders / tracks), in display order.
    pub root: Vec<TemplateNode>,
    /// Routing destinations the layout sends into (mix buses, stems, etc.).
    pub buses: Vec<TemplateBus>,
}

/// One node in the template tree: a folder (has `children`) or a leaf track.
#[derive(Clone, Debug, Facet)]
pub struct TemplateNode {
    /// Display name, e.g. `"Drums"`, `"Electric Gtr"`, `"Kick In"`.
    pub name: String,
    /// Canonical group path this node belongs to (top-level first), matching
    /// the music-catalog taxonomy / classification-engine names. Empty for
    /// purely structural folders. e.g. `["Guitars", "Electric"]`.
    pub group_path: Vec<String>,
    /// Folder children, in display order. Empty ⇒ leaf track.
    pub children: Vec<TemplateNode>,
    /// Per-track defaults (ignored for pure folders).
    pub defaults: TrackDefaults,
    /// Group-slot membership for this node (the canonical 128-slot partition).
    pub group_membership: Option<GroupMembership>,
    /// How this node routes its output.
    pub routing: NodeRouting,
    /// When true, `children` are an **exemplar** of a per-project repeated
    /// pattern (e.g. synth layers, BGV stacks) rather than a fixed list — the
    /// engine instantiates as many as the project needs, naming them per take.
    pub variadic: bool,
}

impl TemplateNode {
    /// A folder node carrying a canonical group path. No defaults applied.
    pub fn folder(name: impl Into<String>, group_path: Vec<String>) -> Self {
        Self {
            name: name.into(),
            group_path,
            children: Vec::new(),
            defaults: TrackDefaults::default(),
            group_membership: None,
            routing: NodeRouting::default(),
            variadic: false,
        }
    }

    /// A leaf track with no canonical group path (inherits color/membership
    /// from its parent folder).
    pub fn track(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            group_path: Vec::new(),
            children: Vec::new(),
            defaults: TrackDefaults::default(),
            group_membership: None,
            routing: NodeRouting::default(),
            variadic: false,
        }
    }

    /// Add a child node (builder-style).
    pub fn with_child(mut self, child: TemplateNode) -> Self {
        self.children.push(child);
        self
    }

    /// Append several child nodes (builder-style).
    pub fn with_children(mut self, children: impl IntoIterator<Item = TemplateNode>) -> Self {
        self.children.extend(children);
        self
    }

    /// Set the canonical group path from string slices (builder-style).
    pub fn with_path(mut self, path: &[&str]) -> Self {
        self.group_path = path.iter().map(|s| (*s).to_string()).collect();
        self
    }

    /// Mark this folder's children as a per-project repeated pattern (the
    /// listed children are exemplars). Builder-style. See [`TemplateNode::variadic`].
    pub fn variadic(mut self) -> Self {
        self.variadic = true;
        self
    }

    /// Assign this node to a canonical group-slot category (builder-style).
    pub fn in_group(mut self, category: impl Into<String>) -> Self {
        self.group_membership = Some(GroupMembership {
            category: category.into(),
        });
        self
    }
}

/// Per-track default settings the template seeds a track with.
#[derive(Clone, Debug, Facet)]
pub struct TrackDefaults {
    /// Track color as `#RRGGBB`, or `None` to inherit from the group path.
    pub color_hex: Option<String>,
    /// Input monitoring mode.
    pub monitor: MonitorMode,
    /// Hardware/track input, or `None` to leave unassigned.
    pub input: Option<TrackInput>,
    /// Whether the track is record-armed by default.
    pub record_armed: bool,
    /// Default fader volume in dB (0.0 = unity).
    pub volume_db: f64,
    /// Default pan, -1.0 (hard left) … 1.0 (hard right).
    pub pan: f64,
}

impl Default for TrackDefaults {
    fn default() -> Self {
        Self {
            color_hex: None,
            monitor: MonitorMode::Auto,
            input: None,
            record_armed: false,
            volume_db: 0.0,
            pan: 0.0,
        }
    }
}

/// Input-monitoring mode for a track.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum MonitorMode {
    /// Never monitor input.
    Off,
    /// Always monitor input.
    On,
    /// Monitor only while record-armed (REAPER "auto").
    Auto,
}

/// A track's input source.
#[derive(Clone, Debug, PartialEq, Eq, Facet)]
pub struct TrackInput {
    /// 1-based hardware channel.
    pub channel: u32,
    /// Stereo input (channel + channel+1) when true, else mono.
    pub stereo: bool,
}

/// Canonical group-slot membership: which slot band this node joins.
#[derive(Clone, Debug, PartialEq, Eq, Facet)]
pub struct GroupMembership {
    /// Canonical category whose slot band this node joins, resolved against
    /// `music_catalog::groups`. e.g. `"Guitars/Electric"`, `"Drums"`.
    pub category: String,
}

/// How a node routes its output.
#[derive(Clone, Debug, Facet)]
pub struct NodeRouting {
    /// Send post-fader to the parent folder (the usual case).
    pub parent_send: bool,
    /// Name of a [`TemplateBus`] this node additionally sends to, if any.
    pub bus: Option<String>,
}

impl Default for NodeRouting {
    fn default() -> Self {
        Self {
            parent_send: true,
            bus: None,
        }
    }
}

/// A routing destination (mix bus / stem) the layout sends into.
#[derive(Clone, Debug, Facet)]
pub struct TemplateBus {
    /// Bus name, e.g. `"Drum Bus"`, `"Stems"`.
    pub name: String,
    /// Channel count (2 = stereo).
    pub channels: u32,
}

impl IdealFullSessionTemplate {
    /// The canonical FTS golden session. There's one top-level folder per
    /// instrument-category band in [`music_catalog::groups::SLOT_BANDS`], in
    /// partition order. Bands authored in detail (see [`layout`]) expand into
    /// their full subtree; the rest are top-level folder stubs carrying the
    /// canonical path / color / group-slot membership until authored.
    pub fn golden() -> Self {
        let root = SLOT_BANDS
            .iter()
            .map(|band| layout::authored(band.label).unwrap_or_else(|| layout::stub(band)))
            .collect();

        Self {
            name: "FTS Golden Session".to_string(),
            version: 1,
            root,
            buses: vec![TemplateBus {
                name: "Mix Bus".to_string(),
                channels: 2,
            }],
        }
    }
}

/// The authored golden-session track layout — the actual ideal tree, built up
/// group by group. Each top-level instrument band gets a `pub fn` returning
/// its full subtree; [`golden`](IdealFullSessionTemplate::golden) assembles
/// them in partition order, stubbing any band not yet authored.
pub mod layout {
    use super::*;

    /// A leaf track (inherits color / membership from its parent folder).
    fn track(name: &str) -> TemplateNode {
        TemplateNode::track(name)
    }

    /// A folder carrying a canonical group `path` and `children`.
    fn folder(name: &str, path: &[&str], children: Vec<TemplateNode>) -> TemplateNode {
        TemplateNode::folder(name, path.iter().map(|s| (*s).to_string()).collect())
            .with_children(children)
    }

    /// A variadic *structural* folder — a per-project organizational level
    /// (Performer / Arrangement / Layer / Channel) whose name is a placeholder
    /// filled per instance and which collapses away when it has a single
    /// child. Carries no canonical path of its own (inherits its color from
    /// the nearest instrument ancestor).
    fn level(name: &str, children: Vec<TemplateNode>) -> TemplateNode {
        TemplateNode::folder(name, Vec::new())
            .with_children(children)
            .variadic()
    }

    /// Top-level folder stub for a band that isn't authored in detail yet —
    /// carries the canonical path, resolved color, and group-slot membership
    /// straight from the slot partition.
    pub fn stub(band: &music_catalog::groups::GroupSlotBand) -> TemplateNode {
        let mut node = TemplateNode::folder(
            band.label,
            band.path.iter().map(|p| (*p).to_string()).collect(),
        );
        node.defaults.color_hex = band.color().map(|c| c.to_hex_string());
        node.group_membership = Some(GroupMembership {
            category: band.path.join("/"),
        });
        node
    }

    /// The detailed subtree for a top-level band by its REAPER label, or
    /// `None` if not yet authored.
    pub fn authored(label: &str) -> Option<TemplateNode> {
        match label {
            "Drums" => Some(drums()),
            "Bass" => Some(bass()),
            "Electric Gtr" => Some(electric_gtr()),
            "Acoustic Gtr" => Some(acoustic_gtr()),
            _ => None,
        }
    }

    /// Bass — the maximal form: a Guitar source (DI) and a Synth source
    /// (variadic per-project layers). When only one source is present the
    /// engine collapses this down (e.g. just `Bass / DI`).
    pub fn bass() -> TemplateNode {
        folder(
            "Bass",
            &["Bass"],
            vec![
                folder("Guitar", &["Bass", "Guitar"], vec![track("DI")]),
                // Synth layers are named per take, so this folder is variadic;
                // the single child is an exemplar layer.
                folder("Synth", &["Bass", "Synth"], vec![track("Layer")]).variadic(),
            ],
        )
        .in_group("Bass")
    }

    /// Drums — Kick / Snare / Toms / Cymbals / Rooms.
    pub fn drums() -> TemplateNode {
        folder(
            "Drums",
            &["Drums"],
            vec![
                folder(
                    "Kick",
                    &["Drums", "Kick"],
                    vec![
                        folder(
                            "Sum",
                            &["Drums", "Kick"],
                            vec![track("In"), track("Out"), track("Trig")],
                        ),
                        track("Sub"),
                    ],
                ),
                folder(
                    "Snare",
                    &["Drums", "Snare"],
                    vec![
                        folder(
                            "SUM",
                            &["Drums", "Snare"],
                            vec![track("Top"), track("Bottom"), track("Trig")],
                        ),
                        track("Verb"),
                    ],
                ),
                folder(
                    "Toms",
                    &["Drums", "Tom"],
                    vec![track("T1"), track("T2"), track("T3"), track("T4")],
                ),
                folder(
                    "Cymbals",
                    &["Drums", "Cymbals"],
                    vec![
                        track("Hi-Hat").with_path(&["Drums", "Hi-Hat"]),
                        track("Overheads"),
                    ],
                ),
                folder(
                    "Rooms",
                    &["Drums", "Room"],
                    vec![
                        track("Rooms Far"),
                        track("Rooms Close"),
                        track("Rooms Mono"),
                    ],
                ),
            ],
        )
        .in_group("Drums")
    }

    /// Shared guitar chain — the most dynamic shape. A run of variadic
    /// structural levels (Performer → Arrangement → Layer → Channel) bottoming
    /// out in the multi-mic captures. Every level collapses when singular, so
    /// the same template renders a one-off DI as `<name> / DI` and a fully
    /// tracked, multi-performer part as the full nested tree. Electric and
    /// acoustic guitars share this exactly; only the display name, canonical
    /// sub-path, and slot-band category differ.
    fn guitar(name: &str, sub: &str, category: &str) -> TemplateNode {
        folder(
            name,
            &["Guitars", sub],
            vec![level(
                "Performer",
                vec![level(
                    "Arrangement",
                    vec![level(
                        "Layer",
                        vec![level(
                            "Channel",
                            vec![
                                track("DI"),
                                track("DI-Pedalboard"),
                                track("Amp 1"),
                                track("Amp 2"),
                            ],
                        )],
                    )],
                )],
            )],
        )
        .in_group(category)
    }

    /// Electric guitar. See [`guitar`].
    pub fn electric_gtr() -> TemplateNode {
        guitar("GTR E", "Electric", "Electric Gtr")
    }

    /// Acoustic guitar — same dynamic chain as electric. See [`guitar`].
    pub fn acoustic_gtr() -> TemplateNode {
        guitar("GTR A", "Acoustic", "Acoustic Gtr")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn golden_has_one_folder_per_slot_band() {
        let t = IdealFullSessionTemplate::golden();
        assert_eq!(t.root.len(), SLOT_BANDS.len());
        // Every top-level node carries a canonical group path + membership.
        for node in &t.root {
            assert!(
                !node.group_path.is_empty(),
                "{} has no group path",
                node.name
            );
            assert!(
                node.group_membership.is_some(),
                "{} has no group membership",
                node.name
            );
        }
        // First band is Drums, last is Background Vox — order preserved.
        assert_eq!(t.root.first().unwrap().name, "Drums");
        assert_eq!(t.root.last().unwrap().name, "Background Vox");
    }

    #[test]
    fn golden_resolves_canonical_colors() {
        let t = IdealFullSessionTemplate::golden();
        // Keys (a stub band) resolves a color via its canonical path.
        let keys = t.root.iter().find(|n| n.name == "Keys").unwrap();
        assert!(keys.defaults.color_hex.is_some());
    }

    #[test]
    fn drums_layout_has_authored_shape() {
        let drums = layout::drums();
        assert_eq!(drums.name, "Drums");
        // Top-level drums joins the Drums group-slot band.
        assert_eq!(
            drums.group_membership.as_ref().map(|g| g.category.as_str()),
            Some("Drums")
        );

        let names: Vec<&str> = drums.children.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, ["Kick", "Snare", "Toms", "Cymbals", "Rooms"]);

        // Kick: a Sum sub-folder (In/Out/Trig) plus a Sub track.
        let kick = drums.children.iter().find(|c| c.name == "Kick").unwrap();
        let sum = kick.children.iter().find(|c| c.name == "Sum").unwrap();
        let sum_kids: Vec<&str> = sum.children.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(sum_kids, ["In", "Out", "Trig"]);
        assert!(kick.children.iter().any(|c| c.name == "Sub"));

        // Toms are four leaf tracks.
        let toms = drums.children.iter().find(|c| c.name == "Toms").unwrap();
        assert_eq!(toms.children.len(), 4);
        assert!(toms.children.iter().all(|c| c.children.is_empty()));

        // Hi-Hat carries its own canonical path for color.
        let cymbals = drums.children.iter().find(|c| c.name == "Cymbals").unwrap();
        let hat = cymbals
            .children
            .iter()
            .find(|c| c.name == "Hi-Hat")
            .unwrap();
        assert_eq!(hat.group_path, vec!["Drums", "Hi-Hat"]);
    }

    #[test]
    fn bass_layout_splits_guitar_and_synth() {
        let bass = layout::bass();
        assert_eq!(bass.name, "Bass");
        assert_eq!(
            bass.group_membership.as_ref().map(|g| g.category.as_str()),
            Some("Bass")
        );

        let guitar = bass.children.iter().find(|c| c.name == "Guitar").unwrap();
        assert_eq!(guitar.group_path, vec!["Bass", "Guitar"]);
        assert!(guitar.children.iter().any(|c| c.name == "DI"));
        assert!(!guitar.variadic);

        // Synth is variadic — its children are per-project exemplar layers.
        let synth = bass.children.iter().find(|c| c.name == "Synth").unwrap();
        assert_eq!(synth.group_path, vec!["Bass", "Synth"]);
        assert!(synth.variadic);
    }

    #[test]
    fn electric_gtr_nests_variadic_levels_down_to_mics() {
        let gtr = layout::electric_gtr();
        assert_eq!(gtr.name, "GTR E");
        assert_eq!(gtr.group_path, vec!["Guitars", "Electric"]);
        assert!(!gtr.variadic, "the GTR E anchor itself is not variadic");

        // Walk the structural chain: Performer > Arrangement > Layer > Channel,
        // each a variadic level, then the multi-mic leaf captures.
        let chain = ["Performer", "Arrangement", "Layer", "Channel"];
        let mut node = &gtr;
        for level_name in chain {
            node = &node.children[0];
            assert_eq!(node.name, level_name);
            assert!(node.variadic, "{level_name} should be variadic");
            assert!(
                node.group_path.is_empty(),
                "{level_name} inherits its color"
            );
        }

        // Channel holds the multi-mic captures as leaf tracks.
        let mics: Vec<&str> = node.children.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(mics, ["DI", "DI-Pedalboard", "Amp 1", "Amp 2"]);
        assert!(node.children.iter().all(|c| c.children.is_empty()));
    }

    #[test]
    fn acoustic_gtr_mirrors_electric_chain() {
        let acoustic = layout::acoustic_gtr();
        assert_eq!(acoustic.name, "GTR A");
        assert_eq!(acoustic.group_path, vec!["Guitars", "Acoustic"]);
        assert_eq!(
            acoustic
                .group_membership
                .as_ref()
                .map(|g| g.category.as_str()),
            Some("Acoustic Gtr")
        );
        // Same structural depth/shape as electric (Performer→…→Channel→mics).
        let electric = layout::electric_gtr();
        fn shape(n: &TemplateNode) -> Vec<(String, bool)> {
            let mut out = vec![(n.name.clone(), n.variadic)];
            for c in &n.children {
                out.extend(shape(c));
            }
            out
        }
        // Identical but for the root name; compare from the first child down.
        assert_eq!(shape(&acoustic.children[0]), shape(&electric.children[0]));
    }
}
