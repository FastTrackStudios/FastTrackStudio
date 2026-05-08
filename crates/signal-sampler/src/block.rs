//! Sampler-block runtime — Phase 1 of the Block → Module → Layer → Engine
//! hierarchy described in the project docs.
//!
//! A [`SamplerBlock`] is the unit-of-loading for a single `.signalpack`. It
//! wraps a [`SampleEngine`] (the existing voice-allocation + render core)
//! and adds block-level parameters (gain, pan, transpose) that apply to the
//! engine's output. Future block types (`OscillatorBlock`, `FilterBlock`, …)
//! will live alongside this module and share a common `Block` trait.
//!
//! Loading paths:
//! - **`SamplerBlock::from_spec`** — given a parsed [`BlockSpec`] (a
//!   `.signalblock` file), opens its referenced `.signalpack` and applies
//!   the spec's params.
//! - **`SamplerBlock::from_pack`** — bare-pack convenience: creates a
//!   default-params block from a `.signalpack` directly. Used when the TUI
//!   loads a pack without an accompanying `.signalblock`.

use std::path::{Path, PathBuf};

use facet::Facet;

use crate::engine::cache::SampleCache;
use crate::{PlayerPatch, SampleEngine, SamplerError};

// ── Persisted form ──────────────────────────────────────────────────────────

/// Block-level audio params applied at the SamplerBlock's output.
///
/// All defaults are no-ops: `gain_db=0`, `pan=0`, `transpose=0`,
/// `tune_cents=0`. Loading a `.signalpack` with no `.signalblock` file
/// produces a block with default params — same audio you'd get today.
#[derive(Debug, Clone, Default, Facet)]
pub struct BlockParams {
    /// Linear gain in decibels. Default 0 dB.
    #[facet(default)]
    pub gain_db: f32,
    /// Stereo pan. -1.0 = full left, 0 = centre, 1.0 = full right.
    #[facet(default)]
    pub pan: f32,
    /// Transpose incoming MIDI notes by this many semitones before
    /// dispatching to the engine. Useful for octave-shifted drum kits or
    /// pitched samplers.
    #[facet(default)]
    pub transpose: i8,
    /// Fine-tune in cents (informational for now — not yet applied to
    /// per-voice playback rate at the engine level).
    #[facet(default)]
    pub tune_cents: i16,
}

/// Parsed `.signalblock` file. References ONE `.signalpack` plus its
/// block-level parameters.
#[derive(Debug, Clone, Facet)]
pub struct BlockSpec {
    pub name: String,
    /// Discriminator for future block types. `"sampler"` is the only
    /// currently-supported value; others (`"oscillator"`, `"filter"`, …)
    /// will be added as Block trait grows.
    #[facet(default)]
    pub block_type: String,
    /// Path to the referenced `.signalpack`. Relative paths resolve from
    /// the directory containing the `.signalblock` file; absolute paths
    /// are used as-is.
    pub pack: String,
    #[facet(default)]
    pub params: BlockParams,
}

impl BlockSpec {
    pub fn from_file(path: &Path) -> Result<Self, SamplerError> {
        let text = std::fs::read_to_string(path)?;
        facet_styx::from_str(&text).map_err(|e| SamplerError::SpecParse(e.to_string()))
    }
}

// ── Runtime ────────────────────────────────────────────────────────────────

/// Runtime sampler block: one `.signalpack` + block-level params + the
/// engine that voices it.
pub struct SamplerBlock {
    pub name: String,
    engine: SampleEngine,
    params: BlockParams,
    /// Linear gain derived from `params.gain_db` — cached so the audio
    /// callback doesn't recompute it every block.
    gain_lin: f32,
    /// Equal-power pan multipliers for L/R — cached.
    pan_l: f32,
    pan_r: f32,
    /// Scratch buffer used when block params demand post-processing
    /// (gain != 1.0 or pan != 0.0). Pre-allocated; never resized inside
    /// the audio callback.
    scratch: Vec<f32>,
}

impl SamplerBlock {
    /// Create a SamplerBlock from a parsed `BlockSpec`. `spec_dir` is the
    /// directory of the `.signalblock` file, used to resolve a relative
    /// `pack` path.
    pub fn from_spec(
        spec: BlockSpec,
        spec_dir: &Path,
        sample_rate: u32,
    ) -> Result<Self, SamplerError> {
        let pack_buf = PathBuf::from(&spec.pack);
        let pack_path = if pack_buf.is_absolute() {
            pack_buf
        } else {
            spec_dir.join(pack_buf)
        };
        Self::build(spec.name, &pack_path, spec.params, sample_rate)
    }

    /// Construct a SamplerBlock from an already-built `SampleEngine`.
    /// Used by code paths that need to supply a non-default section/mic
    /// at load time (e.g. legacy `load_instrument`); for new code prefer
    /// `from_pack` or `from_spec`.
    pub fn from_engine(name: String, engine: SampleEngine, params: BlockParams) -> Self {
        let gain_lin = db_to_lin(params.gain_db);
        let (pan_l, pan_r) = pan_gains(params.pan);
        Self {
            name,
            engine,
            params,
            gain_lin,
            pan_l: pan_l * gain_lin,
            pan_r: pan_r * gain_lin,
            scratch: Vec::new(),
        }
    }

    /// Create a SamplerBlock from a bare `.signalpack` with default params.
    /// Convenience for callers that don't have a `.signalblock` file.
    pub fn from_pack(pack_path: &Path, sample_rate: u32) -> Result<Self, SamplerError> {
        let name = pack_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("block")
            .to_string();
        Self::build(name, pack_path, BlockParams::default(), sample_rate)
    }

    fn build(
        name: String,
        pack_path: &Path,
        params: BlockParams,
        sample_rate: u32,
    ) -> Result<Self, SamplerError> {
        let patch = PlayerPatch::from_pack(pack_path)?;
        let section = patch
            .spec
            .sections
            .first()
            .map(|s| s.id.clone())
            .unwrap_or_default();
        let mic = patch
            .spec
            .mics
            .first()
            .map(|m| m.id.clone())
            .unwrap_or_default();
        let engine = SampleEngine::new(patch, sample_rate, section, mic);
        let gain_lin = db_to_lin(params.gain_db);
        let (pan_l, pan_r) = pan_gains(params.pan);
        Ok(Self {
            name,
            engine,
            params,
            gain_lin,
            pan_l,
            pan_r,
            scratch: Vec::new(),
        })
    }

    // ── MIDI / playback ────────────────────────────────────────────────────

    pub fn note_on(&mut self, note: u8, velocity: u8) {
        let n = transposed(note, self.params.transpose);
        self.engine.note_on(n, velocity);
    }

    pub fn note_off(&mut self, note: u8) {
        let n = transposed(note, self.params.transpose);
        self.engine.note_off(n);
    }

    pub fn note_off_with_velocity(&mut self, note: u8, velocity: u8) {
        let n = transposed(note, self.params.transpose);
        self.engine.note_off_with_velocity(n, velocity);
    }

    pub fn cc(&mut self, controller: u8, value: u8) {
        self.engine.cc(controller, value);
    }

    /// Mix the block's audio into `output` (interleaved stereo).
    ///
    /// If gain/pan are at defaults the engine renders straight into
    /// `output` (zero-overhead path). Otherwise we render to a scratch
    /// buffer and mix in with gain + pan applied.
    pub fn render(&mut self, output: &mut [f32]) {
        if self.gain_lin == 1.0 && self.params.pan == 0.0 {
            self.engine.render(output);
            return;
        }

        // Render the engine's contribution to scratch (cleared first), then
        // accumulate with per-channel gain into `output`.
        if self.scratch.len() != output.len() {
            self.scratch.resize(output.len(), 0.0);
        }
        for s in self.scratch.iter_mut() {
            *s = 0.0;
        }
        self.engine.render(&mut self.scratch);
        for (out_pair, in_pair) in output.chunks_exact_mut(2).zip(self.scratch.chunks_exact(2)) {
            out_pair[0] += in_pair[0] * self.pan_l;
            out_pair[1] += in_pair[1] * self.pan_r;
        }
    }

    // ── Params ─────────────────────────────────────────────────────────────

    pub fn params(&self) -> &BlockParams {
        &self.params
    }

    pub fn set_gain_db(&mut self, db: f32) {
        self.params.gain_db = db;
        self.gain_lin = db_to_lin(db);
        let (l, r) = pan_gains(self.params.pan);
        self.pan_l = l * self.gain_lin;
        self.pan_r = r * self.gain_lin;
    }

    pub fn set_pan(&mut self, pan: f32) {
        self.params.pan = pan.clamp(-1.0, 1.0);
        let (l, r) = pan_gains(self.params.pan);
        self.pan_l = l * self.gain_lin;
        self.pan_r = r * self.gain_lin;
    }

    // ── Engine pass-throughs ───────────────────────────────────────────────

    pub fn cache_handle(&self) -> SampleCache {
        self.engine.cache_handle()
    }

    pub fn sample_paths_centered(&self, center: u8) -> Vec<PathBuf> {
        self.engine.sample_paths_centered(center)
    }

    pub fn loaded_sample_count(&self) -> usize {
        self.engine.loaded_sample_count()
    }

    pub fn total_sample_count(&self) -> usize {
        self.engine.total_sample_count()
    }

    pub fn patch(&self) -> &PlayerPatch {
        self.engine.patch()
    }

    pub fn active_voices(&self) -> usize {
        self.engine.active_voices()
    }

    /// Synchronously preload all referenced samples. Used by tests; the
    /// production path is the bank's background coordinator thread.
    pub fn preload_samples(&mut self) -> crate::engine::cache::PreloadStats {
        self.engine.preload_samples()
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

fn transposed(note: u8, semitones: i8) -> u8 {
    ((note as i16) + (semitones as i16)).clamp(0, 127) as u8
}

fn db_to_lin(db: f32) -> f32 {
    if db == 0.0 {
        1.0
    } else {
        10f32.powf(db / 20.0)
    }
}

/// Equal-power pan: at centre both L and R are 0.7071 (-3 dB).
fn pan_gains(pan: f32) -> (f32, f32) {
    let p = pan.clamp(-1.0, 1.0);
    let theta = (p + 1.0) * std::f32::consts::FRAC_PI_4;
    (theta.cos(), theta.sin())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_are_no_ops() {
        let p = BlockParams::default();
        assert_eq!(p.gain_db, 0.0);
        assert_eq!(p.pan, 0.0);
        assert_eq!(p.transpose, 0);
        assert_eq!(db_to_lin(0.0), 1.0);
    }

    #[test]
    fn pan_is_equal_power() {
        let (l, r) = pan_gains(0.0);
        assert!((l - r).abs() < 1e-6);
        assert!((l - 0.70710677).abs() < 1e-6);
    }

    #[test]
    fn transpose_clamps() {
        assert_eq!(transposed(60, 12), 72);
        assert_eq!(transposed(60, -12), 48);
        assert_eq!(transposed(120, 12), 127);
        assert_eq!(transposed(5, -10), 0);
    }
}
