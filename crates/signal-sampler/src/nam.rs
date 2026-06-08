//! Built-in Neural Amp Modeler backend for the FX chain.
//!
//! Wraps a `neural_amp_modeler::NamModel` for use as one variant of
//! [`crate::mixer::FxBackend`]. NAM models are **mono in / mono out** and
//! operate on `f64` buffers; the FX chain processes interleaved-stereo
//! `f32`. [`NamProcessor::process_interleaved`] handles the conversion:
//! input is collapsed to mono by summing L+R (with `input_gain_lin`
//! applied), the model is run, and the mono output is broadcast back to
//! both channels with `output_gain_lin`. Guitar amps are mono — stereo
//! width comes from cabinet IRs downstream.
//!
//! Scratch buffers (`in_mono`, `out_mono`) are pre-sized at
//! `reset(sample_rate, max_block)` time so the hot path doesn't allocate.

use neural_amp_modeler::NamModel;

/// Decibels → linear gain factor.
fn db_to_lin(db: f32) -> f32 {
    10f32.powf(db / 20.0)
}

/// A loaded NAM model with per-block scratch + user-facing input/output
/// trim. Lives inside an [`FxSlot`](crate::mixer::FxSlot) under the
/// [`FxBackend::Nam`](crate::mixer::FxBackend) variant.
pub struct NamProcessor {
    model: NamModel,
    /// Mono input scratch (L+R summed, gained). Reused per block.
    in_mono: Vec<f64>,
    /// Mono output scratch the model writes into. Reused per block.
    out_mono: Vec<f64>,
    /// Path the model was loaded from — for the UI label.
    pub model_path: String,
    /// Display name (filename stem) — cached so the UI doesn't touch the
    /// audio thread to label the slot.
    pub display_name: String,
    /// User trim before NAM. Default 0 dB.
    pub input_gain_db: f32,
    /// User trim after NAM. Default 0 dB.
    pub output_gain_db: f32,
    /// Sample rate the model was prepared at.
    pub sample_rate: f64,
}

impl NamProcessor {
    /// Load a `.nam` model from disk and prepare it for `sample_rate` /
    /// `max_block` frames. The display name is derived from the
    /// filename stem.
    pub fn load(
        path: impl AsRef<std::path::Path>,
        sample_rate: f64,
        max_block: usize,
    ) -> Result<Self, String> {
        let path = path.as_ref();
        let mut model = NamModel::load(path)?;
        model.reset(sample_rate, max_block);
        let display_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("NAM")
            .to_string();
        Ok(Self {
            model,
            in_mono: vec![0.0; max_block],
            out_mono: vec![0.0; max_block],
            model_path: path.to_string_lossy().to_string(),
            display_name,
            input_gain_db: 0.0,
            output_gain_db: 0.0,
            sample_rate,
        })
    }

    /// Re-prepare the model at a new sample rate / block size. Resets
    /// internal state so the next block starts clean (recommended after
    /// loading or when the audio config changes).
    pub fn reset(&mut self, sample_rate: f64, max_block: usize) {
        self.sample_rate = sample_rate;
        self.in_mono.resize(max_block, 0.0);
        self.out_mono.resize(max_block, 0.0);
        self.model.reset(sample_rate, max_block);
    }

    /// Process one interleaved-stereo `[L, R, L, R, …]` `f32` buffer
    /// in place: collapse to mono → run NAM → broadcast mono back to
    /// both channels. `input_gain` and `output_gain` apply pre/post the
    /// model so the user can match the NAM model's expected input level
    /// without recalibrating the entire chain.
    pub fn process_interleaved(&mut self, inout: &mut [f32]) {
        let frames = inout.len() / 2;
        if frames == 0 {
            return;
        }
        if frames > self.in_mono.len() {
            self.in_mono.resize(frames, 0.0);
            self.out_mono.resize(frames, 0.0);
        }
        let gin = db_to_lin(self.input_gain_db) as f64;
        let gout = db_to_lin(self.output_gain_db) as f64;
        // De-interleave + sum to mono (× input gain). Halve the sum so
        // a centered signal lands at unity instead of doubling.
        for i in 0..frames {
            let l = inout[2 * i] as f64;
            let r = inout[2 * i + 1] as f64;
            self.in_mono[i] = ((l + r) * 0.5) * gin;
        }
        self.model
            .process(&self.in_mono[..frames], &mut self.out_mono[..frames]);
        // Broadcast mono back to both channels with output gain.
        for i in 0..frames {
            let y = (self.out_mono[i] * gout) as f32;
            inout[2 * i] = y;
            inout[2 * i + 1] = y;
        }
    }
}

impl std::fmt::Debug for NamProcessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamProcessor")
            .field("display_name", &self.display_name)
            .field("model_path", &self.model_path)
            .field("sample_rate", &self.sample_rate)
            .field("input_gain_db", &self.input_gain_db)
            .field("output_gain_db", &self.output_gain_db)
            .finish()
    }
}
