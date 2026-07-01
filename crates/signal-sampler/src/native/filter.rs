//! Native **Filter** block — a stereo state-variable filter (Cytomic/Zavalishin
//! TPT SVF), the built-in DSP for `BlockType::Filter`.
//!
//! Covers the Nord filter menu's core shapes (LP/HP/BP; LP24 later by cascading
//! two sections). Defaults are transparent-ish (LP just under Nyquist) so a
//! placeholder-parameterized preset keeps passing audio.

use signal_plugin_host::{
    PluginDescriptor, PluginError, PluginEvents, PluginFormat, PluginInstance, PluginParamInfo,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FilterMode {
    Lowpass,
    Highpass,
    Bandpass,
}

/// One TPT state-variable filter section (mono).
#[derive(Clone, Copy, Debug, Default)]
pub struct Svf {
    // Coefficients.
    a1: f32,
    a2: f32,
    a3: f32,
    k: f32,
    // State.
    ic1: f32,
    ic2: f32,
}

impl Svf {
    /// Set cutoff/resonance. `q` ≥ ~0.5; 0.707 = flat.
    pub fn set(&mut self, cutoff_hz: f32, q: f32, sample_rate: f32) {
        let sr = sample_rate.max(1.0);
        let fc = cutoff_hz.clamp(10.0, sr * 0.45);
        let g = (core::f32::consts::PI * fc / sr).tan();
        let k = 1.0 / q.max(0.1);
        let a1 = 1.0 / (1.0 + g * (g + k));
        self.a1 = a1;
        self.a2 = g * a1;
        self.a3 = g * self.a2;
        self.k = k;
    }

    pub fn reset(&mut self) {
        self.ic1 = 0.0;
        self.ic2 = 0.0;
    }

    /// Process one sample, returning `(lowpass, bandpass, highpass)`.
    #[inline]
    pub fn tick(&mut self, v0: f32) -> (f32, f32, f32) {
        let v3 = v0 - self.ic2;
        let v1 = self.a1 * self.ic1 + self.a2 * v3;
        let v2 = self.ic2 + self.a2 * self.ic1 + self.a3 * v3;
        self.ic1 = 2.0 * v1 - self.ic1;
        self.ic2 = 2.0 * v2 - self.ic2;
        (v2, v1, v0 - self.k * v1 - v2)
    }
}

/// The `Filter` block: stereo SVF processor.
pub struct NativeFilter {
    sample_rate: f32,
    mode: FilterMode,
    cutoff_hz: f32,
    q: f32,
    left: Svf,
    right: Svf,
    prepared: bool,
}

impl NativeFilter {
    pub fn new(sample_rate: u32) -> Self {
        let mut f = Self {
            sample_rate: sample_rate.max(1) as f32,
            mode: FilterMode::Lowpass,
            cutoff_hz: 20_000.0,
            q: core::f32::consts::FRAC_1_SQRT_2,
            left: Svf::default(),
            right: Svf::default(),
            prepared: false,
        };
        f.update_coeffs();
        f
    }

    #[must_use]
    pub fn with_mode(mut self, mode: FilterMode) -> Self {
        self.mode = mode;
        self
    }

    #[must_use]
    pub fn with_cutoff(mut self, hz: f32) -> Self {
        self.cutoff_hz = hz;
        self.update_coeffs();
        self
    }

    #[must_use]
    pub fn with_q(mut self, q: f32) -> Self {
        self.q = q;
        self.update_coeffs();
        self
    }

    fn update_coeffs(&mut self) {
        self.left.set(self.cutoff_hz, self.q, self.sample_rate);
        self.right.set(self.cutoff_hz, self.q, self.sample_rate);
    }

    #[inline]
    fn pick(mode: FilterMode, lp: f32, bp: f32, hp: f32) -> f32 {
        match mode {
            FilterMode::Lowpass => lp,
            FilterMode::Highpass => hp,
            FilterMode::Bandpass => bp,
        }
    }
}

impl PluginInstance for NativeFilter {
    fn descriptor(&self) -> PluginDescriptor {
        PluginDescriptor {
            id: "signal.native.filter".into(),
            name: "Filter".into(),
            vendor: "Signal".into(),
            version: String::new(),
            format: PluginFormat::Synthetic,
        }
    }

    fn params(&mut self) -> Vec<PluginParamInfo> {
        Vec::new()
    }
    fn param_value(&mut self, _id: u32) -> Option<f64> {
        None
    }
    fn value_to_text(&mut self, _id: u32, _value: f64) -> Option<String> {
        None
    }
    fn text_to_value(&mut self, _id: u32, _text: &str) -> Option<f64> {
        None
    }
    fn latency(&mut self) -> u32 {
        0
    }

    fn prepare(&mut self, sample_rate: f64, _block_size: u32) -> Result<(), PluginError> {
        self.sample_rate = sample_rate.max(1.0) as f32;
        self.update_coeffs();
        self.left.reset();
        self.right.reset();
        self.prepared = true;
        Ok(())
    }

    fn is_prepared(&self) -> bool {
        self.prepared
    }

    fn process_block(
        &mut self,
        in_l: &[f32],
        in_r: &[f32],
        out_l: &mut [f32],
        out_r: &mut [f32],
        _events: &PluginEvents<'_>,
    ) -> Result<(), PluginError> {
        let frames = out_l.len().min(out_r.len()).min(in_l.len()).min(in_r.len());
        for f in 0..frames {
            let (lp, bp, hp) = self.left.tick(in_l[f]);
            out_l[f] = Self::pick(self.mode, lp, bp, hp);
            let (lp, bp, hp) = self.right.tick(in_r[f]);
            out_r[f] = Self::pick(self.mode, lp, bp, hp);
        }
        Ok(())
    }

    fn deactivate(&mut self) {
        self.prepared = false;
        self.left.reset();
        self.right.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rms(buf: &[f32]) -> f32 {
        (buf.iter().map(|s| s * s).sum::<f32>() / buf.len().max(1) as f32).sqrt()
    }

    /// Render a sine at `freq` through the filter, return output RMS.
    fn sine_response(filter: &mut NativeFilter, freq: f32, sr: f32) -> f32 {
        let n = 4_096;
        let input: Vec<f32> = (0..n)
            .map(|i| (core::f32::consts::TAU * freq * i as f32 / sr).sin())
            .collect();
        let (mut out_l, mut out_r) = (vec![0.0; n], vec![0.0; n]);
        let ev = PluginEvents {
            params: &[],
            midi: &[],
            note_expressions: &[],
        };
        filter
            .process_block(&input, &input, &mut out_l, &mut out_r, &ev)
            .unwrap();
        // Skip the transient at the head.
        rms(&out_l[n / 2..])
    }

    #[test]
    fn lowpass_passes_low_attenuates_high() {
        let sr = 48_000.0;
        let mut f = NativeFilter::new(48_000).with_cutoff(1_000.0);
        f.prepare(48_000.0, 4_096).unwrap();
        let low = sine_response(&mut f, 100.0, sr);
        f.prepare(48_000.0, 4_096).unwrap(); // reset state
        let high = sine_response(&mut f, 10_000.0, sr);
        assert!(low > 0.6, "passband ~unity, rms={low}");
        assert!(high < 0.1, "10 kHz through a 1 kHz LP is >20 dB down, rms={high}");
    }

    #[test]
    fn highpass_mirrors() {
        let sr = 48_000.0;
        let mut f = NativeFilter::new(48_000)
            .with_mode(FilterMode::Highpass)
            .with_cutoff(1_000.0);
        f.prepare(48_000.0, 4_096).unwrap();
        let low = sine_response(&mut f, 100.0, sr);
        f.prepare(48_000.0, 4_096).unwrap();
        let high = sine_response(&mut f, 10_000.0, sr);
        assert!(high > 0.6, "highs pass, rms={high}");
        assert!(low < 0.1, "lows cut, rms={low}");
    }

    #[test]
    fn default_filter_is_transparent_enough() {
        // A default (LP ~20 kHz) filter must not swallow a preset's audio.
        let sr = 48_000.0;
        let mut f = NativeFilter::new(48_000);
        f.prepare(48_000.0, 4_096).unwrap();
        let mid = sine_response(&mut f, 440.0, sr);
        assert!(mid > 0.6, "default filter passes midrange, rms={mid}");
    }
}
