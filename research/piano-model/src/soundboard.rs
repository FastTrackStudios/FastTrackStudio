//! Physically-modeled soundboard — a bank of resonant modes, NOT an impulse
//! response. Each mode is a tunable damped resonator (biquad); the board is
//! their parallel sum. Everything is parametric: mode frequency, Q (damping),
//! and gain are all controllable, so the board can be re-voiced or swapped
//! without re-sampling anything.
//!
//! Driven primarily by the hammer strike (the board's impulsive excitation)
//! plus a small tap of the string signal. Because the measured string partials
//! already carry the sampled board's tonal balance, the board here is mixed in
//! at modest gain to add body resonance and the inter-partial "wood" tail
//! rather than to re-impose the whole spectral envelope (which would
//! double-count). A future de-embedding step can separate bare-string source
//! from board filter for a full source/filter split.

/// A single resonant mode: a constant-Q bandpass resonator.
#[derive(Clone)]
pub struct Resonator {
    b0: f32,
    b2: f32,
    a1: f32,
    a2: f32,
    x1: f32,
    x2: f32,
    y1: f32,
    y2: f32,
    gain: f32,
}

impl Resonator {
    pub fn new(freq: f32, q: f32, gain: f32, sr: u32) -> Self {
        let w0 = std::f32::consts::TAU * freq / sr as f32;
        let (sn, cs) = w0.sin_cos();
        let alpha = sn / (2.0 * q);
        let a0 = 1.0 + alpha;
        Self {
            b0: alpha / a0,
            b2: -alpha / a0,
            a1: -2.0 * cs / a0,
            a2: (1.0 - alpha) / a0,
            x1: 0.0,
            x2: 0.0,
            y1: 0.0,
            y2: 0.0,
            gain,
        }
    }
    #[inline]
    fn process(&mut self, x: f32) -> f32 {
        let y = self.b0 * x + self.b2 * self.x2 - self.a1 * self.y1 - self.a2 * self.y2;
        self.x2 = self.x1;
        self.x1 = x;
        self.y2 = self.y1;
        self.y1 = y;
        self.gain * y
    }
}

/// A parametric soundboard: a parallel bank of resonant modes.
pub struct Soundboard {
    modes: Vec<Resonator>,
}

impl Soundboard {
    /// Build from explicit (freq, Q, gain) mode specs.
    pub fn from_modes(specs: &[(f32, f32, f32)], sr: u32) -> Self {
        Self {
            modes: specs
                .iter()
                .map(|&(f, q, g)| Resonator::new(f, q, g, sr))
                .collect(),
        }
    }

    /// Filter one sample through the whole bank (sum of modes).
    #[inline]
    pub fn process(&mut self, x: f32) -> f32 {
        let mut sum = 0.0;
        for m in &mut self.modes {
            sum += m.process(x);
        }
        sum
    }

    /// Process a buffer in place, mixing the board output into `out` at `mix`.
    /// `excitation` drives the board (hammer strike + string tap).
    pub fn add_body(&mut self, out: &mut [f32], excitation: &[f32], mix: f32) {
        for (o, &x) in out.iter_mut().zip(excitation.iter()) {
            *o += mix * self.process(x);
        }
    }
}
