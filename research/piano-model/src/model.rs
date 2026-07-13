//! Model voicing configuration.
//!
//! One physical *engine*, many instrument *models*. A model = a measured
//! parameter table (per-note modal partials) + this voicing config, which
//! turns each physical component on/off and sets its parameters. A grand needs
//! unison beating + soundboard; a Rhodes/Wurli reed does not; a toy piano has
//! almost no board. Every block is `Option<..>` — `None` means that component
//! is disabled for this model.

use serde::{Deserialize, Serialize};

/// Hammer/mallet strike transient (bandpass noise burst).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttackCfg {
    /// Peak amplitude scale (multiplied by velocity²).
    pub amp: f32,
    /// Exponential decay time constant (s).
    pub tau: f32,
    /// Burst duration (s).
    pub dur: f32,
    /// Bandpass center = clamp(f0 * center_mult, center_min, center_max).
    pub center_mult: f32,
    pub center_min: f32,
    pub center_max: f32,
    /// Bandpass Q.
    pub q: f32,
}

/// Per-mode Ornstein-Uhlenbeck frequency jitter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JitterCfg {
    /// RMS frequency deviation as a fraction of mode frequency (~0.0004 = 4c).
    pub sigma: f32,
    /// OU correlation time (s).
    pub tau: f32,
}

/// Unison strings (detuned copies → beating). Register breaks set how many
/// strings each note gets: 1 below `two_break`, 2 below `three_break`, else 3.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnisonCfg {
    pub detune_cents: f32,
    pub two_break: u8,
    pub three_break: u8,
}

impl UnisonCfg {
    pub fn strings_for(&self, midi: u8) -> usize {
        if midi < self.two_break {
            1
        } else if midi < self.three_break {
            2
        } else {
            3
        }
    }
}

/// Parametric soundboard resonator bank (freq, Q, gain per mode).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoundboardCfg {
    /// Overall board output mixed into the note.
    pub mix: f32,
    /// Continuous string→board drive (bridge coupling): the strings drive the
    /// board through the whole note, so a dense mode bank radiates inter-partial
    /// body energy during sustain, not just at the strike.
    pub drive: f32,
    /// Note-enveloped broadband excitation of the board (the board's stochastic
    /// radiation / mechanical body noise). Linear filtering of the harmonic
    /// string can't create inter-partial energy — this generative noise source,
    /// shaped by the note's own amplitude envelope, fills the body spectrum.
    pub noise_drive: f32,
    pub modes: Vec<(f32, f32, f32)>,
}

/// Full voicing for one instrument model.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelConfig {
    pub name: String,
    /// Use the two-stage (prompt/aftersound) decay envelope. When false, the
    /// partial collapses to a single (slow) decay.
    pub two_stage_decay: bool,
    pub attack: Option<AttackCfg>,
    pub jitter: Option<JitterCfg>,
    pub unison: Option<UnisonCfg>,
    pub soundboard: Option<SoundboardCfg>,
}

impl ModelConfig {
    /// The City Grand voicing — everything on. (Source data: the owned
    /// LA Custom C7 Grand sample set.)
    pub fn city_grand() -> Self {
        Self {
            name: "City Grand".into(),
            two_stage_decay: true,
            attack: Some(AttackCfg {
                amp: 0.05,
                tau: 0.004,
                dur: 0.015,
                center_mult: 5.0,
                center_min: 200.0,
                center_max: 2000.0,
                q: 0.7,
            }),
            jitter: Some(JitterCfg {
                sigma: 0.0004,
                tau: 0.020,
            }),
            unison: Some(UnisonCfg {
                detune_cents: 0.6,
                two_break: 28,
                three_break: 40,
            }),
            // Best hand-tuned state: a subtle board adds body under the attack.
            // Closing the (12–165×) inter-partial broadband gap needs the board
            // response FIT from data or a learned residual, not hand-tuning —
            // guessed noise (noise_drive) has the wrong spectral shape and hurts.
            soundboard: Some(SoundboardCfg {
                mix: 0.15,
                drive: 0.15,
                noise_drive: 0.0,
                modes: dense_soundboard_modes(),
            }),
        }
    }

    /// Load a config from a JSON file.
    pub fn load(path: &std::path::Path) -> anyhow::Result<Self> {
        let s = std::fs::read_to_string(path)?;
        Ok(serde_json::from_str(&s)?)
    }
}

/// A dense soundboard mode set: a real piano soundboard has a high modal
/// density (hundreds of overlapping modes above ~1 kHz) that radiates a
/// quasi-continuous broadband response. This models it as ~72 log-spaced
/// resonators from 50 Hz to 9 kHz with moderate Q and a ~1/f gain taper, so
/// that when driven by the strings it fills the inter-partial spectrum with
/// body energy. All values are parametric and can be tuned or fit to data.
pub fn dense_soundboard_modes() -> Vec<(f32, f32, f32)> {
    const N: usize = 72;
    let (f_lo, f_hi) = (50.0f32, 9000.0f32);
    let mut modes = Vec::with_capacity(N);
    for i in 0..N {
        let frac = i as f32 / (N as f32 - 1.0);
        let freq = f_lo * (f_hi / f_lo).powf(frac);
        // Slight deterministic detune off the perfect log grid so modes aren't
        // exactly harmonic with anything.
        let jitter = 1.0 + 0.03 * ((i as f32 * 12.9898).sin());
        let freq = freq * jitter;
        // Q rises with frequency; gain tapers ~1/sqrt(f) (board radiates less up top).
        let q = 12.0 + 28.0 * frac;
        let gain = (f_lo / freq).sqrt() * 0.5;
        modes.push((freq, q, gain));
    }
    modes
}
