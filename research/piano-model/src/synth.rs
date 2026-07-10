//! Modal synthesis: render a struck note as the sum of its extracted damped
//! sinusoids. This is the physical-model voice — no samples played back, just
//! the modal parameters driving oscillators.
//!
//! Deliberately kept simple and allocation-light in the inner loop so the same
//! math can later move into a `no_std` realtime engine: per-partial state is a
//! phasor increment and an exponential amplitude multiplier.

use crate::analyze::Partial;
use crate::model::{AttackCfg, JitterCfg, ModelConfig};
use crate::soundboard::Soundboard;

/// Simple LCG → uniform noise in [-1, 1] with unit variance scaling.
#[inline]
fn lcg(state: &mut u32) -> f32 {
    *state = state.wrapping_mul(1664525).wrapping_add(1013904223);
    let u = (*state >> 1) as f32 / (u32::MAX as f32 / 2.0);
    (u * 2.0 - 1.0) * 1.7320508 // uniform(-√3,√3) has unit variance
}

/// Detune factors for `n` unison strings spread symmetrically by `cents`.
fn detune_factors(n: usize, cents: f32) -> Vec<f32> {
    if n <= 1 {
        return vec![1.0];
    }
    (0..n)
        .map(|i| {
            // spread across [-cents/2, +cents/2]
            let frac = i as f32 / (n as f32 - 1.0) - 0.5;
            2f32.powf(frac * cents / 1200.0)
        })
        .collect()
}

/// Render one string's modal partials into `out` (additive). `jitter` and
/// `two_stage` gate those components per the model config.
fn render_string(
    out: &mut [f32],
    partials: &[Partial],
    sr: u32,
    detune: f32,
    seed: u32,
    jitter: Option<&JitterCfg>,
    two_stage: bool,
) {
    let n = out.len();
    let dt = 1.0 / sr as f32;
    let attack = (0.004 * sr as f32) as usize;
    // OU jitter coefficients (or disabled).
    let (revert, diffusion) = match jitter {
        Some(j) => {
            let r = (-dt / j.tau).exp();
            (r, j.sigma * (1.0 - r * r).sqrt())
        }
        None => (0.0, 0.0),
    };

    for p in partials {
        let freq = p.freq * detune;
        if freq <= 0.0 || freq >= sr as f32 / 2.0 {
            continue;
        }
        let w = std::f32::consts::TAU * freq / sr as f32;
        let phase0 = (p.k as f32 * 0.61803399 + seed as f32 * 1e-6).fract()
            * std::f32::consts::TAU;
        let mut rng = seed
            .wrapping_mul(2654435761)
            .wrapping_add(p.k.wrapping_mul(40503))
            .max(1);
        let mut drift = 0.0f32;
        let mut phase = phase0;
        for i in 0..n {
            let t = i as f32 * dt;
            let env = if two_stage {
                p.amp
                    * (p.mix * (-p.decay_fast * t).exp()
                        + (1.0 - p.mix) * (-p.decay_slow * t).exp())
            } else {
                p.amp * (-p.decay_slow * t).exp()
            };
            let a = if i < attack {
                let x = i as f32 / attack as f32;
                0.5 - 0.5 * (std::f32::consts::PI * x).cos()
            } else {
                1.0
            };
            out[i] += a * env * phase.sin();
            if jitter.is_some() && i & 15 == 0 {
                drift = revert * drift + diffusion * lcg(&mut rng);
            }
            phase += w * (1.0 + drift);
        }
    }
}

/// Render a full note under a model config. Each physical block (unison,
/// jitter, two-stage decay, attack, soundboard) is applied only if its config
/// is present — so the same engine voices any instrument in the family.
pub fn render(
    partials: &[Partial],
    sr: u32,
    dur_s: f32,
    vel01: f32,
    seed: u32,
    note: u8,
    cfg: &ModelConfig,
) -> Vec<f32> {
    let n = (dur_s * sr as f32) as usize;
    let mut strings = vec![0.0f32; n];

    // Unison strings (→ beating) if configured, else a single string.
    let (n_strings, detunes) = match &cfg.unison {
        Some(u) => {
            let ns = u.strings_for(note);
            (ns, detune_factors(ns, u.detune_cents))
        }
        None => (1, vec![1.0]),
    };
    let g = 1.0 / (n_strings as f32).sqrt();
    for (si, &d) in detunes.iter().enumerate() {
        let mut buf = vec![0.0f32; n];
        render_string(
            &mut buf,
            partials,
            sr,
            d,
            seed.wrapping_add(0x1000 * si as u32 + 1),
            cfg.jitter.as_ref(),
            cfg.two_stage_decay,
        );
        for (o, b) in strings.iter_mut().zip(buf.iter()) {
            *o += g * b;
        }
    }

    // Hammer strike as its own buffer (also drives the soundboard).
    let mut attack = vec![0.0f32; n];
    if let (Some(ac), Some(f0)) = (&cfg.attack, partials.first().map(|p| p.freq)) {
        add_attack_noise(&mut attack, sr, f0, vel01, seed ^ 0x9e3779b9, ac);
    }

    let mut out = strings.clone();
    for (o, a) in out.iter_mut().zip(attack.iter()) {
        *o += a;
    }

    // Soundboard body: parametric resonator bank driven by the hammer strike
    // plus a continuous string→board drive (bridge coupling), so a dense mode
    // bank radiates inter-partial body energy across the whole note.
    if let Some(sb) = &cfg.soundboard {
        if sb.mix > 0.0 {
            // Track the note's amplitude envelope (one-pole on |strings|) so the
            // body noise decays with the note rather than sounding like a static
            // hiss bed.
            let mut env = 0.0f32;
            let env_coef = (-1.0 / (0.02 * sr as f32)).exp(); // ~20ms smoothing
            let mut nrng = (seed ^ 0x5bd1e995).max(1);
            let excitation: Vec<f32> = attack
                .iter()
                .zip(strings.iter())
                .map(|(&a, &s)| {
                    env = env_coef * env + (1.0 - env_coef) * s.abs();
                    let noise = if sb.noise_drive > 0.0 {
                        sb.noise_drive * env * lcg(&mut nrng)
                    } else {
                        0.0
                    };
                    a + sb.drive * s + noise
                })
                .collect();
            let mut board = Soundboard::from_modes(&sb.modes, sr);
            board.add_body(&mut out, &excitation, sb.mix);
        }
    }
    out
}

/// A one-pole-two-zero bandpass biquad (RBJ), just enough for the noise burst.
struct Bandpass {
    b0: f32,
    b1: f32,
    b2: f32,
    a1: f32,
    a2: f32,
    x1: f32,
    x2: f32,
    y1: f32,
    y2: f32,
}
impl Bandpass {
    fn new(center: f32, q: f32, sr: u32) -> Self {
        let w0 = std::f32::consts::TAU * center / sr as f32;
        let (sn, cs) = w0.sin_cos();
        let alpha = sn / (2.0 * q);
        let a0 = 1.0 + alpha;
        Self {
            b0: alpha / a0,
            b1: 0.0,
            b2: -alpha / a0,
            a1: -2.0 * cs / a0,
            a2: (1.0 - alpha) / a0,
            x1: 0.0,
            x2: 0.0,
            y1: 0.0,
            y2: 0.0,
        }
    }
    fn process(&mut self, x: f32) -> f32 {
        let y = self.b0 * x + self.b1 * self.x1 + self.b2 * self.x2
            - self.a1 * self.y1
            - self.a2 * self.y2;
        self.x2 = self.x1;
        self.x1 = x;
        self.y2 = self.y1;
        self.y1 = y;
        y
    }
}

/// Render the stochastic residual (SMS "noise" part): white noise shaped by the
/// residual's per-band gains and enveloped by level*exp(-decay*t). This is the
/// broadband body — soundboard air, hammer noise, room — that pure sinusoids
/// can't make, with the spectral shape MEASURED from the real note.
pub fn add_residual(out: &mut [f32], sr: u32, res: &crate::analyze::Residual, gain: f32, seed: u32) {
    if res.level <= 0.0 || res.band_gain.is_empty() {
        return;
    }
    // one bandpass per band; Q so bandwidths tile (constant-Q-ish).
    let mut filters: Vec<(Bandpass, f32)> = Vec::with_capacity(res.band_hz.len());
    let q = 4.0; // moderate overlap
    for (i, &fc) in res.band_hz.iter().enumerate() {
        if fc > 0.0 && fc < sr as f32 * 0.5 {
            filters.push((Bandpass::new(fc, q, sr), res.band_gain[i]));
        }
    }
    if filters.is_empty() {
        return;
    }

    // Generate the shaped noise once, then self-calibrate its onset RMS to the
    // measured residual level — robust to the filterbank's absolute gain.
    let mut rng = seed.max(1);
    let mut shaped = vec![0.0f32; out.len()];
    for s in shaped.iter_mut() {
        let noise = lcg(&mut rng) / 1.7320508; // uniform[-1,1]
        let mut v = 0.0f32;
        for (bp, g) in &mut filters {
            v += *g * bp.process(noise);
        }
        *s = v;
    }
    // onset RMS over the first ~100 ms (after filters settle a few samples)
    let win = (0.1 * sr as f32) as usize;
    let start = 8usize.min(shaped.len());
    let end = (start + win).min(shaped.len());
    let raw_rms = if end > start {
        (shaped[start..end].iter().map(|x| x * x).sum::<f32>() / (end - start) as f32).sqrt()
    } else {
        0.0
    };
    if raw_rms <= 1e-9 {
        return;
    }
    let scale = gain * res.level / raw_rms;

    let dt = 1.0 / sr as f32;
    for (i, o) in out.iter_mut().enumerate() {
        let env = (-res.decay * i as f32 * dt).exp();
        *o += scale * env * shaped[i];
    }
}

fn add_attack_noise(out: &mut [f32], sr: u32, f0: f32, vel01: f32, seed: u32, ac: &AttackCfg) {
    let center = (f0 * ac.center_mult).clamp(ac.center_min, ac.center_max);
    let mut bpf = Bandpass::new(center, ac.q, sr);
    let amp0 = ac.amp * vel01 * vel01;
    let decay = (-1.0 / (ac.tau * sr as f32)).exp();
    let dur = (ac.dur * sr as f32) as usize;
    let fade = 16usize;
    let mut rng = seed.max(1);
    let mut amp = amp0;
    for i in 0..dur.min(out.len()) {
        let env = if i < fade {
            0.5 - 0.5 * (std::f32::consts::PI * i as f32 / fade as f32).cos()
        } else {
            1.0
        };
        let noise = lcg(&mut rng) / 1.7320508; // back to uniform[-1,1]
        out[i] += amp * env * bpf.process(noise);
        amp *= decay;
    }
}

/// Peak-normalize to a target linear peak.
pub fn normalize(buf: &mut [f32], target_peak: f32) {
    let peak = buf.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
    if peak > 0.0 {
        let g = target_peak / peak;
        for x in buf.iter_mut() {
            *x *= g;
        }
    }
}

/// Write mono f32 samples to a 24-bit WAV.
pub fn write_wav(path: &std::path::Path, buf: &[f32], sr: u32) -> anyhow::Result<()> {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: sr,
        bits_per_sample: 24,
        sample_format: hound::SampleFormat::Int,
    };
    let mut w = hound::WavWriter::create(path, spec)?;
    let scale = ((1i32 << 23) - 1) as f32;
    for &s in buf {
        let v = (s.clamp(-1.0, 1.0) * scale) as i32;
        w.write_sample(v)?;
    }
    w.finalize()?;
    Ok(())
}
