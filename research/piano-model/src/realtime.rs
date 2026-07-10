//! Realtime polyphonic voice engine — the playable form of the City Grand
//! model. Same physics as the offline `synth`, but per-sample and incremental:
//! quadrature oscillators (no `sin()` per sample) and exponential-multiplier
//! envelopes (no `exp(t)` per sample). This is the shape the DSP takes when it
//! later moves into a `no_std` voice in `features/fx` / signal-engine.

use crate::model::ModelConfig;
use crate::table::Table;

const TAU: f32 = std::f32::consts::TAU;

/// One oscillator = one partial of one string. Quadrature rotation + two-stage
/// exponential envelope + subsampled OU frequency jitter.
struct Osc {
    s: f32,
    c: f32,
    cos_inc: f32,
    sin_inc: f32,
    phase_inc: f32,
    amp: f32,
    mix: f32,
    fast_env: f32,
    slow_env: f32,
    fast_mult: f32,
    slow_mult: f32,
    drift: f32,
    rng: u32,
}

struct Voice {
    note: u8,
    active: bool,
    age: u64,
    oscs: Vec<Osc>,
    gain: f32,
    // hammer attack noise burst
    atk_remaining: u32,
    atk_amp: f32,
    atk_decay: f32,
    atk_bpf: Biquad,
    atk_rng: u32,
    // note-off damper
    rel_env: f32,
    rel_mult: f32,
    releasing: bool,
    // renorm counter
    sample: u64,
}

impl Voice {
    fn silent() -> Self {
        Voice {
            note: 0,
            active: false,
            age: 0,
            oscs: Vec::with_capacity(48 * 3),
            gain: 0.0,
            atk_remaining: 0,
            atk_amp: 0.0,
            atk_decay: 1.0,
            atk_bpf: Biquad::default(),
            atk_rng: 1,
            rel_env: 1.0,
            rel_mult: 1.0,
            releasing: false,
            sample: 0,
        }
    }
}

#[inline]
fn lcg(state: &mut u32) -> f32 {
    *state = state.wrapping_mul(1664525).wrapping_add(1013904223);
    let u = (*state >> 1) as f32 / (u32::MAX as f32 / 2.0);
    (u * 2.0 - 1.0) * 1.7320508
}

#[derive(Default)]
struct Biquad {
    b0: f32,
    b2: f32,
    a1: f32,
    a2: f32,
    x1: f32,
    x2: f32,
    y1: f32,
    y2: f32,
}
impl Biquad {
    fn bandpass(center: f32, q: f32, sr: f32) -> Self {
        let w0 = TAU * center / sr;
        let (sn, cs) = w0.sin_cos();
        let alpha = sn / (2.0 * q);
        let a0 = 1.0 + alpha;
        Self {
            b0: alpha / a0,
            b2: -alpha / a0,
            a1: -2.0 * cs / a0,
            a2: (1.0 - alpha) / a0,
            ..Default::default()
        }
    }
    #[inline]
    fn process(&mut self, x: f32) -> f32 {
        let y = self.b0 * x + self.b2 * self.x2 - self.a1 * self.y1 - self.a2 * self.y2;
        self.x2 = self.x1;
        self.x1 = x;
        self.y2 = self.y1;
        self.y1 = y;
        y
    }
}

pub struct Engine {
    sr: f32,
    table: Table,
    cfg: ModelConfig,
    voices: Vec<Voice>,
    age_counter: u64,
    master_gain: f32,
    // OU jitter coefficients
    jitter_revert: f32,
    jitter_diffusion: f32,
}

impl Engine {
    pub fn new(table: Table, cfg: ModelConfig, sr: f32, polyphony: usize) -> Self {
        let (revert, diffusion) = match &cfg.jitter {
            Some(j) => {
                let r = (-1.0 / (j.tau * sr)).exp();
                (r, j.sigma * (1.0 - r * r).sqrt())
            }
            None => (0.0, 0.0),
        };
        Self {
            sr,
            table,
            cfg,
            voices: (0..polyphony).map(|_| Voice::silent()).collect(),
            age_counter: 0,
            master_gain: 4.0,
            jitter_revert: revert,
            jitter_diffusion: diffusion,
        }
    }

    pub fn note_on(&mut self, note: u8, vel: u8) {
        let vel = vel.max(1);
        let (voicing, scale) = match self.table.lookup(note, vel) {
            Some(x) => x,
            None => return,
        };
        // unison strings + detune from config
        let (n_strings, detunes): (usize, Vec<f32>) = match &self.cfg.unison {
            Some(u) => {
                let ns = u.strings_for(note);
                (ns, detune_factors(ns, u.detune_cents))
            }
            None => (1, vec![1.0]),
        };
        let two_stage = self.cfg.two_stage_decay;
        let g_string = 1.0 / (n_strings as f32).sqrt();

        // pick voice: free one, else oldest
        self.age_counter += 1;
        let idx = self
            .voices
            .iter()
            .position(|v| !v.active)
            .unwrap_or_else(|| {
                self.voices
                    .iter()
                    .enumerate()
                    .min_by_key(|(_, v)| v.age)
                    .map(|(i, _)| i)
                    .unwrap()
            });

        let sr = self.sr;
        let base_seed = ((note as u32) << 8) ^ (vel as u32).wrapping_mul(2654435761);
        let v = &mut self.voices[idx];
        v.oscs.clear();
        for (si, &d) in detunes.iter().enumerate() {
            for p in &voicing.modal {
                let freq = p.freq * d * scale;
                if freq <= 0.0 || freq >= sr * 0.5 {
                    continue;
                }
                let phase_inc = TAU * freq / sr;
                let (sin_inc, cos_inc) = phase_inc.sin_cos();
                v.oscs.push(Osc {
                    s: 0.0,
                    c: 1.0,
                    cos_inc,
                    sin_inc,
                    phase_inc,
                    amp: p.amp * g_string,
                    mix: if two_stage { p.mix } else { 0.0 },
                    fast_env: 1.0,
                    slow_env: 1.0,
                    fast_mult: (-p.decay_fast / sr).exp(),
                    slow_mult: (-p.decay_slow / sr).exp(),
                    drift: 0.0,
                    rng: base_seed
                        .wrapping_add((si as u32) << 16)
                        .wrapping_add(p.k.wrapping_mul(40503))
                        .max(1),
                });
            }
        }

        // hammer attack burst
        let vel01 = (vel as f32 / 127.0).clamp(0.0, 1.0);
        if let Some(ac) = &self.cfg.attack {
            let f0 = voicing.modal.first().map(|p| p.freq * scale).unwrap_or(440.0);
            let center = (f0 * ac.center_mult).clamp(ac.center_min, ac.center_max);
            v.atk_bpf = Biquad::bandpass(center, ac.q, sr);
            v.atk_amp = ac.amp * vel01 * vel01;
            v.atk_decay = (-1.0 / (ac.tau * sr)).exp();
            v.atk_remaining = (ac.dur * sr) as u32;
            v.atk_rng = base_seed ^ 0x9e3779b9;
        } else {
            v.atk_remaining = 0;
        }

        v.note = note;
        v.active = true;
        v.age = self.age_counter;
        v.gain = self.master_gain;
        v.rel_env = 1.0;
        v.releasing = false;
        v.sample = 0;
    }

    pub fn note_off(&mut self, note: u8) {
        // Damper: fast release. Top notes (no damper) ring on.
        let rel_mult = if note >= 100 {
            1.0
        } else {
            // register-dependent damper time (faster in treble)
            let t = if note < 48 { 0.30 } else if note < 72 { 0.18 } else { 0.10 };
            (-1.0 / (t * self.sr)).exp()
        };
        for v in &mut self.voices {
            if v.active && v.note == note && !v.releasing {
                v.releasing = true;
                v.rel_mult = rel_mult;
            }
        }
    }

    /// Render mono into `out` (overwrites).
    pub fn process(&mut self, out: &mut [f32]) {
        for s in out.iter_mut() {
            *s = 0.0;
        }
        let revert = self.jitter_revert;
        let diffusion = self.jitter_diffusion;
        let jitter_on = self.cfg.jitter.is_some();

        for v in &mut self.voices {
            if !v.active {
                continue;
            }
            let mut voice_alive = false;
            for (i, out_s) in out.iter_mut().enumerate() {
                let mut sum = 0.0f32;
                for o in &mut v.oscs {
                    let env = o.amp * (o.mix * o.fast_env + (1.0 - o.mix) * o.slow_env);
                    sum += env * o.s;
                    // jittered quadrature rotation (first-order)
                    if jitter_on && (v.sample + i as u64) & 15 == 0 {
                        o.drift = revert * o.drift + diffusion * lcg(&mut o.rng);
                    }
                    let dph = o.drift * o.phase_inc;
                    let ci = o.cos_inc - dph * o.sin_inc;
                    let si = o.sin_inc + dph * o.cos_inc;
                    let s_new = o.s * ci + o.c * si;
                    let c_new = o.c * ci - o.s * si;
                    o.s = s_new;
                    o.c = c_new;
                    o.fast_env *= o.fast_mult;
                    o.slow_env *= o.slow_mult;
                }
                // hammer attack burst
                if v.atk_remaining > 0 {
                    let noise = lcg(&mut v.atk_rng) / 1.7320508;
                    sum += v.atk_amp * v.atk_bpf.process(noise);
                    v.atk_amp *= v.atk_decay;
                    v.atk_remaining -= 1;
                }
                // release damper
                if v.releasing {
                    v.rel_env *= v.rel_mult;
                }
                let s = sum * v.gain * v.rel_env;
                *out_s += s;
                if s.abs() > 1e-5 {
                    voice_alive = true;
                }
            }
            // periodic quadrature renorm (prevents amplitude drift on long notes)
            for o in &mut v.oscs {
                let r2 = o.s * o.s + o.c * o.c;
                if r2 > 0.0 {
                    let inv = 1.0 / r2.sqrt();
                    o.s *= inv;
                    o.c *= inv;
                }
            }
            v.sample += out.len() as u64;
            // deactivate if fully released and quiet, or envelope collapsed
            if (v.releasing && v.rel_env < 1e-4) || (!voice_alive && v.sample > self.sr as u64 / 4) {
                v.active = false;
                v.oscs.clear();
            }
        }

        // safety soft-clip
        for s in out.iter_mut() {
            *s = s.tanh();
        }
    }
}

fn detune_factors(n: usize, cents: f32) -> Vec<f32> {
    if n <= 1 {
        return vec![1.0];
    }
    (0..n)
        .map(|i| {
            let frac = i as f32 / (n as f32 - 1.0) - 0.5;
            2f32.powf(frac * cents / 1200.0)
        })
        .collect()
}
