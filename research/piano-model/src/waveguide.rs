//! Stiff-string digital waveguide — the coupled/physical-model voice (Pianoteq
//! paradigm: sound is *computed*, not fitted). A note is a delay-line loop:
//!
//!   loop = loop_gain · dispersion( loss_lp( delay(P) ) )
//!
//! - **delay line** of ~sr/f0 → the fundamental.
//! - **loss one-pole LP** → high frequencies decay faster (the soundboard
//!   impedance cutoff/slope); overall `loop_gain` sets sustain length.
//! - **dispersion allpass cascade** → frequency-dependent delay → the string
//!   partials run sharp = **inharmonicity** (Pianoteq's "string length").
//! - **fractional-delay allpass** → exact tuning.
//! - **hammer** injects a velocity-dependent excitation (harder = shorter
//!   contact = brighter → the nonlinear velocity→brightness we measured).
//!
//! References: J.O. Smith, *Physical Audio Signal Processing* (waveguides, stiff
//! string); Bensa et al. 2003 (physics→waveguide); Rauhala & Välimäki
//! (dispersion filter design).

const TAU: f32 = std::f32::consts::TAU;

/// First-order allpass: y[n] = a·x[n] + x[n-1] − a·y[n-1]. Used both for
/// fractional tuning and, cascaded, for dispersion (inharmonicity).
#[derive(Clone, Copy, Default)]
struct Allpass {
    a: f32,
    x1: f32,
    y1: f32,
}
impl Allpass {
    fn new(a: f32) -> Self {
        Self { a, x1: 0.0, y1: 0.0 }
    }
    #[inline]
    fn process(&mut self, x: f32) -> f32 {
        let y = self.a * x + self.x1 - self.a * self.y1;
        self.x1 = x;
        self.y1 = y;
        y
    }
}

#[derive(Clone, Copy)]
pub struct StringParams {
    pub f0: f32,
    pub t60: f32,          // overall decay time (s)
    pub brightness: f32,   // loss LP: 0 = dark/fast HF decay .. 1 = bright
    pub inharmonicity: f32,// target B (dispersion strength)
    pub n_disp: usize,     // dispersion allpass count
}

pub struct StringWaveguide {
    buf: Vec<f32>,
    n: usize,
    idx: usize,
    loop_gain: f32,
    // loss one-pole LP: y = (1-d)x + d·y1
    d: f32,
    lp: f32,
    tune: Allpass,
    disp: Vec<Allpass>,
    sr: f32,
    // hammer excitation queued at note-on
    exc: Vec<f32>,
    exc_pos: usize,
}

/// Exact phase delay (in samples) of the first-order allpass
/// H(z) = (a + z⁻¹)/(1 + a·z⁻¹) at normalized frequency ω.
fn ap_phase_delay(a: f64, w: f64) -> f64 {
    let (s, c) = w.sin_cos();
    let phi = (-s).atan2(a + c) - (-a * s).atan2(1.0 + a * c);
    -phi / w
}

/// Exact phase delay of the loss one-pole H(z) = (1−d)/(1 − d·z⁻¹) at ω.
fn lp_phase_delay(d: f64, w: f64) -> f64 {
    let phi = -(d * w.sin()).atan2(1.0 - d * w.cos());
    -phi / w
}

/// The full loop design, solved numerically (Rauhala–Välimäki in spirit):
/// given target f0, inharmonicity B, dispersion-cascade size M and loss d,
/// find the dispersion coefficient `a`, integer delay N and tuning coefficient
/// so that partial 1 lands exactly on f0 and partial k lands on the stiff-
/// string target k·f0·√(1+B·k²). Uses exact allpass/LP phase delays — no
/// empirical trims.
pub struct LoopDesign {
    pub n: usize,
    pub tune_a: f32,
    pub disp_a: f32,
}

pub fn design_loop(f0: f64, b: f64, m: usize, d: f64, sr: f64) -> LoopDesign {
    let w0 = std::f64::consts::TAU * f0 / sr;
    // measurement partial: high enough to see stiffness, below ~0.4·sr
    let k = ((0.4 * sr / f0).floor() as usize).clamp(2, 12) as f64;

    // Given a dispersion coefficient, budget the delay line + tuning allpass
    // so partial 1 sits exactly at f0; return (N, tune_a, predicted fk).
    let solve = |a: f64| -> (usize, f64, f64) {
        let period = sr / f0;
        let fixed = m as f64 * ap_phase_delay(a, w0) + lp_phase_delay(d, w0);
        let dline = period - fixed; // delay line + tuning allpass, in samples
        let n = (dline - 1.0).floor().max(2.0);
        // tuning allpass: iterate exact phase delay at w0 to hit the residue
        let mut frac = (dline - n).clamp(0.05, 1.95);
        let mut ta = (1.0 - frac) / (1.0 + frac);
        for _ in 0..4 {
            let err = (dline - n) - ap_phase_delay(ta, w0);
            frac = (frac + err).clamp(0.02, 1.98);
            ta = (1.0 - frac) / (1.0 + frac);
        }
        // predicted partial k: fixed point of f = k·sr / τ_total(f)
        let mut fk = k * f0;
        for _ in 0..24 {
            let w = std::f64::consts::TAU * fk / sr;
            let tau = n + ap_phase_delay(ta, w)
                + m as f64 * ap_phase_delay(a, w)
                + lp_phase_delay(d, w);
            fk = k * sr / tau;
        }
        (n as usize, ta, fk)
    };

    // target: fk = k·f0·√(1+B·k²) → bisect the dispersion coefficient.
    let fk_target = k * f0 * (1.0 + b * k * k).sqrt();
    let (mut lo, mut hi) = (-0.6f64, 0.0f64); // more negative = more dispersion
    let mut a = 0.0;
    if b > 1e-9 {
        for _ in 0..40 {
            a = 0.5 * (lo + hi);
            let (_, _, fk) = solve(a);
            if fk < fk_target {
                hi = a; // need more dispersion (more negative)
            } else {
                lo = a;
            }
        }
    }
    let (n, ta, _) = solve(a);
    LoopDesign { n, tune_a: ta as f32, disp_a: a as f32 }
}

impl StringWaveguide {
    pub fn new(p: &StringParams, sr: u32) -> Self {
        let sr = sr as f32;
        let d = (1.0 - p.brightness).clamp(0.0, 0.98) * 0.5;
        let m = p.n_disp.max(1);
        let des = design_loop(
            p.f0 as f64,
            p.inharmonicity as f64,
            m,
            d as f64,
            sr as f64,
        );
        let disp: Vec<Allpass> = (0..m).map(|_| Allpass::new(des.disp_a)).collect();

        // per-loop gain for the target T60: gain^(f0·t60) = 1e-3 — COMPENSATED
        // for the loss LP's own attenuation at f0. |H_lp(ω0)| < 1 adds decay
        // the naive formula ignores; negligible in bass (ω0 small) but it
        // dominates in the treble (−96 dB/s at note 84 with a dark filter —
        // the "high notes are plucks" bug). Stability bound: |H_lp| peaks at
        // DC = 1, so loop_gain itself must stay < 1.
        let w0 = TAU * p.f0 / sr;
        let hlp = (1.0 - d) / (1.0 - 2.0 * d * w0.cos() + d * d).sqrt();
        let loops = (p.f0 * p.t60).max(1.0);
        let loop_gain = (10f32.powf(-3.0 / loops) / hlp.max(1e-3)).min(0.99995);

        Self {
            buf: vec![0.0; des.n.max(2)],
            n: des.n.max(2),
            idx: 0,
            loop_gain,
            d,
            lp: 0.0,
            tune: Allpass::new(des.tune_a),
            disp,
            sr,
            exc: Vec::new(),
            exc_pos: 0,
        }
    }

    /// Strike the string. `vel01` 0..1. `strike_pos` 0..1 along the string
    /// (comb-nulls harmonics at the node).
    ///
    /// The excitation is a **nonlinear felt hammer** integrated at note-on
    /// (Chaigne & Askenfelt 1994; Stulov 1995, sans hysteresis):
    ///   m·ẍ_h = −F,   F = k·u^p  (u = felt compression),   ẏ_s = F/(2R).
    /// Harder hits compress the felt into its stiff region → shorter contact
    /// → brighter — the velocity→brightness curve comes out of the ODE, not
    /// a pulse-shape heuristic.
    pub fn strike(&mut self, vel01: f32, strike_pos: f32) {
        // Hammers are graded along the keyboard: bass = heavy + soft felt,
        // treble = light + hard (Conklin; Pianoteq's "hammer hardness").
        // Scale mass and stiffness from the string's fundamental.
        let f0_est = self.sr / self.n as f32;
        let g = (f0_est / 220.0).clamp(0.1, 20.0); // 1.0 at A3
        let m = (0.009 * g.powf(-0.3)).clamp(0.004, 0.014); // kg
        let k = (1.5e9 * g.powf(1.5)).clamp(1e7, 1e11); // felt stiffness
        let p_exp = 2.8f32; // felt stiffness exponent
        let two_r = 10.0f32; // 2× string wave impedance, kg/s
        let dt = 1.0 / self.sr;
        let v0 = 1.2 + 4.3 * vel01; // hammer speed at contact, m/s

        let mut xh = 0.0f32; // hammer position
        let mut vh = v0;
        let mut ys = 0.0f32; // string displacement at contact point
        let mut pulse: Vec<f32> = Vec::with_capacity(256);
        let g_exc = 0.02; // force → loop-amplitude scale
        for _ in 0..(0.02 * self.sr) as usize {
            let u = xh - ys;
            if u <= 0.0 && !pulse.is_empty() {
                break; // hammer left the string
            }
            let f = if u > 0.0 { k * u.powf(p_exp) } else { 0.0 };
            vh -= f / m * dt;
            xh += vh * dt;
            ys += f / two_r * dt;
            pulse.push(g_exc * f / two_r); // injected velocity wave
        }
        // strike-point comb: subtract a copy delayed by the node distance
        let d = ((strike_pos * self.n as f32) as usize).clamp(1, self.n.saturating_sub(1));
        let orig = pulse.clone();
        for i in d..pulse.len() {
            pulse[i] -= orig[i - d];
        }
        self.exc = pulse;
        self.exc_pos = 0;
    }

    /// Scale the queued hammer excitation (unison amp skew).
    fn scale_exc(&mut self, k: f32) {
        for x in &mut self.exc {
            *x *= k;
        }
    }

    /// Delay the queued hammer excitation by whole samples (unison timing skew).
    fn delay_exc(&mut self, samples: usize) {
        if samples > 0 {
            self.exc.splice(0..0, std::iter::repeat(0.0).take(samples));
        }
    }

    /// Phase A of a sample: read the delay output and run it through the loop
    /// filters (loss LP → dispersion → tuning·gain). Returns the string's
    /// outgoing wave arriving at the bridge. MUST be followed by `commit`.
    ///
    /// The bridge junction must be computed from these *filtered* waves —
    /// mixing raw delay taps with filtered feedback breaks passivity (the
    /// filter's phase rotation lets |G(z) − gN| exceed 1 → slow blow-up).
    #[inline]
    pub fn reflect(&mut self) -> f32 {
        let out = self.buf[self.idx];
        self.lp = (1.0 - self.d) * out + self.d * self.lp; // loss LP
        let mut s = self.lp;
        for ap in &mut self.disp {
            s = ap.process(s); // dispersion (inharmonicity)
        }
        self.loop_gain * self.tune.process(s) // tuning + decay
    }

    /// Phase B: write the reflected wave (+ bridge term) back, inject the
    /// hammer, advance. `refl` is what returns down the string.
    #[inline]
    pub fn commit(&mut self, mut refl: f32) {
        if self.exc_pos < self.exc.len() {
            refl += self.exc[self.exc_pos];
            self.exc_pos += 1;
        }
        self.buf[self.idx] = refl;
        self.idx += 1;
        if self.idx >= self.n {
            self.idx = 0;
        }
    }

    #[inline]
    pub fn process(&mut self) -> f32 {
        let out = self.buf[self.idx];
        let r = self.reflect();
        self.commit(r);
        out
    }
}

/// The 2–3 unison strings of one note, coupled through a shared **bridge
/// junction** (Smith PASP, parallel junction of N waveguides into a load).
///
/// Each string of impedance R meets a bridge load of impedance `zb·R`.
/// Junction velocity  v_J = g·Σ v_i⁺  with  g = 2/(N + zb); each string's
/// reflected wave is  v_i⁻ = v_J − v_i⁺  (the nut's −1 makes the loop
/// feedback  own − v_J). The reflection matrix is  I − g·11ᵀ:
///
/// - **symmetric mode** (strings in phase): eigenvalue (zb−N)/(zb+N) < 1 —
///   energy flows into the bridge → the fast "prompt" decay;
/// - **antisymmetric modes**: Σ = 0 → v_J = 0 → lossless reflection — the
///   slow "aftersound" (limited only by the strings' internal loss).
///
/// Both |eigenvalues| ≤ 1 for any zb ≥ 0, so the coupling is **passive —
/// unconditionally stable**. Detuning mixes the modes → the two-stage decay
/// and unison beating EMERGE (Weinreich, "Coupled piano strings", 1977).
/// The radiated output is v_J itself — what the bridge actually delivers to
/// the soundboard.
pub struct CoupledStrings {
    strings: Vec<StringWaveguide>,
    g: f32, // junction gain 2/(N + zb)
    outs: Vec<f32>,
    /// Hammer force skew across the unisons (± fraction). Seeds the
    /// antisymmetric modes → sets the aftersound level = the envelope knee.
    pub skew: f32,
}

impl CoupledStrings {
    /// `zb` = bridge impedance / string impedance. Large = rigid bridge
    /// (little transmission, long prompt decay); small = soft (fast decay).
    /// Prompt T60 ≈ 6.908 / (f0 · −ln((zb−N)/(zb+N))).
    pub fn new(
        p: &StringParams,
        sr: u32,
        n_strings: usize,
        detune_cents: f32,
        zb: f32,
    ) -> Self {
        let n = n_strings.max(1);
        let strings: Vec<StringWaveguide> = (0..n)
            .map(|i| {
                // symmetric detune spread across the unison group
                let frac = if n > 1 { i as f32 / (n as f32 - 1.0) - 0.5 } else { 0.0 };
                let f = p.f0 * 2f32.powf(frac * detune_cents / 1200.0);
                StringWaveguide::new(&StringParams { f0: f, ..*p }, sr)
            })
            .collect();
        let g = 2.0 / (n as f32 + zb.max(0.0));
        Self { strings, g, outs: vec![0.0; n], skew: 0.15 }
    }

    /// Strike all unison strings — with a slight amp + timing skew per string.
    /// A real hammer never hits the unisons perfectly equally (felt grading,
    /// micro-misalignment); that asymmetry is what SEEDS the antisymmetric
    /// modes, so the aftersound starts ~20 dB under the prompt sound instead
    /// of near silence (Weinreich §V).
    pub fn strike(&mut self, vel01: f32, strike_pos: f32) {
        let n = self.strings.len();
        for (i, s) in self.strings.iter_mut().enumerate() {
            s.strike(vel01, strike_pos);
            let frac = if n > 1 { i as f32 / (n as f32 - 1.0) - 0.5 } else { 0.0 };
            s.scale_exc(1.0 + self.skew * frac); // hammer force skew
            let skew = (0.0003 * i as f32 * s.sr) as usize; // 0.3 ms contact stagger
            s.delay_exc(skew);
        }
    }

    /// One sample through the bridge junction. Returns v_J (bridge velocity —
    /// the signal handed to the soundboard).
    #[inline]
    pub fn process(&mut self) -> f32 {
        let mut sum = 0.0;
        for (o, s) in self.outs.iter_mut().zip(self.strings.iter_mut()) {
            *o = s.reflect();
            sum += *o;
        }
        let vj = self.g * sum;
        for (o, s) in self.outs.iter().zip(self.strings.iter_mut()) {
            s.commit(*o - vj);
        }
        vj
    }
}
