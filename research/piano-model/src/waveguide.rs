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

impl StringWaveguide {
    pub fn new(p: &StringParams, sr: u32) -> Self {
        let sr = sr as f32;
        let period = sr / p.f0;

        // dispersion allpass coefficient tuned to produce ~B inharmonicity.
        // Each allpass adds a small frequency-dependent phase; a small negative
        // coefficient makes high partials run sharp. (Empirically scaled;
        // measure B via `pm probe` and adjust.)
        let disp_a = -(p.inharmonicity * 800.0).clamp(0.0, 0.35);
        let disp: Vec<Allpass> = (0..p.n_disp.max(1)).map(|_| Allpass::new(disp_a)).collect();
        // dispersion filters add ~ phase delay at f0; approximate their delay so
        // the loop length lands on f0. For small a, group delay ≈ (1-a)/(1+a).
        let disp_delay = p.n_disp as f32 * (1.0 - disp_a) / (1.0 + disp_a);

        // loss LP phase delay at f0 (~ d/(1-d) small); fold into the budget.
        let d = (1.0 - p.brightness).clamp(0.0, 0.98) * 0.5;
        let lp_delay = d; // approx

        // integer delay + fractional tuning allpass to hit the exact period.
        // (The 0.14 vs 1.0 loop-sample term is an empirical tuning trim for the
        // constant ~7.5-cent offset; proper per-note tuning uses precise allpass
        // group-delay accounting — Smith PASP / Rauhala-Välimäki.)
        let target = period - disp_delay - lp_delay - 0.14;
        let n = target.floor().max(2.0) as usize;
        let frac = target - n as f32; // 0..1
        // first-order allpass fractional delay: a = (1-frac)/(1+frac)
        let ta = ((1.0 - frac) / (1.0 + frac)).clamp(-0.99, 0.99);

        // per-loop gain for the target T60: gain^(f0·t60) = 1e-3
        let loops = (p.f0 * p.t60).max(1.0);
        let loop_gain = 10f32.powf(-3.0 / loops);

        Self {
            buf: vec![0.0; n.max(2)],
            n: n.max(2),
            idx: 0,
            loop_gain,
            d,
            lp: 0.0,
            tune: Allpass::new(ta),
            disp,
            sr,
            exc: Vec::new(),
            exc_pos: 0,
        }
    }

    /// Strike the string. `vel01` 0..1. Harder = shorter contact = brighter.
    /// `strike_pos` 0..1 along the string (comb-nulls harmonics at the node).
    pub fn strike(&mut self, vel01: f32, strike_pos: f32) {
        // contact time: soft ~4 ms, hard ~1 ms → shorter = broader = brighter.
        let contact = (0.004 - 0.003 * vel01).max(0.0005);
        let w = ((contact * self.sr) as usize).max(2);
        let amp = 0.5 * vel01;
        // raised-cosine force pulse
        let mut pulse: Vec<f32> = (0..w)
            .map(|i| amp * (0.5 - 0.5 * (TAU * i as f32 / w as f32).cos()))
            .collect();
        // strike-point comb: subtract a copy delayed by the node distance
        let d = ((strike_pos * self.n as f32) as usize).clamp(1, self.n.saturating_sub(1));
        let orig = pulse.clone();
        for i in d..pulse.len() {
            pulse[i] -= orig[i - d];
        }
        self.exc = pulse;
        self.exc_pos = 0;
    }

    /// Current bridge output without advancing (for the coupling read).
    #[inline]
    pub fn peek(&self) -> f32 {
        self.buf[self.idx]
    }

    /// Advance the loop one sample, injecting `ext` (bridge coupling) alongside
    /// the hammer excitation. Returns the (pre-step) bridge output.
    #[inline]
    pub fn step(&mut self, ext: f32) -> f32 {
        let out = self.buf[self.idx];
        self.lp = (1.0 - self.d) * out + self.d * self.lp; // loss LP
        let mut s = self.lp;
        for ap in &mut self.disp {
            s = ap.process(s); // dispersion (inharmonicity)
        }
        s = self.loop_gain * self.tune.process(s); // tuning + decay
        if self.exc_pos < self.exc.len() {
            s += self.exc[self.exc_pos];
            self.exc_pos += 1;
        }
        s += ext; // bridge coupling
        self.buf[self.idx] = s;
        self.idx += 1;
        if self.idx >= self.n {
            self.idx = 0;
        }
        out
    }

    #[inline]
    pub fn process(&mut self) -> f32 {
        self.step(0.0)
    }
}

/// The 2–3 unison strings of one note, coupled through a shared bridge. The
/// two-stage decay (fast "prompt" symmetric mode → slow "aftersound"
/// antisymmetric mode) and the unison beating EMERGE from the coupling — they
/// are not programmed (Weinreich, "Coupled piano strings", 1977).
pub struct CoupledStrings {
    strings: Vec<StringWaveguide>,
    coupling: f32,
}

impl CoupledStrings {
    pub fn new(
        p: &StringParams,
        sr: u32,
        n_strings: usize,
        detune_cents: f32,
        coupling: f32,
    ) -> Self {
        let n = n_strings.max(1);
        let strings = (0..n)
            .map(|i| {
                // symmetric detune spread across the unison group
                let frac = if n > 1 { i as f32 / (n as f32 - 1.0) - 0.5 } else { 0.0 };
                let f = p.f0 * 2f32.powf(frac * detune_cents / 1200.0);
                let mut pp = StringParams { f0: f, ..*p };
                pp.f0 = f;
                StringWaveguide::new(&pp, sr)
            })
            .collect();
        Self { strings, coupling }
    }

    pub fn strike(&mut self, vel01: f32, strike_pos: f32) {
        for s in &mut self.strings {
            s.strike(vel01, strike_pos);
        }
    }

    /// One sample: each string feels the bridge motion driven by the OTHER
    /// strings (self-loading is already in its own termination). That mutual
    /// coupling is what produces the two-stage decay + beating.
    #[inline]
    pub fn process(&mut self) -> f32 {
        let outs: Vec<f32> = self.strings.iter().map(|s| s.peek()).collect();
        let sum: f32 = outs.iter().sum();
        for (i, s) in self.strings.iter_mut().enumerate() {
            let others = sum - outs[i];
            s.step(self.coupling * others);
        }
        sum // radiated bridge force
    }
}
