//! Engine-vs-reference validation harness.
//!
//! Pattern adapted from melange's SPICE-validation pipeline (GPL-3, as is this
//! crate): golden reference + multi-metric tolerance gates + strict/default/
//! relaxed presets + an HTML diff report. The references here are sample
//! libraries (Keyscape) or Pianoteq renders instead of ngspice, so the
//! metrics are perceptual/physical rather than sample-exact: a physical model
//! never nulls a recording, but its *measurable physics* must match —
//! tuning, inharmonicity, two-stage decay, velocity→brightness, spectrum
//! (LSD) and the loudness envelope.

use std::fmt::Write as _;

use crate::analyze;

/// Tolerance gate per metric. Same three-preset shape as melange's
/// `ComparisonConfig` (strict / default / relaxed).
#[derive(Debug, Clone, Copy)]
pub struct Tolerances {
    /// |tuning error| of the fundamental, cents.
    pub tune_cents: f32,
    /// |B_model − B_ref| / B_ref.
    pub b_rel: f32,
    /// |prompt T60 err| relative.
    pub prompt_rel: f32,
    /// |aftersound T60 err| relative.
    pub after_rel: f32,
    /// |brightness err| relative (partials>5 / first-5 energy).
    pub bright_rel: f32,
    /// Multi-res log-spectral distance ceiling, dB. Calibration: same file
    /// = 0; two Keyscape takes of one note 16 vel apart = 8.8 dB.
    pub lsd_db: f32,
    /// Minimum Pearson correlation of the dB loudness envelopes.
    pub env_corr_min: f32,
    /// Max sample-by-sample RMS error of the dB envelopes (sustain/dropoff).
    pub env_rmse_db: f32,
    /// Max energy-weighted per-partial envelope RMSE (THE strict gate).
    pub partial_env_db: f32,
}

impl Tolerances {
    pub fn strict() -> Self {
        Self {
            tune_cents: 1.0,
            b_rel: 0.10,
            prompt_rel: 0.15,
            after_rel: 0.25,
            bright_rel: 0.30,
            lsd_db: 12.0,
            env_corr_min: 0.98,
            env_rmse_db: 2.0,
            partial_env_db: 5.0,
        }
    }
    pub fn default_level() -> Self {
        Self {
            tune_cents: 3.0,
            b_rel: 0.30,
            prompt_rel: 0.35,
            after_rel: 0.50,
            bright_rel: 0.60,
            lsd_db: 18.0,
            env_corr_min: 0.95,
            env_rmse_db: 4.0,
            partial_env_db: 7.5,
        }
    }
    pub fn relaxed() -> Self {
        Self {
            tune_cents: 8.0,
            b_rel: 0.60,
            prompt_rel: 0.70,
            after_rel: 1.00,
            bright_rel: 1.50,
            lsd_db: 26.0,
            env_corr_min: 0.85,
            env_rmse_db: 7.0,
            partial_env_db: 11.0,
        }
    }
    pub fn by_name(name: &str) -> Self {
        match name {
            "strict" => Self::strict(),
            "relaxed" => Self::relaxed(),
            _ => Self::default_level(),
        }
    }
}

/// One (note, velocity) cell's scorecard.
#[derive(Debug, Clone)]
pub struct CellReport {
    pub note: u8,
    pub vel: u8,
    pub tune_cents: f32,
    pub b_model: f32,
    pub b_ref: f32,
    pub prompt_model: f32,
    pub prompt_ref: f32,
    pub after_model: f32,
    pub after_ref: f32,
    pub bright_model: f32,
    pub bright_ref: f32,
    pub lsd_db: f32,
    pub env_corr: f32,
    pub env_rmse: f32,
    pub partial_env: f32,
    pub passed: bool,
    pub failures: Vec<String>,
}

fn rel_err(model: f32, reference: f32) -> f32 {
    (model - reference).abs() / reference.abs().max(1e-9)
}

/// dB loudness envelope: 50 ms RMS frames, floored at −90 dB.
pub fn envelope_db(x: &[f32], sr: u32) -> Vec<f32> {
    let hop = (sr as usize / 20).max(1);
    x.chunks(hop)
        .map(|c| {
            let rms = (c.iter().map(|v| v * v).sum::<f32>() / c.len() as f32).sqrt();
            20.0 * rms.max(3.16e-5).log10()
        })
        .collect()
}

/// Sample-by-sample RMS error between two dB envelopes. This is THE sustain/
/// dropoff metric — melange-style sample-exact comparison, applied in the
/// envelope domain where a physical model CAN null a recording (raw waveforms
/// can't: phases differ).
///
/// Fairness rules (each was a measured failure mode, not a guess):
/// - onset-align both signals first (samples carry leading silence);
/// - peak-align both to 0 dB;
/// - floor BOTH at the reference's noise floor (its minimum frame + 3 dB) —
///   the recording's tail flattens at mic-noise level while the model decays
///   to silence, and grading that difference is grading hiss reproduction.
pub fn envelope_rmse_db(model: &[f32], real: &[f32], sr: u32) -> f32 {
    let em = envelope_db(&model[analyze::onset(model).min(model.len())..], sr);
    let er = envelope_db(&real[analyze::onset(real).min(real.len())..], sr);
    let n = em.len().min(er.len());
    if n == 0 {
        return f32::NAN;
    }
    let pm = em.iter().cloned().fold(f32::MIN, f32::max);
    let pr = er.iter().cloned().fold(f32::MIN, f32::max);
    let floor = er.iter().cloned().fold(f32::MAX, f32::min) - pr + 3.0;
    let mut acc = 0.0f64;
    for i in 0..n {
        let a = (em[i] - pm).max(floor);
        let b = (er[i] - pr).max(floor);
        let d = (a - b) as f64;
        acc += d * d;
    }
    (acc / n as f64).sqrt() as f32
}

/// STRICTEST comparison: the **per-partial envelope matrix**. Track each of
/// the reference's first K partials' dB trajectories over time (STFT bin
/// magnitude at the partial frequency) and compare the model's trajectory at
/// the SAME frequencies, frame by frame, energy-weighted.
///
/// This catches what every averaged metric smears over: a partial that's the
/// right average level but decays wrong, a partial that's too strong from the
/// start, a missing two-stage knee in ONE partial. Both matrices are
/// normalized by their own global peak (so relative partial balance is graded
/// too) and floored at the reference's per-partial noise floor.
pub fn partial_env_rmse_db(model: &[f32], real: &[f32], sr: u32, f0: f32) -> f32 {
    match RefTrack::build(real, sr, f0) {
        Some(rt) => rt.rmse_vs(model, sr),
        None => f32::NAN,
    }
}

const PT_K: usize = 12;
const PT_FFT: usize = 8192;
const PT_HOP: usize = 2048;

fn pt_track(x: &[f32], sr: u32, freqs: &[f32], frames: usize) -> Vec<Vec<f32>> {
    use rustfft::{num_complex::Complex, FftPlanner};
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(PT_FFT);
    let win: Vec<f32> = (0..PT_FFT)
        .map(|i| {
            let w = std::f32::consts::PI * i as f32 / (PT_FFT as f32 - 1.0);
            w.sin() * w.sin()
        })
        .collect();
    let bin_of = |f: f32| ((f * PT_FFT as f32 / sr as f32).round() as usize).min(PT_FFT / 2 - 2);
    let mut out = vec![Vec::with_capacity(frames); freqs.len()];
    let mut buf = vec![Complex::new(0.0f32, 0.0); PT_FFT];
    for fr in 0..frames {
        let off = fr * PT_HOP;
        for i in 0..PT_FFT {
            buf[i] = Complex::new(x[off + i] * win[i], 0.0);
        }
        fft.process(&mut buf);
        for (k, f) in freqs.iter().enumerate() {
            let b = bin_of(*f);
            // peak over ±1 bin (detune/beating wander)
            let m = buf[b - 1].norm().max(buf[b].norm()).max(buf[b + 1].norm());
            out[k].push(20.0 * m.max(1e-9).log10());
        }
    }
    out
}

/// Attack signature: broadband band energies in the first 50 ms, expressed
/// RELATIVE to the fundamental partial's level in the same window (so it is
/// normalization-invariant). This is what the per-partial matrix cannot see —
/// a model whose attack is a thump 20 dB above its tone scores identically on
/// partials, and sounds like an impulse, not a piano.
pub fn attack_signature(x: &[f32], sr: u32, f0: f32) -> [f32; 4] {
    use rustfft::{num_complex::Complex, FftPlanner};
    const N: usize = 4096;
    let on = analyze::onset(x).min(x.len());
    let x = &x[on..];
    let mut buf = vec![Complex::new(0.0f32, 0.0); N];
    for i in 0..N.min(x.len()) {
        let w = 0.5 - 0.5 * (std::f32::consts::TAU * i as f32 / N as f32).cos();
        buf[i] = Complex::new(x[i] * w, 0.0);
    }
    FftPlanner::new().plan_fft_forward(N).process(&mut buf);
    let mag: Vec<f32> = buf[..N / 2].iter().map(|c| c.norm()).collect();
    let bin = |f: f32| ((f * N as f32 / sr as f32) as usize).clamp(1, N / 2 - 2);
    // fundamental level (peak over ±2 bins)
    let fb = bin(f0);
    let tone = mag[fb.saturating_sub(2)..(fb + 3).min(N / 2)]
        .iter()
        .cloned()
        .fold(1e-9f32, f32::max);
    // band mean energies, skipping bins within ±3 of any partial
    let bands = [(150.0, 800.0), (800.0, 1800.0), (2300.0, 4000.0), (4500.0, 9000.0)];
    let mut out = [0.0f32; 4];
    for (bi, (lo, hi)) in bands.iter().enumerate() {
        let (mut acc, mut n) = (0.0f64, 0usize);
        for b in bin(*lo)..bin(*hi) {
            let f = b as f32 * sr as f32 / N as f32;
            let k = (f / f0).round().max(1.0);
            if (f - k * f0).abs() < 3.0 * sr as f32 / N as f32 {
                continue; // partial bin — the matrix already grades those
            }
            acc += (mag[b] as f64).powi(2);
            n += 1;
        }
        let rms = if n > 0 { (acc / n as f64).sqrt() as f32 } else { 1e-9 };
        out[bi] = 20.0 * (rms.max(1e-9) / tone).log10(); // dB rel tone
    }
    out
}

/// Cached reference side of the per-partial metric — build once per
/// (note, layer), score many candidate renders against it cheaply.
pub struct RefTrack {
    freqs: Vec<f32>,
    mat: Vec<Vec<f32>>,
    peak: f32,
    frames: usize,
    len: usize,
}

impl RefTrack {
    pub fn build(real: &[f32], sr: u32, f0: f32) -> Option<Self> {
        let r0 = analyze::onset(real).min(real.len());
        let real = &real[r0..];
        if real.len() < PT_FFT * 2 {
            return None;
        }
        let ar = analyze::analyze_note(real, sr, f0, PT_K);
        let freqs: Vec<f32> = ar.partials.iter().copied().take(PT_K).collect();
        if freqs.is_empty() {
            return None;
        }
        let frames = (real.len() - PT_FFT) / PT_HOP;
        let mat = pt_track(real, sr, &freqs, frames);
        let peak = mat.iter().flat_map(|r| r.iter().copied()).fold(f32::MIN, f32::max);
        Some(Self { freqs, mat, peak, frames, len: real.len() })
    }

    pub fn rmse_vs(&self, model: &[f32], sr: u32) -> f32 {
        let m0 = analyze::onset(model).min(model.len());
        let model = &model[m0..];
        let len = model.len().min(self.len);
        if len < PT_FFT * 2 {
            return f32::NAN;
        }
        let frames = ((len - PT_FFT) / PT_HOP).min(self.frames);
        let tm = pt_track(model, sr, &self.freqs, frames);
        let pm = tm.iter().flat_map(|r| r.iter().copied()).fold(f32::MIN, f32::max);
        let (mut acc, mut wsum) = (0.0f64, 0.0f64);
        for k in 0..self.freqs.len() {
            let floor = self.mat[k].iter().cloned().fold(f32::MAX, f32::min) - self.peak + 3.0;
            // weight: the partial's peak level (linear) in WHICHEVER signal is
            // louder. Weighting by the reference alone makes a partial the
            // model renders 40 dB too loud nearly invisible to the loss when
            // the reference barely has it (measured at note 96 — the fitter
            // plateaued because its worst error carried no weight).
            let ref_pk = self.mat[k].iter().cloned().fold(f32::MIN, f32::max) - self.peak;
            let mod_pk = tm[k].iter().cloned().fold(f32::MIN, f32::max) - pm;
            let w = 10f64.powf(ref_pk.max(mod_pk) as f64 / 20.0);
            for i in 0..frames.min(tm[k].len()).min(self.mat[k].len()) {
                let a = (tm[k][i] - pm).max(floor);
                let b = (self.mat[k][i] - self.peak).max(floor);
                let d = (a - b) as f64;
                acc += w * d * d;
                wsum += w;
            }
        }
        if wsum > 0.0 {
            (acc / wsum).sqrt() as f32
        } else {
            f32::NAN
        }
    }
}

pub fn pearson(a: &[f32], b: &[f32]) -> f32 {
    let n = a.len().min(b.len());
    if n < 2 {
        return 0.0;
    }
    let (a, b) = (&a[..n], &b[..n]);
    let ma = a.iter().sum::<f32>() / n as f32;
    let mb = b.iter().sum::<f32>() / n as f32;
    let (mut num, mut da, mut db) = (0.0f64, 0.0f64, 0.0f64);
    for i in 0..n {
        let x = (a[i] - ma) as f64;
        let y = (b[i] - mb) as f64;
        num += x * y;
        da += x * x;
        db += y * y;
    }
    if da > 0.0 && db > 0.0 {
        (num / (da.sqrt() * db.sqrt())) as f32
    } else {
        0.0
    }
}

fn two_stage(a: &analyze::NoteAnalysis) -> (f32, f32) {
    a.modal
        .first()
        .map(|p| {
            let f = if p.decay_fast > 0.0 { 6.908 / p.decay_fast } else { 0.0 };
            let s = if p.decay_slow > 0.0 { 6.908 / p.decay_slow } else { 0.0 };
            (f, s)
        })
        .unwrap_or((0.0, 0.0))
}

fn brightness(a: &analyze::NoteAnalysis, samples: &[f32], sr: u32) -> f32 {
    let mag = analyze::avg_mag(samples, sr, 3.0);
    let bin_hz = sr as f32 / (mag.len() as f32 * 2.0);
    let (mut lo, mut hi) = (0.0f64, 0.0f64);
    for (i, p) in a.modal.iter().enumerate() {
        let b = (p.freq / bin_hz).round() as usize;
        let e = mag.get(b).map(|&m| (m as f64).powi(2)).unwrap_or(0.0);
        if i < 5 {
            lo += e
        } else {
            hi += e
        }
    }
    (hi / lo.max(1e-12)) as f32
}

/// Score one cell: model render vs reference recording. Both are analyzed
/// over the SAME duration (the shorter), so decay fits are comparable.
pub fn validate_cell(
    note: u8,
    vel: u8,
    model: &[f32],
    real: &[f32],
    sr: u32,
    tol: &Tolerances,
) -> CellReport {
    let n = model.len().min(real.len());
    let (model, real) = (&model[..n], &real[..n]);
    let expected = analyze::midi_hz(note);
    let am = analyze::analyze_note(model, sr, expected, 24);
    let ar = analyze::analyze_note(real, sr, expected, 24);

    let tune_cents = 1200.0 * (am.f0 / ar.f0.max(1.0)).log2();
    let (prompt_m, after_m) = two_stage(&am);
    let (prompt_r, after_r) = two_stage(&ar);
    let bright_m = brightness(&am, model, sr);
    let bright_r = brightness(&ar, real, sr);
    let lsd_db = analyze::accuracy_lsd(model, real, sr);
    let env_corr = pearson(&envelope_db(model, sr), &envelope_db(real, sr));
    let env_rmse = envelope_rmse_db(model, real, sr);
    let partial_env = partial_env_rmse_db(model, real, sr, expected);

    let mut failures = Vec::new();
    if tune_cents.abs() > tol.tune_cents {
        failures.push(format!("tuning {tune_cents:+.1}c > ±{:.1}c", tol.tune_cents));
    }
    if rel_err(am.inharmonicity_b, ar.inharmonicity_b) > tol.b_rel {
        failures.push(format!(
            "B {:.2e} vs {:.2e} (>{:.0}%)",
            am.inharmonicity_b,
            ar.inharmonicity_b,
            tol.b_rel * 100.0
        ));
    }
    // NOTE: the old prompt/after gates (k1 two-stage fit comparison) are
    // retired — the sample-by-sample envelope and per-partial trajectory
    // gates below measure decay directly and far more strictly; the k1
    // numbers remain in the report for reference.
    if rel_err(bright_m, bright_r) > tol.bright_rel {
        failures.push(format!(
            "brightness {bright_m:.3} vs {bright_r:.3} (>{:.0}%)",
            tol.bright_rel * 100.0
        ));
    }
    if lsd_db > tol.lsd_db {
        failures.push(format!("LSD {lsd_db:.1} dB > {:.1} dB", tol.lsd_db));
    }
    if env_corr < tol.env_corr_min {
        failures.push(format!("env corr {env_corr:.3} < {:.3}", tol.env_corr_min));
    }
    if env_rmse > tol.env_rmse_db {
        failures.push(format!("env RMSE {env_rmse:.1} dB > {:.1} dB", tol.env_rmse_db));
    }
    if partial_env.is_finite() && partial_env > tol.partial_env_db {
        failures.push(format!(
            "partial-env {partial_env:.1} dB > {:.1} dB",
            tol.partial_env_db
        ));
    }

    CellReport {
        note,
        vel,
        tune_cents,
        b_model: am.inharmonicity_b,
        b_ref: ar.inharmonicity_b,
        prompt_model: prompt_m,
        prompt_ref: prompt_r,
        after_model: after_m,
        after_ref: after_r,
        bright_model: bright_m,
        bright_ref: bright_r,
        lsd_db,
        env_corr,
        env_rmse,
        partial_env,
        passed: failures.is_empty(),
        failures,
    }
}

/// HTML diff report (melange `visualizer.rs` pattern): summary header + a
/// per-cell metric table, failing cells highlighted, worst metric first.
pub fn html_report(title: &str, level: &str, cells: &[CellReport]) -> String {
    let passed = cells.iter().filter(|c| c.passed).count();
    let mut h = String::new();
    let _ = write!(
        h,
        r#"<!DOCTYPE html><html><head><meta charset="utf-8"><title>{title}</title>
<style>
body{{font-family:system-ui,sans-serif;margin:2rem;background:#111;color:#ddd}}
h1{{font-size:1.3rem}} .sub{{color:#888}}
table{{border-collapse:collapse;margin-top:1rem;font-size:0.85rem}}
th,td{{padding:4px 10px;border:1px solid #333;text-align:right}}
th{{background:#1c1c1c}} td.name{{text-align:left}}
tr.pass td.status{{color:#4c4}} tr.fail td.status{{color:#e55}}
td.bad{{background:#3a1414}}
.failures{{color:#e88;font-size:0.8rem;text-align:left;max-width:34rem}}
</style></head><body>
<h1>{title}</h1>
<p class="sub">tolerance level: <b>{level}</b> — {passed}/{n} cells passed</p>
<table><tr><th>note</th><th>vel</th><th>status</th><th>tune ¢</th>
<th>B model</th><th>B ref</th><th>prompt m/r (s)</th><th>after m/r (s)</th>
<th>bright m/r</th><th>LSD dB</th><th>env corr</th><th>env RMSE</th><th>part-env</th><th class="failures">failures</th></tr>
"#,
        n = cells.len()
    );
    for c in cells {
        let cls = if c.passed { "pass" } else { "fail" };
        let status = if c.passed { "PASS" } else { "FAIL" };
        let _ = write!(
            h,
            "<tr class=\"{cls}\"><td>{}</td><td>{}</td><td class=\"status\">{status}</td>\
             <td>{:+.1}</td><td>{:.2e}</td><td>{:.2e}</td>\
             <td>{:.1} / {:.1}</td><td>{:.1} / {:.1}</td>\
             <td>{:.3} / {:.3}</td><td>{:.1}</td><td>{:.3}</td><td>{:.1}</td><td>{:.1}</td>\
             <td class=\"failures\">{}</td></tr>\n",
            c.note,
            c.vel,
            c.tune_cents,
            c.b_model,
            c.b_ref,
            c.prompt_model,
            c.prompt_ref,
            c.after_model,
            c.after_ref,
            c.bright_model,
            c.bright_ref,
            c.lsd_db,
            c.env_corr,
            c.env_rmse,
            c.partial_env,
            c.failures.join("; "),
        );
    }
    h.push_str("</table></body></html>\n");
    h
}
