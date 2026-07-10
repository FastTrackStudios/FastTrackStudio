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
    // decay gates only apply when the reference's own two-stage fit is sane —
    // a collapsed fit (prompt ≈ after) or absurd values are metric noise, not
    // model error, and must not produce false FAILs
    let ref_fit_ok = after_r > prompt_r * 1.2 && prompt_r > 0.05 && after_r < 150.0;
    if ref_fit_ok && rel_err(prompt_m, prompt_r) > tol.prompt_rel {
        failures.push(format!(
            "prompt {prompt_m:.1}s vs {prompt_r:.1}s (>{:.0}%)",
            tol.prompt_rel * 100.0
        ));
    }
    if ref_fit_ok && rel_err(after_m, after_r) > tol.after_rel {
        failures.push(format!(
            "after {after_m:.1}s vs {after_r:.1}s (>{:.0}%)",
            tol.after_rel * 100.0
        ));
    }
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
<th>bright m/r</th><th>LSD dB</th><th>env corr</th><th class="failures">failures</th></tr>
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
             <td>{:.3} / {:.3}</td><td>{:.1}</td><td>{:.3}</td>\
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
            c.failures.join("; "),
        );
    }
    h.push_str("</table></body></html>\n");
    h
}
