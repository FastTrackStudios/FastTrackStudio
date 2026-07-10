//! Per-note waveguide parameter table, extracted FROM the reference library.
//!
//! Every parameter the validation harness flagged as note-dependent gets a
//! per-note value, inverted analytically from `analyze` measurements where
//! the physics permits:
//!
//! - **f0**: the measured fundamental — captures the piano's stretch tuning
//!   (Railsback), not equal temperament.
//! - **B**: measured inharmonicity → the dispersion design target.
//! - **n_disp**: grown until the designed allpass coefficient is inside its
//!   usable range (bass needs a deeper cascade — Rauhala–Välimäki).
//! - **t60**: string-internal loss = the measured aftersound T60.
//! - **zb**: inverted from the measured prompt T60 through the junction
//!   eigenvalue: prompt rate = bridge rate + internal rate,
//!   λ = exp(−6.908/(f0·t_bridge)), zb = N·(1+λ)/(1−λ).
//! - **brightness**: optionally refined per note by scoring candidate loss
//!   settings with the LSD metric against the reference.
//!
//! The table doubles as the dataset for cross-note correlation — the goal is
//! to discover smooth trends (B vs note, stretch vs note, decay vs note) that
//! collapse 88 entries into a few physical curves.

use std::path::Path;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::waveguide;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WgNote {
    pub note: u8,
    /// Measured (stretch-tuned) fundamental, Hz.
    pub f0: f32,
    /// Inharmonicity target.
    pub b: f32,
    /// Dispersion cascade size (designed, per note).
    pub n_disp: usize,
    /// String-internal T60 (the aftersound), s.
    pub t60: f32,
    /// Bridge/string impedance ratio (sets the prompt decay).
    pub zb: f32,
    /// Loss-LP brightness 0..1.
    pub brightness: f32,
    pub strike: f32,
    pub detune: f32,
    /// Body/radiation EQ breakpoints (freq Hz, linear gain): the reference's
    /// partial amplitudes ÷ the raw bridge output's, sampled AT the partials.
    /// This is the commuted-synthesis body — the piece a bridge-velocity
    /// output can't supply. None = flat.
    #[serde(default)]
    pub body: Option<Vec<(f32, f32)>>,
    // measured context, kept for correlation studies
    pub prompt_ref: f32,
    pub after_ref: f32,
    pub bright_ref: f32,
    pub cents_vs_et: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WgTable {
    pub library: String,
    pub notes: Vec<WgNote>,
}

impl WgTable {
    pub fn load(path: &Path) -> Result<Self> {
        Ok(serde_json::from_str(&std::fs::read_to_string(path)?)?)
    }
    pub fn save(&self, path: &Path) -> Result<()> {
        if let Some(par) = path.parent() {
            std::fs::create_dir_all(par)?;
        }
        std::fs::write(path, serde_json::to_string_pretty(self)?)?;
        Ok(())
    }
    pub fn get(&self, note: u8) -> Option<&WgNote> {
        self.notes.iter().find(|n| n.note == note)
    }
    /// Params for the engine from a table entry.
    pub fn params(n: &WgNote) -> waveguide::StringParams {
        waveguide::StringParams {
            f0: n.f0,
            t60: n.t60,
            brightness: n.brightness,
            inharmonicity: n.b,
            n_disp: n.n_disp,
        }
    }
}

/// Invert the bridge impedance ratio from the measured prompt T60.
/// The total prompt rate is bridge loss + string-internal loss; peel the
/// internal part off, then invert the symmetric-mode eigenvalue.
pub fn invert_zb(f0: f32, prompt: f32, t60_internal: f32, n_strings: f32) -> f32 {
    let rate_total = 1.0 / prompt.clamp(0.05, 120.0);
    let rate_int = 1.0 / t60_internal.clamp(0.5, 200.0);
    let rate_bridge = (rate_total - rate_int).max(1e-3);
    let neg_ln_lambda = 6.908 / (f0 * (1.0 / rate_bridge));
    let lambda = (-neg_ln_lambda).exp();
    (n_strings * (1.0 + lambda) / (1.0 - lambda).max(1e-6)).clamp(10.0, 50_000.0)
}

fn median(v: &mut Vec<f32>) -> f32 {
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    v[v.len() / 2]
}

/// Median-of-(2r+1) across note neighbors, skipping entries `bad` marks as
/// unusable (they get replaced entirely by the neighborhood median).
fn median_filter(vals: &[f32], bad: &[bool], r: usize) -> Vec<f32> {
    (0..vals.len())
        .map(|i| {
            let lo = i.saturating_sub(r);
            let hi = (i + r + 1).min(vals.len());
            let mut win: Vec<f32> = (lo..hi).filter(|&j| !bad[j]).map(|j| vals[j]).collect();
            if win.is_empty() {
                vals[i]
            } else {
                median(&mut win)
            }
        })
        .collect()
}

/// Cross-note robust smoothing. Single-note extraction is noisy — octave
/// errors in bass f0, collapsed two-stage fits (prompt == after → zb at the
/// clamp), B at the clamp floor. But a real piano's parameter curves are
/// SMOOTH functions of note number, so neighbors are evidence: median-filter
/// each parameter in its natural domain (log for B/t60/zb, cents for tuning),
/// treating clamped/collapsed values as missing. Raw measurements stay in the
/// *_ref fields for correlation studies.
pub fn smooth(rows: &mut [WgNote], sr: u32) {
    let n = rows.len();
    if n < 5 {
        return;
    }
    // tuning: cents vs ET; octave-error guard at ±80 c
    let cents: Vec<f32> = rows.iter().map(|r| r.cents_vs_et).collect();
    let bad: Vec<bool> = cents.iter().map(|c| c.abs() > 80.0).collect();
    let cents_s = median_filter(&cents, &bad, 2);

    // B in log domain; clamp-floor and huge values are missing
    let lnb: Vec<f32> = rows.iter().map(|r| r.b.ln()).collect();
    let bad_b: Vec<bool> = rows.iter().map(|r| r.b <= 2e-6 || r.b >= 2e-2).collect();
    let lnb_s = median_filter(&lnb, &bad_b, 3);

    // t60 / zb in log domain; collapsed fits (prompt≈after or zb at clamp) missing
    let lnt: Vec<f32> = rows.iter().map(|r| r.t60.ln()).collect();
    let bad_t: Vec<bool> = rows
        .iter()
        .map(|r| (r.prompt_ref - r.after_ref).abs() < 0.05 * r.after_ref || r.t60 >= 119.0)
        .collect();
    let lnt_s = median_filter(&lnt, &bad_t, 3);
    let lnz: Vec<f32> = rows.iter().map(|r| r.zb.ln()).collect();
    let bad_z: Vec<bool> = rows
        .iter()
        .zip(&bad_t)
        .map(|(r, &bt)| bt || r.zb >= 49_000.0 || r.zb <= 11.0)
        .collect();
    let lnz_s = median_filter(&lnz, &bad_z, 3);

    for (i, r) in rows.iter_mut().enumerate() {
        r.cents_vs_et = cents_s[i];
        r.f0 = crate::analyze::midi_hz(r.note) * 2f32.powf(cents_s[i] / 1200.0);
        r.b = lnb_s[i].exp();
        r.t60 = lnt_s[i].exp().clamp(2.0, 120.0);
        r.zb = lnz_s[i].exp().clamp(20.0, 20_000.0);
        r.n_disp = pick_n_disp(r.f0, r.b, r.brightness, sr);
    }
}

/// Smallest dispersion-cascade size whose designed coefficient stays inside
/// the usable range (|a| well below the bisection clamp at 0.6).
pub fn pick_n_disp(f0: f32, b: f32, brightness: f32, sr: u32) -> usize {
    let d = (1.0 - brightness).clamp(0.0, 0.98) * 0.5;
    for m in [4usize, 8, 12, 16, 24, 32, 48, 64] {
        let des = waveguide::design_loop(f0 as f64, b as f64, m, d as f64, sr as f64);
        if des.disp_a > -0.5 {
            return m;
        }
    }
    64
}
