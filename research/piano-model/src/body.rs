//! Soundboard / body radiation filter, designed FROM DATA: the smoothed
//! log-spectral envelope ratio between a reference recording and the raw
//! bridge output. The strings' waveguide gives the right partials + decay;
//! the body filter supplies the radiation coloration (soundboard modes, rim,
//! lid, mic chain) that raw bridge velocity lacks. Commuted-synthesis idea
//! (Smith PASP): the body is LTI, so it can be a fixed post filter.

use rustfft::{num_complex::Complex, FftPlanner};

use crate::analyze;

/// Design an FIR body filter so `model`'s average spectrum matches `real`'s.
/// Octave-fraction smoothing keeps it an ENVELOPE correction (body/mic), not
/// a per-partial cheat; gain is clamped to ±18 dB.
pub fn design_fir(model: &[f32], real: &[f32], sr: u32, taps: usize) -> Vec<f32> {
    let mm = analyze::avg_mag(model, sr, 3.0);
    let mr = analyze::avg_mag(real, sr, 3.0);
    let nb = mm.len().min(mr.len());

    // log-magnitude ratio, then 1/6-octave smoothing
    let ratio: Vec<f32> = (0..nb)
        .map(|i| ((mr[i].max(1e-9)) / (mm[i].max(1e-9))).ln())
        .collect();
    let mut smooth = vec![0.0f32; nb];
    for i in 0..nb {
        let half = ((i as f32) * (2f32.powf(1.0 / 12.0) - 1.0)).max(2.0) as usize;
        let a = i.saturating_sub(half);
        let b = (i + half + 1).min(nb);
        smooth[i] = ratio[a..b].iter().sum::<f32>() / (b - a) as f32;
    }
    let clamp = 18.0 / 8.686; // ±18 dB in nepers
    for s in &mut smooth {
        *s = s.clamp(-clamp, clamp);
    }

    // build a zero-phase spectrum on an N grid and IFFT → linear-phase FIR
    let n = (taps.next_power_of_two() * 4).max(2048);
    let mut spec = vec![Complex::new(0.0f32, 0.0); n];
    for (k, s) in spec.iter_mut().enumerate().take(n / 2 + 1) {
        // map design bin k → analysis bin
        let src = (k as f32 * nb as f32 / (n as f32 / 2.0)) as usize;
        let g = smooth[src.min(nb - 1)].exp();
        *s = Complex::new(g, 0.0);
    }
    for k in 1..n / 2 {
        spec[n - k] = spec[k].conj();
    }
    let mut planner = FftPlanner::new();
    planner.plan_fft_inverse(n).process(&mut spec);

    // circular-shift the zero-phase impulse to the center, Hann-window to taps
    let mut fir = vec![0.0f32; taps];
    let half = taps / 2;
    for (i, f) in fir.iter_mut().enumerate() {
        let src = (i + n - half) % n;
        let w = 0.5 - 0.5 * (std::f32::consts::TAU * i as f32 / taps as f32).cos();
        *f = spec[src].re / n as f32 * w;
    }
    fir
}

/// Convolve (direct form — offline tool use; the realtime voice will use a
/// partitioned/biquad version).
pub fn apply_fir(x: &[f32], fir: &[f32]) -> Vec<f32> {
    let half = fir.len() / 2;
    let mut y = vec![0.0f32; x.len()];
    for (i, yi) in y.iter_mut().enumerate() {
        let mut acc = 0.0f32;
        for (j, &h) in fir.iter().enumerate() {
            let k = i + half;
            if k >= j {
                if let Some(&xv) = x.get(k - j) {
                    acc += h * xv;
                }
            }
        }
        *yi = acc;
    }
    y
}
