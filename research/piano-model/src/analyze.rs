//! Per-note spectral analysis: fundamental, partial series, inharmonicity
//! coefficient B, and a decay estimate. This is the physical-parameter
//! extraction stage — it turns a struck-note recording into the numbers a
//! modal/waveguide string model is tuned to.

use std::sync::Arc;

use rustfft::{num_complex::Complex, Fft, FftPlanner};
use serde::{Deserialize, Serialize};

/// One modal component of a struck note: a damped sinusoid with a two-stage
/// (double-exponential) decay envelope, the piano's signature prompt-sound /
/// aftersound behavior arising from coupled strings at the bridge:
///
///   env(t) = amp * ( mix * exp(-decay_fast t) + (1-mix) * exp(-decay_slow t) )
///
/// `mix` is the fast ("prompt") fraction. When a partial shows only a single
/// decay, decay_fast == decay_slow and the two terms collapse.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Partial {
    pub k: u32,
    pub freq: f32,
    /// Initial linear amplitude at onset (t=0).
    pub amp: f32,
    /// Fast ("prompt sound") decay rate, nepers/s.
    pub decay_fast: f32,
    /// Slow ("aftersound") decay rate, nepers/s.
    pub decay_slow: f32,
    /// Fraction of initial amplitude in the fast component (0..1).
    pub mix: f32,
}

/// Equal-tempered frequency of a MIDI note (A4=69=440Hz).
pub fn midi_hz(note: u8) -> f32 {
    440.0 * 2f32.powf((note as f32 - 69.0) / 12.0)
}

pub struct NoteAnalysis {
    pub sr: u32,
    pub f0: f32,
    /// Measured frequency of each partial k=1..=n (Hz).
    pub partials: Vec<f32>,
    /// Inharmonicity coefficient B from f_k = k*f0*sqrt(1 + B*k^2).
    pub inharmonicity_b: f32,
    /// Rough -60 dB decay time of the total energy envelope (s).
    pub decay_t60: f32,
    /// Peak RMS (linear) — a proxy for the velocity→loudness curve.
    pub peak_rms: f32,
    /// Modal components (freq, amp, decay) — the resynthesis parameters.
    pub modal: Vec<Partial>,
    /// Stochastic residual (the broadband body left after the sinusoids).
    pub residual: Residual,
}

fn hann(n: usize) -> Vec<f32> {
    (0..n)
        .map(|i| {
            let x = std::f32::consts::PI * i as f32 / (n as f32 - 1.0);
            x.sin().powi(2)
        })
        .collect()
}

/// Magnitude spectrum of `frame` (already windowed length = fft size).
fn mag_spectrum(fft: &Arc<dyn Fft<f32>>, frame: &[f32]) -> Vec<f32> {
    let n = frame.len();
    let mut buf: Vec<Complex<f32>> = frame.iter().map(|&x| Complex::new(x, 0.0)).collect();
    fft.process(&mut buf);
    buf[..n / 2].iter().map(|c| c.norm()).collect()
}

/// Parabolic-interpolated peak frequency near bin `k`.
fn refine_peak(mag: &[f32], k: usize, sr: u32, n: usize) -> f32 {
    if k == 0 || k + 1 >= mag.len() {
        return k as f32 * sr as f32 / n as f32;
    }
    let (a, b, c) = (mag[k - 1], mag[k], mag[k + 1]);
    let denom = a - 2.0 * b + c;
    let delta = if denom.abs() > 1e-12 {
        0.5 * (a - c) / denom
    } else {
        0.0
    };
    (k as f32 + delta) * sr as f32 / n as f32
}

const FFT_SIZE: usize = 32768;

pub fn analyze_note(samples: &[f32], sr: u32, expected_f0: f32, n_partials: usize) -> NoteAnalysis {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(FFT_SIZE);
    let win = hann(FFT_SIZE.min(samples.len()));

    // Analysis frame taken shortly after onset (skip the hammer transient),
    // over the sustaining body of the note.
    let onset_skip = (sr as usize / 20).min(samples.len().saturating_sub(1)); // ~50ms
    let start = onset_skip;
    let len = win.len().min(samples.len() - start);
    let mut frame = vec![0.0f32; FFT_SIZE];
    for i in 0..len {
        frame[i] = samples[start + i] * win[i];
    }
    let mag = mag_spectrum(&fft, &frame);

    // Locate partials. A stiff piano string is inharmonic:
    //   f_k = k*f0*sqrt(1 + B*k^2)
    // so high partials run sharp — the higher k, the sharper. A fixed
    // percentage window clips them and biases B low. Instead iterate: start
    // near the equal-tempered series, fit (f0, B), predict each partial's
    // stretched centre, and re-search a tight window there.
    let bin_hz = sr as f32 / FFT_SIZE as f32;
    let nyq = sr as f32 / 2.0;

    // Asymmetric search: a stiff string only ever runs SHARP of the harmonic,
    // by an amount that grows with k. Search from just-below the equal-tempered
    // harmonic up to the B-predicted stretched position (plus margin), and take
    // the strongest peak in that band. B is fit only on the low-to-mid partials
    // where the single struck string dominates and picks are unambiguous;
    // higher partials (string-pair beating, phantom partials, soundboard modes)
    // are reported but excluded from the fit.
    let pick_band = |lo_hz: f32, hi_hz: f32| -> Option<f32> {
        if lo_hz <= 0.0 || hi_hz >= nyq || lo_hz >= hi_hz {
            return None;
        }
        let lo = (lo_hz / bin_hz) as usize;
        let hi = ((hi_hz / bin_hz) as usize + 1).min(mag.len() - 2);
        if lo >= hi {
            return None;
        }
        let (mut best, mut bestv) = (lo, 0.0f32);
        for b in lo..=hi {
            if mag[b] > bestv {
                bestv = mag[b];
                best = b;
            }
        }
        Some(refine_peak(&mag, best, sr, FFT_SIZE))
    };

    const FIT_PARTIALS: usize = 12; // reliable range for the B fit

    let mut f0 = expected_f0;
    let mut b_est = 0.0f32;
    let mut partials = Vec::new();
    for _ in 0..4 {
        partials.clear();
        for k in 1..=n_partials {
            let kf = k as f32;
            let harmonic = f0 * kf;
            let stretched = harmonic * (1.0 + b_est * (kf * kf)).sqrt();
            // band: from 0.5% below the plain harmonic to 1% above the
            // predicted stretched position (widens automatically with B and k).
            let lo = harmonic * 0.995;
            let hi = (stretched * 1.01).max(harmonic * 1.005);
            match pick_band(lo, hi) {
                Some(f) => partials.push(f),
                None => break,
            }
        }
        f0 = partials.first().copied().unwrap_or(expected_f0);
        let fit_n = partials.len().min(FIT_PARTIALS);
        b_est = fit_inharmonicity(&partials[..fit_n], f0);
    }
    let inharmonicity_b = b_est;
    let decay_t60 = estimate_t60(samples, sr);
    let peak_rms = peak_rms(samples, sr);
    let modal = extract_modal(samples, sr, &partials);
    let residual = extract_residual(samples, sr, &partials);

    NoteAnalysis {
        sr,
        f0,
        partials,
        inharmonicity_b,
        decay_t60,
        peak_rms,
        modal,
        residual,
    }
}

const STFT_SIZE: usize = 8192;
const STFT_HOP: usize = 2048;

/// Track each partial's amplitude envelope over time via STFT, then fit an
/// exponential decay. Produces the modal (freq, amp, decay) triples used for
/// resynthesis.
pub fn extract_modal(samples: &[f32], sr: u32, partial_freqs: &[f32]) -> Vec<Partial> {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(STFT_SIZE);
    let win = hann(STFT_SIZE);
    // Coherent gain of the Hann window: a sinusoid of amplitude A shows a
    // peak-bin magnitude of A * sum(win)/2. Invert that to recover A.
    let wsum: f32 = win.iter().sum();

    let bin_hz = sr as f32 / STFT_SIZE as f32;

    // magnitude track per partial across frames
    let n_p = partial_freqs.len();
    let mut tracks: Vec<Vec<(f32, f32)>> = vec![Vec::new(); n_p]; // (time_s, amp)

    let mut pos = 0usize;
    let mut frame = vec![0.0f32; STFT_SIZE];
    while pos + STFT_SIZE <= samples.len() {
        for i in 0..STFT_SIZE {
            frame[i] = samples[pos + i] * win[i];
        }
        let mag = mag_spectrum(&fft, &frame);
        let t = pos as f32 / sr as f32;
        for (p, &f) in partial_freqs.iter().enumerate() {
            let center = (f / bin_hz).round() as usize;
            if center == 0 || center + 1 >= mag.len() {
                continue;
            }
            // strongest of the nearest few bins (partial may sit between bins)
            let lo = center.saturating_sub(1);
            let hi = (center + 1).min(mag.len() - 1);
            let peak = mag[lo..=hi].iter().cloned().fold(0.0f32, f32::max);
            let amp = peak * 2.0 / wsum;
            tracks[p].push((t, amp));
        }
        pos += STFT_HOP;
    }

    let mut out = Vec::with_capacity(n_p);
    for (p, track) in tracks.iter().enumerate() {
        if let Some(ts) = fit_two_stage(track) {
            out.push(Partial {
                k: (p + 1) as u32,
                freq: partial_freqs[p],
                amp: ts.amp,
                decay_fast: ts.decay_fast,
                decay_slow: ts.decay_slow,
                mix: ts.mix,
            });
        }
    }
    out
}

struct TwoStage {
    amp: f32,
    decay_fast: f32,
    decay_slow: f32,
    mix: f32,
}

/// Linear regression of ln(amp) vs t over a slice, returning (decay, amp0) with
/// amp0 the t=0 intercept. `t` is measured relative to the slice's own origin.
fn log_fit(pts: &[(f32, f32)]) -> Option<(f32, f32)> {
    let (mut sx, mut sy, mut sxx, mut sxy, mut n) = (0.0f64, 0.0f64, 0.0f64, 0.0f64, 0.0f64);
    for &(t, a) in pts {
        if a <= 0.0 {
            continue;
        }
        let (x, y) = (t as f64, (a as f64).ln());
        sx += x;
        sy += y;
        sxx += x * x;
        sxy += x * y;
        n += 1.0;
    }
    if n < 3.0 {
        return None;
    }
    let denom = n * sxx - sx * sx;
    if denom.abs() < 1e-12 {
        return None;
    }
    let slope = (n * sxy - sx * sy) / denom;
    let intercept = (sy - slope * sx) / n;
    let decay = (-slope).max(0.0) as f32;
    let amp0 = intercept.exp() as f32;
    Some((decay, amp0))
}

/// Fit a two-stage (double-exponential) decay by exponential peeling:
/// fit the slow aftersound from the tail, subtract it, fit the fast prompt
/// component from the early residual. Falls back to single-exponential when the
/// data doesn't support a distinct fast component.
fn fit_two_stage(track: &[(f32, f32)]) -> Option<TwoStage> {
    if track.len() < 6 {
        return None;
    }
    let (peak_i, &(peak_t, peak_amp)) = track
        .iter()
        .enumerate()
        .max_by(|a, b| a.1 .1.partial_cmp(&b.1 .1).unwrap())?;
    if peak_amp <= 0.0 {
        return None;
    }
    let floor = peak_amp * 1e-3; // -60 dB

    // Envelope from the peak, times relative to peak, above the floor.
    let env: Vec<(f32, f32)> = track[peak_i..]
        .iter()
        .take_while(|&&(_, a)| a >= floor)
        .map(|&(t, a)| (t - peak_t, a))
        .collect();
    if env.len() < 6 {
        return None;
    }

    // Single-exponential fit as the fallback / baseline.
    let (decay1, amp1) = log_fit(&env)?;

    // Slow component: fit the later 60% of the envelope.
    let split = env.len() * 2 / 5;
    let tail = &env[split..];
    let slow = log_fit(tail);

    if let Some((decay_slow, amp_slow_local)) = slow {
        // Re-reference the slow amplitude to t=0 (peak).
        let t_split = env[split].0;
        let amp_slow0 = amp_slow_local * (-decay_slow * (0.0 - t_split)).exp();
        // Residual early portion = envelope minus the slow component.
        let resid: Vec<(f32, f32)> = env[..split]
            .iter()
            .map(|&(t, a)| (t, a - amp_slow0 * (-decay_slow * t).exp()))
            .filter(|&(_, r)| r > floor)
            .collect();
        if resid.len() >= 3 {
            if let Some((decay_fast, amp_fast0)) = log_fit(&resid) {
                // Accept only a genuinely faster, non-trivial prompt component.
                if decay_fast > decay_slow * 1.3 && amp_fast0 > 0.0 {
                    let amp = amp_fast0 + amp_slow0;
                    let mix = (amp_fast0 / amp).clamp(0.0, 1.0);
                    if amp.is_finite() && decay_fast.is_finite() && decay_slow.is_finite() {
                        return Some(TwoStage {
                            amp,
                            decay_fast,
                            decay_slow,
                            mix,
                        });
                    }
                }
            }
        }
    }

    // Single-stage fallback: both rates equal.
    if !amp1.is_finite() || !decay1.is_finite() {
        return None;
    }
    Some(TwoStage {
        amp: amp1,
        decay_fast: decay1,
        decay_slow: decay1,
        mix: 1.0,
    })
}

/// Least-squares fit of B in f_k^2 = (k f0)^2 (1 + B k^2).
/// Linearize: (f_k/(k f0))^2 - 1 = B k^2  → slope through origin.
fn fit_inharmonicity(partials: &[f32], f0: f32) -> f32 {
    let (mut num, mut den) = (0.0f64, 0.0f64);
    for (i, &fk) in partials.iter().enumerate() {
        let k = (i + 1) as f64;
        let ratio = (fk as f64 / (k * f0 as f64)).powi(2) - 1.0;
        let x = k * k;
        num += x * ratio;
        den += x * x;
    }
    if den > 0.0 {
        (num / den) as f32
    } else {
        0.0
    }
}

/// RMS energy envelope → time to fall 60 dB from peak.
fn estimate_t60(samples: &[f32], sr: u32) -> f32 {
    let win = (sr as usize / 100).max(1); // 10ms frames
    let mut env = Vec::new();
    let mut i = 0;
    while i + win <= samples.len() {
        let e: f32 = samples[i..i + win].iter().map(|x| x * x).sum::<f32>() / win as f32;
        env.push(e.sqrt());
        i += win;
    }
    if env.is_empty() {
        return 0.0;
    }
    let (peak_idx, &peak) = env
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
        .unwrap();
    if peak <= 0.0 {
        return 0.0;
    }
    let thresh = peak * 10f32.powf(-60.0 / 20.0); // -60 dB
    for j in peak_idx..env.len() {
        if env[j] <= thresh {
            return (j - peak_idx) as f32 * win as f32 / sr as f32;
        }
    }
    // never reached threshold within the sample
    (env.len() - peak_idx) as f32 * win as f32 / sr as f32
}

/// Number of log-spaced bands the stochastic residual is modeled in.
pub const RES_BANDS: usize = 40;

/// The stochastic (noise) residual of a note — the part left after the
/// deterministic sinusoids are removed. This is the broadband "body" (soundboard
/// air, hammer noise, room) that pure modal synthesis can't produce. Modeled as
/// a band-energy spectral shape with an overall temporal decay: at playback,
/// white noise is shaped to `band_gain` and enveloped by `level*exp(-decay*t)`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Residual {
    /// Per-band linear gain (spectral shape of the residual). Length RES_BANDS.
    pub band_gain: Vec<f32>,
    /// Band centre frequencies (Hz), for the synthesis filterbank.
    pub band_hz: Vec<f32>,
    /// Residual energy decay rate (nepers/s).
    pub decay: f32,
    /// Initial residual RMS level (sample amplitude units).
    pub level: f32,
}

/// Log-spaced band edges from `lo` to `hi` (RES_BANDS bands → RES_BANDS+1 edges).
fn res_band_edges(lo: f32, hi: f32) -> Vec<f32> {
    (0..=RES_BANDS)
        .map(|i| lo * (hi / lo).powf(i as f32 / RES_BANDS as f32))
        .collect()
}

/// Model the hammer/attack noise from an ISOLATED attack window: its spectral
/// shape (band gains, from a single windowed FFT) + level (RMS) + a fast decay
/// (exp fit of the window's RMS envelope). Rendered as a filtered-noise burst —
/// the magnitude envelope IS the fit (noise has no phase to match), so it
/// transfers exactly. This is the "attack" component, measured per note.
pub fn attack_noise_model(window: &[f32], sr: u32, partial_freqs: &[f32]) -> Residual {
    let fft_size = 4096usize;
    let nyq = sr as f32 / 2.0;
    let hi = nyq.min(16_000.0);
    let edges = res_band_edges(50.0, hi);
    let band_hz: Vec<f32> = (0..RES_BANDS)
        .map(|b| (edges[b] * edges[b + 1]).sqrt())
        .collect();

    let n = window.len().min(fft_size);
    let win = hann(fft_size);
    let mut frame = vec![0.0f32; fft_size];
    for i in 0..n {
        frame[i] = window[i] * win[i];
    }
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);
    let mag = mag_spectrum(&fft, &frame);
    let bin_hz = sr as f32 / fft_size as f32;

    // Mask the partial bins so band_gain is the BROADBAND strike (partials are
    // handled by the harmonic model). Guard scales with the fundamental.
    let f0 = partial_freqs.first().copied().unwrap_or(100.0);
    let guard = ((0.4 * f0 / bin_hz).round() as i32).clamp(2, 10);
    let mut is_partial = vec![false; mag.len()];
    for &f in partial_freqs {
        let c = (f / bin_hz).round() as i32;
        for d in -guard..=guard {
            let j = c + d;
            if j >= 0 && (j as usize) < mag.len() {
                is_partial[j as usize] = true;
            }
        }
    }

    let mut band_e = vec![0.0f64; RES_BANDS];
    let mut bb_energy = 0.0f64;
    for (bin, &m) in mag.iter().enumerate() {
        if is_partial[bin] {
            continue;
        }
        let f = bin as f32 * bin_hz;
        if f < edges[0] || f >= hi {
            continue;
        }
        let mut b = 0;
        while b + 1 < RES_BANDS && f >= edges[b + 1] {
            b += 1;
        }
        band_e[b] += (m * m) as f64;
        bb_energy += (m * m) as f64;
    }
    let mut band_gain: Vec<f32> = band_e.iter().map(|&e| e.sqrt() as f32).collect();
    let peak = band_gain.iter().cloned().fold(0.0f32, f32::max);
    if peak > 0.0 {
        for g in &mut band_gain {
            *g /= peak;
        }
    }

    // Broadband strike level (time-domain RMS via Parseval, Hann ms=0.375),
    // NOT the full-window RMS — excludes the sustaining partials.
    let level = ((2.0 * bb_energy).sqrt() / (fft_size as f64 * 0.612372)) as f32;
    // Force a genuinely fast burst: die to ~-40 dB by the end of the window.
    let dt = (window.len() as f32 / sr as f32).max(2e-3);
    let decay = (4.0f32.ln() * 10.0 / dt).clamp(40.0, 4000.0); // T60 ~= window

    Residual {
        band_gain,
        band_hz,
        decay,
        level,
    }
}

/// Extract the stochastic residual: STFT the note, null out the bins at every
/// partial (the deterministic/sinusoidal part), and measure what remains as a
/// per-band energy shape plus an overall temporal decay.
pub fn extract_residual(samples: &[f32], sr: u32, partial_freqs: &[f32]) -> Residual {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(STFT_SIZE);
    let win = hann(STFT_SIZE);
    let bin_hz = sr as f32 / STFT_SIZE as f32;
    let nyq = sr as f32 / 2.0;
    let hi = nyq.min(16_000.0);
    let edges = res_band_edges(50.0, hi);
    let band_hz: Vec<f32> = (0..RES_BANDS)
        .map(|b| (edges[b] * edges[b + 1]).sqrt())
        .collect();

    // Guard half-width (bins) to null around each partial. It must scale to the
    // partial SPACING (≈ f0): a fixed wide guard covers the whole inter-partial
    // gap in the dense bass (masking the residual away), while the treble needs
    // a wide guard to reject the strong partials' skirts. ~35% of the spacing,
    // clamped to [2, 8] bins.
    let f0 = partial_freqs.first().copied().unwrap_or(100.0);
    let guard = ((0.35 * f0 / bin_hz).round() as i32).clamp(2, 8);
    let mut is_partial = vec![false; STFT_SIZE / 2];
    for &f in partial_freqs {
        let c = (f / bin_hz).round() as i32;
        for d in -guard..=guard {
            let j = c + d;
            if j >= 0 && (j as usize) < is_partial.len() {
                is_partial[j as usize] = true;
            }
        }
    }

    // per-frame residual band energy + total residual energy (for the envelope)
    let mut band_accum = vec![0.0f64; RES_BANDS];
    let mut accum_frames = 0usize;
    let mut env: Vec<f32> = Vec::new();

    let mut pos = 0usize;
    let mut frame = vec![0.0f32; STFT_SIZE];
    // aggregate the band SHAPE over the first ~250 ms (attack + early body)
    let shape_frames = ((0.25 * sr as f32) / STFT_HOP as f32).ceil() as usize;

    while pos + STFT_SIZE <= samples.len() {
        for i in 0..STFT_SIZE {
            frame[i] = samples[pos + i] * win[i];
        }
        let mag = mag_spectrum(&fft, &frame);
        // integrate residual (non-partial bins) into bands
        let mut frame_energy = 0.0f64;
        let mut fi = 0usize; // frame band accumulation
        let mut bands = vec![0.0f64; RES_BANDS];
        for (bin, &m) in mag.iter().enumerate() {
            if is_partial[bin] {
                continue;
            }
            let f = bin as f32 * bin_hz;
            if f < edges[0] || f >= hi {
                continue;
            }
            // find band (linear scan is fine; bins are ordered)
            while fi + 1 < RES_BANDS && f >= edges[fi + 1] {
                fi += 1;
            }
            let e = (m * m) as f64;
            bands[fi] += e;
            frame_energy += e;
        }
        // Convert half-spectrum residual energy to a time-domain RMS
        // (Parseval, Hann mean-square = 0.375, unnormalized rustfft):
        //   sum_half|X|² = 0.5·N·sum(xw)² = 0.5·N·(RMS²·N·0.375)
        //   → RMS = sqrt(2·E) / (N·sqrt(0.375))
        let rms = ((2.0 * frame_energy).sqrt() / (STFT_SIZE as f64 * 0.612372)) as f32;
        env.push(rms);
        if accum_frames < shape_frames {
            for b in 0..RES_BANDS {
                band_accum[b] += bands[b];
            }
            accum_frames += 1;
        }
        pos += STFT_HOP;
    }

    // band shape → linear gains (sqrt of energy), normalized to unit peak
    let mut band_gain: Vec<f32> = band_accum.iter().map(|&e| (e.max(0.0)).sqrt() as f32).collect();
    let peak = band_gain.iter().cloned().fold(0.0f32, f32::max);
    if peak > 0.0 {
        for g in &mut band_gain {
            *g /= peak;
        }
    }

    // temporal envelope of the residual: level at onset + decay rate
    let (level, decay) = fit_residual_envelope(&env, sr);

    Residual {
        band_gain,
        band_hz,
        decay,
        level,
    }
}

/// Fit residual RMS envelope to level*exp(-decay*t). `level` is the residual
/// RMS averaged over the first ~1 s from the peak (a robust sustained level, not
/// the attack spike — so a single synthesis gain matches across the keyboard);
/// `decay` is fit over the tail.
fn fit_residual_envelope(env: &[f32], sr: u32) -> (f32, f32) {
    if env.len() < 4 {
        return (0.0, 1.0);
    }
    let dt = STFT_HOP as f32 / sr as f32;
    let (peak_i, &peak) = env
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
        .unwrap();
    if peak <= 0.0 {
        return (0.0, 1.0);
    }
    // sustained level: RMS-average of env over the first ~1 s from the peak
    let win = ((1.0 * sr as f32) / STFT_HOP as f32).ceil() as usize;
    let end = (peak_i + win).min(env.len());
    let level = {
        let slice = &env[peak_i..end];
        (slice.iter().map(|x| x * x).sum::<f32>() / slice.len().max(1) as f32).sqrt()
    };
    let floor = peak * 1e-3;
    let (mut sx, mut sy, mut sxx, mut sxy, mut n) = (0.0f64, 0.0f64, 0.0f64, 0.0f64, 0.0f64);
    for (j, &a) in env[peak_i..].iter().enumerate() {
        if a < floor {
            break;
        }
        let (x, y) = ((j as f32 * dt) as f64, (a as f64).ln());
        sx += x;
        sy += y;
        sxx += x * x;
        sxy += x * y;
        n += 1.0;
    }
    if n < 3.0 {
        return (level, 1.0);
    }
    let denom = n * sxx - sx * sx;
    let slope = if denom.abs() > 1e-12 {
        (n * sxy - sx * sy) / denom
    } else {
        0.0
    };
    let decay = (-slope).max(0.0) as f32;
    (level, decay)
}

/// Long-term log-magnitude spectrum of the first `secs` seconds, for A/B
/// spectral comparison. Returns dB-ish values (log of magnitude).
pub fn logmag(samples: &[f32], sr: u32, secs: f32) -> Vec<f32> {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(FFT_SIZE);
    let win = hann(FFT_SIZE);
    let n = ((secs * sr as f32) as usize).min(samples.len());
    // average magnitude across overlapping frames in the window
    let hop = FFT_SIZE / 2;
    let mut acc = vec![0.0f32; FFT_SIZE / 2];
    let mut frames = 0usize;
    let mut pos = 0usize;
    let mut frame = vec![0.0f32; FFT_SIZE];
    while pos + FFT_SIZE <= n.max(FFT_SIZE) && pos + FFT_SIZE <= samples.len() {
        for i in 0..FFT_SIZE {
            frame[i] = samples[pos + i] * win[i];
        }
        let mag = mag_spectrum(&fft, &frame);
        for (a, m) in acc.iter_mut().zip(mag.iter()) {
            *a += m;
        }
        frames += 1;
        pos += hop;
    }
    if frames == 0 {
        return acc;
    }
    for a in acc.iter_mut() {
        *a = (*a / frames as f32 + 1e-9).ln();
    }
    acc
}

/// Linear averaged magnitude spectrum over the first `secs` seconds.
pub fn avg_mag(samples: &[f32], sr: u32, secs: f32) -> Vec<f32> {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(FFT_SIZE);
    let win = hann(FFT_SIZE);
    let n = ((secs * sr as f32) as usize).min(samples.len());
    let hop = FFT_SIZE / 2;
    let mut acc = vec![0.0f32; FFT_SIZE / 2];
    let mut frames = 0usize;
    let mut pos = 0usize;
    let mut frame = vec![0.0f32; FFT_SIZE];
    while pos + FFT_SIZE <= samples.len() && pos < n.max(1) {
        for i in 0..FFT_SIZE {
            frame[i] = samples[pos + i] * win[i];
        }
        let mag = mag_spectrum(&fft, &frame);
        for (a, m) in acc.iter_mut().zip(mag.iter()) {
            *a += m;
        }
        frames += 1;
        pos += hop;
    }
    if frames > 0 {
        for a in acc.iter_mut() {
            *a /= frames as f32;
        }
    }
    acc
}

/// Decompose model↔real spectral agreement into a harmonic part and a broadband
/// part. Returns (harmonic_cos, broadband_ratio):
///   - harmonic_cos: cosine of the log-magnitudes sampled AT the partial
///     frequencies — how faithful the harmonic balance is (1.0 = perfect).
///   - broadband_ratio: real inter-partial energy / model inter-partial energy
///     (bins away from every partial). >1 means the model is too "thin" between
///     partials (missing soundboard/body/noise); ~1 means we match the body.
pub fn spectral_diag(
    model: &[f32],
    real: &[f32],
    sr: u32,
    freqs: &[f32],
    secs: f32,
) -> (f32, f32) {
    let mm = avg_mag(model, sr, secs);
    let rm = avg_mag(real, sr, secs);
    let bin_hz = sr as f32 / FFT_SIZE as f32;
    let nbins = mm.len().min(rm.len());

    // Harmonic bins (nearest bin to each partial) and a ±guard mask around them.
    let mut is_harmonic = vec![false; nbins];
    let mut hm = Vec::new();
    let mut hr = Vec::new();
    for &f in freqs {
        let b = (f / bin_hz).round() as usize;
        if b == 0 || b >= nbins {
            continue;
        }
        hm.push((mm[b] + 1e-9).ln());
        hr.push((rm[b] + 1e-9).ln());
        for d in -3i32..=3 {
            let j = b as i32 + d;
            if j >= 0 && (j as usize) < nbins {
                is_harmonic[j as usize] = true;
            }
        }
    }
    let harmonic_cos = cosine(&hm, &hr);

    // Broadband energy = sum of magnitude² in non-harmonic bins.
    let mut me = 0.0f64;
    let mut re = 0.0f64;
    for b in 0..nbins {
        if !is_harmonic[b] {
            me += (mm[b] as f64).powi(2);
            re += (rm[b] as f64).powi(2);
        }
    }
    let broadband_ratio = if me > 1e-12 { (re / me) as f32 } else { f32::INFINITY };
    (harmonic_cos, broadband_ratio)
}

/// Total inter-partial (broadband) energy of a signal over the first `secs`
/// seconds — magnitude² summed over bins away from every partial. Used to
/// self-calibrate the residual level per note so it matches the real recording.
pub fn broadband_energy(samples: &[f32], sr: u32, freqs: &[f32], secs: f32) -> f64 {
    let mag = avg_mag(samples, sr, secs);
    let bin_hz = sr as f32 / FFT_SIZE as f32;
    let n = mag.len();
    let mut is_h = vec![false; n];
    for &f in freqs {
        let b = (f / bin_hz).round() as i32;
        for d in -3i32..=3 {
            let j = b + d;
            if j >= 0 && (j as usize) < n {
                is_h[j as usize] = true;
            }
        }
    }
    (0..n)
        .filter(|&b| !is_h[b])
        .map(|b| (mag[b] as f64).powi(2))
        .sum()
}

/// RMS envelope (10 ms frames).
pub fn envelope(samples: &[f32], sr: u32) -> Vec<f32> {
    let win = (sr as usize / 100).max(1);
    let mut env = Vec::new();
    let mut i = 0;
    while i + win <= samples.len() {
        let e: f32 = samples[i..i + win].iter().map(|x| x * x).sum::<f32>() / win as f32;
        env.push(e.sqrt());
        i += win;
    }
    env
}

/// Onset index: first sample exceeding 1% of peak.
fn onset(samples: &[f32]) -> usize {
    let peak = samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
    let thr = peak * 0.01;
    samples.iter().position(|x| x.abs() > thr).unwrap_or(0)
}

/// Log-spectral distance (dB) between two signals at one FFT resolution:
/// RMS over time-frames and frequency-bins of the 20·log10 magnitude
/// difference. 0 dB = identical magnitude spectrogram.
fn lsd_at(a: &[f32], b: &[f32], sr: u32, fft_size: usize) -> f32 {
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);
    let win = hann(fft_size);
    let hop = fft_size / 4;
    let frames = ((a.len().min(b.len())).saturating_sub(fft_size)) / hop;
    if frames == 0 {
        return 0.0;
    }
    let mut fa = vec![0.0f32; fft_size];
    let mut fb = vec![0.0f32; fft_size];
    let mut las: Vec<f32> = Vec::new();
    let mut lbs: Vec<f32> = Vec::new();
    for f in 0..frames {
        let off = f * hop;
        for i in 0..fft_size {
            fa[i] = a[off + i] * win[i];
            fb[i] = b[off + i] * win[i];
        }
        let ma = mag_spectrum(&fft, &fa);
        let mb = mag_spectrum(&fft, &fb);
        for k in 0..ma.len() {
            las.push(20.0 * ma[k].max(1e-12).log10());
            lbs.push(20.0 * mb[k].max(1e-12).log10());
        }
    }
    if lbs.is_empty() {
        return 0.0;
    }
    // Floor at the REFERENCE's own noise floor: a recording carries mic/room
    // noise the model rightly does not reproduce (Pianoteq's broadband is
    // ~100× below Keyscape's — see the handoff §2.2). Grade only what is
    // above the recording's noise: floor = the 20th percentile of the ref's
    // log-magnitudes (a decaying note spends its tail near the noise floor,
    // so a low percentile tracks it), clamped to [-80, -35] dB.
    let mut sorted = lbs.clone();
    sorted.sort_by(|x, y| x.partial_cmp(y).unwrap());
    let floor = (sorted[sorted.len() / 5] as f64).clamp(-80.0, -35.0);
    let (mut acc, mut n) = (0.0f64, 0.0f64);
    for (la, lb) in las.iter().zip(lbs.iter()) {
        let la = (*la as f64).max(floor);
        let lb = (*lb as f64).max(floor);
        let d = la - lb;
        acc += d * d;
        n += 1.0;
    }
    if n > 0.0 {
        (acc / n).sqrt() as f32
    } else {
        0.0
    }
}

/// **Definitive accuracy score**: multi-resolution log-spectral distance (dB)
/// between model and real, onset-aligned and peak-normalized. Phase-invariant
/// (fair to a physical model's arbitrary partial phases). 0 dB = perfect
/// magnitude-spectrogram match; lower is better. Averaged over three FFT sizes
/// so it captures both broad timbre and fine partial structure.
pub fn accuracy_lsd(model: &[f32], real: &[f32], sr: u32) -> f32 {
    // onset-align
    let om = onset(model);
    let or = onset(real);
    let a = &model[om..];
    let b = &real[or..];
    let len = a.len().min(b.len());
    if len < 16384 {
        return f32::NAN;
    }
    // peak-normalize both
    let mut a = a[..len].to_vec();
    let mut b = b[..len].to_vec();
    let pa = a.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-9);
    let pb = b.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-9);
    for x in &mut a {
        *x /= pa;
    }
    for x in &mut b {
        *x /= pb;
    }
    let sizes = [1024usize, 4096, 16384];
    let sum: f32 = sizes.iter().map(|&s| lsd_at(&a, &b, sr, s)).sum();
    sum / sizes.len() as f32
}

/// Cosine similarity of two equal-length vectors (mean-centered).
pub fn cosine(a: &[f32], b: &[f32]) -> f32 {
    let n = a.len().min(b.len());
    if n == 0 {
        return 0.0;
    }
    let (a, b) = (&a[..n], &b[..n]);
    let ma = a.iter().sum::<f32>() / n as f32;
    let mb = b.iter().sum::<f32>() / n as f32;
    let (mut dot, mut na, mut nb) = (0.0f32, 0.0f32, 0.0f32);
    for i in 0..n {
        let (x, y) = (a[i] - ma, b[i] - mb);
        dot += x * y;
        na += x * x;
        nb += y * y;
    }
    if na <= 0.0 || nb <= 0.0 {
        return 0.0;
    }
    dot / (na.sqrt() * nb.sqrt())
}

fn peak_rms(samples: &[f32], sr: u32) -> f32 {
    let win = (sr as usize / 100).max(1);
    let mut peak = 0.0f32;
    let mut i = 0;
    while i + win <= samples.len() {
        let e: f32 = samples[i..i + win].iter().map(|x| x * x).sum::<f32>() / win as f32;
        peak = peak.max(e.sqrt());
        i += win;
    }
    peak
}
