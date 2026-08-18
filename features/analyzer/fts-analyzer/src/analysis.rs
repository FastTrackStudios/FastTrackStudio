//! Analysis utilities for comparing plugin behavior.

use rustfft::FftPlanner;
use rustfft::num_complex::Complex;

/// Per-sample difference between two signals.
pub fn diff(a: &[f32], b: &[f32]) -> Vec<f32> {
    a.iter().zip(b.iter()).map(|(x, y)| x - y).collect()
}

/// RMS level of a signal.
pub fn rms(signal: &[f32]) -> f32 {
    if signal.is_empty() {
        return 0.0;
    }
    let sum_sq: f64 = signal.iter().map(|&s| (s as f64) * (s as f64)).sum();
    (sum_sq / signal.len() as f64).sqrt() as f32
}

/// RMS level in dBFS.
pub fn rms_db(signal: &[f32]) -> f32 {
    let r = rms(signal);
    if r <= 0.0 {
        f32::NEG_INFINITY
    } else {
        20.0 * r.log10()
    }
}

/// Peak level of a signal (absolute value).
pub fn peak(signal: &[f32]) -> f32 {
    signal.iter().map(|s| s.abs()).fold(0.0f32, f32::max)
}

/// Peak level in dBFS.
pub fn peak_db(signal: &[f32]) -> f32 {
    let p = peak(signal);
    if p <= 0.0 {
        f32::NEG_INFINITY
    } else {
        20.0 * p.log10()
    }
}

/// Measure gain reduction by comparing input and output RMS in dB.
pub fn gain_reduction_db(input: &[f32], output: &[f32]) -> f32 {
    rms_db(output) - rms_db(input)
}

/// Compute the transfer curve: for each input level, what is the output level?
///
/// Returns a vec of `(input_db, output_db)` pairs.
/// Measures in windows of `window_size` samples.
pub fn transfer_curve(input: &[f32], output: &[f32], window_size: usize) -> Vec<(f32, f32)> {
    input
        .chunks(window_size)
        .zip(output.chunks(window_size))
        .map(|(i_win, o_win)| (rms_db(i_win), rms_db(o_win)))
        .filter(|(i_db, _)| i_db.is_finite())
        .collect()
}

/// Summary comparison between two plugin outputs processed from the same input.
#[derive(Debug)]
pub struct Comparison {
    /// RMS of the difference signal (lower = more similar).
    pub diff_rms_db: f32,
    /// Peak of the difference signal.
    pub diff_peak_db: f32,
    /// RMS of output A.
    pub a_rms_db: f32,
    /// RMS of output B.
    pub b_rms_db: f32,
    /// Gain reduction applied by A.
    pub a_gain_reduction_db: f32,
    /// Gain reduction applied by B.
    pub b_gain_reduction_db: f32,
}

/// Compare two plugin outputs that were processed from the same input signal.
pub fn compare(input: &[f32], output_a: &[f32], output_b: &[f32]) -> Comparison {
    let d = diff(output_a, output_b);
    Comparison {
        diff_rms_db: rms_db(&d),
        diff_peak_db: peak_db(&d),
        a_rms_db: rms_db(output_a),
        b_rms_db: rms_db(output_b),
        a_gain_reduction_db: gain_reduction_db(input, output_a),
        b_gain_reduction_db: gain_reduction_db(input, output_b),
    }
}

// ---------------------------------------------------------------------------
// FFT-based frequency response comparison
// ---------------------------------------------------------------------------

/// Default FFT size for frequency response analysis.
pub const DEFAULT_FFT_SIZE: usize = 4096;

/// Hann window coefficient for index `i` out of `n` samples.
#[inline]
fn hann(i: usize, n: usize) -> f32 {
    0.5 * (1.0 - (2.0 * std::f32::consts::PI * i as f32 / (n - 1) as f32).cos())
}

/// Accumulated spectrum for Welch's method — both power and cross-spectrum.
struct WelchSpectrum {
    /// Sum of |X(k)|² across all windows.
    sum_mag_sq: Vec<f64>,
    /// Number of averaged windows.
    num_averages: usize,
}

impl WelchSpectrum {
    fn new(num_bins: usize) -> Self {
        Self {
            sum_mag_sq: vec![0.0; num_bins],
            num_averages: 0,
        }
    }
}

/// Accumulated cross-spectrum for phase computation.
/// Stores sum of Y(k) * conj(X(k)) to get averaged H(k) = Y/X with phase.
struct CrossSpectrum {
    /// Sum of Y(k) * conj(X(k)) across all windows.
    sum_cross: Vec<(f64, f64)>, // (real, imag)
    /// Sum of |X(k)|² for normalization.
    sum_xx: Vec<f64>,
    num_averages: usize,
}

impl CrossSpectrum {
    fn new(num_bins: usize) -> Self {
        Self {
            sum_cross: vec![(0.0, 0.0); num_bins],
            sum_xx: vec![0.0; num_bins],
            num_averages: 0,
        }
    }
}

/// Compute the averaged power spectrum of a signal using Welch's method
/// (non-overlapping Hann-windowed FFT frames).
fn welch_power_spectrum(signal: &[f32], fft_size: usize) -> WelchSpectrum {
    let num_bins = fft_size / 2;
    let mut spectrum = WelchSpectrum::new(num_bins);
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);

    let mut offset = 0;
    while offset + fft_size <= signal.len() {
        let mut buffer: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(signal[offset + i] * hann(i, fft_size), 0.0))
            .collect();

        fft.process(&mut buffer);

        #[allow(clippy::needless_range_loop)]
        for k in 0..num_bins {
            let mag = buffer[k].norm();
            spectrum.sum_mag_sq[k] += (mag as f64) * (mag as f64);
        }
        spectrum.num_averages += 1;

        offset += fft_size;
    }

    spectrum
}

/// Compute the averaged cross-spectrum of input/output using Welch's method.
/// This gives both magnitude and phase of H(f) = Y(f)/X(f).
fn welch_cross_spectrum(input: &[f32], output: &[f32], fft_size: usize) -> CrossSpectrum {
    let num_bins = fft_size / 2;
    let mut cross = CrossSpectrum::new(num_bins);
    let mut planner = FftPlanner::new();
    let fft = planner.plan_fft_forward(fft_size);

    let mut offset = 0;
    while offset + fft_size <= input.len().min(output.len()) {
        let mut x_buf: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(input[offset + i] * hann(i, fft_size), 0.0))
            .collect();
        let mut y_buf: Vec<Complex<f32>> = (0..fft_size)
            .map(|i| Complex::new(output[offset + i] * hann(i, fft_size), 0.0))
            .collect();

        fft.process(&mut x_buf);
        fft.process(&mut y_buf);

        for k in 0..num_bins {
            // Y * conj(X)
            let yr = y_buf[k].re as f64;
            let yi = y_buf[k].im as f64;
            let xr = x_buf[k].re as f64;
            let xi = x_buf[k].im as f64;
            // conj(X) = (xr, -xi)
            cross.sum_cross[k].0 += yr * xr + yi * xi;
            cross.sum_cross[k].1 += yi * xr - yr * xi;
            cross.sum_xx[k] += xr * xr + xi * xi;
        }
        cross.num_averages += 1;

        offset += fft_size;
    }

    cross
}

/// Compute H(f) in dB from averaged input/output power spectra.
fn transfer_function_db(
    input_spectrum: &WelchSpectrum,
    output_spectrum: &WelchSpectrum,
) -> Vec<f64> {
    let num_bins = input_spectrum.sum_mag_sq.len();
    let in_avg = input_spectrum.num_averages.max(1) as f64;
    let out_avg = output_spectrum.num_averages.max(1) as f64;

    (0..num_bins)
        .map(|k| {
            let mag_in = (input_spectrum.sum_mag_sq[k] / in_avg).sqrt();
            let mag_out = (output_spectrum.sum_mag_sq[k] / out_avg).sqrt();

            if mag_in <= 1e-10 {
                f64::NEG_INFINITY
            } else {
                20.0 * (mag_out / mag_in).max(1e-10).log10()
            }
        })
        .collect()
}

/// A single frequency bin with magnitude, phase, and group delay.
#[derive(Debug, Clone, Copy)]
pub struct ResponseBin {
    pub freq_hz: f32,
    pub mag_db: f32,
    pub phase_rad: f32,
    /// Group delay in samples (−dφ/dω, where ω is in radians/sample).
    pub group_delay_samples: f32,
}

/// Compute the full transfer function H(f) from a deterministic impulse response.
///
/// This is noise-free: the FFT of the impulse response gives the exact
/// discrete-time transfer function of the system. Compared to the cross-spectrum
/// method (which averages noisy realizations), this produces clean magnitudes
/// for flat-response filters like allpass.
///
/// `impulse_response` should be long enough for the filter tail to decay.
/// The FFT is sized to the length of `impulse_response` (must be a power of 2
/// for efficiency but works for any length).
pub fn transfer_function_from_impulse(
    impulse_response: &[f32],
    sample_rate: f64,
    min_freq_hz: f32,
    max_freq_hz: f32,
) -> Vec<ResponseBin> {
    let fft_size = impulse_response.len();
    if fft_size == 0 {
        return Vec::new();
    }

    let sr = sample_rate as f32;
    let bin_hz = sr / fft_size as f32;

    // FFT the impulse response.
    let mut planner = FftPlanner::<f32>::new();
    let fft = planner.plan_fft_forward(fft_size);
    let mut buf: Vec<Complex<f32>> = impulse_response
        .iter()
        .map(|&x| Complex::new(x, 0.0))
        .collect();
    fft.process(&mut buf);

    // Extract one-sided spectrum (DC and Nyquist skipped to avoid DC/Nyquist edge artifacts).
    let num_bins = fft_size / 2;
    let mut result = Vec::with_capacity(num_bins);
    #[allow(clippy::needless_range_loop)]
    for k in 1..num_bins {
        let freq = k as f32 * bin_hz;
        if freq < min_freq_hz {
            continue;
        }
        if freq > max_freq_hz {
            break;
        }

        let h = buf[k];
        let mag_lin = (h.re * h.re + h.im * h.im).sqrt();
        let mag_db = if mag_lin <= 1e-12 {
            -240.0
        } else {
            20.0 * mag_lin.log10()
        };
        let phase_rad = h.im.atan2(h.re);

        result.push(ResponseBin {
            freq_hz: freq,
            mag_db,
            phase_rad,
            group_delay_samples: 0.0,
        });
    }

    // Fill group delay via numerical differentiation of phase.
    let gd = compute_group_delay_from_bins(&result, sample_rate);
    for (bin, &gd_val) in result.iter_mut().zip(gd.iter()) {
        bin.group_delay_samples = gd_val;
    }

    result
}

/// Summary of a full response comparison (magnitude + phase + group delay).
#[derive(Debug, Clone)]
pub struct FullResponseComparison {
    pub mag_rms_db: f32,
    pub mag_max_db: f32,
    pub phase_rms_rad: f32,
    pub phase_max_rad: f32,
    /// Group delay RMS difference in samples.
    pub gd_rms_samples: f32,
    /// Group delay max difference in samples.
    pub gd_max_samples: f32,
}

/// Compute the full transfer function H(f) with magnitude, phase, and group delay.
///
/// Uses cross-spectrum method: H(k) = avg[Y·conj(X)] / avg[|X|²]
/// giving magnitude (in dB), phase (in radians), and group delay (in samples).
///
/// `fft_size` controls frequency resolution: bin spacing = sample_rate / fft_size.
/// Use `DEFAULT_FFT_SIZE` (4096) for standard resolution, or 8192/16384 for finer.
pub fn transfer_function_full(
    input: &[f32],
    output: &[f32],
    sample_rate: f64,
    min_freq_hz: f32,
    max_freq_hz: f32,
) -> Vec<ResponseBin> {
    transfer_function_with_fft_size(
        input,
        output,
        sample_rate,
        min_freq_hz,
        max_freq_hz,
        DEFAULT_FFT_SIZE,
    )
}

/// Like `transfer_function_full` but with a configurable FFT size.
pub fn transfer_function_with_fft_size(
    input: &[f32],
    output: &[f32],
    sample_rate: f64,
    min_freq_hz: f32,
    max_freq_hz: f32,
    fft_size: usize,
) -> Vec<ResponseBin> {
    let sr = sample_rate as f32;
    let num_bins = fft_size / 2;
    let bin_hz = sr / fft_size as f32;

    let cross = welch_cross_spectrum(input, output, fft_size);

    // First pass: collect mag+phase bins (group delay filled in second pass)
    let mut result = Vec::new();
    for k in 0..num_bins {
        let freq = k as f32 * bin_hz;
        if freq < min_freq_hz || freq > max_freq_hz {
            continue;
        }

        let xx = cross.sum_xx[k];
        if xx <= 1e-20 {
            continue;
        }

        // H(k) = sum[Y·conj(X)] / sum[|X|²]
        let h_re = cross.sum_cross[k].0 / xx;
        let h_im = cross.sum_cross[k].1 / xx;

        let mag = (h_re * h_re + h_im * h_im).sqrt();
        let mag_db = if mag <= 1e-10 {
            f64::NEG_INFINITY
        } else {
            20.0 * mag.log10()
        };

        if !mag_db.is_finite() {
            continue;
        }

        let phase = h_im.atan2(h_re);

        result.push(ResponseBin {
            freq_hz: freq,
            mag_db: mag_db as f32,
            phase_rad: phase as f32,
            group_delay_samples: 0.0, // filled below
        });
    }

    // Second pass: compute group delay from phase via numerical differentiation
    let gd = compute_group_delay_from_bins(&result, sample_rate);
    for (bin, &gd_val) in result.iter_mut().zip(gd.iter()) {
        bin.group_delay_samples = gd_val;
    }

    result
}

/// A single frequency bin in the response comparison.
#[derive(Debug, Clone)]
pub struct FreqBin {
    pub freq_hz: f32,
    /// H(f) of plugin A in dB.
    pub a_db: f32,
    /// H(f) of plugin B in dB.
    pub b_db: f32,
    /// Difference: b_db - a_db.
    pub diff_db: f32,
}

/// Result of an FFT-based frequency response comparison between two plugin outputs.
#[derive(Debug, Clone)]
pub struct FreqResponseComparison {
    /// Per-bin comparison data.
    pub bins: Vec<FreqBin>,
    /// Maximum absolute difference across all bins (dB).
    pub max_diff_db: f32,
    /// RMS of the per-bin dB differences.
    pub rms_diff_db: f32,
    /// Frequency at which the maximum difference occurs.
    pub max_diff_freq_hz: f32,
}

/// Compare the frequency response of two plugin outputs given the same input.
///
/// Uses Welch's method: accumulates |X(k)|², |Y_a(k)|², |Y_b(k)|² over
/// multiple Hann-windowed FFT frames, then computes:
///   H_a(k) = sqrt( avg|Y_a|² / avg|X|² )
///   H_b(k) = sqrt( avg|Y_b|² / avg|X|² )
/// and reports per-bin dB differences across `[min_freq_hz, max_freq_hz]`.
pub fn compare_freq_response(
    input: &[f32],
    output_a: &[f32],
    output_b: &[f32],
    sample_rate: f64,
    min_freq_hz: f32,
    max_freq_hz: f32,
) -> FreqResponseComparison {
    let fft_size = DEFAULT_FFT_SIZE;
    let sr = sample_rate as f32;
    let num_bins = fft_size / 2;
    let bin_hz = sr / fft_size as f32;

    // Compute averaged power spectra (Welch's method)
    let x_psd = welch_power_spectrum(input, fft_size);
    let ya_psd = welch_power_spectrum(output_a, fft_size);
    let yb_psd = welch_power_spectrum(output_b, fft_size);

    // Compute transfer functions H_a(f) and H_b(f) in dB
    let h_a = transfer_function_db(&x_psd, &ya_psd);
    let h_b = transfer_function_db(&x_psd, &yb_psd);

    let mut bins = Vec::new();
    let mut max_diff_db: f32 = 0.0;
    let mut max_diff_freq: f32 = 0.0;
    let mut sum_sq_diff: f64 = 0.0;

    for k in 0..num_bins {
        let freq = k as f32 * bin_hz;
        if freq < min_freq_hz || freq > max_freq_hz {
            continue;
        }

        let a_db = h_a[k];
        let b_db = h_b[k];

        if !a_db.is_finite() || !b_db.is_finite() {
            continue;
        }

        let diff = (b_db - a_db) as f32;
        let abs_diff = diff.abs();

        if abs_diff > max_diff_db {
            max_diff_db = abs_diff;
            max_diff_freq = freq;
        }
        sum_sq_diff += (diff as f64) * (diff as f64);

        bins.push(FreqBin {
            freq_hz: freq,
            a_db: a_db as f32,
            b_db: b_db as f32,
            diff_db: diff,
        });
    }

    let rms_diff = if bins.is_empty() {
        f32::NAN
    } else {
        (sum_sq_diff / bins.len() as f64).sqrt() as f32
    };

    FreqResponseComparison {
        bins,
        max_diff_db,
        rms_diff_db: rms_diff,
        max_diff_freq_hz: max_diff_freq,
    }
}

// ---------------------------------------------------------------------------
// Group delay and full response comparison
// ---------------------------------------------------------------------------

/// Unwrap phase by removing jumps greater than π.
fn unwrap_phase(phases: &[f32]) -> Vec<f32> {
    if phases.is_empty() {
        return vec![];
    }
    let mut result = Vec::with_capacity(phases.len());
    result.push(phases[0]);
    let mut offset = 0.0f32;
    for i in 1..phases.len() {
        let mut diff = phases[i] - phases[i - 1];
        if diff > std::f32::consts::PI {
            diff -= 2.0 * std::f32::consts::PI;
            offset -= 2.0 * std::f32::consts::PI;
        } else if diff < -std::f32::consts::PI {
            diff += 2.0 * std::f32::consts::PI;
            offset += 2.0 * std::f32::consts::PI;
        }
        let _ = diff; // used only for offset tracking
        result.push(phases[i] + offset);
    }
    result
}

/// Compute group delay from response bins.
///
/// Group delay = −dφ/dω where ω is angular frequency.
/// Returns values in samples. Uses central differences for interior points.
pub fn compute_group_delay(bins: &[ResponseBin], sample_rate: f64) -> Vec<f32> {
    compute_group_delay_from_bins(bins, sample_rate)
}

/// Internal: compute group delay from bins with freq_hz and phase_rad fields.
fn compute_group_delay_from_bins(bins: &[ResponseBin], sample_rate: f64) -> Vec<f32> {
    if bins.len() < 2 {
        return vec![0.0; bins.len()];
    }

    let phases: Vec<f32> = bins.iter().map(|b| b.phase_rad).collect();
    let unwrapped = unwrap_phase(&phases);
    let sr = sample_rate as f32;

    let mut gd = Vec::with_capacity(bins.len());

    // Forward difference for first point
    let dw = 2.0 * std::f32::consts::PI * (bins[1].freq_hz - bins[0].freq_hz) / sr;
    if dw.abs() > 1e-10 {
        gd.push(-(unwrapped[1] - unwrapped[0]) / dw);
    } else {
        gd.push(0.0);
    }

    // Central differences for interior
    for i in 1..bins.len() - 1 {
        let dw = 2.0 * std::f32::consts::PI * (bins[i + 1].freq_hz - bins[i - 1].freq_hz) / sr;
        if dw.abs() > 1e-10 {
            gd.push(-(unwrapped[i + 1] - unwrapped[i - 1]) / dw);
        } else {
            gd.push(0.0);
        }
    }

    // Backward difference for last point
    let n = bins.len();
    let dw = 2.0 * std::f32::consts::PI * (bins[n - 1].freq_hz - bins[n - 2].freq_hz) / sr;
    if dw.abs() > 1e-10 {
        gd.push(-(unwrapped[n - 1] - unwrapped[n - 2]) / dw);
    } else {
        gd.push(0.0);
    }

    gd
}

/// Wrap a phase difference to [−π, π].
#[inline]
fn wrap_phase_diff(d: f32) -> f32 {
    let mut r = d % (2.0 * std::f32::consts::PI);
    if r > std::f32::consts::PI {
        r -= 2.0 * std::f32::consts::PI;
    } else if r < -std::f32::consts::PI {
        r += 2.0 * std::f32::consts::PI;
    }
    r
}

/// Compare two sets of response bins on magnitude, phase, and group delay.
///
/// Bins are matched by nearest frequency. Phase differences are wrapped to [−π, π].
/// Group delay is read directly from the bins (already computed during capture/measurement).
pub fn compare_response_full(
    ref_bins: &[ResponseBin],
    test_bins: &[ResponseBin],
) -> FullResponseComparison {
    if ref_bins.is_empty() || test_bins.is_empty() {
        return FullResponseComparison {
            mag_rms_db: f32::NAN,
            mag_max_db: 0.0,
            phase_rms_rad: f32::NAN,
            phase_max_rad: 0.0,
            gd_rms_samples: f32::NAN,
            gd_max_samples: 0.0,
        };
    }

    let mut mag_sum_sq = 0.0f64;
    let mut mag_max = 0.0f32;
    let mut phase_sum_sq = 0.0f64;
    let mut phase_max = 0.0f32;
    let mut gd_sum_sq = 0.0f64;
    let mut gd_max = 0.0f32;
    let mut count = 0usize;

    let mut test_idx = 0;
    for ref_bin in ref_bins.iter() {
        // Find nearest test bin by frequency
        while test_idx + 1 < test_bins.len()
            && (test_bins[test_idx + 1].freq_hz - ref_bin.freq_hz).abs()
                < (test_bins[test_idx].freq_hz - ref_bin.freq_hz).abs()
        {
            test_idx += 1;
        }

        // Magnitude
        let mag_diff = (test_bins[test_idx].mag_db - ref_bin.mag_db).abs();
        mag_sum_sq += mag_diff as f64 * mag_diff as f64;
        if mag_diff > mag_max {
            mag_max = mag_diff;
        }

        // Phase (wrapped difference)
        let phase_diff = wrap_phase_diff(test_bins[test_idx].phase_rad - ref_bin.phase_rad).abs();
        phase_sum_sq += phase_diff as f64 * phase_diff as f64;
        if phase_diff > phase_max {
            phase_max = phase_diff;
        }

        // Group delay (from pre-computed field)
        let gd_diff = (test_bins[test_idx].group_delay_samples - ref_bin.group_delay_samples).abs();
        // Skip extreme outliers (e.g. at DC or Nyquist where GD is unreliable)
        if gd_diff < 1000.0 {
            gd_sum_sq += gd_diff as f64 * gd_diff as f64;
            if gd_diff > gd_max {
                gd_max = gd_diff;
            }
        }

        count += 1;
    }

    let n = count.max(1) as f64;
    FullResponseComparison {
        mag_rms_db: (mag_sum_sq / n).sqrt() as f32,
        mag_max_db: mag_max,
        phase_rms_rad: (phase_sum_sq / n).sqrt() as f32,
        phase_max_rad: phase_max,
        gd_rms_samples: (gd_sum_sq / n).sqrt() as f32,
        gd_max_samples: gd_max,
    }
}

// ---------------------------------------------------------------------------
// Impulse response analysis and biquad coefficient extraction
// ---------------------------------------------------------------------------

/// Biquad filter coefficients (Direct Form I).
///
/// Transfer function: H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
#[derive(Debug, Clone, Copy)]
pub struct BiquadCoefficients {
    pub b0: f64,
    pub b1: f64,
    pub b2: f64,
    pub a1: f64,
    pub a2: f64,
}

/// Extract biquad coefficients from an impulse response.
///
/// For a single second-order IIR (biquad), the impulse response satisfies:
///   y[n] = b0*δ[n] + b1*δ[n-1] + b2*δ[n-2] - a1*y[n-1] - a2*y[n-2]
///
/// We solve for a1, a2 from y[3..4] (where input terms are zero), then
/// back-solve for b0, b1, b2. Returns None if the IR doesn't look like
/// a single biquad (e.g. determinant too small).
pub fn extract_biquad_coefficients(ir: &[f32]) -> Option<BiquadCoefficients> {
    if ir.len() < 5 {
        return None;
    }

    let y: Vec<f64> = ir.iter().map(|&s| s as f64).collect();

    // For n >= 3, x[n] = 0, so: y[n] = -a1*y[n-1] - a2*y[n-2]
    // Use samples 3 and 4 to form a 2x2 system:
    //   y[3] = -a1*y[2] - a2*y[1]
    //   y[4] = -a1*y[3] - a2*y[2]
    let det = y[2] * y[2] - y[1] * y[3];
    if det.abs() < 1e-30 {
        return None; // Singular — not a clean biquad
    }

    let a1 = (y[3] * y[2] - y[4] * y[1]) / det;
    let a2 = (y[2] * y[4] - y[3] * y[3]) / det;

    // Back-solve feedforward coefficients:
    //   y[0] = b0                        (x[0]=1, no prior state)
    //   y[1] = b1 - a1*y[0]             (x[1]=0)
    //   y[2] = b2 - a1*y[1] - a2*y[0]  (x[2]=0)
    let b0 = y[0];
    let b1 = y[1] + a1 * y[0];
    let b2 = y[2] + a1 * y[1] + a2 * y[0];

    Some(BiquadCoefficients { b0, b1, b2, a1, a2 })
}

/// Verify extracted biquad coefficients by resynthesizing the impulse response
/// and comparing to the original. Returns the RMS error.
pub fn verify_biquad(coeffs: &BiquadCoefficients, ir: &[f32]) -> f64 {
    let len = ir.len();
    let mut synth = vec![0.0f64; len];

    for n in 0..len {
        let x_n = if n == 0 { 1.0 } else { 0.0 };
        let x_n1 = if n == 1 { 1.0 } else { 0.0 };
        let x_n2 = if n == 2 { 1.0 } else { 0.0 };

        synth[n] = coeffs.b0 * x_n
            + if n >= 1 { coeffs.b1 * x_n1 } else { 0.0 }
            + if n >= 2 { coeffs.b2 * x_n2 } else { 0.0 }
            - if n >= 1 {
                coeffs.a1 * synth[n - 1]
            } else {
                0.0
            }
            - if n >= 2 {
                coeffs.a2 * synth[n - 2]
            } else {
                0.0
            };
    }

    let sum_sq: f64 = ir
        .iter()
        .zip(synth.iter())
        .map(|(&r, &s)| {
            let d = r as f64 - s;
            d * d
        })
        .sum();

    (sum_sq / len as f64).sqrt()
}

/// Result of comparing two impulse responses.
#[derive(Debug, Clone)]
pub struct ImpulseComparison {
    /// RMS of sample-by-sample difference.
    pub rms_diff: f64,
    /// Peak absolute difference.
    pub peak_diff: f64,
    /// Sample index of peak difference.
    pub peak_diff_sample: usize,
    /// Number of samples compared.
    pub num_samples: usize,
}

/// Compare two impulse responses sample-by-sample.
pub fn compare_impulse_response(ref_ir: &[f32], test_ir: &[f32]) -> ImpulseComparison {
    let len = ref_ir.len().min(test_ir.len());
    if len == 0 {
        return ImpulseComparison {
            rms_diff: f64::NAN,
            peak_diff: 0.0,
            peak_diff_sample: 0,
            num_samples: 0,
        };
    }

    let mut sum_sq = 0.0f64;
    let mut peak = 0.0f64;
    let mut peak_idx = 0;

    for i in 0..len {
        let d = (test_ir[i] as f64 - ref_ir[i] as f64).abs();
        sum_sq += d * d;
        if d > peak {
            peak = d;
            peak_idx = i;
        }
    }

    ImpulseComparison {
        rms_diff: (sum_sq / len as f64).sqrt(),
        peak_diff: peak,
        peak_diff_sample: peak_idx,
        num_samples: len,
    }
}

/// Result of multi-level linearity testing.
#[derive(Debug, Clone)]
pub struct LinearityResult {
    /// Input level in dBFS.
    pub level_db: f32,
    /// Magnitude RMS difference at this level.
    pub mag_rms_db: f32,
    /// Phase RMS difference at this level.
    pub phase_rms_rad: f32,
}

/// Check linearity by comparing frequency responses at multiple input levels.
///
/// For a linear system, the frequency response should be identical regardless
/// of input level. Any deviation indicates nonlinear behavior (saturation,
/// soft-clipping, noise floor issues).
pub fn check_linearity(responses: &[(f32, Vec<ResponseBin>)]) -> Vec<LinearityResult> {
    if responses.len() < 2 {
        return vec![];
    }

    // Use the first level as reference
    let ref_bins = &responses[0].1;
    let _ref_level = responses[0].0;

    let mut results = Vec::new();
    // Compare each level's response to the reference
    for (level, bins) in responses {
        let cmp = compare_response_full(ref_bins, bins);
        results.push(LinearityResult {
            level_db: *level,
            mag_rms_db: cmp.mag_rms_db,
            phase_rms_rad: cmp.phase_rms_rad,
        });
    }

    results
}
