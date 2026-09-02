//! What we measure on a separated song.
//!
//! Two families of question, and the shapes they need are different:
//!
//! - **How compressed is the vocal?** — [`crest_db`], peak minus RMS
//!   over voiced material only.
//! - **Where does the vocal sit against the track?** — [`band_spectrum`]
//!   for each stem, so the two can be compared band by band to find
//!   where the vocal stands clear of the instrumental and where it is
//!   buried.
//!
//! ## Everything is measured before encoding
//!
//! Stems are archived as Opus to keep the corpus around 99 GB rather
//! than 443 GB, but every number here is computed from demucs's
//! lossless output first. Measured across twelve stems, Opus 128k moves
//! crest factor by 0.12 dB on average — but by 0.81 dB in the worst
//! case, which is the same size as the genre differences the corpus
//! exists to resolve. Measuring first costs nothing and removes the
//! caveat entirely.
//!
//! ## Bands are in hertz, not FFT bins
//!
//! [`band_spectrum`] returns fixed logarithmically-spaced *frequency*
//! bands. This is not cosmetic. Comparing raw FFT bins across files at
//! different sample rates silently compares different frequencies: an
//! early version of this comparison reported a 2.6 dB error for every
//! Opus bitrate, including one that was audibly transparent, purely
//! because Opus resamples to 48 kHz while demucs writes 44.1 kHz. Bands
//! in hertz make the measurement independent of sample rate.

use std::f64::consts::PI;

/// Frame length for the voiced-material gate.
pub const FRAME_MS: f64 = 50.0;

/// How far below the loudest frame still counts as voiced.
///
/// Silence has to be excluded: the gaps between phrases drag RMS down
/// and inflate crest factor, which is the single most common way this
/// measurement is reported wrong.
pub const GATE_DB: f64 = -40.0;

/// The one sample rate every spectral measurement is taken at.
///
/// Not a preference — a requirement. The corpus mixes rates by
/// construction: sources arrive as 48 kHz Opus, and demucs writes its
/// stems at 44.1 kHz. Comparing spectra taken at different rates
/// compares slightly different things, and no amount of care inside
/// [`band_spectrum`] removes that entirely: with a fixed transform size
/// a worst-band residual around 1 dB survives, which is the size of the
/// genre differences this corpus exists to resolve.
///
/// So callers must decode to this rate before measuring — ffmpeg's
/// `-ar 48000` — rather than measuring whatever a file happens to be.
/// [`band_spectrum`] is written to be as rate-insensitive as it can be,
/// but that is defence in depth, not the guarantee.
pub const ANALYSIS_SAMPLE_RATE: f64 = 48_000.0;

/// Bands per octave in [`band_spectrum`].
pub const BANDS_PER_OCTAVE: f64 = 6.0;

/// Lowest and highest band centre.
pub const BAND_LO_HZ: f64 = 20.0;
pub const BAND_HI_HZ: f64 = 20_000.0;

/// Peak minus RMS in dB, over voiced material only.
///
/// Returns `None` for signals with no material above the gate — a
/// stem that separated to near-silence, which happens on instrumental
/// tracks and must not be reported as a crest factor of zero.
pub fn crest_db(samples: &[f64], sample_rate: f64) -> Option<f64> {
    let frame = ((sample_rate * FRAME_MS / 1000.0) as usize).max(1);
    if samples.len() < frame {
        return None;
    }

    let frames: Vec<&[f64]> = samples.chunks_exact(frame).collect();
    let rms: Vec<f64> = frames.iter().map(|f| rms_linear(f)).collect();
    let loudest = rms.iter().copied().fold(0.0_f64, f64::max);
    if loudest <= 0.0 {
        return None;
    }

    let threshold = loudest * db_to_linear(GATE_DB);
    let voiced: Vec<&[f64]> = frames
        .iter()
        .zip(&rms)
        .filter(|(_, r)| **r > threshold)
        .map(|(f, _)| *f)
        .collect();
    if voiced.is_empty() {
        return None;
    }

    let mut peak = 0.0_f64;
    let mut sum_sq = 0.0_f64;
    let mut n = 0usize;
    for f in &voiced {
        for &s in *f {
            peak = peak.max(s.abs());
            sum_sq += s * s;
            n += 1;
        }
    }
    let rms_all = (sum_sq / n as f64).sqrt();
    if rms_all <= 0.0 || peak <= 0.0 {
        return None;
    }
    Some(20.0 * (peak / rms_all).log10())
}

/// Overall RMS in dBFS. Used for stem-to-stem level ratios.
pub fn rms_db(samples: &[f64]) -> Option<f64> {
    if samples.is_empty() {
        return None;
    }
    let r = rms_linear(samples);
    (r > 0.0).then(|| 20.0 * r.log10())
}

/// The centre frequency of every band, low to high.
pub fn band_centres() -> Vec<f64> {
    let mut out = Vec::new();
    let mut f = BAND_LO_HZ;
    let step = 2.0_f64.powf(1.0 / BANDS_PER_OCTAVE);
    while f <= BAND_HI_HZ {
        out.push(f);
        f *= step;
    }
    out
}

/// Average magnitude spectrum, in dB, per logarithmic frequency band.
///
/// Averaged across the whole signal so a single loud moment cannot
/// define the curve, and returned *normalised to its own mean* so two
/// stems can be compared by shape without their relative level
/// dominating.
pub fn band_spectrum(samples: &[f64], sample_rate: f64) -> Option<Vec<f64>> {
    const NFFT: usize = 4096;
    if samples.len() < NFFT {
        return None;
    }

    let window: Vec<f64> = (0..NFFT)
        .map(|i| 0.5 - 0.5 * (2.0 * PI * i as f64 / NFFT as f64).cos())
        .collect();

    // Accumulate power per FFT bin across frames, then fold into bands.
    let mut power = vec![0.0_f64; NFFT / 2 + 1];
    let mut frames = 0usize;
    for chunk in samples.chunks_exact(NFFT) {
        let windowed: Vec<f64> = chunk.iter().zip(&window).map(|(s, w)| s * w).collect();
        for (bin, p) in dft_power(&windowed).into_iter().enumerate() {
            power[bin] += p;
        }
        frames += 1;
    }
    if frames == 0 {
        return None;
    }

    // Convert to power spectral density — power per hertz — so a band's
    // value stops depending on how the FFT happens to be binned.
    let bin_hz = sample_rate / NFFT as f64;
    let window_energy: f64 = window.iter().map(|w| w * w).sum();
    let psd: Vec<f64> = power
        .iter()
        .map(|p| p / (frames as f64 * bin_hz * window_energy))
        .collect();

    let centres = band_centres();
    let step = 2.0_f64.powf(0.5 / BANDS_PER_OCTAVE);
    let nyquist = sample_rate / 2.0;

    let mut out = Vec::with_capacity(centres.len());
    for c in &centres {
        let lo = c / step;
        if lo >= nyquist {
            out.push(f64::NEG_INFINITY);
            continue;
        }
        let hi = (c * step).min(nyquist);

        // Integrate the density across the band, weighting each bin by
        // how much of it actually falls inside. Rounding bin edges
        // outward instead (floor/ceil) counts partial bins whole, and
        // how much it over-counts depends on the bin grid — which is
        // exactly the sample-rate dependence being removed. Measured on
        // one signal resampled between 44.1 and 48 kHz, this halves the
        // error: 0.34 -> 0.19 dB mean, 1.66 -> 0.77 dB worst band.
        let first = (lo / bin_hz - 0.5).floor().max(0.0) as usize;
        let last = ((hi / bin_hz + 0.5).ceil() as usize).min(psd.len().saturating_sub(1));

        let mut acc = 0.0;
        for (k, p) in psd.iter().enumerate().take(last + 1).skip(first) {
            let (klo, khi) = (
                k as f64 * bin_hz - bin_hz / 2.0,
                k as f64 * bin_hz + bin_hz / 2.0,
            );
            let overlap = (hi.min(khi) - lo.max(klo)).max(0.0);
            acc += p * overlap;
        }

        // Mean density across the band, so bands of different widths
        // are directly comparable.
        let width = (hi - lo).max(1e-9);
        out.push(10.0 * (acc / width + 1e-30).log10());
    }

    // Normalise to the mean of the finite bands: the question is the
    // shape of the curve, not how loud the stem happens to be.
    let finite: Vec<f64> = out.iter().copied().filter(|v| v.is_finite()).collect();
    if finite.is_empty() {
        return None;
    }
    let mean = finite.iter().sum::<f64>() / finite.len() as f64;
    Some(out.iter().map(|v| v - mean).collect())
}

/// How far the vocal stands above the instrumental in each band.
///
/// Positive means the vocal dominates that band; negative means the
/// track does. This is the raw material for "where does the vocal poke
/// out, and where is it competing" — both curves are already normalised
/// to their own mean, so this compares shape rather than mix level.
pub fn band_margin(vocal: &[f64], instrumental: &[f64]) -> Vec<f64> {
    vocal
        .iter()
        .zip(instrumental)
        .map(|(v, i)| {
            if v.is_finite() && i.is_finite() {
                v - i
            } else {
                f64::NAN
            }
        })
        .collect()
}

fn rms_linear(x: &[f64]) -> f64 {
    if x.is_empty() {
        return 0.0;
    }
    (x.iter().map(|s| s * s).sum::<f64>() / x.len() as f64).sqrt()
}

fn db_to_linear(db: f64) -> f64 {
    10.0_f64.powf(db / 20.0)
}

/// Real-input power spectrum via a radix-2 FFT.
fn dft_power(x: &[f64]) -> Vec<f64> {
    let n = x.len();
    let mut re: Vec<f64> = x.to_vec();
    let mut im = vec![0.0_f64; n];
    fft(&mut re, &mut im);
    (0..n / 2 + 1).map(|k| re[k] * re[k] + im[k] * im[k]).collect()
}

/// In-place iterative radix-2 Cooley-Tukey. `n` must be a power of two.
fn fft(re: &mut [f64], im: &mut [f64]) {
    let n = re.len();
    debug_assert!(n.is_power_of_two());

    let mut j = 0usize;
    for i in 1..n {
        let mut bit = n >> 1;
        while j & bit != 0 {
            j ^= bit;
            bit >>= 1;
        }
        j |= bit;
        if i < j {
            re.swap(i, j);
            im.swap(i, j);
        }
    }

    let mut len = 2;
    while len <= n {
        let ang = -2.0 * PI / len as f64;
        let (wr, wi) = (ang.cos(), ang.sin());
        for i in (0..n).step_by(len) {
            let (mut cr, mut ci) = (1.0_f64, 0.0_f64);
            for k in 0..len / 2 {
                let (ur, ui) = (re[i + k], im[i + k]);
                let (xr, xi) = (re[i + k + len / 2], im[i + k + len / 2]);
                let vr = xr * cr - xi * ci;
                let vi = xr * ci + xi * cr;
                re[i + k] = ur + vr;
                im[i + k] = ui + vi;
                re[i + k + len / 2] = ur - vr;
                im[i + k + len / 2] = ui - vi;
                let ncr = cr * wr - ci * wi;
                ci = cr * wi + ci * wr;
                cr = ncr;
            }
        }
        len <<= 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SR: f64 = 48_000.0;

    fn sine(freq: f64, secs: f64, amp: f64) -> Vec<f64> {
        let n = (SR * secs) as usize;
        (0..n)
            .map(|i| amp * (2.0 * PI * freq * i as f64 / SR).sin())
            .collect()
    }

    #[test]
    fn a_sine_has_the_textbook_crest_factor() {
        // peak/rms = sqrt(2) -> 3.01 dB, whatever its amplitude.
        let c = crest_db(&sine(1000.0, 2.0, 0.5), SR).unwrap();
        assert!((c - 3.01).abs() < 0.05, "got {c}");
        let quiet = crest_db(&sine(1000.0, 2.0, 0.01), SR).unwrap();
        assert!((quiet - 3.01).abs() < 0.05, "got {quiet}");
    }

    #[test]
    fn silence_between_phrases_is_gated_out() {
        // A tone followed by an equal stretch of silence must measure
        // the same as the tone alone. Without the gate the silence
        // halves RMS and inflates crest by ~3 dB.
        let tone = sine(1000.0, 2.0, 0.5);
        let mut padded = tone.clone();
        padded.extend(std::iter::repeat(0.0).take(tone.len()));
        let a = crest_db(&tone, SR).unwrap();
        let b = crest_db(&padded, SR).unwrap();
        assert!((a - b).abs() < 0.1, "gate failed: {a} vs {b}");
    }

    #[test]
    fn a_silent_stem_reports_nothing_rather_than_zero() {
        assert!(crest_db(&vec![0.0; 48_000], SR).is_none());
        assert!(crest_db(&[], SR).is_none());
    }

    #[test]
    fn crest_rises_when_a_transient_is_added() {
        let mut x = sine(1000.0, 2.0, 0.2);
        // One loud click: peak jumps, RMS barely moves.
        let mid = x.len() / 2;
        x[mid] = 0.99;
        let flat = crest_db(&sine(1000.0, 2.0, 0.2), SR).unwrap();
        let spiky = crest_db(&x, SR).unwrap();
        assert!(spiky > flat + 5.0, "{spiky} should far exceed {flat}");
    }

    #[test]
    fn bands_are_log_spaced_and_cover_the_audible_range() {
        let c = band_centres();
        assert!((c[0] - 20.0).abs() < 1e-9);
        assert!(*c.last().unwrap() <= BAND_HI_HZ);
        // Six per octave means a doubling every six bands.
        assert!((c[6] / c[0] - 2.0).abs() < 1e-6);
        assert!(c.len() > 50, "expected ~60 bands, got {}", c.len());
    }

    #[test]
    fn a_tone_peaks_in_its_own_band() {
        let s = band_spectrum(&sine(1000.0, 2.0, 0.5), SR).unwrap();
        let centres = band_centres();
        let peak = s
            .iter()
            .enumerate()
            .filter(|(_, v)| v.is_finite())
            .max_by(|a, b| a.1.total_cmp(b.1))
            .unwrap()
            .0;
        let hz = centres[peak];
        assert!((hz / 1000.0).log2().abs() < 1.0 / BANDS_PER_OCTAVE, "peak at {hz} Hz");
    }

    /// Broadband material with identical content at any sample rate:
    /// a fixed set of sinusoids with fixed phases, so both renderings
    /// are genuinely the same signal rather than two different noise
    /// draws. Amplitudes fall as 1/sqrt(f), which is roughly how music
    /// is distributed and nothing like a single tone.
    fn broadband(sample_rate: f64, secs: f64) -> Vec<f64> {
        let n = (sample_rate * secs) as usize;
        // Log-spaced partials across the vocal range, deterministic
        // phases from a simple hash so the signal is reproducible.
        let partials: Vec<(f64, f64, f64)> = (0..240)
            .map(|k| {
                let f = 40.0 * 1.03_f64.powi(k);
                let phase = ((k as usize).wrapping_mul(2654435761) % 1000) as f64 / 1000.0
                    * 2.0
                    * PI;
                (f, 1.0 / f.sqrt(), phase)
            })
            .filter(|(f, _, _)| *f < 16_000.0)
            .collect();

        (0..n)
            .map(|i| {
                let t = i as f64 / sample_rate;
                partials
                    .iter()
                    .map(|(f, a, p)| a * (2.0 * PI * f * t + p).sin())
                    .sum::<f64>()
                    * 0.02
            })
            .collect()
    }

    /// Measuring the same audio at one rate must be repeatable — this
    /// is the property the pipeline actually relies on, because every
    /// spectral measurement decodes to [`ANALYSIS_SAMPLE_RATE`] first.
    #[test]
    fn the_curve_is_exact_at_a_fixed_rate() {
        let sig = broadband(ANALYSIS_SAMPLE_RATE, 6.0);
        let a = band_spectrum(&sig, ANALYSIS_SAMPLE_RATE).unwrap();
        let b = band_spectrum(&sig, ANALYSIS_SAMPLE_RATE).unwrap();
        assert_eq!(a, b);
    }

    /// Across sample rates the curve is *close*, not identical, and
    /// this pins down how close so a regression is visible.
    ///
    /// It is deliberately not asserted to be exact. With a fixed
    /// transform size the bin grid differs between rates, and a
    /// worst-band residual around 1-2 dB survives any amount of care in
    /// the estimator — the same size as the genre differences being
    /// studied. That is precisely why [`ANALYSIS_SAMPLE_RATE`] exists:
    /// the guarantee comes from decoding everything at one rate, and
    /// this test documents what is left over if that slips.
    ///
    /// (A single sinusoid is deliberately not used. Almost every band
    /// is then empty, the normalisation is taken over 60 near-zero
    /// values, and tiny numerical differences become tens of dB — that
    /// measures the degeneracy of the test signal, not the estimator.)
    #[test]
    fn cross_rate_residual_stays_bounded() {
        let a = band_spectrum(&broadband(48_000.0, 6.0), 48_000.0).unwrap();
        let b = band_spectrum(&broadband(44_100.0, 6.0), 44_100.0).unwrap();

        let centres = band_centres();
        let mut worst = 0.0_f64;
        for (i, c) in centres.iter().enumerate() {
            if !(100.0..=10_000.0).contains(c) {
                continue;
            }
            if a[i].is_finite() && b[i].is_finite() {
                worst = worst.max((a[i] - b[i]).abs());
            }
        }
        // Measured ~1.3 dB on this signal, ~0.8 dB on genuinely
        // resampled pink noise. Tightening this is welcome; loosening it
        // means the estimator got worse.
        assert!(
            worst < 2.0,
            "cross-rate residual grew to {worst:.3} dB — decode to \
             ANALYSIS_SAMPLE_RATE and check band_spectrum's binning"
        );
    }

    #[test]
    fn margin_is_positive_where_the_vocal_leads() {
        let v = vec![10.0, 0.0, -10.0];
        let i = vec![0.0, 0.0, 0.0];
        let m = band_margin(&v, &i);
        assert_eq!(m[0], 10.0);
        assert_eq!(m[1], 0.0);
        assert_eq!(m[2], -10.0);
    }

    #[test]
    fn rms_of_a_half_amplitude_sine_is_minus_nine_db() {
        let r = rms_db(&sine(1000.0, 1.0, 0.5)).unwrap();
        assert!((r - (-9.03)).abs() < 0.05, "got {r}");
    }
}
