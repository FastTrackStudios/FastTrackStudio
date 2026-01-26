//! Filter design and coefficient calculation for parametric EQ.
//!
//! Based on Robert Bristow-Johnson's Audio EQ Cookbook and
//! ZLEqualizer's MartinCoeff implementation.

use std::f64::consts::PI;

use super::params::{BandParams, BandShape, FilterSlope};

/// Biquad filter coefficients in normalized form.
///
/// Transfer function: H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
#[derive(Debug, Clone, Copy, Default)]
pub struct BiquadCoeffs {
    pub b0: f64,
    pub b1: f64,
    pub b2: f64,
    pub a1: f64,
    pub a2: f64,
}

impl BiquadCoeffs {
    /// Unity gain passthrough.
    pub const UNITY: Self = Self {
        b0: 1.0,
        b1: 0.0,
        b2: 0.0,
        a1: 0.0,
        a2: 0.0,
    };

    /// Calculate magnitude response at normalized frequency.
    ///
    /// `w` is the normalized angular frequency (0 to π).
    #[must_use]
    pub fn magnitude_at(&self, w: f64) -> f64 {
        let cos_w = w.cos();
        let cos_2w = (2.0 * w).cos();

        let num = self.b0 * self.b0 + self.b1 * self.b1 + self.b2 * self.b2
            + 2.0 * (self.b0 * self.b1 + self.b1 * self.b2) * cos_w
            + 2.0 * self.b0 * self.b2 * cos_2w;

        let den = 1.0 + self.a1 * self.a1 + self.a2 * self.a2
            + 2.0 * (self.a1 + self.a1 * self.a2) * cos_w
            + 2.0 * self.a2 * cos_2w;

        (num / den).sqrt()
    }

    /// Calculate magnitude squared response (more efficient for curves).
    #[must_use]
    pub fn magnitude_squared_at(&self, w: f64) -> f64 {
        let cos_w = w.cos();
        let cos_2w = (2.0 * w).cos();

        let num = self.b0 * self.b0 + self.b1 * self.b1 + self.b2 * self.b2
            + 2.0 * (self.b0 * self.b1 + self.b1 * self.b2) * cos_w
            + 2.0 * self.b0 * self.b2 * cos_2w;

        let den = 1.0 + self.a1 * self.a1 + self.a2 * self.a2
            + 2.0 * (self.a1 + self.a1 * self.a2) * cos_w
            + 2.0 * self.a2 * cos_2w;

        num / den
    }
}

/// Filter coefficient calculator.
///
/// Generates biquad coefficients for various filter types.
pub struct FilterDesign;

impl FilterDesign {
    /// Calculate normalized angular frequency.
    ///
    /// `freq` is the frequency in Hz, `sample_rate` is the sample rate.
    #[must_use]
    pub fn omega(freq: f64, sample_rate: f64) -> f64 {
        2.0 * PI * freq / sample_rate
    }

    /// Calculate coefficients for a peaking/bell EQ filter.
    ///
    /// # Parameters
    /// - `w0`: Normalized angular frequency (2π * f / fs)
    /// - `gain_db`: Gain in dB
    /// - `q`: Q factor
    #[must_use]
    pub fn peak(w0: f64, gain_db: f64, q: f64) -> BiquadCoeffs {
        let a = 10.0_f64.powf(gain_db / 40.0);
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = 1.0 + alpha * a;
        let b1 = -2.0 * cos_w0;
        let b2 = 1.0 - alpha * a;
        let a0 = 1.0 + alpha / a;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha / a;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a low shelf filter.
    #[must_use]
    pub fn low_shelf(w0: f64, gain_db: f64, q: f64) -> BiquadCoeffs {
        let a = 10.0_f64.powf(gain_db / 40.0);
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);
        let sqrt_a = a.sqrt();
        let two_sqrt_a_alpha = 2.0 * sqrt_a * alpha;

        let b0 = a * ((a + 1.0) - (a - 1.0) * cos_w0 + two_sqrt_a_alpha);
        let b1 = 2.0 * a * ((a - 1.0) - (a + 1.0) * cos_w0);
        let b2 = a * ((a + 1.0) - (a - 1.0) * cos_w0 - two_sqrt_a_alpha);
        let a0 = (a + 1.0) + (a - 1.0) * cos_w0 + two_sqrt_a_alpha;
        let a1 = -2.0 * ((a - 1.0) + (a + 1.0) * cos_w0);
        let a2 = (a + 1.0) + (a - 1.0) * cos_w0 - two_sqrt_a_alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a high shelf filter.
    #[must_use]
    pub fn high_shelf(w0: f64, gain_db: f64, q: f64) -> BiquadCoeffs {
        let a = 10.0_f64.powf(gain_db / 40.0);
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);
        let sqrt_a = a.sqrt();
        let two_sqrt_a_alpha = 2.0 * sqrt_a * alpha;

        let b0 = a * ((a + 1.0) + (a - 1.0) * cos_w0 + two_sqrt_a_alpha);
        let b1 = -2.0 * a * ((a - 1.0) + (a + 1.0) * cos_w0);
        let b2 = a * ((a + 1.0) + (a - 1.0) * cos_w0 - two_sqrt_a_alpha);
        let a0 = (a + 1.0) - (a - 1.0) * cos_w0 + two_sqrt_a_alpha;
        let a1 = 2.0 * ((a - 1.0) - (a + 1.0) * cos_w0);
        let a2 = (a + 1.0) - (a - 1.0) * cos_w0 - two_sqrt_a_alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a low pass filter.
    #[must_use]
    pub fn low_pass(w0: f64, q: f64) -> BiquadCoeffs {
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = (1.0 - cos_w0) / 2.0;
        let b1 = 1.0 - cos_w0;
        let b2 = (1.0 - cos_w0) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a high pass filter.
    #[must_use]
    pub fn high_pass(w0: f64, q: f64) -> BiquadCoeffs {
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = (1.0 + cos_w0) / 2.0;
        let b1 = -(1.0 + cos_w0);
        let b2 = (1.0 + cos_w0) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a notch filter.
    #[must_use]
    pub fn notch(w0: f64, q: f64) -> BiquadCoeffs {
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = 1.0;
        let b1 = -2.0 * cos_w0;
        let b2 = 1.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a band pass filter.
    #[must_use]
    pub fn band_pass(w0: f64, q: f64) -> BiquadCoeffs {
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = alpha;
        let b1 = 0.0;
        let b2 = -alpha;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for an all pass filter.
    #[must_use]
    pub fn all_pass(w0: f64, q: f64) -> BiquadCoeffs {
        let cos_w0 = w0.cos();
        let sin_w0 = w0.sin();
        let alpha = sin_w0 / (2.0 * q);

        let b0 = 1.0 - alpha;
        let b1 = -2.0 * cos_w0;
        let b2 = 1.0 + alpha;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_w0;
        let a2 = 1.0 - alpha;

        BiquadCoeffs {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Calculate coefficients for a tilt shelf filter.
    ///
    /// Tilt shelf boosts/cuts highs while cutting/boosting lows.
    #[must_use]
    pub fn tilt_shelf(w0: f64, gain_db: f64, q: f64) -> BiquadCoeffs {
        // Tilt shelf is a combination of low shelf and high shelf
        // with opposite gains at the pivot frequency
        let half_gain = gain_db / 2.0;
        Self::high_shelf(w0, half_gain, q)
    }

    /// Calculate coefficients for a first-order low pass.
    #[must_use]
    pub fn low_pass_1st(w0: f64) -> BiquadCoeffs {
        let k = w0.tan() / 2.0;
        let norm = 1.0 / (1.0 + k);

        BiquadCoeffs {
            b0: k * norm,
            b1: k * norm,
            b2: 0.0,
            a1: (k - 1.0) * norm,
            a2: 0.0,
        }
    }

    /// Calculate coefficients for a first-order high pass.
    #[must_use]
    pub fn high_pass_1st(w0: f64) -> BiquadCoeffs {
        let k = w0.tan() / 2.0;
        let norm = 1.0 / (1.0 + k);

        BiquadCoeffs {
            b0: norm,
            b1: -norm,
            b2: 0.0,
            a1: (k - 1.0) * norm,
            a2: 0.0,
        }
    }

    /// Generate coefficients for a band based on its parameters.
    #[must_use]
    pub fn from_band(band: &BandParams, sample_rate: f64) -> Vec<BiquadCoeffs> {
        let w0 = Self::omega(band.frequency as f64, sample_rate);
        let gain = band.gain as f64;
        let q = band.q as f64;
        let order = band.slope.order();

        match band.shape {
            BandShape::Bell => vec![Self::peak(w0, gain, q)],
            BandShape::LowShelf => vec![Self::low_shelf(w0, gain, q)],
            BandShape::HighShelf => vec![Self::high_shelf(w0, gain, q)],
            BandShape::LowCut => Self::cascaded_high_pass(w0, q, order),
            BandShape::HighCut => Self::cascaded_low_pass(w0, q, order),
            BandShape::Notch => vec![Self::notch(w0, q)],
            BandShape::BandPass => vec![Self::band_pass(w0, q)],
            BandShape::TiltShelf => vec![Self::tilt_shelf(w0, gain, q)],
            BandShape::FlatTilt => vec![Self::tilt_shelf(w0, gain, 0.707)],
            BandShape::AllPass => vec![Self::all_pass(w0, q)],
        }
    }

    /// Generate cascaded high pass filters for steep slopes.
    fn cascaded_high_pass(w0: f64, q: f64, order: usize) -> Vec<BiquadCoeffs> {
        if order == 1 {
            return vec![Self::high_pass_1st(w0)];
        }

        let num_stages = (order + 1) / 2;
        let mut coeffs = Vec::with_capacity(num_stages);

        // Butterworth Q values for cascaded filters
        for i in 0..num_stages {
            let stage_q = if order % 2 == 1 && i == 0 {
                // First stage is first-order for odd orders
                continue;
            } else {
                // Butterworth pole angles
                let angle = PI * (2.0 * i as f64 + 1.0) / (2.0 * order as f64);
                1.0 / (2.0 * angle.cos())
            };
            coeffs.push(Self::high_pass(w0, stage_q * q));
        }

        if order % 2 == 1 {
            coeffs.insert(0, Self::high_pass_1st(w0));
        }

        coeffs
    }

    /// Generate cascaded low pass filters for steep slopes.
    fn cascaded_low_pass(w0: f64, q: f64, order: usize) -> Vec<BiquadCoeffs> {
        if order == 1 {
            return vec![Self::low_pass_1st(w0)];
        }

        let num_stages = (order + 1) / 2;
        let mut coeffs = Vec::with_capacity(num_stages);

        for i in 0..num_stages {
            let stage_q = if order % 2 == 1 && i == 0 {
                continue;
            } else {
                let angle = PI * (2.0 * i as f64 + 1.0) / (2.0 * order as f64);
                1.0 / (2.0 * angle.cos())
            };
            coeffs.push(Self::low_pass(w0, stage_q * q));
        }

        if order % 2 == 1 {
            coeffs.insert(0, Self::low_pass_1st(w0));
        }

        coeffs
    }
}

/// EQ frequency response calculator.
///
/// Calculates the magnitude response of a complete EQ curve
/// for display purposes.
pub struct FrequencyResponse {
    /// Sample rate in Hz.
    sample_rate: f64,
    /// Number of frequency points.
    num_points: usize,
    /// Frequency values (Hz).
    frequencies: Vec<f64>,
    /// Angular frequencies (radians).
    omegas: Vec<f64>,
}

impl FrequencyResponse {
    /// Create a new frequency response calculator.
    ///
    /// # Parameters
    /// - `sample_rate`: Sample rate in Hz
    /// - `num_points`: Number of frequency points (default 400)
    /// - `min_freq`: Minimum frequency (default 10 Hz)
    /// - `max_freq`: Maximum frequency (default 30 kHz)
    #[must_use]
    pub fn new(sample_rate: f64, num_points: usize, min_freq: f64, max_freq: f64) -> Self {
        let log_min = min_freq.log10();
        let log_max = max_freq.log10();

        let frequencies: Vec<f64> = (0..num_points)
            .map(|i| {
                let t = i as f64 / (num_points - 1) as f64;
                10.0_f64.powf(log_min + t * (log_max - log_min))
            })
            .collect();

        let omegas: Vec<f64> = frequencies
            .iter()
            .map(|&f| 2.0 * PI * f / sample_rate)
            .collect();

        Self {
            sample_rate,
            num_points,
            frequencies,
            omegas,
        }
    }

    /// Create with default settings (400 points, 10 Hz - 30 kHz).
    #[must_use]
    pub fn default_for_sample_rate(sample_rate: f64) -> Self {
        Self::new(sample_rate, 400, 10.0, 30000.0)
    }

    /// Get the frequency values.
    #[must_use]
    pub fn frequencies(&self) -> &[f64] {
        &self.frequencies
    }

    /// Calculate magnitude response for a single band in dB.
    #[must_use]
    pub fn band_response_db(&self, band: &BandParams) -> Vec<f64> {
        if !band.used || !band.enabled {
            return vec![0.0; self.num_points];
        }

        let coeffs = FilterDesign::from_band(band, self.sample_rate);

        self.omegas
            .iter()
            .map(|&w| {
                let mag_sq: f64 = coeffs.iter().map(|c| c.magnitude_squared_at(w)).product();
                10.0 * mag_sq.log10()
            })
            .collect()
    }

    /// Calculate combined magnitude response for all bands in dB.
    #[must_use]
    pub fn combined_response_db(&self, bands: &[BandParams]) -> Vec<f64> {
        let mut result = vec![0.0; self.num_points];

        for band in bands {
            if band.used && band.enabled {
                let band_response = self.band_response_db(band);
                for (i, db) in band_response.into_iter().enumerate() {
                    result[i] += db;
                }
            }
        }

        result
    }

    /// Convert frequency to X position (0.0 - 1.0, logarithmic).
    #[must_use]
    pub fn freq_to_x(&self, freq: f64) -> f64 {
        let log_min = self.frequencies[0].log10();
        let log_max = self.frequencies[self.num_points - 1].log10();
        (freq.log10() - log_min) / (log_max - log_min)
    }

    /// Convert X position to frequency.
    #[must_use]
    pub fn x_to_freq(&self, x: f64) -> f64 {
        let log_min = self.frequencies[0].log10();
        let log_max = self.frequencies[self.num_points - 1].log10();
        10.0_f64.powf(log_min + x * (log_max - log_min))
    }

    /// Convert dB to Y position (0.0 - 1.0).
    #[must_use]
    pub fn db_to_y(&self, db: f64, range_db: f64) -> f64 {
        0.5 - db / (2.0 * range_db)
    }

    /// Convert Y position to dB.
    #[must_use]
    pub fn y_to_db(&self, y: f64, range_db: f64) -> f64 {
        (0.5 - y) * 2.0 * range_db
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peak_filter_unity_at_zero_gain() {
        let w0 = FilterDesign::omega(1000.0, 48000.0);
        let coeffs = FilterDesign::peak(w0, 0.0, 1.0);

        // At zero gain, peak filter should be unity
        let mag = coeffs.magnitude_at(w0);
        assert!((mag - 1.0).abs() < 0.001);
    }

    #[test]
    fn low_pass_attenuates_high_frequencies() {
        let w0 = FilterDesign::omega(1000.0, 48000.0);
        let coeffs = FilterDesign::low_pass(w0, 0.707);

        // At cutoff, should be -3dB
        let mag_at_cutoff = coeffs.magnitude_at(w0);
        let db_at_cutoff = 20.0 * mag_at_cutoff.log10();
        assert!((db_at_cutoff - (-3.0)).abs() < 0.5);

        // Well above cutoff should be attenuated
        let w_high = FilterDesign::omega(10000.0, 48000.0);
        let mag_high = coeffs.magnitude_at(w_high);
        assert!(mag_high < 0.1);
    }

    #[test]
    fn frequency_response_logarithmic() {
        let fr = FrequencyResponse::default_for_sample_rate(48000.0);
        let freqs = fr.frequencies();

        // Should be logarithmically spaced
        assert!((freqs[0] - 10.0).abs() < 1.0);
        assert!((freqs[freqs.len() - 1] - 30000.0).abs() < 100.0);

        // Middle should be around 550 Hz (geometric mean)
        let mid_freq = freqs[freqs.len() / 2];
        let expected_mid = (10.0_f64 * 30000.0).sqrt();
        assert!((mid_freq - expected_mid).abs() / expected_mid < 0.1);
    }

    #[test]
    fn freq_to_x_inverse() {
        let fr = FrequencyResponse::default_for_sample_rate(48000.0);

        let freq = 1000.0;
        let x = fr.freq_to_x(freq);
        let freq_back = fr.x_to_freq(x);

        assert!((freq - freq_back).abs() < 1.0);
    }
}
