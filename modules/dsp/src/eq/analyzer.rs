//! Spectrum analyzer for EQ visualization.
//!
//! Provides FFT-based spectrum analysis with:
//! - Pre/post EQ spectrum display
//! - Configurable resolution and speed
//! - Tilt compensation
//! - Collision detection between spectrum and EQ curve

use realfft::{RealFftPlanner, RealToComplex};
use std::sync::Arc;

use super::params::{AnalyzerParams, AnalyzerResolution, AnalyzerSpeed, AnalyzerTilt};

/// FFT analyzer for spectrum display.
pub struct SpectrumAnalyzer {
    /// FFT size.
    fft_size: usize,
    /// Sample rate.
    sample_rate: f64,
    /// FFT planner.
    fft: Arc<dyn RealToComplex<f64>>,
    /// Input buffer.
    input_buffer: Vec<f64>,
    /// FFT output buffer.
    output_buffer: Vec<num_complex::Complex<f64>>,
    /// Magnitude buffer (smoothed).
    magnitudes: Vec<f64>,
    /// Decay factor for smoothing.
    decay: f64,
    /// Tilt compensation in dB/octave.
    tilt_db_oct: f64,
    /// Minimum display dB.
    min_db: f64,
    /// Maximum display dB.
    max_db: f64,
}

impl SpectrumAnalyzer {
    /// Create a new spectrum analyzer.
    #[must_use]
    pub fn new(sample_rate: f64, fft_size: usize) -> Self {
        let mut planner = RealFftPlanner::<f64>::new();
        let fft = planner.plan_fft_forward(fft_size);

        let num_bins = fft_size / 2 + 1;

        Self {
            fft_size,
            sample_rate,
            fft,
            input_buffer: vec![0.0; fft_size],
            output_buffer: vec![num_complex::Complex::default(); num_bins],
            magnitudes: vec![0.0; num_bins],
            decay: 0.9, // Default medium speed
            tilt_db_oct: 4.5,
            min_db: -90.0,
            max_db: 0.0,
        }
    }

    /// Create from analyzer parameters.
    #[must_use]
    pub fn from_params(params: &AnalyzerParams, sample_rate: f64) -> Self {
        let fft_size = match params.resolution {
            AnalyzerResolution::Low => 2048,
            AnalyzerResolution::Medium => 4096,
            AnalyzerResolution::High => 8192,
            AnalyzerResolution::Maximum => 16384,
        };

        let mut analyzer = Self::new(sample_rate, fft_size);
        analyzer.set_speed(params.speed);
        analyzer.set_tilt(params.tilt);
        analyzer
    }

    /// Set analyzer speed (decay factor).
    pub fn set_speed(&mut self, speed: AnalyzerSpeed) {
        self.decay = match speed {
            AnalyzerSpeed::VeryFast => 0.85,
            AnalyzerSpeed::Fast => 0.88,
            AnalyzerSpeed::Medium => 0.92,
            AnalyzerSpeed::Slow => 0.95,
            AnalyzerSpeed::VerySlow => 0.98,
        };
    }

    /// Set tilt compensation.
    pub fn set_tilt(&mut self, tilt: AnalyzerTilt) {
        self.tilt_db_oct = tilt.db_per_octave() as f64;
    }

    /// Process a block of audio samples.
    pub fn process(&mut self, samples: &[f64]) {
        // Simple overlap-add: just use the last fft_size samples
        let start = samples.len().saturating_sub(self.fft_size);
        let input_slice = &samples[start..];

        // Copy and apply window
        for (i, &sample) in input_slice.iter().enumerate() {
            let window = 0.5 * (1.0 - (2.0 * std::f64::consts::PI * i as f64 / self.fft_size as f64).cos());
            self.input_buffer[i] = sample * window;
        }

        // Perform FFT
        let _ = self.fft.process(&mut self.input_buffer, &mut self.output_buffer);

        // Update magnitudes with decay
        let norm = 2.0 / self.fft_size as f64;
        for (i, complex) in self.output_buffer.iter().enumerate() {
            let mag = (complex.re * complex.re + complex.im * complex.im).sqrt() * norm;
            self.magnitudes[i] = self.magnitudes[i] * self.decay + mag * (1.0 - self.decay);
        }
    }

    /// Get magnitude at a specific bin in dB.
    #[must_use]
    pub fn magnitude_db(&self, bin: usize) -> f64 {
        if bin >= self.magnitudes.len() {
            return self.min_db;
        }

        let mag = self.magnitudes[bin];
        if mag < 1e-10 {
            return self.min_db;
        }

        let db = 20.0 * mag.log10();

        // Apply tilt compensation
        if self.tilt_db_oct.abs() > 0.01 {
            let freq = self.bin_to_freq(bin);
            let tilt = self.tilt_db_oct * (freq / 1000.0).log2();
            db + tilt
        } else {
            db
        }
    }

    /// Convert bin index to frequency.
    #[must_use]
    pub fn bin_to_freq(&self, bin: usize) -> f64 {
        bin as f64 * self.sample_rate / self.fft_size as f64
    }

    /// Convert frequency to bin index.
    #[must_use]
    pub fn freq_to_bin(&self, freq: f64) -> usize {
        (freq * self.fft_size as f64 / self.sample_rate).round() as usize
    }

    /// Get the number of bins.
    #[must_use]
    pub fn num_bins(&self) -> usize {
        self.magnitudes.len()
    }

    /// Get magnitudes for display (logarithmically resampled).
    ///
    /// Returns magnitudes in dB at logarithmically spaced frequencies.
    #[must_use]
    pub fn get_display_magnitudes(&self, num_points: usize, min_freq: f64, max_freq: f64) -> Vec<f64> {
        let log_min = min_freq.log10();
        let log_max = max_freq.log10();

        (0..num_points)
            .map(|i| {
                let t = i as f64 / (num_points - 1) as f64;
                let freq = 10.0_f64.powf(log_min + t * (log_max - log_min));
                let bin = self.freq_to_bin(freq);
                self.magnitude_db(bin)
            })
            .collect()
    }

    /// Reset the analyzer (clear accumulated magnitudes).
    pub fn reset(&mut self) {
        for mag in &mut self.magnitudes {
            *mag = 0.0;
        }
    }
}

/// Collision detector between spectrum and EQ curve.
pub struct CollisionDetector {
    /// Threshold for collision detection in dB.
    threshold_db: f64,
}

impl CollisionDetector {
    /// Create a new collision detector.
    #[must_use]
    pub fn new(threshold_db: f64) -> Self {
        Self { threshold_db }
    }

    /// Detect collisions between spectrum and EQ curve.
    ///
    /// Returns indices where the spectrum is within threshold_db of the EQ curve.
    #[must_use]
    pub fn detect(&self, spectrum_db: &[f64], eq_curve_db: &[f64]) -> Vec<usize> {
        spectrum_db
            .iter()
            .zip(eq_curve_db.iter())
            .enumerate()
            .filter_map(|(i, (&spec, &eq))| {
                if (spec - eq).abs() < self.threshold_db {
                    Some(i)
                } else {
                    None
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analyzer_creation() {
        let analyzer = SpectrumAnalyzer::new(48000.0, 4096);
        assert_eq!(analyzer.num_bins(), 2049);
    }

    #[test]
    fn bin_freq_conversion() {
        let analyzer = SpectrumAnalyzer::new(48000.0, 4096);

        // DC bin
        assert!((analyzer.bin_to_freq(0) - 0.0).abs() < 0.1);

        // Nyquist bin
        let nyquist_bin = analyzer.num_bins() - 1;
        assert!((analyzer.bin_to_freq(nyquist_bin) - 24000.0).abs() < 100.0);

        // Round trip
        let freq = 1000.0;
        let bin = analyzer.freq_to_bin(freq);
        let freq_back = analyzer.bin_to_freq(bin);
        assert!((freq - freq_back).abs() < 20.0);
    }
}
