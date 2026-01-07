//! Embedded editor for REAPER's TCP/MCP inline FX UI.
//!
//! This module provides a CPU-rendered spectrum analyzer that renders directly
//! into REAPER's track control panel or mixer control panel.

use nih_plug::editor::embedded::{
    EmbedBitmap, EmbedContext, EmbedDrawInfo, EmbedSizeHints, EmbeddedEditor,
};
use std::sync::Arc;

use crate::audio::meter::MeterConsumer;
use crate::audio::spectrum::SpectrumConsumer;
use crate::{AmplitudeRange, SAPluginParams};

/// Colors for the embedded spectrum display (BGRA format).
mod colors {
    use super::EmbedBitmap;

    pub const BACKGROUND: u32 = EmbedBitmap::rgba(24, 24, 28, 255);
    pub const SPECTRUM_FILL: u32 = EmbedBitmap::rgba(60, 180, 100, 180);
    pub const SPECTRUM_LINE: u32 = EmbedBitmap::rgba(80, 220, 120, 255);
    pub const METER_LEFT: u32 = EmbedBitmap::rgba(80, 200, 120, 255);
    pub const METER_RIGHT: u32 = EmbedBitmap::rgba(60, 180, 100, 255);
    pub const METER_PEAK: u32 = EmbedBitmap::rgba(255, 100, 80, 255);
    pub const METER_BACKGROUND: u32 = EmbedBitmap::rgba(40, 40, 45, 255);
    pub const GRID_LINE: u32 = EmbedBitmap::rgba(50, 50, 55, 255);
}

/// Minimum and maximum frequencies for display (Hz).
const MIN_FREQ_HZ: f32 = 20.0;
const MAX_FREQ_HZ: f32 = 20000.0;

/// Width of the level meters on the right side.
const METER_WIDTH: i32 = 8;
const METER_GAP: i32 = 2;
const METER_TOTAL_WIDTH: i32 = METER_WIDTH * 2 + METER_GAP * 3;

/// Peak hold time in frames (approximate).
const PEAK_HOLD_FRAMES: u32 = 30;

/// Embedded spectrum analyzer editor.
pub struct SpectrumEmbeddedEditor {
    /// Plugin parameters for reading amplitude range.
    params: Arc<SAPluginParams>,
    /// Spectrum data consumer (reads from audio thread).
    spectrum_consumer: SpectrumConsumer,
    /// Meter data consumer (reads peak levels).
    meter_consumer: MeterConsumer,
    /// Sample rate for frequency calculations.
    sample_rate: Arc<atomic_float::AtomicF32>,
    /// Peak hold values [left, right].
    peak_hold: std::sync::Mutex<PeakHoldState>,
}

/// State for peak hold indicators.
struct PeakHoldState {
    left_peak: f32,
    right_peak: f32,
    left_hold_frames: u32,
    right_hold_frames: u32,
}

impl Default for PeakHoldState {
    fn default() -> Self {
        Self {
            left_peak: -120.0,
            right_peak: -120.0,
            left_hold_frames: 0,
            right_hold_frames: 0,
        }
    }
}

impl SpectrumEmbeddedEditor {
    /// Create a new embedded editor.
    pub fn new(
        params: Arc<SAPluginParams>,
        spectrum_consumer: SpectrumConsumer,
        meter_consumer: MeterConsumer,
        sample_rate: Arc<atomic_float::AtomicF32>,
    ) -> Self {
        Self {
            params,
            spectrum_consumer,
            meter_consumer,
            sample_rate,
            peak_hold: std::sync::Mutex::new(PeakHoldState::default()),
        }
    }

    /// Map a frequency (Hz) to an x-coordinate using logarithmic scaling.
    fn freq_to_x(&self, freq: f32, width: i32) -> i32 {
        if freq <= MIN_FREQ_HZ {
            return 0;
        }
        if freq >= MAX_FREQ_HZ {
            return width - 1;
        }

        let log_min = MIN_FREQ_HZ.ln();
        let log_max = MAX_FREQ_HZ.ln();
        let log_freq = freq.ln();

        let normalized = (log_freq - log_min) / (log_max - log_min);
        (normalized * (width - 1) as f32) as i32
    }

    /// Map a dB value to a y-coordinate.
    fn db_to_y(&self, db: f32, height: i32, db_range: (f32, f32)) -> i32 {
        let (min_db, max_db) = db_range;
        let clamped = db.clamp(min_db, max_db);
        let normalized = (clamped - min_db) / (max_db - min_db);
        // Invert: higher dB = lower y (towards top)
        ((1.0 - normalized) * (height - 1) as f32) as i32
    }

    /// Draw the spectrum curve.
    fn draw_spectrum(
        &self,
        bitmap: &mut EmbedBitmap<'_>,
        spectrum: &[f32],
        spectrum_width: i32,
        height: i32,
        db_range: (f32, f32),
    ) {
        if spectrum.is_empty() || spectrum_width <= 0 || height <= 0 {
            return;
        }

        let sample_rate = self.sample_rate.load(std::sync::atomic::Ordering::Relaxed);
        let num_bins = spectrum.len();

        // FFT size is (num_bins - 1) * 2 for real FFT
        let fft_size = (num_bins - 1) * 2;
        let bin_freq_resolution = sample_rate / fft_size as f32;

        // Draw filled area and line
        let mut prev_x = -1i32;
        let mut prev_y = height;

        for x in 0..spectrum_width {
            // Map x to frequency (logarithmic)
            let normalized_x = x as f32 / (spectrum_width - 1).max(1) as f32;
            let log_min = MIN_FREQ_HZ.ln();
            let log_max = MAX_FREQ_HZ.ln();
            let log_freq = log_min + normalized_x * (log_max - log_min);
            let freq = log_freq.exp();

            // Map frequency to bin index
            let bin_index = (freq / bin_freq_resolution) as usize;
            let bin_index = bin_index.min(num_bins - 1);

            // Get dB value (with interpolation for smoother display)
            let db = if bin_index + 1 < num_bins {
                let frac = (freq / bin_freq_resolution) - bin_index as f32;
                spectrum[bin_index] * (1.0 - frac) + spectrum[bin_index + 1] * frac
            } else {
                spectrum[bin_index]
            };

            let y = self.db_to_y(db, height, db_range);

            // Draw vertical fill from bottom to current point
            if y < height {
                for fill_y in y..height {
                    bitmap.set_pixel(x as u32, fill_y as u32, colors::SPECTRUM_FILL);
                }
            }

            // Draw line connecting to previous point
            if prev_x >= 0 {
                self.draw_line(bitmap, prev_x, prev_y, x, y, colors::SPECTRUM_LINE);
            }

            prev_x = x;
            prev_y = y;
        }
    }

    /// Draw a line between two points (Bresenham's algorithm).
    fn draw_line(
        &self,
        bitmap: &mut EmbedBitmap<'_>,
        x0: i32,
        y0: i32,
        x1: i32,
        y1: i32,
        color: u32,
    ) {
        let dx = (x1 - x0).abs();
        let dy = -(y1 - y0).abs();
        let sx = if x0 < x1 { 1 } else { -1 };
        let sy = if y0 < y1 { 1 } else { -1 };
        let mut err = dx + dy;

        let mut x = x0;
        let mut y = y0;

        loop {
            if x >= 0 && x < bitmap.width as i32 && y >= 0 && y < bitmap.height as i32 {
                bitmap.set_pixel(x as u32, y as u32, color);
            }

            if x == x1 && y == y1 {
                break;
            }

            let e2 = 2 * err;
            if e2 >= dy {
                if x == x1 {
                    break;
                }
                err += dy;
                x += sx;
            }
            if e2 <= dx {
                if y == y1 {
                    break;
                }
                err += dx;
                y += sy;
            }
        }
    }

    /// Draw level meters on the right side.
    fn draw_meters(&self, bitmap: &mut EmbedBitmap<'_>, x_offset: i32, height: i32) {
        let db_range = self.params.range.value().to_db_range();

        // Get current peak levels (using smoothed values)
        // First update the internal smoothing state, then get the values
        self.meter_consumer.update();
        let (left_db, right_db) = self.meter_consumer.get_smoothed_levels_or_silence();

        // Update peak hold
        let (left_peak, right_peak) = {
            let mut peak_state = self.peak_hold.lock().unwrap();

            // Update left peak
            if left_db >= peak_state.left_peak {
                peak_state.left_peak = left_db;
                peak_state.left_hold_frames = PEAK_HOLD_FRAMES;
            } else if peak_state.left_hold_frames > 0 {
                peak_state.left_hold_frames -= 1;
            } else {
                peak_state.left_peak = (peak_state.left_peak - 1.0).max(db_range.0);
            }

            // Update right peak
            if right_db >= peak_state.right_peak {
                peak_state.right_peak = right_db;
                peak_state.right_hold_frames = PEAK_HOLD_FRAMES;
            } else if peak_state.right_hold_frames > 0 {
                peak_state.right_hold_frames -= 1;
            } else {
                peak_state.right_peak = (peak_state.right_peak - 1.0).max(db_range.0);
            }

            (peak_state.left_peak, peak_state.right_peak)
        };

        let left_x = x_offset + METER_GAP;
        let right_x = left_x + METER_WIDTH + METER_GAP;

        // Draw meter backgrounds
        bitmap.fill_rect(left_x, 0, METER_WIDTH, height, colors::METER_BACKGROUND);
        bitmap.fill_rect(right_x, 0, METER_WIDTH, height, colors::METER_BACKGROUND);

        // Draw meter fills
        let left_y = self.db_to_y(left_db, height, db_range);
        let right_y = self.db_to_y(right_db, height, db_range);

        if left_y < height {
            bitmap.fill_rect(left_x, left_y, METER_WIDTH, height - left_y, colors::METER_LEFT);
        }
        if right_y < height {
            bitmap.fill_rect(
                right_x,
                right_y,
                METER_WIDTH,
                height - right_y,
                colors::METER_RIGHT,
            );
        }

        // Draw peak indicators
        let left_peak_y = self.db_to_y(left_peak, height, db_range);
        let right_peak_y = self.db_to_y(right_peak, height, db_range);

        if left_peak_y >= 0 && left_peak_y < height {
            bitmap.hline(left_x, left_peak_y, METER_WIDTH, colors::METER_PEAK);
        }
        if right_peak_y >= 0 && right_peak_y < height {
            bitmap.hline(right_x, right_peak_y, METER_WIDTH, colors::METER_PEAK);
        }
    }

    /// Draw grid lines for frequency and dB reference.
    fn draw_grid(&self, bitmap: &mut EmbedBitmap<'_>, spectrum_width: i32, height: i32) {
        // Draw horizontal dB grid lines at common intervals
        let db_range = self.params.range.value().to_db_range();
        let db_step = match self.params.range.value() {
            AmplitudeRange::Range60dB => 12.0,
            AmplitudeRange::Range90dB => 18.0,
            AmplitudeRange::Range120dB => 24.0,
        };

        let mut db = db_range.1; // Start from top (0 dB)
        while db >= db_range.0 {
            let y = self.db_to_y(db, height, db_range);
            if y >= 0 && y < height {
                bitmap.hline(0, y, spectrum_width, colors::GRID_LINE);
            }
            db -= db_step;
        }

        // Draw vertical frequency grid lines at decade intervals
        let freqs = [100.0, 1000.0, 10000.0];
        for freq in freqs {
            let x = self.freq_to_x(freq, spectrum_width);
            if x > 0 && x < spectrum_width {
                bitmap.vline(x, 0, height, colors::GRID_LINE);
            }
        }
    }
}

impl EmbeddedEditor for SpectrumEmbeddedEditor {
    fn is_available(&self) -> bool {
        true
    }

    fn size_hints(&self, _context: EmbedContext, _dpi: f32) -> Option<EmbedSizeHints> {
        Some(EmbedSizeHints {
            preferred_aspect: 2.0, // Prefer wider than tall
            minimum_aspect: 1.0,
            min_width: 80,
            min_height: 40,
            max_width: 400,
            max_height: 200,
        })
    }

    fn paint(&self, bitmap: &mut EmbedBitmap<'_>, _info: &EmbedDrawInfo) -> bool {
        let width = bitmap.width as i32;
        let height = bitmap.height as i32;

        if width <= 0 || height <= 0 {
            return false;
        }

        // Clear background
        bitmap.clear(colors::BACKGROUND);

        // Calculate spectrum display area (leave room for meters on right)
        let spectrum_width = (width - METER_TOTAL_WIDTH).max(10);

        // Draw grid first (behind spectrum)
        self.draw_grid(bitmap, spectrum_width, height);

        // Get spectrum data
        if let Ok(spectrum) = self.spectrum_consumer.read() {
            let db_range = self.params.range.value().to_db_range();
            self.draw_spectrum(bitmap, &spectrum, spectrum_width, height, db_range);
        }

        // Draw meters on the right
        self.draw_meters(bitmap, spectrum_width, height);

        true
    }
}
