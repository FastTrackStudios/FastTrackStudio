//! Measurement pass for multi-pass chart layout.
//!
//! This module implements Pass 1 of the Measure → Layout → Paint pipeline.
//! It pre-computes exact sizes of all elements before layout, replacing
//! the previous estimation-based approach.
//!
//! # Why Measurement Caching?
//!
//! The old approach estimated chord widths multiple times:
//! 1. `estimate_measure_content_weight()` - predict relative weight
//! 2. `compute_minimum_measure_width()` - predict collision space
//! 3. `compute_chord_min_widths()` - predict segment minimums
//! 4. Render-time measurement in `layout_harmony()`
//!
//! This caused issues:
//! - Same chord measured 3-4x with different code paths
//! - Estimates didn't match actual rendered widths
//! - Post-hoc collision fixes broke barline positions
//!
//! The new approach measures everything once, caches it, and uses
//! real measurements throughout layout and rendering.

use std::collections::HashMap;

use crate::chart::types::Measure;
use crate::engraver::layout::text_metrics::TextFontMetrics;
use crate::engraver::layout::tlayout::HarmonyStyle;

/// Cache for measured element sizes.
///
/// This cache is session-scoped: created fresh for each `layout_chart()` call,
/// used throughout the pass, then dropped. No staleness issues.
///
/// # Cache Keys
///
/// - Chord widths: `(symbol, font_size_quantized)` → width in points
/// - Font size is quantized to 0.1pt precision (multiply by 10, cast to i32)
#[derive(Debug, Default)]
pub struct MeasurementCache {
    /// Chord symbol widths: (symbol, quantized_font_size) → width in points
    chord_widths: HashMap<(String, i32), f64>,
}

impl MeasurementCache {
    /// Create a new empty measurement cache.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Quantize font size for use as a hash key.
    /// Multiplies by 10 to preserve 0.1pt precision.
    fn quantize_font_size(font_size: f64) -> i32 {
        (font_size * 10.0).round() as i32
    }

    /// Measure a chord symbol's width, returning cached value if available.
    ///
    /// # Arguments
    /// * `symbol` - The chord symbol string (e.g., "Cmaj7", "F#m7b5")
    /// * `font_size` - Font size in points
    /// * `metrics` - Text font metrics for measurement
    ///
    /// # Returns
    /// Width in points
    pub fn measure_chord_width(
        &mut self,
        symbol: &str,
        font_size: f64,
        metrics: &TextFontMetrics,
    ) -> f64 {
        let quantized_size = Self::quantize_font_size(font_size);
        let key = (symbol.to_string(), quantized_size);

        *self
            .chord_widths
            .entry(key)
            .or_insert_with(|| metrics.horizontal_advance(symbol, font_size))
    }

    /// Get a cached chord width without measuring.
    /// Returns None if not cached.
    #[must_use]
    pub fn get_chord_width(&self, symbol: &str, font_size: f64) -> Option<f64> {
        let quantized_size = Self::quantize_font_size(font_size);
        let key = (symbol.to_string(), quantized_size);
        self.chord_widths.get(&key).copied()
    }

    /// Number of cached entries (for debugging/stats).
    #[must_use]
    pub fn len(&self) -> usize {
        self.chord_widths.len()
    }

    /// Whether the cache is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.chord_widths.is_empty()
    }
}

/// Measurement data for a single measure.
///
/// Contains pre-computed chord width measurements. This is the output of Pass 1
/// and provides accurate sizing data for the layout pass.
///
/// Note: Rhythm-based measurements (segment count, triplet detection) are intentionally
/// NOT included here. The rhythm builder already handles those correctly during layout,
/// and duplicating that logic would be error-prone. The measure pass focuses exclusively
/// on chord symbol width caching.
#[derive(Debug, Clone)]
pub struct MeasureMeasurements {
    /// Actual widths of each visible chord symbol (in points).
    /// Indexed by chord position within the measure.
    pub chord_widths: Vec<f64>,

    /// Total minimum width needed for this measure (in points).
    /// Calculated from actual chord widths + minimum gaps.
    pub min_width: f64,

    /// Number of visible chords in this measure.
    pub visible_chord_count: usize,
}

impl Default for MeasureMeasurements {
    fn default() -> Self {
        Self {
            chord_widths: Vec::new(),
            min_width: 0.0,
            visible_chord_count: 0,
        }
    }
}

/// Measurements for an entire chart.
///
/// This is the result of Pass 1 (Measure pass), containing pre-computed
/// measurements for all measures across all sections.
#[derive(Debug, Default)]
pub struct ChartMeasurements {
    /// Measurements for each measure, in order.
    /// Index corresponds to global measure index across all sections.
    pub measures: Vec<MeasureMeasurements>,
}

impl ChartMeasurements {
    /// Create empty chart measurements.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add measurements for a measure.
    pub fn push(&mut self, measurements: MeasureMeasurements) {
        self.measures.push(measurements);
    }

    /// Get measurements for a specific measure index.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&MeasureMeasurements> {
        self.measures.get(index)
    }

    /// Total number of measures.
    #[must_use]
    pub fn len(&self) -> usize {
        self.measures.len()
    }

    /// Whether there are no measurements.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.measures.is_empty()
    }
}

/// Check if a chord symbol is a placeholder (space/rest).
#[inline]
fn is_placeholder(symbol: &str) -> bool {
    symbol.is_empty() || symbol == "s" || symbol == "r"
}

/// Measure a single measure's content.
///
/// This replaces the estimation logic in `compute_minimum_measure_width()`,
/// `compute_chord_min_widths()`, and parts of `estimate_measure_content_weight()`.
///
/// # Arguments
/// * `measure` - The measure to measure
/// * `style` - Harmony style (provides font size)
/// * `cache` - Measurement cache to use/populate
///
/// # Returns
/// Measurement data for the measure
pub fn measure_measure(
    measure: &Measure,
    style: &HarmonyStyle,
    cache: &mut MeasurementCache,
) -> MeasureMeasurements {
    let text_metrics = match style.text_font_metrics.as_ref() {
        Some(m) => m,
        None => {
            // No metrics available - return defaults
            return MeasureMeasurements::default();
        }
    };

    let font_size = style.root_size;
    let min_gap = font_size * 0.5; // Minimum gap between chord symbols

    // Collect visible chord widths
    let mut chord_widths = Vec::new();
    let mut visible_chord_count = 0;

    for chord in &measure.chords {
        if !is_placeholder(&chord.full_symbol) {
            let width = cache.measure_chord_width(&chord.full_symbol, font_size, text_metrics);
            // Apply minimum width floor (same as old code)
            let width = width.max(font_size * 1.5);
            chord_widths.push(width);
            visible_chord_count += 1;
        }
    }

    // Calculate minimum width from actual measurements
    // This replaces compute_minimum_measure_width()
    let min_width = if chord_widths.len() < 2 {
        // No collision possible with 0 or 1 chord
        0.0
    } else {
        // Sum all chord widths + gaps between them
        let total_chord_width: f64 = chord_widths.iter().sum();
        let total_gaps = (chord_widths.len() - 1) as f64 * min_gap;
        total_chord_width + total_gaps
    };

    MeasureMeasurements {
        chord_widths,
        min_width,
        visible_chord_count,
    }
}

/// Measure all content in a chart.
///
/// This is the main entry point for Pass 1 (Measure pass).
/// Pre-measures all elements in the chart and returns cached measurements.
///
/// # Arguments
/// * `sections` - Iterator over chart sections with measures
/// * `style` - Harmony style for chord symbols
/// * `cache` - Measurement cache to populate
///
/// # Returns
/// Complete chart measurements
pub fn measure_chart<'a, I, M>(
    sections: I,
    style: &HarmonyStyle,
    cache: &mut MeasurementCache,
) -> ChartMeasurements
where
    I: Iterator<Item = M>,
    M: AsRef<[Measure]>,
{
    let mut measurements = ChartMeasurements::new();

    for section_measures in sections {
        for measure in section_measures.as_ref() {
            let measure_data = measure_measure(measure, style, cache);
            measurements.push(measure_data);
        }
    }

    measurements
}

/// Calculate measure content weight from pre-computed measurements.
///
/// This provides a base weight based on chord complexity. The rhythm builder
/// may add additional weight during layout for triplets and complex rhythms.
///
/// # Arguments
/// * `measurements` - Pre-computed measurements for this measure
/// * `segment_count` - Number of rhythm segments (from rhythm builder)
/// * `triplet_count` - Number of triplet elements (from rhythm builder)
///
/// # Returns
/// Weight value for spring-based width distribution (typically 0.5-4.0)
#[must_use]
pub fn compute_measure_weight(
    measurements: &MeasureMeasurements,
    segment_count: usize,
    triplet_count: usize,
) -> f64 {
    const TRIPLET_BONUS: f64 = 0.15; // Extra weight per triplet element
    const CHORD_COLLISION_BONUS: f64 = 0.25; // Extra weight when chords might collide

    // Base weight from segment count (4 segments = 1.0 weight for standard 4/4)
    let segment_weight = segment_count as f64 / 4.0;

    // Triplet complexity bonus
    let triplet_bonus = triplet_count as f64 * TRIPLET_BONUS;

    // Collision potential bonus (when 2+ visible chords exist)
    let collision_bonus = if measurements.visible_chord_count >= 2 {
        // More chords = more potential for collisions
        (measurements.visible_chord_count - 1) as f64 * CHORD_COLLISION_BONUS * 0.5
    } else {
        0.0
    };

    // Combine weights and clamp to reasonable range
    let weight = segment_weight.max(1.0) + triplet_bonus + collision_bonus;
    weight.clamp(0.5, 4.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    // Create a minimal test HarmonyStyle
    fn make_test_style() -> HarmonyStyle {
        let font_data = Arc::new(crate::engraver::fonts::EMPTY_FONT_DATA_FOR_TESTS.to_vec());
        HarmonyStyle::default().with_text_font_metrics(TextFontMetrics::new(font_data))
    }

    #[test]
    fn test_measurement_cache_basic() {
        let mut cache = MeasurementCache::new();
        let style = make_test_style();
        let metrics = style.text_font_metrics.as_ref().unwrap();

        // First call measures
        let width1 = cache.measure_chord_width("Cmaj7", 14.0, metrics);
        assert!(width1 > 0.0);

        // Second call returns cached value
        let width2 = cache.measure_chord_width("Cmaj7", 14.0, metrics);
        assert!((width1 - width2).abs() < 0.001);

        // Different font size = different cache entry
        let width3 = cache.measure_chord_width("Cmaj7", 12.0, metrics);
        assert!((width1 - width3).abs() > 0.1);

        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn test_is_placeholder() {
        assert!(is_placeholder(""));
        assert!(is_placeholder("s"));
        assert!(is_placeholder("r"));
        assert!(!is_placeholder("C"));
        assert!(!is_placeholder("Am7"));
    }

    #[test]
    fn test_compute_measure_weight() {
        // Simple measure with 4 segments
        let measurements = MeasureMeasurements {
            chord_widths: vec![50.0],
            min_width: 0.0,
            visible_chord_count: 1,
        };
        let weight = compute_measure_weight(&measurements, 4, 0);
        assert!((weight - 1.0).abs() < 0.01); // 4/4 = 1.0 base weight

        // Measure with triplets
        let measurements_triplet = MeasureMeasurements {
            chord_widths: vec![50.0, 50.0],
            min_width: 100.0,
            visible_chord_count: 2,
        };
        let weight_triplet = compute_measure_weight(&measurements_triplet, 6, 3);
        assert!(weight_triplet > weight); // Should have triplet bonus
    }

    #[test]
    fn test_chart_measurements() {
        let mut measurements = ChartMeasurements::new();
        assert!(measurements.is_empty());

        measurements.push(MeasureMeasurements::default());
        measurements.push(MeasureMeasurements::default());

        assert_eq!(measurements.len(), 2);
        assert!(measurements.get(0).is_some());
        assert!(measurements.get(2).is_none());
    }
}
