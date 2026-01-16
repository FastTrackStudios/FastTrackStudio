//! Chart Layout Engine
//!
//! Converts Keyflow Chart data to engraver SceneNode trees for rendering.
//! Supports both paginated (MuseScore-style) and continuous scroll modes.

use std::sync::Arc;

use engraver::layout::context::LayoutContext;
use engraver::layout::orchestrator::{PageLayout, PageMargins, SystemLayout};
use engraver::layout::segment::SegmentType;
use engraver::layout::text_metrics::TextFontMetrics;
use engraver::layout::tlayout::{
    layout_clef, layout_harmony, layout_margin_label, layout_tie, layout_timesig, parse_chord,
    rehearsal_themes, BarlineType, ClefParams, ClefType, HarmonyParams, HarmonyStyle,
    MarginLabelParams, NoteHeadType, SlurDirection, SlurEndpoint, SlurTieConfig, TimeSigParams,
    TimeSigType,
};
use engraver::notation::{Duration, MeasureBuilder, MeasureScene};
use engraver::scene::id::{ElementType, SemanticId};
use engraver::scene::node::{metadata_keys, SceneNode};
use engraver::scene::paint::PaintCommand;
use engraver::style::MStyle;
use keyflow::sections::SectionType;
use keyflow::{Chart, ChartPosition, SourceLink};
use kurbo::{Affine, Rect};
use vello::peniko::Color;

/// Layout mode for chart rendering.
#[derive(Debug, Clone)]
pub enum LayoutMode {
    /// Paginated layout with discrete pages and page breaks.
    Paginated {
        page_width: f64,
        page_height: f64,
    },
    /// Continuous scroll with unbounded vertical layout.
    ContinuousScroll {
        width: f64,
    },
}

impl Default for LayoutMode {
    fn default() -> Self {
        // Default to Letter size pages
        Self::Paginated {
            page_width: 612.0,  // 8.5" in points
            page_height: 792.0, // 11" in points
        }
    }
}

/// Result of chart layout calculation.
#[derive(Debug, Clone)]
pub struct ChartLayoutResult {
    /// Root scene node containing all rendered content.
    pub scene: SceneNode,
    /// Page layouts (for paginated mode).
    pub pages: Vec<PageLayout>,
    /// Total height of content (for continuous scroll).
    pub total_height: f64,
    /// Total width of content.
    pub total_width: f64,
}

impl ChartLayoutResult {
    /// Get layout metrics for a specific page.
    ///
    /// Returns detailed spacing information for debugging and verification.
    pub fn page_metrics(&self, page_number: u32) -> Option<PageLayoutMetrics> {
        let page = self.pages.iter().find(|p| p.number == page_number)?;
        Some(PageLayoutMetrics::from_page(page))
    }

    /// Get layout metrics for all pages.
    pub fn all_page_metrics(&self) -> Vec<PageLayoutMetrics> {
        self.pages.iter().map(PageLayoutMetrics::from_page).collect()
    }
}

/// Layout metrics for a single page - used for debugging and verification.
///
/// Similar to LilyPond's debug mode, this provides detailed information about
/// system placement on a page.
#[derive(Debug, Clone)]
pub struct PageLayoutMetrics {
    /// Page number (1-indexed).
    pub page_number: u32,
    /// Page dimensions.
    pub page_width: f64,
    pub page_height: f64,
    /// Number of systems on this page.
    pub system_count: usize,
    /// Y positions of each system (from top of page).
    pub system_y_positions: Vec<f64>,
    /// Spacing between consecutive systems.
    pub inter_system_spacing: Vec<f64>,
    /// Distance from top margin to first system.
    pub top_margin_to_first_system: f64,
    /// Distance from last system bottom to page bottom.
    pub last_system_to_bottom: f64,
    /// Total content height (all systems + spacing).
    pub content_height: f64,
    /// Available content height (page height - top margin - bottom margin).
    pub available_height: f64,
    /// Page margins.
    pub margins: PageMargins,
}

impl PageLayoutMetrics {
    /// Create metrics from a PageLayout.
    pub fn from_page(page: &PageLayout) -> Self {
        let system_count = page.systems.len();
        let system_y_positions: Vec<f64> = page.systems.iter().map(|s| s.y).collect();

        // Calculate inter-system spacing
        let inter_system_spacing: Vec<f64> = if system_count > 1 {
            page.systems.windows(2)
                .map(|pair| {
                    let first_bottom = pair[0].y + pair[0].height;
                    let second_top = pair[1].y;
                    second_top - first_bottom
                })
                .collect()
        } else {
            Vec::new()
        };

        // Calculate distances
        let top_margin_to_first_system = if let Some(first) = page.systems.first() {
            first.y - page.margins.top
        } else {
            0.0
        };

        let last_system_to_bottom = if let Some(last) = page.systems.last() {
            let last_bottom = last.y + last.height;
            page.height - page.margins.bottom - last_bottom
        } else {
            page.height - page.margins.top - page.margins.bottom
        };

        let content_height = if let (Some(first), Some(last)) = (page.systems.first(), page.systems.last()) {
            (last.y + last.height) - first.y
        } else {
            0.0
        };

        let available_height = page.height - page.margins.top - page.margins.bottom;

        Self {
            page_number: page.number,
            page_width: page.width,
            page_height: page.height,
            system_count,
            system_y_positions,
            inter_system_spacing,
            top_margin_to_first_system,
            last_system_to_bottom,
            content_height,
            available_height,
            margins: page.margins,
        }
    }

    /// Print a debug summary of the page layout (similar to LilyPond debug mode).
    pub fn print_debug(&self) {
        println!("=== Page {} Layout Metrics ===", self.page_number);
        println!("Page size: {:.1} × {:.1} points", self.page_width, self.page_height);
        println!("Margins: top={:.1}, bottom={:.1}, left={:.1}, right={:.1}",
            self.margins.top, self.margins.bottom, self.margins.left, self.margins.right);
        println!("Available height: {:.1} points", self.available_height);
        println!("Systems: {}", self.system_count);
        println!();

        if !self.system_y_positions.is_empty() {
            println!("System positions (from page top):");
            for (i, y) in self.system_y_positions.iter().enumerate() {
                println!("  System {}: y={:.1}", i + 1, y);
            }
            println!();
        }

        if !self.inter_system_spacing.is_empty() {
            println!("Inter-system spacing:");
            for (i, spacing) in self.inter_system_spacing.iter().enumerate() {
                println!("  Between {} and {}: {:.1} points", i + 1, i + 2, spacing);
            }
            println!();
        }

        println!("Top margin to first system: {:.1} points", self.top_margin_to_first_system);
        println!("Last system to bottom: {:.1} points", self.last_system_to_bottom);
        println!("Total content height: {:.1} points", self.content_height);
        println!("Remaining space: {:.1} points", self.available_height - self.content_height);
        println!();
    }

    /// Check if the page has reasonable spacing.
    ///
    /// Returns a list of warnings if spacing issues are detected.
    pub fn check_spacing(&self, min_system_spacing: f64, max_system_spacing: f64) -> Vec<String> {
        let mut warnings = Vec::new();

        // Check inter-system spacing
        for (i, &spacing) in self.inter_system_spacing.iter().enumerate() {
            if spacing < min_system_spacing {
                warnings.push(format!(
                    "Systems {} and {} are too close: {:.1} points (min: {:.1})",
                    i + 1, i + 2, spacing, min_system_spacing
                ));
            }
            if spacing > max_system_spacing {
                warnings.push(format!(
                    "Systems {} and {} are too far apart: {:.1} points (max: {:.1})",
                    i + 1, i + 2, spacing, max_system_spacing
                ));
            }
        }

        // Check bottom margin
        if self.last_system_to_bottom < 0.0 {
            warnings.push(format!(
                "Content extends past bottom margin by {:.1} points",
                -self.last_system_to_bottom
            ));
        }

        // Check if too much empty space at bottom
        let excessive_bottom = self.available_height * 0.3; // More than 30% empty
        if self.last_system_to_bottom > excessive_bottom {
            warnings.push(format!(
                "Excessive empty space at bottom: {:.1} points ({:.0}% of available height)",
                self.last_system_to_bottom,
                (self.last_system_to_bottom / self.available_height) * 100.0
            ));
        }

        warnings
    }
}

// region:    --- Melody Spillover Types

/// A segment of a melody note that fits within a single measure.
///
/// When a melody spans multiple measures, notes are split at barlines.
/// Each segment represents the portion of a note (or complete note) that
/// fits within one measure.
#[derive(Debug, Clone)]
pub struct MelodyNoteSegment {
    /// The pitch (e.g., "C", "F#", "r" for rest)
    pub pitch: String,
    /// Duration of this segment in beats (quarter note = 1.0)
    pub beats: f64,
    /// True if this is dotted
    pub dotted: bool,
    /// True if this is a continuation from previous measure (needs incoming tie)
    pub tie_from_previous: bool,
    /// True if this continues into next measure (needs outgoing tie)
    pub tie_to_next: bool,
    /// True if this is a rest
    pub is_rest: bool,
}

impl MelodyNoteSegment {
    /// Convert beats to a Duration enum value
    pub fn to_duration(&self) -> Duration {
        // Map beats to duration (approximating to nearest standard duration)
        // For dotted notes, divide by 1.5 to get the base duration
        let base_beats = if self.dotted { self.beats / 1.5 } else { self.beats };

        let base_duration = match base_beats {
            b if b >= 3.5 => Duration::Whole,
            b if b >= 1.75 => Duration::Half,
            b if b >= 0.875 => Duration::Quarter,
            b if b >= 0.4375 => Duration::Eighth,
            b if b >= 0.21875 => Duration::Sixteenth,
            _ => Duration::ThirtySecond,
        };

        // Return dotted version if appropriate
        if self.dotted {
            match base_duration {
                Duration::Half => Duration::DottedHalf,
                Duration::Quarter => Duration::DottedQuarter,
                Duration::Eighth => Duration::DottedEighth,
                Duration::Sixteenth => Duration::DottedSixteenth,
                // No DottedWhole or DottedThirtySecond in engraver
                other => other,
            }
        } else {
            base_duration
        }
    }
}

/// Preprocessed melody data for a measure.
///
/// Contains all melody note segments that should appear in this measure,
/// including segments that spilled over from previous measures.
#[derive(Debug, Clone, Default)]
pub struct MeasureMelodyData {
    /// Note segments to render in this measure
    pub segments: Vec<MelodyNoteSegment>,
    /// Total beats consumed by melody segments
    pub total_beats: f64,
}

impl MeasureMelodyData {
    /// Check if this measure has any melody content
    pub fn has_content(&self) -> bool {
        !self.segments.is_empty()
    }
}

/// Expands melodies across measure boundaries.
///
/// This processes all melodies in a chart section and distributes them
/// across measures, splitting notes at barlines and tracking ties.
pub fn expand_melodies_across_measures(
    section_measures: &[keyflow::chart::types::Measure],
    beats_per_measure: f64,
) -> std::collections::HashMap<usize, MeasureMelodyData> {
    use std::collections::HashMap;

    let mut result: HashMap<usize, MeasureMelodyData> = HashMap::new();

    for (measure_idx, measure) in section_measures.iter().enumerate() {
        if measure.melodies.is_empty() {
            continue;
        }

        #[cfg(debug_assertions)]
        eprintln!(
            "[melody-spillover] Measure {} has {} melodies",
            measure_idx,
            measure.melodies.len()
        );

        // Process each melody attached to this measure
        for melody in &measure.melodies {
            #[cfg(debug_assertions)]
            eprintln!(
                "[melody-spillover]   Processing melody with {} notes, total_beats: {:.2}",
                melody.notes.len(),
                melody.notes.iter().map(|n| n.duration_beats()).sum::<f64>()
            );
            let mut current_measure = measure_idx;
            let mut beats_remaining_in_measure = beats_per_measure;

            // Check if this measure already has melody content (from previous spillover)
            if let Some(existing) = result.get(&current_measure) {
                beats_remaining_in_measure = beats_per_measure - existing.total_beats;
            }

            for (note_idx, note) in melody.notes.iter().enumerate() {
                let note_beats = note.duration_beats();
                let mut beats_to_place = note_beats;
                let mut is_first_segment = true;

                while beats_to_place > 0.001 {
                    // Get or create melody data for current measure
                    let measure_data = result.entry(current_measure).or_default();

                    // Check remaining capacity in this measure
                    let capacity = beats_per_measure - measure_data.total_beats;

                    if capacity <= 0.001 {
                        // Measure is full, move to next
                        current_measure += 1;
                        beats_remaining_in_measure = beats_per_measure;
                        continue;
                    }

                    // Determine how much of this note fits in current measure
                    let segment_beats = beats_to_place.min(capacity);
                    let is_last_segment = (beats_to_place - segment_beats) < 0.001;

                    // Create segment
                    let segment = MelodyNoteSegment {
                        pitch: note.pitch.clone(),
                        beats: segment_beats,
                        dotted: note.dotted && is_last_segment, // Only dot the final segment
                        tie_from_previous: !is_first_segment,
                        tie_to_next: !is_last_segment && !note.is_rest(),
                        is_rest: note.is_rest(),
                    };

                    measure_data.segments.push(segment);
                    measure_data.total_beats += segment_beats;

                    beats_to_place -= segment_beats;
                    is_first_segment = false;

                    // If we consumed all capacity, move to next measure for remaining beats
                    if measure_data.total_beats >= beats_per_measure - 0.001 {
                        current_measure += 1;
                        beats_remaining_in_measure = beats_per_measure;
                    }
                }
            }
        }
    }

    // Log summary of melody distribution
    #[cfg(debug_assertions)]
    {
        for (measure_idx, data) in result.iter() {
            eprintln!(
                "[melody-spillover] Output measure {}: {} segments, {:.2} total beats",
                measure_idx,
                data.segments.len(),
                data.total_beats
            );
            for (i, seg) in data.segments.iter().enumerate() {
                eprintln!(
                    "[melody-spillover]   Segment {}: pitch={}, beats={:.2}, tie_in={}, tie_out={}",
                    i, seg.pitch, seg.beats, seg.tie_from_previous, seg.tie_to_next
                );
            }
        }
    }

    result
}

// endregion: --- Melody Spillover Types

/// Chart layout engine configuration.
#[derive(Debug, Clone)]
pub struct ChartLayoutConfig {
    /// Page margins.
    pub margins: PageMargins,
    /// Staff space (spatium) in points.
    pub spatium: f64,
    /// Spacing between systems.
    pub system_spacing: f64,
    /// Maximum measures per system.
    pub max_measures_per_system: usize,
    /// Minimum measure width.
    pub min_measure_width: f64,
    /// Harmony style for chord symbols.
    pub harmony_style: HarmonyStyle,
    /// Hide repeated consecutive chord symbols.
    /// When true, if a chord is the same as the previous chord, it won't be displayed.
    /// This reduces visual clutter in charts with many repeated chords.
    pub hide_repeated_chords: bool,
}

impl Default for ChartLayoutConfig {
    fn default() -> Self {
        Self {
            margins: PageMargins {
                top: 50.0,
                right: 50.0,
                bottom: 50.0,
                left: 72.0, // Extra left margin for section labels
            },
            spatium: 5.0,
            // System spacing: 4 spatiums (20pt) allows 10 systems per page
            // 10×50pt (system) + 9×20pt (spacing) = 680pt, fits in 692pt available
            system_spacing: 20.0,
            max_measures_per_system: 4,
            min_measure_width: 100.0,
            harmony_style: HarmonyStyle::musejazz(),
            hide_repeated_chords: true, // Default to hiding repeated chords
        }
    }
}

/// Chart layout engine.
///
/// Converts Keyflow Chart data structures to engraver SceneNode trees.
#[derive(Clone)]
pub struct ChartLayoutEngine {
    config: ChartLayoutConfig,
    style: &'static MStyle,
    text_font_data: Arc<Vec<u8>>,
    symbol_font_data: Arc<Vec<u8>>,
}

impl ChartLayoutEngine {
    /// Create a new chart layout engine.
    pub fn new(
        style: &'static MStyle,
        text_font_data: Arc<Vec<u8>>,
        symbol_font_data: Arc<Vec<u8>>,
    ) -> Self {
        Self {
            config: ChartLayoutConfig::default(),
            style,
            text_font_data,
            symbol_font_data,
        }
    }

    /// Create with custom configuration.
    pub fn with_config(
        config: ChartLayoutConfig,
        style: &'static MStyle,
        text_font_data: Arc<Vec<u8>>,
        symbol_font_data: Arc<Vec<u8>>,
    ) -> Self {
        Self {
            config,
            style,
            text_font_data,
            symbol_font_data,
        }
    }

    /// Set the harmony style for chord symbols.
    pub fn set_harmony_style(&mut self, style: HarmonyStyle) {
        self.config.harmony_style = style;
    }

    /// Layout a chart in the specified mode.
    pub fn layout_chart(&self, chart: &Chart, mode: &LayoutMode) -> ChartLayoutResult {
        match mode {
            LayoutMode::Paginated { page_width, page_height } => {
                self.layout_paginated(chart, *page_width, *page_height)
            }
            LayoutMode::ContinuousScroll { width } => {
                self.layout_continuous(chart, *width)
            }
        }
    }

    /// Layout chart in paginated mode with page breaks.
    fn layout_paginated(
        &self,
        chart: &Chart,
        page_width: f64,
        page_height: f64,
    ) -> ChartLayoutResult {
        let ctx = LayoutContext::minimal(self.style);
        let text_metrics = TextFontMetrics::new(self.text_font_data.clone());
        let symbol_metrics = TextFontMetrics::new(self.symbol_font_data.clone());

        let harmony_style = self.config.harmony_style.clone()
            .with_text_font_metrics(text_metrics.clone())
            .with_symbol_font_metrics(symbol_metrics.clone());

        let content_width = page_width - self.config.margins.left - self.config.margins.right;
        let content_height = page_height - self.config.margins.top - self.config.margins.bottom;
        let staff_height = self.config.spatium * 4.0;

        let mut root = SceneNode::group(SemanticId::new(ElementType::Page, 0));
        let mut pages: Vec<PageLayout> = Vec::new();
        let mut current_page_systems: Vec<SystemLayout> = Vec::new();
        let mut page_number = 1u32;
        let mut page_y = self.config.margins.top;
        let mut global_system_index = 0usize;
        let mut id_counter = 100u64;

        // Track previous chord to hide duplicates
        let mut previous_chord_symbol: Option<String> = None;

        // Calculate page offset for multi-page rendering
        let page_offset_x = 20.0;
        let page_offset_y = 20.0;
        let page_gap = 40.0;

        // Process each section
        for (section_idx, chart_section) in chart.sections.iter().enumerate() {
            // Reset chord tracking at section boundaries (rehearsal marks)
            // so the first chord of each section always shows
            previous_chord_symbol = None;

            // Preprocess melodies to handle cross-measure spillover
            let time_signature = chart.time_signature
                .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                .unwrap_or((4u8, 4u8));
            let beats_per_measure = time_signature.0 as f64;
            let melody_data_map = expand_melodies_across_measures(
                chart_section.measures(),
                beats_per_measure,
            );

            // Group measures into systems
            let systems = self.group_measures_into_systems(chart_section.measures(), content_width);

            for (sys_idx, measure_indices) in systems.iter().enumerate() {
                let system_height = staff_height + 30.0; // Staff + chord space

                // Check for page overflow (don't include spacing - last system doesn't need it)
                if page_y + system_height > self.config.margins.top + content_height {
                    // Finalize current page (background was already added when page started)
                    if !current_page_systems.is_empty() {
                        pages.push(PageLayout {
                            number: page_number,
                            width: page_width,
                            height: page_height,
                            systems: std::mem::take(&mut current_page_systems),
                            margins: self.config.margins.clone(),
                        });
                        page_number += 1;
                        page_y = self.config.margins.top;
                    }
                }

                // Calculate page position
                let page_x = page_offset_x + (page_number as f64 - 1.0) * (page_width + page_gap);
                let content_x = page_x + self.config.margins.left;
                let staff_y = page_offset_y + page_y;

                // Add page background if this is first system on page
                if current_page_systems.is_empty() {
                    self.add_page_background(&mut root, page_x, page_offset_y, page_width, page_height);
                }

                // Calculate system prefix width (clef + time sig) FIRST
                // Needed to determine staff line width for short systems
                let include_clef = true; // Always show clef at system start
                let include_time_sig = global_system_index == 0; // Time sig only on first system
                let clef_spacing = 0.5 * self.config.spatium; // Space after clef
                let time_sig_spacing = 0.8 * self.config.spatium; // Space after time sig

                let clef_width = ClefType::Treble.width() * self.config.spatium + clef_spacing;
                let time_sig_width = if include_time_sig {
                    2.0 * self.config.spatium + time_sig_spacing // Approximate time sig width
                } else {
                    0.0
                };
                let prefix_width = clef_width + time_sig_width;

                // Calculate measure width - use natural width for short systems
                let measures_area_width = content_width - prefix_width;
                let num_measures = measure_indices.len();
                let max_measures = self.config.max_measures_per_system;

                // For short lines (< max_measures), use natural width based on max_measures
                // This prevents stretching 1-3 measures across the full line width
                // Like LilyPond's pseudo-indent system
                let (measure_width, is_short_system) = if num_measures < max_measures {
                    // Natural width = what a measure would be if we had max_measures
                    (measures_area_width / max_measures as f64, true)
                } else {
                    // Full line - distribute evenly
                    (measures_area_width / num_measures as f64, false)
                };

                // Calculate actual system width (for short systems, staff lines are shorter)
                let actual_system_width = if is_short_system {
                    prefix_width + (num_measures as f64 * measure_width)
                } else {
                    content_width
                };

                // Draw staff lines (shortened for short systems)
                root.add_child(SceneNode::anonymous_leaf(
                    self.draw_staff_lines(content_x, staff_y, actual_system_width),
                ));

                let chord_y = staff_y - 8.0; // Above staff

                // Add section label for first system of section
                if sys_idx == 0 {
                    let mut section_label = self.create_section_label(
                        &chart_section.section,
                        page_x,
                        self.config.margins.left,
                        staff_y,
                        staff_height,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 1;
                    section_label.metadata.insert("page".to_string(), page_number.to_string());
                    root.add_child(section_label);
                }

                // Render clef at system start
                let mut prefix_x = content_x;
                if include_clef {
                    let clef_params = ClefParams {
                        id: id_counter,
                        clef_type: ClefType::Treble,
                        ..Default::default()
                    };
                    id_counter += 1;
                    let (_, mut clef_node) = layout_clef(&clef_params, &ctx);
                    // Position clef on staff (middle line = 2 spatiums from top)
                    clef_node.transform = Affine::translate((
                        prefix_x,
                        staff_y + 2.0 * self.config.spatium,
                    ));
                    clef_node.metadata.insert("page".to_string(), page_number.to_string());
                    root.add_child(clef_node);
                    prefix_x += clef_width;
                }

                // Render time signature on first system
                if include_time_sig {
                    let ts = chart.time_signature
                        .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                        .unwrap_or((4u8, 4u8));
                    let ts_params = TimeSigParams {
                        id: id_counter,
                        sig_type: TimeSigType::Numeric { numerator: ts.0, denominator: ts.1 },
                        ..Default::default()
                    };
                    id_counter += 1;
                    let (_, mut ts_node) = layout_timesig(&ts_params, &ctx);
                    ts_node.transform = Affine::translate((
                        prefix_x,
                        staff_y + 2.0 * self.config.spatium,
                    ));
                    ts_node.metadata.insert("page".to_string(), page_number.to_string());
                    root.add_child(ts_node);
                    prefix_x += time_sig_width;
                }

                // Start measures after prefix
                let mut measure_x = content_x + prefix_width;
                let time_signature = chart.time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                for &measure_idx in measure_indices.iter() {
                    if let Some(measure) = chart_section.measures().get(measure_idx) {
                        // Get preprocessed melody data for this measure (handles spillover)
                        let melody_data = melody_data_map.get(&measure_idx);

                        // Layout measure content (rhythm slashes) - no clef/time sig inside measure
                        let measure_result = self.layout_measure(
                            measure,
                            melody_data,
                            measure_width,
                            false, // clef rendered separately
                            false, // time sig rendered separately
                            time_signature,
                            &ctx,
                            id_counter,
                        );
                        id_counter += 10;

                        // Get ChordRest segment x-positions (already in points after spacing)
                        let segment_positions: Vec<f64> = self.get_chord_rest_positions(&measure_result);

                        let mut measure_container = SceneNode::group(
                            SemanticId::new(ElementType::Measure, id_counter),
                        );
                        id_counter += 1;
                        measure_container.transform = Affine::translate((
                            measure_x,
                            staff_y + 2.0 * self.config.spatium,
                        ));
                        measure_container.metadata.insert("page".to_string(), page_number.to_string());
                        measure_container.add_child(measure_result.scene);
                        root.add_child(measure_container);

                        // Layout chord symbols - align with actual rhythm slash positions
                        for (chord_idx, chord) in measure.chords.iter().enumerate() {
                            // Skip space/rest placeholders (keyflow: "s" = empty space, "r" = explicit rest)
                            let current_symbol = &chord.full_symbol;
                            if current_symbol == "s" || current_symbol == "r" || current_symbol.is_empty() {
                                continue;
                            }

                            // Check if we should hide this chord (duplicate of previous)
                            let is_duplicate = self.config.hide_repeated_chords
                                && previous_chord_symbol.as_ref() == Some(current_symbol);

                            // Update previous chord tracker
                            previous_chord_symbol = Some(current_symbol.clone());

                            // Skip rendering if duplicate
                            if is_duplicate {
                                continue;
                            }

                            let segment_x = segment_positions
                                .get(chord_idx)
                                .copied()
                                .unwrap_or_else(|| segment_positions.first().copied().unwrap_or(0.0));

                            let chord_x = measure_x + segment_x;

                            let mut params = self.chord_to_harmony_params(chord, &harmony_style);
                            params.position = kurbo::Point::new(chord_x, chord_y);
                            params.id = id_counter;
                            id_counter += 1;

                            let (_, mut chord_node) = layout_harmony(&params, &ctx);

                            // Add comprehensive metadata for click-to-highlight and queries
                            chord_node.set_page(page_number);
                            chord_node.set_system(global_system_index as u32);
                            chord_node.set_measure(measure_idx as u32);
                            chord_node.set_beat(chord_idx as u32);
                            chord_node.set_element_type("chord");
                            chord_node.set_section_type(&chart_section.section.section_type.full_name());

                            // Build ChartPosition for musical coordinates
                            let chart_pos = ChartPosition::new(
                                global_system_index as u32,
                                measure_idx as u32,
                                chord_idx as u32,
                            );
                            chord_node.set_json_metadata(metadata_keys::CHART_POSITION, &chart_pos);

                            // Add source span if available (for click-to-highlight)
                            if let Some(ref span) = chord.source_span {
                                chord_node.set_json_metadata(metadata_keys::SOURCE_SPAN, span);

                                // Build full SourceLink for bidirectional mapping
                                let source_link = SourceLink::new(*span, chart_pos.clone());
                                chord_node.set_json_metadata(metadata_keys::SOURCE_LINK, &source_link);
                            }

                            root.add_child(chord_node);
                        }

                        measure_x += measure_width;

                        // Barline after every measure
                        root.add_child(self.draw_barline(
                            measure_x,
                            staff_y,
                            staff_height,
                            BarlineType::Single,
                        ));
                    }
                }

                // Track system layout
                current_page_systems.push(SystemLayout {
                    index: global_system_index,
                    y: page_y,
                    width: content_width,
                    height: system_height,
                    measure_indices: measure_indices.clone(),
                });

                page_y += system_height + self.config.system_spacing;
                global_system_index += 1;
            }
        }

        // Finalize last page
        if !current_page_systems.is_empty() {
            pages.push(PageLayout {
                number: page_number,
                width: page_width,
                height: page_height,
                systems: current_page_systems,
                margins: self.config.margins.clone(),
            });
        }

        let total_width = page_offset_x * 2.0 + page_number as f64 * (page_width + page_gap);
        let total_height = page_height + page_offset_y * 2.0;

        ChartLayoutResult {
            scene: root,
            pages,
            total_height,
            total_width,
        }
    }

    /// Layout chart in continuous scroll mode.
    fn layout_continuous(&self, chart: &Chart, width: f64) -> ChartLayoutResult {
        let ctx = LayoutContext::minimal(self.style);
        let text_metrics = TextFontMetrics::new(self.text_font_data.clone());
        let symbol_metrics = TextFontMetrics::new(self.symbol_font_data.clone());

        let harmony_style = self.config.harmony_style.clone()
            .with_text_font_metrics(text_metrics.clone())
            .with_symbol_font_metrics(symbol_metrics.clone());

        let content_width = width - self.config.margins.left - self.config.margins.right;
        let content_x = self.config.margins.left;
        let staff_height = self.config.spatium * 4.0;

        let mut root = SceneNode::group(SemanticId::new(ElementType::Page, 0));
        let mut total_height = self.config.margins.top;
        let mut id_counter = 100u64;
        let mut global_system_index = 0usize;

        // Track previous chord to hide duplicates
        let mut previous_chord_symbol: Option<String> = None;

        // Process each section
        for (section_idx, chart_section) in chart.sections.iter().enumerate() {
            // Reset chord tracking at section boundaries (rehearsal marks)
            // so the first chord of each section always shows
            previous_chord_symbol = None;

            // Preprocess melodies to handle cross-measure spillover
            let time_signature = chart.time_signature
                .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                .unwrap_or((4u8, 4u8));
            let beats_per_measure = time_signature.0 as f64;
            let melody_data_map = expand_melodies_across_measures(
                chart_section.measures(),
                beats_per_measure,
            );

            // Group measures into systems
            let systems = self.group_measures_into_systems(chart_section.measures(), content_width);

            for (sys_idx, measure_indices) in systems.iter().enumerate() {
                let staff_y = total_height;
                let system_height = staff_height + 30.0;

                // Calculate system prefix width (clef + time sig) FIRST
                // Needed to determine staff line width for short systems
                let include_clef = true;
                let include_time_sig = global_system_index == 0;
                let clef_spacing = 0.5 * self.config.spatium;
                let time_sig_spacing = 0.8 * self.config.spatium;

                let clef_width = ClefType::Treble.width() * self.config.spatium + clef_spacing;
                let time_sig_width = if include_time_sig {
                    2.0 * self.config.spatium + time_sig_spacing
                } else {
                    0.0
                };
                let prefix_width = clef_width + time_sig_width;

                // Calculate measure width - use natural width for short systems
                let measures_area_width = content_width - prefix_width;
                let num_measures = measure_indices.len();
                let max_measures = self.config.max_measures_per_system;

                // For short lines (< max_measures), use natural width based on max_measures
                let (measure_width, is_short_system) = if num_measures < max_measures {
                    (measures_area_width / max_measures as f64, true)
                } else {
                    (measures_area_width / num_measures as f64, false)
                };

                // Calculate actual system width (for short systems, staff lines are shorter)
                let actual_system_width = if is_short_system {
                    prefix_width + (num_measures as f64 * measure_width)
                } else {
                    content_width
                };

                // Draw staff lines (shortened for short systems)
                root.add_child(SceneNode::anonymous_leaf(
                    self.draw_staff_lines(content_x, staff_y, actual_system_width),
                ));

                let chord_y = staff_y - 8.0;

                // Add section label for first system of section
                if sys_idx == 0 {
                    let section_label = self.create_section_label(
                        &chart_section.section,
                        0.0,
                        self.config.margins.left,
                        staff_y,
                        staff_height,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 1;
                    root.add_child(section_label);
                }

                // Render clef at system start
                let mut prefix_x = content_x;
                if include_clef {
                    let clef_params = ClefParams {
                        id: id_counter,
                        clef_type: ClefType::Treble,
                        ..Default::default()
                    };
                    id_counter += 1;
                    let (_, mut clef_node) = layout_clef(&clef_params, &ctx);
                    clef_node.transform = Affine::translate((
                        prefix_x,
                        staff_y + 2.0 * self.config.spatium,
                    ));
                    root.add_child(clef_node);
                    prefix_x += clef_width;
                }

                // Render time signature on first system
                if include_time_sig {
                    let ts = chart.time_signature
                        .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                        .unwrap_or((4u8, 4u8));
                    let ts_params = TimeSigParams {
                        id: id_counter,
                        sig_type: TimeSigType::Numeric { numerator: ts.0, denominator: ts.1 },
                        ..Default::default()
                    };
                    id_counter += 1;
                    let (_, mut ts_node) = layout_timesig(&ts_params, &ctx);
                    ts_node.transform = Affine::translate((
                        prefix_x,
                        staff_y + 2.0 * self.config.spatium,
                    ));
                    root.add_child(ts_node);
                }

                // Start measures after prefix
                let mut measure_x = content_x + prefix_width;
                let time_signature = chart.time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                for &measure_idx in measure_indices.iter() {
                    if let Some(measure) = chart_section.measures().get(measure_idx) {
                        let melody_data = melody_data_map.get(&measure_idx);
                        let measure_result = self.layout_measure(
                            measure,
                            melody_data,
                            measure_width,
                            false,
                            false,
                            time_signature,
                            &ctx,
                            id_counter,
                        );
                        id_counter += 10;

                        let segment_positions: Vec<f64> = self.get_chord_rest_positions(&measure_result);

                        let mut measure_container = SceneNode::group(
                            SemanticId::new(ElementType::Measure, id_counter),
                        );
                        id_counter += 1;
                        measure_container.transform = Affine::translate((
                            measure_x,
                            staff_y + 2.0 * self.config.spatium,
                        ));
                        measure_container.add_child(measure_result.scene);
                        root.add_child(measure_container);

                        // Layout chord symbols
                        for (chord_idx, chord) in measure.chords.iter().enumerate() {
                            // Skip space/rest placeholders (keyflow: "s" = empty space, "r" = explicit rest)
                            let current_symbol = &chord.full_symbol;
                            if current_symbol == "s" || current_symbol == "r" || current_symbol.is_empty() {
                                continue;
                            }

                            // Check if we should hide this chord (duplicate of previous)
                            let is_duplicate = self.config.hide_repeated_chords
                                && previous_chord_symbol.as_ref() == Some(current_symbol);

                            // Update previous chord tracker
                            previous_chord_symbol = Some(current_symbol.clone());

                            // Skip rendering if duplicate
                            if is_duplicate {
                                continue;
                            }

                            let segment_x = segment_positions
                                .get(chord_idx)
                                .copied()
                                .unwrap_or_else(|| segment_positions.first().copied().unwrap_or(0.0));

                            let chord_x = measure_x + segment_x;

                            let mut params = self.chord_to_harmony_params(chord, &harmony_style);
                            params.position = kurbo::Point::new(chord_x, chord_y);
                            params.id = id_counter;
                            id_counter += 1;

                            let (_, mut chord_node) = layout_harmony(&params, &ctx);

                            // Add comprehensive metadata for click-to-highlight and queries
                            chord_node.set_system(global_system_index as u32);
                            chord_node.set_measure(measure_idx as u32);
                            chord_node.set_beat(chord_idx as u32);
                            chord_node.set_element_type("chord");
                            chord_node.set_section_type(&chart_section.section.section_type.full_name());

                            // Build ChartPosition for musical coordinates
                            let chart_pos = ChartPosition::new(
                                global_system_index as u32,
                                measure_idx as u32,
                                chord_idx as u32,
                            );
                            chord_node.set_json_metadata(metadata_keys::CHART_POSITION, &chart_pos);

                            // Add source span if available (for click-to-highlight)
                            if let Some(ref span) = chord.source_span {
                                chord_node.set_json_metadata(metadata_keys::SOURCE_SPAN, span);

                                // Build full SourceLink for bidirectional mapping
                                let source_link = SourceLink::new(*span, chart_pos.clone());
                                chord_node.set_json_metadata(metadata_keys::SOURCE_LINK, &source_link);
                            }

                            root.add_child(chord_node);
                        }

                        measure_x += measure_width;

                        root.add_child(self.draw_barline(
                            measure_x,
                            staff_y,
                            staff_height,
                            BarlineType::Single,
                        ));
                    }
                }

                total_height += system_height + self.config.system_spacing;
                global_system_index += 1;
            }
        }

        total_height += self.config.margins.bottom;

        ChartLayoutResult {
            scene: root,
            pages: Vec::new(),
            total_height,
            total_width: width,
        }
    }

    /// Group measures into systems based on available width.
    fn group_measures_into_systems(
        &self,
        measures: &[keyflow::chart::types::Measure],
        _content_width: f64,
    ) -> Vec<Vec<usize>> {
        let mut systems = Vec::new();
        let mut current_system = Vec::new();

        for i in 0..measures.len() {
            current_system.push(i);
            if current_system.len() >= self.config.max_measures_per_system {
                systems.push(std::mem::take(&mut current_system));
            }
        }

        if !current_system.is_empty() {
            systems.push(current_system);
        }

        systems
    }

    /// Convert a Keyflow ChordInstance to engraver HarmonyParams.
    fn chord_to_harmony_params(
        &self,
        chord: &keyflow::chart::types::ChordInstance,
        harmony_style: &HarmonyStyle,
    ) -> HarmonyParams {
        // Use the full_symbol which is already formatted for display
        let mut params = parse_chord(&chord.full_symbol);
        params.style = harmony_style.clone();
        params
    }

    /// Create a section label scene node.
    fn create_section_label(
        &self,
        section: &keyflow::sections::Section,
        page_x: f64,
        margin_width: f64,
        staff_y: f64,
        staff_height: f64,
        ctx: &LayoutContext<'_>,
        id: u64,
    ) -> SceneNode {
        let (section_type, abbreviation) = self.section_type_to_strings(&section.section_type);
        let number = section.number;

        let (_, label_node) = layout_margin_label(
            &MarginLabelParams {
                section_type,
                abbreviation,
                number,
                page_x,
                margin_width,
                staff_y,
                staff_height,
                style: self.get_section_theme(&section.section_type),
                ..Default::default()
            },
            ctx,
        );

        let mut container = SceneNode::group(SemanticId::new(ElementType::RehearsalMark, id));
        container.add_child(label_node);
        container
    }

    /// Convert SectionType to display strings.
    fn section_type_to_strings(&self, section_type: &SectionType) -> (String, String) {
        match section_type {
            SectionType::Intro => ("Intro".to_string(), "INT".to_string()),
            SectionType::Verse => ("Verse".to_string(), "VS".to_string()),
            SectionType::Chorus => ("Chorus".to_string(), "CH".to_string()),
            SectionType::Bridge => ("Bridge".to_string(), "BR".to_string()),
            SectionType::Outro => ("Outro".to_string(), "OUT".to_string()),
            SectionType::Instrumental => ("Instrumental".to_string(), "INST".to_string()),
            SectionType::Pre(target) => {
                let (name, _) = self.section_type_to_strings(target);
                (format!("Pre-{}", name), "PRE".to_string())
            }
            SectionType::Post(target) => {
                let (name, _) = self.section_type_to_strings(target);
                (format!("Post-{}", name), "POST".to_string())
            }
            SectionType::Custom(name) => (name.clone(), name.chars().take(3).collect::<String>().to_uppercase()),
        }
    }

    /// Get theme for section type.
    fn get_section_theme(&self, section_type: &SectionType) -> engraver::layout::tlayout::rehearsal_mark::RehearsalMarkStyle {
        match section_type {
            SectionType::Intro | SectionType::Outro => rehearsal_themes::blue(),
            SectionType::Verse => rehearsal_themes::green(),
            SectionType::Chorus => rehearsal_themes::purple(),
            SectionType::Bridge => rehearsal_themes::light(),
            SectionType::Instrumental => rehearsal_themes::outline(),
            SectionType::Pre(_) | SectionType::Post(_) => rehearsal_themes::purple(),
            SectionType::Custom(_) => rehearsal_themes::dark(),
        }
    }

    /// Draw staff lines.
    fn draw_staff_lines(&self, x: f64, y: f64, width: f64) -> Vec<PaintCommand> {
        let mut commands = Vec::new();
        let line_thickness = 0.5;

        for i in 0..5 {
            let line_y = y + i as f64 * self.config.spatium;
            commands.push(PaintCommand::line(
                kurbo::Point::new(x, line_y),
                kurbo::Point::new(x + width, line_y),
                Color::BLACK,
                line_thickness,
            ));
        }

        commands
    }

    /// Draw a barline.
    fn draw_barline(&self, x: f64, y: f64, height: f64, barline_type: BarlineType) -> SceneNode {
        let mut commands = Vec::new();
        let thin = 0.5;
        let thick = 2.0;

        match barline_type {
            BarlineType::Single => {
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x, y),
                    kurbo::Point::new(x, y + height),
                    Color::BLACK,
                    thin,
                ));
            }
            BarlineType::Double => {
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x - 3.0, y),
                    kurbo::Point::new(x - 3.0, y + height),
                    Color::BLACK,
                    thin,
                ));
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x, y),
                    kurbo::Point::new(x, y + height),
                    Color::BLACK,
                    thin,
                ));
            }
            BarlineType::End => {
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x - 5.0, y),
                    kurbo::Point::new(x - 5.0, y + height),
                    Color::BLACK,
                    thin,
                ));
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x, y),
                    kurbo::Point::new(x, y + height),
                    Color::BLACK,
                    thick,
                ));
            }
            _ => {
                commands.push(PaintCommand::line(
                    kurbo::Point::new(x, y),
                    kurbo::Point::new(x, y + height),
                    Color::BLACK,
                    thin,
                ));
            }
        }

        SceneNode::anonymous_leaf(commands)
    }

    /// Add page background (paper with shadow).
    fn add_page_background(
        &self,
        root: &mut SceneNode,
        page_x: f64,
        page_y: f64,
        page_width: f64,
        page_height: f64,
    ) {
        let shadow_offset = 4.0;

        // Shadow
        root.add_child(SceneNode::anonymous_leaf(vec![
            PaintCommand::filled_rect(
                Rect::new(
                    page_x + shadow_offset,
                    page_y + shadow_offset,
                    page_x + shadow_offset + page_width,
                    page_y + shadow_offset + page_height,
                ),
                Color::from_rgb8(180, 180, 180),
            ),
        ]));

        // White paper
        root.add_child(SceneNode::anonymous_leaf(vec![
            PaintCommand::filled_rect(
                Rect::new(page_x, page_y, page_x + page_width, page_y + page_height),
                Color::WHITE,
            ),
        ]));
    }

    /// Layout a single measure, returning the full MeasureScene with segment positions.
    ///
    /// If `melody_data` is provided, it contains preprocessed melody segments that account
    /// for spillover across measure boundaries. Otherwise, falls back to the measure's
    /// raw melody data (legacy behavior, doesn't handle cross-measure melodies).
    fn layout_measure(
        &self,
        measure: &keyflow::chart::types::Measure,
        melody_data: Option<&MeasureMelodyData>,
        measure_width: f64,
        include_clef: bool,
        include_time_sig: bool,
        time_signature: (u8, u8),
        ctx: &LayoutContext<'_>,
        id_base: u64,
    ) -> MeasureScene {
        // Check if this measure has melodies (from preprocessed data or raw measure)
        let has_melodies = melody_data.map_or_else(
            || !measure.melodies.is_empty(),
            |data| data.has_content()
        );

        // Calculate measure duration in ticks based on time signature
        // Quarter note = 480 ticks, so beat duration depends on denominator
        let beat_ticks = match time_signature.1 {
            2 => 960,  // Half note beat
            4 => 480,  // Quarter note beat
            8 => 240,  // Eighth note beat
            _ => 480,  // Default to quarter
        };
        let measure_ticks = beat_ticks * time_signature.0 as i32;
        let beats_per_measure = time_signature.0 as f64;

        // Build rhythm array from preprocessed melody data or raw measure
        let (rhythm, melody_ticks) = if let Some(data) = melody_data {
            // Use preprocessed melody segments
            let melody_durations: Vec<Duration> = data.segments.iter()
                .map(|seg| seg.to_duration())
                .collect();

            let melody_ticks: i32 = melody_durations.iter()
                .map(|d| d.ticks())
                .sum();

            (melody_durations, melody_ticks)
        } else if has_melodies {
            // Legacy: Convert raw melody notes to durations (no spillover handling)
            let melody_durations: Vec<Duration> = measure.melodies.iter()
                .flat_map(|m| m.notes.iter())
                .map(|note| self.melody_note_to_duration(note))
                .collect();

            let melody_ticks: i32 = melody_durations.iter()
                .map(|d| d.ticks())
                .sum();

            (melody_durations, melody_ticks)
        } else {
            // No melodies - will use pure slash notation
            let num_beats = measure.chords.len().max(time_signature.0 as usize);
            let rhythm: Vec<Duration> = (0..num_beats).map(|_| Duration::Quarter).collect();
            let ticks: i32 = rhythm.iter().map(|d| d.ticks()).sum();
            (rhythm, ticks)
        };

        // Fill remaining space with quarter note slashes if melody doesn't cover full measure
        let remaining_ticks = measure_ticks - melody_ticks;
        let num_melody_notes = rhythm.len();
        let fill_slashes = if has_melodies && remaining_ticks > 0 {
            // Calculate how many quarter notes fit in remaining space
            let quarter_ticks = 480;
            let num_fill_quarters = remaining_ticks / quarter_ticks;
            (0..num_fill_quarters).map(|_| Duration::Quarter).collect::<Vec<_>>()
        } else {
            Vec::new()
        };
        let num_fill_notes = fill_slashes.len();

        // Combine melody durations with fill slashes
        let full_rhythm: Vec<Duration> = rhythm.into_iter()
            .chain(fill_slashes.into_iter())
            .collect();

        // Build head type overrides: melody notes use standard heads, fill notes use slashes
        let head_type_overrides: Vec<Option<NoteHeadType>> = if has_melodies && num_fill_notes > 0 {
            let mut overrides = Vec::with_capacity(num_melody_notes + num_fill_notes);
            // Melody notes: None = use default (standard noteheads since mode is Standard)
            for _ in 0..num_melody_notes {
                overrides.push(None);
            }
            // Fill notes: explicitly use Slash noteheads
            for _ in 0..num_fill_notes {
                overrides.push(Some(NoteHeadType::Slash));
            }
            overrides
        } else {
            Vec::new()
        };

        // Convert measure width from points to spatiums for justification
        let width_spatiums = measure_width / self.config.spatium;

        let mut builder = MeasureBuilder::new()
            .rhythm(full_rhythm)
            .id_base(id_base as u64)
            .justify_to(width_spatiums)
            .no_barlines(); // Barlines handled by chart_layout

        // Apply head type overrides for mixed melody/slash notation
        if !head_type_overrides.is_empty() {
            builder = builder.head_type_overrides(head_type_overrides);
        }

        // Use rhythmic (slash) notation only when no melodies
        // With melodies, use standard notation as base (overrides handle the slash fills)
        if !has_melodies {
            builder = builder.rhythmic();
        }

        if include_clef {
            builder = builder.clef(ClefType::Treble);
        }

        if include_time_sig {
            builder = builder.time_signature(time_signature.0, time_signature.1);
        }

        let mut result = builder.build(ctx);

        // Add ties for cross-barline notes
        if let Some(data) = melody_data {
            self.add_melody_ties(&mut result, data, ctx);
        }

        result
    }

    /// Add tie arcs for melody notes that cross barlines.
    fn add_melody_ties(
        &self,
        measure_result: &mut MeasureScene,
        melody_data: &MeasureMelodyData,
        ctx: &LayoutContext<'_>,
    ) {
        let spatium = ctx.spatium();
        let tie_config = SlurTieConfig::default();

        // Find chord/rest segment positions
        let chord_segments: Vec<_> = measure_result
            .segments
            .iter()
            .filter(|seg| seg.seg_type.contains(SegmentType::CHORD_REST))
            .collect();

        // Staff line 0 is the middle line (B4 in treble clef)
        // For melody notes, we'll use a fixed position for now
        let note_y = 2.0 * spatium; // Middle of staff

        for (i, segment) in melody_data.segments.iter().enumerate() {
            if segment.is_rest {
                continue; // Rests don't have ties
            }

            // Get the corresponding segment position
            if let Some(seg) = chord_segments.get(i) {
                let note_x = seg.x * spatium;

                // Tie going to the right (note continues in next measure)
                if segment.tie_to_next {
                    let start = SlurEndpoint {
                        x: note_x + spatium, // Right side of notehead
                        y: note_y,
                        stem_up: false,
                    };
                    let end = SlurEndpoint {
                        x: measure_result.width * spatium + spatium * 0.5, // Just past measure end
                        y: note_y,
                        stem_up: false,
                    };

                    let tie_layout = layout_tie(
                        &start,
                        &end,
                        SlurDirection::Down,
                        1000 + i as u64,
                        spatium,
                        &tie_config,
                    );

                    let tie_node = SceneNode::anonymous_leaf(tie_layout.commands);
                    measure_result.scene.add_child(tie_node);
                }

                // Tie coming from the left (note continues from previous measure)
                if segment.tie_from_previous {
                    let start = SlurEndpoint {
                        x: -spatium * 0.5, // Just before measure start
                        y: note_y,
                        stem_up: false,
                    };
                    let end = SlurEndpoint {
                        x: note_x, // Left side of notehead
                        y: note_y,
                        stem_up: false,
                    };

                    let tie_layout = layout_tie(
                        &start,
                        &end,
                        SlurDirection::Down,
                        2000 + i as u64,
                        spatium,
                        &tie_config,
                    );

                    let tie_node = SceneNode::anonymous_leaf(tie_layout.commands);
                    measure_result.scene.add_child(tie_node);
                }
            }
        }
    }

    /// Convert a melody note to a Duration
    fn melody_note_to_duration(&self, note: &keyflow::chart::melody::MelodyNote) -> Duration {
        let base_duration = match note.duration {
            1 => Duration::Whole,
            2 => Duration::Half,
            4 => Duration::Quarter,
            8 => Duration::Eighth,
            16 => Duration::Sixteenth,
            32 => Duration::ThirtySecond,
            _ => Duration::Quarter, // Default to quarter note
        };

        if note.dotted {
            // Convert to dotted version (no DottedWhole exists, use Whole)
            match base_duration {
                Duration::Whole => Duration::Whole, // No dotted whole in engraver
                Duration::Half => Duration::DottedHalf,
                Duration::Quarter => Duration::DottedQuarter,
                Duration::Eighth => Duration::DottedEighth,
                Duration::Sixteenth => Duration::DottedSixteenth,
                _ => base_duration,
            }
        } else {
            base_duration
        }
    }

    /// Extract ChordRest segment x-positions from a MeasureScene (in spatiums).
    fn get_chord_rest_positions(&self, measure_scene: &MeasureScene) -> Vec<f64> {
        measure_scene.segments.iter()
            .filter(|seg| seg.seg_type.contains(SegmentType::CHORD_REST))
            .map(|seg| seg.x)
            .collect()
    }
}

// region:    --- Test Utilities

/// Query utilities for headless layout testing.
///
/// These utilities enable position verification tests without GPU rendering.
#[cfg(test)]
pub mod test_utils {
    use super::*;
    use engraver::scene::id::ElementType;
    use engraver::scene::traverse::SceneNodeExt;
    use kurbo::Point;

    /// Element position extracted from scene graph.
    #[derive(Debug, Clone)]
    pub struct ElementPosition {
        pub element_type: ElementType,
        pub id: u64,
        pub world_x: f64,
        pub world_y: f64,
        pub bounds: Option<Rect>,
        /// Page number (1-indexed), if the element was tagged during layout.
        pub page: Option<u32>,
    }

    /// Extract all elements of a specific type with their world positions.
    ///
    /// Note: For elements like ChordSymbol where positions are baked into
    /// paint commands rather than node transforms, this returns the transform
    /// position only. Use `find_elements_with_content_bounds` for paint-based positions.
    pub fn find_elements_by_type(
        scene: &SceneNode,
        element_type: ElementType,
    ) -> Vec<ElementPosition> {
        scene
            .iter_with_transforms()
            .filter_map(|(node, transform)| {
                let id = node.id.as_ref()?;
                if id.element_type != element_type {
                    return None;
                }

                // Get world position from transform
                let world_origin = transform * Point::ORIGIN;

                // Extract page metadata if present
                let page = node.metadata.get("page")
                    .and_then(|s| s.parse::<u32>().ok());

                Some(ElementPosition {
                    element_type: id.element_type,
                    id: id.id,
                    world_x: world_origin.x,
                    world_y: world_origin.y,
                    bounds: if node.bounds.is_zero_area() { None } else { Some(node.bounds) },
                    page,
                })
            })
            .collect()
    }

    /// Count elements of a specific type in the scene graph.
    pub fn count_elements_by_type(scene: &SceneNode, element_type: ElementType) -> usize {
        scene
            .iter_with_transforms()
            .filter(|(node, _)| {
                node.id.as_ref().map_or(false, |id| id.element_type == element_type)
            })
            .count()
    }

    /// Check if two x-positions are within tolerance (for alignment verification).
    pub fn x_positions_aligned(x1: f64, x2: f64, tolerance: f64) -> bool {
        (x1 - x2).abs() <= tolerance
    }

    /// Find elements by type and ID.
    pub fn find_element_by_id(
        scene: &SceneNode,
        element_type: ElementType,
        id: u64,
    ) -> Option<ElementPosition> {
        find_elements_by_type(scene, element_type)
            .into_iter()
            .find(|e| e.id == id)
    }

    /// Find elements by type on a specific page.
    pub fn find_elements_on_page(
        scene: &SceneNode,
        element_type: ElementType,
        page: u32,
    ) -> Vec<ElementPosition> {
        find_elements_by_type(scene, element_type)
            .into_iter()
            .filter(|e| e.page == Some(page))
            .collect()
    }

    /// Group elements by page number.
    pub fn group_elements_by_page(
        elements: &[ElementPosition],
    ) -> std::collections::HashMap<u32, Vec<&ElementPosition>> {
        use std::collections::HashMap;
        let mut grouped: HashMap<u32, Vec<&ElementPosition>> = HashMap::new();
        for elem in elements {
            if let Some(page) = elem.page {
                grouped.entry(page).or_default().push(elem);
            }
        }
        grouped
    }
}

// endregion: --- Test Utilities

// region:    --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use super::test_utils::*;
    use engraver::scene::id::ElementType;
    use engraver::style::MStyle;
    use keyflow::{
        AbsolutePosition, Chart, ChartSection, Chord, ChordInstance, ChordQuality,
        ChordRhythm, Measure, MusicalDuration, MusicalNote, MusicalPosition, RootNotation,
        Section, SectionType, TimeSignature,
    };
    use std::sync::Arc;

    /// Create a static MStyle for testing (leaked for 'static lifetime).
    fn test_style() -> &'static MStyle {
        Box::leak(Box::new(MStyle::default()))
    }

    /// Helper to create a RootNotation from a note name string.
    fn root(name: &str) -> RootNotation {
        RootNotation::from_note_name(MusicalNote::from_string(name).unwrap())
    }

    /// Create a simple test chart with known chord positions.
    fn create_test_chart() -> Chart {
        // Create a chart with 2 measures, each with chords on specific beats
        let mut chart = Chart::new();
        chart.time_signature = Some(TimeSignature::new(4, 4));

        // Measure 1: Chord on beat 0
        let measure1 = Measure {
            chords: vec![
                ChordInstance::new(
                    root("C"),
                    "C".to_string(),
                    Chord::new(root("C"), ChordQuality::Major),
                    ChordRhythm::Default,
                    "C".to_string(),
                    MusicalDuration::new(0, 4, 0), // whole note
                    AbsolutePosition::new(
                        MusicalPosition::try_new(0, 0, 0).unwrap(), // beat 0
                        0, // section index
                    ),
                ),
            ],
            ..Default::default()
        };

        // Measure 2: Two chords - beat 0 and beat 2
        let measure2 = Measure {
            chords: vec![
                ChordInstance::new(
                    root("G"),
                    "G".to_string(),
                    Chord::new(root("G"), ChordQuality::Major),
                    ChordRhythm::Default,
                    "G".to_string(),
                    MusicalDuration::new(0, 2, 0), // half note
                    AbsolutePosition::new(
                        MusicalPosition::try_new(1, 0, 0).unwrap(), // measure 1, beat 0
                        0, // section index
                    ),
                ),
                ChordInstance::new(
                    root("D"),
                    "Dm".to_string(),
                    Chord::new(root("D"), ChordQuality::Minor),
                    ChordRhythm::Default,
                    "Dm".to_string(),
                    MusicalDuration::new(0, 2, 0), // half note
                    AbsolutePosition::new(
                        MusicalPosition::try_new(1, 2, 0).unwrap(), // measure 1, beat 2
                        0, // section index
                    ),
                ),
            ],
            ..Default::default()
        };

        let mut section_info = Section::new(SectionType::Verse);
        section_info.number = Some(1);
        section_info.measure_count = Some(2);

        let section = ChartSection::new(section_info)
            .with_measures(vec![measure1, measure2]);

        chart.sections.push(section);
        chart
    }

    /// Test that chord symbols are extracted from the scene graph.
    #[test]
    fn test_find_chord_symbols_in_scene() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Count chord symbols in the scene
        let chord_count = count_elements_by_type(&result.scene, ElementType::ChordSymbol);

        // Should have 3 chord symbols total (1 in measure 1, 2 in measure 2)
        assert_eq!(chord_count, 3, "Expected 3 chord symbols, found {}", chord_count);
    }

    /// Test that measures are present in the scene.
    #[test]
    fn test_find_measures_in_scene() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Count measure containers in the scene
        // Note: The layout creates a measure container for each measure in each system
        let measure_count = count_elements_by_type(&result.scene, ElementType::Measure);

        // With 2 chart measures on 1 system, we get 2 measure containers
        // But the MeasureBuilder also creates internal measure structure
        // So we just verify we have at least 2 measures
        assert!(measure_count >= 2, "Expected at least 2 measures, found {}", measure_count);
    }

    /// Test that chart layout produces valid result structure.
    #[test]
    fn test_chart_layout_result_structure() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Verify result has valid dimensions
        assert!(result.total_width > 0.0, "Total width should be positive");
        assert!(result.total_height > 0.0, "Total height should be positive");

        // Verify we have at least one page
        assert!(!result.pages.is_empty(), "Should have at least one page");
    }

    /// Test that chord symbols are present for each chord in the chart.
    #[test]
    fn test_chord_symbol_count_matches_chart() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        // Count total chords in the chart
        let chart_chord_count: usize = chart.sections.iter()
            .flat_map(|s| s.measures())
            .map(|m| m.chords.len())
            .sum();

        let result = engine.layout_chart(&chart, &LayoutMode::default());
        let scene_chord_count = count_elements_by_type(&result.scene, ElementType::ChordSymbol);

        assert_eq!(
            scene_chord_count, chart_chord_count,
            "Scene should have same number of chord symbols as chart: expected {}, got {}",
            chart_chord_count, scene_chord_count
        );
    }

    /// Test that chord symbol positions are accessible via transforms.
    /// This verifies the transform-based positioning infrastructure works.
    #[test]
    fn test_chord_symbol_world_positions() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        let result = engine.layout_chart(&chart, &LayoutMode::default());
        let chord_symbols = find_elements_by_type(&result.scene, ElementType::ChordSymbol);

        // All chord symbols should have positive x positions (transforms are now applied)
        for (i, cs) in chord_symbols.iter().enumerate() {
            assert!(
                cs.world_x > 0.0,
                "Chord symbol {} should have positive world x position, got {}",
                i, cs.world_x
            );
            println!("Chord symbol {}: world_x={:.1}, world_y={:.1}", i, cs.world_x, cs.world_y);
        }

        // Chord symbols should be in increasing x order within each measure
        // The last two chords (G and Dm in measure 2) should have G.x < Dm.x
        if chord_symbols.len() >= 2 {
            let dm_chord = &chord_symbols[chord_symbols.len() - 1]; // Dm is last
            let g_chord = &chord_symbols[chord_symbols.len() - 2];  // G is second-to-last

            assert!(
                dm_chord.world_x > g_chord.world_x,
                "Dm chord (beat 2) should be to the right of G chord (beat 0): Dm.x={:.1} should be > G.x={:.1}",
                dm_chord.world_x, g_chord.world_x
            );
        }
    }

    /// Test that chord symbols in different measures are positioned correctly.
    #[test]
    fn test_chord_symbol_positions_across_measures() {
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let chart = create_test_chart();

        let result = engine.layout_chart(&chart, &LayoutMode::default());
        let chord_symbols = find_elements_by_type(&result.scene, ElementType::ChordSymbol);

        // We have 3 chords: C (measure 1), G (measure 2, beat 0), Dm (measure 2, beat 2)
        assert_eq!(chord_symbols.len(), 3);

        let c_chord = &chord_symbols[0];  // C in measure 1
        let g_chord = &chord_symbols[1];  // G in measure 2
        let dm_chord = &chord_symbols[2]; // Dm in measure 2

        // G should be to the right of C (different measure)
        assert!(
            g_chord.world_x > c_chord.world_x,
            "G (measure 2) should be right of C (measure 1): G.x={:.1} > C.x={:.1}",
            g_chord.world_x, c_chord.world_x
        );

        // Dm should be to the right of G (same measure, later beat)
        assert!(
            dm_chord.world_x > g_chord.world_x,
            "Dm (beat 2) should be right of G (beat 0): Dm.x={:.1} > G.x={:.1}",
            dm_chord.world_x, g_chord.world_x
        );

        // Print positions for verification
        println!("C chord (m1): x={:.1}", c_chord.world_x);
        println!("G chord (m2, b0): x={:.1}", g_chord.world_x);
        println!("Dm chord (m2, b2): x={:.1}", dm_chord.world_x);
    }

    /// Test that all measures on the same system have equal width.
    /// Uses "Autumn Leaves" chart - all sections have 4 measures with 1 chord each.
    /// Because content is identical, all 4 measures per line should have equal width.
    #[test]
    fn test_equal_measure_widths_autumn_leaves() {
        let autumn_leaves = r#"
Autumn Leaves - Joseph Kosma
120bpm 4/4 #G

intro 4
Gmaj7 Cmaj7 F#m7b5 B7

vs 8
Em7 Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em

ch 8
Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em7 E7

br 4
Am7 D7 Gmaj7 B7

outro 4
Em7 Am7 D7 Gmaj7
"#;

        let chart = Chart::parse(autumn_leaves).expect("Failed to parse Autumn Leaves chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Get all measure positions - these now have page metadata
        let measures = find_elements_by_type(&result.scene, ElementType::Measure);

        // Deduplicate measures by position (MeasureBuilder creates nested elements)
        // Keep only unique (x, y, page) positions with page metadata
        use std::collections::{HashMap, HashSet};
        let mut seen_positions: HashSet<(i64, i64, u32)> = HashSet::new();
        let unique_measures: Vec<_> = measures.iter()
            .filter(|m| {
                if let Some(page) = m.page {
                    let key = (m.world_x.round() as i64, m.world_y.round() as i64, page);
                    seen_positions.insert(key)
                } else {
                    false // Skip measures without page metadata
                }
            })
            .collect();

        println!("Found {} unique measure positions with page tags:", unique_measures.len());
        for (i, m) in unique_measures.iter().enumerate() {
            println!("  Measure {}: x={:.1}, y={:.1}, page={:?}", i, m.world_x, m.world_y, m.page);
        }

        // Group measures by (page, y_position) - same page + same y = same system
        let mut measures_by_system: HashMap<(u32, i64), Vec<&ElementPosition>> = HashMap::new();
        for m in &unique_measures {
            if let Some(page) = m.page {
                let system_key = (page, m.world_y.round() as i64);
                measures_by_system.entry(system_key).or_default().push(m);
            }
        }

        // Verify equal widths within each system (page + y group)
        for ((page, system_y), system_measures) in measures_by_system.iter() {
            if system_measures.len() < 2 {
                continue;
            }

            // Sort by x position
            let mut sorted: Vec<_> = system_measures.clone();
            sorted.sort_by(|a, b| a.world_x.partial_cmp(&b.world_x).unwrap());

            // Calculate widths (distance between consecutive measures)
            let widths: Vec<f64> = sorted
                .windows(2)
                .map(|pair| pair[1].world_x - pair[0].world_x)
                .collect();

            if widths.is_empty() {
                continue;
            }

            // All widths should be approximately equal (within 0.1 points tolerance)
            let first_width = widths[0];
            let tolerance = 0.1;

            for (i, &width) in widths.iter().enumerate() {
                let diff = (width - first_width).abs();
                assert!(
                    diff <= tolerance,
                    "Page {}, system y={}: Measure {} width ({:.1}) differs from measure 0 width ({:.1}) by {:.3}",
                    page, system_y, i + 1, width, first_width, diff
                );
            }

            println!(
                "Page {}, system y={}: {} measures, all widths equal ({:.1} points)",
                page, system_y, sorted.len(), first_width
            );
        }

        // Also verify total measure count matches chart
        let expected_measures: usize = chart.sections.iter()
            .map(|s| s.measures().len())
            .sum();

        // Account for MeasureBuilder creating internal structure
        assert!(
            measures.len() >= expected_measures,
            "Expected at least {} measures, found {}",
            expected_measures, measures.len()
        );
    }

    /// Test that page metadata is correctly assigned to elements.
    #[test]
    fn test_page_metadata_assigned() {
        let autumn_leaves = r#"
Autumn Leaves - Joseph Kosma
120bpm 4/4 #G

intro 4
Gmaj7 Cmaj7 F#m7b5 B7

vs 8
Em7 Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em
"#;

        let chart = Chart::parse(autumn_leaves).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Check that measures have page metadata
        let measures = find_elements_by_type(&result.scene, ElementType::Measure);
        let measures_with_page: Vec<_> = measures.iter().filter(|m| m.page.is_some()).collect();

        println!("Total measures: {}, with page metadata: {}", measures.len(), measures_with_page.len());

        // At least the outer measure containers should have page metadata
        assert!(
            !measures_with_page.is_empty(),
            "Expected some measures to have page metadata"
        );

        // Check that chord symbols have page metadata
        let chords = find_elements_by_type(&result.scene, ElementType::ChordSymbol);
        let chords_with_page: Vec<_> = chords.iter().filter(|c| c.page.is_some()).collect();

        println!("Total chord symbols: {}, with page metadata: {}", chords.len(), chords_with_page.len());

        assert!(
            !chords_with_page.is_empty(),
            "Expected chord symbols to have page metadata"
        );

        // Group by page and verify distribution
        let measures_cloned: Vec<_> = measures_with_page.iter().map(|m| (*m).clone()).collect();
        let measures_by_page = group_elements_by_page(&measures_cloned);
        println!("Measures distributed across {} page(s)", measures_by_page.len());
        for (page, page_measures) in &measures_by_page {
            println!("  Page {}: {} measures", page, page_measures.len());
        }
    }

    // ======================================================================
    // Layout Metrics Tests
    // ======================================================================

    /// Test that a typical chart fits 9 systems on a page with new spacing.
    #[test]
    fn test_systems_per_page_count() {
        // Create a chart with enough sections to require multiple systems
        let chart_text = r#"
Song - Artist
120bpm 4/4 #C

intro 4
C G Am F

vs1 8
C G Am F x2

ch 8
F C G Am x2

vs2 8
C G Am F x2

ch 8
F C G Am x2

br 4
Dm G C Am

ch 8
F C G Am x2

outro 4
C G Am F
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Get metrics for page 1
        let metrics = result.page_metrics(1).expect("Should have page 1");

        println!("\n=== System Count Test ===");
        metrics.print_debug();

        // With 20pt system spacing, we should fit 10 systems on a page
        // (10×50pt systems + 9×20pt spacing = 680pt, available = 692pt)
        assert!(
            metrics.system_count >= 9,
            "Expected at least 9 systems on page 1, got {}. \
            Available height: {:.1}, content height: {:.1}",
            metrics.system_count, metrics.available_height, metrics.content_height
        );

        // Should not exceed 10 systems
        assert!(
            metrics.system_count <= 10,
            "Expected at most 10 systems on page 1, got {}",
            metrics.system_count
        );
    }

    /// Test inter-system spacing is consistent.
    #[test]
    fn test_inter_system_spacing_consistency() {
        let chart_text = r#"
Song - Artist
120bpm 4/4 #C

vs 32
C G Am F x8
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let metrics = result.page_metrics(1).expect("Should have page 1");

        println!("\n=== Inter-System Spacing Test ===");
        metrics.print_debug();

        // All inter-system spacing should be equal (within 0.1 points)
        if metrics.inter_system_spacing.len() >= 2 {
            let first_spacing = metrics.inter_system_spacing[0];
            for (i, &spacing) in metrics.inter_system_spacing.iter().enumerate() {
                let diff = (spacing - first_spacing).abs();
                assert!(
                    diff <= 0.1,
                    "Inter-system spacing {} ({:.1}) differs from first ({:.1}) by {:.2}",
                    i + 1, spacing, first_spacing, diff
                );
            }
            println!("All inter-system spacings are consistent: {:.1} points", first_spacing);
        }
    }

    /// Test that last system doesn't overflow the bottom margin.
    #[test]
    fn test_last_system_to_bottom_margin() {
        let chart_text = r#"
Song - Artist
120bpm 4/4 #C

vs 32
C G Am F x8
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        for page_metrics in result.all_page_metrics() {
            println!("\n=== Page {} Bottom Margin Test ===", page_metrics.page_number);
            page_metrics.print_debug();

            // Last system should not overflow the bottom margin
            assert!(
                page_metrics.last_system_to_bottom >= 0.0,
                "Page {}: Last system overflows bottom margin by {:.1} points",
                page_metrics.page_number, -page_metrics.last_system_to_bottom
            );

            // Should have reasonable bottom space (not too much empty space)
            let max_bottom_space = page_metrics.available_height * 0.35;
            assert!(
                page_metrics.last_system_to_bottom <= max_bottom_space,
                "Page {}: Too much empty space at bottom: {:.1} points ({:.0}%)",
                page_metrics.page_number,
                page_metrics.last_system_to_bottom,
                (page_metrics.last_system_to_bottom / page_metrics.available_height) * 100.0
            );
        }
    }

    /// Test spacing check warnings.
    #[test]
    fn test_spacing_check_warnings() {
        let chart_text = r#"
Song - Artist
120bpm 4/4 #C

vs 16
C G Am F x4
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let metrics = result.page_metrics(1).expect("Should have page 1");

        // Check with reasonable min/max spacing (20-60 points)
        let warnings = metrics.check_spacing(20.0, 60.0);

        println!("\n=== Spacing Check Test ===");
        metrics.print_debug();

        if warnings.is_empty() {
            println!("No spacing warnings - all systems properly spaced");
        } else {
            for warning in &warnings {
                println!("Warning: {}", warning);
            }
        }

        // With default config, we shouldn't have critical warnings
        // (systems too close or content overflow)
        let critical_warnings: Vec<_> = warnings.iter()
            .filter(|w| w.contains("too close") || w.contains("extends past"))
            .collect();

        assert!(
            critical_warnings.is_empty(),
            "Found critical spacing warnings: {:?}",
            critical_warnings
        );
    }

    /// Example test demonstrating debug output for a real chart.
    #[test]
    fn example_page_layout_debug_output() {
        let autumn_leaves = r#"
Autumn Leaves - Joseph Kosma
120bpm 4/4 #G

intro 4
Gmaj7 Cmaj7 F#m7b5 B7

vs 8
Em7 Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em

ch 8
Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em7 E7

br 4
Am7 D7 Gmaj7 B7

outro 4
Em7 Am7 D7 Gmaj7
"#;

        let chart = Chart::parse(autumn_leaves).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== Autumn Leaves Layout Debug ===");
        println!("Total pages: {}", result.pages.len());
        println!("Total dimensions: {:.1} × {:.1}", result.total_width, result.total_height);
        println!();

        for metrics in result.all_page_metrics() {
            metrics.print_debug();

            // Also show check results
            let warnings = metrics.check_spacing(15.0, 50.0);
            if !warnings.is_empty() {
                println!("Warnings:");
                for w in &warnings {
                    println!("  - {}", w);
                }
                println!();
            }
        }

        // Verify the layout is reasonable
        let page1 = result.page_metrics(1).unwrap();
        assert!(page1.system_count >= 5, "Should have at least 5 systems on page 1");
    }

    /// Test that header space is accounted for on first page.
    #[test]
    fn test_first_page_header_space() {
        let chart_text = r#"
My Long Song Title - Famous Artist Name
120bpm 4/4 #C

vs 32
C G Am F x8
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        // Get metrics for multiple pages
        let all_metrics = result.all_page_metrics();

        println!("\n=== First Page Header Space Test ===");
        for m in &all_metrics {
            println!(
                "Page {}: {} systems, first system at y={:.1}",
                m.page_number, m.system_count, m.system_y_positions.first().unwrap_or(&0.0)
            );
        }

        // First page should have systems starting at margin.top
        // (header text is handled separately in the renderer)
        if let Some(page1) = all_metrics.first() {
            let first_system_y = page1.system_y_positions.first().copied().unwrap_or(0.0);
            assert!(
                first_system_y >= page1.margins.top - 1.0, // Allow 1pt tolerance
                "First system should start at or below top margin: y={:.1}, margin={:.1}",
                first_system_y, page1.margins.top
            );
        }
    }

    /// Test multi-page layout with extended chart (Autumn Leaves Extended).
    /// This chart has ~120 measures across 10 sections, requiring ~4 pages.
    #[test]
    fn test_multipage_layout_extended_chart() {
        let extended_chart = r#"
Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 A7 Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4
Am7 D7 Bm7 E7

ch 16
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 E7
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 Em7

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4

ch 16

br 8
Cmaj7 Bm7 Am7 Gmaj7
F#m7b5 B7 Em7 A7

inst 16
Gmaj7 Cmaj7 F#m7b5 B7
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 G7

ch 16

outro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 Am7 D7 Gmaj7
"#;

        let chart = Chart::parse(extended_chart).expect("Failed to parse extended chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== Extended Chart Multi-Page Layout Test ===");
        println!("Sections: {}", chart.sections.len());
        println!("Total measures: {}", chart.sections.iter().map(|s| s.measures().len()).sum::<usize>());
        println!("Total pages: {}", result.pages.len());
        println!();

        // Should span multiple pages
        assert!(
            result.pages.len() >= 3,
            "Extended chart should require at least 3 pages, got {}",
            result.pages.len()
        );

        // Print metrics for each page
        let mut total_systems = 0;
        for metrics in result.all_page_metrics() {
            println!(
                "Page {}: {} systems, content height {:.1}pt, remaining {:.1}pt",
                metrics.page_number,
                metrics.system_count,
                metrics.content_height,
                metrics.last_system_to_bottom
            );
            total_systems += metrics.system_count;

            // Each page should have reasonable system count
            assert!(
                metrics.system_count >= 1,
                "Page {} should have at least 1 system",
                metrics.page_number
            );
            assert!(
                metrics.system_count <= 10,
                "Page {} should have at most 10 systems, got {}",
                metrics.page_number, metrics.system_count
            );

            // No overflow
            assert!(
                metrics.last_system_to_bottom >= -1.0, // 1pt tolerance for rounding
                "Page {}: Content overflows bottom margin by {:.1}pt",
                metrics.page_number, -metrics.last_system_to_bottom
            );
        }

        // Total systems should be roughly 32+ (chart has ~128 measures at 4 per system)
        // Parser may add slightly more due to smart chord memory
        assert!(
            total_systems >= 30 && total_systems <= 40,
            "Expected ~30-40 total systems across all pages, got {}",
            total_systems
        );

        println!("\nTotal systems across all pages: {}", total_systems);
        println!("Systems per page average: {:.1}", total_systems as f64 / result.pages.len() as f64);
    }

    /// Test that duplicate consecutive chords are hidden when setting is enabled.
    #[test]
    fn test_hide_repeated_chords() {
        // Chart with repeated chords: C C C G G C
        // With hide_repeated_chords=true, should show: C _ _ G _ C
        let chart_text = r#"
Test - Artist
120bpm 4/4 #C

vs 6
C C C G G C
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        // Test with hide_repeated_chords = true (default)
        let engine = ChartLayoutEngine::new(style, text_font.clone(), symbol_font.clone());
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let chord_count_hidden = count_elements_by_type(&result.scene, ElementType::ChordSymbol);
        println!("With hide_repeated_chords=true: {} chord symbols rendered", chord_count_hidden);

        // With hiding, we should see: C, G, C = 3 unique chord changes
        assert_eq!(
            chord_count_hidden, 3,
            "Expected 3 chord symbols with hiding enabled (C, G, C), got {}",
            chord_count_hidden
        );

        // Test with hide_repeated_chords = false
        let mut config = ChartLayoutConfig::default();
        config.hide_repeated_chords = false;
        let engine_no_hide = ChartLayoutEngine::with_config(
            config, style, text_font, symbol_font
        );
        let result_no_hide = engine_no_hide.layout_chart(&chart, &LayoutMode::default());

        let chord_count_all = count_elements_by_type(&result_no_hide.scene, ElementType::ChordSymbol);
        println!("With hide_repeated_chords=false: {} chord symbols rendered", chord_count_all);

        // Without hiding, we should see all 6 chords
        assert_eq!(
            chord_count_all, 6,
            "Expected 6 chord symbols with hiding disabled, got {}",
            chord_count_all
        );
    }

    /// Test that chord hiding works across measure boundaries.
    #[test]
    fn test_hide_repeated_chords_across_measures() {
        // Chart where the same chord spans multiple measures
        let chart_text = r#"
Test - Artist
120bpm 4/4 #C

vs 8
C C C C G G G G
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let chord_count = count_elements_by_type(&result.scene, ElementType::ChordSymbol);
        println!("Chord symbols rendered: {}", chord_count);

        // With 4 Cs followed by 4 Gs, we should see: C, G = 2 unique chord changes
        assert_eq!(
            chord_count, 2,
            "Expected 2 chord symbols (C, G), got {}",
            chord_count
        );
    }

    /// Test that chord hiding resets at section boundaries (rehearsal marks).
    /// The first chord of each section should always show, even if it's the same
    /// as the last chord of the previous section.
    #[test]
    fn test_chord_shows_at_section_boundary() {
        // Verse ends with C, Chorus starts with C - both should show
        let chart_text = r#"
Test - Artist
120bpm 4/4 #C

vs 4
G Am F C

ch 4
C G Am F
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let chord_count = count_elements_by_type(&result.scene, ElementType::ChordSymbol);
        println!("Chord symbols rendered: {}", chord_count);

        // Verse: G, Am, F, C = 4 unique chords
        // Chorus: C (shows because new section), G, Am, F = 4 chords
        // Total = 8 chord symbols
        assert_eq!(
            chord_count, 8,
            "Expected 8 chord symbols (section boundary resets tracking), got {}",
            chord_count
        );
    }

    /// Test that repeated chords within a section are still hidden.
    #[test]
    fn test_repeated_chords_hidden_within_section() {
        let chart_text = r#"
Test - Artist
120bpm 4/4 #C

vs 4
C C G G

ch 4
Am Am F F
"#;
        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        let chord_count = count_elements_by_type(&result.scene, ElementType::ChordSymbol);
        println!("Chord symbols rendered: {}", chord_count);

        // Verse: C, G = 2 unique chords (duplicates hidden)
        // Chorus: Am, F = 2 unique chords (duplicates hidden)
        // Total = 4 chord symbols
        assert_eq!(
            chord_count, 4,
            "Expected 4 chord symbols (duplicates hidden within sections), got {}",
            chord_count
        );
    }

    /// Debug test to see what chords are being parsed from the example chart.
    #[test]
    fn debug_example_chart_chords() {
        let chart_text = r#"
Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 A7 Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4
Am7 D7 Bm7 E7

ch 16
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 E7
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 Em7
"#;

        let chart = Chart::parse(chart_text).expect("Failed to parse chart");

        println!("\n=== Debug: Parsed Chart Chords ===");
        println!("Total sections: {}", chart.sections.len());

        for (section_idx, section) in chart.sections.iter().enumerate() {
            println!("\nSection {}: {:?} ({} measures)",
                section_idx,
                section.section.section_type,
                section.measures().len()
            );

            for (measure_idx, measure) in section.measures().iter().enumerate() {
                print!("  Measure {}: ", measure_idx);
                for chord in &measure.chords {
                    print!("'{}' ", chord.full_symbol);
                }
                println!("({} chords)", measure.chords.len());
            }
        }

        // Check if any chord has "C5" in it
        let mut c5_found = false;
        for section in &chart.sections {
            for measure in section.measures() {
                for chord in &measure.chords {
                    if chord.full_symbol.contains("C5") || chord.full_symbol == "C" {
                        println!("\nFound chord with 'C' or 'C5': '{}'", chord.full_symbol);
                        c5_found = true;
                    }
                }
            }
        }

        if !c5_found {
            println!("\nNo 'C5' or plain 'C' chord found in parsed chart");
        }
    }

    #[test]
    fn debug_verse_bar5_keyflow() {
        // Test what keyflow parses for bar 5 of first verse
        // vs 16 with only 4 chords specified - what fills measures 5-16?
        let chart_text = r#"Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 A7 Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
"#;

        let chart = Chart::parse(chart_text).expect("Failed to parse chart");

        println!("\n=== KEYFLOW LEVEL: Verse Section Analysis ===");

        // Find the verse section
        let verse_section = chart.sections.iter()
            .find(|s| matches!(s.section.section_type, SectionType::Verse))
            .expect("Should have a verse section");

        println!("Verse section: {} measures declared", verse_section.measures().len());

        // Print all measures with their chords
        for (i, measure) in verse_section.measures().iter().enumerate() {
            let chords: Vec<&str> = measure.chords.iter()
                .map(|c| c.full_symbol.as_str())
                .collect();
            println!("  Bar {}: {:?}", i + 1, chords);

            // Highlight bar 5 specifically
            if i == 4 {
                println!("    ^^^ BAR 5 - Is there a C chord here?");
                for chord in &measure.chords {
                    if chord.full_symbol.starts_with('C') || chord.full_symbol == "C" {
                        println!("    !!! FOUND: '{}' in bar 5", chord.full_symbol);
                    }
                }
            }
        }
    }

    #[test]
    fn debug_verse_bar5_scene_graph() {
        // Test what the scene graph renders for bar 5 of first verse
        let chart_text = r#"Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 A7 Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
"#;

        let chart = Chart::parse(chart_text).expect("Failed to parse chart");

        let style = Box::leak(Box::new(MStyle::default()));
        let engine = ChartLayoutEngine::new(
            style,
            Arc::new(Vec::new()),
            Arc::new(Vec::new()),
        );

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== SCENE GRAPH LEVEL: Chord Nodes with Measure Metadata ===");

        // Find all chord nodes and group by section/measure
        fn find_chords_with_measure(node: &SceneNode, chords: &mut Vec<(String, Option<u32>, Option<String>)>) {
            // Check if this is a chord node
            if node.metadata.get("element_type") == Some(&"chord".to_string()) {
                // Extract the text content
                let mut chord_text = String::new();
                for cmd in &node.commands {
                    if let PaintCommand::Text { text, .. } = cmd {
                        chord_text.push_str(text);
                    }
                }

                let measure = node.metadata.get("measure")
                    .and_then(|m| m.parse::<u32>().ok());
                let section_type = node.metadata.get("section_type").cloned();

                chords.push((chord_text, measure, section_type));
            }

            for child in &node.children {
                find_chords_with_measure(child, chords);
            }
        }

        let mut chords = Vec::new();
        find_chords_with_measure(&result.scene, &mut chords);

        // Group by section and print
        println!("\nAll rendered chords:");
        let mut current_section = String::new();
        for (chord, measure, section) in &chords {
            let section_name = section.as_deref().unwrap_or("unknown");
            if section_name != current_section {
                println!("\n  {} section:", section_name);
                current_section = section_name.to_string();
            }
            println!("    Measure {}: '{}'", measure.unwrap_or(999), chord);
        }

        // Specifically check bar 5 of verse (measure index 4)
        println!("\n=== BAR 5 OF VERSE (measure index 4) ===");
        let verse_bar5_chords: Vec<_> = chords.iter()
            .filter(|(_, measure, section)| {
                section.as_deref() == Some("Verse") && *measure == Some(4)
            })
            .collect();

        if verse_bar5_chords.is_empty() {
            println!("No chords rendered for verse bar 5 (might be hidden as duplicate)");
        } else {
            for (chord, _, _) in verse_bar5_chords {
                println!("  Rendered: '{}'", chord);
                if chord.starts_with('C') || chord == "C" {
                    println!("  !!! FOUND C chord in bar 5!");
                }
            }
        }
    }

    #[test]
    fn debug_scene_paint_commands() {
        // Trace all text/glyph commands in the rendered scene to find "C5"
        // Using full DEFAULT_CHART_TEXT to see if C5 appears after first verse
        let chart_text = r#"Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
Gmaj7 Cmaj7 F#m7b5 B7
Em7 A7 Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4
Am7 D7 Bm7 E7
"#;

        let chart = Chart::parse(chart_text).expect("Failed to parse chart");

        // Create minimal layout config for testing
        let style = Box::leak(Box::new(MStyle::default()));
        let engine = ChartLayoutEngine::new(
            style,
            Arc::new(Vec::new()), // text font
            Arc::new(Vec::new()), // symbol font
        );

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== Debug: Scene Paint Commands ===");

        // Recursive function to collect all text commands
        fn collect_text_commands(node: &SceneNode, depth: usize) {
            let indent = "  ".repeat(depth);

            // Check node metadata for element type
            if let Some(elem_type) = node.metadata.get("element_type") {
                println!("{}Node: {} ({})", indent, elem_type, node.commands.len());
            }

            // Check paint commands
            for cmd in &node.commands {
                match cmd {
                    PaintCommand::Text { text, .. } => {
                        println!("{}  Text: '{}'", indent, text);
                        if text.contains('C') || text.contains('5') {
                            println!("{}  ^^^ FOUND C or 5 in text!", indent);
                        }
                    }
                    PaintCommand::Glyph { codepoint, .. } => {
                        println!("{}  Glyph: U+{:04X} ('{}')", indent, *codepoint as u32, codepoint);
                    }
                    _ => {}
                }
            }

            // Recurse into children
            for child in &node.children {
                collect_text_commands(child, depth + 1);
            }
        }

        collect_text_commands(&result.scene, 0);
    }

    #[test]
    fn test_short_system_width() {
        // Test that short systems (< 4 measures) don't stretch to full width
        // Like LilyPond's pseudo-indent system
        let chart_text = r#"Short Line Test - Test Artist
120bpm 4/4 #C

vs 6
C G Am F | C G
"#;

        let chart = Chart::parse(chart_text).expect("Failed to parse chart");
        let style = test_style();
        let text_font = Arc::new(Vec::new());
        let symbol_font = Arc::new(Vec::new());

        let engine = ChartLayoutEngine::new(style, text_font, symbol_font);
        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== Short System Width Test ===");

        // Find all staff line commands and their widths
        fn find_staff_line_widths(node: &SceneNode, widths: &mut Vec<f64>) {
            for cmd in &node.commands {
                if let PaintCommand::Line { start, end, .. } = cmd {
                    // Staff lines are horizontal (same y coordinate)
                    if (start.y - end.y).abs() < 0.1 {
                        let width = (end.x - start.x).abs();
                        if width > 100.0 {
                            // Only count substantial lines (staff lines, not decorations)
                            widths.push(width);
                        }
                    }
                }
            }
            for child in &node.children {
                find_staff_line_widths(child, widths);
            }
        }

        let mut staff_widths = Vec::new();
        find_staff_line_widths(&result.scene, &mut staff_widths);

        // Group by similar widths (staff lines come in groups of 5)
        let mut unique_widths: Vec<f64> = Vec::new();
        for width in &staff_widths {
            if !unique_widths.iter().any(|w| (w - width).abs() < 1.0) {
                unique_widths.push(*width);
            }
        }
        unique_widths.sort_by(|a, b| a.partial_cmp(b).unwrap());

        println!("Staff line width groups: {:?}", unique_widths);

        // We should have at least 2 different widths:
        // - Full width for 4-measure system
        // - Shorter width for 2-measure system
        // (6 measures = 1 system of 4 + 1 system of 2)
        if unique_widths.len() >= 2 {
            let short_width = unique_widths[0];
            let full_width = unique_widths[unique_widths.len() - 1];
            println!("Short system width: {:.1}", short_width);
            println!("Full system width: {:.1}", full_width);

            // Short system should be approximately 50% of full width (2/4 measures)
            let ratio = short_width / full_width;
            println!("Ratio (short/full): {:.2}", ratio);

            assert!(
                ratio < 0.75,
                "Short system should be significantly narrower than full system. Ratio: {:.2}",
                ratio
            );
        }
    }
}

// endregion: --- Tests
