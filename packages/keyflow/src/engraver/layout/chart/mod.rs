//! Chart Layout Engine
//!
//! Converts Keyflow Chart data to engraver SceneNode trees for rendering.
//! Supports both paginated (MuseScore-style) and continuous scroll modes.

pub mod adapters;
pub mod chord_layout;
pub mod chord_renderer;
pub mod config;
pub mod constants;
pub mod layout_state;
pub mod measure_layout;
pub mod page_rendering;
pub mod pipeline;
pub mod prefix_renderer;
pub mod rhythm_builder;
pub mod section_layout;
pub mod types;

// Re-export new config types (ChartLayoutConfig is still defined locally for backward compatibility)
pub use config::{BehavioralFlags, LayoutParams, RenderOptions, DEFAULT_MIN_CHORD_SYMBOL_GAP};
// Note: config::ChartLayoutConfig and config::FlatChartLayoutConfig are available
// but not re-exported to avoid conflict with the legacy struct below

// Re-export main types for convenience
pub use types::{
    BeatPosition, ChartLayoutResult, LayoutMode, MeasureMelodyData, MelodyNoteSegment,
    PageLayoutMetrics, PushSpillback, detect_push_spillbacks, detect_section_start_spillback,
    expand_melodies_across_measures, slash_glyph_for_ticks,
};

use std::sync::Arc;

use crate::engraver::layout::context::LayoutContext;
use crate::engraver::layout::orchestrator::{PageLayout, PageMargins, SystemLayout};
use crate::engraver::layout::segment::SegmentType;
use crate::engraver::layout::segment_list::SegmentList;
use crate::engraver::layout::text_metrics::TextFontMetrics;
use crate::engraver::layout::tlayout::{
    BarlineType, ClefParams, ClefType, HarmonyParams, HarmonyStyle, MarginLabelParams,
    NoteHeadType, RestDuration, RestParams, SlurDirection, SlurEndpoint, SlurTieConfig,
    TimeSigParams, TimeSigType, layout_clef, layout_harmony, layout_margin_label, layout_rest,
    layout_tie, layout_timesig, parse_chord, rehearsal_themes,
};
use crate::engraver::model::{DurationKind, NoteHead};
use crate::engraver::notation::{Duration, MeasureBuilder, MeasureScene, RhythmEntry, TupletRatio, TupletSpec};
use crate::engraver::scene::id::{ElementType, SemanticId};
use crate::engraver::scene::node::{SceneNode, metadata_keys};
use crate::engraver::scene::paint::{FontStyle, FontWeight, PaintCommand, TextAnchor};
use crate::engraver::style::MStyle;
use crate::chart::types::{Measure, RhythmElement};
use crate::chord::{ChordRhythm, LilySyntax, PushPullBase};
use crate::sections::SectionType;
use crate::time::TimeSignature;
use crate::{Chart, ChartPosition, SourceLink};
use kurbo::{Affine, Rect};
use vello::peniko::Color;


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
    /// Use stemmed rhythm notation (for charts with push/pull timing).
    /// When false (default), uses stemless slash notation.
    /// When true, uses stemmed rhythmic notation with beams and triplet brackets.
    pub use_stems: bool,
    /// Automatically fill whole/half notes with quarter note slashes.
    /// When true (default), chords with whole note or half note durations
    /// are automatically expanded to quarter note slashes:
    /// - A whole note chord (4 beats in 4/4) becomes 4 quarter slashes
    /// - A half note chord (2 beats) becomes 2 quarter slashes
    /// This is standard notation for master rhythm charts.
    /// Can be disabled with `/AUTO_RHYTHM_SLASHES=false` in the chart.
    pub auto_rhythm_slashes: bool,
    /// Show measure numbers above the first measure of each system.
    /// When true, displays the measure number (accounting for offset) above bars.
    pub show_measure_numbers: bool,
    /// Offset for measure numbering (from REAPER's projmeasoffs or song start).
    /// If the project starts at measure 5, set this to 4 so measure 1 displays as "5".
    pub measure_number_offset: i32,
    /// Number of count-in measures (0, 1, or 2).
    /// Determined by the distance between Count-In marker and SONGSTART marker.
    /// Count-in measures are the N measures immediately before measure 1.
    pub count_in_measures: u8,
    /// Snippet mode: use simple white background without shadow.
    pub snippet_mode: bool,
    /// Minimum horizontal gap between adjacent chord symbols (in points).
    /// When chord symbols would overlap or be closer than this gap,
    /// they are automatically pushed apart during rendering.
    /// Default is 4.0 points.
    pub min_chord_symbol_gap: f64,
}

impl Default for ChartLayoutConfig {
    fn default() -> Self {
        // Default is the master rhythm preset
        Self::master_rhythm()
    }
}

impl ChartLayoutConfig {
    /// Master Rhythm Chart preset.
    ///
    /// Optimized for professional master rhythm charts with:
    /// - A4-friendly margins (extra left margin for section labels)
    /// - Compact system spacing (~10 systems per page)
    /// - Stemless quarter slashes for sustained chords (auto_rhythm_slashes)
    /// - MuseJazz harmony style
    /// - 4 measures per system
    ///
    /// This is the standard format for rhythm section charts used in
    /// professional music production and live performance.
    #[must_use]
    pub fn master_rhythm() -> Self {
        Self {
            margins: PageMargins {
                top: 36.0,    // Reduced top for header closer to top
                right: 50.0,  // Standard right margin
                bottom: 50.0, // Standard bottom margin
                left: 72.0,   // Extra left margin for section labels
            },
            spatium: 5.0,
            // System spacing: 20pt (4 spatiums) allows ~10 systems per page
            // 10×50pt (system) + 9×20pt (spacing) = 680pt, fits in 756pt available
            system_spacing: 20.0,
            max_measures_per_system: 4,
            min_measure_width: 100.0,
            harmony_style: HarmonyStyle::musejazz(),
            hide_repeated_chords: true,  // Hide repeated chord symbols
            use_stems: true,             // Use stems for explicit rhythms (triplets, pushes)
            auto_rhythm_slashes: true,   // Auto-fill with stemless quarter slashes
            show_measure_numbers: true,  // Show measure numbers
            measure_number_offset: 0,    // No offset (measure 1 = 1)
            count_in_measures: 0,        // No count-in by default
            snippet_mode: false,         // Full page mode with shadow
            min_chord_symbol_gap: DEFAULT_MIN_CHORD_SYMBOL_GAP,
        }
    }

    /// Snippet preset for small embedded chart excerpts.
    ///
    /// Optimized for displaying chart snippets in documentation,
    /// tutorials, or inline displays with minimal margins.
    #[must_use]
    pub fn snippet() -> Self {
        Self {
            margins: PageMargins {
                top: 20.0,
                bottom: 20.0,
                left: 20.0,
                right: 20.0,
            },
            spatium: 5.0,
            system_spacing: 30.0,
            max_measures_per_system: 4,
            min_measure_width: 80.0,
            harmony_style: HarmonyStyle::musejazz(),
            hide_repeated_chords: false, // Show all chords in snippets
            use_stems: true,
            auto_rhythm_slashes: true,
            show_measure_numbers: true,
            measure_number_offset: 0,
            count_in_measures: 0,
            snippet_mode: true, // Simple white background
            min_chord_symbol_gap: DEFAULT_MIN_CHORD_SYMBOL_GAP,
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
            LayoutMode::Paginated {
                page_width,
                page_height,
            } => self.layout_paginated(chart, *page_width, *page_height),
            LayoutMode::ContinuousScroll { width } => self.layout_continuous(chart, *width),
            LayoutMode::Snippet { page_width } => {
                // Use tighter margins for snippets
                self.layout_snippet(chart, *page_width)
            }
        }
    }

    /// Layout a snippet with bounds-based sizing.
    /// Renders content once, measures bounds, then adjusts page background to fit.
    fn layout_snippet(&self, chart: &Chart, page_width: f64) -> ChartLayoutResult {
        // layout_paginated uses a fixed page_offset of 20.0 for positioning
        let page_offset = 20.0;

        // Step 1: Layout with snippet mode and small margins
        let snippet_margins = PageMargins {
            top: 5.0,
            bottom: 5.0,
            left: 50.0,
            right: 5.0,
        };

        let measure_config = ChartLayoutConfig {
            margins: snippet_margins.clone(),
            snippet_mode: true, // Simple white background
            ..self.config.clone()
        };

        let measure_engine = ChartLayoutEngine {
            config: measure_config,
            style: self.style,
            text_font_data: self.text_font_data.clone(),
            symbol_font_data: self.symbol_font_data.clone(),
        };

        // Layout with the provided page width (constrain measure widths to this)
        // Use a tall page height to fit all content vertically
        let layout_width = if page_width > 0.0 { page_width } else { 612.0 }; // Default to letter width
        let mut result = measure_engine.layout_paginated(chart, layout_width, 10000.0);

        // Step 2: Compute actual content bounds by examining children
        // Skip the first child which is page background (white rect in snippet mode)
        let mut content_bounds = Rect::ZERO;
        for (i, child) in result.scene.children.iter().enumerate() {
            // Skip first child (page background)
            if i < 1 {
                continue;
            }
            let child_bounds = child.compute_bounds();
            if !child_bounds.is_zero_area() {
                let transformed = child.transform.transform_rect_bbox(child_bounds);
                if content_bounds.is_zero_area() {
                    content_bounds = transformed;
                } else {
                    content_bounds = content_bounds.union(transformed);
                }
            }
        }

        // Step 3: Calculate final page dimensions from content bounds
        let padding = 5.0;
        let clef_bottom_padding = 8.0;

        // Content might extend above page_offset (chord symbols)
        let content_above_page = (page_offset - content_bounds.y0).max(0.0);

        // Final dimensions based on actual content
        let final_width = content_bounds.x1 + padding;
        let final_height = content_bounds.y1 + padding + clef_bottom_padding;

        // Step 4: Replace the page background with correctly sized one
        // The first child is the white background rect - replace it
        if !result.scene.children.is_empty() {
            let new_background = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
                Rect::new(page_offset, page_offset, final_width, final_height),
                vello::peniko::Color::WHITE,
            )]);
            result.scene.children[0] = new_background;
        }

        // If content extends above page_offset, shift all content down
        if content_above_page > 0.0 {
            let shift = Affine::translate((0.0, content_above_page));
            for (i, child) in result.scene.children.iter_mut().enumerate() {
                if i > 0 {
                    // Skip background
                    child.transform = shift * child.transform;
                }
            }
            // Update background to account for shifted content
            if !result.scene.children.is_empty() {
                let new_background = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
                    Rect::new(
                        page_offset,
                        page_offset,
                        final_width,
                        final_height + content_above_page,
                    ),
                    vello::peniko::Color::WHITE,
                )]);
                result.scene.children[0] = new_background;
            }
            // Update total dimensions with shifted content
            result.total_width = final_width + page_offset;
            result.total_height = final_height + content_above_page + page_offset;
        } else {
            // Update total dimensions to match actual content
            result.total_width = final_width + page_offset;
            result.total_height = final_height + page_offset;
        }

        result
    }

    /// Layout a chart with explicit configuration.
    ///
    /// This temporarily applies the given config for layout, then restores
    /// the engine's default config. Useful for charts with rhythmic complexity
    /// that need stemmed notation.
    pub fn layout_chart_with_config(
        &self,
        chart: &Chart,
        mode: &LayoutMode,
        config: &ChartLayoutConfig,
    ) -> ChartLayoutResult {
        // Create a temporary engine with the custom config
        let temp_engine = ChartLayoutEngine {
            config: config.clone(),
            style: self.style,
            text_font_data: self.text_font_data.clone(),
            symbol_font_data: self.symbol_font_data.clone(),
        };
        temp_engine.layout_chart(chart, mode)
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

        let harmony_style = self
            .config
            .harmony_style
            .clone()
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

        // Beat position collection for cursor/highlight rendering
        let mut beat_positions: Vec<BeatPosition> = Vec::new();
        let mut global_measure_index: usize = 0;

        // Calculate seconds per tick from tempo (480 ticks per quarter note)
        let tempo_bpm = chart.tempo.map(|t| t.bpm as f64).unwrap_or(120.0);
        let seconds_per_quarter = 60.0 / tempo_bpm;
        let seconds_per_tick = seconds_per_quarter / 480.0;

        // Get time signature for count-in duration calculation
        let time_signature = chart
            .time_signature
            .map(|ts| (ts.numerator as u8, ts.denominator as u8))
            .unwrap_or((4u8, 4u8));

        // Calculate count-in duration in ticks and seconds
        // Count-in should have NEGATIVE time values so that:
        // - Count-in measures have negative time (before SONGSTART)
        // - Real measures start at time 0 (at SONGSTART)
        let ticks_per_measure = time_signature.0 as i64 * (1920i64 / time_signature.1 as i64);
        let count_in_ticks = self.config.count_in_measures as i64 * ticks_per_measure;
        let count_in_seconds = count_in_ticks as f64 * seconds_per_tick;

        // Track cumulative time and ticks through the song
        // Start at negative offset for count-in so real measures begin at 0
        let mut cumulative_time: f64 = -count_in_seconds;
        let mut cumulative_ticks: i64 = -count_in_ticks;

        // Calculate page offset for multi-page rendering
        let page_offset_x = 20.0;
        let page_offset_y = 20.0;
        let page_gap = 40.0;

        // Track whether we've rendered count-in (only on first system of first section)
        let mut count_in_rendered = false;

        // Pre-compute section letters for consecutive repeats
        let section_letters = self.compute_section_letters(&chart.sections);

        // Track if we've added the title header (only on first page)
        let mut title_header_added = false;

        // Process each section
        for (section_idx, chart_section) in chart.sections.iter().enumerate() {
            // Skip count-in sections - we synthesize count-in from config.count_in_measures
            if chart_section.section.section_type.is_compact() {
                continue; // Don't render count-in as separate section
            }

            // Skip End sections entirely - they are only for progress bars, not charts
            if matches!(chart_section.section.section_type, SectionType::End) {
                continue;
            }

            // Reset chord tracking at section boundaries (rehearsal marks)
            // so the first chord of each section always shows
            previous_chord_symbol = None;

            // Preprocess melodies to handle cross-measure spillover
            let beats_per_measure = time_signature.0 as f64;
            let melody_data_map =
                expand_melodies_across_measures(chart_section.measures(), beats_per_measure);

            // Detect push spillbacks (chords from next measure that push back)
            let mut push_spillback_map = detect_push_spillbacks(chart_section.measures());

            // Check for cross-section spillback: if the NEXT section starts with a pushed chord,
            // it spills back to THIS section's last measure
            let next_non_compact_section = chart
                .sections
                .iter()
                .skip(section_idx + 1)
                .find(|s| {
                    !s.section.section_type.is_compact()
                        && !matches!(s.section.section_type, SectionType::End)
                });
            if let Some(next_section) = next_non_compact_section {
                if let Some(mut spillback) = detect_section_start_spillback(next_section.measures())
                {
                    // Adjust beat position based on this section's last measure time signature
                    if let Some(last_measure) = chart_section.measures().last() {
                        let ts = last_measure.time_signature;
                        spillback.beat_position = (ts.0 as usize).saturating_sub(1);
                    }
                    let last_measure_idx = chart_section.measures().len().saturating_sub(1);

                    #[cfg(debug_assertions)]
                    eprintln!(
                        "[cross-section-spillback] Section {} -> {} spillback: '{}' at measure {} beat {}",
                        chart_section.section.section_type.full_name(),
                        next_section.section.section_type.full_name(),
                        spillback.chord_symbol,
                        last_measure_idx,
                        spillback.beat_position
                    );

                    push_spillback_map
                        .entry(last_measure_idx)
                        .or_default()
                        .push(spillback);
                }
            }

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

                // Add page background if this is first system on page
                if current_page_systems.is_empty() {
                    self.add_page_background(
                        &mut root,
                        page_x,
                        page_offset_y,
                        page_width,
                        page_height,
                    );
                    self.add_page_footer(&mut root, page_x, page_offset_y, page_width, page_height, &chart.metadata);

                    // Add title header on first page only
                    if !title_header_added {
                        let header_height = self.add_title_header(
                            &mut root,
                            page_x,
                            page_offset_y,
                            page_width,
                            &chart.metadata,
                            chart.tempo.as_ref(),
                        );
                        page_y += header_height;
                        title_header_added = true;
                    }
                }

                // Calculate staff_y (after potential header adjustment)
                let staff_y = page_offset_y + page_y;

                // Calculate system prefix width (clef + key sig + time sig) FIRST
                // Needed to determine staff line width for short systems
                let include_clef = true; // Always show clef at system start
                let include_key_sig = true; // Key sig on every system (standard notation)
                let include_time_sig = global_system_index == 0; // Time sig only on first system

                // Get key signature from chart (number of sharps/flats)
                let key_signature: i8 = chart
                    .initial_key
                    .as_ref()
                    .map(prefix_renderer::key_to_fifths)
                    .unwrap_or(0);

                let (clef_width, key_sig_width, time_sig_width, prefix_width) =
                    prefix_renderer::calculate_prefix_width(
                        self.config.spatium,
                        include_clef,
                        include_key_sig,
                        key_signature,
                        include_time_sig,
                    );

                // Calculate measure width using spring-based distribution
                let measures_area_width = content_width - prefix_width;
                let num_measures = measure_indices.len();
                let max_measures = self.config.max_measures_per_system;

                // Add count-in measures on the first system of the first section only
                let num_count_in = if section_idx == 0 && sys_idx == 0 && !count_in_rendered {
                    count_in_rendered = true;
                    self.config.count_in_measures as usize
                } else {
                    0
                };
                let compact_scale = 0.4; // Count-in measures at 40% width

                // Calculate content weights for spring-based distribution
                // Also check for spillbacks from the next measure (triplet pushes that affect current measure width)
                let all_measures = chart_section.measures();
                // Calculate weights based purely on beat count for consistent measure widths
                // Spillback rendering is handled separately without affecting measure widths
                let measure_weights: Vec<f64> = measure_indices
                    .iter()
                    .filter_map(|&idx| all_measures.get(idx))
                    .map(|m| self.estimate_measure_content_weight(m, &text_metrics))
                    .collect();

                // Calculate base measure width based on mode
                let base_measure_width = if self.config.snippet_mode {
                    // Snippet mode: use ideal width based on content
                    // 90pt per weight unit gives comfortable, readable spacing
                    // (This is the "ideal" spacing; collision detection is the minimum floor)
                    let ideal_width_per_weight = 90.0;
                    let avg_weight = if measure_weights.is_empty() {
                        1.0
                    } else {
                        measure_weights.iter().sum::<f64>() / measure_weights.len() as f64
                    };
                    ideal_width_per_weight * avg_weight.max(1.0)
                } else {
                    // Normal mode: distribute across available space
                    measures_area_width / max_measures as f64
                };

                // For width calculation, count-in measures count as fractional
                let effective_measure_count =
                    num_count_in as f64 * compact_scale + num_measures as f64;
                // Snippet mode always treated as short system (no stretching)
                let is_short_system = self.config.snippet_mode || effective_measure_count < max_measures as f64;

                // Distribute width proportionally using spring physics
                // For full systems, distribute the entire measures_area_width
                // For short systems, distribute only the proportional amount
                let total_width_to_distribute = if self.config.snippet_mode {
                    // Snippet mode: use ideal widths based on content weight
                    // 90pt per weight unit provides comfortable spacing
                    // Collision detection enforces minimum spacing as needed
                    let weight_sum: f64 = measure_weights.iter().sum();
                    let ideal_width_per_weight = 90.0;
                    num_count_in as f64 * base_measure_width * compact_scale
                        + weight_sum * ideal_width_per_weight
                } else if is_short_system {
                    // Short system: only use width proportional to measure count
                    num_count_in as f64 * base_measure_width * compact_scale
                        + num_measures as f64 * base_measure_width
                } else {
                    measures_area_width
                };

                let distributed_widths = self.distribute_measure_widths(
                    &measure_weights,
                    num_count_in,
                    total_width_to_distribute,
                    compact_scale,
                    base_measure_width,
                );

                // Count-in measure width (compact) - from distribution
                let count_in_width = if num_count_in > 0 {
                    distributed_widths.first().copied().unwrap_or(base_measure_width * compact_scale)
                } else {
                    base_measure_width * compact_scale
                };

                // Calculate actual system width
                let actual_system_width = if is_short_system {
                    prefix_width + total_width_to_distribute
                } else {
                    content_width
                };

                // Draw staff lines (shortened for short systems)
                root.add_child(SceneNode::anonymous_leaf(self.draw_staff_lines(
                    content_x,
                    staff_y,
                    actual_system_width,
                )));

                let chord_y = staff_y - 8.0; // Above staff

                // Add section label for first system of section (skip for count-in)
                if sys_idx == 0 && chart_section.section.section_type.should_show_header() {
                    let letter = section_letters.get(&section_idx).copied();
                    let mut section_label = self.create_section_label(
                        &chart_section.section,
                        page_x,
                        self.config.margins.left,
                        staff_y,
                        staff_height,
                        letter,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 1;
                    section_label
                        .metadata
                        .insert("page".to_string(), page_number.to_string());
                    root.add_child(section_label);
                }

                // Render system prefix (clef, key signature, and time signature)
                let ts = chart
                    .time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                let prefix_ctx = prefix_renderer::PrefixRenderContext {
                    x: content_x,
                    staff_y,
                    spatium: self.config.spatium,
                    include_clef,
                    include_key_sig,
                    include_time_sig,
                    key_signature,
                    time_signature: ts,
                    clef_width,
                    key_sig_width,
                    time_sig_width,
                    page_number: Some(page_number),
                };

                let prefix_result = prefix_renderer::render_system_prefix(&prefix_ctx, id_counter, &ctx);

                for node in prefix_result.nodes {
                    root.add_child(node);
                }
                id_counter = prefix_result.next_id;

                // Start measures after prefix
                let mut measure_x = content_x + prefix_width;
                let time_signature = chart
                    .time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                // Render count-in measures with quarter note slashes
                for count_in_idx in 0..num_count_in {
                    // Create a synthetic empty measure for count-in (compact mode for minimal margins)
                    let count_in_measure = Measure::new().with_time_signature(time_signature);
                    let count_in_result = self.layout_measure_compact(
                        &count_in_measure,
                        None, // no melody
                        None, // no spillbacks for count-in
                        count_in_width,
                        false, // clef rendered separately
                        false, // time sig rendered separately
                        time_signature,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 10;

                    let mut count_in_container =
                        SceneNode::group(SemanticId::new(ElementType::Measure, id_counter));
                    id_counter += 1;
                    count_in_container.transform =
                        Affine::translate((measure_x, staff_y + 2.0 * self.config.spatium));
                    count_in_container
                        .metadata
                        .insert("page".to_string(), page_number.to_string());
                    count_in_container
                        .metadata
                        .insert("count_in".to_string(), "true".to_string());
                    count_in_container.add_child(count_in_result.scene);
                    root.add_child(count_in_container);

                    // Only show measure number for the first count-in measure
                    // Position above the clef (at content_x) rather than the measure start
                    let count_in_display_num = (1 - num_count_in as i32) + count_in_idx as i32;
                    if self.config.show_measure_numbers && count_in_idx == 0 {
                        let measure_num_node = self.create_measure_number(
                            count_in_display_num,
                            content_x, // Position above clef at start of line
                            staff_y,
                            id_counter,
                        );
                        id_counter += 1;
                        root.add_child(measure_num_node);
                    }

                    // Add beat positions and count text for count-in measures
                    let num_beats = time_signature.0 as usize;
                    let count_in_measure_time_start = cumulative_time;
                    let count_in_measure_tick_start = cumulative_ticks;

                    for (beat_idx, segment) in count_in_result
                        .segments
                        .iter_type(SegmentType::CHORD_REST)
                        .enumerate()
                    {
                        let time_start =
                            count_in_measure_time_start + (segment.tick as f64 * seconds_per_tick);
                        let time_end = count_in_measure_time_start
                            + ((segment.tick + segment.ticks) as f64 * seconds_per_tick);
                        let absolute_tick = count_in_measure_tick_start + segment.tick as i64;

                        beat_positions.push(BeatPosition {
                            page: page_number,
                            system: global_system_index,
                            measure: global_measure_index,
                            beat: beat_idx,
                            tick: segment.tick,
                            duration_ticks: segment.ticks,
                            absolute_tick,
                            x: measure_x + segment.x,
                            width: segment.width,
                            staff_y,
                            staff_height,
                            time_start,
                            time_end,
                            glyph_codepoint: Some(slash_glyph_for_ticks(segment.ticks)),
                            glyph_size: self.config.spatium,
                            glyph_y: staff_y + staff_height / 2.0, // Center on staff
                            has_stem: false, // Count-in uses stemless slashes
                            stem_up: true,
                            flag_count: 0,
                        });

                        // Render count text below slashes
                        if let Some(count_text) =
                            self.get_count_in_text(count_in_display_num, beat_idx, num_beats)
                        {
                            let count_node = self.create_count_text(
                                &count_text,
                                measure_x + segment.x,
                                staff_y,
                                staff_height,
                                id_counter,
                            );
                            id_counter += 1;
                            root.add_child(count_node);
                        }
                    }

                    // Update cumulative time/ticks for count-in measures
                    let measure_duration_ticks =
                        time_signature.0 as i32 * (1920 / time_signature.1 as i32);
                    cumulative_time += measure_duration_ticks as f64 * seconds_per_tick;
                    cumulative_ticks += measure_duration_ticks as i64;
                    global_measure_index += 1;

                    measure_x += count_in_width;

                    // Barline after count-in measure
                    let mut barline =
                        self.draw_barline(measure_x, staff_y, staff_height, BarlineType::Single);
                    barline
                        .metadata
                        .insert("page".to_string(), page_number.to_string());
                    root.add_child(barline);
                }

                for (local_measure_idx, &measure_idx) in measure_indices.iter().enumerate() {
                    if let Some(measure) = chart_section.measures().get(measure_idx) {
                        // Get preprocessed melody data for this measure (handles spillover)
                        let melody_data = melody_data_map.get(&measure_idx);

                        // Get distributed width for this measure (skip count-in widths)
                        let this_measure_width = distributed_widths
                            .get(num_count_in + local_measure_idx)
                            .copied()
                            .unwrap_or(base_measure_width);

                        // Get spillback chords for this measure (from next measure pushing back)
                        let spillbacks = push_spillback_map.get(&measure_idx).map(|v| v.as_slice());

                        // Layout measure content (rhythm slashes) - no clef/time sig inside measure
                        let measure_result = self.layout_measure(
                            measure,
                            melody_data,
                            spillbacks,
                            this_measure_width,
                            false, // clef rendered separately
                            false, // time sig rendered separately
                            time_signature,
                            &ctx,
                            id_counter,
                        );
                        id_counter += 10;

                        // Get ChordRest segment x-positions (already in points after spacing)
                        let segment_positions: Vec<f64> =
                            self.get_chord_rest_positions(&measure_result);

                        let mut measure_container =
                            SceneNode::group(SemanticId::new(ElementType::Measure, id_counter));
                        id_counter += 1;
                        measure_container.transform =
                            Affine::translate((measure_x, staff_y + 2.0 * self.config.spatium));
                        measure_container
                            .metadata
                            .insert("page".to_string(), page_number.to_string());
                        measure_container.add_child(measure_result.scene);
                        root.add_child(measure_container);

                        // Add measure number on first measure of each system
                        // Real measures start at 1, after count-in measures
                        // Position above the clef (at content_x) rather than the measure start
                        if self.config.show_measure_numbers && local_measure_idx == 0 {
                            let display_measure_num = (global_measure_index as i32)
                                - self.config.count_in_measures as i32
                                + 1;
                            let measure_num_node = self.create_measure_number(
                                display_measure_num,
                                content_x, // Position above clef at start of line
                                staff_y,
                                id_counter,
                            );
                            id_counter += 1;
                            root.add_child(measure_num_node);
                        }

                        // Collect beat positions from actual segment data
                        let measure_time_start = cumulative_time;
                        let measure_tick_start = cumulative_ticks;

                        for (beat_idx, segment) in measure_result
                            .segments
                            .iter_type(SegmentType::CHORD_REST)
                            .enumerate()
                        {
                            let time_start =
                                measure_time_start + (segment.tick as f64 * seconds_per_tick);
                            let time_end = measure_time_start
                                + ((segment.tick + segment.ticks) as f64 * seconds_per_tick);
                            let absolute_tick = measure_tick_start + segment.tick as i64;

                            // Compute stem/flag info based on duration
                            // Whole (1920) and half (960) are stemless in slash notation
                            // Quarter (480) and shorter have stems
                            let has_stem = segment.ticks < 960;
                            let flag_count = match segment.ticks {
                                t if t >= 480 => 0, // Quarter or longer: no flags
                                t if t >= 240 => 1, // Eighth: 1 flag
                                t if t >= 120 => 2, // 16th: 2 flags
                                t if t >= 60 => 3,  // 32nd: 3 flags
                                _ => 4,             // 64th: 4 flags
                            };

                            beat_positions.push(BeatPosition {
                                page: page_number,
                                system: global_system_index,
                                measure: global_measure_index,
                                beat: beat_idx,
                                tick: segment.tick,
                                duration_ticks: segment.ticks,
                                absolute_tick,
                                x: measure_x + segment.x,
                                width: segment.width,
                                staff_y,
                                staff_height,
                                time_start,
                                time_end,
                                glyph_codepoint: Some(slash_glyph_for_ticks(segment.ticks)),
                                glyph_size: self.config.spatium,
                                glyph_y: staff_y + staff_height / 2.0, // Center on staff
                                has_stem,
                                stem_up: true, // Default to stem up
                                flag_count,
                            });
                        }

                        // Calculate measure duration in ticks and update cumulative time/ticks
                        let measure_duration_ticks =
                            time_signature.0 as i32 * (1920 / time_signature.1 as i32);
                        cumulative_time += measure_duration_ticks as f64 * seconds_per_tick;
                        cumulative_ticks += measure_duration_ticks as i64;
                        global_measure_index += 1;

                        // Render chord symbols using chord_renderer module
                        let chord_ctx = chord_renderer::ChordRenderContext {
                            measure_x,
                            measure_width: this_measure_width,
                            chord_y,
                            page_number: Some(page_number),
                            global_system_index,
                            measure_idx,
                            local_measure_idx,
                            section_name: &chart_section.section.section_type.full_name(),
                            segment_positions: &segment_positions,
                            internal_push_positions: &measure_result.internal_push_positions,
                            harmony_style: &harmony_style,
                            time_signature,
                            hide_repeated_chords: self.config.hide_repeated_chords,
                            min_chord_symbol_gap: self.config.min_chord_symbol_gap,
                        };

                        let chord_result = chord_renderer::render_chord_symbols(
                            &chord_ctx,
                            measure,
                            previous_chord_symbol.as_deref(),
                            id_counter,
                            &ctx,
                        );

                        for node in chord_result.nodes {
                            root.add_child(node);
                        }
                        previous_chord_symbol = chord_result.last_chord_symbol;
                        id_counter = chord_result.next_id;

                        // Render spillback chord symbols (from next measure pushing back)
                        if let Some(spillback_list) = spillbacks {
                            let spillback_result = chord_renderer::render_spillback_chords(
                                &chord_ctx,
                                spillback_list,
                                previous_chord_symbol.as_deref(),
                                id_counter,
                                &ctx,
                            );

                            for node in spillback_result.nodes {
                                root.add_child(node);
                            }
                            previous_chord_symbol = spillback_result.last_chord_symbol;
                            id_counter = spillback_result.next_id;
                        }

                        measure_x += this_measure_width;

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

        // Post-process beat positions to make them contiguous (no gaps at barlines).
        // Each beat's width is extended to reach the next beat's x position,
        // ensuring smooth cursor movement across measure boundaries.
        let mut beat_positions = beat_positions;
        for i in 0..beat_positions.len().saturating_sub(1) {
            let next_x = beat_positions[i + 1].x;
            let current_x = beat_positions[i].x;
            // Only extend if next beat is on same system (same y position)
            // and to the right (not a system break)
            if beat_positions[i + 1].staff_y == beat_positions[i].staff_y && next_x > current_x {
                beat_positions[i].width = next_x - current_x;
            }
        }

        ChartLayoutResult {
            scene: root,
            pages,
            total_height,
            total_width,
            beat_positions,
        }
    }

    /// Layout chart in continuous scroll mode.
    fn layout_continuous(&self, chart: &Chart, width: f64) -> ChartLayoutResult {
        let ctx = LayoutContext::minimal(self.style);
        let text_metrics = TextFontMetrics::new(self.text_font_data.clone());
        let symbol_metrics = TextFontMetrics::new(self.symbol_font_data.clone());

        let harmony_style = self
            .config
            .harmony_style
            .clone()
            .with_text_font_metrics(text_metrics.clone())
            .with_symbol_font_metrics(symbol_metrics.clone());

        let content_width = width - self.config.margins.left - self.config.margins.right;
        let content_x = self.config.margins.left;
        let staff_height = self.config.spatium * 4.0;

        let mut root = SceneNode::group(SemanticId::new(ElementType::Page, 0));
        let mut total_height = self.config.margins.top;
        let mut id_counter = 100u64;
        let mut global_system_index = 0usize;
        let mut global_measure_index = 0usize;

        // Track previous chord to hide duplicates
        let mut previous_chord_symbol: Option<String> = None;

        // Track whether we've rendered count-in (only on first system of first section)
        let mut count_in_rendered = false;

        // Pre-compute section letters for consecutive repeats
        let section_letters = self.compute_section_letters(&chart.sections);

        // Get time signature for beat calculations
        let time_signature = chart
            .time_signature
            .map(|ts| (ts.numerator as u8, ts.denominator as u8))
            .unwrap_or((4u8, 4u8));

        // Process each section
        for (section_idx, chart_section) in chart.sections.iter().enumerate() {
            // Skip count-in sections - we synthesize count-in from config.count_in_measures
            if chart_section.section.section_type.is_compact() {
                continue; // Don't render count-in as separate section
            }

            // Skip End sections entirely - they are only for progress bars, not charts
            if matches!(chart_section.section.section_type, SectionType::End) {
                continue;
            }

            // Reset chord tracking at section boundaries (rehearsal marks)
            // so the first chord of each section always shows
            previous_chord_symbol = None;

            // Preprocess melodies to handle cross-measure spillover
            let beats_per_measure = time_signature.0 as f64;
            let melody_data_map =
                expand_melodies_across_measures(chart_section.measures(), beats_per_measure);

            // Detect push spillbacks (chords from next measure that push back)
            let mut push_spillback_map = detect_push_spillbacks(chart_section.measures());

            // Check for cross-section spillback: if the NEXT section starts with a pushed chord,
            // it spills back to THIS section's last measure
            let next_non_compact_section = chart
                .sections
                .iter()
                .skip(section_idx + 1)
                .find(|s| {
                    !s.section.section_type.is_compact()
                        && !matches!(s.section.section_type, SectionType::End)
                });
            if let Some(next_section) = next_non_compact_section {
                if let Some(mut spillback) = detect_section_start_spillback(next_section.measures())
                {
                    // Adjust beat position based on this section's last measure time signature
                    if let Some(last_measure) = chart_section.measures().last() {
                        let ts = last_measure.time_signature;
                        spillback.beat_position = (ts.0 as usize).saturating_sub(1);
                    }
                    let last_measure_idx = chart_section.measures().len().saturating_sub(1);
                    push_spillback_map
                        .entry(last_measure_idx)
                        .or_default()
                        .push(spillback);
                }
            }

            // Group measures into systems
            let systems = self.group_measures_into_systems(chart_section.measures(), content_width);

            for (sys_idx, measure_indices) in systems.iter().enumerate() {
                let staff_y = total_height;
                let system_height = staff_height + 30.0;

                // Calculate system prefix width (clef + key sig + time sig) FIRST
                // Needed to determine staff line width for short systems
                let include_clef = true;
                let include_key_sig = true; // Key sig on every system (standard notation)
                let include_time_sig = global_system_index == 0;

                // Get key signature from chart (number of sharps/flats)
                let key_signature: i8 = chart
                    .initial_key
                    .as_ref()
                    .map(prefix_renderer::key_to_fifths)
                    .unwrap_or(0);

                let (clef_width, key_sig_width, time_sig_width, prefix_width) =
                    prefix_renderer::calculate_prefix_width(
                        self.config.spatium,
                        include_clef,
                        include_key_sig,
                        key_signature,
                        include_time_sig,
                    );

                // Calculate measure width using spring-based distribution
                let measures_area_width = content_width - prefix_width;
                let num_measures = measure_indices.len();
                let max_measures = self.config.max_measures_per_system;

                // Add count-in measures on the first system of the first section only
                let num_count_in = if section_idx == 0 && sys_idx == 0 && !count_in_rendered {
                    count_in_rendered = true;
                    self.config.count_in_measures as usize
                } else {
                    0
                };
                let compact_scale = 0.4; // Count-in measures at 40% width

                // Calculate base measure width (what a normal measure would be)
                let base_measure_width = measures_area_width / max_measures as f64;

                // For width calculation, count-in measures count as fractional
                let effective_measure_count =
                    num_count_in as f64 * compact_scale + num_measures as f64;
                let is_short_system = effective_measure_count < max_measures as f64;

                // Calculate content weights for spring-based distribution
                // Weights are purely beat-based for consistent measure widths
                // Spillback rendering is handled separately without affecting measure widths
                let all_measures = chart_section.measures();
                let measure_weights: Vec<f64> = measure_indices
                    .iter()
                    .filter_map(|&idx| all_measures.get(idx))
                    .map(|m| self.estimate_measure_content_weight(m, &text_metrics))
                    .collect();

                // Distribute width proportionally using spring physics
                let total_width_to_distribute = if is_short_system {
                    num_count_in as f64 * base_measure_width * compact_scale
                        + num_measures as f64 * base_measure_width
                } else {
                    measures_area_width
                };

                let distributed_widths = self.distribute_measure_widths(
                    &measure_weights,
                    num_count_in,
                    total_width_to_distribute,
                    compact_scale,
                    base_measure_width,
                );

                // Count-in measure width (compact) - from distribution
                let count_in_width = if num_count_in > 0 {
                    distributed_widths.first().copied().unwrap_or(base_measure_width * compact_scale)
                } else {
                    base_measure_width * compact_scale
                };

                // Calculate actual system width
                let actual_system_width = if is_short_system {
                    prefix_width + total_width_to_distribute
                } else {
                    content_width
                };

                // Draw staff lines (shortened for short systems)
                root.add_child(SceneNode::anonymous_leaf(self.draw_staff_lines(
                    content_x,
                    staff_y,
                    actual_system_width,
                )));

                let chord_y = staff_y - 8.0;

                // Add section label for first system of section (skip for count-in)
                if sys_idx == 0 && chart_section.section.section_type.should_show_header() {
                    let letter = section_letters.get(&section_idx).copied();
                    let section_label = self.create_section_label(
                        &chart_section.section,
                        0.0,
                        self.config.margins.left,
                        staff_y,
                        staff_height,
                        letter,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 1;
                    root.add_child(section_label);
                }

                // Render system prefix (clef, key signature, and time signature)
                let ts = chart
                    .time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                let prefix_ctx = prefix_renderer::PrefixRenderContext {
                    x: content_x,
                    staff_y,
                    spatium: self.config.spatium,
                    include_clef,
                    include_key_sig,
                    include_time_sig,
                    key_signature,
                    time_signature: ts,
                    clef_width,
                    key_sig_width,
                    time_sig_width,
                    page_number: None, // Continuous mode has no pages
                };

                let prefix_result = prefix_renderer::render_system_prefix(&prefix_ctx, id_counter, &ctx);

                for node in prefix_result.nodes {
                    root.add_child(node);
                }
                id_counter = prefix_result.next_id;

                // Start measures after prefix
                let mut measure_x = content_x + prefix_width;
                let time_signature = chart
                    .time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4u8, 4u8));

                // Render count-in measures with quarter note slashes (continuous layout)
                for count_in_idx in 0..num_count_in {
                    // Create a synthetic empty measure for count-in (compact mode for minimal margins)
                    let count_in_measure = Measure::new().with_time_signature(time_signature);
                    let count_in_result = self.layout_measure_compact(
                        &count_in_measure,
                        None, // no melody
                        None, // no spillbacks for count-in
                        count_in_width,
                        false, // clef rendered separately
                        false, // time sig rendered separately
                        time_signature,
                        &ctx,
                        id_counter,
                    );
                    id_counter += 10;

                    let mut count_in_container =
                        SceneNode::group(SemanticId::new(ElementType::Measure, id_counter));
                    id_counter += 1;
                    count_in_container.transform =
                        Affine::translate((measure_x, staff_y + 2.0 * self.config.spatium));
                    count_in_container
                        .metadata
                        .insert("count_in".to_string(), "true".to_string());
                    count_in_container.add_child(count_in_result.scene);
                    root.add_child(count_in_container);

                    // Only show measure number for the first count-in measure
                    // Position above the clef (at content_x) rather than the measure start
                    let count_in_display_num = (1 - num_count_in as i32) + count_in_idx as i32;
                    if self.config.show_measure_numbers && count_in_idx == 0 {
                        let measure_num_node = self.create_measure_number(
                            count_in_display_num,
                            content_x, // Position above clef at start of line
                            staff_y,
                            id_counter,
                        );
                        id_counter += 1;
                        root.add_child(measure_num_node);
                    }

                    // Add count text for count-in measures
                    let num_beats = time_signature.0 as usize;
                    for (beat_idx, segment) in count_in_result
                        .segments
                        .iter_type(SegmentType::CHORD_REST)
                        .enumerate()
                    {
                        if let Some(count_text) =
                            self.get_count_in_text(count_in_display_num, beat_idx, num_beats)
                        {
                            let count_node = self.create_count_text(
                                &count_text,
                                measure_x + segment.x,
                                staff_y,
                                staff_height,
                                id_counter,
                            );
                            id_counter += 1;
                            root.add_child(count_node);
                        }
                    }

                    global_measure_index += 1;
                    measure_x += count_in_width;

                    // Barline after count-in measure
                    let mut barline =
                        self.draw_barline(measure_x, staff_y, staff_height, BarlineType::Single);
                    root.add_child(barline);
                }

                for (local_measure_idx, &measure_idx) in measure_indices.iter().enumerate() {
                    if let Some(measure) = chart_section.measures().get(measure_idx) {
                        let melody_data = melody_data_map.get(&measure_idx);

                        // Get distributed width for this measure (skip count-in widths)
                        let this_measure_width = distributed_widths
                            .get(num_count_in + local_measure_idx)
                            .copied()
                            .unwrap_or(base_measure_width);

                        // Get spillback chords for this measure (from next measure pushing back)
                        let spillbacks = push_spillback_map.get(&measure_idx).map(|v| v.as_slice());

                        let measure_result = self.layout_measure(
                            measure,
                            melody_data,
                            spillbacks,
                            this_measure_width,
                            false,
                            false,
                            time_signature,
                            &ctx,
                            id_counter,
                        );
                        id_counter += 10;

                        let segment_positions: Vec<f64> =
                            self.get_chord_rest_positions(&measure_result);

                        let mut measure_container =
                            SceneNode::group(SemanticId::new(ElementType::Measure, id_counter));
                        id_counter += 1;
                        measure_container.transform =
                            Affine::translate((measure_x, staff_y + 2.0 * self.config.spatium));
                        measure_container.add_child(measure_result.scene);
                        root.add_child(measure_container);

                        // Add measure number on first measure of each system (real measures start at 1)
                        // Position above the clef (at content_x) rather than the measure start
                        if self.config.show_measure_numbers && local_measure_idx == 0 {
                            let display_measure_num = (global_measure_index as i32)
                                - self.config.count_in_measures as i32
                                + 1;
                            let measure_num_node = self.create_measure_number(
                                display_measure_num,
                                content_x, // Position above clef at start of line
                                staff_y,
                                id_counter,
                            );
                            id_counter += 1;
                            root.add_child(measure_num_node);
                        }

                        // Render chord symbols using chord_renderer module
                        let chord_ctx = chord_renderer::ChordRenderContext {
                            measure_x,
                            measure_width: this_measure_width,
                            chord_y,
                            page_number: None, // Continuous mode has no pages
                            global_system_index,
                            measure_idx,
                            local_measure_idx,
                            section_name: &chart_section.section.section_type.full_name(),
                            segment_positions: &segment_positions,
                            internal_push_positions: &measure_result.internal_push_positions,
                            harmony_style: &harmony_style,
                            time_signature,
                            hide_repeated_chords: self.config.hide_repeated_chords,
                            min_chord_symbol_gap: self.config.min_chord_symbol_gap,
                        };

                        let chord_result = chord_renderer::render_chord_symbols(
                            &chord_ctx,
                            measure,
                            previous_chord_symbol.as_deref(),
                            id_counter,
                            &ctx,
                        );

                        for node in chord_result.nodes {
                            root.add_child(node);
                        }
                        previous_chord_symbol = chord_result.last_chord_symbol;
                        id_counter = chord_result.next_id;

                        // Render spillback chord symbols (from next measure pushing back)
                        if let Some(spillback_list) = spillbacks {
                            let spillback_result = chord_renderer::render_spillback_chords(
                                &chord_ctx,
                                spillback_list,
                                previous_chord_symbol.as_deref(),
                                id_counter,
                                &ctx,
                            );

                            for node in spillback_result.nodes {
                                root.add_child(node);
                            }
                            previous_chord_symbol = spillback_result.last_chord_symbol;
                            id_counter = spillback_result.next_id;
                        }

                        measure_x += this_measure_width;

                        root.add_child(self.draw_barline(
                            measure_x,
                            staff_y,
                            staff_height,
                            BarlineType::Single,
                        ));

                        global_measure_index += 1;
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
            beat_positions: Vec::new(), // TODO: collect beat positions for continuous mode
        }
    }

    /// Group measures into systems based on available width.
    ///
    /// Delegates to [`measure_layout::group_measures_into_systems`].
    fn group_measures_into_systems(
        &self,
        measures: &[crate::chart::types::Measure],
        _content_width: f64,
    ) -> Vec<Vec<usize>> {
        measure_layout::group_measures_into_systems(measures.len(), self.config.max_measures_per_system)
    }

    /// Convert a Keyflow ChordInstance to engraver HarmonyParams.
    ///
    /// Delegates to [`chord_layout::chord_to_harmony_params`].
    fn chord_to_harmony_params(
        &self,
        chord: &crate::chart::types::ChordInstance,
        harmony_style: &HarmonyStyle,
    ) -> HarmonyParams {
        chord_layout::chord_to_harmony_params(chord, harmony_style)
    }

    /// Create a section label scene node.
    fn create_section_label(
        &self,
        section: &crate::sections::Section,
        page_x: f64,
        margin_width: f64,
        staff_y: f64,
        staff_height: f64,
        letter: Option<char>,
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
                letter,
                comment: section.comment.clone(),
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

    /// Create a measure number scene node.
    ///
    /// Renders the measure number above the staff, positioned at the start of the measure.
    /// Based on MuseScore's measure number placement: left-aligned to barline, 2 spatiums above staff.
    fn create_measure_number(
        &self,
        measure_number: i32,
        measure_x: f64,
        staff_y: f64,
        id: u64,
    ) -> SceneNode {
        let spatium = self.config.spatium;

        // MuseScore default: measure number positioned 2 spatiums above staff
        let y_offset = -2.0 * spatium;

        // Small offset from barline (0.25 spatiums to the right)
        let x_offset = 0.25 * spatium;

        // Create text command for measure number
        let text = measure_number.to_string();
        let font_size = spatium * 1.6; // Slightly smaller than chord symbols

        let text_command = PaintCommand::text(
            text,
            "FreeSans", // Use the same font as other text
            font_size,
            kurbo::Point::new(measure_x + x_offset, staff_y + y_offset),
            Color::from_rgb8(100, 100, 100), // Gray color for measure numbers (less prominent)
        );

        let mut node = SceneNode::leaf(
            SemanticId::new(ElementType::MeasureNumber, id),
            vec![text_command],
        );
        node.metadata
            .insert("measure_number".to_string(), measure_number.to_string());
        node
    }

    /// Create count-in text below the staff for a beat position.
    ///
    /// For count-in measures, renders the count number below each slash notehead.
    /// This allows musicians to follow along during count-in and enables cursor
    /// tracking through the count-in beats.
    fn create_count_text(
        &self,
        count_text: &str,
        x: f64,
        staff_y: f64,
        staff_height: f64,
        id: u64,
    ) -> SceneNode {
        let spatium = self.config.spatium;

        // Position below the staff (lyrics position: 2 spatiums below bottom of staff)
        let y_offset = staff_height + 2.0 * spatium;

        // Create text command for count number
        let font_size = spatium * 2.0; // Larger for visibility

        let text_command = PaintCommand::text(
            count_text.to_string(),
            "FreeSans",
            font_size,
            kurbo::Point::new(x, staff_y + y_offset),
            Color::from_rgb8(80, 80, 80), // Slightly darker gray for count text
        );

        let mut node = SceneNode::leaf(
            SemanticId::new(ElementType::Lyrics, id), // Use Lyrics element type for count text
            vec![text_command],
        );
        node.metadata
            .insert("count_text".to_string(), count_text.to_string());
        node
    }

    /// Get the count-in text for a beat in a count-in measure.
    ///
    /// Count-in measures are determined by the config.count_in_measures value,
    /// which comes from the distance between Count-In marker and SONGSTART marker.
    /// - 1 measure count-in: measure 0 gets full count (1,2,3,4)
    /// - 2 measure count-in: measure -1 gets half-time (1,2), measure 0 gets full (1,2,3,4)
    fn get_count_in_text(
        &self,
        display_measure_num: i32,
        beat_idx: usize,
        _num_beats: usize,
    ) -> Option<String> {
        let count_in = self.config.count_in_measures;

        if count_in == 0 {
            return None; // No count-in configured
        }

        // Count-in measures are the N measures immediately before measure 1
        // So for 2-measure count-in: -1 and 0
        // For 1-measure count-in: just 0
        let first_count_in_measure = 1 - count_in as i32; // e.g., -1 for 2 measures, 0 for 1 measure

        if display_measure_num < first_count_in_measure || display_measure_num > 0 {
            return None; // Not a count-in measure
        }

        // Determine which count-in measure this is (0 = first, 1 = second)
        let count_in_index = (display_measure_num - first_count_in_measure) as usize;

        if count_in == 2 && count_in_index == 0 {
            // First of two count-in measures: half-time feel (1 on beat 1, 2 on beat 3)
            match beat_idx {
                0 => Some("1".to_string()),
                2 => Some("2".to_string()),
                _ => None, // No text on beats 2 and 4
            }
        } else {
            // Last count-in measure (or only count-in): full count (1, 2, 3, 4)
            Some((beat_idx + 1).to_string())
        }
    }

    /// Compute section letters for consecutive repeats of the same section type.
    ///
    /// Delegates to [`section_layout::compute_section_letters`].
    fn compute_section_letters(
        &self,
        sections: &[crate::ChartSection],
    ) -> std::collections::HashMap<usize, char> {
        section_layout::compute_section_letters(sections)
    }

    /// Get a string key for section type comparison (ignoring section number).
    ///
    /// Delegates to `SectionType::key()` from keyflow.
    fn section_type_key(&self, section_type: &SectionType) -> String {
        section_type.key()
    }

    /// Convert SectionType to display strings.
    ///
    /// Delegates to `SectionType::full_name()` and `SectionType::abbreviation()` from keyflow.
    fn section_type_to_strings(&self, section_type: &SectionType) -> (String, String) {
        (section_type.full_name(), section_type.abbreviation())
    }

    /// Get theme for section type.
    ///
    /// Delegates to [`section_layout::get_section_theme`].
    fn get_section_theme(
        &self,
        section_type: &SectionType,
    ) -> crate::engraver::layout::tlayout::rehearsal_mark::RehearsalMarkStyle {
        section_layout::get_section_theme(section_type)
    }

    /// Draw staff lines.
    ///
    /// Delegates to [`page_rendering::draw_staff_lines`].
    fn draw_staff_lines(&self, x: f64, y: f64, width: f64) -> Vec<PaintCommand> {
        page_rendering::draw_staff_lines(x, y, width, self.config.spatium)
    }

    /// Draw a barline.
    ///
    /// Delegates to [`page_rendering::draw_barline`].
    fn draw_barline(&self, x: f64, y: f64, height: f64, barline_type: BarlineType) -> SceneNode {
        page_rendering::draw_barline(x, y, height, barline_type)
    }

    /// Add page background (paper with shadow).
    ///
    /// Delegates to [`page_rendering::add_page_background`].
    fn add_page_background(
        &self,
        root: &mut SceneNode,
        page_x: f64,
        page_y: f64,
        page_width: f64,
        page_height: f64,
    ) {
        if self.config.snippet_mode {
            // Simple white background for snippets (no shadow)
            page_rendering::add_snippet_background(root, page_x, page_y, page_width, page_height);
        } else {
            // Full page background with shadow
            page_rendering::add_page_background(root, page_x, page_y, page_width, page_height);
        }
    }

    /// Add page footer with "Created with FastTrackStudio" text.
    ///
    /// Only renders footer when the chart has a title. Titleless charts (snippets)
    /// don't need the footer branding.
    ///
    /// Delegates to [`page_rendering::add_page_footer`].
    fn add_page_footer(
        &self,
        root: &mut SceneNode,
        page_x: f64,
        page_y: f64,
        page_width: f64,
        page_height: f64,
        metadata: &crate::SongMetadata,
    ) {
        // Skip footer for titleless charts (snippets)
        if metadata.title.is_none() {
            return;
        }
        page_rendering::add_page_footer(root, page_x, page_y, page_width, page_height);
    }

    /// Add title header section on first page (Title, Subtitle, Composer, etc.)
    ///
    /// Returns the height consumed by the header for layout adjustment.
    /// Delegates to [`page_rendering::add_title_header`].
    fn add_title_header(
        &self,
        root: &mut SceneNode,
        page_x: f64,
        page_y: f64,
        page_width: f64,
        metadata: &crate::SongMetadata,
        tempo: Option<&crate::time::Tempo>,
    ) -> f64 {
        page_rendering::add_title_header(
            root,
            page_x,
            page_y,
            page_width,
            &self.config.margins,
            self.config.spatium,
            metadata,
            tempo,
        )
    }

    /// Layout a single measure, returning the full MeasureScene with segment positions.
    ///
    /// If `melody_data` is provided, it contains preprocessed melody segments that account
    /// for spillover across measure boundaries. Otherwise, falls back to the measure's
    /// raw melody data (legacy behavior, doesn't handle cross-measure melodies).
    ///
    /// If `spillbacks` is provided, it contains chords from the next measure that push
    /// back across the barline and need to be rendered in this measure.
    fn layout_measure(
        &self,
        measure: &crate::chart::types::Measure,
        melody_data: Option<&MeasureMelodyData>,
        spillbacks: Option<&[PushSpillback]>,
        measure_width: f64,
        include_clef: bool,
        include_time_sig: bool,
        time_signature: (u8, u8),
        ctx: &LayoutContext<'_>,
        id_base: u64,
    ) -> MeasureScene {
        self.layout_measure_inner(
            measure,
            melody_data,
            spillbacks,
            measure_width,
            include_clef,
            include_time_sig,
            time_signature,
            ctx,
            id_base,
            false,
        )
    }

    /// Layout a measure with compact mode (minimal left margin).
    fn layout_measure_compact(
        &self,
        measure: &crate::chart::types::Measure,
        melody_data: Option<&MeasureMelodyData>,
        spillbacks: Option<&[PushSpillback]>,
        measure_width: f64,
        include_clef: bool,
        include_time_sig: bool,
        time_signature: (u8, u8),
        ctx: &LayoutContext<'_>,
        id_base: u64,
    ) -> MeasureScene {
        self.layout_measure_inner(
            measure,
            melody_data,
            spillbacks,
            measure_width,
            include_clef,
            include_time_sig,
            time_signature,
            ctx,
            id_base,
            true,
        )
    }

    fn layout_measure_inner(
        &self,
        measure: &crate::chart::types::Measure,
        melody_data: Option<&MeasureMelodyData>,
        spillbacks: Option<&[PushSpillback]>,
        measure_width: f64,
        include_clef: bool,
        include_time_sig: bool,
        time_signature: (u8, u8),
        ctx: &LayoutContext<'_>,
        id_base: u64,
        compact: bool,
    ) -> MeasureScene {
        // Check if this measure has melodies (from preprocessed data or raw measure)
        let has_melodies =
            melody_data.map_or_else(|| !measure.melodies.is_empty(), |data| data.has_content());

        // Calculate measure duration in ticks based on time signature
        // Quarter note = 480 ticks, so beat duration depends on denominator
        let beat_ticks = match time_signature.1 {
            2 => 960, // Half note beat
            4 => 480, // Quarter note beat
            8 => 240, // Eighth note beat
            _ => 480, // Default to quarter
        };
        let measure_ticks = beat_ticks * time_signature.0 as i32;
        let beats_per_measure = time_signature.0 as f64;

        // Check if the measure has explicit chord rhythms (Lily, Rest, Space notation)
        // These take precedence over melodies and slash fills
        let has_explicit_chord_rhythm = self.measure_has_explicit_chord_rhythm(measure);

        // When we have explicit chord rhythms, use entries directly (notes and rests)
        // Otherwise, use the existing rhythm-based approach
        let (rhythm_entries, full_rhythm, rhythm_ticks, tuplet_specs, head_type_overrides, internal_push_positions) =
            if has_explicit_chord_rhythm {
                // Build rhythm from explicit chord rhythms (r8t, Ab9_8t, etc.)
                let (mut entries, ticks, specs) = self.build_rhythm_from_chord_rhythms(measure);

                // Fill remaining space with quarter note slashes to match time signature
                // This ensures measures with incomplete explicit rhythms still fill properly
                let remaining_ticks = measure_ticks - ticks;
                if remaining_ticks > 0 {
                    let quarter_ticks = 480;
                    let num_fill_quarters = remaining_ticks / quarter_ticks;
                    for _ in 0..num_fill_quarters {
                        entries.push(RhythmEntry::Note(Duration::Quarter));
                    }
                }

                let final_ticks: i32 = entries.iter().map(|e| e.duration().ticks()).sum();
                (Some(entries), Vec::new(), final_ticks, specs, Vec::new(), Vec::new())
            } else if let Some(data) = melody_data {
                // Use preprocessed melody segments
                let melody_durations: Vec<Duration> =
                    data.segments.iter().map(|seg| seg.to_duration()).collect();
                let melody_ticks: i32 = melody_durations.iter().map(|d| d.ticks()).sum();

                // Fill remaining space with quarter note slashes
                let remaining_ticks = measure_ticks - melody_ticks;
                let num_melody_notes = melody_durations.len();
                let fill_slashes = if remaining_ticks > 0 {
                    let quarter_ticks = 480;
                    let num_fill_quarters = remaining_ticks / quarter_ticks;
                    (0..num_fill_quarters)
                        .map(|_| Duration::Quarter)
                        .collect::<Vec<_>>()
                } else {
                    Vec::new()
                };
                let num_fill_notes = fill_slashes.len();

                let full_rhythm: Vec<Duration> = melody_durations
                    .into_iter()
                    .chain(fill_slashes.into_iter())
                    .collect();

                let head_overrides: Vec<Option<NoteHeadType>> = if num_fill_notes > 0 {
                    let mut overrides = Vec::with_capacity(num_melody_notes + num_fill_notes);
                    for _ in 0..num_melody_notes {
                        overrides.push(None);
                    }
                    for _ in 0..num_fill_notes {
                        overrides.push(Some(NoteHeadType::Slash));
                    }
                    overrides
                } else {
                    Vec::new()
                };

                let ticks: i32 = full_rhythm.iter().map(|d| d.ticks()).sum();
                (None, full_rhythm, ticks, Vec::new(), head_overrides, Vec::new())
            } else if has_melodies {
                // Legacy: Convert raw melody notes to durations (no spillover handling)
                let melody_durations: Vec<Duration> = measure
                    .melodies
                    .iter()
                    .flat_map(|m| m.notes.iter())
                    .map(|note| self.melody_note_to_duration(note))
                    .collect();
                let melody_ticks: i32 = melody_durations.iter().map(|d| d.ticks()).sum();

                // Fill remaining space with quarter note slashes
                let remaining_ticks = measure_ticks - melody_ticks;
                let num_melody_notes = melody_durations.len();
                let fill_slashes = if remaining_ticks > 0 {
                    let quarter_ticks = 480;
                    let num_fill_quarters = remaining_ticks / quarter_ticks;
                    (0..num_fill_quarters)
                        .map(|_| Duration::Quarter)
                        .collect::<Vec<_>>()
                } else {
                    Vec::new()
                };
                let num_fill_notes = fill_slashes.len();

                let full_rhythm: Vec<Duration> = melody_durations
                    .into_iter()
                    .chain(fill_slashes.into_iter())
                    .collect();

                let head_overrides: Vec<Option<NoteHeadType>> = if num_fill_notes > 0 {
                    let mut overrides = Vec::with_capacity(num_melody_notes + num_fill_notes);
                    for _ in 0..num_melody_notes {
                        overrides.push(None);
                    }
                    for _ in 0..num_fill_notes {
                        overrides.push(Some(NoteHeadType::Slash));
                    }
                    overrides
                } else {
                    Vec::new()
                };

                let ticks: i32 = full_rhythm.iter().map(|d| d.ticks()).sum();
                (None, full_rhythm, ticks, Vec::new(), head_overrides, Vec::new())
            } else {
                // No melodies or explicit rhythms - use slash notation
                // Count only non-pushed chords - pushed chords spill to the previous beat
                // and don't consume their own beat position
                let non_pushed_count = measure.chords.iter().filter(|c| {
                    c.push_pull.as_ref().map_or(true, |(is_push, _)| !is_push)
                }).count();
                let num_beats = non_pushed_count.max(time_signature.0 as usize);

                // Check if any chord in this measure has a triplet push
                let has_triplet_push = measure.chords.iter().any(|c| {
                    if let Some((is_push, amount)) = &c.push_pull {
                        *is_push && amount.base == PushPullBase::Triplet && amount.level == 1
                    } else {
                        false
                    }
                });

                // Check for spillback chords (from next measure pushing back)
                // and build rhythm with triplet subdivisions if:
                // - use_stems is enabled, OR
                // - there are spillbacks, OR
                // - any chord has a triplet push
                let (rhythm, ticks, specs, _spillback_chords, internal_push_positions) = if self.config.use_stems || spillbacks.is_some() || has_triplet_push {
                    // Build rhythm with triplet subdivisions for pushed chords
                    self.build_rhythm_with_triplets(measure, num_beats, spillbacks)
                } else {
                    // Simple quarter note slashes
                    let rhythm: Vec<Duration> =
                        (0..num_beats).map(|_| Duration::Quarter).collect();
                    let ticks: i32 = rhythm.iter().map(|d| d.ticks()).sum();
                    (rhythm, ticks, Vec::new(), Vec::new(), Vec::new())
                };
                (None, rhythm, ticks, specs, Vec::new(), internal_push_positions)
            };

        // Convert measure width from points to spatiums for justification
        let width_spatiums = measure_width / self.config.spatium;

        // Calculate number of rhythm segments (for chord min width calculation)
        let num_segments = if let Some(ref entries) = rhythm_entries {
            entries.len()
        } else {
            full_rhythm.len()
        };

        // Compute minimum segment widths based on actual chord symbol layout bounds
        // This ensures segments are wide enough to prevent chord symbol collisions
        let chord_min_widths =
            self.compute_chord_min_widths(measure, num_segments, measure_width, ctx);

        // Apply auto_rhythm_slashes: expand whole/half notes to quarter slashes
        // This makes master rhythm charts easier to read by showing consistent quarter slashes
        // instead of whole note diamonds for sustained chords
        // Track whether we did auto-expansion (those slashes should be stemless)
        let auto_expanded = self.config.auto_rhythm_slashes && !has_melodies && !has_explicit_chord_rhythm;
        let full_rhythm = if auto_expanded {
            self.expand_rhythm_to_quarters(full_rhythm)
        } else {
            full_rhythm
        };

        // Build the measure based on whether we have explicit rhythm entries or not
        let mut builder = MeasureBuilder::new()
            .id_base(id_base as u64)
            .justify_to(width_spatiums)
            .no_barlines() // Barlines handled by chart_layout
            .segment_min_widths(chord_min_widths);

        // Set the rhythm using either entries (for explicit rhythms) or rhythm (for slash fills)
        if let Some(entries) = rhythm_entries {
            // Also expand entries if auto_rhythm_slashes is on (but only for non-explicit rhythms)
            // IMPORTANT: Don't expand when has_explicit_chord_rhythm - user specified exact durations
            let entries = if self.config.auto_rhythm_slashes && !has_melodies && !has_explicit_chord_rhythm {
                self.expand_entries_to_quarters(entries)
            } else {
                entries
            };
            builder = builder.entries(entries);
        } else {
            builder = builder.rhythm(full_rhythm);
        }

        // Apply compact mode for minimal margins (e.g., count-in measures)
        if compact {
            builder = builder.compact();
        }

        // Apply head type overrides for mixed melody/slash notation
        if !head_type_overrides.is_empty() {
            builder = builder.head_type_overrides(head_type_overrides);
        }

        // Use rhythmic (slash) notation when:
        // - We have explicit chord rhythms (these should render as slashes with stems)
        // - We don't have melodies (pure slash notation)
        if has_explicit_chord_rhythm || !has_melodies {
            builder = builder.rhythmic();

            // Stemless behavior:
            // 1. When use_stems is false and no triplets - all stemless via builder.stemless()
            // 2. When auto_rhythm_slashes is true - use auto_stemless() which makes
            //    plain quarters stemless but keeps tuplet notes with stems
            // 3. Otherwise - use auto_stemless() for per-note computation
            let has_triplets = !tuplet_specs.is_empty();

            if !self.config.use_stems && !has_explicit_chord_rhythm && !has_triplets {
                // Pure stemless mode - no triplets, just make everything stemless
                builder = builder.stemless();
            } else if self.config.auto_rhythm_slashes && !has_melodies {
                // Auto-rhythm slashes mode - use per-note auto_stemless()
                // This makes plain quarters stemless but keeps triplet notes with stems
                builder = builder.auto_stemless();
            }
            // Otherwise, compute_auto_stemless() runs automatically in build()
        }

        if include_clef {
            builder = builder.clef(ClefType::Treble);
        }

        if include_time_sig {
            builder = builder.time_signature(time_signature.0, time_signature.1);
        }

        // Apply tuplet specifications for triplet groups
        for spec in &tuplet_specs {
            builder = builder.tuplet_group(spec.start_idx, spec.end_idx, spec.ratio);
        }

        let mut result = builder.build(ctx);

        // Set internal push positions for chord rendering
        result.internal_push_positions = internal_push_positions;

        // Add ties for cross-barline notes
        if let Some(data) = melody_data {
            self.add_melody_ties(&mut result, data, ctx);
        }

        result
    }

    /// Build rhythm array with triplet subdivisions for pushed chords.
    ///
    /// Delegates to the standalone function in [`rhythm_builder`].
    #[allow(clippy::type_complexity)]
    fn build_rhythm_with_triplets(
        &self,
        measure: &crate::chart::types::Measure,
        num_beats: usize,
        spillbacks: Option<&[PushSpillback]>,
    ) -> (Vec<Duration>, i32, Vec<TupletSpec>, Vec<(usize, String)>, Vec<(usize, usize)>) {
        rhythm_builder::build_rhythm_with_triplets(measure, num_beats, spillbacks)
    }

    /// Check if a measure has explicit chord rhythms (Lily or Rest notation).
    ///
    /// Delegates to the standalone function in [`rhythm_builder`].
    fn measure_has_explicit_chord_rhythm(&self, measure: &Measure) -> bool {
        rhythm_builder::measure_has_explicit_chord_rhythm(measure)
    }

    /// Build rhythm entries from explicit chord rhythms.
    ///
    /// Delegates to the standalone function in [`rhythm_builder`].
    fn build_rhythm_from_chord_rhythms(
        &self,
        measure: &Measure,
    ) -> (Vec<RhythmEntry>, i32, Vec<TupletSpec>) {
        rhythm_builder::build_rhythm_from_chord_rhythms(measure)
    }

    /// Convert keyflow LilySyntax to engraver Duration.
    ///
    /// Delegates to the standalone function in [`rhythm_builder`].
    fn lily_syntax_to_duration(&self, lily: LilySyntax, dotted: bool, triplet: bool) -> Duration {
        rhythm_builder::lily_syntax_to_duration(lily, dotted, triplet)
    }

    /// Expand whole/half note durations to quarter notes for auto_rhythm_slashes.
    ///
    /// This converts sustained chord notation (diamonds) to rhythmic slashes,
    /// which is standard for master rhythm charts.
    fn expand_rhythm_to_quarters(&self, rhythm: Vec<Duration>) -> Vec<Duration> {
        let mut expanded = Vec::with_capacity(rhythm.len() * 4);
        for dur in rhythm {
            let ticks = dur.ticks();
            let quarter_ticks = Duration::Quarter.ticks(); // 480

            if ticks >= quarter_ticks * 2 {
                // Whole note (1920 ticks) -> 4 quarters
                // Half note (960 ticks) -> 2 quarters
                // Dotted half (1440 ticks) -> 3 quarters
                let num_quarters = ticks / quarter_ticks;
                for _ in 0..num_quarters {
                    expanded.push(Duration::Quarter);
                }
                // Handle remaining ticks (e.g., dotted rhythms may have fractional beats)
                let remaining = ticks % quarter_ticks;
                if remaining > 0 {
                    // For now, just add an eighth if there's a half-beat remainder
                    if remaining >= Duration::Eighth.ticks() {
                        expanded.push(Duration::Eighth);
                    }
                }
            } else {
                // Quarter notes and shorter stay as-is
                expanded.push(dur);
            }
        }
        expanded
    }

    /// Expand rhythm entries (notes/rests) to quarters for auto_rhythm_slashes.
    fn expand_entries_to_quarters(&self, entries: Vec<RhythmEntry>) -> Vec<RhythmEntry> {
        let mut expanded = Vec::with_capacity(entries.len() * 4);
        for entry in entries {
            match entry {
                RhythmEntry::Note(dur) => {
                    let ticks = dur.ticks();
                    let quarter_ticks = Duration::Quarter.ticks();

                    if ticks >= quarter_ticks * 2 {
                        let num_quarters = ticks / quarter_ticks;
                        for _ in 0..num_quarters {
                            expanded.push(RhythmEntry::Note(Duration::Quarter));
                        }
                        let remaining = ticks % quarter_ticks;
                        if remaining >= Duration::Eighth.ticks() {
                            expanded.push(RhythmEntry::Note(Duration::Eighth));
                        }
                    } else {
                        expanded.push(RhythmEntry::Note(dur));
                    }
                }
                RhythmEntry::Rest(dur) => {
                    // Keep rests as-is (don't expand sustained rests to quarter slashes)
                    expanded.push(RhythmEntry::Rest(dur));
                }
            }
        }
        expanded
    }

    /// Layout a count-in measure with a whole rest.
    ///
    /// Count-in measures are rendered smaller and show only a whole rest,
    /// indicating empty beats before the song starts.
    fn layout_count_in_measure(
        &self,
        measure_width: f64,
        include_clef: bool,
        include_time_sig: bool,
        time_signature: (u8, u8),
        ctx: &LayoutContext<'_>,
        id_base: u64,
    ) -> MeasureScene {
        let spatium = ctx.spatium();
        let width_spatiums = measure_width / spatium;

        // Create the root scene node for this measure
        let mut root = SceneNode::group(SemanticId::new(ElementType::Measure, id_base));

        let mut x_offset = 0.0;

        // Optionally render clef
        if include_clef {
            let clef_params = ClefParams {
                id: id_base + 1,
                clef_type: ClefType::Treble,
                ..Default::default()
            };
            let (clef_layout, mut clef_node) = layout_clef(&clef_params, ctx);
            clef_node.transform = Affine::translate((x_offset, 0.0));
            root.add_child(clef_node);
            x_offset += clef_layout.bbox.width() + spatium * 0.5;
        }

        // Optionally render time signature
        if include_time_sig {
            let ts_params = TimeSigParams {
                id: id_base + 2,
                sig_type: TimeSigType::Numeric {
                    numerator: time_signature.0,
                    denominator: time_signature.1,
                },
                ..Default::default()
            };
            let (ts_layout, mut ts_node) = layout_timesig(&ts_params, ctx);
            ts_node.transform = Affine::translate((x_offset, 0.0));
            root.add_child(ts_node);
            x_offset += ts_layout.bbox.width() + spatium * 0.5;
        }

        // Render whole rest centered in remaining space
        let rest_params = RestParams {
            id: id_base + 3,
            duration: RestDuration::Whole,
            dots: 0,
            line: 0, // Center line
        };
        let (rest_layout, mut rest_node) = layout_rest(&rest_params, ctx);

        // Center the rest in the remaining measure width
        let remaining_width = measure_width - x_offset;
        let rest_x = x_offset + (remaining_width - rest_layout.bbox.width()) / 2.0;
        rest_node.transform = Affine::translate((rest_x, 0.0));
        root.add_child(rest_node);

        MeasureScene {
            scene: root,
            width: width_spatiums,
            segments: SegmentList::new(), // Empty segment list for count-in
            internal_push_positions: Vec::new(),
        }
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
    fn melody_note_to_duration(&self, note: &crate::chart::melody::MelodyNote) -> Duration {
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
    ///
    /// Delegates to [`chord_layout::get_chord_rest_positions`].
    fn get_chord_rest_positions(&self, measure_scene: &MeasureScene) -> Vec<f64> {
        chord_layout::get_chord_rest_positions(measure_scene)
    }

    /// Estimate the "content weight" of a measure for spring-based layout.
    ///
    /// Content weight determines how much space a measure should receive relative
    /// to other measures in the same system. Measures with more content (more chords,
    /// longer chord names, more complex rhythms) get higher weights.
    ///
    /// # Arguments
    /// * `measure` - The measure to estimate
    /// * `text_metrics` - Text metrics for measuring chord name widths
    ///
    /// # Returns
    /// A weight value (typically 1.0-3.0) where higher = more space needed
    /// Compute minimum segment widths based on actual chord symbol layout bounds.
    ///
    /// Compute minimum segment widths for chord symbol collision avoidance.
    ///
    /// NOTE: Pre-layout collision detection is disabled as it was causing inconsistent
    /// measure widths. Returns zeros to let natural spacing occur. Collision resolution
    /// happens post-render in chord_renderer where we have actual rendered bounds.
    fn compute_chord_min_widths(
        &self,
        _measure: &crate::chart::types::Measure,
        num_segments: usize,
        _measure_width: f64,
        _ctx: &LayoutContext<'_>,
    ) -> Vec<f64> {
        vec![0.0; num_segments]
    }

    fn estimate_measure_content_weight(
        &self,
        measure: &crate::chart::types::Measure,
        _text_metrics: &TextFontMetrics,
    ) -> f64 {
        // Weight is based on the sum of natural widths of all rhythm elements.
        // This follows MuseScore's duration stretch formula: slope^log2(ticks/480)
        // where slope (measure_spacing) is typically 1.3.
        //
        // This ensures measures with more segments (especially shorter notes)
        // get proportionally more space to fit all content.
        //
        // Additionally, triplets receive extra weight because they require bracket
        // notation (└3┘) which needs horizontal space for visual clarity.

        const QUARTER_TICKS: f64 = 480.0;
        const MEASURE_SPACING: f64 = 1.3; // MuseScore default slope
        const TRIPLET_BONUS: f64 = 0.15;  // Extra weight per triplet element

        /// Calculate duration stretch for a given tick count using MuseScore formula
        fn duration_stretch(ticks: i32) -> f64 {
            if ticks <= 0 {
                return 1.0;
            }
            let ratio = f64::from(ticks) / QUARTER_TICKS;
            MEASURE_SPACING.powf(ratio.log2())
        }

        /// Extract ticks from a ChordRhythm
        fn rhythm_ticks(rhythm: &ChordRhythm) -> i32 {
            match rhythm {
                ChordRhythm::Slashes { count, dotted, .. } => {
                    let base = 480 * (*count as i32);
                    if *dotted { base * 3 / 2 } else { base }
                }
                ChordRhythm::Default => 1920, // One bar = 4 beats
                ChordRhythm::Explicit(nd) => nd.total_ticks_480(),
            }
        }

        /// Check if a ChordRhythm is a triplet
        fn is_triplet(rhythm: &ChordRhythm) -> bool {
            rhythm.lily_parts().map_or(false, |(_, _, triplet)| triplet)
        }

        // Sum the natural widths (stretch factors) for all rhythm elements,
        // count segments, and count triplet elements
        let (total_stretch, segment_count, triplet_count): (f64, usize, usize) =
            if !measure.rhythm_elements.is_empty() {
                // Use rhythm_elements which includes both chords and rests
                let mut stretch = 0.0;
                let mut triplets = 0;
                for elem in &measure.rhythm_elements {
                    let (ticks, is_trip) = match elem {
                        crate::chart::types::RhythmElement::Chord(chord) => {
                            (rhythm_ticks(&chord.rhythm), is_triplet(&chord.rhythm))
                        }
                        crate::chart::types::RhythmElement::Rest(rest) => {
                            (rhythm_ticks(&rest.rhythm), is_triplet(&rest.rhythm))
                        }
                        crate::chart::types::RhythmElement::Space(space) => {
                            (480, is_triplet(&space.rhythm)) // Quarter note equivalent
                        }
                    };
                    stretch += duration_stretch(ticks);
                    if is_trip {
                        triplets += 1;
                    }
                }
                (stretch, measure.rhythm_elements.len(), triplets)
            } else {
                // Fallback: use chords only
                let mut stretch = 0.0;
                let mut triplets = 0;
                for c in &measure.chords {
                    stretch += duration_stretch(rhythm_ticks(&c.rhythm));
                    if is_triplet(&c.rhythm) {
                        triplets += 1;
                    }
                }
                (stretch, measure.chords.len().max(1), triplets)
            };

        // Calculate weight from two factors:
        // 1. Stretch-based weight: sum of rhythmic stretches / 4 (so 4 quarter notes = 1.0)
        // 2. Segment-based weight: number of segments / 4 (so 4 segments = 1.0)
        //
        // Take the MAX to ensure measures with many short notes get enough space.
        // A measure with 6 thirty-second notes has low stretch (~2.8) but needs 6 segment widths.
        let stretch_weight = total_stretch / 4.0;
        let segment_weight = segment_count as f64 / 4.0;

        // Add triplet complexity bonus - brackets need extra horizontal space
        let triplet_bonus = triplet_count as f64 * TRIPLET_BONUS;

        // Use the larger of the two weights, plus triplet bonus
        let weight = stretch_weight.max(segment_weight).max(1.0) + triplet_bonus;

        // Clamp to reasonable range
        weight.clamp(0.5, 4.0)
    }

    /// Distribute available width among measures using spring physics.
    ///
    /// Delegates to [`measure_layout::distribute_measure_widths`].
    fn distribute_measure_widths(
        &self,
        weights: &[f64],
        count_in_measures: usize,
        total_width: f64,
        compact_scale: f64,
        base_measure_width: f64,
    ) -> Vec<f64> {
        measure_layout::distribute_measure_widths(
            weights,
            count_in_measures,
            total_width,
            compact_scale,
            base_measure_width,
        )
    }

    /// Compute chord symbol widths for a measure and check for collisions.
    ///
    /// Returns the optimal font scale factor (1.0 = no scaling needed, < 1.0 = reduce size).
    /// This implements dynamic font sizing to prevent chord symbol collisions.
    ///
    /// # Arguments
    /// * `chords` - The chord symbols in the measure
    /// * `segment_positions` - X positions of each segment (from rhythm layout)
    /// * `measure_width` - Total width of the measure
    /// * `style` - Current harmony style (for font size reference)
    /// * `text_metrics` - Text metrics for width calculation
    ///
    /// # Returns
    /// A scale factor to apply to the harmony style (1.0 if no scaling needed)
    fn compute_chord_font_scale(
        &self,
        chords: &[crate::chart::types::ChordInstance],
        segment_positions: &[f64],
        measure_width: f64,
        style: &HarmonyStyle,
        text_metrics: &TextFontMetrics,
    ) -> f64 {
        // Filter to only visible chords (not spaces/rests)
        let visible_chords: Vec<_> = chords
            .iter()
            .enumerate()
            .filter(|(_, c)| !c.full_symbol.is_empty() && c.full_symbol != "s" && c.full_symbol != "r")
            .collect();

        if visible_chords.len() <= 1 {
            return 1.0; // No collision possible with 0 or 1 chord
        }

        // Compute widths of all visible chord symbols at current font size
        let base_font_size = style.root_size;
        let min_spacing = base_font_size * 0.3; // Minimum space between chords

        let chord_widths: Vec<(usize, f64)> = visible_chords
            .iter()
            .map(|(idx, c)| {
                let width = text_metrics.horizontal_advance(&c.full_symbol, base_font_size);
                (*idx, width.max(base_font_size * 1.5)) // Minimum width
            })
            .collect();

        // Check for collisions between consecutive chords
        let mut max_scale: f64 = 1.0;

        for i in 0..chord_widths.len() - 1 {
            let (idx1, width1) = chord_widths[i];
            let (idx2, _width2) = chord_widths[i + 1];

            // Get segment positions for these chords
            let pos1 = segment_positions.get(idx1).copied().unwrap_or(0.0);
            let pos2 = segment_positions.get(idx2).copied().unwrap_or(measure_width);

            // Available space between chord positions
            let available_space = pos2 - pos1;

            // Required space = chord width + minimum spacing
            let required_space = width1 + min_spacing;

            // If chord extends beyond available space, compute scale factor
            if required_space > available_space && available_space > 0.0 {
                let scale: f64 = available_space / required_space;
                max_scale = max_scale.min(scale);
            }
        }

        // Clamp to reasonable range (don't scale below 50% or above 100%)
        max_scale.clamp(0.5, 1.0)
    }

    /// Create a scaled harmony style for collision avoidance.
    ///
    /// # Arguments
    /// * `base_style` - The original harmony style
    /// * `scale` - Scale factor (0.5 to 1.0)
    ///
    /// # Returns
    /// A new harmony style with scaled font sizes
    fn scaled_harmony_style(&self, base_style: &HarmonyStyle, scale: f64) -> HarmonyStyle {
        let mut scaled = base_style.clone();
        scaled.root_size *= scale;
        scaled
    }
}

// region:    --- Test Utilities

/// Query utilities for headless layout testing.
///
/// These utilities enable position verification tests without GPU rendering.
#[cfg(test)]
pub mod test_utils {
    use super::*;
    use crate::engraver::scene::id::ElementType;
    use crate::engraver::scene::traverse::SceneNodeExt;
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
                let page = node
                    .metadata
                    .get("page")
                    .and_then(|s| s.parse::<u32>().ok());

                Some(ElementPosition {
                    element_type: id.element_type,
                    id: id.id,
                    world_x: world_origin.x,
                    world_y: world_origin.y,
                    bounds: if node.bounds.is_zero_area() {
                        None
                    } else {
                        Some(node.bounds)
                    },
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
                node.id
                    .as_ref()
                    .map_or(false, |id| id.element_type == element_type)
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
    use super::test_utils::*;
    use super::*;
    use crate::engraver::scene::id::ElementType;
    use crate::engraver::style::MStyle;
    use crate::{
        AbsolutePosition, Chart, ChartSection, Chord, ChordInstance, ChordQuality, ChordRhythm,
        Measure, MusicalDuration, MusicalNote, MusicalPosition, RootNotation, Section, SectionType,
        TimeSignature,
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
            chords: vec![ChordInstance::new(
                root("C"),
                "C".to_string(),
                Chord::new(root("C"), ChordQuality::Major),
                ChordRhythm::Default,
                "C".to_string(),
                MusicalDuration::new(0, 4, 0), // whole note
                AbsolutePosition::new(
                    MusicalPosition::try_new(0, 0, 0).unwrap(), // beat 0
                    0,                                          // section index
                ),
            )],
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
                        0,                                          // section index
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
                        0,                                          // section index
                    ),
                ),
            ],
            ..Default::default()
        };

        let mut section_info = Section::new(SectionType::Verse);
        section_info.number = Some(1);
        section_info.measure_count = Some(2);

        let section = ChartSection::new(section_info).with_measures(vec![measure1, measure2]);

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
        assert_eq!(
            chord_count, 3,
            "Expected 3 chord symbols, found {}",
            chord_count
        );
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
        assert!(
            measure_count >= 2,
            "Expected at least 2 measures, found {}",
            measure_count
        );
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
        let chart_chord_count: usize = chart
            .sections
            .iter()
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
                i,
                cs.world_x
            );
            println!(
                "Chord symbol {}: world_x={:.1}, world_y={:.1}",
                i, cs.world_x, cs.world_y
            );
        }

        // Chord symbols should be in increasing x order within each measure
        // The last two chords (G and Dm in measure 2) should have G.x < Dm.x
        if chord_symbols.len() >= 2 {
            let dm_chord = &chord_symbols[chord_symbols.len() - 1]; // Dm is last
            let g_chord = &chord_symbols[chord_symbols.len() - 2]; // G is second-to-last

            assert!(
                dm_chord.world_x > g_chord.world_x,
                "Dm chord (beat 2) should be to the right of G chord (beat 0): Dm.x={:.1} should be > G.x={:.1}",
                dm_chord.world_x,
                g_chord.world_x
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

        let c_chord = &chord_symbols[0]; // C in measure 1
        let g_chord = &chord_symbols[1]; // G in measure 2
        let dm_chord = &chord_symbols[2]; // Dm in measure 2

        // G should be to the right of C (different measure)
        assert!(
            g_chord.world_x > c_chord.world_x,
            "G (measure 2) should be right of C (measure 1): G.x={:.1} > C.x={:.1}",
            g_chord.world_x,
            c_chord.world_x
        );

        // Dm should be to the right of G (same measure, later beat)
        assert!(
            dm_chord.world_x > g_chord.world_x,
            "Dm (beat 2) should be right of G (beat 0): Dm.x={:.1} > G.x={:.1}",
            dm_chord.world_x,
            g_chord.world_x
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
        let unique_measures: Vec<_> = measures
            .iter()
            .filter(|m| {
                if let Some(page) = m.page {
                    let key = (m.world_x.round() as i64, m.world_y.round() as i64, page);
                    seen_positions.insert(key)
                } else {
                    false // Skip measures without page metadata
                }
            })
            .collect();

        println!(
            "Found {} unique measure positions with page tags:",
            unique_measures.len()
        );
        for (i, m) in unique_measures.iter().enumerate() {
            println!(
                "  Measure {}: x={:.1}, y={:.1}, page={:?}",
                i, m.world_x, m.world_y, m.page
            );
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
                    page,
                    system_y,
                    i + 1,
                    width,
                    first_width,
                    diff
                );
            }

            println!(
                "Page {}, system y={}: {} measures, all widths equal ({:.1} points)",
                page,
                system_y,
                sorted.len(),
                first_width
            );
        }

        // Also verify total measure count matches chart
        let expected_measures: usize = chart.sections.iter().map(|s| s.measures().len()).sum();

        // Account for MeasureBuilder creating internal structure
        assert!(
            measures.len() >= expected_measures,
            "Expected at least {} measures, found {}",
            expected_measures,
            measures.len()
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

        println!(
            "Total measures: {}, with page metadata: {}",
            measures.len(),
            measures_with_page.len()
        );

        // At least the outer measure containers should have page metadata
        assert!(
            !measures_with_page.is_empty(),
            "Expected some measures to have page metadata"
        );

        // Check that chord symbols have page metadata
        let chords = find_elements_by_type(&result.scene, ElementType::ChordSymbol);
        let chords_with_page: Vec<_> = chords.iter().filter(|c| c.page.is_some()).collect();

        println!(
            "Total chord symbols: {}, with page metadata: {}",
            chords.len(),
            chords_with_page.len()
        );

        assert!(
            !chords_with_page.is_empty(),
            "Expected chord symbols to have page metadata"
        );

        // Group by page and verify distribution
        let measures_cloned: Vec<_> = measures_with_page.iter().map(|m| (*m).clone()).collect();
        let measures_by_page = group_elements_by_page(&measures_cloned);
        println!(
            "Measures distributed across {} page(s)",
            measures_by_page.len()
        );
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

        // With 20pt system spacing and title header, we should fit 8-10 systems on a page
        // (8×50pt systems + 7×20pt spacing + ~65pt header = ~605pt, available = 706pt)
        assert!(
            metrics.system_count >= 8,
            "Expected at least 8 systems on page 1, got {}. \
            Available height: {:.1}, content height: {:.1}",
            metrics.system_count,
            metrics.available_height,
            metrics.content_height
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
                    i + 1,
                    spacing,
                    first_spacing,
                    diff
                );
            }
            println!(
                "All inter-system spacings are consistent: {:.1} points",
                first_spacing
            );
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
            println!(
                "\n=== Page {} Bottom Margin Test ===",
                page_metrics.page_number
            );
            page_metrics.print_debug();

            // Last system should not overflow the bottom margin
            assert!(
                page_metrics.last_system_to_bottom >= 0.0,
                "Page {}: Last system overflows bottom margin by {:.1} points",
                page_metrics.page_number,
                -page_metrics.last_system_to_bottom
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
        let critical_warnings: Vec<_> = warnings
            .iter()
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
        println!(
            "Total dimensions: {:.1} × {:.1}",
            result.total_width, result.total_height
        );
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
        assert!(
            page1.system_count >= 5,
            "Should have at least 5 systems on page 1"
        );
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
                m.page_number,
                m.system_count,
                m.system_y_positions.first().unwrap_or(&0.0)
            );
        }

        // First page should have systems starting at margin.top
        // (header text is handled separately in the renderer)
        if let Some(page1) = all_metrics.first() {
            let first_system_y = page1.system_y_positions.first().copied().unwrap_or(0.0);
            assert!(
                first_system_y >= page1.margins.top - 1.0, // Allow 1pt tolerance
                "First system should start at or below top margin: y={:.1}, margin={:.1}",
                first_system_y,
                page1.margins.top
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
        println!(
            "Total measures: {}",
            chart
                .sections
                .iter()
                .map(|s| s.measures().len())
                .sum::<usize>()
        );
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
                metrics.page_number,
                metrics.system_count
            );

            // No overflow
            assert!(
                metrics.last_system_to_bottom >= -1.0, // 1pt tolerance for rounding
                "Page {}: Content overflows bottom margin by {:.1}pt",
                metrics.page_number,
                -metrics.last_system_to_bottom
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
        println!(
            "Systems per page average: {:.1}",
            total_systems as f64 / result.pages.len() as f64
        );
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
        println!(
            "With hide_repeated_chords=true: {} chord symbols rendered",
            chord_count_hidden
        );

        // With hiding, we should see: C, G, C = 3 unique chord changes
        assert_eq!(
            chord_count_hidden, 3,
            "Expected 3 chord symbols with hiding enabled (C, G, C), got {}",
            chord_count_hidden
        );

        // Test with hide_repeated_chords = false
        let mut config = ChartLayoutConfig::default();
        config.hide_repeated_chords = false;
        let engine_no_hide = ChartLayoutEngine::with_config(config, style, text_font, symbol_font);
        let result_no_hide = engine_no_hide.layout_chart(&chart, &LayoutMode::default());

        let chord_count_all =
            count_elements_by_type(&result_no_hide.scene, ElementType::ChordSymbol);
        println!(
            "With hide_repeated_chords=false: {} chord symbols rendered",
            chord_count_all
        );

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
            println!(
                "\nSection {}: {:?} ({} measures)",
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
        let verse_section = chart
            .sections
            .iter()
            .find(|s| matches!(s.section.section_type, SectionType::Verse))
            .expect("Should have a verse section");

        println!(
            "Verse section: {} measures declared",
            verse_section.measures().len()
        );

        // Print all measures with their chords
        for (i, measure) in verse_section.measures().iter().enumerate() {
            let chords: Vec<&str> = measure
                .chords
                .iter()
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
        let engine = ChartLayoutEngine::new(style, Arc::new(Vec::new()), Arc::new(Vec::new()));

        let result = engine.layout_chart(&chart, &LayoutMode::default());

        println!("\n=== SCENE GRAPH LEVEL: Chord Nodes with Measure Metadata ===");

        // Find all chord nodes and group by section/measure
        fn find_chords_with_measure(
            node: &SceneNode,
            chords: &mut Vec<(String, Option<u32>, Option<String>)>,
        ) {
            // Check if this is a chord node
            if node.metadata.get("element_type") == Some(&"chord".to_string()) {
                // Extract the text content
                let mut chord_text = String::new();
                for cmd in &node.commands {
                    if let PaintCommand::Text { text, .. } = cmd {
                        chord_text.push_str(text);
                    }
                }

                let measure = node
                    .metadata
                    .get("measure")
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
        let verse_bar5_chords: Vec<_> = chords
            .iter()
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
                        println!(
                            "{}  Glyph: U+{:04X} ('{}')",
                            indent, *codepoint as u32, codepoint
                        );
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
