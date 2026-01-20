//! Chord symbol rendering for chart layout.
//!
//! This module extracts the duplicated chord rendering logic from
//! `layout_paginated` and `layout_continuous` into reusable functions.

use crate::engraver::layout::context::LayoutContext;
use crate::engraver::layout::tlayout::{layout_harmony, parse_chord, HarmonyParams, HarmonyStyle};
use crate::engraver::scene::node::{metadata_keys, SceneNode};
use crate::chart::types::{ChordInstance, Measure, RhythmElement};
use crate::chord::ChordRhythm;
use crate::time::TimeSignature;
use crate::{ChartPosition, SourceLink};

use super::types::PushSpillback;

/// Context for rendering chord symbols in a measure.
#[derive(Debug, Clone)]
pub struct ChordRenderContext<'a> {
    /// Measure x position (start of measure content).
    pub measure_x: f64,
    /// Chord y position (above staff).
    pub chord_y: f64,
    /// Page number (1-indexed, for paginated mode).
    pub page_number: Option<u32>,
    /// Global system index (0-indexed).
    pub global_system_index: usize,
    /// Section measure index (within this section).
    pub measure_idx: usize,
    /// Local measure index (within the current system).
    pub local_measure_idx: usize,
    /// Section type name for metadata.
    pub section_name: &'a str,
    /// Segment positions from measure layout.
    pub segment_positions: &'a [f64],
    /// Internal push positions from rhythm builder.
    pub internal_push_positions: &'a [(usize, usize)],
    /// Harmony style for chord symbols.
    pub harmony_style: &'a HarmonyStyle,
    /// Time signature (numerator, denominator).
    pub time_signature: (u8, u8),
    /// Whether to hide repeated consecutive chords.
    pub hide_repeated_chords: bool,
}

/// Result of chord symbol rendering.
#[derive(Debug)]
pub struct ChordRenderResult {
    /// Rendered chord nodes.
    pub nodes: Vec<SceneNode>,
    /// Updated previous chord symbol (for duplicate detection).
    pub last_chord_symbol: Option<String>,
    /// Next ID counter value.
    pub next_id: u64,
}

/// Determine if a chord should be skipped (is a space/rest placeholder).
#[must_use]
pub fn is_placeholder_chord(symbol: &str) -> bool {
    symbol.is_empty() || symbol == "s" || symbol == "r"
}

/// Check if this is the first real (non-placeholder) chord in the measure.
#[must_use]
pub fn is_first_real_chord(chords: &[ChordInstance], chord_idx: usize) -> bool {
    chords
        .iter()
        .take(chord_idx)
        .all(|c| is_placeholder_chord(&c.full_symbol))
}

/// Check if a chord should be rendered at a section/system boundary.
///
/// Pushed chords at boundaries should show even if they would normally spill back.
#[must_use]
pub fn is_at_boundary(measure_idx: usize, local_measure_idx: usize) -> bool {
    let is_first_measure_of_section = measure_idx == 0;
    let is_first_measure_of_system = local_measure_idx == 0;
    is_first_measure_of_section || is_first_measure_of_system
}

/// Calculate the segment index for a chord symbol.
///
/// This complex logic handles multiple cases:
/// - Pushed chords at boundaries (force to segment 0)
/// - Internal pushed chords (use precomputed positions)
/// - Explicit rhythm notation
/// - Slash notation
/// - Simple measures
#[must_use]
pub fn calculate_segment_index(
    measure: &Measure,
    chord_idx: usize,
    chord: &ChordInstance,
    segment_positions: &[f64],
    internal_push_positions: &[(usize, usize)],
    is_first_real: bool,
    is_boundary: bool,
) -> usize {
    // Check if this is a pushed chord at a boundary
    let is_pushed_at_boundary = chord
        .push_pull
        .as_ref()
        .map_or(false, |(is_push, _)| *is_push)
        && is_first_real
        && is_boundary;

    if is_pushed_at_boundary {
        // Force pushed chord to segment 0 (beat 1) at section/line start
        return 0;
    }

    // Check if this is an internal pushed chord (pushed but not spillback)
    let is_internal_push = chord
        .push_pull
        .as_ref()
        .map_or(false, |(is_push, _)| *is_push)
        && !is_first_real
        && !internal_push_positions.is_empty();

    if is_internal_push {
        // Internal pushed chord - look up precomputed segment
        if let Some((_, seg_idx)) = internal_push_positions
            .iter()
            .find(|(c_idx, _)| *c_idx == chord_idx)
        {
            return *seg_idx;
        }
        // Fallback
        return chord_idx.min(segment_positions.len().saturating_sub(1));
    }

    // Check for explicit rhythm elements
    if !measure.rhythm_elements.is_empty() {
        let has_explicit_rhythm = measure_has_explicit_chord_rhythm(measure);

        if has_explicit_rhythm {
            // Explicit rhythm: find chord's index in rhythm_elements
            let mut seen_chord_count = 0;
            let mut found_idx = None;
            for (idx, elem) in measure.rhythm_elements.iter().enumerate() {
                if let RhythmElement::Chord(_) = elem {
                    if seen_chord_count == chord_idx {
                        found_idx = Some(idx);
                        break;
                    }
                    seen_chord_count += 1;
                }
            }
            return found_idx
                .unwrap_or(chord_idx)
                .min(segment_positions.len().saturating_sub(1));
        }

        // Slash notation: calculate segment from cumulative beat durations
        let mut cumulative_beats = 0usize;
        let mut found_beat_pos = None;
        let mut seen_chord_count = 0;

        for elem in measure.rhythm_elements.iter() {
            if let RhythmElement::Chord(c) = elem {
                if seen_chord_count == chord_idx {
                    found_beat_pos = Some(cumulative_beats);
                    break;
                }
                let chord_beats = match &c.rhythm {
                    ChordRhythm::Slashes(n) => *n as usize,
                    ChordRhythm::Default => 1,
                    _ => 1,
                };
                cumulative_beats += chord_beats;
                seen_chord_count += 1;
            }
        }
        return found_beat_pos
            .unwrap_or(chord_idx)
            .min(segment_positions.len().saturating_sub(1));
    }

    // Simple measure - calculate segment from cumulative chord beats
    let mut cumulative_beats = 0usize;
    for (idx, c) in measure.chords.iter().enumerate() {
        if idx == chord_idx {
            break;
        }
        let chord_beats = match &c.rhythm {
            ChordRhythm::Slashes(n) => *n as usize,
            ChordRhythm::Default => 1,
            _ => 1,
        };
        cumulative_beats += chord_beats;
    }
    cumulative_beats.min(segment_positions.len().saturating_sub(1))
}

/// Check if a measure has explicit chord rhythms (Lily or Rest notation).
fn measure_has_explicit_chord_rhythm(measure: &Measure) -> bool {
    super::rhythm_builder::measure_has_explicit_chord_rhythm(measure)
}

/// Check if a chord should be hidden due to being a duplicate.
#[must_use]
pub fn should_hide_chord(
    chord: &ChordInstance,
    current_symbol: &str,
    previous_symbol: Option<&str>,
    is_pushed_at_boundary: bool,
    time_signature: (u8, u8),
    hide_repeated_chords: bool,
) -> bool {
    if !hide_repeated_chords {
        return false;
    }

    // Short duration chords should always be shown (hits/stabs)
    let ts = TimeSignature::new(time_signature.0.into(), time_signature.1.into());
    let chord_beats = chord.duration.to_beats(ts);
    let is_short_duration = chord_beats <= 0.5;

    if is_short_duration {
        return false;
    }

    // Pushed chords at boundaries should show
    if is_pushed_at_boundary {
        return false;
    }

    // Check for duplicate
    previous_symbol.map_or(false, |prev| prev == current_symbol)
}

/// Render chord symbols for a measure.
///
/// This handles all the complex logic for determining which chords to render,
/// where to position them, and what metadata to attach.
///
/// Note: `internal_push_positions` should already be included in `ctx`.
pub fn render_chord_symbols(
    ctx: &ChordRenderContext<'_>,
    measure: &Measure,
    previous_chord_symbol: Option<&str>,
    mut id_counter: u64,
    layout_ctx: &LayoutContext<'_>,
) -> ChordRenderResult {
    let mut nodes = Vec::new();
    let mut last_chord_symbol = previous_chord_symbol.map(String::from);

    let is_boundary = is_at_boundary(ctx.measure_idx, ctx.local_measure_idx);

    for (chord_idx, chord) in measure.chords.iter().enumerate() {
        let current_symbol = &chord.full_symbol;

        // Skip placeholders
        if is_placeholder_chord(current_symbol) {
            continue;
        }

        let is_first_real = is_first_real_chord(&measure.chords, chord_idx);

        // Skip pushed chords that spill back (except at boundaries)
        if let Some((is_push, _)) = &chord.push_pull {
            if *is_push && is_first_real && !is_boundary {
                continue;
            }
        }

        // Check for pushed chord at boundary
        let is_pushed_at_boundary = chord
            .push_pull
            .as_ref()
            .map_or(false, |(is_push, _)| *is_push)
            && is_first_real
            && is_boundary;

        // Check if duplicate
        if should_hide_chord(
            chord,
            current_symbol,
            last_chord_symbol.as_deref(),
            is_pushed_at_boundary,
            ctx.time_signature,
            ctx.hide_repeated_chords,
        ) {
            last_chord_symbol = Some(current_symbol.clone());
            continue;
        }

        // Update tracker
        last_chord_symbol = Some(current_symbol.clone());

        // Calculate segment index
        let segment_idx = calculate_segment_index(
            measure,
            chord_idx,
            chord,
            ctx.segment_positions,
            ctx.internal_push_positions,
            is_first_real,
            is_boundary,
        );

        #[cfg(debug_assertions)]
        eprintln!(
            "[chord-layout] section={} measure={} local={} chord_idx={} '{}' segment_idx={} rhythm_elems={} pushed={} boundary={}",
            ctx.section_name,
            ctx.measure_idx,
            ctx.local_measure_idx,
            chord_idx,
            current_symbol,
            segment_idx,
            measure.rhythm_elements.len(),
            chord.push_pull.is_some(),
            is_pushed_at_boundary
        );

        // Get segment x position
        let segment_x = ctx
            .segment_positions
            .get(segment_idx)
            .copied()
            .unwrap_or_else(|| ctx.segment_positions.first().copied().unwrap_or(0.0));

        let chord_x = ctx.measure_x + segment_x;

        // Create harmony params
        let mut params = super::chord_layout::chord_to_harmony_params(chord, ctx.harmony_style);
        params.position = kurbo::Point::new(chord_x, ctx.chord_y);
        params.id = id_counter;
        id_counter += 1;

        let (_, mut chord_node) = layout_harmony(&params, layout_ctx);

        // Add metadata
        if let Some(page) = ctx.page_number {
            chord_node.set_page(page);
        }
        chord_node.set_system(ctx.global_system_index as u32);
        chord_node.set_measure(ctx.measure_idx as u32);
        chord_node.set_beat(segment_idx as u32);
        chord_node.set_element_type("chord");
        chord_node.set_section_type(ctx.section_name);

        // Chart position for musical coordinates
        let chart_pos = ChartPosition::new(
            ctx.global_system_index as u32,
            ctx.measure_idx as u32,
            chord_idx as u32,
        );
        chord_node.set_json_metadata(metadata_keys::CHART_POSITION, &chart_pos);

        // Source span for click-to-highlight
        if let Some(ref span) = chord.source_span {
            chord_node.set_json_metadata(metadata_keys::SOURCE_SPAN, span);
            let source_link = SourceLink::new(*span, chart_pos.clone());
            chord_node.set_json_metadata(metadata_keys::SOURCE_LINK, &source_link);
        }

        nodes.push(chord_node);
    }

    ChordRenderResult {
        nodes,
        last_chord_symbol,
        next_id: id_counter,
    }
}

/// Render spillback chord symbols (from next measure pushing back).
pub fn render_spillback_chords(
    ctx: &ChordRenderContext<'_>,
    spillbacks: &[PushSpillback],
    previous_chord_symbol: Option<&str>,
    mut id_counter: u64,
    layout_ctx: &LayoutContext<'_>,
) -> ChordRenderResult {
    let mut nodes = Vec::new();
    let mut last_chord_symbol = previous_chord_symbol.map(String::from);

    for spillback in spillbacks {
        // Spillback chords always land at the last segment
        let segment_idx = ctx.segment_positions.len().saturating_sub(1);

        #[cfg(debug_assertions)]
        eprintln!(
            "[spillback-render] section={} measure={} '{}' beat_pos={} segment_idx={} positions_len={}",
            ctx.section_name,
            ctx.measure_idx,
            spillback.chord_symbol,
            spillback.beat_position,
            segment_idx,
            ctx.segment_positions.len()
        );

        let segment_x = ctx
            .segment_positions
            .get(segment_idx)
            .copied()
            .unwrap_or_else(|| ctx.segment_positions.last().copied().unwrap_or(0.0));

        let chord_x = ctx.measure_x + segment_x;

        let mut params = parse_chord(&spillback.chord_symbol);
        params.style = ctx.harmony_style.clone();
        params.position = kurbo::Point::new(chord_x, ctx.chord_y);
        params.id = id_counter;
        id_counter += 1;

        let (_, mut spillback_node) = layout_harmony(&params, layout_ctx);

        // Add metadata
        if let Some(page) = ctx.page_number {
            spillback_node.set_page(page);
        }
        spillback_node.set_system(ctx.global_system_index as u32);
        spillback_node.set_measure(ctx.measure_idx as u32);
        spillback_node.set_element_type("chord");
        spillback_node
            .metadata
            .insert("spillback".to_string(), "true".to_string());
        spillback_node.set_section_type(ctx.section_name);

        // Update tracker for duplicate detection
        last_chord_symbol = Some(spillback.chord_symbol.clone());

        nodes.push(spillback_node);
    }

    ChordRenderResult {
        nodes,
        last_chord_symbol,
        next_id: id_counter,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_placeholder_chord() {
        assert!(is_placeholder_chord(""));
        assert!(is_placeholder_chord("s"));
        assert!(is_placeholder_chord("r"));
        assert!(!is_placeholder_chord("C"));
        assert!(!is_placeholder_chord("Am7"));
    }

    #[test]
    fn test_is_at_boundary() {
        // First measure of section
        assert!(is_at_boundary(0, 0));
        assert!(is_at_boundary(0, 1));

        // First measure of system (not section)
        assert!(is_at_boundary(5, 0));

        // Neither
        assert!(!is_at_boundary(5, 2));
    }
}
