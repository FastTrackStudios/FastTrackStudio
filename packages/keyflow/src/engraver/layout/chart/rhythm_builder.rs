//! Rhythm building utilities for chart layout.
//!
//! This module provides a unified pipeline for converting keyflow rhythm notation
//! to engraver Duration types and building rhythm patterns for measures.
//!
//! ## Pipeline
//!
//! All rhythm building flows through a single entry point:
//! ```text
//! Extract → Fill to Measure → Detect Triplets → Auto-expand
//! ```
//!
//! This consolidates the previous 4+ code paths into one clean API.

use crate::chart::types::{Measure, RhythmElement};
use crate::chord::{ChordRhythm, LilySyntax, PushPullBase};
use crate::engraver::model::DurationKind;
use crate::engraver::notation::{Duration, RhythmEntry, TupletRatio, TupletSpec};

use super::types::MeasureMelodyData;
use super::PushSpillback;

// ============================================================================
// Core Types
// ============================================================================

/// Input source for rhythm - unifies all input types.
///
/// This enum allows the rhythm builder to handle different input sources
/// through a single code path, eliminating branching in the caller.
pub enum RhythmSource<'a> {
    /// Explicit chord rhythms (r8t Ab9_8t r8t...)
    /// These have LilyPond-style duration notation attached to each element.
    ExplicitRhythm(&'a [RhythmElement]),

    /// Preprocessed melody segments from cross-barline expansion.
    /// These come from `expand_melodies_across_measures()`.
    MelodyData(&'a MeasureMelodyData),

    /// Slash/chord notation with optional push/pull timing support.
    /// This is the most common case for chord charts.
    SlashNotation {
        chords: &'a [crate::ChordInstance],
        spillbacks: Option<&'a [PushSpillback]>,
    },
}

/// Configuration for rhythm building.
#[derive(Debug, Clone)]
pub struct RhythmBuildConfig {
    /// Time signature as (numerator, denominator), e.g., (4, 4) for 4/4.
    pub time_signature: (u8, u8),
    /// Whether to use stemmed notation (for charts with push/pull timing).
    pub use_stems: bool,
    /// Whether to expand whole/half notes to quarter slashes for master rhythm charts.
    pub auto_rhythm_slashes: bool,
    /// Whether push/pull notation creates rhythmic subdivisions (triplet groups).
    /// When true, triplet pushes generate quarter+eighth triplet groups.
    /// When false, pushed chords show apostrophe markers instead.
    pub push_alters_rhythm: bool,
}

impl Default for RhythmBuildConfig {
    fn default() -> Self {
        Self {
            time_signature: (4, 4),
            use_stems: false,
            auto_rhythm_slashes: false,
            push_alters_rhythm: true,
        }
    }
}

/// Identifies the type of notehead to use for a rhythm entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteHeadOverride {
    /// Use default notehead for the duration
    Default,
    /// Force slash notehead (for fill slashes after melody)
    Slash,
}

/// Complete output of rhythm building.
///
/// Contains all the information needed to render a measure's rhythm,
/// including entries, tuplet brackets, and position tracking.
#[derive(Debug, Clone)]
pub struct RhythmBuildResult {
    /// The rhythm entries (notes and rests) for this measure.
    pub entries: Vec<RhythmEntry>,
    /// Total duration in ticks.
    pub total_ticks: i32,
    /// Tuplet bracket specifications (for triplet groups, etc.).
    pub tuplet_specs: Vec<TupletSpec>,
    /// Head type overrides for mixed melody/slash notation.
    /// When non-empty, same length as entries - Some(Slash) means force slash head.
    pub head_type_overrides: Vec<Option<NoteHeadOverride>>,
    /// Spillback chord positions: (rhythm_idx, chord_symbol).
    /// These are chords from the next measure that push back into this measure.
    pub spillback_positions: Vec<(usize, String)>,
    /// Internal push positions: (chord_idx, rhythm_idx).
    /// Maps pushed chords to their rhythm entry positions within this measure.
    pub internal_push_positions: Vec<(usize, usize)>,
}

impl Default for RhythmBuildResult {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            total_ticks: 0,
            tuplet_specs: Vec::new(),
            head_type_overrides: Vec::new(),
            spillback_positions: Vec::new(),
            internal_push_positions: Vec::new(),
        }
    }
}

impl RhythmBuildResult {
    /// Returns true if any triplet groups were detected.
    #[must_use]
    pub fn has_triplets(&self) -> bool {
        !self.tuplet_specs.is_empty()
    }

    /// Returns the number of rhythm entries.
    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns true if there are no rhythm entries.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Build rhythm entries for a measure from any source.
///
/// This is the unified entry point for all rhythm building. It handles:
/// - Explicit rhythms (LilyPond notation)
/// - Melody data (cross-barline expanded notes)
/// - Slash notation (chord charts with optional push/pull)
///
/// The pipeline is:
/// 1. Extract base rhythm from source
/// 2. Fill incomplete measures with quarter slashes
/// 3. Detect triplet groups for brackets
/// 4. Optionally expand whole/half notes to quarters
#[must_use]
pub fn build_rhythm(source: RhythmSource<'_>, config: &RhythmBuildConfig) -> RhythmBuildResult {
    // Step 1: Extract base rhythm from source
    let mut result = extract_base_rhythm(source, config);

    // Step 2: Fill incomplete measures with quarter slashes
    fill_to_measure(&mut result, config);

    // Step 3: Detect triplet groups for brackets
    // (ExplicitRhythm and SlashNotation already set tuplet_specs during extraction)
    // This step is mainly for melody data which doesn't have triplets

    // Step 4: Apply auto_rhythm_slashes expansion if enabled
    // This converts whole/half notes to quarters for master rhythm chart style
    if config.auto_rhythm_slashes {
        expand_to_quarters(&mut result);
    }

    result
}

// ============================================================================
// Source-Specific Extractors
// ============================================================================

/// Extract base rhythm from the given source.
fn extract_base_rhythm(source: RhythmSource<'_>, config: &RhythmBuildConfig) -> RhythmBuildResult {
    match source {
        RhythmSource::ExplicitRhythm(elements) => {
            #[cfg(debug_assertions)]
            eprintln!("[rhythm-source] Using ExplicitRhythm ({} elements)", elements.len());
            extract_from_explicit(elements)
        }
        RhythmSource::MelodyData(data) => {
            #[cfg(debug_assertions)]
            eprintln!("[rhythm-source] Using MelodyData");
            extract_from_melody(data)
        }
        RhythmSource::SlashNotation { chords, spillbacks } => {
            #[cfg(debug_assertions)]
            eprintln!(
                "[rhythm-source] Using SlashNotation ({} chords, {} spillbacks)",
                chords.len(),
                spillbacks.map_or(0, |s| s.len())
            );
            extract_from_slash(chords, spillbacks, config)
        }
    }
}

/// Extract rhythm from explicit chord rhythms (r8t Ab9_8t r8t...).
///
/// These have LilyPond-style duration notation attached to each element.
fn extract_from_explicit(elements: &[RhythmElement]) -> RhythmBuildResult {
    let mut result = RhythmBuildResult::default();
    let mut triplet_group_start: Option<usize> = None;
    let mut triplet_group_ticks: i32 = 0;

    // Triplets are grouped by beat: 3 triplet eighths = 480 ticks (one quarter note)
    const TRIPLET_BEAT_TICKS: i32 = 480;

    for (idx, element) in elements.iter().enumerate() {
        let (duration, is_rest, is_triplet) = extract_rhythm_parts(element);

        // Track triplet groups for bracket rendering
        if is_triplet {
            if triplet_group_start.is_none() {
                triplet_group_start = Some(idx);
                triplet_group_ticks = 0;
            }
            triplet_group_ticks += duration.ticks();
        } else if let Some(start) = triplet_group_start {
            // Non-triplet closes the group
            result.tuplet_specs.push(TupletSpec::triplet(start, idx));
            triplet_group_start = None;
            triplet_group_ticks = 0;
        }

        // Create RhythmEntry
        if is_rest {
            result.entries.push(RhythmEntry::Rest(duration));
        } else {
            result.entries.push(RhythmEntry::Note(duration));
        }

        // Close triplet group if we've hit a beat boundary
        if is_triplet && triplet_group_ticks >= TRIPLET_BEAT_TICKS {
            if let Some(start) = triplet_group_start {
                result
                    .tuplet_specs
                    .push(TupletSpec::triplet(start, idx + 1));
                triplet_group_start = None;
                triplet_group_ticks = 0;
            }
        }
    }

    // Close any pending triplet group
    if let Some(start) = triplet_group_start {
        result
            .tuplet_specs
            .push(TupletSpec::triplet(start, result.entries.len()));
    }

    result.total_ticks = result.entries.iter().map(|e| e.duration().ticks()).sum();
    result
}

/// Extract rhythm from preprocessed melody data.
///
/// Melody segments come from `expand_melodies_across_measures()` and represent
/// notes that may have been split at barlines.
fn extract_from_melody(data: &MeasureMelodyData) -> RhythmBuildResult {
    let mut result = RhythmBuildResult::default();
    let num_melody_notes = data.segments.len();

    for segment in &data.segments {
        let duration = segment.to_duration();
        if segment.is_rest {
            result.entries.push(RhythmEntry::Rest(duration));
        } else {
            result.entries.push(RhythmEntry::Note(duration));
        }
        // Head type override will be None for melody notes (use default head)
        result.head_type_overrides.push(None);
    }

    result.total_ticks = result.entries.iter().map(|e| e.duration().ticks()).sum();

    // Store the count of melody notes so fill_to_measure can add Slash overrides
    // We use a marker by ensuring head_type_overrides has exactly num_melody_notes entries
    // (fill_to_measure will extend it with Slash entries for fill notes)
    debug_assert_eq!(result.head_type_overrides.len(), num_melody_notes);

    result
}

/// Extract rhythm from slash/chord notation with push/pull support.
///
/// This handles:
/// - Basic slash notation (one quarter per chord)
/// - Triplet pushes (chord anticipates by triplet eighth)
/// - Spillback chords (pushed chords from next measure)
fn extract_from_slash(
    chords: &[crate::ChordInstance],
    spillbacks: Option<&[PushSpillback]>,
    config: &RhythmBuildConfig,
) -> RhythmBuildResult {
    let mut result = RhythmBuildResult::default();

    // Count non-pushed chords to determine number of beats
    // Pushed chords don't consume their own beat position
    let non_pushed_count = chords
        .iter()
        .filter(|c| c.push_pull.as_ref().is_none_or(|(is_push, _)| !is_push))
        .count();
    let num_beats = non_pushed_count.max(config.time_signature.0 as usize);

    // Build a list of beats, tracking which ones have triplet pushes
    // (has_triplet, pushed_chord_idx)
    let mut beats_with_triplets: Vec<(bool, Option<usize>)> = vec![(false, None); num_beats];

    // Calculate natural beat positions and mark triplet pushes
    let mut cumulative_beats = 0usize;
    for (chord_idx, chord) in chords.iter().enumerate() {
        let is_triplet_push = chord.push_pull.as_ref().is_some_and(|(is_push, amount)| {
            *is_push && amount.base == PushPullBase::Triplet && amount.level == 1
        });

        let chord_duration_beats = match &chord.rhythm {
            ChordRhythm::Slashes { count, .. } => *count as usize,
            _ => 1,
        };

        // Triplet push affects the PREVIOUS beat (where the anticipation lands)
        if is_triplet_push && chord_idx > 0 {
            let target_beat = cumulative_beats.saturating_sub(1);
            if target_beat < num_beats {
                beats_with_triplets[target_beat] = (true, Some(chord_idx));
            }
        }

        cumulative_beats += chord_duration_beats;
    }

    // Check spillbacks from next measure
    if let Some(spills) = spillbacks {
        #[cfg(debug_assertions)]
        if !spills.is_empty() {
            eprintln!(
                "[rhythm-builder] Processing {} spillbacks, num_beats={}, push_alters_rhythm={}",
                spills.len(),
                num_beats,
                config.push_alters_rhythm
            );
        }
        for spillback in spills {
            #[cfg(debug_assertions)]
            eprintln!(
                "[rhythm-builder] spillback: chord='{}' beat={} base={:?} level={}",
                spillback.chord_symbol,
                spillback.beat_position,
                spillback.push_base,
                spillback.push_level
            );
            if spillback.push_base == PushPullBase::Triplet && spillback.push_level == 1 {
                let target_beat = spillback.beat_position;
                if target_beat < num_beats && !beats_with_triplets[target_beat].0 {
                    // Mark as triplet but no internal chord (it's from next measure)
                    beats_with_triplets[target_beat] = (true, None);
                    #[cfg(debug_assertions)]
                    eprintln!("[rhythm-builder] Marked beat {} as triplet from spillback", target_beat);
                }
            }
        }
    }

    // Build the rhythm array
    let mut rhythm_index = 0;
    for (beat_idx, (has_triplet, pushed_chord_idx)) in
        beats_with_triplets.iter().enumerate().take(num_beats)
    {
        if *has_triplet && config.push_alters_rhythm {
            // Triplet beat: [TripletQuarter, TripletEighth]
            let start_idx = rhythm_index;
            result
                .entries
                .push(RhythmEntry::Note(Duration::TripletQuarter));
            result
                .entries
                .push(RhythmEntry::Note(Duration::TripletEighth));

            // Track internal push position
            if let Some(chord_idx) = pushed_chord_idx {
                result
                    .internal_push_positions
                    .push((*chord_idx, rhythm_index + 1));
            }

            // Track spillback chord position
            if let Some(spills) = spillbacks {
                if let Some(spillback) = spills.iter().find(|s| s.beat_position == beat_idx) {
                    result
                        .spillback_positions
                        .push((rhythm_index + 1, spillback.chord_symbol.clone()));
                }
            }

            rhythm_index += 2;
            result
                .tuplet_specs
                .push(TupletSpec::triplet(start_idx, rhythm_index));
        } else {
            // Standard quarter note beat
            // Check for standard (non-triplet) spillbacks
            if let Some(spills) = spillbacks {
                if let Some(spillback) = spills
                    .iter()
                    .find(|s| s.beat_position == beat_idx && s.push_base == PushPullBase::Standard)
                {
                    result
                        .spillback_positions
                        .push((rhythm_index, spillback.chord_symbol.clone()));
                }
            }

            result.entries.push(RhythmEntry::Note(Duration::Quarter));
            rhythm_index += 1;
        }
    }

    result.total_ticks = result.entries.iter().map(|e| e.duration().ticks()).sum();
    result
}

// ============================================================================
// Pipeline Stages
// ============================================================================

/// Fill incomplete measures with quarter slashes to match time signature.
fn fill_to_measure(result: &mut RhythmBuildResult, config: &RhythmBuildConfig) {
    let measure_ticks = calculate_measure_ticks(config.time_signature);
    let remaining = measure_ticks - result.total_ticks;

    if remaining > 0 {
        let quarter_ticks = 480;
        let num_quarters = remaining / quarter_ticks;

        // Track if we have head_type_overrides (from melody extraction)
        let had_overrides = !result.head_type_overrides.is_empty();

        for _ in 0..num_quarters {
            result.entries.push(RhythmEntry::Note(Duration::Quarter));

            // If we had head type overrides, add Slash for fill notes
            if had_overrides {
                result
                    .head_type_overrides
                    .push(Some(NoteHeadOverride::Slash));
            }
        }

        result.total_ticks = measure_ticks;
    }
}

/// Expand whole/half notes to quarter slashes for master rhythm chart style.
///
/// This converts sustained chord notation (diamonds) to rhythmic slashes.
/// Rests are preserved as-is. Triplet entries (eighths, etc.) are NOT expanded
/// and their tuplet_specs are preserved with remapped indices.
fn expand_to_quarters(result: &mut RhythmBuildResult) {
    let quarter_ticks = 480;
    let mut expanded = Vec::with_capacity(result.entries.len() * 4);
    let mut expanded_overrides = if result.head_type_overrides.is_empty() {
        Vec::new()
    } else {
        Vec::with_capacity(result.entries.len() * 4)
    };

    // Track index mapping: old_index -> new_start_index
    // Used to remap tuplet_specs after expansion
    let mut index_map: Vec<usize> = Vec::with_capacity(result.entries.len());

    for (i, entry) in result.entries.iter().enumerate() {
        // Record the new index for this original entry
        index_map.push(expanded.len());

        match entry {
            RhythmEntry::Note(dur) => {
                let ticks = dur.ticks();

                if ticks >= quarter_ticks * 2 {
                    // Expand to quarters
                    let num_quarters = ticks / quarter_ticks;
                    let original_override = result.head_type_overrides.get(i).copied().flatten();

                    for _ in 0..num_quarters {
                        expanded.push(RhythmEntry::Note(Duration::Quarter));
                        if !result.head_type_overrides.is_empty() {
                            expanded_overrides.push(original_override);
                        }
                    }

                    // Handle remaining ticks (e.g., dotted rhythms)
                    let remaining = ticks % quarter_ticks;
                    if remaining >= Duration::Eighth.ticks() {
                        expanded.push(RhythmEntry::Note(Duration::Eighth));
                        if !result.head_type_overrides.is_empty() {
                            expanded_overrides.push(original_override);
                        }
                    }
                } else {
                    // Quarter notes and shorter stay as-is
                    expanded.push(entry.clone());
                    if !result.head_type_overrides.is_empty() {
                        expanded_overrides
                            .push(result.head_type_overrides.get(i).copied().flatten());
                    }
                }
            }
            RhythmEntry::Rest(dur) => {
                // Keep rests as-is (don't expand sustained rests)
                expanded.push(RhythmEntry::Rest(*dur));
                if !result.head_type_overrides.is_empty() {
                    expanded_overrides.push(result.head_type_overrides.get(i).copied().flatten());
                }
            }
        }
    }

    result.entries = expanded;
    result.head_type_overrides = expanded_overrides;
    result.total_ticks = result.entries.iter().map(|e| e.duration().ticks()).sum();

    // Remap tuplet_specs indices using the index mapping
    // This preserves triplet brackets for entries that weren't expanded
    for spec in &mut result.tuplet_specs {
        // Remap start_idx
        if spec.start_idx < index_map.len() {
            spec.start_idx = index_map[spec.start_idx];
        }
        // Remap end_idx (exclusive) - use the mapped index of end_idx or expanded.len() if at end
        if spec.end_idx <= index_map.len() {
            spec.end_idx = if spec.end_idx == index_map.len() {
                result.entries.len()
            } else {
                index_map[spec.end_idx]
            };
        }
    }
}

/// Calculate measure duration in ticks based on time signature.
#[must_use]
pub fn calculate_measure_ticks(time_signature: (u8, u8)) -> i32 {
    let beat_ticks = match time_signature.1 {
        2 => 960, // Half note beat
        4 => 480, // Quarter note beat
        8 => 240, // Eighth note beat
        _ => 480, // Default to quarter
    };
    beat_ticks * i32::from(time_signature.0)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Convert keyflow LilySyntax to engraver Duration.
///
/// This is a standalone function that can be used without a ChartLayoutEngine instance.
/// It applies dotted and triplet modifiers to create the final Duration.
#[must_use]
pub fn lily_syntax_to_duration(lily: LilySyntax, dotted: bool, triplet: bool) -> Duration {
    // Convert LilySyntax to DurationKind via NoteValue
    let note_value = crate::core::NoteValue::from(lily);
    let kind = DurationKind::from(note_value);

    if triplet {
        if dotted {
            // Dotted triplet
            Duration {
                kind,
                dots: 1,
                tuplet: Some(TupletRatio::triplet()),
            }
        } else {
            Duration::triplet(kind)
        }
    } else if dotted {
        Duration::dotted(kind)
    } else {
        Duration::new(kind)
    }
}

/// Extract rhythm parts from a RhythmElement.
///
/// Returns (duration, is_rest, is_triplet)
fn extract_rhythm_parts(element: &RhythmElement) -> (Duration, bool, bool) {
    match element {
        RhythmElement::Chord(chord) => {
            if let Some((lily, dotted, triplet)) = chord.rhythm.lily_parts() {
                let dur = lily_syntax_to_duration(lily, dotted, triplet);
                (dur, false, triplet)
            } else {
                (Duration::Quarter, false, false)
            }
        }
        RhythmElement::Rest(rest) => {
            if let Some((lily, dotted, triplet)) = rest.rhythm.lily_parts() {
                let dur = lily_syntax_to_duration(lily, dotted, triplet);
                (dur, true, triplet)
            } else {
                (Duration::Quarter, true, false)
            }
        }
        RhythmElement::Space(_) => (Duration::Quarter, true, false),
    }
}

/// Check if a measure has explicit chord rhythms (Lily or Rest notation).
///
/// When chords have explicit rhythms like `r8t Ab9_8t r8t r4t F9_8t r2`,
/// we should render those rhythms instead of using slash notation.
///
/// Note: `Space` (s1, s2, etc.) does NOT count as explicit rhythm - it means
/// "fill this measure with automatic slashes".
#[must_use]
pub fn measure_has_explicit_chord_rhythm(measure: &Measure) -> bool {
    // Check rhythm_elements first (preferred - contains both chords and rests in order)
    if !measure.rhythm_elements.is_empty() {
        let has_real_rhythm = measure.rhythm_elements.iter().any(|elem| {
            match elem {
                RhythmElement::Chord(chord) => {
                    // Only Explicit durations count as explicit rhythm
                    // Skip pushed first chords (they spill back to previous measure)
                    let is_pushed_first = chord
                        .push_pull
                        .as_ref()
                        .map_or(false, |(is_push, _)| *is_push);
                    !is_pushed_first && chord.rhythm.has_lily_duration()
                }
                RhythmElement::Rest(_) => true, // Rests count as real rhythm
                RhythmElement::Space(_) => false, // Space triggers auto-fill
            }
        });
        if has_real_rhythm {
            return true;
        }
    }

    // Fallback: check chords for explicit rhythms
    measure.chords.iter().enumerate().any(|(idx, chord)| {
        let is_pushed_first = idx == 0
            && chord
                .push_pull
                .as_ref()
                .map_or(false, |(is_push, _)| *is_push);
        !is_pushed_first && (chord.rhythm.has_lily_duration() || chord.rhythm.is_rest())
    })
}

// ============================================================================
// Legacy API (Deprecated)
// ============================================================================

/// Estimate the number of rhythm elements and triplets that will be generated
/// for a measure after push/pull expansion.
///
/// # Deprecated
/// This function is maintained for backward compatibility. New code should use
/// `build_rhythm()` and inspect the result directly.
#[deprecated(
    since = "0.1.0",
    note = "Use build_rhythm() and inspect result.len() instead"
)]
#[must_use]
pub fn estimate_expanded_rhythm_counts(measure: &Measure, num_beats: usize) -> (usize, usize) {
    // If measure has explicit rhythm, count from rhythm_elements
    if measure_has_explicit_chord_rhythm(measure) {
        let mut element_count = 0;
        let mut triplet_count = 0;

        for elem in &measure.rhythm_elements {
            element_count += 1;
            let is_triplet = match elem {
                RhythmElement::Chord(c) => c.rhythm.lily_parts().is_some_and(|(_, _, t)| t),
                RhythmElement::Rest(r) => r.rhythm.lily_parts().is_some_and(|(_, _, t)| t),
                RhythmElement::Space(_) => false,
            };
            if is_triplet {
                triplet_count += 1;
            }
        }

        return (element_count, triplet_count);
    }

    // For slash measures, simulate the expansion
    let mut beats_with_triplets = vec![false; num_beats];
    let mut cumulative_beats = 0usize;

    for (chord_idx, chord) in measure.chords.iter().enumerate() {
        let is_triplet_push = chord.push_pull.as_ref().is_some_and(|(is_push, amount)| {
            *is_push && amount.base == PushPullBase::Triplet && amount.level == 1
        });

        let chord_duration_beats = match &chord.rhythm {
            ChordRhythm::Slashes { count, .. } => *count as usize,
            _ => 1,
        };

        if is_triplet_push && chord_idx > 0 {
            let target_beat = cumulative_beats.saturating_sub(1);
            if target_beat < num_beats {
                beats_with_triplets[target_beat] = true;
            }
        }

        cumulative_beats += chord_duration_beats;
    }

    // Count elements: regular beats = 1 element, triplet beats = 2 elements
    let triplet_beats = beats_with_triplets.iter().filter(|&&b| b).count();
    let regular_beats = num_beats - triplet_beats;

    let element_count = regular_beats + (triplet_beats * 2);
    let triplet_element_count = triplet_beats * 2;

    (element_count, triplet_element_count)
}

/// Build rhythm entries from explicit chord rhythms.
///
/// # Deprecated
/// This function is maintained for backward compatibility. New code should use
/// `build_rhythm(RhythmSource::ExplicitRhythm(...), config)` instead.
#[deprecated(
    since = "0.1.0",
    note = "Use build_rhythm() with RhythmSource::ExplicitRhythm"
)]
#[must_use]
pub fn build_rhythm_from_chord_rhythms(
    measure: &Measure,
) -> (Vec<RhythmEntry>, i32, Vec<TupletSpec>) {
    let result = extract_from_explicit(&measure.rhythm_elements);
    (result.entries, result.total_ticks, result.tuplet_specs)
}

/// Build rhythm pattern with triplet support for pushed chords.
///
/// # Deprecated
/// This function is maintained for backward compatibility. New code should use
/// `build_rhythm(RhythmSource::SlashNotation { ... }, config)` instead.
#[deprecated(
    since = "0.1.0",
    note = "Use build_rhythm() with RhythmSource::SlashNotation"
)]
#[allow(clippy::type_complexity)]
#[must_use]
pub fn build_rhythm_with_triplets(
    measure: &Measure,
    num_beats: usize,
    spillbacks: Option<&[PushSpillback]>,
) -> (
    Vec<Duration>,
    i32,
    Vec<TupletSpec>,
    Vec<(usize, String)>,
    Vec<(usize, usize)>,
) {
    // Use the new unified system with use_stems=true
    let config = RhythmBuildConfig {
        time_signature: (num_beats as u8, 4),
        use_stems: true,
        auto_rhythm_slashes: false,
        push_alters_rhythm: true,
    };
    let source = RhythmSource::SlashNotation {
        chords: &measure.chords,
        spillbacks,
    };
    let result = extract_from_slash(&measure.chords, spillbacks, &config);

    // Convert entries back to Duration vec for legacy API
    let rhythm: Vec<Duration> = result
        .entries
        .iter()
        .map(|e| match e {
            RhythmEntry::Note(d) | RhythmEntry::Rest(d) => *d,
        })
        .collect();

    (
        rhythm,
        result.total_ticks,
        result.tuplet_specs,
        result.spillback_positions,
        result.internal_push_positions,
    )
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lily_syntax_to_duration_basic() {
        let dur = lily_syntax_to_duration(LilySyntax::Quarter, false, false);
        assert_eq!(dur.kind, DurationKind::Quarter);
        assert_eq!(dur.dots, 0);
        assert!(dur.tuplet.is_none());
    }

    #[test]
    fn test_lily_syntax_to_duration_dotted() {
        let dur = lily_syntax_to_duration(LilySyntax::Half, true, false);
        assert_eq!(dur.kind, DurationKind::Half);
        assert_eq!(dur.dots, 1);
        assert!(dur.tuplet.is_none());
    }

    #[test]
    fn test_lily_syntax_to_duration_triplet() {
        let dur = lily_syntax_to_duration(LilySyntax::Eighth, false, true);
        assert_eq!(dur.kind, DurationKind::Eighth);
        assert_eq!(dur.dots, 0);
        assert!(dur.tuplet.is_some());
    }

    #[test]
    fn test_lily_syntax_to_duration_all_note_values() {
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Whole, false, false).kind,
            DurationKind::Whole
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Half, false, false).kind,
            DurationKind::Half
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Quarter, false, false).kind,
            DurationKind::Quarter
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Eighth, false, false).kind,
            DurationKind::Eighth
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::Sixteenth, false, false).kind,
            DurationKind::Sixteenth
        );
        assert_eq!(
            lily_syntax_to_duration(LilySyntax::ThirtySecond, false, false).kind,
            DurationKind::ThirtySecond
        );
    }

    #[test]
    fn test_calculate_measure_ticks() {
        // 4/4 time = 4 * 480 = 1920 ticks
        assert_eq!(calculate_measure_ticks((4, 4)), 1920);
        // 3/4 time = 3 * 480 = 1440 ticks
        assert_eq!(calculate_measure_ticks((3, 4)), 1440);
        // 6/8 time = 6 * 240 = 1440 ticks
        assert_eq!(calculate_measure_ticks((6, 8)), 1440);
        // 2/2 time = 2 * 960 = 1920 ticks
        assert_eq!(calculate_measure_ticks((2, 2)), 1920);
    }

    #[test]
    fn test_build_rhythm_result_helpers() {
        let result = RhythmBuildResult::default();
        assert!(result.is_empty());
        assert_eq!(result.len(), 0);
        assert!(!result.has_triplets());
    }

    #[test]
    fn test_fill_to_measure_4_4() {
        let config = RhythmBuildConfig {
            time_signature: (4, 4),
            use_stems: false,
            auto_rhythm_slashes: false,
            push_alters_rhythm: true,
        };

        // Start with 2 quarter notes (960 ticks)
        let mut result = RhythmBuildResult {
            entries: vec![
                RhythmEntry::Note(Duration::Quarter),
                RhythmEntry::Note(Duration::Quarter),
            ],
            total_ticks: 960,
            ..Default::default()
        };

        fill_to_measure(&mut result, &config);

        // Should have filled to 4 quarters
        assert_eq!(result.entries.len(), 4);
        assert_eq!(result.total_ticks, 1920);
    }
}
