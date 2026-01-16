//! Builders for automatic music notation layout.

use kurbo::{Affine, Point, Rect};
use vello::peniko::Color;

use crate::layout::context::LayoutContext;
use crate::layout::segment::{Segment, SegmentType};
use crate::layout::segment_list::SegmentList;
use crate::layout::spacing::HorizontalSpacing;
use crate::layout::tlayout::{
    layout_barline, layout_beam, layout_chord, layout_clef, layout_note, layout_timesig,
    Accidental, BarlineParams, BarlineType, BeamLayoutConfig, BeamNote, ChordNote, ChordParams,
    ClefParams, ClefType, NoteHeadType, NoteParams, StemDirection, TimeSigParams, TimeSigType,
};
use crate::scene::id::{ElementType, SemanticId};
use crate::scene::node::SceneNode;
use crate::scene::paint::PaintCommand;

use super::mode::NotationMode;
use super::{Duration, TimeSignature};

/// Builder for creating a single measure of music with automatic spacing.
#[derive(Debug, Clone)]
pub struct MeasureBuilder {
    /// Clef type (None = no clef displayed)
    clef: Option<ClefType>,
    /// Time signature (None = no time sig displayed)
    time_signature: Option<TimeSignature>,
    /// Notation mode (Standard, Rhythmic, etc.)
    mode: NotationMode,
    /// Rhythm pattern (list of durations)
    rhythm: Vec<Duration>,
    /// Per-note head type overrides (index -> head type).
    /// When set, overrides the mode's default head type for specific notes.
    head_type_overrides: Vec<Option<NoteHeadType>>,
    /// Starting barline type
    start_barline: Option<BarlineType>,
    /// Ending barline type
    end_barline: Option<BarlineType>,
    /// Staff width in spatiums (for justification)
    width: Option<f64>,
    /// Whether to justify (stretch to fill width)
    justify: bool,
    /// Unique ID base for elements
    id_base: u64,
    /// Whether notes should be stemless
    stemless: bool,
}

impl Default for MeasureBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl MeasureBuilder {
    /// Create a new measure builder.
    #[must_use]
    pub fn new() -> Self {
        Self {
            clef: None,
            time_signature: None,
            mode: NotationMode::Standard,
            rhythm: Vec::new(),
            head_type_overrides: Vec::new(),
            start_barline: None,
            end_barline: Some(BarlineType::Single),
            width: None,
            justify: false,
            id_base: 1,
            stemless: false,
        }
    }

    /// Set the clef.
    #[must_use]
    pub fn clef(mut self, clef: ClefType) -> Self {
        self.clef = Some(clef);
        self
    }

    /// Set the time signature.
    #[must_use]
    pub fn time_signature(mut self, numerator: u8, denominator: u8) -> Self {
        self.time_signature = Some(TimeSignature::new(numerator, denominator));
        self
    }

    /// Set the time signature from a TimeSignature struct.
    #[must_use]
    pub fn time_sig(mut self, ts: TimeSignature) -> Self {
        self.time_signature = Some(ts);
        self
    }

    /// Set the notation mode.
    #[must_use]
    pub fn mode(mut self, mode: NotationMode) -> Self {
        self.mode = mode;
        self
    }

    /// Set to rhythmic (slash) notation mode.
    #[must_use]
    pub fn rhythmic(mut self) -> Self {
        self.mode = NotationMode::Rhythmic;
        self
    }

    /// Set notes to be stemless (noteheads only, no stems).
    #[must_use]
    pub fn stemless(mut self) -> Self {
        self.stemless = true;
        self
    }

    /// Enable automatic stemless detection for rhythmic notation.
    ///
    /// In rhythmic/slash notation, consecutive quarter notes (2 or more)
    /// should be displayed without stems for cleaner chord chart appearance.
    /// This follows the LilyPond convention where groups of identical
    /// quarter note slashes are stemless.
    #[must_use]
    pub fn auto_stemless(mut self) -> Self {
        // Only applies to rhythmic mode
        if matches!(self.mode, NotationMode::Rhythmic) {
            self.stemless = false; // Will be computed per-note
        }
        self
    }

    /// Compute which notes should be stemless based on consecutive quarter note analysis.
    ///
    /// Returns a Vec<bool> where true means the note at that index should be stemless.
    ///
    /// The algorithm (based on LilyPond convention):
    /// 1. Consecutive quarter notes (no dots) are candidates for stemless
    /// 2. Non-quarter notes break the consecutive chain
    /// 3. Strong beats also break the chain (in 4/4, beats 1 and 3 are strong)
    /// 4. Groups of 2+ consecutive quarters within the same beat-group = stemless
    ///
    /// For example in 4/4 with quarters on beats 2, 3, 4:
    /// - Beat 2 is alone before the strong beat 3 -> has stem
    /// - Beats 3-4 are consecutive after strong beat 3 -> stemless
    fn compute_auto_stemless(&self) -> Vec<bool> {
        let mut result = vec![false; self.rhythm.len()];

        // Only apply in rhythmic mode
        if !matches!(self.mode, NotationMode::Rhythmic) {
            return result;
        }

        // Get time signature info for strong beats
        let ts = self.time_signature.unwrap_or(TimeSignature::COMMON);
        let (beats_per_measure, beat_unit) = (ts.numerator, ts.denominator);
        let ticks_per_beat = match beat_unit {
            4 => 480,  // Quarter note = 480 ticks
            8 => 240,  // Eighth note = 240 ticks
            2 => 960,  // Half note = 960 ticks
            _ => 480,
        };

        // Determine strong beat positions (in ticks)
        // In 4/4: beats 1 and 3 are strong (ticks 0 and 960)
        // In 3/4: only beat 1 is strong (tick 0)
        // In 2/4: only beat 1 is strong (tick 0)
        // In 6/8: beats 1 and 4 are strong (ticks 0 and 720)
        let strong_beat_ticks: Vec<i32> = if beats_per_measure == 4 && beat_unit == 4 {
            vec![0, ticks_per_beat * 2] // Beats 1 and 3
        } else if beats_per_measure == 6 && beat_unit == 8 {
            vec![0, ticks_per_beat * 3] // Beats 1 and 4
        } else {
            vec![0] // Only beat 1 is strong
        };

        // Track tick position and consecutive quarter groups
        let mut current_tick: i32 = 0;
        let mut consecutive_quarters: Vec<usize> = Vec::new();
        let mut last_strong_beat_crossed: i32 = -1;

        // Helper to mark a group as stemless if it has 2+ members
        let mark_group = |result: &mut Vec<bool>, group: &[usize]| {
            if group.len() >= 2 {
                for &idx in group {
                    result[idx] = true;
                }
            }
        };

        for (i, dur) in self.rhythm.iter().enumerate() {
            let note_tick = current_tick;
            let is_quarter = matches!(dur, Duration::Quarter) && dur.dots() == 0;

            // Check if we've crossed a strong beat since the last quarter
            let crossed_strong_beat = strong_beat_ticks.iter().any(|&sb| {
                sb > 0 && sb > last_strong_beat_crossed && sb <= note_tick
            });

            if is_quarter {
                // If we crossed a strong beat, finalize the previous group
                if crossed_strong_beat && !consecutive_quarters.is_empty() {
                    mark_group(&mut result, &consecutive_quarters);
                    consecutive_quarters.clear();
                }

                consecutive_quarters.push(i);

                // Update last strong beat if we're on one
                if strong_beat_ticks.contains(&note_tick) {
                    last_strong_beat_crossed = note_tick;
                }
            } else {
                // Non-quarter note: finalize the current group
                mark_group(&mut result, &consecutive_quarters);
                consecutive_quarters.clear();
            }

            current_tick += dur.ticks();
        }

        // Handle any remaining consecutive quarters at end of measure
        mark_group(&mut result, &consecutive_quarters);

        result
    }

    /// Set the rhythm pattern.
    #[must_use]
    pub fn rhythm(mut self, rhythm: Vec<Duration>) -> Self {
        self.rhythm = rhythm;
        self
    }

    /// Add a single duration to the rhythm.
    #[must_use]
    pub fn add(mut self, duration: Duration) -> Self {
        self.rhythm.push(duration);
        self
    }

    /// Set per-note head type overrides.
    ///
    /// Each entry corresponds to a note in the rhythm array by index.
    /// `Some(head_type)` overrides the mode's default, `None` uses the mode's default.
    ///
    /// This allows mixing standard noteheads with slash noteheads in the same measure,
    /// useful for showing melody notes alongside rhythm slashes.
    #[must_use]
    pub fn head_type_overrides(mut self, overrides: Vec<Option<NoteHeadType>>) -> Self {
        self.head_type_overrides = overrides;
        self
    }

    /// Set the starting barline.
    #[must_use]
    pub fn start_barline(mut self, barline: BarlineType) -> Self {
        self.start_barline = Some(barline);
        self
    }

    /// Set the ending barline.
    #[must_use]
    pub fn end_barline(mut self, barline: BarlineType) -> Self {
        self.end_barline = Some(barline);
        self
    }

    /// Disable all barlines (for when barlines are handled externally).
    #[must_use]
    pub fn no_barlines(mut self) -> Self {
        self.start_barline = None;
        self.end_barline = None;
        self
    }

    /// Set the target width and enable justification.
    #[must_use]
    pub fn justify_to(mut self, width_spatiums: f64) -> Self {
        self.width = Some(width_spatiums);
        self.justify = true;
        self
    }

    /// Set the ID base for generated elements.
    #[must_use]
    pub fn id_base(mut self, base: u64) -> Self {
        self.id_base = base;
        self
    }

    /// Build the measure scene with automatic spacing.
    #[must_use]
    pub fn build(self, ctx: &LayoutContext) -> MeasureScene {
        let spatium = ctx.spatium();
        let mut segment_vec: Vec<Segment> = Vec::new(); // Use Vec first, convert to SegmentList after sorting
        let mut scene_elements: Vec<SceneElement> = Vec::new();
        let mut current_tick: i32 = 0;
        let mut id = self.id_base;

        // Compute auto-stemless flags for rhythmic notation
        // If explicit stemless is set, all notes are stemless
        // Otherwise, compute based on consecutive quarter note analysis
        let auto_stemless_flags = if self.stemless {
            vec![true; self.rhythm.len()]
        } else {
            self.compute_auto_stemless()
        };
        let mut rhythm_index: usize = 0;

        // Get mode-specific settings
        let default_head_type = self.mode.notehead_type();
        let stem_dir = self.mode.default_stem_direction();
        let note_line = self.mode.default_line();

        // Helper to get head type for a specific note index, using override if available
        let get_head_type = |idx: usize| -> NoteHeadType {
            self.head_type_overrides
                .get(idx)
                .and_then(|opt| *opt)
                .unwrap_or(default_head_type)
        };

        // 1. Add clef segment
        if let Some(clef_type) = self.clef {
            let mut seg = Segment::clef(current_tick);
            seg.min_width = spatium * 4.0; // Approximate clef width (use min_width so spacing respects it)
            segment_vec.push(seg);

            let (_, clef_node) = layout_clef(
                &ClefParams {
                    id,
                    clef_type,
                    ..Default::default()
                },
                ctx,
            );
            scene_elements.push(SceneElement::Clef { id, node: clef_node });
            id += 1;
        }

        // 2. Add time signature segment
        if let Some(ts) = self.time_signature {
            let mut seg = Segment::time_sig(current_tick);
            seg.min_width = spatium * 3.0; // Approximate time sig width (use min_width so spacing respects it)
            segment_vec.push(seg);

            let (_, ts_node) = layout_timesig(
                &TimeSigParams {
                    id,
                    sig_type: TimeSigType::Numeric {
                        numerator: ts.numerator,
                        denominator: ts.denominator,
                    },
                    ..Default::default()
                },
                ctx,
            );
            scene_elements.push(SceneElement::TimeSignature { id, node: ts_node });
            id += 1;
        }

        // 3. Add start barline if specified
        if let Some(barline_type) = self.start_barline {
            let mut seg = Segment::barline(current_tick);
            seg.min_width = spatium * 1.0; // Use min_width so spacing respects it
            segment_vec.push(seg);

            let (_, bl_node) = layout_barline(
                &BarlineParams {
                    id,
                    barline_type,
                    ..Default::default()
                },
                ctx,
            );
            scene_elements.push(SceneElement::Barline { id, node: bl_node });
            id += 1;
        }

        // 4. Group rhythm into beam groups based on time signature
        let beam_groups = self.compute_beam_groups();

        // 5. Add chord/rest segments for each note
        for group in &beam_groups {
            if group.notes.len() == 1 {
                // Single note - use chord layout (handles both flagged and non-flagged)
                // Note: layout_beam returns empty for single notes, so we must use chord layout
                let dur = group.notes[0];
                let mut seg = Segment::chord_rest(current_tick, dur.ticks());
                seg.ticks = dur.ticks();
                segment_vec.push(seg);

                // Get stemless flag for this note from auto-computed or explicit stemless
                let note_stemless = auto_stemless_flags
                    .get(rhythm_index)
                    .copied()
                    .unwrap_or(false);

                let note_head_type = get_head_type(rhythm_index);
                let (_, chord_node) = layout_chord(
                    &ChordParams {
                        id,
                        duration: dur.to_note_duration(),
                        head_type: note_head_type,
                        notes: vec![ChordNote {
                            line: note_line,
                            accidental: Accidental::None,
                            tie: false,
                        }],
                        stem_direction: stem_dir,
                        dots: dur.dots(),
                        beamed: false,
                        stemless: note_stemless,
                    },
                    ctx,
                );

                scene_elements.push(SceneElement::Chord {
                    id,
                    node: chord_node,
                    tick: current_tick,
                });

                current_tick += dur.ticks();
                rhythm_index += 1;
                id += 1;
            } else if group.notes.len() >= 2 && group.notes.iter().any(|d| d.needs_flag()) {
                // Beamed group (2+ notes with at least one flagged) - create beam notes
                let group_start_tick = current_tick;
                let mut beam_notes: Vec<BeamNoteInfo> = Vec::new();
                // Use first note's head type for the whole beam group
                let beam_head_type = get_head_type(rhythm_index);

                for dur in &group.notes {
                    let mut seg = Segment::chord_rest(current_tick, dur.ticks());
                    seg.ticks = dur.ticks();
                    segment_vec.push(seg);

                    // Store info for beam layout (x position will be set after spacing)
                    beam_notes.push(BeamNoteInfo {
                        id,
                        tick: current_tick,
                        duration: *dur,
                    });

                    current_tick += dur.ticks();
                    rhythm_index += 1;
                    id += 1;
                }

                scene_elements.push(SceneElement::BeamGroup {
                    start_tick: group_start_tick,
                    notes: beam_notes,
                    head_type: beam_head_type,
                    stem_dir,
                    note_line,
                });
            } else {
                // Multiple non-flagged notes - individual chords
                for dur in &group.notes {
                    let mut seg = Segment::chord_rest(current_tick, dur.ticks());
                    seg.ticks = dur.ticks();
                    segment_vec.push(seg);

                    // Get stemless flag for this note from auto-computed or explicit stemless
                    let note_stemless = auto_stemless_flags
                        .get(rhythm_index)
                        .copied()
                        .unwrap_or(false);

                    let note_head_type = get_head_type(rhythm_index);
                    let (_, chord_node) = layout_chord(
                        &ChordParams {
                            id,
                            duration: dur.to_note_duration(),
                            head_type: note_head_type,
                            notes: vec![ChordNote {
                                line: note_line,
                                accidental: Accidental::None,
                                tie: false,
                            }],
                            stem_direction: stem_dir,
                            dots: dur.dots(),
                            beamed: false,
                            stemless: note_stemless,
                        },
                        ctx,
                    );

                    scene_elements.push(SceneElement::Chord {
                        id,
                        node: chord_node,
                        tick: current_tick,
                    });

                    current_tick += dur.ticks();
                    rhythm_index += 1;
                    id += 1;
                }
            }
        }

        // 6. Add end barline
        if let Some(barline_type) = self.end_barline {
            let mut seg = Segment::end_barline(current_tick);
            seg.min_width = spatium * 1.0; // Use min_width so spacing respects it
            segment_vec.push(seg);

            let (_, bl_node) = layout_barline(
                &BarlineParams {
                    id,
                    barline_type,
                    ..Default::default()
                },
                ctx,
            );
            scene_elements.push(SceneElement::Barline { id, node: bl_node });
        }

        // 7. Sort segments by tick and type, then convert to SegmentList
        segment_vec.sort();
        let mut segments = SegmentList::from_sorted(segment_vec);

        // 8. Apply bar-note distance (leading space) to first chord/rest segment
        // and note-bar distance (trailing space) to target width
        let bar_note_distance = ctx.style_distance(crate::style::Sid::BarNoteDistance);
        let note_bar_distance = ctx.style_distance(crate::style::Sid::NoteBarDistance);

        // Find first ChordRest segment and add leading space
        if let Some(first_chord) = segments.iter_mut().find(|s| s.seg_type.is_chord_rest()) {
            first_chord.extra_leading_space = bar_note_distance;
        }

        // 9. Apply horizontal spacing
        // Account for bar margins when justifying
        let spacing = HorizontalSpacing::new(spatium);
        let target_width = self.width.map(|w| {
            let full_width = w * spatium;
            // When justifying, the available space is reduced by the trailing margin
            if self.justify {
                full_width - note_bar_distance
            } else {
                full_width
            }
        }).unwrap_or(f64::MAX);
        let spacing_result = spacing.compute_spacing(&mut segments, target_width, self.justify);

        // 9. Build final scene with computed positions
        let scene = self.build_scene(ctx, &segments, &scene_elements);

        MeasureScene {
            scene,
            width: spacing_result.total_width,
            segments,
        }
    }

    /// Compute beam groups based on beat boundaries.
    ///
    /// Rules:
    /// 1. Non-flagged notes (quarter and longer) are never beamed
    /// 2. Flagged notes (8ths, 16ths, 32nds) are grouped within beats
    /// 3. Beam groups never cross beat boundaries
    /// 4. Within a beat, all consecutive flagged notes are beamed together
    fn compute_beam_groups(&self) -> Vec<BeamGroup> {
        if self.rhythm.is_empty() {
            return Vec::new();
        }

        let ts = self.time_signature.unwrap_or(TimeSignature::COMMON);
        let beat_ticks = ts.beat_ticks();

        let mut groups: Vec<BeamGroup> = Vec::new();
        let mut current_group: Vec<Duration> = Vec::new();
        let mut current_tick: i32 = 0;

        for &dur in &self.rhythm {
            let dur_ticks = dur.ticks();
            let needs_flag = dur.needs_flag();

            // Calculate which beat we're starting in
            let start_beat = current_tick / beat_ticks;
            let end_tick = current_tick + dur_ticks;
            let end_beat = (end_tick - 1) / beat_ticks; // -1 because note ending exactly on beat boundary belongs to previous beat

            // Non-flagged notes (quarters, halves, etc.) break any current beam group
            if !needs_flag {
                // Finish any pending beam group
                if !current_group.is_empty() {
                    groups.push(BeamGroup {
                        notes: std::mem::take(&mut current_group),
                    });
                }
                // Add this note as its own group (not beamable)
                groups.push(BeamGroup { notes: vec![dur] });
                current_tick = end_tick;
                continue;
            }

            // For flagged notes: check if we're crossing into a new beat
            let crosses_beat = start_beat != end_beat;

            // If we're at a beat boundary and have a pending group, finish it
            if current_tick > 0 && current_tick % beat_ticks == 0 && !current_group.is_empty() {
                groups.push(BeamGroup {
                    notes: std::mem::take(&mut current_group),
                });
            }

            // Add this flagged note to current group
            current_group.push(dur);
            current_tick = end_tick;

            // If this note crosses a beat boundary, finish the group
            // (The note itself completes this beat's group)
            if crosses_beat || current_tick % beat_ticks == 0 {
                groups.push(BeamGroup {
                    notes: std::mem::take(&mut current_group),
                });
            }
        }

        // Add any remaining notes
        if !current_group.is_empty() {
            groups.push(BeamGroup {
                notes: current_group,
            });
        }

        groups
    }

    /// Build the final scene from computed segment positions.
    fn build_scene(
        &self,
        ctx: &LayoutContext,
        segments: &SegmentList,
        elements: &[SceneElement],
    ) -> SceneNode {
        let spatium = ctx.spatium();
        let mut root = SceneNode::group(SemanticId::new(ElementType::Measure, self.id_base));

        // Helper to find segment X by tick and type
        let find_segment_x = |tick: i32, seg_type: SegmentType| -> f64 {
            for seg in segments.iter() {
                if seg.tick == tick && seg.seg_type == seg_type {
                    return seg.x;
                }
            }
            0.0
        };

        // Helper to find chord/rest segment X by tick
        let find_chord_x = |tick: i32| -> f64 {
            for seg in segments.iter() {
                if seg.tick == tick && seg.seg_type.is_chord_rest() {
                    return seg.x;
                }
            }
            // Fallback: find closest segment before this tick
            segments
                .iter()
                .filter(|s| s.tick <= tick && s.seg_type.is_chord_rest())
                .last()
                .map(|s| s.x)
                .unwrap_or(0.0)
        };

        // Track which tick positions have been used for barlines
        let mut barline_count = 0;

        for element in elements {
            match element {
                SceneElement::Clef { id, node } => {
                    // Clef is at tick 0 with CLEF segment type
                    let x = find_segment_x(0, SegmentType::CLEF);
                    let mut container = SceneNode::group(SemanticId::new(ElementType::Clef, *id));
                    container.transform = Affine::translate((x, 0.0));
                    container.add_child(node.clone());
                    root.add_child(container);
                }
                SceneElement::TimeSignature { id, node } => {
                    // Time sig is at tick 0 with TIME_SIG segment type
                    let x = find_segment_x(0, SegmentType::TIME_SIG);
                    let mut container =
                        SceneNode::group(SemanticId::new(ElementType::TimeSignature, *id));
                    container.transform = Affine::translate((x, 0.0));
                    container.add_child(node.clone());
                    root.add_child(container);
                }
                SceneElement::Barline { id, node } => {
                    // Find the barline segment - first one is start barline, last is end barline
                    let barline_segments: Vec<_> = segments
                        .iter()
                        .filter(|s| s.seg_type.is_barline())
                        .collect();

                    let x = if barline_count < barline_segments.len() {
                        barline_segments[barline_count].x
                    } else {
                        // End barline - position at the end of last segment
                        segments.iter().last().map(|s| s.x + s.width).unwrap_or(0.0)
                    };
                    barline_count += 1;

                    let mut container =
                        SceneNode::group(SemanticId::new(ElementType::Barline, *id));
                    container.transform = Affine::translate((x, 0.0));
                    container.add_child(node.clone());
                    root.add_child(container);
                }
                SceneElement::Chord { id, node, tick } => {
                    let x = find_chord_x(*tick);
                    let mut container = SceneNode::group(SemanticId::chord(*id));
                    container.transform = Affine::translate((x, 0.0));
                    container.add_child(node.clone());
                    root.add_child(container);
                }
                SceneElement::BeamGroup {
                    start_tick: _,
                    notes,
                    head_type,
                    stem_dir,
                    note_line,
                } => {
                    // Build beam notes with computed X positions
                    let beam_notes: Vec<BeamNote> = notes
                        .iter()
                        .map(|info| {
                            let x = find_chord_x(info.tick);
                            BeamNote {
                                x,
                                line: *note_line,
                                duration: info.duration.to_note_duration(),
                                stem_direction: *stem_dir,
                                head_type: *head_type,
                            }
                        })
                        .collect();

                    // Layout noteheads
                    for info in notes {
                        let x = find_chord_x(info.tick);
                        let (_, note_node) = layout_note(
                            &NoteParams {
                                id: info.id,
                                duration: info.duration.to_note_duration(),
                                head_type: *head_type,
                                line: *note_line,
                                dots: info.duration.dots(),
                                ledger_lines: false,
                                ..Default::default()
                            },
                            ctx,
                        );
                        let mut container =
                            SceneNode::group(SemanticId::new(ElementType::Note, info.id));
                        container.transform = Affine::translate((x, 0.0));
                        container.add_child(note_node);
                        root.add_child(container);
                    }

                    // Layout beam
                    let beam_config = BeamLayoutConfig::default();
                    let beam_result = layout_beam(&beam_notes, spatium, &beam_config);
                    let beam_node = SceneNode::anonymous_leaf(beam_result.commands);
                    root.add_child(beam_node);
                }
            }
        }

        root
    }
}

/// Information about a beam note before position is computed.
#[derive(Debug, Clone)]
struct BeamNoteInfo {
    id: u64,
    tick: i32,
    duration: Duration,
}

/// A group of notes that should be beamed together.
#[derive(Debug, Clone)]
struct BeamGroup {
    notes: Vec<Duration>,
}

/// Scene element before final positioning.
#[derive(Debug, Clone)]
enum SceneElement {
    Clef {
        id: u64,
        node: SceneNode,
    },
    TimeSignature {
        id: u64,
        node: SceneNode,
    },
    Barline {
        id: u64,
        node: SceneNode,
    },
    Chord {
        id: u64,
        node: SceneNode,
        tick: i32,
    },
    BeamGroup {
        start_tick: i32,
        notes: Vec<BeamNoteInfo>,
        head_type: NoteHeadType,
        stem_dir: StemDirection,
        note_line: i32,
    },
}

/// Result of building a measure.
#[derive(Debug)]
pub struct MeasureScene {
    /// The scene graph for the measure
    pub scene: SceneNode,
    /// Total width after spacing
    pub width: f64,
    /// The segment list (for debugging/inspection)
    pub segments: SegmentList,
}

/// Builder for creating a system (line) of multiple measures.
#[derive(Debug, Clone)]
pub struct SystemBuilder {
    /// Measures in this system
    measures: Vec<MeasureBuilder>,
    /// Total system width in spatiums
    system_width: f64,
    /// Staff Y position
    staff_y: f64,
}

impl SystemBuilder {
    /// Create a new system builder.
    #[must_use]
    pub fn new(system_width: f64) -> Self {
        Self {
            measures: Vec::new(),
            system_width,
            staff_y: 0.0,
        }
    }

    /// Add a measure to the system.
    #[must_use]
    pub fn measure(mut self, measure: MeasureBuilder) -> Self {
        self.measures.push(measure);
        self
    }

    /// Set the staff Y position.
    #[must_use]
    pub fn at_y(mut self, y: f64) -> Self {
        self.staff_y = y;
        self
    }

    /// Build the system scene.
    #[must_use]
    pub fn build(self, ctx: &LayoutContext) -> SystemScene {
        let spatium = ctx.spatium();
        let mut root = SceneNode::group(SemanticId::new(ElementType::System, 1));

        // Draw staff lines
        let staff_lines = draw_staff_lines(0.0, 0.0, self.system_width * spatium, spatium);
        root.add_child(SceneNode::anonymous_leaf(staff_lines));

        // Build each measure and position them sequentially
        let mut x_offset = 0.0;
        let mut measure_scenes = Vec::new();

        for (i, measure) in self.measures.into_iter().enumerate() {
            let measure_scene = measure.id_base((i as u64 + 1) * 1000).build(ctx);

            let mut measure_container =
                SceneNode::group(SemanticId::new(ElementType::Measure, i as u64 + 1));
            measure_container.transform = Affine::translate((x_offset, 0.0));
            measure_container.add_child(measure_scene.scene.clone());
            root.add_child(measure_container);

            x_offset += measure_scene.width;
            measure_scenes.push(measure_scene);
        }

        // Position root at staff Y
        root.transform = Affine::translate((0.0, self.staff_y));

        SystemScene {
            scene: root,
            width: x_offset,
            measures: measure_scenes,
        }
    }
}

/// Result of building a system.
#[derive(Debug)]
pub struct SystemScene {
    /// The scene graph for the system
    pub scene: SceneNode,
    /// Total width
    pub width: f64,
    /// Individual measure scenes
    pub measures: Vec<MeasureScene>,
}

/// Draw 5 staff lines.
fn draw_staff_lines(x: f64, y: f64, width: f64, spatium: f64) -> Vec<PaintCommand> {
    let mut commands = Vec::new();
    let line_thickness = spatium * 0.1;

    for i in 0..5 {
        let line_y = y + i as f64 * spatium;
        commands.push(PaintCommand::line(
            Point::new(x, line_y),
            Point::new(x + width, line_y),
            Color::BLACK,
            line_thickness,
        ));
    }

    commands
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_auto_stemless_all_quarters_in_4_4() {
        // 4 consecutive quarters in 4/4: beats 1-2 are one group, beats 3-4 are another
        // Both groups have 2+ quarters, so all are stemless
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Quarter,
                Duration::Quarter,
                Duration::Quarter,
                Duration::Quarter,
            ]);

        let flags = builder.compute_auto_stemless();
        // Beats 1-2 (before beat 3) = stemless, beats 3-4 (after beat 3) = stemless
        assert_eq!(flags, vec![true, true, true, true]);
    }

    #[test]
    fn test_auto_stemless_two_quarters() {
        // 2 consecutive quarters should be stemless (minimum threshold)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![Duration::Quarter, Duration::Quarter]);

        let flags = builder.compute_auto_stemless();
        assert_eq!(flags, vec![true, true]);
    }

    #[test]
    fn test_auto_stemless_single_quarter() {
        // Single quarter should NOT be stemless (needs 2+)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![Duration::Quarter]);

        let flags = builder.compute_auto_stemless();
        assert_eq!(flags, vec![false]);
    }

    #[test]
    fn test_auto_stemless_mixed_eighths_quarters() {
        // 8th 8th Q Q Q starting on beat 1:
        // - 8th 8th (beat 1) = not quarters
        // - Q on beat 2 (before strong beat 3) = alone = has stem
        // - Q Q on beats 3-4 = consecutive after beat 3 = stemless
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Eighth,  // beat 1
                Duration::Eighth,  // beat 1.5
                Duration::Quarter, // beat 2 (alone before beat 3)
                Duration::Quarter, // beat 3
                Duration::Quarter, // beat 4
            ]);

        let flags = builder.compute_auto_stemless();
        // Beat 2 quarter is alone before beat 3 = false
        // Beat 3-4 quarters are consecutive = true
        assert_eq!(flags, vec![false, false, false, true, true]);
    }

    #[test]
    fn test_auto_stemless_quarter_breaks_chain() {
        // Q 8th Q Q → first Q alone (false), 8th false, last 2 Q consecutive (true)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Quarter, // beat 1 (alone)
                Duration::Eighth,  // beat 2 (breaks chain)
                Duration::Quarter, // beat 2.5 (before beat 3)
                Duration::Quarter, // beat 3
            ]);

        let flags = builder.compute_auto_stemless();
        // Q on beat 1 alone, Q before beat 3 alone, Q on beat 3 alone
        // Actually the last two are at beats 2.5 and 3, crossing beat 3
        assert_eq!(flags, vec![false, false, false, false]);
    }

    #[test]
    fn test_auto_stemless_half_note_breaks_chain() {
        // Half + Q + Q → half on beats 1-2, Q Q on beats 3-4
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![Duration::Half, Duration::Quarter, Duration::Quarter]);

        let flags = builder.compute_auto_stemless();
        // Half is not a quarter (false), Q Q on beats 3-4 are consecutive = true
        assert_eq!(flags, vec![false, true, true]);
    }

    #[test]
    fn test_auto_stemless_dotted_quarter_not_plain_quarter() {
        // Dotted quarters are NOT plain quarters, so they don't form stemless groups
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::DottedQuarter,
                Duration::DottedQuarter,
                Duration::Quarter,
            ]);

        let flags = builder.compute_auto_stemless();
        // Dotted quarters are NOT plain quarters, single plain quarter = false
        assert_eq!(flags, vec![false, false, false]);
    }

    #[test]
    fn test_auto_stemless_standard_mode_disabled() {
        // Auto-stemless only applies to rhythmic mode
        let builder = MeasureBuilder::new()
            .mode(NotationMode::Standard)
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::Quarter,
                Duration::Quarter,
                Duration::Quarter,
                Duration::Quarter,
            ]);

        let flags = builder.compute_auto_stemless();
        // Standard mode = no auto-stemless
        assert_eq!(flags, vec![false, false, false, false]);
    }

    #[test]
    fn test_auto_stemless_strong_beat_crossing() {
        // In 4/4: Q rest Q Q starting on beat 2
        // Beat 2: Q (alone before beat 3) = stem
        // Beat 3: rest (not a quarter, but starts group after beat 3)
        // Beats 3.5-4: Q Q = consecutive after beat 3 = stemless
        // But wait, we start on beat 1, so let's be precise:
        // If we have rest, Q, Q, Q:
        // - rest on beat 1
        // - Q on beat 2 (alone before beat 3)
        // - Q on beat 3
        // - Q on beat 4
        // Beat 2 Q is alone (before beat 3), beats 3-4 are consecutive
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Quarter, // beat 1
                Duration::Quarter, // beat 2
                Duration::Quarter, // beat 3
                Duration::Quarter, // beat 4
            ]);

        let flags = builder.compute_auto_stemless();
        // Beats 1-2 form one group (2 quarters), beats 3-4 form another
        assert_eq!(flags, vec![true, true, true, true]);
    }

    #[test]
    fn test_auto_stemless_beat_2_3_4_pattern() {
        // Specific case from user: 8th 8th Q Q Q starting beat 1
        // 8th on beat 1, 8th on beat 1.5, Q on beat 2, Q on beat 3, Q on beat 4
        // Q on beat 2 is alone (before strong beat 3) = stem
        // Q on beats 3-4 are consecutive = stemless
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Eighth,  // beat 1
                Duration::Eighth,  // beat 1.5
                Duration::Quarter, // beat 2
                Duration::Quarter, // beat 3
                Duration::Quarter, // beat 4
            ]);

        let flags = builder.compute_auto_stemless();
        // 8ths are not quarters (false), beat 2 Q alone (false), beats 3-4 Q consecutive (true)
        assert_eq!(flags, vec![false, false, false, true, true]);
    }

    #[test]
    fn test_auto_stemless_no_quarters_at_all() {
        // When there are NO plain quarter notes, all notes should have stems
        // This tests dotted eighths + sixteenths + eighths pattern
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::DottedEighth,
                Duration::Sixteenth,
                Duration::DottedEighth,
                Duration::Sixteenth,
                Duration::DottedEighth,
                Duration::Sixteenth,
                Duration::Eighth,
            ]);

        let flags = builder.compute_auto_stemless();
        // No plain quarters = no stemless notes
        assert_eq!(flags, vec![false, false, false, false, false, false, false]);
    }

    #[test]
    fn test_auto_stemless_syncopation_no_quarters() {
        // Complex syncopation with no plain quarters
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythmic()
            .rhythm(vec![
                Duration::Eighth,
                Duration::DottedQuarter,
                Duration::Eighth,
                Duration::Half,
            ]);

        let flags = builder.compute_auto_stemless();
        // No plain quarters = no stemless notes
        assert_eq!(flags, vec![false, false, false, false]);
    }

    #[test]
    fn test_beam_groups_sixteenths_by_beat() {
        // 8 sixteenth notes in 4/4 should create 2 beam groups (4 per beat)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::Sixteenth,
                Duration::Sixteenth,
                Duration::Sixteenth,
                Duration::Sixteenth, // End of beat 1
                Duration::Sixteenth,
                Duration::Sixteenth,
                Duration::Sixteenth,
                Duration::Sixteenth, // End of beat 2
            ]);

        let groups = builder.compute_beam_groups();
        // Should be 2 groups of 4 sixteenths each
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].notes.len(), 4);
        assert_eq!(groups[1].notes.len(), 4);
    }

    #[test]
    fn test_beam_groups_eighths_by_beat() {
        // 4 eighth notes in 4/4 should create 2 beam groups (2 per beat)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::Eighth,
                Duration::Eighth, // End of beat 1
                Duration::Eighth,
                Duration::Eighth, // End of beat 2
            ]);

        let groups = builder.compute_beam_groups();
        // Should be 2 groups of 2 eighths each
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].notes.len(), 2);
        assert_eq!(groups[1].notes.len(), 2);
    }

    #[test]
    fn test_beam_groups_mixed_rhythms() {
        // Quarter + 2 eighths + quarter in 4/4
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::Quarter,  // Beat 1 (not beamed)
                Duration::Eighth,   // Beat 2
                Duration::Eighth,   // Beat 2
                Duration::Quarter,  // Beat 3 (not beamed)
            ]);

        let groups = builder.compute_beam_groups();
        // Should be: [Quarter], [Eighth, Eighth], [Quarter]
        assert_eq!(groups.len(), 3);
        assert_eq!(groups[0].notes.len(), 1); // Quarter
        assert_eq!(groups[1].notes.len(), 2); // 2 eighths beamed
        assert_eq!(groups[2].notes.len(), 1); // Quarter
    }

    #[test]
    fn test_beam_groups_32nds_by_beat() {
        // 8 thirty-second notes in 4/4 (covers half a beat)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond,
                Duration::ThirtySecond, // Half of beat 1
            ]);

        let groups = builder.compute_beam_groups();
        // All 8 should be in one group (within beat 1)
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].notes.len(), 8);
    }

    #[test]
    fn test_beam_groups_cross_beat_boundary() {
        // 3 eighths starting on beat 1.5 should break at beat 2
        // (This is beat 1: eighth, then 2 eighths that cross into beat 2)
        let builder = MeasureBuilder::new()
            .time_signature(4, 4)
            .rhythm(vec![
                Duration::Eighth,   // Beat 1 first half
                Duration::Eighth,   // Beat 1 second half - completes beat 1
                Duration::Eighth,   // Beat 2 first half
            ]);

        let groups = builder.compute_beam_groups();
        // Should be: [Eighth, Eighth] (beat 1), [Eighth] (beat 2)
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].notes.len(), 2);
        assert_eq!(groups[1].notes.len(), 1);
    }
}
