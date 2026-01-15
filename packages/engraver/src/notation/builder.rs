//! Builders for automatic music notation layout.

use kurbo::{Affine, Point, Rect};
use peniko::Color;

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
            start_barline: None,
            end_barline: Some(BarlineType::Single),
            width: None,
            justify: false,
            id_base: 1,
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

        // Get mode-specific settings
        let head_type = self.mode.notehead_type();
        let stem_dir = self.mode.default_stem_direction();
        let note_line = self.mode.default_line();

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
            if group.notes.len() == 1 && !group.notes[0].needs_flag() {
                // Single note, no beaming needed - use chord layout
                let dur = group.notes[0];
                let mut seg = Segment::chord_rest(current_tick, dur.ticks());
                seg.ticks = dur.ticks();
                segment_vec.push(seg);

                let (_, chord_node) = layout_chord(
                    &ChordParams {
                        id,
                        duration: dur.to_note_duration(),
                        head_type,
                        notes: vec![ChordNote {
                            line: note_line,
                            accidental: Accidental::None,
                            tie: false,
                        }],
                        stem_direction: stem_dir,
                        dots: dur.dots(),
                        beamed: false,
                    },
                    ctx,
                );

                scene_elements.push(SceneElement::Chord {
                    id,
                    node: chord_node,
                    tick: current_tick,
                });

                current_tick += dur.ticks();
                id += 1;
            } else if group.notes.iter().any(|d| d.needs_flag()) {
                // Beamed group - create beam notes
                let group_start_tick = current_tick;
                let mut beam_notes: Vec<BeamNoteInfo> = Vec::new();

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
                    id += 1;
                }

                scene_elements.push(SceneElement::BeamGroup {
                    start_tick: group_start_tick,
                    notes: beam_notes,
                    head_type,
                    stem_dir,
                    note_line,
                });
            } else {
                // Multiple non-flagged notes - individual chords
                for dur in &group.notes {
                    let mut seg = Segment::chord_rest(current_tick, dur.ticks());
                    seg.ticks = dur.ticks();
                    segment_vec.push(seg);

                    let (_, chord_node) = layout_chord(
                        &ChordParams {
                            id,
                            duration: dur.to_note_duration(),
                            head_type,
                            notes: vec![ChordNote {
                                line: note_line,
                                accidental: Accidental::None,
                                tie: false,
                            }],
                            stem_direction: stem_dir,
                            dots: dur.dots(),
                            beamed: false,
                        },
                        ctx,
                    );

                    scene_elements.push(SceneElement::Chord {
                        id,
                        node: chord_node,
                        tick: current_tick,
                    });

                    current_tick += dur.ticks();
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

        // 8. Apply horizontal spacing
        let spacing = HorizontalSpacing::new(spatium);
        let target_width = self.width.map(|w| w * spatium).unwrap_or(f64::MAX);
        let spacing_result = spacing.compute_spacing(&mut segments, target_width, self.justify);

        // 9. Build final scene with computed positions
        let scene = self.build_scene(ctx, &segments, &scene_elements);

        MeasureScene {
            scene,
            width: spacing_result.total_width,
            segments,
        }
    }

    /// Compute beam groups based on time signature and rhythm.
    ///
    /// Rules:
    /// - Only flagged notes (8ths, 16ths, etc.) can be beamed together
    /// - Non-flagged notes (quarters, halves, etc.) are always in their own group
    /// - Beat boundaries break beam groups
    fn compute_beam_groups(&self) -> Vec<BeamGroup> {
        if self.rhythm.is_empty() {
            return Vec::new();
        }

        let ts = self.time_signature.unwrap_or(TimeSignature::COMMON);
        let beat_ticks = ts.beat_ticks();

        let mut groups: Vec<BeamGroup> = Vec::new();
        let mut current_group: Vec<Duration> = Vec::new();
        let mut beat_position: i32 = 0;

        for &dur in &self.rhythm {
            let dur_ticks = dur.ticks();
            let needs_flag = dur.needs_flag();

            // Non-flagged notes (quarters, halves, etc.) break any current beam group
            // and go in their own single-note group
            if !needs_flag {
                // Finish any pending beam group
                if !current_group.is_empty() {
                    groups.push(BeamGroup {
                        notes: std::mem::take(&mut current_group),
                    });
                }
                // Add this note as its own group
                groups.push(BeamGroup { notes: vec![dur] });
                beat_position += dur_ticks;
                continue;
            }

            // For flagged notes: check if this note crosses a beat boundary
            let remaining_in_beat = beat_ticks - (beat_position % beat_ticks);

            if dur_ticks > remaining_in_beat && !current_group.is_empty() {
                // Finish current group before beat boundary
                groups.push(BeamGroup {
                    notes: std::mem::take(&mut current_group),
                });
            }

            // Add flagged note to current group
            current_group.push(dur);
            beat_position += dur_ticks;

            // Check if we've completed a beat
            if beat_position % beat_ticks == 0 {
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
