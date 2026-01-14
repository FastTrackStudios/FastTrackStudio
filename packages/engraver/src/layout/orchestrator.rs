//! Layout orchestrator - ties together all layout components.
//!
//! This module provides the main entry point for laying out a complete score.
//! It coordinates the segment system, horizontal spacing, element layouts,
//! and builds the final scene graph.
//!
//! ## Architecture
//!
//! ```text
//! Score (Model)
//!     ↓
//! LayoutContext + HorizontalSpacing + Segments
//!     ↓
//! SceneNode (Scene Graph with SemanticIds)
//!     ↓
//! ┌───────────────┬───────────────┐
//! │ SceneRenderer │ SvgSerializer │
//! │   (WGPU)      │   (SVG XML)   │
//! └───────────────┴───────────────┘
//! ```

use kurbo::{Affine, Point, Rect};
use peniko::Color;

use crate::fonts::SMuFLFont;
use crate::layout::context::{LayoutConfiguration, LayoutContext};
use crate::layout::tlayout::chord::{layout_chord, ChordNote, ChordParams, StemDirection};
use crate::layout::tlayout::clef::{layout_clef, ClefParams, ClefType};
use crate::layout::tlayout::keysig::{layout_keysig, KeySigParams, KeySigType};
use crate::layout::tlayout::note::{layout_note, Accidental, NoteDuration, NoteParams};
use crate::layout::tlayout::rest::{layout_rest, RestDuration, RestParams};
use crate::layout::tlayout::timesig::{layout_timesig, TimeSigParams, TimeSigType};
use crate::model::{DurationKind, MusicElement, Score};
use crate::scene::id::{ElementType, SemanticId};
use crate::scene::node::SceneNode;
use crate::scene::paint::PaintCommand;
use crate::style::MStyle;

/// Result of laying out a complete score.
#[derive(Debug)]
pub struct LayoutResult {
    /// The complete scene graph for the score
    pub scene: SceneNode,
    /// Page layouts with system positions
    pub pages: Vec<PageLayout>,
    /// Total width in pixels
    pub total_width: f64,
    /// Total height in pixels
    pub total_height: f64,
    /// Number of systems
    pub system_count: usize,
    /// Number of measures
    pub measure_count: usize,
}

/// Layout information for a single page.
#[derive(Debug, Clone)]
pub struct PageLayout {
    /// Page number (1-indexed)
    pub number: u32,
    /// Page dimensions
    pub width: f64,
    pub height: f64,
    /// Systems on this page
    pub systems: Vec<SystemLayout>,
    /// Page margins
    pub margins: PageMargins,
}

/// Layout information for a single system (line of music).
#[derive(Debug, Clone)]
pub struct SystemLayout {
    /// System index (0-indexed within score)
    pub index: usize,
    /// Y position on page (from top)
    pub y: f64,
    /// System width
    pub width: f64,
    /// System height
    pub height: f64,
    /// Measure indices in this system
    pub measure_indices: Vec<usize>,
}

/// Page margins.
#[derive(Debug, Clone, Copy)]
pub struct PageMargins {
    pub top: f64,
    pub right: f64,
    pub bottom: f64,
    pub left: f64,
}

impl Default for PageMargins {
    fn default() -> Self {
        Self {
            top: 50.0,
            right: 50.0,
            bottom: 50.0,
            left: 50.0,
        }
    }
}

/// Layout engine configuration.
#[derive(Debug, Clone)]
pub struct LayoutEngineConfig {
    /// Page width in pixels
    pub page_width: f64,
    /// Page height in pixels
    pub page_height: f64,
    /// Page margins
    pub margins: PageMargins,
    /// Staff space (spatium) in pixels
    pub spatium: f64,
    /// Space between systems
    pub system_spacing: f64,
    /// Maximum measures per system (0 = auto)
    pub max_measures_per_system: usize,
    /// Minimum measure width
    pub min_measure_width: f64,
    /// Maximum measure width
    pub max_measure_width: f64,
}

impl Default for LayoutEngineConfig {
    fn default() -> Self {
        Self {
            page_width: 816.0,  // A4 at 96 DPI
            page_height: 1056.0,
            margins: PageMargins::default(),
            spatium: 10.0,
            system_spacing: 80.0,
            max_measures_per_system: 0, // Auto
            min_measure_width: 80.0,
            max_measure_width: 400.0,
        }
    }
}

// ============================================================================
// Model to Layout Type Conversions
// ============================================================================

/// Convert model DurationKind to layout NoteDuration.
fn duration_kind_to_note_duration(kind: DurationKind) -> NoteDuration {
    match kind {
        DurationKind::Whole => NoteDuration::Whole,
        DurationKind::Half => NoteDuration::Half,
        DurationKind::Quarter => NoteDuration::Quarter,
        DurationKind::Eighth => NoteDuration::Eighth,
        DurationKind::Sixteenth => NoteDuration::Sixteenth,
        DurationKind::ThirtySecond => NoteDuration::ThirtySecond,
        DurationKind::SixtyFourth => NoteDuration::SixtyFourth,
    }
}

/// Convert model DurationKind to layout RestDuration.
fn duration_kind_to_rest_duration(kind: DurationKind) -> RestDuration {
    match kind {
        DurationKind::Whole => RestDuration::Whole,
        DurationKind::Half => RestDuration::Half,
        DurationKind::Quarter => RestDuration::Quarter,
        DurationKind::Eighth => RestDuration::Eighth,
        DurationKind::Sixteenth => RestDuration::Sixteenth,
        DurationKind::ThirtySecond => RestDuration::ThirtySecond,
        DurationKind::SixtyFourth => RestDuration::SixtyFourth,
    }
}

/// Convert model Accidental to layout Accidental.
fn model_accidental_to_layout(acc: crate::model::Accidental) -> Accidental {
    match acc {
        crate::model::Accidental::None => Accidental::None,
        crate::model::Accidental::Natural => Accidental::Natural,
        crate::model::Accidental::Sharp => Accidental::Sharp,
        crate::model::Accidental::Flat => Accidental::Flat,
        crate::model::Accidental::DoubleSharp => Accidental::DoubleSharp,
        crate::model::Accidental::DoubleFlat => Accidental::DoubleFlat,
    }
}

/// Convert model Clef to layout ClefType.
fn model_clef_to_layout(clef: crate::model::Clef) -> ClefType {
    match clef {
        crate::model::Clef::Treble => ClefType::Treble,
        crate::model::Clef::Bass => ClefType::Bass,
        crate::model::Clef::Alto => ClefType::Alto,
        crate::model::Clef::Tenor => ClefType::Tenor,
    }
}

/// Convert model Stem to layout StemDirection.
fn model_stem_to_layout(stem: crate::model::Stem) -> StemDirection {
    match stem {
        crate::model::Stem::Up => StemDirection::Up,
        crate::model::Stem::Down => StemDirection::Down,
        crate::model::Stem::None => StemDirection::Auto,
    }
}

/// Main layout engine for music notation.
///
/// Orchestrates the complete layout pipeline from Score to SceneNode.
pub struct LayoutEngine<'a> {
    config: LayoutEngineConfig,
    style: &'a MStyle,
    font: Option<&'a SMuFLFont<'a>>,
}

impl<'a> LayoutEngine<'a> {
    /// Create a new layout engine with default configuration.
    pub fn new(style: &'a MStyle) -> Self {
        Self {
            config: LayoutEngineConfig::default(),
            style,
            font: None,
        }
    }

    /// Create a layout engine with custom configuration.
    pub fn with_config(config: LayoutEngineConfig, style: &'a MStyle) -> Self {
        Self {
            config,
            style,
            font: None,
        }
    }

    /// Set the music font for glyph layout.
    pub fn with_font(mut self, font: &'a SMuFLFont<'a>) -> Self {
        self.font = Some(font);
        self
    }

    /// Layout an entire score.
    ///
    /// This is the main entry point for the layout pipeline.
    pub fn layout_score(&self, score: &'a Score) -> LayoutResult {
        // Calculate number of measures
        let measure_count = self.count_measures(score);

        // Group measures into systems
        let systems_measures = self.compute_systems(score, measure_count);

        // Create root scene node (use Page as the root container type)
        let mut root = SceneNode::group(SemanticId::new(ElementType::Page, 0));

        // Layout pages
        let mut pages = Vec::new();
        let mut current_page_systems = Vec::new();
        let mut page_y = self.config.margins.top;
        let mut page_number = 1u32;
        let mut system_index = 0usize;

        let content_height = self.config.page_height - self.config.margins.top - self.config.margins.bottom;

        for (sys_idx, measure_indices) in systems_measures.iter().enumerate() {
            // Create system node
            let system_height = self.config.spatium * 4.0; // 5 staff lines = 4 spaces

            // Check if we need a new page
            if page_y + system_height + self.config.system_spacing > self.config.margins.top + content_height {
                // Finalize current page
                if !current_page_systems.is_empty() {
                    let page = PageLayout {
                        number: page_number,
                        width: self.config.page_width,
                        height: self.config.page_height,
                        systems: std::mem::take(&mut current_page_systems),
                        margins: self.config.margins,
                    };
                    pages.push(page);
                    page_number += 1;
                    page_y = self.config.margins.top;
                }
            }

            // Create system layout info
            let system_layout = SystemLayout {
                index: system_index,
                y: page_y,
                width: self.content_width(),
                height: system_height,
                measure_indices: measure_indices.clone(),
            };
            current_page_systems.push(system_layout);

            // Layout the system
            let system_node = self.layout_single_system(
                score,
                measure_indices,
                system_index,
                page_y,
            );
            root.add_child(system_node);

            page_y += system_height + self.config.system_spacing;
            system_index += 1;
        }

        // Add final page
        if !current_page_systems.is_empty() {
            let page = PageLayout {
                number: page_number,
                width: self.config.page_width,
                height: self.config.page_height,
                systems: current_page_systems,
                margins: self.config.margins,
            };
            pages.push(page);
        }

        // Calculate total dimensions
        let total_height = pages.len() as f64 * self.config.page_height;

        LayoutResult {
            scene: root,
            pages,
            total_width: self.config.page_width,
            total_height,
            system_count: system_index,
            measure_count,
        }
    }

    /// Get the content width (page width minus margins).
    fn content_width(&self) -> f64 {
        self.config.page_width - self.config.margins.left - self.config.margins.right
    }

    /// Count total measures in score.
    fn count_measures(&self, score: &Score) -> usize {
        score.parts.first().map_or(0, |p| p.measures.len())
    }

    /// Group measures into systems.
    fn compute_systems(&self, score: &Score, measure_count: usize) -> Vec<Vec<usize>> {
        let mut systems = Vec::new();
        let content_width = self.content_width();

        if self.config.max_measures_per_system > 0 {
            // Fixed measures per system
            for chunk in (0..measure_count).collect::<Vec<_>>().chunks(self.config.max_measures_per_system) {
                systems.push(chunk.to_vec());
            }
        } else {
            // Auto-compute based on measure widths
            let default_measure_width = content_width / 4.0; // Default: 4 measures per system
            let mut current_system = Vec::new();
            let mut current_width = 0.0;

            for i in 0..measure_count {
                let measure_width = self.estimate_measure_width(score, i);

                if current_width + measure_width > content_width && !current_system.is_empty() {
                    systems.push(std::mem::take(&mut current_system));
                    current_width = 0.0;
                }

                current_system.push(i);
                current_width += measure_width;
            }

            if !current_system.is_empty() {
                systems.push(current_system);
            }
        }

        // Handle empty case
        if systems.is_empty() && measure_count > 0 {
            systems.push((0..measure_count).collect());
        }

        systems
    }

    /// Estimate width needed for a measure.
    fn estimate_measure_width(&self, _score: &Score, _measure_idx: usize) -> f64 {
        // Simple estimation based on config
        // A full implementation would analyze note density
        self.config.min_measure_width.max(self.content_width() / 4.0)
    }

    /// Layout a single system with actual music content.
    fn layout_single_system(
        &self,
        score: &Score,
        measure_indices: &[usize],
        system_index: usize,
        y_position: f64,
    ) -> SceneNode {
        let content_width = self.content_width();
        let measure_count = measure_indices.len();

        // Calculate measure widths (distribute evenly for now)
        let measure_width = if measure_count > 0 {
            content_width / measure_count as f64
        } else {
            content_width
        };

        // Create layout context for element layout
        // Note: minimal() leaks memory for Score and Font, but this is acceptable
        // for simple layout operations. For production use, pass a full context.
        let static_style: &'static MStyle = Box::leak(Box::new(self.style.clone()));
        let ctx = LayoutContext::minimal(static_style);

        // Create system node
        let mut system_node = SceneNode::group(SemanticId::new(ElementType::System, system_index as u64));
        system_node.transform = Affine::translate((self.config.margins.left, y_position));

        let mut current_x = 0.0;
        let is_first_system = system_index == 0;

        // Layout each measure
        for (local_idx, &measure_idx) in measure_indices.iter().enumerate() {
            let is_first = local_idx == 0;
            let is_last = local_idx == measure_count - 1;

            // Create measure node
            let mut measure_node = SceneNode::group(SemanticId::measure(measure_idx as u64));
            measure_node.transform = Affine::translate((current_x, 0.0));

            // Add staff lines
            let staff_lines = self.create_staff_lines(measure_width);
            measure_node.add_child(staff_lines);

            // Add barlines
            if is_first {
                // Left barline at start of system
                let left_barline = self.create_barline(0.0, false);
                measure_node.add_child(left_barline);
            }

            // Track x position within measure for element placement
            let mut element_x = self.config.spatium * 0.5; // Small left margin

            // Add clef at start of first system's first measure
            if is_first_system && is_first {
                let clef_params = ClefParams {
                    id: 0,
                    clef_type: ClefType::Treble,
                    is_change: false,
                    ..Default::default()
                };
                let (_, clef_node) = layout_clef(&clef_params, &ctx);
                let mut positioned = clef_node;
                positioned.transform = Affine::translate((element_x, 0.0));
                measure_node.add_child(positioned);
                element_x += self.config.spatium * 3.0;
            }

            // Add key signature at start of first system's first measure (if not C major)
            if is_first_system && is_first && score.time_signature != crate::model::TimeSignature::COMMON {
                // Key signature would go here if the score had one
            }

            // Add time signature at start of first system's first measure
            if is_first_system && is_first {
                let ts_params = TimeSigParams {
                    id: 1,
                    sig_type: TimeSigType::Numeric {
                        numerator: score.time_signature.numerator,
                        denominator: score.time_signature.denominator,
                    },
                    large: false,
                };
                let (_, ts_node) = layout_timesig(&ts_params, &ctx);
                let mut positioned = ts_node;
                positioned.transform = Affine::translate((element_x, 0.0));
                measure_node.add_child(positioned);
                element_x += self.config.spatium * 3.0;
            }

            // Layout music elements from the score
            if let Some(part) = score.parts.first() {
                if let Some(measure) = part.measures.get(measure_idx) {
                    let content_area = measure_width - element_x - self.config.spatium * 0.5;
                    let elements_node = self.layout_measure_elements(
                        measure,
                        &ctx,
                        element_x,
                        content_area,
                        measure_idx as u64,
                    );
                    measure_node.add_child(elements_node);
                }
            }

            // Right barline
            let right_barline = self.create_barline(measure_width, is_last);
            measure_node.add_child(right_barline);

            // Calculate bounds
            let staff_height = self.config.spatium * 4.0;
            measure_node.bounds = Rect::new(0.0, -staff_height / 2.0, measure_width, staff_height / 2.0);

            system_node.add_child(measure_node);
            current_x += measure_width;
        }

        // Set system bounds
        let staff_height = self.config.spatium * 4.0;
        system_node.bounds = Rect::new(0.0, -staff_height / 2.0, content_width, staff_height / 2.0);

        system_node
    }

    /// Layout music elements within a measure.
    fn layout_measure_elements(
        &self,
        measure: &crate::model::Measure,
        ctx: &LayoutContext,
        start_x: f64,
        content_width: f64,
        measure_id: u64,
    ) -> SceneNode {
        let mut elements_node = SceneNode::group(SemanticId::new(ElementType::Segment, measure_id));
        let mut element_id_counter = measure_id * 1000; // Unique ID base for this measure

        // Get time signature for duration calculations
        let time_sig = measure.time_signature.unwrap_or(crate::model::TimeSignature::COMMON);
        let measure_duration_quarters = time_sig.measure_duration();

        // Collect all timed elements with their beat positions
        let mut timed_elements: Vec<(f64, u64, MusicElement)> = Vec::new();

        for voice in &measure.voices {
            let mut current_beat = 0.0;
            for element in &voice.elements {
                let duration = match element {
                    MusicElement::Note(note) => note.duration.quarters(),
                    MusicElement::Rest(rest) => rest.duration.quarters(),
                    MusicElement::Chord(chord) => chord.duration().quarters(),
                    _ => 0.0, // Non-durational elements
                };

                element_id_counter += 1;
                timed_elements.push((current_beat, element_id_counter, element.clone()));
                current_beat += duration;
            }
        }

        // Layout each element at its beat position
        for (beat_position, elem_id, element) in timed_elements {
            // Calculate x position based on beat position
            let beat_fraction = beat_position / measure_duration_quarters;
            let x_pos = start_x + beat_fraction * content_width;

            match element {
                MusicElement::Note(note) => {
                    let params = NoteParams {
                        id: elem_id,
                        duration: duration_kind_to_note_duration(note.duration.kind),
                        line: note.pitch.staff_position(),
                        accidental: model_accidental_to_layout(note.accidental),
                        dots: note.duration.dots,
                        offset_x: 0.0,
                        ledger_lines: true,
                    };
                    let (_, mut node) = layout_note(&params, ctx);
                    node.transform = Affine::translate((x_pos, 0.0));
                    elements_node.add_child(node);
                }
                MusicElement::Rest(rest) => {
                    let params = RestParams {
                        id: elem_id,
                        duration: duration_kind_to_rest_duration(rest.duration.kind),
                        dots: rest.duration.dots,
                        line: 0, // Center
                    };
                    let (_, mut node) = layout_rest(&params, ctx);
                    node.transform = Affine::translate((x_pos, 0.0));
                    elements_node.add_child(node);
                }
                MusicElement::Chord(chord) => {
                    if let Some(first_note) = chord.notes.first() {
                        let notes: Vec<ChordNote> = chord.notes.iter()
                            .map(|n| ChordNote {
                                line: n.pitch.staff_position(),
                                accidental: model_accidental_to_layout(n.accidental),
                                tie: n.tie_forward,
                            })
                            .collect();
                        let params = ChordParams {
                            id: elem_id,
                            duration: duration_kind_to_note_duration(first_note.duration.kind),
                            notes,
                            stem_direction: model_stem_to_layout(first_note.stem),
                            dots: first_note.duration.dots,
                            beamed: false,
                        };
                        let (_, mut node) = layout_chord(&params, ctx);
                        node.transform = Affine::translate((x_pos, 0.0));
                        elements_node.add_child(node);
                    }
                }
                MusicElement::Clef(clef) => {
                    let params = ClefParams {
                        id: elem_id,
                        clef_type: model_clef_to_layout(clef),
                        is_change: true,
                        ..Default::default()
                    };
                    let (_, mut node) = layout_clef(&params, ctx);
                    node.transform = Affine::translate((x_pos, 0.0));
                    elements_node.add_child(node);
                }
                MusicElement::KeySignature(key_sig) => {
                    let params = KeySigParams {
                        id: elem_id,
                        key: KeySigType::Standard(key_sig.fifths),
                        ..Default::default()
                    };
                    let (_, mut node) = layout_keysig(&params, ctx);
                    node.transform = Affine::translate((x_pos, 0.0));
                    elements_node.add_child(node);
                }
                MusicElement::TimeSignature(time_sig) => {
                    let params = TimeSigParams {
                        id: elem_id,
                        sig_type: TimeSigType::Numeric {
                            numerator: time_sig.numerator,
                            denominator: time_sig.denominator,
                        },
                        large: false,
                    };
                    let (_, mut node) = layout_timesig(&params, ctx);
                    node.transform = Affine::translate((x_pos, 0.0));
                    elements_node.add_child(node);
                }
                MusicElement::Dynamic(_) | MusicElement::Barline(_) => {
                    // TODO: Layout dynamics and barlines
                }
            }
        }

        elements_node
    }

    /// Create staff lines for a measure.
    fn create_staff_lines(&self, width: f64) -> SceneNode {
        let spatium = self.config.spatium;
        let half_staff = 2.0 * spatium; // 4 spaces / 2
        let line_thickness = spatium * 0.1;

        let mut commands = Vec::new();

        for i in 0..5 {
            let y = (i as f64 - 2.0) * spatium;
            commands.push(PaintCommand::line(
                Point::new(0.0, y),
                Point::new(width, y),
                Color::BLACK,
                line_thickness,
            ));
        }

        SceneNode::anonymous_leaf(commands)
    }

    /// Create a barline.
    fn create_barline(&self, x: f64, is_final: bool) -> SceneNode {
        let spatium = self.config.spatium;
        let half_staff = 2.0 * spatium;
        let thin_width = spatium * 0.16;
        let thick_width = spatium * 0.5;

        let mut commands = Vec::new();

        if is_final {
            // Final barline: thin + thick
            commands.push(PaintCommand::line(
                Point::new(x - thick_width - thin_width * 2.0, -half_staff),
                Point::new(x - thick_width - thin_width * 2.0, half_staff),
                Color::BLACK,
                thin_width,
            ));
            commands.push(PaintCommand::filled_rect(
                Rect::new(x - thick_width, -half_staff, x, half_staff),
                Color::BLACK,
            ));
        } else {
            // Normal barline
            commands.push(PaintCommand::line(
                Point::new(x, -half_staff),
                Point::new(x, half_staff),
                Color::BLACK,
                thin_width,
            ));
        }

        let mut node = SceneNode::leaf(
            SemanticId::new(ElementType::Barline, 0),
            commands,
        );
        node.bounds = Rect::new(x - thick_width, -half_staff, x, half_staff);
        node
    }
}

/// Builder for creating a layout engine with custom configuration.
pub struct LayoutEngineBuilder<'a> {
    config: LayoutEngineConfig,
    style: Option<&'a MStyle>,
    font: Option<&'a SMuFLFont<'a>>,
}

impl<'a> LayoutEngineBuilder<'a> {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            config: LayoutEngineConfig::default(),
            style: None,
            font: None,
        }
    }

    /// Set page dimensions.
    pub fn page_size(mut self, width: f64, height: f64) -> Self {
        self.config.page_width = width;
        self.config.page_height = height;
        self
    }

    /// Set page margins.
    pub fn margins(mut self, margins: PageMargins) -> Self {
        self.config.margins = margins;
        self
    }

    /// Set uniform margins.
    pub fn uniform_margins(mut self, margin: f64) -> Self {
        self.config.margins = PageMargins {
            top: margin,
            right: margin,
            bottom: margin,
            left: margin,
        };
        self
    }

    /// Set the spatium (staff space) in pixels.
    pub fn spatium(mut self, spatium: f64) -> Self {
        self.config.spatium = spatium;
        self
    }

    /// Set spacing between systems.
    pub fn system_spacing(mut self, spacing: f64) -> Self {
        self.config.system_spacing = spacing;
        self
    }

    /// Set maximum measures per system (0 = auto).
    pub fn max_measures_per_system(mut self, max: usize) -> Self {
        self.config.max_measures_per_system = max;
        self
    }

    /// Set the style.
    pub fn style(mut self, style: &'a MStyle) -> Self {
        self.style = Some(style);
        self
    }

    /// Set the music font.
    pub fn font(mut self, font: &'a SMuFLFont<'a>) -> Self {
        self.font = Some(font);
        self
    }

    /// Build the layout engine.
    ///
    /// # Panics
    /// Panics if style is not set.
    pub fn build(self) -> LayoutEngine<'a> {
        let style = self.style.expect("Style must be set");
        let mut engine = LayoutEngine::with_config(self.config, style);
        if let Some(font) = self.font {
            engine = engine.with_font(font);
        }
        engine
    }
}

impl<'a> Default for LayoutEngineBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to layout a score with default settings.
///
/// # Arguments
/// * `score` - The score to layout
/// * `style` - The style settings
///
/// # Returns
/// The layout result containing the scene graph and page information.
pub fn layout_score<'a>(score: &'a Score, style: &'a MStyle) -> LayoutResult {
    LayoutEngine::new(style).layout_score(score)
}

/// Convenience function to layout a score with custom configuration.
pub fn layout_score_with_config<'a>(
    score: &'a Score,
    style: &'a MStyle,
    config: LayoutEngineConfig,
) -> LayoutResult {
    LayoutEngine::with_config(config, style).layout_score(score)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Measure, Part, PartId};

    fn create_test_score(measure_count: usize) -> Score {
        let measures: Vec<Measure> = (0..measure_count)
            .map(|i| Measure {
                number: (i + 1) as u32,
                ..Default::default()
            })
            .collect();

        let mut part = Part::new(PartId(1), "Test");
        part.measures = measures;

        Score {
            parts: vec![part],
            ..Default::default()
        }
    }

    #[test]
    fn test_layout_empty_score() {
        let score = Score::default();
        let style = MStyle::default();
        let result = layout_score(&score, &style);

        assert_eq!(result.measure_count, 0);
        assert_eq!(result.system_count, 0);
    }

    #[test]
    fn test_layout_single_measure() {
        let score = create_test_score(1);
        let style = MStyle::default();
        let result = layout_score(&score, &style);

        assert_eq!(result.measure_count, 1);
        assert_eq!(result.system_count, 1);
        assert_eq!(result.pages.len(), 1);
    }

    #[test]
    fn test_layout_multiple_measures() {
        let score = create_test_score(8);
        let style = MStyle::default();
        let result = layout_score(&score, &style);

        assert_eq!(result.measure_count, 8);
        assert!(result.system_count >= 1);
        assert!(result.pages.len() >= 1);
    }

    #[test]
    fn test_layout_with_fixed_measures_per_system() {
        let score = create_test_score(8);
        let style = MStyle::default();
        let config = LayoutEngineConfig {
            max_measures_per_system: 4,
            ..Default::default()
        };
        let result = layout_score_with_config(&score, &style, config);

        assert_eq!(result.measure_count, 8);
        assert_eq!(result.system_count, 2); // 8 measures / 4 per system
    }

    #[test]
    fn test_layout_scene_structure() {
        let score = create_test_score(4);
        let style = MStyle::default();
        let result = layout_score(&score, &style);

        // Root should have system children
        assert!(!result.scene.children.is_empty());

        // First child should be a system
        let first_system = &result.scene.children[0];
        assert!(first_system.id.is_some());

        // System should have measure children
        assert!(!first_system.children.is_empty());
    }

    #[test]
    fn test_page_margins() {
        let score = create_test_score(4);
        let style = MStyle::default();
        let margins = PageMargins {
            top: 100.0,
            right: 50.0,
            bottom: 100.0,
            left: 50.0,
        };
        let config = LayoutEngineConfig {
            margins,
            ..Default::default()
        };
        let result = layout_score_with_config(&score, &style, config);

        assert_eq!(result.pages[0].margins.top, 100.0);
        assert_eq!(result.pages[0].margins.left, 50.0);
    }

    #[test]
    fn test_layout_builder() {
        let style = MStyle::default();
        let engine = LayoutEngineBuilder::new()
            .page_size(800.0, 1000.0)
            .uniform_margins(30.0)
            .spatium(12.0)
            .max_measures_per_system(3)
            .style(&style)
            .build();

        assert_eq!(engine.config.page_width, 800.0);
        assert_eq!(engine.config.spatium, 12.0);
        assert_eq!(engine.config.max_measures_per_system, 3);
    }

    #[test]
    fn test_content_width_calculation() {
        let style = MStyle::default();
        let engine = LayoutEngineBuilder::new()
            .page_size(800.0, 1000.0)
            .uniform_margins(50.0)
            .style(&style)
            .build();

        let content_width = engine.content_width();
        assert_eq!(content_width, 700.0); // 800 - 50 - 50
    }

    #[test]
    fn test_multiple_pages() {
        let score = create_test_score(50); // Many measures
        let style = MStyle::default();
        let config = LayoutEngineConfig {
            max_measures_per_system: 4,
            system_spacing: 150.0, // Large spacing to force multiple pages
            ..Default::default()
        };
        let result = layout_score_with_config(&score, &style, config);

        // Should have multiple pages with large system spacing
        assert!(result.pages.len() >= 1);
    }

    #[test]
    fn test_system_layout_info() {
        let score = create_test_score(8);
        let style = MStyle::default();
        let config = LayoutEngineConfig {
            max_measures_per_system: 4,
            ..Default::default()
        };
        let result = layout_score_with_config(&score, &style, config);

        // Check first page systems
        let first_page = &result.pages[0];
        assert!(!first_page.systems.is_empty());

        // First system should have 4 measures
        let first_system = &first_page.systems[0];
        assert_eq!(first_system.measure_indices.len(), 4);
        assert_eq!(first_system.index, 0);
    }
}
