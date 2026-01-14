//! Element-specific layout implementations.
//!
//! This module provides the `Layout` trait and element-specific layout
//! implementations, replacing MuseScore's 6,719-line TLayout.cpp with
//! modular, trait-based dispatch.

use kurbo::{Point, Rect};

use crate::layout::context::LayoutContext;
use crate::layout::shape::Shape;
use crate::model::ElementId;

/// Layout data produced for a music element.
///
/// Contains the computed position, bounding box, and collision shape
/// for a single element after layout.
#[derive(Debug, Clone)]
pub struct LayoutData {
    /// Position relative to parent element (in points)
    pub position: Point,
    /// Bounding box for hit testing (in points)
    pub bbox: Rect,
    /// Detailed shape for collision detection
    pub shape: Shape,
    /// Child element layouts (for hierarchical elements)
    pub children: Vec<(ElementId, LayoutData)>,
}

impl LayoutData {
    /// Create new layout data.
    #[must_use]
    pub fn new(position: Point, bbox: Rect, shape: Shape) -> Self {
        Self {
            position,
            bbox,
            shape,
            children: Vec::new(),
        }
    }

    /// Create layout data with children.
    #[must_use]
    pub fn with_children(
        position: Point,
        bbox: Rect,
        shape: Shape,
        children: Vec<(ElementId, LayoutData)>,
    ) -> Self {
        Self {
            position,
            bbox,
            shape,
            children,
        }
    }

    /// Add a child element's layout data.
    pub fn add_child(&mut self, id: ElementId, layout: LayoutData) {
        self.children.push((id, layout));
    }

    /// Translate this layout data by an offset.
    #[must_use]
    pub fn translate(&self, offset: Point) -> Self {
        Self {
            position: Point::new(
                self.position.x + offset.x,
                self.position.y + offset.y,
            ),
            bbox: self.bbox.with_origin(Point::new(
                self.bbox.x0 + offset.x,
                self.bbox.y0 + offset.y,
            )),
            shape: self.shape.translate(offset),
            children: self.children.clone(),
        }
    }
}

/// Core layout trait for music elements.
///
/// All music notation elements implement this trait to provide their
/// layout logic. This replaces MuseScore's TLayout static factory class
/// with a more idiomatic Rust approach using trait dispatch.
///
/// # Implementation Strategy
///
/// - Implement directly for concrete types (Note, Harmony, Rest, etc.)
/// - Use enum dispatch via MusicElement for generic handling
/// - Each element type has a dedicated module (harmony.rs, chord.rs, etc.)
///
/// # Example
///
/// ```ignore
/// impl Layout for Harmony {
///     fn layout(&self, ctx: &LayoutContext) -> LayoutData {
///         // Chord symbol-specific layout logic
///         harmony::layout_harmony(self, ctx)
///     }
///
///     fn shape(&self, ctx: &LayoutContext) -> Shape {
///         harmony::harmony_shape(self, ctx)
///     }
/// }
/// ```
pub trait Layout {
    /// Compute layout for this element.
    ///
    /// Returns a `LayoutData` containing position, bounding box,
    /// and collision shape for the element.
    fn layout(&self, ctx: &LayoutContext) -> LayoutData;

    /// Get bounding shape for collision detection.
    ///
    /// Returns a `Shape` representing the collision boundary
    /// of this element. Used for horizontal spacing and autoplace.
    fn shape(&self, ctx: &LayoutContext) -> Shape;

    /// Get natural width of this element (before stretching).
    ///
    /// Default implementation uses the shape's bounding box width.
    fn natural_width(&self, ctx: &LayoutContext) -> f64 {
        self.shape(ctx).bbox().width()
    }
}

// Element-specific layout modules
pub mod accidentals_layout;
pub mod barline;
pub mod beam_layout;
pub mod chord;
pub mod clef;
pub mod dynamics;
pub mod keysig;
pub mod lyrics;
pub mod measure;
pub mod note;
pub mod rest;
pub mod timesig;

// Re-exports for convenient access
pub use accidentals_layout::{
    layout_accidentals, AccidentalInfo, AccidentalLayoutConfig, AccidentalPlacement,
};
pub use barline::{layout_barline, BarlineParams, BarlineType};
pub use beam_layout::{layout_beam, BeamLayout, BeamLayoutConfig, BeamNote};
pub use chord::{layout_chord, ChordNote, ChordParams, StemDirection};
pub use clef::{layout_clef, ClefOctave, ClefParams, ClefType};
pub use dynamics::{layout_dynamic, DynamicType, DynamicsAlign, DynamicsParams, DynamicsPlacement};
pub use keysig::{layout_keysig, ClefContext, KeySigParams, KeySigType};
pub use lyrics::{
    layout_lyrics, layout_lyrics_dash, layout_melisma, LyricsParams, LyricsPlacement, SyllabicType,
};
pub use measure::{layout_measure, layout_system, MeasureParams};
pub use note::{layout_note, note_shape, Accidental, NoteDuration, NoteParams};
pub use rest::{layout_multi_measure_rest, layout_rest, RestDuration, RestParams};
pub use timesig::{layout_timesig, TimeSigParams, TimeSigType};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_layout_data_new() {
        let pos = Point::new(10.0, 20.0);
        let bbox = Rect::new(10.0, 20.0, 50.0, 40.0);
        let shape = Shape::from_rect(bbox);

        let data = LayoutData::new(pos, bbox, shape);

        assert_eq!(data.position, pos);
        assert_eq!(data.bbox, bbox);
        assert_eq!(data.children.len(), 0);
    }

    #[test]
    fn test_layout_data_translate() {
        let pos = Point::new(10.0, 20.0);
        let bbox = Rect::new(10.0, 20.0, 50.0, 40.0);
        let shape = Shape::from_rect(bbox);
        let data = LayoutData::new(pos, bbox, shape);

        let offset = Point::new(5.0, 3.0);
        let translated = data.translate(offset);

        assert_eq!(translated.position, Point::new(15.0, 23.0));
        assert_eq!(translated.bbox.x0, 15.0);
        assert_eq!(translated.bbox.y0, 23.0);
    }
}
