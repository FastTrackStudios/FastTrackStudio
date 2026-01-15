//! Rehearsal Mark / Section Label Layout
//!
//! Provides layout functions for rendering section markers like "VS 1", "CH 2", "BR 1",
//! "INTRO", "OUTRO", etc. These are drawn as capsule-shaped labels (rounded rectangles
//! with text inside).
//!
//! # Rendering via Vello
//!
//! The layout functions generate `PaintCommand`s that the VelloSceneRenderer handles:
//! - `PaintCommand::Rect` with `corner_radius` for the capsule background
//! - `PaintCommand::Text` for the label text
//!
//! Vello efficiently renders these using GPU-accelerated vector graphics.

use kurbo::{Point, Rect};
use peniko::Color;

use crate::layout::context::LayoutContext;
use crate::scene::node::SceneNode;
use crate::scene::paint::PaintCommand;
use crate::ui::{format_rehearsal_label, CapsuleLabelConfig, CapsuleLabelMode, ComputedCapsuleLabel};

/// Parameters for rehearsal mark layout.
#[derive(Debug, Clone)]
pub struct RehearsalMarkParams {
    /// Unique identifier
    pub id: u64,
    /// The label text to display (e.g., "VS 1", "INTRO")
    pub text: String,
    /// Position X (left edge of capsule or center, depending on alignment)
    pub x: f64,
    /// Position Y (baseline or top, depending on config)
    pub y: f64,
    /// Style configuration
    pub style: RehearsalMarkStyle,
}

impl Default for RehearsalMarkParams {
    fn default() -> Self {
        Self {
            id: 0,
            text: String::new(),
            x: 0.0,
            y: 0.0,
            style: RehearsalMarkStyle::default(),
        }
    }
}

/// Style configuration for rehearsal marks.
#[derive(Debug, Clone)]
pub struct RehearsalMarkStyle {
    /// Background color of the capsule
    pub background_color: Color,
    /// Text color
    pub text_color: Color,
    /// Border color (None = no border)
    pub border_color: Option<Color>,
    /// Border width
    pub border_width: f64,
    /// Font size for the text
    pub font_size: f64,
    /// Horizontal padding inside the capsule
    pub padding_h: f64,
    /// Vertical padding inside the capsule
    pub padding_v: f64,
    /// Estimated character width ratio (actual width / font_size)
    pub char_width_ratio: f64,
}

impl Default for RehearsalMarkStyle {
    fn default() -> Self {
        Self {
            background_color: Color::from_rgb8(66, 66, 66),
            text_color: Color::WHITE,
            border_color: None,
            border_width: 1.0,
            font_size: 12.0,
            padding_h: 8.0,
            padding_v: 4.0,
            char_width_ratio: 0.6,
        }
    }
}

/// Layout data returned from rehearsal mark layout.
#[derive(Debug, Clone)]
pub struct RehearsalMarkLayoutData {
    /// Bounding box of the entire element
    pub bounds: Rect,
    /// Width of the capsule
    pub width: f64,
    /// Height of the capsule
    pub height: f64,
}

/// Layout a rehearsal mark / section label.
///
/// Returns layout data and a scene node containing the capsule and text.
///
/// # Arguments
///
/// * `params` - Rehearsal mark parameters
/// * `_ctx` - Layout context (unused currently, but available for font metrics)
///
/// # Returns
///
/// A tuple of (layout data, scene node).
#[must_use]
pub fn layout_rehearsal_mark(
    params: &RehearsalMarkParams,
    _ctx: &LayoutContext<'_>,
) -> (RehearsalMarkLayoutData, SceneNode) {
    let style = &params.style;

    // Estimate text width based on character count and font size
    // In a full implementation, we'd use actual font metrics from glyphon/skrifa
    let estimated_text_width = params.text.len() as f64 * style.font_size * style.char_width_ratio;

    // Compute capsule dimensions
    let capsule_width = estimated_text_width + style.padding_h * 2.0;
    let capsule_height = style.font_size + style.padding_v * 2.0;
    let corner_radius = capsule_height / 4.0;

    // Position the capsule (params.x, params.y is top-left)
    let capsule_x = params.x;
    let capsule_y = params.y;

    // Create the capsule rectangle
    let capsule_rect = Rect::new(
        capsule_x,
        capsule_y,
        capsule_x + capsule_width,
        capsule_y + capsule_height,
    );

    // Build paint commands
    let mut commands = Vec::new();

    // 1. Background capsule
    commands.push(PaintCommand::Rect {
        rect: capsule_rect,
        fill: Some(style.background_color),
        stroke: style.border_color,
        stroke_width: style.border_width,
        corner_radius: Some(corner_radius),
    });

    // 2. Text (centered in capsule)
    let text_x = capsule_x + style.padding_h;
    let text_y = capsule_y + style.padding_v + style.font_size * 0.85; // Approximate baseline
    commands.push(PaintCommand::text(
        params.text.clone(),
        "sans-serif",
        style.font_size,
        Point::new(text_x, text_y),
        style.text_color,
    ));

    // Create layout data
    let layout_data = RehearsalMarkLayoutData {
        bounds: capsule_rect,
        width: capsule_width,
        height: capsule_height,
    };

    // Create scene node
    let node = SceneNode::anonymous_leaf(commands);

    (layout_data, node)
}

/// Convenience function to create a section label with standard formatting.
///
/// Uses the format_rehearsal_label function to create consistent labels:
/// - Intro/Outro: Full name uppercase ("INTRO", "OUTRO")
/// - Others: Abbreviation + number ("VS 1", "CH 2")
///
/// # Arguments
///
/// * `section_type` - Type name (e.g., "Verse", "Chorus", "Bridge")
/// * `abbreviation` - Short form (e.g., "VS", "CH", "BR")
/// * `number` - Optional section number
/// * `x` - X position
/// * `y` - Y position
/// * `style` - Style configuration
/// * `ctx` - Layout context
///
/// # Returns
///
/// A tuple of (layout data, scene node).
#[must_use]
pub fn layout_section_label(
    section_type: &str,
    abbreviation: &str,
    number: Option<u32>,
    x: f64,
    y: f64,
    style: Option<RehearsalMarkStyle>,
    ctx: &LayoutContext<'_>,
) -> (RehearsalMarkLayoutData, SceneNode) {
    let text = format_rehearsal_label(section_type, abbreviation, number);
    let params = RehearsalMarkParams {
        id: 0,
        text,
        x,
        y,
        style: style.unwrap_or_default(),
    };
    layout_rehearsal_mark(&params, ctx)
}

/// Parameters for margin-positioned section labels.
#[derive(Debug, Clone)]
pub struct MarginLabelParams {
    /// Section type name (e.g., "Verse", "Chorus")
    pub section_type: String,
    /// Abbreviation (e.g., "VS", "CH")
    pub abbreviation: String,
    /// Optional section number
    pub number: Option<u32>,
    /// Left edge of the page
    pub page_x: f64,
    /// Available margin width (distance from page edge to content)
    pub margin_width: f64,
    /// Top of the staff (Y coordinate)
    pub staff_y: f64,
    /// Height of the staff (4 * spatium)
    pub staff_height: f64,
    /// Horizontal padding from page edge
    pub padding_h: f64,
    /// Vertical padding from staff top/bottom
    pub padding_v: f64,
    /// Style configuration
    pub style: RehearsalMarkStyle,
}

impl Default for MarginLabelParams {
    fn default() -> Self {
        Self {
            section_type: String::new(),
            abbreviation: String::new(),
            number: None,
            page_x: 0.0,
            margin_width: 50.0,
            staff_y: 0.0,
            staff_height: 20.0,
            padding_h: 4.0,
            padding_v: 3.0,
            style: RehearsalMarkStyle::default(),
        }
    }
}

/// Layout a section label that fits within the left margin of a staff.
///
/// The label will be sized to fit within the available margin space,
/// scaling the text down if necessary.
///
/// # Arguments
///
/// * `params` - Margin label parameters
/// * `_ctx` - Layout context
///
/// # Returns
///
/// A tuple of (layout data, scene node).
#[must_use]
pub fn layout_margin_label(
    params: &MarginLabelParams,
    _ctx: &LayoutContext<'_>,
) -> (RehearsalMarkLayoutData, SceneNode) {
    let text = format_rehearsal_label(&params.section_type, &params.abbreviation, params.number);
    let style = &params.style;

    // Calculate available space in the margin
    let capsule_x = params.page_x + params.padding_h;
    let available_width = params.margin_width - (params.padding_h * 2.0);
    let available_height = params.staff_height - (params.padding_v * 2.0);
    let capsule_y = params.staff_y + params.padding_v;

    // Use fixed dimensions for the capsule
    let capsule_width = available_width;
    let capsule_height = available_height;
    let corner_radius = capsule_height / 4.0;

    // Estimate text width and scale if needed
    let estimated_text_width = text.len() as f64 * style.font_size * style.char_width_ratio;
    let internal_padding = 4.0;
    let text_available_width = capsule_width - internal_padding * 2.0;

    // Calculate text scale to fit
    let text_scale = if estimated_text_width > text_available_width {
        text_available_width / estimated_text_width
    } else {
        1.0
    };
    let scaled_font_size = style.font_size * text_scale;

    // Create the capsule rectangle
    let capsule_rect = Rect::new(
        capsule_x,
        capsule_y,
        capsule_x + capsule_width,
        capsule_y + capsule_height,
    );

    // Build paint commands
    let mut commands = Vec::new();

    // 1. Background capsule
    commands.push(PaintCommand::Rect {
        rect: capsule_rect,
        fill: Some(style.background_color),
        stroke: style.border_color,
        stroke_width: style.border_width,
        corner_radius: Some(corner_radius),
    });

    // 2. Text (centered in capsule)
    let scaled_text_width = text.len() as f64 * scaled_font_size * style.char_width_ratio;
    let text_x = capsule_x + (capsule_width - scaled_text_width) / 2.0;
    let text_y = capsule_y + (capsule_height + scaled_font_size * 0.7) / 2.0; // Vertically center
    commands.push(PaintCommand::text(
        text,
        "sans-serif",
        scaled_font_size,
        Point::new(text_x, text_y),
        style.text_color,
    ));

    // Create layout data
    let layout_data = RehearsalMarkLayoutData {
        bounds: capsule_rect,
        width: capsule_width,
        height: capsule_height,
    };

    // Create scene node
    let node = SceneNode::anonymous_leaf(commands);

    (layout_data, node)
}

/// Pre-defined color themes for rehearsal marks.
pub mod themes {
    use peniko::Color;
    use super::RehearsalMarkStyle;

    /// Dark theme with white text on dark background (default).
    #[must_use]
    pub fn dark() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::from_rgb8(66, 66, 66),
            text_color: Color::WHITE,
            border_color: None,
            ..Default::default()
        }
    }

    /// Light theme with dark text on light background.
    #[must_use]
    pub fn light() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::from_rgb8(230, 230, 230),
            text_color: Color::from_rgb8(33, 33, 33),
            border_color: Some(Color::from_rgb8(180, 180, 180)),
            ..Default::default()
        }
    }

    /// Blue accent theme.
    #[must_use]
    pub fn blue() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::from_rgb8(41, 98, 255),
            text_color: Color::WHITE,
            border_color: None,
            ..Default::default()
        }
    }

    /// Green accent theme.
    #[must_use]
    pub fn green() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::from_rgb8(46, 125, 50),
            text_color: Color::WHITE,
            border_color: None,
            ..Default::default()
        }
    }

    /// Purple accent theme.
    #[must_use]
    pub fn purple() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::from_rgb8(106, 27, 154),
            text_color: Color::WHITE,
            border_color: None,
            ..Default::default()
        }
    }

    /// Outline-only style (transparent background with border).
    #[must_use]
    pub fn outline() -> RehearsalMarkStyle {
        RehearsalMarkStyle {
            background_color: Color::TRANSPARENT,
            text_color: Color::BLACK,
            border_color: Some(Color::BLACK),
            border_width: 1.5,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_ctx<'a>() -> LayoutContext<'a> {
        use crate::style::MStyle;
        LayoutContext::new(5.0, &MStyle::default(), None)
    }

    #[test]
    fn test_basic_rehearsal_mark() {
        let ctx = make_ctx();
        let params = RehearsalMarkParams {
            id: 1,
            text: "VS 1".to_string(),
            x: 100.0,
            y: 50.0,
            ..Default::default()
        };

        let (layout, node) = layout_rehearsal_mark(&params, &ctx);

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert!(!node.commands.is_empty());
    }

    #[test]
    fn test_section_label_verse() {
        let ctx = make_ctx();
        let (layout, _node) = layout_section_label("Verse", "VS", Some(1), 0.0, 0.0, None, &ctx);
        assert!(layout.width > 0.0);
    }

    #[test]
    fn test_section_label_intro() {
        let ctx = make_ctx();
        let (layout, _node) = layout_section_label("Intro", "IN", None, 0.0, 0.0, None, &ctx);
        // Intro should produce "INTRO" which is wider than "VS 1"
        assert!(layout.width > 0.0);
    }

    #[test]
    fn test_themes() {
        let dark = themes::dark();
        let light = themes::light();
        let outline = themes::outline();

        assert!(dark.border_color.is_none());
        assert!(light.border_color.is_some());
        assert!(outline.background_color == Color::TRANSPARENT);
    }
}
