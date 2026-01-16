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
use vello::peniko::Color;

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
/// with multiline support for long text. The font is maximized to fill
/// the available space. Section letters (A, B, C, D) are placed on a new line.
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
    let raw_text = format_rehearsal_label(&params.section_type, &params.abbreviation, params.number);
    let style = &params.style;

    // Calculate available space in the margin
    let capsule_x = params.page_x + params.padding_h;
    let available_width = params.margin_width - (params.padding_h * 2.0);
    // Align top of capsule with top of staff
    let capsule_y = params.staff_y;

    let internal_padding = 4.0;
    let text_available_width = available_width - internal_padding * 2.0;
    let line_height_ratio = 1.3; // Line height relative to font size

    // Split text into lines for multiline layout
    let lines = split_into_lines(&raw_text, text_available_width, style.char_width_ratio);
    let num_lines = lines.len().max(1);

    // Calculate optimal font size that fits all lines
    let max_single_line_font_size = params.staff_height * 0.7; // Max for single line
    let max_multiline_font_size = params.staff_height / (num_lines as f64 * line_height_ratio) * 1.2;

    // For each line, calculate the max font size that fits width
    let mut optimal_font_size = if num_lines == 1 {
        max_single_line_font_size
    } else {
        max_multiline_font_size
    };

    for line in &lines {
        if !line.is_empty() {
            let line_width = line.len() as f64 * optimal_font_size * style.char_width_ratio;
            if line_width > text_available_width {
                let scale = text_available_width / (line.len() as f64 * style.char_width_ratio);
                optimal_font_size = optimal_font_size.min(scale);
            }
        }
    }

    // Calculate capsule height - default to staff height, expand if needed for multiline
    let total_text_height = num_lines as f64 * optimal_font_size * line_height_ratio;
    let capsule_height = (total_text_height + params.padding_v * 2.0).max(params.staff_height);
    let capsule_width = available_width;
    let corner_radius = (capsule_height / (num_lines as f64 + 2.0)).min(capsule_height / 4.0);

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

    // 2. Text lines (centered in capsule)
    // Use a more accurate line height calculation
    let line_spacing = optimal_font_size * 0.2; // Spacing between lines
    let total_text_block_height = if num_lines == 1 {
        optimal_font_size
    } else {
        (num_lines as f64 * optimal_font_size) + ((num_lines - 1) as f64 * line_spacing)
    };

    // Calculate the starting Y position to vertically center all lines
    // The text baseline is at the bottom of the text, so we offset by 0.75 * font_size
    let block_top_y = capsule_y + (capsule_height - total_text_block_height) / 2.0;

    // Capsule center X for horizontal text centering
    let capsule_center_x = capsule_x + capsule_width / 2.0;

    for (i, line) in lines.iter().enumerate() {
        // Position vertically - baseline is at 0.75 of font size from top
        let line_top = block_top_y + (i as f64 * (optimal_font_size + line_spacing));
        let text_y = line_top + optimal_font_size * 0.75;

        // Use centered text - renderer will handle actual font metrics for centering
        commands.push(PaintCommand::text_centered(
            line.clone(),
            "sans-serif",
            optimal_font_size,
            Point::new(capsule_center_x, text_y),
            style.text_color,
        ));
    }

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

/// Split text into lines for multiline layout.
///
/// Rules:
/// - Section letters (single A-D at end) go on their own line
/// - Words are broken at spaces
/// - Long words that don't fit are kept together (will be scaled down)
fn split_into_lines(text: &str, available_width: f64, char_width_ratio: f64) -> Vec<String> {
    let mut lines = Vec::new();

    // Check for section letter at end (e.g., "CH 1 B" -> ["CH 1", "B"])
    let parts: Vec<&str> = text.split_whitespace().collect();
    if let (Some(last), true) = (parts.last(), parts.len() >= 2) {
        // Check if last part is a single letter A-Z (section identifier)
        if last.len() == 1 && last.chars().next().map_or(false, |c| c.is_ascii_uppercase()) {
            // Put section letter on its own line
            let main_text: String = parts[..parts.len() - 1].join(" ");
            lines.push(main_text);
            lines.push((*last).to_string());
            return lines;
        }
    }

    // Check if text fits on one line
    let estimated_width = text.len() as f64 * 12.0 * char_width_ratio; // Use base font size for estimation
    if estimated_width <= available_width {
        lines.push(text.to_string());
        return lines;
    }

    // Split at spaces for multi-word text
    if parts.len() >= 2 {
        // Try splitting into two lines at the middle
        let mid = parts.len() / 2;
        let line1: String = parts[..mid].join(" ");
        let line2: String = parts[mid..].join(" ");
        lines.push(line1);
        lines.push(line2);
    } else {
        // Single word - keep as is (will be scaled down)
        lines.push(text.to_string());
    }

    lines
}

/// Pre-defined color themes for rehearsal marks.
pub mod themes {
    use vello::peniko::Color;
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
    use crate::layout::context::LayoutConfiguration;

    fn make_ctx() -> LayoutContext<'static> {
        use crate::style::MStyle;
        let style = Box::leak(Box::new(MStyle::default()));
        LayoutContext::new_for_test(LayoutConfiguration::default(), style)
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
