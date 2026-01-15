//! Slur and tie layout module.
//!
//! Implements layout for slurs (phrase marks) and ties (note connections)
//! following MuseScore's slur/tie layout algorithm using cubic Bezier curves.

use kurbo::{BezPath, Point, Rect, Vec2};
use peniko::Color;

use crate::scene::{PaintCommand, SceneNode};
use crate::scene::id::{ElementType, SemanticId};

/// Direction of a slur or tie.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SlurDirection {
    /// Curve goes above the notes
    #[default]
    Up,
    /// Curve goes below the notes
    Down,
    /// Automatically determine based on note positions
    Auto,
}

/// Style of slur/tie rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SlurStyle {
    /// Solid filled curve
    #[default]
    Solid,
    /// Dashed line (for editorial marks)
    Dashed,
    /// Dotted line
    Dotted,
}

/// Configuration for slur/tie layout.
#[derive(Debug, Clone)]
pub struct SlurTieConfig {
    /// Minimum shoulder height as fraction of spatium
    pub min_shoulder_height: f64,
    /// Maximum shoulder height as fraction of spatium
    pub max_shoulder_height: f64,
    /// Shoulder width as fraction of total length (0.0-1.0)
    pub shoulder_width: f64,
    /// Line thickness as fraction of spatium
    pub thickness: f64,
    /// Distance from notehead to curve endpoint
    pub endpoint_offset: f64,
    /// Style of the curve
    pub style: SlurStyle,
}

impl Default for SlurTieConfig {
    fn default() -> Self {
        Self {
            min_shoulder_height: 0.4,
            max_shoulder_height: 1.5,
            shoulder_width: 0.6,
            thickness: 0.12,
            endpoint_offset: 0.4,
            style: SlurStyle::Solid,
        }
    }
}

/// Endpoint information for a slur or tie.
#[derive(Debug, Clone)]
pub struct SlurEndpoint {
    /// X position
    pub x: f64,
    /// Y position of the notehead center
    pub y: f64,
    /// Whether this note has stem going up
    pub stem_up: bool,
}

/// Result of slur/tie layout.
#[derive(Debug, Clone)]
pub struct SlurTieLayout {
    /// Paint commands for the slur/tie
    pub commands: Vec<PaintCommand>,
    /// Bounding box
    pub bbox: Rect,
    /// Scene node
    pub scene: SceneNode,
}

/// Layout a tie between two notes.
///
/// Ties connect two notes of the same pitch, typically within a measure
/// or across a barline.
///
/// # Arguments
/// * `start` - Start endpoint (first note)
/// * `end` - End endpoint (second note)
/// * `direction` - Curve direction
/// * `id` - Semantic ID
/// * `spatium` - Staff space height
/// * `config` - Configuration
pub fn layout_tie(
    start: &SlurEndpoint,
    end: &SlurEndpoint,
    direction: SlurDirection,
    id: u64,
    spatium: f64,
    config: &SlurTieConfig,
) -> SlurTieLayout {
    layout_curve(start, end, direction, ElementType::Tie, id, spatium, config)
}

/// Layout a slur between two notes.
///
/// Slurs indicate phrasing and connect multiple notes.
///
/// # Arguments
/// * `start` - Start endpoint (first note)
/// * `end` - End endpoint (last note)
/// * `direction` - Curve direction
/// * `id` - Semantic ID
/// * `spatium` - Staff space height
/// * `config` - Configuration
pub fn layout_slur(
    start: &SlurEndpoint,
    end: &SlurEndpoint,
    direction: SlurDirection,
    id: u64,
    spatium: f64,
    config: &SlurTieConfig,
) -> SlurTieLayout {
    layout_curve(start, end, direction, ElementType::Slur, id, spatium, config)
}

/// Internal function to layout a slur or tie curve.
fn layout_curve(
    start: &SlurEndpoint,
    end: &SlurEndpoint,
    direction: SlurDirection,
    element_type: ElementType,
    id: u64,
    spatium: f64,
    config: &SlurTieConfig,
) -> SlurTieLayout {
    let mut commands = Vec::new();

    // Determine curve direction
    let is_up = match direction {
        SlurDirection::Up => true,
        SlurDirection::Down => false,
        SlurDirection::Auto => {
            // If both stems go up, curve goes up; otherwise down
            start.stem_up && end.stem_up
        }
    };

    // Calculate start and end points with offset
    let offset = config.endpoint_offset * spatium;
    let y_offset = if is_up { -offset } else { offset };

    let p1 = Point::new(start.x, start.y + y_offset);
    let p2 = Point::new(end.x, end.y + y_offset);

    // Normalize to origin for calculations
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;

    if dx.abs() < 0.001 {
        // Start and end are at same X position, can't draw curve
        return SlurTieLayout {
            commands: Vec::new(),
            bbox: Rect::ZERO,
            scene: SceneNode::group(SemanticId::new(element_type, id)),
        };
    }

    // Calculate angle of the line from start to end
    let angle = dy.atan2(dx);

    // Calculate length in spatiums for shoulder height computation
    let length = (dx * dx + dy * dy).sqrt();
    let length_sp = length / spatium;

    // Compute shoulder height based on length (MuseScore formula)
    let mut shoulder_h = config.min_shoulder_height * spatium
        + spatium * 0.3 * (length_sp - 1.0).abs().sqrt();
    shoulder_h = shoulder_h.clamp(
        config.min_shoulder_height * spatium,
        config.max_shoulder_height * spatium,
    );

    // Direction multiplier
    let dir_mult = if is_up { -1.0 } else { 1.0 };
    shoulder_h *= dir_mult;

    // Compute Bezier control points
    // The control points are positioned at shoulder_width fraction of the length
    let shoulder_w = config.shoulder_width;
    let bezier1_x = length * (1.0 - shoulder_w) * 0.5;
    let bezier2_x = bezier1_x + length * shoulder_w;

    // In the rotated coordinate system (horizontal)
    let bezier1_local = Point::new(bezier1_x, -shoulder_h);
    let bezier2_local = Point::new(bezier2_x, -shoulder_h);
    let end_local = Point::new(length, 0.0);

    // Transform back to original coordinate system
    let cos_a = angle.cos();
    let sin_a = angle.sin();

    let transform = |p: Point| -> Point {
        Point::new(
            p1.x + p.x * cos_a - p.y * sin_a,
            p1.y + p.x * sin_a + p.y * cos_a,
        )
    };

    let bezier1 = transform(bezier1_local);
    let bezier2 = transform(bezier2_local);
    let p2_final = transform(end_local);

    // Create the curve path
    let thickness = config.thickness * spatium;

    match config.style {
        SlurStyle::Solid => {
            // For solid slurs, we draw two cubic Bezier curves forming a filled shape
            let half_thick = thickness / 2.0;

            // Offset control points for inner and outer curves
            let offset_vec = Vec2::new(-sin_a, cos_a) * half_thick * dir_mult.abs();

            // Outer curve
            let mut path = BezPath::new();
            path.move_to(p1);
            path.curve_to(
                bezier1 - offset_vec,
                bezier2 - offset_vec,
                p2_final,
            );
            // Inner curve (reverse direction)
            path.curve_to(
                bezier2 + offset_vec,
                bezier1 + offset_vec,
                p1,
            );
            path.close_path();

            commands.push(PaintCommand::filled_path(path, Color::BLACK));
        }
        SlurStyle::Dashed | SlurStyle::Dotted => {
            // For dashed/dotted, just stroke the curve
            let mut path = BezPath::new();
            path.move_to(p1);
            path.curve_to(bezier1, bezier2, p2_final);

            let dash_pattern = match config.style {
                SlurStyle::Dashed => vec![spatium * 0.5, spatium * 0.25],
                SlurStyle::Dotted => vec![spatium * 0.1, spatium * 0.2],
                _ => vec![],
            };

            commands.push(PaintCommand::Stroke {
                path,
                color: Color::BLACK,
                width: thickness,
                line_cap: crate::scene::paint::LineCap::Round,
                line_join: crate::scene::paint::LineJoin::Round,
                dash_pattern,
                dash_offset: 0.0,
            });
        }
    }

    // Calculate bounding box
    let min_x = p1.x.min(bezier1.x).min(bezier2.x).min(p2_final.x);
    let max_x = p1.x.max(bezier1.x).max(bezier2.x).max(p2_final.x);
    let min_y = p1.y.min(bezier1.y).min(bezier2.y).min(p2_final.y) - thickness;
    let max_y = p1.y.max(bezier1.y).max(bezier2.y).max(p2_final.y) + thickness;
    let bbox = Rect::new(min_x, min_y, max_x, max_y);

    // Create scene node
    let mut scene = SceneNode::group(SemanticId::new(element_type, id));
    scene.commands = commands.clone();
    scene.bounds = bbox;

    SlurTieLayout {
        commands,
        bbox,
        scene,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tie_basic() {
        let start = SlurEndpoint {
            x: 0.0,
            y: 0.0,
            stem_up: true,
        };
        let end = SlurEndpoint {
            x: 20.0,
            y: 0.0,
            stem_up: true,
        };

        let config = SlurTieConfig::default();
        let result = layout_tie(&start, &end, SlurDirection::Up, 1, 5.0, &config);

        assert!(!result.commands.is_empty());
        assert!(result.bbox.width() > 0.0);
    }

    #[test]
    fn test_slur_basic() {
        let start = SlurEndpoint {
            x: 0.0,
            y: 0.0,
            stem_up: true,
        };
        let end = SlurEndpoint {
            x: 50.0,
            y: -5.0,
            stem_up: false,
        };

        let config = SlurTieConfig::default();
        let result = layout_slur(&start, &end, SlurDirection::Up, 1, 5.0, &config);

        assert!(!result.commands.is_empty());
        assert!(result.bbox.width() > 0.0);
    }

    #[test]
    fn test_slur_direction_auto() {
        let start = SlurEndpoint {
            x: 0.0,
            y: 0.0,
            stem_up: true,
        };
        let end = SlurEndpoint {
            x: 30.0,
            y: 0.0,
            stem_up: true,
        };

        let config = SlurTieConfig::default();
        let result = layout_slur(&start, &end, SlurDirection::Auto, 1, 5.0, &config);

        // With both stems up, the slur should curve up (negative Y for shoulder)
        // The bounding box should extend above the endpoints
        assert!(result.bbox.y0 < 0.0);
    }

    #[test]
    fn test_slur_dashed() {
        let start = SlurEndpoint {
            x: 0.0,
            y: 0.0,
            stem_up: true,
        };
        let end = SlurEndpoint {
            x: 30.0,
            y: 0.0,
            stem_up: true,
        };

        let config = SlurTieConfig {
            style: SlurStyle::Dashed,
            ..Default::default()
        };
        let result = layout_slur(&start, &end, SlurDirection::Up, 1, 5.0, &config);

        // Should produce a stroke command with dash pattern
        assert!(!result.commands.is_empty());
        match &result.commands[0] {
            PaintCommand::Stroke { dash_pattern, .. } => {
                assert!(!dash_pattern.is_empty());
            }
            _ => panic!("Expected Stroke command"),
        }
    }

    #[test]
    fn test_tie_same_position_no_crash() {
        let start = SlurEndpoint {
            x: 10.0,
            y: 0.0,
            stem_up: true,
        };
        let end = SlurEndpoint {
            x: 10.0, // Same X as start
            y: 0.0,
            stem_up: true,
        };

        let config = SlurTieConfig::default();
        let result = layout_tie(&start, &end, SlurDirection::Up, 1, 5.0, &config);

        // Should return empty result without crashing
        assert!(result.commands.is_empty());
    }
}
