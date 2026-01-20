//! System prefix rendering (clef and time signature).
//!
//! This module extracts the duplicated prefix rendering logic from
//! `layout_paginated` and `layout_continuous` into reusable functions.

use crate::engraver::layout::context::LayoutContext;
use crate::engraver::layout::tlayout::{
    layout_clef, layout_timesig, ClefParams, ClefType, TimeSigParams, TimeSigType,
};
use crate::engraver::scene::node::SceneNode;
use kurbo::Affine;

/// Context for rendering system prefix (clef and time signature).
#[derive(Debug, Clone)]
pub struct PrefixRenderContext {
    /// Starting x position for prefix elements.
    pub x: f64,
    /// Y position of the staff.
    pub staff_y: f64,
    /// Spatium (staff space) in points.
    pub spatium: f64,
    /// Whether to render the clef.
    pub include_clef: bool,
    /// Whether to render the time signature.
    pub include_time_sig: bool,
    /// Time signature (numerator, denominator).
    pub time_signature: (u8, u8),
    /// Width of the clef element.
    pub clef_width: f64,
    /// Width of the time signature element.
    pub time_sig_width: f64,
    /// Page number (for metadata, optional in continuous mode).
    pub page_number: Option<u32>,
}

/// Result of prefix rendering.
#[derive(Debug)]
pub struct PrefixRenderResult {
    /// Rendered prefix nodes (clef and/or time signature).
    pub nodes: Vec<SceneNode>,
    /// Next ID counter value.
    pub next_id: u64,
    /// Total width consumed by prefix elements.
    pub total_width: f64,
}

/// Render system prefix (clef and time signature).
///
/// Returns the rendered nodes and the total width consumed.
pub fn render_system_prefix(
    ctx: &PrefixRenderContext,
    mut id_counter: u64,
    layout_ctx: &LayoutContext<'_>,
) -> PrefixRenderResult {
    let mut nodes = Vec::new();
    let mut prefix_x = ctx.x;

    // Render clef if requested
    if ctx.include_clef {
        let clef_params = ClefParams {
            id: id_counter,
            clef_type: ClefType::Treble,
            ..Default::default()
        };
        id_counter += 1;

        let (_, mut clef_node) = layout_clef(&clef_params, layout_ctx);

        // Position clef on staff (middle line = 2 spatiums from top)
        clef_node.transform = Affine::translate((prefix_x, ctx.staff_y + 2.0 * ctx.spatium));

        // Add page metadata if available
        if let Some(page) = ctx.page_number {
            clef_node
                .metadata
                .insert("page".to_string(), page.to_string());
        }

        nodes.push(clef_node);
        prefix_x += ctx.clef_width;
    }

    // Render time signature if requested
    if ctx.include_time_sig {
        let ts_params = TimeSigParams {
            id: id_counter,
            sig_type: TimeSigType::Numeric {
                numerator: ctx.time_signature.0,
                denominator: ctx.time_signature.1,
            },
            ..Default::default()
        };
        id_counter += 1;

        let (_, mut ts_node) = layout_timesig(&ts_params, layout_ctx);
        ts_node.transform = Affine::translate((prefix_x, ctx.staff_y + 2.0 * ctx.spatium));

        // Add page metadata if available
        if let Some(page) = ctx.page_number {
            ts_node
                .metadata
                .insert("page".to_string(), page.to_string());
        }

        nodes.push(ts_node);
        prefix_x += ctx.time_sig_width;
    }

    let total_width = prefix_x - ctx.x;

    PrefixRenderResult {
        nodes,
        next_id: id_counter,
        total_width,
    }
}

/// Calculate prefix width without rendering.
///
/// This is useful for layout calculations before rendering.
#[must_use]
pub fn calculate_prefix_width(
    spatium: f64,
    include_clef: bool,
    include_time_sig: bool,
) -> (f64, f64, f64) {
    let clef_spacing = 0.5 * spatium;
    let time_sig_spacing = 0.8 * spatium;

    let clef_width = if include_clef {
        ClefType::Treble.width() * spatium + clef_spacing
    } else {
        0.0
    };

    let time_sig_width = if include_time_sig {
        2.0 * spatium + time_sig_spacing
    } else {
        0.0
    };

    let total_width = clef_width + time_sig_width;

    (clef_width, time_sig_width, total_width)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_prefix_width_both() {
        let spatium = 5.0;
        let (clef_w, time_sig_w, total) = calculate_prefix_width(spatium, true, true);

        // Clef: ClefType::Treble.width() * 5.0 + 2.5 = ~8 spatiums * 5 + 2.5
        assert!(clef_w > 0.0);
        // Time sig: 2.0 * 5.0 + 4.0 = 14.0
        assert!((time_sig_w - 14.0).abs() < 0.01);
        assert!((total - (clef_w + time_sig_w)).abs() < 0.01);
    }

    #[test]
    fn test_calculate_prefix_width_clef_only() {
        let spatium = 5.0;
        let (clef_w, time_sig_w, total) = calculate_prefix_width(spatium, true, false);

        assert!(clef_w > 0.0);
        assert_eq!(time_sig_w, 0.0);
        assert_eq!(total, clef_w);
    }

    #[test]
    fn test_calculate_prefix_width_none() {
        let spatium = 5.0;
        let (clef_w, time_sig_w, total) = calculate_prefix_width(spatium, false, false);

        assert_eq!(clef_w, 0.0);
        assert_eq!(time_sig_w, 0.0);
        assert_eq!(total, 0.0);
    }
}
