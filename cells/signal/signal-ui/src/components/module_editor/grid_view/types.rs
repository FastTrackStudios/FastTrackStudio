//! Typestate types for the grid view — making invalid states unrepresentable.
//!
//! Instead of ad-hoc boolean flags combined in if/else chains, each visual
//! and interaction state is modeled as an enum with exactly one active variant.

use super::super::module_editor_view::CompositionSlot;
use crate::components::rig_grid::block_colors::BlockColor;

// ─────────────────────────────────────────────────────────────────────────────
// Block visual state
// ─────────────────────────────────────────────────────────────────────────────

/// Visual state of a block cell. Exactly one state at a time.
///
/// Priority order (highest → lowest): Dragging, DropTarget, Bypassed,
/// Template, Selected, Normal. This matches the rendering cascade.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BlockVisualState {
    /// Normal block with a loaded plugin.
    Normal,
    /// Selected (highlighted border + brighter fill).
    Selected,
    /// Template placeholder (no plugin loaded yet). Dashed border, muted.
    Template,
    /// Bypassed (signal passes through unprocessed). Very low opacity.
    Bypassed,
    /// Being dragged — ghost at original position.
    Dragging,
    /// Drop target — another block is hovering over this cell.
    DropTarget,
}

impl BlockVisualState {
    /// Compute from raw flags. Priority order matches the old if/else chain.
    pub fn resolve(
        is_being_dragged: bool,
        is_drop_target: bool,
        is_bypassed: bool,
        is_template: bool,
        is_selected: bool,
    ) -> Self {
        if is_being_dragged {
            Self::Dragging
        } else if is_drop_target {
            Self::DropTarget
        } else if is_bypassed {
            Self::Bypassed
        } else if is_template {
            Self::Template
        } else if is_selected {
            Self::Selected
        } else {
            Self::Normal
        }
    }

    /// Inline style string for the block cell.
    ///
    /// Always explicitly sets `opacity` to work around Wry's WebView not
    /// reliably resetting omitted inline style properties on VDOM diff.
    pub fn cell_style(&self, color: &BlockColor) -> String {
        match self {
            Self::Dragging => format!(
                "background-color: {}10; border-color: {}20; color: {}40; opacity: 0.4;",
                color.bg, color.bg, color.fg,
            ),
            Self::Bypassed => format!(
                "background-color: {}08; border-color: {}15; color: {}30; opacity: 0.25;",
                color.bg, color.bg, color.fg,
            ),
            Self::Template => format!(
                "background-color: {}08; border-color: {}25; color: {}60; opacity: 1; border-style: dashed;",
                color.bg, color.bg, color.fg,
            ),
            Self::Selected => format!(
                "background-color: {}25; border-color: {}; color: {}; opacity: 1;",
                color.bg, color.bg, color.fg,
            ),
            Self::DropTarget => format!(
                "background-color: {}20; border-color: {}60; color: {}; opacity: 1;",
                color.bg, color.bg, color.fg,
            ),
            Self::Normal => format!(
                "background-color: {}15; border-color: {}40; color: {}; opacity: 1;",
                color.bg, color.bg, color.fg,
            ),
        }
    }

    /// Port dot opacity for this visual state.
    pub fn port_opacity(&self) -> &'static str {
        match self {
            Self::Bypassed => "0.25",
            _ => "1",
        }
    }

    /// CSS class for the cell container div.
    ///
    /// No `transition-all` — Wry flickers on signal-driven class changes.
    pub fn cell_class(&self) -> &'static str {
        match self {
            Self::Dragging => {
                "absolute inset-0 flex flex-col items-center justify-center gap-1 \
                 rounded-lg border-2 border-dashed"
            }
            _ => {
                "absolute inset-0 flex flex-col items-center justify-center gap-1 \
                 rounded-lg border-2 cursor-grab \
                 hover:brightness-110 active:cursor-grabbing"
            }
        }
    }

    /// Whether the block is in a bypassed state.
    pub fn is_bypassed(&self) -> bool {
        matches!(self, Self::Bypassed)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Module visual state
// ─────────────────────────────────────────────────────────────────────────────

/// Visual state of a module container background.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ModuleVisualState {
    /// Normal module — full opacity, solid border.
    Normal,
    /// Selected module — glow ring.
    Selected,
    /// All blocks are template placeholders — dashed border.
    Template,
    /// All blocks are bypassed — dim opacity.
    Bypassed,
    /// Module is being dragged.
    Dragging,
}

impl ModuleVisualState {
    /// Compute from the module's constituent slots.
    pub fn from_slots(slots: &[&CompositionSlot], is_selected: bool, is_dragging: bool) -> Self {
        if is_dragging {
            return Self::Dragging;
        }
        let all_bypassed = !slots.is_empty() && slots.iter().all(|s| s.bypassed);
        let all_template = !slots.is_empty() && slots.iter().all(|s| s.is_template);
        if all_bypassed {
            Self::Bypassed
        } else if all_template {
            Self::Template
        } else if is_selected {
            Self::Selected
        } else {
            Self::Normal
        }
    }

    /// Opacity for the module container. Always explicit (Wry workaround).
    pub fn opacity(&self) -> &'static str {
        match self {
            Self::Dragging => "0.85",
            Self::Bypassed => "0.25",
            _ => "1",
        }
    }

    /// Extra inline style fragments (border-style, z-index).
    pub fn extra_style(&self) -> &'static str {
        match self {
            Self::Dragging => "z-index: 50;",
            Self::Template => "border-style: dashed;",
            _ => "",
        }
    }

    /// CSS transition for transform (disabled during drag for instant feedback).
    pub fn transition(&self) -> &'static str {
        match self {
            Self::Dragging => "none",
            _ => "transform 0.15s ease",
        }
    }

    /// Selection glow box-shadow. Always explicit — Wry's WebView doesn't
    /// reliably reset omitted inline style properties on VDOM diff.
    pub fn selection_glow(&self, bg_color: &str) -> String {
        match self {
            Self::Selected => {
                format!(
                    "box-shadow: 0 0 0 2px {}90, 0 0 12px {}30;",
                    bg_color, bg_color
                )
            }
            _ => "box-shadow: none;".to_string(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Cable kind
// ─────────────────────────────────────────────────────────────────────────────

/// How a cable is rendered between two points.
///
/// Replaces `straight: bool` + `route_y: Option<f64>` — those two fields
/// could represent 3 valid states but also the invalid `straight=true` +
/// `route_y=Some(...)`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CableKind {
    /// Standard Bézier curve (most cables).
    Bezier,
    /// Straight line (currently unused but reserved for pass-through lanes).
    Straight,
    /// Routed through a horizontal channel at the given Y coordinate,
    /// with rounded corners at the bends.
    Routed { channel_y: f64 },
}
