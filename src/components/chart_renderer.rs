//! Chart Renderer types
//!
//! Layout mode shared by the static chart renderers.

/// Layout mode for chart rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LayoutMode {
    /// Content-sized layout (measures don't fill line)
    #[default]
    Snippet,
    /// A4 page layout with Master Rhythm preset (measures fill line)
    Page,
    /// iReal Pro-style responsive layout: page width snaps to viewport,
    /// staff/chord sizing picked from a Phone/Tablet/Desktop breakpoint.
    /// Vertical scroll only — no horizontal overflow at any width.
    ///
    /// Supported by the renderer (maps to `ContinuousScroll`) but not currently
    /// selectable from any page — reserved for the responsive preview mode.
    #[allow(dead_code)]
    Responsive,
}
