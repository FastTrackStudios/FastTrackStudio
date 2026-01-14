//! Layout context for orchestrating music notation layout.
//!
//! This module provides the central `LayoutContext` struct that coordinates
//! all layout operations, similar to MuseScore's LayoutContext class.

use std::cell::{Ref, RefCell, RefMut};

use crate::model::Score;
use crate::style::MStyle;
use crate::fonts::SMuFLFont;

/// Layout view mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutMode {
    /// Standard page view (default)
    Page,
    /// Continuous horizontal view
    Horizontal,
    /// Continuous vertical view (line mode)
    Line,
}

impl Default for LayoutMode {
    fn default() -> Self {
        Self::Page
    }
}

/// Configuration for layout operations (immutable).
#[derive(Debug, Clone)]
pub struct LayoutConfiguration {
    /// View mode for layout
    pub view_mode: LayoutMode,
    /// Whether to show invisible elements
    pub show_invisible: bool,
    /// Note head width in spatiums (for spacing calculations)
    pub note_head_width: f64,
    /// Page number offset (for multi-document layouts)
    pub page_number_offset: usize,
}

impl Default for LayoutConfiguration {
    fn default() -> Self {
        Self {
            view_mode: LayoutMode::Page,
            show_invisible: false,
            note_head_width: 1.6,  // Standard notehead width
            page_number_offset: 0,
        }
    }
}

/// Mutable state during layout pass.
#[derive(Debug, Default)]
pub struct LayoutState {
    /// Current measure being laid out
    pub current_measure: Option<usize>,
    /// Current system being laid out
    pub current_system: Option<usize>,
    /// Current page being laid out
    pub current_page: Option<usize>,
    /// Current tick position (for tempo calculations)
    pub tick: i32,
    /// Current measure number (1-indexed for display)
    pub measure_no: usize,
    /// Whether this is the first system on the page
    pub first_system: bool,
}

/// Central orchestrator for layout operations.
///
/// The `LayoutContext` contains all configuration and state needed for
/// laying out a musical score. It provides access to:
/// - The score data (read-only)
/// - Style properties (spacing, fonts, etc.)
/// - SMuFL font for glyph metrics
/// - Mutable state for tracking layout progress
///
/// Based on MuseScore's LayoutContext + LayoutConfiguration + DomAccessor.
///
/// # Lifetime
///
/// The `'score` lifetime ties the context to the score being laid out,
/// preventing use-after-free and enabling zero-copy references.
///
/// # Example
///
/// ```ignore
/// let config = LayoutConfiguration::default();
/// let ctx = LayoutContext::new(config, &score, &style, &font);
///
/// // Use spatium-based queries
/// let bar_distance = ctx.style_distance(Sid::BarNoteDistance);
///
/// // Track layout progress
/// ctx.state_mut().current_measure = Some(0);
/// ```
pub struct LayoutContext<'score> {
    /// Configuration (immutable)
    pub config: LayoutConfiguration,
    /// Read-only access to score DOM
    pub score: &'score Score,
    /// MStyle for all spacing/style queries
    pub style: &'score MStyle,
    /// SMuFL font for glyph metrics
    pub font: &'score SMuFLFont<'score>,
    /// Mutable state for current layout pass
    state: RefCell<LayoutState>,
}

impl<'score> LayoutContext<'score> {
    /// Create a new layout context.
    #[must_use]
    pub fn new(
        config: LayoutConfiguration,
        score: &'score Score,
        style: &'score MStyle,
        font: &'score SMuFLFont<'score>,
    ) -> Self {
        Self {
            config,
            score,
            style,
            font,
            state: RefCell::new(LayoutState::default()),
        }
    }

    /// Create a minimal layout context with just style information.
    ///
    /// This is useful for layout operations that only need spatium/style access
    /// without requiring a full Score or font.
    #[must_use]
    pub fn minimal(style: &'score MStyle) -> Self {
        // Leak a default Score and SMuFLFont
        let score = Box::leak(Box::new(Score::default()));
        let font = Box::leak(Box::new(SMuFLFont::empty()));
        Self {
            config: LayoutConfiguration::default(),
            score,
            style,
            font,
            state: RefCell::new(LayoutState::default()),
        }
    }

    /// Create a minimal layout context for testing.
    ///
    /// This constructor is only available in test builds and creates a context
    /// with stub Score and SMuFLFont references. Use for layout tests that only
    /// need spatium/style access.
    #[cfg(test)]
    #[must_use]
    pub fn new_for_test(config: LayoutConfiguration, style: &'score MStyle) -> Self {
        // Leak a default Score and SMuFLFont for testing
        let score = Box::leak(Box::new(Score::default()));
        let font = Box::leak(Box::new(SMuFLFont::empty()));
        Self {
            config,
            score,
            style,
            font,
            state: RefCell::new(LayoutState::default()),
        }
    }

    /// Get the base spatium value in points.
    ///
    /// Spatium is the fundamental unit in music notation,
    /// representing one staff space (1/4 of staff height).
    #[must_use]
    pub fn spatium(&self) -> f64 {
        self.style.base_spatium() as f64
    }

    /// Get a style distance in points.
    ///
    /// Converts a spatium-based style property to points.
    #[must_use]
    pub fn style_distance(&self, sid: crate::style::Sid) -> f64 {
        self.style.spatium(sid) as f64 * self.spatium()
    }

    /// Get a real-valued style property.
    #[must_use]
    pub fn style_real(&self, sid: crate::style::Sid) -> f64 {
        self.style.real(sid) as f64
    }

    /// Get a boolean style property.
    #[must_use]
    pub fn style_bool(&self, sid: crate::style::Sid) -> bool {
        self.style.bool(sid)
    }

    /// Access current layout state (immutable).
    #[must_use]
    pub fn state(&self) -> Ref<'_, LayoutState> {
        self.state.borrow()
    }

    /// Access current layout state (mutable).
    #[must_use]
    pub fn state_mut(&self) -> RefMut<'_, LayoutState> {
        self.state.borrow_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_layout_configuration_default() {
        let config = LayoutConfiguration::default();
        assert_eq!(config.view_mode, LayoutMode::Page);
        assert!(!config.show_invisible);
        assert_eq!(config.note_head_width, 1.6);
    }

    #[test]
    fn test_layout_state_default() {
        let state = LayoutState::default();
        assert_eq!(state.current_measure, None);
        assert_eq!(state.tick, 0);
        assert_eq!(state.measure_no, 0);
        assert!(!state.first_system);
    }
}
