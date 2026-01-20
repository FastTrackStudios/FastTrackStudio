//! Behavioral flags for chart layout.
//!
//! Controls behavioral aspects of the layout engine, such as whether to
//! hide repeated chords or use stemmed notation.

/// Behavioral flags for chart layout.
///
/// These flags control the behavior of the layout engine rather than
/// visual styling or physical layout dimensions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BehavioralFlags {
    /// Hide repeated consecutive chord symbols.
    ///
    /// When true, if a chord is the same as the previous chord, it won't
    /// be displayed. This reduces visual clutter in charts with many
    /// repeated chords (e.g., jazz charts with long vamps).
    pub hide_repeated_chords: bool,

    /// Use stemmed rhythm notation.
    ///
    /// When false (default), uses stemless slash notation for charts.
    /// When true, uses stemmed rhythmic notation with beams and
    /// triplet brackets. This is useful for charts with complex
    /// push/pull timing that needs to be precisely notated.
    pub use_stems: bool,
}

impl Default for BehavioralFlags {
    fn default() -> Self {
        Self {
            hide_repeated_chords: true,
            use_stems: false,
        }
    }
}

impl BehavioralFlags {
    /// Create behavioral flags with default values.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create flags for simple chart rendering.
    ///
    /// Hides repeated chords and uses stemless notation.
    #[must_use]
    pub fn simple() -> Self {
        Self {
            hide_repeated_chords: true,
            use_stems: false,
        }
    }

    /// Create flags for detailed chart rendering.
    ///
    /// Shows all chords and uses stemmed notation.
    #[must_use]
    pub fn detailed() -> Self {
        Self {
            hide_repeated_chords: false,
            use_stems: true,
        }
    }

    /// Create flags for verbose display (show everything).
    #[must_use]
    pub fn verbose() -> Self {
        Self {
            hide_repeated_chords: false,
            use_stems: false,
        }
    }

    // =========================================================================
    // Builder-style setters
    // =========================================================================

    /// Set whether to hide repeated chords.
    #[must_use]
    pub const fn with_hide_repeated_chords(mut self, hide: bool) -> Self {
        self.hide_repeated_chords = hide;
        self
    }

    /// Set whether to use stemmed notation.
    #[must_use]
    pub const fn with_use_stems(mut self, use_stems: bool) -> Self {
        self.use_stems = use_stems;
        self
    }

    // =========================================================================
    // Queries
    // =========================================================================

    /// Check if a chord should be rendered given the previous chord.
    ///
    /// Returns `true` if the chord should be displayed, `false` if it
    /// should be hidden due to being a repeat.
    #[must_use]
    pub fn should_render_chord(&self, current: &str, previous: Option<&str>) -> bool {
        if !self.hide_repeated_chords {
            return true;
        }

        match previous {
            Some(prev) => current != prev,
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_flags() {
        let flags = BehavioralFlags::default();

        assert!(flags.hide_repeated_chords);
        assert!(!flags.use_stems);
    }

    #[test]
    fn test_simple_preset() {
        let flags = BehavioralFlags::simple();

        assert!(flags.hide_repeated_chords);
        assert!(!flags.use_stems);
    }

    #[test]
    fn test_detailed_preset() {
        let flags = BehavioralFlags::detailed();

        assert!(!flags.hide_repeated_chords);
        assert!(flags.use_stems);
    }

    #[test]
    fn test_should_render_chord() {
        let flags = BehavioralFlags::default();

        // First chord should always render
        assert!(flags.should_render_chord("Cmaj7", None));

        // Different chord should render
        assert!(flags.should_render_chord("Dm7", Some("Cmaj7")));

        // Same chord should not render (hidden)
        assert!(!flags.should_render_chord("Cmaj7", Some("Cmaj7")));

        // With hide_repeated_chords = false, same chord should render
        let verbose = BehavioralFlags::verbose();
        assert!(verbose.should_render_chord("Cmaj7", Some("Cmaj7")));
    }

    #[test]
    fn test_builder() {
        let flags = BehavioralFlags::new()
            .with_hide_repeated_chords(false)
            .with_use_stems(true);

        assert!(!flags.hide_repeated_chords);
        assert!(flags.use_stems);
    }
}
