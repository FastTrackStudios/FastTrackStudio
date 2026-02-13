//! Multi-axis rating system for presets.
//!
//! Instead of a flat 5-star system, presets get a workflow status
//! combined with a compact multi-axis rating.

// ─── PresetStatus ────────────────────────────────────────────────

/// Workflow status — where is this preset in your pipeline?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum PresetStatus {
    /// Just created, not tested live.
    #[default]
    Draft = 0,
    /// Actively tweaking / work in progress.
    Tweaking = 1,
    /// Tested and ready for live use.
    Ready = 2,
    /// Go-to preset, top of the list.
    Favorite = 3,
    /// Kept for reference but not actively used.
    Archived = 4,
}

impl PresetStatus {
    /// Display name for UI.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Draft => "Draft",
            Self::Tweaking => "Tweaking",
            Self::Ready => "Ready",
            Self::Favorite => "Favorite",
            Self::Archived => "Archived",
        }
    }

    /// All variants in display order.
    pub const fn all() -> &'static [PresetStatus] {
        &[
            Self::Draft,
            Self::Tweaking,
            Self::Ready,
            Self::Favorite,
            Self::Archived,
        ]
    }

    /// Whether this status means the preset is actively usable.
    pub const fn is_active(&self) -> bool {
        matches!(self, Self::Ready | Self::Favorite | Self::Tweaking)
    }
}

// ─── PresetRating ────────────────────────────────────────────────

/// Multi-axis rating (each axis 0–5).
///
/// - **Quality**: How good does it sound?
/// - **Versatility**: How many contexts/genres can it work in?
/// - **Production**: Is it gig-ready right now? (levels, gain staging, transitions)
#[derive(Debug, Clone, PartialEq, Eq, Hash, ::facet::Facet, Default)]
pub struct PresetRating {
    /// Overall sound quality (0–5).
    pub quality: u8,
    /// How many contexts/genres it works in (0–5).
    pub versatility: u8,
    /// Is it gig-ready right now? (0–5).
    pub production: u8,
}

impl PresetRating {
    /// Create a new rating with all axes at 0.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a rating with specific values (clamped to 0–5).
    pub fn with_values(quality: u8, versatility: u8, production: u8) -> Self {
        Self {
            quality: quality.min(5),
            versatility: versatility.min(5),
            production: production.min(5),
        }
    }

    /// Composite score: quality * 0.5 + versatility * 0.25 + production * 0.25.
    /// Returns a value in 0.0–5.0.
    pub fn composite_score(&self) -> f64 {
        self.quality as f64 * 0.5 + self.versatility as f64 * 0.25 + self.production as f64 * 0.25
    }

    /// Whether any axis has been rated (non-zero).
    pub fn is_rated(&self) -> bool {
        self.quality > 0 || self.versatility > 0 || self.production > 0
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preset_status_default() {
        assert_eq!(PresetStatus::default(), PresetStatus::Draft);
    }

    #[test]
    fn preset_status_is_active() {
        assert!(!PresetStatus::Draft.is_active());
        assert!(PresetStatus::Tweaking.is_active());
        assert!(PresetStatus::Ready.is_active());
        assert!(PresetStatus::Favorite.is_active());
        assert!(!PresetStatus::Archived.is_active());
    }

    #[test]
    fn preset_status_all() {
        assert_eq!(PresetStatus::all().len(), 5);
    }

    #[test]
    fn preset_rating_default() {
        let r = PresetRating::new();
        assert_eq!(r.quality, 0);
        assert_eq!(r.versatility, 0);
        assert_eq!(r.production, 0);
        assert!(!r.is_rated());
    }

    #[test]
    fn preset_rating_with_values() {
        let r = PresetRating::with_values(4, 3, 5);
        assert_eq!(r.quality, 4);
        assert_eq!(r.versatility, 3);
        assert_eq!(r.production, 5);
        assert!(r.is_rated());
    }

    #[test]
    fn preset_rating_clamps() {
        let r = PresetRating::with_values(10, 255, 6);
        assert_eq!(r.quality, 5);
        assert_eq!(r.versatility, 5);
        assert_eq!(r.production, 5);
    }

    #[test]
    fn preset_rating_composite_score() {
        let r = PresetRating::with_values(4, 2, 4);
        // 4*0.5 + 2*0.25 + 4*0.25 = 2.0 + 0.5 + 1.0 = 3.5
        assert!((r.composite_score() - 3.5).abs() < f64::EPSILON);
    }

    #[test]
    fn preset_rating_composite_max() {
        let r = PresetRating::with_values(5, 5, 5);
        assert!((r.composite_score() - 5.0).abs() < f64::EPSILON);
    }
}
