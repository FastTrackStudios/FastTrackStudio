//! Plugin window sizing with rack panel aspect ratios and size tiers.
//!
//! This module provides a comprehensive sizing system for audio plugin GUIs,
//! supporting standard rack panel formats (1U-4U), half-rack, 500 series modules,
//! and custom aspect ratios.
//!
//! # Rack Panel Standards
//!
//! - Rack inner width (between rails): 17.75 inches
//! - Rack unit height: 1U = 1.75 inches
//! - Base pixel width: 71 units (1420px at Medium, 710px at Tiny)
//!
//! # Example
//!
//! ```ignore
//! use fts_plugin_core::sizing::{AspectRatio, SizeTier};
//!
//! let aspect = AspectRatio::Rack2U;
//! let tier = SizeTier::Medium;
//! let (width, height) = aspect.dimensions(tier);
//! assert_eq!((width, height), (1420, 280));
//! ```

use std::fmt;

/// Aspect ratio categories for plugin windows.
///
/// These represent standard rack equipment formats plus common screen ratios.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum AspectRatio {
    // Rack formats (71 units wide = 17.75" at 4 units/inch)
    /// 1U Rack: 71:7 ratio (17.75" x 1.75")
    Rack1U,
    /// 2U Rack: 71:14 ratio (17.75" x 3.5")
    Rack2U,
    /// 3U Rack: 71:21 ratio (17.75" x 5.25")
    Rack3U,
    /// 4U Rack: 71:28 ratio (17.75" x 7")
    Rack4U,

    // Half-rack formats (35.5 units wide = 8.875")
    /// Half-rack 1U: 71:14 ratio
    HalfRack1U,
    /// Half-rack 2U: 71:28 ratio
    HalfRack2U,
    /// Half-rack 3U: 71:42 ratio
    HalfRack3U,
    /// Half-rack 4U: 71:56 ratio
    HalfRack4U,

    // 500 Series modules (portrait orientation)
    /// 500 Series Single: 2:7 ratio (1.5" x 5.25")
    Series500Single,
    /// 500 Series Double: 4:7 ratio (3" x 5.25")
    Series500Double,

    // Standard formats
    /// Square: 1:1 ratio
    Square,
    /// Widescreen: 16:9 ratio (default)
    #[default]
    Widescreen,

    // Custom format
    /// Custom aspect ratio with user-defined width:height
    Custom {
        /// Width component of the ratio
        width_ratio: u32,
        /// Height component of the ratio
        height_ratio: u32,
        /// Base width in pixels (at Medium tier)
        base_width: u32,
    },
}

/// Size tier multipliers for plugin windows.
///
/// Each tier applies a multiplier to the base dimensions of the aspect ratio.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum SizeTier {
    /// Tiny: 0.5x base dimensions
    Tiny,
    /// Small: 0.75x base dimensions
    Small,
    /// Medium: 1.0x base dimensions (reference size)
    #[default]
    Medium,
    /// Large: 1.25x base dimensions
    Large,
    /// Very Large: 1.5x base dimensions
    VeryLarge,
    /// Fullscreen: uses maximum available size
    Fullscreen,
}

// Base dimensions at Medium (1.0x) tier
// Rack formats: 71 units = 1420px width (20px per unit)
const RACK_BASE_WIDTH: u32 = 1420;
const HALF_RACK_BASE_WIDTH: u32 = 710;
const UNIT_HEIGHT: u32 = 140; // 1U = 140px (7 units at 20px)

// 500 Series base dimensions
const SERIES_500_SINGLE_WIDTH: u32 = 300;
const SERIES_500_DOUBLE_WIDTH: u32 = 600;
const SERIES_500_HEIGHT: u32 = 1050; // 5.25" at 200dpi reference

// Standard format bases
const SQUARE_BASE: u32 = 600;
const WIDESCREEN_BASE_WIDTH: u32 = 640;
const WIDESCREEN_BASE_HEIGHT: u32 = 360;

// Fullscreen fallback
const FULLSCREEN_WIDTH: u32 = 1920;
const FULLSCREEN_HEIGHT: u32 = 1080;

impl AspectRatio {
    /// Get base (Medium tier) dimensions for this aspect ratio.
    #[must_use]
    pub const fn base_dimensions(&self) -> (u32, u32) {
        match self {
            Self::Rack1U => (RACK_BASE_WIDTH, UNIT_HEIGHT),
            Self::Rack2U => (RACK_BASE_WIDTH, UNIT_HEIGHT * 2),
            Self::Rack3U => (RACK_BASE_WIDTH, UNIT_HEIGHT * 3),
            Self::Rack4U => (RACK_BASE_WIDTH, UNIT_HEIGHT * 4),

            Self::HalfRack1U => (HALF_RACK_BASE_WIDTH, UNIT_HEIGHT),
            Self::HalfRack2U => (HALF_RACK_BASE_WIDTH, UNIT_HEIGHT * 2),
            Self::HalfRack3U => (HALF_RACK_BASE_WIDTH, UNIT_HEIGHT * 3),
            Self::HalfRack4U => (HALF_RACK_BASE_WIDTH, UNIT_HEIGHT * 4),

            Self::Series500Single => (SERIES_500_SINGLE_WIDTH, SERIES_500_HEIGHT),
            Self::Series500Double => (SERIES_500_DOUBLE_WIDTH, SERIES_500_HEIGHT),

            Self::Square => (SQUARE_BASE, SQUARE_BASE),
            Self::Widescreen => (WIDESCREEN_BASE_WIDTH, WIDESCREEN_BASE_HEIGHT),

            Self::Custom {
                width_ratio,
                height_ratio,
                base_width,
            } => {
                let base_height = (*base_width as u64 * *height_ratio as u64 / *width_ratio as u64) as u32;
                (*base_width, base_height)
            }
        }
    }

    /// Get dimensions for a specific size tier.
    ///
    /// For `Fullscreen`, returns the fallback maximum size (1920x1080).
    /// Use `dimensions_with_max` for actual fullscreen with known screen bounds.
    #[must_use]
    pub fn dimensions(&self, tier: SizeTier) -> (u32, u32) {
        if tier == SizeTier::Fullscreen {
            return (FULLSCREEN_WIDTH, FULLSCREEN_HEIGHT);
        }

        let (base_w, base_h) = self.base_dimensions();
        let multiplier = tier.multiplier();

        let width = ((base_w as f32) * multiplier).round() as u32;
        let height = ((base_h as f32) * multiplier).round() as u32;

        (width, height)
    }

    /// Get dimensions with custom maximum bounds (for Fullscreen mode).
    #[must_use]
    pub fn dimensions_with_max(&self, tier: SizeTier, max_size: (u32, u32)) -> (u32, u32) {
        if tier == SizeTier::Fullscreen {
            return max_size;
        }
        self.dimensions(tier)
    }

    /// Get the aspect ratio as width:height integers.
    #[must_use]
    pub const fn ratio(&self) -> (u32, u32) {
        match self {
            Self::Rack1U => (71, 7),
            Self::Rack2U => (71, 14),
            Self::Rack3U => (71, 21),
            Self::Rack4U => (71, 28),

            Self::HalfRack1U => (71, 14),
            Self::HalfRack2U => (71, 28),
            Self::HalfRack3U => (71, 42),
            Self::HalfRack4U => (71, 56),

            Self::Series500Single => (2, 7),
            Self::Series500Double => (4, 7),

            Self::Square => (1, 1),
            Self::Widescreen => (16, 9),

            Self::Custom {
                width_ratio,
                height_ratio,
                ..
            } => (*width_ratio, *height_ratio),
        }
    }

    /// Get display name for this aspect ratio.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Rack1U => "Rack 1U",
            Self::Rack2U => "Rack 2U",
            Self::Rack3U => "Rack 3U",
            Self::Rack4U => "Rack 4U",
            Self::HalfRack1U => "Half Rack 1U",
            Self::HalfRack2U => "Half Rack 2U",
            Self::HalfRack3U => "Half Rack 3U",
            Self::HalfRack4U => "Half Rack 4U",
            Self::Series500Single => "500 Series",
            Self::Series500Double => "500 Double",
            Self::Square => "Square",
            Self::Widescreen => "16:9",
            Self::Custom { .. } => "Custom",
        }
    }

    /// Get short name for UI buttons.
    #[must_use]
    pub const fn short_name(&self) -> &'static str {
        match self {
            Self::Rack1U => "R1U",
            Self::Rack2U => "R2U",
            Self::Rack3U => "R3U",
            Self::Rack4U => "R4U",
            Self::HalfRack1U => "H1U",
            Self::HalfRack2U => "H2U",
            Self::HalfRack3U => "H3U",
            Self::HalfRack4U => "H4U",
            Self::Series500Single => "500",
            Self::Series500Double => "500D",
            Self::Square => "1:1",
            Self::Widescreen => "16:9",
            Self::Custom { .. } => "Cust",
        }
    }

    /// Check if this is a portrait (vertical) aspect ratio.
    #[must_use]
    pub const fn is_portrait(&self) -> bool {
        matches!(self, Self::Series500Single | Self::Series500Double)
    }

    /// Check if this is a rack format.
    #[must_use]
    pub const fn is_rack(&self) -> bool {
        matches!(
            self,
            Self::Rack1U
                | Self::Rack2U
                | Self::Rack3U
                | Self::Rack4U
                | Self::HalfRack1U
                | Self::HalfRack2U
                | Self::HalfRack3U
                | Self::HalfRack4U
        )
    }

    /// Get all standard (non-custom) aspect ratios.
    #[must_use]
    pub const fn all() -> &'static [AspectRatio] {
        &[
            Self::Widescreen,
            Self::Square,
            Self::Rack1U,
            Self::Rack2U,
            Self::Rack3U,
            Self::Rack4U,
            Self::HalfRack1U,
            Self::HalfRack2U,
            Self::HalfRack3U,
            Self::HalfRack4U,
            Self::Series500Single,
            Self::Series500Double,
        ]
    }

    /// Create a custom aspect ratio.
    #[must_use]
    pub const fn custom(width_ratio: u32, height_ratio: u32, base_width: u32) -> Self {
        Self::Custom {
            width_ratio,
            height_ratio,
            base_width,
        }
    }
}

impl fmt::Display for AspectRatio {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl SizeTier {
    /// Get the multiplier for this size tier.
    #[must_use]
    pub const fn multiplier(&self) -> f32 {
        match self {
            Self::Tiny => 0.5,
            Self::Small => 0.75,
            Self::Medium => 1.0,
            Self::Large => 1.25,
            Self::VeryLarge => 1.5,
            Self::Fullscreen => 1.0, // N/A - fullscreen overrides dimensions
        }
    }

    /// Get the short label for UI buttons.
    #[must_use]
    pub const fn label(&self) -> &'static str {
        match self {
            Self::Tiny => "T",
            Self::Small => "S",
            Self::Medium => "M",
            Self::Large => "L",
            Self::VeryLarge => "XL",
            Self::Fullscreen => "F",
        }
    }

    /// Get the display name for this tier.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Tiny => "Tiny",
            Self::Small => "Small",
            Self::Medium => "Medium",
            Self::Large => "Large",
            Self::VeryLarge => "Very Large",
            Self::Fullscreen => "Fullscreen",
        }
    }

    /// Get all size tiers.
    #[must_use]
    pub const fn all() -> &'static [SizeTier] {
        &[
            Self::Tiny,
            Self::Small,
            Self::Medium,
            Self::Large,
            Self::VeryLarge,
            Self::Fullscreen,
        ]
    }

    /// Get size tier by index (0-5).
    #[must_use]
    pub const fn from_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(Self::Tiny),
            1 => Some(Self::Small),
            2 => Some(Self::Medium),
            3 => Some(Self::Large),
            4 => Some(Self::VeryLarge),
            5 => Some(Self::Fullscreen),
            _ => None,
        }
    }

    /// Get index for this tier (0-5).
    #[must_use]
    pub const fn index(&self) -> usize {
        match self {
            Self::Tiny => 0,
            Self::Small => 1,
            Self::Medium => 2,
            Self::Large => 3,
            Self::VeryLarge => 4,
            Self::Fullscreen => 5,
        }
    }
}

impl fmt::Display for SizeTier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Calculate dimensions for an aspect ratio and tier, respecting min/max constraints.
#[must_use]
pub fn calculate_size(
    aspect: AspectRatio,
    tier: SizeTier,
    min: (u32, u32),
    max: (u32, u32),
) -> (u32, u32) {
    let (width, height) = aspect.dimensions_with_max(tier, max);

    let width = width.clamp(min.0, max.0);
    let height = height.clamp(min.1, max.1);

    (width, height)
}

/// Get the default window size for a new plugin.
#[must_use]
pub const fn default_window_size() -> (u32, u32) {
    AspectRatio::Widescreen.base_dimensions()
}

/// Minimum window size constraint.
pub const MIN_WINDOW_SIZE: (u32, u32) = (200, 100);

/// Maximum window size constraint (to stay within GPU texture limits).
pub const MAX_WINDOW_SIZE: (u32, u32) = (1920, 1080);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rack_dimensions() {
        assert_eq!(AspectRatio::Rack1U.base_dimensions(), (1420, 140));
        assert_eq!(AspectRatio::Rack2U.base_dimensions(), (1420, 280));
        assert_eq!(AspectRatio::Rack3U.base_dimensions(), (1420, 420));
        assert_eq!(AspectRatio::Rack4U.base_dimensions(), (1420, 560));
    }

    #[test]
    fn test_half_rack_dimensions() {
        assert_eq!(AspectRatio::HalfRack1U.base_dimensions(), (710, 140));
        assert_eq!(AspectRatio::HalfRack2U.base_dimensions(), (710, 280));
    }

    #[test]
    fn test_500_series_dimensions() {
        assert_eq!(AspectRatio::Series500Single.base_dimensions(), (300, 1050));
        assert_eq!(AspectRatio::Series500Double.base_dimensions(), (600, 1050));
    }

    #[test]
    fn test_size_tier_multipliers() {
        let aspect = AspectRatio::Widescreen;
        assert_eq!(aspect.dimensions(SizeTier::Tiny), (320, 180));
        assert_eq!(aspect.dimensions(SizeTier::Small), (480, 270));
        assert_eq!(aspect.dimensions(SizeTier::Medium), (640, 360));
        assert_eq!(aspect.dimensions(SizeTier::Large), (800, 450));
        assert_eq!(aspect.dimensions(SizeTier::VeryLarge), (960, 540));
    }

    #[test]
    fn test_rack_ratios() {
        assert_eq!(AspectRatio::Rack1U.ratio(), (71, 7));
        assert_eq!(AspectRatio::Rack2U.ratio(), (71, 14));
        assert_eq!(AspectRatio::Rack3U.ratio(), (71, 21));
        assert_eq!(AspectRatio::Rack4U.ratio(), (71, 28));
    }

    #[test]
    fn test_custom_aspect() {
        let custom = AspectRatio::custom(4, 3, 800);
        assert_eq!(custom.base_dimensions(), (800, 600));
        assert_eq!(custom.ratio(), (4, 3));
    }

    #[test]
    fn test_size_tier_indices() {
        for (i, tier) in SizeTier::all().iter().enumerate() {
            assert_eq!(tier.index(), i);
            assert_eq!(SizeTier::from_index(i), Some(*tier));
        }
        assert_eq!(SizeTier::from_index(6), None);
    }
}
