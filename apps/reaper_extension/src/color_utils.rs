//! REAPER Color Conversion Utilities
//!
//! Standardizes color conversion between REAPER's native BGR format and RGB format
//! used in the web UI.
//!
//! REAPER stores colors in BGR format (Blue-Green-Red), not RGB.
//! This module provides utilities to convert between formats consistently.

use reaper_medium::{NativeColor, RgbColor};
use reaper_high::Reaper;

/// Pack an RGB color into a u32 for storage/serialization
/// Format: (r << 16) | (g << 8) | b
/// This format is used consistently across the codebase for transmission to the web UI.
pub fn pack_rgb_to_u32(rgb: RgbColor) -> u32 {
    (rgb.r as u32) << 16 | (rgb.g as u32) << 8 | rgb.b as u32
}

/// Unpack a u32 color value back to RGB components
/// Format: (r << 16) | (g << 8) | b
pub fn unpack_u32_to_rgb(packed: u32) -> RgbColor {
    RgbColor {
        r: ((packed >> 16) & 0xFF) as u8,
        g: ((packed >> 8) & 0xFF) as u8,
        b: (packed & 0xFF) as u8,
    }
}

/// Extract color from REAPER native color, returning packed RGB u32 or None if black
/// 
/// REAPER stores colors internally in BGR format (Blue-Green-Red), but the
/// `color_from_native()` API function converts from OS-dependent format to RGB.
/// 
/// We pack the RGB values as (r << 16) | (g << 8) | b for consistent transmission
/// to the web UI, which expects RGB format.
/// 
/// Black (0x000000) is treated as "no color" and returns None.
/// 
/// # Format
/// - Packed format: `(r << 16) | (g << 8) | b` where r, g, b are u8 values
/// - Frontend extracts: `r = (packed >>> 16) & 0xFF, g = (packed >>> 8) & 0xFF, b = packed & 0xFF`
pub fn extract_color_from_native(native_color: NativeColor) -> Option<u32> {
    let reaper = Reaper::get();
    let rgb = reaper.medium_reaper().color_from_native(native_color);
    let packed = pack_rgb_to_u32(rgb);
    
    // Only store if not black (0x000000), as black typically means "no color" in REAPER
    if packed == 0 {
        None
    } else {
        Some(packed)
    }
}

/// Convert a packed RGB u32 to hex string format (#RRGGBB)
pub fn packed_to_hex(packed: u32) -> String {
    let rgb = unpack_u32_to_rgb(packed);
    format!("#{:02X}{:02X}{:02X}", rgb.r, rgb.g, rgb.b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pack_unpack_rgb() {
        let rgb = RgbColor { r: 255, g: 128, b: 64 };
        let packed = pack_rgb_to_u32(rgb);
        let unpacked = unpack_u32_to_rgb(packed);
        assert_eq!(rgb.r, unpacked.r);
        assert_eq!(rgb.g, unpacked.g);
        assert_eq!(rgb.b, unpacked.b);
    }

    #[test]
    fn test_packed_to_hex() {
        // Test red
        let red = pack_rgb_to_u32(RgbColor { r: 255, g: 0, b: 0 });
        assert_eq!(packed_to_hex(red), "#FF0000");
        
        // Test green
        let green = pack_rgb_to_u32(RgbColor { r: 0, g: 255, b: 0 });
        assert_eq!(packed_to_hex(green), "#00FF00");
        
        // Test blue
        let blue = pack_rgb_to_u32(RgbColor { r: 0, g: 0, b: 255 });
        assert_eq!(packed_to_hex(blue), "#0000FF");
    }
}

