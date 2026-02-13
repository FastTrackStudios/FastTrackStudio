//! Block type color palette for the rig grid UI.
//!
//! Colors are inspired by the Quad Cortex and Helix modelers, providing
//! visual differentiation between different types of DSP blocks.

use signal_control::block::BlockType;

/// Color configuration for a block type.
#[derive(Debug, Clone, Copy)]
pub struct BlockColor {
    /// Background color (Tailwind-compatible hex).
    pub bg: &'static str,
    /// Text/foreground color (Tailwind-compatible hex).
    pub fg: &'static str,
    /// Border/accent color (slightly darker than bg).
    pub border: &'static str,
}

impl BlockColor {
    const fn new(bg: &'static str, fg: &'static str, border: &'static str) -> Self {
        Self { bg, fg, border }
    }
}

/// Get the color configuration for a block type.
///
/// Returns a Quad Cortex-inspired color palette where:
/// - Dynamics (compressor, gate, limiter) = Blue
/// - Drive/Saturation = Orange/Red
/// - Amp = Gold
/// - Cabinet = Brown
/// - EQ = Green
/// - Modulation = Purple
/// - Time (delay, reverb) = Cyan/Sky
/// - Special/Utility = Gray/Pink
pub const fn block_type_color(block_type: BlockType) -> BlockColor {
    match block_type {
        // Input/Output - Neutral gray
        BlockType::Input => BlockColor::new("#6B7280", "#F9FAFB", "#4B5563"),
        BlockType::Send => BlockColor::new("#6B7280", "#F9FAFB", "#4B5563"),
        BlockType::Volume => BlockColor::new("#6B7280", "#F9FAFB", "#4B5563"),

        // Dynamics - Blue family
        BlockType::Compressor => BlockColor::new("#3B82F6", "#EFF6FF", "#2563EB"),
        BlockType::Gate => BlockColor::new("#3B82F6", "#EFF6FF", "#2563EB"),
        BlockType::Limiter => BlockColor::new("#3B82F6", "#EFF6FF", "#2563EB"),
        BlockType::DeEsser => BlockColor::new("#60A5FA", "#EFF6FF", "#3B82F6"),

        // Drive/Saturation - Orange/Red family
        BlockType::Drive => BlockColor::new("#F97316", "#FFF7ED", "#EA580C"),
        BlockType::Saturator => BlockColor::new("#EF4444", "#FEF2F2", "#DC2626"),

        // Amp - Gold
        BlockType::Amp => BlockColor::new("#EAB308", "#FEFCE8", "#CA8A04"),

        // Cabinet - Warm Brown (brighter for dark backgrounds)
        BlockType::Cabinet => BlockColor::new("#B45309", "#FEF3C7", "#92400E"),

        // EQ - Green
        BlockType::Eq => BlockColor::new("#22C55E", "#F0FDF4", "#16A34A"),

        // Modulation - Purple family
        BlockType::Modulation => BlockColor::new("#A855F7", "#FAF5FF", "#9333EA"),
        BlockType::Chorus => BlockColor::new("#A855F7", "#FAF5FF", "#9333EA"),
        BlockType::Flanger => BlockColor::new("#A855F7", "#FAF5FF", "#9333EA"),
        BlockType::Phaser => BlockColor::new("#A855F7", "#FAF5FF", "#9333EA"),
        BlockType::RingModulator => BlockColor::new("#9333EA", "#FAF5FF", "#7E22CE"),

        // Motion - Lighter purple
        BlockType::Tremolo => BlockColor::new("#C084FC", "#FAF5FF", "#A855F7"),
        BlockType::Panner => BlockColor::new("#C084FC", "#FAF5FF", "#A855F7"),
        BlockType::Vibrato => BlockColor::new("#C084FC", "#FAF5FF", "#A855F7"),
        BlockType::Rotary => BlockColor::new("#C084FC", "#FAF5FF", "#A855F7"),

        // Special - Pink family
        BlockType::Pitch => BlockColor::new("#8B5CF6", "#FAF5FF", "#7C3AED"),

        // Time-based - Cyan/Sky family
        BlockType::Delay => BlockColor::new("#06B6D4", "#ECFEFF", "#0891B2"),
        BlockType::Reverb => BlockColor::new("#0EA5E9", "#F0F9FF", "#0284C7"),
        BlockType::Freeze => BlockColor::new("#22D3EE", "#ECFEFF", "#06B6D4"),

        // Special/Utility - Pink/Gray
        BlockType::Special => BlockColor::new("#EC4899", "#FDF2F8", "#DB2777"),
        BlockType::Wah => BlockColor::new("#EC4899", "#FDF2F8", "#DB2777"),
        BlockType::Filter => BlockColor::new("#EC4899", "#FDF2F8", "#DB2777"),
        BlockType::Doubler => BlockColor::new("#EC4899", "#FDF2F8", "#DB2777"),
        BlockType::Tuner => BlockColor::new("#78716C", "#FAFAF9", "#57534E"),
        BlockType::Custom => BlockColor::new("#A8A29E", "#FAFAF9", "#78716C"),

        // EQ subcategory - Green
        BlockType::Crossover => BlockColor::new("#22C55E", "#F0FDF4", "#16A34A"),

        // Drive subcategory - Orange
        BlockType::Boost => BlockColor::new("#FB923C", "#FFF7ED", "#F97316"),
    }
}

/// Get just the background color for a block type.
///
/// Convenience function for simple use cases.
pub const fn block_type_bg(block_type: BlockType) -> &'static str {
    block_type_color(block_type).bg
}

/// Get the text color for a block type.
///
/// Convenience function for simple use cases.
pub const fn block_type_fg(block_type: BlockType) -> &'static str {
    block_type_color(block_type).fg
}

/// Get a CSS style string for a block type.
///
/// Returns an inline style suitable for use in Dioxus `style` attributes.
pub fn block_type_style(block_type: BlockType) -> String {
    let color = block_type_color(block_type);
    format!(
        "background-color: {}; color: {}; border-color: {};",
        color.bg, color.fg, color.border
    )
}

/// Get a lighter/faded version of the block color for bypassed state.
pub fn block_type_bypassed_style(block_type: BlockType) -> String {
    let color = block_type_color(block_type);
    format!(
        "background-color: {}40; color: {}80; border-color: {}40; opacity: 0.6;",
        color.bg, color.fg, color.border
    )
}

/// Get a slightly varied color for a specific block instance.
///
/// This function takes the base color for a block type and applies a subtle
/// variation based on the instance identifier (e.g., "Drive 1", "Drive 2").
/// This helps distinguish between multiple blocks of the same type.
pub fn block_instance_color(block_type: BlockType, instance_id: &str) -> BlockColor {
    let base = block_type_color(block_type);

    // Generate a hash from the instance_id to get a consistent variation
    let hash: u32 = instance_id.bytes().map(|b| b as u32).sum();
    let variation = (hash % 15) as i32 - 7; // -7 to +7 variation

    // Parse the hex color and apply variation
    let varied_bg = vary_hex_color(base.bg, variation);
    let varied_border = vary_hex_color(base.border, variation);

    BlockColor {
        bg: Box::leak(varied_bg.into_boxed_str()),
        fg: base.fg,
        border: Box::leak(varied_border.into_boxed_str()),
    }
}

/// Vary a hex color by adjusting its lightness.
fn vary_hex_color(hex: &str, variation: i32) -> String {
    // Parse hex color (#RRGGBB)
    let hex = hex.trim_start_matches('#');
    if hex.len() != 6 {
        return format!("#{}", hex);
    }

    let r = i32::from_str_radix(&hex[0..2], 16).unwrap_or(0);
    let g = i32::from_str_radix(&hex[2..4], 16).unwrap_or(0);
    let b = i32::from_str_radix(&hex[4..6], 16).unwrap_or(0);

    // Apply variation with clamping
    let adjust = |val: i32| -> u8 { (val + variation * 3).clamp(0, 255) as u8 };

    let new_r = adjust(r);
    let new_g = adjust(g);
    let new_b = adjust(b);

    format!("#{:02X}{:02X}{:02X}", new_r, new_g, new_b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_block_types_have_colors() {
        // Ensure all block types return valid colors (no panics)
        let types = [
            BlockType::Input,
            BlockType::Compressor,
            BlockType::Drive,
            BlockType::Amp,
            BlockType::Cabinet,
            BlockType::Eq,
            BlockType::Modulation,
            BlockType::Delay,
            BlockType::Reverb,
            BlockType::Gate,
            BlockType::Volume,
            BlockType::Pitch,
            BlockType::Tremolo,
            BlockType::Limiter,
            BlockType::Send,
            BlockType::Special,
            BlockType::Freeze,
            BlockType::Custom,
            BlockType::DeEsser,
            BlockType::Saturator,
            BlockType::Tuner,
            BlockType::Chorus,
            BlockType::Flanger,
            BlockType::Phaser,
            BlockType::RingModulator,
            BlockType::Wah,
            BlockType::Filter,
            BlockType::Doubler,
            BlockType::Panner,
            BlockType::Vibrato,
            BlockType::Rotary,
            BlockType::Crossover,
            BlockType::Boost,
        ];

        for bt in types {
            let color = block_type_color(bt);
            assert!(!color.bg.is_empty());
            assert!(!color.fg.is_empty());
            assert!(!color.border.is_empty());
        }
    }
}
