//! Block type color palette for the rig grid UI.
//!
//! Colors are inspired by the Quad Cortex and Helix modelers, providing
//! visual differentiation between different types of DSP blocks.

use rig_control::block::BlockType;

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
        BlockType::Limiter => BlockColor::new("#2563EB", "#EFF6FF", "#1D4ED8"),
        BlockType::DeEsser => BlockColor::new("#60A5FA", "#EFF6FF", "#3B82F6"),

        // Drive/Saturation - Orange/Red family
        BlockType::Drive => BlockColor::new("#F97316", "#FFF7ED", "#EA580C"),
        BlockType::Saturator => BlockColor::new("#EF4444", "#FEF2F2", "#DC2626"),

        // Amp - Gold
        BlockType::Amp => BlockColor::new("#EAB308", "#FEFCE8", "#CA8A04"),

        // Cabinet - Brown
        BlockType::Cabinet => BlockColor::new("#92400E", "#FEF3C7", "#78350F"),

        // EQ - Green
        BlockType::Eq => BlockColor::new("#22C55E", "#F0FDF4", "#16A34A"),

        // Modulation - Purple family
        BlockType::Modulation => BlockColor::new("#A855F7", "#FAF5FF", "#9333EA"),
        BlockType::Tremolo => BlockColor::new("#C084FC", "#FAF5FF", "#A855F7"),
        BlockType::Pitch => BlockColor::new("#8B5CF6", "#FAF5FF", "#7C3AED"),

        // Time-based - Cyan/Sky family
        BlockType::Delay => BlockColor::new("#06B6D4", "#ECFEFF", "#0891B2"),
        BlockType::Reverb => BlockColor::new("#0EA5E9", "#F0F9FF", "#0284C7"),
        BlockType::Freeze => BlockColor::new("#22D3EE", "#ECFEFF", "#06B6D4"),

        // Special/Utility - Pink/Gray
        BlockType::Special => BlockColor::new("#EC4899", "#FDF2F8", "#DB2777"),
        BlockType::Tuner => BlockColor::new("#78716C", "#FAFAF9", "#57534E"),
        BlockType::Custom => BlockColor::new("#A8A29E", "#FAFAF9", "#78716C"),
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
        ];

        for bt in types {
            let color = block_type_color(bt);
            assert!(!color.bg.is_empty());
            assert!(!color.fg.is_empty());
            assert!(!color.border.is_empty());
        }
    }
}
