//! Compact module view - colored block pills (Quad Cortex style).
//!
//! In compact mode, blocks are displayed as colored pills with:
//! - Background color based on `BlockType`
//! - Block name (truncated if needed)
//! - Bypass indicator dot (green=active, red=bypassed)
//! - Click to toggle bypass

use crate::prelude::*;
use signal_control::block::BlockType;
use signal_control::module::ModuleBlock;
use uuid::Uuid;

use super::block_colors::{block_type_bypassed_style, block_type_style};

/// Props for the compact module view.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleCompactViewProps {
    /// Blocks to display as pills.
    pub blocks: Vec<ModuleBlock>,
    /// Callback when a block's bypass is toggled.
    pub on_toggle_bypass: Callback<Uuid>,
}

/// Compact view showing blocks as colored pills.
///
/// This view mode takes ~128px width and is ideal for getting a quick
/// overview of the entire rig while being able to toggle blocks on/off.
#[component]
pub fn ModuleCompactView(props: ModuleCompactViewProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-xs text-muted-foreground text-center py-2",
                "No blocks"
            }
        };
    }

    // Sort blocks by order
    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order.get());

    rsx! {
        div { class: "flex flex-col gap-1 p-2",
            for block in sorted_blocks {
                CompactBlockPill {
                    key: "{block.id}",
                    block: block.clone(),
                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                }
            }
        }
    }
}

/// Props for a single block pill.
#[derive(Props, Clone, PartialEq)]
struct CompactBlockPillProps {
    block: ModuleBlock,
    on_toggle_bypass: Callback<Uuid>,
}

/// A single colored pill representing a block.
#[component]
fn CompactBlockPill(props: CompactBlockPillProps) -> Element {
    let block = &props.block;
    let block_id = block.block.id.as_uuid();
    let is_bypassed = block.block.bypassed;

    // Get block type for coloring (parse from plugin or default to Drive)
    let block_type = infer_block_type(&block.block.name);

    // Style based on bypass state
    let style = if is_bypassed {
        block_type_bypassed_style(block_type)
    } else {
        block_type_style(block_type)
    };

    // Use alias if set, otherwise truncate name for compact display
    let display_name = truncate_name(block.block.display_name(), 10);
    let is_placeholder = block.block.is_placeholder();

    let on_toggle = props.on_toggle_bypass.clone();

    // Placeholder blocks get a dashed border and muted colors
    let placeholder_class = if is_placeholder {
        " border-dashed opacity-50"
    } else {
        ""
    };

    rsx! {
        button {
            class: "flex items-center justify-between px-2 py-1.5 rounded text-xs font-medium \
                    border transition-all cursor-pointer hover:brightness-110 active:brightness-90{placeholder_class}",
            style: "{style}",
            onclick: move |_| on_toggle.call(block_id),
            title: "{block.block.name}",

            // Block name (alias or truncated name)
            span { class: "truncate", "{display_name}" }

            // Bypass indicator dot
            div {
                class: if is_bypassed {
                    "w-2 h-2 rounded-full bg-red-500 flex-shrink-0 ml-1"
                } else {
                    "w-2 h-2 rounded-full bg-green-500 flex-shrink-0 ml-1"
                },
            }
        }
    }
}

/// Infer BlockType from block name.
///
/// This is a heuristic based on common naming conventions.
/// A proper implementation would store BlockType directly on the block.
pub fn infer_block_type(name: &str) -> BlockType {
    let lower = name.to_lowercase();

    if lower.contains("comp") || lower.contains("dyn") {
        BlockType::Compressor
    } else if lower.contains("gate") || lower.contains("noise") {
        BlockType::Gate
    } else if lower.contains("drive") || lower.contains("od") || lower.contains("dist") {
        BlockType::Drive
    } else if lower.contains("fuzz") || lower.contains("sat") {
        BlockType::Saturator
    } else if lower.contains("amp") || lower.contains("preamp") {
        BlockType::Amp
    } else if lower.contains("cab") || lower.contains("ir") || lower.contains("speaker") {
        BlockType::Cabinet
    } else if lower.contains("eq") || lower.contains("filter") || lower.contains("tone") {
        BlockType::Eq
    } else if lower.contains("chorus")
        || lower.contains("flang")
        || lower.contains("phase")
        || lower.contains("mod")
        || lower.contains("vibe")
        || lower.contains("rotary")
    {
        BlockType::Modulation
    } else if lower.contains("trem") {
        BlockType::Tremolo
    } else if lower.contains("delay") || lower.contains("echo") {
        BlockType::Delay
    } else if lower.contains("rev")
        || lower.contains("verb")
        || lower.contains("hall")
        || lower.contains("room")
        || lower.contains("plate")
    {
        BlockType::Reverb
    } else if lower.contains("pitch") || lower.contains("harm") || lower.contains("oct") {
        BlockType::Pitch
    } else if lower.contains("vol") || lower.contains("level") {
        BlockType::Volume
    } else if lower.contains("limit") {
        BlockType::Limiter
    } else if lower.contains("send") || lower.contains("aux") {
        BlockType::Send
    } else if lower.contains("freeze") || lower.contains("hold") {
        BlockType::Freeze
    } else if lower.contains("tuner") {
        BlockType::Tuner
    } else if lower.contains("deess") {
        BlockType::DeEsser
    } else if lower.contains("input") || lower.contains("in") {
        BlockType::Input
    } else {
        // Default to Drive for unknown types
        BlockType::Drive
    }
}

/// Truncate a name to fit in compact view.
fn truncate_name(name: &str, max_len: usize) -> String {
    if name.len() <= max_len {
        name.to_string()
    } else {
        format!("{}…", &name[..max_len - 1])
    }
}
