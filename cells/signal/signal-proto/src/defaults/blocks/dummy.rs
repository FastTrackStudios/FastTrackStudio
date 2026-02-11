//! Dummy blocks — test/placeholder blocks for every [`BlockType`].
//!
//! Each dummy block has a fake plugin ID (`com.dummy.*`) and is backed
//! by a non-unassigned [`PluginId`], so it looks like a "real" block
//! that has been assigned — unlike template placeholders.
//!
//! Used for testing override workflows (replacing a template placeholder
//! with a concrete block) and for populating the block preset library
//! with browsable entries that have snapshots.

use crate::block::{Block, BlockType, PluginId};

/// All block type variants, in declaration order.
///
/// Useful for iterating over the full set of block types
/// without relying on a derive macro.
pub const ALL_BLOCK_TYPES: &[BlockType] = &[
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

/// Snapshot names for dummy block presets.
pub const DUMMY_SNAPSHOT_NAMES: &[&str] = &["1", "2", "3", "4"];

/// Create a dummy block for the given block type.
///
/// The block has a fake VST3 plugin ID (`com.dummy.<type>`) so it
/// registers as a real (non-placeholder) block in the system.
pub fn dummy(block_type: BlockType) -> Block {
    let display = block_type.display_name();
    let slug = display.to_lowercase().replace(' ', "-");

    Block::new(
        format!("Dummy {display}"),
        PluginId::vst3(format!("com.dummy.{slug}"), format!("Dummy {display}")),
    )
    .with_block_type(block_type)
    .with_description(format!("Dummy {display} for testing"))
}

/// Create dummy blocks for all block types.
pub fn all_dummy_blocks() -> Vec<Block> {
    ALL_BLOCK_TYPES.iter().map(|&bt| dummy(bt)).collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_block_types_is_exhaustive() {
        // If someone adds a new BlockType variant, this count should be updated.
        // Current count: 33 variants.
        assert_eq!(ALL_BLOCK_TYPES.len(), 33);
    }

    #[test]
    fn dummy_block_is_not_placeholder() {
        for &bt in ALL_BLOCK_TYPES {
            let block = dummy(bt);
            assert!(
                !block.is_placeholder(),
                "Dummy {:?} should not be a placeholder",
                bt
            );
        }
    }

    #[test]
    fn dummy_block_has_correct_type() {
        for &bt in ALL_BLOCK_TYPES {
            let block = dummy(bt);
            assert_eq!(block.block_type, bt);
        }
    }

    #[test]
    fn dummy_block_names_are_unique() {
        let blocks = all_dummy_blocks();
        let names: Vec<&str> = blocks.iter().map(|b| b.name.as_str()).collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(names.len(), unique.len(), "duplicate dummy block names found");
    }

    #[test]
    fn dummy_block_plugin_ids_are_unique() {
        let blocks = all_dummy_blocks();
        let uids: Vec<&str> = blocks.iter().map(|b| b.plugin_id.uid.as_str()).collect();
        let mut unique = uids.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(uids.len(), unique.len(), "duplicate plugin UIDs found");
    }

    #[test]
    fn all_dummy_blocks_returns_all_types() {
        let blocks = all_dummy_blocks();
        assert_eq!(blocks.len(), ALL_BLOCK_TYPES.len());
    }
}
