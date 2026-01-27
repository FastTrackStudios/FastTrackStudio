//! Bridge between fts BlockType and audio-controls BlockCategory
//!
//! This module provides the single authoritative mapping between the two block
//! type systems used in different parts of the codebase.

use fts::rig::core::BlockType;
use audio_controls::prelude::BlockCategory;

/// Convert from fts::BlockType to audio_controls::BlockCategory.
///
/// This is the single source of truth for type mapping. All code that needs to
/// convert between the two systems should use this function.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(block_type_to_category(&BlockType::Compressor), BlockCategory::Dynamics);
/// assert_eq!(block_type_to_category(&BlockType::Delay), BlockCategory::Delay);
/// ```
pub fn block_type_to_category(block_type: &BlockType) -> BlockCategory {
    match block_type {
        BlockType::Input => BlockCategory::Input,
        BlockType::Compressor | BlockType::Gate => BlockCategory::Dynamics,
        BlockType::Eq => BlockCategory::Eq,
        BlockType::Drive => BlockCategory::Drive,
        BlockType::Amp => BlockCategory::Amp,
        BlockType::Cabinet => BlockCategory::Cabinet,
        BlockType::Modulation => BlockCategory::Modulation,
        BlockType::Delay => BlockCategory::Delay,
        BlockType::Reverb => BlockCategory::Reverb,
        BlockType::Volume | BlockType::Custom => BlockCategory::Utility,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_block_types_mapped() {
        // Ensure every BlockType variant is handled
        let test_types = vec![
            BlockType::Input,
            BlockType::Compressor,
            BlockType::Gate,
            BlockType::Eq,
            BlockType::Drive,
            BlockType::Amp,
            BlockType::Cabinet,
            BlockType::Modulation,
            BlockType::Delay,
            BlockType::Reverb,
            BlockType::Volume,
            BlockType::Custom,
        ];

        for block_type in test_types {
            let category = block_type_to_category(&block_type);
            // Just verify that conversion succeeds without panicking
            println!("{:?} -> {:?}", block_type, category);
        }
    }

    #[test]
    fn test_dynamics_mapping() {
        assert_eq!(
            block_type_to_category(&BlockType::Compressor),
            BlockCategory::Dynamics
        );
        assert_eq!(
            block_type_to_category(&BlockType::Gate),
            BlockCategory::Dynamics
        );
    }

    #[test]
    fn test_utility_mapping() {
        assert_eq!(
            block_type_to_category(&BlockType::Volume),
            BlockCategory::Utility
        );
        assert_eq!(
            block_type_to_category(&BlockType::Custom),
            BlockCategory::Utility
        );
    }
}
