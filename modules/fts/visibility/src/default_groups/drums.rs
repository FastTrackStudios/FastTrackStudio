//! Drums group and subgroups

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Drums group
pub struct Drums;
impl SortingGroup for Drums {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "DRUMS",
            name: "Drums",
            prefix: "D",
            children: vec!["Kick", "Snare", "Hi-Hat", "Toms", "Cymbals", "Overheads", "Room"],
        }
    }
}

