//! Synths group

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Synths group
pub struct Synths;
impl SortingGroup for Synths {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "SYNTHS",
            name: "Synths",
            prefix: "Synth",
            children: vec![],
        }
    }
}

