//! Bass group

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Bass group
pub struct Bass;
impl SortingGroup for Bass {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "BASS",
            name: "Bass",
            prefix: "Bass",
            children: vec![],
        }
    }
}

