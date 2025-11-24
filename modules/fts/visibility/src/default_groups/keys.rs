//! Keys group

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Keys group
pub struct Keys;
impl SortingGroup for Keys {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "KEYS",
            name: "Keys",
            prefix: "Keys",
            children: vec![],
        }
    }
}

