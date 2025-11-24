//! Orchestra group and subgroups

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Orchestra group (with nested subgroups)
pub struct Orchestra;
impl SortingGroup for Orchestra {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "ORCHESTRA",
            name: "Orchestra",
            prefix: "Orch",
            children: vec!["Winds", "Brass", "Strings", "Orch Percussion", "Harp"],
        }
    }
}

