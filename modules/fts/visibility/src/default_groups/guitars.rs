//! Guitar groups (Electric and Acoustic)

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Electric Guitar group
pub struct GtrElectric;
impl SortingGroup for GtrElectric {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "GTR_ELECTRIC",
            name: "Gtr Electric",
            prefix: "GTR",
            children: vec![],
        }
    }
}

/// Acoustic Guitar group
pub struct GtrAcoustic;
impl SortingGroup for GtrAcoustic {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "GTR_ACOUSTIC",
            name: "Gtr Acoustic",
            prefix: "GTR",
            children: vec![],
        }
    }
}

