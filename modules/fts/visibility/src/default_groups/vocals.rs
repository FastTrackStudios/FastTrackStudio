//! Vocal groups (Vocals, Background Vocals, Choir)

use super::super::group::SortingGroup;
use super::super::group_config::GroupConfig;

/// Vocals group
pub struct Vocals;
impl SortingGroup for Vocals {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "VOCALS",
            name: "Vocals",
            prefix: "V",
            children: vec![],
        }
    }
}

/// Background Vocals group
pub struct VBgvs;
impl SortingGroup for VBgvs {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "V_BGVS",
            name: "V BGVs",
            prefix: "V",
            children: vec![],
        }
    }
}

/// Choir group
pub struct VChoir;
impl SortingGroup for VChoir {
    fn config() -> GroupConfig {
        GroupConfig {
            id: "V_CHOIR",
            name: "V Choir",
            prefix: "V",
            children: vec![],
        }
    }
}

