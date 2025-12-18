//! Default group configurations
//!
//! Creates default GroupConfig instances for all groups.
//! These are used by the parser to match track names to groups.

use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::Group;
use crate::smart_template::presets::drums::{
    Kick,
    Snare,
    Tom,
    Cymbals,
    Room,
};
use crate::smart_template::presets::bass::Bass;
use crate::smart_template::presets::guitar_electric::GuitarElectric;
use crate::smart_template::presets::guitar_acoustic::GuitarAcoustic;
use crate::smart_template::presets::keys::Keys;
use crate::smart_template::presets::synths::Synths;
use crate::smart_template::presets::vocals::{
    Vocals,
    BGVs,
};

/// Create all default groups as GroupConfig
/// 
/// This collects GroupConfigs from all the group parsers.
/// The NOT_SORTED group is always added last as a catch-all for unmatched items.
pub fn create_default_groups() -> Vec<GroupConfig> {
    let mut groups = vec![
        // Drum kit groups
        Kick::new().group_config(),
        Snare::new().group_config(),
        Tom::new().group_config(),
        Cymbals::new().group_config(),
        Room::new().group_config(),
        
        // Bass group
        Bass::new().group_config(),
        
        // Guitar groups
        GuitarElectric::new().group_config(),
        GuitarAcoustic::new().group_config(),
        
        // Keys group
        Keys::new().group_config(),
        
        // Synths group
        Synths::new().group_config(),
        
        // Vocals groups
        Vocals::new().group_config(),
        BGVs::new().group_config(),
    ];
    
    // Add NOT_SORTED group last as catch-all (lowest priority)
    groups.push(create_not_sorted_group());
    
    groups
}

/// Create the NOT_SORTED group configuration
/// 
/// This group is used as a catch-all for track names that don't match
/// any other group. It has the lowest priority and matches everything.
pub fn create_not_sorted_group() -> GroupConfig {
    let mut config = GroupConfig::default();
    config.name = "NOT_SORTED".to_string();
    config.prefix = "UNSORTED".to_string();
    config.patterns = vec![]; // Empty patterns means it matches everything (as fallback)
    config.create_if_missing = Some(false); // Don't auto-create unsorted tracks
    config.priority = Some(i32::MIN); // Lowest priority - only matches if nothing else does
    config
}
