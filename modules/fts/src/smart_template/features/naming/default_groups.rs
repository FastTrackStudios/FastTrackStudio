//! Default group configurations
//!
//! Creates default GroupConfig instances for all groups.
//! These are used by the parser to match track names to groups.

use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::presets::drums::{
    Kick,
    Snare,
    Tom,
    Cymbals,
    Room,
};
use crate::smart_template::presets::bass::naming::BassParser;
use crate::smart_template::presets::guitar_electric::naming::GuitarElectricParser;
use crate::smart_template::presets::guitar_acoustic::naming::GuitarAcousticParser;
use crate::smart_template::presets::keys::naming::KeysParser;
use crate::smart_template::presets::synths::naming::SynthsParser;
use crate::smart_template::presets::vocals::{
    VocalsParser,
    BGVsParser,
};

/// Create all default groups as GroupConfig
/// 
/// This collects GroupConfigs from all the group parsers.
/// The NOT_SORTED group is always added last as a catch-all for unmatched items.
pub fn create_default_groups() -> Vec<GroupConfig> {
    let mut groups = vec![
        // Drum kit groups
        Kick::new().config,
        Snare::new().config,
        Tom::new().config,
        Cymbals::new().config,
        Room::new().config,
        
        // Bass group
        BassParser::default_bass_config(),
        
        // Guitar groups
        GuitarElectricParser::default_guitar_electric_config(),
        GuitarAcousticParser::default_guitar_acoustic_config(),
        
        // Keys group
        KeysParser::default_keys_config(),
        
        // Synths group
        SynthsParser::default_synths_config(),
        
        // Vocals groups
        VocalsParser::default_vocals_config(),
        BGVsParser::default_bgvs_config(),
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
