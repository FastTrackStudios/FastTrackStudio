//! Default group configurations
//!
//! Creates default GroupConfig instances for all groups.
//! These are used by the parser to match track names to groups.

use crate::smart_template::config::group_config::GroupConfig;
use crate::smart_template::naming::implementations::drum_kit::{
    kick::KickParser,
    snare::SnareParser,
    tom::TomParser,
    cymbals::CymbalsParser,
    room::RoomParser,
};
use crate::smart_template::naming::implementations::bass::bass::BassParser;
use crate::smart_template::naming::implementations::guitar_electric::guitar_electric::GuitarElectricParser;
use crate::smart_template::naming::implementations::guitar_acoustic::guitar_acoustic::GuitarAcousticParser;
use crate::smart_template::naming::implementations::keys::keys::KeysParser;
use crate::smart_template::naming::implementations::synths::synths::SynthsParser;
use crate::smart_template::naming::implementations::vocals::{
    vocals::VocalsParser,
    bgvs::BGVsParser,
};

/// Create all default groups as GroupConfig
/// 
/// This collects GroupConfigs from all the group parsers.
pub fn create_default_groups() -> Vec<GroupConfig> {
    vec![
        // Drum kit groups
        KickParser::default_kick_config(),
        SnareParser::default_snare_config(),
        TomParser::default_tom_config(),
        CymbalsParser::default_cymbals_config(),
        RoomParser::default_room_config(),
        
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
    ]
}
