//! Fixed item lanes settings (REAPER 7+)

use serde::{Deserialize, Serialize};

/// Fixed lanes settings (REAPER 7+)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FixedLanesSettings {
    pub bitfield: i32,             // field 1 - bitfield for various options
    pub allow_editing: bool,       // field 2 - allow editing source media while comping
    pub show_play_only_lane: bool, // field 3 - show/play only lane
    pub mask_playback: bool,       // field 4 - media items in higher numbered lanes mask playback
    pub recording_behavior: i32,   // field 5 - recording behavior bitfield
}

/// Lane solo settings (REAPER 7+)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LaneSoloSettings {
    pub playing_lanes: i32,   // field 1 - bitfield of playing lanes
    pub unknown_field_2: i32, // field 2 - unknown
    pub unknown_field_3: i32, // field 3 - unknown
    pub unknown_field_4: i32, // field 4 - unknown
    pub unknown_field_5: i32, // field 5 - unknown
    pub unknown_field_6: i32, // field 6 - unknown
    pub unknown_field_7: i32, // field 7 - unknown
    pub unknown_field_8: i32, // field 8 - unknown
}

/// Lane record settings (REAPER 7+)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LaneRecordSettings {
    pub record_enabled_lane: i32, // field 1 - 0-based index of record enabled lane
    pub comping_enabled_lane: i32, // field 2 - 0-based index of comping enabled lane
    pub last_comping_lane: i32,   // field 3 - 0-based index of last comping enabled lane
}

/// Lane name settings (REAPER 7+)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LaneNameSettings {
    pub lane_count: i32,         // field 1 - number of lanes
    pub lane_names: Vec<String>, // field 2+ - lane names
}
