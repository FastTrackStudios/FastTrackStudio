//! Media item data structures for REAPER

use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

/// Fade curve types for items
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(i32)]
pub enum FadeCurveType {
    Linear = 0,
    Square = 1,
    SlowStartEnd = 2,
    FastStart = 3,
    FastEnd = 4,
    Bezier = 5,
    Unknown(i32),
}

impl From<i32> for FadeCurveType {
    fn from(value: i32) -> Self {
        match value {
            0 => FadeCurveType::Linear,
            1 => FadeCurveType::Square,
            2 => FadeCurveType::SlowStartEnd,
            3 => FadeCurveType::FastStart,
            4 => FadeCurveType::FastEnd,
            5 => FadeCurveType::Bezier,
            _ => FadeCurveType::Unknown(value),
        }
    }
}

impl fmt::Display for FadeCurveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FadeCurveType::Linear => write!(f, "Linear"),
            FadeCurveType::Square => write!(f, "Square"),
            FadeCurveType::SlowStartEnd => write!(f, "Slow Start/End"),
            FadeCurveType::FastStart => write!(f, "Fast Start"),
            FadeCurveType::FastEnd => write!(f, "Fast End"),
            FadeCurveType::Bezier => write!(f, "Bezier"),
            FadeCurveType::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// Channel mode for takes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(i32)]
pub enum ChannelMode {
    Normal = 0,
    ReverseStereo = 1,
    MonoDownmix = 2,
    MonoLeft = 3,
    MonoRight = 4,
    MonoChannel(u8),  // 5-194 for mono channels 3-128
    StereChannel(u8), // 67-257 for stereo channels 1-128
    Unknown(i32),
}

impl From<i32> for ChannelMode {
    fn from(value: i32) -> Self {
        match value {
            0 => ChannelMode::Normal,
            1 => ChannelMode::ReverseStereo,
            2 => ChannelMode::MonoDownmix,
            3 => ChannelMode::MonoLeft,
            4 => ChannelMode::MonoRight,
            5..=66 => ChannelMode::MonoChannel((value - 2) as u8), // 5-66 -> 3-64
            67..=130 => ChannelMode::StereChannel((value - 66) as u8), // 67-130 -> 1-64
            131..=194 => ChannelMode::MonoChannel((value - 66) as u8), // 131-194 -> 65-128
            195..=257 => ChannelMode::StereChannel((value - 128) as u8), // 195-257 -> 67-128
            _ => ChannelMode::Unknown(value),
        }
    }
}

impl fmt::Display for ChannelMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChannelMode::Normal => write!(f, "Normal"),
            ChannelMode::ReverseStereo => write!(f, "Reverse Stereo"),
            ChannelMode::MonoDownmix => write!(f, "Mono (Downmix)"),
            ChannelMode::MonoLeft => write!(f, "Mono (Left)"),
            ChannelMode::MonoRight => write!(f, "Mono (Right)"),
            ChannelMode::MonoChannel(ch) => write!(f, "Mono (Channel {})", ch),
            ChannelMode::StereChannel(ch) => write!(f, "Stereo (Channels {}/{})", ch, ch + 1),
            ChannelMode::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// Pitch shifting and time stretch modes for PLAYRATE field 4
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(i32)]
pub enum PitchMode {
    ProjectDefault = -1,
    SoundTouchPreset1 = 0,
    SoundTouchPreset2 = 1,
    SoundTouchPreset3 = 2,
    DiracLE(u8),              // 0-31 presets (65536-65567)
    LowQualityWindowed(u8),   // 0-47 presets (131072-131119)
    ElastiquePro(u8),         // 0-31 presets (196608-196639)
    ElastiqueEfficient(u8),   // 0-3 presets (262144-262147)
    ElastiqueSoloist(u8),     // 0-3 presets (327680-327683)
    Elastique21Pro(u8),       // 0-31 presets (393216-393247)
    Elastique21Efficient(u8), // 0-3 presets (458752-458755)
    Elastique21Soloist(u8),   // 0-3 presets (524288-524291)
    Unknown(i32),
}

impl From<i32> for PitchMode {
    fn from(value: i32) -> Self {
        match value {
            -1 => PitchMode::ProjectDefault,
            0 => PitchMode::SoundTouchPreset1,
            1 => PitchMode::SoundTouchPreset2,
            2 => PitchMode::SoundTouchPreset3,
            65536..=65567 => PitchMode::DiracLE((value - 65536) as u8),
            131072..=131119 => PitchMode::LowQualityWindowed((value - 131072) as u8),
            196608..=196639 => PitchMode::ElastiquePro((value - 196608) as u8),
            262144..=262147 => PitchMode::ElastiqueEfficient((value - 262144) as u8),
            327680..=327683 => PitchMode::ElastiqueSoloist((value - 327680) as u8),
            393216..=393247 => PitchMode::Elastique21Pro((value - 393216) as u8),
            458752..=458755 => PitchMode::Elastique21Efficient((value - 458752) as u8),
            524288..=524291 => PitchMode::Elastique21Soloist((value - 524288) as u8),
            _ => PitchMode::Unknown(value),
        }
    }
}

impl fmt::Display for PitchMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PitchMode::ProjectDefault => write!(f, "Project Default"),
            PitchMode::SoundTouchPreset1 => write!(f, "Sound Touch (Preset 1)"),
            PitchMode::SoundTouchPreset2 => write!(f, "Sound Touch (Preset 2)"),
            PitchMode::SoundTouchPreset3 => write!(f, "Sound Touch (Preset 3)"),
            PitchMode::DiracLE(preset) => write!(f, "Dirac LE (Preset {})", preset + 1),
            PitchMode::LowQualityWindowed(preset) => {
                write!(f, "Low Quality Windowed (Preset {})", preset + 1)
            }
            PitchMode::ElastiquePro(preset) => write!(f, "élastique Pro (Preset {})", preset + 1),
            PitchMode::ElastiqueEfficient(preset) => {
                write!(f, "élastique Efficient (Preset {})", preset + 1)
            }
            PitchMode::ElastiqueSoloist(preset) => {
                write!(f, "élastique SOLOIST (Preset {})", preset + 1)
            }
            PitchMode::Elastique21Pro(preset) => {
                write!(f, "élastique 2.1 Pro (Preset {})", preset + 1)
            }
            PitchMode::Elastique21Efficient(preset) => {
                write!(f, "élastique 2.1 Efficient (Preset {})", preset + 1)
            }
            PitchMode::Elastique21Soloist(preset) => {
                write!(f, "élastique 2.1 SOLOIST (Preset {})", preset + 1)
            }
            PitchMode::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// Solo states for MUTE field 2
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(i32)]
pub enum SoloState {
    NotSoloed = 0,
    Soloed = -1,
    SoloOverridden = 1,
    Unknown(i32),
}

impl From<i32> for SoloState {
    fn from(value: i32) -> Self {
        match value {
            0 => SoloState::NotSoloed,
            -1 => SoloState::Soloed,
            1 => SoloState::SoloOverridden,
            _ => SoloState::Unknown(value),
        }
    }
}

impl fmt::Display for SoloState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SoloState::NotSoloed => write!(f, "Not Soloed"),
            SoloState::Soloed => write!(f, "Soloed"),
            SoloState::SoloOverridden => write!(f, "Solo Overridden"),
            SoloState::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// Source types for SOURCE blocks
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceType {
    Wave,
    Midi,
    OfflineWave,
    Unknown(String),
}

impl From<&str> for SourceType {
    fn from(value: &str) -> Self {
        match value.to_uppercase().as_str() {
            "WAVE" => SourceType::Wave,
            "MIDI" => SourceType::Midi,
            "_OFFLINE_WAVE" => SourceType::OfflineWave,
            _ => SourceType::Unknown(value.to_string()),
        }
    }
}

impl fmt::Display for SourceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SourceType::Wave => write!(f, "Wave"),
            SourceType::Midi => write!(f, "MIDI"),
            SourceType::OfflineWave => write!(f, "Offline Wave"),
            SourceType::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// Item timebase for BEAT field
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(i32)]
pub enum ItemTimebase {
    ProjectDefault = -1,
    Time = 0,
    Beats = 1,
    Unknown(i32),
}

impl From<i32> for ItemTimebase {
    fn from(value: i32) -> Self {
        match value {
            -1 => ItemTimebase::ProjectDefault,
            0 => ItemTimebase::Time,
            1 => ItemTimebase::Beats,
            _ => ItemTimebase::Unknown(value),
        }
    }
}

impl fmt::Display for ItemTimebase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ItemTimebase::ProjectDefault => write!(f, "Project Default"),
            ItemTimebase::Time => write!(f, "Time"),
            ItemTimebase::Beats => write!(f, "Beats"),
            ItemTimebase::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}

/// A REAPER media item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Builder)]
#[builder(setter(into), default)]
pub struct Item {
    // Item ID
    pub id: Uuid,

    // Basic item properties
    pub position: f64,              // POSITION - Position on timeline in seconds
    pub snap_offset: f64,           // SNAPOFFS - Snap offset in seconds
    pub length: f64,                // LENGTH - Item length in seconds
    pub loop_source: bool,          // LOOP - Loop source flag
    pub play_all_takes: bool,       // ALLTAKES - Play all takes flag
    pub color: Option<i32>,         // COLOR - Item color (optional)
    pub beat: Option<ItemTimebase>, // BEAT - Item timebase (optional)
    pub selected: bool,             // SEL - Is item selected

    // Fade settings
    pub fade_in: Option<FadeSettings>,  // FADEIN - Fade in settings
    pub fade_out: Option<FadeSettings>, // FADEOUT - Fade out settings

    // Mute/Solo settings
    pub mute: Option<MuteSettings>, // MUTE - Mute and solo settings

    // Item identification
    pub item_guid: Option<String>, // IGUID - Item GUID
    pub item_id: Option<i32>,      // IID - Item ordinal number (deprecated)

    // Item properties
    pub name: String,                       // NAME - Item name
    pub volpan: Option<VolPanSettings>,     // VOLPAN - Volume and pan settings
    pub slip_offset: f64,                   // SOFFS - Slip offset in seconds
    pub playrate: Option<PlayRateSettings>, // PLAYRATE - Play rate settings
    pub channel_mode: ChannelMode,          // CHANMODE - Channel mode
    pub take_guid: Option<String>,          // GUID - Take GUID
    pub rec_pass: Option<i32>,              // RECPASS - Recording pass number

    // Takes
    pub takes: Vec<Take>,
}

/// Fade settings for an item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FadeSettings {
    pub curve_type: FadeCurveType, // field 1 - fade curve type
    pub time: f64,                 // field 2 - fade time in seconds
    pub unknown_field_3: f64,      // field 3 - unknown
    pub unknown_field_4: i32,      // field 4 - unknown
    pub unknown_field_5: i32,      // field 5 - unknown
    pub unknown_field_6: i32,      // field 6 - unknown
    pub unknown_field_7: i32,      // field 7 - unknown
}

/// Mute and solo settings for an item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MuteSettings {
    pub muted: bool,           // field 1 - item is muted
    pub solo_state: SoloState, // field 2 - solo state (-1, 0, 1)
}

/// Volume and pan settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VolPanSettings {
    pub item_trim: f64,    // field 1 - item trim (1.0 = 0 dB)
    pub take_pan: f64,     // field 2 - take pan (-1.0 to 1.0)
    pub take_volume: f64,  // field 3 - take volume
    pub take_pan_law: f64, // field 4 - take pan law
}

/// Play rate settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlayRateSettings {
    pub rate: f64,             // field 1 - play rate
    pub preserve_pitch: bool,  // field 2 - preserve pitch while changing rate
    pub pitch_adjust: f64,     // field 3 - pitch adjust in semitones.cents
    pub pitch_mode: PitchMode, // field 4 - pitch shifting/time stretch mode
    pub unknown_field_5: i32,  // field 5 - unknown
    pub unknown_field_6: f64,  // field 6 - unknown
}

/// A take within a media item
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Take {
    pub is_selected: bool,                  // TAKE SEL - Is this take selected
    pub name: String,                       // NAME - Take name
    pub volpan: Option<VolPanSettings>,     // TAKEVOLPAN - Take volume and pan
    pub slip_offset: f64,                   // SOFFS - Take slip offset
    pub playrate: Option<PlayRateSettings>, // PLAYRATE - Take play rate
    pub channel_mode: ChannelMode,          // CHANMODE - Take channel mode
    pub take_color: Option<i32>,            // TAKECOLOR - Take color
    pub take_guid: Option<String>,          // GUID - Take GUID
    pub rec_pass: Option<i32>,              // RECPASS - Recording pass number
    pub source: Option<SourceBlock>,        // SOURCE block
}

/// Source block for a take
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceBlock {
    pub source_type: SourceType, // WAVE, MIDI, etc.
    pub file_path: String,       // FILE - Source file path
}

impl Default for Item {
    fn default() -> Self {
        Self {
            id: Uuid::new_v4(),
            position: 0.0,
            snap_offset: 0.0,
            length: 0.0,
            loop_source: false,
            play_all_takes: false,
            color: None,
            beat: None,
            selected: false,
            fade_in: None,
            fade_out: None,
            mute: None,
            item_guid: None,
            item_id: None,
            name: String::new(),
            volpan: None,
            slip_offset: 0.0,
            playrate: None,
            channel_mode: ChannelMode::Normal,
            take_guid: None,
            rec_pass: None,
            takes: Vec::new(),
        }
    }
}

impl Item {
    /// Create a new empty item
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new item with a name
    pub fn with_name(name: String) -> Self {
        Self {
            name,
            ..Self::default()
        }
    }

    /// Check if this item is an audio item (has a WAVE source)
    pub fn is_audio(&self) -> bool {
        self.takes.iter().any(|take| {
            take.source
                .as_ref()
                .map(|s| matches!(s.source_type, SourceType::Wave | SourceType::OfflineWave))
                .unwrap_or(false)
        })
    }

    /// Check if this item is a MIDI item (has a MIDI source)
    pub fn is_midi(&self) -> bool {
        self.takes.iter().any(|take| {
            take.source
                .as_ref()
                .map(|s| matches!(s.source_type, SourceType::Midi))
                .unwrap_or(false)
        })
    }
}
