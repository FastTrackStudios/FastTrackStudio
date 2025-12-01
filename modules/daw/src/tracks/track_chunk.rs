//! Track State Chunk Parser
//!
//! Parses REAPER track state chunks (from GetTrackStateChunk) into track properties.
//! This avoids making many individual API calls by reading all properties from a single chunk.
//!
//! This parser converts REAPER state chunk format to daw::tracks::Track structs.

use crate::tracks::Track;
use crate::tracks::api::folder::{TcpFolderState, McpFolderState, TrackDepth, FolderDepthChange};
use crate::tracks::api::solo::SoloMode;
use crate::tracks::api::automation::AutomationMode;
use crate::tracks::api::timebase::TrackTimebase;
use crate::tracks::api::collapse::{ArrangeCollapseState, MixerCollapseState, WiringCollapseState, BusCompactSettings};

/// Parse a track state chunk into a Track struct
/// 
/// The chunk should be in the format returned by GetTrackStateChunk:
/// <TRACK
///   NAME "Track Name"
///   VOLPAN 0.8 0.0 -1.0
///   ...
/// >
pub fn parse_track_chunk(chunk: &str, default_index: usize) -> Result<ParsedTrackChunk, String> {
    let mut parsed = ParsedTrackChunk::default(default_index);
    
    for line in chunk.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        
        // Skip block markers
        if trimmed.starts_with('<') || trimmed == ">" {
            continue;
        }
        
        // Parse token line - split by whitespace
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();
        if tokens.is_empty() {
            continue;
        }
        
        let identifier = tokens[0];
        
        match identifier {
            "NAME" => {
                if tokens.len() > 1 {
                    let name = tokens[1..].join(" ");
                    // Remove quotes if present
                    let cleaned_name = name.trim_matches('"').trim_matches('\'').trim_matches('`').to_string();
                    parsed.name = cleaned_name;
                }
                // If NAME field exists but has no value, we leave the default name from ParsedTrackChunk::default
                // which is "Track {index}"
            }
            "LOCK" => {
                if tokens.len() > 1 {
                    parsed.locked = parse_int(tokens[1])? != 0;
                }
            }
            "PEAKCOL" => {
                if tokens.len() > 1 {
                    parsed.peak_color = Some(parse_int(tokens[1])?);
                }
            }
            "BEAT" => {
                if tokens.len() > 1 {
                    let beat_val = parse_int(tokens[1])?;
                    parsed.beat = Some(TrackTimebase::from_raw(beat_val));
                }
            }
            "AUTOMODE" => {
                if tokens.len() > 1 {
                    parsed.automation_mode = AutomationMode::from_raw(parse_int(tokens[1])?);
                }
            }
            "VOLPAN" => {
                if tokens.len() >= 2 {
                    // REAPER volume is in dB, convert to linear (0.0-1.0)
                    let vol_db = parse_float(tokens[1])?;
                    parsed.volume = (10.0_f64).powf(vol_db / 20.0);
                }
                if tokens.len() >= 3 {
                    parsed.pan = parse_float(tokens[2])?;
                }
                if tokens.len() >= 4 {
                    parsed.pan_law = Some(parse_float(tokens[3])?);
                }
                if tokens.len() >= 5 {
                    parsed.width = parse_float(tokens[4])?;
                }
            }
            "MUTESOLO" => {
                if tokens.len() >= 2 {
                    parsed.muted = parse_int(tokens[1])? != 0;
                }
                if tokens.len() >= 3 {
                    parsed.solo_state = SoloMode::from_raw(parse_int(tokens[2])?);
                }
                if tokens.len() >= 4 {
                    parsed.solo_defeat = parse_int(tokens[3])? != 0;
                }
            }
            "IPHASE" => {
                if tokens.len() > 1 {
                    parsed.invert_phase = parse_int(tokens[1])? != 0;
                }
            }
            "ISBUS" => {
                if tokens.len() >= 2 {
                    let folder_type = parse_int(tokens[1])?;
                    parsed.is_folder = folder_type == 1;
                    if tokens.len() >= 3 {
                        // ISBUS field 2 is the relative indentation change (folder_depth_change)
                        // This is the relative change: 0=normal, 1=folder start, -1=closes one level, etc.
                        let depth_change_value = parse_int(tokens[2])?;
                        parsed.folder_depth_change = FolderDepthChange::from_reaper_value(depth_change_value);
                        // Note: track_depth (absolute cumulative depth) will be calculated
                        // in the implementation layer by summing folder_depth_change values
                    }
                }
            }
            "BUSCOMP" => {
                if tokens.len() >= 2 {
                    let arrange_val = parse_int(tokens[1])?;
                    parsed.folder_state_tcp = Some(match arrange_val {
                        0 => TcpFolderState::Normal,
                        1 => TcpFolderState::Small,
                        2 => TcpFolderState::Collapsed,
                        _ => TcpFolderState::Normal,
                    });
                }
                if tokens.len() >= 3 {
                    let mixer_val = parse_int(tokens[2])?;
                    parsed.folder_state_mcp = Some(match mixer_val {
                        0 => McpFolderState::Normal,
                        1 => McpFolderState::Collapsed,
                        _ => McpFolderState::Normal,
                    });
                }
                // Parse full BUSCOMP for BusCompactSettings
                if tokens.len() >= 6 {
                    let arrange = ArrangeCollapseState::from_raw(parse_int(tokens[1])?);
                    let mixer = MixerCollapseState::from_raw(parse_int(tokens[2])?);
                    let wiring = WiringCollapseState::from_raw(parse_int(tokens[3])?);
                    let x = parse_int(tokens[4])?;
                    let y = if tokens.len() > 5 {
                        parse_int(tokens[5])?
                    } else {
                        0
                    };
                    
                    parsed.bus_compact = Some(BusCompactSettings {
                        arrange,
                        mixer,
                        wiring,
                        wiring_window_x: x,
                        wiring_window_y: y,
                    });
                }
            }
            "SHOWINMIX" => {
                if tokens.len() >= 2 {
                    parsed.show_in_mixer = parse_int(tokens[1])? != 0;
                }
                if tokens.len() >= 5 {
                    parsed.show_in_track_list = parse_int(tokens[4])? != 0;
                }
            }
            "NCHAN" => {
                if tokens.len() > 1 {
                    parsed.channel_count = parse_int(tokens[1])? as u32;
                }
            }
            "FX" => {
                if tokens.len() > 1 {
                    parsed.has_fx = parse_int(tokens[1])? != 0;
                }
            }
            "REC" => {
                if tokens.len() >= 2 {
                    parsed.record_armed = parse_int(tokens[1])? != 0;
                }
            }
            "I_CUSTOMCOLOR" => {
                if tokens.len() > 1 {
                    let color_val = parse_int(tokens[1])?;
                    // REAPER color format: 0xBBGGRR (BGR, not RGB)
                    // Convert to RGB packed format: (r << 16) | (g << 8) | b
                    if color_val != 0 {
                        let r = ((color_val >> 0) & 0xFF) as u8;
                        let g = ((color_val >> 8) & 0xFF) as u8;
                        let b = ((color_val >> 16) & 0xFF) as u8;
                        // Pack as RGB: (r << 16) | (g << 8) | b
                        parsed.color = Some((r as u32) << 16 | (g as u32) << 8 | b as u32);
                    }
                }
            }
            "TRACKID" => {
                if tokens.len() > 1 {
                    // TRACKID is the GUID in format {GUID} or just GUID
                    let guid_str = tokens[1..].join(" ");
                    // Remove quotes and braces if present
                    let guid = guid_str
                        .trim_matches('"')
                        .trim_matches('\'')
                        .trim_matches('`')
                        .trim_matches('{')
                        .trim_matches('}')
                        .to_string();
                    if !guid.is_empty() {
                        parsed.guid = Some(guid);
                    }
                }
            }
            _ => {
                // Ignore unknown fields
            }
        }
    }
    
    Ok(parsed)
}

/// Parsed track properties from chunk
pub struct ParsedTrackChunk {
    name: String,
    volume: f64,
    pan: f64,
    width: f64,
    pan_law: Option<f64>,
    muted: bool,
    solo_state: SoloMode,
    solo_defeat: bool,
    automation_mode: AutomationMode,
    record_armed: bool,
    has_fx: bool,
    show_in_mixer: bool,
    show_in_track_list: bool,
    color: Option<u32>,
    channel_count: u32,
    is_folder: bool,
    folder_state_tcp: Option<TcpFolderState>,
    folder_state_mcp: Option<McpFolderState>,
    track_depth: TrackDepth,
    folder_depth_change: FolderDepthChange,
    bus_compact: Option<BusCompactSettings>,
    locked: bool,
    peak_color: Option<i32>,
    beat: Option<TrackTimebase>,
    invert_phase: bool,
    pub guid: Option<String>,
}

impl ParsedTrackChunk {
    fn default(default_index: usize) -> Self {
        Self {
            name: format!("Track {}", default_index),
            volume: 1.0,
            pan: 0.0,
            width: 1.0,
            pan_law: None,
            muted: false,
            solo_state: SoloMode::Off,
            solo_defeat: false,
            automation_mode: AutomationMode::TrimRead,
            record_armed: false,
            has_fx: false,
            show_in_mixer: true,
            show_in_track_list: true,
            color: None,
            channel_count: 2,
            is_folder: false,
            folder_state_tcp: None,
            folder_state_mcp: None,
            track_depth: TrackDepth::default(),
            folder_depth_change: FolderDepthChange::Normal,
            bus_compact: None,
            locked: false,
            peak_color: None,
            beat: None,
            invert_phase: false,
            guid: None,
        }
    }
    
    /// Convert to daw::tracks::Track
    pub fn to_track(self, index: Option<usize>, selected: bool) -> Track {
        let mut track = Track::new(self.name);
        track.index = index;
        track.guid = self.guid;
        track.volume = self.volume;
        track.pan = self.pan;
        track.width = self.width;
        track.pan_law = self.pan_law;
        track.muted = self.muted;
        track.solo_state = self.solo_state;
        track.solo_defeat = self.solo_defeat;
        track.automation_mode = self.automation_mode;
        track.record_armed = self.record_armed;
        track.has_fx = self.has_fx;
        track.show_in_mixer = self.show_in_mixer;
        track.show_in_track_list = self.show_in_track_list;
        track.color = self.color;
        track.channel_count = self.channel_count;
        track.is_folder = self.is_folder;
        track.folder_state_tcp = self.folder_state_tcp;
        track.folder_state_mcp = self.folder_state_mcp;
        track.track_depth = self.track_depth;
        track.folder_depth_change = self.folder_depth_change;
        track.bus_compact = self.bus_compact;
        track.selected = selected;
        track.locked = self.locked;
        track.peak_color = self.peak_color;
        track.beat = self.beat;
        track.invert_phase = self.invert_phase;
        track
    }
}

/// Parse an integer from a string
fn parse_int(s: &str) -> Result<i32, String> {
    // Handle hex format
    if s.starts_with("0x") || s.starts_with("0X") {
        i32::from_str_radix(&s[2..], 16)
            .map_err(|e| format!("Invalid hex integer '{}': {}", s, e))
    } else {
        s.parse::<i32>()
            .map_err(|e| format!("Invalid integer '{}': {}", s, e))
    }
}

/// Parse a float from a string
fn parse_float(s: &str) -> Result<f64, String> {
    s.parse::<f64>()
        .map_err(|e| format!("Invalid float '{}': {}", s, e))
}

