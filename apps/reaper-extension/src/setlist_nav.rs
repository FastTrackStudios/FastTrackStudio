//! Setlist navigation — next/previous song, section, and variation switching.
//!
//! When a setlist is loaded as pre-loaded track groups (one `[S]` folder per song,
//! `[L]` variation tracks inside), this module handles activation by muting/unmuting
//! the appropriate tracks.
//!
//! **Navigation levels:**
//! - Song: next/prev song folder (mutes current, unmutes target)
//! - Section: follows the song's section→variation mapping in order
//! - Variation: direct next/prev through unique variations (ignores section order)
//!
//! **Section mapping:** Each `[S]` folder track stores its section mapping in
//! REAPER ExtState (`P_EXT:FTS_SECTIONS`), formatted as `section:variation,...`.
//! This is written by the setlist builder and read by the nav module.

use daw::service::{ProjectContext, TrackService};
use std::sync::Mutex;
use tracing::info;

// ─── Types ───────────────────────────────────────────────────

/// A section within a song: name + which variation track to activate.
#[derive(Clone, Debug)]
struct Section {
    name: String,
    /// Index into the song's variation_indices (which [L] track to unmute)
    variation_idx: usize,
}

/// A loaded song with its track structure and section mapping.
#[derive(Clone, Debug)]
struct SongEntry {
    /// Display name (from [S] folder track)
    name: String,
    /// GUID of the [S] folder track
    folder_guid: String,
    /// GUIDs of [L] variation tracks (ordered by track position)
    variation_guids: Vec<String>,
    /// Names of [L] variation tracks (parallel to guids)
    variation_names: Vec<String>,
    /// Ordered section list (section name → variation index)
    sections: Vec<Section>,
}

/// Current navigation state.
struct NavState {
    songs: Vec<SongEntry>,
    /// Active song index
    song_idx: usize,
    /// Active section index within the song (follows section order)
    section_idx: usize,
}

static NAV_STATE: Mutex<Option<NavState>> = Mutex::new(None);

// ─── Scanning ────────────────────────────────────────────────

/// Scan the project for [S] song folders, their [L] children, and section mappings.
async fn scan_setlist() -> Vec<SongEntry> {
    let track_svc = daw::reaper::ReaperTrack::new();
    let tracks = track_svc.get_tracks(ProjectContext::Current).await;

    let mut songs = Vec::new();
    let mut i = 0;

    while i < tracks.len() {
        let track = &tracks[i];
        if track.name.starts_with("[S] ") && track.is_folder {
            let folder_guid = track.guid.clone();
            let song_name = track.name.clone();

            // Collect child [L] tracks
            let mut variation_guids = Vec::new();
            let mut variation_names = Vec::new();
            let mut depth: i32 = 1;
            let mut j = i + 1;

            while j < tracks.len() && depth > 0 {
                let child = &tracks[j];
                if child.name.starts_with("[L] ") {
                    variation_guids.push(child.guid.clone());
                    // Strip [L] prefix for matching
                    let var_name = child
                        .name
                        .strip_prefix("[L] ")
                        .unwrap_or(&child.name)
                        .to_string();
                    variation_names.push(var_name);
                }
                depth += child.folder_depth;
                j += 1;
            }

            // Read section mapping from ExtState on the folder track
            let sections = read_section_mapping(&track_svc, &folder_guid, &variation_names).await;

            songs.push(SongEntry {
                name: song_name,
                folder_guid,
                variation_guids,
                variation_names,
                sections,
            });

            i = j; // Skip past this folder's children
        } else {
            i += 1;
        }
    }

    songs
}

/// Read the section mapping from a track's ExtState.
/// Format: "Intro:Clean,Verse:Clean,Chorus:Crunch,Solo:Lead"
async fn read_section_mapping(
    track_svc: &daw::reaper::ReaperTrack,
    folder_guid: &str,
    variation_names: &[String],
) -> Vec<Section> {
    let request = daw::service::TrackExtStateRequest {
        section: "FTS_SECTIONS".to_string(),
        key: "mapping".to_string(),
        value: String::new(),
    };
    let result = track_svc
        .get_ext_state(
            ProjectContext::Current,
            daw::service::TrackRef::Guid(folder_guid.to_string()),
            request,
        )
        .await;

    let mapping_str = match result {
        Some(s) if !s.is_empty() => s,
        _ => {
            // No mapping stored — create a default one (one section per variation)
            return variation_names
                .iter()
                .enumerate()
                .map(|(idx, name)| Section {
                    name: name.clone(),
                    variation_idx: idx,
                })
                .collect();
        }
    };

    // Parse "SectionName:VariationName,..."
    mapping_str
        .split(',')
        .filter_map(|pair| {
            let (section_name, var_name) = pair.split_once(':')?;
            let var_idx = variation_names
                .iter()
                .position(|v| v == var_name.trim())
                .unwrap_or(0);
            Some(Section {
                name: section_name.trim().to_string(),
                variation_idx: var_idx,
            })
        })
        .collect()
}

/// Write a section mapping to a track's ExtState.
pub async fn write_section_mapping(
    folder_guid: &str,
    sections: &[(&str, &str)], // (section_name, variation_name)
) {
    let track_svc = daw::reaper::ReaperTrack::new();
    let mapping = sections
        .iter()
        .map(|(s, v)| format!("{s}:{v}"))
        .collect::<Vec<_>>()
        .join(",");

    let request = daw::service::TrackExtStateRequest {
        section: "FTS_SECTIONS".to_string(),
        key: "mapping".to_string(),
        value: mapping,
    };
    let _ = track_svc
        .set_ext_state(
            ProjectContext::Current,
            daw::service::TrackRef::Guid(folder_guid.to_string()),
            request,
        )
        .await;
}

// ─── Mute/unmute helpers ─────────────────────────────────────

async fn mute_song(song: &SongEntry) {
    let track_svc = daw::reaper::ReaperTrack::new();
    // Mute folder
    let _ = track_svc
        .set_muted(
            ProjectContext::Current,
            daw::service::TrackRef::Guid(song.folder_guid.clone()),
            true,
        )
        .await;
    // Mute all variations
    for guid in &song.variation_guids {
        let _ = track_svc
            .set_muted(
                ProjectContext::Current,
                daw::service::TrackRef::Guid(guid.clone()),
                true,
            )
            .await;
    }
}

async fn activate_section(song: &SongEntry, section: &Section) {
    let track_svc = daw::reaper::ReaperTrack::new();

    // Unmute folder
    let _ = track_svc
        .set_muted(
            ProjectContext::Current,
            daw::service::TrackRef::Guid(song.folder_guid.clone()),
            false,
        )
        .await;

    // Unmute only the target variation, mute others
    for (i, guid) in song.variation_guids.iter().enumerate() {
        let should_mute = i != section.variation_idx;
        let _ = track_svc
            .set_muted(
                ProjectContext::Current,
                daw::service::TrackRef::Guid(guid.clone()),
                should_mute,
            )
            .await;
    }
}

fn console_msg(msg: &str) {
    reaper_high::Reaper::get().show_console_msg(format!("{msg}\n"));
}

// ─── Public actions ──────────────────────────────────────────

/// Initialize setlist navigation: scan songs, mute all, activate first song's first section.
pub async fn init_setlist() {
    let songs = scan_setlist().await;
    if songs.is_empty() {
        console_msg("[Setlist] No [S] song folders found");
        return;
    }

    // Mute everything
    for song in &songs {
        mute_song(song).await;
    }

    // Activate first song, first section
    let first_section = songs[0].sections.first().cloned();
    if let Some(section) = &first_section {
        activate_section(&songs[0], section).await;
    }

    let song_count = songs.len();
    let song_name = &songs[0].name;
    let section_name = first_section
        .as_ref()
        .map(|s| s.name.as_str())
        .unwrap_or("?");

    console_msg(&format!(
        "[Setlist] Init: {song_count} songs — {song_name} / {section_name}"
    ));

    *NAV_STATE.lock().unwrap() = Some(NavState {
        songs,
        song_idx: 0,
        section_idx: 0,
    });
}

/// Switch to the next song (wraps around). Activates first section of the new song.
pub async fn next_song() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        s.song_idx = (s.song_idx + 1) % s.songs.len();
        s.section_idx = 0;
        (s.song_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, 0, &target.1).await;
}

/// Switch to the previous song (wraps around).
pub async fn prev_song() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        s.song_idx = if s.song_idx == 0 {
            s.songs.len() - 1
        } else {
            s.song_idx - 1
        };
        s.section_idx = 0;
        (s.song_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, 0, &target.1).await;
}

/// Advance to the next section within the current song.
/// If at the last section, wraps to the first section of the NEXT song.
pub async fn next_section() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        let song = &s.songs[s.song_idx];
        if s.section_idx + 1 < song.sections.len() {
            // Next section in same song
            s.section_idx += 1;
        } else {
            // Wrap to next song
            s.song_idx = (s.song_idx + 1) % s.songs.len();
            s.section_idx = 0;
        }
        (s.song_idx, s.section_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, target.1, &target.2).await;
}

/// Go back to the previous section.
/// If at the first section, wraps to the last section of the PREVIOUS song.
pub async fn prev_section() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        if s.section_idx > 0 {
            s.section_idx -= 1;
        } else {
            // Wrap to previous song, last section
            s.song_idx = if s.song_idx == 0 {
                s.songs.len() - 1
            } else {
                s.song_idx - 1
            };
            s.section_idx = s.songs[s.song_idx].sections.len().saturating_sub(1);
        }
        (s.song_idx, s.section_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, target.1, &target.2).await;
}

/// Direct variation cycling (ignores section order).
pub async fn next_variation() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        let song = &s.songs[s.song_idx];
        let var_count = song.variation_guids.len();
        if var_count == 0 {
            return;
        }

        // Find current variation index from current section
        let current_var = song
            .sections
            .get(s.section_idx)
            .map(|sec| sec.variation_idx)
            .unwrap_or(0);
        let next_var = (current_var + 1) % var_count;

        // Find any section that uses this variation (or create a synthetic one)
        if let Some(sec_idx) = song
            .sections
            .iter()
            .position(|sec| sec.variation_idx == next_var)
        {
            s.section_idx = sec_idx;
        }
        (s.song_idx, s.section_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, target.1, &target.2).await;
}

/// Direct variation cycling backward.
pub async fn prev_variation() {
    let target = {
        let mut state = NAV_STATE.lock().unwrap();
        let Some(ref mut s) = *state else {
            console_msg("[Setlist] Not initialized");
            return;
        };
        let song = &s.songs[s.song_idx];
        let var_count = song.variation_guids.len();
        if var_count == 0 {
            return;
        }

        let current_var = song
            .sections
            .get(s.section_idx)
            .map(|sec| sec.variation_idx)
            .unwrap_or(0);
        let prev_var = if current_var == 0 {
            var_count - 1
        } else {
            current_var - 1
        };

        if let Some(sec_idx) = song
            .sections
            .iter()
            .position(|sec| sec.variation_idx == prev_var)
        {
            s.section_idx = sec_idx;
        }
        (s.song_idx, s.section_idx, s.songs.clone())
    };
    switch_to_song_section(target.0, target.1, &target.2).await;
}

// ─── Internal ────────────────────────────────────────────────

async fn switch_to_song_section(song_idx: usize, section_idx: usize, songs: &[SongEntry]) {
    // Mute all songs
    for song in songs {
        mute_song(song).await;
    }

    // Activate the target
    if let Some(song) = songs.get(song_idx) {
        if let Some(section) = song.sections.get(section_idx) {
            activate_section(song, section).await;

            let var_name = song
                .variation_names
                .get(section.variation_idx)
                .map(|s| s.as_str())
                .unwrap_or("?");

            console_msg(&format!(
                "[Setlist] {} — {} ({})",
                song.name, section.name, var_name
            ));
            info!(
                "Setlist: song={} section={} variation={}",
                song.name, section.name, var_name
            );
        }
    }
}
