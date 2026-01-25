//! Hook for syncing setlist service state to global signals
//!
//! This hook subscribes to the setlist service via ROAM and populates the global
//! signals that UI components read from:
//! - `SETLIST` - Full SetlistApi with progress (main signal for UI)
//! - `SETLIST_STRUCTURE` - Just the Setlist structure
//! - `ACTIVE_INDICES` - Current song/section/slide indices
//! - Individual progress signals for components that need them separately

use crate::context::setlist::use_setlist_service;
use dioxus::prelude::*;
use daw::primitives::TimeSignature;
use fts::setlist::service::{ActiveIndices, SongInfo, SectionInfo};
use fts::setlist::{ACTIVE_INDICES, SETLIST, SETLIST_STRUCTURE, SONG_PROGRESS, SECTION_PROGRESS, IS_PLAYING};
use fts::setlist::core::{Setlist, Song, Section, SectionType, section_from_seconds};
use fts::SetlistApi;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Hook that subscribes to setlist service events and updates global signals
///
/// This hook:
/// 1. Fetches the initial setlist structure from the service
/// 2. Subscribes to active indices changes via ROAM channels
/// 3. Updates global signals when the service notifies of changes
///
/// Call this hook in a component that wraps your setlist UI.
///
/// # Example
/// ```ignore
/// #[component]
/// fn SetlistApp() -> Element {
///     use_setlist_subscription();
///
///     rsx! {
///         Sidebar { /* reads from SETLIST_STRUCTURE */ }
///         MainContent { /* reads from SETLIST_STRUCTURE */ }
///     }
/// }
/// ```
pub fn use_setlist_subscription() {
    let ctx = use_setlist_service();

    // Initialize and subscribe on mount
    use_effect(move || {
        let client = ctx.client.clone();

        spawn(async move {
            tracing::info!("use_setlist_subscription: starting subscription");

            // Shared setlist structure that we'll update with progress on each tick
            let setlist_cache: Arc<RwLock<Option<Setlist>>> = Arc::new(RwLock::new(None));

            // Fetch initial setlist structure
            if let Some(info) = client.get_setlist().await {
                tracing::info!(
                    "use_setlist_subscription: loaded setlist '{}' with {} songs",
                    info.name,
                    info.song_count
                );

                // Build the full Setlist structure from service data
                let songs = client.get_songs().await;
                tracing::debug!("use_setlist_subscription: fetched {} songs", songs.len());

                // Build full Song structures with sections and measures
                let mut full_songs = Vec::with_capacity(songs.len());
                for song_info in &songs {
                    let sections = client.get_sections(song_info.index).await;
                    let measures = client.get_measures(song_info.index).await;
                    tracing::debug!(
                        "use_setlist_subscription: song '{}' has {} sections, {} measures",
                        song_info.name,
                        sections.len(),
                        measures.len()
                    );
                    let full_song = build_song_from_info(song_info, &sections, &measures);
                    full_songs.push(full_song);
                }

                // Create and store the full setlist
                let setlist = Setlist {
                    id: None,
                    name: info.name.clone(),
                    songs: full_songs,
                    metadata: HashMap::new(),
                };
                tracing::info!(
                    "use_setlist_subscription: storing setlist '{}' in SETLIST_STRUCTURE",
                    setlist.name
                );

                // Store in both the cache and the global signal
                *setlist_cache.write().unwrap() = Some(setlist.clone());
                *SETLIST_STRUCTURE.write() = Some(setlist);
            } else {
                tracing::warn!("use_setlist_subscription: no setlist available from service");
            }

            // Fetch initial active indices
            let indices = client.get_active_indices().await;
            tracing::info!(
                "use_setlist_subscription: initial indices song={:?}, section={:?}, song_progress={:?}",
                indices.song_index,
                indices.section_index,
                indices.song_progress
            );
            update_all_signals(&indices, &setlist_cache);

            // Create a channel for receiving active indices updates
            let (tx, mut rx) = roam::channel::<ActiveIndices>();

            // Subscribe to active indices changes
            client.subscribe_active(tx).await;
            tracing::debug!("use_setlist_subscription: subscribed to active indices updates");

            // Listen for updates and sync to global signals
            while let Ok(Some(indices)) = rx.recv().await {
                update_all_signals(&indices, &setlist_cache);
            }
            tracing::debug!("use_setlist_subscription: subscription loop ended");
        });
    });
}

/// Build a Song domain type from SongInfo, sections, and measures
fn build_song_from_info(
    info: &SongInfo,
    sections: &[SectionInfo],
    measures: &[fts::setlist::service::MeasureInfo],
) -> Song {
    use daw::primitives::{MusicalPosition, Position};

    let mut song = Song::new(info.name.clone()).unwrap_or_else(|_| Song::new("Unknown".to_string()).unwrap());

    // Set timing info - use markers to set positions
    song.starting_tempo = info.tempo;
    song.starting_time_signature = parse_time_signature(&info.time_signature);
    song.sections = sections.iter().map(build_section_from_info).collect();

    // Set start/end markers using marker builders
    use daw::marker_region::core::Marker;
    song.start_marker = Some(Marker::from_seconds(info.start, "SONGSTART".to_string()));
    song.song_end_marker = Some(Marker::from_seconds(info.end, "SONGEND".to_string()));

    // Build measure positions from MeasureInfo
    song.measure_positions = measures
        .iter()
        .map(|m| {
            let mut pos = Position::from_seconds(m.time_seconds);
            pos.musical = MusicalPosition::new(m.measure, 0, 0);
            pos
        })
        .collect();

    song
}

/// Build a Section domain type from SectionInfo
fn build_section_from_info(info: &SectionInfo) -> Section {
    use daw::primitives::{MusicalPosition, Position, TimePosition};

    let section_type = parse_section_type(&info.section_type);

    // Create section with proper positions including musical positions from server
    let mut section = section_from_seconds(
        section_type,
        info.start,
        info.end,
        info.name.clone(),
        info.number,
    ).unwrap_or_else(|e| {
        tracing::warn!("Failed to create section '{}': {}", info.name, e);
        // Fallback: create a minimal section
        section_from_seconds(
            SectionType::Custom("Unknown".to_string()),
            0.0,
            1.0,
            info.name.clone(),
            None,
        ).unwrap()
    });

    // Override musical positions with accurate data from server (if available)
    if let Some(start_measure) = info.start_measure {
        let mut start_pos = Position::from_seconds(info.start);
        start_pos.musical = MusicalPosition::new(start_measure, 0, 0);
        section.start_position = Some(start_pos);
    }

    if let Some(end_measure) = info.end_measure {
        let mut end_pos = Position::from_seconds(info.end);
        end_pos.musical = MusicalPosition::new(end_measure, 0, 0);
        section.end_position = Some(end_pos);
    }

    section
}

/// Parse section type string into SectionType enum
fn parse_section_type(type_str: &str) -> SectionType {
    match type_str.to_uppercase().as_str() {
        "VERSE" | "VS" => SectionType::Verse,
        "CHORUS" | "CH" => SectionType::Chorus,
        "BRIDGE" | "BR" => SectionType::Bridge,
        "INTRO" | "IN" => SectionType::Intro,
        "OUTRO" | "OUT" => SectionType::Outro,
        "PRE-CHORUS" | "PRECHORUS" | "PC" => SectionType::Pre(Box::new(SectionType::Chorus)),
        "INSTRUMENTAL" | "INST" => SectionType::Instrumental,
        "BREAKDOWN" | "BD" => SectionType::Breakdown,
        "INTERLUDE" | "IL" => SectionType::Interlude,
        "HITS" => SectionType::Hits,
        "COUNT-IN" | "COUNTIN" => SectionType::CountIn,
        "END" => SectionType::End,
        // Everything else becomes Custom (Solo, Hook, Tag, Turnaround, Vamp, etc.)
        _ => SectionType::Custom(type_str.to_string()),
    }
}

/// Parse time signature string like "4/4" into TimeSignature
fn parse_time_signature(ts_str: &Option<String>) -> Option<TimeSignature> {
    let ts_str = ts_str.as_ref()?;
    let parts: Vec<&str> = ts_str.split('/').collect();
    if parts.len() == 2 {
        let numerator = parts[0].parse().ok()?;
        let denominator = parts[1].parse().ok()?;
        Some(TimeSignature {
            numerator,
            denominator,
        })
    } else {
        None
    }
}

/// Update all global signals from service ActiveIndices
///
/// This updates:
/// - SETLIST (full SetlistApi with progress - main signal for UI components)
/// - ACTIVE_INDICES (song, section, slide indices)
/// - SONG_PROGRESS (0.0 to 1.0)
/// - SECTION_PROGRESS (0.0 to 1.0)
/// - IS_PLAYING (from the is_playing field in ActiveIndices)
fn update_all_signals(indices: &ActiveIndices, setlist_cache: &Arc<RwLock<Option<Setlist>>>) {
    tracing::debug!(
        "update_all_signals: song_progress={:?}, section_progress={:?}, is_playing={}",
        indices.song_progress,
        indices.section_progress,
        indices.is_playing
    );

    // Update SETLIST (the main signal that UI components read from)
    if let Some(setlist) = setlist_cache.read().unwrap().clone() {
        let api = SetlistApi::new(
            setlist,
            indices.song_index,
            indices.section_index,
            indices.slide_index,
            indices.song_progress,
            indices.section_progress,
        );
        tracing::debug!(
            "update_all_signals: updating SETLIST with song_progress={:?}",
            api.song_progress
        );
        *SETLIST.write() = Some(api);
    } else {
        tracing::warn!("update_all_signals: setlist_cache is empty, cannot update SETLIST");
    }

    // Update indices signal
    *ACTIVE_INDICES.write() = (
        indices.song_index,
        indices.section_index,
        indices.slide_index,
    );

    // Update individual progress signals (for components that need them separately)
    *SONG_PROGRESS.write() = indices.song_progress;
    *SECTION_PROGRESS.write() = indices.section_progress;

    // Update is_playing from the explicit field in ActiveIndices
    *IS_PLAYING.write() = indices.is_playing;
}

/// Hook that fetches initial setlist data and populates SETLIST_STRUCTURE
///
/// This is a simpler version that just loads the initial data without
/// continuous subscription. Use this when you only need to load once.
///
/// NOTE: Prefer `use_setlist_subscription()` for full functionality including
/// live progress updates.
pub fn use_setlist_init() {
    // Just delegate to the full subscription hook
    use_setlist_subscription();
}
