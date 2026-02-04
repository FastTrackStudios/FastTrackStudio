//! SetlistService implementation

use crate::setlist_builder::SetlistBuilder;
use daw_control::Daw;
use roam::Tx;
use roam::session::Context;
use session_proto::{
    ActiveIndices, MeasureInfo, QueuedTarget, Section, Setlist, SetlistEvent, SetlistService, Song,
    SongTransportState,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Implementation of SetlistService
#[derive(Clone)]
pub struct SetlistServiceImpl {
    /// Current setlist state
    setlist: Arc<RwLock<Option<Setlist>>>,
    /// Currently active song ID (cached locally to avoid RPC calls)
    /// Using ID instead of index ensures stability when songs are reordered
    active_song_id: Arc<RwLock<Option<String>>>,
    /// Cached active indices (updated by polling loop at 60Hz)
    /// Used for instant navigation without RPC calls
    cached_indices: Arc<RwLock<ActiveIndices>>,
    /// Queued navigation target (flashes in UI until transport reaches it)
    /// Only one target can be queued at a time
    queued_target: Arc<RwLock<Option<QueuedTarget>>>,
}

impl SetlistServiceImpl {
    pub fn new() -> Self {
        Self {
            setlist: Arc::new(RwLock::new(None)),
            active_song_id: Arc::new(RwLock::new(None)),
            cached_indices: Arc::new(RwLock::new(ActiveIndices::default())),
            queued_target: Arc::new(RwLock::new(None)),
        }
    }

    /// Get the cached active indices (updated by polling loop, no RPC calls)
    async fn get_cached_indices(&self) -> ActiveIndices {
        self.cached_indices.read().await.clone()
    }

    /// Update cached indices (called by polling loop)
    async fn set_cached_indices(&self, indices: ActiveIndices) {
        *self.cached_indices.write().await = indices;
    }

    /// Set a queued navigation target
    async fn queue_target(&self, target: QueuedTarget) {
        *self.queued_target.write().await = Some(target);
    }

    /// Clear the queued navigation target
    async fn clear_queued_target(&self) {
        *self.queued_target.write().await = None;
    }

    /// Get the current queued target
    async fn get_queued_target(&self) -> Option<QueuedTarget> {
        self.queued_target.read().await.clone()
    }

    /// Check if the transport has reached the queued target and clear it if so
    async fn check_and_clear_queue(
        &self,
        song_index: usize,
        section_index: Option<usize>,
        position_seconds: f64,
    ) {
        let queued = self.queued_target.read().await.clone();
        if let Some(target) = queued {
            let reached = match &target {
                QueuedTarget::Section {
                    song_index: q_song,
                    section_index: q_section,
                } => song_index == *q_song && section_index == Some(*q_section),
                QueuedTarget::Time {
                    song_index: q_song,
                    position_seconds: q_pos,
                } => {
                    // Consider reached if within 0.1 seconds
                    song_index == *q_song && (position_seconds - q_pos).abs() < 0.1
                }
                QueuedTarget::Measure {
                    song_index: q_song, ..
                } => {
                    // For measures, we'd need to check musical position - for now just check song
                    song_index == *q_song
                }
                QueuedTarget::Comment {
                    song_index: q_song,
                    position_seconds: q_pos,
                } => {
                    // Consider reached if within 0.1 seconds
                    song_index == *q_song && (position_seconds - q_pos).abs() < 0.1
                }
            };

            if reached {
                self.clear_queued_target().await;
            }
        }
    }

    /// Get a specific song by index (internal helper)
    async fn get_song_internal(&self, index: usize) -> Option<session_proto::Song> {
        let setlist = self.setlist.read().await;
        setlist.as_ref()?.songs.get(index).cloned()
    }

    /// Get a specific song by ID (internal helper)
    async fn get_song_by_id(&self, id: &str) -> Option<Song> {
        let setlist = self.setlist.read().await;
        let setlist = setlist.as_ref()?;
        setlist.songs.iter().find(|song| song.id == id).cloned()
    }

    /// Get the active song from cached local state (no RPC calls)
    async fn get_cached_active_song(&self) -> Option<Song> {
        let song_id = self.active_song_id.read().await.clone();
        let song_id = song_id?;
        self.get_song_by_id(&song_id).await
    }

    /// Set the active song by ID (called when navigating)
    async fn set_active_song_id(&self, id: &str) {
        *self.active_song_id.write().await = Some(id.to_string());
    }

    /// Calculate transport state for a specific song based on its project's transport
    fn calculate_song_transport(
        song: &Song,
        song_index: usize,
        position: daw_proto::Position,
        is_playing: bool,
        is_looping: bool,
        loop_region: Option<daw_proto::LoopRegion>,
        tempo: f64,
        time_sig: (u32, u32),
    ) -> SongTransportState {
        let song_duration = song.duration();
        let song_start = song.start_seconds();

        // Get position in seconds for progress calculations
        let position_seconds = position.time.map(|t| t.as_seconds()).unwrap_or(0.0);

        // Calculate progress within song
        let relative_pos = position_seconds - song_start;
        let progress = if song_duration > 0.0 {
            (relative_pos / song_duration).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Find section at position
        let (section_index, section_progress) = if let Some((sec_idx, section)) =
            song.section_at_position_with_index(position_seconds)
        {
            let sec_duration = section.duration();
            let sec_progress = if sec_duration > 0.0 {
                ((position_seconds - section.start_seconds) / sec_duration).clamp(0.0, 1.0)
            } else {
                0.0
            };
            (Some(sec_idx), Some(sec_progress))
        } else {
            (None, None)
        };

        // Convert loop region from project-absolute to song-relative coordinates
        let song_loop_region = loop_region.and_then(|region| {
            // Only include loop region if it overlaps with the song
            let region_start_relative = region.start_seconds - song_start;
            let region_end_relative = region.end_seconds - song_start;

            // Check if loop region is within song bounds (with some tolerance)
            if region_end_relative > 0.0 && region_start_relative < song_duration {
                Some(daw_proto::LoopRegion::new(
                    region_start_relative.max(0.0),
                    region_end_relative.min(song_duration),
                ))
            } else {
                None
            }
        });

        SongTransportState {
            song_index,
            position,
            progress,
            section_index,
            section_progress,
            is_playing,
            is_looping,
            loop_region: song_loop_region,
            bpm: tempo,
            time_sig_num: time_sig.0,
            time_sig_denom: time_sig.1,
        }
    }

    /// Get transport state for ALL songs by querying each project
    ///
    /// Uses `get_state()` which returns all transport info in ONE RPC call per project,
    /// rather than making 5 separate calls (position, is_playing, is_looping, tempo, time_sig).
    async fn get_all_song_transports(&self) -> Vec<SongTransportState> {
        let daw = Daw::get();
        let setlist = self.setlist.read().await;
        let Some(ref setlist) = *setlist else {
            return Vec::new();
        };

        let mut transports = Vec::with_capacity(setlist.songs.len());

        for (song_index, song) in setlist.songs.iter().enumerate() {
            // Get the project for this song
            match daw.project(&song.project_guid).await {
                Ok(project) => {
                    let transport = project.transport();

                    // Get full transport state in ONE RPC call
                    match transport.get_state().await {
                        Ok(state) => {
                            let is_playing = state.play_state == daw_proto::PlayState::Playing
                                || state.play_state == daw_proto::PlayState::Recording;

                            // Use playhead position when playing, edit cursor when stopped
                            // The Position includes both time and musical position from REAPER's tempo map
                            let position = if is_playing {
                                state.playhead_position.clone()
                            } else {
                                state.edit_position.clone()
                            };

                            let is_looping = state.looping;
                            let loop_region = state.loop_region.clone();
                            let tempo = state.tempo.bpm();
                            let time_sig = (
                                state.time_signature.numerator(),
                                state.time_signature.denominator(),
                            );

                            let song_transport = Self::calculate_song_transport(
                                song,
                                song_index,
                                position,
                                is_playing,
                                is_looping,
                                loop_region,
                                tempo,
                                time_sig,
                            );

                            transports.push(song_transport);
                        }
                        Err(e) => {
                            debug!(
                                "Could not get transport state for project {}: {}",
                                song.project_guid, e
                            );
                            transports.push(SongTransportState {
                                song_index,
                                ..Default::default()
                            });
                        }
                    }
                }
                Err(e) => {
                    debug!(
                        "Could not get project {} for song {}: {}",
                        song.project_guid, song.name, e
                    );
                    // Push a default state for this song
                    transports.push(SongTransportState {
                        song_index,
                        ..Default::default()
                    });
                }
            }
        }

        transports
    }

    /// Find the active song and section based on current DAW project
    async fn calculate_active_indices(&self) -> ActiveIndices {
        let daw = Daw::get();

        // Get current project (the one that's selected/focused in the DAW)
        let current_project = match daw.current_project().await {
            Ok(p) => p,
            Err(e) => {
                warn!("Failed to get current project: {}", e);
                return ActiveIndices::default();
            }
        };

        let current_guid = current_project.guid().to_string();

        // Get transport state for current project
        let transport = current_project.transport();
        let position = transport.get_position().await.unwrap_or(0.0);
        let is_playing = transport.is_playing().await.unwrap_or(false);
        let looping = transport.is_looping().await.unwrap_or(false);

        // Find which song corresponds to the current project
        let setlist = self.setlist.read().await;
        let Some(ref setlist) = *setlist else {
            return ActiveIndices {
                is_playing,
                looping,
                ..Default::default()
            };
        };

        // Find the song that matches the current project
        let song_data = setlist
            .songs
            .iter()
            .enumerate()
            .find(|(_, song)| song.project_guid == current_guid);

        if let Some((song_index, song)) = song_data {
            // Calculate song progress
            let song_duration = song.duration();
            let song_relative_pos = position - song.start_seconds();
            let song_progress = if song_duration > 0.0 {
                Some((song_relative_pos / song_duration).clamp(0.0, 1.0))
            } else {
                None
            };

            // Find section at current position
            if let Some((section_index, section)) = song.section_at_position_with_index(position) {
                // Calculate section progress
                let section_duration = section.duration();
                let section_relative_pos = position - section.start_seconds;
                let section_progress = if section_duration > 0.0 {
                    Some((section_relative_pos / section_duration).clamp(0.0, 1.0))
                } else {
                    None
                };

                ActiveIndices {
                    song_index: Some(song_index),
                    section_index: Some(section_index),
                    slide_index: None,
                    song_progress,
                    section_progress,
                    is_playing,
                    looping,
                    loop_selection: None,
                    queued_target: None,
                }
            } else {
                // In song but not in a specific section
                ActiveIndices {
                    song_index: Some(song_index),
                    section_index: None,
                    slide_index: None,
                    song_progress,
                    section_progress: None,
                    is_playing,
                    looping,
                    loop_selection: None,
                    queued_target: None,
                }
            }
        } else {
            // Current project doesn't match any song in setlist
            ActiveIndices {
                song_index: None,
                section_index: None,
                slide_index: None,
                song_progress: None,
                section_progress: None,
                is_playing,
                looping,
                loop_selection: None,
                queued_target: None,
            }
        }
    }
}

impl SetlistService for SetlistServiceImpl {
    // =========================================================================
    // Query Methods
    // =========================================================================

    async fn get_setlist(&self, _cx: &Context) -> Option<Setlist> {
        let setlist = self.setlist.read().await;
        setlist.clone()
    }

    async fn get_songs(&self, _cx: &Context) -> Vec<Song> {
        let setlist = self.setlist.read().await;
        let Some(ref setlist) = *setlist else {
            return Vec::new();
        };

        setlist.songs.clone()
    }

    async fn get_song(&self, _cx: &Context, index: usize) -> Option<Song> {
        let setlist = self.setlist.read().await;
        setlist.as_ref()?.songs.get(index).cloned()
    }

    async fn get_sections(&self, _cx: &Context, song_index: usize) -> Vec<Section> {
        let setlist = self.setlist.read().await;
        if let Some(song) = setlist.as_ref().and_then(|s| s.songs.get(song_index)) {
            song.sections.clone()
        } else {
            Vec::new()
        }
    }

    async fn get_section(
        &self,
        _cx: &Context,
        song_index: usize,
        section_index: usize,
    ) -> Option<Section> {
        let setlist = self.setlist.read().await;
        let song = setlist.as_ref()?.songs.get(song_index)?;
        song.sections.get(section_index).cloned()
    }

    async fn get_measures(&self, _cx: &Context, song_index: usize) -> Vec<MeasureInfo> {
        let setlist = self.setlist.read().await;
        if let Some(song) = setlist.as_ref().and_then(|s| s.songs.get(song_index)) {
            // If we have pre-calculated measure positions, use them
            if !song.measure_positions.is_empty() {
                let ts = song
                    .time_signature
                    .unwrap_or(daw_proto::TimeSignature::new(4, 4));
                return song
                    .measure_positions
                    .iter()
                    .enumerate()
                    .map(|(idx, pos)| MeasureInfo {
                        measure: idx as i32,
                        time_seconds: pos.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0),
                        time_sig_numerator: ts.numerator() as i32,
                        time_sig_denominator: ts.denominator() as i32,
                    })
                    .collect();
            }

            // Otherwise, calculate measures from tempo and time signature
            let ts = song
                .time_signature
                .unwrap_or(daw_proto::TimeSignature::new(4, 4));
            let tempo = song.tempo.unwrap_or(120.0);

            // Calculate measure duration in seconds
            let beats_per_measure = ts.numerator() as f64;
            let seconds_per_beat = 60.0 / tempo;
            let measure_duration = beats_per_measure * seconds_per_beat;

            // Generate measures for the song duration
            let song_duration = song.duration();
            let measure_count = (song_duration / measure_duration).ceil() as i32;

            (0..measure_count)
                .map(|idx| MeasureInfo {
                    measure: idx,
                    time_seconds: song.start_seconds + (idx as f64 * measure_duration),
                    time_sig_numerator: ts.numerator() as i32,
                    time_sig_denominator: ts.denominator() as i32,
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    async fn get_active_song(&self, _cx: &Context) -> Option<Song> {
        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        let song_index = active.song_index?;
        let setlist = self.setlist.read().await;
        setlist.as_ref()?.songs.get(song_index).cloned()
    }

    async fn get_active_section(&self, _cx: &Context) -> Option<Section> {
        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        let song_index = active.song_index?;
        let section_index = active.section_index?;
        let setlist = self.setlist.read().await;
        let song = setlist.as_ref()?.songs.get(song_index)?;
        song.sections.get(section_index).cloned()
    }

    async fn get_song_at(&self, _cx: &Context, seconds: f64) -> Option<Song> {
        let setlist = self.setlist.read().await;
        let (_, song) = setlist.as_ref()?.song_at(seconds)?;
        Some(song.clone())
    }

    async fn get_section_at(&self, _cx: &Context, seconds: f64) -> Option<Section> {
        let setlist = self.setlist.read().await;
        let (_, song) = setlist.as_ref()?.song_at(seconds)?;
        let (_, section) = song.section_at_position_with_index(seconds)?;
        Some(section.clone())
    }

    // =========================================================================
    // Navigation Commands
    // =========================================================================

    async fn go_to_song(&self, _cx: &Context, index: usize) {
        debug!("go_to_song: {}", index);

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(index).await {
            // Update the cached active song ID for fast playback commands
            self.set_active_song_id(&song.id).await;

            // First, switch to the correct project
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    let transport = project.transport();

                    // Only seek to song start if the project is NOT already playing
                    // This preserves playback position when switching between songs
                    let is_playing = transport.is_playing().await.unwrap_or(false);
                    if !is_playing {
                        if let Err(e) = transport.set_position(song.start_seconds()).await {
                            warn!("Failed to set position for song {}: {}", index, e);
                        } else {
                            info!(
                                "Navigated to song {} ({}) in project {}",
                                index, song.name, song.project_guid
                            );
                        }
                    } else {
                        info!(
                            "Song {} ({}) is already playing, preserving position",
                            index, song.name
                        );
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to switch to project {} for song {}: {}",
                        song.project_guid, index, e
                    );
                }
            }
        }
    }

    async fn next_song(&self, _cx: &Context) {
        debug!("next_song");

        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        if let Some(current_idx) = active.song_index {
            let next_idx = current_idx + 1;
            self.go_to_song(_cx, next_idx).await;
        }
    }

    async fn previous_song(&self, _cx: &Context) {
        debug!("previous_song");

        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        if let Some(current_idx) = active.song_index {
            if current_idx > 0 {
                let prev_idx = current_idx - 1;
                self.go_to_song(_cx, prev_idx).await;
            }
        }
    }

    async fn go_to_section(&self, _cx: &Context, index: usize) {
        debug!("go_to_section: {}", index);

        let daw = Daw::get();
        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;

        if let Some(song_idx) = active.song_index {
            // Queue the target immediately for visual feedback
            self.queue_target(QueuedTarget::Section {
                song_index: song_idx,
                section_index: index,
            })
            .await;

            if let Some(song) = self.get_song_internal(song_idx).await {
                if let Some(section) = song.sections.get(index) {
                    // First, switch to the correct project (in case we're on a different one)
                    match daw.select_project(&song.project_guid).await {
                        Ok(project) => {
                            // Then seek to the section's start position
                            if let Err(e) = project
                                .transport()
                                .set_position(section.start_seconds)
                                .await
                            {
                                warn!("Failed to navigate to section {}: {}", index, e);
                                // Clear queue on failure
                                self.clear_queued_target().await;
                            } else {
                                info!(
                                    "Navigated to section {} ({}) in song {} (project {})",
                                    index, section.name, song.name, song.project_guid
                                );
                            }
                        }
                        Err(e) => {
                            warn!(
                                "Failed to switch to project {} for section navigation: {}",
                                song.project_guid, e
                            );
                        }
                    }
                }
            }
        }
    }

    async fn next_section(&self, _cx: &Context) {
        debug!("next_section");

        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        if let Some(section_idx) = active.section_index {
            let next_idx = section_idx + 1;
            self.go_to_section(_cx, next_idx).await;
        }
    }

    async fn previous_section(&self, _cx: &Context) {
        debug!("previous_section");

        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;
        if let Some(section_idx) = active.section_index {
            // Smart previous: if we're past the beginning of the section (>5% progress),
            // go to the start of the current section. Only go to previous section
            // if we're already at/near the beginning.
            let at_section_start = active
                .section_progress
                .map(|p| p < 0.05) // Within first 5% of section
                .unwrap_or(true);

            if at_section_start && section_idx > 0 {
                // Already at the start, go to previous section
                let prev_idx = section_idx - 1;
                self.go_to_section(_cx, prev_idx).await;
            } else {
                // Not at start, go to beginning of current section
                self.go_to_section(_cx, section_idx).await;
            }
        }
    }

    async fn seek_to(&self, _cx: &Context, seconds: f64) {
        debug!("seek_to: {}", seconds);

        let daw = Daw::get();
        // Use cached indices for instant response (updated at 60Hz by polling loop)
        let active = self.get_cached_indices().await;

        if let Some(song_idx) = active.song_index {
            if let Some(song) = self.get_song_internal(song_idx).await {
                let absolute_pos = song.start_seconds() + seconds;
                match daw.project(&song.project_guid).await {
                    Ok(project) => {
                        if let Err(e) = project.transport().set_position(absolute_pos).await {
                            warn!("Failed to seek to {}: {}", seconds, e);
                        }
                    }
                    Err(e) => {
                        warn!("Failed to get project: {}", e);
                    }
                }
            }
        }
    }

    async fn seek_to_time(&self, _cx: &Context, song_index: usize, seconds: f64) {
        info!(
            "seek_to_time: song_index={}, seconds={}",
            song_index, seconds
        );

        // Queue the target immediately for visual feedback (comment marker)
        self.queue_target(QueuedTarget::Comment {
            song_index,
            position_seconds: seconds,
        })
        .await;

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(song_index).await {
            // First, switch to the correct project
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    // Seek to the absolute time position
                    if let Err(e) = project.transport().set_position(seconds).await {
                        warn!(
                            "Failed to seek to {} seconds in song {}: {}",
                            seconds, song_index, e
                        );
                        // Clear queue on failure
                        self.clear_queued_target().await;
                    } else {
                        info!(
                            "Seeked to {} seconds in song {} ({})",
                            seconds, song_index, song.name
                        );
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to switch to project {} for song {}: {}",
                        song.project_guid, song_index, e
                    );
                }
            }
        } else {
            warn!("Song {} not found", song_index);
        }
    }

    async fn seek_to_song(&self, _cx: &Context, song_index: usize) {
        info!("seek_to_song called: song_index={}", song_index);

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(song_index).await {
            // Update the cached active song ID for fast playback commands
            self.set_active_song_id(&song.id).await;

            info!(
                "seek_to_song: found song '{}' with project_guid={}, start_seconds={}",
                song.name,
                song.project_guid,
                song.start_seconds()
            );

            // First, switch to the correct project
            info!("seek_to_song: switching to project {}", song.project_guid);
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    let transport = project.transport();

                    // Only seek to song start if the project is NOT already playing
                    // This preserves playback position when switching between songs
                    let is_playing = transport.is_playing().await.unwrap_or(false);
                    if is_playing {
                        info!(
                            "seek_to_song: project {} is already playing, preserving position",
                            song.project_guid
                        );
                    } else {
                        info!(
                            "seek_to_song: project not playing, seeking to position {}",
                            song.start_seconds()
                        );
                        if let Err(e) = transport.set_position(song.start_seconds()).await {
                            warn!("Failed to seek to song {}: {}", song_index, e);
                        } else {
                            info!(
                                "Seeked to song {} ({}) in project {}",
                                song_index, song.name, song.project_guid
                            );
                        }
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to switch to project {} for song {}: {}",
                        song.project_guid, song_index, e
                    );
                }
            }
        } else {
            warn!("Song {} not found in setlist", song_index);
        }
    }

    async fn seek_to_section(&self, _cx: &Context, song_index: usize, section_index: usize) {
        debug!(
            "seek_to_section: song={}, section={}",
            song_index, section_index
        );

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(song_index).await {
            if let Some(section) = song.sections.get(section_index) {
                // First, switch to the correct project
                match daw.select_project(&song.project_guid).await {
                    Ok(project) => {
                        // Then seek to the section's start position
                        if let Err(e) = project
                            .transport()
                            .set_position(section.start_seconds)
                            .await
                        {
                            warn!(
                                "Failed to seek to section {} in song {}: {}",
                                section_index, song_index, e
                            );
                        } else {
                            info!(
                                "Seeked to section {} ({}) in song {} (project {})",
                                section_index, section.name, song.name, song.project_guid
                            );
                        }
                    }
                    Err(e) => {
                        warn!(
                            "Failed to switch to project {} for song {}: {}",
                            song.project_guid, song_index, e
                        );
                    }
                }
            } else {
                warn!("Section {} not found in song {}", section_index, song_index);
            }
        } else {
            warn!("Song {} not found", song_index);
        }
    }

    async fn seek_to_musical_position(
        &self,
        _cx: &Context,
        song_index: usize,
        position: daw_proto::MusicalPosition,
    ) {
        debug!(
            "seek_to_musical_position: song={}, position={}.{}.{}",
            song_index, position.measure, position.beat, position.subdivision
        );

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(song_index).await {
            // First, switch to the correct project
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    // The musical position is relative to the song start
                    // We need to convert it to absolute position using the tempo map
                    let fraction = position.subdivision as f64 / 1000.0;
                    let relative_seconds = project
                        .tempo_map()
                        .musical_to_time(position.measure, position.beat, fraction)
                        .await
                        .unwrap_or(0.0);

                    // Add the song start offset to get absolute position
                    let absolute_pos = song.start_seconds() + relative_seconds;

                    if let Err(e) = project.transport().set_position(absolute_pos).await {
                        warn!(
                            "Failed to seek to musical position in song {}: {}",
                            song_index, e
                        );
                    } else {
                        info!(
                            "Seeked to {}.{}.{} in song {} (project {})",
                            position.measure,
                            position.beat,
                            position.subdivision,
                            song.name,
                            song.project_guid
                        );
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to switch to project {} for song {}: {}",
                        song.project_guid, song_index, e
                    );
                }
            }
        } else {
            warn!("Song {} not found", song_index);
        }
    }

    async fn goto_measure(&self, _cx: &Context, song_index: usize, measure: i32) {
        info!(
            "goto_measure: song_index={}, measure={}",
            song_index, measure
        );

        let daw = Daw::get();

        if let Some(song) = self.get_song_internal(song_index).await {
            // First, switch to the correct project
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    // Use the transport's goto_measure which handles tempo map conversion
                    if let Err(e) = project.transport().goto_measure(measure).await {
                        warn!(
                            "Failed to goto measure {} in song {}: {}",
                            measure, song_index, e
                        );
                    } else {
                        info!(
                            "Went to measure {} in song {} ({})",
                            measure, song.name, song.project_guid
                        );
                    }
                }
                Err(e) => {
                    warn!(
                        "Failed to switch to project {} for song {}: {}",
                        song.project_guid, song_index, e
                    );
                }
            }
        } else {
            warn!("Song {} not found", song_index);
        }
    }

    // =========================================================================
    // Playback Commands
    // =========================================================================

    async fn toggle_playback(&self, _cx: &Context) {
        debug!("toggle_playback");

        // Use cached active song ID for instant lookup (no RPC calls)
        if let Some(song) = self.get_cached_active_song().await {
            let daw = Daw::get();
            match daw.project(&song.project_guid).await {
                Ok(project) => {
                    if let Err(e) = project.transport().play_pause().await {
                        warn!("Failed to toggle playback: {}", e);
                    }
                }
                Err(e) => {
                    warn!("Failed to get project {}: {}", song.project_guid, e);
                }
            }
        } else {
            warn!("No active song to toggle playback (navigate to a song first)");
        }
    }

    async fn play(&self, _cx: &Context) {
        debug!("play");

        // Use cached active song ID for instant lookup (no RPC calls)
        if let Some(song) = self.get_cached_active_song().await {
            let daw = Daw::get();
            match daw.project(&song.project_guid).await {
                Ok(project) => {
                    if let Err(e) = project.transport().play().await {
                        warn!("Failed to play: {}", e);
                    }
                }
                Err(e) => {
                    warn!("Failed to get project {}: {}", song.project_guid, e);
                }
            }
        } else {
            warn!("No active song to play (navigate to a song first)");
        }
    }

    async fn pause(&self, _cx: &Context) {
        debug!("pause");

        // Use cached active song ID for instant lookup (no RPC calls)
        if let Some(song) = self.get_cached_active_song().await {
            let daw = Daw::get();
            match daw.project(&song.project_guid).await {
                Ok(project) => {
                    if let Err(e) = project.transport().pause().await {
                        warn!("Failed to pause: {}", e);
                    }
                }
                Err(e) => {
                    warn!("Failed to get project {}: {}", song.project_guid, e);
                }
            }
        } else {
            warn!("No active song to pause (navigate to a song first)");
        }
    }

    async fn stop(&self, _cx: &Context) {
        debug!("stop");

        // Use cached active song ID for instant lookup (no RPC calls)
        if let Some(song) = self.get_cached_active_song().await {
            let daw = Daw::get();
            match daw.project(&song.project_guid).await {
                Ok(project) => {
                    if let Err(e) = project.transport().stop().await {
                        warn!("Failed to stop: {}", e);
                    }
                }
                Err(e) => {
                    warn!("Failed to get project {}: {}", song.project_guid, e);
                }
            }
        } else {
            warn!("No active song to stop (navigate to a song first)");
        }
    }

    // =========================================================================
    // Loop Control
    // =========================================================================

    async fn toggle_song_loop(&self, _cx: &Context) {
        debug!("toggle_song_loop");

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().toggle_loop().await {
                    warn!("Failed to toggle song loop: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
        }
    }

    async fn toggle_section_loop(&self, _cx: &Context) {
        debug!("toggle_section_loop");
        // TODO: Implement section-specific loop using loop points
        warn!("toggle_section_loop not yet implemented");
    }

    async fn set_loop_region(&self, _cx: &Context, _start_seconds: f64, _end_seconds: f64) {
        debug!("set_loop_region: {} - {}", _start_seconds, _end_seconds);
        // TODO: Implement setting loop region
        warn!("set_loop_region not yet implemented");
    }

    async fn clear_loop(&self, _cx: &Context) {
        debug!("clear_loop");

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().set_loop(false).await {
                    warn!("Failed to clear loop: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
        }
    }

    // =========================================================================
    // Build/Refresh
    // =========================================================================

    async fn build_from_open_projects(&self, _cx: &Context) {
        info!("Building setlist from open projects...");

        // Check if DAW is initialized (it may not be ready yet after cell startup)
        let Some(daw) = Daw::try_get() else {
            warn!("DAW not initialized yet, cannot build setlist");
            return;
        };

        match SetlistBuilder::build_from_open_projects(daw).await {
            Ok(setlist) => {
                info!(
                    "Successfully built setlist with {} songs",
                    setlist.songs.len()
                );

                // Initialize active song to first song if available
                if let Some(first_song) = setlist.songs.first() {
                    *self.active_song_id.write().await = Some(first_song.id.clone());
                    info!(
                        "Set initial active song to: {} ({})",
                        first_song.name, first_song.id
                    );
                }

                *self.setlist.write().await = Some(setlist);
            }
            Err(e) => {
                warn!("Failed to build setlist: {}", e);
            }
        }
    }

    async fn refresh(&self, _cx: &Context) {
        info!("Refreshing setlist...");
        self.build_from_open_projects(_cx).await;
    }

    // =========================================================================
    // Subscriptions
    // =========================================================================

    async fn subscribe(&self, _cx: &Context, events: Tx<SetlistEvent>) {
        info!("SetlistService::subscribe() - starting fully reactive event stream");

        // Clone self for the spawned task
        let this = self.clone();

        // Spawn the streaming loop so this method returns immediately
        tokio::spawn(async move {
            // Get songs for GUID -> index mapping
            let songs: Vec<Song>;
            {
                let setlist = this.setlist.read().await;
                if let Some(ref sl) = *setlist {
                    songs = sl.songs.clone();
                    // Send initial setlist state
                    if events
                        .send(&SetlistEvent::SetlistChanged(sl.clone()))
                        .await
                        .is_err()
                    {
                        debug!(
                            "SetlistService::subscribe() - client disconnected during initial send"
                        );
                        return;
                    }
                } else {
                    info!("SetlistService::subscribe() - no setlist available");
                    return;
                }
            }

            // Build project GUID -> song index mapping
            let guid_to_index: std::collections::HashMap<String, usize> = songs
                .iter()
                .enumerate()
                .map(|(idx, song)| (song.project_guid.clone(), idx))
                .collect();

            // Subscribe to the reactive per-project transport stream
            let daw = Daw::get();
            let transport_rx = match daw.current_project().await {
                Ok(project) => match project.transport().subscribe_all_projects().await {
                    Ok(rx) => rx,
                    Err(e) => {
                        warn!("Failed to subscribe to all projects transport: {}", e);
                        return;
                    }
                },
                Err(e) => {
                    warn!(
                        "Failed to get current project for transport subscription: {}",
                        e
                    );
                    return;
                }
            };

            // Get initial active indices from REAPER's current project (makes RPC calls)
            // This ensures the UI shows the correct song/section on startup
            let mut last_indices = this.calculate_active_indices().await;

            // Update cached indices so navigation works correctly
            this.set_cached_indices(last_indices.clone()).await;

            // Also update active_song_id if we found a song
            if let Some(song_idx) = last_indices.song_index {
                if let Some(song) = songs.get(song_idx) {
                    *this.active_song_id.write().await = Some(song.id.clone());
                }
            }
            if events
                .send(&SetlistEvent::ActiveIndicesChanged(last_indices.clone()))
                .await
                .is_err()
            {
                debug!("SetlistService::subscribe() - client disconnected");
                return;
            }

            // Send initial transport state for all songs (one-time poll)
            let initial_transports = this.get_all_song_transports().await;
            if events
                .send(&SetlistEvent::TransportUpdate(initial_transports))
                .await
                .is_err()
            {
                debug!("SetlistService::subscribe() - client disconnected");
                return;
            }

            // Track current song/section for enter/exit events
            // Keyed by song_index to track section per song
            let mut last_section_by_song: std::collections::HashMap<usize, Option<usize>> =
                std::collections::HashMap::new();

            // Track last known current project GUID for detecting tab switches
            let mut last_current_project_guid: Option<String> = {
                let daw = Daw::get();
                daw.current_project()
                    .await
                    .ok()
                    .map(|p| p.guid().to_string())
            };

            // Timer for checking project tab switches (every 500ms)
            // This is separate from transport updates which come at ~30Hz
            let mut project_check_interval = tokio::time::interval(Duration::from_millis(500));
            project_check_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

            // Fully reactive loop - processes transport stream updates and periodic project checks
            let mut transport_rx = transport_rx;

            loop {
                tokio::select! {
                    // Handle transport updates from the broadcast channel
                    result = transport_rx.recv() => {
                        match result {
                    Ok(Some(update)) => {
                        // Convert project GUIDs to song indices and build SongTransportState
                        let mut song_transports: Vec<SongTransportState> = Vec::new();

                        for proj_state in update.projects {
                            if let Some(&song_index) = guid_to_index.get(&proj_state.project_guid) {
                                let song = &songs[song_index];
                                let is_playing = proj_state.transport.play_state
                                    == daw_proto::PlayState::Playing
                                    || proj_state.transport.play_state
                                        == daw_proto::PlayState::Recording;

                                // Use playhead position when playing, edit cursor when stopped
                                // The Position includes both time and musical position from REAPER's tempo map
                                let position = if is_playing {
                                    proj_state.transport.playhead_position.clone()
                                } else {
                                    proj_state.transport.edit_position.clone()
                                };

                                let song_transport = Self::calculate_song_transport(
                                    song,
                                    song_index,
                                    position,
                                    is_playing,
                                    proj_state.transport.looping,
                                    proj_state.transport.loop_region.clone(),
                                    proj_state.transport.tempo.bpm(),
                                    (
                                        proj_state.transport.time_signature.numerator(),
                                        proj_state.transport.time_signature.denominator(),
                                    ),
                                );

                                // Track section changes for this song (derived from transport data)
                                let current_section = song_transport.section_index;
                                let last_section =
                                    last_section_by_song.get(&song_index).copied().flatten();

                                if current_section != last_section {
                                    // Section changed for this song
                                    if let Some(sec_idx) = last_section {
                                        let _ = events
                                            .send(&SetlistEvent::SectionExited {
                                                song_index,
                                                section_index: sec_idx,
                                            })
                                            .await;
                                    }

                                    if let Some(sec_idx) = current_section {
                                        if let Some(section) = song.sections.get(sec_idx) {
                                            let _ = events
                                                .send(&SetlistEvent::SectionEntered {
                                                    song_index,
                                                    section_index: sec_idx,
                                                    section: section.clone(),
                                                })
                                                .await;
                                        }
                                    }

                                    last_section_by_song.insert(song_index, current_section);
                                }

                                song_transports.push(song_transport);
                            }
                        }

                        if !song_transports.is_empty() {
                            // Update ActiveIndices from the first playing song (or first song with updates)
                            // Find the currently active song based on cached ID
                            let active_song_index = {
                                let song_id = this.active_song_id.read().await.clone();
                                song_id.and_then(|id| songs.iter().position(|s| s.id == id))
                            };

                            // Find transport for active song, or use first available
                            let active_transport = active_song_index
                                .and_then(|idx| {
                                    song_transports.iter().find(|t| t.song_index == idx)
                                })
                                .or_else(|| song_transports.first());

                            if let Some(transport) = active_transport {
                                // Get current position in seconds for queue checking
                                let position_seconds = transport
                                    .position
                                    .time
                                    .map(|t| t.as_seconds())
                                    .unwrap_or(0.0);

                                // Check if we've reached the queued target
                                this.check_and_clear_queue(
                                    transport.song_index,
                                    transport.section_index,
                                    position_seconds,
                                )
                                .await;

                                // Get current queued target to include in indices
                                let queued_target = this.get_queued_target().await;

                                let current_indices = ActiveIndices {
                                    song_index: Some(transport.song_index),
                                    section_index: transport.section_index,
                                    slide_index: None,
                                    song_progress: Some(transport.progress),
                                    section_progress: transport.section_progress,
                                    is_playing: transport.is_playing,
                                    looping: transport.is_looping,
                                    loop_selection: None,
                                    queued_target,
                                };

                                // Cache indices for instant navigation (no RPC needed)
                                this.set_cached_indices(current_indices.clone()).await;

                                // Only send if changed
                                if current_indices != last_indices {
                                    if events
                                        .send(&SetlistEvent::ActiveIndicesChanged(
                                            current_indices.clone(),
                                        ))
                                        .await
                                        .is_err()
                                    {
                                        break;
                                    }
                                    last_indices = current_indices;
                                }
                            }

                            // Send transport updates
                            if events
                                .send(&SetlistEvent::TransportUpdate(song_transports))
                                .await
                                .is_err()
                            {
                                break;
                            }
                        }
                    }
                    Ok(None) => {
                        // Stream ended
                        info!("SetlistService::subscribe() - transport stream ended");
                        break;
                    }
                    Err(e) => {
                        warn!(
                            "SetlistService::subscribe() - transport stream error: {}",
                            e
                        );
                        break;
                    }
                        }
                    }

                    // Periodically check if the current REAPER project tab has changed
                    _ = project_check_interval.tick() => {
                        let daw = Daw::get();
                        if let Ok(current_project) = daw.current_project().await {
                            let current_guid = current_project.guid().to_string();

                            // Check if project tab changed
                            if last_current_project_guid.as_ref() != Some(&current_guid) {
                                debug!("Project tab changed from {:?} to {}", last_current_project_guid, current_guid);

                                // Check if previously active project was paused - if so, stop it
                                if let Some(prev_guid) = &last_current_project_guid {
                                    if let Ok(prev_project) = daw.project(prev_guid).await {
                                        let transport = prev_project.transport();
                                        if let Ok(state) = transport.get_play_state().await {
                                            if state == daw_proto::PlayState::Paused {
                                                // Stop the paused project
                                                debug!("Stopping paused project {}", prev_guid);
                                                let _ = transport.stop().await;
                                            }
                                        }
                                    }
                                }

                                // Find the song that matches the new current project
                                if let Some(&song_idx) = guid_to_index.get(&current_guid) {
                                    // Update active song ID
                                    if let Some(song) = songs.get(song_idx) {
                                        *this.active_song_id.write().await = Some(song.id.clone());

                                        // Calculate and send new active indices
                                        let new_indices = this.calculate_active_indices().await;
                                        this.set_cached_indices(new_indices.clone()).await;

                                        if new_indices != last_indices {
                                            if events
                                                .send(&SetlistEvent::ActiveIndicesChanged(new_indices.clone()))
                                                .await
                                                .is_err()
                                            {
                                                break;
                                            }
                                            last_indices = new_indices;
                                        }
                                    }
                                }

                                last_current_project_guid = Some(current_guid);
                            }
                        }
                    }
                }
            }

            info!("SetlistService::subscribe() - stream ended");
        });
    }

    async fn subscribe_active(&self, _cx: &Context, indices: Tx<ActiveIndices>) {
        info!("SetlistService::subscribe_active() - starting active indices stream");

        // Clone self for the spawned task
        let this = self.clone();

        // Spawn the streaming loop so this method returns immediately
        tokio::spawn(async move {
            // Send initial state
            let mut last_indices = this.calculate_active_indices().await;
            if indices.send(&last_indices).await.is_err() {
                debug!(
                    "SetlistService::subscribe_active() - client disconnected during initial send"
                );
                return;
            }

            // Poll for changes at 60Hz (smooth updates during playback)
            loop {
                tokio::time::sleep(Duration::from_micros(16667)).await;

                let current_indices = this.calculate_active_indices().await;

                // Only send if something changed
                if current_indices != last_indices {
                    if indices.send(&current_indices).await.is_err() {
                        debug!("SetlistService::subscribe_active() - client disconnected");
                        break;
                    }
                    last_indices = current_indices;
                }
            }

            info!("SetlistService::subscribe_active() - stream ended");
        });
    }

    async fn get_audio_latency(&self, _cx: &Context) -> f64 {
        let daw = Daw::get();
        daw.audio_engine()
            .get_output_latency_seconds()
            .await
            .unwrap_or(0.0)
    }

    async fn get_audio_latency_info(&self, _cx: &Context) -> session_proto::AudioLatencyInfo {
        let daw = Daw::get();
        match daw.audio_engine().get_state().await {
            Ok(state) => session_proto::AudioLatencyInfo {
                input_samples: state.latency.input_samples,
                output_samples: state.latency.output_samples,
                output_seconds: state.latency.output_seconds,
                sample_rate: state.latency.sample_rate,
                is_running: state.is_running,
            },
            Err(_) => session_proto::AudioLatencyInfo::default(),
        }
    }
}
