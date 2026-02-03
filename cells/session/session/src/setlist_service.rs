//! SetlistService implementation

use crate::setlist_builder::SetlistBuilder;
use daw_control::Daw;
use roam::Tx;
use roam::session::Context;
use session_proto::{
    ActiveIndices, MeasureInfo, Section, Setlist, SetlistEvent, SetlistService, Song,
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
}

impl SetlistServiceImpl {
    pub fn new() -> Self {
        Self {
            setlist: Arc::new(RwLock::new(None)),
        }
    }

    /// Get a specific song by index (internal helper)
    async fn get_song_internal(&self, index: usize) -> Option<session_proto::Song> {
        let setlist = self.setlist.read().await;
        setlist.as_ref()?.songs.get(index).cloned()
    }

    /// Find the song index for a given project GUID
    async fn find_song_index_for_project(&self, project_guid: &str) -> Option<usize> {
        let setlist = self.setlist.read().await;
        let setlist = setlist.as_ref()?;
        setlist
            .songs
            .iter()
            .position(|song| song.project_guid == project_guid)
    }

    /// Calculate transport state for a specific song based on its project's transport
    fn calculate_song_transport(
        song: &Song,
        song_index: usize,
        position: f64,
        is_playing: bool,
        is_looping: bool,
        tempo: f64,
        time_sig: (u32, u32),
    ) -> SongTransportState {
        let song_duration = song.duration();
        let song_start = song.start_seconds();

        // Calculate progress within song
        let relative_pos = position - song_start;
        let progress = if song_duration > 0.0 {
            (relative_pos / song_duration).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Find section at position
        let (section_index, section_progress) =
            if let Some((sec_idx, section)) = song.section_at_position_with_index(position) {
                let sec_duration = section.duration();
                let sec_progress = if sec_duration > 0.0 {
                    ((position - section.start_seconds) / sec_duration).clamp(0.0, 1.0)
                } else {
                    0.0
                };
                (Some(sec_idx), Some(sec_progress))
            } else {
                (None, None)
            };

        SongTransportState {
            song_index,
            position,
            progress,
            section_index,
            section_progress,
            is_playing,
            is_looping,
            bpm: tempo,
            time_sig_num: time_sig.0,
            time_sig_denom: time_sig.1,
        }
    }

    /// Get transport state for ALL songs by querying each project
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

                    // Get transport state
                    let position = transport.get_position().await.unwrap_or(0.0);
                    let is_playing = transport.is_playing().await.unwrap_or(false);
                    let is_looping = transport.is_looping().await.unwrap_or(false);
                    let tempo = transport.get_tempo().await.unwrap_or(120.0);
                    let time_sig = transport
                        .get_time_signature()
                        .await
                        .map(|ts| (ts.numerator(), ts.denominator()))
                        .unwrap_or((4, 4));

                    let song_transport = Self::calculate_song_transport(
                        song, song_index, position, is_playing, is_looping, tempo, time_sig,
                    );

                    transports.push(song_transport);
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
        let active = self.calculate_active_indices().await;
        let song_index = active.song_index?;
        let setlist = self.setlist.read().await;
        setlist.as_ref()?.songs.get(song_index).cloned()
    }

    async fn get_active_section(&self, _cx: &Context) -> Option<Section> {
        let active = self.calculate_active_indices().await;
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
            // First, switch to the correct project
            match daw.select_project(&song.project_guid).await {
                Ok(project) => {
                    // Then seek to the song's start position
                    if let Err(e) = project.transport().set_position(song.start_seconds()).await {
                        warn!("Failed to set position for song {}: {}", index, e);
                    } else {
                        info!(
                            "Navigated to song {} ({}) in project {}",
                            index, song.name, song.project_guid
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

        let active = self.calculate_active_indices().await;
        if let Some(current_idx) = active.song_index {
            let next_idx = current_idx + 1;
            self.go_to_song(_cx, next_idx).await;
        }
    }

    async fn previous_song(&self, _cx: &Context) {
        debug!("previous_song");

        let active = self.calculate_active_indices().await;
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
        let active = self.calculate_active_indices().await;

        if let Some(song_idx) = active.song_index {
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

        let active = self.calculate_active_indices().await;
        if let Some(section_idx) = active.section_index {
            let next_idx = section_idx + 1;
            self.go_to_section(_cx, next_idx).await;
        }
    }

    async fn previous_section(&self, _cx: &Context) {
        debug!("previous_section");

        let active = self.calculate_active_indices().await;
        if let Some(section_idx) = active.section_index {
            if section_idx > 0 {
                let prev_idx = section_idx - 1;
                self.go_to_section(_cx, prev_idx).await;
            }
        }
    }

    async fn seek_to(&self, _cx: &Context, seconds: f64) {
        debug!("seek_to: {}", seconds);

        let daw = Daw::get();
        let active = self.calculate_active_indices().await;

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
                    info!(
                        "seek_to_song: switched to project, now seeking to position {}",
                        song.start_seconds()
                    );
                    // Then seek to the song's start position within that project
                    if let Err(e) = project.transport().set_position(song.start_seconds()).await {
                        warn!("Failed to seek to song {}: {}", song_index, e);
                    } else {
                        info!(
                            "Seeked to song {} ({}) in project {}",
                            song_index, song.name, song.project_guid
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

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().play_pause().await {
                    warn!("Failed to toggle playback: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
        }
    }

    async fn play(&self, _cx: &Context) {
        debug!("play");

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().play().await {
                    warn!("Failed to play: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
        }
    }

    async fn pause(&self, _cx: &Context) {
        debug!("pause");

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().pause().await {
                    warn!("Failed to pause: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
        }
    }

    async fn stop(&self, _cx: &Context) {
        debug!("stop");

        let daw = Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                if let Err(e) = project.transport().stop().await {
                    warn!("Failed to stop: {}", e);
                }
            }
            Err(e) => {
                warn!("Failed to get current project: {}", e);
            }
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
        info!("SetlistService::subscribe() - starting event stream");

        // Clone self for the spawned task
        let this = self.clone();

        // Spawn the streaming loop so this method returns immediately
        tokio::spawn(async move {
            // Send initial setlist state
            {
                let setlist = this.setlist.read().await;
                if let Some(ref sl) = *setlist {
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
                }
            }

            // Send initial active indices
            let mut last_indices = this.calculate_active_indices().await;
            if events
                .send(&SetlistEvent::ActiveIndicesChanged(last_indices.clone()))
                .await
                .is_err()
            {
                debug!("SetlistService::subscribe() - client disconnected");
                return;
            }

            // Send initial transport state for all songs
            let mut last_transports = this.get_all_song_transports().await;
            if events
                .send(&SetlistEvent::TransportUpdate(last_transports.clone()))
                .await
                .is_err()
            {
                debug!("SetlistService::subscribe() - client disconnected");
                return;
            }

            // Track current song/section for enter/exit events
            let mut last_song_index = last_indices.song_index;
            let mut last_section_index = last_indices.section_index;

            // Poll for changes at 60Hz (same rate as transport)
            loop {
                tokio::time::sleep(Duration::from_micros(16667)).await;

                // Get current state
                let current_indices = this.calculate_active_indices().await;
                let current_transports = this.get_all_song_transports().await;

                // Always send transport update (positions change constantly during playback)
                if current_transports != last_transports {
                    if events
                        .send(&SetlistEvent::TransportUpdate(current_transports.clone()))
                        .await
                        .is_err()
                    {
                        break;
                    }
                    last_transports = current_transports;
                }

                // Check for song change (active song switched)
                if current_indices.song_index != last_song_index {
                    // Song exited
                    if let Some(idx) = last_song_index {
                        if events
                            .send(&SetlistEvent::SongExited { index: idx })
                            .await
                            .is_err()
                        {
                            break;
                        }
                    }

                    // Song entered
                    if let Some(idx) = current_indices.song_index {
                        let setlist = this.setlist.read().await;
                        if let Some(ref sl) = *setlist {
                            if let Some(song) = sl.songs.get(idx) {
                                if events
                                    .send(&SetlistEvent::SongEntered {
                                        index: idx,
                                        song: song.clone(),
                                    })
                                    .await
                                    .is_err()
                                {
                                    break;
                                }
                            }
                        }
                    }

                    // Also send ActiveIndicesChanged when song changes
                    if events
                        .send(&SetlistEvent::ActiveIndicesChanged(current_indices.clone()))
                        .await
                        .is_err()
                    {
                        break;
                    }

                    last_song_index = current_indices.song_index;
                    // Reset section tracking when song changes
                    last_section_index = None;
                }

                // Check for section change
                if current_indices.section_index != last_section_index {
                    // Section exited
                    if let (Some(song_idx), Some(sec_idx)) =
                        (current_indices.song_index, last_section_index)
                    {
                        if events
                            .send(&SetlistEvent::SectionExited {
                                song_index: song_idx,
                                section_index: sec_idx,
                            })
                            .await
                            .is_err()
                        {
                            break;
                        }
                    }

                    // Section entered
                    if let (Some(song_idx), Some(sec_idx)) =
                        (current_indices.song_index, current_indices.section_index)
                    {
                        let setlist = this.setlist.read().await;
                        if let Some(ref sl) = *setlist {
                            if let Some(song) = sl.songs.get(song_idx) {
                                if let Some(section) = song.sections.get(sec_idx) {
                                    if events
                                        .send(&SetlistEvent::SectionEntered {
                                            song_index: song_idx,
                                            section_index: sec_idx,
                                            section: section.clone(),
                                        })
                                        .await
                                        .is_err()
                                    {
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    last_section_index = current_indices.section_index;
                }

                // Update last_indices for next iteration
                last_indices = current_indices;
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
}
