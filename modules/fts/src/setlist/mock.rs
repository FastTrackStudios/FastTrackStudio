//! MockSetlist - In-memory setlist service for testing and web demos
//!
//! This implementation provides a fully functional setlist service that doesn't
//! require REAPER. Useful for:
//! - Unit testing business logic
//! - Integration testing without DAW
//! - Web demos and previews
//! - Development/prototyping
//!
//! # Subscription Support
//!
//! This mock supports proper long-running subscriptions using broadcast channels.
//! When `subscribe()` or `subscribe_active()` is called, a background task spawns
//! that forwards events to the caller's `Tx`. Events are broadcast when:
//! - Commands are executed (state changes)
//! - Playback advances (internal clock tick)
//!
//! # Playback Simulation
//!
//! The mock includes an internal clock that simulates playback:
//! - Call `start_playback()` to begin advancing time automatically
//! - Call `stop_playback()` to pause
//! - Position automatically maps to the correct song/section
//! - Progress bars update in real-time

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::time::Duration;

use roam::{Context, Tx};

use super::core::Setlist;
use super::service::{
    ActiveIndices, MeasureInfo, SectionInfo, SetlistCommand, SetlistEvent, SetlistInfo,
    SetlistService, SongInfo,
};

// Platform-agnostic broadcast channel
#[cfg(not(target_arch = "wasm32"))]
use tokio::sync::broadcast;

#[cfg(target_arch = "wasm32")]
mod wasm_broadcast {
    //! Simple broadcast channel implementation for WASM using async-channel
    use std::sync::{Arc, Mutex};

    pub struct Sender<T: Clone> {
        subscribers: Arc<Mutex<Vec<async_channel::Sender<T>>>>,
    }

    impl<T: Clone> Clone for Sender<T> {
        fn clone(&self) -> Self {
            Self {
                subscribers: Arc::clone(&self.subscribers),
            }
        }
    }

    pub struct Receiver<T> {
        inner: async_channel::Receiver<T>,
    }

    impl<T: Clone> Sender<T> {
        pub fn send(&self, value: T) -> Result<usize, ()> {
            let Some(mut subs) = self.subscribers.lock().ok() else {
                // Mutex is poisoned or contended, skip this send
                return Ok(0);
            };
            // Remove closed channels and send to remaining
            subs.retain(|tx| !tx.is_closed());
            let count = subs.len();
            for tx in subs.iter() {
                let _ = tx.try_send(value.clone());
            }
            Ok(count)
        }

        pub fn subscribe(&self) -> Receiver<T> {
            let (tx, rx) = async_channel::bounded(64);
            if let Some(mut subs) = self.subscribers.lock().ok() {
                subs.push(tx);
            }
            Receiver { inner: rx }
        }
    }

    impl<T> Receiver<T> {
        pub async fn recv(&mut self) -> Result<T, RecvError> {
            self.inner.recv().await.map_err(|_| RecvError)
        }
    }

    pub struct RecvError;

    pub fn channel<T: Clone>(_capacity: usize) -> (Sender<T>, Receiver<T>) {
        let sender = Sender {
            subscribers: Arc::new(Mutex::new(Vec::new())),
        };
        let receiver = sender.subscribe();
        (sender, receiver)
    }
}

#[cfg(target_arch = "wasm32")]
use wasm_broadcast as broadcast;

// region:    --- MockSetlist

/// Tick interval for internal position updates (60 fps = ~16ms)
const TICK_INTERVAL_MS: u64 = 16;
/// Playback speed multiplier (1.0 = real-time)
const PLAYBACK_SPEED: f64 = 1.0;

/// Mock implementation of SetlistService for testing and web demos.
///
/// Uses the same Setlist struct as all other implementations.
/// Maintains state in memory and supports proper long-running subscriptions
/// via broadcast channels. Events are emitted when:
/// - Commands are executed (state changes)
/// - Playback clock advances
pub struct MockSetlist {
    /// The setlist state (shared domain type)
    setlist: Arc<RwLock<Option<Setlist>>>,
    /// Active song index
    active_song: Arc<RwLock<Option<usize>>>,
    /// Active section index within song
    active_section: Arc<RwLock<Option<usize>>>,
    /// Active slide/lyric index
    active_slide: Arc<RwLock<Option<usize>>>,
    /// Current playback position in seconds
    position: Arc<RwLock<f64>>,
    /// Whether playback is currently active
    is_playing: Arc<AtomicBool>,
    /// Whether looping is enabled
    looping: Arc<AtomicBool>,
    /// Loop region (start and end in absolute seconds)
    loop_selection: Arc<RwLock<Option<daw::primitives::TimeSelection>>>,
    /// Whether the playback loop has been started
    playback_started: Arc<AtomicBool>,
    /// Broadcast channel for SetlistEvent
    event_tx: broadcast::Sender<SetlistEvent>,
    /// Broadcast channel for ActiveIndices updates
    indices_tx: broadcast::Sender<ActiveIndices>,
}

impl MockSetlist {
    /// Create a new empty MockSetlist
    pub fn new() -> Self {
        tracing::debug!("MockSetlist::new() - creating empty mock");
        let (event_tx, _event_rx) = broadcast::channel(64);
        let (indices_tx, _indices_rx) = broadcast::channel(64);
        Self {
            setlist: Arc::new(RwLock::new(None)),
            active_song: Arc::new(RwLock::new(None)),
            active_section: Arc::new(RwLock::new(None)),
            active_slide: Arc::new(RwLock::new(None)),
            position: Arc::new(RwLock::new(0.0)),
            is_playing: Arc::new(AtomicBool::new(false)),
            looping: Arc::new(AtomicBool::new(false)),
            loop_selection: Arc::new(RwLock::new(None)),
            playback_started: Arc::new(AtomicBool::new(false)),
            event_tx,
            indices_tx,
        }
    }

    /// Create a MockSetlist with sample concert data and auto-start playback
    pub fn with_sample_data() -> Self {
        tracing::info!("MockSetlist::with_sample_data() - creating mock with sample concert data");
        let mock = Self::new();
        let setlist = Setlist::sample_concert_setlist().unwrap();
        tracing::info!(
            "MockSetlist: loaded setlist '{}' with {} songs",
            setlist.name,
            setlist.songs.len()
        );
        for (i, song) in setlist.songs.iter().enumerate() {
            tracing::debug!(
                "  Song {}: '{}' with {} sections",
                i,
                song.name,
                song.sections.len()
            );
        }
        mock.set_setlist(setlist);
        mock.set_active_indices_internal(Some(0), Some(0), None);

        // Set initial position to the start of song 0
        {
            let setlist = mock.setlist.read().unwrap();
            if let Some(s) = setlist.as_ref() {
                if let Some(song) = s.songs.first() {
                    let start = song.effective_start();
                    tracing::info!("MockSetlist: setting initial position to {} (song start)", start);
                    *mock.position.write().unwrap() = start;
                }
            }
        }

        tracing::info!("MockSetlist: initialized with song 0, section 0 active, starting playback");
        mock.start_playback();
        mock
    }

    /// Create a MockSetlist with a specific setlist
    pub fn with_setlist(setlist: Setlist) -> Self {
        let mock = Self::new();
        mock.set_setlist(setlist);
        mock
    }

    /// Get the underlying Setlist (for testing/inspection)
    pub fn setlist(&self) -> Option<Setlist> {
        self.setlist.read().unwrap().clone()
    }

    /// Get current active indices
    pub fn active_indices(&self) -> (Option<usize>, Option<usize>, Option<usize>) {
        (
            *self.active_song.read().unwrap(),
            *self.active_section.read().unwrap(),
            *self.active_slide.read().unwrap(),
        )
    }

    /// Get current position in seconds
    pub fn position(&self) -> f64 {
        *self.position.read().unwrap()
    }

    /// Set the setlist and emit event
    pub fn set_setlist(&self, setlist: Setlist) {
        tracing::debug!("MockSetlist::set_setlist() - setting '{}'", setlist.name);
        let info = SetlistInfo::from_setlist(&setlist);
        *self.setlist.write().unwrap() = Some(setlist);
        let _ = self.event_tx.send(SetlistEvent::SetlistChanged(info));
    }

    /// Update active indices (internal, does not notify)
    fn set_active_indices_internal(
        &self,
        song: Option<usize>,
        section: Option<usize>,
        slide: Option<usize>,
    ) {
        *self.active_song.write().unwrap() = song;
        *self.active_section.write().unwrap() = section;
        *self.active_slide.write().unwrap() = slide;
    }

    /// Update active indices and emit events
    pub fn set_active_indices(
        &self,
        song: Option<usize>,
        section: Option<usize>,
        slide: Option<usize>,
    ) {
        tracing::debug!(
            "MockSetlist::set_active_indices(song={:?}, section={:?}, slide={:?})",
            song,
            section,
            slide
        );
        self.set_active_indices_internal(song, section, slide);

        // Emit events
        let indices = self.build_active_indices();
        let _ = self.indices_tx.send(indices.clone());
        let _ = self.event_tx.send(SetlistEvent::ActiveIndicesChanged(indices));
    }

    /// Simulate playback position change and emit events
    pub fn simulate_position(&self, seconds: f64) {
        *self.position.write().unwrap() = seconds;

        // Emit position changed event
        let indices = self.build_active_indices();
        let _ = self.indices_tx.send(indices.clone());
        let _ = self
            .event_tx
            .send(SetlistEvent::PositionChanged { seconds, indices });
    }

    /// Build the current ActiveIndices
    fn build_active_indices(&self) -> ActiveIndices {
        let position = *self.position.read().unwrap();
        ActiveIndices {
            song_index: *self.active_song.read().unwrap(),
            section_index: *self.active_section.read().unwrap(),
            slide_index: *self.active_slide.read().unwrap(),
            song_progress: self.calculate_song_progress(position),
            section_progress: self.calculate_section_progress(position),
            is_playing: self.is_playing.load(Ordering::Relaxed),
            looping: self.looping.load(Ordering::Relaxed),
            loop_selection: self.loop_selection.read().unwrap().clone(),
        }
    }

    fn calculate_song_progress(&self, seconds: f64) -> Option<f64> {
        let song_idx = (*self.active_song.read().unwrap())?;
        let setlist = self.setlist.read().unwrap();
        let song = setlist.as_ref()?.songs.get(song_idx)?;

        let start = song.effective_start();
        let end = song.effective_end();
        let duration = end - start;

        if duration <= 0.0 {
            return Some(0.0);
        }

        let progress = (seconds - start) / duration;
        Some(progress.clamp(0.0, 1.0))
    }

    fn calculate_section_progress(&self, seconds: f64) -> Option<f64> {
        let song_idx = (*self.active_song.read().unwrap())?;
        let section_idx = (*self.active_section.read().unwrap())?;
        let setlist = self.setlist.read().unwrap();
        let song = setlist.as_ref()?.songs.get(song_idx)?;
        let section = song.sections.get(section_idx)?;

        let start = section.start_seconds()?;
        let end = section.end_seconds()?;
        let duration = end - start;

        if duration <= 0.0 {
            return Some(0.0);
        }

        let progress = (seconds - start) / duration;
        Some(progress.clamp(0.0, 1.0))
    }

    /// Get song count
    fn song_count(&self) -> usize {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .map(|s| s.songs.len())
            .unwrap_or(0)
    }

    /// Get section count for a song
    fn section_count(&self, song_index: usize) -> usize {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .map(|song| song.sections.len())
            .unwrap_or(0)
    }

    /// Get the start position of a song in seconds
    fn get_song_start(&self, song_index: usize) -> Option<f64> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .map(|song| song.effective_start())
    }

    /// Get the start position of a section in seconds
    fn get_section_start(&self, song_index: usize, section_index: usize) -> Option<f64> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .and_then(|song| song.sections.get(section_index))
            .and_then(|section| section.start_seconds())
    }

    /// Check if playback is currently active
    pub fn is_playing(&self) -> bool {
        self.is_playing.load(Ordering::Relaxed)
    }

    /// Start playback (begins advancing position automatically)
    pub fn start_playback(&self) {
        tracing::info!("MockSetlist::start_playback()");
        self.is_playing.store(true, Ordering::Relaxed);

        // Only start the tick loop once
        if self
            .playback_started
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::Relaxed)
            .is_ok()
        {
            self.spawn_playback_loop();
        }
    }

    /// Stop playback (pauses position advancement)
    pub fn stop_playback(&self) {
        tracing::info!("MockSetlist::stop_playback()");
        self.is_playing.store(false, Ordering::Relaxed);
    }

    /// Toggle playback state
    pub fn toggle_playback(&self) {
        if self.is_playing() {
            self.stop_playback();
        } else {
            self.start_playback();
        }
    }

    /// Seek to a specific position (in seconds)
    pub fn seek_to(&self, seconds: f64) {
        tracing::debug!("MockSetlist::seek_to({})", seconds);
        *self.position.write().unwrap() = seconds;
        self.update_indices_from_position(seconds);
        self.emit_position_update(seconds);
    }

    /// Spawn the background playback loop
    fn spawn_playback_loop(&self) {
        let mock = self.clone();
        tracing::debug!("MockSetlist: spawning playback loop");

        roam::session::runtime::spawn(async move {
            let tick_duration = Duration::from_millis(TICK_INTERVAL_MS);
            let delta = (TICK_INTERVAL_MS as f64 / 1000.0) * PLAYBACK_SPEED;

            loop {
                // Sleep for tick interval
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(tick_duration).await;
                #[cfg(target_arch = "wasm32")]
                gloo_timers::future::TimeoutFuture::new(TICK_INTERVAL_MS as u32).await;

                // Only advance if playing
                if !mock.is_playing.load(Ordering::Relaxed) {
                    continue;
                }

                // Advance position
                let new_position = {
                    let mut pos = mock.position.write().unwrap();
                    *pos += delta;

                    // Check for loop boundary and wrap if needed
                    if mock.looping.load(Ordering::Relaxed) {
                        if let Some(ref selection) = *mock.loop_selection.read().unwrap() {
                            let loop_end = selection.end.time.to_seconds();
                            let loop_start = selection.start.time.to_seconds();
                            if *pos >= loop_end {
                                tracing::debug!(
                                    "MockSetlist: loop end reached at {}, jumping back to {}",
                                    loop_end,
                                    loop_start
                                );
                                *pos = loop_start;
                            }
                        }
                    }

                    *pos
                };

                // Update indices based on new position
                mock.update_indices_from_position(new_position);

                // Always emit position update at 60fps for smooth progress bars
                mock.emit_position_update(new_position);
            }
        });
    }

    /// Update song/section indices based on current position
    /// Returns true if indices actually changed
    fn update_indices_from_position(&self, seconds: f64) -> bool {
        let setlist = self.setlist.read().unwrap();
        let Some(setlist) = setlist.as_ref() else {
            return false;
        };

        // Find which song contains this position
        let mut new_song_idx = None;
        let mut new_section_idx = None;

        for (song_idx, song) in setlist.songs.iter().enumerate() {
            let song_start = song.effective_start();
            let song_end = song.effective_end();

            if seconds >= song_start && seconds < song_end {
                new_song_idx = Some(song_idx);

                // Find which section contains this position
                for (section_idx, section) in song.sections.iter().enumerate() {
                    if let (Some(sec_start), Some(sec_end)) =
                        (section.start_seconds(), section.end_seconds())
                    {
                        if seconds >= sec_start && seconds < sec_end {
                            new_section_idx = Some(section_idx);
                            break;
                        }
                    }
                }
                break;
            }
        }

        // Update indices if changed
        let current_song = *self.active_song.read().unwrap();
        let current_section = *self.active_section.read().unwrap();

        let changed = new_song_idx != current_song || new_section_idx != current_section;

        if changed {
            tracing::debug!(
                "MockSetlist: indices changed - position {} -> song {:?} section {:?}",
                seconds,
                new_song_idx,
                new_section_idx
            );
            self.set_active_indices_internal(new_song_idx, new_section_idx, None);
        }

        changed
    }

    /// Emit a position update event (called at 60fps for smooth progress bars)
    fn emit_position_update(&self, seconds: f64) {
        let indices = self.build_active_indices();

        let _ = self.indices_tx.send(indices.clone());
        let _ = self
            .event_tx
            .send(SetlistEvent::PositionChanged { seconds, indices });
    }
}

impl Default for MockSetlist {
    fn default() -> Self {
        Self::new()
    }
}

// Implement PartialEq by always returning true (services are singletons)
// This is needed for Dioxus component props
impl PartialEq for MockSetlist {
    fn eq(&self, _other: &Self) -> bool {
        // Services are typically singletons, so we consider them equal
        // This allows Arc<MockSetlist> to be used as a Dioxus component prop
        true
    }
}

// endregion: --- MockSetlist

// region:    --- SetlistService Implementation

impl SetlistService for MockSetlist {
    async fn get_setlist(&self, _cx: &Context) -> Option<SetlistInfo> {
        let result = self
            .setlist
            .read()
            .unwrap()
            .as_ref()
            .map(SetlistInfo::from_setlist);
        tracing::debug!("MockSetlist::get_setlist() -> {:?}", result.as_ref().map(|s| &s.name));
        result
    }

    async fn get_songs(&self, _cx: &Context) -> Vec<SongInfo> {
        let result: Vec<SongInfo> = self
            .setlist
            .read()
            .unwrap()
            .as_ref()
            .map(|s| {
                s.songs
                    .iter()
                    .enumerate()
                    .map(|(i, song)| SongInfo::from_song(i, song))
                    .collect()
            })
            .unwrap_or_default();
        tracing::debug!("MockSetlist::get_songs() -> {} songs", result.len());
        result
    }

    async fn get_song(&self, _cx: &Context, index: usize) -> Option<SongInfo> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(index))
            .map(|song| SongInfo::from_song(index, song))
    }

    async fn get_sections(&self, _cx: &Context, song_index: usize) -> Vec<SectionInfo> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .map(|song| {
                song.sections
                    .iter()
                    .enumerate()
                    .map(|(i, section)| SectionInfo::from_section(i, section))
                    .collect()
            })
            .unwrap_or_default()
    }

    async fn get_section(
        &self,
        _cx: &Context,
        song_index: usize,
        section_index: usize,
    ) -> Option<SectionInfo> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .and_then(|song| song.sections.get(section_index))
            .map(|section| SectionInfo::from_section(section_index, section))
    }

    async fn get_measures(&self, _cx: &Context, song_index: usize) -> Vec<MeasureInfo> {
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_index))
            .map(|song| {
                // Get starting time signature
                let starting_time_sig = song
                    .starting_time_signature
                    .map(|ts| (ts.numerator, ts.denominator))
                    .unwrap_or((4, 4));

                song.measure_positions
                    .iter()
                    .map(|pos| {
                        // Find time signature at this measure's position
                        let mut time_sig = starting_time_sig;
                        let measure_time = pos.time.to_seconds();
                        for change in song.tempo_time_sig_changes.iter() {
                            let change_time = song.effective_start() + change.position_seconds();
                            if change_time <= measure_time {
                                if let Some((num, den)) = change.time_signature {
                                    time_sig = (num, den);
                                }
                            } else {
                                break;
                            }
                        }

                        MeasureInfo {
                            measure: pos.musical.measure,
                            time_seconds: pos.time.to_seconds(),
                            time_sig_numerator: time_sig.0,
                            time_sig_denominator: time_sig.1,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    async fn get_active_indices(&self, _cx: &Context) -> ActiveIndices {
        let indices = self.build_active_indices();
        tracing::debug!(
            "MockSetlist::get_active_indices() -> song={:?}, section={:?}",
            indices.song_index,
            indices.section_index
        );
        indices
    }

    async fn get_active_song(&self, _cx: &Context) -> Option<SongInfo> {
        let song_idx = (*self.active_song.read().unwrap())?;
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_idx))
            .map(|song| SongInfo::from_song(song_idx, song))
    }

    async fn get_active_section(&self, _cx: &Context) -> Option<SectionInfo> {
        let song_idx = (*self.active_song.read().unwrap())?;
        let section_idx = (*self.active_section.read().unwrap())?;
        self.setlist
            .read()
            .unwrap()
            .as_ref()
            .and_then(|s| s.songs.get(song_idx))
            .and_then(|song| song.sections.get(section_idx))
            .map(|section| SectionInfo::from_section(section_idx, section))
    }

    async fn get_song_at(&self, _cx: &Context, seconds: f64) -> Option<SongInfo> {
        let setlist = self.setlist.read().unwrap();
        setlist.as_ref().and_then(|s| {
            s.songs.iter().enumerate().find_map(|(i, song)| {
                let start = song.effective_start();
                let end = song.effective_end();
                if seconds >= start && seconds < end {
                    Some(SongInfo::from_song(i, song))
                } else {
                    None
                }
            })
        })
    }

    async fn get_section_at(&self, _cx: &Context, seconds: f64) -> Option<SectionInfo> {
        let setlist = self.setlist.read().unwrap();
        setlist.as_ref().and_then(|s| {
            for song in &s.songs {
                for (i, section) in song.sections.iter().enumerate() {
                    if section.contains_position(seconds) {
                        return Some(SectionInfo::from_section(i, section));
                    }
                }
            }
            None
        })
    }

    async fn execute(&self, _cx: &Context, cmd: SetlistCommand) {
        tracing::info!("MockSetlist::execute({:?})", cmd);
        match cmd {
            SetlistCommand::GoToSong { index } => {
                if index < self.song_count() {
                    // Seek to the start of the song
                    if let Some(start) = self.get_song_start(index) {
                        tracing::debug!("GoToSong: seeking to position {}", start);
                        self.seek_to(start);
                    }
                    self.set_active_indices(Some(index), Some(0), None);
                }
            }
            SetlistCommand::GoToSection { index } => {
                let song = *self.active_song.read().unwrap();
                if let Some(song_idx) = song {
                    if index < self.section_count(song_idx) {
                        // Seek to the start of the section
                        if let Some(start) = self.get_section_start(song_idx, index) {
                            tracing::debug!("GoToSection: seeking to position {}", start);
                            self.seek_to(start);
                        }
                        self.set_active_indices(Some(song_idx), Some(index), None);
                    }
                }
            }
            SetlistCommand::NextSong => {
                if let Some(current) = *self.active_song.read().unwrap() {
                    let next = current + 1;
                    if next < self.song_count() {
                        if let Some(start) = self.get_song_start(next) {
                            self.seek_to(start);
                        }
                        self.set_active_indices(Some(next), Some(0), None);
                    }
                }
            }
            SetlistCommand::PreviousSong => {
                if let Some(current) = *self.active_song.read().unwrap() {
                    if current > 0 {
                        let prev = current - 1;
                        if let Some(start) = self.get_song_start(prev) {
                            self.seek_to(start);
                        }
                        self.set_active_indices(Some(prev), Some(0), None);
                    }
                }
            }
            SetlistCommand::NextSection => {
                let song = *self.active_song.read().unwrap();
                let section = *self.active_section.read().unwrap();
                if let (Some(song_idx), Some(current)) = (song, section) {
                    let next = current + 1;
                    if next < self.section_count(song_idx) {
                        if let Some(start) = self.get_section_start(song_idx, next) {
                            self.seek_to(start);
                        }
                        self.set_active_indices(Some(song_idx), Some(next), None);
                    }
                }
            }
            SetlistCommand::PreviousSection => {
                let song = *self.active_song.read().unwrap();
                let section = *self.active_section.read().unwrap();
                if let (Some(song_idx), Some(current)) = (song, section) {
                    if current > 0 {
                        let prev = current - 1;
                        if let Some(start) = self.get_section_start(song_idx, prev) {
                            self.seek_to(start);
                        }
                        self.set_active_indices(Some(song_idx), Some(prev), None);
                    }
                }
            }
            SetlistCommand::SeekTo { seconds } => {
                self.seek_to(seconds);
            }
            SetlistCommand::ToggleSongLoop => {
                // Toggle loop and set loop region to current song boundaries
                let was_looping = self.looping.fetch_xor(true, Ordering::Relaxed);
                let now_looping = !was_looping;

                if now_looping {
                    // Set loop region to current song boundaries
                    if let Some(song_idx) = *self.active_song.read().unwrap() {
                        if let Some(setlist) = self.setlist.read().unwrap().as_ref() {
                            if let Some(song) = setlist.songs.get(song_idx) {
                                let start = song.effective_start();
                                let end = song.effective_end();
                                let selection = daw::primitives::TimeSelection::from_seconds(start, end);
                                *self.loop_selection.write().unwrap() = Some(selection);
                                tracing::debug!(
                                    "MockSetlist: song loop enabled, region {:.2} - {:.2}",
                                    start,
                                    end
                                );
                            }
                        }
                    }
                } else {
                    tracing::debug!("MockSetlist: loop disabled");
                }

                // Emit update to notify UI
                let position = *self.position.read().unwrap();
                self.emit_position_update(position);
            }
            SetlistCommand::ToggleSectionLoop => {
                // Toggle loop and set loop region to current section boundaries
                let was_looping = self.looping.fetch_xor(true, Ordering::Relaxed);
                let now_looping = !was_looping;

                if now_looping {
                    // Set loop region to current section boundaries
                    let song_idx = *self.active_song.read().unwrap();
                    let section_idx = *self.active_section.read().unwrap();
                    if let (Some(song_idx), Some(section_idx)) = (song_idx, section_idx) {
                        if let Some(setlist) = self.setlist.read().unwrap().as_ref() {
                            if let Some(song) = setlist.songs.get(song_idx) {
                                if let Some(section) = song.sections.get(section_idx) {
                                    if let (Some(start), Some(end)) =
                                        (section.start_seconds(), section.end_seconds())
                                    {
                                        let selection =
                                            daw::primitives::TimeSelection::from_seconds(start, end);
                                        *self.loop_selection.write().unwrap() = Some(selection);
                                        tracing::debug!(
                                            "MockSetlist: section loop enabled, region {:.2} - {:.2}",
                                            start,
                                            end
                                        );
                                    }
                                }
                            }
                        }
                    }
                } else {
                    tracing::debug!("MockSetlist: loop disabled");
                }

                // Emit update to notify UI
                let position = *self.position.read().unwrap();
                self.emit_position_update(position);
            }
            SetlistCommand::SetLoopRegion {
                start_seconds,
                end_seconds,
            } => {
                let selection = daw::primitives::TimeSelection::from_seconds(start_seconds, end_seconds);
                *self.loop_selection.write().unwrap() = Some(selection);
                self.looping.store(true, Ordering::Relaxed);
                tracing::debug!(
                    "MockSetlist: custom loop region set {:.2} - {:.2}",
                    start_seconds,
                    end_seconds
                );

                // Emit update to notify UI
                let position = *self.position.read().unwrap();
                self.emit_position_update(position);
            }
            SetlistCommand::ClearLoop => {
                self.looping.store(false, Ordering::Relaxed);
                *self.loop_selection.write().unwrap() = None;
                tracing::debug!("MockSetlist: loop cleared");

                // Emit update to notify UI
                let position = *self.position.read().unwrap();
                self.emit_position_update(position);
            }
            SetlistCommand::TogglePlayback => {
                self.toggle_playback();
                // Emit an immediate update so the UI reflects the new state
                let position = *self.position.read().unwrap();
                self.emit_position_update(position);
            }
        }
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<SetlistEvent>) {
        // Get initial setlist info (release lock before awaiting)
        let info = self
            .setlist
            .read()
            .unwrap()
            .as_ref()
            .map(SetlistInfo::from_setlist);

        // Send initial setlist event if we have a setlist
        if let Some(info) = info {
            let _ = events.send(&SetlistEvent::SetlistChanged(info)).await;
        }

        // Subscribe to broadcast channel and forward events to Tx
        let mut rx = self.event_tx.subscribe();
        roam::session::runtime::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        if events.send(&event).await.is_err() {
                            // Caller dropped their Rx, stop forwarding
                            break;
                        }
                    }
                    Err(_) => {
                        // Broadcast channel closed (shouldn't happen with Arc<MockSetlist>)
                        break;
                    }
                }
            }
        });
    }

    async fn subscribe_active(&self, _cx: &Context, indices: Tx<ActiveIndices>) {
        // Build indices (release lock before awaiting)
        let current = self.build_active_indices();
        // Send initial state immediately
        let _ = indices.send(&current).await;

        // Subscribe to broadcast channel and forward events to Tx
        let mut rx = self.indices_tx.subscribe();
        roam::session::runtime::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(idx) => {
                        if indices.send(&idx).await.is_err() {
                            // Caller dropped their Rx, stop forwarding
                            break;
                        }
                    }
                    Err(_) => {
                        // Broadcast channel closed
                        break;
                    }
                }
            }
        });
    }
}

// endregion: --- SetlistService Implementation

// region:    --- Clone implementation

impl Clone for MockSetlist {
    fn clone(&self) -> Self {
        Self {
            setlist: Arc::clone(&self.setlist),
            active_song: Arc::clone(&self.active_song),
            active_section: Arc::clone(&self.active_section),
            active_slide: Arc::clone(&self.active_slide),
            position: Arc::clone(&self.position),
            is_playing: Arc::clone(&self.is_playing),
            looping: Arc::clone(&self.looping),
            loop_selection: Arc::clone(&self.loop_selection),
            playback_started: Arc::clone(&self.playback_started),
            event_tx: self.event_tx.clone(),
            indices_tx: self.indices_tx.clone(),
        }
    }
}

// endregion: --- Clone implementation

// region:    --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_mock_is_empty() {
        let mock = MockSetlist::new();
        assert!(mock.setlist().is_none());
        assert_eq!(mock.active_indices(), (None, None, None));
    }

    #[test]
    fn test_with_sample_data() {
        let mock = MockSetlist::with_sample_data();
        let setlist = mock.setlist().expect("should have setlist");
        assert_eq!(setlist.songs.len(), 3); // Sample has 3 songs
        assert_eq!(mock.active_indices(), (Some(0), Some(0), None));
    }

    #[test]
    fn test_set_active_indices() {
        let mock = MockSetlist::with_sample_data();
        mock.set_active_indices(Some(1), Some(2), Some(3));
        assert_eq!(mock.active_indices(), (Some(1), Some(2), Some(3)));
    }

    #[test]
    fn test_simulate_position() {
        let mock = MockSetlist::with_sample_data();
        mock.simulate_position(42.5);
        assert!((mock.position() - 42.5).abs() < 0.001);
    }
}

// endregion: --- Tests
