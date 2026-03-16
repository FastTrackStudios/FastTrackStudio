//! Sync bridge — wires the sync engine + Ableton Link into the REAPER extension.
//!
//! Provides:
//! - `ReaperLinkCallbacks` — implements `sync::link::LinkCallbacks` using REAPER APIs
//! - `init()` — creates and stores the Link engine
//! - `tick()` — called from `timer_callback()` to drive Link transport sync
//! - `on_audio_buffer()` — called from audio hook for precise timing
//! - `connect_peer()` — connects to another REAPER instance for full sync

use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext::CurrentProject};
use std::sync::{Mutex, OnceLock};
use sync::link::{AudioBufferInfo, EngineConfig, LinkCallbacks, LinkState, Mode};
use tracing::info;

// ── Global State ─────────────────────────────────────────────────────────────

static LINK_ENGINE: OnceLock<Mutex<sync::link::Engine>> = OnceLock::new();

/// Initialize the sync bridge.
///
/// Creates the Link engine with the current project tempo.
/// Call this during extension startup, after daw-reaper is initialized.
pub fn init() {
    let reaper = Reaper::get().medium_reaper();
    let tempo = unsafe { reaper.low().Master_GetTempo() };

    let config = EngineConfig {
        mode: Mode::Off, // Start disabled — enabled via action or API
        push_local_tempo: true,
        start_stop_sync: true,
        phase_tolerance: None,
    };

    let engine = sync::link::Engine::new(tempo, config);

    let _ = LINK_ENGINE.set(Mutex::new(engine));
    info!("Sync bridge initialized (Link engine created, mode=Off)");
}

/// Get a lock on the Link engine. Returns None if not initialized.
fn with_engine<R>(f: impl FnOnce(&mut sync::link::Engine) -> R) -> Option<R> {
    LINK_ENGINE.get().and_then(|m| m.lock().ok()).map(|mut e| f(&mut e))
}

// ── Public API (called from timer_callback, actions, etc.) ───────────────────

/// Tick the Link engine. Call from `timer_callback()` on the main thread.
///
/// Also checks for ExtState-based control:
/// - `FTS_SYNC/link_mode` = "puppet" | "master" | "off"
pub fn tick() {
    // Check for ExtState-based Link control (allows tests and UI to enable/disable)
    check_ext_state_control();

    if let Some(engine_mutex) = LINK_ENGINE.get() {
        if let Ok(mut engine) = engine_mutex.lock() {
            if engine.mode() != Mode::Off {
                // Log peer count periodically (every ~5s = 150 ticks at 30Hz)
                static TICK_COUNT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
                let count = TICK_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if count % 150 == 0 {
                    info!(
                        "Link tick: mode={:?}, enabled={}, peers={}, offset={:.4}s",
                        engine.mode(),
                        engine.is_enabled(),
                        engine.num_peers(),
                        engine.timeline_offset()
                    );
                }

                let callbacks = ReaperLinkCallbacks;
                engine.tick(&callbacks);
            }
        }
    }
}

/// Poll ExtState for Link control commands.
fn check_ext_state_control() {
    static LAST_MODE: OnceLock<Mutex<String>> = OnceLock::new();
    let last = LAST_MODE.get_or_init(|| Mutex::new(String::new()));

    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();

    let section = c"FTS_SYNC";
    let key = c"link_mode";

    let value = unsafe {
        let ptr = low.GetExtState(section.as_ptr(), key.as_ptr());
        if ptr.is_null() {
            String::new()
        } else {
            std::ffi::CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    };

    if let Ok(mut last_mode) = last.lock() {
        if value != *last_mode && !value.is_empty() {
            *last_mode = value.clone();
            match value.as_str() {
                "puppet" => enable_link(Mode::Puppet),
                "master" => enable_link(Mode::Master),
                "off" => disable_link(),
                _ => {}
            }
        }
    }
}

/// Feed audio buffer timing info. Call from the audio hook (pre-buffer).
pub fn on_audio_buffer(buf_len: usize, sample_rate: f64, wall_clock_time: f64) {
    if let Some(engine_mutex) = LINK_ENGINE.get() {
        if let Ok(mut engine) = engine_mutex.lock() {
            engine.on_audio_buffer(AudioBufferInfo {
                buf_len,
                sample_rate,
                wall_clock_time,
            });
        }
    }
}

/// Enable Ableton Link and set the sync mode.
pub fn enable_link(mode: Mode) {
    with_engine(|engine| {
        engine.set_enabled(true);
        engine.set_mode(mode);
        info!("Link enabled, mode={:?}", mode);
    });
}

/// Disable Ableton Link.
pub fn disable_link() {
    with_engine(|engine| {
        engine.set_mode(Mode::Off);
        engine.set_enabled(false);
        info!("Link disabled");
    });
}

/// Get current Link status.
pub fn is_link_enabled() -> bool {
    with_engine(|engine| engine.is_enabled()).unwrap_or(false)
}

/// Get number of Link peers.
pub fn num_link_peers() -> u64 {
    with_engine(|engine| engine.num_peers()).unwrap_or(0)
}

/// Get the current timeline offset (REAPER vs Link phase difference in seconds).
pub fn timeline_offset() -> f64 {
    with_engine(|engine| engine.timeline_offset()).unwrap_or(0.0)
}

// ── LinkCallbacks Implementation ─────────────────────────────────────────────

/// Implements `LinkCallbacks` using REAPER's C API via reaper-high/medium/low.
///
/// All methods run on the main thread (called from timer_callback).
struct ReaperLinkCallbacks;

impl LinkCallbacks for ReaperLinkCallbacks {
    fn capture_state(&self) -> LinkState {
        let reaper = Reaper::get().medium_reaper();
        let low = reaper.low();

        unsafe {
            let play_state = low.GetPlayState();
            let is_playing = (play_state & 1) != 0;

            let position = if is_playing {
                low.GetPlayPosition2()
            } else {
                low.GetCursorPosition()
            };

            let tempo = low.Master_GetTempo();
            let playrate = low.Master_GetPlayRate(std::ptr::null_mut());
            let output_latency = low.GetOutputLatency();

            // Get time signature at current position
            let mut timesig_num: i32 = 4;
            let mut timesig_denom: i32 = 4;
            let mut _tempo_at: f64 = 0.0;
            low.TimeMap_GetTimeSigAtTime(
                std::ptr::null_mut(),
                position,
                &mut timesig_num,
                &mut timesig_denom,
                &mut _tempo_at,
            );

            // Get beat and QN position
            let mut measures: i32 = 0;
            let beat = low.TimeMap2_timeToBeats(
                std::ptr::null_mut(),
                position,
                &mut measures,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );
            let qn = low.TimeMap2_timeToQN(std::ptr::null_mut(), position);

            // Loop state
            let is_looping = low.GetSetRepeat(-1) == 1;

            // Loop range
            let mut loop_start: f64 = 0.0;
            let mut loop_end: f64 = 0.0;
            low.GetSet_LoopTimeRange(
                false,
                false,
                &mut loop_start,
                &mut loop_end,
                false,
            );
            let loop_range = if is_looping && loop_end > loop_start {
                Some((loop_start, loop_end))
            } else {
                None
            };

            // Loop range in beats
            let loop_range_beats = loop_range.map(|(start, end)| {
                let start_beat = low.TimeMap2_timeToBeats(
                    std::ptr::null_mut(),
                    start,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                );
                let end_beat = low.TimeMap2_timeToBeats(
                    std::ptr::null_mut(),
                    end,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                );
                (start_beat, end_beat)
            });

            LinkState {
                is_playing,
                position_seconds: position,
                beat_position: beat,
                qn_position: qn,
                tempo_bpm: tempo,
                timesig_num,
                timesig_denom,
                output_latency,
                is_looping,
                playrate,
                loop_range,
                loop_range_beats,
            }
        }
    }

    fn play(&self) {
        let reaper = Reaper::get().medium_reaper();
        unsafe { reaper.low().OnPlayButton() };
    }

    fn stop(&self) {
        let reaper = Reaper::get().medium_reaper();
        unsafe { reaper.low().OnStopButton() };
    }

    fn set_cursor_position(&self, seconds: f64) {
        let reaper = Reaper::get().medium_reaper();
        unsafe { reaper.low().SetEditCurPos(seconds, false, false) };
    }

    fn set_tempo(&self, bpm: f64) {
        let reaper = Reaper::get().medium_reaper();
        let low = reaper.low();
        unsafe {
            let pos = if (low.GetPlayState() & 1) != 0 {
                low.GetPlayPosition2()
            } else {
                low.GetCursorPosition()
            };

            // Find the tempo marker at current position and update it
            let ptidx = low.FindTempoTimeSigMarker(std::ptr::null_mut(), pos);
            let mut timepos: f64 = 0.0;
            let mut _measurepos: i32 = 0;
            let mut _beatpos: f64 = 0.0;
            let mut timesig_num: i32 = 0;
            let mut timesig_denom: i32 = 0;
            let mut _lineartempo = false;
            low.GetTempoTimeSigMarker(
                std::ptr::null_mut(),
                ptidx,
                &mut timepos,
                &mut _measurepos,
                &mut _beatpos,
                std::ptr::null_mut(),
                &mut timesig_num,
                &mut timesig_denom,
                &mut _lineartempo,
            );
            low.SetTempoTimeSigMarker(
                std::ptr::null_mut(),
                ptidx,
                timepos,
                _measurepos,
                _beatpos,
                bpm,
                timesig_num,
                timesig_denom,
                _lineartempo,
            );
        }
    }

    fn get_loop_range(&self) -> Option<(f64, f64)> {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            let mut start: f64 = 0.0;
            let mut end: f64 = 0.0;
            reaper.low().GetSet_LoopTimeRange(false, false, &mut start, &mut end, false);
            if end > start {
                Some((start, end))
            } else {
                None
            }
        }
    }

    fn nudge_playrate_up(&self) {
        let reaper = Reaper::get().medium_reaper();
        // Action 40524: "Transport: Increase playrate by ~6% (one semitone)"
        reaper.main_on_command_ex(CommandId::new(40524), 0, CurrentProject);
    }

    fn nudge_playrate_down(&self) {
        let reaper = Reaper::get().medium_reaper();
        // Action 40525: "Transport: Decrease playrate by ~6% (one semitone)"
        reaper.main_on_command_ex(CommandId::new(40525), 0, CurrentProject);
    }

    fn reset_playrate(&self) {
        let reaper = Reaper::get().medium_reaper();
        // Action 40521: "Transport: Set playrate to 1.0"
        reaper.main_on_command_ex(CommandId::new(40521), 0, CurrentProject);
    }

    fn go_to_region(&self, region_id: i32) {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            reaper.low().GoToRegion(std::ptr::null_mut(), region_id, false);
        }
    }

    fn add_project_marker(&self, is_region: bool, start: f64, end: f64, name: &str) -> i32 {
        let reaper = Reaper::get().medium_reaper();
        let c_name = std::ffi::CString::new(name).unwrap_or_default();
        unsafe {
            reaper.low().AddProjectMarker2(
                std::ptr::null_mut(),
                is_region,
                start,
                end,
                c_name.as_ptr(),
                -1,
                0,
            )
        }
    }

    fn remove_project_marker(&self, marker_id: i32) {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            reaper.low().DeleteProjectMarkerByIndex(std::ptr::null_mut(), marker_id);
        }
    }

    fn set_tempo_at_position(
        &self,
        position: f64,
        bpm: f64,
        timesig_num: i32,
        timesig_denom: i32,
    ) {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            reaper.low().SetTempoTimeSigMarker(
                std::ptr::null_mut(),
                -1, // -1 = add new marker
                position,
                0,
                0.0,
                bpm,
                timesig_num,
                timesig_denom,
                false,
            );
        }
    }

    fn get_next_measure_position(&self) -> f64 {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            let low = reaper.low();
            let pos = low.GetCursorPosition();
            let mut measures: i32 = 0;
            let mut beats: i32 = 0;
            low.TimeMap2_timeToBeats(
                std::ptr::null_mut(),
                pos,
                &mut measures,
                &mut beats,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );
            // Next full measure
            low.TimeMap2_beatsToTime(std::ptr::null_mut(), 0.0, &mut measures)
        }
    }

    fn create_midi_item(&self, track_index: usize, start: f64, end: f64) {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            let track = reaper.low().GetTrack(std::ptr::null_mut(), track_index as i32);
            if !track.is_null() {
                reaper.low().CreateNewMIDIItemInProj(track, start, end, std::ptr::null_mut());
            }
        }
    }

    fn get_project_length(&self) -> f64 {
        let reaper = Reaper::get().medium_reaper();
        unsafe { reaper.low().GetProjectLength(std::ptr::null_mut()) }
    }

    fn time_to_beats(&self, time_seconds: f64) -> (f64, i32) {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            let mut measures: i32 = 0;
            let beat = reaper.low().TimeMap2_timeToBeats(
                std::ptr::null_mut(),
                time_seconds,
                &mut measures,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );
            (beat, measures)
        }
    }

    fn beats_to_time(&self, beat: f64, measure: i32) -> f64 {
        let reaper = Reaper::get().medium_reaper();
        unsafe {
            let mut m = measure;
            reaper.low().TimeMap2_beatsToTime(std::ptr::null_mut(), beat, &mut m)
        }
    }

    fn clear_link_dummy_objects(&self) {
        // Clear any markers/regions with "realink" in the name
        // (created during quantized launch)
        let reaper = Reaper::get().medium_reaper();
        let low = reaper.low();
        unsafe {
            let count = low.CountProjectMarkers(std::ptr::null_mut(), std::ptr::null_mut(), std::ptr::null_mut());
            // Iterate in reverse to avoid index shifting
            for i in (0..count).rev() {
                let mut is_region = false;
                let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
                low.EnumProjectMarkers(
                    i,
                    &mut is_region,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    &mut name_ptr,
                    std::ptr::null_mut(),
                );
                if !name_ptr.is_null() {
                    let name = std::ffi::CStr::from_ptr(name_ptr).to_string_lossy();
                    if name.contains("realink") {
                        low.DeleteProjectMarkerByIndex(std::ptr::null_mut(), i);
                    }
                }
            }
        }
    }
}
