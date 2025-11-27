//! Reactive Stream Logger Service
//!
//! Logs all reactive stream events from ControlSurfaceRx and ReactivePollingStreams
//! to verify that the reactive change detection is working correctly.

use reaper_rx::ControlSurfaceRx;
use crate::infrastructure::reactive_polling::ReactivePollingStreams;
use reaper_high::Reaper;
use reaper_medium::PositionInSeconds;
use rxrust::prelude::*;
use tracing::{debug, info, trace};

/// Helper function to format musical position from time
fn format_musical_position(pos: f64) -> String {
    let reaper = Reaper::get();
    let project = reaper.current_project();
    let medium_reaper = reaper.medium_reaper();
    
    if let Ok(pos_seconds) = PositionInSeconds::new(pos) {
        // Use TimeMap2_timeToBeats directly for more accurate musical position
        let beat_info = medium_reaper.time_map_2_time_to_beats(project.context(), pos_seconds);
        
        // Get the project measure offset using high-level API and apply it
        let measure_offset = project.measure_offset();
        let measure = beat_info.measure_index + measure_offset; // Apply project measure offset
        
        let beats_since_measure = beat_info.beats_since_measure.get();
        let beat = beats_since_measure.floor() as i32;
        let subdivision = ((beats_since_measure - beats_since_measure.floor()) * 1000.0).round() as i32;
        let subdivision = subdivision.max(0).min(999);
        format!("{}.{}.{:03}", measure, beat, subdivision)
    } else {
        String::new()
    }
}

/// Initialize logging for all reactive stream events
/// 
/// This subscribes to ALL streams in ControlSurfaceRx and logs when events occur.
/// Useful for testing and debugging to verify that reactive streams are working.
/// 
/// Note: This must be called from the main thread after ControlSurfaceRx is created
pub fn init_reactive_stream_logger(rx: &ControlSurfaceRx) {
    info!("🔍 Initializing reactive stream logger for ALL reactive events");
    
    // Main thread idle
    rx.main_thread_idle().subscribe(move |_| {
        trace!("💤 MAIN THREAD IDLE");
    });
    
    // Project events
    rx.project_switched().subscribe(move |project| {
        info!("📁 PROJECT SWITCHED: {:?}", project);
    });
    
    // Note: project_closed exists as a field but doesn't have a public method yet
    
    rx.global_automation_override_changed().subscribe(move |_| {
        let reaper = Reaper::get();
        let override_mode = reaper.global_automation_override();
        debug!("🌐 GLOBAL AUTOMATION OVERRIDE CHANGED: {:?}", override_mode);
    });
    
    rx.bookmarks_changed().subscribe(move |_| {
        debug!("🔖 BOOKMARKS CHANGED");
    });
    
    // Track structure events
    rx.track_added().subscribe(move |track| {
        info!("➕ TRACK ADDED: {:?}", track);
    });
    
    rx.track_removed().subscribe(move |track| {
        info!("➖ TRACK REMOVED: {:?}", track);
    });
    
    rx.tracks_reordered().subscribe(move |project| {
        info!("🔄 TRACKS REORDERED: {:?}", project);
    });
    
    // Track property events
    rx.track_volume_changed().subscribe(move |track| {
        if track.is_available() {
            let volume = track.volume();
            let db = volume.to_db();
            debug!("🔊 TRACK VOLUME CHANGED: track={:?} volume={}", track.name(), db);
        } else {
            debug!("🔊 TRACK VOLUME CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_volume_touched().subscribe(move |track| {
        trace!("👆 TRACK VOLUME TOUCHED: {:?}", track);
    });
    
    rx.track_pan_changed().subscribe(move |track| {
        if track.is_available() {
            let pan = track.pan();
            debug!("🎚️ TRACK PAN CHANGED: track={:?} pan={}", track.name(), pan);
        } else {
            debug!("🎚️ TRACK PAN CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_pan_touched().subscribe(move |(track, old_pan, new_pan)| {
        trace!("👆 TRACK PAN TOUCHED: track={:?} old={:?} new={:?}", track, old_pan, new_pan);
    });
    
    rx.track_mute_changed().subscribe(move |track| {
        if track.is_available() {
            let is_muted = track.is_muted();
            debug!("🔇 TRACK MUTE CHANGED: track={:?} muted={}", track.name(), is_muted);
        } else {
            debug!("🔇 TRACK MUTE CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_mute_touched().subscribe(move |track| {
        trace!("👆 TRACK MUTE TOUCHED: {:?}", track);
    });
    
    rx.track_solo_changed().subscribe(move |track| {
        if track.is_available() {
            let solo_mode = track.solo_mode();
            debug!("🎵 TRACK SOLO CHANGED: track={:?} solo={:?}", track.name(), solo_mode);
        } else {
            debug!("🎵 TRACK SOLO CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_arm_changed().subscribe(move |track| {
        if track.is_available() {
            let is_armed = track.is_armed(false);
            debug!("🎙️ TRACK ARM CHANGED: track={:?} armed={}", track.name(), is_armed);
        } else {
            debug!("🎙️ TRACK ARM CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_selected_changed().subscribe(move |(track, is_selected)| {
        debug!("✅ TRACK SELECTED CHANGED: track={:?} selected={}", track, is_selected);
    });
    
    rx.track_name_changed().subscribe(move |track| {
        info!("🏷️ TRACK NAME CHANGED: {:?}", track);
    });
    
    rx.track_input_changed().subscribe(move |track| {
        if track.is_available() {
            let rec_input = track.recording_input();
            trace!("🎤 TRACK INPUT CHANGED: track={:?} input={:?}", track.name(), rec_input);
        } else {
            trace!("🎤 TRACK INPUT CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_input_monitoring_changed().subscribe(move |track| {
        if track.is_available() {
            let monitoring = track.input_monitoring_mode();
            trace!("👂 TRACK INPUT MONITORING CHANGED: track={:?} mode={:?}", track.name(), monitoring);
        } else {
            trace!("👂 TRACK INPUT MONITORING CHANGED: {:?} (unavailable)", track);
        }
    });
    
    rx.track_automation_mode_changed().subscribe(move |track| {
        if track.is_available() {
            let auto_mode = track.effective_automation_mode();
            trace!("🤖 TRACK AUTOMATION MODE CHANGED: track={:?} mode={:?}", track.name(), auto_mode);
        } else {
            trace!("🤖 TRACK AUTOMATION MODE CHANGED: {:?} (unavailable)", track);
        }
    });
    
    // Route events
    rx.track_route_volume_changed().subscribe(move |route| {
        trace!("🔊 ROUTE VOLUME CHANGED: {:?}", route);
    });
    
    rx.track_route_volume_touched().subscribe(move |route| {
        trace!("👆 ROUTE VOLUME TOUCHED: {:?}", route);
    });
    
    rx.track_route_pan_changed().subscribe(move |route| {
        trace!("🎚️ ROUTE PAN CHANGED: {:?}", route);
    });
    
    rx.track_route_pan_touched().subscribe(move |route| {
        trace!("👆 ROUTE PAN TOUCHED: {:?}", route);
    });
    
    rx.receive_count_changed().subscribe(move |track| {
        trace!("📥 RECEIVE COUNT CHANGED: {:?}", track);
    });
    
    rx.track_send_count_changed().subscribe(move |track| {
        trace!("📤 TRACK SEND COUNT CHANGED: {:?}", track);
    });
    
    rx.hardware_output_send_count_changed().subscribe(move |track| {
        trace!("🔌 HARDWARE OUTPUT SEND COUNT CHANGED: {:?}", track);
    });
    
    // FX events
    rx.fx_added().subscribe(move |fx| {
        debug!("✨ FX ADDED: {:?}", fx);
    });
    
    rx.fx_removed().subscribe(move |fx| {
        debug!("🗑️ FX REMOVED: {:?}", fx);
    });
    
    rx.fx_enabled_changed().subscribe(move |fx| {
        if fx.is_available() {
            let is_enabled = fx.is_enabled();
            trace!("⚡ FX ENABLED CHANGED: fx={:?} enabled={}", fx.name(), is_enabled);
        } else {
            trace!("⚡ FX ENABLED CHANGED: {:?} (unavailable)", fx);
        }
    });
    
    rx.fx_opened().subscribe(move |fx| {
        debug!("📂 FX OPENED: {:?}", fx);
    });
    
    rx.fx_closed().subscribe(move |fx| {
        debug!("📁 FX CLOSED: {:?}", fx);
    });
    
    rx.fx_focused().subscribe(move |fx_opt| {
        match fx_opt {
            Some(fx) => debug!("🎯 FX FOCUSED: {:?}", fx),
            None => trace!("🎯 FX FOCUS CLEARED"),
        }
    });
    
    rx.fx_reordered().subscribe(move |track| {
        debug!("🔄 FX REORDERED: track={:?}", track);
    });
    
    rx.fx_parameter_value_changed().subscribe(move |param| {
        trace!("🎛️ FX PARAMETER VALUE CHANGED: {:?}", param);
    });
    
    rx.fx_parameter_touched().subscribe(move |param| {
        trace!("👆 FX PARAMETER TOUCHED: {:?}", param);
    });
    
    rx.fx_preset_changed().subscribe(move |fx| {
        debug!("💾 FX PRESET CHANGED: {:?}", fx);
    });
    
    // Transport events
    rx.play_state_changed().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let is_playing = project.is_playing();
        let is_paused = project.is_paused();
        let is_recording = project.is_recording();
        let state_str = if is_recording {
            "recording"
        } else if is_playing {
            "playing"
        } else if is_paused {
            "paused"
        } else {
            "stopped"
        };
        info!("▶️ PLAY STATE CHANGED: {}", state_str);
    });
    
    rx.master_tempo_changed().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let tempo = project.tempo();
        info!("🎼 MASTER TEMPO CHANGED: {:.2} BPM", tempo.bpm());
    });
    
    rx.master_tempo_touched().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let tempo = project.tempo();
        trace!("👆 MASTER TEMPO TOUCHED: {:.2} BPM", tempo.bpm());
    });
    
    rx.master_playrate_changed().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let playrate = project.play_rate();
        let factor = playrate.playback_speed_factor();
        debug!("⏩ MASTER PLAY RATE CHANGED: {:.3}x", factor.get());
    });
    
    rx.master_playrate_touched().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let playrate = project.play_rate();
        let factor = playrate.playback_speed_factor();
        trace!("👆 MASTER PLAY RATE TOUCHED: {:.3}x", factor.get());
    });
    
    rx.repeat_state_changed().subscribe(move |_| {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let is_repeat = project.repeat_is_enabled();
        debug!("🔁 REPEAT STATE CHANGED: {}", if is_repeat { "enabled" } else { "disabled" });
    });
    
    info!("✅ Reactive stream logger initialized - ALL reactive events will be logged");
}

/// Initialize logging for reactive polling streams
/// 
/// This subscribes to all polling streams and logs when events occur.
/// Useful for testing and debugging to verify that polling streams are working.
/// 
/// Note: This must be called from the main thread after ReactivePollingStreams is created
pub fn init_reactive_polling_logger(streams: &ReactivePollingStreams) {
    info!("🔍 Initializing reactive polling stream logger");
    
    // Subscribe to edit cursor position changes - using TimeMap2_timeToBeats directly
    streams.edit_cursor_position_changed().subscribe(move |pos| {
        let musical = format_musical_position(pos);
        if !musical.is_empty() {
            info!("✏️ EDIT CURSOR POSITION CHANGED: {:.3}s | {}", pos, musical);
        } else {
            info!("✏️ EDIT CURSOR POSITION CHANGED: {:.3}s", pos);
        }
    });
    
    // Subscribe to play cursor position changes - using TimeMap2_timeToBeats directly
    streams.play_cursor_position_changed().subscribe(move |pos| {
        let musical = format_musical_position(pos);
        if !musical.is_empty() {
            info!("▶️ PLAY CURSOR POSITION CHANGED: {:.3}s | {}", pos, musical);
        } else {
            info!("▶️ PLAY CURSOR POSITION CHANGED: {:.3}s", pos);
        }
    });
    
    // Subscribe to transport position changes - using TimeMap2_timeToBeats directly
    streams.transport_position_changed().subscribe(move |pos| {
        let musical = format_musical_position(pos);
        if !musical.is_empty() {
            info!("🎵 TRANSPORT POSITION CHANGED: {:.3}s | {}", pos, musical);
        } else {
            info!("🎵 TRANSPORT POSITION CHANGED: {:.3}s", pos);
        }
    });
    
    streams.active_song_index_changed().subscribe(move |song_idx| {
        info!("🎵 ACTIVE SONG INDEX CHANGED: {:?}", song_idx);
    });
    
    streams.active_section_index_changed().subscribe(move |section_idx| {
        info!("📑 ACTIVE SECTION INDEX CHANGED: {:?}", section_idx);
    });
    
    streams.active_slide_index_changed().subscribe(move |slide_idx| {
        info!("📄 ACTIVE SLIDE INDEX CHANGED: {:?}", slide_idx);
    });
    
    // Subscribe to full transport state changes
    streams.transport_changed().subscribe(move |transport| {
        info!("🎛️ TRANSPORT CHANGED: {}", transport);
    });
    
    info!("✅ Reactive polling stream logger initialized - ALL polling events will be logged");
}

