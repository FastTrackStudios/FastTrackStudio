//! Command handler for processing IPC commands from desktop app
//! TODO: Replace with irpc ReaperProtocol handler

/*
use peer_2_peer::unix_socket::IpcMessage;
use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext};
use crate::reaper_transport::ReaperTransport;
use daw::transport::TransportActions;
use tracing::{info, warn};

/// Handle an IPC command from the desktop app
/// This runs on REAPER's main thread (called from timer callback or action)
pub fn handle_ipc_command(message: IpcMessage) {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    let mut transport = ReaperTransport::new(current_project);
    
    match message {
        // Global transport commands
        IpcMessage::GlobalPlay => {
            if let Err(e) = transport.play() {
                warn!("Failed to play: {}", e);
            } else {
                info!("Play command executed");
            }
        }
        IpcMessage::GlobalPause => {
            if let Err(e) = transport.pause() {
                warn!("Failed to pause: {}", e);
            } else {
                info!("Pause command executed");
            }
        }
        IpcMessage::GlobalStop => {
            if let Err(e) = transport.stop() {
                warn!("Failed to stop: {}", e);
            } else {
                info!("Stop command executed");
            }
        }
        IpcMessage::GlobalStartPlaying => {
            // Start playing from last cursor position (same as play)
            if let Err(e) = transport.play() {
                warn!("Failed to start playing: {}", e);
            } else {
                info!("Start playing command executed");
            }
        }
        IpcMessage::GlobalContinuePlaying => {
            // Continue from current playhead (same as play)
            if let Err(e) = transport.play() {
                warn!("Failed to continue playing: {}", e);
            } else {
                info!("Continue playing command executed");
            }
        }
        IpcMessage::GlobalPlayPause => {
            if let Err(e) = transport.play_pause() {
                warn!("Failed to play/pause: {}", e);
            } else {
                info!("Play/pause toggle executed");
            }
        }
        IpcMessage::GlobalPlayStop => {
            if let Err(e) = transport.play_stop() {
                warn!("Failed to play/stop: {}", e);
            } else {
                info!("Play/stop toggle executed");
            }
        }
        IpcMessage::GlobalStartRecording => {
            if let Err(e) = transport.start_recording() {
                warn!("Failed to start recording: {}", e);
            } else {
                info!("Start recording command executed");
            }
        }
        IpcMessage::GlobalStopRecording => {
            if let Err(e) = transport.stop_recording() {
                warn!("Failed to stop recording: {}", e);
            } else {
                info!("Stop recording command executed");
            }
        }
        IpcMessage::GlobalToggleRecording => {
            if let Err(e) = transport.toggle_recording() {
                warn!("Failed to toggle recording: {}", e);
            } else {
                info!("Toggle recording executed");
            }
        }
        IpcMessage::GlobalSetQuantization { quantization } => {
            // REAPER doesn't have a direct quantization API
            // This would need to be implemented via actions or custom logic
            warn!("Set quantization not yet implemented: {:?}", quantization);
        }
        
        // Loop commands
        IpcMessage::LoopEnable => {
            // REAPER loop is controlled via actions
            let cmd_id = CommandId::new(1068); // Toggle repeat
            reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Loop enabled");
        }
        IpcMessage::LoopEscape => {
            // Disable loop and potentially jump to next section
            let cmd_id = CommandId::new(1068); // Toggle repeat
            reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Loop escaped");
        }
        IpcMessage::LoopToggle => {
            let cmd_id = CommandId::new(1068); // Toggle repeat
            reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Loop toggled");
        }
        
        // Setlist navigation commands
        IpcMessage::SetlistJumpToTime { time_beats } => {
            // Convert beats to seconds and seek
            // This is approximate - would need tempo map for accurate conversion
            warn!("Jump to time (beats) not yet fully implemented: {} beats", time_beats);
        }
        IpcMessage::SetlistJumpToSong { target } => {
            // Jump to song by name or index
            warn!("Jump to song not yet implemented: {}", target);
        }
        IpcMessage::SetlistJumpBySongs { steps, force } => {
            // Jump forward/backward by N songs
            warn!("Jump by songs not yet implemented: steps={}, force={}", steps, force);
        }
        IpcMessage::SetlistJumpToSection { target, fuzzy } => {
            // Jump to section by name or index
            warn!("Jump to section not yet implemented: target={}, fuzzy={}", target, fuzzy);
        }
        IpcMessage::SetlistJumpBySections { steps, force } => {
            // Jump forward/backward by N sections
            warn!("Jump by sections not yet implemented: steps={}, force={}", steps, force);
        }
        IpcMessage::SetlistJumpToQueued { instant } => {
            warn!("Jump to queued not yet implemented: instant={}", instant);
        }
        IpcMessage::SetlistPlayCuedSong => {
            warn!("Play cued song not yet implemented");
        }
        IpcMessage::SetlistSetLocatorsFromClips => {
            warn!("Set locators from clips not yet implemented");
        }
        IpcMessage::SetlistRemoveClipLocators => {
            warn!("Remove clip locators not yet implemented");
        }
        
        // Lyrics commands
        IpcMessage::LyricsJumpByLines { track_slug, lines } => {
            warn!("Lyrics jump by lines not yet implemented: track={}, lines={}", track_slug, lines);
        }
        
        // Click track commands
        IpcMessage::ClickMute { mute } => {
            warn!("Click mute not yet implemented: {:?}", mute);
        }
        IpcMessage::ClickSolo { solo } => {
            warn!("Click solo not yet implemented: {:?}", solo);
        }
        
        // Audio interface commands
        IpcMessage::AudioInterfacesSetScene { scene } => {
            warn!("Audio interface set scene not yet implemented: {}", scene);
        }
        IpcMessage::AudioInterfacesToggleScene => {
            warn!("Audio interface toggle scene not yet implemented");
        }
        
        // Floating window commands
        IpcMessage::FloatingWindowShow => {
            warn!("Floating window show not yet implemented");
        }
        IpcMessage::FloatingWindowHide => {
            warn!("Floating window hide not yet implemented");
        }
        IpcMessage::FloatingWindowToggle => {
            warn!("Floating window toggle not yet implemented");
        }
        
        // Settings commands
        IpcMessage::SettingsSetAbleNet { enabled } => {
            warn!("Settings AbleNet not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetAbleNetDriftCorrection { enabled } => {
            warn!("Settings drift correction not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetAutoplay { enabled } => {
            warn!("Settings autoplay not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetAlwaysStopOnSongEnd { enabled } => {
            warn!("Settings always stop on song end not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetAutoJumpToNextSong { enabled } => {
            warn!("Settings auto jump to next song not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetAutoLoopCurrentSection { enabled } => {
            warn!("Settings auto loop current section not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetCountIn { enabled } => {
            warn!("Settings count in not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetCountInSoloClick { enabled } => {
            warn!("Settings count in solo click not yet implemented: {:?}", enabled);
        }
        IpcMessage::SettingsSetCountInDuration { bars } => {
            warn!("Settings count in duration not yet implemented: {} bars", bars);
        }
        IpcMessage::SettingsSetJumpMode { mode } => {
            warn!("Settings jump mode not yet implemented: {}", mode);
        }
        IpcMessage::SettingsSetSafeMode { enabled } => {
            warn!("Settings safe mode not yet implemented: {:?}", enabled);
        }
        
        // Device control commands
        IpcMessage::DevicesShowPage { devices, page } => {
            warn!("Devices show page not yet implemented: devices={}, page={}", devices, page);
        }
        IpcMessage::DevicesSetSetting { devices, setting, value } => {
            warn!("Devices set setting not yet implemented: devices={}, setting={}, value={}", devices, setting, value);
        }
        IpcMessage::DevicesLock { devices } => {
            warn!("Devices lock not yet implemented: {}", devices);
        }
        IpcMessage::DevicesUnlock { devices } => {
            warn!("Devices unlock not yet implemented: {}", devices);
        }
        IpcMessage::DevicesToggleLock { devices } => {
            warn!("Devices toggle lock not yet implemented: {}", devices);
        }
        
        // Notification commands
        IpcMessage::NotifyToast { devices, title, description, duration_ms } => {
            warn!("Notify toast not yet implemented: devices={}, title={}", devices, title);
        }
        IpcMessage::NotifyBig { devices, message, duration_ms, color } => {
            warn!("Notify big not yet implemented: devices={}, message={}", devices, message);
        }
        
        // Legacy commands (for compatibility)
        IpcMessage::SwitchToProject { project_name } => {
            warn!("Switch to project not yet implemented: {}", project_name);
        }
        IpcMessage::SeekToSection { project_name, song_name, section_name } => {
            warn!("Seek to section not yet implemented: project={}, song={}, section={}", project_name, song_name, section_name);
        }
        
        // Ping/Pong - handled separately
        IpcMessage::Ping => {
            // Ping is handled by connection monitoring
        }
        IpcMessage::Pong => {
            // Pong is handled by connection monitoring
        }
        
        // State updates - these shouldn't be received as commands
        IpcMessage::Log { .. } |
        IpcMessage::GlobalStateUpdate { .. } |
        IpcMessage::SetlistStateUpdate { .. } |
        IpcMessage::SetlistUpdate { .. } |
        IpcMessage::SetlistUpdateBinary { .. } |
        IpcMessage::ReaperTransportUpdate { .. } |
        IpcMessage::Custom { .. } => {
            warn!("Received state update message as command - ignoring: {:?}", std::mem::discriminant(&message));
        }
    }
}
*/

/// Placeholder - will be replaced with irpc handler
pub fn handle_ipc_command(_message: ()) {
    // TODO: Implement using irpc ReaperProtocol
}

