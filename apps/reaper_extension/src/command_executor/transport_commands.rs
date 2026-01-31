//! Transport Commands
//!
//! REAPER commands for transport control (play, stop, pause, etc.)

use crate::command_executor::{define_command, define_command_with_fields, ReaperCommand};
use reaper_high::Reaper;
use reaper_medium::{CommandId, PositionInSeconds, ProjectContext, SetEditCurPosOptions};

// ============================================================
// Playback Control Commands
// ============================================================

define_command! {
    /// Play from current position
    pub struct PlayCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(1007), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Stop playback
    pub struct StopCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Pause playback
    pub struct PauseCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(1008), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Toggle play/pause
    pub struct TogglePlayPauseCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(40073), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Toggle play/stop
    pub struct TogglePlayStopCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(40044), 0, ProjectContext::CurrentProject);
    }
}

// ============================================================
// Recording Commands
// ============================================================

define_command! {
    /// Start recording
    pub struct RecordCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(1013), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Stop recording (only if currently recording)
    pub struct StopRecordingCommand;
    execute(reaper) {
        let state = reaper.medium_reaper()
            .get_play_state_ex(ProjectContext::CurrentProject);
        if state.is_recording {
            reaper.medium_reaper()
                .main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
        }
    }
}

// ============================================================
// Position Commands
// ============================================================

define_command_with_fields! {
    /// Set play/edit cursor position
    pub struct SetPositionCommand {
        pub position_seconds: f64,
        pub move_view: bool,
        pub seek_play: bool,
    }
    execute(reaper, self) {
        if let Ok(pos) = PositionInSeconds::new(self.position_seconds) {
            let options = SetEditCurPosOptions {
                move_view: self.move_view,
                seek_play: self.seek_play,
            };
            reaper.current_project()
                .set_edit_cursor_position(pos, options);
        }
    }
}

define_command! {
    /// Go to start of project
    pub struct GoToStartCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(40042), 0, ProjectContext::CurrentProject);
    }
}

define_command! {
    /// Go to end of project
    pub struct GoToEndCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(40043), 0, ProjectContext::CurrentProject);
    }
}

// ============================================================
// Loop Commands
// ============================================================

define_command! {
    /// Toggle loop/repeat mode
    pub struct ToggleLoopCommand;
    execute(reaper) {
        reaper.medium_reaper()
            .main_on_command_ex(CommandId::new(1068), 0, ProjectContext::CurrentProject);
    }
}

// ============================================================
// Convenient re-exports
// ============================================================

pub use GoToEndCommand as GoToEnd;
pub use GoToStartCommand as GoToStart;
pub use PauseCommand as Pause;
pub use PlayCommand as Play;
pub use RecordCommand as Record;
pub use SetPositionCommand as SetPosition;
pub use StopCommand as Stop;
pub use StopRecordingCommand as StopRecording;
pub use ToggleLoopCommand as ToggleLoop;
pub use TogglePlayPauseCommand as TogglePlayPause;
pub use TogglePlayStopCommand as TogglePlayStop;
