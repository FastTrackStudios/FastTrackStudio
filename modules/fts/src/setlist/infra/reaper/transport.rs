//! REAPER Transport Adapter
//!
//! Implements TransportActions for REAPER projects using reaper-high API.

use reaper_high::{Project, Reaper};
use reaper_medium::{CommandId, PositionInSeconds, ProjectContext, SetEditCurPosOptions};
use daw::transport::{
    PlayState, RecordMode, Tempo, Transport, TransportActions, TransportError,
};
use daw::primitives::{Position, TimePosition, MusicalPosition, TimeSignature};

/// REAPER transport adapter that implements TransportActions
/// 
/// This wraps a REAPER Project and provides transport control via the REAPER API.
/// **IMPORTANT**: This MUST be used from the main thread only!
pub struct ReaperTransport {
    project: Project,
}

impl ReaperTransport {
    /// Create a new REAPER transport adapter for a specific project
    pub fn new(project: Project) -> Self {
        Self { project }
    }

    /// Get the underlying REAPER project
    pub fn project(&self) -> &Project {
        &self.project
    }

    /// Get the project measure offset for this project
    /// Uses the high-level API for safe access
    fn get_project_measure_offset(&self) -> i32 {
        self.project.measure_offset()
    }

    /// Convert REAPER play state to our PlayState
    fn play_state_to_transport(&self) -> PlayState {
        let play_state = self.project.play_state();
        if play_state.is_recording {
            PlayState::Recording
        } else if play_state.is_playing {
            PlayState::Playing
        } else if play_state.is_paused {
            PlayState::Paused
        } else {
            PlayState::Stopped
        }
    }

    /// Get tempo for a specific project at a specific time position
    /// Uses the high-level TimeMap API which automatically handles tempo markers
    fn get_project_tempo(
        &self,
        medium_reaper: &reaper_medium::Reaper,
        project_context: ProjectContext,
        play_position: PositionInSeconds,
    ) -> f64 {
        // Use TimeMap API to get tempo at specific position
        // This automatically accounts for tempo markers and is much more efficient
        // than manually iterating through markers
        medium_reaper
            .time_map_2_get_divided_bpm_at_time(project_context, play_position)
            .get()
    }

    /// Read current transport state from REAPER
    pub fn read_transport(&self) -> Result<Transport, TransportError> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = self.project.context();
        let play_state = self.play_state_to_transport();
        
        // Get edit cursor position
        let edit_position_seconds = self
            .project
            .edit_cursor_position()
            .unwrap_or(PositionInSeconds::ZERO)
            .get();
        
        // When not playing, use edit cursor position (play cursor follows edit cursor when stopped)
        // When playing, use play position (latency-compensated)
        let is_playing = matches!(play_state, PlayState::Playing | PlayState::Recording);
        let play_position_seconds = if is_playing {
            self.project.play_position_latency_compensated().get()
        } else {
            edit_position_seconds
        };
        
        let play_position_pos = PositionInSeconds::new(play_position_seconds)
            .map_err(|e| TransportError::NotReady(format!("Invalid play position: {:?}", e)))?;
        
        // Get tempo for this specific project at the current position
        // Uses TimeMap API which automatically handles tempo markers
        let tempo_bpm = self.get_project_tempo(&medium_reaper, project_context, play_position_pos);
        let tempo = Tempo::new(tempo_bpm);
        
        // Get time signature from beat info (REAPER provides this per-project)
        let play_beat_info = self.project.beat_info_at(play_position_pos);
        let time_sig = TimeSignature::new(
            play_beat_info.time_signature.numerator.get() as i32,
            play_beat_info.time_signature.denominator.get() as i32,
        );
        
        // Convert play position to musical position using REAPER's beat info
        // beat_info_at uses TimeMap2_timeToBeats internally, which is the correct API
        let measure_offset = self.get_project_measure_offset();
        let play_measure = play_beat_info.measure_index + measure_offset; // Apply project measure offset
        let play_beats_since_measure = play_beat_info.beats_since_measure.get();
        let play_beat = play_beats_since_measure.floor() as i32;
        let play_subdivision = ((play_beats_since_measure - play_beats_since_measure.floor()) * 1000.0).round() as i32;
        let play_subdivision = play_subdivision.max(0).min(999);
        
        let play_musical = MusicalPosition::try_new(play_measure, play_beat, play_subdivision)
            .map_err(|e| TransportError::NotReady(format!("Invalid musical position: {}", e)))?;
        let play_time = TimePosition::from_seconds(play_position_seconds);
        let playhead_position = Position::new(play_musical, play_time);
        
        // Convert edit position to musical position
        let edit_position_pos = PositionInSeconds::new(edit_position_seconds)
            .map_err(|e| TransportError::NotReady(format!("Invalid edit position: {:?}", e)))?;
        let edit_beat_info = self.project.beat_info_at(edit_position_pos);
        let edit_measure = edit_beat_info.measure_index + measure_offset; // Apply project measure offset
        let edit_beats_since_measure = edit_beat_info.beats_since_measure.get();
        let edit_beat = edit_beats_since_measure.floor() as i32;
        let edit_subdivision = ((edit_beats_since_measure - edit_beats_since_measure.floor()) * 1000.0).round() as i32;
        let edit_subdivision = edit_subdivision.max(0).min(999);
        
        let edit_musical = MusicalPosition::try_new(edit_measure, edit_beat, edit_subdivision)
            .map_err(|e| TransportError::NotReady(format!("Invalid musical position: {}", e)))?;
        let edit_time = TimePosition::from_seconds(edit_position_seconds);
        let edit_position = Position::new(edit_musical, edit_time);
        
        // Get playrate
        let playrate_obj = self.project.play_rate();
        let playrate = playrate_obj.playback_speed_factor().get();
        
        // Check if looping - REAPER doesn't expose this directly, use default false for now
        // TODO: Find correct API to check loop state
        let looping = false;
        
        Ok(Transport {
            play_state,
            record_mode: RecordMode::default(), // REAPER doesn't expose this directly
            looping,
            tempo,
            playrate,
            time_signature: time_sig,
            playhead_position,
            edit_position,
            time_selection: None, // TODO: Get from REAPER
            loop_selection: None, // TODO: Get from REAPER
            input_monitoring: false, // TODO: Get from REAPER
            preroll: 0.0, // TODO: Get from REAPER
            postroll: 0.0, // TODO: Get from REAPER
        })
    }
}

impl TransportActions for ReaperTransport {
    fn play(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let cmd_id = CommandId::new(1007); // Play
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Play".to_string())
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let cmd_id = CommandId::new(1008); // Pause
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Pause".to_string())
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let cmd_id = CommandId::new(1016); // Stop
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Stop".to_string())
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let cmd_id = CommandId::new(1007); // Play (toggles play/pause)
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Play/Pause".to_string())
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        if self.is_playing()? {
            self.stop()
        } else {
            self.play()
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let play_state = self.project.play_state();
        
        if !play_state.is_playing {
            // Start playing first
            let play_cmd_id = CommandId::new(1007); // Play
            reaper.medium_reaper().main_on_command_ex(play_cmd_id, 0, ProjectContext::CurrentProject);
        }
        
        let cmd_id = CommandId::new(1013); // Record
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Start Recording".to_string())
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        let reaper = Reaper::get();
        let cmd_id = CommandId::new(1013); // Record (toggles)
        reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        Ok("Stop Recording".to_string())
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        self.start_recording()
    }

    fn set_tempo(&mut self, _tempo: Tempo) -> Result<String, TransportError> {
        // REAPER doesn't have a direct API to set tempo, we'd need to use actions
        // For now, return an error indicating this isn't supported
        Err(TransportError::NotReady(
            "Setting tempo via API is not directly supported".to_string(),
        ))
    }

    fn set_time_signature(
        &mut self,
        _time_signature: TimeSignature,
    ) -> Result<String, TransportError> {
        // REAPER doesn't have a direct API to set time signature
        Err(TransportError::NotReady(
            "Setting time signature via API is not directly supported".to_string(),
        ))
    }

    fn set_record_mode(&mut self, _record_mode: RecordMode) -> Result<String, TransportError> {
        // REAPER doesn't expose record mode directly
        Err(TransportError::NotReady(
            "Setting record mode via API is not directly supported".to_string(),
        ))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        let position = PositionInSeconds::new(seconds)
            .map_err(|e| TransportError::NotReady(format!("Invalid position: {:?}", e)))?;
        self.project.set_edit_cursor_position(
            position,
            SetEditCurPosOptions {
                move_view: false,
                seek_play: false,
            },
        );
        Ok(format!("Set position to {:.2}s", seconds))
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        let tempo = self.project.tempo();
        let bpm = tempo.bpm().get();
        Ok(Tempo::new(bpm))
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        // REAPER doesn't expose time signature directly per-project
        // Return default for now
        Ok(TimeSignature::default())
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        // REAPER doesn't expose record mode directly
        Ok(RecordMode::default())
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        let position = self.project.play_position_latency_compensated();
        Ok(position.get())
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        let play_state = self.project.play_state();
        Ok(play_state.is_playing)
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        let play_state = self.project.play_state();
        Ok(play_state.is_recording)
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        self.read_transport()
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        // REAPER project is always ready if we have a reference to it
        Ok(true)
    }
}
