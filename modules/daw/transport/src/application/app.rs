use crate::{
    core::transport::{PlayState, Transport},
    core::{RecordMode, Tempo, TransportActions, TransportError},
};
use primitives::{Position, TimeSignature};

fn transition_to(transport: &mut Transport, next: PlayState) -> Result<(), TransportError> {
    let current = transport.play_state;
    if current == next {
        return Ok(());
    }

    if can_transition(current, next) {
        transport.play_state = next;
        Ok(())
    } else {
        Err(TransportError::InvalidState(format!(
            "cannot transition from {:?} to {:?}",
            current, next
        )))
    }
}

fn can_transition(current: PlayState, next: PlayState) -> bool {
    use PlayState::*;

    match (current, next) {
        (Stopped, Playing) | (Stopped, Recording) => true,
        (Playing, Paused) | (Playing, Recording) | (Playing, Stopped) => true,
        (Paused, Playing) | (Paused, Stopped) => true,
        (Recording, Playing) | (Recording, Paused) | (Recording, Stopped) => true,
        _ => false,
    }
}

fn set_tempo_inner(transport: &mut Transport, tempo: Tempo) -> Result<(), TransportError> {
    if !tempo.is_valid() {
        return Err(TransportError::InvalidTempo(format!("{} BPM", tempo.bpm)));
    }

    transport.tempo = tempo;
    Ok(())
}

fn validate_time_signature(time_signature: &TimeSignature) -> Result<(), TransportError> {
    if time_signature.numerator <= 0 {
        return Err(TransportError::InvalidTimeSignature(
            "numerator must be positive".to_string(),
        ));
    }

    if time_signature.denominator <= 0 {
        return Err(TransportError::InvalidTimeSignature(
            "denominator must be positive".to_string(),
        ));
    }

    let denom = time_signature.denominator as u32;
    if denom.count_ones() != 1 {
        return Err(TransportError::InvalidTimeSignature(format!(
            "denominator must be a power of two, got {}",
            time_signature.denominator
        )));
    }

    Ok(())
}

impl TransportActions for Transport {
    fn play(&mut self) -> Result<String, TransportError> {
        if matches!(self.play_state, PlayState::Playing | PlayState::Recording) {
            return Ok("Playback already running".to_string());
        }

        transition_to(self, PlayState::Playing)?;
        Ok("Playback started".to_string())
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        match self.play_state {
            PlayState::Playing | PlayState::Recording => {
                transition_to(self, PlayState::Paused)?;
                Ok("Playback paused".to_string())
            }
            PlayState::Paused => Ok("Playback already paused".to_string()),
            PlayState::Stopped => Err(TransportError::InvalidState(
                "cannot pause while stopped".to_string(),
            )),
        }
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        self.reset();
        Ok("Playback stopped".to_string())
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        if self.is_playing() {
            self.pause()
        } else {
            self.play()
        }
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        if self.is_playing() {
            self.stop()
        } else {
            self.play()
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        if self.is_recording() {
            return Ok("Recording already active".to_string());
        }

        transition_to(self, PlayState::Recording)?;
        Ok("Recording started".to_string())
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        if !self.is_recording() {
            return Err(TransportError::InvalidState(
                "cannot stop recording while not recording".to_string(),
            ));
        }

        transition_to(self, PlayState::Playing)?;
        Ok("Recording stopped".to_string())
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        if self.is_recording() {
            self.stop_recording()
        } else {
            self.start_recording()
        }
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        set_tempo_inner(self, tempo)?;
        Ok(format!("Tempo set to {} BPM", tempo.bpm))
    }

    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, TransportError> {
        validate_time_signature(&time_signature)?;
        self.time_signature = time_signature;
        Ok(format!(
            "Time signature set to {}/{}",
            time_signature.numerator, time_signature.denominator
        ))
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        self.record_mode = record_mode;
        Ok(format!("Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        if !seconds.is_finite() || seconds < 0.0 {
            return Err(TransportError::InvalidPosition(format!(
                "invalid seconds value: {seconds}"
            )));
        }

        let position = Position::from_seconds(seconds);
        self.playhead_position = position.clone();
        self.edit_position = position;

        Ok(format!("Position set to {:.3} seconds", seconds))
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        Ok(self.tempo)
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        Ok(self.time_signature)
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        Ok(self.record_mode)
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        Ok(self.playhead_position.time.to_seconds())
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        Ok(self.is_playing())
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        Ok(self.is_recording())
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        Ok(self.clone())
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_transitions_through_common_states() {
        let mut transport = Transport::new();

        assert_eq!(transport.play_state, PlayState::Stopped);
        transport.play().unwrap();
        assert_eq!(transport.play_state, PlayState::Playing);
        transport.pause().unwrap();
        assert_eq!(transport.play_state, PlayState::Paused);
        transport.play().unwrap();
        assert_eq!(transport.play_state, PlayState::Playing);
        transport.start_recording().unwrap();
        assert_eq!(transport.play_state, PlayState::Recording);
        transport.stop_recording().unwrap();
        assert_eq!(transport.play_state, PlayState::Playing);
        transport.stop().unwrap();
        assert_eq!(transport.play_state, PlayState::Stopped);
    }

    #[test]
    fn invalid_transitions_error() {
        let mut transport = Transport::new();

        let pause_err = transport.pause().unwrap_err();
        match pause_err {
            TransportError::InvalidState(_) => {}
            other => panic!("expected invalid state error, got {other:?}"),
        }
    }

    #[test]
    fn validates_time_signature_and_position() {
        let mut transport = Transport::new();

        assert!(
            transport
                .set_time_signature(TimeSignature {
                    numerator: 7,
                    denominator: 8
                })
                .is_ok()
        );

        let invalid_sig = transport.set_time_signature(TimeSignature {
            numerator: 0,
            denominator: 3,
        });
        assert!(matches!(
            invalid_sig,
            Err(TransportError::InvalidTimeSignature(_))
        ));

        let invalid_position = transport.set_position(-1.0);
        assert!(matches!(
            invalid_position,
            Err(TransportError::InvalidPosition(_))
        ));
    }
}
