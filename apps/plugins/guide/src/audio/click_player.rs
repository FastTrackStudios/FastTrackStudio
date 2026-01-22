//! Click sample playback
//!
//! Handles playback of click samples: beat, eighth, sixteenth, triplet, and measure accent.

use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;

use super::routing::AudioRouter;

/// Click sample player state
pub struct ClickPlayerState {
    pub playback_position_beat: usize,
    pub playback_position_eighth: usize,
    pub playback_position_sixteenth: usize,
    pub playback_position_triplet: usize,
    pub playback_position_measure_accent: usize,
    pub is_playing_beat: bool,
    pub is_playing_eighth: bool,
    pub is_playing_sixteenth: bool,
    pub is_playing_triplet: bool,
    pub is_playing_measure_accent: bool,
}

impl ClickPlayerState {
    pub fn new() -> Self {
        Self {
            playback_position_beat: 0,
            playback_position_eighth: 0,
            playback_position_sixteenth: 0,
            playback_position_triplet: 0,
            playback_position_measure_accent: 0,
            is_playing_beat: false,
            is_playing_eighth: false,
            is_playing_sixteenth: false,
            is_playing_triplet: false,
            is_playing_measure_accent: false,
        }
    }

    /// Reset all playback states
    pub fn reset(&mut self) {
        self.playback_position_beat = 0;
        self.playback_position_eighth = 0;
        self.playback_position_sixteenth = 0;
        self.playback_position_triplet = 0;
        self.playback_position_measure_accent = 0;
        self.is_playing_beat = false;
        self.is_playing_eighth = false;
        self.is_playing_sixteenth = false;
        self.is_playing_triplet = false;
        self.is_playing_measure_accent = false;
    }
}

impl Default for ClickPlayerState {
    fn default() -> Self {
        Self::new()
    }
}

/// Click sample player
pub struct ClickPlayer;

impl ClickPlayer {
    /// Play beat sample and mix to output
    pub fn play_beat(
        state: &mut ClickPlayerState,
        sample_data: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        if !state.is_playing_beat {
            return;
        }

        let sample_data = sample_data.lock().unwrap();
        if let Some(ref decoded_audio) = *sample_data {
            if state.playback_position_beat < decoded_audio.frames() {
                AudioRouter::mix_decoded_audio(
                    decoded_audio,
                    state.playback_position_beat,
                    gain,
                    click_left,
                    click_right,
                );
                state.playback_position_beat += 1;

                if state.playback_position_beat >= decoded_audio.frames() {
                    state.is_playing_beat = false;
                    state.playback_position_beat = 0;
                }
            } else {
                state.is_playing_beat = false;
                state.playback_position_beat = 0;
            }
        }
    }

    /// Play eighth sample and mix to output
    pub fn play_eighth(
        state: &mut ClickPlayerState,
        sample_data: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        if !state.is_playing_eighth {
            return;
        }

        let sample_data = sample_data.lock().unwrap();
        if let Some(ref decoded_audio) = *sample_data {
            if state.playback_position_eighth < decoded_audio.frames() {
                AudioRouter::mix_decoded_audio(
                    decoded_audio,
                    state.playback_position_eighth,
                    gain,
                    click_left,
                    click_right,
                );
                state.playback_position_eighth += 1;

                if state.playback_position_eighth >= decoded_audio.frames() {
                    state.is_playing_eighth = false;
                    state.playback_position_eighth = 0;
                }
            } else {
                state.is_playing_eighth = false;
                state.playback_position_eighth = 0;
            }
        }
    }

    /// Play sixteenth sample and mix to output
    pub fn play_sixteenth(
        state: &mut ClickPlayerState,
        sample_data: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        if !state.is_playing_sixteenth {
            return;
        }

        let sample_data = sample_data.lock().unwrap();
        if let Some(ref decoded_audio) = *sample_data {
            if state.playback_position_sixteenth < decoded_audio.frames() {
                AudioRouter::mix_decoded_audio(
                    decoded_audio,
                    state.playback_position_sixteenth,
                    gain,
                    click_left,
                    click_right,
                );
                state.playback_position_sixteenth += 1;

                if state.playback_position_sixteenth >= decoded_audio.frames() {
                    state.is_playing_sixteenth = false;
                    state.playback_position_sixteenth = 0;
                }
            } else {
                state.is_playing_sixteenth = false;
                state.playback_position_sixteenth = 0;
            }
        }
    }

    /// Play triplet sample and mix to output
    pub fn play_triplet(
        state: &mut ClickPlayerState,
        sample_data: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        if !state.is_playing_triplet {
            return;
        }

        let sample_data = sample_data.lock().unwrap();
        if let Some(ref decoded_audio) = *sample_data {
            if state.playback_position_triplet < decoded_audio.frames() {
                AudioRouter::mix_decoded_audio(
                    decoded_audio,
                    state.playback_position_triplet,
                    gain,
                    click_left,
                    click_right,
                );
                state.playback_position_triplet += 1;

                if state.playback_position_triplet >= decoded_audio.frames() {
                    state.is_playing_triplet = false;
                    state.playback_position_triplet = 0;
                }
            } else {
                state.is_playing_triplet = false;
                state.playback_position_triplet = 0;
            }
        }
    }

    /// Play measure accent sample and mix to output
    pub fn play_measure_accent(
        state: &mut ClickPlayerState,
        sample_data: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        if !state.is_playing_measure_accent {
            return;
        }

        let sample_data = sample_data.lock().unwrap();
        if let Some(ref decoded_audio) = *sample_data {
            if state.playback_position_measure_accent < decoded_audio.frames() {
                AudioRouter::mix_decoded_audio(
                    decoded_audio,
                    state.playback_position_measure_accent,
                    gain,
                    click_left,
                    click_right,
                );
                state.playback_position_measure_accent += 1;

                if state.playback_position_measure_accent >= decoded_audio.frames() {
                    state.is_playing_measure_accent = false;
                    state.playback_position_measure_accent = 0;
                }
            } else {
                state.is_playing_measure_accent = false;
                state.playback_position_measure_accent = 0;
            }
        }
    }

    /// Play all active click samples and mix to output
    #[allow(clippy::too_many_arguments)]
    pub fn play_all(
        state: &mut ClickPlayerState,
        sample_data_beat: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_eighth: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_sixteenth: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_triplet: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_measure_accent: &Arc<Mutex<Option<DecodedAudioF32>>>,
        gain: f32,
        click_left: &mut f32,
        click_right: &mut f32,
    ) {
        Self::play_beat(state, sample_data_beat, gain, click_left, click_right);
        Self::play_eighth(state, sample_data_eighth, gain, click_left, click_right);
        Self::play_sixteenth(state, sample_data_sixteenth, gain, click_left, click_right);
        Self::play_triplet(state, sample_data_triplet, gain, click_left, click_right);
        Self::play_measure_accent(
            state,
            sample_data_measure_accent,
            gain,
            click_left,
            click_right,
        );
    }
}
