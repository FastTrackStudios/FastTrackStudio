//! Guide sample playback
//!
//! Handles playback of guide samples for section announcements.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;

use super::routing::AudioRouter;

/// Guide sample player state
pub struct GuidePlayerState {
    pub playback_position_guide: usize,
    pub is_playing_guide: bool,
    pub guide_has_triggered: bool,
}

impl GuidePlayerState {
    pub fn new() -> Self {
        Self {
            playback_position_guide: 0,
            is_playing_guide: false,
            guide_has_triggered: false,
        }
    }

    /// Reset playback state
    pub fn reset(&mut self) {
        self.playback_position_guide = 0;
        self.is_playing_guide = false;
        self.guide_has_triggered = false;
    }
}

impl Default for GuidePlayerState {
    fn default() -> Self {
        Self::new()
    }
}

/// Guide sample player
pub struct GuidePlayer;

impl GuidePlayer {
    /// Play guide sample and mix to output
    ///
    /// Returns true if guide is still playing, false if it finished
    pub fn play(
        state: &mut GuidePlayerState,
        current_guide_key: &Arc<Mutex<Option<String>>>,
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
        gain: f32,
        guide_left: &mut f32,
        guide_right: &mut f32,
    ) -> bool {
        if !state.is_playing_guide {
            return false;
        }

        let current_guide_key = current_guide_key.lock().unwrap();
        if let Some(ref guide_key) = *current_guide_key {
            let guide_samples = guide_samples.lock().unwrap();
            if let Some(guide_audio) = guide_samples.get(guide_key) {
                if state.playback_position_guide < guide_audio.frames() {
                    AudioRouter::mix_decoded_audio(
                        guide_audio,
                        state.playback_position_guide,
                        gain,
                        guide_left,
                        guide_right,
                    );
                    state.playback_position_guide += 1;

                    if state.playback_position_guide >= guide_audio.frames() {
                        // Finished playing
                        state.is_playing_guide = false;
                        state.playback_position_guide = 0;
                        return false;
                    }
                    return true;
                }
                // Past end of sample
                state.is_playing_guide = false;
                state.playback_position_guide = 0;
                return false;
            }
        }

        // No guide key or sample not found
        state.is_playing_guide = false;
        state.playback_position_guide = 0;
        false
    }

    /// Stop guide playback
    pub fn stop(state: &mut GuidePlayerState, current_guide_key: &Arc<Mutex<Option<String>>>) {
        state.is_playing_guide = false;
        state.playback_position_guide = 0;
        let mut current_guide_key = current_guide_key.lock().unwrap();
        *current_guide_key = None;
    }
}
