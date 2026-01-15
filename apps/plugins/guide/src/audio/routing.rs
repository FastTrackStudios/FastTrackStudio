//! Multi-channel audio routing
//!
//! Routes audio samples to the appropriate output channels (stereo pairs).

use symphonium::DecodedAudioF32;

/// Audio channel routing configuration
pub struct AudioRouter;

impl AudioRouter {
    /// Route a single channel sample based on channel index
    #[allow(clippy::too_many_arguments)]
    pub fn route_single_channel(
        channel_idx: usize,
        output_sample: &mut f32,
        click_left: f32,
        click_right: f32,
        shaker_left: f32,
        shaker_right: f32,
        count_left: f32,
        count_right: f32,
        guide_left: f32,
        guide_right: f32,
    ) {
        match channel_idx {
            0 => {
                // Click channel 1 (left)
                *output_sample = click_left;
            }
            1 => {
                // Click channel 2 (right)
                *output_sample = click_right;
            }
            2 => {
                // Shaker channel 3 (left) - empty for now
                *output_sample = shaker_left;
            }
            3 => {
                // Shaker channel 4 (right) - empty for now
                *output_sample = shaker_right;
            }
            4 => {
                // Count channel 5 (left)
                *output_sample = count_left;
            }
            5 => {
                // Count channel 6 (right)
                *output_sample = count_right;
            }
            6 => {
                // Guide channel 7 (left)
                *output_sample = guide_left;
            }
            7 => {
                // Guide channel 8 (right)
                *output_sample = guide_right;
            }
            _ => {
                // Additional channels (shouldn't happen with 8 channels)
                *output_sample = 0.0;
            }
        }
    }

    /// Mix a mono sample to stereo (duplicates to both channels)
    pub fn mix_mono_to_stereo(sample: f32, gain: f32, left: &mut f32, right: &mut f32) {
        *left += sample * gain;
        *right += sample * gain;
    }

    /// Mix a stereo sample (routes channel 0 to left, channel 1 to right)
    pub fn mix_stereo_to_stereo(
        sample_left: f32,
        sample_right: f32,
        gain: f32,
        left: &mut f32,
        right: &mut f32,
    ) {
        *left += sample_left * gain;
        *right += sample_right * gain;
    }

    /// Mix decoded audio (auto-detects mono vs stereo)
    pub fn mix_decoded_audio(
        decoded_audio: &DecodedAudioF32,
        position: usize,
        gain: f32,
        left: &mut f32,
        right: &mut f32,
    ) {
        if decoded_audio.data.len() == 1 {
            // Mono source: duplicate to both channels
            let sample_val = decoded_audio.data[0][position];
            Self::mix_mono_to_stereo(sample_val, gain, left, right);
        } else {
            // Stereo source: route channel 0 to left, channel 1 to right
            let left_val = decoded_audio.data[0][position];
            let right_val = if decoded_audio.data.len() > 1 {
                decoded_audio.data[1][position]
            } else {
                0.0
            };
            Self::mix_stereo_to_stereo(left_val, right_val, gain, left, right);
        }
    }
}
