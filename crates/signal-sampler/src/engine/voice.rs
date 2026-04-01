//! Voice — a single playing sample with pitch shifting and amplitude envelope.
//!
//! Pitch shifting is implemented as a playback rate change with linear
//! interpolation between frames. A pitch shift of N semitones multiplies
//! the playback rate by `2^(N/12)`.
//!
//! The amplitude envelope is a simple two-stage model:
//! - Playing: flat at `gain`
//! - Releasing: linear fade to zero over `release_frames`
//!
//! CSS sustain samples have their own natural releases baked in; we do not
//! apply an envelope to them. Release samples are played to completion at
//! reduced gain.

use std::sync::Arc;

use super::cache::SampleData;

// ── Voice state ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoiceState {
    /// Sample is playing normally.
    Playing,
    /// Note-off received — fading out.
    Releasing { frames_remaining: usize },
    /// Playback finished — ready for reuse.
    Done,
}

/// Classification of what triggered this voice.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoiceKind {
    /// Non-vibrato sustain, lower CC1 dynamic layer.
    SustainNVLo,
    /// Non-vibrato sustain, upper CC1 dynamic layer.
    SustainNVHi,
    /// Vibrato sustain, lower CC1 dynamic layer (CC2 crossfade pair).
    SustainVibLo,
    /// Vibrato sustain, upper CC1 dynamic layer (CC2 crossfade pair).
    SustainVibHi,
    /// Primary sustain layer A (lower dynamic).
    SustainLo,
    /// Primary sustain layer B (upper dynamic, for CC1 crossfade).
    SustainHi,
    /// Legato transition sample.
    Legato,
    /// Release trail (triggered on note-off).
    Release,
    /// Short note (one-shot — plays to completion regardless of note-off).
    Short,
}

// ── Voice ─────────────────────────────────────────────────────────────────────

/// One active sample playback.
pub struct Voice {
    /// The decoded sample data (shared via Arc).
    pub data: Arc<SampleData>,

    /// Current playback position in frames (fractional for pitch shifting).
    position: f64,

    /// Playback rate. 1.0 = original pitch, 2^(semitones/12) for transposition.
    rate: f64,

    /// Output gain [0.0, 1.0].
    pub gain: f32,

    /// Target gain for CC1 crossfade blend (updated per render block).
    pub target_gain: f32,

    /// Gain smoothing — frames to ramp from `gain` to `target_gain`.
    gain_ramp_frames: usize,

    pub state: VoiceState,
    pub kind: VoiceKind,

    /// MIDI note this voice belongs to (for note-off matching).
    pub note: u8,

    /// Release fade duration in frames. Used when state transitions to Releasing.
    release_frames: usize,
}

impl Voice {
    /// Create a new voice.
    ///
    /// - `semitone_offset`: how many semitones to shift the sample pitch.
    ///   Positive = up, negative = down.
    /// - `gain`: initial output gain (0.0–1.0).
    /// - `release_frames`: fade-out length when note-off arrives.
    pub fn new(
        data: Arc<SampleData>,
        note: u8,
        kind: VoiceKind,
        semitone_offset: i8,
        gain: f32,
        release_frames: usize,
    ) -> Self {
        let rate = 2.0f64.powf(semitone_offset as f64 / 12.0);
        Self {
            data,
            position: 0.0,
            rate,
            gain,
            target_gain: gain,
            gain_ramp_frames: 0,
            state: VoiceState::Playing,
            kind,
            note,
            release_frames,
        }
    }

    /// Schedule a gain ramp to `target` over `frames` frames.
    pub fn ramp_gain(&mut self, target: f32, frames: usize) {
        self.target_gain = target;
        self.gain_ramp_frames = frames;
    }

    /// Trigger note-off. Short notes and release samples play to completion.
    pub fn note_off(&mut self) {
        match self.kind {
            VoiceKind::Short | VoiceKind::Release => {
                // Play to end — do not release early.
            }
            _ => {
                if self.state == VoiceState::Playing {
                    self.state = VoiceState::Releasing {
                        frames_remaining: self.release_frames,
                    };
                }
            }
        }
    }

    /// Returns true when this voice should be removed from the pool.
    pub fn is_done(&self) -> bool {
        self.state == VoiceState::Done
    }

    /// Render one stereo frame. Returns (L, R) and advances internal state.
    #[inline]
    pub fn next_frame(&mut self) -> (f32, f32) {
        if self.state == VoiceState::Done {
            return (0.0, 0.0);
        }

        // Gain smoothing
        if self.gain_ramp_frames > 0 {
            self.gain += (self.target_gain - self.gain) / self.gain_ramp_frames as f32;
            self.gain_ramp_frames -= 1;
        } else {
            self.gain = self.target_gain;
        }

        // Envelope
        let env = match &mut self.state {
            VoiceState::Playing => 1.0f32,
            VoiceState::Releasing { frames_remaining } => {
                if *frames_remaining == 0 {
                    self.state = VoiceState::Done;
                    return (0.0, 0.0);
                }
                let t = *frames_remaining as f32 / self.release_frames.max(1) as f32;
                *frames_remaining -= 1;
                t
            }
            VoiceState::Done => return (0.0, 0.0),
        };

        // Read sample with linear interpolation
        let frame_idx = self.position as usize;
        if frame_idx >= self.data.num_frames {
            self.state = VoiceState::Done;
            return (0.0, 0.0);
        }

        let frac = (self.position - frame_idx as f64) as f32;
        let (l0, r0) = self.data.frame(frame_idx);
        let (l1, r1) = self.data.frame((frame_idx + 1).min(self.data.num_frames - 1));

        let l = l0 + (l1 - l0) * frac;
        let r = r0 + (r1 - r0) * frac;

        let amp = self.gain * env;

        // Advance position
        self.position += self.rate;

        (l * amp, r * amp)
    }

    /// Render a block of stereo frames into `output` (interleaved L/R).
    /// Returns the number of frames rendered (may be less if sample ends).
    pub fn render_block(&mut self, output: &mut [f32]) -> usize {
        let num_frames = output.len() / 2;
        let mut rendered = 0;
        for i in 0..num_frames {
            let (l, r) = self.next_frame();
            output[i * 2] += l;
            output[i * 2 + 1] += r;
            rendered += 1;
            if self.is_done() { break; }
        }
        rendered
    }
}

// ── Voice pool ────────────────────────────────────────────────────────────────

/// Maximum simultaneous voices before stealing.
const MAX_VOICES: usize = 64;

/// Pool of active voices with simple stealing policy.
pub struct VoicePool {
    voices: Vec<Voice>,
}

impl VoicePool {
    pub fn new() -> Self {
        Self { voices: Vec::with_capacity(MAX_VOICES) }
    }

    /// Add a voice, stealing the oldest if at capacity.
    pub fn spawn(&mut self, voice: Voice) {
        // Remove done voices first
        self.voices.retain(|v| !v.is_done());

        if self.voices.len() >= MAX_VOICES {
            // Steal: silence the oldest non-release voice
            if let Some(idx) = self.voices.iter().position(|v| v.kind != VoiceKind::Release) {
                self.voices.remove(idx);
            } else {
                self.voices.remove(0);
            }
        }
        self.voices.push(voice);
    }

    /// Send note-off to all voices playing `note` (except one-shot kinds).
    pub fn note_off(&mut self, note: u8) {
        for v in &mut self.voices {
            if v.note == note {
                v.note_off();
            }
        }
    }

    /// Silence all voices for `note` immediately (used when legato transition fires).
    pub fn silence_note(&mut self, note: u8, fade_frames: usize) {
        for v in &mut self.voices {
            if v.note == note
                && matches!(
                    v.kind,
                    VoiceKind::SustainNVLo
                        | VoiceKind::SustainNVHi
                        | VoiceKind::SustainVibLo
                        | VoiceKind::SustainVibHi
                        | VoiceKind::SustainLo
                        | VoiceKind::SustainHi
                )
            {
                v.ramp_gain(0.0, fade_frames);
                v.state = VoiceState::Releasing { frames_remaining: fade_frames };
            }
        }
    }

    /// Render all active voices into an interleaved stereo buffer.
    pub fn render(&mut self, output: &mut [f32]) {
        for v in &mut self.voices {
            v.render_block(output);
        }
        self.voices.retain(|v| !v.is_done());
    }

    pub fn active_count(&self) -> usize { self.voices.len() }

    /// Mutable iterator over all active voices (used by engine for CC1 updates).
    pub fn voices_mut(&mut self) -> &mut Vec<Voice> {
        &mut self.voices
    }
}
