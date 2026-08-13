//! What to sample, how hard, for how long, and where the audio comes back.

use std::path::PathBuf;

/// One complete auto-sampling job.
#[derive(Debug, Clone)]
pub struct AutoSampleConfig {
    /// Instrument name — the pack's `name` and the sample filename stem.
    pub name: String,
    /// Vendor recorded in the spec (e.g. `Korg`).
    pub vendor: String,

    /// The note/velocity grid to walk.
    pub grid: Grid,
    /// Note-hold and tail timings.
    pub timing: Timing,
    /// MIDI output port + channel.
    pub midi: MidiRoute,
    /// Capture device + the channel pair the instrument returns on.
    pub audio: AudioRoute,

    /// Whether to give each zone a sustain loop, so held notes ring on past
    /// the recorded length instead of stopping when the sample runs out.
    pub loops: bool,
    /// How those loops are shaped.
    pub loop_policy: crate::loops::LoopPolicy,

    /// Directory that receives the WAVs and `library.styx`.
    pub out_dir: PathBuf,
    /// Pack path to build. `None` leaves the folder unpacked.
    pub pack_path: Option<PathBuf>,
}

/// The (note × velocity) matrix to record.
#[derive(Debug, Clone)]
pub struct Grid {
    /// Lowest MIDI note to sample (inclusive).
    pub low_note: u8,
    /// Highest MIDI note to sample (inclusive).
    pub high_note: u8,
    /// Semitones between sampled roots. 1 = chromatic (largest, most faithful);
    /// 3–4 is the usual quality/size compromise.
    pub note_interval: u8,
    /// Lowest velocity to strike.
    pub low_velocity: u8,
    /// Highest velocity to strike.
    pub high_velocity: u8,
    /// How many velocity layers to record per note. 1 = no dynamic layering.
    pub velocity_layers: u8,
}

impl Default for Grid {
    fn default() -> Self {
        // A full 88-key range at every 3rd semitone, three dynamic layers —
        // 30 roots × 3 = 90 samples, a few minutes of sampling.
        Self {
            low_note: 21,
            high_note: 108,
            note_interval: 3,
            low_velocity: 1,
            high_velocity: 127,
            velocity_layers: 3,
        }
    }
}

/// How long each note is held and how much tail is kept.
///
/// These are **limits, not fixed waits**. The sampler watches the actual level
/// and stops as soon as the note has decayed, so a percussive patch produces
/// short samples and a sustaining one is allowed its full release — without
/// either being configured per patch. The caps exist so a droning or
/// self-oscillating patch can't record forever.
#[derive(Debug, Clone)]
pub struct Timing {
    /// Longest the note is held down, in milliseconds. A patch that decays to
    /// silence while held is released early.
    pub note_length_ms: u32,
    /// Longest to keep recording after note-off, in milliseconds. Recording
    /// stops as soon as the release has decayed.
    pub max_tail_ms: u32,
    /// How long the level must stay below the silence threshold before the
    /// note counts as finished. Guards against a tremolo or LFO trough being
    /// mistaken for the end of the note.
    pub silence_hold_ms: u32,
    /// Silence threshold in dB below the note's own peak. -60 dB is inaudible
    /// under any normal playback gain.
    pub silence_db: f32,
    /// Silence between cells, so one note's tail never bleeds into the next
    /// capture. Also lets the instrument's voice allocator settle.
    pub settle_ms: u32,
}

impl Default for Timing {
    fn default() -> Self {
        Self {
            note_length_ms: 3000,
            max_tail_ms: 8000,
            silence_hold_ms: 150,
            silence_db: -60.0,
            settle_ms: 250,
        }
    }
}

impl Timing {
    /// The silence threshold for a note whose peak was `peak`, as a linear
    /// amplitude. Never returns less than `floor` (the measured noise floor
    /// threshold) — below that we would be chasing the converter, not the note.
    pub fn silence_threshold(&self, peak: f32, floor: f32) -> f32 {
        (peak * 10f32.powf(self.silence_db / 20.0)).max(floor)
    }
}

/// Where the notes go out.
#[derive(Debug, Clone)]
pub struct MidiRoute {
    /// Substring matched case-insensitively against the MIDI output port name
    /// (e.g. `Kronos`). Empty selects the default port.
    pub port: String,
    /// MIDI channel, 1-16 as shown on the instrument's front panel.
    pub channel: u8,
}

impl Default for MidiRoute {
    fn default() -> Self {
        Self {
            port: String::new(),
            channel: 1,
        }
    }
}

/// Where the audio comes back.
#[derive(Debug, Clone)]
pub struct AudioRoute {
    /// Substring matched against the input device name (e.g. `Galaxy`).
    /// Empty selects the system default input.
    pub device: String,
    /// Capture sample rate.
    pub sample_rate: u32,
    /// **1-based** interface input carrying the instrument's left output —
    /// what the front panel and patchbay call "input 5".
    pub left_input: u16,
    /// 1-based interface input carrying the right output.
    pub right_input: u16,
}

impl Default for AudioRoute {
    fn default() -> Self {
        Self {
            device: String::new(),
            sample_rate: 48_000,
            left_input: 1,
            right_input: 2,
        }
    }
}

impl AudioRoute {
    /// 0-based channel index of the left input, as cpal interleaves it.
    pub fn left_index(&self) -> usize {
        self.left_input.saturating_sub(1) as usize
    }

    /// 0-based channel index of the right input.
    pub fn right_index(&self) -> usize {
        self.right_input.saturating_sub(1) as usize
    }

    /// Highest 0-based channel index the stream must reach.
    pub fn max_index(&self) -> usize {
        self.left_index().max(self.right_index())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_based_inputs_map_to_zero_based_channels() {
        let route = AudioRoute {
            left_input: 5,
            right_input: 6,
            ..Default::default()
        };
        assert_eq!(route.left_index(), 4, "input 5 is channel index 4");
        assert_eq!(route.right_index(), 5);
        assert_eq!(route.max_index(), 5, "stream must reach index 5");
    }

    #[test]
    fn silence_threshold_scales_with_the_notes_own_peak() {
        let t = Timing {
            silence_db: -60.0,
            ..Default::default()
        };
        // -60 dB is a factor of 1/1000.
        assert!((t.silence_threshold(1.0, 0.0) - 0.001).abs() < 1e-6);
        assert!((t.silence_threshold(0.5, 0.0) - 0.0005).abs() < 1e-6);
    }

    #[test]
    fn silence_threshold_never_dips_below_the_noise_floor() {
        let t = Timing::default();
        // A quiet note would otherwise demand a threshold below the converter's
        // own noise, which it can never reach — the wait would always time out.
        assert_eq!(t.silence_threshold(0.001, 1e-4), 1e-4);
    }
}
