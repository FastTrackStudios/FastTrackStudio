//! FTS Guide - A comprehensive guide track plugin with click, counts, and section guides
//!
//! This plugin provides:
//! - Click track with multiple subdivision options (beat, eighth, sixteenth, triplet)
//! - Count-in samples for sections
//! - Section guide announcements
//! - 8-channel output routing: Click (1/2), Shaker (3/4), Count (5/6), Guide (7/8)

use atomic_float::AtomicF32;
use fts_plugin_core::prelude::*;
use std::collections::HashMap;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;

mod audio;
mod count_in;
mod midi;
mod samples;

mod editor;

use audio::{ClickPlayer, ClickPlayerState, CountPlayer, CountPlayerState};
use audio::{GuidePlayer, GuidePlayerState};
use samples::ClickSampleLoader;

/// Information about a section for count-in purposes
#[derive(Clone, Debug)]
pub struct SectionInfo {
    /// Section start position in seconds
    pub start_seconds: f64,
    /// Section end position in seconds
    pub end_seconds: f64,
    /// Section name
    pub name: String,
    /// Count-in marker position (if this is the first section of a song)
    pub count_in_position: Option<f64>,
    /// SONGEND marker position (if this is the last section before SONGEND)
    pub song_end_position: Option<f64>,
    /// Whether this is the first section of its song
    pub is_first_section: bool,
    /// Section type name for guide mapping (e.g., "Verse", "Chorus", "Bridge")
    pub section_type_name: String,
    /// Section number (if numbered, e.g., 1, 2, 3)
    pub section_number: Option<u32>,
}

/// Click sound selection
#[derive(Enum, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClickSound {
    #[id = "blip"]
    Blip,
    #[id = "classic"]
    Classic,
    #[id = "cowbell"]
    Cowbell,
    #[id = "digital"]
    Digital,
    #[id = "gentle"]
    Gentle,
    #[id = "percussive"]
    Percussive,
    #[id = "saw"]
    Saw,
    #[id = "woodblock"]
    Woodblock,
}

impl Default for ClickSound {
    fn default() -> Self {
        Self::Blip
    }
}

/// Main Guide plugin struct
pub struct FtsGuide {
    params: Arc<GuideParams>,

    /// Loaded audio sample data for each subdivision
    sample_data_beat: Arc<Mutex<Option<DecodedAudioF32>>>,
    sample_data_eighth: Arc<Mutex<Option<DecodedAudioF32>>>,
    sample_data_sixteenth: Arc<Mutex<Option<DecodedAudioF32>>>,
    sample_data_triplet: Arc<Mutex<Option<DecodedAudioF32>>>,
    sample_data_measure_accent: Arc<Mutex<Option<DecodedAudioF32>>>,

    /// Count samples (1-8) for counting before regions
    sample_data_count: [Arc<Mutex<Option<DecodedAudioF32>>>; 8],

    /// Guide samples - maps section name to loaded audio
    guide_samples: Arc<Mutex<HashMap<String, DecodedAudioF32>>>,

    /// Current guide sample being played (section key)
    current_guide_key: Arc<Mutex<Option<String>>>,

    /// Click player state
    click_state: ClickPlayerState,

    /// Count player state
    count_state: CountPlayerState,

    /// Guide player state
    guide_state: GuidePlayerState,

    /// Current plugin sample rate
    sample_rate: f32,

    /// Number of channels in the loaded audio files
    num_channels: u32,

    /// Length of samples in samples
    sample_length: usize,

    /// Previous beat position (to detect beat boundaries)
    previous_beat_position: f64,

    /// Last triggered positions for each subdivision
    last_triggered_beat: f64,
    last_triggered_eighth: f64,
    last_triggered_sixteenth: f64,
    last_triggered_triplet: f64,

    /// Track the last time signature
    last_time_sig_numerator: Option<i32>,
    last_time_sig_denominator: Option<i32>,

    /// Track the last tempo
    last_tempo: Option<f64>,

    /// Track the last bar number
    last_bar_number: Option<i32>,

    /// Cached section information
    cached_regions: Arc<Mutex<Vec<SectionInfo>>>,

    /// Flag to request region refresh
    request_region_refresh: Arc<AtomicBool>,

    /// Transport information shared with GUI
    transport_tempo: Arc<AtomicF32>,
    transport_time_sig_numerator: Arc<AtomicI32>,
    transport_time_sig_denominator: Arc<AtomicI32>,
    transport_bar_number: Arc<AtomicI32>,
    transport_beat_position: Arc<AtomicF32>,
    transport_playing: Arc<AtomicBool>,

    /// Plugin sample rate (shared with editor)
    plugin_sample_rate: Arc<AtomicF32>,

    /// Currently loaded click sound
    current_click_sound: ClickSound,
}

/// Plugin parameters
#[derive(Params)]
pub struct GuideParams {
    /// Editor state
    #[persist = "editor-state"]
    pub editor_state: Arc<DioxusState>,

    #[id = "gain"]
    pub gain: FloatParam,

    #[id = "enable_beat"]
    pub enable_beat: BoolParam,

    #[id = "enable_eighth"]
    pub enable_eighth: BoolParam,

    #[id = "enable_sixteenth"]
    pub enable_sixteenth: BoolParam,

    #[id = "enable_triplet"]
    pub enable_triplet: BoolParam,

    #[id = "enable_measure_accent"]
    pub enable_measure_accent: BoolParam,

    #[id = "enable_count"]
    pub enable_count: BoolParam,

    #[id = "enable_guide"]
    pub enable_guide: BoolParam,

    #[id = "click_sound"]
    pub click_sound: EnumParam<ClickSound>,

    #[id = "guide_replace_beat1"]
    pub guide_replace_beat1: BoolParam,

    #[id = "offset_count_by_one"]
    pub offset_count_by_one: BoolParam,

    #[id = "extend_songend_count"]
    pub extend_songend_count: BoolParam,

    #[id = "full_count_odd_time"]
    pub full_count_odd_time: BoolParam,
}

impl Default for FtsGuide {
    fn default() -> Self {
        Self {
            params: Arc::new(GuideParams::default()),
            sample_data_beat: Arc::new(Mutex::new(None)),
            sample_data_eighth: Arc::new(Mutex::new(None)),
            sample_data_sixteenth: Arc::new(Mutex::new(None)),
            sample_data_triplet: Arc::new(Mutex::new(None)),
            sample_data_measure_accent: Arc::new(Mutex::new(None)),
            sample_data_count: [
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
                Arc::new(Mutex::new(None)),
            ],
            guide_samples: Arc::new(Mutex::new(HashMap::new())),
            current_guide_key: Arc::new(Mutex::new(None)),
            click_state: ClickPlayerState::new(),
            count_state: CountPlayerState::new(),
            guide_state: GuidePlayerState::new(),
            sample_rate: 44100.0,
            num_channels: 2,
            sample_length: 0,
            previous_beat_position: -1.0,
            last_triggered_beat: -1.0,
            last_triggered_eighth: -1.0,
            last_triggered_sixteenth: -1.0,
            last_triggered_triplet: -1.0,
            last_time_sig_numerator: None,
            last_time_sig_denominator: None,
            last_tempo: None,
            last_bar_number: None,
            cached_regions: Arc::new(Mutex::new(Vec::new())),
            request_region_refresh: Arc::new(AtomicBool::new(false)),
            transport_tempo: Arc::new(AtomicF32::new(120.0)),
            transport_time_sig_numerator: Arc::new(AtomicI32::new(4)),
            transport_time_sig_denominator: Arc::new(AtomicI32::new(4)),
            transport_bar_number: Arc::new(AtomicI32::new(0)),
            transport_beat_position: Arc::new(AtomicF32::new(0.0)),
            transport_playing: Arc::new(AtomicBool::new(false)),
            plugin_sample_rate: Arc::new(AtomicF32::new(44100.0)),
            current_click_sound: ClickSound::default(),
        }
    }
}

impl Default for GuideParams {
    fn default() -> Self {
        Self {
            editor_state: fts_plugin_core::default_editor_state(),

            gain: FloatParam::new(
                "Gain",
                util::db_to_gain(0.0),
                FloatRange::Skewed {
                    min: util::db_to_gain(-60.0),
                    max: util::db_to_gain(12.0),
                    factor: FloatRange::skew_factor(-2.0),
                },
            )
            .with_smoother(SmoothingStyle::Logarithmic(50.0))
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_gain_to_db(2))
            .with_string_to_value(formatters::s2v_f32_gain_to_db()),

            enable_beat: BoolParam::new("Enable Beat", true),
            enable_eighth: BoolParam::new("Enable Eighth", false),
            enable_sixteenth: BoolParam::new("Enable Sixteenth", false),
            enable_triplet: BoolParam::new("Enable Triplet", false),
            enable_measure_accent: BoolParam::new("Measure Accent", true),
            enable_count: BoolParam::new("Enable Count", true),
            enable_guide: BoolParam::new("Enable Guide", true),
            click_sound: EnumParam::new("Click Sound", ClickSound::Blip),
            guide_replace_beat1: BoolParam::new("Guide Replaces Beat 1", true),
            offset_count_by_one: BoolParam::new("Offset Count By One", true),
            extend_songend_count: BoolParam::new("Extend SONGEND Count", true),
            full_count_odd_time: BoolParam::new("Full Count for Odd Time", true),
        }
    }
}

impl Plugin for FtsGuide {
    const NAME: &'static str = "FTS Guide";
    const VENDOR: &'static str = "FastTrackStudio";
    const URL: &'static str = "https://github.com/FastTrackStudios/FastTrackStudio";
    const EMAIL: &'static str = "info@fasttrackstudio.com";
    const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[AudioIOLayout {
        main_input_channels: NonZeroU32::new(2),
        main_output_channels: NonZeroU32::new(8), // 8 output channels
        ..AudioIOLayout::const_default()
    }];

    const MIDI_INPUT: MidiConfig = MidiConfig::Basic;
    const MIDI_OUTPUT: MidiConfig = MidiConfig::Basic;
    const SAMPLE_ACCURATE_AUTOMATION: bool = true;

    type SysExMessage = ();
    type BackgroundTask = ();

    fn params(&self) -> Arc<dyn Params> {
        self.params.clone()
    }

    fn editor(&mut self, _async_executor: AsyncExecutor<Self>) -> Option<Box<dyn Editor>> {
        editor::create(
            self.params.clone(),
            self.transport_tempo.clone(),
            self.transport_time_sig_numerator.clone(),
            self.transport_time_sig_denominator.clone(),
            self.transport_bar_number.clone(),
            self.transport_beat_position.clone(),
            self.transport_playing.clone(),
        )
    }

    fn initialize(
        &mut self,
        _audio_io_layout: &AudioIOLayout,
        buffer_config: &BufferConfig,
        _context: &mut impl InitContext<Self>,
    ) -> bool {
        self.sample_rate = buffer_config.sample_rate;
        self.plugin_sample_rate
            .store(buffer_config.sample_rate, Ordering::Relaxed);

        // Load click samples
        let click_sound = self.params.click_sound.value();
        ClickSampleLoader::load_samples(
            click_sound,
            self.sample_rate,
            &self.sample_data_beat,
            &self.sample_data_eighth,
            &self.sample_data_sixteenth,
            &self.sample_data_triplet,
            &self.sample_data_measure_accent,
            &mut self.num_channels,
            &mut self.sample_length,
        );
        self.current_click_sound = click_sound;

        true
    }

    fn reset(&mut self) {
        self.click_state.reset();
        self.count_state.reset();
        self.guide_state.reset();
        self.previous_beat_position = -1.0;
        self.last_triggered_beat = -1.0;
        self.last_triggered_eighth = -1.0;
        self.last_triggered_sixteenth = -1.0;
        self.last_triggered_triplet = -1.0;
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        // Check if click sound changed
        let click_sound = self.params.click_sound.value();
        if click_sound != self.current_click_sound {
            ClickSampleLoader::load_samples(
                click_sound,
                self.sample_rate,
                &self.sample_data_beat,
                &self.sample_data_eighth,
                &self.sample_data_sixteenth,
                &self.sample_data_triplet,
                &self.sample_data_measure_accent,
                &mut self.num_channels,
                &mut self.sample_length,
            );
            self.current_click_sound = click_sound;
        }

        // Get transport info
        let transport = context.transport();
        let is_playing = transport.playing;

        // Update shared transport state for GUI
        if let Some(tempo) = transport.tempo {
            self.transport_tempo.store(tempo as f32, Ordering::Relaxed);
        }
        if let Some((num, denom)) = transport
            .time_sig_numerator
            .zip(transport.time_sig_denominator)
        {
            self.transport_time_sig_numerator
                .store(num, Ordering::Relaxed);
            self.transport_time_sig_denominator
                .store(denom, Ordering::Relaxed);
        }
        if let Some(pos_beats) = transport.pos_beats() {
            self.transport_beat_position
                .store(pos_beats as f32, Ordering::Relaxed);
        }
        self.transport_playing.store(is_playing, Ordering::Relaxed);

        let gain = self.params.gain.smoothed.next();

        // Process each sample
        for (_sample_idx, channel_samples) in buffer.iter_samples().enumerate() {
            // Initialize output channels
            let mut click_left = 0.0f32;
            let mut click_right = 0.0f32;
            let mut shaker_left = 0.0f32;
            let mut shaker_right = 0.0f32;
            let mut count_left = 0.0f32;
            let mut count_right = 0.0f32;
            let mut guide_left = 0.0f32;
            let mut guide_right = 0.0f32;

            // Play active click samples
            ClickPlayer::play_all(
                &mut self.click_state,
                &self.sample_data_beat,
                &self.sample_data_eighth,
                &self.sample_data_sixteenth,
                &self.sample_data_triplet,
                &self.sample_data_measure_accent,
                gain,
                &mut click_left,
                &mut click_right,
            );

            // Play active count samples
            CountPlayer::play_all(
                &mut self.count_state,
                &self.sample_data_count,
                gain,
                &mut count_left,
                &mut count_right,
            );

            // Play guide sample
            GuidePlayer::play(
                &mut self.guide_state,
                &self.current_guide_key,
                &self.guide_samples,
                gain,
                &mut guide_left,
                &mut guide_right,
            );

            // Route to output channels
            for (channel_idx, sample) in channel_samples.into_iter().enumerate() {
                audio::AudioRouter::route_single_channel(
                    channel_idx,
                    sample,
                    click_left,
                    click_right,
                    shaker_left,
                    shaker_right,
                    count_left,
                    count_right,
                    guide_left,
                    guide_right,
                );
            }
        }

        ProcessStatus::Normal
    }
}

impl ClapPlugin for FtsGuide {
    const CLAP_ID: &'static str = "com.fasttrackstudio.fts-guide";
    const CLAP_DESCRIPTION: Option<&'static str> =
        Some("Click track, count-in, and section guide plugin for REAPER");
    const CLAP_MANUAL_URL: Option<&'static str> = Some(Self::URL);
    const CLAP_SUPPORT_URL: Option<&'static str> = None;
    const CLAP_FEATURES: &'static [ClapFeature] = &[
        ClapFeature::AudioEffect,
        ClapFeature::Utility,
        ClapFeature::Custom("click-track"),
    ];
}

impl Vst3Plugin for FtsGuide {
    const VST3_CLASS_ID: [u8; 16] = *b"FtsGuidePlugin__";
    const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
        &[Vst3SubCategory::Fx, Vst3SubCategory::Tools];
}

nih_export_clap!(FtsGuide);
nih_export_vst3!(FtsGuide);
