//! FTS Gain - CLAP/VST3 plugin wrapper.
//!
//! This is a thin wrapper around `fts-gain-core` that provides the NIH-plug
//! plugin implementation for CLAP and VST3 formats.

use fts_gain_core::{GainParamValues, GainProcessor};
use fts_plugin_core::prelude::*;
use std::sync::Arc;

mod editor;

/// Main plugin struct.
pub struct FtsGain {
    params: Arc<GainParams>,
    /// DSP processor from core module
    processor: GainProcessor,
}

/// Plugin parameters (NIH-plug wrapper around core params).
#[derive(Params)]
pub struct GainParams {
    /// Editor state (window size, etc.).
    #[persist = "editor-state"]
    pub editor_state: Arc<DioxusState>,

    /// Main gain parameter in dB.
    #[id = "gain"]
    pub gain: FloatParam,

    /// Phase invert toggle.
    #[id = "phase"]
    pub phase_invert: BoolParam,
}

impl GainParams {
    /// Get the current parameter values as a core type.
    pub fn to_values(&self) -> GainParamValues {
        GainParamValues {
            gain_db: util::gain_to_db(self.gain.value()),
            phase_invert: self.phase_invert.value(),
        }
    }
}

impl Default for FtsGain {
    fn default() -> Self {
        Self {
            params: Arc::new(GainParams::default()),
            processor: GainProcessor::new(48000.0),
        }
    }
}

impl Default for GainParams {
    fn default() -> Self {
        Self {
            editor_state: fts_plugin_core::default_editor_state(),

            gain: FloatParam::new(
                "Gain",
                util::db_to_gain(0.0),
                FloatRange::Skewed {
                    min: util::db_to_gain(-60.0),
                    max: util::db_to_gain(24.0),
                    factor: FloatRange::gain_skew_factor(-60.0, 24.0),
                },
            )
            .with_smoother(SmoothingStyle::Logarithmic(50.0))
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_gain_to_db(2))
            .with_string_to_value(formatters::s2v_f32_gain_to_db()),

            phase_invert: BoolParam::new("Phase Invert", false),
        }
    }
}

impl Plugin for FtsGain {
    const NAME: &'static str = "FTS Gain";
    const VENDOR: &'static str = "FastTrackStudio";
    const URL: &'static str = "https://github.com/FastTrackStudios/FastTrackStudio";
    const EMAIL: &'static str = "info@fasttrackstudio.com";
    const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[
        // Stereo
        AudioIOLayout {
            main_input_channels: NonZeroU32::new(2),
            main_output_channels: NonZeroU32::new(2),
            ..AudioIOLayout::const_default()
        },
        // Mono
        AudioIOLayout {
            main_input_channels: NonZeroU32::new(1),
            main_output_channels: NonZeroU32::new(1),
            ..AudioIOLayout::const_default()
        },
    ];

    const SAMPLE_ACCURATE_AUTOMATION: bool = true;

    type SysExMessage = ();
    type BackgroundTask = ();

    fn params(&self) -> Arc<dyn Params> {
        self.params.clone()
    }

    fn editor(&mut self, _async_executor: AsyncExecutor<Self>) -> Option<Box<dyn Editor>> {
        let meters = self.processor.meters_clone();
        editor::create(self.params.clone(), meters)
    }

    fn initialize(
        &mut self,
        _audio_io_layout: &AudioIOLayout,
        buffer_config: &BufferConfig,
        _context: &mut impl InitContext<Self>,
    ) -> bool {
        self.processor.set_sample_rate(buffer_config.sample_rate);
        true
    }

    fn reset(&mut self) {
        self.processor.reset();
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        _context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        let update_meters = self.params.editor_state.is_open();

        // Get smoothed gain value and convert to param values
        let gain_linear = self.params.gain.smoothed.next();
        let params = GainParamValues {
            gain_db: util::gain_to_db(gain_linear),
            phase_invert: self.params.phase_invert.value(),
        };

        // Process based on channel count
        let num_channels = buffer.channels();

        if num_channels == 2 {
            // Stereo processing
            let (left, right) = buffer.as_slice().split_at_mut(1);
            if let (Some(left_ch), Some(right_ch)) = (left.first_mut(), right.first_mut()) {
                self.processor
                    .process_stereo(left_ch, right_ch, &params, update_meters);
            }
        } else {
            // Mono or process channels independently
            for channel in buffer.as_slice() {
                self.processor.process_mono(channel, &params, update_meters);
            }
        }

        ProcessStatus::Normal
    }
}

impl ClapPlugin for FtsGain {
    const CLAP_ID: &'static str = "com.fasttrackstudio.fts-gain";
    const CLAP_DESCRIPTION: Option<&'static str> = Some("Simple gain/utility plugin with metering");
    const CLAP_MANUAL_URL: Option<&'static str> = Some(Self::URL);
    const CLAP_SUPPORT_URL: Option<&'static str> = None;
    const CLAP_FEATURES: &'static [ClapFeature] = &[
        ClapFeature::AudioEffect,
        ClapFeature::Stereo,
        ClapFeature::Mono,
        ClapFeature::Utility,
    ];
}

impl Vst3Plugin for FtsGain {
    const VST3_CLASS_ID: [u8; 16] = *b"FtsGainPlugin___";
    const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
        &[Vst3SubCategory::Fx, Vst3SubCategory::Tools];
}

nih_export_clap!(FtsGain);
nih_export_vst3!(FtsGain);
