//! FTS EQ - 4-band parametric equalizer with low/high shelf
//!
//! This plugin provides a professional-grade parametric EQ with:
//! - Low shelf filter
//! - Two parametric peak filters  
//! - High shelf filter
//!
//! Each band has frequency, Q/resonance, and gain controls.

use atomic_float::AtomicF32;
use fts_plugin_core::dsp::{BiquadCoeffs, BiquadState};
use fts_plugin_core::prelude::*;
use std::sync::atomic::Ordering;
use std::sync::Arc;

mod editor;

/// Number of EQ bands
const NUM_BANDS: usize = 4;

/// Plugin parameters
#[derive(Params)]
pub struct EqParams {
    /// Editor state for resizable window
    #[persist = "editor-state"]
    pub editor_state: Arc<DioxusEditorState>,

    // === Low Shelf (Band 0) ===
    #[id = "low_freq"]
    pub low_freq: FloatParam,
    #[id = "low_gain"]
    pub low_gain: FloatParam,
    #[id = "low_q"]
    pub low_q: FloatParam,

    // === Low-Mid Peak (Band 1) ===
    #[id = "lowmid_freq"]
    pub lowmid_freq: FloatParam,
    #[id = "lowmid_gain"]
    pub lowmid_gain: FloatParam,
    #[id = "lowmid_q"]
    pub lowmid_q: FloatParam,

    // === High-Mid Peak (Band 2) ===
    #[id = "highmid_freq"]
    pub highmid_freq: FloatParam,
    #[id = "highmid_gain"]
    pub highmid_gain: FloatParam,
    #[id = "highmid_q"]
    pub highmid_q: FloatParam,

    // === High Shelf (Band 3) ===
    #[id = "high_freq"]
    pub high_freq: FloatParam,
    #[id = "high_gain"]
    pub high_gain: FloatParam,
    #[id = "high_q"]
    pub high_q: FloatParam,

    /// Output gain
    #[id = "output_gain"]
    pub output_gain: FloatParam,
}

impl Default for EqParams {
    fn default() -> Self {
        Self {
            editor_state: default_editor_state(),

            // Low shelf - 100 Hz default
            low_freq: FloatParam::new(
                "Low Freq",
                100.0,
                FloatRange::Skewed {
                    min: 20.0,
                    max: 500.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_unit(" Hz")
            .with_value_to_string(formatters::v2s_f32_hz_then_khz(0)),

            low_gain: FloatParam::new(
                "Low Gain",
                0.0,
                FloatRange::Linear {
                    min: -18.0,
                    max: 18.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            low_q: FloatParam::new(
                "Low Q",
                0.707,
                FloatRange::Skewed {
                    min: 0.1,
                    max: 4.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_value_to_string(formatters::v2s_f32_rounded(2)),

            // Low-mid peak - 400 Hz default
            lowmid_freq: FloatParam::new(
                "Low-Mid Freq",
                400.0,
                FloatRange::Skewed {
                    min: 100.0,
                    max: 2000.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_unit(" Hz")
            .with_value_to_string(formatters::v2s_f32_hz_then_khz(0)),

            lowmid_gain: FloatParam::new(
                "Low-Mid Gain",
                0.0,
                FloatRange::Linear {
                    min: -18.0,
                    max: 18.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            lowmid_q: FloatParam::new(
                "Low-Mid Q",
                1.0,
                FloatRange::Skewed {
                    min: 0.1,
                    max: 10.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_value_to_string(formatters::v2s_f32_rounded(2)),

            // High-mid peak - 2000 Hz default
            highmid_freq: FloatParam::new(
                "High-Mid Freq",
                2000.0,
                FloatRange::Skewed {
                    min: 500.0,
                    max: 8000.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_unit(" Hz")
            .with_value_to_string(formatters::v2s_f32_hz_then_khz(0)),

            highmid_gain: FloatParam::new(
                "High-Mid Gain",
                0.0,
                FloatRange::Linear {
                    min: -18.0,
                    max: 18.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            highmid_q: FloatParam::new(
                "High-Mid Q",
                1.0,
                FloatRange::Skewed {
                    min: 0.1,
                    max: 10.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_value_to_string(formatters::v2s_f32_rounded(2)),

            // High shelf - 8000 Hz default
            high_freq: FloatParam::new(
                "High Freq",
                8000.0,
                FloatRange::Skewed {
                    min: 2000.0,
                    max: 20000.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_unit(" Hz")
            .with_value_to_string(formatters::v2s_f32_hz_then_khz(0)),

            high_gain: FloatParam::new(
                "High Gain",
                0.0,
                FloatRange::Linear {
                    min: -18.0,
                    max: 18.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            high_q: FloatParam::new(
                "High Q",
                0.707,
                FloatRange::Skewed {
                    min: 0.1,
                    max: 4.0,
                    factor: FloatRange::skew_factor(-1.0),
                },
            )
            .with_value_to_string(formatters::v2s_f32_rounded(2)),

            // Output gain
            output_gain: FloatParam::new(
                "Output",
                0.0,
                FloatRange::Linear {
                    min: -12.0,
                    max: 12.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),
        }
    }
}

/// FTS EQ Plugin
pub struct FtsEq {
    params: Arc<EqParams>,
    sample_rate: f32,

    /// Filter coefficients for each band (L/R share coefficients)
    coeffs: [BiquadCoeffs; NUM_BANDS],

    /// Filter state for left channel
    state_l: [BiquadState; NUM_BANDS],
    /// Filter state for right channel
    state_r: [BiquadState; NUM_BANDS],

    /// Input peak meter (for UI)
    input_peak: Arc<AtomicF32>,
    /// Output peak meter (for UI)
    output_peak: Arc<AtomicF32>,
}

impl Default for FtsEq {
    fn default() -> Self {
        Self {
            params: Arc::new(EqParams::default()),
            sample_rate: 48000.0,
            coeffs: [BiquadCoeffs::default(); NUM_BANDS],
            state_l: [BiquadState::default(); NUM_BANDS],
            state_r: [BiquadState::default(); NUM_BANDS],
            input_peak: Arc::new(AtomicF32::new(0.0)),
            output_peak: Arc::new(AtomicF32::new(0.0)),
        }
    }
}

impl FtsEq {
    /// Update filter coefficients from parameters
    fn update_coefficients(&mut self) {
        let sr = self.sample_rate;

        // Band 0: Low shelf
        self.coeffs[0] = BiquadCoeffs::lowshelf(
            self.params.low_freq.value(),
            self.params.low_q.value(),
            self.params.low_gain.value(),
            sr,
        );

        // Band 1: Low-mid peak
        self.coeffs[1] = BiquadCoeffs::peak(
            self.params.lowmid_freq.value(),
            self.params.lowmid_q.value(),
            self.params.lowmid_gain.value(),
            sr,
        );

        // Band 2: High-mid peak
        self.coeffs[2] = BiquadCoeffs::peak(
            self.params.highmid_freq.value(),
            self.params.highmid_q.value(),
            self.params.highmid_gain.value(),
            sr,
        );

        // Band 3: High shelf
        self.coeffs[3] = BiquadCoeffs::highshelf(
            self.params.high_freq.value(),
            self.params.high_q.value(),
            self.params.high_gain.value(),
            sr,
        );
    }
}

impl Plugin for FtsEq {
    const NAME: &'static str = "FTS EQ";
    const VENDOR: &'static str = "FastTrackStudio";
    const URL: &'static str = "https://fasttrackstudio.io";
    const EMAIL: &'static str = "support@fasttrackstudio.io";
    const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[
        AudioIOLayout {
            main_input_channels: NonZeroU32::new(2),
            main_output_channels: NonZeroU32::new(2),
            ..AudioIOLayout::const_default()
        },
        AudioIOLayout {
            main_input_channels: NonZeroU32::new(1),
            main_output_channels: NonZeroU32::new(1),
            ..AudioIOLayout::const_default()
        },
    ];

    type SysExMessage = ();
    type BackgroundTask = ();

    fn params(&self) -> Arc<dyn Params> {
        self.params.clone()
    }

    fn editor(&mut self, _async_executor: AsyncExecutor<Self>) -> Option<Box<dyn Editor>> {
        editor::create(
            self.params.clone(),
            self.input_peak.clone(),
            self.output_peak.clone(),
        )
    }

    fn initialize(
        &mut self,
        _audio_io_layout: &AudioIOLayout,
        buffer_config: &BufferConfig,
        _context: &mut impl InitContext<Self>,
    ) -> bool {
        self.sample_rate = buffer_config.sample_rate;
        self.update_coefficients();
        true
    }

    fn reset(&mut self) {
        for state in &mut self.state_l {
            state.reset();
        }
        for state in &mut self.state_r {
            state.reset();
        }
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        _context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        // Update coefficients (could be optimized to only update on change)
        self.update_coefficients();

        let output_gain = fts_plugin_core::dsp::db_to_gain(self.params.output_gain.value());

        let mut input_peak_max: f32 = 0.0;
        let mut output_peak_max: f32 = 0.0;

        for channel_samples in buffer.iter_samples() {
            let mut samples: Vec<f32> = channel_samples.into_iter().map(|s| *s).collect();
            let num_channels = samples.len();

            // Track input peak
            for &sample in &samples {
                input_peak_max = input_peak_max.max(sample.abs());
            }

            // Process each sample through all bands
            for (ch, sample) in samples.iter_mut().enumerate() {
                let states = if ch == 0 {
                    &mut self.state_l
                } else {
                    &mut self.state_r
                };

                // Process through each band in series
                let mut x = *sample;
                for (band, state) in states.iter_mut().enumerate() {
                    x = state.process(x, &self.coeffs[band]);
                }

                // Apply output gain
                *sample = x * output_gain;
            }

            // Track output peak
            for &sample in &samples {
                output_peak_max = output_peak_max.max(sample.abs());
            }

            // Write back processed samples
            for (out_sample, processed) in channel_samples.into_iter().zip(samples.iter()) {
                *out_sample = *processed;
            }
        }

        // Update peak meters with decay
        let current_input = self.input_peak.load(Ordering::Relaxed);
        let current_output = self.output_peak.load(Ordering::Relaxed);

        self.input_peak.store(
            input_peak_max.max(current_input * 0.99),
            Ordering::Relaxed,
        );
        self.output_peak.store(
            output_peak_max.max(current_output * 0.99),
            Ordering::Relaxed,
        );

        ProcessStatus::Normal
    }
}

impl ClapPlugin for FtsEq {
    const CLAP_ID: &'static str = "io.fasttrackstudio.fts-eq";
    const CLAP_DESCRIPTION: Option<&'static str> =
        Some("4-band parametric equalizer with low/high shelf");
    const CLAP_MANUAL_URL: Option<&'static str> = Some(Self::URL);
    const CLAP_SUPPORT_URL: Option<&'static str> = None;
    const CLAP_FEATURES: &'static [ClapFeature] = &[
        ClapFeature::AudioEffect,
        ClapFeature::Stereo,
        ClapFeature::Mono,
        ClapFeature::Equalizer,
    ];
}

impl Vst3Plugin for FtsEq {
    const VST3_CLASS_ID: [u8; 16] = *b"FtsEq___________";
    const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
        &[Vst3SubCategory::Fx, Vst3SubCategory::Eq];
}

nih_export_clap!(FtsEq);
nih_export_vst3!(FtsEq);
