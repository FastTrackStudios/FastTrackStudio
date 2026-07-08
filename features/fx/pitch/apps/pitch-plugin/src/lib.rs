//! FTS Pitch — nih-plug entry point with 4-algorithm pitch shifter and Dioxus GUI.

use atomic_float::AtomicF32;
use fts_plugin_core::prelude::*;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use fts_dsp::{AudioConfig, Processor};
use pitch_dsp::chain::{Algorithm, PitchChain};
use pitch_dsp::pll::SubWaveform;

mod editor;

// ── Shared UI State ──────────────────────────────────────────────────

/// Audio-thread -> UI metering data.
pub struct PitchUiState {
    pub params: Arc<FtsPitchParams>,
    /// Peak input level in dB.
    pub input_peak_db: AtomicF32,
    /// Peak output level in dB.
    pub output_peak_db: AtomicF32,
    /// Current latency in samples (for display).
    pub latency_samples: AtomicF32,
}

impl PitchUiState {
    fn new(params: Arc<FtsPitchParams>) -> Self {
        Self {
            params,
            input_peak_db: AtomicF32::new(-100.0),
            output_peak_db: AtomicF32::new(-100.0),
            latency_samples: AtomicF32::new(0.0),
        }
    }
}

// ── Parameters ───────────────────────────────────────────────────────

#[derive(Params)]
pub struct FtsPitchParams {
    /// Algorithm selection.
    #[id = "algorithm"]
    pub algorithm: IntParam,

    /// Pitch shift in whole semitones (-24 to +24).
    #[id = "pitch"]
    pub pitch: IntParam,

    /// Fine-tune adjustment in cents (-50 to +50).
    #[id = "fine_tune"]
    pub fine_tune: FloatParam,

    /// Dry/wet mix (0.0-1.0).
    #[id = "mix"]
    pub mix: FloatParam,

    /// PLL sub-oscillator waveform: 0=Square, 1=Saw, 2=Triangle.
    #[id = "pll_waveform"]
    pub pll_waveform: IntParam,

    /// Grain size in samples (256-4096) for Granular mode.
    #[id = "grain_size"]
    pub grain_size: IntParam,

    /// Formant shift in semitones (-24 to +24). Only active when unlinked.
    #[id = "formant"]
    pub formant: IntParam,

    /// Link formant to pitch (formants stay in place when shifting).
    #[id = "formant_link"]
    pub formant_link: BoolParam,

    /// Output gain in dB.
    #[id = "output_gain"]
    pub output_gain_db: FloatParam,

    /// Live mode: minimize latency at the cost of quality.
    #[id = "live"]
    pub live: BoolParam,
}

impl Default for FtsPitchParams {
    fn default() -> Self {
        Self {
            algorithm: IntParam::new("Algorithm", 5, IntRange::Linear { min: 0, max: 8 })
                .with_value_to_string(Arc::new(|v| match v {
                    0 => "Divider".to_string(),
                    1 => "PLL".to_string(),
                    2 => "Granular".to_string(),
                    3 => "PSOLA".to_string(),
                    4 => "WSOLA".to_string(),
                    5 => "Signalsmith".to_string(),
                    6 => "Rubberband".to_string(),
                    7 => "Allpass".to_string(),
                    8 => "POG".to_string(),
                    _ => format!("{v}"),
                }))
                .with_string_to_value(Arc::new(|s| match s.trim().to_lowercase().as_str() {
                    "divider" | "freq" | "0" => Some(0),
                    "pll" | "1" => Some(1),
                    "granular" | "grain" | "2" => Some(2),
                    "psola" | "3" => Some(3),
                    "wsola" | "4" => Some(4),
                    "signalsmith" | "5" => Some(5),
                    "rubberband" | "rubber" | "6" => Some(6),
                    "allpass" | "7" => Some(7),
                    "pog" | "polyoctave" | "poly" | "8" => Some(8),
                    _ => s.parse().ok(),
                })),

            pitch: IntParam::new("Pitch", -12, IntRange::Linear { min: -24, max: 24 })
                .with_unit(" st")
                .with_value_to_string(Arc::new(|v| {
                    if v > 0 {
                        format!("+{v}")
                    } else {
                        format!("{v}")
                    }
                }))
                .with_string_to_value(Arc::new(|s| s.trim().parse().ok())),

            fine_tune: FloatParam::new(
                "Fine",
                0.0,
                FloatRange::Linear {
                    min: -50.0,
                    max: 50.0,
                },
            )
            .with_unit(" ct")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            mix: FloatParam::new("Mix", 1.0, FloatRange::Linear { min: 0.0, max: 1.0 })
                .with_unit("%")
                .with_value_to_string(formatters::v2s_f32_percentage(0)),

            pll_waveform: IntParam::new("PLL Wave", 1, IntRange::Linear { min: 0, max: 2 })
                .with_value_to_string(Arc::new(|v| match v {
                    0 => "Square".to_string(),
                    1 => "Saw".to_string(),
                    2 => "Triangle".to_string(),
                    _ => format!("{v}"),
                }))
                .with_string_to_value(Arc::new(|s| match s.trim().to_lowercase().as_str() {
                    "square" | "sq" | "0" => Some(0),
                    "saw" | "sawtooth" | "1" => Some(1),
                    "triangle" | "tri" | "2" => Some(2),
                    _ => s.parse().ok(),
                })),

            grain_size: IntParam::new(
                "Grain Size",
                1024,
                IntRange::Linear {
                    min: 256,
                    max: 4096,
                },
            )
            .with_unit(" smp")
            .with_value_to_string(Arc::new(|v| format!("{v}")))
            .with_string_to_value(Arc::new(|s| s.parse().ok())),

            formant: IntParam::new("Formant", 0, IntRange::Linear { min: -24, max: 24 })
                .with_unit(" st")
                .with_value_to_string(Arc::new(|v| {
                    if v > 0 {
                        format!("+{v}")
                    } else {
                        format!("{v}")
                    }
                }))
                .with_string_to_value(Arc::new(|s| s.trim().parse().ok())),

            formant_link: BoolParam::new("Formant Link", true),

            output_gain_db: FloatParam::new(
                "Output",
                0.0,
                FloatRange::Linear {
                    min: -24.0,
                    max: 24.0,
                },
            )
            .with_unit(" dB")
            .with_value_to_string(formatters::v2s_f32_rounded(1)),

            live: BoolParam::new("Live", false),
        }
    }
}

// ── Plugin ───────────────────────────────────────────────────────────

struct FtsPitch {
    params: Arc<FtsPitchParams>,
    ui_state: Arc<PitchUiState>,
    editor_state: Arc<DioxusState>,
    chain: PitchChain,
    sample_rate: f64,
}

impl Default for FtsPitch {
    fn default() -> Self {
        let params = Arc::new(FtsPitchParams::default());
        let ui_state = Arc::new(PitchUiState::new(params.clone()));
        Self {
            params,
            ui_state,
            editor_state: DioxusState::new(|| (640, 400)),
            chain: PitchChain::new(),
            sample_rate: 48000.0,
        }
    }
}

impl FtsPitch {
    /// Sync nih-plug params -> pitch-dsp parameters.
    fn sync_params(&mut self) {
        self.chain.algorithm = match self.params.algorithm.value() {
            0 => Algorithm::FreqDivider,
            1 => Algorithm::Pll,
            2 => Algorithm::Granular,
            3 => Algorithm::Psola,
            4 => Algorithm::Wsola,
            5 => Algorithm::Signalsmith,
            6 => Algorithm::Rubberband,
            7 => Algorithm::Allpass,
            8 => Algorithm::PolyOctave,
            _ => Algorithm::Signalsmith,
        };
        self.chain.semitones =
            self.params.pitch.value() as f64 + self.params.fine_tune.value() as f64 / 100.0;
        self.chain.mix = self.params.mix.value() as f64;
        self.chain.pll_waveform = match self.params.pll_waveform.value() {
            0 => SubWaveform::Square,
            1 => SubWaveform::Saw,
            2 => SubWaveform::Triangle,
            _ => SubWaveform::Saw,
        };
        self.chain.grain_size = self.params.grain_size.value() as usize;
        self.chain.formant_linked = self.params.formant_link.value();
        self.chain.formant_semitones = self.params.formant.value() as f64;
        self.chain.live = self.params.live.value();
    }
}

impl Plugin for FtsPitch {
    const NAME: &'static str = "FTS Pitch";
    const VENDOR: &'static str = "FastTrackStudio";
    const URL: &'static str = "";
    const EMAIL: &'static str = "";
    const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[AudioIOLayout {
        main_input_channels: NonZeroU32::new(2),
        main_output_channels: NonZeroU32::new(2),
        ..AudioIOLayout::const_default()
    }];

    type SysExMessage = ();
    type BackgroundTask = ();

    fn params(&self) -> Arc<dyn Params> {
        self.params.clone()
    }

    fn editor(&mut self, _async_executor: AsyncExecutor<Self>) -> Option<Box<dyn Editor>> {
        create_dioxus_editor_with_state(
            self.editor_state.clone(),
            self.ui_state.clone(),
            editor::App,
        )
    }

    fn initialize(
        &mut self,
        _audio_io_layout: &AudioIOLayout,
        buffer_config: &BufferConfig,
        _context: &mut impl InitContext<Self>,
    ) -> bool {
        self.sample_rate = buffer_config.sample_rate as f64;
        self.chain.update(AudioConfig {
            sample_rate: self.sample_rate,
            max_buffer_size: buffer_config.max_buffer_size as usize,
        });
        true
    }

    fn reset(&mut self) {
        self.chain.reset();
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        self.sync_params();

        // Report latency to host.
        let latency = self.chain.latency() as u32;
        context.set_latency_samples(latency);
        self.ui_state
            .latency_samples
            .store(latency as f32, Ordering::Relaxed);

        let output_gain = 10.0f64.powf(self.params.output_gain_db.value() as f64 / 20.0);

        for mut frame in buffer.iter_samples() {
            let mut channels = frame.iter_mut();
            let left_ref = channels.next().unwrap();
            let right_ref = channels.next().unwrap();

            let input_peak = (*left_ref).abs().max((*right_ref).abs());

            let mut left = *left_ref as f64;
            let mut right = *right_ref as f64;

            // Process through pitch chain (mono processing, copies to right).
            let mut mono = [left];
            let mut mono_r = [right];
            self.chain.process(&mut mono, &mut mono_r);
            left = mono[0] * output_gain;
            right = mono_r[0] * output_gain;

            *left_ref = left as f32;
            *right_ref = right as f32;

            let output_peak = (left.abs().max(right.abs())) as f32;

            // Update metering with exponential decay.
            let prev_in = self.ui_state.input_peak_db.load(Ordering::Relaxed);
            let in_db = if input_peak > 0.0 {
                20.0 * input_peak.log10()
            } else {
                -100.0
            };
            let new_in = if in_db > prev_in {
                in_db
            } else {
                prev_in - 0.3
            };
            self.ui_state.input_peak_db.store(new_in, Ordering::Relaxed);

            let prev_out = self.ui_state.output_peak_db.load(Ordering::Relaxed);
            let out_db = if output_peak > 0.0 {
                20.0 * output_peak.log10()
            } else {
                -100.0
            };
            let new_out = if out_db > prev_out {
                out_db
            } else {
                prev_out - 0.3
            };
            self.ui_state
                .output_peak_db
                .store(new_out, Ordering::Relaxed);
        }

        ProcessStatus::Normal
    }
}

impl ClapPlugin for FtsPitch {
    const CLAP_ID: &'static str = "com.fasttrackstudio.pitch";
    const CLAP_DESCRIPTION: Option<&'static str> =
        Some("Multi-algorithm pitch shifter (divider, PLL, granular, PSOLA)");
    const CLAP_MANUAL_URL: Option<&'static str> = None;
    const CLAP_SUPPORT_URL: Option<&'static str> = None;
    const CLAP_FEATURES: &'static [ClapFeature] = &[
        ClapFeature::AudioEffect,
        ClapFeature::PitchShifter,
        ClapFeature::Stereo,
    ];
}

impl Vst3Plugin for FtsPitch {
    const VST3_CLASS_ID: [u8; 16] = *b"FtsPitchPlugn001";
    const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
        &[Vst3SubCategory::Fx, Vst3SubCategory::PitchShift];
}

nih_export_clap!(FtsPitch);
nih_export_vst3!(FtsPitch);
