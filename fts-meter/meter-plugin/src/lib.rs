//! FTS Meter — nih-plug entry point: passthrough metering plugin.
//!
//! All DSP processors run on the audio thread. Their shared `Arc<*State>`
//! objects are passed to the editor UI for display.

use std::sync::Arc;

use fts_plugin_core::prelude::*;

use meter_dsp::bit_depth::BitDepthAnalyzer;
use meter_dsp::k_meter::{KMeter, KMode};
use meter_dsp::lufs::LufsMeter;
use meter_dsp::phase::PhaseCorrelation;
use meter_dsp::spectrum::SpectrumAnalyzer;

pub mod editor;

// ── Shared UI state ──────────────────────────────────────────────────────────

/// Shared read-only state between audio thread and UI.
///
/// Each field is an `Arc` to the inner `*State` that the DSP processors
/// write to. The UI painters clone these arcs and read from them during
/// rendering. No mutable access needed from the UI side.
pub struct MeterUiState {
    pub spectrum_state: Arc<meter_dsp::spectrum::SpectrumState>,
    pub spectrum_r_state: Arc<meter_dsp::spectrum::SpectrumState>,
    pub lufs_state: Arc<meter_dsp::lufs::LufsState>,
    pub k_meter_l_state: Arc<meter_dsp::k_meter::KMeterState>,
    pub k_meter_r_state: Arc<meter_dsp::k_meter::KMeterState>,
    pub phase_state: Arc<meter_dsp::phase::PhaseState>,
    pub bits_state: Arc<meter_dsp::bit_depth::BitDepthState>,
}

// ── Parameters ───────────────────────────────────────────────────────────────

/// No automatable parameters — this is a pure metering plugin.
#[derive(Params)]
pub struct MeterParams {}

impl Default for MeterParams {
    fn default() -> Self {
        Self {}
    }
}

// ── Plugin struct ─────────────────────────────────────────────────────────────

struct FtsMeter {
    params: Arc<MeterParams>,
    ui_state: Arc<MeterUiState>,
    editor_state: Arc<DioxusState>,

    // DSP processors live here (not shared, not Arc — audio thread only)
    spectrum: SpectrumAnalyzer,
    spectrum_r: SpectrumAnalyzer,
    lufs: LufsMeter,
    k_meter_l: KMeter,
    k_meter_r: KMeter,
    phase: PhaseCorrelation,
    bits: BitDepthAnalyzer,
}

impl Default for FtsMeter {
    fn default() -> Self {
        let spectrum = SpectrumAnalyzer::new(48000.0, 2048);
        let spectrum_r = SpectrumAnalyzer::new(48000.0, 2048);
        let lufs = LufsMeter::new(48000.0);
        let k_meter_l = KMeter::new(48000.0, KMode::K20);
        let k_meter_r = KMeter::new(48000.0, KMode::K20);
        let phase = PhaseCorrelation::new(48000.0);
        let bits = BitDepthAnalyzer::new(48000.0);

        let ui_state = Arc::new(MeterUiState {
            spectrum_state: spectrum.state.clone(),
            spectrum_r_state: spectrum_r.state.clone(),
            lufs_state: lufs.state.clone(),
            k_meter_l_state: k_meter_l.state.clone(),
            k_meter_r_state: k_meter_r.state.clone(),
            phase_state: phase.state.clone(),
            bits_state: bits.state.clone(),
        });

        Self {
            params: Arc::new(MeterParams::default()),
            ui_state,
            editor_state: DioxusState::new(|| (800, 500)),
            spectrum,
            spectrum_r,
            lufs,
            k_meter_l,
            k_meter_r,
            phase,
            bits,
        }
    }
}

// ── Plugin impl ───────────────────────────────────────────────────────────────

impl Plugin for FtsMeter {
    const NAME: &'static str = "FTS Meter";
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
        let sr = buffer_config.sample_rate;

        // Rebuild DSP processors with the actual sample rate, but reuse the
        // existing Arc<*State> objects so the editor (which already captured
        // those arcs) keeps reading live data.
        let mut spectrum = SpectrumAnalyzer::new(sr, 2048);
        let mut spectrum_r = SpectrumAnalyzer::new(sr, 2048);
        let mut lufs = LufsMeter::new(sr);
        let mut k_meter_l = KMeter::new(sr, KMode::K20);
        let mut k_meter_r = KMeter::new(sr, KMode::K20);
        let mut phase = PhaseCorrelation::new(sr);
        let mut bits = BitDepthAnalyzer::new(sr);

        // Swap state arcs: point new processors at the existing shared state
        // so the editor's painters continue reading from the same arcs.
        // Also update sample_rate in SpectrumState so the freq axis stays correct.
        use std::sync::atomic::Ordering;
        self.ui_state.spectrum_state.sample_rate.store(sr, Ordering::Relaxed);
        self.ui_state.spectrum_r_state.sample_rate.store(sr, Ordering::Relaxed);
        spectrum.state = self.ui_state.spectrum_state.clone();
        spectrum_r.state = self.ui_state.spectrum_r_state.clone();
        lufs.state = self.ui_state.lufs_state.clone();
        k_meter_l.state = self.ui_state.k_meter_l_state.clone();
        k_meter_r.state = self.ui_state.k_meter_r_state.clone();
        phase.state = self.ui_state.phase_state.clone();
        bits.state = self.ui_state.bits_state.clone();

        self.spectrum = spectrum;
        self.spectrum_r = spectrum_r;
        self.lufs = lufs;
        self.k_meter_l = k_meter_l;
        self.k_meter_r = k_meter_r;
        self.phase = phase;
        self.bits = bits;

        true
    }

    fn reset(&mut self) {
        self.lufs.reset();
        self.k_meter_l.reset();
        self.k_meter_r.reset();
        self.phase.reset();
        self.bits.reset();
        self.spectrum.reset_max();
        self.spectrum_r.reset_max();
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        _context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        // Collect all samples into channel buffers for block-oriented DSP.
        // We also do passthrough (audio is unchanged).
        let mut left_buf = Vec::with_capacity(buffer.samples());
        let mut right_buf = Vec::with_capacity(buffer.samples());

        for mut frame in buffer.iter_samples() {
            let mut channels = frame.iter_mut();
            let left = channels.next().map(|s| *s).unwrap_or(0.0);
            let right = channels.next().map(|s| *s).unwrap_or(0.0);
            left_buf.push(left);
            right_buf.push(right);
        }

        // Feed DSP processors
        self.spectrum.process(&left_buf);
        self.spectrum_r.process(&right_buf);
        self.lufs.process(&left_buf, &right_buf);
        self.k_meter_l.process(&left_buf);
        self.k_meter_r.process(&right_buf);
        self.phase.process_block(&left_buf, &right_buf);
        self.bits.process(&left_buf);

        ProcessStatus::Normal
    }
}

// ── CLAP / VST3 ───────────────────────────────────────────────────────────────

impl ClapPlugin for FtsMeter {
    const CLAP_ID: &'static str = "com.fasttrackstudio.meter";
    const CLAP_DESCRIPTION: Option<&'static str> = Some("Audio metering plugin");
    const CLAP_MANUAL_URL: Option<&'static str> = None;
    const CLAP_SUPPORT_URL: Option<&'static str> = None;
    const CLAP_FEATURES: &'static [ClapFeature] =
        &[ClapFeature::AudioEffect, ClapFeature::Utility, ClapFeature::Stereo];
}

impl Vst3Plugin for FtsMeter {
    const VST3_CLASS_ID: [u8; 16] = *b"FtsMeterPlugin01";
    const VST3_SUBCATEGORIES: &'static [Vst3SubCategory] =
        &[Vst3SubCategory::Fx, Vst3SubCategory::Analyzer];
}

nih_export_clap!(FtsMeter);
nih_export_vst3!(FtsMeter);
