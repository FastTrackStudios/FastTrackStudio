//! Pro-Q 4 compatible EQ parameters.
//!
//! This module defines all parameters needed for a professional parametric EQ,
//! structured to be 1:1 compatible with FabFilter Pro-Q 4 for preset mapping.
//!
//! # Parameter Organization
//!
//! Parameters are organized into:
//! - **Band Parameters**: Per-band EQ controls (up to 24 bands)
//! - **Dynamics Parameters**: Per-band dynamic EQ settings
//! - **Spectral Parameters**: Per-band spectral dynamics
//! - **Side-Chain Parameters**: Per-band side-chain configuration
//! - **Global Parameters**: Processing mode, output, character
//! - **Analyzer Parameters**: Spectrum display settings

use serde::{Deserialize, Serialize};

/// Maximum number of EQ bands.
pub const MAX_BANDS: usize = 24;

/// Complete EQ state with all parameters.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EqParams {
    /// Per-band parameters (up to 24 bands).
    pub bands: [BandParams; MAX_BANDS],
    /// Global processing parameters.
    pub global: GlobalParams,
    /// Analyzer display parameters.
    pub analyzer: AnalyzerParams,
}

impl Default for EqParams {
    fn default() -> Self {
        Self {
            bands: std::array::from_fn(|_| BandParams::default()),
            global: GlobalParams::default(),
            analyzer: AnalyzerParams::default(),
        }
    }
}

// =============================================================================
// Band Parameters
// =============================================================================

/// Per-band EQ parameters.
///
/// Maps to Pro-Q 4 band parameters:
/// - Band N Used
/// - Band N Enabled
/// - Band N Frequency
/// - Band N Gain
/// - Band N Q
/// - Band N Shape
/// - Band N Slope
/// - Band N Stereo Placement
/// - Band N Speakers (for surround)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BandParams {
    /// Whether this band slot is used (Unused/Used).
    pub used: bool,
    /// Whether the band is enabled (Enabled/Bypassed).
    pub enabled: bool,
    /// Center frequency in Hz (10 Hz - 30,000 Hz).
    pub frequency: f32,
    /// Gain in dB (-30.0 to +30.0 dB).
    pub gain: f32,
    /// Q factor / bandwidth (0.025 to 40.0, default 1.0).
    pub q: f32,
    /// Filter shape type.
    pub shape: BandShape,
    /// Filter slope in dB/octave (0 to 96, or Brickwall).
    pub slope: FilterSlope,
    /// Stereo placement mode.
    pub stereo_placement: StereoPlacement,
    /// Speaker selection for surround (bitmask).
    pub speakers: SpeakerSelection,
    /// Dynamic EQ parameters.
    pub dynamics: DynamicsParams,
    /// Side-chain parameters.
    pub side_chain: SideChainParams,
    /// Spectral dynamics parameters.
    pub spectral: SpectralParams,
}

impl Default for BandParams {
    fn default() -> Self {
        Self {
            used: false,
            enabled: true,
            frequency: 1000.0,
            gain: 0.0,
            q: 1.0,
            shape: BandShape::Bell,
            slope: FilterSlope::Db12,
            stereo_placement: StereoPlacement::Stereo,
            speakers: SpeakerSelection::all_except_lfe(),
            dynamics: DynamicsParams::default(),
            side_chain: SideChainParams::default(),
            spectral: SpectralParams::default(),
        }
    }
}

impl BandParams {
    /// Create a new band at the given frequency.
    #[must_use]
    pub fn at_frequency(freq: f32) -> Self {
        Self {
            used: true,
            frequency: freq.clamp(10.0, 30000.0),
            ..Default::default()
        }
    }

    /// Set frequency with validation.
    pub fn set_frequency(&mut self, freq: f32) {
        self.frequency = freq.clamp(10.0, 30000.0);
    }

    /// Set gain with validation.
    pub fn set_gain(&mut self, gain: f32) {
        self.gain = gain.clamp(-30.0, 30.0);
    }

    /// Set Q with validation.
    pub fn set_q(&mut self, q: f32) {
        self.q = q.clamp(0.025, 40.0);
    }
}

/// Filter shape/type.
///
/// Maps to Pro-Q 4 "Band N Shape" parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BandShape {
    /// Bell/Peaking filter.
    #[default]
    Bell,
    /// Low shelf filter.
    LowShelf,
    /// Low cut/high pass filter.
    LowCut,
    /// High shelf filter.
    HighShelf,
    /// High cut/low pass filter.
    HighCut,
    /// Notch filter.
    Notch,
    /// Band pass filter.
    BandPass,
    /// Tilt shelf filter.
    TiltShelf,
    /// Flat tilt filter.
    FlatTilt,
    /// All pass filter (phase shift only).
    AllPass,
}

impl BandShape {
    /// Get all available shapes.
    pub const ALL: [BandShape; 10] = [
        BandShape::Bell,
        BandShape::LowShelf,
        BandShape::LowCut,
        BandShape::HighShelf,
        BandShape::HighCut,
        BandShape::Notch,
        BandShape::BandPass,
        BandShape::TiltShelf,
        BandShape::FlatTilt,
        BandShape::AllPass,
    ];

    /// Whether this shape uses gain parameter.
    #[must_use]
    pub const fn uses_gain(&self) -> bool {
        matches!(
            self,
            BandShape::Bell
                | BandShape::LowShelf
                | BandShape::HighShelf
                | BandShape::TiltShelf
                | BandShape::FlatTilt
        )
    }

    /// Whether this shape supports dynamic EQ.
    #[must_use]
    pub const fn supports_dynamics(&self) -> bool {
        matches!(
            self,
            BandShape::Bell
                | BandShape::LowShelf
                | BandShape::HighShelf
                | BandShape::TiltShelf
                | BandShape::FlatTilt
        )
    }

    /// Get the minimum slope for this shape.
    #[must_use]
    pub const fn min_slope_db_oct(&self) -> f32 {
        match self {
            BandShape::Bell | BandShape::Notch => 12.0,
            BandShape::LowCut | BandShape::HighCut | BandShape::BandPass => 0.0,
            _ => 6.0,
        }
    }

    /// Display name.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            BandShape::Bell => "Bell",
            BandShape::LowShelf => "Low Shelf",
            BandShape::LowCut => "Low Cut",
            BandShape::HighShelf => "High Shelf",
            BandShape::HighCut => "High Cut",
            BandShape::Notch => "Notch",
            BandShape::BandPass => "Band Pass",
            BandShape::TiltShelf => "Tilt Shelf",
            BandShape::FlatTilt => "Flat Tilt",
            BandShape::AllPass => "All Pass",
        }
    }
}

/// Filter slope in dB/octave.
///
/// Maps to Pro-Q 4 "Band N Slope" parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum FilterSlope {
    /// 6 dB/octave (1st order).
    Db6,
    /// 12 dB/octave (2nd order).
    #[default]
    Db12,
    /// 18 dB/octave (3rd order).
    Db18,
    /// 24 dB/octave (4th order).
    Db24,
    /// 30 dB/octave.
    Db30,
    /// 36 dB/octave (6th order).
    Db36,
    /// 48 dB/octave (8th order).
    Db48,
    /// 60 dB/octave.
    Db60,
    /// 72 dB/octave (12th order).
    Db72,
    /// 96 dB/octave (16th order).
    Db96,
    /// Brickwall (only for Low Cut / High Cut).
    Brickwall,
    /// Custom fractional slope.
    Custom(u8), // Stores slope * 2 for 0.5 dB precision
}

impl FilterSlope {
    /// Get slope value in dB/octave.
    #[must_use]
    pub const fn db_per_octave(&self) -> f32 {
        match self {
            FilterSlope::Db6 => 6.0,
            FilterSlope::Db12 => 12.0,
            FilterSlope::Db18 => 18.0,
            FilterSlope::Db24 => 24.0,
            FilterSlope::Db30 => 30.0,
            FilterSlope::Db36 => 36.0,
            FilterSlope::Db48 => 48.0,
            FilterSlope::Db60 => 60.0,
            FilterSlope::Db72 => 72.0,
            FilterSlope::Db96 => 96.0,
            FilterSlope::Brickwall => f32::INFINITY,
            FilterSlope::Custom(v) => *v as f32 * 0.5,
        }
    }

    /// Get the filter order (number of biquad stages).
    #[must_use]
    pub const fn order(&self) -> usize {
        match self {
            FilterSlope::Db6 => 1,
            FilterSlope::Db12 => 2,
            FilterSlope::Db18 => 3,
            FilterSlope::Db24 => 4,
            FilterSlope::Db30 => 5,
            FilterSlope::Db36 => 6,
            FilterSlope::Db48 => 8,
            FilterSlope::Db60 => 10,
            FilterSlope::Db72 => 12,
            FilterSlope::Db96 => 16,
            FilterSlope::Brickwall => 16, // Uses FIR
            FilterSlope::Custom(v) => {
                let order = (*v as usize) / 12;
                if order < 1 { 1 } else { order }
            }
        }
    }
}

/// Stereo placement mode.
///
/// Maps to Pro-Q 4 "Band N Stereo Placement" parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum StereoPlacement {
    /// Process both channels equally.
    #[default]
    Stereo,
    /// Process only left channel.
    Left,
    /// Process only right channel.
    Right,
    /// Process mid (center) signal.
    Mid,
    /// Process side (stereo width) signal.
    Side,
}

impl StereoPlacement {
    /// Display name.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            StereoPlacement::Stereo => "Stereo",
            StereoPlacement::Left => "Left",
            StereoPlacement::Right => "Right",
            StereoPlacement::Mid => "Mid",
            StereoPlacement::Side => "Side",
        }
    }
}

/// Speaker selection for surround (bitmask).
///
/// Maps to Pro-Q 4 "Band N Speakers" parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpeakerSelection(pub u16);

impl SpeakerSelection {
    pub const L: u16 = 1 << 0;
    pub const R: u16 = 1 << 1;
    pub const C: u16 = 1 << 2;
    pub const LFE: u16 = 1 << 3;
    pub const LS: u16 = 1 << 4;
    pub const RS: u16 = 1 << 5;
    pub const LRS: u16 = 1 << 6;
    pub const RRS: u16 = 1 << 7;
    pub const TFL: u16 = 1 << 8;
    pub const TFR: u16 = 1 << 9;
    pub const TRL: u16 = 1 << 10;
    pub const TRR: u16 = 1 << 11;
    pub const TSL: u16 = 1 << 12;
    pub const TSR: u16 = 1 << 13;

    /// All channels except LFE.
    #[must_use]
    pub const fn all_except_lfe() -> Self {
        Self(0xFFFF & !Self::LFE)
    }

    /// All channels.
    #[must_use]
    pub const fn all() -> Self {
        Self(0xFFFF)
    }

    /// Check if a speaker is selected.
    #[must_use]
    pub const fn contains(&self, speaker: u16) -> bool {
        (self.0 & speaker) != 0
    }
}

impl Default for SpeakerSelection {
    fn default() -> Self {
        Self::all_except_lfe()
    }
}

// =============================================================================
// Dynamics Parameters
// =============================================================================

/// Dynamic EQ parameters.
///
/// Maps to Pro-Q 4:
/// - Band N Dynamic Range
/// - Band N Dynamics Enabled
/// - Band N Dynamics Auto
/// - Band N Threshold
/// - Band N Attack
/// - Band N Release
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DynamicsParams {
    /// Whether dynamics processing is enabled.
    pub enabled: DynamicsEnabled,
    /// Dynamic range in dB (-30 to +30).
    /// Positive = expansion, negative = compression.
    pub range: f32,
    /// Whether to use auto threshold/attack/release.
    pub auto_mode: DynamicsAutoMode,
    /// Threshold in dB (-60 to 0), or Auto.
    pub threshold: f32,
    /// Attack time (0.0 = auto, otherwise 0-100% of range).
    pub attack: f32,
    /// Release time (0.0 = auto, otherwise 0-100% of range).
    pub release: f32,
}

impl Default for DynamicsParams {
    fn default() -> Self {
        Self {
            enabled: DynamicsEnabled::Disabled,
            range: 0.0,
            auto_mode: DynamicsAutoMode::Auto,
            threshold: -20.0,
            attack: 50.0, // 50% = auto
            release: 50.0,
        }
    }
}

impl DynamicsParams {
    /// Whether dynamics are actively processing.
    #[must_use]
    pub fn is_active(&self) -> bool {
        self.enabled == DynamicsEnabled::Enabled && self.range.abs() > 0.01
    }
}

/// Dynamics enabled state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum DynamicsEnabled {
    /// Dynamics disabled.
    #[default]
    Disabled,
    /// Dynamics enabled.
    Enabled,
}

/// Dynamics auto mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum DynamicsAutoMode {
    /// Automatic threshold, attack, release.
    #[default]
    Auto,
    /// Manual settings.
    Manual,
}

// =============================================================================
// Side-Chain Parameters
// =============================================================================

/// Side-chain parameters for dynamic EQ triggering.
///
/// Maps to Pro-Q 4:
/// - Band N External Side Chain
/// - Band N Side Chain Filtering
/// - Band N Side Chain Low Frequency
/// - Band N Side Chain High Frequency
/// - Band N Side Chain Audition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SideChainParams {
    /// External side-chain source.
    pub external: ExternalSideChain,
    /// Side-chain filtering mode.
    pub filtering: SideChainFiltering,
    /// Low frequency for "Free" filtering mode (10 Hz - 30 kHz).
    pub low_freq: f32,
    /// High frequency for "Free" filtering mode (10 Hz - 30 kHz).
    pub high_freq: f32,
    /// Whether to audition the side-chain signal.
    pub audition: bool,
}

impl Default for SideChainParams {
    fn default() -> Self {
        Self {
            external: ExternalSideChain::Off,
            filtering: SideChainFiltering::Band,
            low_freq: 100.0,
            high_freq: 3000.0,
            audition: false,
        }
    }
}

/// External side-chain source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ExternalSideChain {
    /// Use internal signal.
    #[default]
    Off,
    /// Use external side-chain input.
    On,
}

/// Side-chain filtering mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum SideChainFiltering {
    /// Use the band's frequency as trigger.
    #[default]
    Band,
    /// Use custom low/high frequency range.
    Free,
}

// =============================================================================
// Spectral Parameters
// =============================================================================

/// Spectral dynamics parameters.
///
/// Maps to Pro-Q 4:
/// - Band N Spectral Enabled
/// - Band N Spectral Density
/// - Band N Spectral Tilt
/// - Band N Solo
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpectralParams {
    /// Whether spectral processing is enabled.
    pub enabled: SpectralEnabled,
    /// Spectral density (0-100%).
    /// Lower = wider bands, higher = narrow/specific.
    pub density: f32,
    /// Spectral tilt on/off.
    pub tilt: bool,
    /// Solo this band.
    pub solo: BandSolo,
}

impl Default for SpectralParams {
    fn default() -> Self {
        Self {
            enabled: SpectralEnabled::Disabled,
            density: 50.0,
            tilt: true, // On by default for new spectral bands
            solo: BandSolo::Disabled,
        }
    }
}

/// Spectral enabled state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum SpectralEnabled {
    /// Spectral processing disabled.
    #[default]
    Disabled,
    /// Spectral processing enabled.
    Enabled,
}

/// Band solo state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BandSolo {
    /// Solo disabled.
    #[default]
    Disabled,
    /// Solo enabled.
    Enabled,
}

// =============================================================================
// Global Parameters
// =============================================================================

/// Global EQ parameters.
///
/// Maps to Pro-Q 4:
/// - Processing Mode
/// - Processing Resolution
/// - Character
/// - Gain Scale
/// - Output Level
/// - Output Pan
/// - Output Pan Mode
/// - Bypass
/// - Output Invert Phase
/// - Auto Gain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobalParams {
    /// Processing mode (phase behavior).
    pub processing_mode: ProcessingMode,
    /// Processing resolution for linear phase.
    pub processing_resolution: ProcessingResolution,
    /// Character mode (Clean, Subtle, Warm).
    pub character: CharacterMode,
    /// Gain scale multiplier (0-200%).
    pub gain_scale: f32,
    /// Output level in dB (-30 to +30).
    pub output_level: f32,
    /// Output pan (-100 to +100, 0 = center).
    pub output_pan: f32,
    /// Output pan mode.
    pub output_pan_mode: OutputPanMode,
    /// Global bypass.
    pub bypass: BypassState,
    /// Invert output phase.
    pub invert_phase: InvertPhaseMode,
    /// Auto gain compensation.
    pub auto_gain: AutoGainMode,
    /// Solo gain adjustment (dB).
    pub solo_gain: f32,
    /// Receive MIDI for frequency targeting.
    pub receive_midi: bool,
}

impl Default for GlobalParams {
    fn default() -> Self {
        Self {
            processing_mode: ProcessingMode::ZeroLatency,
            processing_resolution: ProcessingResolution::Medium,
            character: CharacterMode::Clean,
            gain_scale: 100.0,
            output_level: 0.0,
            output_pan: 0.0,
            output_pan_mode: OutputPanMode::LeftRight,
            bypass: BypassState::NotBypassed,
            invert_phase: InvertPhaseMode::Normal,
            auto_gain: AutoGainMode::Off,
            solo_gain: 0.0,
            receive_midi: true,
        }
    }
}

/// Processing mode.
///
/// Maps to Pro-Q 4 "Processing Mode".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ProcessingMode {
    /// Zero latency, minimum phase.
    #[default]
    ZeroLatency,
    /// Natural phase mode.
    NaturalPhase,
    /// Linear phase mode.
    LinearPhase,
}

impl ProcessingMode {
    /// Display name.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            ProcessingMode::ZeroLatency => "Zero Latency",
            ProcessingMode::NaturalPhase => "Natural Phase",
            ProcessingMode::LinearPhase => "Linear Phase",
        }
    }
}

/// Processing resolution for linear phase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ProcessingResolution {
    /// Low resolution (less latency).
    Low,
    /// Medium resolution.
    #[default]
    Medium,
    /// High resolution.
    High,
    /// Very high resolution.
    VeryHigh,
    /// Maximum resolution (most latency).
    Maximum,
}

impl ProcessingResolution {
    /// Display name.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            ProcessingResolution::Low => "Low",
            ProcessingResolution::Medium => "Medium",
            ProcessingResolution::High => "High",
            ProcessingResolution::VeryHigh => "Very High",
            ProcessingResolution::Maximum => "Maximum",
        }
    }
}

/// Character mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum CharacterMode {
    /// Clean, transparent processing.
    #[default]
    Clean,
    /// Subtle analog-like warmth.
    Subtle,
    /// Warm, vintage character.
    Warm,
}

impl CharacterMode {
    /// Display name.
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            CharacterMode::Clean => "Clean",
            CharacterMode::Subtle => "Subtle",
            CharacterMode::Warm => "Warm",
        }
    }
}

/// Output pan mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum OutputPanMode {
    /// Left/Right panning.
    #[default]
    LeftRight,
    /// Mid/Side balance.
    MidSide,
}

/// Bypass state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BypassState {
    /// Not bypassed.
    #[default]
    NotBypassed,
    /// Bypassed.
    Bypassed,
}

/// Invert phase mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum InvertPhaseMode {
    /// Normal phase.
    #[default]
    Normal,
    /// Inverted phase.
    Inverted,
}

/// Auto gain mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AutoGainMode {
    /// Auto gain off.
    #[default]
    Off,
    /// Auto gain on.
    On,
}

// =============================================================================
// Band Status (for parameter mapping)
// =============================================================================

/// Band status for host parameter mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BandStatus {
    /// Band slot is unused.
    #[default]
    Unused,
    /// Band slot is used.
    Used,
}

/// Channel mode (legacy compatibility).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ChannelMode {
    /// Stereo processing.
    #[default]
    Stereo,
    /// Left only.
    Left,
    /// Right only.
    Right,
    /// Mid only.
    Mid,
    /// Side only.
    Side,
}

// =============================================================================
// Analyzer Parameters
// =============================================================================

/// Spectrum analyzer parameters.
///
/// Maps to Pro-Q 4 analyzer settings.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalyzerParams {
    /// Show pre-EQ spectrum.
    pub show_pre: bool,
    /// Show post-EQ spectrum.
    pub show_post: bool,
    /// Show external spectrum.
    pub show_external: bool,
    /// External spectrum channel (-1 = none).
    pub external_channel: i8,
    /// Display range in dB (6, 12, 30, 90).
    pub range_db: AnalyzerRange,
    /// Analyzer resolution.
    pub resolution: AnalyzerResolution,
    /// Analyzer speed.
    pub speed: AnalyzerSpeed,
    /// Analyzer tilt in dB/octave.
    pub tilt: AnalyzerTilt,
    /// Freeze analyzer display.
    pub freeze: bool,
    /// Show collision detection.
    pub show_collisions: bool,
    /// Enable spectrum grab.
    pub spectrum_grab: bool,
    /// Display range (3, 6, 12, 30 dB).
    pub display_range: DisplayRange,
    /// Per-band spectral tilt settings.
    pub band_spectral_tilt: [bool; MAX_BANDS],
}

impl Default for AnalyzerParams {
    fn default() -> Self {
        Self {
            show_pre: true,
            show_post: true,
            show_external: false,
            external_channel: -1,
            range_db: AnalyzerRange::Db90,
            resolution: AnalyzerResolution::High,
            speed: AnalyzerSpeed::Medium,
            tilt: AnalyzerTilt::Db4_5,
            freeze: false,
            show_collisions: true,
            spectrum_grab: true,
            display_range: DisplayRange::Db12,
            band_spectral_tilt: [false; MAX_BANDS],
        }
    }
}

/// Analyzer dB range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AnalyzerRange {
    /// 60 dB range.
    Db60,
    /// 72 dB range.
    Db72,
    /// 90 dB range.
    #[default]
    Db90,
    /// 96 dB range.
    Db96,
}

impl AnalyzerRange {
    /// Get the dB value.
    #[must_use]
    pub const fn db(&self) -> f32 {
        match self {
            AnalyzerRange::Db60 => 60.0,
            AnalyzerRange::Db72 => 72.0,
            AnalyzerRange::Db90 => 90.0,
            AnalyzerRange::Db96 => 96.0,
        }
    }
}

/// Analyzer resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AnalyzerResolution {
    /// Low resolution.
    Low,
    /// Medium resolution.
    Medium,
    /// High resolution.
    #[default]
    High,
    /// Maximum resolution.
    Maximum,
}

/// Analyzer speed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AnalyzerSpeed {
    /// Very slow decay.
    VerySlow,
    /// Slow decay.
    Slow,
    /// Medium decay.
    #[default]
    Medium,
    /// Fast decay.
    Fast,
    /// Very fast decay.
    VeryFast,
}

/// Analyzer tilt in dB/octave.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum AnalyzerTilt {
    /// 0 dB/oct (flat).
    Db0,
    /// 1.5 dB/oct.
    Db1_5,
    /// 3 dB/oct.
    Db3,
    /// 4.5 dB/oct.
    #[default]
    Db4_5,
    /// 6 dB/oct.
    Db6,
}

impl AnalyzerTilt {
    /// Get the dB/octave value.
    #[must_use]
    pub const fn db_per_octave(&self) -> f32 {
        match self {
            AnalyzerTilt::Db0 => 0.0,
            AnalyzerTilt::Db1_5 => 1.5,
            AnalyzerTilt::Db3 => 3.0,
            AnalyzerTilt::Db4_5 => 4.5,
            AnalyzerTilt::Db6 => 6.0,
        }
    }
}

/// Display range for EQ curve.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum DisplayRange {
    /// 3 dB range (mastering).
    Db3,
    /// 6 dB range (mastering).
    Db6,
    /// 12 dB range (mixing).
    #[default]
    Db12,
    /// 30 dB range (extreme).
    Db30,
}

impl DisplayRange {
    /// Get the dB value.
    #[must_use]
    pub const fn db(&self) -> f32 {
        match self {
            DisplayRange::Db3 => 3.0,
            DisplayRange::Db6 => 6.0,
            DisplayRange::Db12 => 12.0,
            DisplayRange::Db30 => 30.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_eq_params() {
        let params = EqParams::default();
        assert_eq!(params.bands.len(), MAX_BANDS);
        assert!(!params.bands[0].used);
        assert_eq!(params.global.output_level, 0.0);
    }

    #[test]
    fn band_frequency_clamping() {
        let mut band = BandParams::default();
        band.set_frequency(5.0);
        assert_eq!(band.frequency, 10.0);
        band.set_frequency(50000.0);
        assert_eq!(band.frequency, 30000.0);
    }

    #[test]
    fn band_shape_properties() {
        assert!(BandShape::Bell.uses_gain());
        assert!(!BandShape::Notch.uses_gain());
        assert!(BandShape::Bell.supports_dynamics());
        assert!(!BandShape::AllPass.supports_dynamics());
    }

    #[test]
    fn filter_slope_values() {
        assert_eq!(FilterSlope::Db12.db_per_octave(), 12.0);
        assert_eq!(FilterSlope::Db96.order(), 16);
    }
}
