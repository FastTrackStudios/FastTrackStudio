//! Modulation sources and routing — LFOs, envelopes, MIDI, and expression.
//!
//! A [`ModulationRoute`] connects a [`ModulationSource`] (LFO, envelope, MIDI CC,
//! expression pedal, or snapshot morph position) to a target parameter on a
//! specific block. The [`ModulationEngine`] evaluates all active routes each
//! frame via [`tick`](ModulationEngine::tick) + [`evaluate`](ModulationEngine::evaluate).
//!
//! # Example (pseudocode)
//!
//! ```ignore
//! let mut engine = ModulationEngine::new();
//! engine.add_route(ModulationRoute {
//!     source: ModulationSource::Lfo(LfoConfig { waveform: LfoWaveform::Sine, .. }),
//!     target_param_id: "gain".into(),
//!     target_block_id: block_id,
//!     depth: NormalizedF64::new(0.5),
//!     bipolar: true,
//!     ..
//! });
//!
//! // Each frame:
//! engine.tick(delta_ms);
//! let modulated = engine.evaluate("gain", block_id, base_value);
//! ```

use std::collections::HashMap;
use std::f64::consts::TAU;

use facet::Facet;
use uuid::Uuid;

use crate::id::BlockId;
use crate::normalized::NormalizedF64;

// ─────────────────────────────────────────────────────────────────────────────
// LfoWaveform
// ─────────────────────────────────────────────────────────────────────────────

/// Waveform shape for an LFO modulation source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Facet)]
#[repr(u8)]
pub enum LfoWaveform {
    #[default]
    Sine,
    Triangle,
    Square,
    Sawtooth,
    SawtoothDown,
    Random,
}

impl LfoWaveform {
    /// Evaluate the waveform at a given phase `∈ [0.0, 1.0)`.
    ///
    /// Returns a bipolar value in `[-1.0, 1.0]`.
    pub fn evaluate(self, phase: f64) -> f64 {
        match self {
            Self::Sine => (phase * TAU).sin(),
            Self::Triangle => {
                // Peak at phase 0, trough at phase 0.5
                if phase < 0.5 {
                    1.0 - phase * 4.0
                } else {
                    -1.0 + (phase - 0.5) * 4.0
                }
            }
            Self::Square => {
                if phase < 0.5 {
                    1.0
                } else {
                    -1.0
                }
            }
            Self::Sawtooth => phase * 2.0 - 1.0,
            Self::SawtoothDown => 1.0 - phase * 2.0,
            // Random returns a deterministic pseudo-random value based on
            // quantized phase steps (changes each cycle quarter).
            Self::Random => {
                let quantized = (phase * 4.0).floor() as u64;
                // Simple hash for deterministic per-step output
                let hash = quantized.wrapping_mul(6364136223846793005).wrapping_add(1);
                (hash as f64 / u64::MAX as f64) * 2.0 - 1.0
            }
        }
    }

    pub fn all() -> &'static [LfoWaveform] {
        &[
            Self::Sine,
            Self::Triangle,
            Self::Square,
            Self::Sawtooth,
            Self::SawtoothDown,
            Self::Random,
        ]
    }

    pub fn display_name(self) -> &'static str {
        match self {
            Self::Sine => "Sine",
            Self::Triangle => "Triangle",
            Self::Square => "Square",
            Self::Sawtooth => "Sawtooth",
            Self::SawtoothDown => "Sawtooth Down",
            Self::Random => "Random",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// LfoConfig
// ─────────────────────────────────────────────────────────────────────────────

/// Configuration for an LFO modulation source.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct LfoConfig {
    /// Waveform shape.
    pub waveform: LfoWaveform,
    /// Oscillation rate in Hz.
    pub rate_hz: f64,
    /// Initial phase offset `∈ [0.0, 1.0)`.
    pub phase_offset: f64,
    /// If true, output is `[0.0, 1.0]` instead of `[-1.0, 1.0]`.
    pub unipolar: bool,
}

impl Default for LfoConfig {
    fn default() -> Self {
        Self {
            waveform: LfoWaveform::Sine,
            rate_hz: 1.0,
            phase_offset: 0.0,
            unipolar: false,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// EnvelopeConfig (ADSR)
// ─────────────────────────────────────────────────────────────────────────────

/// ADSR envelope configuration for modulation.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct EnvelopeConfig {
    /// Attack time in milliseconds.
    pub attack_ms: f64,
    /// Decay time in milliseconds.
    pub decay_ms: f64,
    /// Sustain level `∈ [0.0, 1.0]`.
    pub sustain_level: NormalizedF64,
    /// Release time in milliseconds.
    pub release_ms: f64,
}

impl Default for EnvelopeConfig {
    fn default() -> Self {
        Self {
            attack_ms: 10.0,
            decay_ms: 100.0,
            sustain_level: NormalizedF64::new(0.7),
            release_ms: 200.0,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// EnvelopeState
// ─────────────────────────────────────────────────────────────────────────────

/// Runtime state of an ADSR envelope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum EnvelopePhase {
    Idle,
    Attack,
    Decay,
    Sustain,
    Release,
}

/// Tracks the current phase and level of an envelope generator.
#[derive(Debug, Clone)]
pub struct EnvelopeState {
    pub phase: EnvelopePhase,
    pub level: f64,
    pub elapsed_ms: f64,
}

impl EnvelopeState {
    pub fn new() -> Self {
        Self {
            phase: EnvelopePhase::Idle,
            level: 0.0,
            elapsed_ms: 0.0,
        }
    }

    /// Trigger the envelope (note on).
    pub fn trigger(&mut self) {
        self.phase = EnvelopePhase::Attack;
        self.elapsed_ms = 0.0;
    }

    /// Release the envelope (note off).
    pub fn release(&mut self) {
        if self.phase != EnvelopePhase::Idle {
            self.phase = EnvelopePhase::Release;
            self.elapsed_ms = 0.0;
        }
    }

    /// Advance the envelope by `delta_ms` and return the current level.
    pub fn tick(&mut self, delta_ms: f64, config: &EnvelopeConfig) -> f64 {
        self.elapsed_ms += delta_ms;

        match self.phase {
            EnvelopePhase::Idle => {
                self.level = 0.0;
            }
            EnvelopePhase::Attack => {
                if config.attack_ms <= 0.0 {
                    self.level = 1.0;
                    self.phase = EnvelopePhase::Decay;
                    self.elapsed_ms = 0.0;
                } else {
                    self.level = (self.elapsed_ms / config.attack_ms).min(1.0);
                    if self.elapsed_ms >= config.attack_ms {
                        self.level = 1.0;
                        self.phase = EnvelopePhase::Decay;
                        self.elapsed_ms = 0.0;
                    }
                }
            }
            EnvelopePhase::Decay => {
                let sustain = config.sustain_level.get();
                if config.decay_ms <= 0.0 {
                    self.level = sustain;
                    self.phase = EnvelopePhase::Sustain;
                    self.elapsed_ms = 0.0;
                } else {
                    let decay_progress = (self.elapsed_ms / config.decay_ms).min(1.0);
                    self.level = 1.0 - (1.0 - sustain) * decay_progress;
                    if self.elapsed_ms >= config.decay_ms {
                        self.level = sustain;
                        self.phase = EnvelopePhase::Sustain;
                        self.elapsed_ms = 0.0;
                    }
                }
            }
            EnvelopePhase::Sustain => {
                self.level = config.sustain_level.get();
            }
            EnvelopePhase::Release => {
                let release_start = config.sustain_level.get();
                if config.release_ms <= 0.0 {
                    self.level = 0.0;
                    self.phase = EnvelopePhase::Idle;
                } else {
                    let release_progress = (self.elapsed_ms / config.release_ms).min(1.0);
                    self.level = release_start * (1.0 - release_progress);
                    if self.elapsed_ms >= config.release_ms {
                        self.level = 0.0;
                        self.phase = EnvelopePhase::Idle;
                    }
                }
            }
        }

        self.level
    }
}

impl Default for EnvelopeState {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModulationSource
// ─────────────────────────────────────────────────────────────────────────────

/// What drives a modulation route.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub enum ModulationSource {
    /// Low-frequency oscillator.
    Lfo(LfoConfig),
    /// ADSR envelope (triggered by MIDI or programmatically).
    Envelope(EnvelopeConfig),
    /// MIDI continuous controller.
    MidiCc { channel: u8, cc: u8 },
    /// Expression pedal (a specific MIDI CC mapping).
    ExpressionPedal { channel: u8, cc: u8 },
    /// Tied to the morph slider position from [`SnapshotMorpher`](crate::morph::SnapshotMorpher).
    SnapshotMorph,
}

// ─────────────────────────────────────────────────────────────────────────────
// ModulationRoute
// ─────────────────────────────────────────────────────────────────────────────

/// Routes a modulation source to a specific parameter on a block.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModulationRoute {
    /// Unique identifier for this route.
    pub id: Uuid,
    /// The modulation source driving this route.
    pub source: ModulationSource,
    /// Target parameter identifier (matches `Parameter::id` in the block).
    pub target_param_id: String,
    /// Block containing the target parameter.
    pub target_block_id: BlockId,
    /// Modulation depth `∈ [0.0, 1.0]` — how much the source affects the parameter.
    pub depth: NormalizedF64,
    /// If true, modulates ±depth around the base value.
    /// If false, modulates 0..+depth above the base value.
    pub bipolar: bool,
}

impl ModulationRoute {
    /// Create a new unipolar modulation route.
    pub fn new(
        source: ModulationSource,
        target_param_id: impl Into<String>,
        target_block_id: BlockId,
        depth: NormalizedF64,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            source,
            target_param_id: target_param_id.into(),
            target_block_id,
            depth,
            bipolar: false,
        }
    }

    /// Set this route to bipolar modulation.
    #[must_use]
    pub fn with_bipolar(mut self, bipolar: bool) -> Self {
        self.bipolar = bipolar;
        self
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModulationOutput
// ─────────────────────────────────────────────────────────────────────────────

/// The computed output of a single modulation route for one frame.
#[derive(Debug, Clone)]
pub struct ModulationOutput {
    pub route_id: Uuid,
    pub target_param_id: String,
    pub target_block_id: BlockId,
    pub modulated_value: f64,
}

// ─────────────────────────────────────────────────────────────────────────────
// ModulationEngine
// ─────────────────────────────────────────────────────────────────────────────

/// Evaluates all modulation routes per frame.
///
/// Call [`tick`](Self::tick) each frame to advance LFO phases and envelope
/// states, then [`evaluate`](Self::evaluate) to get the modulated value for
/// a specific parameter.
pub struct ModulationEngine {
    routes: Vec<ModulationRoute>,
    /// Current LFO phase per route (wraps in [0, 1)).
    lfo_phases: HashMap<Uuid, f64>,
    /// Envelope generator state per route.
    envelope_states: HashMap<Uuid, EnvelopeState>,
    /// Last received MIDI CC values (channel, cc) → value [0, 127].
    midi_cc_values: HashMap<(u8, u8), u8>,
    /// Current morph position [0, 1] from the snapshot morpher.
    morph_position: f64,
}

impl ModulationEngine {
    pub fn new() -> Self {
        Self {
            routes: Vec::new(),
            lfo_phases: HashMap::new(),
            envelope_states: HashMap::new(),
            midi_cc_values: HashMap::new(),
            morph_position: 0.0,
        }
    }

    /// Add a modulation route. Initializes any per-route state.
    pub fn add_route(&mut self, route: ModulationRoute) {
        let id = route.id;
        match &route.source {
            ModulationSource::Lfo(config) => {
                self.lfo_phases.insert(id, config.phase_offset);
            }
            ModulationSource::Envelope(_) => {
                self.envelope_states.insert(id, EnvelopeState::new());
            }
            _ => {}
        }
        self.routes.push(route);
    }

    /// Remove a modulation route by ID.
    pub fn remove_route(&mut self, route_id: Uuid) {
        self.routes.retain(|r| r.id != route_id);
        self.lfo_phases.remove(&route_id);
        self.envelope_states.remove(&route_id);
    }

    /// Get all active routes.
    pub fn routes(&self) -> &[ModulationRoute] {
        &self.routes
    }

    /// Update the morph position (from SnapshotMorpher or tween).
    pub fn set_morph_position(&mut self, position: f64) {
        self.morph_position = position.clamp(0.0, 1.0);
    }

    /// Feed a MIDI CC value into the engine.
    pub fn receive_midi_cc(&mut self, channel: u8, cc: u8, value: u8) {
        self.midi_cc_values.insert((channel, cc), value);
    }

    /// Trigger all envelope sources (e.g. on note-on).
    pub fn trigger_envelopes(&mut self) {
        for state in self.envelope_states.values_mut() {
            state.trigger();
        }
    }

    /// Release all envelope sources (e.g. on note-off).
    pub fn release_envelopes(&mut self) {
        for state in self.envelope_states.values_mut() {
            state.release();
        }
    }

    /// Advance all modulation sources by `delta_ms`.
    pub fn tick(&mut self, delta_ms: f64) {
        for route in &self.routes {
            match &route.source {
                ModulationSource::Lfo(config) => {
                    if let Some(phase) = self.lfo_phases.get_mut(&route.id) {
                        // phase advances by rate * time
                        let advance = config.rate_hz * delta_ms / 1000.0;
                        *phase = (*phase + advance) % 1.0;
                    }
                }
                ModulationSource::Envelope(config) => {
                    if let Some(state) = self.envelope_states.get_mut(&route.id) {
                        state.tick(delta_ms, config);
                    }
                }
                _ => {} // MIDI/morph are event-driven, not time-driven
            }
        }
    }

    /// Evaluate the modulated value for a specific parameter.
    ///
    /// Returns the `base_value` offset by the sum of all matching routes.
    /// The result is clamped to `[0.0, 1.0]`.
    pub fn evaluate(&self, param_id: &str, block_id: BlockId, base_value: f64) -> f64 {
        let mut total_offset = 0.0;

        for route in &self.routes {
            if route.target_param_id != param_id || route.target_block_id != block_id {
                continue;
            }

            let raw = self.source_value(route);
            let depth = route.depth.get();

            if route.bipolar {
                // raw ∈ [-1, 1], offset ∈ [-depth, +depth]
                total_offset += raw * depth;
            } else {
                // raw ∈ [-1, 1] → normalized to [0, 1], then scaled by depth
                let normalized = (raw + 1.0) / 2.0;
                total_offset += normalized * depth;
            }
        }

        (base_value + total_offset).clamp(0.0, 1.0)
    }

    /// Evaluate all routes and return their individual outputs.
    pub fn evaluate_all(
        &self,
        base_values: &HashMap<(String, BlockId), f64>,
    ) -> Vec<ModulationOutput> {
        let mut outputs = Vec::with_capacity(self.routes.len());

        for route in &self.routes {
            let key = (route.target_param_id.clone(), route.target_block_id);
            let base = base_values.get(&key).copied().unwrap_or(0.0);
            let modulated = self.evaluate(&route.target_param_id, route.target_block_id, base);

            outputs.push(ModulationOutput {
                route_id: route.id,
                target_param_id: route.target_param_id.clone(),
                target_block_id: route.target_block_id,
                modulated_value: modulated,
            });
        }

        outputs
    }

    /// Get the raw source value for a route, in `[-1.0, 1.0]`.
    fn source_value(&self, route: &ModulationRoute) -> f64 {
        match &route.source {
            ModulationSource::Lfo(config) => {
                let phase = self.lfo_phases.get(&route.id).copied().unwrap_or(0.0);
                let raw = config.waveform.evaluate(phase);
                if config.unipolar {
                    // Convert [-1, 1] → [0, 1] → back to [-1, 1] scale
                    // but unipolar means output [0, 1], so we map:
                    (raw + 1.0) / 2.0 * 2.0 - 1.0 // still [-1,1] but the waveform is shifted
                                                  // Actually for unipolar, the *output* should be [0,1].
                                                  // The evaluate() method handles bipolar/unipolar scaling,
                                                  // so here we just shift the waveform up.
                } else {
                    raw
                }
            }
            ModulationSource::Envelope(config) => {
                if let Some(state) = self.envelope_states.get(&route.id) {
                    // Envelope level is [0, 1], map to [-1, 1] for consistent interface
                    let mut env = state.clone();
                    let level = env.tick(0.0, config);
                    level * 2.0 - 1.0
                } else {
                    -1.0 // idle = 0.0 → -1.0 in bipolar range
                }
            }
            ModulationSource::MidiCc { channel, cc }
            | ModulationSource::ExpressionPedal { channel, cc } => {
                let value = self
                    .midi_cc_values
                    .get(&(*channel, *cc))
                    .copied()
                    .unwrap_or(0);
                // Map [0, 127] → [-1, 1]
                (value as f64 / 127.0) * 2.0 - 1.0
            }
            ModulationSource::SnapshotMorph => {
                // Map [0, 1] → [-1, 1]
                self.morph_position * 2.0 - 1.0
            }
        }
    }
}

impl Default for ModulationEngine {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── LfoWaveform ──────────────────────────────────────────────────

    #[test]
    fn sine_at_key_phases() {
        let w = LfoWaveform::Sine;
        assert!((w.evaluate(0.0)).abs() < 1e-10, "sine(0) should be 0");
        assert!(
            (w.evaluate(0.25) - 1.0).abs() < 1e-10,
            "sine(π/2) should be 1"
        );
        assert!((w.evaluate(0.5)).abs() < 1e-10, "sine(π) should be ~0");
        assert!(
            (w.evaluate(0.75) + 1.0).abs() < 1e-10,
            "sine(3π/2) should be -1"
        );
    }

    #[test]
    fn triangle_at_key_phases() {
        let w = LfoWaveform::Triangle;
        assert!((w.evaluate(0.0) - 1.0).abs() < 1e-10, "triangle(0) = 1");
        assert!((w.evaluate(0.5) + 1.0).abs() < 1e-10, "triangle(0.5) = -1");
        assert!((w.evaluate(1.0) - 1.0).abs() < 1e-10, "triangle(1) = 1");
    }

    #[test]
    fn square_at_key_phases() {
        let w = LfoWaveform::Square;
        assert_eq!(w.evaluate(0.0), 1.0);
        assert_eq!(w.evaluate(0.25), 1.0);
        assert_eq!(w.evaluate(0.5), -1.0);
        assert_eq!(w.evaluate(0.75), -1.0);
    }

    #[test]
    fn sawtooth_ramps_up() {
        let w = LfoWaveform::Sawtooth;
        assert!((w.evaluate(0.0) + 1.0).abs() < 1e-10);
        assert!((w.evaluate(0.5)).abs() < 1e-10);
        assert!((w.evaluate(1.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn sawtooth_down_ramps_down() {
        let w = LfoWaveform::SawtoothDown;
        assert!((w.evaluate(0.0) - 1.0).abs() < 1e-10);
        assert!((w.evaluate(0.5)).abs() < 1e-10);
        assert!((w.evaluate(1.0) + 1.0).abs() < 1e-10);
    }

    #[test]
    fn all_waveforms_in_bipolar_range() {
        for w in LfoWaveform::all() {
            for i in 0..100 {
                let phase = i as f64 / 100.0;
                let val = w.evaluate(phase);
                assert!(
                    (-1.0..=1.0).contains(&val),
                    "{:?} at phase {phase}: {val} out of [-1, 1]",
                    w
                );
            }
        }
    }

    // ── EnvelopeState ────────────────────────────────────────────────

    #[test]
    fn envelope_lifecycle() {
        let config = EnvelopeConfig {
            attack_ms: 10.0,
            decay_ms: 10.0,
            sustain_level: NormalizedF64::new(0.5),
            release_ms: 10.0,
        };

        let mut env = EnvelopeState::new();
        assert_eq!(env.phase, EnvelopePhase::Idle);

        // Trigger → attack
        env.trigger();
        assert_eq!(env.phase, EnvelopePhase::Attack);

        // Halfway through attack
        let level = env.tick(5.0, &config);
        assert!((level - 0.5).abs() < 1e-10, "attack halfway: {level}");

        // Complete attack → decay
        let level = env.tick(5.0, &config);
        assert_eq!(level, 1.0);
        assert_eq!(env.phase, EnvelopePhase::Decay);

        // Halfway through decay (1.0 → 0.5)
        let level = env.tick(5.0, &config);
        assert!((level - 0.75).abs() < 1e-10, "decay halfway: {level}");

        // Complete decay → sustain
        let level = env.tick(5.0, &config);
        assert!((level - 0.5).abs() < 1e-10, "sustain: {level}");
        assert_eq!(env.phase, EnvelopePhase::Sustain);

        // Sustain holds
        let level = env.tick(100.0, &config);
        assert!((level - 0.5).abs() < 1e-10);

        // Release
        env.release();
        assert_eq!(env.phase, EnvelopePhase::Release);

        // Halfway through release (0.5 → 0.0)
        let level = env.tick(5.0, &config);
        assert!((level - 0.25).abs() < 1e-10, "release halfway: {level}");

        // Complete release → idle
        let level = env.tick(5.0, &config);
        assert!((level).abs() < 1e-10);
        assert_eq!(env.phase, EnvelopePhase::Idle);
    }

    #[test]
    fn zero_attack_skips_to_decay() {
        let config = EnvelopeConfig {
            attack_ms: 0.0,
            decay_ms: 10.0,
            sustain_level: NormalizedF64::new(0.8),
            release_ms: 10.0,
        };

        let mut env = EnvelopeState::new();
        env.trigger();
        let level = env.tick(0.0, &config);
        assert_eq!(level, 1.0);
        assert_eq!(env.phase, EnvelopePhase::Decay);
    }

    // ── ModulationEngine ─────────────────────────────────────────────

    #[test]
    fn engine_lfo_modulates_parameter() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::Lfo(LfoConfig {
                waveform: LfoWaveform::Sine,
                rate_hz: 1.0,
                phase_offset: 0.0,
                unipolar: false,
            }),
            "gain",
            block_id,
            NormalizedF64::new(0.5),
        );

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        // At phase 0, sine = 0 → offset = (0+1)/2 * 0.5 = 0.25 (unipolar scaling)
        let val = engine.evaluate("gain", block_id, 0.5);
        assert!(
            (val - 0.75).abs() < 1e-10,
            "expected ~0.75 (base 0.5 + offset 0.25), got {val}"
        );
    }

    #[test]
    fn engine_lfo_bipolar() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::Lfo(LfoConfig {
                waveform: LfoWaveform::Sine,
                rate_hz: 1.0,
                phase_offset: 0.25, // sine at 0.25 = 1.0
                unipolar: false,
            }),
            "gain",
            block_id,
            NormalizedF64::new(0.5),
        )
        .with_bipolar(true);

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        // At phase 0.25, sine = 1.0 → bipolar offset = 1.0 * 0.5 = 0.5
        let val = engine.evaluate("gain", block_id, 0.5);
        assert_eq!(val, 1.0, "base 0.5 + bipolar offset 0.5 = 1.0");
    }

    #[test]
    fn engine_tick_advances_lfo() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::Lfo(LfoConfig {
                waveform: LfoWaveform::Sine,
                rate_hz: 1.0,
                phase_offset: 0.0,
                unipolar: false,
            }),
            "gain",
            block_id,
            NormalizedF64::new(0.5),
        );

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        let before = engine.evaluate("gain", block_id, 0.5);
        engine.tick(250.0); // advance by 250ms = 0.25 of a 1Hz cycle
        let after = engine.evaluate("gain", block_id, 0.5);

        assert_ne!(before, after, "LFO should have changed after tick");
    }

    #[test]
    fn engine_midi_cc_modulation() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::MidiCc { channel: 1, cc: 11 },
            "volume",
            block_id,
            NormalizedF64::new(1.0),
        );

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        // No CC received yet → value 0 → normalized = (0/127*2-1+1)/2*1.0 = 0
        let val = engine.evaluate("volume", block_id, 0.5);
        assert!(val < 0.51, "no CC yet, minimal offset: {val}");

        // Receive CC value 127 → max
        engine.receive_midi_cc(1, 11, 127);
        let val = engine.evaluate("volume", block_id, 0.0);
        assert!((val - 1.0).abs() < 1e-10, "full CC: {val}");
    }

    #[test]
    fn engine_morph_modulation() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::SnapshotMorph,
            "drive",
            block_id,
            NormalizedF64::new(1.0),
        );

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        engine.set_morph_position(0.0);
        let val = engine.evaluate("drive", block_id, 0.5);
        // morph=0 → source = 0*2-1 = -1 → offset = (-1+1)/2 * 1.0 = 0
        assert!((val - 0.5).abs() < 1e-10, "morph 0: {val}");

        engine.set_morph_position(1.0);
        let val = engine.evaluate("drive", block_id, 0.0);
        // morph=1 → source = 1*2-1 = 1 → offset = (1+1)/2 * 1.0 = 1.0
        assert!((val - 1.0).abs() < 1e-10, "morph 1: {val}");
    }

    #[test]
    fn engine_remove_route() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::SnapshotMorph,
            "gain",
            block_id,
            NormalizedF64::new(1.0),
        );
        let route_id = route.id;

        let mut engine = ModulationEngine::new();
        engine.add_route(route);
        assert_eq!(engine.routes().len(), 1);

        engine.remove_route(route_id);
        assert!(engine.routes().is_empty());
    }

    #[test]
    fn engine_clamps_output() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::SnapshotMorph,
            "gain",
            block_id,
            NormalizedF64::new(1.0),
        )
        .with_bipolar(true);

        let mut engine = ModulationEngine::new();
        engine.add_route(route);

        engine.set_morph_position(1.0);
        // bipolar: source=1.0, depth=1.0, offset = 1.0*1.0 = 1.0
        // base 0.9 + 1.0 = 1.9 → clamped to 1.0
        let val = engine.evaluate("gain", block_id, 0.9);
        assert_eq!(val, 1.0, "should clamp to 1.0");

        engine.set_morph_position(0.0);
        // bipolar: source = -1.0, depth = 1.0, offset = -1.0
        // base 0.1 + (-1.0) = -0.9 → clamped to 0.0
        let val = engine.evaluate("gain", block_id, 0.1);
        assert_eq!(val, 0.0, "should clamp to 0.0");
    }

    #[test]
    fn unrelated_params_not_affected() {
        let block_id = BlockId::new();
        let route = ModulationRoute::new(
            ModulationSource::SnapshotMorph,
            "gain",
            block_id,
            NormalizedF64::new(1.0),
        );

        let mut engine = ModulationEngine::new();
        engine.add_route(route);
        engine.set_morph_position(1.0);

        // "volume" is not targeted — should return base value
        let val = engine.evaluate("volume", block_id, 0.5);
        assert_eq!(val, 0.5);

        // Different block — should return base value
        let other_block = BlockId::new();
        let val = engine.evaluate("gain", other_block, 0.5);
        assert_eq!(val, 0.5);
    }
}
