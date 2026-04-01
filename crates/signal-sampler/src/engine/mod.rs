//! Audio engine — MIDI event processing and real-time sample rendering.
//!
//! # Design
//!
//! `SampleEngine` is a single playback instance for one sample library patch. It owns:
//! - the `PlayerPatch` (spec + sample index)
//! - a `SampleCache` for decoded WAV data
//! - a `VoicePool` of active voices
//! - `RrCounters` to cycle through round-robin slots
//!
//! One `SampleEngine` per MIDI track / instrument section is the expected usage.
//!
//! # CC1 dynamics
//!
//! Two voices are kept alive simultaneously for the current note:
//! - `SustainLo` — the softer adjacent layer
//! - `SustainHi` — the louder adjacent layer
//!
//! Their gains crossfade linearly as CC1 moves through the overlap region
//! between adjacent dynamic layers. Gain updates are ramped over a short
//! window to avoid zipper noise.
//!
//! # Legato
//!
//! When a second note arrives while a note is held, `SampleEngine` enters
//! `LegatoState::Pending` and counts down `frames_remaining` (derived from
//! the velocity-based pre-delay in the spec). When the countdown expires the
//! old sustain is faded out and the legato transition sample fires.

pub mod cache;
pub mod filter;
pub mod rr;
pub mod voice;

use std::collections::HashMap;

use crate::spec::{ArticulationKind, Cc1Layer};
use crate::PlayerPatch;
use cache::SampleCache;
use filter::BiquadFilter;
use rr::RrCounters;
use voice::{Voice, VoiceKind, VoicePool};

// ── Constants ─────────────────────────────────────────────────────────────────

/// CC1 gain ramp length (ms). Smooths dynamic crossfade to avoid clicks.
const CC1_RAMP_MS: u32 = 20;

/// Default release fade on note-off for sustain voices (ms).
const RELEASE_MS: u32 = 500;

/// Default legato crossfade — old sustain ramps out over this many ms.
const LEGATO_FADE_MS: u32 = 30;

// ── Legato state ──────────────────────────────────────────────────────────────

enum LegatoState {
    Idle,
    Pending {
        frames_remaining: usize,
        from_note: u8,
        to_note: u8,
        to_note_velocity: u8,
        /// Use Port samples (portamento glide) instead of Leg samples.
        portamento: bool,
    },
}

// ── SampleEngine ──────────────────────────────────────────────────────────────────

/// Real-time sample playback engine for one sample library section.
pub struct SampleEngine {
    patch: PlayerPatch,
    cache: SampleCache,
    voices: VoicePool,
    rr: RrCounters,

    /// Audio sample rate (Hz).
    pub sample_rate: u32,

    /// Active section ID (e.g. `"1v"`, `"Va"`, `"Ce"`).
    section: String,
    /// Active articulation ID (e.g. `"Vibsus"`, `"Staccato"`).
    articulation: String,
    /// Active microphone position ID (e.g. `"Mix"`, `"Main"`).
    mic: String,

    /// Current CC1 value [0–127], drives dynamic layer crossfade.
    cc1: u8,
    /// Current CC2 value [0–127], drives vibrato / non-vibrato crossfade.
    cc2: u8,
    /// Current CC58 value, selects articulation / legato mode.
    cc58: u8,
    /// CC64 (sustain pedal) held state.
    cc64_held: bool,
    /// Whether Con Sordino mode is active. When true, `articulation` holds the
    /// sordino artic ID (e.g. `"SordVibsus"`); switching modes remaps it.
    con_sordino: bool,
    /// Whether legato processing is enabled. When false, every note-on triggers
    /// a fresh sustain even if notes are held (equivalent to "Legato Off" in CSS).
    legato_enabled: bool,
    /// True = expressive mode (3 zones, 333/250/100ms), false = low-latency (2 zones, 100/150ms).
    legato_expressive: bool,

    /// Notes currently held down: MIDI note → velocity.
    held_notes: HashMap<u8, u8>,

    /// Legato pre-delay countdown.
    legato_state: LegatoState,

    /// Con Sordino bus-level filter (placeholder lowpass — see filter.rs).
    sord_filter: BiquadFilter,

    /// Fade duration (frames) applied to old sustain when legato fires.
    legato_fade_frames: usize,
    /// Ramp length (frames) for CC1 gain updates.
    cc1_ramp_frames: usize,
    /// Default release duration (frames) for sustain voices.
    release_frames: usize,
}

impl SampleEngine {
    /// Create a new engine for the given patch, sample rate, section, and mic.
    ///
    /// `section_id` — one of the spec's `[[section]]` IDs (e.g. `"1v"`).
    /// `mic_id`     — one of the spec's `[[mic]]` IDs (e.g. `"Mix"`).
    pub fn new(
        patch: PlayerPatch,
        sample_rate: u32,
        section_id: impl Into<String>,
        mic_id: impl Into<String>,
    ) -> Self {
        let section = section_id.into();
        let mic = mic_id.into();

        // Default to the first sustain-type articulation in the spec.
        let articulation = patch
            .spec
            .articulations
            .iter()
            .find(|a| a.kind == ArticulationKind::Sustain)
            .map(|a| a.id.clone())
            .unwrap_or_default();

        let legato_fade_frames = ms_to_frames(LEGATO_FADE_MS, sample_rate);
        let cc1_ramp_frames = ms_to_frames(CC1_RAMP_MS, sample_rate);
        let release_frames = ms_to_frames(RELEASE_MS, sample_rate);

        Self {
            patch,
            cache: SampleCache::new(),
            voices: VoicePool::new(),
            rr: RrCounters::new(),
            sample_rate,
            section,
            articulation,
            mic,
            cc1: 64,
            cc2: 0,
            cc58: 0,
            cc64_held: false,
            con_sordino: false,
            legato_enabled: true,
            legato_expressive: false, // default: low-latency mode
            sord_filter: BiquadFilter::lowpass(filter::SORD_FC, filter::SORD_Q, sample_rate),
            held_notes: HashMap::new(),
            legato_state: LegatoState::Idle,
            legato_fade_frames,
            cc1_ramp_frames,
            release_frames,
        }
    }

    // ── Configuration ─────────────────────────────────────────────────────────

    /// Switch to a different section. Resets RR counters.
    pub fn set_section(&mut self, section_id: impl Into<String>) {
        self.section = section_id.into();
        self.rr.reset();
    }

    /// Switch to a different microphone position.
    pub fn set_mic(&mut self, mic_id: impl Into<String>) {
        self.mic = mic_id.into();
    }

    /// Returns the currently active articulation ID.
    pub fn articulation(&self) -> &str {
        &self.articulation
    }

    /// Directly set the active articulation. Used in tests; production code
    /// should generally go through CC58 / keyswitches.
    pub fn set_articulation(&mut self, artic_id: impl Into<String>) {
        self.articulation = artic_id.into();
    }

    /// Toggle Con Sordino mode.
    ///
    /// When enabled the engine remaps the current articulation to its sordino
    /// counterpart (`"Vibsus"` → `"SordVibsus"`, etc.) using the `"Sord"`
    /// prefix convention. When disabled it strips the prefix. If no
    /// counterpart exists in the spec the articulation is left unchanged.
    pub fn set_con_sordino(&mut self, active: bool) {
        if self.con_sordino == active {
            return;
        }
        self.con_sordino = active;
        self.articulation = self.remap_sordino(&self.articulation.clone(), active);
        if !active {
            // Clear filter state so stale tail doesn't bleed into dry output.
            self.sord_filter.reset();
        }
    }

    /// Returns whether Con Sordino mode is currently active.
    pub fn con_sordino(&self) -> bool {
        self.con_sordino
    }

    /// Number of voices currently active.
    pub fn active_voices(&self) -> usize {
        self.voices.active_count()
    }

    // ── MIDI input ────────────────────────────────────────────────────────────

    /// Process a MIDI note-on event.
    pub fn note_on(&mut self, note: u8, velocity: u8) {
        if velocity == 0 {
            self.note_off(note);
            return;
        }

        // Legzero: same note re-trigger while sustain pedal is held.
        let legzero = self.cc64_held && self.held_notes.contains_key(&note);

        // Whether any other note is currently held (legato condition).
        let other_held = self.held_notes.keys().any(|&n| n != note);

        self.held_notes.insert(note, velocity);

        let artic_id = self.articulation.clone();
        let artic_kind = self
            .patch
            .spec
            .articulation(&artic_id)
            .map(|a| a.kind.clone());

        match artic_kind {
            Some(ArticulationKind::Sustain | ArticulationKind::Looped) => {
                if legzero {
                    self.trigger_legzero(note, velocity);
                } else if other_held && self.legato_enabled {
                    self.initiate_legato(note, velocity);
                } else {
                    self.trigger_sustain(note);
                }
            }
            Some(ArticulationKind::Short | ArticulationKind::OneShot) => {
                self.trigger_short(note, velocity);
            }
            Some(ArticulationKind::Legato | ArticulationKind::Release) => {
                // These are not triggered directly by note-on.
            }
            Some(ArticulationKind::Trill | ArticulationKind::Special) => {
                // Treat special/trill as sustain for basic playback.
                self.trigger_sustain(note);
            }
            None => {
                tracing::warn!(
                    artic = %artic_id,
                    "note_on: unknown articulation — skipping"
                );
            }
        }
    }

    /// Process a MIDI note-off event.
    pub fn note_off(&mut self, note: u8) {
        if self.cc64_held {
            // Sustain pedal held — defer release.
            return;
        }
        self.held_notes.remove(&note);
        self.do_note_off(note);
    }

    /// Process a MIDI CC event.
    pub fn cc(&mut self, controller: u8, value: u8) {
        match controller {
            1 => {
                self.cc1 = value;
                // Short-note articulations use CC1 to select sub-type (spiccato/
                // staccato/pizzicato/etc.); sustain articulations use it for dynamics.
                let is_short = self.patch.spec.articulation(&self.articulation)
                    .map(|a| a.kind == ArticulationKind::Short)
                    .unwrap_or(false);
                if is_short {
                    self.apply_cc1_short_select();
                } else {
                    self.update_sustain_gains();
                }
            }
            2 => {
                self.cc2 = value;
                self.update_sustain_gains();
            }
            58 => {
                self.cc58 = value;
                self.apply_cc58();
            }
            59 => {
                // CC59: round-robin reset (v1.7). Value is the 0-based starting
                // index. Resets all RR counters so the next short-note passage
                // plays back the same RR sequence every time.
                self.rr.reset_to(value as usize);
            }
            64 => {
                let was_held = self.cc64_held;
                self.cc64_held = value >= 64;
                if was_held && !self.cc64_held {
                    // Pedal released — send deferred note-offs.
                    let notes: Vec<u8> = self.held_notes.keys().cloned().collect();
                    self.held_notes.clear();
                    for n in notes {
                        self.do_note_off(n);
                    }
                }
            }
            _ => {}
        }
    }

    // ── Render ────────────────────────────────────────────────────────────────

    /// Mix all active voices into `output` (interleaved stereo, += accumulates).
    ///
    /// Also advances the legato countdown and fires legato samples when due.
    pub fn render(&mut self, output: &mut [f32]) {
        let block_frames = output.len() / 2;

        // Advance legato countdown.
        let fire = match &mut self.legato_state {
            LegatoState::Pending { frames_remaining, .. } => {
                if *frames_remaining <= block_frames {
                    true
                } else {
                    *frames_remaining -= block_frames;
                    false
                }
            }
            LegatoState::Idle => false,
        };

        if fire {
            if let LegatoState::Pending {
                from_note,
                to_note,
                to_note_velocity,
                portamento,
                ..
            } = std::mem::replace(&mut self.legato_state, LegatoState::Idle)
            {
                self.fire_legato(from_note, to_note, to_note_velocity, portamento);
            }
        }

        self.voices.render(output);

        // Apply Con Sordino placeholder filter to the entire output bus.
        if self.con_sordino {
            self.sord_filter.process(output);
        }
    }

    // ── Private — note trigger helpers ────────────────────────────────────────

    fn trigger_sustain(&mut self, note: u8) {
        let vib_blend = self.cc2_blend();
        let nv_scale = 1.0 - vib_blend;
        let vb_scale = vib_blend;

        let artic = self.articulation.clone();
        let vib_artic = self.find_vibrato_pair_id(&artic);
        let section = self.section.clone();
        let mic = self.mic.clone();
        let release_frames = self.release_frames;

        // Compute CC1 layers separately for each articulation so that
        // Vibsus (4 dyns: ppp/p/mf/ff) and Nonvib (3 dyns: p/mf/ff) each
        // use their own crossfade map. Without this, Nonvib gets asked for
        // "ppp" samples that don't exist at very low CC1 values.
        let (nv_lo, nv_hi, nv_cc1_blend) = self.layers_for_artic(&artic);
        let nv_lo_gain = nv_scale * (1.0 - nv_cc1_blend);
        let nv_hi_gain = nv_scale * nv_cc1_blend;

        if let Some(v) = self.make_voice(
            &artic, &section, &mic, &nv_lo, note, "", VoiceKind::SustainNVLo, nv_lo_gain, release_frames,
        ) {
            self.voices.spawn(v);
        }
        if nv_hi != nv_lo {
            if let Some(v) = self.make_voice(
                &artic, &section, &mic, &nv_hi, note, "", VoiceKind::SustainNVHi, nv_hi_gain, release_frames,
            ) {
                self.voices.spawn(v);
            }
        }

        // Vibrato voices — only if a vibrato-pair articulation exists.
        if let Some(vib_id) = vib_artic {
            let (vb_lo, vb_hi, vb_cc1_blend) = self.layers_for_artic(&vib_id.clone());
            let vb_lo_gain = vb_scale * (1.0 - vb_cc1_blend);
            let vb_hi_gain = vb_scale * vb_cc1_blend;

            if let Some(v) = self.make_voice(
                &vib_id, &section, &mic, &vb_lo, note, "", VoiceKind::SustainVibLo, vb_lo_gain, release_frames,
            ) {
                self.voices.spawn(v);
            }
            if vb_hi != vb_lo {
                if let Some(v) = self.make_voice(
                    &vib_id, &section, &mic, &vb_hi, note, "", VoiceKind::SustainVibHi, vb_hi_gain, release_frames,
                ) {
                    self.voices.spawn(v);
                }
            }
        }
    }

    fn trigger_short(&mut self, note: u8, velocity: u8) {
        // Pick dynamic layer based on velocity and spec short_note_cc1_map.
        let dynamic = self.short_note_dynamic(velocity);
        let artic = self.articulation.clone();
        let section = self.section.clone();
        let mic = self.mic.clone();
        let release_frames = self.release_frames;

        if let Some(v) =
            self.make_voice(&artic, &section, &mic, &dynamic, note, "", VoiceKind::Short, 1.0, release_frames)
        {
            self.voices.spawn(v);
        }
    }

    fn trigger_legzero(&mut self, note: u8, _velocity: u8) {
        // Find a Legato-kind articulation for same-note retrigger.
        let Some(rz_id) = self.find_legato_artic_id(true) else { return };
        let section = self.section.clone();
        let mic = self.mic.clone();
        let (lo_dyn, _, _) = self.current_layers_owned();
        let release_frames = self.release_frames;

        if let Some(v) =
            self.make_voice(&rz_id, &section, &mic, &lo_dyn, note, "", VoiceKind::Legato, 1.0, release_frames)
        {
            self.voices.spawn(v);
        }
    }

    fn initiate_legato(&mut self, to_note: u8, velocity: u8) {
        let from_note = *self
            .held_notes
            .keys()
            .find(|&&n| n != to_note)
            .unwrap_or(&to_note);

        // Check portamento threshold (default 20, velocity ≤ threshold triggers glide).
        let port_thresh = self.patch.spec.legato_engine.as_ref()
            .and_then(|le| le.portamento.as_ref())
            .map(|p| p.trigger_vel_max)
            .unwrap_or(0); // 0 disables portamento
        let portamento = port_thresh > 0 && velocity <= port_thresh;

        let delay_ms = if portamento {
            0 // portamento fires immediately — the glide pitch ramp is the "delay"
        } else if self.legato_expressive {
            self.patch.legato_delay_expressive(velocity).unwrap_or(100)
        } else {
            self.patch.legato_delay_low_latency(velocity).unwrap_or(100)
        };

        let frames_remaining = ms_to_frames(delay_ms, self.sample_rate);

        self.legato_state = LegatoState::Pending {
            frames_remaining,
            from_note,
            to_note,
            to_note_velocity: velocity,
            portamento,
        };
    }

    fn fire_legato(&mut self, from_note: u8, to_note: u8, _velocity: u8, portamento: bool) {
        let direction = if to_note > from_note { "up" } else { "down" };

        // Fade out old sustain voice.
        self.voices.silence_note(from_note, self.legato_fade_frames);

        // For portamento, look for Port-type articulation; otherwise Leg/NVLeg.
        let leg_id = if portamento {
            self.find_port_artic_id().or_else(|| self.find_legato_artic_id(false))
        } else {
            self.find_legato_artic_id(false)
        };

        let Some(leg_id) = leg_id else {
            self.trigger_sustain(to_note);
            return;
        };

        let section = self.section.clone();
        let mic = self.mic.clone();
        let (lo_dyn, _, _) = self.current_layers_owned();
        let release_frames = self.release_frames;

        // Try directional first; fall back to directionless if not found.
        let v = self
            .make_voice(&leg_id, &section, &mic, &lo_dyn, to_note, direction, VoiceKind::Legato, 1.0, release_frames)
            .or_else(|| {
                self.make_voice(&leg_id, &section, &mic, &lo_dyn, to_note, "", VoiceKind::Legato, 1.0, release_frames)
            });

        if let Some(v) = v {
            self.voices.spawn(v);
        }
        // Always trigger a background sustain so the note doesn't go silent
        // when the legato transition sample finishes. The Leg sample provides
        // the attack character; the Vibsus/Nonvib body takes over after it ends.
        self.trigger_sustain(to_note);
    }

    /// Find the Port articulation matching the current sordino state.
    fn find_port_artic_id(&self) -> Option<String> {
        let want_sord = self.articulation.starts_with("Sord");
        self.patch.spec.articulations.iter()
            .filter(|a| a.kind == ArticulationKind::Legato)
            .filter(|a| a.id.starts_with("Sord") == want_sord)
            .find(|a| a.id.to_lowercase().contains("port"))
            .map(|a| a.id.clone())
    }

    fn do_note_off(&mut self, note: u8) {
        // Trigger release trail if the current articulation specifies one.
        let release_artic = self
            .patch
            .spec
            .articulation(&self.articulation.clone())
            .and_then(|a| a.release_artic.clone());

        if let Some(rel_id) = release_artic {
            let section = self.section.clone();
            let mic = self.mic.clone();
            let (lo_dyn, _, _) = self.current_layers_owned();
            let release_frames = self.release_frames;

            if let Some(v) = self.make_voice(
                &rel_id, &section, &mic, &lo_dyn, note, "", VoiceKind::Release, 0.7, release_frames,
            ) {
                self.voices.spawn(v);
            }
        }

        self.voices.note_off(note);
    }

    // ── Private — CC handlers ─────────────────────────────────────────────────

    /// Recompute and ramp all 4 sustain voice gains when CC1 or CC2 changes.
    fn update_sustain_gains(&mut self) {
        let vib_blend = self.cc2_blend();
        let nv = 1.0 - vib_blend;
        let vb = vib_blend;
        let ramp = self.cc1_ramp_frames;

        // Use per-articulation layer sets so NV and Vib voices each use
        // their own dynamics count (see trigger_sustain for full rationale).
        let artic = self.articulation.clone();
        let vib_artic = self.find_vibrato_pair_id(&artic);

        let (_, _, nv_blend) = self.layers_for_artic(&artic);
        let (_, _, vb_blend) = vib_artic.as_deref()
            .map(|id| self.layers_for_artic(id))
            .unwrap_or((String::new(), String::new(), nv_blend));

        self.voices.update_sustain_blend(
            nv * (1.0 - nv_blend), // NVLo
            nv * nv_blend,          // NVHi
            vb * (1.0 - vb_blend), // VibLo
            vb * vb_blend,          // VibHi
            ramp,
        );
    }

    /// Compute the vibrato blend factor [0.0, 1.0] from the current CC2 value.
    ///
    /// `"on_off"` mode (CSSS): snaps at 64. All other libraries: linear.
    fn cc2_blend(&self) -> f32 {
        match self.patch.spec.dynamics.vibrato_mode.as_deref() {
            Some("on_off") => if self.cc2 >= 64 { 1.0 } else { 0.0 },
            _ => self.cc2 as f32 / 127.0,
        }
    }

    /// Find the vibrato counterpart of `artic_id`.
    ///
    /// CSS/CSSS convention: if the current artic has no "NV"/"Nonvib" in its
    /// name we look for one that does (and vice-versa), staying within the
    /// same family (Con Sordino vs regular).
    fn find_vibrato_pair_id(&self, artic_id: &str) -> Option<String> {
        // Only applies when CC2 is the vibrato controller.
        self.patch.spec.dynamics.vibrato_controller.as_deref()?;

        let id_lower = artic_id.to_lowercase();
        let is_sord = id_lower.contains("sord");
        let is_nv = id_lower.contains("nv") || id_lower.contains("nonvib");

        self.patch.spec.articulations.iter()
            .filter(|a| a.id != artic_id)
            .filter(|a| a.kind == ArticulationKind::Sustain)
            .filter(|a| {
                let other = a.id.to_lowercase();
                // Same family (sord vs non-sord)
                other.contains("sord") == is_sord
                    // Opposite vibrato side
                    && (other.contains("nv") || other.contains("nonvib")) != is_nv
            })
            .map(|a| a.id.clone())
            .next()
    }

    /// Map CC58 → action and execute it.
    ///
    /// Several CC58 values are **mode switches** rather than articulation selectors:
    /// - `"Con Sordino On"` / `"Con Sordino Off"` → toggle sordino sample set
    /// - `"Legato On"` / `"Legato Off"` → enable/disable legato processing
    /// - `"Sustain: Low Latency Legato"` / `"Sustain: Expressive Legato"` → select
    ///   legato pre-delay mode (latency/expressive) without changing the articulation
    ///
    /// All other labels are treated as articulation IDs or display labels. If Con
    /// Sordino mode is active, the matched articulation is remapped to its sordino
    /// counterpart.
    fn apply_cc58(&mut self) {
        let Some(ks) = self.patch.spec.keyswitch.as_ref() else { return };
        let Some(label) = ks.cc58_function(self.cc58) else { return };
        let label = label.to_string();

        // ── Mode switches ────────────────────────────────────────────────────
        match label.as_str() {
            "Con Sordino On" => {
                self.set_con_sordino(true);
                return;
            }
            "Con Sordino Off" => {
                self.set_con_sordino(false);
                return;
            }
            "Legato On" => {
                self.legato_enabled = true;
                return;
            }
            "Legato Off" => {
                self.legato_enabled = false;
                return;
            }
            "Sustain: Low Latency Legato" => {
                self.legato_enabled = true;
                self.legato_expressive = false;
                return;
            }
            "Sustain: Expressive Legato" => {
                self.legato_enabled = true;
                self.legato_expressive = true;
                return;
            }
            "Measured Tremolo" => {
                // Scripted mode — no dedicated samples. Cannot replicate without a
                // built-in scripted repeating trigger. Ignore for now.
                return;
            }
            _ => {}
        }

        // ── Articulation selection ───────────────────────────────────────────
        let matched = self
            .patch
            .spec
            .articulations
            .iter()
            .find(|a| a.id == label || a.label == label)
            .map(|a| a.id.clone());

        if let Some(id) = matched {
            self.articulation = self.remap_sordino(&id, self.con_sordino);
        }
    }

    // ── Private — helpers ─────────────────────────────────────────────────────

    /// Build a `Voice` for a resolved sample, or `None` if the sample can't
    /// be found or loaded.
    #[allow(clippy::too_many_arguments)]
    fn make_voice(
        &mut self,
        artic_id: &str,
        section: &str,
        mic: &str,
        dynamic: &str,
        note: u8,
        direction: &str,
        kind: VoiceKind,
        gain: f32,
        release_frames: usize,
    ) -> Option<Voice> {
        let max_rr = self
            .patch
            .spec
            .articulation(artic_id)
            .map(|a| a.rr)
            .unwrap_or(1);

        let rr_idx = self.rr.next(section, artic_id, dynamic, max_rr);

        let (path, sampled_note) =
            self.patch.resolve(section, artic_id, mic, dynamic, note, direction, rr_idx)?;

        let data = match self.cache.get(&path) {
            Ok(d) => d,
            Err(e) => {
                tracing::warn!("sample load failed {}: {e}", path.display());
                return None;
            }
        };

        let semitone_offset = note as i16 - sampled_note as i16;
        Some(Voice::new(
            data,
            note,
            kind,
            semitone_offset.clamp(i8::MIN as i16, i8::MAX as i16) as i8,
            gain,
            release_frames,
        ))
    }

    /// Returns `(lo_label, hi_label, hi_blend)` for a specific articulation ID
    /// at the current CC1 value, using that articulation's own dynamics count.
    fn layers_for_artic(&self, artic_id: &str) -> (String, String, f32) {
        let n = self.patch.spec.articulation(artic_id)
            .map(|a| a.dynamics.len())
            .unwrap_or(0);
        let d = &self.patch.spec.dynamics;
        let layers: &[Cc1Layer] = match n {
            2 => &d.cc1_layers_2,
            3 => &d.cc1_layers_3,
            4 => &d.cc1_layers_4,
            5 => &d.cc1_layers_5,
            6 => &d.cc1_layers_6,
            _ => return ("p".into(), "p".into(), 0.0),
        };
        Self::cc1_blend(layers, self.cc1)
    }

    /// Returns `(lo_label, hi_label, hi_blend)` for the current CC1 value,
    /// using the active articulation's own layer set.
    fn current_layers_owned(&self) -> (String, String, f32) {
        Self::cc1_blend(self.active_cc1_layers(), self.cc1)
    }

    /// Core crossfade algorithm: walk adjacent layer pairs and return
    /// `(lo_label, hi_label, hi_blend)` for a given CC value.
    fn cc1_blend(layers: &[Cc1Layer], cc1: u8) -> (String, String, f32) {
        if layers.is_empty() {
            return ("p".into(), "p".into(), 0.0);
        }
        // Walk through adjacent pairs. The crossfade region between layer i
        // and layer i+1 is [layers[i+1].cc_range[0], layers[i].cc_range[1]].
        for i in 0..layers.len().saturating_sub(1) {
            let lo = &layers[i];
            let hi = &layers[i + 1];
            let xfade_start = hi.cc_range[0];
            let xfade_end = lo.cc_range[1];

            if cc1 <= xfade_end {
                if cc1 < xfade_start {
                    return (lo.label.clone(), lo.label.clone(), 0.0);
                } else {
                    let span = (xfade_end - xfade_start + 1).max(1) as f32;
                    let blend = (cc1 - xfade_start) as f32 / span;
                    return (lo.label.clone(), hi.label.clone(), blend);
                }
            }
        }
        let top = &layers[layers.len() - 1];
        (top.label.clone(), top.label.clone(), 0.0)
    }

    /// Return the correct CC1 layer slice for the current articulation.
    fn active_cc1_layers(&self) -> &[Cc1Layer] {
        let Some(artic) = self.patch.spec.articulation(&self.articulation) else {
            return &[];
        };
        let n = artic.dynamics.len();
        let d = &self.patch.spec.dynamics;
        match n {
            2 => &d.cc1_layers_2,
            3 => &d.cc1_layers_3,
            4 => &d.cc1_layers_4,
            5 => &d.cc1_layers_5,
            6 => &d.cc1_layers_6,
            _ => &[],
        }
    }

    /// Determine the dynamic layer label for a short note from velocity.
    ///
    /// Divides the velocity range [0–127] evenly across the articulation's
    /// `dynamics` array. For example, Staccato with ["pp","mp","f","fff"]
    /// maps vel 0–31→pp, 32–63→mp, 64–95→f, 96–127→fff.
    fn short_note_dynamic(&self, velocity: u8) -> String {
        let Some(artic) = self.patch.spec.articulation(&self.articulation) else {
            return "p".into();
        };
        if artic.dynamics.is_empty() {
            return "p".into();
        }
        let n = artic.dynamics.len();
        let idx = (velocity as usize * n / 128).min(n - 1);
        artic.dynamics[idx].clone()
    }

    /// When CC1 moves and the active articulation is a short-note type, switch
    /// to the sub-type that corresponds to the new CC1 value.
    ///
    /// CSS maps:
    ///   `short_note_cc1_map`  — Spiccato / Staccatissimo / Staccato / Sfz
    ///   `pizzicato_cc1_map`   — Pizzicato / Bartokpizz / Clegno
    fn apply_cc1_short_select(&mut self) {
        let d = &self.patch.spec.dynamics;

        // Determine which map to consult based on current articulation family.
        let in_pizz_family = d.pizzicato_cc1_map.values()
            .any(|id| id == &self.articulation);
        let in_short_family = d.short_note_cc1_map.values()
            .any(|id| id == &self.articulation);

        if !in_short_family && !in_pizz_family {
            return; // not in a switchable short-note family
        }

        let map = if in_pizz_family { &d.pizzicato_cc1_map } else { &d.short_note_cc1_map };
        let cc1 = self.cc1;

        for (range_str, artic_id) in map {
            if let Some((lo, hi)) = crate::spec::parse_range(range_str) {
                if cc1 >= lo && cc1 <= hi {
                    if self.patch.spec.articulation(artic_id).is_some() {
                        self.articulation = artic_id.clone();
                    }
                    return;
                }
            }
        }
    }

    /// Find the ID of a Legato-type articulation appropriate for the current
    /// section. `retrigger` selects the same-note (Legzero) variant.
    ///
    /// Automatically matches:
    /// - Sordino state: `"Sord"` prefix ↔ current articulation prefix.
    /// - Vibrato state: if the current articulation is NV-type (Nonvib/NVLeg),
    ///   prefer `NVLeg`; otherwise prefer `Leg`. Falls back to any match if
    ///   the preferred variant is absent.
    fn find_legato_artic_id(&self, retrigger: bool) -> Option<String> {
        let want_sord = self.articulation.starts_with("Sord");
        let artic_lower = self.articulation.to_lowercase();
        let prefer_nv = artic_lower.contains("nv") || artic_lower.contains("nonvib");

        let candidates: Vec<&crate::spec::ArticulationSpec> = self.patch
            .spec
            .articulations
            .iter()
            .filter(|a| a.kind == ArticulationKind::Legato)
            .filter(|a| {
                a.instrument_filter.is_empty()
                    || a.instrument_filter.contains(&self.section)
            })
            .filter(|a| a.id.starts_with("Sord") == want_sord)
            .filter(|a| {
                let id_lower = a.id.to_lowercase();
                if retrigger {
                    id_lower.contains("zero")
                } else {
                    !id_lower.contains("zero")
                }
            })
            .collect();

        // Prefer NVLeg when in non-vibrato mode, Leg otherwise.
        let preferred = if prefer_nv {
            candidates.iter().find(|a| a.id.to_lowercase().contains("nv"))
        } else {
            candidates.iter().find(|a| !a.id.to_lowercase().contains("nv"))
        };

        preferred
            .or_else(|| candidates.first())
            .map(|a| a.id.clone())
    }

    /// Map an articulation ID to/from its Con Sordino counterpart.
    ///
    /// `"Vibsus"` + `active=true`  → `"SordVibsus"` (if it exists in the spec)
    /// `"SordVibsus"` + `active=false` → `"Vibsus"` (if it exists in the spec)
    /// Returns the original ID unchanged if no counterpart is found.
    fn remap_sordino(&self, artic_id: &str, active: bool) -> String {
        if active {
            if !artic_id.starts_with("Sord") {
                let sord_id = format!("Sord{artic_id}");
                if self.patch.spec.articulation(&sord_id).is_some() {
                    return sord_id;
                }
            }
        } else if let Some(base) = artic_id.strip_prefix("Sord") {
            if self.patch.spec.articulation(base).is_some() {
                return base.to_string();
            }
        }
        artic_id.to_string()
    }
}

// ── VoicePool extension ───────────────────────────────────────────────────────

impl VoicePool {
    /// Ramp all four sustain voice kinds to their new gains over `ramp_frames`.
    ///
    /// Called whenever CC1 or CC2 changes so the dynamic/vibrato blend updates
    /// smoothly without zipper noise.
    pub fn update_sustain_blend(
        &mut self,
        nv_lo: f32,
        nv_hi: f32,
        vib_lo: f32,
        vib_hi: f32,
        ramp_frames: usize,
    ) {
        for v in self.voices_mut() {
            match v.kind {
                VoiceKind::SustainNVLo  => v.ramp_gain(nv_lo,  ramp_frames),
                VoiceKind::SustainNVHi  => v.ramp_gain(nv_hi,  ramp_frames),
                VoiceKind::SustainVibLo => v.ramp_gain(vib_lo, ramp_frames),
                VoiceKind::SustainVibHi => v.ramp_gain(vib_hi, ramp_frames),
                // Legacy kinds still accepted; treat as NV Lo/Hi.
                VoiceKind::SustainLo => v.ramp_gain(nv_lo, ramp_frames),
                VoiceKind::SustainHi => v.ramp_gain(nv_hi, ramp_frames),
                _ => {}
            }
        }
    }
}

// ── Utilities ─────────────────────────────────────────────────────────────────

/// Convert milliseconds to audio frames at the given sample rate.
#[inline]
pub fn ms_to_frames(ms: u32, sample_rate: u32) -> usize {
    (ms as f64 * sample_rate as f64 / 1000.0).round() as usize
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ms_to_frames_44100() {
        assert_eq!(ms_to_frames(0, 44100), 0);
        assert_eq!(ms_to_frames(1000, 44100), 44100);
        assert_eq!(ms_to_frames(100, 44100), 4410);
    }

    #[test]
    fn cc1_layer_selection() {
        // Simulate a 3-layer [p=0-42, mf=33-94, ff=85-127] setup.
        use crate::spec::Cc1Layer;

        // Build a minimal PlayerPatch-less engine via a stub spec.
        // We test current_layers_owned() logic in isolation by constructing
        // a mock layers slice and exercising the algorithm directly.
        let layers: &[Cc1Layer] = &[
            Cc1Layer { label: "p".into(),  cc_range: [0, 42] },
            Cc1Layer { label: "mf".into(), cc_range: [33, 94] },
            Cc1Layer { label: "ff".into(), cc_range: [85, 127] },
        ];

        // Inline the algorithm (same logic as current_layers_owned).
        let probe = |cc1: u8| -> (String, String, f32) {
            for i in 0..layers.len().saturating_sub(1) {
                let lo = &layers[i];
                let hi = &layers[i + 1];
                let xs = hi.cc_range[0];
                let xe = lo.cc_range[1];
                if cc1 <= xe {
                    if cc1 < xs {
                        return (lo.label.clone(), lo.label.clone(), 0.0);
                    } else {
                        let span = (xe - xs + 1).max(1) as f32;
                        let blend = (cc1 - xs) as f32 / span;
                        return (lo.label.clone(), hi.label.clone(), blend);
                    }
                }
            }
            let top = &layers[layers.len() - 1];
            (top.label.clone(), top.label.clone(), 0.0)
        };

        let (lo, hi, blend) = probe(10);
        assert_eq!(lo, "p");
        assert_eq!(hi, "p");
        assert_eq!(blend, 0.0);

        let (lo, hi, blend) = probe(33);
        assert_eq!(lo, "p");
        assert_eq!(hi, "mf");
        assert!(blend >= 0.0 && blend <= 1.0);

        let (lo, hi, blend) = probe(50);
        assert_eq!(lo, "mf");
        assert_eq!(hi, "mf");
        assert_eq!(blend, 0.0);

        let (lo, hi, _blend) = probe(127);
        assert_eq!(lo, "ff");
        assert_eq!(hi, "ff");
    }

    #[test]
    fn con_sordino_remap() {
        // Load the CSS spec and exercise the sordino switch logic using the
        // real articulation list.
        let specs_dir = {
            let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
            std::path::Path::new(&manifest)
                .parent().unwrap()
                .parent().unwrap()
                .join("specs")
        };
        let spec_path = specs_dir.join("cinematic-strings.toml");
        if !spec_path.exists() { return; }

        let spec = crate::LibrarySpec::from_file(&spec_path).expect("load CSS spec");
        let patch = crate::PlayerPatch::from_spec(spec);

        let mut engine = SampleEngine::new(patch, 44100, "1v", "Mix");
        engine.set_articulation("Vibsus");
        assert!(!engine.con_sordino());

        // Enable Con Sordino — should remap to SordVibsus.
        engine.set_con_sordino(true);
        assert!(engine.con_sordino());
        assert_eq!(engine.articulation(), "SordVibsus");

        // Disable — should revert to Vibsus.
        engine.set_con_sordino(false);
        assert_eq!(engine.articulation(), "Vibsus");

        // Nonvib → SordNonvib and back.
        engine.set_articulation("Nonvib");
        engine.set_con_sordino(true);
        assert_eq!(engine.articulation(), "SordNonvib");
        engine.set_con_sordino(false);
        assert_eq!(engine.articulation(), "Nonvib");
    }
}
