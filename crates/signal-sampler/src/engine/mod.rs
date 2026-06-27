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

use std::cell::{Cell, RefCell};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::path::Path;

use crate::spec::{ArticulationKind, Cc1Layer};
use crate::{PlayerPatch, VoiceConfig};
use cache::{EvictStats, PreloadStats, SampleCache};
use filter::BiquadFilter;
use rr::RrCounters;
use voice::{Voice, VoiceKind, VoicePool, VoiceStealPolicy};

// ── Constants ─────────────────────────────────────────────────────────────────

/// CC1 gain ramp length (ms). Smooths dynamic crossfade to avoid clicks.
const CC1_RAMP_MS: u32 = 20;

/// Default release fade on note-off for sustain voices (ms).
///
/// This is the damper-down time: on key-up (sustain pedal NOT held) the note
/// must stop, not ring out. A short linear fade reads as an immediate cutoff
/// while avoiding a click. Sustain itself comes only from holding CC64, which
/// defers the note-off entirely (see `note_off_with_velocity`); the half-pedal
/// curve still scales this up for partial-pedal positions. Was 500 ms, which
/// made every key-up sound like a half-second sustain even with the pedal up.
const RELEASE_MS: u32 = 60;
/// Extra damping time at the top of the half-pedal range.
const HALF_PEDAL_MAX_RELEASE_MULTIPLIER: f32 = 4.0;

/// Short de-click fade for sample-playback instruments that have explicit
/// release/noise samples. The tone should stop on key-up; this only avoids a pop.
/// Damper-engagement time on note-off when a release_artic supplies the
/// natural tail (e.g. Keyscape pianos). Long enough that the body fades
/// naturally — short enough that holding the key is what determines note
/// length. Real Rhodes dampers ring out over ~50–100 ms.
const KEY_UP_DECLICK_MS: u32 = 80;

/// Cap on the lifetime of a `VoiceKind::Release` voice. The library's
/// release-tail FLACs can be very long (Keyscape ships 30 s clips) and
/// release voices play to sample-end, so without a cap each note-off
/// consumes a voice slot for 30 s and the 64-voice pool fills up after
/// ~20 key presses — causing voice-stealing and intermittent silence.
const RELEASE_MAX_LIFETIME_MS: u32 = 2_000;

/// Default legato crossfade — old sustain ramps out over this many ms.
const LEGATO_FADE_MS: u32 = 30;

/// Maximum gain for a release-tail voice at MIDI release-velocity 127. The
/// per-fire gain scales linearly down with release velocity so soft releases
/// produce a quiet damper click while hard releases ring out.
const RELEASE_SAMPLE_GAIN_MAX: f32 = 0.35;
/// Below this release velocity the release sample is suppressed entirely —
/// avoids producing an audible click on every note-off when a controller's
/// release-velocity is effectively 0.
const RELEASE_SAMPLE_VELOCITY_MIN: u8 = 1;
const RECENT_MISS_LIMIT: usize = 8;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ZoneTrigger {
    Attack,
    Release,
    PedalDown,
    PedalUp,
    Cc,
    Aftertouch,
}

// ── SampleEngine ──────────────────────────────────────────────────────────────────

/// Real-time sample playback engine for one sample library section.
pub struct SampleEngine {
    patch: PlayerPatch,
    cache: SampleCache,
    voices: VoicePool,
    /// Round-robin counters. Interior-mutable so voice resolution
    /// (`make_voice`) can run as `&self` — it only advances RR and records
    /// miss telemetry, letting note-on pass `&self.articulation`/`section`/`mic`
    /// without cloning.
    rr: RefCell<RrCounters>,

    /// Audio sample rate (Hz).
    pub sample_rate: u32,

    /// Active section ID (e.g. `"1v"`, `"Va"`, `"Ce"`).
    section: String,
    /// Active articulation ID (e.g. `"Vibsus"`, `"Staccato"`).
    articulation: String,
    /// Active microphone position ID (e.g. `"Mix"`, `"Main"`).
    mic: String,
    /// Opt-in single-mic filter for multi-mic zone sets that declare no
    /// `mics` block (so mic_index folds everything to bus 0). When `Some`,
    /// only zones whose `mic` matches fire — otherwise every mic in the set
    /// sounds at once (Main + Mix doubling). `None` keeps the default
    /// play-all-mics behaviour used by multi-mic mixing + drum kits.
    solo_mic: Option<String>,
    /// MIDI note → index into `keyswitch.notes`: which incoming notes are
    /// velocity-sensitive keyswitches (selecting articulation / mode) rather
    /// than sounding. Built from the spec at construction.
    keyswitch_notes: HashMap<u8, usize>,
    /// Current legato transition direction (`"up"` / `"down"`) — selects the
    /// directional legato zone (CSS records a separate sample per direction).
    /// Defaults to `"up"`; updated per note from the interval played.
    play_direction: String,

    /// True when the source pack is a percussion / drum-kit library
    /// (`category` ~ "drum-kit", or a percussion `instrument`). Percussion
    /// engines always play zones at natural pitch — the incoming note is a
    /// trigger selector, never a transpose.
    percussion: bool,
    /// True when every zone shares a single `key_min` (single-articulation
    /// drum: kick, tom). Such an engine fires on *any* routed note, so the
    /// preset's `note_routing` is the sole authority for which note plays it.
    single_attack_key: bool,
    /// When set, this engine fires only zones whose `articulation` matches
    /// (case-insensitive), ignoring the incoming key. Lets one drum pack be
    /// addressed as several performance pieces (hats Closed vs Open, …).
    pinned_articulation: Option<String>,
    /// Per-trigger articulation override, set for the duration of a single
    /// `note_on_articulated` call so one shared engine can serve many routed
    /// notes (each route picks the articulation). Takes precedence over
    /// `pinned_articulation`; always cleared after the trigger.
    trigger_articulation: Option<String>,
    /// Engine-wide choke group (pre-hashed). When set, voices join this group
    /// so they can be silenced by a later choking hit. `None` = polyphonic
    /// (kick/snare/toms ring freely).
    engine_choke_group: Option<u64>,
    /// Which articulations actually *trigger* the choke (silence the group).
    /// Empty + a set group = monophonic: every hit chokes (hi-hats — any hit,
    /// incl. pedal, cuts the ringing hat). Non-empty = only these articulations
    /// choke (cymbals: only "Choke" stops the ringing crash; crashes overlap).
    /// Lowercased for case-insensitive matching.
    engine_choke_on: Vec<String>,

    /// Current CC1 value [0–127], drives dynamic layer crossfade.
    cc1: u8,
    /// Current CC2 value [0–127], drives vibrato / non-vibrato crossfade.
    cc2: u8,
    /// Current CC58 value, selects articulation / legato mode.
    cc58: u8,
    cc_values: [u8; 128],
    channel_aftertouch: u8,
    poly_aftertouch: [u8; 128],
    /// CC64 (sustain pedal) held state.
    cc64_held: bool,
    /// Raw CC64 value. 1..63 is treated as half-pedal damping, >=64 as full hold.
    cc64_value: u8,
    /// While the sustain pedal is held, libraries with distinct pedal-down
    /// body samples (e.g. Keyscape `lacrped`) swap `articulation` to the
    /// pedal variant. The original (no-pedal) ID lives here so we can snap
    /// back on pedal-up. `None` when the active articulation isn't a pedal
    /// swap — i.e. normal play.
    no_pedal_articulation: Option<String>,
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
    /// Notes for which `trigger_short`'s body voice actually spawned
    /// (i.e. the body sample was decoded and resolve succeeded). Used to
    /// gate the release-tail voice on note-off: if the body never sounded
    /// we don't want to play just the mechanical release click in
    /// isolation, which sounds buggy.
    body_voiced: std::collections::HashSet<u8>,
    /// Note-off velocities captured while the sustain pedal is held.
    deferred_note_off_velocities: HashMap<u8, u8>,

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

    /// Round-robin counter for zone mode. Increments on every zoned note-on
    /// regardless of (note, velocity) so RR cycling within a matching zone set
    /// behaves as expected when the same key is repeatedly struck.
    zone_rr_counter: usize,
    zone_rr_random_state: u64,
    /// Last-used RR slot per (trigger, note, velocity), keyed by a packed
    /// integer so note-on allocates nothing (was a `format!`-built String key).
    zone_rr_last_slots: HashMap<u64, u32>,

    /// Reusable scratch for the zoned trigger path so note-on doesn't allocate.
    /// Drained/refilled each note-on via `mem::take` + restore.
    zone_indices_scratch: Vec<usize>,
    zone_choked_scratch: Vec<u64>,
    zone_capped_scratch: Vec<(u64, usize)>,

    /// Mic ids in spec declaration order. Empty when the library has no
    /// `mics` array. Used to map a zone's `mic` string to a stable index
    /// the renderer can splay across per-mic buffers.
    mic_ids: Vec<String>,

    // Miss telemetry — interior-mutable so `make_voice`/`record_*` are `&self`.
    cache_misses: Cell<usize>,
    sample_misses: Cell<usize>,
    recent_cache_misses: RefCell<VecDeque<String>>,
    recent_sample_misses: RefCell<VecDeque<String>>,
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

        // Default to a playable articulation. Sustain is preferred; if the
        // spec has no Sustain (e.g. Keyscape Rhodes — all samples are
        // OneShot strikes plus Release tails), fall back to any kind that
        // can be triggered directly by note-on. Release/Legato are NOT
        // triggered by note-on (they fire on note-off / from another voice),
        // so picking one of those as the default produces silence.
        // Skip articulations whose IDs name mechanical-action / key-release
        // layers — those are companions, not the playable body. Without
        // this, Keyscape Classic ships `clrmchr03` (mechanical) alphabetically
        // before `clrr10` (the actual Rhodes body) and the picker locks onto
        // the mechanical noise.
        let is_aux_layer = |id: &str| -> bool {
            let l = id.to_ascii_lowercase();
            l.contains("mch") || l.contains("mech") || l.contains("ped")
        };
        let articulation = patch
            .spec
            .articulations
            .iter()
            .find(|a| a.kind == ArticulationKind::Sustain && !is_aux_layer(&a.id))
            .or_else(|| {
                patch.spec.articulations.iter().find(|a| {
                    !matches!(a.kind, ArticulationKind::Release | ArticulationKind::Legato)
                        && !is_aux_layer(&a.id)
                })
            })
            .or_else(|| {
                patch.spec.articulations.iter().find(|a| {
                    !matches!(a.kind, ArticulationKind::Release | ArticulationKind::Legato,)
                })
            })
            .or_else(|| patch.spec.articulations.first())
            .map(|a| a.id.clone())
            .unwrap_or_default();

        let legato_fade_frames = ms_to_frames(LEGATO_FADE_MS, sample_rate);
        let cc1_ramp_frames = ms_to_frames(CC1_RAMP_MS, sample_rate);
        let release_frames = ms_to_frames(RELEASE_MS, sample_rate);
        let cache = if let Some(pack) = patch.pack.clone() {
            SampleCache::with_pack(pack)
        } else {
            SampleCache::with_prepared(patch.prepared_cache_dir.as_deref())
        };

        let mic_ids: Vec<String> = patch.spec.mics.iter().map(|m| m.id.clone()).collect();

        let percussion = spec_is_percussion(&patch.spec);
        // Single-articulation drum: every zone sits on one key (kick, tom).
        // Such a pack should fire on whatever note the preset routes to it.
        let single_attack_key = {
            let mut iter = patch.spec.zones.iter().map(|z| z.key_min);
            match iter.next() {
                Some(first) => iter.all(|k| k == first),
                None => false,
            }
        };

        // Resolve keyswitch note names → MIDI numbers once (C0 = 12).
        let keyswitch_notes: HashMap<u8, usize> = patch
            .spec
            .keyswitch
            .as_ref()
            .map(|ks| {
                ks.notes
                    .iter()
                    .enumerate()
                    .filter_map(|(i, kn)| {
                        crate::midi::note_name_to_midi(&kn.note).ok().map(|n| (n, i))
                    })
                    .collect()
            })
            .unwrap_or_default();

        Self {
            patch,
            cache,
            voices: VoicePool::new(),
            rr: RefCell::new(RrCounters::new()),
            sample_rate,
            section,
            articulation,
            mic,
            solo_mic: None,
            keyswitch_notes,
            play_direction: "up".to_string(),
            percussion,
            single_attack_key,
            pinned_articulation: None,
            trigger_articulation: None,
            engine_choke_group: None,
            engine_choke_on: Vec::new(),
            cc1: 64,
            cc2: 0,
            cc58: 0,
            cc_values: [0; 128],
            channel_aftertouch: 0,
            poly_aftertouch: [0; 128],
            cc64_held: false,
            cc64_value: 0,
            no_pedal_articulation: None,
            con_sordino: false,
            legato_enabled: true,
            legato_expressive: false, // default: low-latency mode
            sord_filter: BiquadFilter::lowpass(filter::SORD_FC, filter::SORD_Q, sample_rate),
            // Pre-size note-keyed maps to the full MIDI range so note-on never
            // reallocates them on the audio thread.
            held_notes: HashMap::with_capacity(128),
            body_voiced: std::collections::HashSet::with_capacity(128),
            deferred_note_off_velocities: HashMap::with_capacity(128),
            legato_state: LegatoState::Idle,
            legato_fade_frames,
            cc1_ramp_frames,
            release_frames,
            zone_rr_counter: 0,
            zone_rr_random_state: 0x9e37_79b9_7f4a_7c15,
            zone_rr_last_slots: HashMap::with_capacity(128),
            zone_indices_scratch: Vec::with_capacity(32),
            zone_choked_scratch: Vec::with_capacity(16),
            zone_capped_scratch: Vec::with_capacity(16),
            mic_ids,
            cache_misses: Cell::new(0),
            sample_misses: Cell::new(0),
            recent_cache_misses: RefCell::new(VecDeque::with_capacity(RECENT_MISS_LIMIT)),
            recent_sample_misses: RefCell::new(VecDeque::with_capacity(RECENT_MISS_LIMIT)),
        }
    }

    /// Mic ids in declaration order, parallel to the multi-buffer slice
    /// expected by [`render_multi`]. Empty when the library has no `mics`.
    pub fn mic_ids(&self) -> &[String] {
        &self.mic_ids
    }

    /// Resolve a mic id (string) to its index in `mic_ids()`.
    /// `""` or `"default"` resolves to 0 when at least one mic exists.
    pub fn mic_index_for(&self, mic_id: &str) -> Option<u8> {
        if self.mic_ids.is_empty() {
            return None;
        }
        if mic_id.is_empty() || mic_id == "default" {
            return Some(0);
        }
        self.mic_ids
            .iter()
            .position(|m| m == mic_id)
            .map(|i| i as u8)
    }

    /// Cheap clone of the underlying sample cache. Hand this to a
    /// background thread to populate the cache without blocking the
    /// audio thread.
    pub fn cache_handle(&self) -> SampleCache {
        self.cache.clone_handle()
    }

    /// Owned copy of every sample path the patch references — lets a
    /// background-preloader thread iterate without borrowing the engine.
    pub fn sample_paths_owned(&self) -> Vec<std::path::PathBuf> {
        self.patch.sample_paths().cloned().collect()
    }

    /// Sample paths ordered for "middle-out" preload — closest to
    /// `center` (middle C if you pass 60) first, extremes last. Grooves +
    /// wavetables are appended at the end. Use this when feeding the
    /// background preloader so the most-played range becomes audible first.
    pub fn sample_paths_centered(&self, center: u8) -> Vec<std::path::PathBuf> {
        self.patch.sample_paths_centered(center)
    }

    /// How many of `total_samples()` are currently decoded into the cache.
    pub fn loaded_sample_count(&self) -> usize {
        self.cache.len()
    }

    pub fn loaded_sample_bytes(&self) -> usize {
        self.cache.bytes()
    }

    pub fn evict_cache_until_under_budget(&self, budget_bytes: usize) -> EvictStats {
        self.cache.evict_until_under_budget(budget_bytes)
    }

    /// Total number of samples the patch references (loaded or not).
    pub fn total_sample_count(&self) -> usize {
        self.patch.total_samples()
    }

    /// Read-only access to the engine's underlying [`PlayerPatch`] — used
    /// by callers that need to inspect the loaded library spec (tags,
    /// instrument, sections) for things like preload prioritization.
    pub fn patch(&self) -> &crate::PlayerPatch {
        &self.patch
    }

    /// Decode all indexed samples into the cache before live playback.
    pub fn preload_samples(&mut self) -> PreloadStats {
        let start = std::time::Instant::now();
        let total = self.patch.total_samples();
        let stats = self
            .cache
            .preload(self.patch.sample_paths().map(|p| p.as_path()));
        tracing::info!(
            "signal-sampler: preloaded {}/{} samples ({:.1} MiB PCM) in {:.2}s",
            self.cache.len(),
            total,
            stats.bytes as f64 / 1024.0 / 1024.0,
            start.elapsed().as_secs_f64()
        );
        stats
    }

    /// Decode the primary sample needed for a pending note-on on the caller's
    /// thread. This keeps the audio callback non-blocking while avoiding the
    /// "first note is silent until background preload reaches it" failure mode.
    pub fn warm_note_samples(&self, note: u8, velocity: u8) -> PreloadStats {
        let path = if self.patch.is_zoned() {
            self.patch.resolve_zone(note, velocity, 0).map(|z| z.path)
        } else {
            let dynamic = self.short_note_dynamic(velocity);
            self.patch
                .resolve(
                    &self.section,
                    &self.articulation,
                    &self.mic,
                    &dynamic,
                    note,
                    "",
                    0,
                )
                .map(|(path, _)| path)
        };
        let Some(path) = path else {
            return PreloadStats {
                failed: 1,
                ..PreloadStats::default()
            };
        };
        if self.cache.get_loaded(&path).is_some() {
            return PreloadStats::default();
        }
        match self.cache.get(&path) {
            Ok(data) => PreloadStats {
                loaded: 1,
                bytes: data.decoded_bytes(),
                failed: 0,
            },
            Err(_) => PreloadStats {
                failed: 1,
                ..PreloadStats::default()
            },
        }
    }

    // ── Configuration ─────────────────────────────────────────────────────────

    /// Switch to a different section. Resets RR counters.
    pub fn set_section(&mut self, section_id: impl Into<String>) {
        self.section = section_id.into();
        self.rr.borrow_mut().reset();
        self.zone_rr_counter = 0;
        self.zone_rr_last_slots.clear();
    }

    /// Switch to a different microphone position.
    pub fn set_mic(&mut self, mic_id: impl Into<String>) {
        self.mic = mic_id.into();
    }

    /// Restrict zoned playback to a single mic. `Some("Mix")` makes only zones
    /// tagged with that mic fire — the fix for multi-mic libraries (like CSS)
    /// that ship every mic in one zone set but declare no `mics` block, so
    /// without this every mic folds to bus 0 and sounds at once. `None` (the
    /// default) keeps the play-all-mics behaviour for multi-mic mixing.
    pub fn set_solo_mic(&mut self, mic_id: Option<String>) {
        self.solo_mic = mic_id.filter(|m| !m.is_empty());
    }

    /// Warm (decode into cache, on the caller's thread) every attack zone a
    /// note-on at `note` could trigger under the CURRENT articulation pin +
    /// solo mic — across all dynamic layers and round-robins — so the first
    /// hit at any velocity is never silent. Selection mirrors the live trigger
    /// path (unlike [`warm_note_samples`](Self::warm_note_samples), which uses
    /// the pin/mic-agnostic `resolve_zone` and can warm the wrong articulation).
    /// Returns how many samples were decoded; a no-op for non-zoned patches.
    pub fn warm_note(&self, note: u8) -> crate::engine::cache::PreloadStats {
        use crate::engine::cache::PreloadStats;
        if !self.patch.is_zoned() {
            return PreloadStats::default();
        }
        let artic = self.effective_articulation();
        let mut stats = PreloadStats::default();
        for (i, z) in self.patch.spec.zones.iter().enumerate() {
            if !zone_trigger_matches(z, ZoneTrigger::Attack) {
                continue;
            }
            if note < z.key_min || note > z.key_max {
                continue;
            }
            if let Some(p) = artic {
                if !z.articulation.is_empty() && !z.articulation.eq_ignore_ascii_case(p) {
                    continue;
                }
            }
            if let Some(solo) = &self.solo_mic {
                if !z.mic.eq_ignore_ascii_case(solo) {
                    continue;
                }
            }
            let path = &self.patch.zone_paths[i];
            if self.cache.get_loaded(path).is_some() {
                continue;
            }
            match self.cache.get(path) {
                Ok(data) => {
                    stats.loaded += 1;
                    stats.bytes += data.decoded_bytes();
                }
                Err(_) => stats.failed += 1,
            }
        }
        stats
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

    /// Pin this engine to a single articulation (percussion kits). When set,
    /// only zones whose `articulation` matches fire, regardless of the
    /// incoming key — so one drum pack can be split across several
    /// performance notes (hats Closed vs Open, snare Hit vs Cross Stick).
    /// `None` (or an empty string) clears the pin.
    pub fn pin_articulation(&mut self, artic: Option<String>) {
        self.pinned_articulation = artic.filter(|s| !s.is_empty());
    }

    /// Set an engine-wide choke group. Voices join it so they can be silenced.
    /// `choke_on` lists the articulations that actually trigger the choke:
    /// empty = monophonic (every hit chokes — hi-hats); non-empty = only those
    /// articulations choke (cymbals: `["Choke"]` so crashes ring but the choke
    /// stops them). `None`/empty group clears it (engine stays polyphonic).
    pub fn set_choke_group(&mut self, group: Option<&str>, choke_on: &[String]) {
        self.engine_choke_group = group.filter(|s| !s.is_empty()).map(stable_group_hash);
        self.engine_choke_on = choke_on
            .iter()
            .filter(|s| !s.is_empty())
            .map(|s| s.to_ascii_lowercase())
            .collect();
    }

    /// Whether this note-on should silence the engine choke group: true when a
    /// group is set and either it's monophonic (`choke_on` empty) or the active
    /// articulation is one of the configured choke triggers.
    fn should_engine_choke(&self) -> bool {
        if self.engine_choke_group.is_none() {
            return false;
        }
        if self.engine_choke_on.is_empty() {
            return true;
        }
        match self
            .trigger_articulation
            .as_deref()
            .or(self.pinned_articulation.as_deref())
        {
            Some(a) => self
                .engine_choke_on
                .iter()
                .any(|c| c.eq_ignore_ascii_case(a)),
            None => false,
        }
    }

    /// Whether a zone should fire for this `(note, velocity, trigger)`,
    /// honoring percussion mode and any pinned articulation.
    ///
    /// - Pinned articulation: match by articulation only, key ignored.
    /// - Single-key percussion (kick, tom): match any note.
    /// - Otherwise: the zone's key range gates as usual (pitched samplers,
    ///   multi-articulation drum packs addressed by their native keys).
    fn zone_selected(
        &self,
        zone: &crate::spec::ZoneSpec,
        note: u8,
        velocity: u8,
        trigger: ZoneTrigger,
    ) -> bool {
        if !zone_trigger_matches(zone, trigger) {
            return false;
        }
        if velocity < zone.vel_min || velocity > zone.vel_max {
            return false;
        }
        // An explicit pin (percussion kits / routed triggers) matches by
        // articulation ONLY, ignoring the key — any routed note plays the pinned
        // articulation's zone.
        if let Some(pin) = self
            .trigger_articulation
            .as_ref()
            .or(self.pinned_articulation.as_ref())
        {
            return zone.articulation.eq_ignore_ascii_case(pin);
        }
        if self.percussion && self.single_attack_key {
            return true;
        }
        // Melodic libraries: only the currently-selected articulation fires
        // (set by keyswitch / CC58 / `set_articulation`). Without this filter,
        // every articulation in a multi-articulation zone set (e.g. CSS ships
        // ~20 in one set) would sound at once. An empty selection = no filter
        // (legacy behaviour for libraries that don't switch articulations).
        if !self.articulation.is_empty()
            && !zone.articulation.is_empty()
            && !zone.articulation.eq_ignore_ascii_case(&self.articulation)
        {
            return false;
        }
        // Directional zones (CSS legato records up- vs down-transitions
        // separately): only the current play direction fires. Non-directional
        // zones (shorts, sustains) carry an empty `direction` and are unaffected.
        if !zone.direction.is_empty() && !zone.direction.eq_ignore_ascii_case(&self.play_direction) {
            return false;
        }
        // CC1 dynamic layer: CSS sustains/legato record p/mf/ff at full velocity
        // range and crossfade them by CC1 — without picking one layer they all
        // stack. Velocity-driven (short) articulations keep their vel_min/max
        // gating and are left alone here.
        if !zone.dynamic.is_empty() {
            if let Some(dyn_label) = self.current_zone_dynamic_cc1() {
                if !zone.dynamic.eq_ignore_ascii_case(&dyn_label) {
                    return false;
                }
            }
        }
        note >= zone.key_min && note <= zone.key_max
    }

    /// The dominant CC1 dynamic-layer label for the current articulation, or
    /// `None` for articulations that aren't CC1-controlled (shorts) or have no
    /// declared dynamic layers. Used to pick a single zoned dynamic layer.
    fn current_zone_dynamic_cc1(&self) -> Option<String> {
        let kind = self
            .patch
            .spec
            .articulation(&self.articulation)
            .map(|a| a.kind.clone());
        if matches!(
            kind,
            Some(ArticulationKind::Short | ArticulationKind::OneShot)
        ) {
            return None;
        }
        let layers = self.active_cc1_layers();
        if layers.is_empty() {
            return None;
        }
        let (lo, hi, blend) = Self::cc1_blend(layers, self.cc1);
        Some(if blend >= 0.5 { hi } else { lo })
    }

    /// The articulation that drives zone selection / warming: an explicit pin
    /// wins (percussion / routed triggers), otherwise the live selection set by
    /// keyswitch / CC58 / [`set_articulation`](Self::set_articulation). `None`
    /// when nothing is selected (legacy "all articulations" behaviour).
    fn effective_articulation(&self) -> Option<&str> {
        self.trigger_articulation
            .as_deref()
            .or(self.pinned_articulation.as_deref())
            .or(if self.articulation.is_empty() {
                None
            } else {
                Some(self.articulation.as_str())
            })
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
        self.articulation = self.remap_sordino(&self.articulation, active);
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

    pub fn set_voice_config(&mut self, config: &VoiceConfig) {
        if config.polyphony > 0 {
            self.voices.set_max_voices(config.polyphony as usize);
        }
        if !config.voice_steal.trim().is_empty() {
            self.voices
                .set_steal_policy(VoiceStealPolicy::from_str(&config.voice_steal));
        }
    }

    pub fn max_voices(&self) -> usize {
        self.voices.max_voices()
    }

    pub fn voice_steal_policy(&self) -> &'static str {
        match self.voices.steal_policy() {
            VoiceStealPolicy::ReleaseFirstQuietest => "release_first_quietest",
            VoiceStealPolicy::Oldest => "oldest",
            VoiceStealPolicy::Quietest => "quietest",
            VoiceStealPolicy::SameNoteFirst => "same_note_first",
            VoiceStealPolicy::DropNew => "drop_new",
        }
    }

    pub fn stolen_voices(&self) -> usize {
        self.voices.stolen_count()
    }

    pub fn cache_misses(&self) -> usize {
        self.cache_misses.get()
    }

    pub fn sample_misses(&self) -> usize {
        self.sample_misses.get()
    }

    pub fn recent_cache_misses(&self) -> Vec<String> {
        self.recent_cache_misses.borrow().iter().cloned().collect()
    }

    pub fn recent_sample_misses(&self) -> Vec<String> {
        self.recent_sample_misses.borrow().iter().cloned().collect()
    }

    // ── MIDI input ────────────────────────────────────────────────────────────

    /// Process a MIDI note-on event.
    /// Note-on with a per-trigger articulation override (percussion routing).
    /// `articulation` fires only the matching articulation's zones for this
    /// hit, ignoring key — letting one shared drum pack serve many routed
    /// notes. `None` behaves like [`note_on`](Self::note_on).
    pub fn note_on_articulated(&mut self, note: u8, velocity: u8, articulation: Option<&str>) {
        let prev = self.trigger_articulation.take();
        self.trigger_articulation = articulation
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string());
        self.note_on(note, velocity);
        self.trigger_articulation = prev;
    }

    pub fn note_on(&mut self, note: u8, velocity: u8) {
        if velocity == 0 {
            self.note_off(note);
            return;
        }

        // Velocity-sensitive keyswitches (CSS-style): a keyswitch note selects
        // an articulation / mode and does NOT sound.
        if self.try_keyswitch(note, velocity) {
            return;
        }

        if self.patch.is_zoned() {
            // CSS-style legato: a legato articulation, with another note already
            // held, plays a delayed directional transition (the famous latency).
            // The first note of a phrase — or any non-legato articulation —
            // sounds immediately.
            let is_legato = matches!(
                self.patch
                    .spec
                    .articulation(&self.articulation)
                    .map(|a| a.kind.clone()),
                Some(ArticulationKind::Legato)
            );
            let other_held = self.held_notes.keys().any(|&n| n != note);
            self.held_notes.insert(note, velocity);
            if is_legato && self.legato_enabled && other_held {
                // Sets LegatoState::Pending with the velocity-mapped delay;
                // render() fires fire_legato when it elapses.
                self.initiate_legato(note, velocity);
            } else {
                if is_legato {
                    self.play_direction = "up".to_string();
                }
                self.trigger_zoned(note, velocity, ZoneTrigger::Attack, true);
            }
            return;
        }

        // Legzero: same note re-trigger while sustain pedal is held.
        let legzero = self.cc64_held && self.held_notes.contains_key(&note);

        // Whether any other note is currently held (legato condition).
        let other_held = self.held_notes.keys().any(|&n| n != note);

        self.held_notes.insert(note, velocity);
        self.deferred_note_off_velocities.remove(&note);

        let artic_kind = self
            .patch
            .spec
            .articulation(&self.articulation)
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
                    artic = %self.articulation,
                    "note_on: unknown articulation — skipping"
                );
            }
        }
    }

    /// Process a MIDI note-off event.
    pub fn note_off(&mut self, note: u8) {
        self.note_off_with_velocity(note, self.cc1);
    }

    pub fn note_off_with_velocity(&mut self, note: u8, release_velocity: u8) {
        let release_frames = self.pedal_release_frames();
        if self.cc64_held {
            // Sustain pedal held — defer release.
            self.deferred_note_off_velocities
                .insert(note, release_velocity);
            tracing::debug!(note, release_velocity, "pedal defer");
            return;
        }
        self.held_notes.remove(&note);
        self.deferred_note_off_velocities.remove(&note);
        if self.patch.is_zoned() {
            self.trigger_zoned(note, release_velocity, ZoneTrigger::Release, false);
            self.voices
                .note_off_with_release_frames(note, Some(release_frames));
            return;
        }
        self.do_note_off_with_release_frames(note, release_velocity, release_frames);
    }

    /// Release every held/sounding note and reset pedal-deferred state.
    pub fn all_notes_off(&mut self) {
        self.held_notes.clear();
        self.deferred_note_off_velocities.clear();
        self.cc64_held = false;
        self.cc64_value = 0;
        if let Some(orig) = self.no_pedal_articulation.take() {
            self.articulation = orig;
        }
        self.legato_state = LegatoState::Idle;
        self.voices.all_notes_off();
    }

    pub fn panic(&mut self) {
        self.held_notes.clear();
        self.deferred_note_off_velocities.clear();
        self.cc64_held = false;
        self.cc64_value = 0;
        if let Some(orig) = self.no_pedal_articulation.take() {
            self.articulation = orig;
        }
        self.legato_state = LegatoState::Idle;
        self.voices.panic();
    }

    /// Zone-mode trigger. Looks up every zone matching `(note, velocity)`
    /// and spawns one `Zoned` voice per **mic group** within the match
    /// set. RR-cycling happens within each mic group so a kick with 7
    /// mics × 8 RR slots fires 7 simultaneous voices that share the
    /// same RR index.
    fn trigger_zoned(
        &mut self,
        note: u8,
        velocity: u8,
        trigger: ZoneTrigger,
        record_empty_miss: bool,
    ) {
        // Bucket matching zones by mic id so each mic gets its own
        // round-robin within the candidate set.
        let mut by_mic: std::collections::BTreeMap<String, Vec<usize>> =
            std::collections::BTreeMap::new();
        for (i, z) in self.patch.spec.zones.iter().enumerate() {
            if self.zone_selected(z, note, velocity, trigger) {
                by_mic.entry(z.mic.clone()).or_default().push(i);
            }
        }
        self.trigger_zoned_groups(by_mic, Some(note), velocity, trigger, record_empty_miss);
    }

    fn trigger_cc_zones(&mut self, controller: u8, old_value: u8, value: u8) {
        let mut by_mic: std::collections::BTreeMap<String, Vec<usize>> =
            std::collections::BTreeMap::new();
        for (i, z) in self.patch.spec.zones.iter().enumerate() {
            if zone_cc_trigger_crossed(z, controller, old_value, value) {
                by_mic.entry(z.mic.clone()).or_default().push(i);
            }
        }
        self.trigger_zoned_groups(by_mic, None, value, ZoneTrigger::Cc, false);
    }

    fn trigger_aftertouch_zones(&mut self, note: Option<u8>, old_value: u8, value: u8) {
        let mut by_mic: std::collections::BTreeMap<String, Vec<usize>> =
            std::collections::BTreeMap::new();
        for (i, z) in self.patch.spec.zones.iter().enumerate() {
            if zone_aftertouch_trigger_crossed(z, note, old_value, value) {
                by_mic.entry(z.mic.clone()).or_default().push(i);
            }
        }
        self.trigger_zoned_groups(by_mic, note, value, ZoneTrigger::Aftertouch, false);
    }

    fn trigger_event_zones(&mut self, trigger: ZoneTrigger, velocity: u8) {
        let mut by_mic: std::collections::BTreeMap<String, Vec<usize>> =
            std::collections::BTreeMap::new();
        for (i, z) in self.patch.spec.zones.iter().enumerate() {
            if zone_trigger_matches(z, trigger) {
                by_mic.entry(z.mic.clone()).or_default().push(i);
            }
        }
        self.trigger_zoned_groups(by_mic, None, velocity, trigger, false);
    }

    fn trigger_zoned_groups(
        &mut self,
        mut by_mic: std::collections::BTreeMap<String, Vec<usize>>,
        event_note: Option<u8>,
        velocity: u8,
        trigger: ZoneTrigger,
        record_empty_miss: bool,
    ) {
        // Single-mic solo: keep only the requested mic's bucket so multi-mic
        // zone sets (CSS ships Main + Mix in one set, no `mics` block) don't
        // fold every mic to bus 0 and double. Centralised here so it applies to
        // every trigger path (attack / release / CC / aftertouch / event).
        if let Some(solo) = &self.solo_mic {
            by_mic.retain(|mic, _| mic.eq_ignore_ascii_case(solo));
        }
        if by_mic.is_empty() {
            if record_empty_miss {
                self.sample_misses
                    .set(self.sample_misses.get().saturating_add(1));
                self.record_sample_miss(format!(
                    "zone note={} velocity={velocity}",
                    event_note
                        .map(|note| note.to_string())
                        .unwrap_or_else(|| "event".to_string())
                ));
                tracing::debug!(note = ?event_note, velocity, trigger = ?trigger, "zone miss");
            }
            return;
        }

        let rr_idx = self.zone_rr_counter;
        self.zone_rr_counter = self.zone_rr_counter.wrapping_add(1);
        // Reuse scratch buffers across note-ons: `mem::take` swaps in an empty
        // Vec (no allocation) and we restore the grown buffer afterwards, so
        // these allocate only on the first few note-ons, never steady-state.
        let mut all_indices = std::mem::take(&mut self.zone_indices_scratch);
        all_indices.clear();
        all_indices.extend(by_mic.values().flatten().copied());
        // Packed key: trigger discriminant | note (+1, 0 = None) | velocity.
        let rr_key = ((trigger as u8 as u64) << 16)
            | ((event_note.map(|n| n as u64 + 1).unwrap_or(0)) << 8)
            | velocity as u64;
        let last_slot = self.zone_rr_last_slots.get(&rr_key).copied();
        let selected_rr_slot = select_zone_rr_slot(
            &self.patch.spec.zones,
            &all_indices,
            rr_idx,
            last_slot,
            &mut self.zone_rr_random_state,
        );
        self.zone_rr_last_slots.insert(rr_key, selected_rr_slot);
        self.zone_indices_scratch = all_indices;

        let mut choked_groups = std::mem::take(&mut self.zone_choked_scratch);
        choked_groups.clear();
        for indices in by_mic.values() {
            let z = &self.patch.spec.zones
                [select_zone_rr_index_by_slot(&self.patch.spec.zones, indices, selected_rr_slot)];
            for group in z.off_by.iter().filter(|group| !group.is_empty()) {
                push_unique_u64(&mut choked_groups, stable_group_hash(group));
            }
            if !z.choke_group.is_empty() {
                push_unique_u64(&mut choked_groups, stable_group_hash(&z.choke_group));
            }
        }
        // Engine-wide choke: silence the group when this hit is a choking one
        // (mono for hi-hats; only the "Choke" articulation for cymbals).
        if let Some(group) = self.engine_choke_group {
            if self.should_engine_choke() {
                push_unique_u64(&mut choked_groups, group);
            }
        }
        for &group in &choked_groups {
            self.voices
                .silence_choke_group(group, self.legato_fade_frames);
        }
        self.zone_choked_scratch = choked_groups;

        let mut capped_groups = std::mem::take(&mut self.zone_capped_scratch);
        capped_groups.clear();
        for indices in by_mic.values() {
            let z = &self.patch.spec.zones
                [select_zone_rr_index_by_slot(&self.patch.spec.zones, indices, selected_rr_slot)];
            if z.group_polyphony > 0 {
                if let Some(group) = zone_choke_group(z) {
                    push_unique_group_limit(&mut capped_groups, group, z.group_polyphony as usize);
                }
            }
        }
        for &(group, max_voices) in &capped_groups {
            if self.voices.active_choke_group_count(group) >= max_voices {
                self.voices
                    .silence_choke_group(group, self.legato_fade_frames);
            }
        }
        self.zone_capped_scratch = capped_groups;

        for (mic_id, indices) in by_mic {
            let pick =
                select_zone_rr_index_by_slot(&self.patch.spec.zones, &indices, selected_rr_slot);
            let z = &self.patch.spec.zones[pick];
            // Tag the new voice into the engine-wide choke group (if any) so
            // the next hit can silence it; an explicit zone choke wins.
            let choke_group = zone_choke_group(z).or(self.engine_choke_group);
            let path = self.patch.zone_paths[pick].clone();
            let Some(data) = self.cache.get_loaded(&path) else {
                self.cache_misses
                    .set(self.cache_misses.get().saturating_add(1));
                self.record_cache_miss(&path);
                tracing::trace!("zone sample not yet loaded: {}", path.display());
                continue;
            };

            let note = event_note.unwrap_or(z.root_key);
            // Percussion (and articulation-pinned) engines play at natural
            // pitch — the routed note is a trigger selector, not a transpose.
            // Without this a drum routed to any note other than its zone key
            // would detune (e.g. a tom on note 45 vs root 50 = -5 semitones).
            let semitones = if self.percussion || self.pinned_articulation.is_some() {
                0.0
            } else {
                note as f64 - z.root_key as f64
            };
            let total_cents = semitones * 100.0 + z.tune_cents as f64;
            let rate = 2.0f64.powf(total_cents / 1200.0);
            let gain = 10.0f32.powf(z.gain_db / 20.0);
            let mic_index = self.mic_index_for(&mic_id);

            // Percussion plays one-shot: the sample rings to its natural end
            // and note-off never cuts it (a drum is struck, not held). Pitched
            // samplers keep held/zoned semantics unless the zone says one-shot.
            let voice_kind = if trigger == ZoneTrigger::Release {
                VoiceKind::Release
            } else if zone_is_one_shot(z) || self.percussion {
                VoiceKind::Short
            } else {
                VoiceKind::Zoned
            };
            let mut voice =
                Voice::with_rate(data, note, voice_kind, rate, gain, self.release_frames)
                    .with_mic_index(mic_index)
                    .with_choke_group(choke_group)
                    .with_pan(z.pan)
                    .with_sample_window(
                        z.sample_start as usize,
                        (z.sample_end > 0).then_some(z.sample_end as usize),
                    );
            if z.playback_mode.eq_ignore_ascii_case("reverse") {
                voice = voice.reversed();
            } else if zone_is_alternating_loop(z) {
                voice = voice.with_alternating_loop(z.loop_start as usize, z.loop_end as usize);
            } else {
                voice = voice.with_forward_loop(z.loop_start as usize, z.loop_end as usize);
            }
            self.voices.spawn(voice);
        }
    }

    /// Process a MIDI CC event.
    pub fn cc(&mut self, controller: u8, value: u8) {
        let old_value = self
            .cc_values
            .get(controller as usize)
            .copied()
            .unwrap_or(0);
        if let Some(slot) = self.cc_values.get_mut(controller as usize) {
            *slot = value;
        }
        if self.patch.is_zoned() {
            self.trigger_cc_zones(controller, old_value, value);
        }
        match controller {
            1 => {
                self.cc1 = value;
                // Short-note articulations use CC1 to select sub-type (spiccato/
                // staccato/pizzicato/etc.); sustain articulations use it for dynamics.
                let is_short = self
                    .patch
                    .spec
                    .articulation(&self.articulation)
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
                self.rr.borrow_mut().reset_to(value as usize);
                self.zone_rr_counter = value as usize;
                self.zone_rr_last_slots.clear();
            }
            64 => {
                let was_held = self.cc64_held;
                self.cc64_value = value;
                self.cc64_held = value >= 64;

                // Sustain-pedal articulation swap (Keyscape-style libraries
                // ship distinct pedal-down body samples — e.g. `lacrm` strike
                // becomes `lacrped` while pedal is held). Keep `lacrm` as the
                // "no-pedal" default and snap back when the pedal lifts.
                if !was_held && self.cc64_held {
                    let restored = self.voices.repedal_releasing();
                    if restored > 0 {
                        tracing::debug!(restored, "pedal down repedal");
                    }
                    if self.patch.is_zoned() {
                        self.trigger_event_zones(ZoneTrigger::PedalDown, value);
                    }
                    if let Some(pedal_id) = self.find_pedal_pair(&self.articulation) {
                        self.no_pedal_articulation = Some(self.articulation.clone());
                        self.articulation = pedal_id;
                        // Mechanical pedal-down click — one-shot ambience layer
                        // (e.g. `lacrmechped`). Best-effort: skip silently if
                        // the pack doesn't ship one.
                        self.trigger_mechanical_pedal();
                    }
                } else if was_held && !self.cc64_held {
                    if let Some(orig) = self.no_pedal_articulation.take() {
                        self.articulation = orig;
                    }
                    if self.patch.is_zoned() {
                        self.trigger_event_zones(ZoneTrigger::PedalUp, value);
                    }
                }

                if was_held && !self.cc64_held {
                    let release_frames = self.pedal_release_frames();
                    // Pedal released — release only notes that received note-off
                    // while the pedal was down. Notes still physically held have
                    // no deferred note-off entry and must keep sounding.
                    let notes: Vec<(u8, u8)> = self
                        .deferred_note_off_velocities
                        .iter()
                        .map(|(&note, &release_velocity)| (note, release_velocity))
                        .collect();
                    tracing::debug!(
                        deferred = notes.len(),
                        still_held = self.held_notes.len().saturating_sub(notes.len()),
                        "pedal up release"
                    );
                    self.deferred_note_off_velocities.clear();
                    for (note, velocity) in notes {
                        self.held_notes.remove(&note);
                        if self.patch.is_zoned() {
                            self.trigger_zoned(note, velocity, ZoneTrigger::Release, false);
                            self.voices
                                .note_off_with_release_frames(note, Some(release_frames));
                        } else {
                            self.do_note_off_with_release_frames(note, velocity, release_frames);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    pub fn channel_aftertouch(&mut self, value: u8) {
        let old_value = self.channel_aftertouch;
        self.channel_aftertouch = value;
        if self.patch.is_zoned() {
            self.trigger_aftertouch_zones(None, old_value, value);
        }
    }

    pub fn poly_aftertouch(&mut self, note: u8, value: u8) {
        let old_value = self
            .poly_aftertouch
            .get(note as usize)
            .copied()
            .unwrap_or(0);
        if let Some(slot) = self.poly_aftertouch.get_mut(note as usize) {
            *slot = value;
        }
        if self.patch.is_zoned() {
            self.trigger_aftertouch_zones(Some(note), old_value, value);
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
            LegatoState::Pending {
                frames_remaining, ..
            } => {
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

    /// Render voices into per-mic stereo buffers. `outputs.len()` should
    /// match `mic_ids().len()` (or be 1 for libraries without an explicit
    /// mics array). Voices route to their `mic_index`. Buffers are NOT
    /// zeroed here — the caller owns clearing if accumulation isn't
    /// desired. No allocation occurs.
    pub fn render_multi(&mut self, outputs: &mut [Vec<f32>]) {
        let block_frames = outputs.first().map(|b| b.len() / 2).unwrap_or(0);

        // Advance legato countdown — duplicates the logic in render() but
        // avoids a borrow conflict with the per-mic split.
        let fire = match &mut self.legato_state {
            LegatoState::Pending {
                frames_remaining, ..
            } => {
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

        self.voices.render_multi(outputs);

        // Con Sordino filter: apply only to mic 0 (sord is a section/bus
        // effect, not a per-mic effect). Multi-mic libraries rarely use
        // sord, but keep behavior stable for the single-mic case.
        if self.con_sordino {
            if let Some(buf) = outputs.first_mut() {
                self.sord_filter.process(buf);
            }
        }
    }

    // ── Private — note trigger helpers ────────────────────────────────────────

    fn trigger_sustain(&mut self, note: u8) {
        let vib_blend = self.cc2_blend();
        let nv_scale = 1.0 - vib_blend;
        let vb_scale = vib_blend;

        // `make_voice` and the layer/vibrato helpers are `&self`, so the
        // current selection (`articulation`/`section`/`mic`) is passed by
        // reference directly — no per-note `String` clones.
        let vib_artic = self.find_vibrato_pair_id(&self.articulation);
        let release_frames = self.release_frames;

        // Compute CC1 layers separately for each articulation so that
        // Vibsus (4 dyns: ppp/p/mf/ff) and Nonvib (3 dyns: p/mf/ff) each
        // use their own crossfade map. Without this, Nonvib gets asked for
        // "ppp" samples that don't exist at very low CC1 values.
        let (nv_lo, nv_hi, nv_cc1_blend) = self.layers_for_artic(&self.articulation);
        let nv_lo_gain = nv_scale * (1.0 - nv_cc1_blend);
        let nv_hi_gain = nv_scale * nv_cc1_blend;

        if let Some(v) = self.make_voice(
            &self.articulation,
            &self.section,
            &self.mic,
            &nv_lo,
            note,
            "",
            VoiceKind::SustainNVLo,
            nv_lo_gain,
            release_frames,
        ) {
            self.voices.spawn(v);
        }
        if nv_hi != nv_lo {
            if let Some(v) = self.make_voice(
                &self.articulation,
                &self.section,
                &self.mic,
                &nv_hi,
                note,
                "",
                VoiceKind::SustainNVHi,
                nv_hi_gain,
                release_frames,
            ) {
                self.voices.spawn(v);
            }
        }

        // Vibrato voices — only if a vibrato-pair articulation exists.
        if let Some(vib_id) = vib_artic {
            let (vb_lo, vb_hi, vb_cc1_blend) = self.layers_for_artic(&vib_id);
            let vb_lo_gain = vb_scale * (1.0 - vb_cc1_blend);
            let vb_hi_gain = vb_scale * vb_cc1_blend;

            if let Some(v) = self.make_voice(
                &vib_id,
                &self.section,
                &self.mic,
                &vb_lo,
                note,
                "",
                VoiceKind::SustainVibLo,
                vb_lo_gain,
                release_frames,
            ) {
                self.voices.spawn(v);
            }
            if vb_hi != vb_lo {
                if let Some(v) = self.make_voice(
                    &vib_id,
                    &self.section,
                    &self.mic,
                    &vb_hi,
                    note,
                    "",
                    VoiceKind::SustainVibHi,
                    vb_hi_gain,
                    release_frames,
                ) {
                    self.voices.spawn(v);
                }
            }
        }
    }

    fn trigger_short(&mut self, note: u8, velocity: u8) {
        // Pick dynamic layer based on velocity and spec short_note_cc1_map.
        let dynamic = self.short_note_dynamic(velocity);
        let artic_kind = self
            .patch
            .spec
            .articulation(&self.articulation)
            .map(|a| a.kind.clone());
        let has_release_artic = self
            .patch
            .spec
            .articulation(&self.articulation)
            .and_then(|a| a.release_artic.as_ref())
            .is_some();
        // Voice kind + release length:
        // - has_release_artic (Keyscape pianos, orchestral shorts):
        //   body releases on note-off in KEY_UP_DECLICK_MS (effectively
        //   immediate). The dedicated release_artic supplies the tail.
        // - no release_artic: long natural decay; sample's tail IS the
        //   release.
        let is_oneshot = matches!(artic_kind, Some(ArticulationKind::OneShot));
        let voice_kind = if is_oneshot || has_release_artic {
            VoiceKind::SustainLo
        } else {
            VoiceKind::Short
        };
        let release_frames = if has_release_artic {
            ms_to_frames(KEY_UP_DECLICK_MS, self.sample_rate)
        } else {
            self.release_frames
        };
        // For multi-dynamic articulations (lacrm has 21 dyn-layers), the
        // velocity-selected sample IS already recorded at that loudness,
        // so applying velocity² gain on top double-attenuates the body.
        // Use unity gain when the artic has multiple dynamic layers and
        // fall back to a velocity curve when there's just one layer.
        let n_dyn = self
            .patch
            .spec
            .articulation(&self.articulation)
            .map(|a| a.dynamics.len())
            .unwrap_or(0);
        let gain = if n_dyn > 1 {
            1.0
        } else {
            velocity_gain(velocity)
        };
        if let Some(v) = self.make_voice(
            &self.articulation,
            &self.section,
            &self.mic,
            &dynamic,
            note,
            "",
            voice_kind.clone(),
            gain,
            release_frames,
        ) {
            self.voices.spawn(v);
            self.body_voiced.insert(note);
        } else {
            // Primary articulation has no sample for this note (e.g.
            // Keyscape LA Custom: `lacrm` covers notes 21,22,28-108 but
            // notes 23-27 live in `lacrmsp`). Try every other non-pedal,
            // non-Release OneShot articulation in the spec and use the
            // first that resolves a sample for this note. Without this
            // the user hears only the release-tail click on those notes.
            let primary = self.articulation.clone();
            let alt_ids: Vec<String> = self
                .patch
                .spec
                .articulations
                .iter()
                .filter(|a| matches!(a.kind, ArticulationKind::OneShot | ArticulationKind::Short))
                .filter(|a| {
                    let lower = a.id.to_ascii_lowercase();
                    !lower.contains("ped") && !lower.contains("mech")
                })
                .filter(|a| a.id != primary)
                .map(|a| a.id.clone())
                .collect();
            for alt_id in alt_ids {
                let alt_dyn = self.dynamic_for_artic(&alt_id, velocity);
                if let Some(v) = self.make_voice(
                    &alt_id,
                    &self.section,
                    &self.mic,
                    &alt_dyn,
                    note,
                    "",
                    voice_kind.clone(),
                    gain,
                    release_frames,
                ) {
                    self.voices.spawn(v);
                    self.body_voiced.insert(note);
                    break;
                }
            }
        }
    }

    fn trigger_legzero(&mut self, note: u8, _velocity: u8) {
        // Find a Legato-kind articulation for same-note retrigger.
        let Some(rz_id) = self.find_legato_artic_id(true) else {
            return;
        };
        let (lo_dyn, _, _) = self.current_layers_owned();
        let release_frames = self.release_frames;

        if let Some(v) = self.make_voice(
            &rz_id,
            &self.section,
            &self.mic,
            &lo_dyn,
            note,
            "",
            VoiceKind::Legato,
            1.0,
            release_frames,
        ) {
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
        let port_thresh = self
            .patch
            .spec
            .legato_engine
            .as_ref()
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

    fn fire_legato(&mut self, from_note: u8, to_note: u8, velocity: u8, portamento: bool) {
        let direction = if to_note > from_note { "up" } else { "down" };

        // Fade out old sustain voice.
        self.voices.silence_note(from_note, self.legato_fade_frames);

        // Zoned libraries (CSS): the directional legato zone IS the full note
        // (recorded transition + sustained body), selected by `play_direction`.
        // No separate background sustain — the Leg sample carries it.
        if self.patch.is_zoned() {
            self.play_direction = direction.to_string();
            self.trigger_zoned(to_note, velocity, ZoneTrigger::Attack, true);
            return;
        }

        // For portamento, look for Port-type articulation; otherwise Leg/NVLeg.
        let leg_id = if portamento {
            self.find_port_artic_id()
                .or_else(|| self.find_legato_artic_id(false))
        } else {
            self.find_legato_artic_id(false)
        };

        let Some(leg_id) = leg_id else {
            self.trigger_sustain(to_note);
            return;
        };

        let (lo_dyn, _, _) = self.current_layers_owned();
        let release_frames = self.release_frames;

        // Try directional first; fall back to directionless if not found.
        let v = self
            .make_voice(
                &leg_id,
                &self.section,
                &self.mic,
                &lo_dyn,
                to_note,
                direction,
                VoiceKind::Legato,
                1.0,
                release_frames,
            )
            .or_else(|| {
                self.make_voice(
                    &leg_id,
                    &self.section,
                    &self.mic,
                    &lo_dyn,
                    to_note,
                    "",
                    VoiceKind::Legato,
                    1.0,
                    release_frames,
                )
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
        self.patch
            .spec
            .articulations
            .iter()
            .filter(|a| a.kind == ArticulationKind::Legato)
            .filter(|a| a.id.starts_with("Sord") == want_sord)
            .find(|a| a.id.to_lowercase().contains("port"))
            .map(|a| a.id.clone())
    }

    /// Find a sustain-pedal-down sibling of `base` in the spec.
    ///
    /// Naming conventions across libraries vary; we try in priority:
    ///   1. `<base>ped`            — `lacrm` → `lacrmped`
    ///   2. `<base>` with trailing `m` removed + `ped` — `lacrm` → `lacrped`
    ///   3. any non-Release / non-mechanical articulation containing
    ///      `ped` (case-insensitive) — generic fallback
    /// Mechanical-pedal articulations (`mech`/`mechped`) are excluded so the
    /// pedal-down body and the pedal-down mechanical action stay separate.
    fn find_pedal_pair(&self, base: &str) -> Option<String> {
        let id = |s: &str| -> Option<String> {
            self.patch
                .spec
                .articulation(s)
                .filter(|a| !matches!(a.kind, ArticulationKind::Release))
                .map(|a| a.id.clone())
        };
        if let Some(v) = id(&format!("{base}ped")) {
            return Some(v);
        }
        let trimmed = base.strip_suffix('m').unwrap_or(base);
        if trimmed != base {
            if let Some(v) = id(&format!("{trimmed}ped")) {
                return Some(v);
            }
        }
        self.patch
            .spec
            .articulations
            .iter()
            .filter(|a| {
                !matches!(a.kind, ArticulationKind::Release)
                    && a.id.to_ascii_lowercase().contains("ped")
                    && !a.id.to_ascii_lowercase().contains("mech")
            })
            .map(|a| a.id.clone())
            .next()
    }

    /// Fire the mechanical pedal-down click (e.g. `lacrmechped`) once,
    /// at a moderate gain. Pure ambience layer — silently no-ops when the
    /// pack doesn't ship a mechanical-pedal articulation.
    fn trigger_mechanical_pedal(&mut self) {
        let id = self
            .patch
            .spec
            .articulations
            .iter()
            .find(|a| {
                a.id.to_ascii_lowercase().contains("mech")
                    && a.id.to_ascii_lowercase().contains("ped")
            })
            .map(|a| a.id.clone());
        let Some(mech_id) = id else {
            return;
        };
        let dyn_id = self.dynamic_for_artic(&mech_id, 100);
        let release_frames = self.release_frames;
        // Note doesn't matter — these patches map a fixed sample to any
        // input note, so we pick a stable middle-C anchor.
        if let Some(v) = self.make_voice(
            &mech_id,
            &self.section,
            &self.mic,
            &dyn_id,
            60,
            "",
            VoiceKind::Release,
            0.4,
            release_frames,
        ) {
            self.voices.spawn(v);
        }
    }

    fn do_note_off_with_release_frames(&mut self, note: u8, velocity: u8, release_frames: usize) {
        // Trigger release trail if the current articulation specifies one.
        let release_artic = self
            .patch
            .spec
            .articulation(&self.articulation)
            .and_then(|a| a.release_artic.clone());

        if let Some(rel_id) = release_artic
            .as_ref()
            .filter(|_| velocity >= RELEASE_SAMPLE_VELOCITY_MIN)
        {
            // For release-trail dynamics, MIDI controllers commonly send
            // release-velocity 64 as a "no info" default. If we got that
            // exact value, fall through to the body's note-on velocity
            // for layer pick — otherwise the picker would always use the
            // same dynamic regardless of how hard the player struck.
            let dyn_vel = if velocity == 64 {
                self.held_notes.get(&note).copied().unwrap_or(velocity)
            } else {
                velocity
            };
            let rel_dyn = self.dynamic_for_artic(rel_id, dyn_vel);
            // Scale by release velocity so soft note-offs whisper and
            // firm note-offs click clearly. (v/127)^1.5 sits between the
            // body's v² curve and a perceptually-flat linear ramp.
            let v_norm = (velocity as f32 / 127.0).clamp(0.0, 1.0);
            let gain = RELEASE_SAMPLE_GAIN_MAX * v_norm.powf(1.5);
            tracing::debug!(note, velocity, gain, dyn = %rel_dyn, "release sample");

            // Only fire release-tail if the body voice actually sounded —
            // otherwise the user hears just the mechanical key-up click in
            // isolation (a body-cache miss with no audible attack).
            if self.body_voiced.remove(&note) {
                if let Some(v) = self.make_voice(
                    &rel_id,
                    &self.section,
                    &self.mic,
                    &rel_dyn,
                    note,
                    "",
                    VoiceKind::Release,
                    gain,
                    release_frames,
                ) {
                    self.voices.spawn(v);
                }
            }
        } else {
            // No release_artic at all — still need to clear the tracker
            // so a future re-trigger doesn't carry stale state.
            self.body_voiced.remove(&note);
        }

        // Body voices were spawned with a short release window when the
        // articulation has a release_artic (the release sample supplies
        // the natural tail). Use the short cutoff here regardless of the
        // caller's `release_frames` — that parameter is meant for the
        // release voice's own decay, not for the body's fade.
        let body_release = if release_artic.is_some() {
            ms_to_frames(KEY_UP_DECLICK_MS, self.sample_rate)
        } else {
            release_frames
        };
        self.voices
            .note_off_with_release_frames(note, Some(body_release));
    }

    // ── Private — CC handlers ─────────────────────────────────────────────────

    fn pedal_release_frames(&self) -> usize {
        half_pedal_release_frames(
            self.release_frames,
            self.cc64_value,
            &self.patch.spec.dynamics.half_pedal_curve,
            self.patch.spec.dynamics.half_pedal_max_release_multiplier,
        )
    }

    /// Recompute and ramp all 4 sustain voice gains when CC1 or CC2 changes.
    fn update_sustain_gains(&mut self) {
        let vib_blend = self.cc2_blend();
        let nv = 1.0 - vib_blend;
        let vb = vib_blend;
        let ramp = self.cc1_ramp_frames;

        // Use per-articulation layer sets so NV and Vib voices each use
        // their own dynamics count (see trigger_sustain for full rationale).
        let vib_artic = self.find_vibrato_pair_id(&self.articulation);

        let (_, _, nv_blend) = self.layers_for_artic(&self.articulation);
        let (_, _, vb_blend) = vib_artic
            .as_deref()
            .map(|id| self.layers_for_artic(id))
            .unwrap_or((String::new(), String::new(), nv_blend));

        self.voices.update_sustain_blend(
            nv * (1.0 - nv_blend), // NVLo
            nv * nv_blend,         // NVHi
            vb * (1.0 - vb_blend), // VibLo
            vb * vb_blend,         // VibHi
            ramp,
        );
    }

    /// Compute the vibrato blend factor [0.0, 1.0] from the current CC2 value.
    ///
    /// `"on_off"` mode (CSSS): snaps at 64. All other libraries: linear.
    fn cc2_blend(&self) -> f32 {
        match self.patch.spec.dynamics.vibrato_mode.as_deref() {
            Some("on_off") => {
                if self.cc2 >= 64 {
                    1.0
                } else {
                    0.0
                }
            }
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

        self.patch
            .spec
            .articulations
            .iter()
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
    /// If `note` is a configured keyswitch, apply its velocity-mapped value and
    /// return `true` (the note is consumed — keyswitches don't sound).
    fn try_keyswitch(&mut self, note: u8, velocity: u8) -> bool {
        let Some(&idx) = self.keyswitch_notes.get(&note) else {
            return false;
        };
        let value = self
            .patch
            .spec
            .keyswitch
            .as_ref()
            .and_then(|ks| ks.notes.get(idx))
            .and_then(|kn| kn.value_for(velocity))
            .map(|s| s.to_string());
        if let Some(v) = value {
            self.apply_keyswitch_value(&v);
        }
        true
    }

    /// Apply a keyswitch value: `+`-joined tokens, each either an `@mode` token
    /// (`@legato-on`, `@legato-expressive`, `@sordino-off`, …) or a zone
    /// articulation tag (`Spiccato`, `Leg`, …).
    fn apply_keyswitch_value(&mut self, value: &str) {
        for tok in value.split('+').map(str::trim).filter(|t| !t.is_empty()) {
            match tok {
                "@ignore" => {}
                "@legato-on" => self.legato_enabled = true,
                "@legato-off" => self.legato_enabled = false,
                "@legato-low" => {
                    self.legato_enabled = true;
                    self.legato_expressive = false;
                }
                "@legato-expressive" => {
                    self.legato_enabled = true;
                    self.legato_expressive = true;
                }
                "@sordino-on" => self.set_con_sordino(true),
                "@sordino-off" => self.set_con_sordino(false),
                t if t.starts_with('@') => tracing::debug!("unknown keyswitch token {t:?}"),
                tag => self.select_articulation_tag(tag),
            }
        }
    }

    /// Select an articulation by tag/id/label: prefer a matching declared
    /// articulation, else use the tag verbatim (zone tags like `"Leg"` aren't
    /// always declared as articulations). Honours Con Sordino remapping.
    fn select_articulation_tag(&mut self, tag: &str) {
        let id = self
            .patch
            .spec
            .articulations
            .iter()
            .find(|a| a.id.eq_ignore_ascii_case(tag) || a.label.eq_ignore_ascii_case(tag))
            .map(|a| a.id.clone())
            .unwrap_or_else(|| tag.to_string());
        self.articulation = self.remap_sordino(&id, self.con_sordino);
    }

    fn apply_cc58(&mut self) {
        let Some(ks) = self.patch.spec.keyswitch.as_ref() else {
            return;
        };
        let Some(label) = ks.cc58_function(self.cc58) else {
            return;
        };
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
                self.select_articulation_tag("Leg");
                return;
            }
            "Sustain: Expressive Legato" => {
                self.legato_enabled = true;
                self.legato_expressive = true;
                self.select_articulation_tag("Leg");
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
        &self,
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

        let rr_idx = self
            .rr
            .borrow_mut()
            .next(section, artic_id, dynamic, max_rr);

        let (path, sampled_note) = match self
            .patch
            .resolve(section, artic_id, mic, dynamic, note, direction, rr_idx)
        {
            Some(resolved) => resolved,
            None => {
                self.sample_misses
                    .set(self.sample_misses.get().saturating_add(1));
                self.record_sample_miss(format!(
                    "section={section} artic={artic_id} mic={mic} dynamic={dynamic} note={note} direction={direction:?} rr={rr_idx}"
                ));
                tracing::debug!(
                    section,
                    artic = artic_id,
                    mic,
                    dynamic,
                    note,
                    direction = ?direction,
                    rr = rr_idx,
                    "sample miss"
                );
                return None;
            }
        };

        // Audio-thread fast path: skip silently when not yet preloaded.
        let Some(data) = self.cache.get_loaded(&path) else {
            self.cache_misses
                .set(self.cache_misses.get().saturating_add(1));
            self.record_cache_miss(&path);
            tracing::trace!("sample not yet loaded: {}", path.display());
            return None;
        };

        let semitone_offset = note as i16 - sampled_note as i16;
        let mic_index = self.mic_index_for(mic);
        // Cap Release-voice lifetime. lacr release-tail FLACs are 30 s of
        // mostly silence; without this they live in the pool until the
        // sample's natural end and chew through the 64-voice budget, which
        // forces voice-stealing on every subsequent key press and produces
        // intermittent silence/clicks. 2 s is plenty for any real damper
        // click + decay tail.
        let release_lifetime_frames =
            (RELEASE_MAX_LIFETIME_MS as usize) * (self.sample_rate as usize) / 1000;
        let voice = Voice::new(
            data,
            note,
            kind.clone(),
            semitone_offset.clamp(i8::MIN as i16, i8::MAX as i16) as i8,
            gain,
            release_frames,
        )
        .with_mic_index(mic_index);
        let voice = if matches!(kind, VoiceKind::Release) {
            let end = release_lifetime_frames.min(voice.data_num_frames());
            voice.with_sample_window(0, Some(end))
        } else {
            voice
        };
        Some(voice)
    }

    /// Returns `(lo_label, hi_label, hi_blend)` for a specific articulation ID
    /// at the current CC1 value, using that articulation's own dynamics count.
    fn layers_for_artic(&self, artic_id: &str) -> (String, String, f32) {
        let n = self
            .patch
            .spec
            .articulation(artic_id)
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

    fn record_cache_miss(&self, path: &Path) {
        push_recent(
            &mut self.recent_cache_misses.borrow_mut(),
            path.display().to_string(),
            RECENT_MISS_LIMIT,
        );
    }

    fn record_sample_miss(&self, label: String) {
        push_recent(
            &mut self.recent_sample_misses.borrow_mut(),
            label,
            RECENT_MISS_LIMIT,
        );
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
    /// Named orchestral dynamics (`pp`, `mf`, `fff`, etc.) divide the MIDI
    /// velocity range evenly across the articulation's `dynamics` array.
    /// Numeric Keyscape-style dynamics are different: the labels are the
    /// recorded source velocities, so pick the nearest recorded value.
    fn short_note_dynamic(&self, velocity: u8) -> String {
        self.dynamic_for_artic(&self.articulation, velocity)
    }

    fn dynamic_for_artic(&self, artic_id: &str, velocity: u8) -> String {
        let Some(artic) = self.patch.spec.articulation(artic_id) else {
            return "p".into();
        };
        if artic.dynamics.is_empty() {
            return "p".into();
        }
        dynamic_label_for_velocity(&artic.dynamics, velocity).unwrap_or_else(|| "p".into())
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
        let in_pizz_family = d
            .pizzicato_cc1_map
            .values()
            .any(|id| id == &self.articulation);
        let in_short_family = d
            .short_note_cc1_map
            .values()
            .any(|id| id == &self.articulation);

        if !in_short_family && !in_pizz_family {
            return; // not in a switchable short-note family
        }

        let map = if in_pizz_family {
            &d.pizzicato_cc1_map
        } else {
            &d.short_note_cc1_map
        };
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

        let candidates: Vec<&crate::spec::ArticulationSpec> = self
            .patch
            .spec
            .articulations
            .iter()
            .filter(|a| a.kind == ArticulationKind::Legato)
            .filter(|a| {
                a.instrument_filter.is_empty() || a.instrument_filter.contains(&self.section)
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
            candidates
                .iter()
                .find(|a| a.id.to_lowercase().contains("nv"))
        } else {
            candidates
                .iter()
                .find(|a| !a.id.to_lowercase().contains("nv"))
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
                VoiceKind::SustainNVLo => v.ramp_gain(nv_lo, ramp_frames),
                VoiceKind::SustainNVHi => v.ramp_gain(nv_hi, ramp_frames),
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

/// MIDI velocity → linear gain. Standard square-curve mapping
/// (`(v/127)^2`), which approximates the perceptual MIDI velocity curve
/// without needing a per-library response table.
#[inline]
pub fn velocity_gain(velocity: u8) -> f32 {
    let v = (velocity as f32 / 127.0).clamp(0.0, 1.0);
    v * v
}

fn push_recent(recent: &mut VecDeque<String>, value: String, limit: usize) {
    if recent.back() == Some(&value) {
        return;
    }
    if recent.len() >= limit {
        recent.pop_front();
    }
    recent.push_back(value);
}

fn zone_choke_group(zone: &crate::spec::ZoneSpec) -> Option<u64> {
    if !zone.choke_group.is_empty() {
        Some(stable_group_hash(&zone.choke_group))
    } else if !zone.group.is_empty() {
        Some(stable_group_hash(&zone.group))
    } else {
        None
    }
}

fn stable_group_hash(value: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn push_unique_u64(values: &mut Vec<u64>, value: u64) {
    if !values.contains(&value) {
        values.push(value);
    }
}

fn push_unique_group_limit(values: &mut Vec<(u64, usize)>, group: u64, limit: usize) {
    if let Some((_, existing)) = values.iter_mut().find(|(existing, _)| *existing == group) {
        *existing = (*existing).min(limit);
    } else {
        values.push((group, limit));
    }
}

fn zone_is_one_shot(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "one-shot" | "one_shot" | "oneshot"
    )
}

fn zone_is_release_trigger(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "release" | "note-release" | "note_release" | "key-up" | "key_up"
    )
}

fn zone_is_pedal_down_trigger(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "pedal-down" | "pedal_down" | "pedaldown" | "sustain-down" | "sustain_down"
    )
}

fn zone_is_pedal_up_trigger(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "pedal-up" | "pedal_up" | "pedalup" | "sustain-up" | "sustain_up"
    )
}

fn zone_is_cc_trigger(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "cc" | "cc-threshold" | "cc_threshold" | "controller"
    )
}

fn zone_is_aftertouch_trigger(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.trigger_mode.trim().to_ascii_lowercase().as_str(),
        "aftertouch"
            | "channel-aftertouch"
            | "channel_aftertouch"
            | "poly-aftertouch"
            | "poly_aftertouch"
            | "pressure"
    )
}

fn trigger_value_range(zone: &crate::spec::ZoneSpec) -> std::ops::RangeInclusive<u8> {
    let min = if zone.trigger_value_min == 0 {
        64
    } else {
        zone.trigger_value_min
    };
    let max = if zone.trigger_value_max == 0 {
        127
    } else {
        zone.trigger_value_max
    };
    min..=max
}

fn zone_cc_trigger_crossed(
    zone: &crate::spec::ZoneSpec,
    controller: u8,
    old_value: u8,
    value: u8,
) -> bool {
    if !zone_is_cc_trigger(zone) || zone.trigger_cc != controller {
        return false;
    }
    let range = trigger_value_range(zone);
    !range.contains(&old_value) && range.contains(&value)
}

fn zone_aftertouch_trigger_crossed(
    zone: &crate::spec::ZoneSpec,
    note: Option<u8>,
    old_value: u8,
    value: u8,
) -> bool {
    if !zone_is_aftertouch_trigger(zone) {
        return false;
    }
    if let Some(note) = note {
        if !(zone.key_min..=zone.key_max).contains(&note) {
            return false;
        }
    }
    let range = trigger_value_range(zone);
    !range.contains(&old_value) && range.contains(&value)
}

/// Whether a library is a percussion / drum-kit (plays at natural pitch).
/// Driven by the retag-derived `category` / `instrument` so no per-pack flag
/// is needed; sampled instruments (default empty/melodic) stay pitched.
fn spec_is_percussion(spec: &crate::spec::LibrarySpec) -> bool {
    let cat = spec.category.to_ascii_lowercase();
    if cat.contains("drum") || cat.contains("percussion") {
        return true;
    }
    let inst = spec.instrument.to_ascii_lowercase();
    const PERC: &[&str] = &[
        "kick", "snare", "tom", "hat", "ride", "crash", "china", "splash", "cymbal", "clap",
        "cowbell", "perc", "drum",
    ];
    PERC.iter().any(|p| inst.contains(p))
}

fn zone_trigger_matches(zone: &crate::spec::ZoneSpec, trigger: ZoneTrigger) -> bool {
    match trigger {
        ZoneTrigger::Attack => {
            !zone_is_release_trigger(zone)
                && !zone_is_pedal_down_trigger(zone)
                && !zone_is_pedal_up_trigger(zone)
                && !zone_is_cc_trigger(zone)
                && !zone_is_aftertouch_trigger(zone)
        }
        ZoneTrigger::Release => zone_is_release_trigger(zone),
        ZoneTrigger::PedalDown => zone_is_pedal_down_trigger(zone),
        ZoneTrigger::PedalUp => zone_is_pedal_up_trigger(zone),
        ZoneTrigger::Cc => zone_is_cc_trigger(zone),
        ZoneTrigger::Aftertouch => zone_is_aftertouch_trigger(zone),
    }
}

fn zone_is_alternating_loop(zone: &crate::spec::ZoneSpec) -> bool {
    matches!(
        zone.playback_mode.trim().to_ascii_lowercase().as_str(),
        "alternate" | "alternating" | "ping-pong" | "ping_pong"
    )
}

fn dynamic_label_for_velocity(dynamics: &[String], velocity: u8) -> Option<String> {
    if dynamics.is_empty() {
        return None;
    }

    let numeric = dynamics
        .iter()
        .map(|label| label.parse::<u8>())
        .collect::<Result<Vec<_>, _>>();
    if let Ok(values) = numeric {
        let (_, label) = values
            .iter()
            .zip(dynamics.iter())
            .min_by_key(|(value, _)| value.abs_diff(velocity))?;
        return Some(label.clone());
    }

    let n = dynamics.len();
    let idx = (velocity as usize * n / 128).min(n - 1);
    Some(dynamics[idx].clone())
}

fn select_zone_rr_slot(
    zones: &[crate::spec::ZoneSpec],
    indices: &[usize],
    rr_counter: usize,
    last_slot: Option<u32>,
    random_state: &mut u64,
) -> u32 {
    debug_assert!(!indices.is_empty());
    let mut rr_slots = indices
        .iter()
        .map(|&idx| zones[idx].rr_index)
        .collect::<Vec<_>>();
    rr_slots.sort_unstable();
    rr_slots.dedup();
    if rr_slots.len() == 1 {
        return rr_slots[0];
    }

    let mode = zones[indices[0]].rr_mode.trim().to_ascii_lowercase();
    match mode.as_str() {
        "random" => rr_slots[next_zone_random(random_state) % rr_slots.len()],
        "no-repeat-random" | "no_repeat_random" | "norepeat" | "no-repeat" => {
            let mut slot = rr_slots[next_zone_random(random_state) % rr_slots.len()];
            if Some(slot) == last_slot {
                let pos = rr_slots
                    .iter()
                    .position(|candidate| *candidate == slot)
                    .unwrap_or(0);
                slot = rr_slots[(pos + 1) % rr_slots.len()];
            }
            slot
        }
        _ => rr_slots[rr_counter % rr_slots.len()],
    }
}

fn select_zone_rr_index_by_slot(
    zones: &[crate::spec::ZoneSpec],
    indices: &[usize],
    selected: u32,
) -> usize {
    debug_assert!(!indices.is_empty());
    indices
        .iter()
        .copied()
        .find(|&idx| zones[idx].rr_index == selected)
        .unwrap_or(indices[0])
}

fn next_zone_random(state: &mut u64) -> usize {
    let mut x = *state;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *state = x;
    ((x.wrapping_mul(0x2545_f491_4f6c_dd1d)) >> 32) as usize
}

fn half_pedal_release_frames(
    base_frames: usize,
    cc64_value: u8,
    curve: &str,
    max_multiplier: f32,
) -> usize {
    if cc64_value == 0 || cc64_value >= 64 {
        return base_frames;
    }
    let position = (cc64_value as f32 / 63.0).clamp(0.0, 1.0);
    let shaped = match curve.trim().to_ascii_lowercase().as_str() {
        "squared" | "square" | "pow2" => position * position,
        "sqrt" | "square-root" | "square_root" => position.sqrt(),
        _ => position,
    };
    let max_multiplier = if max_multiplier > 0.0 {
        max_multiplier
    } else {
        HALF_PEDAL_MAX_RELEASE_MULTIPLIER
    }
    .max(1.0);
    let multiplier = 1.0 + (max_multiplier - 1.0) * shaped;
    ((base_frames as f32) * multiplier).round() as usize
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
    fn numeric_dynamic_labels_use_nearest_recorded_velocity() {
        let dynamics = [
            "24", "35", "42", "45", "54", "61", "68", "76", "82", "84", "89", "96", "102", "106",
            "110", "114", "118", "121", "124", "126", "127",
        ]
        .into_iter()
        .map(str::to_string)
        .collect::<Vec<_>>();

        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 40).as_deref(),
            Some("42")
        );
        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 100).as_deref(),
            Some("102")
        );
        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 120).as_deref(),
            Some("121")
        );
    }

    #[test]
    fn named_dynamic_labels_keep_even_velocity_bands() {
        let dynamics = ["pp", "mp", "f", "fff"]
            .into_iter()
            .map(str::to_string)
            .collect::<Vec<_>>();

        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 31).as_deref(),
            Some("pp")
        );
        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 32).as_deref(),
            Some("mp")
        );
        assert_eq!(
            dynamic_label_for_velocity(&dynamics, 96).as_deref(),
            Some("fff")
        );
    }

    fn test_zone(rr_index: u32) -> crate::spec::ZoneSpec {
        crate::spec::ZoneSpec {
            file: format!("rr-{rr_index}.wav"),
            key_min: 60,
            key_max: 60,
            root_key: 60,
            vel_min: 0,
            vel_max: 127,
            rr_index,
            rr_mode: String::new(),
            gain_db: 0.0,
            pan: 0.0,
            tune_cents: 0.0,
            sample_start: 0,
            sample_end: 0,
            loop_start: 0,
            loop_end: 0,
            playback_mode: String::new(),
            trigger_mode: String::new(),
            trigger_cc: 0,
            trigger_value_min: 0,
            trigger_value_max: 0,
            mic: String::new(),
            articulation: String::new(),
            dynamic: String::new(),
            direction: String::new(),
            group: String::new(),
            group_polyphony: 0,
            choke_group: String::new(),
            off_by: Vec::new(),
            section: String::new(),
            variant: String::new(),
        }
    }

    #[test]
    fn zone_rr_selection_uses_declared_rr_index() {
        let zones = vec![test_zone(10), test_zone(20), test_zone(30)];
        let indices = vec![0, 1, 2];
        let mut rng = 1;

        assert_eq!(select_zone_rr_slot(&zones, &indices, 0, None, &mut rng), 10);
        assert_eq!(select_zone_rr_slot(&zones, &indices, 1, None, &mut rng), 20);
        assert_eq!(select_zone_rr_slot(&zones, &indices, 2, None, &mut rng), 30);
        assert_eq!(select_zone_rr_slot(&zones, &indices, 3, None, &mut rng), 10);
        assert_eq!(select_zone_rr_index_by_slot(&zones, &indices, 20), 1);
    }

    #[test]
    fn zone_rr_selection_keeps_multimic_slots_aligned() {
        let zones = vec![test_zone(2), test_zone(0), test_zone(2), test_zone(0)];
        let left_mic = vec![0, 1];
        let right_mic = vec![2, 3];
        let mut rng = 1;
        let first_slot = select_zone_rr_slot(&zones, &left_mic, 0, None, &mut rng);
        let second_slot = select_zone_rr_slot(&zones, &left_mic, 1, None, &mut rng);

        assert_eq!(first_slot, 0);
        assert_eq!(second_slot, 2);
        assert_eq!(
            select_zone_rr_index_by_slot(&zones, &left_mic, first_slot),
            1
        );
        assert_eq!(
            select_zone_rr_index_by_slot(&zones, &right_mic, first_slot),
            3
        );
        assert_eq!(
            select_zone_rr_index_by_slot(&zones, &left_mic, second_slot),
            0
        );
        assert_eq!(
            select_zone_rr_index_by_slot(&zones, &right_mic, second_slot),
            2
        );
    }

    #[test]
    fn zone_rr_no_repeat_random_avoids_previous_slot() {
        let mut zones = vec![test_zone(0), test_zone(1), test_zone(2)];
        for zone in &mut zones {
            zone.rr_mode = "no-repeat-random".to_string();
        }
        let indices = vec![0, 1, 2];
        let mut rng = 0x9e37_79b9_7f4a_7c15;
        let mut last = None;

        for _ in 0..64 {
            let slot = select_zone_rr_slot(&zones, &indices, 0, last, &mut rng);
            assert_ne!(Some(slot), last);
            last = Some(slot);
        }
    }

    #[test]
    fn half_pedal_scales_release_frames() {
        let base = 24_000;

        assert_eq!(half_pedal_release_frames(base, 0, "", 0.0), base);
        assert!(half_pedal_release_frames(base, 32, "", 0.0) > base);
        assert_eq!(
            half_pedal_release_frames(base, 63, "", 0.0),
            (base as f32 * HALF_PEDAL_MAX_RELEASE_MULTIPLIER).round() as usize
        );
        assert_eq!(half_pedal_release_frames(base, 64, "", 0.0), base);
    }

    #[test]
    fn half_pedal_uses_authored_curve_and_multiplier() {
        let base = 24_000;
        let linear = half_pedal_release_frames(base, 32, "linear", 3.0);
        let squared = half_pedal_release_frames(base, 32, "squared", 3.0);
        let sqrt = half_pedal_release_frames(base, 32, "sqrt", 3.0);

        assert!(squared < linear);
        assert!(sqrt > linear);
        assert_eq!(half_pedal_release_frames(base, 63, "linear", 2.0), base * 2);
    }

    #[test]
    fn pedal_zone_triggers_are_exclusive_to_pedal_events() {
        let mut pedal_down = test_zone(0);
        pedal_down.trigger_mode = "pedal-down".to_string();
        let mut pedal_up = test_zone(0);
        pedal_up.trigger_mode = "pedal-up".to_string();

        assert!(zone_trigger_matches(&pedal_down, ZoneTrigger::PedalDown));
        assert!(!zone_trigger_matches(&pedal_down, ZoneTrigger::Attack));
        assert!(!zone_trigger_matches(&pedal_down, ZoneTrigger::Release));
        assert!(zone_trigger_matches(&pedal_up, ZoneTrigger::PedalUp));
        assert!(!zone_trigger_matches(&pedal_up, ZoneTrigger::Attack));
        assert!(!zone_trigger_matches(&pedal_up, ZoneTrigger::Release));
    }

    #[test]
    fn cc_zone_trigger_fires_on_threshold_entry_only() {
        let mut zone = test_zone(0);
        zone.trigger_mode = "cc-threshold".to_string();
        zone.trigger_cc = 11;
        zone.trigger_value_min = 40;
        zone.trigger_value_max = 80;

        assert!(zone_cc_trigger_crossed(&zone, 11, 39, 40));
        assert!(!zone_cc_trigger_crossed(&zone, 11, 40, 60));
        assert!(!zone_cc_trigger_crossed(&zone, 11, 20, 90));
        assert!(!zone_cc_trigger_crossed(&zone, 1, 39, 40));
        assert!(!zone_trigger_matches(&zone, ZoneTrigger::Attack));
    }

    #[test]
    fn aftertouch_zone_trigger_fires_on_threshold_entry_only() {
        let mut zone = test_zone(0);
        zone.trigger_mode = "aftertouch".to_string();
        zone.trigger_value_min = 30;
        zone.trigger_value_max = 90;

        assert!(zone_aftertouch_trigger_crossed(&zone, None, 29, 30));
        assert!(!zone_aftertouch_trigger_crossed(&zone, None, 40, 80));
        assert!(!zone_aftertouch_trigger_crossed(&zone, None, 29, 100));
        assert!(zone_aftertouch_trigger_crossed(&zone, Some(60), 29, 30));
        assert!(!zone_aftertouch_trigger_crossed(&zone, Some(61), 29, 30));
        assert!(!zone_trigger_matches(&zone, ZoneTrigger::Attack));
    }

    #[test]
    fn cc1_layer_selection() {
        // Simulate a 3-layer [p=0-42, mf=33-94, ff=85-127] setup.
        use crate::spec::Cc1Layer;

        // Build a minimal PlayerPatch-less engine via a stub spec.
        // We test current_layers_owned() logic in isolation by constructing
        // a mock layers slice and exercising the algorithm directly.
        let layers: &[Cc1Layer] = &[
            Cc1Layer {
                label: "p".into(),
                cc_range: [0, 42],
            },
            Cc1Layer {
                label: "mf".into(),
                cc_range: [33, 94],
            },
            Cc1Layer {
                label: "ff".into(),
                cc_range: [85, 127],
            },
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
    fn recent_miss_ring_keeps_bounded_latest_values() {
        let mut recent = VecDeque::new();
        for idx in 0..10 {
            push_recent(&mut recent, format!("miss-{idx}"), 4);
        }
        push_recent(&mut recent, "miss-9".to_string(), 4);

        assert_eq!(recent.len(), 4);
        assert_eq!(recent.front().map(String::as_str), Some("miss-6"));
        assert_eq!(recent.back().map(String::as_str), Some("miss-9"));
    }

    #[test]
    fn con_sordino_remap() {
        // Load the CSS spec and exercise the sordino switch logic using the
        // real articulation list.
        let specs_dir = {
            let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
            std::path::Path::new(&manifest)
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("specs")
        };
        let spec_path = specs_dir.join("cinematic-strings.toml");
        if !spec_path.exists() {
            return;
        }

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

    fn engine_from_styx(styx: &str) -> SampleEngine {
        let spec = crate::LibrarySpec::from_styx(styx).expect("parse styx");
        let patch = crate::PlayerPatch::from_spec(spec);
        SampleEngine::new(patch, 48_000, "", "")
    }

    #[test]
    fn percussion_single_key_fires_on_any_note() {
        // Kick-style pack: drum-kit, one articulation on a single key.
        let eng = engine_from_styx(
            "name \"k\"\n\
             category \"drum-kit\"\n\
             zones (\n\
               {\n\
                 file \"a.wav\"\n\
                 key_min 36\n\
                 key_max 36\n\
                 root_key 36\n\
                 vel_min 0\n\
                 vel_max 127\n\
                 articulation \"Hit\"\n\
               }\n\
             )\n",
        );
        assert!(eng.percussion && eng.single_attack_key);
        let z = &eng.patch().spec.zones[0];
        // Plays on the routed note even though it's off the zone's key.
        assert!(eng.zone_selected(z, 35, 100, ZoneTrigger::Attack));
        assert!(eng.zone_selected(z, 48, 100, ZoneTrigger::Attack));
        // Velocity still gates.
        assert!(!eng.zone_selected(
            &crate::spec::ZoneSpec {
                vel_min: 64,
                vel_max: 127,
                ..z.clone()
            },
            35,
            10,
            ZoneTrigger::Attack
        ));
    }

    #[test]
    fn keyswitch_note_selects_articulation_velocity_sensitive() {
        // Melodic patch (no drum category) with two-articulation zones + a
        // velocity-sensitive keyswitch note (CSS-style).
        let mut eng = engine_from_styx(
            "name \"s\"\n\
             keyswitch {\n\
               notes (\n\
                 {\n\
                   note \"C0\"\n\
                   label \"Sustain\"\n\
                   vel_map { 0-127 \"Leg\" }\n\
                 }\n\
                 {\n\
                   note \"C#0\"\n\
                   label \"Shorts\"\n\
                   vel_map {\n\
                     0-64 \"Spiccato\"\n\
                     65-127 \"Staccato\"\n\
                   }\n\
                 }\n\
               )\n\
             }\n\
             zones (\n\
               {file \"leg.wav\", key_min 60, key_max 60, root_key 60, vel_min 0, vel_max 127, articulation \"Leg\"}\n\
               {file \"spic.wav\", key_min 60, key_max 60, root_key 60, vel_min 0, vel_max 127, articulation \"Spiccato\"}\n\
               {file \"stac.wav\", key_min 60, key_max 60, root_key 60, vel_min 0, vel_max 127, articulation \"Staccato\"}\n\
             )\n",
        );
        let leg = eng.patch().spec.zones[0].clone();
        let spic = eng.patch().spec.zones[1].clone();
        let stac = eng.patch().spec.zones[2].clone();

        // A keyswitch note is consumed (returns true) and selects its artic.
        assert!(eng.try_keyswitch(12, 100)); // C0 = MIDI 12 → Leg
        assert_eq!(eng.articulation(), "Leg");
        assert!(eng.zone_selected(&leg, 60, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&spic, 60, 100, ZoneTrigger::Attack));

        // Velocity on C#0 picks the variant: soft = Spiccato, hard = Staccato.
        assert!(eng.try_keyswitch(13, 30));
        assert_eq!(eng.articulation(), "Spiccato");
        assert!(eng.zone_selected(&spic, 60, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&stac, 60, 100, ZoneTrigger::Attack));

        assert!(eng.try_keyswitch(13, 120));
        assert_eq!(eng.articulation(), "Staccato");
        assert!(eng.zone_selected(&stac, 60, 100, ZoneTrigger::Attack));

        // A normal (non-keyswitch) note is NOT consumed — it should sound.
        assert!(!eng.try_keyswitch(60, 100));
    }

    #[test]
    fn zoned_legato_delays_then_fires_directional() {
        // Legato articulation with directional zones (up/down) for two notes.
        // Build with synthetic zone_paths so the patch reads as zoned (is_zoned
        // checks resolved paths; the samples themselves never load in a test).
        let spec = crate::LibrarySpec::from_styx(
            "name \"s\"\n\
             articulations (\n\
               {id Leg, label Leg, kind @Legato}\n\
             )\n\
             zones (\n\
               {file \"u60.wav\", key_min 60, key_max 60, root_key 60, vel_min 0, vel_max 127, articulation \"Leg\", direction \"up\"}\n\
               {file \"d60.wav\", key_min 60, key_max 60, root_key 60, vel_min 0, vel_max 127, articulation \"Leg\", direction \"down\"}\n\
               {file \"u62.wav\", key_min 62, key_max 62, root_key 62, vel_min 0, vel_max 127, articulation \"Leg\", direction \"up\"}\n\
               {file \"d62.wav\", key_min 62, key_max 62, root_key 62, vel_min 0, vel_max 127, articulation \"Leg\", direction \"down\"}\n\
             )\n",
        )
        .expect("parse styx");
        let mut patch = crate::PlayerPatch::from_spec(spec);
        patch.zone_paths = (0..patch.spec.zones.len())
            .map(|i| std::path::PathBuf::from(format!("z{i}.wav")))
            .collect();
        let mut eng = SampleEngine::new(patch, 48_000, "", "");
        eng.set_articulation("Leg");
        assert!(eng.legato_enabled, "legato on by default");
        assert_eq!(eng.articulation(), "Leg");
        assert!(
            matches!(
                eng.patch().spec.articulation("Leg").map(|a| a.kind.clone()),
                Some(ArticulationKind::Legato)
            ),
            "Leg parsed as @Legato"
        );
        let u62 = eng.patch().spec.zones[2].clone();
        let d62 = eng.patch().spec.zones[3].clone();

        // First note sounds immediately — no pending legato.
        eng.note_on(60, 100);
        assert!(matches!(eng.legato_state, LegatoState::Idle));

        // Second note while the first is held → delayed (the CSS latency):
        // pending, not yet fired.
        eng.note_on(62, 100);
        assert!(
            matches!(eng.legato_state, LegatoState::Pending { .. }),
            "overlapping legato note should delay (latency), not fire instantly"
        );

        // Render past the (default 100ms) delay → fires; direction up (62 > 60).
        let frames = (eng.sample_rate as usize / 1000) * 200; // 200 ms
        let mut out = vec![0.0f32; frames * 2];
        eng.render(&mut out);
        assert!(matches!(eng.legato_state, LegatoState::Idle), "legato fired");
        assert_eq!(eng.play_direction, "up");
        // The up-zone for the target is now selected; the down-zone is not.
        assert!(eng.zone_selected(&u62, 62, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&d62, 62, 100, ZoneTrigger::Attack));
    }

    #[test]
    fn pinned_articulation_selects_by_artic_ignoring_key() {
        // Hats-style pack: multiple articulations on different keys.
        let mut eng = engine_from_styx(
            "name \"h\"\n\
             category \"drum-kit\"\n\
             zones (\n\
               {\n\
                 file \"c.wav\"\n\
                 key_min 42\n\
                 key_max 42\n\
                 root_key 42\n\
                 vel_min 0\n\
                 vel_max 127\n\
                 articulation \"Closed Tip\"\n\
               }\n\
               {\n\
                 file \"o.wav\"\n\
                 key_min 46\n\
                 key_max 46\n\
                 root_key 46\n\
                 vel_min 0\n\
                 vel_max 127\n\
                 articulation \"Open 1\"\n\
               }\n\
             )\n",
        );
        assert!(eng.percussion && !eng.single_attack_key);
        let closed = eng.patch().spec.zones[0].clone();
        let open = eng.patch().spec.zones[1].clone();

        // No pin: multi-key drum is addressed by its native keys.
        assert!(eng.zone_selected(&closed, 42, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&closed, 49, 100, ZoneTrigger::Attack));

        // Pin "Open 1": fires Open on any note, never Closed.
        eng.pin_articulation(Some("open 1".to_string())); // case-insensitive
        assert!(eng.zone_selected(&open, 53, 100, ZoneTrigger::Attack));
        assert!(eng.zone_selected(&open, 99, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&closed, 53, 100, ZoneTrigger::Attack));

        // Per-trigger (per-route) articulation takes precedence over the pin,
        // so one shared engine can serve many routed notes; cleared after.
        eng.trigger_articulation = Some("Closed Tip".to_string());
        assert!(eng.zone_selected(&closed, 49, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(&open, 49, 100, ZoneTrigger::Attack));
        eng.trigger_articulation = None;
        // Falls back to the pin once the transient clears.
        assert!(eng.zone_selected(&open, 49, 100, ZoneTrigger::Attack));

        // note_on_articulated must leave no residual state.
        eng.note_on_articulated(49, 100, Some("Closed Tip"));
        assert!(eng.trigger_articulation.is_none());
    }

    #[test]
    fn choke_group_setter_and_percussion_one_shot() {
        let mut eng = engine_from_styx(
            "name \"h\"\n\
             category \"drum-kit\"\n\
             zones (\n\
               {\n\
                 file \"c.wav\"\n\
                 key_min 42\n\
                 key_max 42\n\
                 root_key 42\n\
                 vel_min 0\n\
                 vel_max 127\n\
                 articulation \"Closed Tip\"\n\
               }\n\
             )\n",
        );
        // Percussion → voices spawn one-shot (Short) and ignore note-off.
        assert!(eng.percussion);
        // Mono choke (hats): group set, no choke_on → every hit chokes.
        assert!(eng.engine_choke_group.is_none());
        eng.set_choke_group(Some("hats"), &[]);
        assert_eq!(eng.engine_choke_group, Some(stable_group_hash("hats")));
        assert!(eng.should_engine_choke(), "mono: any hit chokes");
        eng.set_choke_group(Some(""), &[]); // empty clears
        assert!(eng.engine_choke_group.is_none());
        assert!(!eng.should_engine_choke());

        // Selective choke (cymbals): only the "Choke" articulation chokes;
        // crashes ring and overlap.
        eng.set_choke_group(Some("ride"), &["Choke".to_string()]);
        eng.trigger_articulation = Some("Crash".to_string());
        assert!(!eng.should_engine_choke(), "crash must not choke the ring");
        eng.trigger_articulation = Some("choke".to_string()); // case-insensitive
        assert!(
            eng.should_engine_choke(),
            "choke articulation stops the ring"
        );
    }

    #[test]
    fn pitched_sampler_still_gates_on_key() {
        // No drum category → pitched: key range gates, no collapse.
        let eng = engine_from_styx(
            "name \"p\"\n\
             instrument \"piano\"\n\
             zones (\n\
               {\n\
                 file \"p.wav\"\n\
                 key_min 60\n\
                 key_max 72\n\
                 root_key 60\n\
                 vel_min 0\n\
                 vel_max 127\n\
               }\n\
             )\n",
        );
        assert!(!eng.percussion);
        let z = &eng.patch().spec.zones[0];
        assert!(eng.zone_selected(z, 65, 100, ZoneTrigger::Attack));
        assert!(!eng.zone_selected(z, 40, 100, ZoneTrigger::Attack));
    }
}
