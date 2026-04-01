//! Sample library playback engine for Signal.
//!
//! Loads and plays any sample library described by a `.styx` spec file:
//! orchestral strings, brass, winds, drums, piano — anything.
//!
//! # Architecture
//!
//! ```text
//! LibrarySpec  (loaded from a .styx spec file via facet-styx)
//!   + SampleMap  (scanned from extracted WAV root directory)
//!   = PlayerPatch  (combined playback context)
//!       → SampleEngine  (MIDI-driven voice engine, one per section/instrument)
//!           → SamplerBank  (N engines, MIDI channel routing, stereo mix)
//!               → SamplerPlayer  (owns cpal output stream)
//! ```
//!
//! # Library specs
//!
//! A library spec is a `.styx` file that describes:
//! - Instrument sections (1v, 2v, Va, Ce, Ba for strings; etc.)
//! - Articulations (Vibsus, Staccato, Leg, etc.)
//! - Dynamics (CC1 layers, velocity ranges)
//! - Legato engine (pre-delay zones, portamento threshold)
//! - Keyswitch/CC58 mapping
//!
//! Third-party libraries can be added by writing a new `.styx` spec file —
//! no code changes required.
//!
//! # Quick start
//!
//! ```rust,no_run
//! use signal_sampler::SamplerPlayer;
//! use std::path::Path;
//!
//! let player = SamplerPlayer::new()?;
//! player.load_instrument(
//!     "strings_1v",
//!     Path::new("specs/cinematic-strings.styx"),
//!     Some(Path::new("/path/to/wavs")),
//!     "1v", "Mix",
//! )?;
//! player.note_on("strings_1v", 60, 100);
//! player.cc("strings_1v", 1, 80);
//! # Ok::<(), eyre::Error>(())
//! ```

pub mod bank;
pub mod engine;
pub mod midi;
pub mod player;
pub mod sample_map;
pub mod spec;

pub use bank::SamplerBank;
pub use engine::SampleEngine;
pub use player::SamplerPlayer;
pub use spec::LibrarySpec;
pub use sample_map::{SampleKey, SampleMap};

use std::path::Path;

/// Identifier for a loaded instrument within the bank.
pub type InstrumentId = String;

// ── Error ─────────────────────────────────────────────────────────────────────

#[derive(Debug, thiserror::Error)]
pub enum SamplerError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("spec parse error: {0}")]
    SpecParse(String),

    #[error("invalid MIDI note name: {0:?}")]
    BadNoteName(String),

    #[error("spec missing section {0:?}")]
    MissingSection(String),

    #[error("spec missing articulation {0:?}")]
    MissingArticulation(String),
}

// ── PlayerPatch ───────────────────────────────────────────────────────────────

/// A fully loaded library patch: spec + sample index.
pub struct PlayerPatch {
    pub spec: LibrarySpec,
    pub map: SampleMap,
}

impl PlayerPatch {
    /// Load a spec and scan WAV files under `samples_root`.
    pub fn load(spec_path: &Path, samples_root: &Path) -> Result<Self, SamplerError> {
        let spec = LibrarySpec::from_file(spec_path)?;
        let map = SampleMap::scan(samples_root)?;
        Ok(Self { spec, map })
    }

    /// Build a patch from an already-parsed spec with an empty sample map.
    pub fn from_spec(spec: LibrarySpec) -> Self {
        Self { spec, map: SampleMap::empty() }
    }

    pub fn total_samples(&self) -> usize {
        self.map.total()
    }

    pub fn resolve(
        &self,
        section_id: &str,
        articulation_id: &str,
        mic_id: &str,
        dynamic: &str,
        target_note: u8,
        direction: &str,
        rr: usize,
    ) -> Option<(std::path::PathBuf, u8)> {
        self.map.resolve(&self.spec, section_id, articulation_id, mic_id, dynamic, target_note, direction, rr)
    }

    pub fn legato_delay_expressive(&self, velocity: u8) -> Option<u32> {
        self.spec.legato_engine.as_ref()?.expressive.as_ref()?.delay_for_velocity(velocity)
    }

    pub fn legato_delay_low_latency(&self, velocity: u8) -> Option<u32> {
        self.spec.legato_engine.as_ref()?.low_latency.as_ref()?.delay_for_velocity(velocity)
    }

    pub fn short_note_pre_delay_ms(&self) -> u32 {
        self.spec.short_note_timing.as_ref().map(|t| t.pre_delay_ms).unwrap_or(0)
    }
}
