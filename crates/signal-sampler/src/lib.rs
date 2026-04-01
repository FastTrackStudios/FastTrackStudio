//! Sample library playback engine for Signal.
//!
//! `SamplerBank` holds any number of named [`SampleEngine`] instances — one per
//! instrument or section (e.g. `"strings_1v"`, `"brass_horns"`, `"drums"`).
//! All engines are mixed into a single stereo output buffer each render block.
//!
//! `SamplerPlayer` owns the cpal output stream and drives a `SamplerBank`.
//!
//! # Quick start
//!
//! ```rust,no_run
//! use signal_sampler::{SamplerPlayer, InstrumentId};
//! use std::path::Path;
//!
//! let mut player = SamplerPlayer::new()?;
//!
//! player.load_instrument(
//!     "strings_1v",
//!     Path::new("specs/cinematic-strings.toml"),
//!     Some(Path::new("/path/to/cinematic-strings/wavs")),
//!     "1v",   // section
//!     "Mix",  // mic
//! )?;
//!
//! // Route MIDI channel 1 to the strings instrument
//! player.set_midi_channel("strings_1v", 1);
//!
//! // Or drive it directly
//! player.note_on("strings_1v", 60, 100);
//! player.cc("strings_1v", 1, 80);  // CC1 = dynamics
//! # Ok::<(), eyre::Error>(())
//! ```

pub mod bank;
pub mod player;

pub use bank::SamplerBank;
pub use player::SamplerPlayer;

/// Identifier for a loaded instrument within the bank.
pub type InstrumentId = String;
