//! Auto-sampler — play an instrument across a note/velocity grid, record what
//! comes back, and emit a zone-mapped `.signalpack`.
//!
//! Built for sampling **external hardware**: MIDI goes out of a port to the
//! instrument, its audio returns on a pair of interface inputs. Because one
//! process owns both ends, note-on and record-arm need no cross-process sync —
//! which is why this is a plain crate rather than the generator/recorder plugin
//! pair a plugin host would force.
//!
//! ```text
//!   grid ── note/velocity cells ──▶ MIDI out ──▶ instrument
//!                                                   │ audio
//!   pack ◀── library.styx ◀── WAV ◀── capture ◀──────┘
//! ```
//!
//! The resulting pack is always **zone-mode**: the sampler chose every note and
//! velocity, so each sample's mapping is stated outright rather than parsed back
//! out of a filename.

pub mod batch;
pub mod capture;
pub mod cli;
pub mod compare;
pub mod config;
pub mod decent;
pub mod gig;
pub mod grid;
pub mod latency;
pub mod loopfind;
pub mod loops;
pub mod midi;
pub mod pack;
pub mod play;
pub mod progress;
pub mod probe;
pub mod reloop;
pub mod session;
pub mod wav;

pub use config::{AudioRoute, AutoSampleConfig, Grid, MidiRoute, Timing};
pub use session::{RunReport, run};
