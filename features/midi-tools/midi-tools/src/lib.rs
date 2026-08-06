//! MIDI note manipulation, as arithmetic.
//!
//! The home for tools that take the notes in a take and give back
//! different notes. Nothing here touches a DAW, a window, or a MIDI port
//! — you hand it notes and it hands you edits, which makes every tool a
//! plain unit test rather than something you have to click on in REAPER
//! to find out whether it works.
//!
//! The two halves that *do* touch the world live next door:
//!
//! - `midi-tools-daw` — reads and writes notes through
//!   [`daw::service::Midi`], so the same tool works against REAPER,
//!   daw-standalone, or a test backend.
//! - `midi-tools-ui` — the Dioxus panel, which runs in a desktop window
//!   for iteration and as a Blitz panel inside REAPER.
//!
//! ## Why not midicore?
//!
//! `midicore` is realtime MIDI *I/O* — ports, streams, `MidiEvent` on the
//! wire. This is offline editing of notes that already exist in a
//! project. They share the word "MIDI" and nothing else: no type flows
//! between them, and folding one into the other would put a port
//! enumerator next to a Bézier curve.
//!
//! ## What's here
//!
//! - [`velocity`] — the four velocity engines ported from mrtnz's
//!   MVelocity: a drawn curve, a cyclic accent pattern, held
//!   randomization, and compress/expand. [`velocity::Session`] composes
//!   them over a held baseline.
//!
//! More of the same family belongs beside it as sibling modules rather
//! than as new crates — an arpeggiator, a note splitter, a chopper are
//! all "notes in, notes out".

pub mod shape;
pub mod sink;
pub mod arp;
pub mod velocity;

pub use sink::{ArpSink, DemoArpSink, DemoSink, VelocitySink};
pub use velocity::{Note, Range, Session, VelocityEdit};
