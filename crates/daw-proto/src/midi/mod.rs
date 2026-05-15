//! MIDI editing — notes, CCs, pitch bends, program changes, SysEx.

mod cc;
mod error;
mod event;
mod note;
mod service;

pub use cc::*;
pub use error::*;
pub use event::*;
pub use note::*;
pub use service::{
    HumanizeParams, Midi, MidiCCCreate, MidiPitchBendCreate, MidiRpc, MidiTakeLocation, PpqRange,
    QuantizeParams,
};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, MidiClient, descriptor, layer, serve};
