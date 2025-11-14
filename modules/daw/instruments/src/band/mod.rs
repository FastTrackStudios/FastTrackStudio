//! Band Instruments
//!
//! Modern band instruments including guitars, keyboards, and percussion

pub mod drums;
pub mod guitars;
pub mod keyboards;

pub use drums::PercussionInstrument;
pub use guitars::GuitarInstrument;
pub use keyboards::KeyboardInstrument;
