//! Signal audio processing — live EQ + Compressor chain with cpal I/O.

pub mod chain;
pub mod engine;

pub use chain::ProcessingChain;
pub use engine::LiveAudioEngine;
