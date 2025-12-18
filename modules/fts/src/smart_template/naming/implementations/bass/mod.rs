//! Bass naming implementation
//!
//! Complete implementation of the Bass group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Bass-specific information.
//!
//! The Bass group includes:
//! - **Bass Guitar**: Multi-mic descriptors (DI, Amp)
//! - **Synth Bass**: No multi-mic descriptors

pub mod bass;

pub use bass::*;
