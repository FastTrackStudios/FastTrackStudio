//! Key System
//!
//! Musical keys, scales, and key-relative operations

pub mod key;
pub mod scale;

// Backward compatibility module
pub mod keys {
    pub use super::key::Key;
    pub use super::scale::{ScaleType, ScaleMode};
}

pub use key::Key;
pub use scale::{
    ScaleType, ScaleMode,
    ScaleFamily,
    DiatonicMode, DiatonicFamily,
    HarmonicMinorMode, HarmonicMinorFamily,
    MelodicMinorMode, MelodicMinorFamily,
};

