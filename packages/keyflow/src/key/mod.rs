//! Key System
//!
//! Musical keys, scales, and key-relative operations

pub mod key;
pub mod scale;

// Backward compatibility module
pub mod keys {
    pub use super::key::Key;
    pub use super::scale::{ScaleMode, ScaleType};
}

pub use key::Key;
pub use scale::{
    DiatonicFamily, DiatonicMode, HarmonicMinorFamily, HarmonicMinorMode, MelodicMinorFamily,
    MelodicMinorMode, ScaleFamily, ScaleMode, ScaleType,
};
