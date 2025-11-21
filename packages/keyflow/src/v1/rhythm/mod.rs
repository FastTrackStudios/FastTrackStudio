pub mod rhythm;
pub mod chord_rhythm;
mod r#trait;

// Re-export the trait and parser trait
pub use r#trait::{Rhythm, RhythmParser};