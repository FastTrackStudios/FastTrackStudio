//! Group definitions for the dynamic template system

pub mod drums;

// Re-export the top-level Drums container
// Individual kits and drum types should be accessed through their respective modules
// e.g., drums::drum_kit::Kick, drums::electronic_kit::Kick
pub use drums::Drums;
