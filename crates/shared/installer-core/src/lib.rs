//! FastTrackStudio installer core — pure installation logic with no UI dependency.
//!
//! Handles downloading REAPER, extracting DMGs, copying extensions and presets,
//! writing portable `reaper.ini`, and setting up shell PATH.

pub mod plan;
pub mod progress;
pub mod runner;
pub mod steps;

pub use plan::InstallPlan;
pub use progress::{EventSender, InstallEvent, InstallStep};
pub use runner::{run_all_steps, InstallContext};
