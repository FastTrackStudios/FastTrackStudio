//! FTS Gain - Core module for the gain/utility plugin.
//!
//! This crate provides:
//! - Parameter definitions (always available)
//! - State types for hub communication (always available)
//! - DSP processing (feature = "dsp")
//! - UI components (feature = "ui")
//!
//! # Features
//!
//! - `dsp` - Include DSP processing code
//! - `ui` - Include UI components (requires dioxus)
//! - `full` - Both DSP and UI
//!
//! # Usage
//!
//! ```rust,ignore
//! // In plugin wrapper (needs DSP + UI)
//! use fts_gain_core::{GainParams, GainProcessor, GainStrip};
//!
//! // In control app (needs UI only)
//! use fts_gain_core::ui::GainStrip;
//!
//! // For state synchronization (always available)
//! use fts_gain_core::state::GainState;
//! ```

pub mod params;
pub mod state;

#[cfg(feature = "dsp")]
pub mod dsp;

#[cfg(feature = "ui")]
pub mod ui;

// Re-exports for convenience
pub use params::*;
pub use state::*;

#[cfg(feature = "dsp")]
pub use dsp::GainProcessor;

#[cfg(feature = "ui")]
pub use ui::GainStrip;
