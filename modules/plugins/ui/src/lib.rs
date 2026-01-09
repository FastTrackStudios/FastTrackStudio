//! Shared UI components for FTS audio plugins.
//!
//! This crate provides reusable Dioxus components for building plugin UIs:
//! - Peak meters
//! - Gain reduction meters
//! - Rotary knobs
//! - Sliders/faders
//! - Toggle switches
//! - Value displays
//!
//! These components are designed to be used both in plugin editors and
//! in the FTS Control desktop app.

mod knob;
mod label;
mod meter;
mod slider;
mod theme;
mod toggle;

pub use knob::*;
pub use label::*;
pub use meter::*;
pub use slider::*;
pub use theme::*;
pub use toggle::*;

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::knob::Knob;
    pub use crate::label::{DbLabel, ValueLabel};
    pub use crate::meter::{GainReductionMeter, PeakMeter, StereoMeter};
    pub use crate::slider::{Fader, Slider};
    pub use crate::theme::{DARK_THEME, Theme};
    pub use crate::toggle::Toggle;
}
