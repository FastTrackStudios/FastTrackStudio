//! nih-plug parameter integration.
//!
//! Provides wrapper components that bind directly to nih-plug parameters,
//! automatically handling value reading, modulation visualization, and
//! parameter change notifications.

mod hooks;
mod param_knob;
mod param_slider;

pub use hooks::{use_param_modulation, use_param_value};
pub use param_knob::ParamKnob;
pub use param_slider::ParamSlider;
