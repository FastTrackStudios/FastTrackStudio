//! Professional audio parameter control widgets for Dioxus.
//!
//! This library provides a comprehensive set of UI widgets for controlling audio parameters
//! in VST/CLAP plugins and control surfaces. All widgets support:
//!
//! - SVG-based rendering (works across native, web, and desktop)
//! - Modulation range visualization
//! - Customizable theming with preset styles (Minimal, SSL, Vintage)
//! - Fine control with modifier keys
//! - Double-click to reset
//!
//! # Example
//!
//! ```ignore
//! use audio_controls::prelude::*;
//!
//! #[component]
//! fn MyUI() -> Element {
//!     let mut gain = use_signal(|| 0.5f32);
//!
//!     rsx! {
//!         Knob {
//!             value: gain,
//!             variant: KnobVariant::Arc,
//!             size: 64,
//!             label: Some("Gain".to_string()),
//!         }
//!     }
//! }
//! ```
//!
//! # nih-plug Integration
//!
//! Enable the `nih-plug` feature for direct parameter binding:
//!
//! ```ignore
//! use audio_controls::prelude::*;
//!
//! #[component]
//! fn PluginUI(params: Arc<MyParams>) -> Element {
//!     rsx! {
//!         ParamKnob {
//!             param_ptr: params.gain.as_ptr(),
//!             variant: KnobVariant::Arc,
//!         }
//!     }
//! }
//! ```

pub mod core;
#[cfg(feature = "nih-plug")]
pub mod integration;
pub mod theming;
pub mod widgets;

/// Prelude for convenient imports.
pub mod prelude {
    // Core types
    pub use crate::core::{
        accessibility::{AriaAttributes, FocusState, KeyAction, KeyboardSteps, Orientation},
        block::{
            definitions as block_definitions, BlockCategory, BlockDefinition, BlockInstance,
            BlockParameter, ParameterFormat,
        },
        gesture::{GestureState, ScrollSensitivity, TouchGesture, TouchRecognizer},
        layout::{FormFactor, FormFactorCategory, LayoutConstraints, LevelOfDetail},
        macro_curve::{presets as macro_presets, CurveType, MacroControl, ParameterMapping},
        modulation::ModulationRange,
        normal::{Normal, NormalParam},
        range::{FreqRange, LogDBRange, ParameterRange},
        sensitivity::{DragSensitivity, DragState, ModifierKeys},
        value::{ParseResult, ValueFormatter, ValueParser, ValueStepping},
    };

    // Theming
    pub use crate::theming::{
        presets,
        style::ControlState,
        use_theme, ControlConfig, ControlVariant, KnobStyle, SliderStyle, ThemeContext,
        ThemeProvider,
    };

    // Widgets
    pub use crate::widgets::{
        block_view::{BlockView, Pedalboard, Rack},
        hslider::{HSlider, SliderVariant},
        knob::{Knob, KnobVariant},
        vslider::VSlider,
        xy_pad::XYPad,
    };

    #[cfg(feature = "nih-plug")]
    pub use crate::integration::{ParamKnob, ParamSlider};
}
