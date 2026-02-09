//! Dioxus bridge for the `input` crate.

pub mod convert;
pub mod hook;

pub use convert::{
    convert_keyboard_event, convert_key, convert_modifiers, convert_mouse_event,
    convert_wheel_event,
};
pub use hook::{use_input_processor, InputHandle, ACTION_CONTEXT};
