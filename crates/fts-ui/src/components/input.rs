//! Input — wraps lumen-blocks Input with FTS defaults.

use dioxus::prelude::*;
pub use lumen_blocks::components::input::{InputSize, InputVariant};

/// FTS-styled text input.
#[derive(Props, Clone, PartialEq)]
pub struct InputProps {
    /// Current value (two-way binding).
    pub value: Signal<String>,

    /// Size.
    #[props(default = InputSize::Medium)]
    pub size: InputSize,

    /// Variant.
    #[props(default = InputVariant::Default)]
    pub variant: InputVariant,

    /// Placeholder text.
    #[props(default)]
    pub placeholder: String,

    /// Disabled state.
    #[props(default = false)]
    pub disabled: bool,

    /// Read-only.
    #[props(default = false)]
    pub readonly: bool,

    /// Change handler (receives the full FormEvent from the underlying input).
    #[props(default)]
    pub on_change: Option<Callback<FormEvent>>,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,
}

#[component]
pub fn Input(props: InputProps) -> Element {
    rsx! {
        lumen_blocks::components::input::Input {
            value: props.value,
            size: props.size,
            variant: props.variant,
            placeholder: props.placeholder,
            disabled: props.disabled,
            readonly: props.readonly,
            on_change: props.on_change,
            class: props.class,
        }
    }
}
