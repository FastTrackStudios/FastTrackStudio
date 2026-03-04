//! Button — wraps lumen-blocks Button with FTS sizing defaults.

use dioxus::prelude::*;
pub use lumen_blocks::components::button::{ButtonSize, ButtonVariant};

/// FTS-styled button.
///
/// Thin wrapper around lumen-blocks `Button` that applies FTS defaults
/// (medium size, primary variant) so callers don't need to specify them
/// every time.
#[derive(Props, Clone, PartialEq)]
pub struct ButtonProps {
    /// Visual variant.
    #[props(default = ButtonVariant::Primary)]
    pub variant: ButtonVariant,

    /// Size.
    #[props(default = ButtonSize::Medium)]
    pub size: ButtonSize,

    /// Click handler.
    #[props(default)]
    pub on_click: Option<Callback<MouseEvent>>,

    /// Disabled state.
    #[props(default = false)]
    pub disabled: bool,

    /// Loading state — disables and shows spinner.
    #[props(default = false)]
    pub loading: bool,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

#[component]
pub fn Button(props: ButtonProps) -> Element {
    rsx! {
        lumen_blocks::components::button::Button {
            variant: props.variant,
            size: props.size,
            on_click: props.on_click,
            disabled: props.disabled,
            loading: props.loading,
            class: props.class,
            {props.children}
        }
    }
}
