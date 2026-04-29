//! Toggle — shadcn v4 maia style.

use dioxus::prelude::*;

/// Variant for the toggle button.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum ToggleVariant {
    #[default]
    Default,
    Outline,
}

/// Size for the toggle button.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum ToggleSize {
    Small,
    #[default]
    Medium,
    Large,
}

impl ToggleSize {
    fn classes(self) -> &'static str {
        match self {
            Self::Small => "h-8 min-w-8 px-3",
            Self::Medium => "h-9 min-w-9 px-3",
            Self::Large => "h-10 min-w-10 px-4",
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ToggleProps {
    pub pressed: bool,
    pub on_toggle: Callback<bool>,
    #[props(default)]
    pub variant: ToggleVariant,
    #[props(default)]
    pub size: ToggleSize,
    #[props(default)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-toggle
#[component]
pub fn Toggle(props: ToggleProps) -> Element {
    let base = "inline-flex items-center justify-center font-medium transition-colors gap-1 rounded-full text-sm [&_svg:not([class*='size-'])]:size-4 hover:text-foreground";

    let pressed_class = if props.pressed { "bg-muted" } else { "" };

    let variant_class = match props.variant {
        ToggleVariant::Default => "bg-transparent",
        ToggleVariant::Outline => "border-input hover:bg-muted border bg-transparent",
    };

    let size_class = props.size.classes();

    let disabled_class = if props.disabled {
        "pointer-events-none opacity-50"
    } else {
        ""
    };

    let pressed = props.pressed;

    rsx! {
        button {
            r#type: "button",
            class: format!(
                "{base} {variant_class} {size_class} {pressed_class} {disabled_class} {}",
                props.class
            ),
            aria_pressed: pressed.to_string(),
            "data-state": if pressed { "on" } else { "off" },
            disabled: props.disabled,
            onclick: move |_| {
                props.on_toggle.call(!pressed);
            },
            {props.children}
        }
    }
}
