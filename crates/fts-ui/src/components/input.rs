//! Input — shadcn v4 maia style, replaces lumen-blocks wrapper.

use dioxus::prelude::*;

/// Input size variants.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum InputSize {
    Small,
    #[default]
    Medium,
    Large,
}

/// Input visual variant.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum InputVariant {
    #[default]
    Default,
    Error,
}

#[derive(Props, Clone, PartialEq)]
pub struct InputProps {
    /// Current value (two-way binding).
    pub value: Signal<String>,

    #[props(default)]
    pub size: InputSize,

    #[props(default)]
    pub variant: InputVariant,

    #[props(default)]
    pub placeholder: String,

    #[props(default = false)]
    pub disabled: bool,

    #[props(default = false)]
    pub readonly: bool,

    #[props(default)]
    pub on_change: Option<Callback<FormEvent>>,

    #[props(default)]
    pub class: String,
}

#[component]
pub fn Input(props: InputProps) -> Element {
    // shadcn v4 maia: cn-input
    let base = "w-full bg-input/30 border-input focus-visible:border-ring focus-visible:ring-ring/50 border px-3 py-1 text-sm transition-colors focus-visible:outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 placeholder:text-muted-foreground";

    let size_class = match props.size {
        InputSize::Small => "h-8 rounded-lg text-xs",
        InputSize::Medium => "h-9 rounded-lg",
        InputSize::Large => "h-10 rounded-lg",
    };

    let variant_class = match props.variant {
        InputVariant::Default => "",
        InputVariant::Error => {
            "aria-invalid:ring-destructive/20 aria-invalid:border-destructive border-destructive"
        }
    };

    let mut value = props.value;

    rsx! {
        input {
            class: crate::cn::merge_slice(&[base, size_class, variant_class, props.class.as_str()]),
            value: "{value}",
            placeholder: "{props.placeholder}",
            disabled: if props.disabled { Some(true) } else { None },
            readonly: if props.readonly { Some(true) } else { None },
            oninput: move |e: FormEvent| {
                value.set(e.value());
                if let Some(cb) = &props.on_change {
                    cb.call(e);
                }
            },
        }
    }
}
