//! Textarea — shadcn v4 maia style.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct TextareaProps {
    /// Current value (two-way binding).
    pub value: Signal<String>,

    #[props(default)]
    pub placeholder: String,

    #[props(default = false)]
    pub disabled: bool,

    #[props(default = false)]
    pub readonly: bool,

    #[props(default)]
    pub rows: Option<u32>,

    #[props(default)]
    pub on_change: Option<Callback<FormEvent>>,

    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-textarea
#[component]
pub fn Textarea(props: TextareaProps) -> Element {
    let base = "w-full bg-input/30 border-input focus-visible:border-ring focus-visible:ring-ring/50 resize-none rounded-xl border px-3 py-3 text-sm transition-colors focus-visible:outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 placeholder:text-muted-foreground";

    let rows = props.rows.unwrap_or(3);
    let mut value = props.value;

    rsx! {
        textarea {
            class: crate::cn::merge_slice(&[base, props.class.as_str()]),
            value: "{value}",
            placeholder: "{props.placeholder}",
            disabled: if props.disabled { Some(true) } else { None },
            readonly: if props.readonly { Some(true) } else { None },
            rows: "{rows}",
            oninput: move |e: FormEvent| {
                value.set(e.value());
                if let Some(cb) = &props.on_change {
                    cb.call(e);
                }
            },
        }
    }
}
