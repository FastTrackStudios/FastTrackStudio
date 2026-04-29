//! Switch — shadcn v4 maia style, standalone (no lumen-blocks).

use dioxus::prelude::*;
use dioxus_primitives::switch::{Switch as PrimitiveSwitch, SwitchThumb};

#[derive(Props, Clone, PartialEq)]
pub struct SwitchProps {
    pub checked: Signal<bool>,
    #[props(default)]
    pub on_change: Option<Callback<bool>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-switch
#[component]
pub fn Switch(props: SwitchProps) -> Element {
    let checked = (props.checked)();
    // shadcn v4 maia: cn-switch track
    let track_base = "inline-flex h-5 w-9 shrink-0 items-center rounded-full border-2 border-transparent transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50";

    let track_color = if checked { "bg-primary" } else { "bg-input" };

    let disabled_cls = if props.disabled {
        "cursor-not-allowed opacity-50"
    } else {
        "cursor-pointer"
    };

    // shadcn v4 maia: cn-switch thumb
    let thumb_base = "pointer-events-none block size-4 rounded-full bg-background shadow-lg ring-0 transition-transform";
    let thumb_translate = if checked {
        "translate-x-4"
    } else {
        "translate-x-0"
    };

    let class =
        crate::cn::merge_slice(&[track_base, track_color, disabled_cls, props.class.as_str()]);

    rsx! {
        PrimitiveSwitch {
            checked: Some(checked),
            disabled: props.disabled,
            class,
            on_checked_change: move |next| {
                if next != checked {
                    let mut checked_sig = props.checked;
                    checked_sig.set(next);
                    if let Some(cb) = &props.on_change {
                        cb.call(next);
                    }
                }
            },
            SwitchThumb {
                class: "{thumb_base} {thumb_translate}",
            }
        }
    }
}
