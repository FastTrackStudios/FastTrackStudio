//! Checkbox — shadcn v4 maia style, standalone (no lumen-blocks).

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct CheckboxProps {
    pub checked: Signal<bool>,
    #[props(default)]
    pub on_change: Option<Callback<bool>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-checkbox
#[component]
pub fn Checkbox(props: CheckboxProps) -> Element {
    let checked = (props.checked)();
    let state = if checked { "checked" } else { "unchecked" };

    // shadcn v4 maia: cn-checkbox
    let base = "border-input dark:bg-input/30 data-[state=checked]:bg-primary data-[state=checked]:text-primary-foreground dark:data-[state=checked]:bg-primary data-[state=checked]:border-primary focus-visible:border-ring focus-visible:ring-ring/50 flex size-4 items-center justify-center rounded-[6px] border transition-shadow focus-visible:ring-[3px]";

    let disabled_cls = if props.disabled {
        "opacity-50 cursor-not-allowed"
    } else {
        "cursor-pointer"
    };

    rsx! {
        button {
            r#type: "button",
            role: "checkbox",
            "aria-checked": "{checked}",
            "data-state": "{state}",
            class: crate::cn::merge_slice(&[base, disabled_cls, props.class.as_str()]),
            disabled: if props.disabled { Some(true) } else { None },
            onclick: move |_| {
                if !props.disabled {
                    let next = !checked;
                    let mut sig = props.checked;
                    sig.set(next);
                    if let Some(cb) = &props.on_change {
                        cb.call(next);
                    }
                }
            },
            if checked {
                svg {
                    class: "size-3.5",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "M20 6 9 17l-5-5" }
                }
            }
        }
    }
}
