//! Radio group — shadcn v4 maia style radio buttons.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Context shared between RadioGroup and RadioGroupItem
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct RadioGroupContext {
    value: Signal<String>,
    on_change: Option<Callback<String>>,
}

// ---------------------------------------------------------------------------
// RadioGroup
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct RadioGroupProps {
    /// The currently selected value (two-way bound).
    pub value: Signal<String>,

    /// Called when the user picks a different option.
    #[props(default)]
    pub on_change: Option<Callback<String>>,

    /// Extra CSS classes on the outer container.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: radio-group
#[component]
pub fn RadioGroup(props: RadioGroupProps) -> Element {
    use_context_provider(|| RadioGroupContext {
        value: props.value,
        on_change: props.on_change,
    });

    rsx! {
        div {
            role: "radiogroup",
            class: format!("grid gap-3 {}", props.class),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// RadioGroupItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct RadioGroupItemProps {
    /// The value this radio item represents.
    pub value: String,

    /// Whether this item is disabled.
    #[props(default = false)]
    pub disabled: bool,

    /// Extra CSS classes on the button.
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: radio-group-item
#[component]
pub fn RadioGroupItem(props: RadioGroupItemProps) -> Element {
    let mut ctx: RadioGroupContext = use_context();
    let is_checked = *ctx.value.read() == props.value;
    let item_value = props.value.clone();

    let outer_class = if is_checked {
        "border-primary bg-primary"
    } else {
        "border-input dark:bg-input/30"
    };

    rsx! {
        button {
            r#type: "button",
            role: "radio",
            disabled: if props.disabled { Some(true) } else { None },
            aria_checked: is_checked.to_string(),
            "data-state": if is_checked { "checked" } else { "unchecked" },
            class: format!(
                "relative flex size-4 items-center justify-center rounded-full border focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] cursor-pointer disabled:cursor-not-allowed disabled:opacity-50 {outer_class} {}",
                props.class
            ),
            onclick: move |_| {
                if !props.disabled {
                    let val = item_value.clone();
                    ctx.value.set(val.clone());
                    if let Some(cb) = &ctx.on_change {
                        cb.call(val);
                    }
                }
            },
            // Inner dot when checked
            if is_checked {
                span {
                    class: "absolute bg-primary-foreground size-2 rounded-full",
                }
            }
        }
    }
}
