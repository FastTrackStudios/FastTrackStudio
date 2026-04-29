//! Toggle group — shadcn v4 maia style.

use dioxus::prelude::*;

use super::toggle::{Toggle, ToggleSize, ToggleVariant};

#[derive(Clone, Copy)]
struct ToggleGroupContext {
    value: Signal<String>,
    on_change: Callback<String>,
}

#[derive(Props, Clone, PartialEq)]
pub struct ToggleGroupProps {
    pub value: Signal<String>,
    pub on_change: Callback<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-toggle-group
#[component]
pub fn ToggleGroup(props: ToggleGroupProps) -> Element {
    use_context_provider(|| ToggleGroupContext {
        value: props.value,
        on_change: props.on_change,
    });

    rsx! {
        div {
            class: format!("inline-flex items-center gap-1 {}", props.class),
            role: "group",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ToggleGroupItemProps {
    pub value: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-toggle-group-item
#[component]
pub fn ToggleGroupItem(props: ToggleGroupItemProps) -> Element {
    let ctx: ToggleGroupContext = use_context();
    let is_pressed = *ctx.value.read() == props.value;
    let item_value = props.value.clone();

    rsx! {
        Toggle {
            pressed: is_pressed,
            on_toggle: move |_new_state: bool| {
                let val = item_value.clone();
                ctx.on_change.call(val);
            },
            variant: ToggleVariant::Default,
            size: ToggleSize::Medium,
            class: props.class,
            {props.children}
        }
    }
}
