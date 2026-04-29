//! Toggle group — shadcn v4 maia style.

use dioxus::prelude::*;
use dioxus_primitives::toggle_group::{
    ToggleGroup as PrimitiveToggleGroup, ToggleItem as PrimitiveToggleItem,
};
use std::collections::HashSet;

#[derive(Props, Clone, PartialEq)]
pub struct ToggleGroupProps {
    #[props(default)]
    pub pressed: Option<HashSet<usize>>,
    #[props(default)]
    pub default_pressed: HashSet<usize>,
    #[props(default)]
    pub on_pressed_change: Option<Callback<HashSet<usize>>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = false)]
    pub allow_multiple_pressed: bool,
    #[props(default = true)]
    pub horizontal: bool,
    #[props(default = true)]
    pub roving_loop: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-toggle-group
#[component]
pub fn ToggleGroup(props: ToggleGroupProps) -> Element {
    rsx! {
        PrimitiveToggleGroup {
            pressed: props.pressed,
            default_pressed: props.default_pressed,
            disabled: props.disabled,
            allow_multiple_pressed: props.allow_multiple_pressed,
            horizontal: props.horizontal,
            roving_loop: props.roving_loop,
            on_pressed_change: move |pressed| {
                if let Some(callback) = &props.on_pressed_change {
                    callback.call(pressed);
                }
            },
            class: crate::cn::merge_slice(&["inline-flex items-center gap-1", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ToggleGroupItemProps {
    pub index: usize,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-toggle-group-item
#[component]
pub fn ToggleGroupItem(props: ToggleGroupItemProps) -> Element {
    rsx! {
        PrimitiveToggleItem {
            index: props.index,
            disabled: props.disabled,
            class: props.class,
            {props.children}
        }
    }
}
