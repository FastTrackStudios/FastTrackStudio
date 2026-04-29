//! Select — primitive-backed single-value select dropdown.

use dioxus::prelude::*;
use dioxus_primitives::select::{
    Select as PrimitiveSelect, SelectGroupLabel as PrimitiveSelectGroupLabel, SelectItemIndicator,
    SelectList as PrimitiveSelectList, SelectOption as PrimitiveSelectOption,
    SelectTrigger as PrimitiveSelectTrigger, SelectValue as PrimitiveSelectValue,
};

#[derive(Props, Clone, PartialEq)]
pub struct SelectProps {
    /// The currently selected value.
    pub value: Signal<String>,
    #[props(default)]
    pub on_change: Option<Callback<String>>,
    #[props(default = "Select...".to_string())]
    pub placeholder: String,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = true)]
    pub roving_loop: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn Select(props: SelectProps) -> Element {
    let mut value = props.value;
    let selected = if value().is_empty() {
        None
    } else {
        Some(value())
    };

    rsx! {
        PrimitiveSelect::<String> {
            value: Some(selected),
            placeholder: props.placeholder,
            disabled: props.disabled,
            roving_loop: props.roving_loop,
            on_value_change: move |next: Option<String>| {
                let next = next.unwrap_or_default();
                value.set(next.clone());
                if let Some(callback) = &props.on_change {
                    callback.call(next);
                }
            },
            class: crate::cn::merge_slice(&["relative inline-block w-full", props.class.as_str()]),
            SelectTrigger {}
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SelectTriggerProps {
    #[props(default)]
    pub class: String,
    #[props(default)]
    pub children: Element,
}

#[component]
pub fn SelectTrigger(props: SelectTriggerProps) -> Element {
    rsx! {
        PrimitiveSelectTrigger {
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-between gap-2 h-9 w-full px-3 text-sm rounded-lg border border-input bg-transparent shadow-xs hover:bg-accent hover:text-accent-foreground transition-colors cursor-pointer select-none disabled:cursor-not-allowed disabled:opacity-50 {}",
                props.class
            )),
            if props.children.is_ok() {
                {props.children}
            } else {
                PrimitiveSelectValue {
                    class: "truncate data-[placeholder=true]:text-muted-foreground",
                }
                svg {
                    class: "size-4 text-muted-foreground shrink-0 opacity-50",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "m7 15 5 5 5-5" }
                    path { d: "m7 9 5-5 5 5" }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SelectContentProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn SelectContent(props: SelectContentProps) -> Element {
    rsx! {
        PrimitiveSelectList {
            id: props.id,
            class: crate::cn::merge(format!(
                "absolute z-50 mt-1 min-w-48 w-full rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-1 overflow-hidden {}",
                props.class
            )),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SelectItemProps {
    pub value: String,
    pub index: usize,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub text_value: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn SelectItem(props: SelectItemProps) -> Element {
    rsx! {
        PrimitiveSelectOption::<String> {
            value: props.value,
            index: props.index,
            disabled: props.disabled,
            text_value: props.text_value,
            class: crate::cn::merge(format!(
                "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors {}",
                props.class
            )),
            span { class: "flex-1", {props.children} }
            SelectItemIndicator {
                svg {
                    class: "size-4 text-current shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
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

#[component]
pub fn SelectSeparator() -> Element {
    rsx! {
        div { class: "bg-border/50 -mx-1 my-1 h-px" }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SelectLabelProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn SelectLabel(props: SelectLabelProps) -> Element {
    rsx! {
        PrimitiveSelectGroupLabel {
            id: props.id,
            class: crate::cn::merge_slice(&["text-muted-foreground px-3 py-2.5 text-xs", props.class.as_str()]),
            {props.children}
        }
    }
}
