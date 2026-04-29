//! Field — shadcn v4 maia style form field components.
//!
//! Provides `Field`, `FieldLabel`, `FieldDescription`, and `FieldMessage`
//! for consistent form layout and validation styling.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct FieldProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Wrapper div for a form field group.
#[component]
pub fn Field(props: FieldProps) -> Element {
    rsx! {
        div {
            class: format!("grid gap-2 {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct FieldLabelProps {
    #[props(default)]
    pub class: String,
    #[props(default = false)]
    pub required: bool,
    pub children: Element,
}

/// Label for a form field. Shows a red asterisk when `required` is true.
#[component]
pub fn FieldLabel(props: FieldLabelProps) -> Element {
    rsx! {
        label {
            class: format!("text-sm font-medium leading-none {}", props.class),
            {props.children}
            if props.required {
                span { class: "text-destructive ml-0.5", "*" }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct FieldDescriptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Help / hint text below a form field.
#[component]
pub fn FieldDescription(props: FieldDescriptionProps) -> Element {
    rsx! {
        p {
            class: format!("text-muted-foreground text-sm {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct FieldMessageProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Validation error message for a form field.
#[component]
pub fn FieldMessage(props: FieldMessageProps) -> Element {
    rsx! {
        p {
            class: format!("text-destructive text-sm {}", props.class),
            {props.children}
        }
    }
}
