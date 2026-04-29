//! Button — shadcn v4 maia style, replaces lumen-blocks wrapper.

use dioxus::prelude::*;

/// Button visual variant.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum ButtonVariant {
    #[default]
    Primary,
    Secondary,
    Outline,
    Ghost,
    Link,
    Destructive,
}

/// Button size.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum ButtonSize {
    Small,
    #[default]
    Medium,
    Large,
}

#[derive(Props, Clone, PartialEq)]
pub struct ButtonProps {
    #[props(default)]
    pub variant: ButtonVariant,
    #[props(default)]
    pub size: ButtonSize,
    #[props(default)]
    pub on_click: Option<Callback<MouseEvent>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = false)]
    pub loading: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn Button(props: ButtonProps) -> Element {
    // shadcn v4 maia: cn-button base
    let base = "inline-flex items-center justify-center font-medium border border-transparent bg-clip-padding text-sm transition-colors focus-visible:outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] disabled:pointer-events-none disabled:opacity-50 [&_svg:not([class*='size-'])]:size-4";

    // shadcn v4 maia: cn-button-variant-*
    let variant = match props.variant {
        ButtonVariant::Primary => "bg-primary text-primary-foreground hover:bg-primary/80",
        ButtonVariant::Secondary => "bg-secondary text-secondary-foreground hover:bg-secondary/80",
        ButtonVariant::Outline => "border-border bg-input/30 hover:bg-input/50 hover:text-foreground",
        ButtonVariant::Ghost => "hover:bg-muted hover:text-foreground dark:hover:bg-muted/50",
        ButtonVariant::Destructive => "bg-destructive/10 hover:bg-destructive/20 text-destructive dark:bg-destructive/20 dark:hover:bg-destructive/30 focus-visible:ring-destructive/20 focus-visible:border-destructive/40",
        ButtonVariant::Link => "text-primary underline-offset-4 hover:underline border-none",
    };

    // shadcn v4 maia: cn-button-size-*
    let size = match props.size {
        ButtonSize::Small => "h-8 gap-1 px-3 rounded-lg",
        ButtonSize::Medium => "h-9 gap-1.5 px-3 rounded-lg",
        ButtonSize::Large => "h-10 gap-1.5 px-4 rounded-lg",
    };

    let disabled = props.disabled || props.loading;

    rsx! {
        button {
            class: "{base} {variant} {size} {props.class}",
            disabled: disabled,
            r#type: "button",
            onclick: move |e| {
                if let Some(cb) = &props.on_click {
                    cb.call(e);
                }
            },
            if props.loading {
                svg {
                    class: "size-4 animate-spin",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "M21 12a9 9 0 1 1-6.219-8.56" }
                }
            }
            {props.children}
        }
    }
}
