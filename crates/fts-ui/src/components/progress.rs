//! Progress — shadcn v4 maia style, standalone (no lumen-blocks).

use dioxus::prelude::*;

/// Visual variant for the progress bar fill.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum ProgressVariant {
    #[default]
    Default,
    Destructive,
    Success,
    Warning,
}

impl ProgressVariant {
    // shadcn v4 maia: cn-progress fill color
    fn fill_class(self) -> &'static str {
        match self {
            Self::Default => "bg-primary",
            Self::Destructive => "bg-destructive",
            Self::Success => "bg-green-500",
            Self::Warning => "bg-yellow-500",
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ProgressProps {
    /// Progress value from 0 to 100.
    #[props(default = 0.0)]
    pub value: f64,
    #[props(default)]
    pub variant: ProgressVariant,
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-progress
#[component]
pub fn Progress(props: ProgressProps) -> Element {
    let value = props.value.clamp(0.0, 100.0);

    // shadcn v4 maia: cn-progress track
    let track = "bg-muted relative h-3 w-full overflow-hidden rounded-full";

    // shadcn v4 maia: cn-progress fill
    let fill_base = "h-full rounded-full transition-all duration-500";
    let fill_color = props.variant.fill_class();

    rsx! {
        div {
            class: "{track} {props.class}",
            role: "progressbar",
            "aria-valuenow": "{value}",
            "aria-valuemin": "0",
            "aria-valuemax": "100",
            div {
                class: "{fill_base} {fill_color}",
                style: "width: {value}%",
            }
        }
    }
}
