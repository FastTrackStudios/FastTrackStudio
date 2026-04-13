//! Slider — shadcn v4 maia style range slider.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Slider
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SliderProps {
    /// The current value (two-way bound).
    pub value: Signal<f64>,

    /// Minimum value.
    #[props(default = 0.0)]
    pub min: f64,

    /// Maximum value.
    #[props(default = 100.0)]
    pub max: f64,

    /// Step increment.
    #[props(default = 1.0)]
    pub step: f64,

    /// Whether the slider is disabled.
    #[props(default = false)]
    pub disabled: bool,

    /// Extra CSS classes on the outer container.
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: slider
#[component]
pub fn Slider(props: SliderProps) -> Element {
    let mut value = props.value;
    let min = props.min;
    let max = props.max;
    let step = props.step;

    let range = max - min;
    let pct = if range > 0.0 {
        ((*value.read() - min) / range * 100.0).clamp(0.0, 100.0)
    } else {
        0.0
    };

    rsx! {
        div {
            class: format!("relative flex w-full touch-none select-none items-center {}", props.class),

            // Visual track
            div { class: "relative h-3 w-full rounded-full bg-muted overflow-hidden",
                // Filled range
                div {
                    class: "absolute h-full bg-primary rounded-full",
                    style: format!("width: {pct}%"),
                }
            }

            // Transparent native range input overlaid on top
            input {
                r#type: "range",
                min: "{min}",
                max: "{max}",
                step: "{step}",
                disabled: if props.disabled { Some(true) } else { None },
                value: "{value}",
                class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer disabled:cursor-not-allowed",
                oninput: move |evt| {
                    if let Ok(v) = evt.value().parse::<f64>() {
                        value.set(v);
                    }
                },
            }
        }
    }
}
