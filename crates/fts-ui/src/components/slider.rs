//! Slider — shadcn v4 maia style range slider.

use dioxus::prelude::*;
use dioxus_primitives::slider::{Slider as PrimitiveSlider, SliderRange, SliderThumb, SliderTrack};

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

    rsx! {
        PrimitiveSlider {
            value: Some(value()),
            min,
            max,
            step,
            disabled: props.disabled,
            label: None::<String>,
            class: crate::cn::merge_slice(&["relative flex w-full touch-none select-none items-center", props.class.as_str()]),
            on_value_change: move |next: f64| {
                value.set(next);
            },
            SliderTrack {
                class: "relative h-3 w-full rounded-full bg-muted overflow-hidden",
                SliderRange {
                    class: "absolute h-full bg-primary rounded-full",
                }
                SliderThumb {
                    class: "absolute top-1/2 size-5 -translate-x-1/2 -translate-y-1/2 rounded-full border border-primary bg-background shadow transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring disabled:pointer-events-none disabled:opacity-50",
                }
            }
        }
    }
}
