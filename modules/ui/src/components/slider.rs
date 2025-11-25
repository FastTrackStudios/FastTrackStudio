use dioxus::prelude::*;

/// Slider component for numeric input
#[component]
pub fn Slider(
    /// Current value
    value: Signal<f64>,
    /// Minimum value
    #[props(default = 0.0)]
    min: f64,
    /// Maximum value
    #[props(default = 100.0)]
    max: f64,
    /// Step size
    #[props(default = 1.0)]
    step: f64,
    /// Optional label
    label: Option<String>,
    /// Optional callback for value changes
    on_change: Option<Callback<f64>>,
) -> Element {
    let current_value = value();
    let clamped_value = current_value.max(min).min(max);
    let percentage = ((clamped_value - min) / (max - min) * 100.0).max(0.0).min(100.0);
    
    rsx! {
        div {
            class: "flex flex-col gap-1",
            if let Some(label_text) = label {
                div {
                    class: "text-xs text-muted-foreground",
                    "{label_text}"
                }
            }
            div {
                class: "relative flex items-center gap-2",
                // Track container
                div {
                    class: "relative h-2 w-32 rounded-full bg-secondary flex-1",
                    // Fill
                    div {
                        class: "absolute h-full rounded-full bg-primary transition-all",
                        style: format!("width: {}%;", percentage),
                    }
                    // Thumb (visual indicator)
                    div {
                        class: "absolute top-1/2 -translate-y-1/2 w-4 h-4 rounded-full bg-primary border-2 border-background shadow-sm transition-all",
                        style: format!("left: calc({}% - 8px);", percentage),
                    }
                    // Hidden input for interaction
                    input {
                        r#type: "range",
                        class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer z-10",
                        min: "{min}",
                        max: "{max}",
                        step: "{step}",
                        value: "{clamped_value}",
                        oninput: move |evt| {
                            if let Ok(val) = evt.value().parse::<f64>() {
                                let clamped = val.max(min).min(max);
                                value.set(clamped);
                                if let Some(callback) = on_change {
                                    callback.call(clamped);
                                }
                            }
                        },
                    }
                }
                // Value display
                div {
                    class: "text-xs text-muted-foreground min-w-[3.5rem] text-right font-mono",
                    "{clamped_value:.1}x"
                }
            }
        }
    }
}

