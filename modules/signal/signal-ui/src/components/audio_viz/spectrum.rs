//! Spectrum analyzer — bar-graph frequency visualization.

use dioxus::prelude::*;

/// A frequency spectrum bar graph.
#[derive(Props, Clone, PartialEq)]
pub struct SpectrumAnalyzerProps {
    /// Frequency bin magnitudes (0.0–1.0 normalized). Each entry is one bar.
    #[props(default)]
    bins: Vec<f64>,

    /// Width in pixels.
    #[props(default = 200)]
    width: u32,

    /// Height in pixels.
    #[props(default = 64)]
    height: u32,

    /// Bar gap in pixels.
    #[props(default = 1)]
    gap: u32,

    /// Extra CSS classes.
    #[props(default)]
    class: String,
}

#[component]
pub fn SpectrumAnalyzer(props: SpectrumAnalyzerProps) -> Element {
    let w = props.width;
    let h = props.height;
    let hf = h as f64;
    let count = props.bins.len().max(1);
    let gap = props.gap as f64;
    let bar_w = ((w as f64 - gap * (count as f64 - 1.0)) / count as f64).max(1.0);

    rsx! {
        div {
            class: format!("relative overflow-hidden rounded bg-muted/30 flex items-end {}", props.class),
            style: "width: {w}px; height: {h}px; gap: {gap}px;",

            for bin in props.bins.iter() {
                {
                    let mag = bin.clamp(0.0, 1.0);
                    let bar_h = (mag * hf).max(1.0);
                    // Color gradient: low=green, mid=yellow, high=red
                    let color = if mag > 0.85 {
                        "bg-red-500"
                    } else if mag > 0.6 {
                        "bg-yellow-500"
                    } else {
                        "bg-green-500"
                    };
                    rsx! {
                        div {
                            class: format!("{color} rounded-t transition-all duration-75"),
                            style: "width: {bar_w:.1}px; height: {bar_h:.1}px;",
                        }
                    }
                }
            }
        }
    }
}
