use dioxus::prelude::*;
use tracing::{debug, info};

/// Mode for text fitting algorithm
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum TextFitMode {
    /// Text fills width, no wrapping
    Oneline,
    /// Text fills width, wraps to multiple lines if needed
    #[default]
    Multiline,
    /// Text fills both width and height, allows wrapping
    Box,
    /// Text fills both width and height, no wrapping
    BoxOneline,
}

/// Props for the TextFit component
#[derive(Props, Clone, PartialEq)]
pub struct TextFitProps {
    /// The text content to fit
    pub text: String,

    /// Optional class name for additional styling
    #[props(default)]
    pub class: Option<String>,

    /// Text fitting mode
    #[props(default)]
    pub mode: TextFitMode,

    /// Minimum font size in pixels
    #[props(default = 8.0)]
    pub min_font_size_px: f64,

    /// Maximum font size in pixels
    #[props(default = 160.0)]
    pub max_font_size_px: f64,
}

/// A component that automatically adjusts text size to fit its container
///
/// This component uses CSS container queries and clamp() to ensure text
/// fits within its container. It works purely through CSS without requiring
/// JavaScript or DOM manipulation.
#[component]
pub fn TextFit(props: TextFitProps) -> Element {
    let container_id = use_signal(|| format!("text-fit-container-{}", uuid::Uuid::new_v4()));

    info!(
        container_id = %container_id.peek(),
        text_length = props.text.len(),
        mode = ?props.mode,
        "TextFit: Component rendering"
    );

    // Build container classes
    let container_classes = vec![
        "@container", // Enable container queries
        "flex items-center justify-center w-full h-full",
        props.class.as_deref().unwrap_or(""),
    ]
    .into_iter()
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>()
    .join(" ");

    // Calculate font size using container query units
    // Use clamp() to ensure it stays within min/max bounds
    // For Box mode, we need to consider both width and height
    let font_size_style = match props.mode {
        TextFitMode::Oneline | TextFitMode::BoxOneline => {
            // Single line: scale based on width only
            format!(
                "font-size: clamp({}px, {}cqw, {}px); white-space: nowrap;",
                props.min_font_size_px,
                // Estimate: assume average character width is ~0.6 of font size
                // So for width W, we want font size such that text fits
                // Rough estimate: use 1.5% of container width per character
                (props.max_font_size_px * 100.0 / props.text.len().max(1) as f64).min(10.0),
                props.max_font_size_px
            )
        }
        TextFitMode::Multiline => {
            // Multi-line: scale based on width, allow wrapping
            format!(
                "font-size: clamp({}px, {}cqw, {}px); word-break: break-word;",
                props.min_font_size_px,
                // For multiline, we can use more of the width
                (props.max_font_size_px * 100.0 / props.text.len().max(1) as f64).min(8.0),
                props.max_font_size_px
            )
        }
        TextFitMode::Box => {
            // Box mode: scale based on both width and height
            // Use the smaller of width-based or height-based sizing
            // Scale down container query units to make effective container smaller (85% = 15% safety margin)
            let container_scale = 0.85; // Use 85% of container size for calculations
            let line_count = props.text.lines().count().max(1) as f64;
            let line_height_factor = 1.5; // Line height multiplier

            // Scale down the container query units
            let width_based_pct = (props.max_font_size_px * 100.0 / props.text.len().max(1) as f64)
                .min(8.0)
                * container_scale;
            let height_based_pct = (100.0 / (line_count * line_height_factor)) * container_scale;

            format!(
                "font-size: clamp({}px, min({}cqw, {}cqh), {}px); word-break: break-word; white-space: pre-wrap;",
                props.min_font_size_px,
                width_based_pct,
                height_based_pct.min(5.0),
                props.max_font_size_px
            )
        }
    };

    let inner_classes = vec![
        "block text-center p-3",
        match props.mode {
            TextFitMode::Oneline | TextFitMode::BoxOneline => "whitespace-nowrap",
            TextFitMode::Multiline => "break-words",
            TextFitMode::Box => "whitespace-pre-wrap break-words",
        },
    ]
    .into_iter()
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>()
    .join(" ");

    rsx! {
        div {
            id: "{container_id.peek()}",
            class: "{container_classes}",
            div {
                class: "{inner_classes}",
                style: "{font_size_style}",
                {props.text}
            }
        }
    }
}
