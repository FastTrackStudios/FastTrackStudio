//! FTS-Native - FastTrackStudio Native Chart Editor
//!
//! A split pane editor with keyflow chart text input on the left
//! and a live WGPU chart preview on the right.

use dioxus_native::prelude::*;
use dioxus_native::use_wgpu;
use keyflow::Chart;
use lumen_blocks::components::button::{Button, ButtonVariant};

mod chart_renderer;
use chart_renderer::{ChartMessage, ChartPaintSource};

/// Default chart text to start with
const DEFAULT_CHART_TEXT: &str = r#"Well - Jacob Collier
120bpm 4/4 #E

Intro 4
1_2 2maj_2 | 4 | x2

VS 16
1_2 2maj_2 | 4 | x^

[Hits]
'1_2 '2maj_2 | '4 |

VS

[Hits]

INST 8

[SOLO Keys] 8

Br 16

br

Outro 16
"#;

fn main() {
    env_logger::init();
    dioxus_native::launch(app);
}

fn app() -> Element {
    // Chart text state
    let mut chart_text = use_signal(|| DEFAULT_CHART_TEXT.to_string());

    // Parse chart text in real-time
    let parsed_chart = use_memo(move || Chart::parse(&chart_text()));
    let parse_error = use_memo(move || parsed_chart().err().map(|e| e.to_string()));

    // Get the parsed chart for the preview
    let chart_for_preview = use_memo(move || parsed_chart().ok());

    rsx! {
        style { {TAILWIND_CSS} }
        div { class: "h-screen w-screen bg-background text-foreground flex flex-col",
            // Header
            div { class: "flex-shrink-0 border-b border-border p-4 flex items-center justify-between",
                h1 { class: "text-xl font-bold", "FTS-Native Chart Editor" }
                div { class: "flex gap-2",
                    Button {
                        variant: ButtonVariant::Secondary,
                        on_click: move |_| chart_text.set(DEFAULT_CHART_TEXT.to_string()),
                        "Reset"
                    }
                }
            }

            // Split pane content
            div { class: "flex-1 flex overflow-hidden",
                // Left pane - Chart text editor
                div { class: "w-1/2 border-r border-border flex flex-col",
                    // Editor header
                    div { class: "flex-shrink-0 p-4 border-b border-border",
                        h2 { class: "text-lg font-semibold mb-2", "Chart Text" }
                        if let Some(error) = parse_error() {
                            div { class: "text-sm text-destructive bg-destructive/10 p-2 rounded",
                                "Parse Error: {error}"
                            }
                        } else {
                            div { class: "text-sm text-muted-foreground",
                                if let Some(chart) = chart_for_preview() {
                                    {format!("✓ Parsed: {} sections, {} total measures",
                                        chart.sections.len(),
                                        chart.sections.iter().map(|s| s.measures.len()).sum::<usize>()
                                    )}
                                } else {
                                    "No chart data"
                                }
                            }
                        }
                    }
                    // Text editor
                    textarea {
                        class: "flex-1 p-4 font-mono text-sm bg-card border-0 resize-none focus:outline-none",
                        value: "{chart_text()}",
                        oninput: move |evt| chart_text.set(evt.value()),
                        placeholder: "Enter chart text here...",
                    }
                }

                // Right pane - WGPU Chart Preview
                div { class: "w-1/2 flex flex-col",
                    // Preview header
                    div { class: "flex-shrink-0 p-4 border-b border-border",
                        h2 { class: "text-lg font-semibold", "Chart Preview" }
                        p { class: "text-sm text-muted-foreground", "WGPU-rendered chart visualization" }
                    }
                    // WGPU canvas
                    div { class: "flex-1 overflow-hidden",
                        ChartPreviewCanvas { chart: chart_for_preview() }
                    }
                }
            }
        }
    }
}

/// WGPU Chart Preview Canvas component
#[component]
fn ChartPreviewCanvas(chart: Option<Chart>) -> Element {
    // Create paint source for the chart (only once)
    let sender = use_hook(|| {
        std::rc::Rc::new(std::cell::RefCell::new(None::<std::sync::mpsc::Sender<ChartMessage>>))
    });

    let paint_source_id = {
        let sender_cell = sender.clone();
        use_wgpu(move || {
            let paint_source = ChartPaintSource::new();
            *sender_cell.borrow_mut() = Some(paint_source.sender());
            paint_source
        })
    };

    // Send chart updates when chart changes
    use_effect(move || {
        if let Some(ref sender) = *sender.borrow() {
            if let Some(chart) = chart.clone() {
                let _ = sender.send(ChartMessage::UpdateChart(chart));
            }
        }
    });

    rsx! {
        div {
            class: "w-full h-full",
            style: "display: grid;",
            canvas {
                id: "chart-preview",
                "src": "{paint_source_id}"
            }
        }
    }
}

const TAILWIND_CSS: &str = r#"
/* Base styles for FTS-Native */
:root {
    --background: #09090b;
    --foreground: #fafafa;
    --card: #18181b;
    --card-foreground: #fafafa;
    --primary: #fafafa;
    --primary-foreground: #18181b;
    --secondary: #27272a;
    --secondary-foreground: #fafafa;
    --muted: #27272a;
    --muted-foreground: #a1a1aa;
    --destructive: #ef4444;
    --destructive-foreground: #fafafa;
    --border: #27272a;
    --input: #27272a;
    --ring: #52525b;
    --radius: 0.5rem;
}

*, *::before, *::after {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

html, body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 16px;
    line-height: 1.5;
    background-color: var(--background);
    color: var(--foreground);
}

/* Layout */
.h-screen { height: 100vh; }
.w-screen { width: 100vw; }
.w-full { width: 100%; }
.h-full { height: 100%; }
.w-1\/2 { width: 50%; }
.flex { display: flex; }
.flex-col { flex-direction: column; }
.flex-1 { flex: 1 1 0%; }
.flex-shrink-0 { flex-shrink: 0; }
.items-center { align-items: center; }
.justify-between { justify-content: space-between; }
.gap-2 { gap: 0.5rem; }
.overflow-hidden { overflow: hidden; }

/* Spacing */
.p-2 { padding: 0.5rem; }
.p-4 { padding: 1rem; }
.mb-2 { margin-bottom: 0.5rem; }

/* Typography */
.text-sm { font-size: 0.875rem; line-height: 1.25rem; }
.text-lg { font-size: 1.125rem; line-height: 1.75rem; }
.text-xl { font-size: 1.25rem; line-height: 1.75rem; }
.font-mono { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; }
.font-semibold { font-weight: 600; }
.font-bold { font-weight: 700; }

/* Colors */
.bg-background { background-color: var(--background); }
.bg-card { background-color: var(--card); }
.text-foreground { color: var(--foreground); }
.text-muted-foreground { color: var(--muted-foreground); }
.text-destructive { color: var(--destructive); }
.bg-destructive\/10 { background-color: rgba(239, 68, 68, 0.1); }

/* Borders */
.border { border-width: 1px; border-style: solid; border-color: var(--border); }
.border-b { border-bottom-width: 1px; border-bottom-style: solid; border-color: var(--border); }
.border-r { border-right-width: 1px; border-right-style: solid; border-color: var(--border); }
.border-border { border-color: var(--border); }
.border-0 { border-width: 0; }
.rounded { border-radius: var(--radius); }

/* Form elements */
textarea {
    width: 100%;
    color: var(--foreground);
    background-color: var(--card);
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
}

textarea:focus {
    outline: none;
}

.resize-none { resize: none; }
.focus\:outline-none:focus { outline: none; }
"#;
