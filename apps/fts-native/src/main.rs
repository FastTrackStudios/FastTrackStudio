//! FTS-Native - FastTrackStudio Native Chart Editor
//!
//! A split pane editor with keyflow chart text input on the left
//! and a live WGPU chart preview on the right.

use dioxus_native::prelude::*;
use dioxus_native::use_wgpu;
use keyflow::Chart;
use lumen_blocks::components::button::{Button, ButtonVariant};
use winit::dpi::LogicalSize;
use winit::window::WindowAttributes;

mod chart_renderer;
use chart_renderer::{ChartMessage, ChartPaintSource};

/// Default chart text to start with
/// Uses keyflow chart syntax: Title - Artist, tempo/time/key, then sections
/// Chords are written as note names with qualities (Gmaj7, Am7, D7)
/// Each space-separated chord is one measure by default
const DEFAULT_CHART_TEXT: &str = r#"Autumn Leaves - Joseph Kosma
120bpm 4/4 #G

intro 4
Gmaj7 Cmaj7 F#m7b5 B7

vs 8
Em7 Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em

ch 8
Am7 D7 Gmaj7 Cmaj7 F#m7b5 B7 Em7 E7

br 4
Am7 D7 Gmaj7 B7

outro 4
Em7 Am7 D7 Gmaj7
"#;

fn main() {
    env_logger::init();

    let window_attrs = WindowAttributes::default()
        .with_title("FTS-Native Chart Editor")
        .with_inner_size(LogicalSize::new(1400.0, 900.0));

    dioxus_native::launch_cfg(app, vec![], vec![Box::new(window_attrs)]);
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
                        // Syntax help
                        details { class: "mt-2",
                            summary { class: "text-xs text-muted-foreground cursor-pointer", "Syntax Help" }
                            div { class: "text-xs text-muted-foreground mt-1 space-y-1",
                                p { "Line 1: Title - Artist" }
                                p { "Line 2: 120bpm 4/4 #C (tempo, time sig, key)" }
                                p { "Sections: intro, vs, ch, br, pre, post, inst, outro + measure count" }
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
                        if let Some(ref chart) = chart_for_preview() {
                            if let Some(ref title) = chart.metadata.title {
                                p { class: "text-sm text-muted-foreground",
                                    {format!("{}{}", title, chart.metadata.artist.as_ref().map(|a| format!(" - {}", a)).unwrap_or_default())}
                                }
                            } else {
                                p { class: "text-sm text-muted-foreground", "WGPU-rendered chart visualization" }
                            }
                        } else {
                            p { class: "text-sm text-muted-foreground", "WGPU-rendered chart visualization" }
                        }
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

    // Clone sender for event handlers
    let wheel_sender = sender.clone();

    // Send chart updates on every render (component re-renders when prop changes)
    // This ensures the renderer always has the latest chart data
    if let Some(ref sender_ref) = *sender.borrow() {
        let _ = sender_ref.send(ChartMessage::UpdateChart(chart.clone()));
    }

    rsx! {
        div {
            class: "w-full h-full",
            style: "display: grid;",
            // Handle mouse wheel for zoom/pan
            // Option/Alt + scroll = zoom, regular scroll = pan
            onwheel: move |evt| {
                if let Some(ref sender) = *wheel_sender.borrow() {
                    let delta_y = evt.delta().strip_units().y as f32;

                    // Check if Option/Alt key is held for zoom
                    if evt.modifiers().alt() {
                        // Zoom - use element coordinates for cursor position
                        let coords = evt.element_coordinates();
                        let _ = sender.send(ChartMessage::Zoom {
                            delta: -delta_y * 0.01,
                            cursor_x: coords.x as f32,
                            cursor_y: coords.y as f32,
                        });
                    } else {
                        // Pan
                        let delta_x = evt.delta().strip_units().x as f32;
                        let _ = sender.send(ChartMessage::Pan {
                            dx: delta_x,
                            dy: delta_y,
                        });
                    }
                }
            },
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
.mt-1 { margin-top: 0.25rem; }
.mt-2 { margin-top: 0.5rem; }
.space-y-1 > * + * { margin-top: 0.25rem; }

/* Typography */
.text-xs { font-size: 0.75rem; line-height: 1rem; }
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
.cursor-pointer { cursor: pointer; }

/* Details/summary styling */
details summary { list-style: none; }
details summary::-webkit-details-marker { display: none; }
details summary::before { content: '▶ '; font-size: 0.6em; }
details[open] summary::before { content: '▼ '; }
"#;
