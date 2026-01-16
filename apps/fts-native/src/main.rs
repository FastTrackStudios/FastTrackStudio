//! FTS-Native - Chart Editor with Vello-based rendering
//!
//! Uses engraver's layout system and Vello for GPU-accelerated chart rendering.

use dioxus_html::input_data::MouseButton;
use dioxus_native::prelude::*;
use dioxus_native::use_wgpu;
use keyflow::Chart;
use winit::dpi::LogicalSize;
use winit::window::WindowAttributes;

mod chart_layout;
mod chart_vello_renderer;
use chart_vello_renderer::{ChartMessage, ChartVelloPaintSource};

/// Default chart text to start with
const DEFAULT_CHART_TEXT: &str = r#"Autumn Leaves (Extended) - Joseph Kosma
120bpm 4/4 #G

intro 8
mainTheme = m{ E_4 F#_4 G_4. A_8 B_2 }
Gmaj7 m{ E_4 F#_4 G_4. A_8 } Cmaj7 m{ B_2 r_2 } F#m7b5 B7
Em7 $mainTheme A7 m{ r_1 } Dmaj7 G7

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4
Am7 m{ A_8 B_8 C_4 D_4 E_4 } D7 Bm7 E7

ch 16
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 E7
Am7 D7 Gmaj7 Cmaj7
F#m7b5 B7 Em7 m{ r_1 }

vs 16
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em7

pre 4

ch 16

br 8
Cmaj7 m{ G_4 F#_4 E_4 D_4 } Bm7 Am7 Gmaj7
F#m7b5 B7 Em7 A7

inst 16
Gmaj7 Cmaj7 F#m7b5 B7
Em7 Am7 D7 Gmaj7
Cmaj7 F#m7b5 B7 Em
Em7 Am7 D7 G7

ch 16

outro 8
Gmaj7 m{ E_2 r_4 D_4 } Cmaj7 m{ C_2. r_4 } F#m7b5 B7
Em7 Am7 D7 Gmaj7 m{ G_1 }
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
        div { class: "h-screen w-screen bg-background text-foreground flex",
            // Left pane - Chart text editor
            div { class: "w-1/2 border-r border-border flex flex-col",
                // Editor header with title info
                div { class: "flex-shrink-0 p-4 border-b border-border",
                    div { class: "flex items-center justify-between mb-2",
                        h2 { class: "text-lg font-semibold", "Chart Editor" }
                        button {
                            class: "px-3 py-1 text-xs bg-secondary text-secondary-foreground rounded hover:bg-secondary/80",
                            onclick: move |_| chart_text.set(DEFAULT_CHART_TEXT.to_string()),
                            "Reset"
                        }
                    }
                    if let Some(error) = parse_error() {
                        div { class: "text-sm text-destructive bg-destructive/10 p-2 rounded",
                            "Parse Error: {error}"
                        }
                    } else {
                        div { class: "text-sm text-muted-foreground",
                            if let Some(chart) = chart_for_preview() {
                                {format!("Parsed: {} sections, {} measures",
                                    chart.sections.len(),
                                    chart.sections.iter().map(|s| s.measures().len()).sum::<usize>()
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
                            p { "Chords: Cmaj7, Am7, D7, F#m7b5, etc." }
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

            // Right pane - Chart Preview (full height for proper aspect ratio)
            div { class: "w-1/2 flex flex-col",
                ChartPreviewCanvas { chart: chart_for_preview() }
            }
        }
    }
}

/// Vello-based Chart Preview component with page navigation controls
#[component]
fn ChartPreviewCanvas(chart: Option<Chart>) -> Element {
    // Create paint source and store sender
    let sender = use_hook(|| {
        std::rc::Rc::new(std::cell::RefCell::new(None::<std::sync::mpsc::Sender<ChartMessage>>))
    });

    let paint_source_id = {
        let sender_cell = sender.clone();
        use_wgpu(move || {
            let paint_source = ChartVelloPaintSource::new();
            *sender_cell.borrow_mut() = Some(paint_source.sender());
            paint_source
        })
    };

    // Clone sender for event handlers
    let wheel_sender = sender.clone();
    let mouse_move_sender = sender.clone();
    let mouse_down_sender = sender.clone();
    let mouse_up_sender = sender.clone();
    let mouse_leave_sender = sender.clone();
    let prev_page_sender = sender.clone();
    let next_page_sender = sender.clone();
    let fit_page_sender = sender.clone();
    let reset_view_sender = sender.clone();

    // Send chart updates on every render (component re-renders when prop changes)
    if let Some(ref sender_ref) = *sender.borrow() {
        let _ = sender_ref.send(ChartMessage::UpdateChart(chart.clone()));
    }

    rsx! {
        // Toolbar with page controls
        div { class: "flex-shrink-0 p-2 border-b border-border flex items-center justify-between bg-card",
            // Page navigation buttons
            div { class: "flex items-center gap-2",
                button {
                    class: "px-2 py-1 text-xs bg-secondary text-secondary-foreground rounded hover:bg-secondary/80",
                    onclick: move |_| {
                        if let Some(ref sender) = *prev_page_sender.borrow() {
                            let _ = sender.send(ChartMessage::PrevPage);
                        }
                    },
                    "Prev"
                }
                button {
                    class: "px-2 py-1 text-xs bg-secondary text-secondary-foreground rounded hover:bg-secondary/80",
                    onclick: move |_| {
                        if let Some(ref sender) = *next_page_sender.borrow() {
                            let _ = sender.send(ChartMessage::NextPage);
                        }
                    },
                    "Next"
                }
            }
            // View controls
            div { class: "flex items-center gap-2",
                button {
                    class: "px-2 py-1 text-xs bg-secondary text-secondary-foreground rounded hover:bg-secondary/80",
                    onclick: move |_| {
                        if let Some(ref sender) = *fit_page_sender.borrow() {
                            // Use approximate viewport size (will be overwritten by actual size)
                            let _ = sender.send(ChartMessage::FitPage {
                                viewport_width: 700.0,
                                viewport_height: 850.0,
                            });
                        }
                    },
                    "Fit Page"
                }
                button {
                    class: "px-2 py-1 text-xs bg-secondary text-secondary-foreground rounded hover:bg-secondary/80",
                    onclick: move |_| {
                        if let Some(ref sender) = *reset_view_sender.borrow() {
                            let _ = sender.send(ChartMessage::ResetView);
                        }
                    },
                    "Reset"
                }
            }
            // Help text
            span { class: "text-xs text-muted-foreground", "Scroll=Zoom | Drag=Pan" }
        }
        // Canvas container (takes remaining space)
        div {
            class: "flex-1 overflow-hidden",
            style: "display: grid; cursor: grab;",

            // Mouse wheel = zoom about cursor (Vello example style)
            onwheel: move |evt| {
                if let Some(ref sender) = *wheel_sender.borrow() {
                    let delta_y = evt.delta().strip_units().y;
                    let coords = evt.element_coordinates();
                    let _ = sender.send(ChartMessage::MouseWheel {
                        delta_y,
                        cursor_x: coords.x,
                        cursor_y: coords.y,
                    });
                }
            },

            // Track mouse movement for drag-to-pan
            onmousemove: move |evt| {
                if let Some(ref sender) = *mouse_move_sender.borrow() {
                    let coords = evt.element_coordinates();
                    let _ = sender.send(ChartMessage::MouseMove {
                        x: coords.x,
                        y: coords.y,
                    });
                }
            },

            // Mouse down = start drag
            onmousedown: move |evt| {
                if evt.trigger_button() == Some(MouseButton::Primary) {
                    if let Some(ref sender) = *mouse_down_sender.borrow() {
                        let _ = sender.send(ChartMessage::MouseButton { pressed: true });
                    }
                }
            },

            // Mouse up = end drag
            onmouseup: move |evt| {
                if evt.trigger_button() == Some(MouseButton::Primary) {
                    if let Some(ref sender) = *mouse_up_sender.borrow() {
                        let _ = sender.send(ChartMessage::MouseButton { pressed: false });
                    }
                }
            },

            // Mouse leave = cancel drag (prevents stuck drag when cursor exits)
            onmouseleave: move |_| {
                if let Some(ref sender) = *mouse_leave_sender.borrow() {
                    let _ = sender.send(ChartMessage::MouseButton { pressed: false });
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

/* Button styles */
.px-3 { padding-left: 0.75rem; padding-right: 0.75rem; }
.py-1 { padding-top: 0.25rem; padding-bottom: 0.25rem; }
.bg-secondary { background-color: var(--secondary); }
.text-secondary-foreground { color: var(--secondary-foreground); }
.hover\:bg-secondary\/80:hover { background-color: rgba(39, 39, 42, 0.8); }
button { border: none; cursor: pointer; }

/* Details/summary styling */
details summary { list-style: none; }
details summary::-webkit-details-marker { display: none; }
details summary::before { content: '▶ '; font-size: 0.6em; }
details[open] summary::before { content: '▼ '; }
"#;
