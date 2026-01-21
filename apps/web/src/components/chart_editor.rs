//! Chart Editor Component
//!
//! Interactive split-view editor with live chart preview.

use dioxus::prelude::*;

/// Default chart content for new editors - demonstrates various keyflow features
const DEFAULT_CHART: &str = r#"---
title: "Example Song"
artist: "Your Name"
key: G
time: 4/4
tempo: 120
---

[Intro]
| G . . . | D . . . |

[Verse 1]
| G . . . | Em . . . |
| C . . . | D  . . . |
| G . . . | Em . . . |
| C . D . | G  . . . |

[Chorus]
| C . . . | G  . . . |
| Am . . . | D . . . |
| C . . . | G  . . . |
| Am . D . | G . . . |

[Verse 2]
| G . . . | Em . . . |
| C . . . | D  . . . |

[Bridge]
| Em . . . | C . . . |
| G  . . . | D . . . |

[Outro]
| G . . . | C . G . |
"#;

/// Chart editor with live preview.
#[component]
pub fn ChartEditor() -> Element {
    // Source text state
    let mut source = use_signal(|| DEFAULT_CHART.to_string());

    rsx! {
        div {
            class: "grid grid-cols-2 h-[calc(100vh-4rem)]",

            // Left side - Text editor
            div {
                class: "flex flex-col border-r border-border overflow-hidden",

                // Editor header
                div {
                    class: "px-4 py-3 border-b border-border bg-card flex items-center justify-between shrink-0",

                    div {
                        class: "flex items-center gap-2",
                        lucide_dioxus::FileText { class: "w-4 h-4 text-primary" }
                        h2 {
                            class: "text-sm font-semibold text-foreground",
                            "Keyflow Source"
                        }
                    }

                    button {
                        class: "text-xs text-muted-foreground hover:text-foreground px-3 py-1.5 rounded-md hover:bg-accent transition-colors border border-border",
                        onclick: move |_| source.set(DEFAULT_CHART.to_string()),
                        "Reset"
                    }
                }

                // Textarea container
                div {
                    class: "flex-1 overflow-hidden",

                    textarea {
                        class: "w-full h-full bg-background text-foreground font-mono text-sm p-4 resize-none focus:outline-none border-none",
                        value: "{source}",
                        spellcheck: false,
                        oninput: move |evt| source.set(evt.value()),
                        placeholder: "Enter keyflow chart notation..."
                    }
                }
            }

            // Right side - Preview
            div {
                class: "flex flex-col bg-muted/30 overflow-hidden",

                // Preview header
                div {
                    class: "px-4 py-3 border-b border-border bg-card flex items-center gap-2 shrink-0",

                    lucide_dioxus::Eye { class: "w-4 h-4 text-primary" }
                    h2 {
                        class: "text-sm font-semibold text-foreground",
                        "Live Preview"
                    }
                    span {
                        class: "text-xs text-muted-foreground ml-2",
                        "(A4 • Scroll to zoom • Drag to pan)"
                    }
                }

                // Chart preview - A4 aspect ratio container
                div {
                    class: "flex-1 p-4 overflow-auto flex items-start justify-center",

                    // A4 page container (210mm x 297mm = 1:1.414 ratio)
                    div {
                        class: "bg-white rounded-lg shadow-xl overflow-hidden",
                        // A4 proportions: width is fixed, height maintains ratio
                        style: "width: min(100%, 595px); aspect-ratio: 210 / 297; min-height: 0;",

                        DynamicChartRenderer {
                            source: source()
                        }
                    }
                }
            }
        }
    }
}

/// Dynamic chart renderer that works with owned strings.
#[component]
fn DynamicChartRenderer(source: String) -> Element {
    // Parse the chart to validate it
    let parse_result = keyflow::Chart::parse(&source);

    match parse_result {
        Ok(chart) => {
            // Get chart metadata for display
            let title = chart.metadata.title.clone().unwrap_or_else(|| "Untitled".to_string());
            let artist = chart.metadata.artist.clone();
            let tempo = chart.metadata.tempo;
            let section_count = chart.sections.len();

            rsx! {
                div {
                    class: "w-full h-full flex flex-col relative",

                    // Canvas for WebGPU rendering - white background like paper
                    canvas {
                        id: "editor-chart-canvas",
                        class: "w-full h-full cursor-grab active:cursor-grabbing",
                        style: "touch-action: none; background: white;",
                    }

                    // Metadata overlay in top-left
                    div {
                        class: "absolute top-3 left-4 pointer-events-none",

                        div {
                            class: "text-lg font-bold text-gray-800",
                            "{title}"
                        }

                        if let Some(artist) = artist {
                            div {
                                class: "text-sm text-gray-600",
                                "{artist}"
                            }
                        }

                        if let Some(tempo) = tempo {
                            div {
                                class: "text-xs text-gray-500 mt-1",
                                "{tempo} BPM"
                            }
                        }
                    }

                    // Section count in bottom-left
                    div {
                        class: "absolute bottom-2 left-3 text-xs text-gray-400 pointer-events-none",
                        "{section_count} section(s)"
                    }

                    // WebGPU rendering
                    DynamicChartCanvas { source: source }
                }
            }
        }
        Err(e) => {
            let error_msg = e.to_string();
            rsx! {
                div {
                    class: "w-full h-full flex items-center justify-center bg-red-50",

                    div {
                        class: "text-center p-6 max-w-md",

                        lucide_dioxus::CircleAlert { class: "w-12 h-12 mx-auto mb-3 text-red-400" }

                        div {
                            class: "text-lg font-semibold text-red-600 mb-2",
                            "Parse Error"
                        }

                        div {
                            class: "text-sm text-red-500 font-mono whitespace-pre-wrap text-left bg-red-100 p-3 rounded-lg",
                            "{error_msg}"
                        }
                    }
                }
            }
        }
    }
}

/// Canvas component with WebGPU rendering for dynamic content.
#[component]
fn DynamicChartCanvas(source: String) -> Element {
    #[cfg(target_arch = "wasm32")]
    {
        use crate::renderer::ChartLayoutManager;
        use wasm_bindgen::prelude::*;
        use wasm_bindgen::JsCast;

        // Create layout manager signal
        let mut layout_manager = use_signal(|| None::<ChartLayoutManager>);
        let mut error_state = use_signal(|| None::<String>);
        let mut is_initialized = use_signal(|| false);

        // Transform state for pan/zoom
        let mut transform_x = use_signal(|| 20.0_f64);
        let mut transform_y = use_signal(|| 20.0_f64);
        let mut scale = use_signal(|| 1.0_f64);

        // Mouse interaction state
        let mut is_dragging = use_signal(|| false);
        let mut last_mouse_x = use_signal(|| 0.0_f64);
        let mut last_mouse_y = use_signal(|| 0.0_f64);

        // Trigger re-render
        let mut render_trigger = use_signal(|| 0_u32);

        // Initialize WebGPU on mount
        use_effect(move || {
            wasm_bindgen_futures::spawn_local(async move {
                match ChartLayoutManager::new() {
                    Ok(manager) => {
                        layout_manager.set(Some(manager));
                        is_initialized.set(true);
                        tracing::info!("Editor chart layout manager initialized");
                    }
                    Err(e) => {
                        error_state.set(Some(e));
                        tracing::error!("Failed to initialize editor chart layout manager");
                    }
                }
            });
        });

        // Setup mouse event listeners
        use_effect(move || {
            let window = match web_sys::window() {
                Some(w) => w,
                None => return,
            };
            let document = match window.document() {
                Some(d) => d,
                None => return,
            };
            let canvas = match document.get_element_by_id("editor-chart-canvas") {
                Some(c) => c,
                None => return,
            };

            // Mouse down - start dragging
            let mut is_dragging_clone = is_dragging.clone();
            let mut last_mouse_x_clone = last_mouse_x.clone();
            let mut last_mouse_y_clone = last_mouse_y.clone();
            let mousedown_closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                is_dragging_clone.set(true);
                last_mouse_x_clone.set(event.client_x() as f64);
                last_mouse_y_clone.set(event.client_y() as f64);
            }) as Box<dyn FnMut(_)>);
            canvas
                .add_event_listener_with_callback("mousedown", mousedown_closure.as_ref().unchecked_ref())
                .ok();
            mousedown_closure.forget();

            // Mouse move - drag to pan
            let is_dragging_clone = is_dragging.clone();
            let mut last_mouse_x_clone = last_mouse_x.clone();
            let mut last_mouse_y_clone = last_mouse_y.clone();
            let mut transform_x_clone = transform_x.clone();
            let mut transform_y_clone = transform_y.clone();
            let mut render_trigger_clone = render_trigger.clone();
            let mousemove_closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                if *is_dragging_clone.read() {
                    let last_x = *last_mouse_x_clone.read();
                    let last_y = *last_mouse_y_clone.read();
                    let dx = event.client_x() as f64 - last_x;
                    let dy = event.client_y() as f64 - last_y;
                    let cur_tx = *transform_x_clone.read();
                    let cur_ty = *transform_y_clone.read();
                    transform_x_clone.set(cur_tx + dx);
                    transform_y_clone.set(cur_ty + dy);
                    last_mouse_x_clone.set(event.client_x() as f64);
                    last_mouse_y_clone.set(event.client_y() as f64);
                    let trigger = *render_trigger_clone.read();
                    render_trigger_clone.set(trigger.wrapping_add(1));
                }
            }) as Box<dyn FnMut(_)>);
            canvas
                .add_event_listener_with_callback("mousemove", mousemove_closure.as_ref().unchecked_ref())
                .ok();
            mousemove_closure.forget();

            // Mouse up - stop dragging
            let mut is_dragging_clone = is_dragging.clone();
            let mouseup_closure = Closure::wrap(Box::new(move |_event: web_sys::MouseEvent| {
                is_dragging_clone.set(false);
            }) as Box<dyn FnMut(_)>);
            window
                .add_event_listener_with_callback("mouseup", mouseup_closure.as_ref().unchecked_ref())
                .ok();
            mouseup_closure.forget();

            // Mouse wheel - zoom
            let mut scale_clone = scale.clone();
            let mut transform_x_clone = transform_x.clone();
            let mut transform_y_clone = transform_y.clone();
            let mut render_trigger_clone = render_trigger.clone();
            let wheel_closure = Closure::wrap(Box::new(move |event: web_sys::WheelEvent| {
                event.prevent_default();

                let delta = -event.delta_y() / 500.0;
                let old_scale = *scale_clone.read();
                let new_scale = (old_scale * (1.0 + delta)).clamp(0.25, 4.0);

                // Zoom towards mouse position
                let rect = event.target()
                    .and_then(|t| t.dyn_into::<web_sys::Element>().ok())
                    .map(|e| e.get_bounding_client_rect());

                if let Some(rect) = rect {
                    let mouse_x = event.client_x() as f64 - rect.left();
                    let mouse_y = event.client_y() as f64 - rect.top();

                    let scale_change = new_scale / old_scale;
                    let cur_tx = *transform_x_clone.read();
                    let cur_ty = *transform_y_clone.read();
                    let new_tx = mouse_x - (mouse_x - cur_tx) * scale_change;
                    let new_ty = mouse_y - (mouse_y - cur_ty) * scale_change;

                    transform_x_clone.set(new_tx);
                    transform_y_clone.set(new_ty);
                }

                scale_clone.set(new_scale);
                let trigger = *render_trigger_clone.read();
                render_trigger_clone.set(trigger.wrapping_add(1));
            }) as Box<dyn FnMut(_)>);
            let mut wheel_options = web_sys::AddEventListenerOptions::new();
            wheel_options.set_passive(false);
            canvas
                .add_event_listener_with_callback_and_add_event_listener_options(
                    "wheel",
                    wheel_closure.as_ref().unchecked_ref(),
                    &wheel_options,
                )
                .ok();
            wheel_closure.forget();
        });

        // Clone source for use in effect
        let source_for_effect = source.clone();

        // Layout and render when source changes or transform changes
        use_effect(move || {
            if !*is_initialized.read() {
                return;
            }

            // Read transform values
            let tx = *transform_x.read();
            let ty = *transform_y.read();
            let s = *scale.read();
            let _trigger = *render_trigger.read();

            let source = source_for_effect.clone();

            wasm_bindgen_futures::spawn_local(async move {
                if let Some(ref mut manager) = *layout_manager.write() {
                    if let Ok(chart) = keyflow::Chart::parse(&source) {
                        if let Some(window) = web_sys::window() {
                            let dpr = window.device_pixel_ratio();

                            if let Some(document) = window.document() {
                                if let Some(canvas) = document.get_element_by_id("editor-chart-canvas") {
                                    if let Ok(html_canvas) = canvas.dyn_into::<web_sys::HtmlCanvasElement>() {
                                        let rect = html_canvas.get_bounding_client_rect();
                                        let css_width = rect.width();
                                        let css_height = rect.height();

                                        let buffer_width = (css_width * dpr) as u32;
                                        let buffer_height = (css_height * dpr) as u32;
                                        html_canvas.set_width(buffer_width);
                                        html_canvas.set_height(buffer_height);

                                        manager.layout_chart(&chart, css_width, css_height);

                                        if let Err(e) = manager.render_to_canvas_with_transform(
                                            &html_canvas,
                                            tx * dpr,
                                            ty * dpr,
                                            s * dpr,
                                        ).await {
                                            tracing::error!("Failed to render chart: {}", e);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            });
        });

        if let Some(error) = error_state.read().as_ref() {
            return rsx! {
                div {
                    class: "absolute inset-0 flex items-center justify-center text-yellow-400 text-sm",
                    "WebGPU not available: {error}"
                }
            };
        }

        if !*is_initialized.read() {
            return rsx! {
                div {
                    class: "absolute inset-0 flex items-center justify-center text-gray-400",
                    "Initializing WebGPU..."
                }
            };
        }

        rsx! {}
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        rsx! {
            div {
                class: "absolute inset-0 flex items-center justify-center text-gray-400",
                "Chart rendering requires WebGPU (browser only)"
            }
        }
    }
}
