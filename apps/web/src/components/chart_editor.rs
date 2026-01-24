//! Chart Editor Component
//!
//! Interactive split-view editor with live chart preview.

use dioxus::dioxus_core::Task;
use dioxus::prelude::*;
use keyflow::highlighting::{HighlightKind, Highlighter};

// =============================================================================
// Example Charts
// =============================================================================

/// Empty chart template
const EMPTY_CHART: &str = r#"New Song
120bpm 4/4 #C

VS
C | G | Am | F
"#;

/// Thriller - Dirty Loops, Cory Wong cover arrangement
/// Demonstrates push/pull triplets and complex rhythm notation
const EXAMPLE_THRILLER: &str = r#"Thriller - Dirty Loops, Cory Wong
Transcribed By: Cody Wright
120bpm 4/4 #Ab
/push = triplet

COUNT 2

HITS
r8t >Ab9_8t r8t r8t r8t >F9_8t r2 | s1

IN
s1 x4

VS
>'F/C . Cm . 'F/C . Cm . 'F/C . Cm . 'F/C . Cm Cm9

CH
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A //// | 'Fm9  ////
>Cm/Eb / 'Eb /// | 'Eb / 'F/C / 'Cm // | 'F/A | r8t >Ab9_8t r8t r8t >'F9_8t r8t r4 >Fm/Ab_4 | s1

BR
>'_4F7 | . |  Abmaj9 //// | // r8t >Abmaj9_8t r8t >Bb_8t r8t >Cm7_8t | Cm7 | Ebmaj7/Bb | Am7b5 | Abmaj7 | G7sus4 | 'G7

VS
'F/C . Cm . 'F/C . Cm . 'F/C . Cm . 'F/C . Cm Cm9
"#;

/// Default chart content - start with an example
const DEFAULT_CHART: &str = EXAMPLE_THRILLER;

/// Preview mode - Snippet (content-sized) or Page (A4)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreviewMode {
    Snippet,
    Page,
}

/// Chart editor with live preview.
#[component]
pub fn ChartEditor() -> Element {
    // Source text state
    let mut source = use_signal(|| DEFAULT_CHART.to_string());
    // Preview mode state - default to Page (A4) mode
    let mut preview_mode = use_signal(|| PreviewMode::Page);

    let is_snippet = *preview_mode.read() == PreviewMode::Snippet;

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

                    div {
                        class: "flex items-center gap-2",

                        // Examples dropdown
                        select {
                            class: "text-xs text-muted-foreground bg-background px-3 py-1.5 rounded-md border border-border hover:border-primary/50 focus:outline-none focus:ring-1 focus:ring-primary transition-colors cursor-pointer",
                            onchange: move |evt| {
                                let value = evt.value();
                                match value.as_str() {
                                    "empty" => source.set(EMPTY_CHART.to_string()),
                                    "thriller" => source.set(EXAMPLE_THRILLER.to_string()),
                                    _ => {}
                                }
                            },
                            option { value: "", disabled: true, selected: true, "Examples" }
                            option { value: "empty", "New (Empty)" }
                            option { value: "thriller", "Thriller - Dirty Loops, Cory Wong" }
                        }

                        button {
                            class: "text-xs text-muted-foreground hover:text-foreground px-3 py-1.5 rounded-md hover:bg-accent transition-colors border border-border",
                            onclick: move |_| source.set(DEFAULT_CHART.to_string()),
                            "Reset"
                        }
                    }
                }

                // Code editor with syntax highlighting
                div {
                    class: "flex-1 overflow-hidden",

                    HighlightedEditor {
                        value: source(),
                        on_change: move |new_value: String| source.set(new_value),
                        placeholder: "Enter keyflow chart notation..."
                    }
                }
            }

            // Right side - Preview
            div {
                class: "flex flex-col bg-muted/30 overflow-hidden",

                // Preview header with mode toggle
                div {
                    class: "px-4 py-3 border-b border-border bg-card flex items-center justify-between shrink-0",

                    // Left side - title
                    div {
                        class: "flex items-center gap-2",
                        lucide_dioxus::Eye { class: "w-4 h-4 text-primary" }
                        h2 {
                            class: "text-sm font-semibold text-foreground",
                            "Live Preview"
                        }
                    }

                    // Right side - mode toggle
                    div {
                        class: "flex items-center gap-1 bg-muted rounded-lg p-1",

                        button {
                            class: if is_snippet {
                                "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium bg-background text-foreground shadow-sm"
                            } else {
                                "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium text-muted-foreground hover:text-foreground transition-colors"
                            },
                            onclick: move |_| preview_mode.set(PreviewMode::Snippet),
                            lucide_dioxus::Scissors { class: "w-3.5 h-3.5" }
                            "Snippet"
                        }

                        button {
                            class: if !is_snippet {
                                "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium bg-background text-foreground shadow-sm"
                            } else {
                                "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-xs font-medium text-muted-foreground hover:text-foreground transition-colors"
                            },
                            onclick: move |_| preview_mode.set(PreviewMode::Page),
                            lucide_dioxus::FileText { class: "w-3.5 h-3.5" }
                            "Page (A4)"
                        }
                    }
                }

                // Chart preview - canvas always fills panel, mode affects internal rendering
                div {
                    class: "flex-1 overflow-hidden",

                    // Pass signals directly so child effects can track them
                    DynamicChartRenderer {
                        source: source,
                        mode: preview_mode
                    }
                }
            }
        }
    }
}

/// Dynamic chart renderer that accepts signals for reactive updates.
/// The canvas always fills the container; mode affects the paper rendering inside.
#[component]
pub fn DynamicChartRenderer(
    source: Signal<String>,
    mode: Signal<PreviewMode>,
    canvas_id: Option<String>,
) -> Element {
    // Read source to trigger re-render on changes and for validation
    let source_value = source.read();
    let mode_value = *mode.read();

    // Debug: log when this component renders
    let first_line = source_value.lines().next().unwrap_or("(empty)");
    tracing::debug!(
        "[ChartRenderer] Rendering with mode={:?}, source_first_line='{}'",
        mode_value,
        first_line
    );

    // Parse the chart to validate it
    let parse_result = keyflow::Chart::parse(&source_value);

    // Use provided canvas_id or default
    let canvas_id = canvas_id.unwrap_or_else(|| "editor-chart-canvas".to_string());

    match parse_result {
        Ok(_chart) => {
            rsx! {
                div {
                    class: "w-full h-full relative",

                    // Canvas for WebGPU rendering - fills entire container
                    // Gray background shows around the white "paper"
                    canvas {
                        id: "{canvas_id}",
                        class: "w-full h-full cursor-grab active:cursor-grabbing",
                        style: "touch-action: none; background: #374151;",
                    }

                    // WebGPU rendering - pass signals for reactive tracking
                    DynamicChartCanvas { source: source, mode: mode, canvas_id: canvas_id.clone() }
                }
            }
        }
        Err(e) => {
            let error_msg = e.to_string();
            rsx! {
                div {
                    class: "w-full h-full flex items-center justify-center",
                    style: "background: #374151;",

                    div {
                        class: "text-center p-6 max-w-md bg-white rounded-lg shadow-xl",

                        lucide_dioxus::CircleAlert { class: "w-12 h-12 mx-auto mb-3 text-red-400" }

                        div {
                            class: "text-lg font-semibold text-red-600 mb-2",
                            "Parse Error"
                        }

                        div {
                            class: "text-sm text-red-500 font-mono whitespace-pre-wrap text-left bg-red-50 p-3 rounded-lg",
                            "{error_msg}"
                        }
                    }
                }
            }
        }
    }
}

/// Canvas component with WebGPU rendering for dynamic content.
/// Accepts signals directly to enable reactive effect tracking.
#[component]
fn DynamicChartCanvas(
    source: Signal<String>,
    mode: Signal<PreviewMode>,
    canvas_id: String,
) -> Element {
    #[cfg(target_arch = "wasm32")]
    {
        use crate::renderer::ChartLayoutManager;
        use wasm_bindgen::JsCast;
        use wasm_bindgen::prelude::*;

        // Create layout manager signal
        let mut layout_manager = use_signal(|| None::<ChartLayoutManager>);
        let mut error_state = use_signal(|| None::<String>);
        let mut is_initialized = use_signal(|| false);

        // Track the current render task so we can cancel it when a new one starts
        // Using RefCell instead of Signal to avoid triggering reactive updates when we store the task
        let current_render_task = use_hook(|| std::cell::RefCell::new(None::<Task>));

        // Trigger re-render for non-signal state changes (mouse, resize, etc.)
        let mut render_trigger = use_signal(|| 0_u32);

        // Transform state for pan/zoom
        let mut transform_x = use_signal(|| 20.0_f64);
        let mut transform_y = use_signal(|| 20.0_f64);
        let mut scale = use_signal(|| 1.0_f64);

        // Mouse interaction state
        let mut is_dragging = use_signal(|| false);
        let mut last_mouse_x = use_signal(|| 0.0_f64);
        let mut last_mouse_y = use_signal(|| 0.0_f64);

        // Initialize WebGPU on mount
        // Using Dioxus's spawn() instead of wasm_bindgen_futures::spawn_local()
        // for proper integration with Dioxus's runtime
        use_effect(move || {
            let mut render_trigger_clone = render_trigger.clone();
            spawn(async move {
                match ChartLayoutManager::new() {
                    Ok(manager) => {
                        layout_manager.set(Some(manager));
                        is_initialized.set(true);
                        tracing::info!("Editor chart layout manager initialized");

                        // Force initial render after a short delay to ensure canvas has proper size
                        // This helps with the initial weird scaling issue
                        #[cfg(target_arch = "wasm32")]
                        {
                            let promise = js_sys::Promise::new(&mut |resolve, _reject| {
                                if let Some(window) = web_sys::window() {
                                    let _ = window
                                        .set_timeout_with_callback_and_timeout_and_arguments_0(
                                            &resolve, 50, // Small delay for DOM to stabilize
                                        );
                                }
                            });
                            let _ = wasm_bindgen_futures::JsFuture::from(promise).await;
                        }

                        // Trigger initial render
                        let trigger = *render_trigger_clone.peek();
                        render_trigger_clone.set(trigger.wrapping_add(1));
                    }
                    Err(e) => {
                        error_state.set(Some(e));
                        tracing::error!("Failed to initialize editor chart layout manager");
                    }
                }
            });
        });

        // Setup mouse event listeners (only once)
        // Use a hook to track if we've already set up the listeners to avoid
        // duplicate registrations and stale signal references
        let events_setup = use_hook(|| std::cell::Cell::new(false));
        let canvas_id_for_events = canvas_id.clone();
        use_effect(move || {
            // Only set up events once - the closures capture signal clones that
            // would become stale if we set up multiple times
            if events_setup.get() {
                return;
            }

            let window = match web_sys::window() {
                Some(w) => w,
                None => return,
            };
            let document = match window.document() {
                Some(d) => d,
                None => return,
            };
            let canvas = match document.get_element_by_id(&canvas_id_for_events) {
                Some(c) => c,
                None => return,
            };

            // Mark as set up before adding listeners
            events_setup.set(true);

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
                .add_event_listener_with_callback(
                    "mousedown",
                    mousedown_closure.as_ref().unchecked_ref(),
                )
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
                .add_event_listener_with_callback(
                    "mousemove",
                    mousemove_closure.as_ref().unchecked_ref(),
                )
                .ok();
            mousemove_closure.forget();

            // Mouse up - stop dragging
            let mut is_dragging_clone = is_dragging.clone();
            let mouseup_closure = Closure::wrap(Box::new(move |_event: web_sys::MouseEvent| {
                is_dragging_clone.set(false);
            }) as Box<dyn FnMut(_)>);
            window
                .add_event_listener_with_callback(
                    "mouseup",
                    mouseup_closure.as_ref().unchecked_ref(),
                )
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
                let rect = event
                    .target()
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

            // Setup ResizeObserver to detect when canvas gets its proper size
            let mut render_trigger_clone = render_trigger.clone();
            let resize_callback = Closure::wrap(Box::new(
                move |_entries: js_sys::Array, _observer: web_sys::ResizeObserver| {
                    // Trigger a re-render when size changes
                    let trigger = *render_trigger_clone.read();
                    render_trigger_clone.set(trigger.wrapping_add(1));
                },
            )
                as Box<dyn FnMut(js_sys::Array, web_sys::ResizeObserver)>);

            if let Ok(observer) =
                web_sys::ResizeObserver::new(resize_callback.as_ref().unchecked_ref())
            {
                if let Some(canvas) = document.get_element_by_id(&canvas_id_for_events) {
                    observer.observe(&canvas);
                }
            }
            resize_callback.forget();
        });

        // Layout and render when source changes, mode changes, or transform changes
        // IMPORTANT: We MUST read ALL signals BEFORE any early returns to establish
        // reactive dependencies. If we return early without reading a signal,
        // it won't be tracked as a dependency and changes to it won't trigger re-runs.
        let canvas_id_for_render = canvas_id.clone();
        use_effect(move || {
            // Clone canvas_id for async block
            let canvas_id_inner = canvas_id_for_render.clone();

            // Read ALL reactive values FIRST - this establishes dependencies
            let initialized = *is_initialized.read();
            let tx = *transform_x.read();
            let ty = *transform_y.read();
            let s = *scale.read();
            let trigger = *render_trigger.read();
            let source_text = source.read().clone();
            let current_mode = *mode.read();
            let is_snippet = current_mode == PreviewMode::Snippet;

            // Debug: log what triggered the effect
            let first_line = source_text.lines().next().unwrap_or("(empty)");
            tracing::debug!(
                "[ChartCanvas] Render effect: initialized={}, trigger={}, mode={:?}, first_line='{}'",
                initialized,
                trigger,
                current_mode,
                first_line
            );

            // NOW check if we should skip rendering
            if !initialized {
                tracing::debug!("[ChartCanvas] Skipping render - not initialized yet");
                return;
            }

            // Cancel any previous render task to avoid borrow conflicts
            if let Some(previous_task) = current_render_task.borrow_mut().take() {
                previous_task.cancel();
            }

            // Spawn the render task and store it so we can cancel it if needed
            let task = spawn(async move {
                // Get mutable access to the layout manager
                let mut manager_guard = layout_manager.write();

                let Some(ref mut manager) = *manager_guard else {
                    tracing::debug!("[ChartCanvas] Skipping render - manager not initialized");
                    return;
                };

                let Ok(chart) = keyflow::Chart::parse(&source_text) else {
                    tracing::debug!("[ChartCanvas] Skipping render - parse failed");
                    return;
                };

                tracing::debug!("[ChartCanvas] Chart parsed, rendering to canvas");

                #[cfg(target_arch = "wasm32")]
                {
                    use wasm_bindgen::JsCast;

                    let Some(window) = web_sys::window() else {
                        return;
                    };
                    let dpr = window.device_pixel_ratio();

                    let Some(document) = window.document() else {
                        return;
                    };
                    let Some(canvas) = document.get_element_by_id(&canvas_id_inner) else {
                        return;
                    };
                    let Ok(html_canvas) = canvas.dyn_into::<web_sys::HtmlCanvasElement>() else {
                        return;
                    };

                    let rect = html_canvas.get_bounding_client_rect();
                    let css_width = rect.width();

                    let buffer_width = (css_width * dpr) as u32;
                    let buffer_height = (rect.height() * dpr) as u32;
                    html_canvas.set_width(buffer_width);
                    html_canvas.set_height(buffer_height);

                    // Use appropriate layout mode based on preview setting
                    manager.layout_chart_with_mode(&chart, css_width, is_snippet);

                    if let Err(e) = manager
                        .render_to_canvas_with_transform(&html_canvas, tx * dpr, ty * dpr, s * dpr)
                        .await
                    {
                        tracing::error!("Failed to render chart: {}", e);
                    }
                }
            });

            // Store the task so we can cancel it on the next effect run (non-reactive storage)
            *current_render_task.borrow_mut() = Some(task);
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

/// Syntax-highlighted code editor for keyflow notation.
///
/// Uses a layered approach with a transparent textarea for input and
/// a highlighted display layer behind it. Both layers must have
/// identical text styling for proper alignment.
#[component]
pub fn HighlightedEditor(
    value: String,
    on_change: EventHandler<String>,
    placeholder: &'static str,
    /// Optional unique ID for the textarea element. Defaults to "keyflow-editor-textarea".
    textarea_id: Option<String>,
) -> Element {
    // Track scroll position to sync layers
    let mut scroll_top = use_signal(|| 0.0_f64);
    let mut scroll_left = use_signal(|| 0.0_f64);

    // Unique ID for the textarea to query scroll position
    let textarea_id = textarea_id.unwrap_or_else(|| "keyflow-editor-textarea".to_string());

    // Common text styling - MUST match exactly between textarea and highlight layer
    // Using explicit line-height to ensure pixel-perfect alignment
    let text_style = "font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; font-size: 14px; line-height: 21px; white-space: pre; tab-size: 4;";

    rsx! {
        div {
            class: "relative w-full h-full",

            // Highlighted display layer (behind textarea)
            div {
                id: "keyflow-editor-highlight",
                class: "absolute inset-0 p-4 overflow-hidden pointer-events-none bg-background",
                style: "{text_style}",

                // Inner container with scroll offset
                div {
                    style: "transform: translate(-{scroll_left}px, -{scroll_top}px);",

                    HighlightedCode { source: value.clone() }
                }
            }

            // Transparent textarea for actual input (on top)
            textarea {
                id: "{textarea_id}",
                class: "absolute inset-0 w-full h-full p-4 resize-none focus:outline-none bg-transparent text-transparent caret-foreground z-10 overflow-auto",
                style: "{text_style}",
                value: "{value}",
                spellcheck: false,
                placeholder: "{placeholder}",
                oninput: {
                    let textarea_id = textarea_id.clone();
                    move |evt| {
                        on_change.call(evt.value());
                        // Update scroll position after input
                        #[cfg(target_arch = "wasm32")]
                        {
                            use wasm_bindgen::JsCast;
                            if let Some(window) = web_sys::window() {
                                if let Some(document) = window.document() {
                                    if let Some(elem) = document.get_element_by_id(&textarea_id) {
                                        if let Ok(html_elem) = elem.dyn_into::<web_sys::HtmlElement>() {
                                            scroll_top.set(html_elem.scroll_top() as f64);
                                            scroll_left.set(html_elem.scroll_left() as f64);
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                onscroll: {
                    let textarea_id = textarea_id.clone();
                    move |_evt| {
                        // Sync scroll with highlighted layer
                        #[cfg(target_arch = "wasm32")]
                        {
                            use wasm_bindgen::JsCast;
                            if let Some(window) = web_sys::window() {
                                if let Some(document) = window.document() {
                                    if let Some(elem) = document.get_element_by_id(&textarea_id) {
                                        if let Ok(html_elem) = elem.dyn_into::<web_sys::HtmlElement>() {
                                            scroll_top.set(html_elem.scroll_top() as f64);
                                            scroll_left.set(html_elem.scroll_left() as f64);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Renders highlighted keyflow source code.
///
/// Renders each line as plain text with inline spans for highlighting.
/// Uses newline characters to separate lines (matching textarea behavior).
#[component]
fn HighlightedCode(source: String) -> Element {
    // Split into lines but preserve the structure
    let lines: Vec<&str> = source.split('\n').collect();

    rsx! {
        // Use a single pre-like container to match textarea rendering
        div {
            class: "text-foreground",

            for (idx, line) in lines.iter().enumerate() {
                // Render each line
                HighlightedLine { line: line.to_string() }
                // Add newline between lines (except after last)
                if idx < lines.len() - 1 {
                    "\n"
                }
            }
        }
    }
}

/// Renders a single highlighted line of keyflow notation.
#[component]
fn HighlightedLine(line: String) -> Element {
    let spans = Highlighter::highlight_line(&line);

    // Empty line - render empty span to preserve height
    if line.is_empty() {
        return rsx! { span { "" } };
    }

    if spans.is_empty() {
        // No highlighting - render as plain text
        return rsx! { span { "{line}" } };
    }

    // Build the highlighted segments
    let mut segments: Vec<Element> = Vec::new();
    let mut last_end = 0;

    for span in &spans {
        let start = span.span.start;
        let end = span.span.start + span.span.len;

        // Add any unhighlighted text before this span
        if start > last_end {
            let text = &line[last_end..start];
            segments.push(rsx! { span { "{text}" } });
        }

        // Add the highlighted span
        let text = &line[start..end.min(line.len())];
        let class = highlight_class(span.kind);
        segments.push(rsx! {
            span { class: "{class}", "{text}" }
        });

        last_end = end;
    }

    // Add any remaining text
    if last_end < line.len() {
        let text = &line[last_end..];
        segments.push(rsx! { span { "{text}" } });
    }

    // Return inline spans (no block element wrapper)
    rsx! {
        for segment in segments {
            {segment}
        }
    }
}

/// Map highlight kinds to Tailwind CSS classes.
///
/// Design decisions:
/// - Root + Accidental use the same color (Ab = same color for A and b)
/// - Quality + Extension use the same color (maj9 = same color for maj and 9)
/// - Barlines (MeasureSeparator) are gray/muted
/// - Unknown/unparsed text is muted, not red (avoids visual noise)
fn highlight_class(kind: HighlightKind) -> &'static str {
    match kind {
        // Chord components - Root and Accidental same color
        HighlightKind::Root => "text-sky-400 font-semibold",
        HighlightKind::Accidental => "text-sky-400", // Same as Root
        HighlightKind::ScaleDegree => "text-purple-400 font-semibold",
        HighlightKind::RomanNumeral => "text-purple-400 font-semibold",

        // Quality and Extension same color
        HighlightKind::Quality => "text-amber-400",
        HighlightKind::Extension => "text-amber-400", // Same as Quality
        HighlightKind::Modifier => "text-yellow-300",

        // Bass note - slightly different shade
        HighlightKind::Bass => "text-sky-300",
        HighlightKind::BassSlash => "text-gray-500",

        // Rhythm notation
        HighlightKind::Duration => "text-violet-400",
        HighlightKind::SlashRhythm => "text-gray-400",
        HighlightKind::Push => "text-rose-400",
        HighlightKind::Pull => "text-rose-400",
        HighlightKind::Triplet => "text-rose-300",
        HighlightKind::Dot => "text-violet-400",

        // Structure - Barlines are gray
        HighlightKind::MeasureSeparator => "text-gray-500",
        HighlightKind::Repeat => "text-indigo-400 font-bold",
        HighlightKind::Section => "text-emerald-400 font-semibold",
        HighlightKind::SectionBracket => "text-emerald-400",
        HighlightKind::MeasureCount => "text-emerald-300",
        HighlightKind::SectionComment => "text-emerald-200 italic",

        // Special
        HighlightKind::Rest => "text-gray-400 italic",
        HighlightKind::Space => "text-gray-500",
        HighlightKind::MemoryRecall => "text-gray-400",
        HighlightKind::Dynamic => "text-red-400 italic",

        // Metadata
        HighlightKind::Title => "text-green-400 font-semibold",
        HighlightKind::Artist => "text-green-300",
        HighlightKind::Tempo => "text-orange-400",
        HighlightKind::TempoArrow => "text-orange-300",
        HighlightKind::Key => "text-violet-400",
        HighlightKind::TimeSignature => "text-cyan-400",

        // Comments - muted gray
        HighlightKind::Comment => "text-gray-500 italic",
        HighlightKind::CommentMarker => "text-gray-500",

        // Melody and tracks
        HighlightKind::MelodyBlock => "text-teal-400",
        HighlightKind::TrackMarker => "text-fuchsia-400 font-semibold",

        // Commands and cues - muted (these are config lines like /push = triplet)
        HighlightKind::Command => "text-gray-500",
        HighlightKind::TextCue => "text-gray-400 italic",

        // Whitespace and unknown - muted, not distracting
        HighlightKind::Whitespace => "",
        HighlightKind::Unknown => "text-gray-500", // Muted instead of red
    }
}
