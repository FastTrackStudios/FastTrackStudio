//! Chart canvas component for WASM WebGL2 rendering.
//!
//! Provides a `<canvas>` element that initializes a `ChartGraphics` WebGL2
//! renderer and exposes it via Dioxus context for chart panel components.

use std::cell::RefCell;
use std::rc::Rc;

use dioxus::prelude::*;
use keyflow_ui::{
    ChartGraphics, ChartLayoutManager, CHART_CURSOR_TICK, CHART_SOURCE, SESSION_CHART_SOURCE,
};
use session_ui::{
    ACTIVE_INDICES, ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL, SONG_CHARTS,
};
use wasm_bindgen::JsCast;
use web_sys::HtmlCanvasElement;

/// Unique DOM id for the chart canvas element.
const CANVAS_ID: &str = "keyflow-chart-canvas";

/// Chart canvas component — renders a `<canvas>` and initializes WebGL2 rendering.
///
/// On mount, obtains the `HtmlCanvasElement` from the DOM and creates a
/// `ChartGraphics` WebGL2 context. The `ChartLayoutManager` is created once
/// and reused across renders for layout caching.
#[component]
pub fn ChartCanvas() -> Element {
    // Chart graphics context (initialized on first render after mount)
    let mut graphics: Signal<Option<Rc<RefCell<ChartGraphics>>>> = use_signal(|| None);
    let layout_manager: Signal<Option<Rc<RefCell<ChartLayoutManager>>>> =
        use_signal(|| match ChartLayoutManager::new() {
            Ok(m) => Some(Rc::new(RefCell::new(m))),
            Err(e) => {
                tracing::error!("Failed to create ChartLayoutManager: {e}");
                None
            }
        });

    // Track layout generation to trigger re-renders
    let mut layout_gen = use_signal(|| 0u64);

    // Initialize graphics on mount
    use_effect(move || {
        // Get canvas from DOM
        let window = web_sys::window().expect("no window");
        let document = window.document().expect("no document");
        let canvas = document
            .get_element_by_id(CANVAS_ID)
            .and_then(|el| el.dyn_into::<HtmlCanvasElement>().ok());

        let Some(canvas) = canvas else {
            tracing::error!("Chart canvas element not found: #{CANVAS_ID}");
            return;
        };

        // Size canvas to its container (accounting for device pixel ratio)
        let dpr = window.device_pixel_ratio();
        let rect = canvas.get_bounding_client_rect();
        let width = (rect.width() * dpr) as u32;
        let height = (rect.height() * dpr) as u32;
        canvas.set_width(width.max(1));
        canvas.set_height(height.max(1));

        let gfx = ChartGraphics::new_web(&canvas, width.max(1), height.max(1));
        tracing::info!("ChartGraphics WebGL initialized: {width}x{height} (dpr={dpr})");

        graphics.set(Some(Rc::new(RefCell::new(gfx))));
    });

    // Sync SESSION_CHART_SOURCE when active song changes
    {
        use_effect(move || {
            let indices = ACTIVE_INDICES.read();
            let charts = SONG_CHARTS.read();

            if let Some(song_index) = indices.song_index {
                // Find chart for active song by index in the charts map
                if let Some((_guid, hydration)) = charts.iter().nth(song_index as usize) {
                    let text = &hydration.chart_text;
                    if !text.is_empty() {
                        let current = SESSION_CHART_SOURCE.peek();
                        if current.as_deref() != Some(text.as_str()) {
                            *SESSION_CHART_SOURCE.write() = Some(text.clone());
                        }
                    }
                }
            }
        });
    }

    // Layout + render effect: reparse and render when chart source changes
    {
        use_effect(move || {
            let _gen = layout_gen(); // subscribe to layout gen changes
            let source = SESSION_CHART_SOURCE
                .read()
                .clone()
                .unwrap_or_else(|| CHART_SOURCE.read().clone());

            // Subscribe to playback signals for cursor rendering
            let playback_musical = *ACTIVE_PLAYBACK_MUSICAL.read();
            let playback_is_playing = *ACTIVE_PLAYBACK_IS_PLAYING.read();
            let current_cursor_tick = *CHART_CURSOR_TICK.read();

            let Some(ref gfx_rc) = *graphics.read() else {
                return;
            };
            let Some(ref manager_rc) = *layout_manager.read() else {
                return;
            };

            let mut manager = manager_rc.borrow_mut();

            // Parse and layout if source changed
            if manager.needs_layout(&source, false) {
                if let Ok(chart) = keyflow::parse(&source) {
                    manager.layout_chart_with_mode(
                        &chart,
                        &source,
                        keyflow_ui::chart_renderer::A4_WIDTH,
                        false,
                    );
                    let next = *layout_gen.peek() + 1;
                    layout_gen.set(next);
                }
            }

            // Compute cursor tick from musical position during playback
            let cursor_tick = if playback_is_playing {
                if let Some(musical) = playback_musical {
                    let tick = manager.tick_for_musical_position(
                        musical.measure - 1,
                        musical.beat - 1,
                        musical.subdivision,
                    );
                    if let Some(tick) = tick {
                        if tick != current_cursor_tick {
                            *CHART_CURSOR_TICK.write() = tick;
                        }
                        tick
                    } else {
                        current_cursor_tick
                    }
                } else {
                    current_cursor_tick
                }
            } else {
                current_cursor_tick
            };

            // Render to WebGL canvas
            let mut gfx = gfx_rc.borrow_mut();
            let (w, h) = gfx.size();
            let width = w as f64;
            let height = h as f64;

            gfx.render_chart(|painter| {
                use anyrender::PaintScene;
                use kurbo::Affine;
                use peniko::Color;

                // Clear with dark background
                painter.fill(
                    peniko::Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgba8(24, 24, 27, 255), // zinc-900
                    None,
                    &kurbo::Rect::new(0.0, 0.0, width, height),
                );

                // Render chart content with cursor
                manager.render_to_scene(
                    painter,
                    width,
                    height,
                    Affine::IDENTITY,
                    Affine::IDENTITY,
                    Some(cursor_tick),
                    None,
                );
            });
        });
    }

    rsx! {
        canvas {
            id: CANVAS_ID,
            class: "w-full h-full block",
            // Prevent default touch actions for future gesture support
            style: "touch-action: none;",
        }
    }
}
