//! Waveform display — renders audio sample data as a centered waveform
//! or a scrolling peak level history.
//!
//! `PeakWaveform` uses a Vello scene overlay for GPU-accelerated rendering,
//! producing zero DOM nodes for the actual bars. The component emits a single
//! placeholder `<div>` for layout and positions the overlay on top of it.

use std::cell::RefCell;
use std::rc::Rc;

use crate::theme::use_theme;
use nih_plug_dioxus::prelude::*;

/// Centered bipolar waveform display.
///
/// Each entry in `samples` renders as one column, centered around the
/// horizontal midline. Values range from -1.0 to 1.0.
#[component]
pub fn WaveformDisplay(
    /// Normalized sample peaks (-1.0 to 1.0). Each entry renders as one column.
    #[props(default)]
    samples: Vec<f64>,
    /// Width in pixels.
    #[props(default = 200)]
    width: u32,
    /// Height in pixels.
    #[props(default = 64)]
    height: u32,
    /// Bar color.
    #[props(default)]
    color: Option<String>,
) -> Element {
    let t = use_theme();
    let t = *t.read();
    let color = color.as_deref().unwrap_or(t.accent);

    let w = width;
    let h = height;
    let hf = h as f64;
    let mid = hf / 2.0;
    let count = samples.len().max(1);
    let bar_w = (w as f64 / count as f64).max(1.0);

    rsx! {
        div {
            style: format!(
                "position:relative; overflow:hidden; border-radius:4px; \
                 background:rgba(34,34,64,0.3); width:{w}px; height:{h}px;"
            ),

            // Center line
            div {
                style: format!(
                    "position:absolute; left:0; top:50%; width:100%; height:1px; \
                     background:rgba(136,136,136,0.2);"
                ),
            }

            // Waveform bars
            for (i, sample) in samples.iter().enumerate() {
                {
                    let amp = sample.abs().clamp(0.0, 1.0);
                    let bar_h = (amp * mid).max(1.0);
                    let bar_top = mid - bar_h;
                    let bar_full_h = bar_h * 2.0;
                    let left = i as f64 * bar_w;
                    rsx! {
                        div {
                            style: format!(
                                "position:absolute; left:{left:.1}px; top:{bar_top:.1}px; \
                                 width:{bar_w:.1}px; height:{bar_full_h:.1}px; \
                                 border-radius:1px; background:{color}; opacity:0.8;"
                            ),
                        }
                    }
                }
            }
        }
    }
}

// ── Reusable VelloCanvas component ──────────────────────────────────────

use nih_plug_dioxus::prelude::vello::kurbo::{Affine, Rect};
use nih_plug_dioxus::prelude::vello::peniko::{Color, Fill};

/// Trait for painting into a Vello scene. Implement this for your custom
/// GPU-rendered content.
pub trait CanvasPainter: 'static {
    /// Paint content into the scene. Coordinates are in CSS pixels,
    /// origin (0,0) at the top-left of the canvas element.
    ///
    /// `transform` maps element-local CSS coords to window physical coords —
    /// pass it to all `scene.fill()` / `scene.stroke()` calls.
    fn paint(
        &self,
        scene: &mut nih_plug_dioxus::prelude::vello::Scene,
        transform: Affine,
        width: f64,
        height: f64,
    );
}

/// Internal overlay adapter: wraps a shared `CanvasPainter` as a `SceneOverlay`.
struct CanvasOverlay {
    painter: Rc<RefCell<dyn CanvasPainter>>,
}

impl SceneOverlay for CanvasOverlay {
    fn paint(
        &mut self,
        scene: &mut nih_plug_dioxus::prelude::vello::Scene,
        transform: Affine,
        width: u32,
        height: u32,
        _scale: f64,
    ) {
        self.painter
            .borrow()
            .paint(scene, transform, width as f64, height as f64);
    }
}

/// Props for [`VelloCanvas`].
#[derive(Props, Clone)]
pub struct VelloCanvasProps {
    /// Shared painter (type-erased).
    painter: Rc<RefCell<dyn CanvasPainter>>,
    /// Width in CSS pixels (ignored when `fill` is true).
    #[props(default = 400.0)]
    width: f32,
    /// Height in CSS pixels (ignored when `fill` is true).
    #[props(default = 200.0)]
    height: f32,
    /// When true, the canvas fills its parent container (flex:1 + 100% size)
    /// and reads its actual dimensions from the bounding rect.
    #[props(default = false)]
    fill: bool,
    /// Optional inline style to merge onto the placeholder div.
    #[props(default)]
    style: Option<String>,
}

// Rc<RefCell<dyn CanvasPainter>> has no PartialEq, so always re-render.
impl PartialEq for VelloCanvasProps {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

/// A reusable Dioxus component that renders custom Vello content, automatically
/// positioned to match its DOM element's layout.
///
/// Renders a single placeholder `<div>` for Taffy layout. A Vello scene overlay
/// is registered and repositioned each render cycle to match the element's
/// bounding rect.
#[allow(non_snake_case)]
pub fn VelloCanvas(props: VelloCanvasProps) -> Element {
    let VelloCanvasProps {
        painter,
        width,
        height,
        fill,
        style,
    } = props;

    let painter_clone = painter.clone();
    let overlay = use_scene_overlay(move || CanvasOverlay {
        painter: painter_clone,
    });

    // Store MountedData so we can re-query position on every render
    let mut mounted: Signal<Option<Rc<MountedData>>> = use_signal(|| None);

    // Re-query the element rect every render and update the overlay position.
    let w = width;
    let h = height;
    {
        let overlay = overlay.clone();
        use_effect(move || {
            let overlay = overlay.clone();
            spawn(async move {
                if let Some(ref el) = *mounted.read() {
                    if let Ok(rect) = el.get_client_rect().await {
                        let origin = rect.origin;
                        let size = rect.size;
                        if fill {
                            // Fill mode: use actual rendered size from bounding rect
                            overlay.set_rect(origin.x, origin.y, size.width, size.height);
                        } else {
                            // Fixed size mode: use props for dimensions
                            overlay.set_rect(origin.x, origin.y, w as f64, h as f64);
                        }
                    }
                }
            });
        });
    }

    let extra_style = style.as_deref().unwrap_or("");

    // In fill mode, the outer style (passed via `style` prop) controls sizing.
    // The div itself has no intrinsic size — it relies on the parent layout.
    // In fixed mode, we set explicit width/height.
    let size_style = if fill {
        String::new()
    } else {
        format!("width:{width}px; height:{height}px;")
    };

    rsx! {
        div {
            style: format!("{size_style} {extra_style}"),
            onmounted: move |event: MountedEvent| {
                mounted.set(Some(event.data()));
            },
        }
    }
}

// ── Vello-rendered PeakWaveform ─────────────────────────────────────────

/// Painter for the peak waveform visualization.
pub struct PeakWaveformPainter {
    levels: Vec<f32>,
    gr_levels: Vec<f32>,
}

impl PeakWaveformPainter {
    pub fn new() -> Self {
        Self {
            levels: Vec::new(),
            gr_levels: Vec::new(),
        }
    }

    pub fn update(&mut self, levels: &[f32], gr_levels: &[f32]) {
        self.levels.clear();
        self.levels.extend_from_slice(levels);
        self.gr_levels.clear();
        self.gr_levels.extend_from_slice(gr_levels);
    }
}

impl CanvasPainter for PeakWaveformPainter {
    fn paint(
        &self,
        scene: &mut nih_plug_dioxus::prelude::vello::Scene,
        transform: Affine,
        w: f64,
        h: f64,
    ) {
        // Background
        scene.fill(
            Fill::NonZero,
            transform,
            &Color::from_rgba8(10, 10, 20, 255),
            None,
            &Rect::new(0.0, 0.0, w, h),
        );

        let num_bars = self.levels.len().max(1);
        let bar_w = w / num_bars as f64;

        // Level bars (from bottom) — accent dim blue
        let level_color = Color::from_rgba8(100, 140, 200, 160);
        for (i, &level) in self.levels.iter().enumerate() {
            let bar_h = (level.clamp(0.0, 1.0) as f64 * h).max(0.0);
            if bar_h < 0.5 {
                continue;
            }
            let x = i as f64 * bar_w;
            scene.fill(
                Fill::NonZero,
                transform,
                &level_color,
                None,
                &Rect::new(x, h - bar_h, x + bar_w, h),
            );
        }

        // GR overlay bars (from top) — red tint
        let gr_color = Color::from_rgba8(248, 113, 113, 77);
        for (i, &gr) in self.gr_levels.iter().enumerate() {
            let gr_h = (gr.clamp(0.0, 1.0) as f64 * h).max(0.0);
            if gr_h < 0.5 {
                continue;
            }
            let x = i as f64 * bar_w;
            scene.fill(
                Fill::NonZero,
                transform,
                &gr_color,
                None,
                &Rect::new(x, 0.0, x + bar_w, gr_h),
            );
        }
    }
}

/// Scrolling peak level waveform with optional gain reduction overlay.
///
/// GPU-rendered via Vello scene overlay — produces only 1 DOM node (the
/// placeholder div) instead of hundreds of positioned divs.
///
/// Used by dynamics plugins (compressor, gate, rider) to show
/// amplitude history over time with GR envelope overlay.
#[component]
pub fn PeakWaveform(
    /// Peak levels (0.0–1.0, newest at end).
    levels: Vec<f32>,
    /// Gain reduction levels (0.0–1.0, same length as `levels`).
    #[props(default = Vec::new())]
    gr_levels: Vec<f32>,
    /// Width in CSS pixels (ignored when `fill` is true).
    #[props(default = 400.0)]
    width: f32,
    /// Height in CSS pixels (ignored when `fill` is true).
    #[props(default = 80.0)]
    height: f32,
    /// Fill parent container instead of using fixed dimensions.
    #[props(default = false)]
    fill: bool,
    /// Optional inline style to merge onto the outer element.
    #[props(default)]
    style: Option<String>,
) -> Element {
    let t = use_theme();
    let t = *t.read();

    // Create shared painter (persists across renders)
    let painter: &Rc<RefCell<PeakWaveformPainter>> =
        &use_hook(|| Rc::new(RefCell::new(PeakWaveformPainter::new())));

    // Update data each render
    painter.borrow_mut().update(&levels, &gr_levels);

    // Type-erase to dyn CanvasPainter for VelloCanvas
    let dyn_painter: Rc<RefCell<dyn CanvasPainter>> = painter.clone();

    let outer_style = style.as_deref().unwrap_or("");
    rsx! {
        VelloCanvas {
            painter: dyn_painter,
            width: width,
            height: height,
            fill: fill,
            style: format!(
                "border-radius:4px; overflow:hidden; border:1px solid {border}; {outer_style}",
                border = t.border,
            ),
        }
    }
}
