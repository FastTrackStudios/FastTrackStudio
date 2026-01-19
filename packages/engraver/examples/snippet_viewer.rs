//! Snippet Viewer Example
//!
//! Visual rendering mode for snippet tests using ChartLayoutEngine.
//! Uses the same rendering approach as fts-native and reaper-extension.
//!
//! # Usage
//!
//! ```bash
//! cargo run -p engraver --example snippet_viewer --features "example keyflow-import"
//! ```
//!
//! # Controls
//!
//! - Left mouse drag: Pan
//! - Scroll: Zoom (centered on cursor)
//! - R key: Reset view
//! - Escape: Close window

use std::sync::Arc;

use kurbo::{Affine, Point, Rect};
use vello::peniko::Color;
use vello::Scene;
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use engraver::fonts::SMuFLFont;
use engraver::layout::chart::{ChartLayoutEngine, ChartLayoutResult, LayoutMode};
use engraver::renderer::{SceneRenderBuilder, VelloRenderContext};
use engraver::style::MStyle;
use keyflow::Chart;

const WINDOW_WIDTH: u32 = 1000;
const WINDOW_HEIGHT: u32 = 700;

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

// Font paths relative to workspace root
const SMUFL_FONT_PATH: &str = "packages/charts/resources/fonts/musescore/fonts/bravura/Bravura.otf";
const SMUFL_METADATA_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/bravura/bravura_metadata.json";
const TEXT_FONT_PATH: &str = "packages/charts/resources/fonts/musescore/fonts/FreeSans.ttf";
const MUSEJAZZ_FONT_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/musejazz/MuseJazzText.otf";

/// The keyflow source for the push/pull triplet snippet.
const KEYFLOW_SOURCE: &str = r#"Push Pull Triplets - Test
120bpm 4/4 #Ab
/push = triplet

COUNT 2

IN
r8t Ab9_8t r8t r8t r8t F9_8t r2 | s1

VS
'F/C . | Cm . | 'F/C . | Cm . | 'F/C . | Cm . | 'F/C . | Cm Cm9

CH
Cm/Eb / Eb ///
"#;

fn main() {
    env_logger::init();

    let event_loop = EventLoop::new().expect("Failed to create event loop");
    let mut app = App::default();
    event_loop.run_app(&mut app).expect("Event loop failed");
}

#[derive(Default)]
struct App {
    state: Option<AppState>,
}

struct AppState {
    // GPU infrastructure (reusable)
    render_ctx: VelloRenderContext,
    // View transform (combined pan/zoom)
    transform: Affine,
    // Mouse state for drag-to-pan
    mouse_down: bool,
    prior_position: Option<Point>,
    // Chart layout result
    layout_result: ChartLayoutResult,
    // Font data
    smufl_font: &'static SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }

        let window_attrs = Window::default_attributes()
            .with_title("Snippet Viewer - Push/Pull Triplets")
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(
            event_loop
                .create_window(window_attrs)
                .expect("Failed to create window"),
        );

        // Create reusable GPU render context
        let render_ctx = VelloRenderContext::new(window);

        // Load fonts
        let font_data: &'static [u8] = Box::leak(
            std::fs::read(SMUFL_FONT_PATH)
                .expect("Failed to load Bravura font")
                .into_boxed_slice(),
        );

        let metadata_file =
            std::fs::File::open(SMUFL_METADATA_PATH).expect("Failed to load Bravura metadata");

        let smufl_font: &'static SMuFLFont<'static> = Box::leak(Box::new(
            SMuFLFont::from_reader(font_data, metadata_file).expect("Failed to load SMuFL font"),
        ));

        let text_font_data = Arc::new(
            std::fs::read(TEXT_FONT_PATH).expect("Failed to load text font"),
        );

        let musejazz_font_data = Arc::new(
            std::fs::read(MUSEJAZZ_FONT_PATH).expect("Failed to load MuseJazz font"),
        );

        // Parse the keyflow chart
        let chart = Chart::parse(KEYFLOW_SOURCE).expect("Failed to parse keyflow source");

        // Create layout engine and layout the chart
        let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
        let layout_engine = ChartLayoutEngine::new(
            style,
            text_font_data.clone(),
            musejazz_font_data.clone(),
        );

        // Use paginated mode with Letter size
        let layout_mode = LayoutMode::Paginated {
            page_width: 612.0,  // 8.5" in points
            page_height: 792.0, // 11" in points
        };

        let layout_result = layout_engine.layout_chart(&chart, &layout_mode);

        // Initial transform: translate to show content, then apply DPI scale
        let transform = Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);

        self.state = Some(AppState {
            render_ctx,
            transform,
            mouse_down: false,
            prior_position: None,
            layout_result,
            smufl_font,
            text_font_data,
            musejazz_font_data,
        });

        if let Some(state) = &self.state {
            state.render_ctx.request_redraw();
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(state) = &mut self.state else {
            return;
        };

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(code) = event.physical_key {
                    match code {
                        KeyCode::Escape => event_loop.exit(),
                        KeyCode::KeyR if event.state == ElementState::Pressed => {
                            // Reset view
                            state.transform =
                                Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);
                            state.render_ctx.request_redraw();
                        }
                        _ => {}
                    }
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                const BASE: f64 = 1.05; // 5% per scroll increment
                const PIXELS_PER_LINE: f64 = 20.0;

                if let Some(prior_position) = state.prior_position {
                    let exponent = match delta {
                        MouseScrollDelta::LineDelta(_, y) => y as f64,
                        MouseScrollDelta::PixelDelta(delta) => delta.y / PIXELS_PER_LINE,
                    };
                    state.transform = state
                        .transform
                        .then_scale_about(BASE.powf(exponent), prior_position);
                    state.render_ctx.request_redraw();
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                let pos = Point::new(position.x, position.y);
                // Drag to pan when mouse is held down
                if state.mouse_down {
                    if let Some(prior) = state.prior_position {
                        let delta = pos - prior;
                        state.transform = state.transform.then_translate(delta);
                        state.render_ctx.request_redraw();
                    }
                }
                state.prior_position = Some(pos);
            }

            WindowEvent::MouseInput {
                state: button_state,
                button,
                ..
            } => {
                // Left mouse button for panning
                if button == MouseButton::Left {
                    state.mouse_down = button_state == ElementState::Pressed;
                }
            }

            WindowEvent::Resized(new_size) => {
                state.render_ctx.resize(new_size.width, new_size.height);
            }

            WindowEvent::RedrawRequested => {
                let (width, height) = state.render_ctx.viewport_size();

                // Build Vello scene
                let mut vello_scene = Scene::new();

                // Dark gray canvas background (page view style)
                vello_scene.fill(
                    vello::peniko::Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(64, 64, 64),
                    None,
                    &Rect::new(0.0, 0.0, width as f64, height as f64),
                );

                // Create scene renderer with fonts
                let mut renderer = SceneRenderBuilder::new()
                    .spatium(5.0)
                    .build()
                    .with_font(state.smufl_font)
                    .with_text_font_arc(state.text_font_data.clone())
                    .with_named_font_arc("MuseJazzText", state.musejazz_font_data.clone())
                    .with_named_font_arc("MuseJazz", state.musejazz_font_data.clone());

                // Apply view transform to scene
                let mut transformed_scene = state.layout_result.scene.clone();
                transformed_scene.transform = state.transform;

                // Render scene nodes to Vello scene
                renderer.render(&mut vello_scene, &transformed_scene);

                // Render to surface
                state.render_ctx.render(&vello_scene);
            }

            _ => {}
        }
    }
}
