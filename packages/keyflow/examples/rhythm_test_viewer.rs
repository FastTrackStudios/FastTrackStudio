//! Rhythm Test Viewer
//!
//! Visual viewer for rhythm notation test patterns.
//! Press 1-6 to switch between patterns.
//!
//! # Usage
//!
//! ```bash
//! cargo run -p keyflow --example rhythm_test_viewer --features engraver-example
//! ```
//!
//! # Controls
//!
//! - 1-6: Switch between test patterns
//! - Drag: Pan
//! - Scroll: Zoom
//! - R: Reset view
//! - Escape: Close

use std::sync::Arc;

use kurbo::{Affine, Point, Rect};
use vello::Scene;
use vello::peniko::Color;
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use keyflow::engraver::fonts::SMuFLFont;
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult, LayoutMode};
use keyflow::engraver::renderer::{SceneRenderBuilder, VelloRenderContext};
use keyflow::engraver::style::MStyle;
use keyflow::Chart;

const WINDOW_WIDTH: u32 = 1400;
const WINDOW_HEIGHT: u32 = 400;

const SCREEN_DPI: f64 = 96.0;
const POINTS_PER_INCH: f64 = 72.0;
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

// Font paths
const SMUFL_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf";
const SMUFL_METADATA_PATH: &str = "libs/reference/sheet-music/musescore/fonts/bravura/bravura_metadata.json";
const TEXT_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/FreeSans.ttf";
const MUSEJAZZ_FONT_PATH: &str = "libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf";

// =============================================================================
// Test Patterns - Loaded from the patterns library
// =============================================================================

use keyflow::patterns::rhythm;

/// Get patterns from the library for the viewer
fn get_patterns() -> Vec<(&'static str, &'static str)> {
    rhythm::ALL
        .iter()
        .map(|p| (p.title, p.source))
        .collect()
}

fn main() {
    env_logger::init();

    // Load patterns from the library
    let patterns = get_patterns();

    println!("=== Rhythm Test Viewer ===");
    println!();
    println!("Press 1-{} to switch patterns:", patterns.len());
    for (i, (name, _)) in patterns.iter().enumerate() {
        println!("  {}: {}", i + 1, name);
    }
    println!();
    println!("Controls: Drag=Pan, Scroll=Zoom, R=Reset, Escape=Close");
    println!();

    // Load fonts
    let font_data: &'static [u8] = Box::leak(
        std::fs::read(SMUFL_FONT_PATH)
            .expect("Failed to load Bravura font")
            .into_boxed_slice(),
    );
    let metadata_file = std::fs::File::open(SMUFL_METADATA_PATH).expect("Failed to load metadata");
    let smufl_font: &'static SMuFLFont<'static> = Box::leak(Box::new(
        SMuFLFont::from_reader(font_data, metadata_file).expect("Failed to load SMuFL font"),
    ));
    let text_font_data = Arc::new(std::fs::read(TEXT_FONT_PATH).expect("Failed to load text font"));
    let musejazz_font_data = Arc::new(std::fs::read(MUSEJAZZ_FONT_PATH).expect("Failed to load MuseJazz font"));

    // Create layout engine
    let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
    let mut config = ChartLayoutConfig::default();
    config.use_stems = true;
    config.hide_repeated_chords = false;

    let layout_engine = ChartLayoutEngine::with_config(
        config,
        style,
        text_font_data.clone(),
        musejazz_font_data.clone(),
    );

    // Initial layout
    let (name, source) = patterns[0];
    let layout = layout_pattern(&layout_engine, source);
    print_beat_positions(name, &layout);

    // Show window
    let event_loop = EventLoop::new().expect("Failed to create event loop");
    let mut app = App {
        state: None,
        current_pattern: 0,
        patterns,
        layout,
        layout_engine,
        smufl_font,
        text_font_data,
        musejazz_font_data,
    };
    event_loop.run_app(&mut app).expect("Event loop failed");
}

fn layout_pattern(engine: &ChartLayoutEngine, source: &str) -> ChartLayoutResult {
    let chart = Chart::parse(source).expect("Failed to parse chart");
    engine.layout_chart(&chart, &LayoutMode::snippet(1000.0))
}

fn print_beat_positions(name: &str, layout: &ChartLayoutResult) {
    println!("\n=== {} ===", name);
    println!("Beat positions:");
    for bp in &layout.beat_positions {
        let beat_num = bp.tick / 480 + 1;
        let beat_fraction = (bp.tick % 480) as f64 / 480.0 * 1000.0;
        println!(
            "  m{} pos={}.{}.{:.0} tick={} dur={} x={:.1}",
            bp.measure + 1,
            bp.measure + 1,
            beat_num,
            beat_fraction,
            bp.tick,
            bp.duration_ticks,
            bp.x
        );
    }
    println!();
}

struct App {
    state: Option<AppState>,
    current_pattern: usize,
    patterns: Vec<(&'static str, &'static str)>,
    layout: ChartLayoutResult,
    layout_engine: ChartLayoutEngine,
    smufl_font: &'static SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
}

struct AppState {
    render_ctx: VelloRenderContext,
    transform: Affine,
    mouse_down: bool,
    prior_position: Option<Point>,
}

impl App {
    fn switch_pattern(&mut self, index: usize) {
        if index >= self.patterns.len() {
            return;
        }
        self.current_pattern = index;
        let (name, source) = self.patterns[index];
        self.layout = layout_pattern(&self.layout_engine, source);
        print_beat_positions(name, &self.layout);

        // Reset transform and redraw
        if let Some(state) = &mut self.state {
            state.transform = Affine::translate((50.0, 100.0)) * Affine::scale(DPI_SCALE);
            state.render_ctx.request_redraw();
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }

        let (name, _) = self.patterns[self.current_pattern];
        let title = format!("Rhythm Test: {} (Press 1-{})", name, self.patterns.len());

        let window_attrs = Window::default_attributes()
            .with_title(&title)
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(event_loop.create_window(window_attrs).expect("Failed to create window"));
        let render_ctx = VelloRenderContext::new(window);
        let transform = Affine::translate((50.0, 100.0)) * Affine::scale(DPI_SCALE);

        self.state = Some(AppState {
            render_ctx,
            transform,
            mouse_down: false,
            prior_position: None,
        });

        if let Some(state) = &self.state {
            state.render_ctx.request_redraw();
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(state) = &mut self.state else { return };

        match event {
            WindowEvent::CloseRequested => event_loop.exit(),

            WindowEvent::KeyboardInput { event, .. } => {
                if event.state == ElementState::Pressed {
                    if let PhysicalKey::Code(code) = event.physical_key {
                        match code {
                            KeyCode::Escape => event_loop.exit(),
                            KeyCode::KeyR => {
                                state.transform = Affine::translate((50.0, 100.0)) * Affine::scale(DPI_SCALE);
                                state.render_ctx.request_redraw();
                            }
                            // Pattern switching with number keys
                            KeyCode::Digit1 | KeyCode::Numpad1 => self.switch_pattern(0),
                            KeyCode::Digit2 | KeyCode::Numpad2 => self.switch_pattern(1),
                            KeyCode::Digit3 | KeyCode::Numpad3 => self.switch_pattern(2),
                            KeyCode::Digit4 | KeyCode::Numpad4 => self.switch_pattern(3),
                            KeyCode::Digit5 | KeyCode::Numpad5 => self.switch_pattern(4),
                            KeyCode::Digit6 | KeyCode::Numpad6 => self.switch_pattern(5),
                            _ => {}
                        }
                    }
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                const BASE: f64 = 1.05;
                const PIXELS_PER_LINE: f64 = 20.0;
                if let Some(prior_position) = state.prior_position {
                    let exponent = match delta {
                        MouseScrollDelta::LineDelta(_, y) => y as f64,
                        MouseScrollDelta::PixelDelta(delta) => delta.y / PIXELS_PER_LINE,
                    };
                    state.transform = state.transform.then_scale_about(BASE.powf(exponent), prior_position);
                    state.render_ctx.request_redraw();
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                let pos = Point::new(position.x, position.y);
                if state.mouse_down {
                    if let Some(prior) = state.prior_position {
                        state.transform = state.transform.then_translate(pos - prior);
                        state.render_ctx.request_redraw();
                    }
                }
                state.prior_position = Some(pos);
            }

            WindowEvent::MouseInput { state: button_state, button, .. } => {
                if button == MouseButton::Left {
                    state.mouse_down = button_state == ElementState::Pressed;
                }
            }

            WindowEvent::Resized(new_size) => {
                state.render_ctx.resize(new_size.width, new_size.height);
            }

            WindowEvent::RedrawRequested => {
                let (width, height) = state.render_ctx.viewport_size();
                let mut vello_scene = Scene::new();

                // Dark background
                vello_scene.fill(
                    vello::peniko::Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(48, 48, 48),
                    None,
                    &Rect::new(0.0, 0.0, width as f64, height as f64),
                );

                // Render chart
                let mut renderer = SceneRenderBuilder::new()
                    .spatium(5.0)
                    .build()
                    .with_font(self.smufl_font)
                    .with_text_font_arc(self.text_font_data.clone())
                    .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                    .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone());

                let mut scene = self.layout.scene.clone();
                scene.transform = state.transform;
                renderer.render(&mut vello_scene, &scene);

                state.render_ctx.render(&vello_scene);
            }

            _ => {}
        }
    }
}
