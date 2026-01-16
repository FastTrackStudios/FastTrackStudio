//! Layout Demo Example
//!
//! Demonstrates rhythmic slash notation using the high-level MeasureBuilder API:
//! - Rhythmic slash notation with various note durations
//! - Auto-stemless quarters (2+ consecutive = stemless)
//! - Complex rhythms (32nds, syncopation, dotted patterns)
//! - Beat-based beam grouping
//!
//! Uses the Scene Graph → VelloSceneRenderer → WGPU pipeline.
//!
//! ## Coordinate System
//!
//! This demo works entirely in POINTS (typographical points, 72pt per inch).
//! All layout functions return coordinates in points (spatium = 5pt = 1.75mm).
//! A DPI scaling transform is applied at render time to convert to screen pixels.
//!
//! This matches MuseScore's approach:
//! - Internal DPI: 1200
//! - Default spatium: 1.75mm × DPMM = ~82.68 internal units
//! - Screen conversion: internal_coords × (screen_dpi / 1200)
//!
//! For us at 96 DPI:
//! - Spatium: 5pt
//! - DPI scale: 96/72 = 1.333 (points to pixels)
//!
//! Run with: cargo run -p engraver --example layout_demo --features example

use std::sync::Arc;
use std::time::Instant;
use std::collections::VecDeque;

use kurbo::{Affine, Point, Rect};
use peniko::Color;
use vello::Scene;
use wgpu::{
    CommandEncoderDescriptor, DeviceDescriptor, Features, Instance, InstanceDescriptor,
    RequestAdapterOptions, TextureDescriptor, TextureDimension, TextureUsages,
    TextureViewDescriptor,
};
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use engraver::fonts::SMuFLFont;
use engraver::layout::context::LayoutContext;
use engraver::model::{PageStyle, PaperSize};
use engraver::layout::text_metrics::TextFontMetrics;
use engraver::layout::tlayout::{
    BarlineType, ClefType,
    layout_margin_label, MarginLabelParams, rehearsal_themes,
    layout_harmony, parse_chord, ChordNotation, HarmonyParams, HarmonyStyle,
};
use engraver::notation::{Duration, MeasureBuilder};
use engraver::renderer::SceneRenderBuilder;
use engraver::scene::id::{ElementType, SemanticId};
use engraver::scene::node::SceneNode;
use engraver::scene::paint::PaintCommand;
use engraver::style::MStyle;

const WINDOW_WIDTH: u32 = 1400;
const WINDOW_HEIGHT: u32 = 900;

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;

/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;

/// DPI scaling factor: converts points to screen pixels
/// At 96 DPI: 1pt = 1.333 pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

// Font paths relative to workspace root
// Using Bravura instead of Leland because Bravura has slash noteheads (U+E100-E10A)
// which Leland is missing
const SMUFL_FONT_PATH: &str = "packages/charts/resources/fonts/musescore/fonts/bravura/Bravura.otf";
const SMUFL_METADATA_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/bravura/bravura_metadata.json";
// Use FreeSans for general text (titles, labels, lyrics, rehearsal marks)
const TEXT_FONT_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/FreeSans.ttf";
// Use MuseJazzText for chord symbol text (root notes, quality, extensions)
const CHORD_TEXT_FONT_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf";
// Use MuseJazz for chord SMuFL symbols (triangle, circle, flat, sharp)
const CHORD_SYMBOL_FONT_PATH: &str =
    "libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazz.otf";

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
    window: Arc<Window>,
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    // Vello rendering
    vello_renderer: vello::Renderer,
    render_texture: wgpu::Texture,
    blitter: wgpu::util::TextureBlitter,
    // View transform (combined pan/zoom) - Vello example style
    transform: Affine,
    // Mouse state for zoom-about-cursor and drag-to-pan
    mouse_down: bool,
    prior_position: Option<Point>,
    // Demo scene
    demo_scene: SceneNode,
    style: &'static MStyle,
    // Font data (must outlive font)
    font_data: &'static [u8],
    font: &'static SMuFLFont<'static>,
    // Text font for general text (titles, labels, rehearsal marks)
    text_font_data: Arc<Vec<u8>>,
    // Chord text font (MuseJazzText for chord root notes, quality, extensions)
    chord_text_font_data: Arc<Vec<u8>>,
    // Chord SMuFL symbol font (MuseJazz for triangle, circle, flat, sharp)
    chord_symbol_font_data: Arc<Vec<u8>>,
    // FPS tracking
    last_frame_time: Instant,
    frame_times: VecDeque<f64>,
    show_fps: bool,
}

/// Simple FPS statistics
struct FpsStats {
    fps: f64,
    frame_time_ms: f64,
    min_ms: f64,
    max_ms: f64,
}

impl FpsStats {
    fn from_samples(samples: &VecDeque<f64>) -> Self {
        if samples.is_empty() {
            return Self {
                fps: 0.0,
                frame_time_ms: 0.0,
                min_ms: 0.0,
                max_ms: 0.0,
            };
        }
        let sum: f64 = samples.iter().sum();
        let avg = sum / samples.len() as f64;
        let min = samples.iter().cloned().fold(f64::MAX, f64::min);
        let max = samples.iter().cloned().fold(f64::MIN, f64::max);
        Self {
            fps: 1000.0 / avg,
            frame_time_ms: avg,
            min_ms: min,
            max_ms: max,
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }

        let window_attrs = Window::default_attributes()
            .with_title("Engraver Layout Demo - All Features")
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(
            event_loop
                .create_window(window_attrs)
                .expect("Failed to create window"),
        );

        let instance = Instance::new(&InstanceDescriptor::default());
        let surface = instance
            .create_surface(window.clone())
            .expect("Failed to create surface");

        let adapter = pollster::block_on(instance.request_adapter(&RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }))
        .expect("Failed to find adapter");

        let (device, queue) = pollster::block_on(adapter.request_device(
            &DeviceDescriptor {
                required_features: Features::empty(),
                ..Default::default()
            },
        ))
        .expect("Failed to create device");

        let size = window.inner_size();

        // Get the preferred surface format
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps.formats.iter()
            .find(|f| !f.is_srgb())
            .copied()
            .unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Create Vello renderer
        let vello_renderer = vello::Renderer::new(&device, vello::RendererOptions::default())
            .expect("Failed to create Vello renderer");

        // Create intermediate render texture (Rgba8Unorm for Vello's compute shaders)
        let render_texture = device.create_texture(&TextureDescriptor {
            label: Some("Vello Render Texture"),
            size: wgpu::Extent3d {
                width: size.width.max(1),
                height: size.height.max(1),
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8Unorm,
            usage: TextureUsages::STORAGE_BINDING | TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });

        // Create TextureBlitter for copying from intermediate texture to surface
        let blitter = wgpu::util::TextureBlitter::new(&device, surface_format);

        // Create style (leaked for 'static lifetime in demo)
        let style = Box::leak(Box::new(MStyle::default()));

        // Load SMuFL font (Bravura - has slash noteheads that Leland lacks)
        let font_data: &'static [u8] = Box::leak(
            std::fs::read(SMUFL_FONT_PATH)
                .expect("Failed to read Bravura.otf")
                .into_boxed_slice(),
        );
        let metadata_file =
            std::fs::File::open(SMUFL_METADATA_PATH).expect("Failed to open metadata file");
        let font: &'static SMuFLFont<'static> = Box::leak(Box::new(
            SMuFLFont::from_reader(font_data, metadata_file)
                .expect("Failed to load SMuFL font"),
        ));

        log::info!("Loaded Bravura SMuFL font successfully");

        // Load text font (FreeSans for general text like labels, rehearsal marks)
        let text_font_data = Arc::new(
            std::fs::read(TEXT_FONT_PATH)
                .expect("Failed to read FreeSans.ttf"),
        );
        log::info!("Loaded FreeSans text font successfully");

        // Load chord text font (MuseJazzText for chord root notes, quality, extensions)
        let chord_text_font_data = Arc::new(
            std::fs::read(CHORD_TEXT_FONT_PATH)
                .expect("Failed to read MuseJazzText.otf (chord text)"),
        );
        log::info!("Loaded MuseJazzText chord text font successfully");

        // Load chord SMuFL symbol font (MuseJazz for triangle, circle, flat, sharp)
        let chord_symbol_font_data = Arc::new(
            std::fs::read(CHORD_SYMBOL_FONT_PATH)
                .expect("Failed to read MuseJazz.otf (chord symbols)"),
        );
        log::info!("Loaded MuseJazz chord symbol font successfully");

        // Build the demo scene with all layout features
        let demo_scene = build_demo_scene(style, chord_text_font_data.clone(), chord_symbol_font_data.clone());

        // Initial transform: translate to show content, then apply DPI scale
        let initial_transform = Affine::translate((50.0, 100.0)) * Affine::scale(DPI_SCALE);

        self.state = Some(AppState {
            window,
            surface,
            device,
            queue,
            config,
            vello_renderer,
            render_texture,
            blitter,
            transform: initial_transform,
            mouse_down: false,
            prior_position: None,
            demo_scene,
            style,
            font_data,
            font,
            text_font_data,
            chord_text_font_data,
            chord_symbol_font_data,
            last_frame_time: Instant::now(),
            frame_times: VecDeque::with_capacity(100),
            show_fps: true, // Show FPS by default
        });
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let Some(state) = &mut self.state else { return };

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::Resized(new_size) => {
                if new_size.width > 0 && new_size.height > 0 {
                    state.config.width = new_size.width;
                    state.config.height = new_size.height;
                    state.surface.configure(&state.device, &state.config);

                    // Recreate render texture with new size
                    state.render_texture = state.device.create_texture(&TextureDescriptor {
                        label: Some("Vello Render Texture"),
                        size: wgpu::Extent3d {
                            width: new_size.width,
                            height: new_size.height,
                            depth_or_array_layers: 1,
                        },
                        mip_level_count: 1,
                        sample_count: 1,
                        dimension: TextureDimension::D2,
                        format: wgpu::TextureFormat::Rgba8Unorm,
                        usage: TextureUsages::STORAGE_BINDING | TextureUsages::TEXTURE_BINDING,
                        view_formats: &[],
                    });

                    state.window.request_redraw();
                }
            }

            // Mouse button tracking for drag-to-pan
            WindowEvent::MouseInput { state: button_state, button, .. } => {
                if button == winit::event::MouseButton::Left {
                    state.mouse_down = button_state == ElementState::Pressed;
                }
            }

            // Mouse wheel: zoom centered on cursor position (Vello example style)
            WindowEvent::MouseWheel { delta, .. } => {
                const BASE: f64 = 1.05; // 5% per scroll increment
                const PIXELS_PER_LINE: f64 = 20.0;

                if let Some(prior_position) = state.prior_position {
                    let exponent = match delta {
                        MouseScrollDelta::PixelDelta(delta) => delta.y / PIXELS_PER_LINE,
                        MouseScrollDelta::LineDelta(_, y) => y as f64,
                    };
                    // Scale about the cursor position
                    state.transform = state.transform.then_scale_about(BASE.powf(exponent), prior_position);
                    state.window.request_redraw();
                }
            }

            // Cursor movement: track position and handle drag-to-pan
            WindowEvent::CursorMoved { position, .. } => {
                let position = Point::new(position.x, position.y);
                // Drag to pan when mouse is held down
                if state.mouse_down {
                    if let Some(prior) = state.prior_position {
                        let delta = position - prior;
                        state.transform = state.transform.then_translate(delta);
                        state.window.request_redraw();
                    }
                }
                state.prior_position = Some(position);
            }

            // Clear mouse position when cursor leaves window
            WindowEvent::CursorLeft { .. } => {
                state.prior_position = None;
            }

            // Pinch-to-zoom on macOS trackpad (zoom about center of window)
            WindowEvent::PinchGesture { delta, .. } => {
                let center = Point::new(
                    state.config.width as f64 / 2.0,
                    state.config.height as f64 / 2.0,
                );
                state.transform = state.transform.then_scale_about(1.0 + delta, center);
                state.window.request_redraw();
            }

            WindowEvent::KeyboardInput { event, .. } => {
                if event.state == ElementState::Pressed {
                    match event.physical_key {
                        PhysicalKey::Code(KeyCode::KeyR) | PhysicalKey::Code(KeyCode::Space) => {
                            // Reset view to initial transform
                            state.transform = Affine::translate((50.0, 100.0)) * Affine::scale(DPI_SCALE);
                            state.window.request_redraw();
                        }
                        PhysicalKey::Code(KeyCode::KeyF) => {
                            // Toggle FPS display
                            state.show_fps = !state.show_fps;
                            state.window.request_redraw();
                        }
                        PhysicalKey::Code(KeyCode::Equal) => {
                            // Zoom in (about center)
                            let center = Point::new(
                                state.config.width as f64 / 2.0,
                                state.config.height as f64 / 2.0,
                            );
                            state.transform = state.transform.then_scale_about(1.1, center);
                            state.window.request_redraw();
                        }
                        PhysicalKey::Code(KeyCode::Minus) => {
                            // Zoom out (about center)
                            let center = Point::new(
                                state.config.width as f64 / 2.0,
                                state.config.height as f64 / 2.0,
                            );
                            state.transform = state.transform.then_scale_about(1.0 / 1.1, center);
                            state.window.request_redraw();
                        }
                        _ => {}
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                // Track frame time
                let now = Instant::now();
                let frame_time_ms = now.duration_since(state.last_frame_time).as_secs_f64() * 1000.0;
                state.last_frame_time = now;

                // Add to sliding window (max 100 samples)
                if state.frame_times.len() >= 100 {
                    state.frame_times.pop_front();
                }
                state.frame_times.push_back(frame_time_ms);

                let output = state
                    .surface
                    .get_current_texture()
                    .expect("Failed to get surface texture");

                // Build Vello scene from our demo scene
                let mut vello_scene = Scene::new();

                // Background (in screen pixels)
                vello_scene.fill(
                    vello::peniko::Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(245, 245, 245), // Light gray canvas background
                    None,
                    &Rect::new(
                        0.0,
                        0.0,
                        state.config.width as f64,
                        state.config.height as f64,
                    ),
                );

                // The transform is managed by mouse/keyboard events (Vello example style)
                // It combines pan, zoom, and DPI scaling into a single affine matrix

                // Render our scene graph using VelloSceneRenderer
                // spatium is in points (5pt = 1.75mm), matching ctx.spatium()
                let mut renderer = SceneRenderBuilder::new()
                    .spatium(5.0) // 5pt = default MuseScore spatium
                    .show_debug_boxes(false)
                    .build()
                    .with_font(state.font)
                    .with_text_font_arc(state.text_font_data.clone())
                    // Register MuseJazzText for chord text (root notes, quality, extensions)
                    .with_named_font_arc("MuseJazzText", state.chord_text_font_data.clone())
                    // Register MuseJazz for SMuFL chord symbols (flat, sharp, triangle, circle)
                    .with_named_font_arc("MuseJazz", state.chord_symbol_font_data.clone());

                // Transform the scene (all coordinates are in points)
                let mut transformed_scene = state.demo_scene.clone();
                transformed_scene.transform = state.transform;

                renderer.render(&mut vello_scene, &transformed_scene);

                // Draw FPS overlay (in screen pixel coordinates)
                if state.show_fps {
                    let stats = FpsStats::from_samples(&state.frame_times);
                    draw_fps_overlay(
                        &mut vello_scene,
                        &state.text_font_data,
                        state.config.width as f64,
                        state.config.height as f64,
                        &stats,
                        &state.frame_times,
                    );
                }

                // Create texture views
                let render_view = state.render_texture.create_view(&TextureViewDescriptor::default());
                let surface_view = output.texture.create_view(&TextureViewDescriptor::default());

                // Render Vello to intermediate Rgba8Unorm texture
                state
                    .vello_renderer
                    .render_to_texture(
                        &state.device,
                        &state.queue,
                        &vello_scene,
                        &render_view,
                        &vello::RenderParams {
                            base_color: vello::peniko::Color::WHITE,
                            width: state.config.width,
                            height: state.config.height,
                            antialiasing_method: vello::AaConfig::Msaa16,
                        },
                    )
                    .expect("Vello render failed");

                // Blit from intermediate texture to surface (handles format conversion)
                let mut encoder = state.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Blit Encoder"),
                });
                state.blitter.copy(&state.device, &mut encoder, &render_view, &surface_view);
                state.queue.submit(std::iter::once(encoder.finish()));

                output.present();

                // Request continuous redraw for FPS measurement
                state.window.request_redraw();
            }

            _ => {}
        }
    }
}

/// Draw FPS overlay in the bottom-right corner of the screen.
///
/// This draws directly in screen pixels (no DPI scaling needed).
fn draw_fps_overlay(
    scene: &mut Scene,
    text_font: &Arc<Vec<u8>>,
    viewport_width: f64,
    viewport_height: f64,
    stats: &FpsStats,
    samples: &VecDeque<f64>,
) {
    use peniko::{Blob, FontData};
    use skrifa::{raw::{FileRef, FontRef}, MetadataProvider, prelude::LocationRef};
    use vello::{Glyph, peniko::Fill};

    let width = 340.0;
    let height = 240.0;
    let padding = 15.0;
    let x_offset = viewport_width - width - padding;
    let y_offset = viewport_height - height - padding; // Bottom right
    let offset = Affine::translate((x_offset, y_offset));

    // Semi-transparent background with rounded appearance
    scene.fill(
        Fill::NonZero,
        offset,
        Color::BLACK.with_alpha(0.8),
        None,
        &Rect::new(0.0, 0.0, width, height),
    );

    // Create font data for text rendering
    let font_data = FontData::new(Blob::new(text_font.clone()), 0);

    // Helper to draw text
    let draw_text = |scene: &mut Scene, text: &str, x: f64, y: f64, size: f32, color: Color| {
        if let Some(font_ref) = {
            let file_ref = FileRef::new(font_data.data.as_ref()).ok();
            file_ref.and_then(|f| match f {
                FileRef::Font(font) => Some(font),
                FileRef::Collection(c) => c.get(0).ok(),
            })
        } {
            let skrifa_size = skrifa::instance::Size::new(size);
            let charmap = font_ref.charmap();
            let glyph_metrics = font_ref.glyph_metrics(skrifa_size, LocationRef::default());

            let mut glyphs = Vec::new();
            let mut pen_x = 0.0_f32;

            for ch in text.chars() {
                let gid = charmap.map(ch).unwrap_or_default();
                let advance = glyph_metrics.advance_width(gid).unwrap_or(size * 0.5);
                glyphs.push(Glyph {
                    id: gid.to_u32(),
                    x: pen_x,
                    y: 0.0,
                });
                pen_x += advance;
            }

            scene
                .draw_glyphs(&font_data)
                .font_size(size)
                .transform(offset * Affine::translate((x, y)))
                .brush(color)
                .draw(Fill::NonZero, glyphs.into_iter());
        }
    };

    // Draw FPS number (larger)
    let fps_color = if stats.fps >= 60.0 {
        Color::from_rgb8(100, 255, 100)
    } else if stats.fps >= 30.0 {
        Color::from_rgb8(255, 200, 0)
    } else {
        Color::from_rgb8(255, 80, 80)
    };

    draw_text(scene, &format!("FPS: {:.1}", stats.fps), 15.0, 32.0, 28.0, fps_color);
    draw_text(scene, &format!("Frame: {:.2}ms", stats.frame_time_ms), 15.0, 56.0, 18.0, Color::WHITE);
    draw_text(scene, &format!("Min: {:.2}ms  Max: {:.2}ms", stats.min_ms, stats.max_ms), 15.0, 78.0, 14.0, Color::from_rgb8(180, 180, 180));
    draw_text(scene, "Press F to toggle", 15.0, 96.0, 12.0, Color::from_rgb8(120, 120, 120));

    // Draw frame time bar graph
    let graph_y = 110.0;
    let graph_height = 115.0;
    let graph_width = width - 30.0;
    let bar_width = graph_width / 100.0;
    let max_time = stats.max_ms.max(16.67);

    for (i, &time_ms) in samples.iter().enumerate() {
        let bar_height = (time_ms / max_time) * graph_height;
        let x = 15.0 + i as f64 * bar_width;

        let bar_color = if time_ms <= 8.33 {
            Color::from_rgb8(100, 143, 255)  // 120fps - blue
        } else if time_ms <= 16.67 {
            Color::from_rgb8(100, 200, 100)  // 60fps - green
        } else if time_ms <= 33.33 {
            Color::from_rgb8(255, 176, 0)    // 30fps - orange
        } else {
            Color::from_rgb8(220, 38, 127)   // <30fps - red
        };

        scene.fill(
            Fill::NonZero,
            offset * Affine::translate((x, graph_y + graph_height - bar_height)),
            bar_color,
            None,
            &Rect::new(0.0, 0.0, bar_width * 0.8, bar_height),
        );
    }

    // Draw threshold lines
    let stroke = vello::kurbo::Stroke::new(1.0);
    let line_60fps = graph_height * (16.67 / max_time);
    scene.stroke(
        &stroke,
        offset * Affine::translate((15.0, graph_y + graph_height - line_60fps)),
        Color::from_rgb8(100, 200, 100).with_alpha(0.5),
        None,
        &vello::kurbo::Line::new((0.0, 0.0), (graph_width, 0.0)),
    );

    let line_120fps = graph_height * (8.33 / max_time);
    scene.stroke(
        &stroke,
        offset * Affine::translate((15.0, graph_y + graph_height - line_120fps)),
        Color::from_rgb8(100, 143, 255).with_alpha(0.5),
        None,
        &vello::kurbo::Line::new((0.0, 0.0), (graph_width, 0.0)),
    );
}

/// Build a comprehensive demo scene showcasing all layout features.
///
/// All coordinates are in POINTS (72pt/inch). The DPI scaling is applied
/// at render time, keeping the layout logic simple and resolution-independent.
fn build_demo_scene(
    style: &'static MStyle,
    text_font_data: Arc<Vec<u8>>,
    symbol_font_data: Arc<Vec<u8>>,
) -> SceneNode {
    // Create font metrics for accurate text measurement
    let text_metrics = TextFontMetrics::new(text_font_data);
    let symbol_metrics = TextFontMetrics::new(symbol_font_data);
    // Use PageStyle for proper page dimensions (Letter = 8.5" x 11")
    let page_style = PageStyle {
        paper_size: PaperSize::Letter,
        ..PageStyle::lead_sheet()
    };

    // Get page dimensions in POINTS (no pixel conversion!)
    let (page_width, page_height) = page_style.paper_size.dimensions_pt();
    let page_width = page_width as f64;
    let page_height = page_height as f64;

    // Margins in POINTS
    let margin_left = page_style.margins.left as f64;
    let margin_right = page_style.margins.right as f64;
    let margin_top = page_style.margins.top as f64;

    // Staff space (spatium) in POINTS - matches ctx.spatium()
    // Default: 5pt = 1.75mm (MuseScore standard)
    let spatium = page_style.staff.staff_space as f64;
    let staff_height = spatium * 4.0; // 5 lines = 4 spaces

    // Content area in POINTS
    let content_left = margin_left;
    let content_width = page_width - margin_left - margin_right;

    // Create layout context - spatium matches page_style.staff.staff_space
    let ctx = LayoutContext::minimal(style);

    // Root scene node
    let mut root = SceneNode::group(SemanticId::page(1));

    // =========================================================================
    // Page Background (paper with shadow)
    // =========================================================================
    let page_x = 20.0; // Page offset from window edge
    let page_y = 20.0;
    let shadow_offset = 4.0;

    // Paper shadow
    root.add_child(SceneNode::anonymous_leaf(vec![
        PaintCommand::filled_rect(
            Rect::new(
                page_x + shadow_offset,
                page_y + shadow_offset,
                page_x + shadow_offset + page_width,
                page_y + shadow_offset + page_height,
            ),
            Color::from_rgb8(180, 180, 180),
        ),
    ]));

    // White paper
    root.add_child(SceneNode::anonymous_leaf(vec![
        PaintCommand::filled_rect(
            Rect::new(page_x, page_y, page_x + page_width, page_y + page_height),
            Color::WHITE,
        ),
    ]));

    // =========================================================================
    // SYSTEM 1: Rhythmic Slash Notation (using high-level MeasureBuilder API)
    // =========================================================================
    let content_x = page_x + content_left;
    let staff1_y = page_y + margin_top + 50.0;
    let staff1_middle = staff1_y + 2.0 * spatium;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff1_y, content_width, spatium,
    )));

    // Section label: "OUTRO" in left margin of staff 1
    // Tests single long word that fills the width
    let (_, intro_label) = layout_margin_label(
        &MarginLabelParams {
            section_type: "Outro".to_string(),
            abbreviation: "OUT".to_string(),
            number: None,
            page_x,
            margin_width: content_left,
            staff_y: staff1_y,
            staff_height,
            style: rehearsal_themes::dark(),
            ..Default::default()
        },
        &ctx,
    );
    let mut intro_container = SceneNode::group(SemanticId::new(ElementType::RehearsalMark, 1));
    intro_container.add_child(intro_label);
    root.add_child(intro_container);

    // Measure 1: Four quarter notes (with clef and time signature)
    let measure1 = MeasureBuilder::new()
        .clef(ClefType::Treble)
        .time_signature(4, 4)
        .rhythmic()
        .rhythm(vec![
            Duration::Quarter,
            Duration::Quarter,
            Duration::Quarter,
            Duration::Quarter,
        ])
        .id_base(500)
        .build(&ctx);

    let mut m1_container = SceneNode::group(SemanticId::new(ElementType::Measure, 500));
    m1_container.transform = Affine::translate((content_x, staff1_middle));
    m1_container.add_child(measure1.scene);
    root.add_child(m1_container);

    // Measure 2: Eight eighth notes (auto-beamed in groups of 4)
    let measure2 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
            Duration::Eighth,
        ])
        .id_base(520)
        .build(&ctx);

    let mut m2_container = SceneNode::group(SemanticId::new(ElementType::Measure, 520));
    m2_container.transform = Affine::translate((content_x + measure1.width, staff1_middle));
    m2_container.add_child(measure2.scene);
    root.add_child(m2_container);

    // Measure 3: Mixed rhythms (16ths + dotted eighth + 16th)
    // Dotted eighth (3/16) + sixteenth (1/16) = quarter note (4/16) - a common rhythm
    let measure3 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Sixteenth,
            Duration::Sixteenth,
            Duration::Sixteenth,
            Duration::Sixteenth,
            Duration::DottedEighth,
            Duration::Sixteenth,
        ])
        .id_base(540)
        .build(&ctx);

    let mut m3_container = SceneNode::group(SemanticId::new(ElementType::Measure, 540));
    m3_container.transform = Affine::translate((content_x + measure1.width + measure2.width, staff1_middle));
    m3_container.add_child(measure3.scene);
    root.add_child(m3_container);

    // Measure 4: Complex syncopation (dotted quarter + eighth + half)
    let measure4 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::DottedQuarter,
            Duration::Eighth,
            Duration::Half,
        ])
        .end_barline(BarlineType::Double)
        .id_base(560)
        .build(&ctx);

    let mut m4_container = SceneNode::group(SemanticId::new(ElementType::Measure, 560));
    m4_container.transform = Affine::translate((content_x + measure1.width + measure2.width + measure3.width, staff1_middle));
    m4_container.add_child(measure4.scene);
    root.add_child(m4_container);

    // =========================================================================
    // SYSTEM 2: Stemless Slash Notation (whole note style slashes)
    // =========================================================================
    let staff2_y = staff1_y + staff_height + 70.0;
    let staff2_middle = staff2_y + 2.0 * spatium;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff2_y, content_width, spatium,
    )));

    // Section label: "Guitar Solo" in left margin of staff 2
    // This tests multiline word wrapping ("Guitar" + "Solo" on separate lines)
    let (_, vs1_label) = layout_margin_label(
        &MarginLabelParams {
            section_type: "Guitar Solo".to_string(),  // Will wrap to two lines
            abbreviation: "GTR SOLO".to_string(),
            number: None,
            page_x,
            margin_width: content_left,
            staff_y: staff2_y,
            staff_height,
            style: rehearsal_themes::blue(),
            ..Default::default()
        },
        &ctx,
    );
    let mut vs1_container = SceneNode::group(SemanticId::new(ElementType::RehearsalMark, 2));
    vs1_container.add_child(vs1_label);
    root.add_child(vs1_container);

    // Measure 1: Four quarters → all auto-stemless (2+ consecutive quarters)
    let measure5 = MeasureBuilder::new()
        .clef(ClefType::Treble)
        .time_signature(4, 4)
        .rhythmic()
        .rhythm(vec![
            Duration::Quarter,
            Duration::Quarter,
            Duration::Quarter,
            Duration::Quarter,
        ])
        .id_base(600)
        .build(&ctx);

    let mut m5_container = SceneNode::group(SemanticId::new(ElementType::Measure, 600));
    m5_container.transform = Affine::translate((content_x, staff2_middle));
    m5_container.add_child(measure5.scene);
    root.add_child(m5_container);

    // Measure 2: Mixed - eighth notes have stems, quarters after them are stemless
    // Pattern: 8th 8th Q Q Q → eighths have stems, 3 quarters are consecutive = stemless
    let measure6 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Eighth,
            Duration::Eighth,
            Duration::Quarter,
            Duration::Quarter,
            Duration::Quarter,
        ])
        .id_base(620)
        .build(&ctx);

    let mut m6_container = SceneNode::group(SemanticId::new(ElementType::Measure, 620));
    m6_container.transform = Affine::translate((content_x + measure5.width, staff2_middle));
    m6_container.add_child(measure6.scene);
    root.add_child(m6_container);

    // Measure 3: Single quarter (not stemless - needs 2+ consecutive) + dotted half
    // Pattern: Q + dotted half → quarter has stem (single), half has stem
    let measure7 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Quarter,
            Duration::DottedHalf,
        ])
        .id_base(640)
        .build(&ctx);

    let mut m7_container = SceneNode::group(SemanticId::new(ElementType::Measure, 640));
    m7_container.transform = Affine::translate((content_x + measure5.width + measure6.width, staff2_middle));
    m7_container.add_child(measure7.scene);
    root.add_child(m7_container);

    // Measure 4: Half + two quarters → half has stem, 2 quarters = stemless
    let measure8 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Half,
            Duration::Quarter,
            Duration::Quarter,
        ])
        .end_barline(BarlineType::End)
        .id_base(660)
        .build(&ctx);

    let mut m8_container = SceneNode::group(SemanticId::new(ElementType::Measure, 660));
    m8_container.transform = Affine::translate((content_x + measure5.width + measure6.width + measure7.width, staff2_middle));
    m8_container.add_child(measure8.scene);
    root.add_child(m8_container);

    // =========================================================================
    // SYSTEM 3: Complex Rhythms (triplets, ties, syncopation patterns)
    // =========================================================================
    let staff3_y = staff2_y + staff_height + 70.0;
    let staff3_middle = staff3_y + 2.0 * spatium;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff3_y, content_width, spatium,
    )));

    // Section label: "CH 1 B" (Chorus 1 section B) in left margin of staff 3
    // This tests multiline with section letter
    let (_, ch1_label) = layout_margin_label(
        &MarginLabelParams {
            section_type: "Chorus".to_string(),
            abbreviation: "CH 1 B".to_string(),  // Section B will be on its own line
            number: None,
            page_x,
            margin_width: content_left,
            staff_y: staff3_y,
            staff_height,
            style: rehearsal_themes::green(),
            ..Default::default()
        },
        &ctx,
    );
    let mut ch1_container = SceneNode::group(SemanticId::new(ElementType::RehearsalMark, 3));
    ch1_container.add_child(ch1_label);
    root.add_child(ch1_container);

    // Measure 1: 32nd note run
    let measure9 = MeasureBuilder::new()
        .clef(ClefType::Treble)
        .time_signature(4, 4)
        .rhythmic()
        .rhythm(vec![
            Duration::ThirtySecond, Duration::ThirtySecond, Duration::ThirtySecond, Duration::ThirtySecond,
            Duration::ThirtySecond, Duration::ThirtySecond, Duration::ThirtySecond, Duration::ThirtySecond,
            Duration::Quarter,
            Duration::Half,
        ])
        .id_base(700)
        .build(&ctx);

    let mut m9_container = SceneNode::group(SemanticId::new(ElementType::Measure, 700));
    m9_container.transform = Affine::translate((content_x, staff3_middle));
    m9_container.add_child(measure9.scene);
    root.add_child(m9_container);

    // Measure 2: Complex syncopation pattern
    let measure10 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::Eighth,
            Duration::DottedQuarter,
            Duration::Eighth,
            Duration::Quarter,
        ])
        .id_base(720)
        .build(&ctx);

    let mut m10_container = SceneNode::group(SemanticId::new(ElementType::Measure, 720));
    m10_container.transform = Affine::translate((content_x + measure9.width, staff3_middle));
    m10_container.add_child(measure10.scene);
    root.add_child(m10_container);

    // Measure 3: Dotted rhythms (dotted sixteenth + 32nd pattern)
    let measure11 = MeasureBuilder::new()
        .rhythmic()
        .rhythm(vec![
            Duration::DottedEighth, Duration::Sixteenth,
            Duration::DottedEighth, Duration::Sixteenth,
            Duration::DottedEighth, Duration::Sixteenth,
            Duration::Eighth,
        ])
        .end_barline(BarlineType::End)
        .id_base(740)
        .build(&ctx);

    let mut m11_container = SceneNode::group(SemanticId::new(ElementType::Measure, 740));
    m11_container.transform = Affine::translate((content_x + measure9.width + measure10.width, staff3_middle));
    m11_container.add_child(measure11.scene);
    root.add_child(m11_container);

    // =========================================================================
    // SYSTEM 4: Chord Symbols - Standard Notation
    // =========================================================================
    let staff4_y = staff3_y + staff_height + 80.0;
    let staff4_middle = staff4_y + 2.0 * spatium;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff4_y, content_width, spatium,
    )));

    // Standard chord symbols above staff (using MuseJazz font for handwritten style)
    let chord_y = staff4_y - 8.0; // Position above staff
    let chord_spacing = 70.0;
    let standard_style = HarmonyStyle::musejazz()
        .with_text_font_metrics(text_metrics.clone())
        .with_symbol_font_metrics(symbol_metrics.clone());

    let standard_chords = [
        "C", "Cm", "Cdim", "Caug", "C5", "Csus4", "Csus2",
    ];

    for (i, chord_str) in standard_chords.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord_y)
            .with_style(standard_style.clone());
        params.id = 800 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // SYSTEM 5: Seventh Chords - Standard Notation
    // =========================================================================
    let staff5_y = staff4_y + staff_height + 60.0;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff5_y, content_width, spatium,
    )));

    let chord5_y = staff5_y - 8.0;
    let seventh_chords = [
        "CMaj7", "C7", "Cm7", "CmMaj7", "Cdim7", "Cm7b5",
    ];

    for (i, chord_str) in seventh_chords.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord5_y)
            .with_style(standard_style.clone());
        params.id = 850 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // SYSTEM 6: Extended/Altered - Standard Notation
    // =========================================================================
    let staff6_y = staff5_y + staff_height + 60.0;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff6_y, content_width, spatium,
    )));

    let chord6_y = staff6_y - 8.0;
    let extended_chords = [
        "C9", "C11", "C13", "C7b9", "C7#9", "C7alt",
    ];

    for (i, chord_str) in extended_chords.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord6_y)
            .with_style(standard_style.clone());
        params.id = 900 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // SYSTEM 7: Chord Symbols - Jazz Notation
    // =========================================================================
    let staff7_y = staff6_y + staff_height + 80.0;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff7_y, content_width, spatium,
    )));

    let chord7_y = staff7_y - 8.0;
    let jazz_style = HarmonyStyle::musejazz_jazz()
        .with_text_font_metrics(text_metrics.clone())
        .with_symbol_font_metrics(symbol_metrics.clone());

    // Jazz triads with special symbols
    let jazz_triads = [
        "C", "Cm", "Cdim", "Caug", "C5", "Csus4", "Csus2",
    ];

    for (i, chord_str) in jazz_triads.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord7_y)
            .with_style(jazz_style.clone());
        params.id = 950 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // SYSTEM 8: Seventh Chords - Jazz Notation
    // =========================================================================
    let staff8_y = staff7_y + staff_height + 60.0;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff8_y, content_width, spatium,
    )));

    let chord8_y = staff8_y - 8.0;
    let jazz_sevenths = [
        "CMaj7", "C7", "Cm7", "CmMaj7", "Cdim7", "Cm7b5",
    ];

    for (i, chord_str) in jazz_sevenths.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord8_y)
            .with_style(jazz_style.clone());
        params.id = 1000 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // SYSTEM 9: Slash Chords - Jazz Notation
    // =========================================================================
    let staff9_y = staff8_y + staff_height + 60.0;

    // Draw staff lines
    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff9_y, content_width, spatium,
    )));

    let chord9_y = staff9_y - 8.0;
    let slash_chords = [
        "C/E", "Cm/G", "C7/Bb", "CMaj7/B", "F#m7/C#", "Bb/D",
    ];

    for (i, chord_str) in slash_chords.iter().enumerate() {
        let mut params = parse_chord(chord_str)
            .at(content_x + 20.0 + i as f64 * chord_spacing, chord9_y)
            .with_style(jazz_style.clone());
        params.id = 1050 + i as u64;

        let (_, chord_node) = layout_harmony(&params, &ctx);
        root.add_child(chord_node);
    }

    // =========================================================================
    // Labels and Title
    // =========================================================================
    let title_x = page_x + page_width / 2.0 - 100.0;
    let title_y = page_y + margin_top / 2.0;

    root.add_child(SceneNode::anonymous_leaf(vec![
        PaintCommand::text("Engraver Layout Demo".to_string(), "sans-serif", 22.0, Point::new(title_x, title_y), Color::BLACK),
    ]));

    // Section labels
    let labels = [
        (content_x, staff1_y - 12.0, "System 1: Rhythmic slash notation (quarters, 8ths, 16ths, syncopation)"),
        (content_x, staff2_y - 12.0, "System 2: Auto-stemless rhythmic notation (2+ consecutive quarters = stemless)"),
        (content_x, staff3_y - 12.0, "System 3: Complex rhythms (32nds, syncopation, dotted patterns)"),
        (content_x, staff4_y - 20.0, "System 4: Basic Triads - Standard Notation"),
        (content_x, staff5_y - 20.0, "System 5: Seventh Chords - Standard Notation"),
        (content_x, staff6_y - 20.0, "System 6: Extended/Altered - Standard Notation"),
        (content_x, staff7_y - 20.0, "System 7: Basic Triads - Jazz Notation"),
        (content_x, staff8_y - 20.0, "System 8: Seventh Chords - Jazz Notation"),
        (content_x, staff9_y - 20.0, "System 9: Slash Chords - Jazz Notation"),
    ];

    for (x, y, text) in labels {
        root.add_child(SceneNode::anonymous_leaf(vec![
            PaintCommand::text(text.to_string(), "sans-serif", 9.0, Point::new(x, y), Color::from_rgb8(80, 80, 80)),
        ]));
    }

    // Controls at bottom
    let controls_y = page_y + page_height - 15.0;
    root.add_child(SceneNode::anonymous_leaf(vec![
        PaintCommand::text(
            "Controls: Scroll=Zoom (on cursor) | Drag=Pan | +/-=Zoom | Space/R=Reset | F=FPS".to_string(),
            "sans-serif", 8.0, Point::new(page_x + margin_left, controls_y), Color::from_rgb8(140, 140, 140)
        ),
    ]));

    root
}

/// Draw 5 staff lines.
fn draw_staff_lines(x: f64, y: f64, width: f64, spatium: f64) -> Vec<PaintCommand> {
    let mut commands = Vec::new();
    let line_thickness = spatium * 0.1;

    for i in 0..5 {
        let line_y = y + i as f64 * spatium;
        commands.push(PaintCommand::line(
            Point::new(x, line_y),
            Point::new(x + width, line_y),
            Color::BLACK,
            line_thickness,
        ));
    }

    commands
}
