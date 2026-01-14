//! Layout Demo Example
//!
//! Demonstrates all engraver layout features rendering together:
//! - Notes, chords, rests
//! - Clefs, key signatures, time signatures
//! - Lyrics with multi-syllable words and verse stacking
//! - Dynamics (ppp to fff, sfz, etc.)
//! - Barlines
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
use engraver::layout::tlayout::{
    // Barlines
    layout_barline, BarlineParams, BarlineType,
    // Beams
    layout_beam, BeamLayoutConfig, BeamNote,
    // Chords (used for all notes - provides stem rendering)
    layout_chord, ChordNote, ChordParams, StemDirection,
    // Clefs
    layout_clef, ClefParams, ClefType,
    // Key signatures
    layout_keysig, ClefContext, KeySigParams, KeySigType,
    // Lyrics
    layout_lyrics, layout_lyrics_dash, LyricsParams, LyricsPlacement, SyllabicType,
    // Notes (for Accidental and NoteDuration types, and layout_note for beamed notes)
    layout_note, Accidental, NoteDuration, NoteParams,
    // Rests
    layout_rest, RestDuration, RestParams,
    // Time signatures
    layout_timesig, TimeSigParams, TimeSigType,
};
use engraver::renderer::{SceneRenderBuilder, VelloSceneRenderer};
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
const LELAND_FONT_PATH: &str = "packages/charts/resources/fonts/musescore/fonts/leland/Leland.otf";
const LELAND_METADATA_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/leland/leland_metadata.json";
// Use FreeSans for general text (titles, labels, lyrics)
// LelandText is mainly for music-specific text symbols
const TEXT_FONT_PATH: &str =
    "packages/charts/resources/fonts/musescore/fonts/FreeSans.ttf";

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
    // Text font for lyrics, titles, etc.
    text_font_data: Arc<Vec<u8>>,
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

        // Load SMuFL font (Leland)
        let font_data: &'static [u8] = Box::leak(
            std::fs::read(LELAND_FONT_PATH)
                .expect("Failed to read Leland.otf")
                .into_boxed_slice(),
        );
        let metadata_file =
            std::fs::File::open(LELAND_METADATA_PATH).expect("Failed to open metadata file");
        let font: &'static SMuFLFont<'static> = Box::leak(Box::new(
            SMuFLFont::from_reader(font_data, metadata_file)
                .expect("Failed to load SMuFL font"),
        ));

        log::info!("Loaded Leland SMuFL font successfully");

        // Load text font (FreeSans for general text)
        let text_font_data = Arc::new(
            std::fs::read(TEXT_FONT_PATH)
                .expect("Failed to read FreeSans.ttf"),
        );
        log::info!("Loaded FreeSans text font successfully");

        // Build the demo scene with all layout features
        let demo_scene = build_demo_scene(style);

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
                    .with_text_font_arc(state.text_font_data.clone());

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
fn build_demo_scene(style: &'static MStyle) -> SceneNode {
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
    // SYSTEM 1: Melody with varied rhythms
    // =========================================================================
    let staff1_y = page_y + margin_top + 50.0;
    let staff1_middle = staff1_y + 2.0 * spatium;
    let content_x = page_x + content_left;

    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff1_y, content_width, spatium,
    )));

    // Treble clef
    let clef_x = content_x + spatium * 0.5;
    let (_, clef_node) = layout_clef(&ClefParams { id: 1, clef_type: ClefType::Treble, ..Default::default() }, &ctx);
    let mut clef_container = SceneNode::group(SemanticId::new(ElementType::Clef, 1));
    clef_container.transform = Affine::translate((clef_x, staff1_middle));
    clef_container.add_child(clef_node);
    root.add_child(clef_container);

    // Key signature: D major (2 sharps)
    let keysig_x = clef_x + spatium * 4.5;
    let (_, keysig_node) = layout_keysig(&KeySigParams { id: 2, key: KeySigType::Standard(2), clef: ClefContext::Treble, ..Default::default() }, &ctx);
    let mut keysig_container = SceneNode::group(SemanticId::new(ElementType::KeySignature, 2));
    keysig_container.transform = Affine::translate((keysig_x, staff1_middle));
    keysig_container.add_child(keysig_node);
    root.add_child(keysig_container);

    // Time signature: 4/4 numeric
    let timesig_x = keysig_x + spatium * 2.5;
    let (_, timesig_node) = layout_timesig(&TimeSigParams {
        id: 3,
        sig_type: TimeSigType::Numeric { numerator: 4, denominator: 4 },
        ..Default::default()
    }, &ctx);
    let mut timesig_container = SceneNode::group(SemanticId::new(ElementType::TimeSignature, 3));
    timesig_container.transform = Affine::translate((timesig_x, staff1_middle));
    timesig_container.add_child(timesig_node);
    root.add_child(timesig_container);

    // Measure 1: Varied rhythm - dotted quarter + eighth + two quarters
    let m1_start = timesig_x + spatium * 4.0;
    let beat_space = spatium * 5.0;

    // Helper to create single-note chord (notes need stems via chord layout)
    let single_note = |id: u64, dur: NoteDuration, line: i32, acc: Accidental, dots: u8| -> ChordParams {
        ChordParams {
            id,
            duration: dur,
            notes: vec![ChordNote { line, accidental: acc, tie: false }],
            stem_direction: if line <= 0 { StemDirection::Down } else { StemDirection::Up },
            dots,
            ..Default::default()
        }
    };

    // Dotted quarter (D5, line -1)
    let (_, n1) = layout_chord(&single_note(10, NoteDuration::Quarter, -1, Accidental::None, 1), &ctx);
    let mut c1 = SceneNode::group(SemanticId::chord(10));
    c1.transform = Affine::translate((m1_start, staff1_middle));
    c1.add_child(n1);
    root.add_child(c1);

    // Eighth note (E5, line -2)
    let (_, n2) = layout_chord(&single_note(11, NoteDuration::Eighth, -2, Accidental::None, 0), &ctx);
    let mut c2 = SceneNode::group(SemanticId::chord(11));
    c2.transform = Affine::translate((m1_start + beat_space * 1.5, staff1_middle));
    c2.add_child(n2);
    root.add_child(c2);

    // Quarter (F#5, line -3)
    let (_, n3) = layout_chord(&single_note(12, NoteDuration::Quarter, -3, Accidental::None, 0), &ctx);
    let mut c3 = SceneNode::group(SemanticId::chord(12));
    c3.transform = Affine::translate((m1_start + beat_space * 2.0, staff1_middle));
    c3.add_child(n3);
    root.add_child(c3);

    // Quarter (G5, line -4) with natural
    let (_, n4) = layout_chord(&single_note(13, NoteDuration::Quarter, -4, Accidental::Natural, 0), &ctx);
    let mut c4 = SceneNode::group(SemanticId::chord(13));
    c4.transform = Affine::translate((m1_start + beat_space * 3.0, staff1_middle));
    c4.add_child(n4);
    root.add_child(c4);

    // Barline
    let bar1_x = m1_start + beat_space * 4.0;
    let (_, bl1) = layout_barline(&BarlineParams { id: 30, barline_type: BarlineType::Single, ..Default::default() }, &ctx);
    let mut bl1_c = SceneNode::group(SemanticId::new(ElementType::Barline, 30));
    bl1_c.transform = Affine::translate((bar1_x, staff1_middle));
    bl1_c.add_child(bl1);
    root.add_child(bl1_c);

    // Measure 2: Sixteenth notes and rests
    let m2_start = bar1_x + spatium * 2.0;

    // Sixteenth notes (fast run)
    for i in 0..4 {
        let (_, n) = layout_chord(&single_note(20 + i, NoteDuration::Sixteenth, i as i32 - 2, Accidental::None, 0), &ctx);
        let mut c = SceneNode::group(SemanticId::chord(20 + i));
        c.transform = Affine::translate((m2_start + (i as f64) * beat_space * 0.25, staff1_middle));
        c.add_child(n);
        root.add_child(c);
    }

    // Quarter rest
    let (_, r1) = layout_rest(&RestParams { id: 24, duration: RestDuration::Quarter, ..Default::default() }, &ctx);
    let mut r1_c = SceneNode::group(SemanticId::rest(24));
    r1_c.transform = Affine::translate((m2_start + beat_space * 1.0, staff1_middle));
    r1_c.add_child(r1);
    root.add_child(r1_c);

    // Eighth rest
    let (_, r2) = layout_rest(&RestParams { id: 25, duration: RestDuration::Eighth, ..Default::default() }, &ctx);
    let mut r2_c = SceneNode::group(SemanticId::rest(25));
    r2_c.transform = Affine::translate((m2_start + beat_space * 2.0, staff1_middle));
    r2_c.add_child(r2);
    root.add_child(r2_c);

    // Two more eighths
    let (_, n5) = layout_chord(&single_note(26, NoteDuration::Eighth, 0, Accidental::None, 0), &ctx);
    let mut c5 = SceneNode::group(SemanticId::chord(26));
    c5.transform = Affine::translate((m2_start + beat_space * 2.5, staff1_middle));
    c5.add_child(n5);
    root.add_child(c5);

    let (_, n6) = layout_chord(&single_note(27, NoteDuration::Eighth, 1, Accidental::None, 0), &ctx);
    let mut c6 = SceneNode::group(SemanticId::chord(27));
    c6.transform = Affine::translate((m2_start + beat_space * 3.0, staff1_middle));
    c6.add_child(n6);
    root.add_child(c6);

    // Half note
    let (_, n7) = layout_chord(&single_note(28, NoteDuration::Half, 2, Accidental::None, 0), &ctx);
    let mut c7 = SceneNode::group(SemanticId::chord(28));
    c7.transform = Affine::translate((m2_start + beat_space * 3.5, staff1_middle));
    c7.add_child(n7);
    root.add_child(c7);

    // Double barline
    let bar2_x = m2_start + beat_space * 4.5;
    let (_, bl2) = layout_barline(&BarlineParams { id: 31, barline_type: BarlineType::Double, ..Default::default() }, &ctx);
    let mut bl2_c = SceneNode::group(SemanticId::new(ElementType::Barline, 31));
    bl2_c.transform = Affine::translate((bar2_x, staff1_middle));
    bl2_c.add_child(bl2);
    root.add_child(bl2_c);

    // =========================================================================
    // SYSTEM 2: Chords (triads, 7ths, open voicings)
    // =========================================================================
    let staff2_y = staff1_y + staff_height + 70.0;
    let staff2_middle = staff2_y + 2.0 * spatium;

    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff2_y, content_width, spatium,
    )));

    // Bass clef
    let (_, bass_clef) = layout_clef(&ClefParams { id: 50, clef_type: ClefType::Bass, ..Default::default() }, &ctx);
    let mut bass_c = SceneNode::group(SemanticId::new(ElementType::Clef, 50));
    bass_c.transform = Affine::translate((clef_x, staff2_middle));
    bass_c.add_child(bass_clef);
    root.add_child(bass_c);

    // Key sig (D major)
    let (_, ks2) = layout_keysig(&KeySigParams { id: 51, key: KeySigType::Standard(2), clef: ClefContext::Bass, ..Default::default() }, &ctx);
    let mut ks2_c = SceneNode::group(SemanticId::new(ElementType::KeySignature, 51));
    ks2_c.transform = Affine::translate((keysig_x, staff2_middle));
    ks2_c.add_child(ks2);
    root.add_child(ks2_c);

    // Time sig 4/4
    let (_, ts2) = layout_timesig(&TimeSigParams { id: 52, sig_type: TimeSigType::Numeric { numerator: 4, denominator: 4 }, ..Default::default() }, &ctx);
    let mut ts2_c = SceneNode::group(SemanticId::new(ElementType::TimeSignature, 52));
    ts2_c.transform = Affine::translate((timesig_x, staff2_middle));
    ts2_c.add_child(ts2);
    root.add_child(ts2_c);

    // Chord 1: D major triad (D-F#-A) - root position
    let chord1_x = timesig_x + spatium * 4.0;
    let (_, ch1) = layout_chord(&ChordParams {
        id: 60,
        duration: NoteDuration::Half,
        notes: vec![
            ChordNote { line: 3, accidental: Accidental::None, tie: false },  // D
            ChordNote { line: 1, accidental: Accidental::None, tie: false },  // F#
            ChordNote { line: -1, accidental: Accidental::None, tie: false }, // A
        ],
        stem_direction: StemDirection::Up,
        ..Default::default()
    }, &ctx);
    let mut ch1_c = SceneNode::group(SemanticId::chord(60));
    ch1_c.transform = Affine::translate((chord1_x, staff2_middle));
    ch1_c.add_child(ch1);
    root.add_child(ch1_c);

    // Chord 2: G major triad (G-B-D)
    let chord2_x = chord1_x + beat_space * 2.0;
    let (_, ch2) = layout_chord(&ChordParams {
        id: 61,
        duration: NoteDuration::Half,
        notes: vec![
            ChordNote { line: 4, accidental: Accidental::None, tie: false },  // G (below staff)
            ChordNote { line: 2, accidental: Accidental::None, tie: false },  // B
            ChordNote { line: 0, accidental: Accidental::None, tie: false },  // D
        ],
        stem_direction: StemDirection::Up,
        ..Default::default()
    }, &ctx);
    let mut ch2_c = SceneNode::group(SemanticId::chord(61));
    ch2_c.transform = Affine::translate((chord2_x, staff2_middle));
    ch2_c.add_child(ch2);
    root.add_child(ch2_c);

    // Barline
    let bar3_x = chord2_x + beat_space * 2.0;
    let (_, bl3) = layout_barline(&BarlineParams { id: 62, barline_type: BarlineType::Single, ..Default::default() }, &ctx);
    let mut bl3_c = SceneNode::group(SemanticId::new(ElementType::Barline, 62));
    bl3_c.transform = Affine::translate((bar3_x, staff2_middle));
    bl3_c.add_child(bl3);
    root.add_child(bl3_c);

    // Chord 3: A7 chord (A-C#-E-G) with accidentals
    let chord3_x = bar3_x + spatium * 2.0;
    let (_, ch3) = layout_chord(&ChordParams {
        id: 63,
        duration: NoteDuration::Quarter,
        notes: vec![
            ChordNote { line: 2, accidental: Accidental::None, tie: false },    // A
            ChordNote { line: 0, accidental: Accidental::Sharp, tie: false },   // C#
            ChordNote { line: -2, accidental: Accidental::None, tie: false },   // E
            ChordNote { line: -4, accidental: Accidental::Natural, tie: false }, // G natural
        ],
        stem_direction: StemDirection::Down,
        ..Default::default()
    }, &ctx);
    let mut ch3_c = SceneNode::group(SemanticId::chord(63));
    ch3_c.transform = Affine::translate((chord3_x, staff2_middle));
    ch3_c.add_child(ch3);
    root.add_child(ch3_c);

    // Chord 4: Dm (D-F-A) with flat
    let chord4_x = chord3_x + beat_space * 1.0;
    let (_, ch4) = layout_chord(&ChordParams {
        id: 64,
        duration: NoteDuration::Quarter,
        notes: vec![
            ChordNote { line: 3, accidental: Accidental::None, tie: false },  // D
            ChordNote { line: 1, accidental: Accidental::Flat, tie: false },  // F natural (flat cancels sharp)
            ChordNote { line: -1, accidental: Accidental::None, tie: false }, // A
        ],
        stem_direction: StemDirection::Up,
        ..Default::default()
    }, &ctx);
    let mut ch4_c = SceneNode::group(SemanticId::chord(64));
    ch4_c.transform = Affine::translate((chord4_x, staff2_middle));
    ch4_c.add_child(ch4);
    root.add_child(ch4_c);

    // Chord 5: Open voicing (wide spread)
    let chord5_x = chord4_x + beat_space * 1.0;
    let (_, ch5) = layout_chord(&ChordParams {
        id: 65,
        duration: NoteDuration::Half,
        notes: vec![
            ChordNote { line: 6, accidental: Accidental::None, tie: false },   // Low
            ChordNote { line: 0, accidental: Accidental::None, tie: false },   // Middle
            ChordNote { line: -4, accidental: Accidental::None, tie: false },  // High
        ],
        stem_direction: StemDirection::Down,
        ..Default::default()
    }, &ctx);
    let mut ch5_c = SceneNode::group(SemanticId::chord(65));
    ch5_c.transform = Affine::translate((chord5_x, staff2_middle));
    ch5_c.add_child(ch5);
    root.add_child(ch5_c);

    // End barline
    let bar4_x = chord5_x + beat_space * 2.0;
    let (_, bl4) = layout_barline(&BarlineParams { id: 66, barline_type: BarlineType::End, ..Default::default() }, &ctx);
    let mut bl4_c = SceneNode::group(SemanticId::new(ElementType::Barline, 66));
    bl4_c.transform = Affine::translate((bar4_x, staff2_middle));
    bl4_c.add_child(bl4);
    root.add_child(bl4_c);

    // =========================================================================
    // SYSTEM 3: Lyrics with melody
    // =========================================================================
    let staff3_y = staff2_y + staff_height + 80.0;
    let staff3_middle = staff3_y + 2.0 * spatium;

    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff3_y, content_width, spatium,
    )));

    // Treble clef
    let (_, clef3) = layout_clef(&ClefParams { id: 70, clef_type: ClefType::Treble, ..Default::default() }, &ctx);
    let mut clef3_c = SceneNode::group(SemanticId::new(ElementType::Clef, 70));
    clef3_c.transform = Affine::translate((clef_x, staff3_middle));
    clef3_c.add_child(clef3);
    root.add_child(clef3_c);

    // Time sig 3/4 (waltz)
    let (_, ts3) = layout_timesig(&TimeSigParams {
        id: 71,
        sig_type: TimeSigType::Numeric { numerator: 3, denominator: 4 },
        ..Default::default()
    }, &ctx);
    let mut ts3_c = SceneNode::group(SemanticId::new(ElementType::TimeSignature, 71));
    ts3_c.transform = Affine::translate((clef_x + spatium * 4.0, staff3_middle));
    ts3_c.add_child(ts3);
    root.add_child(ts3_c);

    // "Amazing Grace" opening: A-ma-zing Grace, how sweet the sound
    let lyrics_data = [
        ("A", SyllabicType::Begin, NoteDuration::Quarter, 0, 0),
        ("ma", SyllabicType::Middle, NoteDuration::Half, -1, 1),
        ("zing", SyllabicType::End, NoteDuration::Quarter, -2, 2),
        ("Grace,", SyllabicType::Single, NoteDuration::Half, 0, 3),
        ("how", SyllabicType::Single, NoteDuration::Quarter, -1, 4),
        ("sweet", SyllabicType::Single, NoteDuration::Quarter, -2, 5),
        ("the", SyllabicType::Single, NoteDuration::Quarter, -1, 6),
        ("sound", SyllabicType::Single, NoteDuration::Half, 0, 7),
    ];

    let lyrics_start = clef_x + spatium * 7.0;
    let lyric_spacing = spatium * 4.5;
    let lyric_y = staff3_y + spatium * 5.5;

    for (text, syllabic, dur, line, idx) in lyrics_data {
        let x = lyrics_start + idx as f64 * lyric_spacing;

        // Note (using chord layout for stem)
        let (_, note) = layout_chord(&ChordParams {
            id: 100 + idx as u64,
            duration: dur,
            notes: vec![ChordNote { line, accidental: Accidental::None, tie: false }],
            stem_direction: if line <= 0 { StemDirection::Down } else { StemDirection::Up },
            ..Default::default()
        }, &ctx);
        let mut note_c = SceneNode::group(SemanticId::chord(100 + idx as u64));
        note_c.transform = Affine::translate((x, staff3_middle));
        note_c.add_child(note);
        root.add_child(note_c);

        // Lyric
        let (_, lyric) = layout_lyrics(&LyricsParams {
            id: 200 + idx as u64,
            text: text.to_string(),
            syllabic,
            verse: 0,
            placement: LyricsPlacement::Below,
            note_x: 0.0,
            note_width: spatium,
            ..Default::default()
        }, &ctx);
        let mut lyric_c = SceneNode::group(SemanticId::new(ElementType::Lyrics, 200 + idx as u64));
        lyric_c.transform = Affine::translate((x, lyric_y));
        lyric_c.add_child(lyric);
        root.add_child(lyric_c);
    }

    // Add lyric dashes
    let dash_y = lyric_y + spatium * 0.3;
    // Dash after "A"
    let (_, dash1) = layout_lyrics_dash(lyrics_start + lyric_spacing * 0.6, lyrics_start + lyric_spacing * 0.9, dash_y, &ctx);
    root.add_child(dash1);
    // Dash after "ma"
    let (_, dash2) = layout_lyrics_dash(lyrics_start + lyric_spacing * 1.6, lyrics_start + lyric_spacing * 1.9, dash_y, &ctx);
    root.add_child(dash2);

    // Barlines for lyrics section
    let bar5_x = lyrics_start + lyric_spacing * 3.3;
    let (_, bl5) = layout_barline(&BarlineParams { id: 80, barline_type: BarlineType::Single, ..Default::default() }, &ctx);
    let mut bl5_c = SceneNode::group(SemanticId::new(ElementType::Barline, 80));
    bl5_c.transform = Affine::translate((bar5_x, staff3_middle));
    bl5_c.add_child(bl5);
    root.add_child(bl5_c);

    let bar6_x = lyrics_start + lyric_spacing * 7.5;
    let (_, bl6) = layout_barline(&BarlineParams { id: 81, barline_type: BarlineType::End, ..Default::default() }, &ctx);
    let mut bl6_c = SceneNode::group(SemanticId::new(ElementType::Barline, 81));
    bl6_c.transform = Affine::translate((bar6_x, staff3_middle));
    bl6_c.add_child(bl6);
    root.add_child(bl6_c);

    // =========================================================================
    // SYSTEM 4: Beamed note groups
    // =========================================================================
    let staff4_y = staff3_y + staff_height + 80.0;
    let staff4_middle = staff4_y + 2.0 * spatium;

    root.add_child(SceneNode::anonymous_leaf(draw_staff_lines(
        content_x, staff4_y, content_width, spatium,
    )));

    // Treble clef
    let (_, clef4) = layout_clef(&ClefParams { id: 90, clef_type: ClefType::Treble, ..Default::default() }, &ctx);
    let mut clef4_c = SceneNode::group(SemanticId::new(ElementType::Clef, 90));
    clef4_c.transform = Affine::translate((clef_x, staff4_middle));
    clef4_c.add_child(clef4);
    root.add_child(clef4_c);

    // Time sig 4/4
    let (_, ts4) = layout_timesig(&TimeSigParams {
        id: 91,
        sig_type: TimeSigType::Numeric { numerator: 4, denominator: 4 },
        ..Default::default()
    }, &ctx);
    let mut ts4_c = SceneNode::group(SemanticId::new(ElementType::TimeSignature, 91));
    ts4_c.transform = Affine::translate((clef_x + spatium * 4.0, staff4_middle));
    ts4_c.add_child(ts4);
    root.add_child(ts4_c);

    // Beamed eighth notes (group of 4)
    let beam1_start = clef_x + spatium * 7.0;
    let beam_note_spacing = spatium * 2.5;

    // First beam group: 4 eighth notes descending
    let beam1_notes = vec![
        BeamNote { x: beam1_start, line: -2, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
        BeamNote { x: beam1_start + beam_note_spacing, line: -1, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
        BeamNote { x: beam1_start + beam_note_spacing * 2.0, line: 0, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
        BeamNote { x: beam1_start + beam_note_spacing * 3.0, line: 1, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
    ];

    // Draw noteheads for beam group 1 (noteheads only - beam draws stems)
    for (i, bn) in beam1_notes.iter().enumerate() {
        let (_, note) = layout_note(&NoteParams {
            id: 300 + i as u64,
            duration: NoteDuration::Eighth,
            line: bn.line,
            accidental: Accidental::None,
            ..Default::default()
        }, &ctx);
        let mut note_c = SceneNode::group(SemanticId::new(ElementType::Note, 300 + i as u64));
        note_c.transform = Affine::translate((bn.x, staff4_middle));
        note_c.add_child(note);
        root.add_child(note_c);
    }

    // Draw beam for group 1
    let beam_config = BeamLayoutConfig::default();
    let beam1_result = layout_beam(&beam1_notes, spatium, &beam_config);
    let mut beam1_node = SceneNode::anonymous_leaf(beam1_result.commands);
    beam1_node.transform = Affine::translate((0.0, staff4_middle));
    root.add_child(beam1_node);

    // Barline after first beam group
    let bar7_x = beam1_start + beam_note_spacing * 4.0;
    let (_, bl7) = layout_barline(&BarlineParams { id: 92, barline_type: BarlineType::Single, ..Default::default() }, &ctx);
    let mut bl7_c = SceneNode::group(SemanticId::new(ElementType::Barline, 92));
    bl7_c.transform = Affine::translate((bar7_x, staff4_middle));
    bl7_c.add_child(bl7);
    root.add_child(bl7_c);

    // Second beam group: 4 sixteenth notes (2 beams)
    let beam2_start = bar7_x + spatium * 2.0;
    let beam2_notes = vec![
        BeamNote { x: beam2_start, line: 0, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Down },
        BeamNote { x: beam2_start + beam_note_spacing * 0.6, line: -1, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Down },
        BeamNote { x: beam2_start + beam_note_spacing * 1.2, line: -2, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Down },
        BeamNote { x: beam2_start + beam_note_spacing * 1.8, line: -3, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Down },
    ];

    // Draw noteheads for beam group 2 (noteheads only - beam draws stems)
    for (i, bn) in beam2_notes.iter().enumerate() {
        let (_, note) = layout_note(&NoteParams {
            id: 310 + i as u64,
            duration: NoteDuration::Sixteenth,
            line: bn.line,
            accidental: Accidental::None,
            ..Default::default()
        }, &ctx);
        let mut note_c = SceneNode::group(SemanticId::new(ElementType::Note, 310 + i as u64));
        note_c.transform = Affine::translate((bn.x, staff4_middle));
        note_c.add_child(note);
        root.add_child(note_c);
    }

    // Draw beam for group 2
    let beam2_result = layout_beam(&beam2_notes, spatium, &beam_config);
    let mut beam2_node = SceneNode::anonymous_leaf(beam2_result.commands);
    beam2_node.transform = Affine::translate((0.0, staff4_middle));
    root.add_child(beam2_node);

    // Third beam group: Mixed rhythms (eighth + 2 sixteenths + eighth)
    let beam3_start = beam2_start + beam_note_spacing * 3.0;
    let beam3_notes = vec![
        BeamNote { x: beam3_start, line: 2, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
        BeamNote { x: beam3_start + beam_note_spacing * 0.5, line: 1, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Up },
        BeamNote { x: beam3_start + beam_note_spacing * 0.8, line: 0, duration: NoteDuration::Sixteenth, stem_direction: StemDirection::Up },
        BeamNote { x: beam3_start + beam_note_spacing * 1.5, line: -1, duration: NoteDuration::Eighth, stem_direction: StemDirection::Up },
    ];

    // Draw noteheads for beam group 3 (noteheads only - beam draws stems)
    for (i, bn) in beam3_notes.iter().enumerate() {
        let dur = bn.duration;
        let (_, note) = layout_note(&NoteParams {
            id: 320 + i as u64,
            duration: dur,
            line: bn.line,
            accidental: Accidental::None,
            ..Default::default()
        }, &ctx);
        let mut note_c = SceneNode::group(SemanticId::new(ElementType::Note, 320 + i as u64));
        note_c.transform = Affine::translate((bn.x, staff4_middle));
        note_c.add_child(note);
        root.add_child(note_c);
    }

    // Draw beam for group 3
    let beam3_result = layout_beam(&beam3_notes, spatium, &beam_config);
    let mut beam3_node = SceneNode::anonymous_leaf(beam3_result.commands);
    beam3_node.transform = Affine::translate((0.0, staff4_middle));
    root.add_child(beam3_node);

    // End barline
    let bar8_x = beam3_start + beam_note_spacing * 2.5;
    let (_, bl8) = layout_barline(&BarlineParams { id: 93, barline_type: BarlineType::End, ..Default::default() }, &ctx);
    let mut bl8_c = SceneNode::group(SemanticId::new(ElementType::Barline, 93));
    bl8_c.transform = Affine::translate((bar8_x, staff4_middle));
    bl8_c.add_child(bl8);
    root.add_child(bl8_c);

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
        (content_x, staff1_y - 12.0, "System 1: Melody with varied rhythms (4/4, D major)"),
        (content_x, staff2_y - 12.0, "System 2: Chord voicings (triads, 7ths, accidentals)"),
        (content_x, staff3_y - 12.0, "System 3: Lyrics (Amazing Grace, 3/4)"),
        (content_x, staff4_y - 12.0, "System 4: Beamed note groups (8ths, 16ths, mixed)"),
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
