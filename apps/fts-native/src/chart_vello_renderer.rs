//! Vello-based chart renderer using shared device.
//!
//! Uses the engraver's ChartLayoutEngine to build scene nodes,
//! then VelloSceneRenderer to render them to the screen.
//!
//! Interaction model (matches engraver layout_demo):
//! - Scroll = zoom about cursor position
//! - Click and drag = pan
//! - Pinch gesture = zoom about center
//! - R/Space = reset view
//! - F = toggle FPS overlay

use std::collections::VecDeque;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;
use std::time::Instant;

use anyrender_vello::{CustomPaintCtx, CustomPaintSource, TextureHandle};
use engraver::fonts::SMuFLFont;
use engraver::renderer::scene_renderer::{SceneRenderBuilder, VelloSceneRenderer};
use engraver::style::MStyle;
use keyflow::Chart;
use kurbo::{Affine, Point, Rect};
use vello::peniko::{Blob, FontData};
use skrifa::prelude::LocationRef;
use skrifa::raw::FileRef;
use skrifa::MetadataProvider;
use vello::peniko::{Color, Fill};
use vello::{AaConfig, AaSupport, Glyph, RenderParams, Renderer as VelloRenderer, RendererOptions};
use wgpu::{Device, Extent3d, Queue, TextureDescriptor, TextureDimension, TextureUsages};
use wgpu_context::DeviceHandle;

use crate::chart_layout::{ChartLayoutEngine, ChartLayoutResult, LayoutMode};

// Embedded fonts from charts package
static BRAVURA_FONT: &[u8] = include_bytes!("../../../packages/charts/resources/fonts/musescore/fonts/bravura/Bravura.otf");
static BRAVURA_METADATA: &[u8] = include_bytes!("../../../packages/charts/resources/fonts/musescore/fonts/bravura/bravura_metadata.json");
static TEXT_FONT: &[u8] = include_bytes!("../../../packages/charts/resources/fonts/musescore/fonts/FreeSans.ttf");
static MUSEJAZZ_TEXT_FONT: &[u8] = include_bytes!("../../../packages/charts/resources/fonts/musescore/fonts/musejazz/MuseJazzText.otf");

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

/// Messages for controlling the chart renderer
#[derive(Debug, Clone)]
pub enum ChartMessage {
    /// Update the chart to render
    UpdateChart(Option<Chart>),
    /// Mouse wheel zoom (scroll delta in pixels, cursor position)
    MouseWheel { delta_y: f64, cursor_x: f64, cursor_y: f64 },
    /// Pinch gesture zoom (delta, center position)
    PinchZoom { delta: f64, center_x: f64, center_y: f64 },
    /// Track mouse position for zoom-about-cursor and drag
    MouseMove { x: f64, y: f64 },
    /// Mouse button state (for drag-to-pan)
    MouseButton { pressed: bool },
    /// Reset view to default
    ResetView,
    /// Toggle FPS overlay
    ToggleFps,
    /// Keyboard zoom (+/- keys)
    KeyboardZoom { zoom_in: bool, center_x: f64, center_y: f64 },
    /// Fit single page to viewport
    FitPage { viewport_width: f64, viewport_height: f64 },
    /// Go to next page
    NextPage,
    /// Go to previous page
    PrevPage,
    /// Go to specific page
    GoToPage(u32),
}

/// FPS statistics
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

/// Chart renderer using shared device
pub struct ChartVelloPaintSource {
    state: RendererState,
    sender: Sender<ChartMessage>,
    receiver: Receiver<ChartMessage>,
    // Chart state
    current_chart: Option<Chart>,
    layout_result: Option<ChartLayoutResult>,
    layout_mode: LayoutMode,
    needs_relayout: bool,
    // View transform (combined pan/zoom/DPI scale) - Vello example style
    transform: Affine,
    // Mouse state for drag-to-pan
    mouse_down: bool,
    prior_position: Option<Point>,
    // FPS tracking
    last_frame_time: Instant,
    frame_times: VecDeque<f64>,
    show_fps: bool,
    // Page navigation
    current_page: u32,
    total_pages: u32,
    // Viewport size for fit-to-page
    last_viewport: (u32, u32),
}

enum RendererState {
    Suspended,
    Active(Box<ActiveRenderer>),
}

#[derive(Clone)]
struct TextureAndHandle {
    texture: wgpu::Texture,
    handle: TextureHandle,
}

struct ActiveRenderer {
    device: Device,
    queue: Queue,
    vello_renderer: VelloRenderer,
    displayed_texture: Option<TextureAndHandle>,
    next_texture: Option<TextureAndHandle>,
    scene: vello::Scene,
    // Font data for rendering
    smufl_font: SMuFLFont<'static>,
    text_font_data: Arc<Vec<u8>>,
    musejazz_font_data: Arc<Vec<u8>>,
    // Layout engine
    layout_engine: ChartLayoutEngine,
}

impl ChartVelloPaintSource {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        // Initial transform: translate to show content, then apply DPI scale
        let initial_transform = Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);

        Self {
            state: RendererState::Suspended,
            sender,
            receiver,
            current_chart: None,
            layout_result: None,
            layout_mode: LayoutMode::default(), // Paginated with Letter size
            needs_relayout: true,
            transform: initial_transform,
            mouse_down: false,
            prior_position: None,
            last_frame_time: Instant::now(),
            frame_times: VecDeque::with_capacity(100),
            show_fps: true, // Show FPS by default
            current_page: 1,
            total_pages: 1,
            last_viewport: (0, 0),
        }
    }

    pub fn sender(&self) -> Sender<ChartMessage> {
        self.sender.clone()
    }

    fn process_messages(&mut self) {
        const BASE: f64 = 1.05; // 5% per scroll increment
        const PIXELS_PER_LINE: f64 = 20.0;

        while let Ok(msg) = self.receiver.try_recv() {
            match msg {
                ChartMessage::UpdateChart(chart) => {
                    if self.current_chart.as_ref().map(|c| format!("{c:?}"))
                        != chart.as_ref().map(|c| format!("{c:?}"))
                    {
                        self.current_chart = chart;
                        self.needs_relayout = true;
                    }
                }
                ChartMessage::MouseWheel { delta_y, .. } => {
                    // Zoom about cursor position (Vello example style)
                    // Use prior_position (last known cursor pos) like engraver example
                    if let Some(cursor) = self.prior_position {
                        // Negate delta: scroll up (negative delta) = zoom in
                        let exponent = -delta_y / PIXELS_PER_LINE;
                        self.transform = self.transform.then_scale_about(BASE.powf(exponent), cursor);
                    }
                }
                ChartMessage::PinchZoom { delta, center_x, center_y } => {
                    // Pinch to zoom about center
                    let center = Point::new(center_x, center_y);
                    self.transform = self.transform.then_scale_about(1.0 + delta, center);
                }
                ChartMessage::MouseMove { x, y } => {
                    let position = Point::new(x, y);
                    // Handle drag-to-pan when mouse is held down
                    if self.mouse_down {
                        if let Some(prior) = self.prior_position {
                            let delta = position - prior;
                            self.transform = self.transform.then_translate(delta);
                        }
                    }
                    self.prior_position = Some(position);
                }
                ChartMessage::MouseButton { pressed } => {
                    self.mouse_down = pressed;
                }
                ChartMessage::ResetView => {
                    self.transform = Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);
                }
                ChartMessage::ToggleFps => {
                    self.show_fps = !self.show_fps;
                }
                ChartMessage::KeyboardZoom { zoom_in, center_x, center_y } => {
                    let center = Point::new(center_x, center_y);
                    let scale = if zoom_in { 1.1 } else { 1.0 / 1.1 };
                    self.transform = self.transform.then_scale_about(scale, center);
                }
                ChartMessage::FitPage { viewport_width, viewport_height } => {
                    self.last_viewport = (viewport_width as u32, viewport_height as u32);
                    // Calculate transform to fit page in viewport
                    if let Some(ref result) = self.layout_result {
                        if !result.pages.is_empty() {
                            let page = &result.pages[0];
                            let scale_x = viewport_width / page.width;
                            let scale_y = viewport_height / page.height;
                            let scale = scale_x.min(scale_y) * 0.9; // 90% to add margins
                            let offset_x = (viewport_width - page.width * scale) / 2.0;
                            let offset_y = (viewport_height - page.height * scale) / 2.0;
                            self.transform = Affine::translate((offset_x, offset_y)) * Affine::scale(scale);
                        }
                    }
                }
                ChartMessage::NextPage => {
                    if self.current_page < self.total_pages {
                        self.current_page += 1;
                    }
                }
                ChartMessage::PrevPage => {
                    if self.current_page > 1 {
                        self.current_page -= 1;
                    }
                }
                ChartMessage::GoToPage(page) => {
                    self.current_page = page.clamp(1, self.total_pages);
                }
            }
        }
    }
}

impl Default for ChartVelloPaintSource {
    fn default() -> Self {
        Self::new()
    }
}

impl CustomPaintSource for ChartVelloPaintSource {
    fn resume(&mut self, device_handle: &DeviceHandle) {
        log::info!("ChartVelloPaintSource resuming with shared device");

        // Clone device and queue from the shared DeviceHandle
        let device = device_handle.device.clone();
        let queue = device_handle.queue.clone();

        // Create VelloRenderer using the shared device
        let vello_renderer = VelloRenderer::new(
            &device,
            RendererOptions {
                use_cpu: false,
                num_init_threads: std::num::NonZeroUsize::new(1),
                antialiasing_support: AaSupport::area_only(),
                pipeline_cache: None,
            },
        )
        .expect("Failed to create VelloRenderer");

        // Load SMuFL font using from_reader with cursor for metadata
        let smufl_font = SMuFLFont::from_reader(BRAVURA_FONT, std::io::Cursor::new(BRAVURA_METADATA))
            .expect("Failed to load Bravura font");

        // Create font data arcs
        let text_font_data = Arc::new(TEXT_FONT.to_vec());
        let musejazz_font_data = Arc::new(MUSEJAZZ_TEXT_FONT.to_vec());

        // Create layout engine with static style
        let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
        let layout_engine = ChartLayoutEngine::new(
            style,
            text_font_data.clone(),
            musejazz_font_data.clone(),
        );

        self.state = RendererState::Active(Box::new(ActiveRenderer {
            device,
            queue,
            vello_renderer,
            displayed_texture: None,
            next_texture: None,
            scene: vello::Scene::new(),
            smufl_font,
            text_font_data,
            musejazz_font_data,
            layout_engine,
        }));

        self.needs_relayout = true;
        log::info!("ChartVelloPaintSource resumed successfully");
    }

    fn suspend(&mut self) {
        self.state = RendererState::Suspended;
        self.layout_result = None;
        log::info!("ChartVelloPaintSource suspended");
    }

    fn render(
        &mut self,
        ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        _scale: f64,
    ) -> Option<TextureHandle> {
        if width == 0 || height == 0 {
            return None;
        }

        // Track frame time
        let now = Instant::now();
        let frame_time_ms = now.duration_since(self.last_frame_time).as_secs_f64() * 1000.0;
        self.last_frame_time = now;

        // Add to sliding window (max 100 samples)
        if self.frame_times.len() >= 100 {
            self.frame_times.pop_front();
        }
        self.frame_times.push_back(frame_time_ms);

        // Process incoming messages
        self.process_messages();

        let RendererState::Active(state) = &mut self.state else {
            return None;
        };

        // Relayout if needed
        if self.needs_relayout {
            if let Some(ref chart) = self.current_chart {
                let result = state.layout_engine.layout_chart(chart, &self.layout_mode);
                // Update total pages from layout result
                self.total_pages = result.pages.len().max(1) as u32;
                self.current_page = self.current_page.clamp(1, self.total_pages);
                self.layout_result = Some(result);
            } else {
                self.layout_result = None;
                self.total_pages = 1;
                self.current_page = 1;
            }
            self.needs_relayout = false;
        }

        // Track viewport size for page fitting
        self.last_viewport = (width, height);

        state.render(
            ctx,
            width,
            height,
            &self.transform,
            self.layout_result.as_ref(),
            self.show_fps,
            &self.frame_times,
        )
    }
}

impl ActiveRenderer {
    fn render(
        &mut self,
        mut ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        transform: &Affine,
        layout_result: Option<&ChartLayoutResult>,
        show_fps: bool,
        frame_times: &VecDeque<f64>,
    ) -> Option<TextureHandle> {
        // Handle texture resizing
        if self
            .next_texture
            .as_ref()
            .is_some_and(|tex| tex.texture.width() != width || tex.texture.height() != height)
        {
            let handle = self.next_texture.take().unwrap().handle;
            ctx.unregister_texture(handle);
        }

        // Create texture if needed
        let texture_and_handle = match &self.next_texture {
            Some(next) => next,
            None => {
                let texture = create_texture(&self.device, width, height);
                let handle = ctx.register_texture(texture.clone());
                self.next_texture = Some(TextureAndHandle { texture, handle });
                self.next_texture.as_ref().unwrap()
            }
        };

        let next_texture = &texture_and_handle.texture;
        let next_texture_handle = texture_and_handle.handle.clone();

        // Build Vello scene
        self.scene.reset();

        // Dark gray canvas background (page view style)
        self.scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(64, 64, 64),
            None,
            &Rect::new(0.0, 0.0, width as f64, height as f64),
        );

        // Render chart if we have layout
        if let Some(layout) = layout_result {
            // Create scene renderer with fonts
            let mut renderer = SceneRenderBuilder::new()
                .spatium(5.0)
                .build()
                .with_font(&self.smufl_font)
                .with_text_font_arc(self.text_font_data.clone())
                .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone());

            // Apply view transform to scene
            let mut transformed_scene = layout.scene.clone();
            transformed_scene.transform = *transform;

            // Render scene nodes to Vello scene
            renderer.render(&mut self.scene, &transformed_scene);
        } else {
            // No chart - show placeholder message
            draw_text(
                &mut self.scene,
                &self.text_font_data,
                "No chart to display",
                width as f64 / 2.0 - 80.0,
                height as f64 / 2.0,
                18.0,
                Color::from_rgb8(150, 150, 150),
            );
        }

        // Draw FPS overlay (in screen pixel coordinates)
        if show_fps {
            let stats = FpsStats::from_samples(frame_times);
            draw_fps_overlay(
                &mut self.scene,
                &self.text_font_data,
                width as f64,
                height as f64,
                &stats,
                frame_times,
            );
        }

        // Render Vello scene to texture
        let texture_view = next_texture.create_view(&wgpu::TextureViewDescriptor::default());
        self.vello_renderer
            .render_to_texture(
                &self.device,
                &self.queue,
                &self.scene,
                &texture_view,
                &RenderParams {
                    base_color: Color::WHITE,
                    width,
                    height,
                    antialiasing_method: AaConfig::Area,
                },
            )
            .expect("Vello render failed");

        // Swap textures (double buffering)
        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);

        Some(next_texture_handle)
    }
}

fn create_texture(device: &Device, width: u32, height: u32) -> wgpu::Texture {
    device.create_texture(&TextureDescriptor {
        label: Some("Chart Vello Texture"),
        size: Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        // STORAGE_BINDING needed for Vello compute shaders
        usage: TextureUsages::STORAGE_BINDING
            | TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_SRC,
        view_formats: &[],
    })
}

/// Draw text using Vello's glyph drawing
fn draw_text(
    scene: &mut vello::Scene,
    text_font: &Arc<Vec<u8>>,
    text: &str,
    x: f64,
    y: f64,
    size: f32,
    color: Color,
) {
    let font_data = FontData::new(Blob::new(text_font.clone()), 0);

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
            .transform(Affine::translate((x, y)))
            .brush(color)
            .draw(Fill::NonZero, glyphs.into_iter());
    }
}

/// Draw FPS overlay in the bottom-right corner of the screen.
fn draw_fps_overlay(
    scene: &mut vello::Scene,
    text_font: &Arc<Vec<u8>>,
    viewport_width: f64,
    viewport_height: f64,
    stats: &FpsStats,
    samples: &VecDeque<f64>,
) {
    let width = 340.0;
    let height = 240.0;
    let padding = 15.0;
    let x_offset = viewport_width - width - padding;
    let y_offset = viewport_height - height - padding;
    let offset = Affine::translate((x_offset, y_offset));

    // Semi-transparent background
    scene.fill(
        Fill::NonZero,
        offset,
        Color::BLACK.multiply_alpha(0.8),
        None,
        &Rect::new(0.0, 0.0, width, height),
    );

    // FPS color based on performance
    let fps_color = if stats.fps >= 60.0 {
        Color::from_rgb8(100, 255, 100)
    } else if stats.fps >= 30.0 {
        Color::from_rgb8(255, 200, 0)
    } else {
        Color::from_rgb8(255, 80, 80)
    };

    // Helper to draw text in overlay
    let font_data = FontData::new(Blob::new(text_font.clone()), 0);
    let draw_overlay_text = |scene: &mut vello::Scene, text: &str, x: f64, y: f64, size: f32, color: Color| {
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

    draw_overlay_text(scene, &format!("FPS: {:.1}", stats.fps), 15.0, 32.0, 28.0, fps_color);
    draw_overlay_text(scene, &format!("Frame: {:.2}ms", stats.frame_time_ms), 15.0, 56.0, 18.0, Color::WHITE);
    draw_overlay_text(scene, &format!("Min: {:.2}ms  Max: {:.2}ms", stats.min_ms, stats.max_ms), 15.0, 78.0, 14.0, Color::from_rgb8(180, 180, 180));
    draw_overlay_text(scene, "Scroll=Zoom | Drag=Pan | R=Reset", 15.0, 96.0, 12.0, Color::from_rgb8(120, 120, 120));

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
        Color::from_rgb8(100, 200, 100).multiply_alpha(0.5),
        None,
        &vello::kurbo::Line::new((0.0, 0.0), (graph_width, 0.0)),
    );

    let line_120fps = graph_height * (8.33 / max_time);
    scene.stroke(
        &stroke,
        offset * Affine::translate((15.0, graph_y + graph_height - line_120fps)),
        Color::from_rgb8(100, 143, 255).multiply_alpha(0.5),
        None,
        &vello::kurbo::Line::new((0.0, 0.0), (graph_width, 0.0)),
    );
}
