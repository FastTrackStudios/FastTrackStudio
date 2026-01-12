//! Chart Renderer - WGPU CustomPaintSource for chart visualization
//!
//! Renders a keyflow Chart using WGPU with sheet music style layout,
//! including SMuFL music notation symbols.
//!
//! Uses shared rendering primitives from engraver for 1:1 rendering
//! consistency with the music_symbols example.

use anyrender_vello::{CustomPaintCtx, CustomPaintSource, TextureHandle};
use engraver::fonts::{get_glyph_id, tessellate_glyph_to_ndc, Glyph, GlyphVertex, SMuFLMetadata};
use engraver::model::{compute_system_layout, LineBreakPolicy};
use engraver::renderer::{
    create_camera_bind_group_layout, create_line, create_main_pipeline, create_rect,
    create_sdf_pipeline, create_sdf_rounded_rect, CameraUniform, SdfRectVertex, Vertex,
};
use engraver::ui::{format_rehearsal_label, CapsuleLabelConfig, CapsuleLabelMode, ComputedCapsuleLabel};
use glyphon::{
    Attrs, Buffer as TextBuffer, Cache as TextCache, Color as TextColor, Family, FontSystem,
    Metrics, Resolution, Shaping, SwashCache, TextArea, TextAtlas, TextBounds, TextRenderer,
    Viewport, Weight,
};
use keyflow::Chart;
use skrifa::FontRef;
use std::io::Cursor;
use std::sync::mpsc::{channel, Receiver, Sender};
use wgpu::util::DeviceExt;
use wgpu_context::DeviceHandle;

// ============================================================================
// Embedded Font Resources
// ============================================================================

/// Leland font embedded at compile time (works with dx serve and cargo run)
static LELAND_FONT_DATA: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../libs/reference/sheet-music/musescore/fonts/leland/Leland.otf"
));

/// Leland metadata embedded at compile time
static LELAND_METADATA_JSON: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../libs/reference/sheet-music/fonts/leland/leland_metadata.json"
));

// ============================================================================
// Page Size System (modeled after MuseScore)
// ============================================================================

/// Millimeters per inch (for conversion)
const MM_PER_INCH: f32 = 25.4;

/// Screen DPI for rendering
const SCREEN_DPI: f32 = 96.0;

/// Page dimensions in inches
#[derive(Debug, Clone, Copy)]
pub struct PageSize {
    pub width: f32,
    pub height: f32,
}

impl PageSize {
    /// US Letter: 8.5" x 11"
    pub const LETTER: Self = Self {
        width: 8.5,
        height: 11.0,
    };

    /// Get width in pixels at given DPI
    pub fn width_px(&self, dpi: f32) -> f32 {
        self.width * dpi
    }

    /// Get height in pixels at given DPI
    pub fn height_px(&self, dpi: f32) -> f32 {
        self.height * dpi
    }
}

/// Page margins in inches
#[derive(Debug, Clone, Copy)]
pub struct PageMargins {
    pub top: f32,
    pub bottom: f32,
    pub left: f32,
    pub right: f32,
}

impl PageMargins {
    /// Default margins: 0.5" all around
    pub const DEFAULT: Self = Self {
        top: 0.5,
        bottom: 0.5,
        left: 0.5,
        right: 0.5,
    };
}

/// Page style combining size, margins, and layout options
#[derive(Debug, Clone, Copy)]
pub struct PageStyle {
    pub size: PageSize,
    pub margins: PageMargins,
    /// Spatium (staff space) in millimeters
    pub spatium_mm: f32,
}

impl Default for PageStyle {
    fn default() -> Self {
        Self {
            size: PageSize::LETTER,
            margins: PageMargins::DEFAULT,
            spatium_mm: 1.764,
        }
    }
}

impl PageStyle {
    /// Get spatium in pixels at given DPI
    pub fn spatium_px(&self, dpi: f32) -> f32 {
        (self.spatium_mm / MM_PER_INCH) * dpi
    }
}

// ============================================================================
// Messages and Paint Source
// ============================================================================

/// Messages to update the chart display
pub enum ChartMessage {
    UpdateChart(Chart),
    /// Zoom at cursor position (delta is scroll amount, positive = zoom in)
    Zoom {
        delta: f32,
        cursor_x: f32,
        cursor_y: f32,
    },
    /// Pan by delta pixels
    Pan { dx: f32, dy: f32 },
    /// Update cursor position for hover-based zoom
    CursorMove { x: f32, y: f32 },
    /// Reset view to default
    ResetView,
}

/// View state for zoom and pan
#[derive(Debug, Clone)]
pub struct ViewState {
    pub zoom: f32,
    pub pan_x: f32,
    pub pan_y: f32,
    pub cursor_x: f32,
    pub cursor_y: f32,
}

impl Default for ViewState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            pan_x: 0.0,
            pan_y: 0.0,
            cursor_x: 0.0,
            cursor_y: 0.0,
        }
    }
}

impl ViewState {
    pub fn reset(&mut self) {
        self.zoom = 1.0;
        self.pan_x = 0.0;
        self.pan_y = 0.0;
    }

    /// Zoom centered on cursor position
    pub fn zoom_at(&mut self, cursor_x: f32, cursor_y: f32, delta: f32, width: f32, height: f32) {
        let old_zoom = self.zoom;

        // Calculate zoom factor
        let zoom_factor = 1.0 + delta * 0.1;
        self.zoom = (self.zoom * zoom_factor).clamp(0.1, 4.0);

        // Convert cursor to normalized coordinates (-1 to 1)
        let cursor_ndc_x = (cursor_x / width) * 2.0 - 1.0;
        let cursor_ndc_y = 1.0 - (cursor_y / height) * 2.0;

        // Adjust pan to keep cursor position stable
        let zoom_ratio = self.zoom / old_zoom;
        self.pan_x = cursor_ndc_x - (cursor_ndc_x - self.pan_x) * zoom_ratio;
        self.pan_y = cursor_ndc_y - (cursor_ndc_y - self.pan_y) * zoom_ratio;
    }

    /// Pan via scroll wheel (in pixels)
    pub fn scroll_pan(&mut self, dx: f32, dy: f32, width: f32, height: f32) {
        self.pan_x += (dx / width * 2.0) / self.zoom;
        self.pan_y -= (dy / height * 2.0) / self.zoom;
    }
}

/// WGPU Paint source for chart rendering
pub struct ChartPaintSource {
    sender: Sender<ChartMessage>,
    receiver: Receiver<ChartMessage>,
    state: ChartRendererState,
    current_chart: Option<Chart>,
    page_style: PageStyle,
    view_state: ViewState,
    canvas_size: (u32, u32),
}

enum ChartRendererState {
    Active(Box<ActiveChartRenderer>),
    Suspended,
}

impl ChartPaintSource {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            sender,
            receiver,
            state: ChartRendererState::Suspended,
            current_chart: None,
            page_style: PageStyle::default(),
            view_state: ViewState::default(),
            canvas_size: (800, 600),
        }
    }

    pub fn sender(&self) -> Sender<ChartMessage> {
        self.sender.clone()
    }

    fn process_messages(&mut self) {
        let (width, height) = self.canvas_size;
        while let Ok(msg) = self.receiver.try_recv() {
            match msg {
                ChartMessage::UpdateChart(chart) => {
                    self.current_chart = Some(chart);
                }
                ChartMessage::Zoom {
                    delta,
                    cursor_x,
                    cursor_y,
                } => {
                    self.view_state
                        .zoom_at(cursor_x, cursor_y, delta, width as f32, height as f32);
                }
                ChartMessage::Pan { dx, dy } => {
                    self.view_state
                        .scroll_pan(dx, dy, width as f32, height as f32);
                }
                ChartMessage::CursorMove { x, y } => {
                    self.view_state.cursor_x = x;
                    self.view_state.cursor_y = y;
                }
                ChartMessage::ResetView => {
                    self.view_state.reset();
                }
            }
        }
    }
}

impl Default for ChartPaintSource {
    fn default() -> Self {
        Self::new()
    }
}

impl CustomPaintSource for ChartPaintSource {
    fn resume(&mut self, device_handle: &DeviceHandle) {
        let active_state = ActiveChartRenderer::new(device_handle);
        self.state = ChartRendererState::Active(Box::new(active_state));
    }

    fn suspend(&mut self) {
        self.state = ChartRendererState::Suspended;
    }

    fn render(
        &mut self,
        ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        _scale: f64,
    ) -> Option<TextureHandle> {
        // Update canvas size for message processing
        self.canvas_size = (width, height);
        self.process_messages();

        if width == 0 || height == 0 {
            return None;
        }

        let ChartRendererState::Active(state) = &mut self.state else {
            return None;
        };

        state.render(
            ctx,
            width,
            height,
            self.current_chart.as_ref(),
            &self.page_style,
            &self.view_state,
        )
    }
}

// ============================================================================
// Loaded Font
// ============================================================================

/// Loaded SMuFL font with metadata
struct LoadedFont {
    font_data: Vec<u8>,
    metadata: Option<SMuFLMetadata>,
}

impl LoadedFont {
    fn load() -> Option<Self> {
        // Use embedded font data (works with both cargo run and dx serve)
        let font_data = LELAND_FONT_DATA.to_vec();

        // Parse embedded metadata JSON
        let metadata = SMuFLMetadata::from_reader(Cursor::new(LELAND_METADATA_JSON)).ok();

        Some(Self {
            font_data,
            metadata,
        })
    }

    fn font_ref(&self) -> Option<FontRef<'_>> {
        FontRef::new(&self.font_data).ok()
    }

    /// Get glyph width in pixels
    fn glyph_width(&self, glyph: Glyph, staff_space: f32) -> f32 {
        self.metadata
            .as_ref()
            .and_then(|m| m.bounding_boxes.get(glyph))
            .map(|bb| (f64::from(bb.ne.x()) - f64::from(bb.sw.x())) as f32 * staff_space)
            .unwrap_or(staff_space)
    }

    /// Get the stem-up anchor point for a glyph
    fn stem_up_se(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.as_ref()?.anchors.get(glyph)?;
        let coord = anchors.stem_up_se?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }

    /// Get the stem-down anchor point for a glyph
    fn stem_down_nw(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.as_ref()?.anchors.get(glyph)?;
        let coord = anchors.stem_down_nw?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }
}

/// Helper to convert GlyphVertex to local Vertex type
fn glyph_vertices_to_vertices(glyph_vertices: Vec<GlyphVertex>) -> Vec<Vertex> {
    glyph_vertices
        .into_iter()
        .map(|gv| Vertex {
            position: gv.position,
            color: gv.color,
        })
        .collect()
}

// ============================================================================
// Active Renderer
// ============================================================================

struct TextureAndHandle {
    texture: wgpu::Texture,
    handle: TextureHandle,
}

struct ActiveChartRenderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    pipeline: wgpu::RenderPipeline,
    sdf_pipeline: wgpu::RenderPipeline,
    camera_bind_group_layout: wgpu::BindGroupLayout,
    displayed_texture: Option<TextureAndHandle>,
    next_texture: Option<TextureAndHandle>,
    // Text rendering
    font_system: FontSystem,
    swash_cache: SwashCache,
    text_cache: TextCache,
    text_atlas: TextAtlas,
    text_renderer: TextRenderer,
    viewport: Viewport,
    // SMuFL font
    loaded_font: Option<LoadedFont>,
}

impl ActiveChartRenderer {
    fn new(device_handle: &DeviceHandle) -> Self {
        let device = &device_handle.device;
        let queue = &device_handle.queue;
        // Use the same format as the surface - this is crucial for correct rendering
        let format = wgpu::TextureFormat::Rgba8Unorm;

        // Create camera bind group layout using shared function
        let camera_bind_group_layout = create_camera_bind_group_layout(device);

        // Create main pipeline using shared function
        let pipeline = create_main_pipeline(device, format, &camera_bind_group_layout);

        // Create SDF pipeline using shared function
        let sdf_pipeline = create_sdf_pipeline(device, format, &camera_bind_group_layout);

        // Initialize text rendering
        let font_system = FontSystem::new();
        let swash_cache = SwashCache::new();
        let text_cache = TextCache::new(device);
        let text_atlas = TextAtlas::new(device, queue, &text_cache, format);
        let mut text_atlas = text_atlas;
        let text_renderer =
            TextRenderer::new(&mut text_atlas, device, wgpu::MultisampleState::default(), None);
        let viewport = Viewport::new(device, &text_cache);

        // Load SMuFL font (embedded at compile time)
        let loaded_font = LoadedFont::load();
        if loaded_font.is_some() {
            log::info!("Loaded embedded Leland font successfully");
        } else {
            log::warn!("Failed to parse embedded Leland font");
        }

        Self {
            device: device.clone(),
            queue: queue.clone(),
            pipeline,
            sdf_pipeline,
            camera_bind_group_layout,
            displayed_texture: None,
            next_texture: None,
            font_system,
            swash_cache,
            text_cache,
            text_atlas,
            text_renderer,
            viewport,
            loaded_font,
        }
    }

    fn render(
        &mut self,
        mut ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        chart: Option<&Chart>,
        page_style: &PageStyle,
        view_state: &ViewState,
    ) -> Option<TextureHandle> {
        // Handle texture management
        if self
            .next_texture
            .as_ref()
            .is_some_and(|tex| tex.texture.width() != width || tex.texture.height() != height)
        {
            let handle = self.next_texture.take().unwrap().handle;
            ctx.unregister_texture(handle);
        }

        if self.next_texture.is_none() {
            let texture = create_texture(&self.device, width, height);
            let handle = ctx.register_texture(texture.clone());
            self.next_texture = Some(TextureAndHandle { texture, handle });
        }

        // Build scene geometry using shared primitives
        let (vertices, sdf_vertices, text_info) =
            self.build_scene(chart, width, height, page_style);

        let texture_and_handle = self.next_texture.as_ref().unwrap();
        let next_texture = &texture_and_handle.texture;
        let next_texture_handle = texture_and_handle.handle.clone();

        // Create camera uniform buffer using shared type (with resolution for SDF shader)
        let camera_uniform = CameraUniform::with_resolution(
            view_state.zoom,
            view_state.pan_x,
            view_state.pan_y,
            width as f32,
            height as f32,
        );
        let camera_buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Camera Uniform Buffer"),
                contents: bytemuck::cast_slice(&[camera_uniform]),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });

        let camera_bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Camera Bind Group"),
            layout: &self.camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: camera_buffer.as_entire_binding(),
            }],
        });

        // Create vertex buffers
        let vertex_buffer = if !vertices.is_empty() {
            Some(
                self.device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Chart Vertex Buffer"),
                        contents: bytemuck::cast_slice(&vertices),
                        usage: wgpu::BufferUsages::VERTEX,
                    }),
            )
        } else {
            None
        };

        let sdf_vertex_buffer = if !sdf_vertices.is_empty() {
            Some(
                self.device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("SDF Vertex Buffer"),
                        contents: bytemuck::cast_slice(&sdf_vertices),
                        usage: wgpu::BufferUsages::VERTEX,
                    }),
            )
        } else {
            None
        };

        // Update viewport
        self.viewport
            .update(&self.queue, Resolution { width, height });

        // Helper to transform pixel coordinates by camera (zoom + pan)
        // Converts: pixel -> NDC -> apply camera -> back to pixel
        let transform_by_camera = |px: f32, py: f32| -> (f32, f32) {
            let w = width as f32;
            let h = height as f32;

            // Convert pixel to NDC (-1 to 1)
            let ndc_x = (px / w) * 2.0 - 1.0;
            let ndc_y = 1.0 - (py / h) * 2.0;

            // Apply camera transform: position * zoom + pan
            let transformed_x = ndc_x * view_state.zoom + view_state.pan_x;
            let transformed_y = ndc_y * view_state.zoom + view_state.pan_y;

            // Convert back to pixel coordinates
            let screen_x = (transformed_x + 1.0) / 2.0 * w;
            let screen_y = (1.0 - transformed_y) / 2.0 * h;

            (screen_x, screen_y)
        };

        // Prepare text areas with camera-transformed positions
        let text_areas: Vec<TextArea> = text_info
            .iter()
            .map(|(buffer, x, y, text_scale)| {
                let (tx, ty) = transform_by_camera(*x, *y);
                TextArea {
                    buffer,
                    left: tx,
                    top: ty,
                    scale: view_state.zoom * text_scale, // Apply both zoom and text fitting scale
                    bounds: TextBounds {
                        left: 0,
                        top: 0,
                        right: width as i32,
                        bottom: height as i32,
                    },
                    default_color: TextColor::rgba(255, 0, 0, 255), // Red to match frame
                    custom_glyphs: &[],
                }
            })
            .collect();

        let _ = self.text_renderer.prepare(
            &self.device,
            &self.queue,
            &mut self.font_system,
            &mut self.text_atlas,
            &self.viewport,
            text_areas,
            &mut self.swash_cache,
        );

        let texture_view = next_texture.create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Chart Render Encoder"),
            });

        // Render pass
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Chart Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &texture_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        // Dark gray canvas background (same as music_symbols example)
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.2,
                            g: 0.2,
                            b: 0.22,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            // Draw main geometry (staff lines, notes, glyphs)
            if let Some(buffer) = &vertex_buffer {
                render_pass.set_pipeline(&self.pipeline);
                render_pass.set_bind_group(0, &camera_bind_group, &[]);
                render_pass.set_vertex_buffer(0, buffer.slice(..));
                render_pass.draw(0..vertices.len() as u32, 0..1);
            }

            // Draw SDF rounded rectangles (section labels)
            if let Some(buffer) = &sdf_vertex_buffer {
                render_pass.set_pipeline(&self.sdf_pipeline);
                render_pass.set_bind_group(0, &camera_bind_group, &[]);
                render_pass.set_vertex_buffer(0, buffer.slice(..));
                render_pass.draw(0..sdf_vertices.len() as u32, 0..1);
            }

            // Draw text
            let _ = self
                .text_renderer
                .render(&self.text_atlas, &self.viewport, &mut render_pass);
        }

        self.queue.submit(std::iter::once(encoder.finish()));

        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
        Some(next_texture_handle)
    }

    fn build_scene(
        &mut self,
        chart: Option<&Chart>,
        width: u32,
        height: u32,
        page_style: &PageStyle,
    ) -> (
        Vec<Vertex>,
        Vec<SdfRectVertex>,
        Vec<(TextBuffer, f32, f32, f32)>, // (buffer, x, y, scale)
    ) {
        let w = width as f32;
        let h = height as f32;

        let mut vertices = Vec::new();
        let mut sdf_vertices = Vec::new();
        let mut text_buffers = Vec::new();

        // Colors (same as music_symbols example)
        let black = [0.0, 0.0, 0.0, 1.0];
        let paper_white = [1.0, 1.0, 1.0, 1.0];
        let paper_shadow = [0.15, 0.15, 0.17, 1.0];
        let rehearsal_red = [1.0, 0.0, 0.0, 1.0];

        // Calculate page dimensions
        let page_width = page_style.size.width_px(SCREEN_DPI);
        let page_height = page_style.size.height_px(SCREEN_DPI);

        // Center the page in the canvas with padding
        let page_x = (w - page_width) / 2.0;
        let page_y = 20.0;

        // Draw paper shadow using shared primitive
        let shadow_offset = 4.0;
        vertices.extend(create_rect(
            page_x + shadow_offset,
            page_y + shadow_offset,
            page_width,
            page_height,
            paper_shadow,
            w,
            h,
        ));

        // Draw paper background using shared primitive
        vertices.extend(create_rect(
            page_x, page_y, page_width, page_height, paper_white, w, h,
        ));

        // Get margins and content area
        let margin_left_px = page_style.margins.left * SCREEN_DPI;
        let margin_top_px = page_style.margins.top * SCREEN_DPI;
        let margin_right_px = page_style.margins.right * SCREEN_DPI;

        let content_left = page_x + margin_left_px;
        let content_top = page_y + margin_top_px;
        let content_right = page_x + page_width - margin_right_px;
        let content_width = content_right - content_left;

        // Spatium-based measurements
        let staff_space = page_style.spatium_px(SCREEN_DPI);
        let system_height = staff_space * 4.0; // 5 lines = 4 spaces
        let system_spacing = staff_space * 8.0;
        let section_extra_spacing = staff_space * 2.0;
        let font_size = staff_space * 4.0; // Font size for SMuFL glyphs

        let Some(chart) = chart else {
            return (vertices, sdf_vertices, text_buffers);
        };

        // Extract section information from chart
        let total_measures: usize = chart.sections.iter().map(|s| s.measures.len()).sum();
        let mut section_starts = Vec::new();
        let mut section_labels: Vec<String> = Vec::new();
        let mut current_measure = 0;

        for section in &chart.sections {
            section_starts.push(current_measure);
            // Use format_rehearsal_label for proper formatting (INTRO, VS 1, CH 1, etc.)
            let label = format_rehearsal_label(
                &section.section.section_type.full_name(),
                &section.section.section_type.abbreviation(),
                section.section.number,
            );
            section_labels.push(label);
            current_measure += section.measures.len();
        }

        // Compute system layout (4 measures per line, breaking at sections)
        let layout = compute_system_layout(
            total_measures,
            &section_starts,
            &LineBreakPolicy::four_per_line(),
        );

        // For section label positioning in left margin
        let margin_padding_h = 2.0;
        let margin_capsule_width = margin_left_px - (margin_padding_h * 2.0);
        let margin_capsule_x = page_x + margin_padding_h;

        // Starting position
        let mut current_y = content_top + staff_space * 4.0;

        // Render each system
        for (sys_idx, sys_info) in layout.systems.iter().enumerate() {
            // Add extra spacing before section starts (except first)
            if sys_info.is_section_start && sys_idx > 0 {
                current_y += section_extra_spacing;
            }

            let staff_y = current_y;

            // Draw 5 staff lines using shared primitive
            for i in 0..5 {
                let y = staff_y + (i as f32) * staff_space;
                vertices.extend(create_line(content_left, y, content_right, y, 1.0, black, w, h));
            }

            // Draw barlines
            let measures_in_system = sys_info.measure_count;
            let measure_width = content_width / measures_in_system.max(1) as f32;

            for m in 0..=measures_in_system {
                let bar_x = content_left + (m as f32) * measure_width;
                let thickness = if m == 0 || m == measures_in_system {
                    2.0
                } else {
                    1.0
                };
                vertices.extend(create_line(
                    bar_x,
                    staff_y,
                    bar_x,
                    staff_y + system_height,
                    thickness,
                    black,
                    w,
                    h,
                ));
            }

            // Draw SMuFL symbols if font is loaded
            if let Some(ref loaded_font) = self.loaded_font {
                if let Some(font_ref) = loaded_font.font_ref() {
                    // G Clef at start of system
                    if let Some(gid) = get_glyph_id(&font_ref, Glyph::GClef) {
                        let clef_x = content_left + staff_space * 0.5;
                        let clef_y = staff_y + staff_space * 3.0; // G line
                        let clef_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                            &font_ref, gid, font_size, clef_x, clef_y, black, w, h,
                        ));
                        vertices.extend(clef_verts);
                    }

                    // Time signature only on first system
                    if sys_idx == 0 {
                        if let Some(gid) = get_glyph_id(&font_ref, Glyph::TimeSigCommon) {
                            let ts_x = content_left + staff_space * 4.5;
                            let ts_y = staff_y + staff_space * 2.0;
                            let ts_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                                &font_ref, gid, font_size, ts_x, ts_y, black, w, h,
                            ));
                            vertices.extend(ts_verts);
                        }
                    }

                    // Draw sample quarter notes in each measure
                    let stem_thickness = loaded_font
                        .metadata
                        .as_ref()
                        .and_then(|m| m.engraving_defaults.stem_thickness)
                        .map(|s| f64::from(s) as f32 * staff_space)
                        .unwrap_or(1.2);

                    let note_area_start = if sys_idx == 0 {
                        content_left + staff_space * 7.0
                    } else {
                        content_left + staff_space * 4.0
                    };

                    for m in 0..measures_in_system {
                        let measure_start = content_left + (m as f32) * measure_width;
                        let measure_end = measure_start + measure_width;

                        // Draw 4 quarter notes per measure
                        for beat in 0..4 {
                            let beat_offset = (beat as f32 + 0.5) / 4.0;
                            let note_x = measure_start.max(note_area_start)
                                + beat_offset
                                    * (measure_end - measure_start.max(note_area_start))
                                    * 0.8;

                            // Vary pitch based on measure and beat
                            let pitch_offset =
                                ((sys_info.start_measure + m + beat) % 7) as f32;
                            let staff_pos = 4.0 - pitch_offset * 0.5;
                            let note_y = staff_y + (4.0 - staff_pos) * staff_space;

                            // Draw notehead
                            if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadBlack) {
                                let nh_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                                    &font_ref, gid, font_size, note_x, note_y, black, w, h,
                                ));
                                vertices.extend(nh_verts);
                            }

                            // Draw stem using shared primitive
                            let stem_up = staff_pos > 2.0;
                            let stem_length = staff_space * 3.5;
                            let (stem_x, stem_attach_y) = if stem_up {
                                if let Some((ax, ay)) =
                                    loaded_font.stem_up_se(Glyph::NoteheadBlack, staff_space)
                                {
                                    (note_x + ax - stem_thickness / 2.0, note_y - ay)
                                } else {
                                    let nh_width =
                                        loaded_font.glyph_width(Glyph::NoteheadBlack, staff_space);
                                    (note_x + nh_width - stem_thickness / 2.0, note_y)
                                }
                            } else if let Some((ax, ay)) =
                                loaded_font.stem_down_nw(Glyph::NoteheadBlack, staff_space)
                            {
                                (note_x + ax + stem_thickness / 2.0, note_y - ay)
                            } else {
                                (note_x + stem_thickness / 2.0, note_y)
                            };

                            let stem_end_y = if stem_up {
                                stem_attach_y - stem_length
                            } else {
                                stem_attach_y + stem_length
                            };

                            vertices.extend(create_line(
                                stem_x,
                                stem_attach_y,
                                stem_x,
                                stem_end_y,
                                stem_thickness,
                                black,
                                w,
                                h,
                            ));
                        }
                    }
                }
            }

            // Add section label if this is a section start
            if sys_info.is_section_start {
                if let Some(section_idx) = section_starts
                    .iter()
                    .position(|&s| s == sys_info.start_measure)
                {
                    let section_label = &section_labels[section_idx];

                    // Create text buffer first to measure text width
                    let mut buffer =
                        TextBuffer::new(&mut self.font_system, Metrics::new(14.0, 18.0));
                    buffer.set_size(
                        &mut self.font_system,
                        Some(500.0), // Large initial size for measurement
                        Some(50.0),
                    );
                    buffer.set_text(
                        &mut self.font_system,
                        section_label,
                        &Attrs::new().family(Family::SansSerif).weight(Weight::BOLD),
                        Shaping::Advanced,
                    );
                    buffer.shape_until_scroll(&mut self.font_system, false);

                    // Measure text width
                    let measured_text_width: f32 = buffer
                        .layout_runs()
                        .map(|run| run.line_w)
                        .next()
                        .unwrap_or(50.0);

                    // Use CapsuleLabelConfig for proper text fitting
                    let label_config = CapsuleLabelConfig {
                        mode: CapsuleLabelMode::FixedWidth {
                            width: margin_capsule_width,
                            height: system_height - 6.0, // margin_padding_v * 2
                            internal_padding_h: 1.0,
                            internal_padding_v: 1.0,
                        },
                        font_size: 14.0,
                        line_height: 18.0,
                    };

                    // Compute label with text fitting
                    let computed = ComputedCapsuleLabel::compute(
                        section_label,
                        margin_capsule_x,
                        staff_y + 3.0, // margin_padding_v
                        measured_text_width,
                        &label_config,
                    );

                    // Add SDF rounded rectangle for section label using shared primitive
                    sdf_vertices.extend(create_sdf_rounded_rect(
                        computed.capsule_x,
                        computed.capsule_y,
                        computed.capsule_width,
                        computed.capsule_height,
                        computed.corner_radius,
                        1.5, // border width
                        rehearsal_red,
                        w,
                        h,
                    ));

                    text_buffers.push((buffer, computed.text_x, computed.text_y, computed.text_scale));
                }
            }

            // Move to next system
            current_y += system_height + system_spacing;
        }

        (vertices, sdf_vertices, text_buffers)
    }
}

// ============================================================================
// Texture Creation
// ============================================================================

fn create_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Chart Texture"),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT
            | wgpu::TextureUsages::TEXTURE_BINDING
            | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    })
}
