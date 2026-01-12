//! Music Symbols Example
//!
//! Demonstrates rendering music notation symbols using SMuFL fonts (Leland)
//! with pure WGPU and Figma-like zoom and pan on standard paper sizes.
//!
//! Features:
//! - MuseScore-style page size system (US Letter, A4, etc.)
//! - Proper margins and printable area
//! - Spatium-based scaling (staff space defines everything)
//!
//! Controls:
//! - Scroll: Pan around
//! - Option/Alt + Scroll OR Pinch: Zoom (centered on cursor)
//! - Middle mouse drag OR Space + Left drag: Pan
//! - R key: Reset view
//!
//! Run with: cargo run -p engraver --example music_symbols --features example

use std::sync::Arc;

use glyphon::{
    Attrs, Buffer as TextBuffer, Cache as TextCache, Color as TextColor, Family, FontSystem,
    Metrics, Resolution, Shaping, SwashCache, TextArea, TextAtlas, TextBounds, TextRenderer,
    Viewport,
};
use lyon::lyon_tessellation::{
    BuffersBuilder, FillOptions, FillTessellator, FillVertex, FillVertexConstructor,
    VertexBuffers,
};
use lyon::path::Path;
use skrifa::{
    instance::Size,
    outline::{DrawSettings, OutlinePen},
    prelude::LocationRef,
    raw::TableProvider,
    FontRef, MetadataProvider,
};
use smufl::{Glyph, Metadata as SMuFLMetadata, StaffSpaces};
use wgpu::{
    util::DeviceExt, BindGroup, BindGroupLayout, BufferUsages, Color, CommandEncoderDescriptor,
    DeviceDescriptor, Features, FragmentState, Instance, InstanceDescriptor, LoadOp,
    MultisampleState, Operations, PipelineLayoutDescriptor, PrimitiveState, PrimitiveTopology,
    RenderPassColorAttachment, RenderPassDescriptor, RenderPipeline, RenderPipelineDescriptor,
    RequestAdapterOptions, StoreOp, TextureViewDescriptor, VertexAttribute, VertexBufferLayout,
    VertexState, VertexStepMode,
};
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

const WINDOW_WIDTH: u32 = 1200;
const WINDOW_HEIGHT: u32 = 900;

// ============================================================================
// Page Size System (modeled after MuseScore)
// ============================================================================

/// Millimeters per inch (for conversion)
const MM_PER_INCH: f32 = 25.4;

/// Screen DPI for rendering (standard display)
/// MuseScore uses 1200 DPI internally, but we render at screen resolution
const SCREEN_DPI: f32 = 96.0;

/// Page dimensions in inches (MuseScore stores page sizes in inches internally)
#[derive(Debug, Clone, Copy)]
pub struct PageSize {
    /// Width in inches
    pub width: f32,
    /// Height in inches
    pub height: f32,
}

impl PageSize {
    /// US Letter: 8.5" × 11"
    pub const LETTER: Self = Self { width: 8.5, height: 11.0 };

    /// US Legal: 8.5" × 14"
    pub const LEGAL: Self = Self { width: 8.5, height: 14.0 };

    /// US Tabloid: 11" × 17"
    pub const TABLOID: Self = Self { width: 11.0, height: 17.0 };

    /// A4: 210mm × 297mm ≈ 8.27" × 11.69"
    pub const A4: Self = Self {
        width: 210.0 / MM_PER_INCH,
        height: 297.0 / MM_PER_INCH
    };

    /// A3: 297mm × 420mm
    pub const A3: Self = Self {
        width: 297.0 / MM_PER_INCH,
        height: 420.0 / MM_PER_INCH
    };

    /// Create a custom page size in inches
    pub fn new(width_inches: f32, height_inches: f32) -> Self {
        Self { width: width_inches, height: height_inches }
    }

    /// Create a page size from millimeters
    pub fn from_mm(width_mm: f32, height_mm: f32) -> Self {
        Self {
            width: width_mm / MM_PER_INCH,
            height: height_mm / MM_PER_INCH,
        }
    }

    /// Get width in pixels at given DPI
    pub fn width_px(&self, dpi: f32) -> f32 {
        self.width * dpi
    }

    /// Get height in pixels at given DPI
    pub fn height_px(&self, dpi: f32) -> f32 {
        self.height * dpi
    }
}

/// Page margins in inches (supports different margins for odd/even pages)
#[derive(Debug, Clone, Copy)]
pub struct PageMargins {
    pub top: f32,
    pub bottom: f32,
    pub left: f32,
    pub right: f32,
}

impl PageMargins {
    /// Default margins: 0.5" all around (like MuseScore's 15mm default)
    pub const DEFAULT: Self = Self {
        top: 0.5,
        bottom: 0.5,
        left: 0.5,
        right: 0.5,
    };

    /// Narrow margins: 0.25" all around
    pub const NARROW: Self = Self {
        top: 0.25,
        bottom: 0.25,
        left: 0.25,
        right: 0.25,
    };

    /// Wide margins: 1" all around
    pub const WIDE: Self = Self {
        top: 1.0,
        bottom: 1.0,
        left: 1.0,
        right: 1.0,
    };

    /// Create custom margins in inches
    pub fn new(top: f32, bottom: f32, left: f32, right: f32) -> Self {
        Self { top, bottom, left, right }
    }

    /// Create margins from millimeters
    pub fn from_mm(top: f32, bottom: f32, left: f32, right: f32) -> Self {
        Self {
            top: top / MM_PER_INCH,
            bottom: bottom / MM_PER_INCH,
            left: left / MM_PER_INCH,
            right: right / MM_PER_INCH,
        }
    }
}

/// Page style combining size, margins, and layout options
#[derive(Debug, Clone, Copy)]
pub struct PageStyle {
    /// Page dimensions
    pub size: PageSize,
    /// Margins for odd pages (and even if not two-sided)
    pub odd_margins: PageMargins,
    /// Margins for even pages (only used if two_sided is true)
    pub even_margins: PageMargins,
    /// Whether to use different margins for odd/even pages
    pub two_sided: bool,
    /// Spatium (staff space) in millimeters - the fundamental unit
    /// MuseScore default is about 1.764mm (which gives standard staff height)
    pub spatium_mm: f32,
}

impl Default for PageStyle {
    fn default() -> Self {
        Self {
            size: PageSize::LETTER,
            odd_margins: PageMargins::DEFAULT,
            even_margins: PageMargins::DEFAULT,
            two_sided: false,
            // MuseScore default spatium is stored as 1.76389mm in the style
            // This gives a staff height of about 7mm (4 spaces × 1.764mm)
            spatium_mm: 1.764,
        }
    }
}

impl PageStyle {
    /// Create a new page style with US Letter and default settings
    pub fn letter() -> Self {
        Self::default()
    }

    /// Create a new page style with A4 paper
    pub fn a4() -> Self {
        Self {
            size: PageSize::A4,
            ..Default::default()
        }
    }

    /// Set spatium in millimeters
    pub fn with_spatium_mm(mut self, spatium: f32) -> Self {
        self.spatium_mm = spatium;
        self
    }

    /// Set spatium based on desired staff height in mm
    /// Staff height = 4 × spatium
    pub fn with_staff_height_mm(mut self, height: f32) -> Self {
        self.spatium_mm = height / 4.0;
        self
    }

    /// Get spatium (staff space) in inches
    pub fn spatium_inches(&self) -> f32 {
        self.spatium_mm / MM_PER_INCH
    }

    /// Get spatium (staff space) in pixels at given DPI
    pub fn spatium_px(&self, dpi: f32) -> f32 {
        self.spatium_inches() * dpi
    }

    /// Get the printable width in inches (page width minus left and right margins)
    pub fn printable_width(&self, is_even_page: bool) -> f32 {
        let margins = if self.two_sided && is_even_page {
            &self.even_margins
        } else {
            &self.odd_margins
        };
        self.size.width - margins.left - margins.right
    }

    /// Get the printable height in inches (page height minus top and bottom margins)
    pub fn printable_height(&self, is_even_page: bool) -> f32 {
        let margins = if self.two_sided && is_even_page {
            &self.even_margins
        } else {
            &self.odd_margins
        };
        self.size.height - margins.top - margins.bottom
    }

    /// Get margins for a specific page
    pub fn margins(&self, is_even_page: bool) -> &PageMargins {
        if self.two_sided && is_even_page {
            &self.even_margins
        } else {
            &self.odd_margins
        }
    }
}

// Path to Leland font files
const LELAND_FONT_PATH: &str = "libs/reference/sheet-music/fonts/leland/Leland.otf";
const LELAND_METADATA_PATH: &str = "libs/reference/sheet-music/fonts/leland/leland_metadata.json";

/// Camera/view transform uniform
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct CameraUniform {
    /// Combined transform: [scale_x, scale_y, offset_x, offset_y]
    transform: [f32; 4],
}

impl CameraUniform {
    fn new() -> Self {
        Self {
            transform: [1.0, 1.0, 0.0, 0.0],
        }
    }

    fn from_view(view: &ViewState) -> Self {
        Self {
            transform: [view.zoom, view.zoom, view.pan_x, view.pan_y],
        }
    }
}

/// View state for zoom and pan
#[derive(Debug, Clone)]
struct ViewState {
    zoom: f32,
    pan_x: f32,
    pan_y: f32,
    // Interaction state
    cursor_x: f32,
    cursor_y: f32,
    is_panning: bool,
    pan_start_x: f32,
    pan_start_y: f32,
    pan_origin_x: f32,
    pan_origin_y: f32,
    space_held: bool,
    middle_held: bool,
    alt_held: bool,
}

impl Default for ViewState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            pan_x: 0.0,
            pan_y: 0.0,
            cursor_x: 0.0,
            cursor_y: 0.0,
            is_panning: false,
            pan_start_x: 0.0,
            pan_start_y: 0.0,
            pan_origin_x: 0.0,
            pan_origin_y: 0.0,
            space_held: false,
            middle_held: false,
            alt_held: false,
        }
    }
}

impl ViewState {
    fn reset(&mut self) {
        self.zoom = 1.0;
        self.pan_x = 0.0;
        self.pan_y = 0.0;
    }

    /// Zoom centered on cursor position
    fn zoom_at(&mut self, cursor_x: f32, cursor_y: f32, delta: f32, width: f32, height: f32) {
        let old_zoom = self.zoom;

        // Calculate zoom factor (smoother for trackpad)
        let zoom_factor = 1.0 + delta * 0.1;
        // Limit zoom to 4x (8x with retina) for glyphon text rendering stability
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
    fn scroll_pan(&mut self, dx: f32, dy: f32, width: f32, height: f32) {
        // Convert pixel delta to NDC delta, accounting for zoom
        self.pan_x += (dx / width * 2.0) / self.zoom;
        self.pan_y -= (dy / height * 2.0) / self.zoom;
    }

    fn start_pan(&mut self, x: f32, y: f32) {
        self.is_panning = true;
        self.pan_start_x = x;
        self.pan_start_y = y;
        self.pan_origin_x = self.pan_x;
        self.pan_origin_y = self.pan_y;
    }

    fn update_pan(&mut self, x: f32, y: f32, width: f32, height: f32) {
        if self.is_panning {
            // Convert pixel delta to NDC delta
            let dx = (x - self.pan_start_x) / width * 2.0;
            let dy = -(y - self.pan_start_y) / height * 2.0;
            self.pan_x = self.pan_origin_x + dx;
            self.pan_y = self.pan_origin_y + dy;
        }
    }

    fn end_pan(&mut self) {
        self.is_panning = false;
    }
}

/// Vertex with position and color
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct Vertex {
    position: [f32; 2],
    color: [f32; 4],
}

impl Vertex {
    const ATTRIBS: [VertexAttribute; 2] = wgpu::vertex_attr_array![0 => Float32x2, 1 => Float32x4];

    fn desc() -> VertexBufferLayout<'static> {
        VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: VertexStepMode::Vertex,
            attributes: &Self::ATTRIBS,
        }
    }
}

// ============================================================================
// SDF Rounded Rectangle Support
// ============================================================================

/// Vertex for SDF-based rounded rectangles - pixel-perfect at any zoom level
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct SdfRectVertex {
    /// Position in NDC
    position: [f32; 2],
    /// Rectangle center in pixels
    rect_center: [f32; 2],
    /// Rectangle half-size (from center to corner) in pixels
    rect_half_size: [f32; 2],
    /// Corner radius in pixels
    corner_radius: f32,
    /// Border width (0 = filled, >0 = stroked)
    border_width: f32,
    /// Fill/border color
    color: [f32; 4],
}

impl SdfRectVertex {
    fn desc() -> VertexBufferLayout<'static> {
        VertexBufferLayout {
            array_stride: std::mem::size_of::<SdfRectVertex>() as wgpu::BufferAddress,
            step_mode: VertexStepMode::Vertex,
            attributes: &[
                VertexAttribute { offset: 0, shader_location: 0, format: wgpu::VertexFormat::Float32x2 },  // position
                VertexAttribute { offset: 8, shader_location: 1, format: wgpu::VertexFormat::Float32x2 },  // rect_center
                VertexAttribute { offset: 16, shader_location: 2, format: wgpu::VertexFormat::Float32x2 }, // rect_half_size
                VertexAttribute { offset: 24, shader_location: 3, format: wgpu::VertexFormat::Float32 },   // corner_radius
                VertexAttribute { offset: 28, shader_location: 4, format: wgpu::VertexFormat::Float32 },   // border_width
                VertexAttribute { offset: 32, shader_location: 5, format: wgpu::VertexFormat::Float32x4 }, // color
            ],
        }
    }
}

/// Create an SDF rounded rectangle (6 vertices for 2 triangles forming a quad)
fn create_sdf_rounded_rect(
    x: f32, y: f32, w: f32, h: f32,
    corner_radius: f32,
    border_width: f32,
    color: [f32; 4],
    canvas_width: f32,
    canvas_height: f32,
) -> Vec<SdfRectVertex> {
    // Add padding for SDF antialiasing
    let padding = 2.0;
    let px = x - padding;
    let py = y - padding;
    let pw = w + padding * 2.0;
    let ph = h + padding * 2.0;

    // Convert to NDC
    let ndc = |px: f32, py: f32| -> [f32; 2] {
        [
            (px / canvas_width) * 2.0 - 1.0,
            1.0 - (py / canvas_height) * 2.0,
        ]
    };

    let rect_center = [x + w / 2.0, y + h / 2.0];
    let rect_half_size = [w / 2.0, h / 2.0];

    let p1 = ndc(px, py);
    let p2 = ndc(px + pw, py);
    let p3 = ndc(px, py + ph);
    let p4 = ndc(px + pw, py + ph);

    let make_vertex = |pos: [f32; 2]| SdfRectVertex {
        position: pos,
        rect_center,
        rect_half_size,
        corner_radius,
        border_width,
        color,
    };

    vec![
        make_vertex(p1),
        make_vertex(p3),
        make_vertex(p2),
        make_vertex(p3),
        make_vertex(p4),
        make_vertex(p2),
    ]
}

/// Vertex constructor for lyon tessellation
struct VertexCtor {
    color: [f32; 4],
}

impl FillVertexConstructor<Vertex> for VertexCtor {
    fn new_vertex(&mut self, vertex: FillVertex) -> Vertex {
        Vertex {
            position: vertex.position().to_array(),
            color: self.color,
        }
    }
}

/// Pen that builds a lyon Path from glyph outlines
struct LyonPen {
    builder: lyon::path::Builder,
}

impl LyonPen {
    fn new() -> Self {
        Self {
            builder: Path::builder(),
        }
    }

    fn build(self) -> Path {
        self.builder.build()
    }
}

impl OutlinePen for LyonPen {
    fn move_to(&mut self, x: f32, y: f32) {
        self.builder.begin(lyon::math::point(x, y));
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.builder.line_to(lyon::math::point(x, y));
    }

    fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
        self.builder
            .quadratic_bezier_to(lyon::math::point(cx0, cy0), lyon::math::point(x, y));
    }

    fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
        self.builder.cubic_bezier_to(
            lyon::math::point(cx0, cy0),
            lyon::math::point(cx1, cy1),
            lyon::math::point(x, y),
        );
    }

    fn close(&mut self) {
        self.builder.end(true);
    }
}

/// Loaded SMuFL font with metadata
struct LoadedFont {
    font_data: Vec<u8>,
    metadata: SMuFLMetadata,
}

impl LoadedFont {
    fn load() -> Result<Self, Box<dyn std::error::Error>> {
        let font_data = std::fs::read(LELAND_FONT_PATH)?;
        let metadata_file = std::fs::File::open(LELAND_METADATA_PATH)?;
        let metadata = SMuFLMetadata::from_reader(std::io::BufReader::new(metadata_file))?;
        Ok(Self {
            font_data,
            metadata,
        })
    }

    fn font_ref(&self) -> FontRef<'_> {
        FontRef::new(&self.font_data).expect("Failed to parse font")
    }

    /// Get the stem-up anchor point for a glyph in pixels
    /// Returns (x, y) offset from glyph origin in staff spaces
    fn stem_up_se(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.anchors.get(glyph)?;
        let coord = anchors.stem_up_se?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }

    /// Get the stem-down anchor point for a glyph in pixels
    /// Returns (x, y) offset from glyph origin in staff spaces
    fn stem_down_nw(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.anchors.get(glyph)?;
        let coord = anchors.stem_down_nw?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }

    /// Get the bounding box of a glyph in staff spaces converted to pixels
    /// Returns (sw_x, sw_y, ne_x, ne_y) - southwest and northeast corners
    fn bbox(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32, f32, f32)> {
        let bbox = self.metadata.bounding_boxes.get(glyph)?;
        Some((
            f64::from(bbox.sw.x()) as f32 * staff_space,
            f64::from(bbox.sw.y()) as f32 * staff_space,
            f64::from(bbox.ne.x()) as f32 * staff_space,
            f64::from(bbox.ne.y()) as f32 * staff_space,
        ))
    }

    /// Get the width of a glyph in pixels
    fn glyph_width(&self, glyph: Glyph, staff_space: f32) -> f32 {
        if let Some((sw_x, _, ne_x, _)) = self.bbox(glyph, staff_space) {
            ne_x - sw_x
        } else {
            staff_space // fallback
        }
    }
}

/// Get the glyph ID for a SMuFL glyph from the font
fn get_glyph_id(font: &FontRef<'_>, smufl_glyph: Glyph) -> Option<skrifa::GlyphId> {
    let cmap = font.charmap();
    let codepoint = smufl_glyph.codepoint();
    cmap.map(codepoint)
}

/// Tessellate a glyph into triangles
fn tessellate_glyph(
    font: &FontRef<'_>,
    glyph_id: skrifa::GlyphId,
    font_size: f32,
    x_offset: f32,
    y_offset: f32,
    color: [f32; 4],
    width: f32,
    height: f32,
) -> Vec<Vertex> {
    let outline_glyphs = font.outline_glyphs();
    let Some(outline) = outline_glyphs.get(glyph_id) else {
        return Vec::new();
    };

    // DrawSettings takes the font size in pixels (ppem)
    // The outline will be drawn at this size directly
    let settings = DrawSettings::unhinted(Size::new(font_size), LocationRef::default());

    let mut pen = LyonPen::new();
    if outline.draw(settings, &mut pen).is_err() {
        return Vec::new();
    }

    let path = pen.build();

    // Tessellate the path
    let mut geometry: VertexBuffers<Vertex, u32> = VertexBuffers::new();
    let mut tessellator = FillTessellator::new();

    let result = tessellator.tessellate_path(
        &path,
        &FillOptions::default(),
        &mut BuffersBuilder::new(&mut geometry, VertexCtor { color }),
    );

    if result.is_err() {
        return Vec::new();
    }

    // Transform vertices to NDC
    // DrawSettings already scales the outline to font_size pixels,
    // so we just need to apply position offset and convert to NDC
    let mut vertices = Vec::new();
    for index in &geometry.indices {
        let v = &geometry.vertices[*index as usize];
        // Position is already in pixels at the target font size
        let px = x_offset + v.position[0];
        // Flip Y because font coordinates are Y-up, screen is Y-down
        let py = y_offset - v.position[1];

        let ndc = px_to_ndc(px, py, width, height);
        vertices.push(Vertex {
            position: ndc,
            color: v.color,
        });
    }

    vertices
}

/// Text rendering state using glyphon
struct TextState {
    font_system: FontSystem,
    swash_cache: SwashCache,
    text_cache: TextCache,
    viewport: Viewport,
    atlas: TextAtlas,
    text_renderer: TextRenderer,
    /// Rehearsal mark text buffers with computed positions
    rehearsal_buffers: Vec<(TextBuffer, f32, f32)>, // (buffer, x, y)
}

struct RenderState {
    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
    device: wgpu::Device,
    queue: wgpu::Queue,
    // Main geometry pipeline
    pipeline: RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    num_vertices: u32,
    // SDF pipeline for rounded rectangles
    sdf_pipeline: RenderPipeline,
    sdf_vertex_buffer: wgpu::Buffer,
    sdf_num_vertices: u32,
    // Shared camera
    camera_buffer: wgpu::Buffer,
    camera_bind_group: BindGroup,
    text_state: TextState,
}

struct App {
    window: Option<Arc<Window>>,
    render_state: Option<RenderState>,
    view: ViewState,
    font: Option<LoadedFont>,
    page_style: PageStyle,
}

impl App {
    fn new() -> Self {
        // Load font at startup
        let font = match LoadedFont::load() {
            Ok(f) => {
                log::info!("Loaded Leland font successfully");
                Some(f)
            }
            Err(e) => {
                log::error!("Failed to load Leland font: {}", e);
                log::info!("Falling back to placeholder shapes");
                None
            }
        };

        // Use US Letter paper with default settings
        let page_style = PageStyle::letter();

        log::info!(
            "Page setup: {:.2}\" × {:.2}\" ({:.1}mm × {:.1}mm) Letter",
            page_style.size.width, page_style.size.height,
            page_style.size.width * MM_PER_INCH, page_style.size.height * MM_PER_INCH
        );
        log::info!(
            "Spatium: {:.3}mm, Staff height: {:.1}mm",
            page_style.spatium_mm, page_style.spatium_mm * 4.0
        );

        Self {
            window: None,
            render_state: None,
            view: ViewState::default(),
            font,
            page_style,
        }
    }

    fn update_camera(&self) {
        if let Some(state) = &self.render_state {
            let camera = CameraUniform::from_view(&self.view);
            state
                .queue
                .write_buffer(&state.camera_buffer, 0, bytemuck::cast_slice(&[camera]));
        }
    }
}

/// Convert pixel coordinates to normalized device coordinates (-1 to 1)
fn px_to_ndc(x: f32, y: f32, width: f32, height: f32) -> [f32; 2] {
    [
        (x / width) * 2.0 - 1.0,
        1.0 - (y / height) * 2.0, // Flip Y for screen coordinates
    ]
}

/// Create a line as two triangles (a thin rectangle)
fn create_line(
    x1: f32,
    y1: f32,
    x2: f32,
    y2: f32,
    thickness: f32,
    color: [f32; 4],
    width: f32,
    height: f32,
) -> [Vertex; 6] {
    let dx = x2 - x1;
    let dy = y2 - y1;
    let len = (dx * dx + dy * dy).sqrt();
    let nx = -dy / len * thickness * 0.5;
    let ny = dx / len * thickness * 0.5;

    let p1 = px_to_ndc(x1 + nx, y1 + ny, width, height);
    let p2 = px_to_ndc(x1 - nx, y1 - ny, width, height);
    let p3 = px_to_ndc(x2 + nx, y2 + ny, width, height);
    let p4 = px_to_ndc(x2 - nx, y2 - ny, width, height);

    [
        Vertex { position: p1, color },
        Vertex { position: p2, color },
        Vertex { position: p3, color },
        Vertex { position: p2, color },
        Vertex { position: p4, color },
        Vertex { position: p3, color },
    ]
}

/// Create a filled rectangle
fn create_rect(
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    color: [f32; 4],
    width: f32,
    height: f32,
) -> [Vertex; 6] {
    let p1 = px_to_ndc(x, y, width, height);
    let p2 = px_to_ndc(x + w, y, width, height);
    let p3 = px_to_ndc(x, y + h, width, height);
    let p4 = px_to_ndc(x + w, y + h, width, height);

    [
        Vertex { position: p1, color },
        Vertex { position: p3, color },
        Vertex { position: p2, color },
        Vertex { position: p3, color },
        Vertex { position: p4, color },
        Vertex { position: p2, color },
    ]
}

/// Create a rounded rectangle frame (outline) with adjustable corner radius
fn create_rounded_frame_rect(
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    radius: f32,
    thickness: f32,
    color: [f32; 4],
    width: f32,
    height: f32,
) -> Vec<Vertex> {
    let mut vertices = Vec::new();

    // Clamp radius to fit within the rectangle
    let r = radius.min(w / 2.0).min(h / 2.0);

    // Number of segments for each corner arc
    let segments = 8;

    // Helper to create arc vertices
    let create_arc = |cx: f32, cy: f32, start_angle: f32, end_angle: f32| -> Vec<Vertex> {
        let mut arc_verts = Vec::new();
        for i in 0..segments {
            let t0 = i as f32 / segments as f32;
            let t1 = (i + 1) as f32 / segments as f32;
            let a0 = start_angle + (end_angle - start_angle) * t0;
            let a1 = start_angle + (end_angle - start_angle) * t1;

            let x0 = cx + r * a0.cos();
            let y0 = cy + r * a0.sin();
            let x1 = cx + r * a1.cos();
            let y1 = cy + r * a1.sin();

            arc_verts.extend_from_slice(&create_line(x0, y0, x1, y1, thickness, color, width, height));
        }
        arc_verts
    };

    use std::f32::consts::PI;

    // Top edge (between top-left and top-right corners)
    vertices.extend_from_slice(&create_line(x + r, y, x + w - r, y, thickness, color, width, height));

    // Top-right corner arc (from top to right, 270° to 360°)
    vertices.extend(create_arc(x + w - r, y + r, -PI / 2.0, 0.0));

    // Right edge
    vertices.extend_from_slice(&create_line(x + w, y + r, x + w, y + h - r, thickness, color, width, height));

    // Bottom-right corner arc (from right to bottom, 0° to 90°)
    vertices.extend(create_arc(x + w - r, y + h - r, 0.0, PI / 2.0));

    // Bottom edge
    vertices.extend_from_slice(&create_line(x + w - r, y + h, x + r, y + h, thickness, color, width, height));

    // Bottom-left corner arc (from bottom to left, 90° to 180°)
    vertices.extend(create_arc(x + r, y + h - r, PI / 2.0, PI));

    // Left edge
    vertices.extend_from_slice(&create_line(x, y + h - r, x, y + r, thickness, color, width, height));

    // Top-left corner arc (from left to top, 180° to 270°)
    vertices.extend(create_arc(x + r, y + r, PI, 3.0 * PI / 2.0));

    vertices
}

// ============================================================================
// Demo Song Structure for Multi-System Layout
// ============================================================================

/// Demo song structure with sections and measures
struct DemoSong {
    sections: Vec<(&'static str, usize)>, // (section_name, measure_count)
}

impl DemoSong {
    fn new() -> Self {
        Self {
            sections: vec![
                ("INTRO", 2),
                ("VS 1", 8),
                ("CH 1", 4),
                ("OUTRO", 4),
            ],
        }
    }

    fn total_measures(&self) -> usize {
        self.sections.iter().map(|(_, count)| count).sum()
    }

    /// Get section starts (measure indices where sections begin)
    fn section_starts(&self) -> Vec<usize> {
        let mut starts = Vec::new();
        let mut current = 0;
        for (_, count) in &self.sections {
            starts.push(current);
            current += count;
        }
        starts
    }

    /// Get section name for a given measure
    fn section_at(&self, measure: usize) -> Option<&'static str> {
        let mut current = 0;
        for (name, count) in &self.sections {
            if measure >= current && measure < current + count {
                return Some(name);
            }
            current += count;
        }
        None
    }
}

/// Computed position for a rehearsal mark / section label
#[derive(Debug, Clone)]
struct RehearsalMarkPosition {
    name: &'static str,
    capsule_x: f32,
    capsule_y: f32,
    capsule_width: f32,
    capsule_height: f32,
}

/// Compute the sky line (highest element Y position) for a system
/// Returns the minimum Y coordinate where content exists (top of notes/stems)
fn compute_system_sky_line(
    staff_y: f32,
    staff_space: f32,
    sys_info: &engraver::model::SystemInfo,
) -> f32 {
    // Simulate the note drawing logic to find the highest stem tip
    let mut min_y = staff_y; // Default: top of staff

    let stem_length = staff_space * 3.5;
    let measures_in_system = sys_info.measure_count;

    for m in 0..measures_in_system {
        for beat in 0..4 {
            // Same pitch calculation as build_scene
            let pitch_offset = ((sys_info.start_measure + m + beat) % 7) as f32;
            let staff_pos = 4.0 - pitch_offset * 0.5;
            let note_y = staff_y + (4.0 - staff_pos) * staff_space;

            // Stem direction and end point
            let stem_up = staff_pos > 2.0;
            if stem_up {
                let stem_tip = note_y - stem_length;
                min_y = min_y.min(stem_tip);
            }
        }
    }

    // Add some padding above the highest element
    min_y - staff_space * 0.5
}

/// Compute the positions of all rehearsal marks based on layout
/// Uses sky line collision avoidance to position marks above content
fn compute_rehearsal_positions(page_style: &PageStyle, window_width: f32) -> Vec<RehearsalMarkPosition> {
    use engraver::model::{compute_system_layout, LineBreakPolicy};

    let song = DemoSong::new();
    let total_measures = song.total_measures();
    let section_starts = song.section_starts();

    let layout = compute_system_layout(
        total_measures,
        &section_starts,
        &LineBreakPolicy::four_per_line(),
    );

    // Calculate page dimensions
    let page_width = page_style.size.width_px(SCREEN_DPI);
    let page_x = (window_width - page_width) / 2.0;
    let page_y = 20.0;

    let margins = page_style.margins(false);
    let margin_left_px = margins.left * SCREEN_DPI;
    let margin_top_px = margins.top * SCREEN_DPI;

    let content_left = page_x + margin_left_px;
    let content_top = page_y + margin_top_px;

    let staff_space = page_style.spatium_px(SCREEN_DPI);
    let system_height = staff_space * 4.0;
    let system_spacing = staff_space * 8.0;
    let section_extra_spacing = staff_space * 2.0;

    let mut positions = Vec::new();
    let mut current_y = content_top + staff_space * 4.0;

    for (sys_idx, sys_info) in layout.systems.iter().enumerate() {
        // Add extra spacing before section starts (except first)
        if sys_info.is_section_start && sys_idx > 0 {
            current_y += section_extra_spacing;
        }

        let staff_y = current_y;

        // Only record positions for section starts
        if sys_info.is_section_start {
            if let Some(name) = song.section_at(sys_info.start_measure) {
                // Same sizing as build_scene
                let char_width = staff_space * 0.7;
                let padding = staff_space * 0.5;
                let capsule_height = staff_space * 1.8;
                let capsule_width = (name.len() as f32 * char_width) + (padding * 2.0);
                let capsule_x = content_left;

                // Compute sky line for collision avoidance
                let sky_line = compute_system_sky_line(staff_y, staff_space, sys_info);

                // Position capsule well above the sky line (increased clearance)
                let capsule_y = sky_line - capsule_height - staff_space * 1.5;

                log::debug!(
                    "Rehearsal '{}': staff_y={:.1}, sky_line={:.1}, capsule_y={:.1}",
                    name, staff_y, sky_line, capsule_y
                );

                positions.push(RehearsalMarkPosition {
                    name,
                    capsule_x,
                    capsule_y,
                    capsule_width,
                    capsule_height,
                });
            }
        }

        current_y += system_height + system_spacing;
    }

    positions
}

/// Convert a single sRGB component (0.0-1.0) to linear color space
/// This is needed because the GPU expects linear colors but blends in linear space,
/// then converts to sRGB on output. If we pass sRGB values directly, they get
/// double-gamma corrected and appear washed out/orange.
fn srgb_to_linear(srgb: f32) -> f32 {
    if srgb <= 0.04045 {
        srgb / 12.92
    } else {
        ((srgb + 0.055) / 1.055).powf(2.4)
    }
}

/// Convert sRGB color (0-255 values) to linear color space for GPU rendering
fn srgb_color_to_linear(r: u8, g: u8, b: u8, a: u8) -> [f32; 4] {
    [
        srgb_to_linear(r as f32 / 255.0),
        srgb_to_linear(g as f32 / 255.0),
        srgb_to_linear(b as f32 / 255.0),
        a as f32 / 255.0, // Alpha is already linear
    ]
}

/// Rehearsal mark red color - converted from sRGB (229, 33, 0) to linear color space
/// This matches the text color which uses TextColor::rgba(229, 33, 0, 255)
fn rehearsal_red_linear() -> [f32; 4] {
    srgb_color_to_linear(229, 33, 0, 255)
}

/// Computed rehearsal mark with actual text-based dimensions
#[derive(Debug, Clone)]
struct ComputedRehearsalMark {
    /// Text to display
    name: String,
    /// Capsule position and size (calculated from text)
    capsule_x: f32,
    capsule_y: f32,
    capsule_width: f32,
    capsule_height: f32,
    /// Text position (centered in capsule)
    text_x: f32,
    text_y: f32,
}

/// Build SDF vertices for rehearsal mark frames using pre-computed positions
fn build_sdf_frames_from_computed(marks: &[ComputedRehearsalMark], window_width: f32, window_height: f32) -> Vec<SdfRectVertex> {
    let mut vertices = Vec::new();
    let color = rehearsal_red_linear(); // Use linear color space for correct GPU blending

    for mark in marks {
        let corner_radius = mark.capsule_height / 4.0; // Subtle rounded corners
        let border_width = 1.5;

        // The SDF rectangle should match exactly where we placed the text
        // text is at (text_x, text_y), capsule origin is (text_x - padding, text_y - padding)
        vertices.extend(create_sdf_rounded_rect(
            mark.capsule_x,
            mark.capsule_y,
            mark.capsule_width,
            mark.capsule_height,
            corner_radius,
            border_width,
            color,
            window_width,
            window_height,
        ));
    }

    vertices
}

/// Create debug corner markers for a rectangle (for debugging positioning)
fn create_debug_corners(
    x: f32, y: f32, w: f32, h: f32,
    color: [f32; 4],
    window_width: f32,
    window_height: f32,
) -> Vec<Vertex> {
    let corner_size = 4.0;
    let mut vertices = Vec::new();

    // Top-left
    vertices.extend_from_slice(&create_rect(x, y, corner_size, corner_size, color, window_width, window_height));
    // Top-right
    vertices.extend_from_slice(&create_rect(x + w - corner_size, y, corner_size, corner_size, color, window_width, window_height));
    // Bottom-left
    vertices.extend_from_slice(&create_rect(x, y + h - corner_size, corner_size, corner_size, color, window_width, window_height));
    // Bottom-right
    vertices.extend_from_slice(&create_rect(x + w - corner_size, y + h - corner_size, corner_size, corner_size, color, window_width, window_height));

    vertices
}

/// Build all the geometry for the music notation demo
/// Uses the page style to render on proper paper dimensions
fn build_scene(window_width: f32, window_height: f32, font: Option<&LoadedFont>, page_style: &PageStyle) -> Vec<Vertex> {
    use engraver::model::{compute_system_layout, LineBreakPolicy};

    let mut vertices = Vec::new();

    // Colors
    let black = [0.0, 0.0, 0.0, 1.0];
    let gray = [0.5, 0.5, 0.5, 1.0];
    let rehearsal_red = [0.898, 0.129, 0.0, 1.0]; // Red-orange for rehearsal marks
    let paper_white = [1.0, 1.0, 1.0, 1.0];
    let paper_shadow = [0.15, 0.15, 0.17, 1.0];

    // Calculate page dimensions in pixels at screen DPI
    let page_width = page_style.size.width_px(SCREEN_DPI);
    let page_height = page_style.size.height_px(SCREEN_DPI);

    // Center the page in the window with some padding
    let page_x = (window_width - page_width) / 2.0;
    let page_y = 20.0;

    // Draw paper shadow
    let shadow_offset = 4.0;
    let shadow = create_rect(
        page_x + shadow_offset,
        page_y + shadow_offset,
        page_width,
        page_height,
        paper_shadow,
        window_width,
        window_height,
    );
    vertices.extend_from_slice(&shadow);

    // Draw paper background
    let paper = create_rect(page_x, page_y, page_width, page_height, paper_white, window_width, window_height);
    vertices.extend_from_slice(&paper);

    // Get margins and calculate printable area
    let margins = page_style.margins(false);
    let margin_left_px = margins.left * SCREEN_DPI;
    let margin_top_px = margins.top * SCREEN_DPI;
    let margin_right_px = margins.right * SCREEN_DPI;

    // Content area boundaries
    let content_left = page_x + margin_left_px;
    let content_top = page_y + margin_top_px;
    let content_right = page_x + page_width - margin_right_px;
    let content_width = content_right - content_left;

    // Use spatium from page style
    let staff_space = page_style.spatium_px(SCREEN_DPI);
    let font_size = staff_space * 4.0;

    // Create demo song structure
    let song = DemoSong::new();
    let total_measures = song.total_measures();
    let section_starts = song.section_starts();

    // Compute system layout (4 measures per line, breaking at sections)
    let layout = compute_system_layout(
        total_measures,
        &section_starts,
        &LineBreakPolicy::four_per_line(),
    );

    // System spacing
    let system_height = staff_space * 4.0; // 5 lines = 4 spaces
    let system_spacing = staff_space * 8.0; // Space between systems
    let section_extra_spacing = staff_space * 2.0; // Extra space before new sections

    // Starting position
    let staff_left = content_left;
    let staff_right = content_right;
    let mut current_y = content_top + staff_space * 4.0; // Start with some top padding

    // Render each system
    for (sys_idx, sys_info) in layout.systems.iter().enumerate() {
        // Add extra spacing before section starts (except first)
        if sys_info.is_section_start && sys_idx > 0 {
            current_y += section_extra_spacing;
        }

        let staff_y = current_y;

        // Get section name if this is a section start
        let section_name = if sys_info.is_section_start {
            song.section_at(sys_info.start_measure)
        } else {
            None
        };

        // Note: Rehearsal mark frames are now rendered using SDF pipeline for pixel-perfect quality
        // The old tessellation-based frames have been replaced

        // Draw 5 staff lines
        for i in 0..5 {
            let y = staff_y + (i as f32) * staff_space;
            let line = create_line(staff_left, y, staff_right, y, 1.0, black, window_width, window_height);
            vertices.extend_from_slice(&line);
        }

        // Calculate measure positions for this system
        let measures_in_system = sys_info.measure_count;
        let measure_width = content_width / measures_in_system as f32;

        // Draw barlines for each measure
        for m in 0..=measures_in_system {
            let bar_x = staff_left + (m as f32) * measure_width;
            let thickness = if m == 0 || m == measures_in_system { 2.0 } else { 1.0 };
            let barline = create_line(
                bar_x,
                staff_y,
                bar_x,
                staff_y + system_height,
                thickness,
                black,
                window_width,
                window_height,
            );
            vertices.extend_from_slice(&barline);

            // Draw measure number below first barline of each measure (except first)
            if m > 0 && m < measures_in_system {
                let measure_num = sys_info.start_measure + m;
                // Small tick mark for measure number position
                let tick = create_line(
                    bar_x,
                    staff_y + system_height + staff_space * 0.2,
                    bar_x,
                    staff_y + system_height + staff_space * 0.5,
                    0.5,
                    gray,
                    window_width,
                    window_height,
                );
                vertices.extend_from_slice(&tick);
            }
        }

        // Render clef and time signature on first system, clef only on others
        if let Some(loaded_font) = font {
            let font_ref = loaded_font.font_ref();

            // G Clef at start of system
            if let Some(gid) = get_glyph_id(&font_ref, Glyph::GClef) {
                let clef_x = staff_left + staff_space * 0.5;
                let clef_y = staff_y + staff_space * 3.0; // G line
                let clef_verts = tessellate_glyph(&font_ref, gid, font_size, clef_x, clef_y, black, window_width, window_height);
                vertices.extend(clef_verts);
            }

            // Time signature only on first system
            if sys_idx == 0 {
                if let Some(gid) = get_glyph_id(&font_ref, Glyph::TimeSigCommon) {
                    let ts_x = staff_left + staff_space * 4.5;
                    let ts_y = staff_y + staff_space * 2.0;
                    let ts_verts = tessellate_glyph(&font_ref, gid, font_size, ts_x, ts_y, black, window_width, window_height);
                    vertices.extend(ts_verts);
                }
            }

            // Draw some sample notes in each measure
            let stem_thickness = loaded_font.metadata.engraving_defaults.stem_thickness
                .map(|s| f64::from(s) as f32 * staff_space)
                .unwrap_or(1.2);

            // Content offset (after clef and time sig)
            let note_area_start = if sys_idx == 0 {
                staff_left + staff_space * 7.0
            } else {
                staff_left + staff_space * 4.0
            };

            // Simple pattern: one note per beat position
            for m in 0..measures_in_system {
                let measure_start = staff_left + (m as f32) * measure_width;
                let measure_end = measure_start + measure_width;
                let note_area = measure_end - note_area_start.max(measure_start);

                // Draw 4 quarter notes per measure (simplified)
                for beat in 0..4 {
                    let beat_offset = (beat as f32 + 0.5) / 4.0;
                    let note_x = measure_start.max(note_area_start) + beat_offset * (measure_end - measure_start.max(note_area_start)) * 0.8;

                    // Vary the pitch based on measure and beat
                    let pitch_offset = ((sys_info.start_measure + m + beat) % 7) as f32;
                    let staff_pos = 4.0 - pitch_offset * 0.5; // E4 to B4 range

                    let note_y = staff_y + (4.0 - staff_pos) * staff_space;

                    // Draw notehead
                    if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadBlack) {
                        let nh_verts = tessellate_glyph(&font_ref, gid, font_size, note_x, note_y, black, window_width, window_height);
                        vertices.extend(nh_verts);
                    }

                    // Draw stem
                    let stem_up = staff_pos > 2.0;
                    let stem_length = staff_space * 3.5;
                    let (stem_x, stem_attach_y) = if stem_up {
                        if let Some((ax, ay)) = loaded_font.stem_up_se(Glyph::NoteheadBlack, staff_space) {
                            (note_x + ax - stem_thickness / 2.0, note_y - ay)
                        } else {
                            let nh_width = loaded_font.glyph_width(Glyph::NoteheadBlack, staff_space);
                            (note_x + nh_width - stem_thickness / 2.0, note_y)
                        }
                    } else {
                        if let Some((ax, ay)) = loaded_font.stem_down_nw(Glyph::NoteheadBlack, staff_space) {
                            (note_x + ax + stem_thickness / 2.0, note_y - ay)
                        } else {
                            (note_x + stem_thickness / 2.0, note_y)
                        }
                    };

                    let stem_end_y = if stem_up {
                        stem_attach_y - stem_length
                    } else {
                        stem_attach_y + stem_length
                    };

                    let stem = create_line(stem_x, stem_attach_y, stem_x, stem_end_y, stem_thickness, black, window_width, window_height);
                    vertices.extend_from_slice(&stem);
                }
            }
        }

        // Move to next system position
        current_y += system_height + system_spacing;
    }

    vertices
}

// WGSL shader with camera transform
const SHADER_SOURCE: &str = r#"
struct Camera {
    transform: vec4<f32>,  // scale_x, scale_y, offset_x, offset_y
}

@group(0) @binding(0)
var<uniform> camera: Camera;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) color: vec4<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    // Apply zoom (scale) and pan (offset)
    let scaled = input.position * camera.transform.xy;
    let transformed = scaled + camera.transform.zw;
    output.position = vec4<f32>(transformed, 0.0, 1.0);
    output.color = input.color;
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    return input.color;
}
"#;

/// SDF shader for pixel-perfect rounded rectangles at any zoom level
const SDF_SHADER_SOURCE: &str = r#"
struct Camera {
    transform: vec4<f32>,  // scale_x, scale_y, offset_x, offset_y
}

@group(0) @binding(0)
var<uniform> camera: Camera;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) rect_center: vec2<f32>,
    @location(2) rect_half_size: vec2<f32>,
    @location(3) corner_radius: f32,
    @location(4) border_width: f32,
    @location(5) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) rect_center: vec2<f32>,
    @location(1) rect_half_size: vec2<f32>,
    @location(2) corner_radius: f32,
    @location(3) border_width: f32,
    @location(4) color: vec4<f32>,
    @location(5) pixel_pos: vec2<f32>,
    @location(6) zoom: f32,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    // Apply camera transform (same as main shader)
    let scaled = in.position * camera.transform.xy;
    let transformed = scaled + camera.transform.zw;
    out.clip_position = vec4<f32>(transformed, 0.0, 1.0);

    // Pass through rect parameters for fragment shader
    out.rect_center = in.rect_center;
    out.rect_half_size = in.rect_half_size;
    out.corner_radius = in.corner_radius;
    out.border_width = in.border_width;
    out.color = in.color;
    out.zoom = camera.transform.x;  // Pass zoom level to fragment shader

    // Calculate pixel position from NDC position (using window dimensions)
    out.pixel_pos = (in.position + 1.0) * 0.5 * vec2<f32>(1200.0, 900.0);
    out.pixel_pos.y = 900.0 - out.pixel_pos.y;  // Flip Y

    return out;
}

// Signed Distance Field for rounded rectangle
fn sdf_rounded_rect(p: vec2<f32>, center: vec2<f32>, half_size: vec2<f32>, radius: f32) -> f32 {
    let d = abs(p - center) - half_size + radius;
    return length(max(d, vec2<f32>(0.0))) + min(max(d.x, d.y), 0.0) - radius;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let dist = sdf_rounded_rect(
        in.pixel_pos,
        in.rect_center,
        in.rect_half_size,
        in.corner_radius
    );

    var color = in.color;

    // Scale the antialiasing width inversely with zoom for crisp edges at all zoom levels
    // At zoom 1.0, aa_width = 0.5 (standard). At zoom 2.0, aa_width = 0.25, etc.
    let aa_width = 0.5 / in.zoom;

    if in.border_width > 0.0 {
        // Stroked rectangle
        let inner_dist = dist + in.border_width;
        let outer_alpha = 1.0 - smoothstep(-aa_width, aa_width, dist);
        let inner_alpha = smoothstep(-aa_width, aa_width, inner_dist);
        color.a *= outer_alpha * inner_alpha;
    } else {
        // Filled rectangle
        color.a *= 1.0 - smoothstep(-aa_width, aa_width, dist);
    }

    return color;
}
"#;

fn create_camera_bind_group_layout(device: &wgpu::Device) -> BindGroupLayout {
    device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("Camera Bind Group Layout"),
        entries: &[wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::VERTEX,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        }],
    })
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }

        let window_attrs = Window::default_attributes()
            .with_title("Engraver - SMuFL Music Notation (Scroll: Pan, Opt+Scroll/Pinch: Zoom, R: Reset)")
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(event_loop.create_window(window_attrs).unwrap());
        self.window = Some(window.clone());

        let instance = Instance::new(&InstanceDescriptor::default());
        let surface = instance.create_surface(window.clone()).unwrap();

        let adapter = pollster::block_on(instance.request_adapter(&RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .expect("Failed to find an appropriate adapter");

        let (device, queue) = pollster::block_on(adapter.request_device(
            &DeviceDescriptor {
                label: Some("Engraver Device"),
                required_features: Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: wgpu::MemoryHints::default(),
                experimental_features: Default::default(),
                trace: Default::default(),
            },
        ))
        .expect("Failed to create device");

        let size = window.inner_size();
        let config = surface
            .get_default_config(&adapter, size.width.max(1), size.height.max(1))
            .expect("Surface not supported");

        surface.configure(&device, &config);

        // Create camera uniform buffer
        let camera = CameraUniform::new();
        let camera_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Camera Buffer"),
            contents: bytemuck::cast_slice(&[camera]),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });

        let camera_bind_group_layout = create_camera_bind_group_layout(&device);
        let camera_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Camera Bind Group"),
            layout: &camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: camera_buffer.as_entire_binding(),
            }],
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Music Notation Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SOURCE.into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Music Pipeline Layout"),
            bind_group_layouts: &[&camera_bind_group_layout],
            immediate_size: 0,
        });

        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Music Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[Vertex::desc()],
                compilation_options: Default::default(),
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(config.format.into())],
                compilation_options: Default::default(),
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        let vertices = build_scene(WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32, self.font.as_ref(), &self.page_style);
        let num_vertices = vertices.len() as u32;

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });

        // Initialize text rendering with glyphon FIRST to measure text dimensions
        let mut font_system = FontSystem::new();
        log::info!("FontSystem initialized with {} fonts", font_system.db().len());

        let swash_cache = SwashCache::new();
        let text_cache = TextCache::new(&device);
        let viewport = Viewport::new(&device, &text_cache);
        let mut atlas = TextAtlas::new(&device, &queue, &text_cache, config.format);
        let text_renderer = TextRenderer::new(
            &mut atlas,
            &device,
            MultisampleState::default(),
            None,
        );

        // Get base positions from layout calculator
        let base_positions = compute_rehearsal_positions(&self.page_style, WINDOW_WIDTH as f32);

        // Create text buffers and compute actual dimensions
        let font_size = 14.0;
        let line_height = 18.0;
        let padding_h = 8.0; // Horizontal padding around text
        let padding_v = 4.0; // Vertical padding around text

        let mut rehearsal_buffers = Vec::new();
        let mut computed_marks = Vec::new();

        for pos in &base_positions {
            // Create text buffer without size constraints to measure natural size
            let mut buffer = TextBuffer::new(&mut font_system, Metrics::new(font_size, line_height));
            buffer.set_size(&mut font_system, Some(500.0), Some(100.0)); // Large initial size
            buffer.set_text(
                &mut font_system,
                pos.name,
                &Attrs::new().family(Family::SansSerif).weight(glyphon::Weight::BOLD),
                Shaping::Advanced,
                None,
            );
            buffer.shape_until_scroll(&mut font_system, false);

            // Get the actual text dimensions from the buffer layout
            let text_width: f32 = buffer.layout_runs()
                .map(|run| run.line_w)
                .next()
                .unwrap_or(50.0);
            let text_height = line_height;

            // TEXT POSITION FIRST: use the capsule position as the text anchor point
            // The layout calculator gives us where the label should appear
            let text_x = pos.capsule_x;
            let text_y = pos.capsule_y;

            // CAPSULE WRAPS AROUND TEXT: derive capsule bounds from text position and size
            let capsule_x = text_x - padding_h;
            let capsule_y = text_y - padding_v;
            let capsule_width = text_width + padding_h * 2.0;
            let capsule_height = text_height + padding_v * 2.0;

            log::info!("Rehearsal '{}': text=({:.1}, {:.1}, {:.1}x{:.1}) -> capsule=({:.1}, {:.1}, {:.1}x{:.1})",
                pos.name, text_x, text_y, text_width, text_height, capsule_x, capsule_y, capsule_width, capsule_height);

            rehearsal_buffers.push((buffer, text_x, text_y));
            computed_marks.push(ComputedRehearsalMark {
                name: pos.name.to_string(),
                capsule_x,
                capsule_y,
                capsule_width,
                capsule_height,
                text_x,
                text_y,
            });
        }

        // Create SDF pipeline for pixel-perfect rounded rectangles
        let sdf_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("SDF Shader"),
            source: wgpu::ShaderSource::Wgsl(SDF_SHADER_SOURCE.into()),
        });

        let sdf_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("SDF Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &sdf_shader,
                entry_point: Some("vs_main"),
                buffers: &[SdfRectVertex::desc()],
                compilation_options: Default::default(),
            },
            fragment: Some(FragmentState {
                module: &sdf_shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        // Build SDF vertices using computed text-based dimensions
        let sdf_vertices = build_sdf_frames_from_computed(&computed_marks, WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32);
        let sdf_num_vertices = sdf_vertices.len() as u32;

        let sdf_vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("SDF Vertex Buffer"),
            contents: bytemuck::cast_slice(&sdf_vertices),
            usage: BufferUsages::VERTEX,
        });

        log::info!("SDF pipeline created with {} vertices for {} rehearsal frames", sdf_num_vertices, computed_marks.len());

        let text_state = TextState {
            font_system,
            swash_cache,
            text_cache,
            viewport,
            atlas,
            text_renderer,
            rehearsal_buffers,
        };

        self.render_state = Some(RenderState {
            surface,
            config,
            device,
            queue,
            pipeline,
            vertex_buffer,
            num_vertices,
            sdf_pipeline,
            sdf_vertex_buffer,
            sdf_num_vertices,
            camera_buffer,
            camera_bind_group,
            text_state,
        });

        window.request_redraw();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let window_size = self.window.as_ref().map(|w| w.inner_size());

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }

            WindowEvent::KeyboardInput { event, .. } => {
                match event.physical_key {
                    PhysicalKey::Code(KeyCode::KeyR) if event.state == ElementState::Pressed => {
                        self.view.reset();
                        self.update_camera();
                        if let Some(window) = &self.window {
                            window.request_redraw();
                        }
                    }
                    PhysicalKey::Code(KeyCode::Space) => {
                        self.view.space_held = event.state == ElementState::Pressed;
                        if !self.view.space_held {
                            self.view.end_pan();
                        }
                    }
                    PhysicalKey::Code(KeyCode::AltLeft | KeyCode::AltRight) => {
                        self.view.alt_held = event.state == ElementState::Pressed;
                    }
                    _ => {}
                }
            }

            WindowEvent::ModifiersChanged(modifiers) => {
                self.view.alt_held = modifiers.state().alt_key();
            }

            WindowEvent::MouseInput { state, button, .. } => {
                match button {
                    MouseButton::Middle => {
                        self.view.middle_held = state == ElementState::Pressed;
                        if state == ElementState::Pressed {
                            self.view.start_pan(self.view.cursor_x, self.view.cursor_y);
                        } else {
                            self.view.end_pan();
                        }
                    }
                    MouseButton::Left => {
                        if self.view.space_held {
                            if state == ElementState::Pressed {
                                self.view.start_pan(self.view.cursor_x, self.view.cursor_y);
                            } else {
                                self.view.end_pan();
                            }
                        }
                    }
                    _ => {}
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                self.view.cursor_x = position.x as f32;
                self.view.cursor_y = position.y as f32;

                if let Some(size) = window_size {
                    if self.view.is_panning {
                        self.view.update_pan(
                            self.view.cursor_x,
                            self.view.cursor_y,
                            size.width as f32,
                            size.height as f32,
                        );
                        self.update_camera();
                        if let Some(window) = &self.window {
                            window.request_redraw();
                        }
                    }
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                if let Some(size) = window_size {
                    let (scroll_x, scroll_y) = match delta {
                        MouseScrollDelta::LineDelta(x, y) => (x * 40.0, y * 40.0),
                        MouseScrollDelta::PixelDelta(pos) => (pos.x as f32, pos.y as f32),
                    };

                    if self.view.alt_held {
                        // Option/Alt + scroll = zoom
                        self.view.zoom_at(
                            self.view.cursor_x,
                            self.view.cursor_y,
                            scroll_y / 50.0,
                            size.width as f32,
                            size.height as f32,
                        );
                    } else {
                        // Normal scroll = pan
                        self.view.scroll_pan(
                            scroll_x,
                            scroll_y,
                            size.width as f32,
                            size.height as f32,
                        );
                    }
                    self.update_camera();
                    if let Some(window) = &self.window {
                        window.request_redraw();
                    }
                }
            }

            // Trackpad pinch-to-zoom gesture (macOS)
            WindowEvent::PinchGesture { delta, .. } => {
                if let Some(size) = window_size {
                    self.view.zoom_at(
                        self.view.cursor_x,
                        self.view.cursor_y,
                        delta as f32 * 5.0,
                        size.width as f32,
                        size.height as f32,
                    );
                    self.update_camera();
                    if let Some(window) = &self.window {
                        window.request_redraw();
                    }
                }
            }

            WindowEvent::Resized(size) => {
                if let Some(state) = &mut self.render_state {
                    state.config.width = size.width.max(1);
                    state.config.height = size.height.max(1);
                    state.surface.configure(&state.device, &state.config);

                    // Use constant window dimensions for consistent layout
                    // (the scene is designed for WINDOW_WIDTH x WINDOW_HEIGHT, camera handles zoom/pan)
                    let vertices = build_scene(WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32, self.font.as_ref(), &self.page_style);
                    state.num_vertices = vertices.len() as u32;
                    state.vertex_buffer =
                        state.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                            label: Some("Vertex Buffer"),
                            contents: bytemuck::cast_slice(&vertices),
                            usage: BufferUsages::VERTEX,
                        });

                    if let Some(window) = &self.window {
                        window.request_redraw();
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                let Some(state) = &mut self.render_state else {
                    return;
                };

                let frame = state
                    .surface
                    .get_current_texture()
                    .expect("Failed to get surface texture");
                let view = frame.texture.create_view(&TextureViewDescriptor::default());

                let mut encoder = state
                    .device
                    .create_command_encoder(&CommandEncoderDescriptor {
                        label: Some("Render Encoder"),
                    });

                // Text rendering setup - must happen before render pass
                let text_state = &mut state.text_state;

                // Update viewport with current surface size
                text_state.viewport.update(
                    &state.queue,
                    Resolution {
                        width: state.config.width,
                        height: state.config.height,
                    },
                );

                // Build text areas for each rehearsal mark with camera transform
                let width = state.config.width as f32;
                let height = state.config.height as f32;
                let zoom = self.view.zoom;
                let pan_x = self.view.pan_x;
                let pan_y = self.view.pan_y;

                // Scale factor for retina displays (physical / logical)
                let scale_factor = width / WINDOW_WIDTH as f32;

                let text_areas: Vec<TextArea> = text_state
                    .rehearsal_buffers
                    .iter()
                    .map(|(buffer, page_x, page_y)| {
                        // Convert page coordinates to NDC (same as geometry vertices)
                        let ndc_x = (*page_x / WINDOW_WIDTH as f32) * 2.0 - 1.0;
                        let ndc_y = 1.0 - (*page_y / WINDOW_HEIGHT as f32) * 2.0;

                        // Apply camera transform (same as vertex shader)
                        let cam_x = ndc_x * zoom + pan_x;
                        let cam_y = ndc_y * zoom + pan_y;

                        // Convert back to logical screen coords, then scale to physical
                        let logical_x = (cam_x + 1.0) * WINDOW_WIDTH as f32 / 2.0;
                        let logical_y = (1.0 - cam_y) * WINDOW_HEIGHT as f32 / 2.0;

                        // Scale to physical screen coords
                        let screen_x = logical_x * scale_factor;
                        let screen_y = logical_y * scale_factor;

                        TextArea {
                            buffer,
                            left: screen_x,
                            top: screen_y,
                            scale: zoom * scale_factor, // Scale with both zoom and DPI
                            bounds: TextBounds {
                                left: 0,
                                top: 0,
                                right: width as i32,
                                bottom: height as i32,
                            },
                            default_color: TextColor::rgba(229, 33, 0, 255), // Matches REHEARSAL_RED_SRGB
                            custom_glyphs: &[],
                        }
                    })
                    .collect();

                // Prepare text for rendering (must happen before render pass)
                if let Err(e) = text_state.text_renderer.prepare(
                    &state.device,
                    &state.queue,
                    &mut text_state.font_system,
                    &mut text_state.atlas,
                    &text_state.viewport,
                    text_areas,
                    &mut text_state.swash_cache,
                ) {
                    log::error!("Failed to prepare text: {:?}", e);
                }

                // Single render pass for both geometry and text
                {
                    let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                        label: Some("Music Render Pass"),
                        color_attachments: &[Some(RenderPassColorAttachment {
                            view: &view,
                            resolve_target: None,
                            ops: Operations {
                                // Dark gray background (like MuseScore/Figma canvas)
                                load: LoadOp::Clear(Color {
                                    r: 0.2,
                                    g: 0.2,
                                    b: 0.22,
                                    a: 1.0,
                                }),
                                store: StoreOp::Store,
                            },
                            depth_slice: None,
                        })],
                        depth_stencil_attachment: None,
                        timestamp_writes: None,
                        occlusion_query_set: None,
                        multiview_mask: None,
                    });

                    // Draw main geometry (staff lines, notes, etc.)
                    render_pass.set_pipeline(&state.pipeline);
                    render_pass.set_bind_group(0, &state.camera_bind_group, &[]);
                    render_pass.set_vertex_buffer(0, state.vertex_buffer.slice(..));
                    render_pass.draw(0..state.num_vertices, 0..1);

                    // Draw SDF rounded rectangles (rehearsal mark frames)
                    if state.sdf_num_vertices > 0 {
                        render_pass.set_pipeline(&state.sdf_pipeline);
                        render_pass.set_bind_group(0, &state.camera_bind_group, &[]);
                        render_pass.set_vertex_buffer(0, state.sdf_vertex_buffer.slice(..));
                        render_pass.draw(0..state.sdf_num_vertices, 0..1);
                    }

                    // Draw text in the same pass
                    if let Err(e) = text_state.text_renderer.render(
                        &text_state.atlas,
                        &text_state.viewport,
                        &mut render_pass,
                    ) {
                        log::error!("Failed to render text: {:?}", e);
                    }
                }

                state.queue.submit(Some(encoder.finish()));
                frame.present();
            }
            _ => {}
        }
    }
}

fn main() {
    env_logger::init();
    log::info!("Starting Engraver Music Notation Demo with SMuFL Font Rendering");
    log::info!("Controls: Scroll=Pan, Opt+Scroll/Pinch=Zoom, Middle-drag/Space+drag=Pan, R=Reset");

    let event_loop = EventLoop::new().unwrap();
    let mut app = App::new();
    event_loop.run_app(&mut app).unwrap();
}
