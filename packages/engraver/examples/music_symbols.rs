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
        self.zoom = (self.zoom * zoom_factor).clamp(0.1, 10.0);

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

struct RenderState {
    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
    device: wgpu::Device,
    queue: wgpu::Queue,
    pipeline: RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    num_vertices: u32,
    camera_buffer: wgpu::Buffer,
    camera_bind_group: BindGroup,
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

/// Build all the geometry for the music notation demo
/// Uses the page style to render on proper paper dimensions
fn build_scene(window_width: f32, window_height: f32, font: Option<&LoadedFont>, page_style: &PageStyle) -> Vec<Vertex> {
    let mut vertices = Vec::new();

    // Colors
    let black = [0.0, 0.0, 0.0, 1.0];
    let paper_white = [1.0, 1.0, 1.0, 1.0];
    let paper_shadow = [0.15, 0.15, 0.17, 1.0]; // Darker shadow for dark background

    // Calculate page dimensions in pixels at screen DPI
    let page_width = page_style.size.width_px(SCREEN_DPI);
    let page_height = page_style.size.height_px(SCREEN_DPI);

    // Center the page in the window with some padding
    let page_x = (window_width - page_width) / 2.0;
    let page_y = 20.0; // Small top margin in window

    // Draw paper shadow (offset slightly down-right)
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
    let margins = page_style.margins(false); // First page (odd)
    let margin_left_px = margins.left * SCREEN_DPI;
    let margin_top_px = margins.top * SCREEN_DPI;
    let margin_right_px = margins.right * SCREEN_DPI;
    let margin_bottom_px = margins.bottom * SCREEN_DPI;

    // Content area boundaries (within page margins)
    let content_left = page_x + margin_left_px;
    let content_top = page_y + margin_top_px;
    let content_right = page_x + page_width - margin_right_px;
    let _content_bottom = page_y + page_height - margin_bottom_px;
    let content_width = content_right - content_left;

    // Use spatium from page style for all music spacing
    let staff_space = page_style.spatium_px(SCREEN_DPI);
    let font_size = staff_space * 4.0; // SMuFL fonts are designed at 4 staff spaces per em

    // === Staff 1: Treble clef with notes ===
    let staff_y = content_top + staff_space * 2.0; // Small offset from top margin
    let staff_left = content_left;
    let staff_right = content_right;

    // Draw 5 staff lines
    for i in 0..5 {
        let y = staff_y + (i as f32) * staff_space;
        let line = create_line(staff_left, y, staff_right, y, 1.0, black, window_width, window_height);
        vertices.extend_from_slice(&line);
    }

    // Left barline
    let barline = create_line(
        staff_left,
        staff_y,
        staff_left,
        staff_y + 4.0 * staff_space,
        2.0,
        black,
        window_width,
        window_height,
    );
    vertices.extend_from_slice(&barline);

    // Right barline
    let barline = create_line(
        staff_right,
        staff_y,
        staff_right,
        staff_y + 4.0 * staff_space,
        2.0,
        black,
        window_width,
        window_height,
    );
    vertices.extend_from_slice(&barline);

    // Render SMuFL glyphs if font is available
    if let Some(loaded_font) = font {
        let font_ref = loaded_font.font_ref();

        // G Clef - SMuFL origin is on the G line (2nd line from bottom = line 1 in 0-4 numbering)
        // In our coords: staff_y is TOP line (line 0), so G line is at staff_y + 3*staff_space
        if let Some(gid) = get_glyph_id(&font_ref, Glyph::GClef) {
            let clef_x = staff_left + staff_space * 0.5;
            // G line is line 3 (counting from top: 0=top, 4=bottom)
            let clef_y = staff_y + staff_space * 3.0;
            let clef_verts = tessellate_glyph(&font_ref, gid, font_size, clef_x, clef_y, black, window_width, window_height);
            vertices.extend(clef_verts);
        }

        // Time signature: Common time (4/4)
        // Origin is centered vertically on the staff (at the middle line B4)
        if let Some(gid) = get_glyph_id(&font_ref, Glyph::TimeSigCommon) {
            let ts_x = staff_left + staff_space * 5.0;
            let ts_y = staff_y + staff_space * 2.0; // Middle line (B4)
            let ts_verts = tessellate_glyph(&font_ref, gid, font_size, ts_x, ts_y, black, window_width, window_height);
            vertices.extend(ts_verts);
        }

        // Notes - quarter notes (black noteheads with stems)
        // Positions are now relative to staff_left using staff_space units
        // Staff positions: 4 = bottom line (E4), 3 = F4, 2 = G4, 1 = A4, 0 = B4 (middle line)
        let note_start_x = staff_left + staff_space * 8.0;
        let note_spacing = staff_space * 3.0;
        let note_positions: [(f32, f32); 8] = [
            (note_start_x + note_spacing * 0.0, 4.0),  // E4 (bottom line)
            (note_start_x + note_spacing * 1.0, 3.5),  // F4
            (note_start_x + note_spacing * 2.0, 3.0),  // G4
            (note_start_x + note_spacing * 3.0, 2.5),  // A4
            (note_start_x + note_spacing * 4.0, 2.0),  // B4 (middle line)
            (note_start_x + note_spacing * 5.0, 1.5),  // C5
            (note_start_x + note_spacing * 6.0, 1.0),  // D5
            (note_start_x + note_spacing * 7.0, 0.5),  // E5
        ];

        // Get stem thickness from engraving defaults
        let stem_thickness = loaded_font.metadata.engraving_defaults.stem_thickness
            .map(|s| f64::from(s) as f32 * staff_space)
            .unwrap_or(1.2);

        for (x, staff_pos) in note_positions {
            // Convert staff position to screen Y
            // staff_pos 4 = bottom line, staff_pos 0 = top line
            let note_y = staff_y + (4.0 - staff_pos) * staff_space;

            // Black notehead - draw at position
            if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadBlack) {
                let nh_verts = tessellate_glyph(&font_ref, gid, font_size, x, note_y, black, window_width, window_height);
                vertices.extend(nh_verts);
            }

            // Stem direction: up for notes below middle line (B4), down for notes on/above
            let stem_up = staff_pos > 2.0;
            let stem_length = staff_space * 3.5;

            // Use SMuFL anchor points for stem attachment
            // SMuFL anchors define the CORNER of the stem rectangle, not the center
            // stemUpSE = bottom-right corner of upward stem
            // stemDownNW = top-left corner of downward stem
            let (stem_x, stem_attach_y) = if stem_up {
                // Stem up: stemUpSE is the bottom-right corner of the stem
                // So stem center X = anchor.x - stemThickness/2
                if let Some((ax, ay)) = loaded_font.stem_up_se(Glyph::NoteheadBlack, staff_space) {
                    // SMuFL Y is up, screen Y is down, so we subtract ay
                    (x + ax - stem_thickness / 2.0, note_y - ay)
                } else {
                    // Fallback: right edge minus half stem
                    let nh_width = loaded_font.glyph_width(Glyph::NoteheadBlack, staff_space);
                    (x + nh_width - stem_thickness / 2.0, note_y)
                }
            } else {
                // Stem down: stemDownNW is the top-left corner of the stem
                // So stem center X = anchor.x + stemThickness/2
                if let Some((ax, ay)) = loaded_font.stem_down_nw(Glyph::NoteheadBlack, staff_space) {
                    (x + ax + stem_thickness / 2.0, note_y - ay)
                } else {
                    // Fallback: left edge plus half stem
                    (x + stem_thickness / 2.0, note_y)
                }
            };

            let stem_end_y = if stem_up {
                stem_attach_y - stem_length
            } else {
                stem_attach_y + stem_length
            };

            let stem = create_line(stem_x, stem_attach_y, stem_x, stem_end_y, stem_thickness, black, window_width, window_height);
            vertices.extend_from_slice(&stem);

            // Ledger lines if needed
            let notehead_width = loaded_font.glyph_width(Glyph::NoteheadBlack, staff_space);
            if staff_pos > 4.0 {
                // Below staff (low notes)
                let mut ledger_pos = 5.0;
                while ledger_pos <= staff_pos + 0.25 {
                    let ledger_y = staff_y + (4.0 - ledger_pos) * staff_space;
                    let ledger = create_line(
                        x - staff_space * 0.3,
                        ledger_y,
                        x + notehead_width + staff_space * 0.3,
                        ledger_y,
                        1.0,
                        black,
                        window_width,
                        window_height,
                    );
                    vertices.extend_from_slice(&ledger);
                    ledger_pos += 1.0;
                }
            } else if staff_pos < 0.0 {
                // Above staff (high notes)
                let mut ledger_pos = -1.0;
                while ledger_pos >= staff_pos - 0.25 {
                    let ledger_y = staff_y + (4.0 - ledger_pos) * staff_space;
                    let ledger = create_line(
                        x - staff_space * 0.3,
                        ledger_y,
                        x + notehead_width + staff_space * 0.3,
                        ledger_y,
                        1.0,
                        black,
                        window_width,
                        window_height,
                    );
                    vertices.extend_from_slice(&ledger);
                    ledger_pos -= 1.0;
                }
            }
        }

        // Half notes (relative to content area)
        let half_note_positions: [(f32, f32); 2] = [
            (note_start_x + note_spacing * 9.0, 3.0), // G4 (3rd line from bottom)
            (note_start_x + note_spacing * 11.0, 2.0), // B4 (middle line)
        ];

        for (x, staff_pos) in half_note_positions {
            let note_y = staff_y + (4.0 - staff_pos) * staff_space;

            // Half notehead (open)
            if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadHalf) {
                let nh_verts = tessellate_glyph(&font_ref, gid, font_size, x, note_y, black, window_width, window_height);
                vertices.extend(nh_verts);
            }

            // Stem up using anchor (stemUpSE is bottom-right corner)
            let stem_length = staff_space * 3.5;
            let (stem_x, stem_attach_y) = if let Some((ax, ay)) = loaded_font.stem_up_se(Glyph::NoteheadHalf, staff_space) {
                (x + ax - stem_thickness / 2.0, note_y - ay)
            } else {
                let nh_width = loaded_font.glyph_width(Glyph::NoteheadHalf, staff_space);
                (x + nh_width - stem_thickness / 2.0, note_y)
            };
            let stem = create_line(stem_x, stem_attach_y, stem_x, stem_attach_y - stem_length, stem_thickness, black, window_width, window_height);
            vertices.extend_from_slice(&stem);
        }

        // Whole note (no stem) - positioned relative to content area
        if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadWhole) {
            let x = note_start_x + note_spacing * 13.0;
            let note_y = staff_y + (4.0 - 3.0) * staff_space; // G4 (line 3)
            let nh_verts = tessellate_glyph(&font_ref, gid, font_size, x, note_y, black, window_width, window_height);
            vertices.extend(nh_verts);
        }

        // === Second staff: Bass clef with rests ===
        // Position relative to first staff using staff_space units
        let staff2_y = staff_y + staff_space * 12.0; // Space between staves

        // Draw 5 staff lines
        for i in 0..5 {
            let y = staff2_y + (i as f32) * staff_space;
            let line = create_line(staff_left, y, staff_right, y, 1.0, black, window_width, window_height);
            vertices.extend_from_slice(&line);
        }

        // Barlines
        let barline = create_line(
            staff_left,
            staff2_y,
            staff_left,
            staff2_y + 4.0 * staff_space,
            2.0,
            black,
            window_width,
            window_height,
        );
        vertices.extend_from_slice(&barline);
        let barline = create_line(
            staff_right,
            staff2_y,
            staff_right,
            staff2_y + 4.0 * staff_space,
            2.0,
            black,
            window_width,
            window_height,
        );
        vertices.extend_from_slice(&barline);

        // Bass clef (F clef sits on the 4th line from bottom = line 1)
        if let Some(gid) = get_glyph_id(&font_ref, Glyph::FClef) {
            let clef_x = staff_left + staff_space * 0.5;
            let clef_y = staff2_y + staff_space * 1.0; // F line position
            let clef_verts = tessellate_glyph(&font_ref, gid, font_size, clef_x, clef_y, black, window_width, window_height);
            vertices.extend(clef_verts);
        }

        // Time signature
        if let Some(gid) = get_glyph_id(&font_ref, Glyph::TimeSigCommon) {
            let ts_x = staff_left + staff_space * 5.0;
            let ts_y = staff2_y + staff_space * 2.0;
            let ts_verts = tessellate_glyph(&font_ref, gid, font_size, ts_x, ts_y, black, window_width, window_height);
            vertices.extend(ts_verts);
        }

        // Rests - positioned relative to content area
        let rest_start_x = staff_left + staff_space * 10.0;
        let rest_spacing = staff_space * 6.0;
        let rests: [(f32, Glyph); 5] = [
            (rest_start_x + rest_spacing * 0.0, Glyph::RestWhole),
            (rest_start_x + rest_spacing * 1.0, Glyph::RestHalf),
            (rest_start_x + rest_spacing * 2.0, Glyph::RestQuarter),
            (rest_start_x + rest_spacing * 3.0, Glyph::Rest8th),
            (rest_start_x + rest_spacing * 4.0, Glyph::Rest16th),
        ];

        for (x, rest_glyph) in rests {
            if let Some(gid) = get_glyph_id(&font_ref, rest_glyph) {
                let rest_y = staff2_y + staff_space * 2.0; // Center of staff
                let rest_verts = tessellate_glyph(&font_ref, gid, font_size, x, rest_y, black, window_width, window_height);
                vertices.extend(rest_verts);
            }
        }

        // Grand staff brace connection
        let brace_x = staff_left - staff_space * 0.5;
        let brace = create_line(brace_x, staff_y, brace_x, staff2_y + 4.0 * staff_space, 3.0, black, window_width, window_height);
        vertices.extend_from_slice(&brace);

        // === Third row: Accidentals demo ===
        let acc_y = staff2_y + staff_space * 12.0;
        let acc_staff_width = content_width * 0.6; // Shorter staff for accidentals

        // Draw staff for accidentals
        for i in 0..5 {
            let y = acc_y + (i as f32) * staff_space;
            let line = create_line(staff_left, y, staff_left + acc_staff_width, y, 1.0, black, window_width, window_height);
            vertices.extend_from_slice(&line);
        }

        // Accidentals - positioned relative to content area
        let acc_spacing = staff_space * 5.0;
        let accidentals: [(f32, Glyph); 5] = [
            (staff_left + acc_spacing * 1.0, Glyph::AccidentalDoubleFlat),
            (staff_left + acc_spacing * 2.0, Glyph::AccidentalFlat),
            (staff_left + acc_spacing * 3.0, Glyph::AccidentalNatural),
            (staff_left + acc_spacing * 4.0, Glyph::AccidentalSharp),
            (staff_left + acc_spacing * 5.0, Glyph::AccidentalDoubleSharp),
        ];

        for (x, acc_glyph) in accidentals {
            if let Some(gid) = get_glyph_id(&font_ref, acc_glyph) {
                let note_y = acc_y + staff_space * 2.0;
                let acc_verts = tessellate_glyph(&font_ref, gid, font_size, x, note_y, black, window_width, window_height);
                vertices.extend(acc_verts);
            }

            // Notehead after accidental
            if let Some(gid) = get_glyph_id(&font_ref, Glyph::NoteheadBlack) {
                let note_y = acc_y + staff_space * 2.0;
                let nh_verts = tessellate_glyph(&font_ref, gid, font_size, x + staff_space * 1.5, note_y, black, window_width, window_height);
                vertices.extend(nh_verts);
            }
        }

        // === Dynamics row ===
        let dyn_y = acc_y + staff_space * 8.0;
        let dyn_spacing = staff_space * 4.0;

        let dynamics: [(f32, Glyph); 6] = [
            (staff_left + dyn_spacing * 0.0, Glyph::DynamicPiano),
            (staff_left + dyn_spacing * 1.0, Glyph::DynamicMezzo),
            (staff_left + dyn_spacing * 2.0, Glyph::DynamicForte),
            (staff_left + dyn_spacing * 4.0, Glyph::DynamicPiano), // pp (would need combining)
            (staff_left + dyn_spacing * 5.0, Glyph::DynamicForte), // ff
            (staff_left + dyn_spacing * 6.0, Glyph::DynamicSforzando1),
        ];

        for (x, dyn_glyph) in dynamics {
            if let Some(gid) = get_glyph_id(&font_ref, dyn_glyph) {
                let dyn_verts = tessellate_glyph(&font_ref, gid, font_size * 0.8, x, dyn_y, black, window_width, window_height);
                vertices.extend(dyn_verts);
            }
        }

    } else {
        // Fallback: placeholder text (no font loaded)
        let hint_bg = create_rect(content_left, content_top + staff_space * 5.0, content_width * 0.6, staff_space * 4.0, [0.95, 0.90, 0.90, 1.0], window_width, window_height);
        vertices.extend_from_slice(&hint_bg);
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
            },
            None,
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
            push_constant_ranges: &[],
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
            multiview: None,
            cache: None,
        });

        let vertices = build_scene(WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32, self.font.as_ref(), &self.page_style);
        let num_vertices = vertices.len() as u32;

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX,
        });

        self.render_state = Some(RenderState {
            surface,
            config,
            device,
            queue,
            pipeline,
            vertex_buffer,
            num_vertices,
            camera_buffer,
            camera_bind_group,
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

                    let vertices = build_scene(size.width as f32, size.height as f32, self.font.as_ref(), &self.page_style);
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
                let Some(state) = &self.render_state else {
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
                        })],
                        depth_stencil_attachment: None,
                        timestamp_writes: None,
                        occlusion_query_set: None,
                    });

                    render_pass.set_pipeline(&state.pipeline);
                    render_pass.set_bind_group(0, &state.camera_bind_group, &[]);
                    render_pass.set_vertex_buffer(0, state.vertex_buffer.slice(..));
                    render_pass.draw(0..state.num_vertices, 0..1);
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
