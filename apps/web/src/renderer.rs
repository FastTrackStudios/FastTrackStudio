//! Vello Chart Renderer Module
//!
//! Renders charts using Vello with WebGPU backend.
//! Ported from apps/web/src/renderer.rs for the documentation site.

use std::sync::Arc;

use keyflow::Chart;
use keyflow::engraver::fonts::SMuFLFont;
use keyflow::engraver::layout::ChartLayoutMode;
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult};
use keyflow::engraver::renderer::scene_renderer::SceneRenderBuilder;
use keyflow::engraver::style::MStyle;
use vello::Scene;
use vello::kurbo::{Affine, Rect};
use vello::peniko::Color;

// Embedded fonts from musescore reference library
static BRAVURA_FONT: &[u8] =
    include_bytes!("../../../libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf");
static BRAVURA_METADATA: &[u8] = include_bytes!(
    "../../../libs/reference/sheet-music/musescore/fonts/bravura/bravura_metadata.json"
);
static TEXT_FONT: &[u8] =
    include_bytes!("../../../libs/reference/sheet-music/musescore/fonts/FreeSans.ttf");
static MUSEJAZZ_TEXT_FONT: &[u8] =
    include_bytes!("../../../libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf");

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

/// Chart layout and rendering engine for web.
///
/// Manages fonts, layout engine, and Vello scene rendering.
pub struct ChartLayoutManager {
    /// SMuFL font for music notation
    smufl_font: SMuFLFont<'static>,
    /// Text font data (FreeSans)
    text_font_data: Arc<Vec<u8>>,
    /// MuseJazz font data for chord symbols
    musejazz_font_data: Arc<Vec<u8>>,
    /// Layout engine
    layout_engine: ChartLayoutEngine,
    /// Cached layout result
    layout_result: Option<ChartLayoutResult>,
    /// Last rendered chart hash (for cache invalidation)
    last_chart_hash: u64,
    /// WebGPU renderer (WASM only)
    #[cfg(target_arch = "wasm32")]
    wgpu_renderer: Option<wasm::WebGpuRenderer>,
}

impl ChartLayoutManager {
    /// Create a new chart layout manager with embedded fonts.
    pub fn new() -> Result<Self, String> {
        // Load SMuFL font with metadata
        let smufl_font = SMuFLFont::from_reader(BRAVURA_FONT, BRAVURA_METADATA)
            .map_err(|e| format!("Failed to load Bravura font: {e}"))?;

        // Store font data as Arc for sharing with renderer
        let text_font_data = Arc::new(TEXT_FONT.to_vec());
        let musejazz_font_data = Arc::new(MUSEJAZZ_TEXT_FONT.to_vec());
        let symbol_font_data = Arc::new(BRAVURA_FONT.to_vec());

        // Create layout engine with fonts
        // Leak the style for 'static lifetime (web app runs for session duration)
        let style = Box::leak(Box::new(MStyle::new()));
        // Use MuseJazz for text metrics since the default harmony style uses MuseJazzText font.
        // This ensures measurements match the rendered glyphs.
        let layout_engine = ChartLayoutEngine::new(style, musejazz_font_data.clone(), symbol_font_data);

        Ok(Self {
            smufl_font,
            text_font_data,
            musejazz_font_data,
            layout_engine,
            layout_result: None,
            last_chart_hash: 0,
            #[cfg(target_arch = "wasm32")]
            wgpu_renderer: None,
        })
    }

    /// Layout a chart using snippet mode (content-sized, no fixed page).
    pub fn layout_chart(&mut self, chart: &Chart, viewport_width: f64, _viewport_height: f64) {
        self.layout_chart_with_mode(chart, viewport_width, true);
    }

    /// Layout a chart with a specified mode.
    ///
    /// # Arguments
    /// * `chart` - The parsed chart to layout
    /// * `viewport_width` - Width of the viewport in CSS pixels
    /// * `snippet_mode` - If true, use snippet mode (content-sized). If false, use A4 paginated mode.
    pub fn layout_chart_with_mode(
        &mut self,
        chart: &Chart,
        viewport_width: f64,
        snippet_mode: bool,
    ) {
        // Simple hash based on chart data and mode
        let chart_hash = self.compute_chart_hash_with_mode(chart, snippet_mode);

        // Skip if already laid out
        if self.layout_result.is_some() && chart_hash == self.last_chart_hash {
            return;
        }

        // A4 dimensions in points (72 points per inch)
        const A4_WIDTH: f64 = 595.0; // 210mm = 8.27" = 595 points
        const A4_HEIGHT: f64 = 842.0; // 297mm = 11.69" = 842 points

        let (mode, config) = if snippet_mode {
            // Snippet mode: content-sized, minimal margins
            let config = ChartLayoutConfig::snippet();
            let mode = ChartLayoutMode::Snippet {
                page_width: viewport_width / DPI_SCALE,
            };
            (mode, config)
        } else {
            // Page mode: A4 paginated with Master Rhythm preset
            let config = ChartLayoutConfig::master_rhythm();
            let mode = ChartLayoutMode::Paginated {
                page_width: A4_WIDTH,
                page_height: A4_HEIGHT,
            };
            (mode, config)
        };

        // Perform layout
        let result = self
            .layout_engine
            .layout_chart_with_config(chart, &mode, &config);

        self.layout_result = Some(result);
        self.last_chart_hash = chart_hash;
    }

    /// Compute a hash of the chart including layout mode.
    ///
    /// This hash is used for cache invalidation - if the hash changes, we re-layout.
    /// We hash the debug representation of the chart which includes all content.
    fn compute_chart_hash_with_mode(&self, chart: &Chart, snippet_mode: bool) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        // Hash the entire chart structure using debug representation
        // This ensures ANY change to the chart content invalidates the cache
        format!("{:?}", chart).hash(&mut hasher);
        snippet_mode.hash(&mut hasher);

        hasher.finish()
    }

    /// Get the cached layout result.
    pub fn layout_result(&self) -> Option<&ChartLayoutResult> {
        self.layout_result.as_ref()
    }

    /// Render the chart to a Vello scene.
    pub fn render_to_scene(&self, scene: &mut Scene, width: f64, height: f64, transform: Affine) {
        // Fill background
        scene.fill(
            vello::peniko::Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(55, 65, 81), // gray-700
            None,
            &Rect::new(0.0, 0.0, width, height),
        );

        // Render layout if available
        if let Some(ref layout) = self.layout_result {
            // Create scene renderer with fonts
            let mut renderer = SceneRenderBuilder::new()
                .spatium(5.0)
                .build()
                .with_font(&self.smufl_font)
                .with_text_font_arc(self.text_font_data.clone())
                .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone())
                .with_named_font_arc("section-note", self.text_font_data.clone())
                .with_named_font_arc("title-bold", self.text_font_data.clone())
                .with_named_font_arc("part-name-bold", self.text_font_data.clone());

            // Set viewport for culling
            let viewport_rect = Rect::new(0.0, 0.0, width, height);
            renderer.set_viewport(viewport_rect);

            // Render with user's view transform
            renderer.render_with_transform(scene, &layout.scene, transform);
        }
    }

    /// Render to a canvas element (WASM only).
    #[cfg(target_arch = "wasm32")]
    pub async fn render_to_canvas(
        &mut self,
        canvas: &web_sys::HtmlCanvasElement,
    ) -> Result<(), String> {
        use wasm::WebGpuRenderer;

        // Initialize renderer if needed
        if self.wgpu_renderer.is_none() {
            let renderer = WebGpuRenderer::new(canvas.clone()).await?;
            self.wgpu_renderer = Some(renderer);
        }

        // Get canvas dimensions first
        let canvas_width = canvas.width();
        let canvas_height = canvas.height();

        // Resize if needed (access renderer mutably)
        if let Some(renderer) = self.wgpu_renderer.as_mut() {
            let (current_width, current_height) = renderer.dimensions();
            if current_width != canvas_width || current_height != canvas_height {
                renderer.resize(canvas_width, canvas_height);
            }
        }

        // Create scene (borrow self immutably for rendering data)
        let mut scene = Scene::new();
        let transform = Affine::translate((20.0, 20.0)) * Affine::scale(DPI_SCALE);

        // Fill background
        scene.fill(
            vello::peniko::Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(55, 65, 81),
            None,
            &Rect::new(0.0, 0.0, canvas_width as f64, canvas_height as f64),
        );

        // Render layout if available
        if let Some(ref layout) = self.layout_result {
            // Create scene renderer with fonts
            let mut renderer = SceneRenderBuilder::new()
                .spatium(5.0)
                .build()
                .with_font(&self.smufl_font)
                .with_text_font_arc(self.text_font_data.clone())
                .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone())
                .with_named_font_arc("section-note", self.text_font_data.clone())
                .with_named_font_arc("title-bold", self.text_font_data.clone())
                .with_named_font_arc("part-name-bold", self.text_font_data.clone());

            // Set viewport for culling
            let viewport_rect = Rect::new(0.0, 0.0, canvas_width as f64, canvas_height as f64);
            renderer.set_viewport(viewport_rect);

            // Render with user's view transform
            renderer.render_with_transform(&mut scene, &layout.scene, transform);
        }

        // Render to canvas (access wgpu_renderer mutably again)
        if let Some(wgpu_renderer) = self.wgpu_renderer.as_mut() {
            wgpu_renderer.render(&scene)
        } else {
            Err("WebGPU renderer not initialized".to_string())
        }
    }

    /// Render to a canvas element with custom transform (WASM only).
    ///
    /// # Arguments
    /// * `canvas` - The HTML canvas element to render to
    /// * `translate_x` - X translation in pixels (already scaled by DPR)
    /// * `translate_y` - Y translation in pixels (already scaled by DPR)
    /// * `scale` - Scale factor (already includes DPR)
    #[cfg(target_arch = "wasm32")]
    pub async fn render_to_canvas_with_transform(
        &mut self,
        canvas: &web_sys::HtmlCanvasElement,
        translate_x: f64,
        translate_y: f64,
        scale: f64,
    ) -> Result<(), String> {
        use wasm::WebGpuRenderer;

        // Initialize renderer if needed
        if self.wgpu_renderer.is_none() {
            let renderer = WebGpuRenderer::new(canvas.clone()).await?;
            self.wgpu_renderer = Some(renderer);
        }

        // Get canvas dimensions first
        let canvas_width = canvas.width();
        let canvas_height = canvas.height();

        // Resize if needed (access renderer mutably)
        if let Some(renderer) = self.wgpu_renderer.as_mut() {
            let (current_width, current_height) = renderer.dimensions();
            if current_width != canvas_width || current_height != canvas_height {
                renderer.resize(canvas_width, canvas_height);
            }
        }

        // Create scene (borrow self immutably for rendering data)
        let mut scene = Scene::new();

        // Build transform: translate then scale
        let transform = Affine::translate((translate_x, translate_y)) * Affine::scale(scale);

        // Fill background
        scene.fill(
            vello::peniko::Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(55, 65, 81),
            None,
            &Rect::new(0.0, 0.0, canvas_width as f64, canvas_height as f64),
        );

        // Render layout if available
        if let Some(ref layout) = self.layout_result {
            // Create scene renderer with fonts
            let mut renderer = SceneRenderBuilder::new()
                .spatium(5.0)
                .build()
                .with_font(&self.smufl_font)
                .with_text_font_arc(self.text_font_data.clone())
                .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone())
                .with_named_font_arc("section-note", self.text_font_data.clone())
                .with_named_font_arc("title-bold", self.text_font_data.clone())
                .with_named_font_arc("part-name-bold", self.text_font_data.clone());

            // Set viewport for culling
            let viewport_rect = Rect::new(0.0, 0.0, canvas_width as f64, canvas_height as f64);
            renderer.set_viewport(viewport_rect);

            // Render with user's view transform
            renderer.render_with_transform(&mut scene, &layout.scene, transform);
        }

        // Render to canvas (access wgpu_renderer mutably again)
        if let Some(wgpu_renderer) = self.wgpu_renderer.as_mut() {
            wgpu_renderer.render(&scene)
        } else {
            Err("WebGPU renderer not initialized".to_string())
        }
    }
}

impl Default for ChartLayoutManager {
    fn default() -> Self {
        Self::new().expect("Failed to initialize chart layout manager")
    }
}

#[cfg(target_arch = "wasm32")]
pub mod wasm {
    //! WASM-specific Vello/WebGPU rendering.

    use super::*;
    use web_sys::HtmlCanvasElement;
    use wgpu::{TextureDescriptor, TextureDimension, TextureUsages, TextureViewDescriptor};

    /// WebGPU renderer state for WASM.
    pub struct WebGpuRenderer {
        device: wgpu::Device,
        queue: wgpu::Queue,
        surface: wgpu::Surface<'static>,
        surface_config: wgpu::SurfaceConfiguration,
        vello_renderer: vello::Renderer,
        render_texture: wgpu::Texture,
        blitter: wgpu::util::TextureBlitter,
        width: u32,
        height: u32,
    }

    impl WebGpuRenderer {
        /// Create a new WebGPU renderer from a canvas element.
        pub async fn new(canvas: HtmlCanvasElement) -> Result<Self, String> {
            // Get canvas dimensions
            let width = canvas.width().max(1);
            let height = canvas.height().max(1);

            // Create wgpu instance
            let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
                backends: wgpu::Backends::BROWSER_WEBGPU | wgpu::Backends::GL,
                ..Default::default()
            });

            // Create surface from canvas
            let surface = instance
                .create_surface(wgpu::SurfaceTarget::Canvas(canvas))
                .map_err(|e| format!("Failed to create surface: {e}"))?;

            // Request adapter
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    compatible_surface: Some(&surface),
                    force_fallback_adapter: false,
                })
                .await
                .map_err(|e| format!("Failed to find suitable GPU adapter: {e}"))?;

            // Request device
            let (device, queue) = adapter
                .request_device(&wgpu::DeviceDescriptor {
                    label: Some("Docsite Chart Renderer"),
                    required_features: wgpu::Features::empty(),
                    required_limits: wgpu::Limits::downlevel_webgl2_defaults(),
                    ..Default::default()
                })
                .await
                .map_err(|e| format!("Failed to create device: {e}"))?;

            // Configure surface (prefer non-sRGB for Vello)
            let surface_caps = surface.get_capabilities(&adapter);
            let surface_format = surface_caps
                .formats
                .iter()
                .find(|f| !f.is_srgb())
                .copied()
                .unwrap_or(surface_caps.formats[0]);

            let surface_config = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format: surface_format,
                width,
                height,
                present_mode: wgpu::PresentMode::AutoVsync,
                alpha_mode: wgpu::CompositeAlphaMode::Auto,
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };
            surface.configure(&device, &surface_config);

            // Create Vello renderer with default options
            let vello_renderer = vello::Renderer::new(&device, vello::RendererOptions::default())
                .map_err(|e| format!("Failed to create Vello renderer: {e}"))?;

            // Create intermediate render texture (Rgba8Unorm for Vello's compute shaders)
            let render_texture = Self::create_render_texture(&device, width, height);

            // Create blitter for copying from intermediate texture to surface
            let blitter = wgpu::util::TextureBlitter::new(&device, surface_format);

            Ok(Self {
                device,
                queue,
                surface,
                surface_config,
                vello_renderer,
                render_texture,
                blitter,
                width,
                height,
            })
        }

        /// Create the intermediate render texture for Vello.
        fn create_render_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Texture {
            device.create_texture(&TextureDescriptor {
                label: Some("Vello Render Texture"),
                size: wgpu::Extent3d {
                    width: width.max(1),
                    height: height.max(1),
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8Unorm,
                usage: TextureUsages::STORAGE_BINDING | TextureUsages::TEXTURE_BINDING,
                view_formats: &[],
            })
        }

        /// Resize the renderer to match new canvas dimensions.
        pub fn resize(&mut self, width: u32, height: u32) {
            if width == 0 || height == 0 {
                return;
            }
            self.width = width;
            self.height = height;
            self.surface_config.width = width;
            self.surface_config.height = height;
            self.surface.configure(&self.device, &self.surface_config);
            self.render_texture = Self::create_render_texture(&self.device, width, height);
        }

        /// Render a Vello scene to the canvas.
        pub fn render(&mut self, scene: &Scene) -> Result<(), String> {
            let surface_texture = self
                .surface
                .get_current_texture()
                .map_err(|e| format!("Failed to get surface texture: {e}"))?;

            // Create views for rendering
            let render_view = self
                .render_texture
                .create_view(&TextureViewDescriptor::default());
            let surface_view = surface_texture
                .texture
                .create_view(&TextureViewDescriptor::default());

            let render_params = vello::RenderParams {
                base_color: Color::from_rgb8(55, 65, 81),
                width: self.width,
                height: self.height,
                antialiasing_method: vello::AaConfig::Msaa16,
            };

            // Render to intermediate texture
            self.vello_renderer
                .render_to_texture(
                    &self.device,
                    &self.queue,
                    scene,
                    &render_view,
                    &render_params,
                )
                .map_err(|e| format!("Failed to render: {e}"))?;

            // Blit to surface
            let mut encoder = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Blit Encoder"),
                });
            self.blitter
                .copy(&self.device, &mut encoder, &render_view, &surface_view);
            self.queue.submit(std::iter::once(encoder.finish()));

            surface_texture.present();
            Ok(())
        }

        /// Get current dimensions.
        pub fn dimensions(&self) -> (u32, u32) {
            (self.width, self.height)
        }
    }
}
