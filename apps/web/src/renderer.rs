//! Vello Chart Renderer Module
//!
//! Renders charts using Vello with WebGPU backend, matching the native REAPER renderer.
//! This provides high-quality vector graphics for music notation.

use std::sync::Arc;

use crate::chart_state::ViewTransform;
use keyflow::engraver::fonts::SMuFLFont;
use keyflow::engraver::layout::{ChartLayoutMode, PageMargins};
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult};
use keyflow::engraver::layout::tlayout::HarmonyStyle;
use keyflow::engraver::renderer::scene_renderer::SceneRenderBuilder;
use keyflow::engraver::style::MStyle;
use keyflow::Chart;
use vello::kurbo::{Affine, Rect};
use vello::peniko::Color;
use vello::Scene;

// Embedded fonts from musescore reference library
static BRAVURA_FONT: &[u8] = include_bytes!(
    "../../../libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf"
);
static BRAVURA_METADATA: &[u8] = include_bytes!(
    "../../../libs/reference/sheet-music/musescore/fonts/bravura/bravura_metadata.json"
);
static TEXT_FONT: &[u8] =
    include_bytes!("../../../libs/reference/sheet-music/musescore/fonts/FreeSans.ttf");
static MUSEJAZZ_TEXT_FONT: &[u8] = include_bytes!(
    "../../../libs/reference/sheet-music/musescore/fonts/musejazz/MuseJazzText.otf"
);

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
        let layout_engine =
            ChartLayoutEngine::new(style, text_font_data.clone(), symbol_font_data);

        Ok(Self {
            smufl_font,
            text_font_data,
            musejazz_font_data,
            layout_engine,
            layout_result: None,
            last_chart_hash: 0,
        })
    }

    /// Layout a chart and cache the result.
    pub fn layout_chart(&mut self, chart: &Chart, viewport_width: f64, viewport_height: f64) {
        // Simple hash based on chart data (could be improved)
        let chart_hash = self.compute_chart_hash(chart);

        // Skip if already laid out
        if self.layout_result.is_some() && chart_hash == self.last_chart_hash {
            return;
        }

        // Configure layout for viewport
        let config = ChartLayoutConfig {
            margins: PageMargins {
                top: 30.0,
                bottom: 30.0,
                left: 30.0,
                right: 30.0,
            },
            spatium: 5.0,
            system_spacing: 40.0,
            max_measures_per_system: 4,
            min_measure_width: 100.0,
            harmony_style: HarmonyStyle::musejazz(),
            hide_repeated_chords: true,
            use_stems: true,
            show_measure_numbers: true,
            measure_number_offset: 0,
            count_in_measures: 0,
        };

        // Determine layout mode based on viewport
        let mode = ChartLayoutMode::Paginated {
            page_width: viewport_width / DPI_SCALE,
            page_height: viewport_height / DPI_SCALE,
        };

        // Perform layout
        let result = self
            .layout_engine
            .layout_chart_with_config(chart, &mode, &config);

        self.layout_result = Some(result);
        self.last_chart_hash = chart_hash;
    }

    /// Compute a simple hash of the chart for cache invalidation.
    fn compute_chart_hash(&self, chart: &Chart) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        chart.sections.len().hash(&mut hasher);
        if let Some(title) = &chart.metadata.title {
            title.hash(&mut hasher);
        }
        hasher.finish()
    }

    /// Get the cached layout result.
    pub fn layout_result(&self) -> Option<&ChartLayoutResult> {
        self.layout_result.as_ref()
    }

    /// Render the chart to a Vello scene.
    pub fn render_to_scene(
        &self,
        scene: &mut Scene,
        width: f64,
        height: f64,
        transform: Affine,
        viewport_rect: Rect,
    ) {
        // Fill background
        scene.fill(
            vello::peniko::Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(64, 64, 64),
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

            // Set viewport for culling optimization
            renderer.set_viewport(viewport_rect);

            // Render with user's view transform
            renderer.render_with_transform(scene, &layout.scene, transform);
        }
    }

    /// Render playback cursor to the scene.
    pub fn render_cursor(
        &self,
        scene: &mut Scene,
        cursor_x: f64,
        cursor_y: f64,
        cursor_height: f64,
        transform: Affine,
    ) {
        use vello::kurbo::{Line, Stroke};

        // Cursor extends above and below staff
        let extension = cursor_height * 0.25;
        let top = cursor_y - extension;
        let bottom = cursor_y + cursor_height + extension;

        let cursor_line = Line::new((cursor_x, top), (cursor_x, bottom));

        // Apply transform to cursor
        let transformed_line = transform * cursor_line;

        // Draw cursor glow (wider, semi-transparent)
        scene.stroke(
            &Stroke::new(8.0),
            Affine::IDENTITY,
            Color::from_rgba8(255, 80, 80, 102), // 40% opacity
            None,
            &transformed_line,
        );

        // Draw cursor line (narrower, solid)
        scene.stroke(
            &Stroke::new(3.0),
            Affine::IDENTITY,
            Color::from_rgba8(255, 80, 80, 255),
            None,
            &transformed_line,
        );
    }
}

impl Default for ChartLayoutManager {
    fn default() -> Self {
        Self::new().expect("Failed to initialize chart layout manager")
    }
}

/// Convert ViewTransform to kurbo Affine.
pub fn view_transform_to_affine(transform: &ViewTransform) -> Affine {
    Affine::translate((transform.offset_x, transform.offset_y)) * Affine::scale(transform.scale)
}

/// Calculate initial fit transform for a chart.
pub fn calculate_fit_transform(
    layout_result: &ChartLayoutResult,
    viewport_width: f64,
    viewport_height: f64,
) -> ViewTransform {
    // Get first page dimensions
    if let Some(first_page) = layout_result.pages.first() {
        let page_width = first_page.width * DPI_SCALE;
        let page_height = first_page.height * DPI_SCALE;

        let margin = 20.0;
        let available_width = viewport_width - margin * 2.0;
        let available_height = viewport_height - margin * 2.0;

        let scale_x = available_width / page_width;
        let scale_y = available_height / page_height;
        let scale = scale_x.min(scale_y).clamp(0.1, 5.0);

        let scaled_width = page_width * scale;
        let scaled_height = page_height * scale;
        let offset_x = (viewport_width - scaled_width) / 2.0;
        let offset_y = (viewport_height - scaled_height) / 2.0;

        ViewTransform {
            offset_x,
            offset_y,
            scale,
        }
    } else {
        ViewTransform::default()
    }
}

#[cfg(target_arch = "wasm32")]
pub mod wasm {
    //! WASM-specific Vello/WebGPU rendering.

    use super::*;
    use wasm_bindgen::prelude::*;
    use web_sys::HtmlCanvasElement;

    /// WebGPU renderer state for WASM.
    pub struct WebGpuRenderer {
        device: wgpu::Device,
        queue: wgpu::Queue,
        surface: wgpu::Surface<'static>,
        surface_config: wgpu::SurfaceConfiguration,
        vello_renderer: vello::Renderer,
        width: u32,
        height: u32,
    }

    impl WebGpuRenderer {
        /// Create a new WebGPU renderer from a canvas element.
        pub async fn new(canvas: HtmlCanvasElement) -> Result<Self, String> {
            // Get canvas dimensions
            let width = canvas.width();
            let height = canvas.height();

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
                .ok_or("Failed to find suitable GPU adapter")?;

            // Request device
            let (device, queue) = adapter
                .request_device(
                    &wgpu::DeviceDescriptor {
                        label: Some("Chart Viewer Device"),
                        required_features: wgpu::Features::empty(),
                        required_limits: wgpu::Limits::downlevel_webgl2_defaults(),
                        memory_hints: wgpu::MemoryHints::Performance,
                    },
                    None,
                )
                .await
                .map_err(|e| format!("Failed to create device: {e}"))?;

            // Configure surface
            let surface_caps = surface.get_capabilities(&adapter);
            let surface_format = surface_caps
                .formats
                .iter()
                .find(|f| f.is_srgb())
                .copied()
                .unwrap_or(surface_caps.formats[0]);

            let surface_config = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format: surface_format,
                width,
                height,
                present_mode: wgpu::PresentMode::AutoVsync,
                alpha_mode: surface_caps.alpha_modes[0],
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };
            surface.configure(&device, &surface_config);

            // Create Vello renderer
            let vello_renderer = vello::Renderer::new(
                &device,
                vello::RendererOptions {
                    surface_format: Some(surface_format),
                    use_cpu: false,
                    antialiasing_support: vello::AaSupport::all(),
                    num_init_threads: None,
                },
            )
            .map_err(|e| format!("Failed to create Vello renderer: {e}"))?;

            Ok(Self {
                device,
                queue,
                surface,
                surface_config,
                vello_renderer,
                width,
                height,
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
        }

        /// Render a Vello scene to the canvas.
        pub fn render(&mut self, scene: &Scene) -> Result<(), String> {
            let surface_texture = self
                .surface
                .get_current_texture()
                .map_err(|e| format!("Failed to get surface texture: {e}"))?;

            let render_params = vello::RenderParams {
                base_color: Color::from_rgb8(64, 64, 64),
                width: self.width,
                height: self.height,
                antialiasing_method: vello::AaConfig::Msaa16,
            };

            self.vello_renderer
                .render_to_surface(
                    &self.device,
                    &self.queue,
                    scene,
                    &surface_texture,
                    &render_params,
                )
                .map_err(|e| format!("Failed to render: {e}"))?;

            surface_texture.present();
            Ok(())
        }

        /// Get current dimensions.
        pub fn dimensions(&self) -> (u32, u32) {
            (self.width, self.height)
        }
    }
}
