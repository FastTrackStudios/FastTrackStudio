//! GPU-accelerated embedded editor for REAPER's TCP/MCP inline FX UI.
//!
//! This module uses WGPU for offscreen rendering with the same shaders as the
//! windowed editor, then blits the result to REAPER's LICE bitmap.

use nih_plug::editor::embedded::{
    EmbedBitmap, EmbedContext, EmbedDrawInfo, EmbedSizeHints, EmbeddedEditor,
};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

// Use wgpu re-exported from nih_plug_iced to ensure version compatibility
use nih_plug_iced::renderer::wgpu::wgpu;

use crate::audio::meter::MeterConsumer;
use crate::audio::spectrum::SpectrumConsumer;
use crate::ui::shaders::grid::pipeline::GridPipeline;
use crate::ui::shaders::spectrum::pipeline::{compute_log_sampled_spectrum, SpectrumPipeline};
use crate::SAPluginParams;

/// GPU context for offscreen rendering - lazily initialized
struct GpuContext {
    device: wgpu::Device,
    queue: wgpu::Queue,
    // Pipelines
    spectrum_pipeline: SpectrumPipeline,
    grid_pipeline: GridPipeline,
    // Offscreen rendering resources (resized as needed)
    render_texture: wgpu::Texture,
    render_texture_view: wgpu::TextureView,
    staging_buffer: wgpu::Buffer,
    // Current texture dimensions
    texture_width: u32,
    texture_height: u32,
    // Animation timing
    start_time: std::time::Instant,
}

impl GpuContext {
    /// Create a new GPU context with the given initial size
    fn new(width: u32, height: u32) -> Option<Self> {
        // Create WGPU instance
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        // Request adapter (prefer high-performance GPU)
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        }))
        .ok()?;

        // Request device and queue (wgpu 27+ API)
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("Embedded Editor Device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: Default::default(),
            trace: Default::default(),
            experimental_features: Default::default(),
        }))
        .ok()?;

        // Use BGRA format to match LICE bitmap format
        let format = wgpu::TextureFormat::Bgra8Unorm;

        // Create pipelines
        let spectrum_pipeline = SpectrumPipeline::new(&device, format);
        let grid_pipeline = GridPipeline::new(&device, format);

        // Create offscreen texture
        let (render_texture, render_texture_view) =
            Self::create_render_texture(&device, width, height, format);

        // Create staging buffer for readback
        let staging_buffer = Self::create_staging_buffer(&device, width, height);

        Some(Self {
            device,
            queue,
            spectrum_pipeline,
            grid_pipeline,
            render_texture,
            render_texture_view,
            staging_buffer,
            texture_width: width,
            texture_height: height,
            start_time: std::time::Instant::now(),
        })
    }

    fn create_render_texture(
        device: &wgpu::Device,
        width: u32,
        height: u32,
        format: wgpu::TextureFormat,
    ) -> (wgpu::Texture, wgpu::TextureView) {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Embedded Render Texture"),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
            view_formats: &[],
        });

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        (texture, view)
    }

    fn create_staging_buffer(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Buffer {
        // WGPU requires row alignment to 256 bytes
        let bytes_per_pixel = 4u32; // BGRA8
        let unpadded_bytes_per_row = width * bytes_per_pixel;
        let align = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let padded_bytes_per_row = (unpadded_bytes_per_row + align - 1) / align * align;
        let buffer_size = (padded_bytes_per_row * height) as u64;

        device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Embedded Staging Buffer"),
            size: buffer_size,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        })
    }

    /// Resize the render texture if dimensions changed
    fn ensure_size(&mut self, width: u32, height: u32) {
        if width != self.texture_width || height != self.texture_height {
            let format = wgpu::TextureFormat::Bgra8Unorm;
            let (texture, view) = Self::create_render_texture(&self.device, width, height, format);
            self.render_texture = texture;
            self.render_texture_view = view;
            self.staging_buffer = Self::create_staging_buffer(&self.device, width, height);
            self.texture_width = width;
            self.texture_height = height;
        }
    }

    /// Render the spectrum and grid to the offscreen texture
    fn render(
        &mut self,
        spectrum_data: &[f32],
        sample_rate: f32,
        min_db: f32,
        max_db: f32,
        animation_speed: f32,
    ) {
        let width = self.texture_width;
        let height = self.texture_height;

        // Create bounds rectangle
        let bounds = nih_plug_iced::Rectangle {
            x: 0.0,
            y: 0.0,
            width: width as f32,
            height: height as f32,
        };

        // Physical size (same as logical for embedded)
        let physical_size = nih_plug_iced::Size::new(width, height);

        // Compute log-sampled spectrum data
        let log_sampled_data = compute_log_sampled_spectrum(spectrum_data, sample_rate);

        // Calculate elapsed time for animation
        let _time = self.start_time.elapsed().as_secs_f32();

        // Update spectrum pipeline
        self.spectrum_pipeline.update_with_physical_size(
            &self.device,
            &self.queue,
            &bounds,
            physical_size,
            &log_sampled_data,
            min_db,
            max_db,
            animation_speed,
        );

        // Update grid pipeline
        self.grid_pipeline
            .update_with_physical_size(&self.queue, &bounds, physical_size);
        self.grid_pipeline
            .prepare_cache(&self.device, &self.queue, physical_size);

        // Create command encoder
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Embedded Render Encoder"),
            });

        // Clear the texture first
        {
            let _render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Clear Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &self.render_texture_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.094,
                            g: 0.094,
                            b: 0.110,
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
        }

        let clip_bounds = nih_plug_iced::Rectangle {
            x: 0,
            y: 0,
            width,
            height,
        };

        // Render grid first (background)
        self.grid_pipeline
            .render(&mut encoder, &self.render_texture_view, clip_bounds);

        // Render spectrum on top
        self.spectrum_pipeline.render(
            &mut encoder,
            &self.render_texture_view,
            clip_bounds,
            &bounds,
        );

        // Copy texture to staging buffer for CPU readback
        let bytes_per_pixel = 4u32;
        let unpadded_bytes_per_row = width * bytes_per_pixel;
        let align = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let padded_bytes_per_row = (unpadded_bytes_per_row + align - 1) / align * align;

        encoder.copy_texture_to_buffer(
            wgpu::TexelCopyTextureInfo {
                texture: &self.render_texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::TexelCopyBufferInfo {
                buffer: &self.staging_buffer,
                layout: wgpu::TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: Some(padded_bytes_per_row),
                    rows_per_image: Some(height),
                },
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );

        // Submit commands
        self.queue.submit(std::iter::once(encoder.finish()));
    }

    /// Copy the rendered result to the LICE bitmap
    fn copy_to_bitmap(&self, bitmap: &mut EmbedBitmap<'_>) {
        let width = self.texture_width;
        let height = self.texture_height;

        // Calculate buffer layout
        let bytes_per_pixel = 4u32;
        let unpadded_bytes_per_row = width * bytes_per_pixel;
        let align = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let padded_bytes_per_row = (unpadded_bytes_per_row + align - 1) / align * align;

        // Map the staging buffer
        let buffer_slice = self.staging_buffer.slice(..);

        // Create a channel to signal when mapping is done
        let (tx, rx) = std::sync::mpsc::channel();
        buffer_slice.map_async(wgpu::MapMode::Read, move |result| {
            tx.send(result).unwrap();
        });

        // Wait for the GPU to finish
        // Wait for GPU work to complete
        let _ = self.device.poll(wgpu::PollType::Wait {
            submission_index: Some(self.queue.submit(std::iter::empty())),
            timeout: Some(std::time::Duration::from_secs(1)),
        });

        // Wait for mapping to complete
        if rx.recv().ok().and_then(|r| r.ok()).is_none() {
            return;
        }

        // Read the data
        {
            let data = buffer_slice.get_mapped_range();
            let pixels_per_row = padded_bytes_per_row / bytes_per_pixel;

            // Copy row by row (handle padding and potential flip)
            for y in 0..height {
                let src_row_start = (y * pixels_per_row) as usize;
                for x in 0..width {
                    let src_idx = src_row_start + x as usize;
                    // Read as bytes and pack into u32
                    let byte_idx = src_idx * 4;
                    if byte_idx + 3 < data.len() {
                        let b = data[byte_idx];
                        let g = data[byte_idx + 1];
                        let r = data[byte_idx + 2];
                        let a = data[byte_idx + 3];
                        let pixel = EmbedBitmap::rgba(r, g, b, a);
                        bitmap.set_pixel(x, y, pixel);
                    }
                }
            }
        }

        // Unmap the buffer
        self.staging_buffer.unmap();
    }
}

/// GPU-accelerated embedded spectrum analyzer editor.
pub struct SpectrumEmbeddedEditorGpu {
    /// Plugin parameters for reading amplitude range.
    params: Arc<SAPluginParams>,
    /// Spectrum data consumer (reads from audio thread).
    spectrum_consumer: SpectrumConsumer,
    /// Meter data consumer (reads peak levels).
    #[allow(dead_code)]
    meter_consumer: MeterConsumer,
    /// Sample rate for frequency calculations.
    sample_rate: Arc<atomic_float::AtomicF32>,
    /// GPU context for TCP (lazily initialized)
    tcp_gpu_context: Mutex<Option<GpuContext>>,
    /// GPU context for MCP (lazily initialized)
    mcp_gpu_context: Mutex<Option<GpuContext>>,
}

impl SpectrumEmbeddedEditorGpu {
    /// Create a new GPU-accelerated embedded editor.
    pub fn new(
        params: Arc<SAPluginParams>,
        spectrum_consumer: SpectrumConsumer,
        meter_consumer: MeterConsumer,
        sample_rate: Arc<atomic_float::AtomicF32>,
    ) -> Self {
        Self {
            params,
            spectrum_consumer,
            meter_consumer,
            sample_rate,
            tcp_gpu_context: Mutex::new(None),
            mcp_gpu_context: Mutex::new(None),
        }
    }
}

impl EmbeddedEditor for SpectrumEmbeddedEditorGpu {
    fn is_available(&self) -> bool {
        true
    }

    fn size_hints(&self, context: EmbedContext, _dpi: f32) -> Option<EmbedSizeHints> {
        // Aspect ratio = width / height
        // 4.0 = 4x wider than tall (e.g., 400x100)
        // 0.25 = 4x taller than wide (e.g., 100x400)
        match context {
            EmbedContext::Mcp => Some(EmbedSizeHints {
                preferred_aspect: 0.5, // MCP: prefer taller (good for vertical mixer strips)
                minimum_aspect: 0.1,   // Allow very tall/narrow
                min_width: 40,
                min_height: 40,
                max_width: 2000,
                max_height: 2000,
            }),
            _ => Some(EmbedSizeHints {
                preferred_aspect: 4.0, // TCP: prefer wide/short (typical spectrum look)
                minimum_aspect: 0.1,   // Very flexible - accept any ratio
                min_width: 40,
                min_height: 40,
                max_width: 2000,
                max_height: 2000,
            }),
        }
    }

    fn paint(&self, bitmap: &mut EmbedBitmap<'_>, info: &EmbedDrawInfo) -> bool {
        let width = bitmap.width;
        let height = bitmap.height;

        if width == 0 || height == 0 {
            return false;
        }

        // Select the appropriate GPU context based on embed context (TCP vs MCP)
        let mut gpu_guard = match info.context {
            EmbedContext::Mcp => self.mcp_gpu_context.lock().unwrap(),
            _ => self.tcp_gpu_context.lock().unwrap(), // TCP or Unknown
        };

        // Initialize GPU context if needed
        if gpu_guard.is_none() {
            *gpu_guard = GpuContext::new(width, height);
            if gpu_guard.is_none() {
                // GPU initialization failed, fall back to clearing
                bitmap.clear(EmbedBitmap::rgba(24, 24, 28, 255));
                return true;
            }
        }

        let gpu = gpu_guard.as_mut().unwrap();

        // Ensure texture is correct size
        gpu.ensure_size(width, height);

        // Get spectrum data
        let spectrum_data = self
            .spectrum_consumer
            .read()
            .unwrap_or_else(|_| vec![-100.0; 256]);

        // Get parameters
        let sample_rate = self.sample_rate.load(Ordering::Relaxed);
        let (min_db, max_db) = self.params.range.value().to_db_range();
        let animation_speed = self.params.speed.value().to_animation_speed_multiplier();

        // Render to GPU texture
        gpu.render(&spectrum_data, sample_rate, min_db, max_db, animation_speed);

        // Copy result to LICE bitmap
        gpu.copy_to_bitmap(bitmap);

        true
    }
}
