//! Chart Renderer - WGPU PaintSource for chart visualization
//!
//! Renders a keyflow Chart using WGPU, similar to the engraver music_symbols example.

use blitz_traits::PaintSource;
use bytemuck::{Pod, Zeroable};
use glyphon::{
    Attrs, Buffer as TextBuffer, Cache as TextCache, Family, FontSystem, Metrics, Resolution,
    Shaping, SwashCache, TextArea, TextAtlas, TextBounds, TextRenderer,
};
use keyflow::Chart;
use std::sync::mpsc::{channel, Receiver, Sender};
use wgpu::util::DeviceExt;

/// Messages to update the chart display
pub enum ChartMessage {
    UpdateChart(Chart),
}

/// WGPU Paint source for chart rendering
pub struct ChartPaintSource {
    sender: Sender<ChartMessage>,
    receiver: Receiver<ChartMessage>,
    state: Option<ChartRenderState>,
    current_chart: Option<Chart>,
}

impl ChartPaintSource {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            sender,
            receiver,
            state: None,
            current_chart: None,
        }
    }

    pub fn sender(&self) -> Sender<ChartMessage> {
        self.sender.clone()
    }

    fn process_messages(&mut self) {
        while let Ok(msg) = self.receiver.try_recv() {
            match msg {
                ChartMessage::UpdateChart(chart) => {
                    self.current_chart = Some(chart);
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

/// Internal render state
struct ChartRenderState {
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    vertex_count: u32,
    // Text rendering
    font_system: FontSystem,
    swash_cache: SwashCache,
    text_cache: TextCache,
    text_atlas: TextAtlas,
    text_renderer: TextRenderer,
    text_buffers: Vec<(TextBuffer, f32, f32)>, // (buffer, x, y)
}

/// Vertex for simple colored shapes
#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
struct Vertex {
    position: [f32; 2],
    color: [f32; 4],
}

impl Vertex {
    const LAYOUT: wgpu::VertexBufferLayout<'static> = wgpu::VertexBufferLayout {
        array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &[
            wgpu::VertexAttribute {
                offset: 0,
                shader_location: 0,
                format: wgpu::VertexFormat::Float32x2,
            },
            wgpu::VertexAttribute {
                offset: 8,
                shader_location: 1,
                format: wgpu::VertexFormat::Float32x4,
            },
        ],
    };
}

impl PaintSource for ChartPaintSource {
    fn paint(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        texture: &wgpu::Texture,
        texture_view: &wgpu::TextureView,
    ) {
        // Process any pending messages
        self.process_messages();

        let width = texture.width();
        let height = texture.height();

        // Initialize or update render state
        if self.state.is_none() {
            self.state = Some(ChartRenderState::new(device, queue, width, height, texture.format()));
        }

        let state = self.state.as_mut().unwrap();

        // Rebuild scene if chart changed
        if let Some(chart) = &self.current_chart {
            state.rebuild_scene(device, queue, chart, width, height);
        }

        // Render
        state.render(device, queue, texture_view, width, height);
    }
}

impl ChartRenderState {
    fn new(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        width: u32,
        height: u32,
        format: wgpu::TextureFormat,
    ) -> Self {
        // Create shader
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Chart Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SOURCE.into()),
        });

        // Create pipeline
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Chart Pipeline Layout"),
            bind_group_layouts: &[],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Chart Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[Vertex::LAYOUT],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: None,
                polygon_mode: wgpu::PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        // Create initial vertex buffer (empty)
        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Chart Vertex Buffer"),
            size: 1024 * std::mem::size_of::<Vertex>() as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        // Initialize text rendering
        let mut font_system = FontSystem::new();
        let swash_cache = SwashCache::new();
        let text_cache = TextCache::new(device);
        let text_atlas = TextAtlas::new(device, queue, &text_cache, format);
        let text_renderer = TextRenderer::new(&text_atlas, device, wgpu::MultisampleState::default(), None);

        Self {
            pipeline,
            vertex_buffer,
            vertex_count: 0,
            font_system,
            swash_cache,
            text_cache,
            text_atlas,
            text_renderer,
            text_buffers: Vec::new(),
        }
    }

    fn rebuild_scene(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        chart: &Chart,
        width: u32,
        height: u32,
    ) {
        let w = width as f32;
        let h = height as f32;

        let mut vertices = Vec::new();
        self.text_buffers.clear();

        // Layout constants
        let margin = 40.0;
        let section_height = 80.0;
        let section_gap = 20.0;

        // Background - dark gray
        let bg_color = [0.1, 0.1, 0.12, 1.0];
        vertices.extend(create_rect(0.0, 0.0, w, h, bg_color, w, h));

        // Render each section
        let mut y = margin;
        for section in &chart.sections {
            let section_name = section.section.display_name();

            // Section header background
            let header_color = [0.15, 0.15, 0.18, 1.0];
            vertices.extend(create_rect(margin, y, w - margin * 2.0, 30.0, header_color, w, h));

            // Section name label (red capsule style)
            let label_color = [1.0, 0.0, 0.0, 1.0]; // Pure red
            let label_width = 60.0;
            let label_height = 24.0;
            vertices.extend(create_rect(
                margin + 10.0,
                y + 3.0,
                label_width,
                label_height,
                label_color,
                w,
                h,
            ));

            // Create text buffer for section name
            let mut buffer = TextBuffer::new(&mut self.font_system, Metrics::new(14.0, 18.0));
            buffer.set_size(&mut self.font_system, Some(200.0), Some(30.0));
            buffer.set_text(
                &mut self.font_system,
                &section_name,
                &Attrs::new().family(Family::SansSerif),
                Shaping::Advanced,
                None,
            );
            buffer.shape_until_scroll(&mut self.font_system, false);
            self.text_buffers.push((buffer, margin + 15.0, y + 6.0));

            // Measure info
            let measures_text = format!("{} measures", section.measures.len());
            let mut measures_buffer = TextBuffer::new(&mut self.font_system, Metrics::new(12.0, 16.0));
            measures_buffer.set_size(&mut self.font_system, Some(200.0), Some(30.0));
            measures_buffer.set_text(
                &mut self.font_system,
                &measures_text,
                &Attrs::new().family(Family::SansSerif),
                Shaping::Advanced,
                None,
            );
            measures_buffer.shape_until_scroll(&mut self.font_system, false);
            self.text_buffers.push((measures_buffer, margin + 80.0, y + 8.0));

            // Section content area
            let content_y = y + 35.0;
            let content_height = section_height - 35.0;
            let content_color = [0.12, 0.12, 0.14, 1.0];
            vertices.extend(create_rect(
                margin,
                content_y,
                w - margin * 2.0,
                content_height,
                content_color,
                w,
                h,
            ));

            // Draw measure boxes (4 per row)
            let measures_per_row = 4;
            let measure_width = (w - margin * 2.0 - 20.0) / measures_per_row as f32;
            let measure_height = content_height - 10.0;

            for (i, _measure) in section.measures.iter().enumerate().take(measures_per_row) {
                let mx = margin + 10.0 + (i as f32 * measure_width);
                let my = content_y + 5.0;

                // Measure box border
                let border_color = [0.3, 0.3, 0.35, 1.0];
                // Top border
                vertices.extend(create_rect(mx, my, measure_width - 5.0, 1.0, border_color, w, h));
                // Bottom border
                vertices.extend(create_rect(mx, my + measure_height - 1.0, measure_width - 5.0, 1.0, border_color, w, h));
                // Left border
                vertices.extend(create_rect(mx, my, 1.0, measure_height, border_color, w, h));
                // Right border
                vertices.extend(create_rect(mx + measure_width - 6.0, my, 1.0, measure_height, border_color, w, h));
            }

            y += section_height + section_gap;
        }

        // Update vertex buffer
        self.vertex_count = vertices.len() as u32;
        if !vertices.is_empty() {
            let buffer_size = (vertices.len() * std::mem::size_of::<Vertex>()) as u64;
            if buffer_size > self.vertex_buffer.size() {
                self.vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("Chart Vertex Buffer"),
                    contents: bytemuck::cast_slice(&vertices),
                    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                });
            } else {
                queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
            }
        }
    }

    fn render(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        texture_view: &wgpu::TextureView,
        width: u32,
        height: u32,
    ) {
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Chart Render Encoder"),
        });

        // Render shapes
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Chart Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: texture_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.05,
                            g: 0.05,
                            b: 0.07,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            if self.vertex_count > 0 {
                render_pass.set_pipeline(&self.pipeline);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
                render_pass.draw(0..self.vertex_count, 0..1);
            }
        }

        // Render text
        if !self.text_buffers.is_empty() {
            let text_areas: Vec<TextArea> = self.text_buffers
                .iter()
                .map(|(buffer, x, y)| TextArea {
                    buffer,
                    left: *x,
                    top: *y,
                    scale: 1.0,
                    bounds: TextBounds {
                        left: 0,
                        top: 0,
                        right: width as i32,
                        bottom: height as i32,
                    },
                    default_color: glyphon::Color::rgba(255, 255, 255, 255),
                    custom_glyphs: &[],
                })
                .collect();

            let _ = self.text_renderer.prepare(
                device,
                queue,
                &mut self.font_system,
                &mut self.text_atlas,
                &mut self.text_cache,
                Resolution { width, height },
                &text_areas,
                &mut self.swash_cache,
            );

            {
                let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("Text Render Pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: texture_view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Load,
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });

                let _ = self.text_renderer.render(&self.text_atlas, &mut render_pass);
            }
        }

        queue.submit(std::iter::once(encoder.finish()));
    }
}

/// Create a rectangle as two triangles
fn create_rect(x: f32, y: f32, w: f32, h: f32, color: [f32; 4], canvas_w: f32, canvas_h: f32) -> Vec<Vertex> {
    // Convert to NDC
    let x1 = (x / canvas_w) * 2.0 - 1.0;
    let y1 = 1.0 - (y / canvas_h) * 2.0;
    let x2 = ((x + w) / canvas_w) * 2.0 - 1.0;
    let y2 = 1.0 - ((y + h) / canvas_h) * 2.0;

    vec![
        Vertex { position: [x1, y1], color },
        Vertex { position: [x1, y2], color },
        Vertex { position: [x2, y1], color },
        Vertex { position: [x1, y2], color },
        Vertex { position: [x2, y2], color },
        Vertex { position: [x2, y1], color },
    ]
}

const SHADER_SOURCE: &str = r#"
struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) color: vec4<f32>,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.position = vec4<f32>(in.position, 0.0, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
"#;
