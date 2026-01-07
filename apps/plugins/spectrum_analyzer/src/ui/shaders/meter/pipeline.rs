use bytemuck::{Pod, Zeroable};
use nih_plug_iced::renderer::wgpu::primitive::Pipeline;
use nih_plug_iced::renderer::wgpu::wgpu::{
    self as wgpu, BindGroup, BindGroupDescriptor, BindGroupEntry, BindGroupLayoutDescriptor,
    BindGroupLayoutEntry, BindingType, BufferBindingType, BufferUsages, Device, Queue,
    RenderPipeline, ShaderStages, TextureFormat,
};
use nih_plug_iced::Rectangle;

/// Meter display range in dB
const METER_MIN_DB: f32 = -60.0;
const METER_MAX_DB: f32 = 0.0;

/// Uniforms for the meter shader
#[repr(C)]
#[derive(Debug, Copy, Clone, Pod, Zeroable)]
pub struct Uniforms {
    /// Screen resolution in pixels
    pub resolution: [f32; 2],
    /// Left channel level (normalized 0-1)
    pub left_level: f32,
    /// Right channel level (normalized 0-1)
    pub right_level: f32,
}

impl Uniforms {
    pub fn new(bounds: &Rectangle, left_db: f32, right_db: f32) -> Self {
        // Normalize dB values to 0-1 range for display
        let left_level = ((left_db - METER_MIN_DB) / (METER_MAX_DB - METER_MIN_DB)).clamp(0.0, 1.0);
        let right_level =
            ((right_db - METER_MIN_DB) / (METER_MAX_DB - METER_MIN_DB)).clamp(0.0, 1.0);

        Self {
            resolution: [bounds.width, bounds.height],
            left_level,
            right_level,
        }
    }
}

/// GPU pipeline for rendering stereo level meters
pub struct MeterPipeline {
    render_pipeline: RenderPipeline,
    uniform_buffer: wgpu::Buffer,
    bind_group: BindGroup,
}

impl MeterPipeline {
    pub fn new(device: &Device, format: TextureFormat) -> Self {
        // Create shader module
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Meter Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("meter.wgsl").into()),
        });

        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Meter Bind Group Layout"),
            entries: &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::FRAGMENT,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });

        // Create pipeline layout
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Meter Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        // Create render pipeline
        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Meter Render Pipeline"),
            layout: Some(&pipeline_layout),
            cache: None,
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[],
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
                topology: wgpu::PrimitiveTopology::TriangleStrip,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: wgpu::PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview: None,
        });

        // Create uniform buffer
        let uniform_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Meter Uniform Buffer"),
            size: std::mem::size_of::<Uniforms>() as u64,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        // Create bind group
        let bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Meter Bind Group"),
            layout: &bind_group_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });

        Self {
            render_pipeline,
            uniform_buffer,
            bind_group,
        }
    }

    /// Update meter levels
    pub fn update(
        &mut self,
        queue: &Queue,
        bounds: &Rectangle,
        physical_size: nih_plug_iced::Size<u32>,
        left_db: f32,
        right_db: f32,
    ) {
        // Use physical size for resolution
        let uniforms = Uniforms {
            resolution: [physical_size.width as f32, physical_size.height as f32],
            left_level: ((left_db - METER_MIN_DB) / (METER_MAX_DB - METER_MIN_DB)).clamp(0.0, 1.0),
            right_level: ((right_db - METER_MIN_DB) / (METER_MAX_DB - METER_MIN_DB))
                .clamp(0.0, 1.0),
        };

        // Suppress unused variable warning - bounds used for logical positioning if needed
        let _ = bounds;

        queue.write_buffer(&self.uniform_buffer, 0, bytemuck::bytes_of(&uniforms));
    }

    /// Render the meters
    pub fn render(
        &self,
        encoder: &mut wgpu::CommandEncoder,
        target: &wgpu::TextureView,
        clip_bounds: Rectangle<u32>,
    ) {
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("Meter Render Pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: target,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: wgpu::StoreOp::Store,
                },
                depth_slice: None,
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        render_pass.set_scissor_rect(
            clip_bounds.x,
            clip_bounds.y,
            clip_bounds.width,
            clip_bounds.height,
        );

        render_pass.set_pipeline(&self.render_pipeline);
        render_pass.set_bind_group(0, &self.bind_group, &[]);
        render_pass.draw(0..3, 0..1);
    }
}

impl Pipeline for MeterPipeline {
    fn new(device: &Device, _queue: &Queue, format: TextureFormat) -> Self {
        MeterPipeline::new(device, format)
    }
}
