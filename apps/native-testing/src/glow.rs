// Multi-pass bloom post-processor
// Based on the physically-based bloom from learnopengl.com and Call of Duty presentations
// Uses progressive downsample/upsample for high-quality bloom

use dioxus_native::{
    wgpu::{self, Device, Queue, TextureFormat, TextureView},
    PostProcessContext, PostProcessor,
};
use std::borrow::Cow;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use wesl::include_wesl;

/// Number of mip levels for bloom (each level is half the previous size)
const MIP_COUNT: usize = 6;

/// Parameters for the bloom effect (GPU uniform)
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct BloomParams {
    /// Brightness threshold for bloom
    pub threshold: f32,
    /// Soft knee for threshold curve
    pub knee: f32,
    /// Final bloom intensity multiplier
    pub intensity: f32,
    /// Whether the effect is enabled (1.0 = enabled, 0.0 = disabled)
    pub enabled: f32,
}

impl Default for BloomParams {
    fn default() -> Self {
        Self {
            threshold: 0.3,  // Lower = more bloom
            knee: 0.3,       // Soft knee
            intensity: 2.0,  // Visible bloom
            enabled: 1.0,
        }
    }
}

/// Shared state for dynamically updating bloom parameters from UI
#[derive(Clone)]
pub struct GlowState {
    threshold: Arc<AtomicU32>,
    knee: Arc<AtomicU32>,
    intensity: Arc<AtomicU32>,
    enabled: Arc<AtomicU32>,
}

impl Default for GlowState {
    fn default() -> Self {
        let defaults = BloomParams::default();
        Self {
            threshold: Arc::new(AtomicU32::new(defaults.threshold.to_bits())),
            knee: Arc::new(AtomicU32::new(defaults.knee.to_bits())),
            intensity: Arc::new(AtomicU32::new(defaults.intensity.to_bits())),
            enabled: Arc::new(AtomicU32::new(defaults.enabled.to_bits())),
        }
    }
}

impl GlowState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_threshold(&self, threshold: f32) {
        self.threshold.store(threshold.to_bits(), Ordering::Relaxed);
    }

    pub fn set_intensity(&self, intensity: f32) {
        self.intensity.store(intensity.to_bits(), Ordering::Relaxed);
    }

    /// Set radius - maps to knee parameter (higher = softer threshold)
    pub fn set_radius(&self, radius: f32) {
        // Map radius (1-20) to knee (0.1-0.5)
        let knee = 0.1 + (radius / 20.0) * 0.4;
        self.knee.store(knee.to_bits(), Ordering::Relaxed);
    }

    pub fn set_enabled(&self, enabled: bool) {
        let value = if enabled { 1.0f32 } else { 0.0f32 };
        self.enabled.store(value.to_bits(), Ordering::Relaxed);
    }

    fn read_params(&self) -> BloomParams {
        BloomParams {
            threshold: f32::from_bits(self.threshold.load(Ordering::Relaxed)),
            knee: f32::from_bits(self.knee.load(Ordering::Relaxed)),
            intensity: f32::from_bits(self.intensity.load(Ordering::Relaxed)),
            enabled: f32::from_bits(self.enabled.load(Ordering::Relaxed)),
        }
    }
}

/// Mip chain texture for downsample/upsample passes
struct MipChain {
    #[allow(dead_code)]
    texture: wgpu::Texture, // Keeps the texture alive; views reference it
    views: Vec<wgpu::TextureView>,
    width: u32,
    height: u32,
}

impl MipChain {
    fn new(device: &Device, width: u32, height: u32, format: TextureFormat, label: &str) -> Self {
        let mip_count = MIP_COUNT as u32;

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some(label),
            size: wgpu::Extent3d {
                width: width.max(1),
                height: height.max(1),
                depth_or_array_layers: 1,
            },
            mip_level_count: mip_count,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });

        let views: Vec<_> = (0..mip_count)
            .map(|mip| {
                texture.create_view(&wgpu::TextureViewDescriptor {
                    label: Some(&format!("{} mip {}", label, mip)),
                    base_mip_level: mip,
                    mip_level_count: Some(1),
                    ..Default::default()
                })
            })
            .collect();

        Self {
            texture,
            views,
            width,
            height,
        }
    }

    fn view(&self, mip: usize) -> &wgpu::TextureView {
        &self.views[mip]
    }

    #[allow(dead_code)]
    fn mip_size(&self, mip: usize) -> (u32, u32) {
        let div = 1u32 << mip;
        ((self.width / div).max(1), (self.height / div).max(1))
    }
}

/// Multi-pass bloom post-processor
pub struct GlowPostProcessor {
    state: GlowState,

    // Pipelines for each pass
    prefilter_pipeline: Option<wgpu::RenderPipeline>,
    downsample_pipeline: Option<wgpu::RenderPipeline>,
    upsample_pipeline: Option<wgpu::RenderPipeline>,
    composite_pipeline: Option<wgpu::RenderPipeline>,
    passthrough_pipeline: Option<wgpu::RenderPipeline>,

    // Bind group layouts
    single_texture_layout: Option<wgpu::BindGroupLayout>,
    dual_texture_layout: Option<wgpu::BindGroupLayout>,

    // Mip chain textures
    downsample_chain: Option<MipChain>,
    upsample_chain: Option<MipChain>,

    // Sampler and params buffer
    sampler: Option<wgpu::Sampler>,
    params_buffer: Option<wgpu::Buffer>,

    // Current texture dimensions
    current_width: u32,
    current_height: u32,
    format: TextureFormat,
}

impl GlowPostProcessor {
    pub fn with_state(state: GlowState) -> Self {
        Self {
            state,
            prefilter_pipeline: None,
            downsample_pipeline: None,
            upsample_pipeline: None,
            composite_pipeline: None,
            passthrough_pipeline: None,
            single_texture_layout: None,
            dual_texture_layout: None,
            downsample_chain: None,
            upsample_chain: None,
            sampler: None,
            params_buffer: None,
            current_width: 0,
            current_height: 0,
            format: TextureFormat::Rgba8Unorm,
        }
    }

    fn create_mip_chains(&mut self, device: &Device, width: u32, height: u32) {
        // Start at half resolution for bloom
        let bloom_width = width / 2;
        let bloom_height = height / 2;

        self.downsample_chain = Some(MipChain::new(
            device,
            bloom_width,
            bloom_height,
            self.format,
            "Bloom downsample chain",
        ));

        self.upsample_chain = Some(MipChain::new(
            device,
            bloom_width,
            bloom_height,
            self.format,
            "Bloom upsample chain",
        ));

        self.current_width = width;
        self.current_height = height;
    }

    fn update_params(&self, queue: &Queue) {
        if let Some(buffer) = &self.params_buffer {
            let params = self.state.read_params();
            queue.write_buffer(buffer, 0, bytemuck::cast_slice(&[params]));
        }
    }
}

impl Default for GlowPostProcessor {
    fn default() -> Self {
        Self::with_state(GlowState::default())
    }
}

impl PostProcessor for GlowPostProcessor {
    fn resume(&mut self, device: &Device, queue: &Queue, format: TextureFormat) {
        self.format = format;

        // Create shader module
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Bloom shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_wesl!("bloom_shader"))),
        });

        // Single texture bind group layout (for prefilter, downsample, composite)
        let single_texture_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Bloom single texture layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                ],
            });

        // Dual texture bind group layout (for upsample which needs two textures)
        let dual_texture_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Bloom dual texture layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 3,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                ],
            });

        // Create sampler
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Bloom sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        // Create params buffer
        let params_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Bloom params buffer"),
            size: std::mem::size_of::<BloomParams>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        // Upload initial params
        let params = self.state.read_params();
        queue.write_buffer(&params_buffer, 0, bytemuck::cast_slice(&[params]));

        // Create pipeline layouts
        let single_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Bloom single texture pipeline layout"),
            bind_group_layouts: &[&single_texture_layout],
            push_constant_ranges: &[],
        });

        let dual_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Bloom dual texture pipeline layout"),
            bind_group_layouts: &[&dual_texture_layout],
            push_constant_ranges: &[],
        });

        // Vertex shader for full-screen quad
        let vertex_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Full-screen quad vertex shader"),
            source: wgpu::ShaderSource::Wgsl(FULLSCREEN_QUAD_VERTEX_SHADER.into()),
        });

        // Helper to create pipeline
        let create_pipeline =
            |label: &str, layout: &wgpu::PipelineLayout, entry_point: &str| -> wgpu::RenderPipeline {
                device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some(label),
                    layout: Some(layout),
                    vertex: wgpu::VertexState {
                        module: &vertex_shader,
                        entry_point: Some("vs_main"),
                        buffers: &[],
                        compilation_options: Default::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &shader,
                        entry_point: Some(entry_point),
                        targets: &[Some(wgpu::ColorTargetState {
                            format,
                            blend: Some(wgpu::BlendState::REPLACE),
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                        compilation_options: Default::default(),
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        ..Default::default()
                    },
                    depth_stencil: None,
                    multisample: wgpu::MultisampleState::default(),
                    multiview: None,
                    cache: None,
                })
            };

        // Create pipelines
        self.prefilter_pipeline = Some(create_pipeline(
            "Bloom prefilter pipeline",
            &single_pipeline_layout,
            "fs_prefilter",
        ));

        self.downsample_pipeline = Some(create_pipeline(
            "Bloom downsample pipeline",
            &single_pipeline_layout,
            "fs_downsample",
        ));

        self.upsample_pipeline = Some(create_pipeline(
            "Bloom upsample pipeline",
            &dual_pipeline_layout,
            "fs_upsample",
        ));

        self.composite_pipeline = Some(create_pipeline(
            "Bloom composite pipeline",
            &dual_pipeline_layout,
            "fs_composite",
        ));

        self.passthrough_pipeline = Some(create_pipeline(
            "Bloom passthrough pipeline",
            &single_pipeline_layout,
            "fs_passthrough",
        ));

        self.single_texture_layout = Some(single_texture_layout);
        self.dual_texture_layout = Some(dual_texture_layout);
        self.sampler = Some(sampler);
        self.params_buffer = Some(params_buffer);
    }

    fn suspend(&mut self) {
        self.prefilter_pipeline = None;
        self.downsample_pipeline = None;
        self.upsample_pipeline = None;
        self.composite_pipeline = None;
        self.passthrough_pipeline = None;
        self.single_texture_layout = None;
        self.dual_texture_layout = None;
        self.downsample_chain = None;
        self.upsample_chain = None;
        self.sampler = None;
        self.params_buffer = None;
    }

    fn process(&mut self, ctx: PostProcessContext<'_>, input: &TextureView, output: &TextureView) {
        // Check if effect is enabled
        let params = self.state.read_params();
        if params.enabled < 0.5 {
            // Just copy input to output
            self.copy_texture(ctx.device, ctx.encoder, input, output);
            return;
        }

        // Check pipelines are initialized
        if self.prefilter_pipeline.is_none()
            || self.downsample_pipeline.is_none()
            || self.upsample_pipeline.is_none()
            || self.composite_pipeline.is_none()
            || self.single_texture_layout.is_none()
            || self.dual_texture_layout.is_none()
            || self.sampler.is_none()
            || self.params_buffer.is_none()
        {
            return;
        }

        // Update params
        self.update_params(ctx.queue);

        // Get input texture dimensions
        let width = ctx.width;
        let height = ctx.height;

        // Recreate mip chains if size changed (do this before borrowing other fields)
        if self.current_width != width || self.current_height != height || self.downsample_chain.is_none() {
            self.create_mip_chains(ctx.device, width, height);
        }

        // Now borrow the fields we need
        let prefilter_pipeline = self.prefilter_pipeline.as_ref().unwrap();
        let downsample_pipeline = self.downsample_pipeline.as_ref().unwrap();
        let upsample_pipeline = self.upsample_pipeline.as_ref().unwrap();
        let composite_pipeline = self.composite_pipeline.as_ref().unwrap();
        let single_layout = self.single_texture_layout.as_ref().unwrap();
        let dual_layout = self.dual_texture_layout.as_ref().unwrap();
        let sampler = self.sampler.as_ref().unwrap();
        let params_buffer = self.params_buffer.as_ref().unwrap();
        let downsample_chain = self.downsample_chain.as_ref().unwrap();
        let upsample_chain = self.upsample_chain.as_ref().unwrap();

        // ========================================
        // PASS 1: Prefilter (input -> downsample[0])
        // ========================================
        {
            let bind_group = ctx.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Bloom prefilter bind group"),
                layout: single_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(input),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: params_buffer.as_entire_binding(),
                    },
                ],
            });

            let mut render_pass = ctx.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom prefilter pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: downsample_chain.view(0),
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            render_pass.set_pipeline(prefilter_pipeline);
            render_pass.set_bind_group(0, &bind_group, &[]);
            render_pass.draw(0..3, 0..1);
        }

        // ========================================
        // PASS 2-N: Downsample chain
        // ========================================
        for mip in 1..MIP_COUNT {
            let bind_group = ctx.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some(&format!("Bloom downsample {} bind group", mip)),
                layout: single_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(downsample_chain.view(mip - 1)),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: params_buffer.as_entire_binding(),
                    },
                ],
            });

            let mut render_pass = ctx.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(&format!("Bloom downsample {} pass", mip)),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: downsample_chain.view(mip),
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            render_pass.set_pipeline(downsample_pipeline);
            render_pass.set_bind_group(0, &bind_group, &[]);
            render_pass.draw(0..3, 0..1);
        }

        // ========================================
        // PASS: First upsample (smallest mip -> upsample[MIP_COUNT-2])
        // Combines downsample[MIP_COUNT-1] upsampled with downsample[MIP_COUNT-2]
        // ========================================
        {
            let mip = MIP_COUNT - 2;
            let bind_group = ctx.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Bloom first upsample bind group"),
                layout: dual_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(downsample_chain.view(MIP_COUNT - 1)),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: params_buffer.as_entire_binding(),
                    },
                    wgpu::BindGroupEntry {
                        binding: 3,
                        resource: wgpu::BindingResource::TextureView(downsample_chain.view(mip)),
                    },
                ],
            });

            let mut render_pass = ctx.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom first upsample pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: upsample_chain.view(mip),
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            render_pass.set_pipeline(upsample_pipeline);
            render_pass.set_bind_group(0, &bind_group, &[]);
            render_pass.draw(0..3, 0..1);
        }

        // ========================================
        // PASS: Remaining upsamples
        // ========================================
        for mip in (0..MIP_COUNT - 2).rev() {
            let bind_group = ctx.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some(&format!("Bloom upsample {} bind group", mip)),
                layout: dual_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(upsample_chain.view(mip + 1)),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: params_buffer.as_entire_binding(),
                    },
                    wgpu::BindGroupEntry {
                        binding: 3,
                        resource: wgpu::BindingResource::TextureView(downsample_chain.view(mip)),
                    },
                ],
            });

            let mut render_pass = ctx.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(&format!("Bloom upsample {} pass", mip)),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: upsample_chain.view(mip),
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            render_pass.set_pipeline(upsample_pipeline);
            render_pass.set_bind_group(0, &bind_group, &[]);
            render_pass.draw(0..3, 0..1);
        }

        // ========================================
        // FINAL PASS: Composite bloom with original
        // ========================================
        {
            let bind_group = ctx.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Bloom composite bind group"),
                layout: dual_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(input),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: params_buffer.as_entire_binding(),
                    },
                    wgpu::BindGroupEntry {
                        binding: 3,
                        resource: wgpu::BindingResource::TextureView(upsample_chain.view(0)),
                    },
                ],
            });

            let mut render_pass = ctx.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom composite pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: output,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            render_pass.set_pipeline(composite_pipeline);
            render_pass.set_bind_group(0, &bind_group, &[]);
            render_pass.draw(0..3, 0..1);
        }
    }
}

impl GlowPostProcessor {
    /// Simple copy from input to output when effect is disabled
    fn copy_texture(
        &self,
        device: &Device,
        encoder: &mut wgpu::CommandEncoder,
        input: &TextureView,
        output: &TextureView,
    ) {
        let Some(passthrough_pipeline) = &self.passthrough_pipeline else {
            return;
        };
        let Some(single_layout) = &self.single_texture_layout else {
            return;
        };
        let Some(sampler) = &self.sampler else { return };
        let Some(params_buffer) = &self.params_buffer else {
            return;
        };

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Passthrough bind group"),
            layout: single_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(input),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: params_buffer.as_entire_binding(),
                },
            ],
        });

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("Passthrough render pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: output,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: wgpu::StoreOp::Store,
                },
                depth_slice: None,
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        render_pass.set_pipeline(passthrough_pipeline);
        render_pass.set_bind_group(0, &bind_group, &[]);
        render_pass.draw(0..3, 0..1);
    }
}

/// Vertex shader for full-screen quad
const FULLSCREEN_QUAD_VERTEX_SHADER: &str = r#"
struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

@vertex
fn vs_main(@builtin(vertex_index) vertex_idx: u32) -> VertexOutput {
    var output: VertexOutput;

    // Full-screen triangle
    output.position = vec4<f32>(
        f32(i32(vertex_idx) / 2) * 4.0 - 1.0,
        f32(i32(vertex_idx) % 2) * 4.0 - 1.0,
        0.0,
        1.0
    );

    output.uv = vec2<f32>(
        f32(i32(vertex_idx) / 2) * 2.0,
        1.0 - f32(i32(vertex_idx) % 2) * 2.0
    );

    return output;
}
"#;
