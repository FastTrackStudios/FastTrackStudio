use bytemuck::{Pod, Zeroable};
use dioxus_native::{CustomPaintCtx, CustomPaintSource, DeviceHandle, TextureHandle};
use nanorand::{Rng, WyRand};
use std::borrow::Cow;
use std::sync::mpsc::{channel, Receiver, Sender};
use wesl::include_wesl;
use wgpu::util::DeviceExt;

const MAX_BUNNIES: usize = 1 << 20;
const BUNNY_SIZE: f32 = 0.15 * 256.0;
const GRAVITY: f32 = -9.8 * 100.0;
const MAX_VELOCITY: f32 = 750.0;

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct Globals {
    mvp: [[f32; 4]; 4],
    size: [f32; 2],
    pad: [f32; 2],
}

#[repr(C, align(256))]
#[derive(Clone, Copy, Pod, Zeroable)]
struct Bunny {
    position: [f32; 2],
    velocity: [f32; 2],
    color: u32,
    _pad: [u32; (256 - 20) / 4],
}

impl Bunny {
    fn update_data(&mut self, delta: f32, extent: &[u32; 2]) {
        self.position[0] += self.velocity[0] * delta;
        self.position[1] += self.velocity[1] * delta;
        self.velocity[1] += GRAVITY * delta;

        if (self.velocity[0] > 0.0 && self.position[0] + 0.5 * BUNNY_SIZE > extent[0] as f32)
            || (self.velocity[0] < 0.0 && self.position[0] - 0.5 * BUNNY_SIZE < 0.0)
        {
            self.velocity[0] *= -1.0;
        }

        if self.velocity[1] < 0.0 && self.position[1] < 0.5 * BUNNY_SIZE {
            self.velocity[1] *= -1.0;
        }

        // Top boundary check
        if self.velocity[1] > 0.0 && self.position[1] + 0.5 * BUNNY_SIZE > extent[1] as f32 {
            self.velocity[1] *= -1.0;
        }
    }
}

pub enum BunnyMessage {
    SpawnBunnies,
}

enum BunnymarkState {
    Active(Box<ActiveBunnymark>),
    Suspended,
}

#[derive(Clone)]
struct TextureAndHandle {
    texture: wgpu::Texture,
    handle: TextureHandle,
}

struct ActiveBunnymark {
    device: wgpu::Device,
    queue: wgpu::Queue,
    bunny_texture_view: wgpu::TextureView,
    sampler: wgpu::Sampler,
    global_bind_group_layout: wgpu::BindGroupLayout,
    local_bind_group_layout: wgpu::BindGroupLayout,
    pipeline: wgpu::RenderPipeline,
    bunnies: Vec<Bunny>,
    local_buffer: wgpu::Buffer,
    extent: [u32; 2],
    rng: WyRand,
    displayed_texture: Option<TextureAndHandle>,
    next_texture: Option<TextureAndHandle>,
}

pub struct BunnymarkPaintSource {
    state: BunnymarkState,
    tx: Sender<BunnyMessage>,
    rx: Receiver<BunnyMessage>,
}

impl CustomPaintSource for BunnymarkPaintSource {
    fn resume(&mut self, device_handle: &DeviceHandle) {
        let device = &device_handle.device;
        let queue = &device_handle.queue;
        let active_state = ActiveBunnymark::new(device, queue);
        self.state = BunnymarkState::Active(Box::new(active_state));
    }

    fn suspend(&mut self) {
        self.state = BunnymarkState::Suspended;
    }

    fn render(
        &mut self,
        ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        _scale: f64,
    ) -> Option<TextureHandle> {
        self.process_messages();
        self.render_inner(ctx, width, height)
    }
}

impl BunnymarkPaintSource {
    pub fn new() -> Self {
        let (tx, rx) = channel();
        Self {
            state: BunnymarkState::Suspended,
            tx,
            rx,
        }
    }

    pub fn sender(&self) -> Sender<BunnyMessage> {
        self.tx.clone()
    }

    fn process_messages(&mut self) {
        loop {
            match self.rx.try_recv() {
                Err(_) => return,
                Ok(msg) => {
                    if let BunnymarkState::Active(state) = &mut self.state {
                        match msg {
                            BunnyMessage::SpawnBunnies => state.spawn_bunnies(),
                        }
                    }
                }
            }
        }
    }

    fn render_inner(
        &mut self,
        ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
    ) -> Option<TextureHandle> {
        if width == 0 || height == 0 {
            return None;
        }
        let BunnymarkState::Active(state) = &mut self.state else {
            return None;
        };

        state.render(ctx, width, height)
    }
}

impl ActiveBunnymark {
    fn new(device: &wgpu::Device, queue: &wgpu::Queue) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Bunnymark Shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_wesl!("bunnymark_shader"))),
        });

        let global_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: wgpu::BufferSize::new(
                                std::mem::size_of::<Globals>() as _,
                            ),
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
                label: None,
            });

        let local_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: true,
                        min_binding_size: wgpu::BufferSize::new(std::mem::size_of::<Bunny>() as _),
                    },
                    count: None,
                }],
                label: None,
            });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&global_bind_group_layout, &local_bind_group_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: Default::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format: wgpu::TextureFormat::Rgba8Unorm,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::default(),
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleStrip,
                strip_index_format: Some(wgpu::IndexFormat::Uint16),
                ..wgpu::PrimitiveState::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        // Load bunny texture
        let bunny_texture = {
            let img_data = include_bytes!("logo.png");
            let decoder = png::Decoder::new(std::io::Cursor::new(img_data));
            let mut reader = decoder.read_info().unwrap();
            let buf_len = reader.output_buffer_size();
            let mut buf = vec![0; buf_len];
            let info = reader.next_frame(&mut buf).unwrap();

            let size = wgpu::Extent3d {
                width: info.width,
                height: info.height,
                depth_or_array_layers: 1,
            };
            let texture = device.create_texture(&wgpu::TextureDescriptor {
                label: None,
                size,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                usage: wgpu::TextureUsages::COPY_DST | wgpu::TextureUsages::TEXTURE_BINDING,
                view_formats: &[],
            });
            queue.write_texture(
                texture.as_image_copy(),
                &buf,
                wgpu::TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: Some(info.width * 4),
                    rows_per_image: None,
                },
                size,
            );
            texture
        };

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        let uniform_alignment =
            device.limits().min_uniform_buffer_offset_alignment as wgpu::BufferAddress;
        let local_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("local"),
            size: (MAX_BUNNIES as wgpu::BufferAddress) * uniform_alignment,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::UNIFORM,
            mapped_at_creation: false,
        });

        let bunny_texture_view = bunny_texture.create_view(&wgpu::TextureViewDescriptor::default());
        let rng = WyRand::new_seed(42);

        let mut ex = Self {
            device: device.clone(),
            queue: queue.clone(),
            bunny_texture_view,
            sampler,
            global_bind_group_layout,
            local_bind_group_layout,
            pipeline,
            bunnies: Vec::new(),
            local_buffer,
            extent: [800, 600], // Will be updated on first render
            rng,
            displayed_texture: None,
            next_texture: None,
        };

        // Spawn initial bunnies
        ex.spawn_bunnies();

        ex
    }

    fn spawn_bunnies(&mut self) {
        let spawn_count = 64;
        let color = self.rng.generate::<u32>();
        println!(
            "Spawning {} bunnies, total at {}",
            spawn_count,
            self.bunnies.len() + spawn_count
        );
        for _ in 0..spawn_count {
            let speed = self.rng.generate::<f32>() * MAX_VELOCITY - (MAX_VELOCITY * 0.5);
            self.bunnies.push(Bunny {
                position: [0.0, 0.5 * (self.extent[1] as f32)],
                velocity: [speed, 0.0],
                color,
                _pad: Zeroable::zeroed(),
            });
        }
    }

    fn render(
        &mut self,
        mut ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
    ) -> Option<TextureHandle> {
        // Update extent if size changed
        if self.extent[0] != width || self.extent[1] != height {
            self.extent = [width, height];
        }

        // If "next texture" size doesn't match specified size then unregister and drop texture
        if let Some(next) = &self.next_texture {
            if next.texture.width() != width || next.texture.height() != height {
                ctx.unregister_texture(self.next_texture.take().unwrap().handle);
            }
        }

        // If there is no "next texture" then create one and register it.
        let texture_and_handle = match &self.next_texture {
            Some(next) => next,
            None => {
                let texture = create_render_texture(&self.device, width, height);
                let handle = ctx.register_texture(texture.clone());
                self.next_texture = Some(TextureAndHandle { texture, handle });
                self.next_texture.as_ref().unwrap()
            }
        };

        let render_texture = &texture_and_handle.texture;
        let next_texture_handle = texture_and_handle.handle.clone();

        // Update bunny physics
        let delta = 0.016; // ~60fps
        for bunny in self.bunnies.iter_mut() {
            bunny.update_data(delta, &self.extent);
        }

        // Create globals with orthographic projection
        let globals = Globals {
            mvp: glam::Mat4::orthographic_rh(0.0, width as f32, 0.0, height as f32, -1.0, 1.0)
                .to_cols_array_2d(),
            size: [BUNNY_SIZE; 2],
            pad: [0.0; 2],
        };

        let global_buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("global"),
                contents: bytemuck::bytes_of(&globals),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::UNIFORM,
            });

        let global_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &self.global_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: global_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(&self.bunny_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
            ],
            label: None,
        });

        let local_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &self.local_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &self.local_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(std::mem::size_of::<Bunny>() as _),
                }),
            }],
            label: None,
        });

        // Write bunny data to buffer
        let uniform_alignment = self.device.limits().min_uniform_buffer_offset_alignment;
        self.queue
            .write_buffer(&self.local_buffer, 0, bytemuck::cast_slice(&self.bunnies));

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        {
            let clear_color = wgpu::Color {
                r: 0.1,
                g: 0.2,
                b: 0.3,
                a: 1.0,
            };
            let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &render_texture.create_view(&wgpu::TextureViewDescriptor::default()),
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(clear_color),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            rpass.set_pipeline(&self.pipeline);
            rpass.set_bind_group(0, &global_group, &[]);
            for i in 0..self.bunnies.len() {
                let offset =
                    (i as wgpu::DynamicOffset) * (uniform_alignment as wgpu::DynamicOffset);
                rpass.set_bind_group(1, &local_group, &[offset]);
                rpass.draw(0..4, 0..1);
            }
        }

        self.queue.submit(Some(encoder.finish()));

        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
        Some(next_texture_handle)
    }
}

fn create_render_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: None,
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
