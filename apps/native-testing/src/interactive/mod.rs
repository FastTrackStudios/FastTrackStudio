use bytemuck::{Pod, Zeroable};
use dioxus_native::{CustomPaintCtx, CustomPaintSource, DeviceHandle, TextureHandle};
use nanorand::{Rng, WyRand};
use std::borrow::Cow;
use std::sync::mpsc::{channel, Receiver, Sender};
use wesl::include_wesl;
use wgpu::util::DeviceExt;

const MAX_PARTICLES: usize = 10000;

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable, Debug)]
pub struct Particle {
    position: [f32; 2],
    size: f32,
    _pad: f32,
    color: [f32; 4],
}

impl Particle {
    fn new(x: f32, y: f32, size: f32, color: [f32; 4]) -> Self {
        Self {
            position: [x, y],
            size,
            _pad: 0.0,
            color,
        }
    }

    fn update(&mut self, velocity: [f32; 2], dt: f32) {
        self.position[0] += velocity[0] * dt;
        self.position[1] += velocity[1] * dt;
    }
}

#[derive(Clone, Copy)]
struct ParticleWithVelocity {
    particle: Particle,
    velocity: [f32; 2],
    lifetime: f32,
    max_lifetime: f32,
}

pub enum InteractiveMessage {
    // Button-based controls (fallback)
    MoveUp,
    MoveDown,
    MoveLeft,
    MoveRight,
    SpawnAtPlayer,
    // Keyboard controls
    KeyDown(String),
    KeyUp(String),
    // Mouse controls - using client coordinates
    Click { x: f32, y: f32 },
}

enum InteractiveState {
    Active(Box<ActiveInteractive>),
    Suspended,
}

#[derive(Clone)]
struct TextureAndHandle {
    texture: wgpu::Texture,
    handle: TextureHandle,
}

struct ActiveInteractive {
    device: wgpu::Device,
    queue: wgpu::Queue,
    pipeline: wgpu::RenderPipeline,
    bind_group_layout: wgpu::BindGroupLayout,
    particles: Vec<ParticleWithVelocity>,
    player_pos: [f32; 2],
    player_velocity: [f32; 2],
    extent: [u32; 2],
    rng: WyRand,
    displayed_texture: Option<TextureAndHandle>,
    next_texture: Option<TextureAndHandle>,
    // Keyboard state
    keys_held: std::collections::HashSet<String>,
    // Canvas offset for coordinate translation (set from UI)
    canvas_offset: [f32; 2],
}

pub struct InteractivePaintSource {
    state: InteractiveState,
    tx: Sender<InteractiveMessage>,
    rx: Receiver<InteractiveMessage>,
}

impl CustomPaintSource for InteractivePaintSource {
    fn resume(&mut self, device_handle: &DeviceHandle) {
        let device = &device_handle.device;
        let queue = &device_handle.queue;
        let active_state = ActiveInteractive::new(device, queue);
        self.state = InteractiveState::Active(Box::new(active_state));
    }

    fn suspend(&mut self) {
        self.state = InteractiveState::Suspended;
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

impl InteractivePaintSource {
    pub fn new() -> Self {
        let (tx, rx) = channel();
        Self {
            state: InteractiveState::Suspended,
            tx,
            rx,
        }
    }

    pub fn sender(&self) -> Sender<InteractiveMessage> {
        self.tx.clone()
    }

    fn process_messages(&mut self) {
        let InteractiveState::Active(state) = &mut self.state else {
            return;
        };

        let mut msg_count = 0;
        loop {
            match self.rx.try_recv() {
                Err(_) => break,
                Ok(msg) => {
                    msg_count += 1;
                    match msg {
                        // Button-based controls
                        InteractiveMessage::MoveUp => {
                            println!("[Interactive] MoveUp");
                            state.move_player(0.0, -30.0);
                        }
                        InteractiveMessage::MoveDown => {
                            println!("[Interactive] MoveDown");
                            state.move_player(0.0, 30.0);
                        }
                        InteractiveMessage::MoveLeft => {
                            println!("[Interactive] MoveLeft");
                            state.move_player(-30.0, 0.0);
                        }
                        InteractiveMessage::MoveRight => {
                            println!("[Interactive] MoveRight");
                            state.move_player(30.0, 0.0);
                        }
                        InteractiveMessage::SpawnAtPlayer => {
                            println!("[Interactive] SpawnAtPlayer");
                            let pos = state.player_pos;
                            state.spawn_particles_at(pos[0], pos[1], 20);
                        }
                        // Keyboard controls - keys are already normalized to lowercase
                        InteractiveMessage::KeyDown(key) => {
                            println!("[Interactive] KeyDown: {}", key);
                            state.keys_held.insert(key);
                        }
                        InteractiveMessage::KeyUp(key) => {
                            println!("[Interactive] KeyUp: {}", key);
                            state.keys_held.remove(&key);
                        }
                        // Mouse click - spawn particles at click location
                        InteractiveMessage::Click { x, y } => {
                            println!("[Interactive] Click at ({}, {})", x, y);
                            // Convert client coordinates to canvas-relative coordinates
                            let canvas_x = x - state.canvas_offset[0];
                            let canvas_y = y - state.canvas_offset[1];
                            // Only spawn if click is within canvas bounds
                            if canvas_x >= 0.0 && canvas_x <= state.extent[0] as f32
                                && canvas_y >= 0.0 && canvas_y <= state.extent[1] as f32
                            {
                                state.spawn_particles_at(canvas_x, canvas_y, 15);
                            }
                        }
                    }
                },
            }
        }
        if msg_count > 0 {
            println!("[Interactive] Processed {} messages, keys_held: {:?}", msg_count, state.keys_held);
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
        let InteractiveState::Active(state) = &mut self.state else {
            return None;
        };

        state.render(ctx, width, height)
    }
}

impl ActiveInteractive {
    fn new(device: &wgpu::Device, _queue: &wgpu::Queue) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Interactive Shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_wesl!("interactive_shader"))),
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Interactive Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: wgpu::BufferSize::new(8), // vec2<f32>
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Interactive Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Interactive Pipeline"),
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
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleStrip,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        Self {
            device: device.clone(),
            queue: _queue.clone(),
            pipeline,
            bind_group_layout,
            particles: Vec::new(),
            player_pos: [400.0, 200.0],
            player_velocity: [0.0, 0.0],
            extent: [800, 400],
            rng: WyRand::new_seed(42),
            displayed_texture: None,
            next_texture: None,
            keys_held: std::collections::HashSet::new(),
            canvas_offset: [0.0, 0.0],
        }
    }

    pub fn set_canvas_offset(&mut self, x: f32, y: f32) {
        self.canvas_offset = [x, y];
    }

    fn move_player(&mut self, dx: f32, dy: f32) {
        self.player_pos[0] += dx;
        self.player_pos[1] += dy;
        // Clamp to bounds
        self.player_pos[0] = self.player_pos[0].clamp(20.0, self.extent[0] as f32 - 20.0);
        self.player_pos[1] = self.player_pos[1].clamp(20.0, self.extent[1] as f32 - 20.0);
    }

    fn spawn_particles_at(&mut self, x: f32, y: f32, count: usize) {
        for _ in 0..count {
            if self.particles.len() >= MAX_PARTICLES {
                break;
            }

            let angle = self.rng.generate::<f32>() * std::f32::consts::TAU;
            let speed = 50.0 + self.rng.generate::<f32>() * 150.0;
            let velocity = [angle.cos() * speed, angle.sin() * speed];

            let hue = self.rng.generate::<f32>();
            let color = hsv_to_rgb(hue, 0.8, 1.0);

            let size = 5.0 + self.rng.generate::<f32>() * 15.0;
            let lifetime = 1.0 + self.rng.generate::<f32>() * 2.0;

            self.particles.push(ParticleWithVelocity {
                particle: Particle::new(x, y, size, [color[0], color[1], color[2], 1.0]),
                velocity,
                lifetime,
                max_lifetime: lifetime,
            });
        }
    }

    fn update(&mut self, dt: f32) {
        // Process keyboard input for player movement
        let move_speed = 300.0; // pixels per second
        let mut target_velocity = [0.0f32, 0.0f32];

        // Debug: print keys_held if not empty
        if !self.keys_held.is_empty() {
            println!("[Interactive] update() keys_held: {:?}", self.keys_held);
        }

        if self.keys_held.contains("w") || self.keys_held.contains("arrowup") {
            target_velocity[1] -= move_speed;
        }
        if self.keys_held.contains("s") || self.keys_held.contains("arrowdown") {
            target_velocity[1] += move_speed;
        }
        if self.keys_held.contains("a") || self.keys_held.contains("arrowleft") {
            target_velocity[0] -= move_speed;
        }
        if self.keys_held.contains("d") || self.keys_held.contains("arrowright") {
            target_velocity[0] += move_speed;
        }

        // Spawn particles when space is held
        if self.keys_held.contains(" ") {
            // Spawn a few particles each frame when space is held
            let pos = self.player_pos;
            self.spawn_particles_at(pos[0], pos[1], 2);
        }

        // Smooth velocity interpolation
        let lerp_factor = 10.0 * dt;
        self.player_velocity[0] += (target_velocity[0] - self.player_velocity[0]) * lerp_factor;
        self.player_velocity[1] += (target_velocity[1] - self.player_velocity[1]) * lerp_factor;

        // Update player position
        self.player_pos[0] += self.player_velocity[0] * dt;
        self.player_pos[1] += self.player_velocity[1] * dt;

        // Clamp to bounds
        self.player_pos[0] = self.player_pos[0].clamp(20.0, self.extent[0] as f32 - 20.0);
        self.player_pos[1] = self.player_pos[1].clamp(20.0, self.extent[1] as f32 - 20.0);

        // Update particles
        for p in &mut self.particles {
            p.particle.update(p.velocity, dt);
            p.lifetime -= dt;

            // Fade out
            let alpha = (p.lifetime / p.max_lifetime).max(0.0);
            p.particle.color[3] = alpha;

            // Apply gravity
            p.velocity[1] += 100.0 * dt;
        }

        // Remove dead particles
        self.particles.retain(|p| p.lifetime > 0.0);
    }

    fn render(
        &mut self,
        mut ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
    ) -> Option<TextureHandle> {
        self.extent = [width, height];

        // Update simulation
        let dt = 0.016; // ~60fps
        self.update(dt);

        // Handle texture management
        if let Some(next) = &self.next_texture {
            if next.texture.width() != width || next.texture.height() != height {
                ctx.unregister_texture(self.next_texture.take().unwrap().handle);
            }
        }

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

        // Build particle data - include player as a particle
        let mut all_particles: Vec<Particle> = self.particles.iter().map(|p| p.particle).collect();

        // Add player particle (larger, white/cyan)
        all_particles.push(Particle::new(
            self.player_pos[0],
            self.player_pos[1],
            20.0,
            [0.0, 1.0, 1.0, 1.0], // Cyan
        ));

        if all_particles.is_empty() {
            // Nothing to render, just clear
            let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
            {
                let _rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: None,
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &render_texture.create_view(&wgpu::TextureViewDescriptor::default()),
                        depth_slice: None,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(wgpu::Color {
                                r: 0.05,
                                g: 0.05,
                                b: 0.1,
                                a: 1.0,
                            }),
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });
            }
            self.queue.submit(Some(encoder.finish()));
            std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
            return Some(next_texture_handle);
        }

        // Create buffers
        let screen_size_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Screen Size Buffer"),
            contents: bytemuck::cast_slice(&[width as f32, height as f32]),
            usage: wgpu::BufferUsages::UNIFORM,
        });

        let particle_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Particle Buffer"),
            contents: bytemuck::cast_slice(&all_particles),
            usage: wgpu::BufferUsages::STORAGE,
        });

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Interactive Bind Group"),
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: screen_size_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: particle_buffer.as_entire_binding(),
                },
            ],
        });

        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        {
            let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &render_texture.create_view(&wgpu::TextureViewDescriptor::default()),
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.05,
                            g: 0.05,
                            b: 0.1,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            rpass.set_pipeline(&self.pipeline);
            rpass.set_bind_group(0, &bind_group, &[]);
            // Draw 4 vertices (quad) per particle instance
            rpass.draw(0..4, 0..all_particles.len() as u32);
        }

        self.queue.submit(Some(encoder.finish()));

        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
        Some(next_texture_handle)
    }
}

fn create_render_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Interactive Render Texture"),
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

fn hsv_to_rgb(h: f32, s: f32, v: f32) -> [f32; 3] {
    let c = v * s;
    let h_prime = h * 6.0;
    let x = c * (1.0 - ((h_prime % 2.0) - 1.0).abs());
    let m = v - c;

    let (r, g, b) = match h_prime as u32 {
        0 => (c, x, 0.0),
        1 => (x, c, 0.0),
        2 => (0.0, c, x),
        3 => (0.0, x, c),
        4 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };

    [r + m, g + m, b + m]
}
