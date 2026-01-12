//! Canvas Shapes Example
//!
//! Demonstrates the Canvas2D drawing API for 2D shapes with WGPU.
//! Includes vector zoom and pan controls for testing shape quality.
//!
//! Controls:
//! - Scroll: Zoom (centered on cursor)
//! - Middle mouse drag OR Space + Left drag: Pan
//! - R key: Reset view
//!
//! Run with: cargo run -p engraver --example canvas_shapes --features example

use std::sync::Arc;

use engraver::renderer::canvas2d::{Canvas2D, Color, Rect, Vertex2D};
use wgpu::{
    util::DeviceExt, BufferUsages, CommandEncoderDescriptor, DeviceDescriptor, Features,
    FragmentState, Instance, InstanceDescriptor, LoadOp, MultisampleState, Operations,
    PipelineLayoutDescriptor, PrimitiveState, PrimitiveTopology, RenderPassColorAttachment,
    RenderPassDescriptor, RenderPipeline, RenderPipelineDescriptor, RequestAdapterOptions, StoreOp,
    TextureViewDescriptor, VertexAttribute, VertexBufferLayout, VertexState, VertexStepMode,
    BindGroup, BindGroupLayout,
};
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent},
    event_loop::{ActiveEventLoop, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowAttributes, WindowId},
};

const WINDOW_WIDTH: u32 = 1024;
const WINDOW_HEIGHT: u32 = 800;

/// View state for zoom and pan
#[derive(Debug, Clone)]
struct ViewState {
    zoom: f32,
    pan_x: f32,
    pan_y: f32,
    mouse_x: f32,
    mouse_y: f32,
    is_panning: bool,
    last_pan_x: f32,
    last_pan_y: f32,
    space_held: bool,
}

impl Default for ViewState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            pan_x: 0.0,
            pan_y: 0.0,
            mouse_x: 0.0,
            mouse_y: 0.0,
            is_panning: false,
            last_pan_x: 0.0,
            last_pan_y: 0.0,
            space_held: false,
        }
    }
}

impl ViewState {
    fn reset(&mut self) {
        *self = Self::default();
    }
}

/// Render state holding GPU resources
struct RenderState {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    pipeline: RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    vertex_count: u32,
    camera_buffer: wgpu::Buffer,
    camera_bind_group: BindGroup,
}

/// Application state
struct App {
    window: Option<Arc<Window>>,
    render_state: Option<RenderState>,
    view: ViewState,
}

impl App {
    fn new() -> Self {
        Self {
            window: None,
            render_state: None,
            view: ViewState::default(),
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let window_attrs = WindowAttributes::default()
            .with_title("Canvas2D Shapes Example")
            .with_inner_size(LogicalSize::new(WINDOW_WIDTH, WINDOW_HEIGHT));

        let window = Arc::new(event_loop.create_window(window_attrs).unwrap());
        self.window = Some(window.clone());

        // Initialize WGPU
        let instance = Instance::new(&InstanceDescriptor::default());
        let surface = instance.create_surface(window.clone()).unwrap();
        let adapter = pollster::block_on(instance.request_adapter(&RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }))
        .unwrap();

        let (device, queue) = pollster::block_on(adapter.request_device(&DeviceDescriptor {
            label: Some("Canvas Device"),
            required_features: Features::empty(),
            required_limits: Default::default(),
            memory_hints: Default::default(),
            experimental_features: Default::default(),
            trace: Default::default(),
        }))
        .unwrap();

        let size = window.inner_size();
        let config = surface.get_default_config(&adapter, size.width, size.height).unwrap();
        surface.configure(&device, &config);

        // Create shader
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER.into()),
        });

        // Camera uniform buffer
        let camera_data = [self.view.zoom, self.view.pan_x, self.view.pan_y, 0.0f32];
        let camera_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Camera Buffer"),
            contents: bytemuck::cast_slice(&camera_data),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });

        let camera_bind_group_layout =
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
            });

        let camera_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Camera Bind Group"),
            layout: &camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: camera_buffer.as_entire_binding(),
            }],
        });

        // Pipeline layout
        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Pipeline Layout"),
            bind_group_layouts: &[&camera_bind_group_layout],
            immediate_size: 0,
        });

        // Create render pipeline
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<Vertex2D>() as u64,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            offset: 0,
                            shader_location: 0,
                            format: wgpu::VertexFormat::Float32x2,
                        },
                        VertexAttribute {
                            offset: 8,
                            shader_location: 1,
                            format: wgpu::VertexFormat::Float32x4,
                        },
                    ],
                }],
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

        // Build initial scene
        let vertices = build_shapes(WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32);

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: BufferUsages::VERTEX | BufferUsages::COPY_DST,
        });

        self.render_state = Some(RenderState {
            surface,
            device,
            queue,
            config,
            pipeline,
            vertex_buffer,
            vertex_count: vertices.len() as u32,
            camera_buffer,
            camera_bind_group,
        });

        window.request_redraw();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),

            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(code) = event.physical_key {
                    match code {
                        KeyCode::Escape => event_loop.exit(),
                        KeyCode::KeyR if event.state == ElementState::Pressed => {
                            self.view.reset();
                            if let Some(window) = &self.window {
                                window.request_redraw();
                            }
                        }
                        KeyCode::Space => {
                            self.view.space_held = event.state == ElementState::Pressed;
                        }
                        _ => {}
                    }
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                let scroll_amount = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y,
                    MouseScrollDelta::PixelDelta(pos) => pos.y as f32 / 50.0,
                };

                // Zoom centered on cursor
                let zoom_factor = 1.1f32.powf(scroll_amount);
                let new_zoom = (self.view.zoom * zoom_factor).clamp(0.1, 50.0);

                if let Some(state) = &self.render_state {
                    let width = state.config.width as f32;
                    let height = state.config.height as f32;

                    // Convert mouse position to NDC
                    let mouse_ndc_x = (self.view.mouse_x / width) * 2.0 - 1.0;
                    let mouse_ndc_y = 1.0 - (self.view.mouse_y / height) * 2.0;

                    // Adjust pan to keep point under cursor fixed
                    self.view.pan_x = mouse_ndc_x - (mouse_ndc_x - self.view.pan_x) * (new_zoom / self.view.zoom);
                    self.view.pan_y = mouse_ndc_y - (mouse_ndc_y - self.view.pan_y) * (new_zoom / self.view.zoom);
                }

                self.view.zoom = new_zoom;

                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                let new_x = position.x as f32;
                let new_y = position.y as f32;

                if self.view.is_panning {
                    if let Some(state) = &self.render_state {
                        let dx = (new_x - self.view.last_pan_x) / state.config.width as f32 * 2.0;
                        let dy = -(new_y - self.view.last_pan_y) / state.config.height as f32 * 2.0;
                        self.view.pan_x += dx;
                        self.view.pan_y += dy;
                    }
                    self.view.last_pan_x = new_x;
                    self.view.last_pan_y = new_y;

                    if let Some(window) = &self.window {
                        window.request_redraw();
                    }
                }

                self.view.mouse_x = new_x;
                self.view.mouse_y = new_y;
            }

            WindowEvent::MouseInput { state: button_state, button, .. } => {
                let pressed = button_state == ElementState::Pressed;

                match button {
                    MouseButton::Middle => {
                        self.view.is_panning = pressed;
                        if pressed {
                            self.view.last_pan_x = self.view.mouse_x;
                            self.view.last_pan_y = self.view.mouse_y;
                        }
                    }
                    MouseButton::Left if self.view.space_held => {
                        self.view.is_panning = pressed;
                        if pressed {
                            self.view.last_pan_x = self.view.mouse_x;
                            self.view.last_pan_y = self.view.mouse_y;
                        }
                    }
                    _ => {}
                }
            }

            WindowEvent::Resized(new_size) => {
                if let Some(state) = &mut self.render_state {
                    if new_size.width > 0 && new_size.height > 0 {
                        state.config.width = new_size.width;
                        state.config.height = new_size.height;
                        state.surface.configure(&state.device, &state.config);
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                if let Some(state) = &self.render_state {
                    // Update camera uniform
                    let camera_data = [self.view.zoom, self.view.pan_x, self.view.pan_y, 0.0f32];
                    state.queue.write_buffer(&state.camera_buffer, 0, bytemuck::cast_slice(&camera_data));

                    // Render
                    let output = state.surface.get_current_texture().unwrap();
                    let view = output.texture.create_view(&TextureViewDescriptor::default());

                    let mut encoder = state.device.create_command_encoder(&CommandEncoderDescriptor {
                        label: Some("Render Encoder"),
                    });

                    {
                        let mut render_pass = encoder.begin_render_pass(&RenderPassDescriptor {
                            label: Some("Render Pass"),
                            color_attachments: &[Some(RenderPassColorAttachment {
                                view: &view,
                                resolve_target: None,
                                ops: Operations {
                                    load: LoadOp::Clear(wgpu::Color {
                                        r: 0.1,
                                        g: 0.1,
                                        b: 0.12,
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

                        render_pass.set_pipeline(&state.pipeline);
                        render_pass.set_bind_group(0, &state.camera_bind_group, &[]);
                        render_pass.set_vertex_buffer(0, state.vertex_buffer.slice(..));
                        render_pass.draw(0..state.vertex_count, 0..1);
                    }

                    state.queue.submit(std::iter::once(encoder.finish()));
                    output.present();
                }
            }

            _ => {}
        }
    }
}

/// Build the scene with various shapes using Canvas2D
fn build_shapes(width: f32, height: f32) -> Vec<Vertex2D> {
    let mut canvas = Canvas2D::new(width, height);

    // Background - light gray
    canvas.fill_rect(
        Rect::new(50.0, 50.0, width - 100.0, height - 100.0),
        Color::rgb(0.95, 0.95, 0.95),
    );

    // Filled rectangles
    canvas.fill_rect(Rect::new(100.0, 100.0, 150.0, 100.0), Color::rgb(0.2, 0.6, 0.9));
    canvas.fill_rect(Rect::new(300.0, 100.0, 100.0, 150.0), Color::rgb(0.9, 0.3, 0.3));

    // Stroked rectangles
    canvas.stroke_rect(Rect::new(100.0, 250.0, 150.0, 100.0), 2.0, Color::BLACK);
    canvas.stroke_rect(Rect::new(300.0, 280.0, 100.0, 100.0), 3.0, Color::rgb(0.1, 0.5, 0.1));

    // Rounded rectangles (filled)
    canvas.fill_rounded_rect(
        Rect::new(500.0, 100.0, 200.0, 80.0),
        15.0,
        Color::rgb(0.9, 0.6, 0.2),
    );
    canvas.fill_rounded_rect(
        Rect::new(500.0, 200.0, 150.0, 150.0),
        30.0,
        Color::rgb(0.6, 0.2, 0.8),
    );

    // Rounded rectangles (stroked) - rehearsal mark style
    canvas.stroke_rounded_rect(
        Rect::new(100.0, 400.0, 100.0, 35.0),
        8.0,
        1.5,
        Color::REHEARSAL_RED,
    );
    canvas.stroke_rounded_rect(
        Rect::new(250.0, 400.0, 80.0, 35.0),
        10.0,
        2.0,
        Color::REHEARSAL_RED,
    );
    canvas.stroke_rounded_rect(
        Rect::new(380.0, 400.0, 120.0, 35.0),
        5.0,
        1.5,
        Color::REHEARSAL_RED,
    );

    // Circles
    canvas.fill_circle(150.0, 550.0, 40.0, Color::rgb(0.3, 0.7, 0.4));
    canvas.fill_circle(280.0, 550.0, 60.0, Color::rgb(0.8, 0.4, 0.6));
    canvas.stroke_circle(420.0, 550.0, 50.0, 2.0, Color::BLACK);

    // Lines
    canvas.stroke_line(550.0, 400.0, 750.0, 400.0, 2.0, Color::BLACK);
    canvas.stroke_line(550.0, 450.0, 750.0, 500.0, 3.0, Color::rgb(0.5, 0.0, 0.5));
    canvas.stroke_line(650.0, 380.0, 650.0, 550.0, 1.5, Color::rgb(0.0, 0.5, 0.5));

    // More rounded rects with different radii
    canvas.stroke_rounded_rect(
        Rect::new(550.0, 550.0, 180.0, 60.0),
        30.0, // Pill-shaped
        2.0,
        Color::rgb(0.2, 0.4, 0.8),
    );
    canvas.stroke_rounded_rect(
        Rect::new(550.0, 630.0, 180.0, 60.0),
        5.0, // Slightly rounded
        2.0,
        Color::rgb(0.2, 0.4, 0.8),
    );

    canvas.vertices().to_vec()
}

const SHADER: &str = r#"
struct Camera {
    zoom: f32,
    pan_x: f32,
    pan_y: f32,
    _padding: f32,
}

@group(0) @binding(0) var<uniform> camera: Camera;

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
    // Apply zoom and pan
    let scaled = in.position * camera.zoom;
    let transformed = scaled + vec2<f32>(camera.pan_x, camera.pan_y);
    out.position = vec4<f32>(transformed, 0.0, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
"#;

fn main() {
    env_logger::init();

    let event_loop = EventLoop::new().unwrap();
    let mut app = App::new();
    event_loop.run_app(&mut app).unwrap();
}
