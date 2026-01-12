//! Canvas SDF Example - Text with Precise Bounding Rectangles
//!
//! Demonstrates:
//! 1. Text rendering with glyphon
//! 2. Precise bounding rectangles around text (no padding)
//! 3. Rectangles with padding
//! 4. Rounded rectangle frames around text
//!
//! Controls:
//! - Scroll: Zoom (centered on cursor)
//! - Middle mouse drag OR Space + Left drag: Pan
//! - R key: Reset view
//!
//! Run with: cargo run -p engraver --example canvas_sdf --features example

use std::sync::Arc;

use glyphon::{
    Attrs, Buffer as TextBuffer, Family, FontSystem, Metrics, Resolution, Shaping, SwashCache,
    TextArea, TextAtlas, TextBounds, TextRenderer, Viewport, Cache as TextCache,
    Color as TextColor,
};
use wgpu::{
    util::DeviceExt, BufferUsages, CommandEncoderDescriptor, DeviceDescriptor, Features,
    FragmentState, Instance, InstanceDescriptor, LoadOp, MultisampleState, Operations,
    PipelineLayoutDescriptor, PrimitiveState, PrimitiveTopology, RenderPassColorAttachment,
    RenderPassDescriptor, RenderPipeline, RenderPipelineDescriptor, RequestAdapterOptions, StoreOp,
    TextureViewDescriptor, VertexAttribute, VertexBufferLayout, VertexState, VertexStepMode,
    BindGroup,
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

/// Vertex for SDF rectangles
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct RectVertex {
    position: [f32; 2],
    rect_center: [f32; 2],
    rect_half_size: [f32; 2],
    corner_radius: f32,
    border_width: f32,
    color: [f32; 4],
}

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

/// Text element with measured bounds
struct TextElement {
    buffer: TextBuffer,
    /// Position for text rendering (top of text area)
    text_x: f32,
    text_y: f32,
    /// Position for rectangle (where glyphs actually appear)
    rect_x: f32,
    rect_y: f32,
    width: f32,
    height: f32,
}

/// Text rendering state
struct TextState {
    font_system: FontSystem,
    swash_cache: SwashCache,
    text_cache: TextCache,
    viewport: Viewport,
    atlas: TextAtlas,
    text_renderer: TextRenderer,
    elements: Vec<TextElement>,
}

/// Render state
struct RenderState {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    sdf_pipeline: RenderPipeline,
    sdf_vertex_buffer: wgpu::Buffer,
    sdf_vertex_count: u32,
    camera_buffer: wgpu::Buffer,
    camera_bind_group: BindGroup,
    text_state: TextState,
}

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

/// Create a rounded rect with 6 vertices
fn create_rounded_rect(
    x: f32, y: f32, w: f32, h: f32,
    corner_radius: f32,
    border_width: f32,
    color: [f32; 4],
    canvas_width: f32,
    canvas_height: f32,
) -> Vec<RectVertex> {
    let padding = 2.0;
    let px = x - padding;
    let py = y - padding;
    let pw = w + padding * 2.0;
    let ph = h + padding * 2.0;

    let ndc = |px: f32, py: f32| -> [f32; 2] {
        [
            (px / canvas_width) * 2.0 - 1.0,
            1.0 - (py / canvas_height) * 2.0,
        ]
    };

    let rect_center = [x + w / 2.0, y + h / 2.0];
    let rect_half_size = [w / 2.0, h / 2.0];

    let p1 = ndc(px, py);
    let p2 = ndc(px + pw, py);
    let p3 = ndc(px, py + ph);
    let p4 = ndc(px + pw, py + ph);

    let make_vertex = |pos: [f32; 2]| RectVertex {
        position: pos,
        rect_center,
        rect_half_size,
        corner_radius,
        border_width,
        color,
    };

    vec![
        make_vertex(p1),
        make_vertex(p3),
        make_vertex(p2),
        make_vertex(p3),
        make_vertex(p4),
        make_vertex(p2),
    ]
}

/// Build SDF rectangles based on text element bounds
fn build_text_frames(elements: &[TextElement], width: f32, height: f32) -> Vec<RectVertex> {
    let mut vertices = Vec::new();

    // Colors
    let blue = [0.2, 0.4, 0.8, 1.0];
    let green = [0.2, 0.6, 0.3, 1.0];
    let red = [0.8, 0.2, 0.2, 1.0];

    for (i, elem) in elements.iter().enumerate() {
        match i {
            // First text: tiny padding, no radius
            0 => {
                let padding = 3.0;
                vertices.extend(create_rounded_rect(
                    elem.rect_x - padding,
                    elem.rect_y - padding,
                    elem.width + padding * 2.0,
                    elem.height + padding * 2.0,
                    0.0,  // no corner radius
                    1.5,  // stroked
                    blue,
                    width, height,
                ));
            }
            // Second text: with padding (no radius)
            1 => {
                let padding = 8.0;
                vertices.extend(create_rounded_rect(
                    elem.rect_x - padding,
                    elem.rect_y - padding,
                    elem.width + padding * 2.0,
                    elem.height + padding * 2.0,
                    0.0,  // no corner radius
                    1.5,
                    green,
                    width, height,
                ));
            }
            // Third text: with padding and rounded corners
            2 => {
                let padding = 8.0;
                let box_height = elem.height + padding * 2.0;
                vertices.extend(create_rounded_rect(
                    elem.rect_x - padding,
                    elem.rect_y - padding,
                    elem.width + padding * 2.0,
                    box_height,
                    box_height / 4.0,  // subtle rounded corners
                    1.5,
                    red,
                    width, height,
                ));
            }
            // More examples with different text lengths
            _ => {
                let padding = 8.0;
                let box_height = elem.height + padding * 2.0;
                vertices.extend(create_rounded_rect(
                    elem.rect_x - padding,
                    elem.rect_y - padding,
                    elem.width + padding * 2.0,
                    box_height,
                    box_height / 4.0,  // subtle rounded corners
                    1.5,
                    red,
                    width, height,
                ));
            }
        }
    }

    vertices
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let window_attrs = WindowAttributes::default()
            .with_title("Text + SDF Rectangles Demo")
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
            label: Some("Device"),
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

        // Initialize text rendering FIRST to measure bounds
        let mut font_system = FontSystem::new();
        let swash_cache = SwashCache::new();
        let text_cache = TextCache::new(&device);
        let viewport = Viewport::new(&device, &text_cache);
        let mut atlas = TextAtlas::new(&device, &queue, &text_cache, config.format);
        let text_renderer = TextRenderer::new(&mut atlas, &device, MultisampleState::default(), None);

        // Create text elements and measure their bounds
        let font_size = 24.0;
        let line_height = font_size; // Use font_size for tighter fit

        let texts = [
            ("Short", 50.0, 100.0),
            ("Medium Length", 50.0, 180.0),
            ("A Longer Text String", 50.0, 260.0),
            ("Intro", 50.0, 380.0),
            ("Verse 1", 150.0, 380.0),
            ("Chorus", 280.0, 380.0),
            ("Bridge", 400.0, 380.0),
            ("Outro", 520.0, 380.0),
        ];

        let mut elements = Vec::new();

        for (text, x, y) in texts {
            let mut buffer = TextBuffer::new(&mut font_system, Metrics::new(font_size, font_size));
            buffer.set_size(&mut font_system, Some(500.0), Some(100.0));
            buffer.set_text(
                &mut font_system,
                text,
                &Attrs::new().family(Family::SansSerif).weight(glyphon::Weight::BOLD),
                Shaping::Advanced,
                None,
            );
            buffer.shape_until_scroll(&mut font_system, false);

            // Measure actual text width from layout runs
            let text_width: f32 = buffer.layout_runs()
                .map(|run| run.line_w)
                .next()
                .unwrap_or(50.0);

            // Get the line top offset from layout runs (where glyphs actually start)
            let line_top: f32 = buffer.layout_runs()
                .map(|run| run.line_top)
                .next()
                .unwrap_or(0.0);

            // Use font_size as the visible text height
            let text_height = font_size;

            // The actual Y position where text renders is offset by line_top
            let actual_y = y + line_top;

            println!("Text '{}': text_pos=({}, {}), rect_pos=({}, {}), size=({:.1}x{:.1})",
                text, x, y, x, actual_y, text_width, text_height);

            elements.push(TextElement {
                buffer,
                text_x: x,
                text_y: y,  // Original position for text rendering
                rect_x: x,
                rect_y: actual_y,  // Adjusted position for rectangle
                width: text_width,
                height: text_height,
            });
        }

        // Create SDF pipeline
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("SDF Shader"),
            source: wgpu::ShaderSource::Wgsl(SDF_SHADER.into()),
        });

        let camera_data = [self.view.zoom, self.view.pan_x, self.view.pan_y, 0.0f32];
        let camera_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Camera Buffer"),
            contents: bytemuck::cast_slice(&camera_data),
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });

        let camera_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Camera Layout"),
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

        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Pipeline Layout"),
            bind_group_layouts: &[&camera_bind_group_layout],
            immediate_size: 0,
        });

        let sdf_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("SDF Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<RectVertex>() as u64,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute { offset: 0, shader_location: 0, format: wgpu::VertexFormat::Float32x2 },
                        VertexAttribute { offset: 8, shader_location: 1, format: wgpu::VertexFormat::Float32x2 },
                        VertexAttribute { offset: 16, shader_location: 2, format: wgpu::VertexFormat::Float32x2 },
                        VertexAttribute { offset: 24, shader_location: 3, format: wgpu::VertexFormat::Float32 },
                        VertexAttribute { offset: 28, shader_location: 4, format: wgpu::VertexFormat::Float32 },
                        VertexAttribute { offset: 32, shader_location: 5, format: wgpu::VertexFormat::Float32x4 },
                    ],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
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

        // Build SDF vertices based on measured text bounds
        let sdf_vertices = build_text_frames(&elements, WINDOW_WIDTH as f32, WINDOW_HEIGHT as f32);
        let sdf_vertex_count = sdf_vertices.len() as u32;

        let sdf_vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("SDF Vertex Buffer"),
            contents: bytemuck::cast_slice(&sdf_vertices),
            usage: BufferUsages::VERTEX,
        });

        let text_state = TextState {
            font_system,
            swash_cache,
            text_cache,
            viewport,
            atlas,
            text_renderer,
            elements,
        };

        self.render_state = Some(RenderState {
            surface,
            device,
            queue,
            config,
            sdf_pipeline,
            sdf_vertex_buffer,
            sdf_vertex_count,
            camera_buffer,
            camera_bind_group,
            text_state,
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
                            self.view = ViewState::default();
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

                let zoom_factor = 1.1f32.powf(scroll_amount);
                // Limit zoom to 4x (8x with retina) for glyphon text rendering stability
                let new_zoom = (self.view.zoom * zoom_factor).clamp(0.1, 4.0);

                if let Some(state) = &self.render_state {
                    let width = state.config.width as f32;
                    let height = state.config.height as f32;

                    let mouse_ndc_x = (self.view.mouse_x / width) * 2.0 - 1.0;
                    let mouse_ndc_y = 1.0 - (self.view.mouse_y / height) * 2.0;

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
                if let Some(state) = &mut self.render_state {
                    // Update camera
                    let camera_data = [self.view.zoom, self.view.pan_x, self.view.pan_y, 0.0f32];
                    state.queue.write_buffer(&state.camera_buffer, 0, bytemuck::cast_slice(&camera_data));

                    // Update viewport
                    state.text_state.viewport.update(
                        &state.queue,
                        Resolution {
                            width: state.config.width,
                            height: state.config.height,
                        },
                    );

                    // Build text areas with camera transform
                    let width = state.config.width as f32;
                    let height = state.config.height as f32;
                    let zoom = self.view.zoom;
                    let pan_x = self.view.pan_x;
                    let pan_y = self.view.pan_y;

                    // Scale factor for retina displays (physical / logical)
                    let scale_factor = width / WINDOW_WIDTH as f32;

                    let text_areas: Vec<TextArea> = state.text_state.elements
                        .iter()
                        .map(|elem| {
                            // Use text_x/text_y for text rendering position
                            // Convert logical position to NDC
                            let ndc_x = (elem.text_x / WINDOW_WIDTH as f32) * 2.0 - 1.0;
                            let ndc_y = 1.0 - (elem.text_y / WINDOW_HEIGHT as f32) * 2.0;

                            // Apply camera transform
                            let cam_x = ndc_x * zoom + pan_x;
                            let cam_y = ndc_y * zoom + pan_y;

                            // Convert back to logical screen coords, then scale to physical
                            let logical_x = (cam_x + 1.0) * WINDOW_WIDTH as f32 / 2.0;
                            let logical_y = (1.0 - cam_y) * WINDOW_HEIGHT as f32 / 2.0;

                            // Scale to physical screen coords
                            let screen_x = logical_x * scale_factor;
                            let screen_y = logical_y * scale_factor;

                            TextArea {
                                buffer: &elem.buffer,
                                left: screen_x,
                                top: screen_y,
                                scale: zoom * scale_factor,
                                bounds: TextBounds {
                                    left: 0,
                                    top: 0,
                                    right: width as i32,
                                    bottom: height as i32,
                                },
                                default_color: TextColor::rgba(0, 0, 0, 255),
                                custom_glyphs: &[],
                            }
                        })
                        .collect();

                    // Prepare text
                    let _ = state.text_state.text_renderer.prepare(
                        &state.device,
                        &state.queue,
                        &mut state.text_state.font_system,
                        &mut state.text_state.atlas,
                        &state.text_state.viewport,
                        text_areas,
                        &mut state.text_state.swash_cache,
                    );

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
                                        r: 0.98,
                                        g: 0.98,
                                        b: 0.98,
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

                        // Draw SDF rectangles first (behind text)
                        render_pass.set_pipeline(&state.sdf_pipeline);
                        render_pass.set_bind_group(0, &state.camera_bind_group, &[]);
                        render_pass.set_vertex_buffer(0, state.sdf_vertex_buffer.slice(..));
                        render_pass.draw(0..state.sdf_vertex_count, 0..1);

                        // Draw text on top
                        let _ = state.text_state.text_renderer.render(
                            &state.text_state.atlas,
                            &state.text_state.viewport,
                            &mut render_pass,
                        );
                    }

                    state.queue.submit(std::iter::once(encoder.finish()));
                    output.present();
                }
            }

            _ => {}
        }
    }
}

/// SDF shader for rounded rectangles
const SDF_SHADER: &str = r#"
struct Camera {
    zoom: f32,
    pan_x: f32,
    pan_y: f32,
    _padding: f32,
}

@group(0) @binding(0) var<uniform> camera: Camera;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) rect_center: vec2<f32>,
    @location(2) rect_half_size: vec2<f32>,
    @location(3) corner_radius: f32,
    @location(4) border_width: f32,
    @location(5) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) rect_center: vec2<f32>,
    @location(1) rect_half_size: vec2<f32>,
    @location(2) corner_radius: f32,
    @location(3) border_width: f32,
    @location(4) color: vec4<f32>,
    @location(5) pixel_pos: vec2<f32>,
    @location(6) zoom: f32,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    let scaled = in.position * camera.zoom;
    let transformed = scaled + vec2<f32>(camera.pan_x, camera.pan_y);
    out.clip_position = vec4<f32>(transformed, 0.0, 1.0);

    out.rect_center = in.rect_center;
    out.rect_half_size = in.rect_half_size;
    out.corner_radius = in.corner_radius;
    out.border_width = in.border_width;
    out.color = in.color;
    out.zoom = camera.zoom;

    out.pixel_pos = (in.position + 1.0) * 0.5 * vec2<f32>(1024.0, 800.0);
    out.pixel_pos.y = 800.0 - out.pixel_pos.y;

    return out;
}

fn sdf_rounded_rect(p: vec2<f32>, center: vec2<f32>, half_size: vec2<f32>, radius: f32) -> f32 {
    let d = abs(p - center) - half_size + radius;
    return length(max(d, vec2<f32>(0.0))) + min(max(d.x, d.y), 0.0) - radius;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let dist = sdf_rounded_rect(
        in.pixel_pos,
        in.rect_center,
        in.rect_half_size,
        in.corner_radius
    );

    var color = in.color;
    let aa_width = 0.5 / in.zoom;

    if in.border_width > 0.0 {
        let inner_dist = dist + in.border_width;
        let outer_alpha = 1.0 - smoothstep(-aa_width, aa_width, dist);
        let inner_alpha = smoothstep(-aa_width, aa_width, inner_dist);
        color.a *= outer_alpha * inner_alpha;
    } else {
        color.a *= 1.0 - smoothstep(-aa_width, aa_width, dist);
    }

    return color;
}
"#;

fn main() {
    env_logger::init();
    println!("Text + SDF Demo");
    println!("- First text: exact hitbox (blue, no padding)");
    println!("- Second text: with padding (green)");
    println!("- Third+ texts: rounded pill shape (red)");
    println!("");
    println!("Controls: Scroll=Zoom, Space+Drag=Pan, R=Reset");

    let event_loop = EventLoop::new().unwrap();
    let mut app = App::new();
    event_loop.run_app(&mut app).unwrap();
}
