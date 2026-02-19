//! Playground for signal collection/variant UI.
//!
//! Tabbed gallery showcasing every smart view in `signal_ui::views`.

#[cfg(feature = "desktop")]
use anyrender::ImageRenderer;
#[cfg(feature = "desktop")]
use anyrender_vello::{VelloImageRenderer, VelloScenePainter};
#[cfg(feature = "desktop")]
use dioxus::desktop::tao::window::Window;
#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::dioxus_elements::geometry::WheelDelta;
use dioxus::prelude::*;
use frame_import::import_figma_bytes_with_diagnostics;
use frame_proto::{AutoLayout, FrameDocument, NodeId};
use frame_ui::{
    build_layout_boxes, build_paint_primitives, collect_render_diagnostics,
    paint_primitives_into_scene_with, TextFontRef,
};
#[cfg(feature = "desktop")]
use kurbo::{Affine, Rect, Stroke};
use lumen_blocks::components::button::{Button, ButtonSize, ButtonVariant};
use lumen_blocks::components::dropdown::{
    Dropdown, DropdownContent, DropdownItem, DropdownTrigger,
};
use lumen_blocks::components::input::{Input, InputSize};
#[cfg(feature = "desktop")]
use peniko::{Color, Fill};
use signal::{bootstrap_in_memory_controller_async, BlockType, SignalController};
use signal_ui::views::{
    BlockEditor, CollectionBrowser, MetadataDisplay, ModuleView, ModuleViewMode, RigSceneGrid,
    SignalSlider,
};
#[cfg(feature = "desktop")]
use std::sync::{Arc, Mutex, OnceLock};
#[cfg(feature = "desktop")]
use std::{
    net::TcpStream,
    process::{Child, Command, Stdio},
    time::Duration,
};

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const FRAME_BRIDGE_JSON_PATH: &str = "/tmp/fts-figma-export.json";
#[cfg(feature = "desktop")]
const FRAME_BRIDGE_ADDR: &str = "127.0.0.1:43123";
#[cfg(feature = "desktop")]
const FRAME_BRIDGE_WORKDIR: &str =
    "/Users/codywright/Documents/Development/Rust/roam-test/packages/fts-figma-import";

// region: --- Tab definitions

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tab {
    Slider,
    BlockEditor,
    ModuleView,
    CollectionBrowser,
    SceneGrid,
    Metadata,
    Frame,
}

impl Tab {
    const ALL: &[Tab] = &[
        Tab::Slider,
        Tab::BlockEditor,
        Tab::ModuleView,
        Tab::CollectionBrowser,
        Tab::SceneGrid,
        Tab::Metadata,
        Tab::Frame,
    ];

    fn label(self) -> &'static str {
        match self {
            Self::Slider => "Slider",
            Self::BlockEditor => "Block Editor",
            Self::ModuleView => "Module View",
            Self::CollectionBrowser => "Browser",
            Self::SceneGrid => "Scene Grid",
            Self::Metadata => "Metadata",
            Self::Frame => "Frame",
        }
    }
}

// endregion: --- Tab definitions

fn main() {
    #[cfg(feature = "desktop")]
    {
        let config = Config::new()
            .with_window(
                WindowBuilder::new()
                    .with_title("Playground")
                    .with_transparent(true),
            )
            .with_background_color((0, 0, 0, 0))
            .with_on_window(|window, dom| {
                let size = window.inner_size();
                let graphics = Arc::new(Mutex::new(FrameGraphics::new(
                    window,
                    size.width,
                    size.height,
                )));
                dom.provide_root_context(graphics);
            })
            .with_as_child_window();

        dioxus::LaunchBuilder::desktop()
            .with_cfg(config)
            .launch(App);
    }

    #[cfg(not(feature = "desktop"))]
    dioxus::launch(App);
}

/// Custom renderer using `vello::util::RenderContext` directly.
///
/// This matches vello's own example architecture:
///   1. `render_to_texture` into an intermediate texture (compute shader)
///   2. `get_current_texture` from the swap chain (natural backpressure point)
///   3. Blit intermediate → surface via `TextureBlitter`
///   4. Present + non-blocking poll (CPU/GPU pipelining)
///
/// This avoids `wgpu_context`'s `MemoryHints::MemoryUsage` which tells the
/// Metal driver to minimize memory at the cost of performance.
#[cfg(feature = "desktop")]
struct FrameGraphics {
    render_cx: vello::util::RenderContext,
    surface: vello::util::RenderSurface<'static>,
    vello_renderer: vello::Renderer,
    scene: vello::Scene,
    width: u32,
    height: u32,
    frame_count: u64,
    phase_accum: [f64; 5], // draw_fn, render_to_tex, get_texture, blit+present, poll
    // GPU content cache — render complex scene once, blit per frame
    content_texture: Option<wgpu::Texture>,
    content_view: Option<wgpu::TextureView>,
    content_width: u32,
    content_height: u32,
    // Custom blit pipeline (fullscreen triangle + texture sample with transform)
    blit_pipeline: wgpu::RenderPipeline,
    blit_bind_group_layout: wgpu::BindGroupLayout,
    blit_sampler: wgpu::Sampler,
    blit_uniform_buffer: wgpu::Buffer,
}

/// Uniform buffer layout for the blit shader (7 × vec4<f32> = 112 bytes).
#[repr(C)]
#[derive(Clone, Copy)]
#[cfg(feature = "desktop")]
struct BlitParams {
    /// xy = screen-space offset where content origin appears, zw = screen-space size of content
    content_offset_scale: [f32; 4],
    /// Panel background color (linear RGBA)
    bg_color: [f32; 4],
    /// Selection overlay rect [x0, y0, x1, y1] in screen pixels (all zeros = no selection)
    sel_rect: [f32; 4],
    /// Selection border color (linear RGBA)
    sel_color: [f32; 4],
    /// Hover overlay rect [x0, y0, x1, y1] in screen pixels (all zeros = no hover)
    hover_rect: [f32; 4],
    /// Hover border color (linear RGBA)
    hover_color: [f32; 4],
    /// x = border_width, y = dpr, zw = unused
    misc: [f32; 4],
}

#[cfg(feature = "desktop")]
const BLIT_SHADER: &str = r#"
struct Params {
    content_offset_scale: vec4<f32>,
    bg_color: vec4<f32>,
    sel_rect: vec4<f32>,
    sel_color: vec4<f32>,
    hover_rect: vec4<f32>,
    hover_color: vec4<f32>,
    misc: vec4<f32>,
}

@group(0) @binding(0) var content_tex: texture_2d<f32>;
@group(0) @binding(1) var content_samp: sampler;
@group(0) @binding(2) var<uniform> params: Params;

@vertex
fn vs_main(@builtin(vertex_index) vi: u32) -> @builtin(position) vec4<f32> {
    // Fullscreen triangle: 3 vertices cover the entire clip space
    let uv = vec2<f32>(f32((vi << 1u) & 2u), f32(vi & 2u));
    return vec4<f32>(uv * 2.0 - 1.0, 0.0, 1.0);
}

fn is_on_border(pos: vec2<f32>, rect: vec4<f32>, w: f32) -> bool {
    if rect.z <= rect.x || rect.w <= rect.y { return false; }
    let outer = pos.x >= rect.x - w && pos.x <= rect.z + w
             && pos.y >= rect.y - w && pos.y <= rect.w + w;
    let inner = pos.x > rect.x + w && pos.x < rect.z - w
             && pos.y > rect.y + w && pos.y < rect.w - w;
    return outer && !inner;
}

@fragment
fn fs_main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {
    let px = pos.xy;
    let uv = (px - params.content_offset_scale.xy) / params.content_offset_scale.zw;
    var color = params.bg_color;

    if uv.x >= 0.0 && uv.x <= 1.0 && uv.y >= 0.0 && uv.y <= 1.0 {
        let tex = textureSample(content_tex, content_samp, uv);
        color = vec4<f32>(tex.rgb + color.rgb * (1.0 - tex.a), 1.0);
    }

    let bw = params.misc.x;
    if is_on_border(px, params.sel_rect, bw) {
        let c = params.sel_color;
        color = vec4<f32>(c.rgb * c.a + color.rgb * (1.0 - c.a), 1.0);
    }
    if is_on_border(px, params.hover_rect, bw * 0.67) {
        let c = params.hover_color;
        color = vec4<f32>(c.rgb * c.a + color.rgb * (1.0 - c.a), 1.0);
    }
    return color;
}
"#;

#[cfg(feature = "desktop")]
impl FrameGraphics {
    fn new(window: Arc<Window>, width: u32, height: u32) -> Self {
        let mut render_cx = vello::util::RenderContext::new();

        let surface = pollster::block_on(render_cx.create_surface(
            window,
            width,
            height,
            wgpu::PresentMode::AutoVsync,
        ))
        .expect("Error creating vello render surface");

        let device_handle = &render_cx.devices[surface.dev_id];
        let vello_renderer = vello::Renderer::new(
            &device_handle.device,
            vello::RendererOptions {
                antialiasing_support: vello::AaSupport::area_only(),
                use_cpu: false,
                num_init_threads: None,
                pipeline_cache: None,
            },
        )
        .expect("Error creating vello renderer");

        // Build custom blit pipeline for GPU-cached content
        let blit_shader = device_handle
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("Blit Shader"),
                source: wgpu::ShaderSource::Wgsl(BLIT_SHADER.into()),
            });
        let blit_bind_group_layout =
            device_handle
                .device
                .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("Blit BGL"),
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
        let blit_pipeline_layout =
            device_handle
                .device
                .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("Blit PL"),
                    bind_group_layouts: &[&blit_bind_group_layout],
                    push_constant_ranges: &[],
                });
        let surface_format = surface.config.format;
        let blit_pipeline =
            device_handle
                .device
                .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some("Blit Pipeline"),
                    layout: Some(&blit_pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: &blit_shader,
                        entry_point: Some("vs_main"),
                        buffers: &[],
                        compilation_options: Default::default(),
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: &blit_shader,
                        entry_point: Some("fs_main"),
                        targets: &[Some(wgpu::ColorTargetState {
                            format: surface_format,
                            blend: None,
                            write_mask: wgpu::ColorWrites::ALL,
                        })],
                        compilation_options: Default::default(),
                    }),
                    primitive: wgpu::PrimitiveState::default(),
                    depth_stencil: None,
                    multisample: wgpu::MultisampleState::default(),
                    multiview: None,
                    cache: None,
                });
        let blit_sampler = device_handle
            .device
            .create_sampler(&wgpu::SamplerDescriptor {
                label: Some("Blit Sampler"),
                mag_filter: wgpu::FilterMode::Linear,
                min_filter: wgpu::FilterMode::Linear,
                ..Default::default()
            });
        let blit_uniform_buffer = device_handle.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Blit Params"),
            size: std::mem::size_of::<BlitParams>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Self {
            render_cx,
            surface,
            vello_renderer,
            scene: vello::Scene::new(),
            width,
            height,
            frame_count: 0,
            phase_accum: [0.0; 5],
            content_texture: None,
            content_view: None,
            content_width: 0,
            content_height: 0,
            blit_pipeline,
            blit_bind_group_layout,
            blit_sampler,
            blit_uniform_buffer,
        }
    }

    fn render<F: FnOnce(&mut vello::Scene)>(&mut self, draw_fn: F) {
        use std::time::Instant;

        let t0 = Instant::now();
        draw_fn(&mut self.scene);
        let t1 = Instant::now();

        let device_handle = &self.render_cx.devices[self.surface.dev_id];

        // 1. Render scene to intermediate texture (vello compute pipeline)
        self.vello_renderer
            .render_to_texture(
                &device_handle.device,
                &device_handle.queue,
                &self.scene,
                &self.surface.target_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: self.surface.config.width,
                    height: self.surface.config.height,
                    antialiasing_method: vello::AaConfig::Area,
                },
            )
            .expect("failed to render to texture");
        let t2 = Instant::now();

        // 2. Acquire swap chain texture (BACKPRESSURE POINT — blocks if GPU is behind)
        let surface_texture = self
            .surface
            .surface
            .get_current_texture()
            .expect("failed to get surface texture");
        let t3 = Instant::now();

        // 3. Blit intermediate → surface
        let mut encoder =
            device_handle
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Surface Blit"),
                });
        let surface_view = surface_texture
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        self.surface.blitter.copy(
            &device_handle.device,
            &mut encoder,
            &self.surface.target_view,
            &surface_view,
        );
        device_handle.queue.submit([encoder.finish()]);

        // 4. Present + non-blocking poll (matches vello examples exactly)
        surface_texture.present();
        let _ = device_handle.device.poll(wgpu::PollType::Poll);
        let t4 = Instant::now();

        self.scene.reset();

        // Phase timing — print every 60 frames
        self.phase_accum[0] += (t1 - t0).as_secs_f64() * 1000.0;
        self.phase_accum[1] += (t2 - t1).as_secs_f64() * 1000.0;
        self.phase_accum[2] += (t3 - t2).as_secs_f64() * 1000.0;
        self.phase_accum[3] += (t4 - t3).as_secs_f64() * 1000.0;
        self.phase_accum[4] += (t4 - t0).as_secs_f64() * 1000.0;
        self.frame_count += 1;

        if self.frame_count % 60 == 0 {
            let n = 60.0;
            eprintln!(
                "[frame {}] avg/60: draw={:.2}ms  render_to_tex={:.2}ms  get_tex={:.2}ms  blit+present={:.2}ms  TOTAL={:.2}ms  ({:.0} effective fps)",
                self.frame_count,
                self.phase_accum[0] / n,
                self.phase_accum[1] / n,
                self.phase_accum[2] / n,
                self.phase_accum[3] / n,
                self.phase_accum[4] / n,
                n / (self.phase_accum[4] / 1000.0),
            );
            self.phase_accum = [0.0; 5];
        }
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
        self.render_cx
            .resize_surface(&mut self.surface, width, height);
    }

    fn size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// Ensure the GPU content cache texture exists at the given size.
    fn ensure_content_texture(&mut self, width: u32, height: u32) {
        if self.content_width == width
            && self.content_height == height
            && self.content_texture.is_some()
        {
            return;
        }
        let device_handle = &self.render_cx.devices[self.surface.dev_id];
        let texture = device_handle
            .device
            .create_texture(&wgpu::TextureDescriptor {
                label: Some("Cached Content"),
                size: wgpu::Extent3d {
                    width,
                    height,
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                // Rgba8Unorm — matches vello's render_to_texture target format
                format: wgpu::TextureFormat::Rgba8Unorm,
                usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
                view_formats: &[],
            });
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        self.content_texture = Some(texture);
        self.content_view = Some(view);
        self.content_width = width;
        self.content_height = height;
    }

    /// Render a vello scene into the GPU content cache. Called once per content change.
    fn render_to_content_cache(&mut self, scene: &vello::Scene, content_w: u32, content_h: u32) {
        self.ensure_content_texture(content_w, content_h);
        let device_handle = &self.render_cx.devices[self.surface.dev_id];
        self.vello_renderer
            .render_to_texture(
                &device_handle.device,
                &device_handle.queue,
                scene,
                self.content_view.as_ref().unwrap(),
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: content_w,
                    height: content_h,
                    antialiasing_method: vello::AaConfig::Area,
                },
            )
            .expect("failed to render content to cache");
    }

    /// Fast path: blit the cached GPU texture to the surface with a viewport transform.
    /// Bypasses vello's compute pipeline entirely — sub-millisecond.
    fn blit_cached(&mut self, params: &BlitParams) {
        let content_view = match self.content_view.as_ref() {
            Some(v) => v,
            None => return,
        };

        let device_handle = &self.render_cx.devices[self.surface.dev_id];

        // Write uniform buffer
        let params_bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                params as *const BlitParams as *const u8,
                std::mem::size_of::<BlitParams>(),
            )
        };
        device_handle
            .queue
            .write_buffer(&self.blit_uniform_buffer, 0, params_bytes);

        // Create bind group (recreated per frame since content_view may change)
        let bind_group = device_handle
            .device
            .create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Blit BG"),
                layout: &self.blit_bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(content_view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&self.blit_sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: self.blit_uniform_buffer.as_entire_binding(),
                    },
                ],
            });

        // Acquire swap chain texture
        let surface_texture = self
            .surface
            .surface
            .get_current_texture()
            .expect("failed to get surface texture");
        let surface_view = surface_texture
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        // Single render pass: fullscreen triangle sampling cached content
        let mut encoder =
            device_handle
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Blit Render"),
                });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Blit Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &surface_view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                ..Default::default()
            });
            pass.set_pipeline(&self.blit_pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.draw(0..3, 0..1);
        }
        device_handle.queue.submit([encoder.finish()]);

        surface_texture.present();
        let _ = device_handle.device.poll(wgpu::PollType::Poll);

        self.scene.reset();
        self.frame_count += 1;
    }

    fn has_cached_content(&self) -> bool {
        self.content_texture.is_some()
    }
}

#[cfg(feature = "desktop")]
fn ensure_frame_bridge_running() -> Result<(), String> {
    if TcpStream::connect_timeout(
        &FRAME_BRIDGE_ADDR
            .parse()
            .map_err(|e| format!("invalid bridge addr {FRAME_BRIDGE_ADDR}: {e}"))?,
        Duration::from_millis(150),
    )
    .is_ok()
    {
        return Ok(());
    }

    static BRIDGE_CHILD: OnceLock<Mutex<Option<Child>>> = OnceLock::new();
    let child_slot = BRIDGE_CHILD.get_or_init(|| Mutex::new(None));
    let mut guard = child_slot
        .lock()
        .map_err(|_| "failed to acquire bridge process lock".to_string())?;

    if guard.is_some() {
        return Ok(());
    }

    let child = Command::new("pnpm")
        .arg("bridge")
        .current_dir(FRAME_BRIDGE_WORKDIR)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|e| format!("failed to start bridge in {FRAME_BRIDGE_WORKDIR}: {e}"))?;

    *guard = Some(child);
    Ok(())
}

#[component]
fn App() -> Element {
    let mut controller = use_signal(|| None::<SignalController>);
    let mut active_tab = use_signal(|| Tab::CollectionBrowser);
    let needs_transparency = active_tab() == Tab::Frame;

    use_effect(move || {
        spawn(async move {
            let built = bootstrap_in_memory_controller_async()
                .await
                .expect("failed to initialize in-memory signal storage");
            controller.set(Some(built));
        });
    });

    #[cfg(feature = "desktop")]
    use_effect(move || {
        if active_tab() == Tab::Frame {
            document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
        } else {
            document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
        }
    });

    rsx! {
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        div {
            class: if needs_transparency {
                "h-screen w-screen flex flex-col text-zinc-100"
            } else {
                "h-screen w-screen flex flex-col bg-zinc-950 text-zinc-100"
            },
            style: if needs_transparency {
                "background: transparent !important; background-color: transparent !important;"
            } else {
                ""
            },
            // Tab bar
            nav {
                class: if needs_transparency {
                    "flex items-center gap-1 px-4 py-2 border-b border-zinc-800/80 bg-zinc-900/50 flex-shrink-0"
                } else {
                    "flex items-center gap-1 px-4 py-2 border-b border-zinc-800 bg-zinc-900/80 flex-shrink-0"
                },
                span { class: "text-xs font-bold text-zinc-500 uppercase tracking-wider mr-3", "signal" }
                for tab in Tab::ALL.iter() {
                    {
                        let t = *tab;
                        let is_active = active_tab() == t;
                        rsx! {
                            button {
                                key: "{t:?}",
                                class: if is_active {
                                    "px-3 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                                } else {
                                    "px-3 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800"
                                },
                                onclick: move |_| active_tab.set(t),
                                "{t.label()}"
                            }
                        }
                    }
                }
            }

            // Content area
            div {
                class: if needs_transparency {
                    "flex-1 min-h-0 overflow-hidden"
                } else {
                    "flex-1 min-h-0 overflow-auto"
                },
                style: if needs_transparency {
                    "background: transparent !important; background-color: transparent !important;"
                } else {
                    ""
                },
                if let Some(ctrl) = controller() {
                    {render_tab(active_tab(), ctrl)}
                } else {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-sm text-zinc-600", "Bootstrapping in-memory signal storage..." }
                    }
                }
            }
        }
    }
}

fn render_tab(tab: Tab, controller: SignalController) -> Element {
    match tab {
        Tab::Slider => rsx! {
            div { class: "max-w-2xl mx-auto p-6",
                SignalSlider { controller }
            }
        },
        Tab::BlockEditor => rsx! {
            div { class: "max-w-xl mx-auto p-6 space-y-6",
                h2 { class: "text-lg font-semibold text-zinc-300 mb-4", "Block Editors" }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Amp }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Drive }
                BlockEditor { controller: controller.clone(), block_type: BlockType::Cabinet }
                BlockEditor { controller, block_type: BlockType::Reverb }
            }
        },
        Tab::ModuleView => rsx! {
            ModuleViewTab { controller }
        },
        Tab::CollectionBrowser => rsx! {
            CollectionBrowserTab { controller }
        },
        Tab::SceneGrid => rsx! {
            SceneGridTab { controller }
        },
        Tab::Metadata => rsx! {
            div { class: "max-w-lg mx-auto p-6 space-y-6",
                h2 { class: "text-lg font-semibold text-zinc-300 mb-4", "MetadataDisplay Demo" }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "With all fields" }
                    MetadataDisplay {
                        tags: vec![
                            "tone:clean".to_string(),
                            "guitar".to_string(),
                            "worship".to_string(),
                            "ambient".to_string(),
                        ],
                        description: Some("A warm, clean tone with shimmer reverb. Great for ambient pads and worship settings.".to_string()),
                        notes: Some("Try boosting the mids slightly for a fuller sound in the mix.".to_string()),
                    }
                }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "Tags only" }
                    MetadataDisplay {
                        tags: vec!["high-gain".to_string(), "metal".to_string(), "lead".to_string()],
                    }
                }

                div { class: "rounded-lg border border-zinc-700 p-4 space-y-3",
                    h3 { class: "text-sm font-medium text-zinc-400", "Empty metadata" }
                    MetadataDisplay {}
                }
            }
        },
        Tab::Frame => rsx! {
            FramePreviewTab {}
        },
    }
}

// region: --- Module View Tab

/// Module view tab — loads a module collection and displays it.
#[component]
fn ModuleViewTab(controller: SignalController) -> Element {
    let mut module_data = use_signal(|| None::<signal::Module>);
    let mut view_mode = use_signal(|| ModuleViewMode::Compact);

    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let collections = controller.module_presets().list().await;
                if let Some(first) = collections.first() {
                    let snapshot = controller
                        .module_presets()
                        .load_default(first.id().to_string())
                        .await;
                    if let Some(snap) = snapshot {
                        module_data.set(Some(snap.module().clone()));
                    }
                }
            });
        });
    }

    rsx! {
        div { class: "max-w-xl mx-auto p-6 space-y-4",
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold text-zinc-300", "Module View" }
                div { class: "flex gap-2",
                    button {
                        class: if view_mode() == ModuleViewMode::Compact {
                            "px-3 py-1 text-xs rounded bg-zinc-700 text-zinc-100"
                        } else {
                            "px-3 py-1 text-xs rounded text-zinc-400 hover:bg-zinc-800"
                        },
                        onclick: move |_| view_mode.set(ModuleViewMode::Compact),
                        "Compact"
                    }
                    button {
                        class: if view_mode() == ModuleViewMode::Detail {
                            "px-3 py-1 text-xs rounded bg-zinc-700 text-zinc-100"
                        } else {
                            "px-3 py-1 text-xs rounded text-zinc-400 hover:bg-zinc-800"
                        },
                        onclick: move |_| view_mode.set(ModuleViewMode::Detail),
                        "Detail"
                    }
                }
            }

            if let Some(module) = module_data() {
                ModuleView {
                    module,
                    view_mode: view_mode(),
                    on_toggle_bypass: move |_block_id: String| {},
                    on_param_change: move |_change: signal_ui::views::ParamChange| {},
                }
            } else {
                p { class: "text-sm text-zinc-500", "Loading module data..." }
            }
        }
    }
}

// endregion: --- Module View Tab

// region: --- Collection Browser Tab

/// Collection browser tab — self-contained multi-column cascading browser.
#[component]
fn CollectionBrowserTab(controller: SignalController) -> Element {
    rsx! {
        div { class: "h-full",
            CollectionBrowser { controller }
        }
    }
}

// endregion: --- Collection Browser Tab

// region: --- Scene Grid Tab

/// Scene grid tab — loads the first rig and displays its scenes.
#[component]
fn SceneGridTab(controller: SignalController) -> Element {
    let mut rig_id = use_signal(|| None::<String>);
    let mut active_scene = use_signal(|| None::<String>);

    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let rigs = controller.rigs().list().await;
                if let Some(first) = rigs.first() {
                    rig_id.set(Some(first.id.to_string()));
                }
            });
        });
    }

    rsx! {
        div { class: "max-w-xl mx-auto p-6 space-y-4",
            h2 { class: "text-lg font-semibold text-zinc-300", "Rig Scene Grid" }

            if let Some(id) = rig_id() {
                div { class: "h-64 rounded-lg overflow-hidden border border-zinc-700",
                    RigSceneGrid {
                        controller,
                        rig_id: id,
                        active_scene_id: active_scene(),
                        on_scene_select: move |scene_id: String| {
                            active_scene.set(Some(scene_id));
                        },
                    }
                }
            } else {
                p { class: "text-sm text-zinc-500", "Loading rigs..." }
            }

            if let Some(scene) = active_scene() {
                p { class: "text-xs text-zinc-400", "Active scene: {scene}" }
            }
        }
    }
}

// endregion: --- Scene Grid Tab

#[component]
fn FramePreviewTab() -> Element {
    let mut selected_frame = use_signal(|| 0usize);
    let mut selected_layer = use_signal(|| None::<NodeId>);
    let mut hovered_layer = use_signal(|| None::<NodeId>);
    let mut node_overrides =
        use_signal(std::collections::HashMap::<NodeId, NodeLayoutOverride>::new);
    let mut edit_drag = use_signal(|| None::<EditDragState>);
    let mut cached_primitives = use_signal(Vec::<frame_ui::PaintPrimitive>::new);
    #[cfg(feature = "desktop")]
    let mut cached_scene_fragment = use_signal(|| None::<vello::Scene>);
    #[cfg(feature = "desktop")]
    let mut cached_raster_image = use_signal(|| None::<peniko::ImageBrush>);
    let mut cached_content_bounds = use_signal(PrimitiveBounds::default);
    let mut cached_node_bounds =
        use_signal(std::collections::HashMap::<NodeId, PrimitiveBounds>::new);
    let mut cached_render_root = use_signal(|| None::<NodeId>);
    let mut canvas_cursor = use_signal(|| "default".to_string());
    let mut view_zoom = use_signal(|| 1.0f64);
    let mut pan_x = use_signal(|| 0.0f64);
    let mut pan_y = use_signal(|| 0.0f64);
    let mut is_panning = use_signal(|| false);
    let mut pan_start_mouse = use_signal(|| (0.0f64, 0.0f64));
    let mut pan_start_offset = use_signal(|| (0.0f64, 0.0f64));
    let mut frame_doc = use_signal(|| None::<FrameDocument>);
    let mut frame_error = use_signal(|| {
        Some(format!(
            "Waiting for bridge export at {}",
            FRAME_BRIDGE_JSON_PATH
        ))
    });
    let mut frame_status =
        use_signal(|| format!("Source: {FRAME_BRIDGE_JSON_PATH} (auto reload every 350ms)"));
    let mut show_constraints_panel = use_signal(|| true);
    let mut undo_stack = use_signal(Vec::<UndoSnapshot>::new);
    let mut redo_stack = use_signal(Vec::<UndoSnapshot>::new);
    let mut undo_history = use_signal(Vec::<String>::new);
    let mut deep_select_state = use_signal(|| None::<DeepSelectState>);
    let mut perf_stats = use_signal(FramePerfStats::default);
    #[cfg(feature = "desktop")]
    let graphics = consume_context::<Arc<Mutex<FrameGraphics>>>();
    #[cfg(feature = "desktop")]
    let mut preview_bounds = use_signal(|| FramePreviewBounds::default());
    let cached_spacing_guides = use_memo(move || {
        let node_bounds = cached_node_bounds();
        let active = selected_layer().or(cached_render_root());
        match (frame_doc(), active) {
            (Some(doc), Some(selected)) => {
                spacing_guides_for_selection(&doc, &node_bounds, selected)
            }
            _ => Vec::new(),
        }
    });

    use_effect(move || {
        spawn(async move {
            let _ = document::eval(
                "setTimeout(() => document.getElementById('frame-preview-root')?.focus(), 0);",
            )
            .await;
        });
    });

    #[cfg(feature = "desktop")]
    {
        use_effect(move || {
            if let Err(err) = ensure_frame_bridge_running() {
                frame_status.set(format!("Bridge autostart failed: {err}"));
            } else {
                frame_status.set(format!(
                    "Source: {FRAME_BRIDGE_JSON_PATH} (bridge auto-start enabled, reload every 350ms)"
                ));
            }
        });

        use_future(move || async move {
            let mut last_signature: Option<(u64, u128)> = None;
            let mut last_error: Option<String> = None;
            loop {
                match tokio::fs::metadata(FRAME_BRIDGE_JSON_PATH).await {
                    Ok(meta) => {
                        let modified = meta
                            .modified()
                            .ok()
                            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                            .map(|d| d.as_millis())
                            .unwrap_or_default();
                        let signature = (meta.len(), modified);
                        if last_signature != Some(signature) {
                            last_signature = Some(signature);
                            match tokio::fs::read(FRAME_BRIDGE_JSON_PATH).await {
                                Ok(bytes) => match tokio::task::spawn_blocking(move || {
                                    import_figma_bytes_with_diagnostics(&bytes)
                                })
                                .await
                                {
                                    Ok(Ok((doc, diagnostics))) => {
                                        frame_doc.set(Some(doc));
                                        frame_error.set(None);
                                        last_error = None;
                                        let unsupported_keys =
                                            diagnostics.unsupported_node_keys.len();
                                        let unsupported_preview = diagnostics
                                            .top_unsupported_keys(5)
                                            .into_iter()
                                            .map(|(k, v)| format!("{k}:{v}"))
                                            .collect::<Vec<_>>()
                                            .join(", ");
                                        frame_status.set(format!(
                                            "Source: {FRAME_BRIDGE_JSON_PATH} ({} bytes, updated {}, schema {}, aliases {}, unsupported keys {} [{}])",
                                            signature.0,
                                            modified,
                                            diagnostics.source_schema.as_deref().unwrap_or("n/a"),
                                            diagnostics.normalized_aliases,
                                            unsupported_keys,
                                            if unsupported_preview.is_empty() {
                                                "none".to_string()
                                            } else {
                                                unsupported_preview
                                            }
                                        ));
                                    }
                                    Ok(Err(err)) => {
                                        let msg = format!(
                                            "Import failed for {}: {}",
                                            FRAME_BRIDGE_JSON_PATH, err
                                        );
                                        if last_error.as_deref() != Some(msg.as_str()) {
                                            frame_error.set(Some(msg.clone()));
                                            last_error = Some(msg);
                                        }
                                    }
                                    Err(join_err) => {
                                        let msg = format!(
                                            "Import panicked for {}: {}",
                                            FRAME_BRIDGE_JSON_PATH, join_err
                                        );
                                        if last_error.as_deref() != Some(msg.as_str()) {
                                            frame_error.set(Some(msg.clone()));
                                            last_error = Some(msg);
                                        }
                                    }
                                },
                                Err(err) => {
                                    let msg = format!(
                                        "Failed reading {}: {}",
                                        FRAME_BRIDGE_JSON_PATH, err
                                    );
                                    if last_error.as_deref() != Some(msg.as_str()) {
                                        frame_error.set(Some(msg.clone()));
                                        last_error = Some(msg);
                                    }
                                }
                            }
                        }
                    }
                    Err(err) => {
                        let msg = format!(
                            "Bridge file not found at {}: {}",
                            FRAME_BRIDGE_JSON_PATH, err
                        );
                        if last_error.as_deref() != Some(msg.as_str()) {
                            frame_error.set(Some(msg.clone()));
                            last_error = Some(msg);
                        }
                    }
                }
                tokio::time::sleep(tokio::time::Duration::from_millis(350)).await;
            }
        });

        use_effect(move || {
            dioxus::desktop::window().window.request_redraw();
        });

        use_future(move || async move {
            loop {
                tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
                let result = document::eval(
                    r#"
                        const el = document.getElementById('frame-preview-surface');
                        if (!el) return "null";
                        const rect = el.getBoundingClientRect();
                        const dpr = window.devicePixelRatio || 1;
                        return JSON.stringify({
                            x: rect.x * dpr,
                            y: rect.y * dpr,
                            width: rect.width * dpr,
                            height: rect.height * dpr,
                            dpr
                        });
                    "#,
                );
                let Ok(value) = result.await else { continue };
                let json_str = value
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| value.to_string());
                if json_str == "null" || json_str == "\"null\"" {
                    continue;
                }
                let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&json_str) else {
                    continue;
                };
                let next = FramePreviewBounds {
                    x: parsed["x"].as_f64().unwrap_or(0.0),
                    y: parsed["y"].as_f64().unwrap_or(0.0),
                    width: parsed["width"].as_f64().unwrap_or(0.0),
                    height: parsed["height"].as_f64().unwrap_or(0.0),
                    dpr: parsed["dpr"].as_f64().unwrap_or(1.0),
                };
                if next.width > 0.0 && next.height > 0.0 {
                    preview_bounds.set(next);
                }
            }
        });
    }

    #[cfg(feature = "desktop")]
    {
        use_effect(move || {
            let Some(doc) = frame_doc() else {
                cached_primitives.set(Vec::new());
                cached_scene_fragment.set(None);
                cached_raster_image.set(None);
                cached_content_bounds.set(PrimitiveBounds::default());
                cached_node_bounds.set(std::collections::HashMap::new());
                return;
            };
            let overrides = node_overrides();
            let sel = selected_frame();

            spawn(async move {
                let result = tokio::task::spawn_blocking(move || {
                    let options = collect_frame_options(&doc);
                    let selected_idx = sel.min(options.len().saturating_sub(1));
                    let root = options
                        .get(selected_idx)
                        .map(|o| o.id)
                        .unwrap_or_else(|| preview_scene_root(&doc));
                    let primitives = build_editor_primitives(&doc, root, &overrides);
                    let content = primitive_bounds(&primitives);
                    let mut node_bounds = layout_bounds_map_for_ui(&doc, root, &overrides);
                    if node_bounds.is_empty() {
                        node_bounds = node_bounds_map_from_primitives(&primitives);
                    }
                    let font_bytes = try_load_system_text_font();
                    let font_ref = font_bytes.as_ref().map(|b| TextFontRef {
                        bytes: b.as_slice(),
                        index: 0,
                    });

                    let mut vello_scene = vello::Scene::new();
                    {
                        let mut painter = VelloScenePainter::new(&mut vello_scene);
                        paint_primitives_into_scene_with(
                            &mut painter,
                            &primitives,
                            Affine::IDENTITY,
                            font_ref,
                        );
                    }

                    // Rasterize the scene for fast pan/zoom rendering
                    let content_w = (content.max_x - content.min_x).max(1.0);
                    let content_h = (content.max_y - content.min_y).max(1.0);
                    let raster_w = (content_w.ceil() as u32).clamp(1, 4096);
                    let raster_h = (content_h.ceil() as u32).clamp(1, 4096);
                    let scale_x = raster_w as f64 / content_w;
                    let scale_y = raster_h as f64 / content_h;

                    let raster_transform = Affine::scale_non_uniform(scale_x, scale_y)
                        * Affine::translate((-content.min_x, -content.min_y));
                    let mut img_renderer = VelloImageRenderer::new(raster_w, raster_h);
                    let mut rgba_buf = Vec::new();
                    img_renderer.render_to_vec(
                        |painter| {
                            paint_primitives_into_scene_with(
                                painter,
                                &primitives,
                                raster_transform,
                                font_ref,
                            );
                        },
                        &mut rgba_buf,
                    );

                    let raster_brush = peniko::ImageBrush::new(peniko::ImageData {
                        data: peniko::Blob::from(rgba_buf),
                        format: peniko::ImageFormat::Rgba8,
                        alpha_type: peniko::ImageAlphaType::AlphaPremultiplied,
                        width: raster_w,
                        height: raster_h,
                    });

                    (
                        primitives,
                        content,
                        node_bounds,
                        vello_scene,
                        raster_brush,
                        root,
                    )
                })
                .await;

                if let Ok((primitives, content, node_bounds, scene, raster_brush, root)) = result {
                    cached_scene_fragment.set(Some(scene));
                    cached_raster_image.set(Some(raster_brush));
                    cached_primitives.set(primitives);
                    cached_content_bounds.set(content);
                    cached_node_bounds.set(node_bounds);
                    cached_render_root.set(Some(root));
                }
            });
        });

        let graphics_loop = graphics.clone();
        use_future(move || {
            let graphics_loop = graphics_loop.clone();
            async move {
                // Dirty-flag state: only re-render when inputs change.
                let mut last_zoom = f64::NAN;
                let mut last_pan_x = f64::NAN;
                let mut last_pan_y = f64::NAN;
                let mut last_bounds = FramePreviewBounds::default();
                let mut last_active_node: Option<NodeId> = None;
                let mut last_hovered_node: Option<NodeId> = None;
                let mut last_prim_count: usize = 0;
                let mut last_scene_gen: usize = 0;
                let mut scene_gen: usize = 0;
                let mut last_win_w: u32 = 0;
                let mut last_win_h: u32 = 0;
                let mut last_spacing_len: usize = 0;
                let mut frames_since_viewport_change: u32 = 0;
                let mut loop_frame_count: u64 = 0;
                let mut loop_accum = [0.0f64; 3]; // dirty_check, lock+render, total_iter
                let mut last_cache_scene_gen: usize = usize::MAX; // force first cache update

                loop {
                    tokio::time::sleep(tokio::time::Duration::from_millis(8)).await;
                    let loop_start = std::time::Instant::now();

                    let bounds = preview_bounds();
                    if !bounds.is_valid() {
                        continue;
                    }

                    // Cheap reads for dirty check — no deep clones
                    let prim_count = cached_primitives.with_peek(|v| v.len());
                    if prim_count == 0 {
                        continue;
                    }
                    let zoom = *view_zoom.peek();
                    let current_pan_x = *pan_x.peek();
                    let current_pan_y = *pan_y.peek();
                    let content = *cached_content_bounds.peek();
                    let active_node = (*selected_layer.peek()).or(*cached_render_root.peek());
                    let hovered_node = *hovered_layer.peek();
                    let spacing_guides_len = cached_spacing_guides.with_peek(|v| v.len());

                    // Track scene fragment changes via primitive count as a proxy.
                    if prim_count != last_prim_count {
                        scene_gen = scene_gen.wrapping_add(1);
                    }

                    let win_size = dioxus::desktop::window().window.inner_size();
                    let dirty = zoom != last_zoom
                        || current_pan_x != last_pan_x
                        || current_pan_y != last_pan_y
                        || bounds.x != last_bounds.x
                        || bounds.y != last_bounds.y
                        || bounds.width != last_bounds.width
                        || bounds.height != last_bounds.height
                        || bounds.dpr != last_bounds.dpr
                        || active_node != last_active_node
                        || hovered_node != last_hovered_node
                        || prim_count != last_prim_count
                        || scene_gen != last_scene_gen
                        || win_size.width != last_win_w
                        || win_size.height != last_win_h
                        || spacing_guides_len != last_spacing_len;

                    if !dirty {
                        continue;
                    }

                    // Detect viewport-only vs content changes for raster/vector switching
                    let viewport_changed = zoom != last_zoom
                        || current_pan_x != last_pan_x
                        || current_pan_y != last_pan_y;
                    let content_changed =
                        prim_count != last_prim_count || scene_gen != last_scene_gen;

                    if viewport_changed {
                        frames_since_viewport_change = 0;
                    } else {
                        frames_since_viewport_change =
                            frames_since_viewport_change.saturating_add(1);
                    }

                    // (raster path superseded by GPU blit — kept for reference)
                    let _use_raster =
                        viewport_changed || (frames_since_viewport_change < 20 && !content_changed);

                    last_zoom = zoom;
                    last_pan_x = current_pan_x;
                    last_pan_y = current_pan_y;
                    last_bounds = bounds;
                    last_active_node = active_node;
                    last_hovered_node = hovered_node;
                    last_prim_count = prim_count;
                    last_scene_gen = scene_gen;
                    last_win_w = win_size.width;
                    last_win_h = win_size.height;
                    last_spacing_len = spacing_guides_len;

                    // Expensive reads only on dirty frames
                    let node_bounds = cached_node_bounds();
                    let selected_overlay_css = active_node
                        .and_then(|id| node_bounds.get(&id).copied())
                        .map(|world| {
                            selection_overlay_rect(
                                world,
                                content,
                                bounds,
                                zoom,
                                current_pan_x,
                                current_pan_y,
                            )
                        });
                    let hovered_overlay_css = hovered_node
                        .and_then(|id| node_bounds.get(&id).copied())
                        .filter(|_| hovered_node != active_node)
                        .map(|world| {
                            selection_overlay_rect(
                                world,
                                content,
                                bounds,
                                zoom,
                                current_pan_x,
                                current_pan_y,
                            )
                        });
                    let pad = 16.0 * bounds.dpr.max(1.0);
                    let transform = Affine::translate((
                        bounds.x + pad + current_pan_x,
                        bounds.y + pad + current_pan_y,
                    )) * Affine::scale(zoom)
                        * Affine::translate((-content.min_x, -content.min_y));

                    let dirty_check_ms = loop_start.elapsed().as_secs_f64() * 1000.0;

                    let lock_start = std::time::Instant::now();
                    let use_blit = viewport_changed && !content_changed;

                    if let Ok(mut gfx) = graphics_loop.lock() {
                        let frame_start = std::time::Instant::now();
                        let (sw, sh) = gfx.size();
                        if sw != win_size.width || sh != win_size.height {
                            gfx.resize(win_size.width, win_size.height);
                        }

                        // Update GPU content cache when scene changes
                        if scene_gen != last_cache_scene_gen {
                            let guard = cached_scene_fragment.peek();
                            if let Some(ref frag) = *guard {
                                let cw = ((content.max_x - content.min_x).ceil() as u32).max(1);
                                let ch = ((content.max_y - content.min_y).ceil() as u32).max(1);
                                let mut cache_scene = vello::Scene::new();
                                cache_scene.append(
                                    frag,
                                    Some(Affine::translate((-content.min_x, -content.min_y))),
                                );
                                let t_cache = std::time::Instant::now();
                                gfx.render_to_content_cache(&cache_scene, cw, ch);
                                eprintln!(
                                    "[cache] rendered {}x{} content to GPU texture in {:.1}ms",
                                    cw,
                                    ch,
                                    t_cache.elapsed().as_secs_f64() * 1000.0
                                );
                                last_cache_scene_gen = scene_gen;
                            }
                        }

                        let render_path;
                        if use_blit && gfx.has_cached_content() {
                            // FAST PATH: blit GPU-cached texture with viewport transform
                            // Bypasses vello's compute pipeline entirely — sub-millisecond.
                            render_path = "gpu-blit";
                            let content_w = (content.max_x - content.min_x).max(1.0);
                            let content_h = (content.max_y - content.min_y).max(1.0);
                            let params = BlitParams {
                                content_offset_scale: [
                                    (bounds.x + pad + current_pan_x) as f32,
                                    (bounds.y + pad + current_pan_y) as f32,
                                    (zoom * content_w) as f32,
                                    (zoom * content_h) as f32,
                                ],
                                bg_color: [5.0 / 255.0, 6.0 / 255.0, 12.0 / 255.0, 1.0],
                                sel_rect: selected_overlay_css
                                    .as_ref()
                                    .map(|s| {
                                        [
                                            (bounds.x + s.x * bounds.dpr) as f32,
                                            (bounds.y + s.y * bounds.dpr) as f32,
                                            (bounds.x + (s.x + s.width) * bounds.dpr) as f32,
                                            (bounds.y + (s.y + s.height) * bounds.dpr) as f32,
                                        ]
                                    })
                                    .unwrap_or([0.0; 4]),
                                sel_color: [
                                    59.0 / 255.0,
                                    130.0 / 255.0,
                                    246.0 / 255.0,
                                    230.0 / 255.0,
                                ],
                                hover_rect: hovered_overlay_css
                                    .as_ref()
                                    .map(|h| {
                                        [
                                            (bounds.x + h.x * bounds.dpr) as f32,
                                            (bounds.y + h.y * bounds.dpr) as f32,
                                            (bounds.x + (h.x + h.width) * bounds.dpr) as f32,
                                            (bounds.y + (h.y + h.height) * bounds.dpr) as f32,
                                        ]
                                    })
                                    .unwrap_or([0.0; 4]),
                                hover_color: [
                                    220.0 / 255.0,
                                    220.0 / 255.0,
                                    225.0 / 255.0,
                                    170.0 / 255.0,
                                ],
                                misc: [
                                    (1.5 * bounds.dpr.max(1.0)) as f32,
                                    (bounds.dpr as f32).max(1.0),
                                    0.0,
                                    0.0,
                                ],
                            };
                            gfx.blit_cached(&params);
                        } else {
                            // FULL VELLO PATH: pixel-perfect with all overlays
                            render_path = if gfx.has_cached_content() {
                                "vello-cached"
                            } else {
                                "vello-full"
                            };
                            gfx.render(|scene| {
                                let panel = Rect::new(
                                    bounds.x,
                                    bounds.y,
                                    bounds.x + bounds.width,
                                    bounds.y + bounds.height,
                                );
                                scene.fill(
                                    Fill::NonZero,
                                    Affine::IDENTITY,
                                    Color::from_rgb8(5, 6, 12),
                                    None,
                                    &panel,
                                );

                                // Use cached scene fragment (append is just a memcpy)
                                let guard = cached_scene_fragment.peek();
                                if let Some(ref frag) = *guard {
                                    scene.append(frag, Some(transform));
                                } else {
                                    let primitives = cached_primitives();
                                    let mut painter = VelloScenePainter::new(scene);
                                    paint_primitives_into_scene_with(
                                        &mut painter,
                                        &primitives,
                                        transform,
                                        None,
                                    );
                                }

                                if let Some(hover) = hovered_overlay_css {
                                    let rect = Rect::new(
                                        bounds.x + hover.x * bounds.dpr,
                                        bounds.y + hover.y * bounds.dpr,
                                        bounds.x + (hover.x + hover.width) * bounds.dpr,
                                        bounds.y + (hover.y + hover.height) * bounds.dpr,
                                    );
                                    scene.stroke(
                                        &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                        Affine::IDENTITY,
                                        Color::from_rgba8(220, 220, 225, 170),
                                        None,
                                        &rect,
                                    );
                                }

                                if let Some(sel) = selected_overlay_css {
                                    let rect = Rect::new(
                                        bounds.x + sel.x * bounds.dpr,
                                        bounds.y + sel.y * bounds.dpr,
                                        bounds.x + (sel.x + sel.width) * bounds.dpr,
                                        bounds.y + (sel.y + sel.height) * bounds.dpr,
                                    );
                                    scene.stroke(
                                        &Stroke::new(1.5 * bounds.dpr.max(1.0)),
                                        Affine::IDENTITY,
                                        Color::from_rgba8(59, 130, 246, 230),
                                        None,
                                        &rect,
                                    );

                                    let handle_size = 8.0 * bounds.dpr.max(1.0);
                                    let half = handle_size * 0.5;
                                    for (hx, hy) in [
                                        (rect.x0, rect.y0),
                                        (rect.x1, rect.y0),
                                        (rect.x0, rect.y1),
                                        (rect.x1, rect.y1),
                                    ] {
                                        let handle_rect =
                                            Rect::new(hx - half, hy - half, hx + half, hy + half);
                                        scene.fill(
                                            Fill::NonZero,
                                            Affine::IDENTITY,
                                            Color::from_rgba8(59, 130, 246, 240),
                                            None,
                                            &handle_rect,
                                        );
                                    }
                                }

                                let spacing_guides = cached_spacing_guides();
                                for guide in &*spacing_guides {
                                    let x0 = bounds.x
                                        + world_to_panel_css_x(
                                            guide.x0,
                                            content,
                                            zoom,
                                            current_pan_x,
                                            bounds.dpr,
                                        ) * bounds.dpr;
                                    let y0 = bounds.y
                                        + world_to_panel_css_y(
                                            guide.y0,
                                            content,
                                            zoom,
                                            current_pan_y,
                                            bounds.dpr,
                                        ) * bounds.dpr;
                                    let x1 = bounds.x
                                        + world_to_panel_css_x(
                                            guide.x1,
                                            content,
                                            zoom,
                                            current_pan_x,
                                            bounds.dpr,
                                        ) * bounds.dpr;
                                    let y1 = bounds.y
                                        + world_to_panel_css_y(
                                            guide.y1,
                                            content,
                                            zoom,
                                            current_pan_y,
                                            bounds.dpr,
                                        ) * bounds.dpr;

                                    let line = kurbo::Line::new((x0, y0), (x1, y1));
                                    scene.stroke(
                                        &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                        Affine::IDENTITY,
                                        Color::from_rgba8(255, 54, 129, 220),
                                        None,
                                        &line,
                                    );

                                    let tick = 4.0 * bounds.dpr.max(1.0);
                                    match guide.axis {
                                        Axis::Horizontal => {
                                            let t0 =
                                                kurbo::Line::new((x0, y0 - tick), (x0, y0 + tick));
                                            let t1 =
                                                kurbo::Line::new((x1, y1 - tick), (x1, y1 + tick));
                                            scene.stroke(
                                                &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                                Affine::IDENTITY,
                                                Color::from_rgba8(255, 54, 129, 220),
                                                None,
                                                &t0,
                                            );
                                            scene.stroke(
                                                &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                                Affine::IDENTITY,
                                                Color::from_rgba8(255, 54, 129, 220),
                                                None,
                                                &t1,
                                            );
                                        }
                                        Axis::Vertical => {
                                            let t0 =
                                                kurbo::Line::new((x0 - tick, y0), (x0 + tick, y0));
                                            let t1 =
                                                kurbo::Line::new((x1 - tick, y1), (x1 + tick, y1));
                                            scene.stroke(
                                                &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                                Affine::IDENTITY,
                                                Color::from_rgba8(255, 54, 129, 220),
                                                None,
                                                &t0,
                                            );
                                            scene.stroke(
                                                &Stroke::new(1.0 * bounds.dpr.max(1.0)),
                                                Affine::IDENTITY,
                                                Color::from_rgba8(255, 54, 129, 220),
                                                None,
                                                &t1,
                                            );
                                        }
                                    }
                                }
                            });
                        }

                        let frame_ms = frame_start.elapsed().as_secs_f64() * 1000.0;
                        let fps = if frame_ms > 0.0 {
                            1000.0 / frame_ms
                        } else {
                            0.0
                        };
                        perf_stats.with_mut(|stats| {
                            stats.update(frame_ms, fps, prim_count);
                        });

                        // Loop-level timing
                        let total_iter_ms = loop_start.elapsed().as_secs_f64() * 1000.0;
                        let lock_render_ms = lock_start.elapsed().as_secs_f64() * 1000.0;
                        loop_accum[0] += dirty_check_ms;
                        loop_accum[1] += lock_render_ms;
                        loop_accum[2] += total_iter_ms;
                        loop_frame_count += 1;
                        if loop_frame_count % 60 == 0 {
                            let n = 60.0;
                            eprintln!(
                                "[loop {}] avg/60: dirty_check={:.2}ms  lock+render={:.2}ms  total_iter={:.2}ms  ({:.0} fps) path={}",
                                loop_frame_count,
                                loop_accum[0] / n,
                                loop_accum[1] / n,
                                loop_accum[2] / n,
                                n / (loop_accum[2] / 1000.0),
                                render_path,
                            );
                            loop_accum = [0.0; 3];
                        }
                    }
                    dioxus::desktop::window().window.request_redraw();
                }
            }
        });
    }

    let frame_content = if let Some(doc) = frame_doc() {
        let options = collect_frame_options(&doc);
        let selected_idx = selected_frame().min(options.len().saturating_sub(1));
        let selected_frame_label = options
            .get(selected_idx)
            .map(|o| o.label.clone())
            .unwrap_or_else(|| "Select frame".to_string());
        let render_root = options
            .get(selected_idx)
            .map(|o| o.id)
            .unwrap_or_else(|| preview_scene_root(&doc));
        let layer_rows = collect_layer_rows(&doc);
        let edited_primitives = cached_primitives();
        let node_bounds_map = cached_node_bounds();
        let content_bounds = cached_content_bounds();
        let active_node = selected_layer().unwrap_or(render_root);
        let selected_world_bounds = node_bounds_map.get(&active_node).copied();
        let spacing_guides_overlay = cached_spacing_guides();
        let hovered_layer_id = hovered_layer();
        let overlay_rect = selected_world_bounds.map(|world| {
            selection_overlay_rect(
                world,
                content_bounds,
                preview_bounds(),
                view_zoom(),
                pan_x(),
                pan_y(),
            )
        });
        let active_projection = doc.project_node(active_node);
        let parent_projection = doc
            .get_node(active_node)
            .and_then(|n| n.parent)
            .and_then(|id| doc.project_node(id));
        let parent_spacing = sibling_spacing_snapshot(&doc, &node_bounds_map, active_node);
        let h_constraint = constraint_horizontal(&doc, active_node);
        let v_constraint = constraint_vertical(&doc, active_node);
        let rotation_value = doc
            .get_node(active_node)
            .and_then(|n| n.raw.get("rotation"))
            .and_then(|v| v.as_f64())
            .unwrap_or(0.0);
        let selected_layer_id = selected_layer();
        let options_for_click = options.clone();
        let options_for_dblclick = options.clone();
        let click_primitives = edited_primitives.clone();
        let dblclick_primitives = edited_primitives.clone();
        let hover_primitives = edited_primitives.clone();
        let doc_for_hover = doc.clone();
        let doc_for_click = doc.clone();
        let doc_for_dblclick = doc.clone();
        let doc_for_tidy = doc.clone();
        let node_bounds_for_tidy = node_bounds_map.clone();
        let doc_for_distribute_v = doc.clone();
        let node_bounds_for_distribute_v = node_bounds_map.clone();
        let doc_for_distribute_h = doc.clone();
        let node_bounds_for_distribute_h = node_bounds_map.clone();
        let doc_for_copy = doc.clone();
        let active_projection_for_copy = active_projection.clone();
        let parent_projection_for_copy = parent_projection.clone();
        let parent_spacing_for_copy = parent_spacing.clone();
        rsx! {
            div { class: "flex gap-3 h-full min-h-0 overflow-hidden",
                div { class: "w-[280px] shrink-0 rounded border border-zinc-800 bg-zinc-900/70 flex flex-col min-h-0",
                    div { class: "px-3 py-2 border-b border-zinc-800 flex items-center justify-between",
                        h3 { class: "text-sm font-semibold text-zinc-200", "Layers" }
                        span { class: "text-[10px] uppercase tracking-wide text-zinc-500", "{layer_rows.len()} nodes" }
                    }
                    div { class: "overflow-auto p-2 space-y-0.5",
                        for row in layer_rows {
                            {
                                let node_id = row.id;
                                let frame_idx = frame_option_index_by_id(&options, node_id);
                                let is_selected = selected_layer_id
                                    .map(|id| id == node_id)
                                    .unwrap_or(node_id == render_root);
                                let is_hovered = hovered_layer_id.map(|id| id == node_id).unwrap_or(false);
                                rsx! {
                                    Button {
                                        key: "{node_id}",
                                        variant: if is_selected {
                                            ButtonVariant::Primary
                                        } else {
                                            ButtonVariant::Outline
                                        },
                                        size: ButtonSize::Small,
                                        class: if is_selected {
                                            "w-full justify-start text-left rounded px-2 py-1 text-xs bg-blue-500/20 text-blue-200 border-blue-500/40"
                                        } else if is_hovered {
                                            "w-full justify-start text-left rounded px-2 py-1 text-xs text-zinc-100 bg-zinc-700/50 border-zinc-500/40"
                                        } else {
                                            "w-full justify-start text-left rounded px-2 py-1 text-xs text-zinc-300 hover:bg-zinc-800/70 border-transparent"
                                        },
                                        style: "padding-left: {8 + row.depth * 16}px;",
                                        on_click: move |_| {
                                            selected_layer.set(Some(node_id));
                                            if let Some(idx) = frame_idx {
                                                selected_frame.set(idx);
                                            }
                                        },
                                        span { class: "inline-block w-7 text-zinc-500 font-mono", "{row.icon}" }
                                        span { class: "text-zinc-100", "{row.name}" }
                                        span { class: "text-zinc-500 ml-1", "{row.figma_type}" }
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "flex-1 min-w-0 min-h-0 flex flex-col",
                    if !options.is_empty() {
                        div { class: "mb-3 flex items-center gap-2 flex-wrap shrink-0",
                            label { class: "text-xs text-zinc-400", "Frame:" }
                            Dropdown {
                                DropdownTrigger {
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        size: ButtonSize::Small,
                                        class: "justify-between min-w-[240px]".to_string(),
                                        "{selected_frame_label}"
                                    }
                                }
                                DropdownContent {
                                    width: "w-80".to_string(),
                                    for (idx, option) in options.iter().enumerate() {
                                        {
                                            let next_idx = idx;
                                            let option_id = option.id;
                                            rsx! {
                                                DropdownItem {
                                                    value: next_idx.to_string(),
                                                    index: next_idx,
                                                    on_select: move |_value: String| {
                                                        selected_frame.set(next_idx);
                                                        selected_layer.set(Some(option_id));
                                                    },
                                                    "{option.label}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                    let bounds = preview_bounds();
                                    if bounds.is_valid() {
                                        let pad = 16.0 * bounds.dpr.max(1.0);
                                        let content_w = (content_bounds.max_x - content_bounds.min_x).max(1.0);
                                        let content_h = (content_bounds.max_y - content_bounds.min_y).max(1.0);
                                        let fit_x = ((bounds.width - pad * 2.0) / content_w).max(0.05);
                                        let fit_y = ((bounds.height - pad * 2.0) / content_h).max(0.05);
                                        view_zoom.set(fit_x.min(fit_y).clamp(0.05, 8.0));
                                        pan_x.set(0.0);
                                        pan_y.set(0.0);
                                    }
                                },
                                "Fit"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                    view_zoom.set(1.0);
                                    pan_x.set(0.0);
                                    pan_y.set(0.0);
                                },
                                "Reset"
                            }
                            span {
                                class: "text-[11px] text-zinc-500",
                                "zoom "
                                {format!("{:.2}x", view_zoom())}
                            }
                        }
                    }
                    div {
                        id: "frame-preview-surface",
                        class: "w-full flex-1 min-h-0 rounded border border-zinc-800 relative overflow-hidden",
                        style: format!("background: transparent; cursor: {};", canvas_cursor()),
                        onmousedown: move |evt| {
                            evt.prevent_default();
                            is_panning.set(true);
                            pan_start_mouse.set((evt.client_coordinates().x, evt.client_coordinates().y));
                            pan_start_offset.set((pan_x(), pan_y()));
                        },
                        onmousemove: move |evt| {
                            if let Some(drag) = edit_drag() {
                                let mx = evt.client_coordinates().x;
                                let my = evt.client_coordinates().y;
                                let dx = (mx - drag.start_mouse.0) * preview_bounds().dpr;
                                let dy = (my - drag.start_mouse.1) * preview_bounds().dpr;
                                let mut next = drag.start_rect;
                                match drag.mode {
                                    EditDragMode::Move => {
                                        next.x = drag.start_rect.x + dx / view_zoom().max(0.0001);
                                        next.y = drag.start_rect.y + dy / view_zoom().max(0.0001);
                                    }
                                    EditDragMode::Resize(handle) => {
                                        let zx = view_zoom().max(0.0001);
                                        apply_resize_handle(&mut next, handle, dx / zx, dy / zx);
                                    }
                                }
                                node_overrides.with_mut(|map| {
                                    map.insert(drag.node_id, NodeLayoutOverride::from_rect(next));
                                });
                                return;
                            }
                            if is_panning() {
                                let (sx, sy) = pan_start_mouse();
                                let (spx, spy) = pan_start_offset();
                                let dx = evt.client_coordinates().x - sx;
                                let dy = evt.client_coordinates().y - sy;
                                pan_x.set(spx + dx * preview_bounds().dpr);
                                pan_y.set(spy + dy * preview_bounds().dpr);
                            } else {
                                let world = client_to_world(
                                    evt.client_coordinates().x,
                                    evt.client_coordinates().y,
                                    preview_bounds(),
                                    content_bounds,
                                    view_zoom(),
                                    pan_x(),
                                    pan_y(),
                                );
                                let hit_stack =
                                    hit_test_node_stack(&doc_for_hover, &hover_primitives, world.0, world.1);
                                hovered_layer.set(hit_stack.first().copied());

                                if let Some(overlay) = overlay_rect {
                                    if let Some(handle) = detect_resize_handle_css(
                                        overlay,
                                        evt.client_coordinates().x,
                                        evt.client_coordinates().y,
                                    ) {
                                        canvas_cursor.set(resize_cursor(handle).to_string());
                                    } else if overlay_contains_css(
                                        overlay,
                                        evt.client_coordinates().x,
                                        evt.client_coordinates().y,
                                    ) {
                                        canvas_cursor.set("move".to_string());
                                    } else {
                                        canvas_cursor.set("default".to_string());
                                    }
                                } else {
                                    canvas_cursor.set("default".to_string());
                                }
                            }
                        },
                        onmouseup: move |_| {
                            is_panning.set(false);
                            edit_drag.set(None);
                            canvas_cursor.set("default".to_string());
                        },
                        onmouseleave: move |_| {
                            is_panning.set(false);
                            edit_drag.set(None);
                            hovered_layer.set(None);
                            canvas_cursor.set("default".to_string());
                        },
                        onwheel: move |evt| {
                            evt.prevent_default();
                            let delta = evt.delta();
                            let (raw_dx, raw_dy) = match delta {
                                WheelDelta::Pixels(p) => (p.x, p.y),
                                WheelDelta::Lines(l) => (l.x * 16.0, l.y * 16.0),
                                WheelDelta::Pages(p) => (p.x * 160.0, p.y * 160.0),
                            };
                            let modifiers = evt.modifiers();
                            let is_ctrl = modifiers.contains(keyboard_types::Modifiers::CONTROL)
                                || modifiers.contains(keyboard_types::Modifiers::META);
                            if is_ctrl {
                                let old_zoom = view_zoom();
                                let factor = if raw_dy < 0.0 { 1.08 } else { 1.0 / 1.08 };
                                view_zoom.set((old_zoom * factor).clamp(0.05, 16.0));
                            } else {
                                pan_x.set(pan_x() - raw_dx * preview_bounds().dpr);
                                pan_y.set(pan_y() - raw_dy * preview_bounds().dpr);
                            }
                        },
                        onclick: move |evt| {
                            let world = client_to_world(
                                evt.client_coordinates().x,
                                evt.client_coordinates().y,
                                preview_bounds(),
                                content_bounds,
                                view_zoom(),
                                pan_x(),
                                pan_y(),
                            );
                            let hit_stack =
                                hit_test_node_stack(&doc_for_click, &click_primitives, world.0, world.1);
                            if let Some(hit) = hit_stack.first().copied() {
                                selected_layer.set(Some(hit));
                                deep_select_state.set(None);
                                if let Some(idx) = frame_option_index_by_id(&options_for_click, hit) {
                                    selected_frame.set(idx);
                                }
                            }
                        },
                        ondoubleclick: move |evt| {
                            let world = client_to_world(
                                evt.client_coordinates().x,
                                evt.client_coordinates().y,
                                preview_bounds(),
                                content_bounds,
                                view_zoom(),
                                pan_x(),
                                pan_y(),
                            );
                            let hit_stack =
                                hit_test_node_stack(&doc_for_dblclick, &dblclick_primitives, world.0, world.1);
                            if hit_stack.is_empty() {
                                deep_select_state.set(None);
                                return;
                            }
                            let mut next_index = 0usize;
                            let reuse_previous = deep_select_state()
                                .as_ref()
                                .map(|prev| prev.matches_cursor(world.0, world.1) && prev.stack == hit_stack)
                                .unwrap_or(false);
                            if reuse_previous {
                                if let Some(prev) = deep_select_state() {
                                    next_index = (prev.index + 1).min(hit_stack.len().saturating_sub(1));
                                }
                            } else if let Some(sel) = selected_layer() {
                                if let Some(idx) = hit_stack.iter().position(|id| *id == sel) {
                                    next_index = idx.min(hit_stack.len().saturating_sub(1));
                                }
                            }
                            let next = hit_stack[next_index];
                            selected_layer.set(Some(next));
                            deep_select_state.set(Some(DeepSelectState {
                                world_x: world.0,
                                world_y: world.1,
                                stack: hit_stack,
                                index: next_index,
                            }));
                            if let Some(idx) = frame_option_index_by_id(&options_for_dblclick, next) {
                                selected_frame.set(idx);
                            }
                        },

                        if let Some(overlay) = overlay_rect {
                            div {
                                class: "absolute pointer-events-auto",
                                style: "left:{overlay.x}px; top:{overlay.y}px; width:{overlay.width}px; height:{overlay.height}px;",
                                onmousedown: move |evt| {
                                    evt.stop_propagation();
                                    evt.prevent_default();
                                    if let Some(world) = selected_world_bounds {
                                        record_undo_snapshot(
                                            &mut undo_stack,
                                            &mut redo_stack,
                                            &mut undo_history,
                                            &frame_doc,
                                            &node_overrides,
                                            "Drag transform",
                                        );
                                        let handle = detect_resize_handle_css(
                                            overlay,
                                            evt.client_coordinates().x,
                                            evt.client_coordinates().y,
                                        );
                                        let drag = EditDragState {
                                            node_id: active_node,
                                            start_mouse: (evt.client_coordinates().x, evt.client_coordinates().y),
                                            start_rect: world.into(),
                                            mode: handle.map(EditDragMode::Resize).unwrap_or(EditDragMode::Move),
                                        };
                                        edit_drag.set(Some(drag));
                                    }
                                },
                            }
                        }

                        for (idx, guide) in spacing_guides_overlay.iter().enumerate() {
                            {
                                let (lx, ly) = guide_label_position_css(
                                    *guide,
                                    content_bounds,
                                    view_zoom(),
                                    pan_x(),
                                    pan_y(),
                                    preview_bounds().dpr,
                                );
                                let distance = spacing_guide_distance(*guide);
                                rsx! {
                                    div {
                                        key: "spacing-label-{idx}",
                                        class: "absolute pointer-events-none text-[10px] font-semibold text-pink-200 bg-pink-900/70 border border-pink-500/60 rounded px-1",
                                        style: "left:{lx}px; top:{ly}px; transform: translate(-50%, -50%);",
                                        "{distance.round() as i64}"
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "w-[280px] shrink-0 rounded border border-zinc-800 bg-zinc-900/70 flex flex-col min-h-0",
                    div { class: "px-3 py-2 border-b border-zinc-800 flex items-center justify-between gap-2",
                        h3 { class: "text-sm font-semibold text-zinc-200", "Inspector" }
                        div { class: "flex items-center gap-2",
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                    undo_last_edit(
                                        &mut undo_stack,
                                        &mut redo_stack,
                                        &mut undo_history,
                                        &mut frame_doc,
                                        &mut node_overrides,
                                    );
                                },
                                class: "h-7",
                                "Undo"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                    redo_last_edit(
                                        &mut undo_stack,
                                        &mut redo_stack,
                                        &mut undo_history,
                                        &mut frame_doc,
                                        &mut node_overrides,
                                    );
                                },
                                class: "h-7",
                                "Redo"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                let snapshot = inspector_debug_snapshot(
                                    &doc_for_copy,
                                    active_node,
                                    active_projection_for_copy.as_ref(),
                                    parent_projection_for_copy.as_ref(),
                                    selected_world_bounds,
                                    &parent_spacing_for_copy,
                                );
                                if let Ok(js_literal) = serde_json::to_string(&snapshot) {
                                    let script = format!(
                                        "(async () => {{ try {{ await navigator.clipboard.writeText({}); }} catch (_) {{}} }})()",
                                        js_literal
                                    );
                                    spawn(async move {
                                        let _ = document::eval(&script).await;
                                    });
                                }
                                },
                                class: "h-7",
                                "Copy"
                            }
                        }
                    }
                    div { class: "p-3 overflow-auto text-xs text-zinc-300 space-y-3",
                        if let Some(node) = doc.get_node(active_node) {
                            div { class: "space-y-1",
                                p { class: "text-zinc-100 font-medium", "{node.name}" }
                                p { class: "text-zinc-500", "{node.figma_type}" }
                                p { class: "text-zinc-500 font-mono break-all", "{node.figma_id}" }
                            }
                        }
                        if let Some(ref proj) = active_projection {
                            div { class: "space-y-2 rounded border border-zinc-800 bg-zinc-900/60 p-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-400", "Position" }
                                div { class: "space-y-1",
                                    p { class: "text-[10px] uppercase tracking-wide text-zinc-500", "Alignment" }
                                    div { class: "flex items-center gap-2",
                                        div { class: "rounded bg-zinc-800 border border-zinc-700 p-1 grid grid-cols-3 gap-1",
                                            Button {
                                                variant: if h_constraint == "LEFT" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint H: LEFT",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "horizontal", "LEFT");
                                                        }
                                                    });
                                                },
                                                "L"
                                            }
                                            Button {
                                                variant: if h_constraint == "CENTER" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint H: CENTER",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "horizontal", "CENTER");
                                                        }
                                                    });
                                                },
                                                "C"
                                            }
                                            Button {
                                                variant: if h_constraint == "RIGHT" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint H: RIGHT",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "horizontal", "RIGHT");
                                                        }
                                                    });
                                                },
                                                "R"
                                            }
                                        }
                                        div { class: "rounded bg-zinc-800 border border-zinc-700 p-1 grid grid-cols-3 gap-1",
                                            Button {
                                                variant: if v_constraint == "TOP" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint V: TOP",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "vertical", "TOP");
                                                        }
                                                    });
                                                },
                                                "T"
                                            }
                                            Button {
                                                variant: if v_constraint == "CENTER" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint V: CENTER",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "vertical", "CENTER");
                                                        }
                                                    });
                                                },
                                                "M"
                                            }
                                            Button {
                                                variant: if v_constraint == "BOTTOM" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                                size: ButtonSize::Small,
                                                on_click: move |_| {
                                                    record_undo_snapshot(
                                                        &mut undo_stack,
                                                        &mut redo_stack,
                                                        &mut undo_history,
                                                        &frame_doc,
                                                        &node_overrides,
                                                        "Constraint V: BOTTOM",
                                                    );
                                                    frame_doc.with_mut(|doc_opt| {
                                                        if let Some(doc) = doc_opt.as_mut() {
                                                            set_node_constraint(doc, active_node, "vertical", "BOTTOM");
                                                        }
                                                    });
                                                },
                                                "B"
                                            }
                                        }
                                        Dropdown {
                                            DropdownTrigger {
                                                Button {
                                                    variant: ButtonVariant::Outline,
                                                    size: ButtonSize::Small,
                                                    "⋯"
                                                }
                                            }
                                            DropdownContent {
                                                width: "w-56".to_string(),
                                                DropdownItem {
                                                    value: "tidy".to_string(),
                                                    index: 0,
                                                    on_select: move |_: String| {
                                                        record_undo_snapshot(
                                                            &mut undo_stack,
                                                            &mut redo_stack,
                                                            &mut undo_history,
                                                            &frame_doc,
                                                            &node_overrides,
                                                            "Tidy up",
                                                        );
                                                        node_overrides.with_mut(|overrides| {
                                                            tidy_up_parent_children(&doc_for_tidy, &node_bounds_for_tidy, overrides, active_node);
                                                        });
                                                    },
                                                    "Tidy Up"
                                                }
                                                DropdownItem {
                                                    value: "dist-v".to_string(),
                                                    index: 1,
                                                    on_select: move |_: String| {
                                                        record_undo_snapshot(
                                                            &mut undo_stack,
                                                            &mut redo_stack,
                                                            &mut undo_history,
                                                            &frame_doc,
                                                            &node_overrides,
                                                            "Distribute vertical spacing",
                                                        );
                                                        node_overrides.with_mut(|overrides| {
                                                            distribute_parent_children(&doc_for_distribute_v, &node_bounds_for_distribute_v, overrides, active_node, Axis::Vertical);
                                                        });
                                                    },
                                                    "Distribute Vertical Spacing"
                                                }
                                                DropdownItem {
                                                    value: "dist-h".to_string(),
                                                    index: 2,
                                                    on_select: move |_: String| {
                                                        record_undo_snapshot(
                                                            &mut undo_stack,
                                                            &mut redo_stack,
                                                            &mut undo_history,
                                                            &frame_doc,
                                                            &node_overrides,
                                                            "Distribute horizontal spacing",
                                                        );
                                                        node_overrides.with_mut(|overrides| {
                                                            distribute_parent_children(&doc_for_distribute_h, &node_bounds_for_distribute_h, overrides, active_node, Axis::Horizontal);
                                                        });
                                                    },
                                                    "Distribute Horizontal Spacing"
                                                }
                                            }
                                        }
                                    }
                                }
                                div { class: "space-y-1",
                                    p { class: "text-[10px] uppercase tracking-wide text-zinc-500", "Position" }
                                    div { class: "grid grid-cols-[1fr_1fr_auto] gap-2 items-end",
                                    NumericInspectorField {
                                        label: "X",
                                        value: selected_world_bounds.map(|b| b.min_x).unwrap_or(0.0),
                                        on_change: move |next: f64| {
                                            if let Some(base) = selected_world_bounds {
                                                record_undo_snapshot(
                                                    &mut undo_stack,
                                                    &mut redo_stack,
                                                    &mut undo_history,
                                                    &frame_doc,
                                                    &node_overrides,
                                                    "Set X",
                                                );
                                                node_overrides.with_mut(|map| {
                                                    let mut r: EditableRect = base.into();
                                                    r.x = next;
                                                    map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                                });
                                            }
                                        }
                                    }
                                    NumericInspectorField {
                                        label: "Y",
                                        value: selected_world_bounds.map(|b| b.min_y).unwrap_or(0.0),
                                        on_change: move |next: f64| {
                                            if let Some(base) = selected_world_bounds {
                                                record_undo_snapshot(
                                                    &mut undo_stack,
                                                    &mut redo_stack,
                                                    &mut undo_history,
                                                    &frame_doc,
                                                    &node_overrides,
                                                    "Set Y",
                                                );
                                                node_overrides.with_mut(|map| {
                                                    let mut r: EditableRect = base.into();
                                                    r.y = next;
                                                    map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                                });
                                            }
                                        }
                                    }
                                        Button {
                                            variant: if show_constraints_panel() { ButtonVariant::Primary } else { ButtonVariant::Outline },
                                            size: ButtonSize::Small,
                                            on_click: move |_| show_constraints_panel.set(!show_constraints_panel()),
                                            class: "h-[32px]",
                                            "↔"
                                        }
                                    }
                                }
                                div { class: "grid grid-cols-2 gap-2",
                                    NumericInspectorField {
                                        label: "W",
                                        value: selected_world_bounds.map(|b| b.max_x - b.min_x).unwrap_or(0.0),
                                        on_change: move |next: f64| {
                                            if let Some(base) = selected_world_bounds {
                                                record_undo_snapshot(
                                                    &mut undo_stack,
                                                    &mut redo_stack,
                                                    &mut undo_history,
                                                    &frame_doc,
                                                    &node_overrides,
                                                    "Set Width",
                                                );
                                                node_overrides.with_mut(|map| {
                                                    let mut r: EditableRect = base.into();
                                                    r.width = next.max(1.0);
                                                    map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                                });
                                            }
                                        }
                                    }
                                    NumericInspectorField {
                                        label: "H",
                                        value: selected_world_bounds.map(|b| b.max_y - b.min_y).unwrap_or(0.0),
                                        on_change: move |next: f64| {
                                            if let Some(base) = selected_world_bounds {
                                                record_undo_snapshot(
                                                    &mut undo_stack,
                                                    &mut redo_stack,
                                                    &mut undo_history,
                                                    &frame_doc,
                                                    &node_overrides,
                                                    "Set Height",
                                                );
                                                node_overrides.with_mut(|map| {
                                                    let mut r: EditableRect = base.into();
                                                    r.height = next.max(1.0);
                                                    map.insert(active_node, NodeLayoutOverride::from_rect(r));
                                                });
                                            }
                                        }
                                    }
                                }
                                if show_constraints_panel() {
                                    div { class: "space-y-1",
                                        p { class: "text-[10px] uppercase tracking-wide text-zinc-500", "Constraints" }
                                        div { class: "grid grid-cols-2 gap-2",
                                            div { class: "flex flex-col gap-1",
                                                span { class: "text-[10px] uppercase tracking-wide text-zinc-500", "Horizontal" }
                                                Dropdown {
                                                    DropdownTrigger {
                                                        Button {
                                                            variant: ButtonVariant::Outline,
                                                            size: ButtonSize::Small,
                                                            full_width: true,
                                                            class: "justify-between w-full".to_string(),
                                                            "{h_constraint}"
                                                        }
                                                    }
                                                    DropdownContent {
                                                        width: "w-44".to_string(),
                                                        DropdownItem {
                                                            value: "LEFT".to_string(),
                                                            index: 0,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "horizontal", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Left"
                                                        }
                                                        DropdownItem {
                                                            value: "CENTER".to_string(),
                                                            index: 1,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "horizontal", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Center"
                                                        }
                                                        DropdownItem {
                                                            value: "RIGHT".to_string(),
                                                            index: 2,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "horizontal", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Right"
                                                        }
                                                        DropdownItem {
                                                            value: "SCALE".to_string(),
                                                            index: 3,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "horizontal", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Scale"
                                                        }
                                                        DropdownItem {
                                                            value: "STRETCH".to_string(),
                                                            index: 4,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "horizontal", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Stretch"
                                                        }
                                                    }
                                                }
                                            }
                                            div { class: "flex flex-col gap-1",
                                                span { class: "text-[10px] uppercase tracking-wide text-zinc-500", "Vertical" }
                                                Dropdown {
                                                    DropdownTrigger {
                                                        Button {
                                                            variant: ButtonVariant::Outline,
                                                            size: ButtonSize::Small,
                                                            full_width: true,
                                                            class: "justify-between w-full".to_string(),
                                                            "{v_constraint}"
                                                        }
                                                    }
                                                    DropdownContent {
                                                        width: "w-44".to_string(),
                                                        DropdownItem {
                                                            value: "TOP".to_string(),
                                                            index: 0,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "vertical", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Top"
                                                        }
                                                        DropdownItem {
                                                            value: "CENTER".to_string(),
                                                            index: 1,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "vertical", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Center"
                                                        }
                                                        DropdownItem {
                                                            value: "BOTTOM".to_string(),
                                                            index: 2,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "vertical", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Bottom"
                                                        }
                                                        DropdownItem {
                                                            value: "SCALE".to_string(),
                                                            index: 3,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "vertical", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Scale"
                                                        }
                                                        DropdownItem {
                                                            value: "STRETCH".to_string(),
                                                            index: 4,
                                                            on_select: move |value: String| {
                                                                frame_doc.with_mut(|doc_opt| {
                                                                    if let Some(doc) = doc_opt.as_mut() {
                                                                        set_node_constraint(doc, active_node, "vertical", value.as_str());
                                                                    }
                                                                });
                                                            },
                                                            "Stretch"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                div { class: "grid grid-cols-2 gap-2",
                                    NumericInspectorField {
                                        label: "Rotation",
                                        value: rotation_value,
                                        on_change: move |next: f64| {
                                            frame_doc.with_mut(|doc_opt| {
                                                if let Some(doc) = doc_opt.as_mut() {
                                                    set_node_rotation(doc, active_node, next);
                                                }
                                            });
                                        }
                                    }
                                    div { class: "rounded bg-zinc-800 border border-zinc-700 p-1 grid grid-cols-4 gap-1",
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            size: ButtonSize::Small,
                                            on_click: move |_| {
                                                frame_doc.with_mut(|doc_opt| {
                                                    if let Some(doc) = doc_opt.as_mut() {
                                                        set_node_rotation(doc, active_node, rotation_value + 90.0);
                                                    }
                                                });
                                            },
                                            "⟳"
                                        }
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            size: ButtonSize::Small,
                                            on_click: move |_| {
                                                frame_doc.with_mut(|doc_opt| {
                                                    if let Some(doc) = doc_opt.as_mut() {
                                                        toggle_node_flag(doc, active_node, "flipHorizontal");
                                                    }
                                                });
                                            },
                                            "↔"
                                        }
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            size: ButtonSize::Small,
                                            on_click: move |_| {
                                                frame_doc.with_mut(|doc_opt| {
                                                    if let Some(doc) = doc_opt.as_mut() {
                                                        toggle_node_flag(doc, active_node, "flipVertical");
                                                    }
                                                });
                                            },
                                            "↕"
                                        }
                                        Dropdown {
                                            DropdownTrigger {
                                                Button {
                                                    variant: ButtonVariant::Outline,
                                                    size: ButtonSize::Small,
                                                    "⋯"
                                                }
                                            }
                                            DropdownContent {
                                                width: "w-56".to_string(),
                                                DropdownItem {
                                                    value: "rot90".to_string(),
                                                    index: 0,
                                                    on_select: move |_: String| {
                                                        frame_doc.with_mut(|doc_opt| {
                                                            if let Some(doc) = doc_opt.as_mut() {
                                                                set_node_rotation(doc, active_node, rotation_value + 90.0);
                                                            }
                                                        });
                                                    },
                                                    "Rotate 90 degrees right"
                                                }
                                                DropdownItem {
                                                    value: "flip-h".to_string(),
                                                    index: 1,
                                                    on_select: move |_: String| {
                                                        frame_doc.with_mut(|doc_opt| {
                                                            if let Some(doc) = doc_opt.as_mut() {
                                                                toggle_node_flag(doc, active_node, "flipHorizontal");
                                                            }
                                                        });
                                                    },
                                                    "Flip Horizontal"
                                                }
                                                DropdownItem {
                                                    value: "flip-v".to_string(),
                                                    index: 2,
                                                    on_select: move |_: String| {
                                                        frame_doc.with_mut(|doc_opt| {
                                                            if let Some(doc) = doc_opt.as_mut() {
                                                                toggle_node_flag(doc, active_node, "flipVertical");
                                                            }
                                                        });
                                                    },
                                                    "Flip Vertical"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            p { class: "text-zinc-500", "Class: " {format!("{:?}", proj.class)} }

                            div { class: "pt-2 border-t border-zinc-800 space-y-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-500", "Layout" }
                                InspectorValueGrid {
                                    rows: vec![
                                        (
                                            "Size".to_string(),
                                            proj.size
                                                .as_ref()
                                                .map(|v| format!("{:.1} x {:.1}", v.x, v.y))
                                                .unwrap_or_else(|| "n/a".to_string()),
                                        ),
                                        ("Opacity".to_string(), proj.opacity.map(|v| format!("{:.3}", v)).unwrap_or_else(|| "n/a".to_string())),
                                        ("Blend".to_string(), proj.blend_mode.as_ref().map(|v| format!("{v:?}")).unwrap_or_else(|| "n/a".to_string())),
                                        ("Visible".to_string(), if proj.visible { "true".to_string() } else { "false".to_string() }),
                                        ("Locked".to_string(), if proj.locked { "true".to_string() } else { "false".to_string() }),
                                        ("Clips".to_string(), doc
                                            .get_node(active_node)
                                            .and_then(|n| n.raw.get("clipsContent"))
                                            .map(raw_value_to_compact)
                                            .unwrap_or_else(|| "n/a".to_string())),
                                    ]
                                }
                            }

                            div { class: "pt-2 border-t border-zinc-800 space-y-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-500", "Auto Layout (Node)" }
                                InspectorValueGrid {
                                    rows: auto_layout_rows(proj.auto_layout.as_ref())
                                }
                            }

                            div { class: "pt-2 border-t border-zinc-800 space-y-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-500", "Auto Layout (Parent Context)" }
                                InspectorValueGrid {
                                    rows: parent_auto_layout_rows(parent_projection.as_ref())
                                }
                            }

                            div { class: "pt-2 border-t border-zinc-800 space-y-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-500", "Measured Spacing" }
                                InspectorValueGrid {
                                    rows: vec![
                                        (
                                            "Prev sibling".to_string(),
                                            parent_spacing.prev_gap.map(|v| format!("{:.1}px", v)).unwrap_or_else(|| "n/a".to_string()),
                                        ),
                                        (
                                            "Next sibling".to_string(),
                                            parent_spacing.next_gap.map(|v| format!("{:.1}px", v)).unwrap_or_else(|| "n/a".to_string()),
                                        ),
                                        ("Axis".to_string(), parent_spacing.axis.clone()),
                                    ]
                                }
                            }

                            div { class: "pt-2 border-t border-zinc-800 space-y-2",
                                p { class: "text-[11px] uppercase tracking-wide text-zinc-500", "Undo History" }
                                div { class: "max-h-32 overflow-auto rounded bg-zinc-950/60 border border-zinc-800 p-2 space-y-1" ,
                                    for (idx, item) in undo_history().iter().rev().take(12).enumerate() {
                                        p {
                                            key: "undo-history-{idx}",
                                            class: "text-[11px] text-zinc-400",
                                            "{item}"
                                        }
                                    }
                                    if undo_history().is_empty() {
                                        p { class: "text-[11px] text-zinc-500", "No edits yet." }
                                    }
                                }
                            }

                        } else {
                            p { class: "text-zinc-500", "Select a node to inspect." }
                        }
                    }
                }
            }
        }
    } else if let Some(err) = frame_error() {
        rsx! {
            div { class: "rounded border border-red-800 bg-red-950/50 text-red-200 p-3 text-sm",
                "Import failed: {err}"
            }
        }
    } else {
        rsx! {
            div { class: "rounded border border-zinc-800 bg-zinc-950/50 text-zinc-300 p-3 text-sm",
                "Waiting for bridge payload at {FRAME_BRIDGE_JSON_PATH}..."
            }
        }
    };
    let render_diag_summary = frame_doc()
        .as_ref()
        .map(|doc| {
            let options = collect_frame_options(doc);
            let selected_idx = selected_frame().min(options.len().saturating_sub(1));
            let render_root = options
                .get(selected_idx)
                .map(|o| o.id)
                .unwrap_or_else(|| preview_scene_root(doc));
            collect_render_diagnostics(doc, render_root).format_summary()
        })
        .unwrap_or_else(|| "Render diag: waiting for document".to_string());

    rsx! {
        div {
            id: "frame-preview-root",
            class: "h-full p-4 overflow-hidden flex flex-col min-h-0",
            tabindex: "0",
            onmousedown: move |_| {
                spawn(async move {
                    let _ = document::eval(
                        "document.getElementById('frame-preview-root')?.focus();"
                    )
                    .await;
                });
            },
            onkeydown: move |evt| {
                let modifiers = evt.modifiers();
                let is_ctrl = modifiers.contains(keyboard_types::Modifiers::CONTROL)
                    || modifiers.contains(keyboard_types::Modifiers::META);
                let is_shift = modifiers.contains(keyboard_types::Modifiers::SHIFT);
                let key = evt.key().to_string().to_lowercase();
                if is_ctrl && is_shift && key == "z" {
                    evt.prevent_default();
                    redo_last_edit(
                        &mut undo_stack,
                        &mut redo_stack,
                        &mut undo_history,
                        &mut frame_doc,
                        &mut node_overrides,
                    );
                } else if is_ctrl && (key == "y") {
                    evt.prevent_default();
                    redo_last_edit(
                        &mut undo_stack,
                        &mut redo_stack,
                        &mut undo_history,
                        &mut frame_doc,
                        &mut node_overrides,
                    );
                } else if is_ctrl && key == "z" {
                    evt.prevent_default();
                    undo_last_edit(
                        &mut undo_stack,
                        &mut redo_stack,
                        &mut undo_history,
                        &mut frame_doc,
                        &mut node_overrides,
                    );
                } else if !is_ctrl
                    && (key == "arrowup"
                        || key == "arrowdown"
                        || key == "arrowleft"
                        || key == "arrowright")
                {
                    let Some(node_id) = selected_layer() else {
                        return;
                    };
                    let bounds_map = cached_node_bounds();
                    let Some(base_bounds) = bounds_map.get(&node_id).copied() else {
                        return;
                    };

                    let step = if is_shift { 10.0 } else { 1.0 };
                    let (dx, dy) = match key.as_str() {
                        "arrowup" => (0.0, -step),
                        "arrowdown" => (0.0, step),
                        "arrowleft" => (-step, 0.0),
                        "arrowright" => (step, 0.0),
                        _ => (0.0, 0.0),
                    };
                    if dx == 0.0 && dy == 0.0 {
                        return;
                    }

                    evt.prevent_default();
                    record_undo_snapshot(
                        &mut undo_stack,
                        &mut redo_stack,
                        &mut undo_history,
                        &frame_doc,
                        &node_overrides,
                        "Nudge node",
                    );
                    node_overrides.with_mut(|overrides| {
                        let mut current = overrides
                            .get(&node_id)
                            .copied()
                            .unwrap_or(NodeLayoutOverride {
                                x: base_bounds.min_x,
                                y: base_bounds.min_y,
                                width: (base_bounds.max_x - base_bounds.min_x).max(1.0),
                                height: (base_bounds.max_y - base_bounds.min_y).max(1.0),
                            });
                        current.x += dx;
                        current.y += dy;
                        overrides.insert(node_id, current);
                    });
                }
            },
            h2 { class: "text-lg font-semibold text-zinc-300 mb-3 shrink-0", "Frame .fig Preview" }
            p { class: "text-xs text-zinc-500 mb-2 shrink-0", "{frame_status()}" }
            p { class: "text-[11px] text-zinc-400 mb-2 shrink-0", "{perf_stats().format()}" }
            p { class: "text-[11px] text-zinc-500 mb-2 shrink-0", "{render_diag_summary} (* = currently approximated)" }
            div { class: "flex-1 min-h-0",
                {frame_content}
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct FramePerfStats {
    frame_ms: f64,
    fps: f64,
    avg_frame_ms: f64,
    avg_fps: f64,
    budget_hit_ratio: f64,
    primitive_count: usize,
    samples: u64,
}

impl FramePerfStats {
    fn update(&mut self, frame_ms: f64, fps: f64, primitive_count: usize) {
        const EWMA_ALPHA: f64 = 0.12;
        const TARGET_FRAME_MS: f64 = 8.33;
        self.frame_ms = frame_ms;
        self.fps = fps;
        self.primitive_count = primitive_count;
        if self.samples == 0 {
            self.avg_frame_ms = frame_ms;
            self.avg_fps = fps;
            self.budget_hit_ratio = if frame_ms <= TARGET_FRAME_MS {
                1.0
            } else {
                0.0
            };
        } else {
            self.avg_frame_ms = (frame_ms * EWMA_ALPHA) + (self.avg_frame_ms * (1.0 - EWMA_ALPHA));
            self.avg_fps = (fps * EWMA_ALPHA) + (self.avg_fps * (1.0 - EWMA_ALPHA));
            let hit = if frame_ms <= TARGET_FRAME_MS {
                1.0
            } else {
                0.0
            };
            self.budget_hit_ratio =
                (hit * EWMA_ALPHA) + (self.budget_hit_ratio * (1.0 - EWMA_ALPHA));
        }
        self.samples = self.samples.saturating_add(1);
    }

    fn format(&self) -> String {
        if self.frame_ms <= 0.0 {
            return "Perf: waiting for frames".to_string();
        }
        let budget_pct = (self.budget_hit_ratio * 100.0).clamp(0.0, 100.0);
        format!(
            "Perf: {:.2} ms ({:.1} FPS) | avg {:.2} ms ({:.1} FPS) | <=8.33ms {:.0}% | primitives {}",
            self.frame_ms,
            self.fps,
            self.avg_frame_ms,
            self.avg_fps,
            budget_pct,
            self.primitive_count
        )
    }
}

#[cfg(feature = "desktop")]
#[derive(Debug, Clone, Copy, Default)]
struct FramePreviewBounds {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
    dpr: f64,
}

#[cfg(feature = "desktop")]
impl FramePreviewBounds {
    fn is_valid(&self) -> bool {
        self.width > 0.0 && self.height > 0.0
    }
}

#[cfg(feature = "desktop")]
#[derive(Debug, Clone, Copy, Default)]
struct PrimitiveBounds {
    min_x: f64,
    min_y: f64,
    max_x: f64,
    max_y: f64,
}

impl PrimitiveBounds {
    fn contains(&self, x: f64, y: f64) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }
}

#[cfg(feature = "desktop")]
fn primitive_bounds(primitives: &[frame_ui::PaintPrimitive]) -> PrimitiveBounds {
    let mut out = PrimitiveBounds {
        min_x: f64::INFINITY,
        min_y: f64::INFINITY,
        max_x: f64::NEG_INFINITY,
        max_y: f64::NEG_INFINITY,
    };

    for p in primitives {
        match p {
            frame_ui::PaintPrimitive::LayerStart {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::ClipStart {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::Rect {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::Path {
                x,
                y,
                width,
                height,
                ..
            } => {
                out.min_x = out.min_x.min(*x);
                out.min_y = out.min_y.min(*y);
                out.max_x = out.max_x.max(*x + *width);
                out.max_y = out.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::LayerEnd { .. }
            | frame_ui::PaintPrimitive::ClipEnd { .. } => {}
            frame_ui::PaintPrimitive::Text {
                x,
                y,
                text,
                font_size,
                ..
            } => {
                let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
                out.min_x = out.min_x.min(*x);
                out.min_y = out.min_y.min(*y);
                out.max_x = out.max_x.max(*x + width);
                out.max_y = out.max_y.max(*y + *font_size * 1.2);
            }
        }
    }

    if !out.min_x.is_finite()
        || !out.min_y.is_finite()
        || !out.max_x.is_finite()
        || !out.max_y.is_finite()
    {
        return PrimitiveBounds {
            min_x: 0.0,
            min_y: 0.0,
            max_x: 1024.0,
            max_y: 768.0,
        };
    }

    out
}

fn primitive_world_bounds(primitive: &frame_ui::PaintPrimitive) -> PrimitiveBounds {
    match primitive {
        frame_ui::PaintPrimitive::LayerStart {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::ClipStart {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::Rect {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::Path {
            x,
            y,
            width,
            height,
            ..
        } => PrimitiveBounds {
            min_x: *x,
            min_y: *y,
            max_x: *x + *width,
            max_y: *y + *height,
        },
        frame_ui::PaintPrimitive::LayerEnd { .. } | frame_ui::PaintPrimitive::ClipEnd { .. } => {
            PrimitiveBounds::default()
        }
        frame_ui::PaintPrimitive::Text {
            x,
            y,
            text,
            font_size,
            ..
        } => {
            let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
            PrimitiveBounds {
                min_x: *x,
                min_y: *y,
                max_x: *x + width,
                max_y: *y + *font_size * 1.2,
            }
        }
    }
}

fn client_to_world(
    client_x_css: f64,
    client_y_css: f64,
    preview: FramePreviewBounds,
    content: PrimitiveBounds,
    zoom: f64,
    pan_x: f64,
    pan_y: f64,
) -> (f64, f64) {
    let dpr = preview.dpr.max(1.0);
    let pad = 16.0 * dpr;
    let local_x = client_x_css * dpr - preview.x;
    let local_y = client_y_css * dpr - preview.y;
    let world_x = content.min_x + (local_x - pad - pan_x) / zoom.max(0.0001);
    let world_y = content.min_y + (local_y - pad - pan_y) / zoom.max(0.0001);
    (world_x, world_y)
}

fn world_to_panel_css_x(
    world_x: f64,
    content: PrimitiveBounds,
    zoom: f64,
    pan_x: f64,
    dpr: f64,
) -> f64 {
    let pad = 16.0 * dpr.max(1.0);
    (pad + pan_x + (world_x - content.min_x) * zoom) / dpr.max(1.0)
}

fn world_to_panel_css_y(
    world_y: f64,
    content: PrimitiveBounds,
    zoom: f64,
    pan_y: f64,
    dpr: f64,
) -> f64 {
    let pad = 16.0 * dpr.max(1.0);
    (pad + pan_y + (world_y - content.min_y) * zoom) / dpr.max(1.0)
}

fn hit_test_node_stack(
    doc: &FrameDocument,
    primitives: &[frame_ui::PaintPrimitive],
    world_x: f64,
    world_y: f64,
) -> Vec<NodeId> {
    let mut seen = std::collections::HashSet::<NodeId>::new();
    let mut candidates: Vec<(NodeId, usize, u8)> = Vec::new();
    for (z_index, primitive) in primitives.iter().rev().enumerate() {
        let Some(hit_kind) = primitive_hit_kind(primitive, world_x, world_y) else {
            continue;
        };
        let node_id = primitive_node_id(primitive);
        let Some(node) = doc.get_node(node_id) else {
            continue;
        };
        // Pointer selection ignores hidden/locked nodes like Figma.
        if !node.visible || node.locked {
            continue;
        }
        if seen.insert(node_id) {
            let priority = node_hit_priority(node.figma_type.as_str(), hit_kind);
            candidates.push((node_id, z_index, priority));
        }
    }
    candidates.sort_by(|a, b| b.2.cmp(&a.2).then_with(|| a.1.cmp(&b.1)));
    candidates.into_iter().map(|c| c.0).collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HitKind {
    Shape,
    TextGlyph,
}

fn primitive_hit_kind(
    primitive: &frame_ui::PaintPrimitive,
    world_x: f64,
    world_y: f64,
) -> Option<HitKind> {
    match primitive {
        frame_ui::PaintPrimitive::Text {
            x,
            y,
            text,
            font_size,
            ..
        } => {
            if text_glyph_hit(*x, *y, text, *font_size, world_x, world_y) {
                Some(HitKind::TextGlyph)
            } else {
                None
            }
        }
        _ => {
            let bounds = primitive_world_bounds(primitive);
            if bounds.contains(world_x, world_y) {
                Some(HitKind::Shape)
            } else {
                None
            }
        }
    }
}

fn text_glyph_hit(x: f64, y: f64, text: &str, font_size: f64, world_x: f64, world_y: f64) -> bool {
    let line_h = font_size * 1.2;
    let glyph_w = (font_size * 0.55).max(1.0);
    let glyph_h = (font_size * 0.9).max(1.0);
    let baseline_top = y + font_size * 0.1;

    for (idx, ch) in text.chars().enumerate() {
        if ch.is_whitespace() {
            continue;
        }
        let gx = x + glyph_w * idx as f64;
        let gy = baseline_top;
        let in_x = world_x >= gx && world_x <= gx + glyph_w;
        let in_y = world_y >= gy && world_y <= gy + glyph_h.min(line_h);
        if in_x && in_y {
            return true;
        }
    }
    false
}

fn node_hit_priority(figma_type: &str, hit_kind: HitKind) -> u8 {
    match (figma_type, hit_kind) {
        ("INSTANCE", _) => 120,
        ("COMPONENT", _) | ("COMPONENT_SET", _) => 110,
        ("TEXT", HitKind::TextGlyph) => 100,
        ("FRAME", _) | ("GROUP", _) | ("SECTION", _) => 80,
        _ => 60,
    }
}

fn spacing_guides_for_selection(
    doc: &FrameDocument,
    node_bounds: &std::collections::HashMap<NodeId, PrimitiveBounds>,
    selected: NodeId,
) -> Vec<SpacingGuide> {
    let Some(sel) = node_bounds.get(&selected).copied() else {
        return Vec::new();
    };
    let Some(node) = doc.get_node(selected) else {
        return Vec::new();
    };
    let Some(parent_id) = node.parent else {
        return Vec::new();
    };
    let Some(parent) = doc.get_node(parent_id) else {
        return Vec::new();
    };

    let mut left: Option<PrimitiveBounds> = None;
    let mut right: Option<PrimitiveBounds> = None;
    let mut top: Option<PrimitiveBounds> = None;
    let mut bottom: Option<PrimitiveBounds> = None;

    for sibling_id in &parent.children {
        if *sibling_id == selected {
            continue;
        }
        let Some(sibling_node) = doc.get_node(*sibling_id) else {
            continue;
        };
        if !sibling_node.visible || sibling_node.locked {
            continue;
        }
        let Some(b) = node_bounds.get(sibling_id).copied() else {
            continue;
        };

        let overlap_y = (sel.max_y.min(b.max_y) - sel.min_y.max(b.min_y)).max(0.0);
        let overlap_x = (sel.max_x.min(b.max_x) - sel.min_x.max(b.min_x)).max(0.0);

        if overlap_y > 0.0 && b.max_x <= sel.min_x {
            if left.map(|cur| b.max_x > cur.max_x).unwrap_or(true) {
                left = Some(b);
            }
        }
        if overlap_y > 0.0 && b.min_x >= sel.max_x {
            if right.map(|cur| b.min_x < cur.min_x).unwrap_or(true) {
                right = Some(b);
            }
        }
        if overlap_x > 0.0 && b.max_y <= sel.min_y {
            if top.map(|cur| b.max_y > cur.max_y).unwrap_or(true) {
                top = Some(b);
            }
        }
        if overlap_x > 0.0 && b.min_y >= sel.max_y {
            if bottom.map(|cur| b.min_y < cur.min_y).unwrap_or(true) {
                bottom = Some(b);
            }
        }
    }

    let mut guides = Vec::new();
    if let Some(l) = left {
        let y =
            ((sel.min_y.max(l.min_y) + sel.max_y.min(l.max_y)) * 0.5).clamp(sel.min_y, sel.max_y);
        guides.push(SpacingGuide {
            axis: Axis::Horizontal,
            x0: l.max_x,
            y0: y,
            x1: sel.min_x,
            y1: y,
        });
    }
    if let Some(r) = right {
        let y =
            ((sel.min_y.max(r.min_y) + sel.max_y.min(r.max_y)) * 0.5).clamp(sel.min_y, sel.max_y);
        guides.push(SpacingGuide {
            axis: Axis::Horizontal,
            x0: sel.max_x,
            y0: y,
            x1: r.min_x,
            y1: y,
        });
    }
    if let Some(t) = top {
        let x =
            ((sel.min_x.max(t.min_x) + sel.max_x.min(t.max_x)) * 0.5).clamp(sel.min_x, sel.max_x);
        guides.push(SpacingGuide {
            axis: Axis::Vertical,
            x0: x,
            y0: t.max_y,
            x1: x,
            y1: sel.min_y,
        });
    }
    if let Some(b) = bottom {
        let x =
            ((sel.min_x.max(b.min_x) + sel.max_x.min(b.max_x)) * 0.5).clamp(sel.min_x, sel.max_x);
        guides.push(SpacingGuide {
            axis: Axis::Vertical,
            x0: x,
            y0: sel.max_y,
            x1: x,
            y1: b.min_y,
        });
    }
    guides
}

fn spacing_guide_distance(guide: SpacingGuide) -> f64 {
    match guide.axis {
        Axis::Horizontal => (guide.x1 - guide.x0).abs(),
        Axis::Vertical => (guide.y1 - guide.y0).abs(),
    }
}

fn guide_label_position_css(
    guide: SpacingGuide,
    content: PrimitiveBounds,
    zoom: f64,
    pan_x: f64,
    pan_y: f64,
    dpr: f64,
) -> (f64, f64) {
    let mid_x_world = (guide.x0 + guide.x1) * 0.5;
    let mid_y_world = (guide.y0 + guide.y1) * 0.5;
    let x = world_to_panel_css_x(mid_x_world, content, zoom, pan_x, dpr);
    let y = world_to_panel_css_y(mid_y_world, content, zoom, pan_y, dpr);
    (x, y)
}

#[derive(Debug, Clone, Copy, Default)]
struct OverlayRectCss {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Axis {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct SpacingGuide {
    axis: Axis,
    x0: f64,
    y0: f64,
    x1: f64,
    y1: f64,
}

#[derive(Debug, Clone, Copy, Default)]
struct EditableRect {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

#[derive(Debug, Clone, Copy, Default)]
struct NodeLayoutOverride {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

#[derive(Debug, Clone)]
struct UndoSnapshot {
    label: String,
    frame_doc: Option<FrameDocument>,
    node_overrides: std::collections::HashMap<NodeId, NodeLayoutOverride>,
}

impl NodeLayoutOverride {
    fn from_rect(r: EditableRect) -> Self {
        Self {
            x: r.x,
            y: r.y,
            width: r.width.max(1.0),
            height: r.height.max(1.0),
        }
    }
}

impl From<PrimitiveBounds> for EditableRect {
    fn from(value: PrimitiveBounds) -> Self {
        Self {
            x: value.min_x,
            y: value.min_y,
            width: (value.max_x - value.min_x).max(1.0),
            height: (value.max_y - value.min_y).max(1.0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResizeHandle {
    Nw,
    Ne,
    Sw,
    Se,
}

#[derive(Debug, Clone, Copy)]
enum EditDragMode {
    Move,
    Resize(ResizeHandle),
}

#[derive(Debug, Clone, Copy)]
struct EditDragState {
    node_id: NodeId,
    start_mouse: (f64, f64),
    start_rect: EditableRect,
    mode: EditDragMode,
}

#[derive(Debug, Clone)]
struct DeepSelectState {
    world_x: f64,
    world_y: f64,
    stack: Vec<NodeId>,
    index: usize,
}

impl DeepSelectState {
    fn matches_cursor(&self, world_x: f64, world_y: f64) -> bool {
        (self.world_x - world_x).abs() <= 2.0 && (self.world_y - world_y).abs() <= 2.0
    }
}

fn build_editor_primitives(
    doc: &FrameDocument,
    root: NodeId,
    overrides: &std::collections::HashMap<NodeId, NodeLayoutOverride>,
) -> Vec<frame_ui::PaintPrimitive> {
    let mut primitives = build_paint_primitives(doc, root);
    if overrides.is_empty() {
        return primitives;
    }

    let base_bounds = node_bounds_map_from_primitives(&primitives);
    for (node_id, ov) in overrides {
        let Some(base) = base_bounds.get(node_id).copied() else {
            continue;
        };
        let base_w = (base.max_x - base.min_x).max(1.0);
        let base_h = (base.max_y - base.min_y).max(1.0);
        let sx = (ov.width / base_w).max(0.01);
        let sy = (ov.height / base_h).max(0.01);
        let subtree = doc.walk_subtree(*node_id);

        for primitive in primitives.iter_mut() {
            let pid = primitive_node_id(primitive);
            if !subtree.iter().any(|id| *id == pid) {
                continue;
            }
            transform_primitive(primitive, base.min_x, base.min_y, ov.x, ov.y, sx, sy);
        }
    }

    primitives
}

fn layout_bounds_map_for_ui(
    doc: &FrameDocument,
    root: NodeId,
    overrides: &std::collections::HashMap<NodeId, NodeLayoutOverride>,
) -> std::collections::HashMap<NodeId, PrimitiveBounds> {
    let mut map: std::collections::HashMap<NodeId, PrimitiveBounds> = build_layout_boxes(doc, root)
        .into_iter()
        .map(|(id, b)| {
            (
                id,
                PrimitiveBounds {
                    min_x: b.x,
                    min_y: b.y,
                    max_x: b.x + b.width,
                    max_y: b.y + b.height,
                },
            )
        })
        .collect();

    for (node_id, ov) in overrides {
        let Some(base) = map.get(node_id).copied() else {
            continue;
        };
        let base_w = (base.max_x - base.min_x).max(1.0);
        let base_h = (base.max_y - base.min_y).max(1.0);
        let sx = (ov.width / base_w).max(0.01);
        let sy = (ov.height / base_h).max(0.01);
        let subtree = doc.walk_subtree(*node_id);
        for sid in subtree {
            if let Some(bounds) = map.get_mut(&sid) {
                let min_x = ov.x + (bounds.min_x - base.min_x) * sx;
                let max_x = ov.x + (bounds.max_x - base.min_x) * sx;
                let min_y = ov.y + (bounds.min_y - base.min_y) * sy;
                let max_y = ov.y + (bounds.max_y - base.min_y) * sy;
                bounds.min_x = min_x;
                bounds.max_x = max_x;
                bounds.min_y = min_y;
                bounds.max_y = max_y;
            }
        }
    }

    map
}

fn primitive_node_id(primitive: &frame_ui::PaintPrimitive) -> NodeId {
    match primitive {
        frame_ui::PaintPrimitive::LayerStart { node_id, .. }
        | frame_ui::PaintPrimitive::LayerEnd { node_id }
        | frame_ui::PaintPrimitive::ClipStart { node_id, .. }
        | frame_ui::PaintPrimitive::ClipEnd { node_id }
        | frame_ui::PaintPrimitive::Rect { node_id, .. }
        | frame_ui::PaintPrimitive::Path { node_id, .. }
        | frame_ui::PaintPrimitive::Text { node_id, .. } => *node_id,
    }
}

fn transform_primitive(
    primitive: &mut frame_ui::PaintPrimitive,
    from_x: f64,
    from_y: f64,
    to_x: f64,
    to_y: f64,
    sx: f64,
    sy: f64,
) {
    match primitive {
        frame_ui::PaintPrimitive::LayerStart {
            x,
            y,
            width,
            height,
            ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *width = (*width * sx).max(1.0);
            *height = (*height * sy).max(1.0);
        }
        frame_ui::PaintPrimitive::LayerEnd { .. } => {}
        frame_ui::PaintPrimitive::ClipStart {
            x,
            y,
            width,
            height,
            ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *width = (*width * sx).max(1.0);
            *height = (*height * sy).max(1.0);
        }
        frame_ui::PaintPrimitive::ClipEnd { .. } => {}
        frame_ui::PaintPrimitive::Rect {
            x,
            y,
            width,
            height,
            ..
        }
        | frame_ui::PaintPrimitive::Path {
            x,
            y,
            width,
            height,
            ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *width = (*width * sx).max(1.0);
            *height = (*height * sy).max(1.0);
        }
        frame_ui::PaintPrimitive::Text {
            x, y, font_size, ..
        } => {
            *x = to_x + (*x - from_x) * sx;
            *y = to_y + (*y - from_y) * sy;
            *font_size = (*font_size * ((sx + sy) * 0.5)).max(1.0);
        }
    }
}

fn node_bounds_map_from_primitives(
    primitives: &[frame_ui::PaintPrimitive],
) -> std::collections::HashMap<NodeId, PrimitiveBounds> {
    let mut map: std::collections::HashMap<NodeId, PrimitiveBounds> =
        std::collections::HashMap::new();
    for primitive in primitives {
        let node_id = primitive_node_id(primitive);
        let entry = map.entry(node_id).or_insert(PrimitiveBounds {
            min_x: f64::INFINITY,
            min_y: f64::INFINITY,
            max_x: f64::NEG_INFINITY,
            max_y: f64::NEG_INFINITY,
        });

        match primitive {
            frame_ui::PaintPrimitive::LayerStart {
                x,
                y,
                width,
                height,
                ..
            } => {
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + *width);
                entry.max_y = entry.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::LayerEnd { .. } => {}
            frame_ui::PaintPrimitive::ClipStart {
                x,
                y,
                width,
                height,
                ..
            } => {
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + *width);
                entry.max_y = entry.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::ClipEnd { .. } => {}
            frame_ui::PaintPrimitive::Rect {
                x,
                y,
                width,
                height,
                ..
            }
            | frame_ui::PaintPrimitive::Path {
                x,
                y,
                width,
                height,
                ..
            } => {
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + *width);
                entry.max_y = entry.max_y.max(*y + *height);
            }
            frame_ui::PaintPrimitive::Text {
                x,
                y,
                text,
                font_size,
                ..
            } => {
                let width = (*font_size * 0.55 * text.chars().count() as f64).max(*font_size);
                entry.min_x = entry.min_x.min(*x);
                entry.min_y = entry.min_y.min(*y);
                entry.max_x = entry.max_x.max(*x + width);
                entry.max_y = entry.max_y.max(*y + *font_size * 1.2);
            }
        }
    }
    map.retain(|_, b| {
        b.min_x.is_finite() && b.min_y.is_finite() && b.max_x.is_finite() && b.max_y.is_finite()
    });
    map
}

fn selection_overlay_rect(
    world: PrimitiveBounds,
    content: PrimitiveBounds,
    preview: FramePreviewBounds,
    zoom: f64,
    pan_x: f64,
    pan_y: f64,
) -> OverlayRectCss {
    let dpr = preview.dpr.max(1.0);
    let pad = 16.0 * dpr;
    let left_dp = pad + pan_x + (world.min_x - content.min_x) * zoom;
    let top_dp = pad + pan_y + (world.min_y - content.min_y) * zoom;
    let width_dp = ((world.max_x - world.min_x) * zoom).max(1.0);
    let height_dp = ((world.max_y - world.min_y) * zoom).max(1.0);

    OverlayRectCss {
        x: left_dp / dpr,
        y: top_dp / dpr,
        width: width_dp / dpr,
        height: height_dp / dpr,
    }
}

fn apply_resize_handle(target: &mut EditableRect, handle: ResizeHandle, dx: f64, dy: f64) {
    match handle {
        ResizeHandle::Nw => {
            target.x += dx;
            target.y += dy;
            target.width -= dx;
            target.height -= dy;
        }
        ResizeHandle::Ne => {
            target.y += dy;
            target.width += dx;
            target.height -= dy;
        }
        ResizeHandle::Sw => {
            target.x += dx;
            target.width -= dx;
            target.height += dy;
        }
        ResizeHandle::Se => {
            target.width += dx;
            target.height += dy;
        }
    }
    if target.width < 1.0 {
        target.width = 1.0;
    }
    if target.height < 1.0 {
        target.height = 1.0;
    }
}

fn detect_resize_handle_css(
    overlay: OverlayRectCss,
    client_x_css: f64,
    client_y_css: f64,
) -> Option<ResizeHandle> {
    let threshold = 10.0;
    let corners = [
        (ResizeHandle::Nw, overlay.x, overlay.y),
        (ResizeHandle::Ne, overlay.x + overlay.width, overlay.y),
        (ResizeHandle::Sw, overlay.x, overlay.y + overlay.height),
        (
            ResizeHandle::Se,
            overlay.x + overlay.width,
            overlay.y + overlay.height,
        ),
    ];
    corners.into_iter().find_map(|(handle, hx, hy)| {
        let dx = client_x_css - hx;
        let dy = client_y_css - hy;
        let dist_sq = dx * dx + dy * dy;
        if dist_sq <= threshold * threshold {
            Some(handle)
        } else {
            None
        }
    })
}

fn overlay_contains_css(overlay: OverlayRectCss, client_x_css: f64, client_y_css: f64) -> bool {
    client_x_css >= overlay.x
        && client_x_css <= overlay.x + overlay.width
        && client_y_css >= overlay.y
        && client_y_css <= overlay.y + overlay.height
}

fn resize_cursor(handle: ResizeHandle) -> &'static str {
    match handle {
        ResizeHandle::Nw | ResizeHandle::Se => "nwse-resize",
        ResizeHandle::Ne | ResizeHandle::Sw => "nesw-resize",
    }
}

#[component]
fn NumericInspectorField(label: String, value: f64, on_change: EventHandler<f64>) -> Element {
    rsx! {
        div { class: "flex flex-col gap-1",
            div { class: "text-[10px] uppercase tracking-wide text-zinc-500", "{label}" }
            Input {
                input_type: "number".to_string(),
                size: InputSize::Small,
                full_width: true,
                value: format!("{:.0}", value),
                class: Some("bg-zinc-800 border-zinc-700 text-zinc-200 h-8".to_string()),
                on_change: move |evt: FormEvent| {
                    if let Ok(next) = evt.value().parse::<f64>() {
                        on_change.call(next);
                    }
                },
            }
        }
    }
}

#[component]
fn InspectorValueGrid(rows: Vec<(String, String)>) -> Element {
    rsx! {
        div { class: "space-y-1",
            for (label, value) in rows {
                div { class: "flex items-start justify-between gap-3",
                    span { class: "text-zinc-500", "{label}" }
                    span { class: "text-zinc-200 text-right break-all font-mono", "{value}" }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct SiblingSpacingSnapshot {
    prev_gap: Option<f64>,
    next_gap: Option<f64>,
    axis: String,
}

fn sibling_spacing_snapshot(
    doc: &FrameDocument,
    node_bounds: &std::collections::HashMap<NodeId, PrimitiveBounds>,
    selected: NodeId,
) -> SiblingSpacingSnapshot {
    let mut out = SiblingSpacingSnapshot {
        prev_gap: None,
        next_gap: None,
        axis: "none".to_string(),
    };
    let Some(node) = doc.get_node(selected) else {
        return out;
    };
    let Some(parent_id) = node.parent else {
        return out;
    };
    let Some(parent) = doc.get_node(parent_id) else {
        return out;
    };
    let Some(sel_bounds) = node_bounds.get(&selected).copied() else {
        return out;
    };
    let Some(index) = parent.children.iter().position(|id| *id == selected) else {
        return out;
    };

    let axis = doc
        .project_node(parent_id)
        .and_then(|p| p.auto_layout)
        .and_then(|al| al.mode)
        .map(|m| format!("{m:?}"))
        .unwrap_or_else(|| "none".to_string());
    out.axis = axis;
    let horizontal = out.axis == "Horizontal";

    if index > 0 {
        let prev_id = parent.children[index - 1];
        if let Some(prev) = node_bounds.get(&prev_id).copied() {
            out.prev_gap = Some(if horizontal {
                sel_bounds.min_x - prev.max_x
            } else {
                sel_bounds.min_y - prev.max_y
            });
        }
    }
    if let Some(next_id) = parent.children.get(index + 1).copied() {
        if let Some(next) = node_bounds.get(&next_id).copied() {
            out.next_gap = Some(if horizontal {
                next.min_x - sel_bounds.max_x
            } else {
                next.min_y - sel_bounds.max_y
            });
        }
    }

    out
}

fn auto_layout_rows(auto: Option<&AutoLayout>) -> Vec<(String, String)> {
    let Some(auto) = auto else {
        return vec![("Enabled".to_string(), "false".to_string())];
    };

    vec![
        (
            "Enabled".to_string(),
            if auto.mode.is_some() { "true" } else { "false" }.to_string(),
        ),
        ("Mode".to_string(), fmt_opt_dbg(auto.mode.as_ref())),
        (
            "Primary sizing".to_string(),
            fmt_opt_dbg(auto.primary_axis_sizing_mode.as_ref()),
        ),
        (
            "Counter sizing".to_string(),
            fmt_opt_dbg(auto.counter_axis_sizing_mode.as_ref()),
        ),
        (
            "Primary align".to_string(),
            fmt_opt_dbg(auto.primary_axis_align_items.as_ref()),
        ),
        (
            "Counter align".to_string(),
            fmt_opt_dbg(auto.counter_axis_align_items.as_ref()),
        ),
        ("Item spacing".to_string(), fmt_opt_px(auto.item_spacing)),
        (
            "Counter spacing".to_string(),
            fmt_opt_px(auto.counter_axis_spacing),
        ),
        (
            "Padding".to_string(),
            format!(
                "L {}  T {}  R {}  B {}",
                fmt_opt_px(auto.padding_left),
                fmt_opt_px(auto.padding_top),
                fmt_opt_px(auto.padding_right),
                fmt_opt_px(auto.padding_bottom)
            ),
        ),
        ("Wrap".to_string(), fmt_opt_dbg(auto.wrap.as_ref())),
        (
            "Positioning".to_string(),
            fmt_opt_dbg(auto.positioning.as_ref()),
        ),
        (
            "Align self".to_string(),
            fmt_opt_dbg(auto.align_self.as_ref()),
        ),
        ("Grow".to_string(), fmt_opt_f(auto.grow)),
        ("Min W".to_string(), fmt_opt_px(auto.min_width)),
        ("Max W".to_string(), fmt_opt_px(auto.max_width)),
        ("Min H".to_string(), fmt_opt_px(auto.min_height)),
        ("Max H".to_string(), fmt_opt_px(auto.max_height)),
    ]
}

fn parent_auto_layout_rows(
    parent: Option<&frame_proto::RenderNodeProjection>,
) -> Vec<(String, String)> {
    let Some(parent) = parent else {
        return vec![("Parent".to_string(), "n/a".to_string())];
    };
    let mut rows = vec![
        ("Parent".to_string(), parent.name.clone()),
        ("Parent type".to_string(), parent.figma_type.clone()),
    ];
    rows.extend(auto_layout_rows(parent.auto_layout.as_ref()));
    rows
}

fn constraint_horizontal(doc: &FrameDocument, node_id: NodeId) -> String {
    doc.get_node(node_id)
        .and_then(|n| n.raw.get("constraints"))
        .and_then(|v| v.get("horizontal"))
        .and_then(|v| v.as_str())
        .unwrap_or("LEFT")
        .to_string()
}

fn constraint_vertical(doc: &FrameDocument, node_id: NodeId) -> String {
    doc.get_node(node_id)
        .and_then(|n| n.raw.get("constraints"))
        .and_then(|v| v.get("vertical"))
        .and_then(|v| v.as_str())
        .unwrap_or("TOP")
        .to_string()
}

fn record_undo_snapshot(
    undo_stack: &mut Signal<Vec<UndoSnapshot>>,
    redo_stack: &mut Signal<Vec<UndoSnapshot>>,
    undo_history: &mut Signal<Vec<String>>,
    frame_doc: &Signal<Option<FrameDocument>>,
    node_overrides: &Signal<std::collections::HashMap<NodeId, NodeLayoutOverride>>,
    label: &str,
) {
    let snapshot = UndoSnapshot {
        label: label.to_string(),
        frame_doc: frame_doc(),
        node_overrides: node_overrides(),
    };
    undo_stack.with_mut(|stack| {
        stack.push(snapshot);
        if stack.len() > 200 {
            stack.remove(0);
        }
    });
    redo_stack.with_mut(|stack| stack.clear());
    undo_history.with_mut(|history| {
        history.push(format!("Edit: {label}"));
        if history.len() > 200 {
            history.remove(0);
        }
    });
}

fn undo_last_edit(
    undo_stack: &mut Signal<Vec<UndoSnapshot>>,
    redo_stack: &mut Signal<Vec<UndoSnapshot>>,
    undo_history: &mut Signal<Vec<String>>,
    frame_doc: &mut Signal<Option<FrameDocument>>,
    node_overrides: &mut Signal<std::collections::HashMap<NodeId, NodeLayoutOverride>>,
) {
    let current = UndoSnapshot {
        label: "current".to_string(),
        frame_doc: frame_doc(),
        node_overrides: node_overrides(),
    };
    let previous = undo_stack.with_mut(|stack| stack.pop());
    if let Some(snapshot) = previous {
        redo_stack.with_mut(|stack| {
            stack.push(current);
            if stack.len() > 200 {
                stack.remove(0);
            }
        });
        frame_doc.set(snapshot.frame_doc);
        node_overrides.set(snapshot.node_overrides);
        undo_history.with_mut(|history| {
            history.push(format!("Undo: {}", snapshot.label));
            if history.len() > 200 {
                history.remove(0);
            }
        });
    }
}

fn redo_last_edit(
    undo_stack: &mut Signal<Vec<UndoSnapshot>>,
    redo_stack: &mut Signal<Vec<UndoSnapshot>>,
    undo_history: &mut Signal<Vec<String>>,
    frame_doc: &mut Signal<Option<FrameDocument>>,
    node_overrides: &mut Signal<std::collections::HashMap<NodeId, NodeLayoutOverride>>,
) {
    let current = UndoSnapshot {
        label: "current".to_string(),
        frame_doc: frame_doc(),
        node_overrides: node_overrides(),
    };
    let next = redo_stack.with_mut(|stack| stack.pop());
    if let Some(snapshot) = next {
        undo_stack.with_mut(|stack| {
            stack.push(current);
            if stack.len() > 200 {
                stack.remove(0);
            }
        });
        frame_doc.set(snapshot.frame_doc);
        node_overrides.set(snapshot.node_overrides);
        undo_history.with_mut(|history| {
            history.push("Redo".to_string());
            if history.len() > 200 {
                history.remove(0);
            }
        });
    }
}

fn set_node_constraint(doc: &mut FrameDocument, node_id: NodeId, axis: &str, value: &str) {
    let Some(node) = doc.get_node_mut(node_id) else {
        return;
    };
    let Some(raw_obj) = node.raw.as_object_mut() else {
        return;
    };
    let constraints = raw_obj
        .entry("constraints".to_string())
        .or_insert_with(|| serde_json::json!({}));
    if !constraints.is_object() {
        *constraints = serde_json::json!({});
    }
    if let Some(obj) = constraints.as_object_mut() {
        obj.insert(
            axis.to_string(),
            serde_json::Value::String(value.to_string()),
        );
    }
}

fn set_node_rotation(doc: &mut FrameDocument, node_id: NodeId, rotation: f64) {
    let Some(node) = doc.get_node_mut(node_id) else {
        return;
    };
    let Some(raw_obj) = node.raw.as_object_mut() else {
        return;
    };
    raw_obj.insert("rotation".to_string(), serde_json::json!(rotation));
}

fn toggle_node_flag(doc: &mut FrameDocument, node_id: NodeId, key: &str) {
    let Some(node) = doc.get_node_mut(node_id) else {
        return;
    };
    let Some(raw_obj) = node.raw.as_object_mut() else {
        return;
    };
    let next = !raw_obj.get(key).and_then(|v| v.as_bool()).unwrap_or(false);
    raw_obj.insert(key.to_string(), serde_json::Value::Bool(next));
}

fn tidy_up_parent_children(
    doc: &FrameDocument,
    node_bounds: &std::collections::HashMap<NodeId, PrimitiveBounds>,
    overrides: &mut std::collections::HashMap<NodeId, NodeLayoutOverride>,
    selected: NodeId,
) {
    let Some(node) = doc.get_node(selected) else {
        return;
    };
    let Some(parent_id) = node.parent else {
        return;
    };
    let Some(parent) = doc.get_node(parent_id) else {
        return;
    };
    for child in &parent.children {
        let Some(bounds) = node_bounds.get(child).copied() else {
            continue;
        };
        let rect = EditableRect {
            x: bounds.min_x.round(),
            y: bounds.min_y.round(),
            width: (bounds.max_x - bounds.min_x).round().max(1.0),
            height: (bounds.max_y - bounds.min_y).round().max(1.0),
        };
        overrides.insert(*child, NodeLayoutOverride::from_rect(rect));
    }
}

fn distribute_parent_children(
    doc: &FrameDocument,
    node_bounds: &std::collections::HashMap<NodeId, PrimitiveBounds>,
    overrides: &mut std::collections::HashMap<NodeId, NodeLayoutOverride>,
    selected: NodeId,
    axis: Axis,
) {
    let Some(node) = doc.get_node(selected) else {
        return;
    };
    let Some(parent_id) = node.parent else {
        return;
    };
    let Some(parent) = doc.get_node(parent_id) else {
        return;
    };
    let mut items: Vec<(NodeId, PrimitiveBounds)> = parent
        .children
        .iter()
        .filter_map(|id| {
            let node = doc.get_node(*id)?;
            if !node.visible || node.locked {
                return None;
            }
            let b = node_bounds.get(id).copied()?;
            Some((*id, b))
        })
        .collect();
    if items.len() < 3 {
        return;
    }

    items.sort_by(|a, b| {
        let ka = if axis == Axis::Horizontal {
            a.1.min_x
        } else {
            a.1.min_y
        };
        let kb = if axis == Axis::Horizontal {
            b.1.min_x
        } else {
            b.1.min_y
        };
        ka.partial_cmp(&kb).unwrap_or(std::cmp::Ordering::Equal)
    });

    let first = items.first().map(|(_, b)| *b).unwrap_or_default();
    let last = items.last().map(|(_, b)| *b).unwrap_or_default();
    let span = if axis == Axis::Horizontal {
        last.max_x - first.min_x
    } else {
        last.max_y - first.min_y
    };
    let sizes_sum: f64 = items
        .iter()
        .map(|(_, b)| {
            if axis == Axis::Horizontal {
                b.max_x - b.min_x
            } else {
                b.max_y - b.min_y
            }
        })
        .sum();
    let gap = ((span - sizes_sum) / ((items.len() - 1) as f64)).max(0.0);

    let mut cursor = if axis == Axis::Horizontal {
        first.min_x
    } else {
        first.min_y
    };
    for (id, b) in items {
        let mut rect: EditableRect = b.into();
        if axis == Axis::Horizontal {
            rect.x = cursor;
            cursor += rect.width + gap;
        } else {
            rect.y = cursor;
            cursor += rect.height + gap;
        }
        overrides.insert(id, NodeLayoutOverride::from_rect(rect));
    }
}

fn inspector_debug_snapshot(
    doc: &FrameDocument,
    node_id: NodeId,
    node_projection: Option<&frame_proto::RenderNodeProjection>,
    parent_projection: Option<&frame_proto::RenderNodeProjection>,
    selected_bounds: Option<PrimitiveBounds>,
    spacing: &SiblingSpacingSnapshot,
) -> String {
    let node = doc.get_node(node_id);
    let payload = serde_json::json!({
        "selected": {
            "node_id": node_id.to_string(),
            "figma_id": node.map(|n| n.figma_id.clone()).unwrap_or_default(),
            "name": node.map(|n| n.name.clone()).unwrap_or_default(),
            "type": node.map(|n| n.figma_type.clone()).unwrap_or_default(),
            "class": node_projection.map(|p| format!("{:?}", p.class)).unwrap_or_else(|| "Unknown".to_string()),
            "bounds": selected_bounds.map(|b| serde_json::json!({
                "x": b.min_x,
                "y": b.min_y,
                "width": b.max_x - b.min_x,
                "height": b.max_y - b.min_y
            })),
            "constraints": {
                "horizontal": constraint_horizontal(doc, node_id),
                "vertical": constraint_vertical(doc, node_id),
            },
            "auto_layout": node_projection.and_then(|p| p.auto_layout.clone()),
        },
        "parent": {
            "name": parent_projection.map(|p| p.name.clone()),
            "type": parent_projection.map(|p| p.figma_type.clone()),
            "auto_layout": parent_projection.and_then(|p| p.auto_layout.clone()),
        },
        "spacing": {
            "axis": spacing.axis,
            "prev_gap": spacing.prev_gap,
            "next_gap": spacing.next_gap,
        }
    });
    serde_json::to_string_pretty(&payload).unwrap_or_else(|_| "{}".to_string())
}

fn fmt_opt_dbg<T: std::fmt::Debug>(v: Option<&T>) -> String {
    v.map(|x| format!("{x:?}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn fmt_opt_px(v: Option<f64>) -> String {
    v.map(|x| format!("{x:.1}px"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn fmt_opt_f(v: Option<f64>) -> String {
    v.map(|x| format!("{x:.3}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn raw_value_to_compact(v: &serde_json::Value) -> String {
    serde_json::to_string(v).unwrap_or_else(|_| "n/a".to_string())
}

#[cfg(feature = "desktop")]
fn try_load_system_text_font() -> Option<&'static Vec<u8>> {
    static FONT: OnceLock<Option<Vec<u8>>> = OnceLock::new();
    FONT.get_or_init(|| {
        let candidates = [
            "/System/Library/Fonts/SFNS.ttf",
            "/System/Library/Fonts/Supplemental/Arial.ttf",
            "/Library/Fonts/Arial.ttf",
        ];
        for path in candidates {
            if let Ok(bytes) = std::fs::read(path) {
                return Some(bytes);
            }
        }
        None
    })
    .as_ref()
}

fn preview_scene_root(doc: &FrameDocument) -> NodeId {
    let Some(page_id) = doc.pages.first().copied() else {
        return doc.root;
    };
    let Some(page) = doc.get_node(page_id) else {
        return page_id;
    };
    let Some(root_id) = page.children.first().copied() else {
        return page_id;
    };
    let Some(root) = doc.get_node(root_id) else {
        return root_id;
    };

    // Common .fig bridge shape: a white wrapper frame with exactly one child
    // that is the actual UI scene we want to preview.
    if root.children.len() == 1 {
        return root.children[0];
    }

    root_id
}

#[derive(Debug, Clone)]
struct FrameOption {
    id: NodeId,
    label: String,
    wrapper_like: bool,
}

#[derive(Debug, Clone)]
struct LayerRow {
    id: NodeId,
    depth: usize,
    name: String,
    figma_type: String,
    icon: &'static str,
}

fn collect_layer_rows(doc: &FrameDocument) -> Vec<LayerRow> {
    let mut out = Vec::new();
    append_layer_rows(doc, doc.root, 0, &mut out);
    out
}

fn append_layer_rows(doc: &FrameDocument, node_id: NodeId, depth: usize, out: &mut Vec<LayerRow>) {
    let Some(node) = doc.get_node(node_id) else {
        return;
    };
    out.push(LayerRow {
        id: node_id,
        depth,
        name: node.name.clone(),
        figma_type: node.figma_type.clone(),
        icon: layer_icon(&node.figma_type),
    });
    for child in &node.children {
        append_layer_rows(doc, *child, depth + 1, out);
    }
}

fn layer_icon(figma_type: &str) -> &'static str {
    match figma_type {
        "DOCUMENT" | "CANVAS" => "##",
        "FRAME" | "GROUP" | "SECTION" => "[]",
        "TEXT" => "T",
        "INSTANCE" | "COMPONENT" | "COMPONENT_SET" => "<>",
        "VECTOR" | "BOOLEAN_OPERATION" | "STAR" | "POLYGON" => "v",
        "ELLIPSE" => "()",
        "LINE" => "--",
        "RECTANGLE" => "[]",
        _ => "..",
    }
}

fn frame_option_index_by_id(options: &[FrameOption], node_id: NodeId) -> Option<usize> {
    options.iter().position(|option| option.id == node_id)
}

fn collect_frame_options(doc: &FrameDocument) -> Vec<FrameOption> {
    let mut out = Vec::new();
    for node_id in doc.walk_subtree(doc.root) {
        let Some(node) = doc.get_node(node_id) else {
            continue;
        };
        if node.figma_type != "FRAME" {
            continue;
        }
        let wrapper_like = node.children.len() == 1;
        let figma_id = if node.figma_id.is_empty() {
            "synthetic".to_string()
        } else {
            node.figma_id.clone()
        };
        out.push(FrameOption {
            id: node_id,
            label: format!("{} ({figma_id})", node.name),
            wrapper_like,
        });
    }

    out.sort_by(|a, b| {
        a.wrapper_like
            .cmp(&b.wrapper_like)
            .then_with(|| a.label.cmp(&b.label))
    });
    out
}
