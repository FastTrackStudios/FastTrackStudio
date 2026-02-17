//! Headless benchmark for the Frame tab render pipeline.
//!
//! Tests the exact same GPU code path as the app (direct vello + wgpu),
//! isolating each component to identify bottlenecks.
//!
//! Usage:
//!   # With a real Figma export (recommended for realistic numbers):
//!   FRAME_BENCH_FIXTURE=/tmp/fts-figma-export.json \
//!     cargo run --bin frame_bench --features desktop --release
//!
//!   # Without a fixture (generates synthetic rectangles):
//!   cargo run --bin frame_bench --features desktop --release
//!
//! Set FRAME_BENCH_FRAMES=500 to override the number of simulated frames (default 200).

use anyrender_vello::VelloScenePainter;
use frame_ui::{
    anyrender_renderer::{BlendMix, EffectKind, FillPaint, Rgba},
    build_paint_primitives, paint_primitives_into_scene_with, PaintPrimitive, TextFontRef,
};
use kurbo::{Affine, Rect};
use peniko::{Color, Fill};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

struct PrimitiveBounds {
    min_x: f64,
    min_y: f64,
    max_x: f64,
    max_y: f64,
}

fn primitive_bounds(primitives: &[PaintPrimitive]) -> PrimitiveBounds {
    let mut out = PrimitiveBounds {
        min_x: f64::INFINITY,
        min_y: f64::INFINITY,
        max_x: f64::NEG_INFINITY,
        max_y: f64::NEG_INFINITY,
    };
    for p in primitives {
        let (x, y, w, h) = match p {
            PaintPrimitive::LayerStart {
                x,
                y,
                width,
                height,
                ..
            }
            | PaintPrimitive::ClipStart {
                x,
                y,
                width,
                height,
                ..
            }
            | PaintPrimitive::Rect {
                x,
                y,
                width,
                height,
                ..
            }
            | PaintPrimitive::Path {
                x,
                y,
                width,
                height,
                ..
            } => (*x, *y, *width, *height),
            PaintPrimitive::Text {
                x, y, font_size, ..
            } => (*x, *y, 200.0, *font_size as f64),
            _ => continue,
        };
        out.min_x = out.min_x.min(x);
        out.min_y = out.min_y.min(y);
        out.max_x = out.max_x.max(x + w);
        out.max_y = out.max_y.max(y + h);
    }
    if out.min_x > out.max_x {
        out.min_x = 0.0;
        out.min_y = 0.0;
        out.max_x = 100.0;
        out.max_y = 100.0;
    }
    out
}

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

fn preview_scene_root(doc: &frame_proto::FrameDocument) -> frame_proto::NodeId {
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
    if root.children.len() == 1 {
        return root.children[0];
    }
    root_id
}

// ---------------------------------------------------------------------------
// Synthetic fixture
// ---------------------------------------------------------------------------

fn build_synthetic_primitives(count: usize) -> Vec<PaintPrimitive> {
    let mut prims = Vec::with_capacity(count);
    let cols = (count as f64).sqrt().ceil() as usize;
    for i in 0..count {
        let row = i / cols;
        let col = i % cols;
        let x = col as f64 * 120.0;
        let y = row as f64 * 80.0;
        prims.push(PaintPrimitive::Rect {
            node_id: frame_proto::NodeId::new(),
            x,
            y,
            width: 100.0,
            height: 60.0,
            fills: vec![(
                FillPaint::Solid(Rgba {
                    r: ((i * 37) % 256) as f64 / 255.0,
                    g: ((i * 73) % 256) as f64 / 255.0,
                    b: ((i * 113) % 256) as f64 / 255.0,
                    a: 1.0,
                }),
                BlendMix::Normal,
            )],
            stroke: None,
            corner_radii: Default::default(),
            effects: Vec::new(),
            blend: BlendMix::Normal,
            rotation: 0.0,
        });
    }
    prims
}

// ---------------------------------------------------------------------------
// Pan/zoom simulation
// ---------------------------------------------------------------------------

fn pan_zoom_sequence(n: usize) -> Vec<(f64, f64, f64)> {
    let mut seq = Vec::with_capacity(n);
    for i in 0..n {
        let t = i as f64 / n as f64;
        let pan_x = (t * std::f64::consts::TAU).sin() * 300.0;
        let pan_y = (t * std::f64::consts::TAU * 0.7).cos() * 200.0;
        let zoom = 1.0 + 0.5 * (t * std::f64::consts::TAU * 1.3).sin();
        seq.push((pan_x, pan_y, zoom));
    }
    seq
}

// ---------------------------------------------------------------------------
// Statistics
// ---------------------------------------------------------------------------

struct BenchResult {
    label: String,
    frames: usize,
    total: Duration,
    min: Duration,
    max: Duration,
    avg: Duration,
    p50: Duration,
    p95: Duration,
    p99: Duration,
}

impl BenchResult {
    fn print(&self) {
        println!("  {} ({} frames):", self.label, self.frames);
        println!(
            "    total: {:.1}ms  avg: {:.3}ms  min: {:.3}ms  max: {:.3}ms",
            self.total.as_secs_f64() * 1000.0,
            self.avg.as_secs_f64() * 1000.0,
            self.min.as_secs_f64() * 1000.0,
            self.max.as_secs_f64() * 1000.0,
        );
        println!(
            "    p50: {:.3}ms  p95: {:.3}ms  p99: {:.3}ms",
            self.p50.as_secs_f64() * 1000.0,
            self.p95.as_secs_f64() * 1000.0,
            self.p99.as_secs_f64() * 1000.0,
        );
        let fps = if self.avg.as_secs_f64() > 0.0 {
            1.0 / self.avg.as_secs_f64()
        } else {
            f64::INFINITY
        };
        println!("    effective fps: {:.0}", fps);
    }
}

fn percentile(sorted: &[Duration], pct: f64) -> Duration {
    let idx = ((sorted.len() as f64 * pct / 100.0).ceil() as usize).saturating_sub(1);
    sorted[idx.min(sorted.len() - 1)]
}

fn summarize(label: &str, mut times: Vec<Duration>) -> BenchResult {
    times.sort();
    let total: Duration = times.iter().sum();
    let avg = total / times.len() as u32;
    BenchResult {
        label: label.to_string(),
        frames: times.len(),
        total,
        min: times[0],
        max: *times.last().unwrap(),
        avg,
        p50: percentile(&times, 50.0),
        p95: percentile(&times, 95.0),
        p99: percentile(&times, 99.0),
    }
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

/// Benchmark: building a vello::Scene from primitives via VelloScenePainter.
fn bench_scene_build(
    primitives: &[PaintPrimitive],
    font_ref: Option<TextFontRef<'_>>,
    iterations: usize,
) -> BenchResult {
    let mut times = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let start = Instant::now();
        let mut scene = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut scene);
            paint_primitives_into_scene_with(&mut painter, primitives, Affine::IDENTITY, font_ref);
        }
        times.push(start.elapsed());
    }
    summarize("Scene build (vello::Scene via VelloScenePainter)", times)
}

/// Benchmark: scene.append() by reference — the per-frame hot path during pan/zoom.
fn bench_scene_append(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
) -> BenchResult {
    let pad = 16.0;
    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let start = Instant::now();
        let mut frame_scene = vello::Scene::new();
        frame_scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &Rect::new(0.0, 0.0, 1920.0, 1080.0),
        );
        frame_scene.append(fragment, Some(transform));
        times.push(start.elapsed());
    }

    summarize("Scene append (vello::Scene by reference)", times)
}

/// Benchmark: full GPU render pipeline — render_to_texture + poll.
/// This uses BufferRenderer (headless GPU) to match the exact vello render path.
fn bench_gpu_pipeline(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
    aa_config: vello::AaConfig,
    aa_support: vello::AaSupport,
    label: &str,
) -> BenchResult {
    let pad = 16.0;

    // Create headless GPU device + vello renderer (same as app's FrameGraphics)
    let features = wgpu::Features::CLEAR_TEXTURE | wgpu::Features::PIPELINE_CACHE;
    let mut wgpu_ctx =
        wgpu_context::WGPUContext::with_features_and_limits(Some(features), None);
    let buffer_renderer = pollster::block_on(wgpu_ctx.create_buffer_renderer(
        wgpu_context::BufferRendererConfig {
            width: canvas_w,
            height: canvas_h,
            usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        },
    ))
    .expect("Failed to create buffer renderer");

    let mut vello_renderer = vello::Renderer::new(
        buffer_renderer.device(),
        vello::RendererOptions {
            antialiasing_support: aa_support,
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    )
    .expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();
    let mut times = Vec::with_capacity(sequence.len());

    // Warm up: one render to initialize GPU pipelines
    {
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
        );
        scene.append(fragment, None);
        let texture_view = buffer_renderer.target_texture_view();
        vello_renderer
            .render_to_texture(
                buffer_renderer.device(),
                buffer_renderer.queue(),
                &scene,
                &texture_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: canvas_w,
                    height: canvas_h,
                    antialiasing_method: aa_config,
                },
            )
            .unwrap();
        buffer_renderer
            .device()
            .poll(wgpu::PollType::wait_indefinitely())
            .unwrap();
        scene.reset();
    }

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let frame_start = Instant::now();

        // Build frame scene (same as app render closure)
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
        );
        scene.append(fragment, Some(transform));

        // GPU render (same as app)
        let texture_view = buffer_renderer.target_texture_view();
        vello_renderer
            .render_to_texture(
                buffer_renderer.device(),
                buffer_renderer.queue(),
                &scene,
                &texture_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: canvas_w,
                    height: canvas_h,
                    antialiasing_method: aa_config,
                },
            )
            .expect("render_to_texture failed");
        drop(texture_view);

        // Blocking GPU wait (same as app)
        buffer_renderer
            .device()
            .poll(wgpu::PollType::wait_indefinitely())
            .unwrap();

        times.push(frame_start.elapsed());
        scene.reset();
    }

    summarize(label, times)
}

/// Benchmark: isolate just the GPU poll time by measuring submit vs wait separately.
fn bench_gpu_breakdown(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) {
    let pad = 16.0;
    let features = wgpu::Features::CLEAR_TEXTURE | wgpu::Features::PIPELINE_CACHE;
    let mut wgpu_ctx =
        wgpu_context::WGPUContext::with_features_and_limits(Some(features), None);
    let buffer_renderer = pollster::block_on(wgpu_ctx.create_buffer_renderer(
        wgpu_context::BufferRendererConfig {
            width: canvas_w,
            height: canvas_h,
            usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        },
    ))
    .expect("Failed to create buffer renderer");

    let mut vello_renderer = vello::Renderer::new(
        buffer_renderer.device(),
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    )
    .expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    {
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
        );
        scene.append(fragment, None);
        let tv = buffer_renderer.target_texture_view();
        vello_renderer
            .render_to_texture(
                buffer_renderer.device(),
                buffer_renderer.queue(),
                &scene,
                &tv,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: canvas_w,
                    height: canvas_h,
                    antialiasing_method: vello::AaConfig::Area,
                },
            )
            .unwrap();
        buffer_renderer
            .device()
            .poll(wgpu::PollType::wait_indefinitely())
            .unwrap();
        scene.reset();
    }

    let mut append_times = Vec::with_capacity(sequence.len());
    let mut submit_times = Vec::with_capacity(sequence.len());
    let mut poll_times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        // Phase 1: scene assembly
        let t0 = Instant::now();
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
        );
        scene.append(fragment, Some(transform));
        let t1 = Instant::now();
        append_times.push(t1 - t0);

        // Phase 2: GPU submit (render_to_texture)
        let texture_view = buffer_renderer.target_texture_view();
        vello_renderer
            .render_to_texture(
                buffer_renderer.device(),
                buffer_renderer.queue(),
                &scene,
                &texture_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT,
                    width: canvas_w,
                    height: canvas_h,
                    antialiasing_method: vello::AaConfig::Area,
                },
            )
            .unwrap();
        drop(texture_view);
        let t2 = Instant::now();
        submit_times.push(t2 - t1);

        // Phase 3: GPU wait
        buffer_renderer
            .device()
            .poll(wgpu::PollType::wait_indefinitely())
            .unwrap();
        let t3 = Instant::now();
        poll_times.push(t3 - t2);

        scene.reset();
    }

    println!("\n--- Per-phase breakdown (Area AA, {} frames) ---\n", sequence.len());
    summarize("Phase 1: scene assembly (fill + append)", append_times).print();
    println!();
    summarize("Phase 2: GPU submit (render_to_texture)", submit_times).print();
    println!();
    summarize("Phase 3: GPU wait (device.poll(wait))", poll_times).print();
}

/// Benchmark: pipelined GPU rendering with non-blocking poll (like vello examples).
/// Measures CPU-side throughput — the time from one frame's submit to the next.
fn bench_gpu_pipelined(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) -> BenchResult {
    let pad = 16.0;
    let features = wgpu::Features::CLEAR_TEXTURE | wgpu::Features::PIPELINE_CACHE;
    let mut wgpu_ctx =
        wgpu_context::WGPUContext::with_features_and_limits(Some(features), None);
    let buffer_renderer = pollster::block_on(wgpu_ctx.create_buffer_renderer(
        wgpu_context::BufferRendererConfig {
            width: canvas_w,
            height: canvas_h,
            usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        },
    ))
    .expect("Failed to create buffer renderer");

    let mut vello_renderer = vello::Renderer::new(
        buffer_renderer.device(),
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    )
    .expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    {
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, None);
        let tv = buffer_renderer.target_texture_view();
        vello_renderer.render_to_texture(
            buffer_renderer.device(), buffer_renderer.queue(),
            &scene, &tv,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            },
        ).unwrap();
        buffer_renderer.device().poll(wgpu::PollType::wait_indefinitely()).unwrap();
        scene.reset();
    }

    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let frame_start = Instant::now();

        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));

        let texture_view = buffer_renderer.target_texture_view();
        vello_renderer.render_to_texture(
            buffer_renderer.device(), buffer_renderer.queue(),
            &scene, &texture_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            },
        ).expect("render_to_texture failed");
        drop(texture_view);

        // Non-blocking poll — same as vello examples!
        let _ = buffer_renderer.device().poll(wgpu::PollType::Poll);

        times.push(frame_start.elapsed());
        scene.reset();
    }

    // Final drain
    buffer_renderer.device().poll(wgpu::PollType::wait_indefinitely()).unwrap();

    summarize("Pipelined (non-blocking poll, like vello examples)", times)
}

/// Benchmark: use vello::util::RenderContext (same GPU device setup as vello examples).
/// This tests whether wgpu_context's MemoryHints::MemoryUsage is causing overhead.
fn bench_vello_render_context(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) -> BenchResult {
    let pad = 16.0;

    // Use vello's own RenderContext — same as their examples
    let mut render_cx = vello::util::RenderContext::new();

    // Create a headless device through vello's context
    let device_handle = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[device_handle].device;
    let queue = &render_cx.devices[device_handle].queue;

    // Create an intermediate texture (same as RenderSurface.target_view)
    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("bench target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(
        device,
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    )
    .expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    {
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, None);
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        scene.reset();
    }

    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let frame_start = Instant::now();

        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));

        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).expect("render_to_texture failed");

        // Non-blocking poll — same as vello examples
        let _ = device.poll(wgpu::PollType::Poll);

        times.push(frame_start.elapsed());
        scene.reset();
    }

    // Final drain
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();

    summarize("vello::util::RenderContext (non-blocking, vello device setup)", times)
}

/// Benchmark: TRUE throughput — measures real wall-clock time for N frames.
/// The per-frame times in pipelined benchmarks are misleading because they only
/// measure CPU submission time. This benchmark measures total elapsed time
/// including GPU drain, giving the actual sustainable throughput.
fn bench_throughput(
    fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) {
    let pad = 16.0;

    // ── vello::util::RenderContext (what the app now uses) ──
    let mut render_cx = vello::util::RenderContext::new();
    let device_handle_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[device_handle_id].device;
    let queue = &render_cx.devices[device_handle_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("throughput target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(
        device,
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    ).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    {
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, None);
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        scene.reset();
    }

    let n = sequence.len();

    // ── Strategy A: Non-blocking poll (submit as fast as possible, drain at end)
    let wall_start = Instant::now();
    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        let _ = device.poll(wgpu::PollType::Poll);
        scene.reset();
    }
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    let wall_a = wall_start.elapsed();

    // ── Strategy B: Blocking poll (serialize CPU+GPU each frame)
    let wall_start = Instant::now();
    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        scene.reset();
    }
    let wall_b = wall_start.elapsed();

    // ── Strategy C: Non-blocking + 8ms sleep (simulate Dioxus render loop)
    let wall_start = Instant::now();
    for &(pan_x, pan_y, zoom) in sequence {
        std::thread::sleep(Duration::from_millis(8));
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        let _ = device.poll(wgpu::PollType::Poll);
        scene.reset();
    }
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    let wall_c = wall_start.elapsed();

    println!("  Strategy A (non-blocking, tight loop):");
    println!("    wall: {:.1}ms for {n} frames = {:.1}ms/frame = {:.0} fps",
        wall_a.as_secs_f64() * 1000.0,
        wall_a.as_secs_f64() * 1000.0 / n as f64,
        n as f64 / wall_a.as_secs_f64());
    println!("  Strategy B (blocking poll each frame):");
    println!("    wall: {:.1}ms for {n} frames = {:.1}ms/frame = {:.0} fps",
        wall_b.as_secs_f64() * 1000.0,
        wall_b.as_secs_f64() * 1000.0 / n as f64,
        n as f64 / wall_b.as_secs_f64());
    println!("  Strategy C (non-blocking + 8ms sleep, simulates Dioxus loop):");
    println!("    wall: {:.1}ms for {n} frames = {:.1}ms/frame = {:.0} fps",
        wall_c.as_secs_f64() * 1000.0,
        wall_c.as_secs_f64() * 1000.0 / n as f64,
        n as f64 / wall_c.as_secs_f64());
    println!("  A vs B speedup: {:.1}x",
        wall_b.as_secs_f64() / wall_a.as_secs_f64().max(1e-9));
}

/// Benchmark: simulate the ACTUAL app render loop headlessly.
/// Tests both the raster image path (pan/zoom) and the vector scene path (stable).
fn bench_render_loop_simulation(
    fragment: &vello::Scene,
    raster_w: u32,
    raster_h: u32,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) {
    let pad = 16.0;

    let mut render_cx = vello::util::RenderContext::new();
    let dev_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[dev_id].device;
    let queue = &render_cx.devices[dev_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("sim target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1, sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(device, vello::RendererOptions {
        antialiasing_support: vello::AaSupport::area_only(),
        use_cpu: false, num_init_threads: None, pipeline_cache: None,
    }).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Pre-render the raster image (what the app does in spawn_blocking)
    let raster_pixels = vec![128u8; (raster_w * raster_h * 4) as usize]; // grey placeholder
    let raster_brush = peniko::ImageBrush::new(peniko::ImageData {
        data: peniko::Blob::from(raster_pixels),
        format: peniko::ImageFormat::Rgba8,
        alpha_type: peniko::ImageAlphaType::AlphaPremultiplied,
        width: raster_w,
        height: raster_h,
    });

    // Warm up
    {
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, None);
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        scene.reset();
    }

    let n = sequence.len();

    // ── RASTER PATH: draw_image with transform (what happens during panning)
    let mut raster_times = Vec::with_capacity(n);
    for &(pan_x, pan_y, zoom) in sequence {
        let img_transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom);

        let t0 = Instant::now();
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.draw_image(&raster_brush, img_transform);
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        raster_times.push(t0.elapsed());
        scene.reset();
    }

    // ── VECTOR PATH: scene.append (what happens when stable after debounce)
    let mut vector_times = Vec::with_capacity(n);
    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let t0 = Instant::now();
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(fragment, Some(transform));
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        vector_times.push(t0.elapsed());
        scene.reset();
    }

    let raster_result = summarize("Raster path (draw_image, pan/zoom simulation)", raster_times);
    let vector_result = summarize("Vector path (scene.append, stable frame)", vector_times);
    raster_result.print();
    println!();
    vector_result.print();
    println!();
    println!("  Raster vs vector: {:.1}x faster during pan/zoom",
        vector_result.avg.as_secs_f64() / raster_result.avg.as_secs_f64().max(1e-9));
}

/// Benchmark: how vello cost scales with primitive count.
/// This answers: is 0.5ms/prim a fundamental cost or does it amortize?
fn bench_primitive_scaling(canvas_w: u32, canvas_h: u32) {
    let mut render_cx = vello::util::RenderContext::new();
    let dev_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[dev_id].device;
    let queue = &render_cx.devices[dev_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("scaling target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1, sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(device, vello::RendererOptions {
        antialiasing_support: vello::AaSupport::area_only(),
        use_cpu: false, num_init_threads: None, pipeline_cache: None,
    }).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
        &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
    vello_renderer.render_to_texture(device, queue, &scene, &target_view,
        &vello::RenderParams {
            base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
            antialiasing_method: vello::AaConfig::Area,
        }).unwrap();
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    scene.reset();

    let trials = 20;
    for count in [1, 5, 10, 20, 50, 100, 200, 500, 1000] {
        let mut times = Vec::with_capacity(trials);
        for trial in 0..trials {
            scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
                &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
            // Add N small rectangles with varying fills
            for i in 0..count {
                let x = ((i * 37 + trial * 13) % 1800) as f64;
                let y = ((i * 53 + trial * 7) % 1000) as f64;
                scene.fill(Fill::NonZero, Affine::IDENTITY,
                    Color::from_rgba8(
                        ((i * 37) % 256) as u8,
                        ((i * 73) % 256) as u8,
                        ((i * 113) % 256) as u8,
                        200,
                    ),
                    None,
                    &Rect::new(x, y, x + 80.0, y + 50.0),
                );
            }
            let t0 = Instant::now();
            vello_renderer.render_to_texture(device, queue, &scene, &target_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                    antialiasing_method: vello::AaConfig::Area,
                }).unwrap();
            device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
            times.push(t0.elapsed());
            scene.reset();
        }
        times.sort();
        let avg = times.iter().sum::<Duration>() / trials as u32;
        let per_prim = if count > 0 { avg.as_secs_f64() * 1000.0 / count as f64 } else { 0.0 };
        println!("  {:>5} rects: avg {:.1}ms ({:.0} fps) — {:.3}ms/rect",
            count,
            avg.as_secs_f64() * 1000.0,
            1.0 / avg.as_secs_f64().max(1e-9),
            per_prim,
        );
    }
}

/// Benchmark: render the SAME 65 rects directly via scene.fill() (no paint function,
/// no clips, no layers, no blend modes) to isolate the paint function overhead.
fn bench_direct_fills(
    primitives: &[PaintPrimitive],
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) -> BenchResult {
    let pad = 16.0;

    // Extract rect bounds from ALL primitives (Rect and Path alike)
    let mut rects: Vec<(f64, f64, f64, f64, u8, u8, u8)> = Vec::new();
    for p in primitives {
        match p {
            PaintPrimitive::Rect { x, y, width, height, .. }
            | PaintPrimitive::Path { x, y, width, height, .. } => {
                let i = rects.len();
                rects.push((
                    *x, *y, *width, *height,
                    ((i * 37) % 256) as u8,
                    ((i * 73) % 256) as u8,
                    ((i * 113) % 256) as u8,
                ));
            }
            _ => {}
        }
    }

    let mut render_cx = vello::util::RenderContext::new();
    let dev_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[dev_id].device;
    let queue = &render_cx.devices[dev_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("direct target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1, sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(device, vello::RendererOptions {
        antialiasing_support: vello::AaSupport::area_only(),
        use_cpu: false, num_init_threads: None, pipeline_cache: None,
    }).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
        &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
    vello_renderer.render_to_texture(device, queue, &scene, &target_view,
        &vello::RenderParams {
            base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
            antialiasing_method: vello::AaConfig::Area,
        }).unwrap();
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    scene.reset();

    // Build a pre-composed scene fragment (like the app does)
    let mut fragment = vello::Scene::new();
    for &(x, y, w, h, r, g, b) in &rects {
        fragment.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(r, g, b), None,
            &Rect::new(x, y, x + w, y + h));
    }

    let content = primitive_bounds(primitives);
    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let t0 = Instant::now();
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.append(&fragment, Some(transform));
        vello_renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        times.push(t0.elapsed());
        scene.reset();
    }

    summarize(&format!("Direct scene.fill() — {} rects, no clips/layers/blends", rects.len()), times)
}

// ---------------------------------------------------------------------------
// GPU Blit Path: render content to texture once, blit per frame
// ---------------------------------------------------------------------------

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

#[repr(C)]
#[derive(Clone, Copy)]
struct BlitParams {
    content_offset_scale: [f32; 4],
    bg_color: [f32; 4],
    sel_rect: [f32; 4],
    sel_color: [f32; 4],
    hover_rect: [f32; 4],
    hover_color: [f32; 4],
    misc: [f32; 4],
}

fn bench_gpu_blit_path(
    cached_fragment: &vello::Scene,
    content: &PrimitiveBounds,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) {
    // 1. Create vello context + renderer (same device for both cache render + blit)
    let mut render_cx = vello::util::RenderContext::new();
    let device_id = pollster::block_on(render_cx.device(None)).expect("need GPU for blit bench");
    let dev = &render_cx.devices[device_id];
    let device = &dev.device;
    let queue = &dev.queue;

    let mut renderer = vello::Renderer::new(
        device,
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    )
    .expect("create renderer");

    // 2. Render content to a cached GPU texture (one-time, expensive)
    let content_w = ((content.max_x - content.min_x).ceil() as u32).max(1);
    let content_h = ((content.max_y - content.min_y).ceil() as u32).max(1);

    let cached_tex = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Cached Content"),
        size: wgpu::Extent3d {
            width: content_w,
            height: content_h,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let cached_view = cached_tex.create_view(&wgpu::TextureViewDescriptor::default());

    // Build a scene with content translated to origin
    let mut cache_scene = vello::Scene::new();
    cache_scene.append(
        cached_fragment,
        Some(Affine::translate((-content.min_x, -content.min_y))),
    );

    let t_cache = Instant::now();
    renderer
        .render_to_texture(
            device,
            queue,
            &cache_scene,
            &cached_view,
            &vello::RenderParams {
                base_color: peniko::Color::TRANSPARENT,
                width: content_w,
                height: content_h,
                antialiasing_method: vello::AaConfig::Area,
            },
        )
        .expect("render to cache");
    device
        .poll(wgpu::PollType::wait_indefinitely())
        .unwrap();
    let cache_ms = t_cache.elapsed().as_secs_f64() * 1000.0;
    println!("  Cache render: {}x{} content → GPU texture in {:.1}ms (one-time)", content_w, content_h, cache_ms);

    // 3. Create blit pipeline
    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some("Blit"),
        source: wgpu::ShaderSource::Wgsl(BLIT_SHADER.into()),
    });
    let bgl = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
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
    let pl = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&bgl],
        push_constant_ranges: &[],
    });
    // Render to an offscreen Rgba8Unorm texture (no surface needed)
    let screen_tex = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Screen"),
        size: wgpu::Extent3d {
            width: canvas_w,
            height: canvas_h,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        view_formats: &[],
    });
    let screen_view = screen_tex.create_view(&wgpu::TextureViewDescriptor::default());

    let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("Blit Pipeline"),
        layout: Some(&pl),
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
                format: wgpu::TextureFormat::Rgba8Unorm,
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
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("Blit Sampler"),
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });
    let uniform_buf = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("Blit Params"),
        size: std::mem::size_of::<BlitParams>() as u64,
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });
    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &bgl,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&cached_view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: uniform_buf.as_entire_binding(),
            },
        ],
    });

    // 4. Benchmark: blit per frame with varying transforms
    let content_wf = (content.max_x - content.min_x).max(1.0);
    let content_hf = (content.max_y - content.min_y).max(1.0);
    let num_frames = sequence.len();

    // Warmup
    for _ in 0..3 {
        let params = BlitParams {
            content_offset_scale: [16.0, 16.0, content_wf as f32, content_hf as f32],
            bg_color: [0.02, 0.023, 0.047, 1.0],
            sel_rect: [100.0, 100.0, 300.0, 250.0],
            sel_color: [0.231, 0.51, 0.965, 0.9],
            hover_rect: [0.0; 4],
            hover_color: [0.0; 4],
            misc: [1.5, 1.0, 0.0, 0.0],
        };
        let bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                &params as *const BlitParams as *const u8,
                std::mem::size_of::<BlitParams>(),
            )
        };
        queue.write_buffer(&uniform_buf, 0, bytes);
        let mut enc = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        {
            let mut pass = enc.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &screen_view,
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
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.draw(0..3, 0..1);
        }
        queue.submit([enc.finish()]);
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    }

    // Timed runs
    let mut times = Vec::with_capacity(num_frames);
    for (i, &(zoom, pan_x, pan_y)) in sequence.iter().enumerate() {
        let params = BlitParams {
            content_offset_scale: [
                (16.0 + pan_x) as f32,
                (16.0 + pan_y) as f32,
                (zoom * content_wf) as f32,
                (zoom * content_hf) as f32,
            ],
            bg_color: [0.02, 0.023, 0.047, 1.0],
            sel_rect: [100.0, 100.0, 300.0, 250.0],
            sel_color: [0.231, 0.51, 0.965, 0.9],
            hover_rect: if i % 3 == 0 {
                [150.0, 120.0, 280.0, 200.0]
            } else {
                [0.0; 4]
            },
            hover_color: [0.863, 0.863, 0.882, 0.667],
            misc: [1.5, 1.0, 0.0, 0.0],
        };
        let bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                &params as *const BlitParams as *const u8,
                std::mem::size_of::<BlitParams>(),
            )
        };
        queue.write_buffer(&uniform_buf, 0, bytes);

        let t0 = Instant::now();
        let mut enc = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        {
            let mut pass = enc.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &screen_view,
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
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.draw(0..3, 0..1);
        }
        queue.submit([enc.finish()]);
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        times.push(t0.elapsed());
    }

    let result = summarize("GPU blit from cached texture (per-frame cost)", times);

    // Compare with full vello path
    println!("\n  Summary:");
    println!(
        "  Blit path (per-frame): avg {:.2}ms ({:.0} fps)",
        result.avg.as_secs_f64() * 1000.0,
        1.0 / result.avg.as_secs_f64().max(1e-9)
    );
    println!("  Cache render (one-time): {:.1}ms", cache_ms);
}

/// Apples-to-apples: compare direct fills vs paint function on the SAME device.
/// Previous benchmarks used different GPU contexts — this eliminates that variable.
fn bench_same_device_comparison(
    primitives: &[PaintPrimitive],
    font_ref: Option<TextFontRef<'_>>,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) {
    let pad = 16.0;

    // Shared vello device
    let mut render_cx = vello::util::RenderContext::new();
    let dev_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[dev_id].device;
    let queue = &render_cx.devices[dev_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("comparison target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1, sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut vello_renderer = vello::Renderer::new(device, vello::RendererOptions {
        antialiasing_support: vello::AaSupport::area_only(),
        use_cpu: false, num_init_threads: None, pipeline_cache: None,
    }).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();
    let content = primitive_bounds(primitives);

    // Warm up
    scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
        &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
    vello_renderer.render_to_texture(device, queue, &scene, &target_view,
        &vello::RenderParams {
            base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
            antialiasing_method: vello::AaConfig::Area,
        }).unwrap();
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    scene.reset();

    // ── Fragment A: direct fills (57 simple rects) ──
    let mut direct_frag = vello::Scene::new();
    let mut rect_count = 0;
    for p in primitives {
        match p {
            PaintPrimitive::Rect { x, y, width, height, .. }
            | PaintPrimitive::Path { x, y, width, height, .. } => {
                let i = rect_count;
                direct_frag.fill(Fill::NonZero, Affine::IDENTITY,
                    Color::from_rgb8(((i * 37) % 256) as u8, ((i * 73) % 256) as u8, ((i * 113) % 256) as u8),
                    None, &Rect::new(*x, *y, *x + *width, *y + *height));
                rect_count += 1;
            }
            _ => {}
        }
    }

    // ── Fragment B: full paint function (as-is) ──
    let mut paint_frag = vello::Scene::new();
    {
        let mut painter = VelloScenePainter::new(&mut paint_frag);
        paint_primitives_into_scene_with(&mut painter, primitives, Affine::IDENTITY, font_ref);
    }

    // ── Fragment C: ultimate minimal (all rects, no layers/clips/effects, through paint fn) ──
    let minimal_prims = strip_layers_and_clips(&strip_effects(&paths_to_rects(primitives)));
    let mut minimal_frag = vello::Scene::new();
    {
        let mut painter = VelloScenePainter::new(&mut minimal_frag);
        paint_primitives_into_scene_with(&mut painter, &minimal_prims, Affine::IDENTITY, font_ref);
    }

    // ── Fragment D: full paint with stripped SVGs (forces bezier fill_paths) ──
    let no_svg_prims = strip_svgs(primitives);
    let mut no_svg_frag = vello::Scene::new();
    {
        let mut painter = VelloScenePainter::new(&mut no_svg_frag);
        paint_primitives_into_scene_with(&mut painter, &no_svg_prims, Affine::IDENTITY, font_ref);
    }

    let fragments: Vec<(&str, &vello::Scene)> = vec![
        ("A: Direct fills (simple rects)", &direct_frag),
        ("B: Full paint function", &paint_frag),
        ("C: Minimal (rects, no layers/fx)", &minimal_frag),
        ("D: Paint without SVGs", &no_svg_frag),
    ];

    for (label, fragment) in &fragments {
        let mut times = Vec::with_capacity(sequence.len());
        for &(pan_x, pan_y, zoom) in sequence {
            let transform = Affine::translate((pad + pan_x, pad + pan_y))
                * Affine::scale(zoom)
                * Affine::translate((-content.min_x, -content.min_y));
            let t0 = Instant::now();
            scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(5, 6, 12), None,
                &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
            scene.append(fragment, Some(transform));
            vello_renderer.render_to_texture(device, queue, &scene, &target_view,
                &vello::RenderParams {
                    base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                    antialiasing_method: vello::AaConfig::Area,
                }).unwrap();
            device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
            times.push(t0.elapsed());
            scene.reset();
        }
        let result = summarize(label, times);
        println!("  {}: avg {:.1}ms ({:.0} fps)  p50={:.1}ms p95={:.1}ms",
            label,
            result.avg.as_secs_f64() * 1000.0,
            1.0 / result.avg.as_secs_f64().max(1e-9),
            result.p50.as_secs_f64() * 1000.0,
            result.p95.as_secs_f64() * 1000.0,
        );
    }
}

/// Benchmark: measure vello's FIXED per-frame overhead with a trivial scene.
/// If rendering 1 rectangle takes the same time as 65 primitives,
/// the bottleneck is vello's compute pipeline setup, not scene complexity.
fn bench_minimal_scene(canvas_w: u32, canvas_h: u32, num_frames: usize) {
    let mut render_cx = vello::util::RenderContext::new();
    let device_handle_id = pollster::block_on(async {
        render_cx.device(None).await.expect("Failed to get device")
    });
    let device = &render_cx.devices[device_handle_id].device;
    let queue = &render_cx.devices[device_handle_id].queue;

    let target_texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("minimal target"),
        size: wgpu::Extent3d { width: canvas_w, height: canvas_h, depth_or_array_layers: 1 },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    });
    let target_view = target_texture.create_view(&wgpu::TextureViewDescriptor::default());

    let mut renderer = vello::Renderer::new(
        device,
        vello::RendererOptions {
            antialiasing_support: vello::AaSupport::area_only(),
            use_cpu: false,
            num_init_threads: None,
            pipeline_cache: None,
        },
    ).expect("Failed to create vello renderer");

    let mut scene = vello::Scene::new();

    // Warm up
    scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(30, 30, 40), None,
        &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
    renderer.render_to_texture(device, queue, &scene, &target_view,
        &vello::RenderParams {
            base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
            antialiasing_method: vello::AaConfig::Area,
        }).unwrap();
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
    scene.reset();

    // Benchmark: 1 rectangle
    let mut times_1rect = Vec::with_capacity(num_frames);
    for i in 0..num_frames {
        let start = Instant::now();
        scene.fill(Fill::NonZero, Affine::IDENTITY, Color::from_rgb8(30, 30, 40), None,
            &Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64));
        scene.fill(Fill::NonZero, Affine::translate((i as f64, 0.0)),
            Color::from_rgb8(200, 50, 50), None, &Rect::new(10.0, 10.0, 200.0, 100.0));
        renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::TRANSPARENT, width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        times_1rect.push(start.elapsed());
        scene.reset();
    }

    // Benchmark: empty scene (just base_color fill)
    let mut times_empty = Vec::with_capacity(num_frames);
    for _ in 0..num_frames {
        let start = Instant::now();
        renderer.render_to_texture(device, queue, &scene, &target_view,
            &vello::RenderParams {
                base_color: Color::from_rgb8(30, 30, 40),
                width: canvas_w, height: canvas_h,
                antialiasing_method: vello::AaConfig::Area,
            }).unwrap();
        device.poll(wgpu::PollType::wait_indefinitely()).unwrap();
        times_empty.push(start.elapsed());
        scene.reset();
    }

    let result_1rect = summarize("1 rectangle (minimal scene)", times_1rect);
    let result_empty = summarize("Empty scene (base_color only)", times_empty);
    result_empty.print();
    println!();
    result_1rect.print();
}

/// Analyze primitives and print effect breakdown.
fn analyze_primitives(primitives: &[PaintPrimitive]) {
    let mut rects = 0usize;
    let mut paths = 0usize;
    let mut texts = 0usize;
    let mut clips = 0usize;
    let mut layers = 0usize;
    let mut drop_shadows = 0usize;
    let mut inner_shadows = 0usize;
    let mut layer_blurs = 0usize;
    let mut bg_blurs = 0usize;
    let mut total_fills = 0usize;
    let mut total_strokes = 0usize;
    let mut svg_paths = 0usize;
    let mut non_normal_blends = 0usize;
    let mut max_blur_radius = 0.0f64;
    let mut solid_fills = 0usize;
    let mut gradient_fills = 0usize;
    let mut image_fills = 0usize;
    let mut total_image_bytes = 0usize;

    let count_fills = |fills: &[(FillPaint, BlendMix)], solid: &mut usize, gradient: &mut usize, image: &mut usize, img_bytes: &mut usize| {
        for (fill, _) in fills {
            match fill {
                FillPaint::Solid(_) => *solid += 1,
                FillPaint::GradientLinear { .. } | FillPaint::GradientRadial { .. } | FillPaint::GradientAngular { .. } => *gradient += 1,
                FillPaint::Image { data_base64, .. } => {
                    *image += 1;
                    *img_bytes += data_base64.len();
                }
            }
        }
    };

    for p in primitives {
        match p {
            PaintPrimitive::Rect { fills, stroke, effects, blend, .. } => {
                rects += 1;
                total_fills += fills.len();
                count_fills(fills, &mut solid_fills, &mut gradient_fills, &mut image_fills, &mut total_image_bytes);
                if stroke.is_some() { total_strokes += 1; }
                if !matches!(blend, BlendMix::Normal) { non_normal_blends += 1; }
                for (_, bm) in fills {
                    if !matches!(bm, BlendMix::Normal) { non_normal_blends += 1; }
                }
                for e in effects {
                    match e.kind {
                        EffectKind::DropShadow => drop_shadows += 1,
                        EffectKind::InnerShadow => inner_shadows += 1,
                        EffectKind::LayerBlur => { layer_blurs += 1; max_blur_radius = max_blur_radius.max(e.radius); },
                        EffectKind::BackgroundBlur => { bg_blurs += 1; max_blur_radius = max_blur_radius.max(e.radius); },
                    }
                }
            }
            PaintPrimitive::Path { fills, stroke, effects, blend, svg_base64, .. } => {
                paths += 1;
                total_fills += fills.len();
                count_fills(fills, &mut solid_fills, &mut gradient_fills, &mut image_fills, &mut total_image_bytes);
                if stroke.is_some() { total_strokes += 1; }
                if svg_base64.is_some() { svg_paths += 1; }
                if !matches!(blend, BlendMix::Normal) { non_normal_blends += 1; }
                for (_, bm) in fills {
                    if !matches!(bm, BlendMix::Normal) { non_normal_blends += 1; }
                }
                for e in effects {
                    match e.kind {
                        EffectKind::DropShadow => drop_shadows += 1,
                        EffectKind::InnerShadow => inner_shadows += 1,
                        EffectKind::LayerBlur => { layer_blurs += 1; max_blur_radius = max_blur_radius.max(e.radius); },
                        EffectKind::BackgroundBlur => { bg_blurs += 1; max_blur_radius = max_blur_radius.max(e.radius); },
                    }
                }
            }
            PaintPrimitive::Text { .. } => texts += 1,
            PaintPrimitive::ClipStart { .. } => clips += 1,
            PaintPrimitive::ClipEnd { .. } => {}
            PaintPrimitive::LayerStart { blend, .. } => {
                layers += 1;
                if !matches!(blend, BlendMix::Normal) { non_normal_blends += 1; }
            }
            PaintPrimitive::LayerEnd { .. } => {}
        }
    }

    // Estimate vello command expansion
    let mut est_commands = 0usize;
    est_commands += total_fills;       // 1 fill call per fill
    est_commands += total_strokes;     // 1 stroke call per stroke
    est_commands += drop_shadows;      // 1 draw_box_shadow per drop shadow
    est_commands += inner_shadows * 6; // 6 commands per inner shadow
    est_commands += non_normal_blends * 2; // push_layer + pop_layer per blend
    est_commands += clips * 2;         // push_clip_layer + pop_layer
    est_commands += layers * 2;        // push_layer + pop_layer

    // LayerBlur/BackgroundBlur multiplier
    let avg_blur_taps = if max_blur_radius > 0.0 {
        ((max_blur_radius / 6.0).round() as usize).clamp(6, 20)
    } else { 0 };
    let blur_expansion = (layer_blurs + bg_blurs) * avg_blur_taps.max(1) * total_fills.max(1) / primitives.len().max(1);
    est_commands += blur_expansion;

    println!("  Primitive types: {} rects, {} paths ({} with SVG), {} texts, {} clips, {} layers",
        rects, paths, svg_paths, texts, clips, layers);
    println!("  Fills: {} ({} solid, {} gradient, {} image), Strokes: {}, Non-normal blends: {}",
        total_fills, solid_fills, gradient_fills, image_fills, total_strokes, non_normal_blends);
    if image_fills > 0 {
        println!("  Image fill data: {:.1} KB base64 total ({:.1} KB avg per image fill)",
            total_image_bytes as f64 / 1024.0,
            total_image_bytes as f64 / (image_fills as f64 * 1024.0));
    }
    println!("  Effects: {} drop shadows, {} inner shadows, {} layer blurs, {} bg blurs",
        drop_shadows, inner_shadows, layer_blurs, bg_blurs);
    if max_blur_radius > 0.0 {
        println!("  Max blur radius: {:.1} → ~{} kernel taps per blur", max_blur_radius, avg_blur_taps);
    }
    println!("  Estimated vello commands: ~{} (from {} primitives = {:.1}x expansion)",
        est_commands, primitives.len(), est_commands as f64 / primitives.len() as f64);
}

/// Strip ALL compositor layer primitives (LayerStart/LayerEnd/ClipStart/ClipEnd).
/// This isolates the GPU cost of compositor layers vs raw geometry.
fn strip_layers_and_clips(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    primitives
        .iter()
        .filter(|p| {
            !matches!(
                p,
                PaintPrimitive::LayerStart { .. }
                    | PaintPrimitive::LayerEnd { .. }
                    | PaintPrimitive::ClipStart { .. }
                    | PaintPrimitive::ClipEnd { .. }
            )
        })
        .cloned()
        .collect()
}

/// Count compositor layer primitives in the list.
fn count_layer_primitives(primitives: &[PaintPrimitive]) -> (usize, usize) {
    let layers = primitives
        .iter()
        .filter(|p| matches!(p, PaintPrimitive::LayerStart { .. }))
        .count();
    let clips = primitives
        .iter()
        .filter(|p| matches!(p, PaintPrimitive::ClipStart { .. }))
        .count();
    (layers, clips)
}

/// Replace all Path primitives with simple Rect equivalents (same bounds, solid fill).
fn paths_to_rects(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    primitives.iter().map(|p| {
        match p {
            PaintPrimitive::Path { node_id, x, y, width, height, fills, .. } => {
                PaintPrimitive::Rect {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fills: if fills.is_empty() {
                        vec![(FillPaint::Solid(Rgba { r: 0.5, g: 0.5, b: 0.5, a: 1.0 }), BlendMix::Normal)]
                    } else {
                        fills.clone()
                    },
                    stroke: None,
                    corner_radii: Default::default(),
                    effects: vec![],
                    blend: BlendMix::Normal,
                    rotation: 0.0,
                }
            }
            other => other.clone(),
        }
    }).collect()
}

/// Strip SVG data from paths, replacing with just fill_path-based rendering.
fn strip_svgs(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    primitives.iter().map(|p| {
        match p {
            PaintPrimitive::Path { node_id, x, y, width, height, fill_paths, stroke_paths, fills, stroke, effects, blend, rotation, .. } => {
                PaintPrimitive::Path {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fill_paths: fill_paths.clone(), stroke_paths: stroke_paths.clone(),
                    svg_base64: None, // Strip SVG
                    fills: fills.clone(), stroke: stroke.clone(),
                    effects: effects.clone(), blend: blend.clone(), rotation: *rotation,
                }
            }
            other => other.clone(),
        }
    }).collect()
}

/// Strip all effects from primitives (clones, sets effects to empty).
fn strip_effects(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    primitives.iter().map(|p| {
        match p {
            PaintPrimitive::Rect { node_id, x, y, width, height, fills, stroke, corner_radii, blend, rotation, .. } => {
                PaintPrimitive::Rect {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fills: fills.clone(), stroke: stroke.clone(),
                    corner_radii: corner_radii.clone(), effects: vec![], blend: blend.clone(), rotation: *rotation,
                }
            }
            PaintPrimitive::Path { node_id, x, y, width, height, fill_paths, stroke_paths, svg_base64, fills, stroke, blend, rotation, .. } => {
                PaintPrimitive::Path {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fill_paths: fill_paths.clone(), stroke_paths: stroke_paths.clone(),
                    svg_base64: svg_base64.clone(), fills: fills.clone(), stroke: stroke.clone(),
                    effects: vec![], blend: blend.clone(), rotation: *rotation,
                }
            }
            other => other.clone(),
        }
    }).collect()
}

/// Replace ALL fills with solid colors. Image fills get the average color of the
/// original, gradients get a mid-stop color. This isolates GPU cost of image
/// decoding/sampling vs pure geometry.
fn solidify_fills(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    let solidify = |fills: &[(FillPaint, BlendMix)]| -> Vec<(FillPaint, BlendMix)> {
        fills.iter().map(|(fill, blend)| {
            let solid = match fill {
                FillPaint::Solid(c) => FillPaint::Solid(c.clone()),
                FillPaint::GradientLinear { stops, .. }
                | FillPaint::GradientRadial { stops, .. }
                | FillPaint::GradientAngular { stops, .. } => {
                    // Use the mid-stop color
                    let mid = stops.get(stops.len() / 2).unwrap_or(&stops[0]);
                    FillPaint::Solid(mid.color.clone())
                }
                FillPaint::Image { alpha, .. } => {
                    // Replace with a placeholder gray at the same alpha
                    FillPaint::Solid(Rgba { r: 0.6, g: 0.6, b: 0.6, a: *alpha })
                }
            };
            (solid, blend.clone())
        }).collect()
    };

    primitives.iter().map(|p| {
        match p {
            PaintPrimitive::Rect { node_id, x, y, width, height, fills, stroke, corner_radii, effects, blend, rotation } => {
                PaintPrimitive::Rect {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fills: solidify(fills), stroke: stroke.clone(),
                    corner_radii: *corner_radii, effects: effects.clone(), blend: blend.clone(), rotation: *rotation,
                }
            }
            PaintPrimitive::Path { node_id, x, y, width, height, fill_paths, stroke_paths, svg_base64, fills, stroke, effects, blend, rotation } => {
                PaintPrimitive::Path {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fill_paths: fill_paths.clone(), stroke_paths: stroke_paths.clone(),
                    svg_base64: svg_base64.clone(), fills: solidify(fills), stroke: stroke.clone(),
                    effects: effects.clone(), blend: blend.clone(), rotation: *rotation,
                }
            }
            other => other.clone(),
        }
    }).collect()
}

/// Strip ONLY image fills, keeping gradients and solid fills. Isolates image-fill
/// GPU cost from gradient cost.
fn strip_image_fills(primitives: &[PaintPrimitive]) -> Vec<PaintPrimitive> {
    let strip = |fills: &[(FillPaint, BlendMix)]| -> Vec<(FillPaint, BlendMix)> {
        fills.iter().map(|(fill, blend)| {
            let replaced = match fill {
                FillPaint::Image { alpha, .. } => {
                    FillPaint::Solid(Rgba { r: 0.6, g: 0.6, b: 0.6, a: *alpha })
                }
                other => other.clone(),
            };
            (replaced, blend.clone())
        }).collect()
    };

    primitives.iter().map(|p| {
        match p {
            PaintPrimitive::Rect { node_id, x, y, width, height, fills, stroke, corner_radii, effects, blend, rotation } => {
                PaintPrimitive::Rect {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fills: strip(fills), stroke: stroke.clone(),
                    corner_radii: *corner_radii, effects: effects.clone(), blend: blend.clone(), rotation: *rotation,
                }
            }
            PaintPrimitive::Path { node_id, x, y, width, height, fill_paths, stroke_paths, svg_base64, fills, stroke, effects, blend, rotation } => {
                PaintPrimitive::Path {
                    node_id: *node_id, x: *x, y: *y, width: *width, height: *height,
                    fill_paths: fill_paths.clone(), stroke_paths: stroke_paths.clone(),
                    svg_base64: svg_base64.clone(), fills: strip(fills), stroke: stroke.clone(),
                    effects: effects.clone(), blend: blend.clone(), rotation: *rotation,
                }
            }
            other => other.clone(),
        }
    }).collect()
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let num_frames: usize = std::env::var("FRAME_BENCH_FRAMES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(200);

    println!("=== Frame Tab Render Pipeline Benchmark ===\n");

    // ── Load or generate primitives ──────────────────────────────────────
    let fixture_path = std::env::var("FRAME_BENCH_FIXTURE").ok();
    let primitives: Vec<PaintPrimitive>;

    if let Some(ref path) = fixture_path {
        println!("Loading fixture: {path}");
        let bytes = std::fs::read(path).expect("Failed to read fixture file");
        let (doc, _diag) =
            frame_import::import_figma_bytes_with_diagnostics(&bytes).expect("Import failed");
        let root = preview_scene_root(&doc);
        primitives = build_paint_primitives(&doc, root);
        println!(
            "  Loaded {} primitives from Figma document\n",
            primitives.len()
        );
    } else {
        let count = 2000;
        println!("No FRAME_BENCH_FIXTURE set, generating {count} synthetic rectangles\n");
        primitives = build_synthetic_primitives(count);
    }

    // ── Primitive analysis ─────────────────────────────────────────────
    println!("\n--- Primitive Analysis ---\n");
    analyze_primitives(&primitives);

    let content = primitive_bounds(&primitives);
    let content_w = (content.max_x - content.min_x).max(1.0);
    let content_h = (content.max_y - content.min_y).max(1.0);
    println!(
        "\nContent bounds: {:.0}x{:.0} (from ({:.0},{:.0}) to ({:.0},{:.0}))",
        content_w, content_h, content.min_x, content.min_y, content.max_x, content.max_y
    );

    let font_bytes = try_load_system_text_font();
    let font_ref = font_bytes.map(|b| TextFontRef {
        bytes: b.as_slice(),
        index: 0,
    });

    // ── 1. Scene build benchmark ─────────────────────────────────────────
    println!("\n--- Scene Build Benchmark ---\n");

    let scene_build = bench_scene_build(&primitives, font_ref, 20);
    scene_build.print();

    // Build the cached scene fragment (what the app caches in the signal)
    let mut cached_fragment = vello::Scene::new();
    {
        let mut painter = VelloScenePainter::new(&mut cached_fragment);
        paint_primitives_into_scene_with(&mut painter, &primitives, Affine::IDENTITY, font_ref);
    }

    // ── 2. Scene append benchmark ────────────────────────────────────────
    let sequence = pan_zoom_sequence(num_frames);

    println!("\n--- Scene Append Benchmark (CPU only, no GPU) ---\n");

    let append_result = bench_scene_append(&cached_fragment, &content, &sequence);
    append_result.print();

    // ── 3. Full GPU pipeline benchmarks ──────────────────────────────────
    let canvas_w = 1920u32;
    let canvas_h = 1080u32;
    let gpu_frames = num_frames.min(100);
    let gpu_sequence = &sequence[..gpu_frames];

    println!("\n--- Full GPU Pipeline: Area AA ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");

    let area_result = bench_gpu_pipeline(
        &cached_fragment,
        &content,
        gpu_sequence,
        canvas_w,
        canvas_h,
        vello::AaConfig::Area,
        vello::AaSupport::area_only(),
        "Full pipeline (Area AA)",
    );
    area_result.print();

    println!(
        "\n--- Full GPU Pipeline: MSAA 16x ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n"
    );

    let msaa_result = bench_gpu_pipeline(
        &cached_fragment,
        &content,
        gpu_sequence,
        canvas_w,
        canvas_h,
        vello::AaConfig::Msaa16,
        vello::AaSupport::all(),
        "Full pipeline (MSAA 16x)",
    );
    msaa_result.print();

    // ── 4. Per-phase breakdown ───────────────────────────────────────────
    bench_gpu_breakdown(&cached_fragment, &content, gpu_sequence, canvas_w, canvas_h);

    // ── Summary ──────────────────────────────────────────────────────────
    println!("\n=== Summary ===");
    println!("  Primitives:        {}", primitives.len());
    println!(
        "  Scene build:       {:.1}ms avg",
        scene_build.avg.as_secs_f64() * 1000.0
    );
    println!(
        "  Scene append:      {:.3}ms avg (CPU only)",
        append_result.avg.as_secs_f64() * 1000.0
    );
    println!(
        "  Area AA pipeline:  {:.1}ms avg ({:.0} fps)",
        area_result.avg.as_secs_f64() * 1000.0,
        1.0 / area_result.avg.as_secs_f64().max(1e-9)
    );
    println!(
        "  MSAA 16x pipeline: {:.1}ms avg ({:.0} fps)",
        msaa_result.avg.as_secs_f64() * 1000.0,
        1.0 / msaa_result.avg.as_secs_f64().max(1e-9)
    );
    let speedup = msaa_result.avg.as_secs_f64() / area_result.avg.as_secs_f64().max(1e-9);
    println!("  AA speedup:        {:.1}x (Area vs MSAA 16x)", speedup);
    println!(
        "\n  Target: full pipeline avg < 8.3ms (120fps)"
    );

    // ── 5. Pipelined benchmark (non-blocking poll, like vello examples) ─
    println!("\n--- Pipelined GPU Pipeline (non-blocking poll, {gpu_frames} frames) ---\n");

    let pipelined_result = bench_gpu_pipelined(
        &cached_fragment,
        &content,
        gpu_sequence,
        canvas_w,
        canvas_h,
    );
    pipelined_result.print();

    println!(
        "\n  Blocking vs pipelined speedup: {:.1}x",
        area_result.avg.as_secs_f64() / pipelined_result.avg.as_secs_f64().max(1e-9)
    );

    // ── 6. vello::util::RenderContext benchmark (same device setup as vello examples)
    println!("\n--- vello::util::RenderContext pipeline ({gpu_frames} frames) ---\n");

    let vello_ctx_result = bench_vello_render_context(
        &cached_fragment,
        &content,
        gpu_sequence,
        canvas_w,
        canvas_h,
    );
    vello_ctx_result.print();

    println!(
        "\n  wgpu_context vs vello::util speedup: {:.1}x",
        area_result.avg.as_secs_f64() / vello_ctx_result.avg.as_secs_f64().max(1e-9)
    );

    // ── 7. TRUE throughput benchmark ─────────────────────────────────────
    println!("\n--- TRUE Throughput Benchmark ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    bench_throughput(&cached_fragment, &content, gpu_sequence, canvas_w, canvas_h);

    // ── 8. Ablation study: effects-stripped and SVG-stripped ────────────
    println!("\n--- Ablation Study ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    {
        // A: Strip effects only
        let no_effects = strip_effects(&primitives);
        let mut no_effects_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut no_effects_frag);
            paint_primitives_into_scene_with(&mut painter, &no_effects, Affine::IDENTITY, font_ref);
        }
        let no_effects_result = bench_gpu_pipeline(
            &no_effects_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Without effects (blur/shadow stripped)",
        );

        // B: Strip SVGs only
        let no_svgs = strip_svgs(&primitives);
        let mut no_svgs_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut no_svgs_frag);
            paint_primitives_into_scene_with(&mut painter, &no_svgs, Affine::IDENTITY, font_ref);
        }
        let no_svgs_result = bench_gpu_pipeline(
            &no_svgs_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Without SVGs (svg_base64 stripped)",
        );

        // C: Strip both
        let no_both = strip_svgs(&strip_effects(&primitives));
        let mut no_both_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut no_both_frag);
            paint_primitives_into_scene_with(&mut painter, &no_both, Affine::IDENTITY, font_ref);
        }
        let no_both_result = bench_gpu_pipeline(
            &no_both_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Without effects AND SVGs",
        );

        // D: Replace all paths with rects (eliminate bezier path complexity)
        let rects_only = paths_to_rects(&strip_effects(&primitives));
        let mut rects_only_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut rects_only_frag);
            paint_primitives_into_scene_with(&mut painter, &rects_only, Affine::IDENTITY, font_ref);
        }
        let rects_only_result = bench_gpu_pipeline(
            &rects_only_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "All paths → rects (no bezier curves)",
        );

        // E: All fills → solid (eliminate image decoding + gradient compute)
        let solid_only = solidify_fills(&primitives);
        let mut solid_only_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut solid_only_frag);
            paint_primitives_into_scene_with(&mut painter, &solid_only, Affine::IDENTITY, font_ref);
        }
        let solid_only_result = bench_gpu_pipeline(
            &solid_only_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "All fills → solid (no images/gradients)",
        );

        // F: Strip image fills only (keep gradients)
        let no_images = strip_image_fills(&primitives);
        let mut no_images_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut no_images_frag);
            paint_primitives_into_scene_with(&mut painter, &no_images, Affine::IDENTITY, font_ref);
        }
        let no_images_result = bench_gpu_pipeline(
            &no_images_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "No image fills (gradients kept)",
        );

        // G: Everything solid + no effects + no SVGs (absolute minimum vello work through paint fn)
        let everything_solid = solidify_fills(&strip_svgs(&strip_effects(&primitives)));
        let mut everything_solid_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut everything_solid_frag);
            paint_primitives_into_scene_with(&mut painter, &everything_solid, Affine::IDENTITY, font_ref);
        }
        let everything_solid_result = bench_gpu_pipeline(
            &everything_solid_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Solid fills + no effects + no SVGs",
        );

        no_effects_result.print();
        println!();
        no_svgs_result.print();
        println!();
        no_both_result.print();
        println!();
        rects_only_result.print();
        println!();
        solid_only_result.print();
        println!();
        no_images_result.print();
        println!();
        everything_solid_result.print();

        println!("\n  === Comparison ===");
        println!("  Original (all):          {:.1}ms ({:.0} fps)",
            area_result.avg.as_secs_f64() * 1000.0, 1.0 / area_result.avg.as_secs_f64().max(1e-9));
        println!("  No effects:              {:.1}ms ({:.0} fps) — {:.1}x speedup",
            no_effects_result.avg.as_secs_f64() * 1000.0, 1.0 / no_effects_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / no_effects_result.avg.as_secs_f64().max(1e-9));
        println!("  No SVGs:                 {:.1}ms ({:.0} fps) — {:.1}x speedup",
            no_svgs_result.avg.as_secs_f64() * 1000.0, 1.0 / no_svgs_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / no_svgs_result.avg.as_secs_f64().max(1e-9));
        println!("  No effects + SVGs:       {:.1}ms ({:.0} fps) — {:.1}x speedup",
            no_both_result.avg.as_secs_f64() * 1000.0, 1.0 / no_both_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / no_both_result.avg.as_secs_f64().max(1e-9));
        println!("  Paths → rects:           {:.1}ms ({:.0} fps) — {:.1}x speedup",
            rects_only_result.avg.as_secs_f64() * 1000.0, 1.0 / rects_only_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / rects_only_result.avg.as_secs_f64().max(1e-9));
        println!("  No image fills:          {:.1}ms ({:.0} fps) — {:.1}x speedup",
            no_images_result.avg.as_secs_f64() * 1000.0, 1.0 / no_images_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / no_images_result.avg.as_secs_f64().max(1e-9));
        println!("  All solid fills:         {:.1}ms ({:.0} fps) — {:.1}x speedup",
            solid_only_result.avg.as_secs_f64() * 1000.0, 1.0 / solid_only_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / solid_only_result.avg.as_secs_f64().max(1e-9));
        println!("  Solid+no fx+no SVG:      {:.1}ms ({:.0} fps) — {:.1}x speedup",
            everything_solid_result.avg.as_secs_f64() * 1000.0, 1.0 / everything_solid_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / everything_solid_result.avg.as_secs_f64().max(1e-9));
    }

    // ── 9. Direct fills (bypass paint function) ────────────────────────
    println!("\n--- Direct Fills vs Paint Function ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    let direct_result = bench_direct_fills(&primitives, gpu_sequence, canvas_w, canvas_h);
    direct_result.print();
    println!();
    println!("  paint_primitives_into_scene_with: {:.1}ms ({:.0} fps)",
        area_result.avg.as_secs_f64() * 1000.0, 1.0 / area_result.avg.as_secs_f64().max(1e-9));
    println!("  direct scene.fill() (same rects): {:.1}ms ({:.0} fps)",
        direct_result.avg.as_secs_f64() * 1000.0, 1.0 / direct_result.avg.as_secs_f64().max(1e-9));
    println!("  Paint function overhead: {:.1}x",
        area_result.avg.as_secs_f64() / direct_result.avg.as_secs_f64().max(1e-9));

    // ── 10a. Layer impact analysis ─────────────────────────────────────
    println!("\n--- Compositor Layer Impact ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    {
        let (layer_count, clip_count) = count_layer_primitives(&primitives);
        println!("  Original primitives: {} LayerStart, {} ClipStart", layer_count, clip_count);

        // Build scene WITHOUT any layers/clips
        let no_layers = strip_layers_and_clips(&primitives);
        let (nl_layers, nl_clips) = count_layer_primitives(&no_layers);
        println!("  Stripped primitives: {} LayerStart, {} ClipStart", nl_layers, nl_clips);

        let mut no_layers_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut no_layers_frag);
            paint_primitives_into_scene_with(&mut painter, &no_layers, Affine::IDENTITY, font_ref);
        }
        let no_layers_result = bench_gpu_pipeline(
            &no_layers_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Without ALL layers & clips",
        );
        no_layers_result.print();
        println!();

        // Also strip effects + SVGs + layers/clips (absolute minimum through paint function)
        let minimal = strip_layers_and_clips(&strip_svgs(&strip_effects(&primitives)));
        let mut minimal_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut minimal_frag);
            paint_primitives_into_scene_with(&mut painter, &minimal, Affine::IDENTITY, font_ref);
        }
        let minimal_result = bench_gpu_pipeline(
            &minimal_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Minimal (no layers, clips, effects, or SVGs)",
        );
        minimal_result.print();
        println!();

        // Ultimate minimal: strip EVERYTHING AND convert paths→rects, through paint function
        let ultimate_min = strip_layers_and_clips(&strip_effects(&paths_to_rects(&primitives)));
        let mut ultimate_min_frag = vello::Scene::new();
        {
            let mut painter = VelloScenePainter::new(&mut ultimate_min_frag);
            paint_primitives_into_scene_with(&mut painter, &ultimate_min, Affine::IDENTITY, font_ref);
        }
        let ultimate_min_result = bench_gpu_pipeline(
            &ultimate_min_frag, &content, gpu_sequence, canvas_w, canvas_h,
            vello::AaConfig::Area, vello::AaSupport::area_only(),
            "Ultimate minimal (all rects, no layers/clips/effects)",
        );
        ultimate_min_result.print();
        println!();

        // Count total path segments in fill_paths
        let mut total_segments = 0usize;
        let mut total_fill_paths = 0usize;
        for p in &primitives {
            if let PaintPrimitive::Path { fill_paths, .. } = p {
                for fp in fill_paths {
                    total_fill_paths += 1;
                    total_segments += fp.matches('M').count()
                        + fp.matches('L').count()
                        + fp.matches('C').count()
                        + fp.matches('Q').count()
                        + fp.matches('A').count()
                        + fp.matches('Z').count()
                        + fp.matches('l').count()
                        + fp.matches('c').count()
                        + fp.matches('q').count()
                        + fp.matches('a').count()
                        + fp.matches('z').count()
                        + fp.matches('m').count();
                }
            }
        }
        println!("  Path complexity: {} fill_paths with ~{} total segments", total_fill_paths, total_segments);

        println!("\n  === Layer Impact Summary ===");
        println!("  Full paint function:     {:.1}ms ({:.0} fps)",
            area_result.avg.as_secs_f64() * 1000.0, 1.0 / area_result.avg.as_secs_f64().max(1e-9));
        println!("  No layers/clips:         {:.1}ms ({:.0} fps) — {:.1}x speedup",
            no_layers_result.avg.as_secs_f64() * 1000.0, 1.0 / no_layers_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / no_layers_result.avg.as_secs_f64().max(1e-9));
        println!("  Minimal through paint:   {:.1}ms ({:.0} fps) — {:.1}x speedup",
            minimal_result.avg.as_secs_f64() * 1000.0, 1.0 / minimal_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / minimal_result.avg.as_secs_f64().max(1e-9));
        println!("  Ultimate minimal (rects):{:.1}ms ({:.0} fps) — {:.1}x speedup",
            ultimate_min_result.avg.as_secs_f64() * 1000.0, 1.0 / ultimate_min_result.avg.as_secs_f64().max(1e-9),
            area_result.avg.as_secs_f64() / ultimate_min_result.avg.as_secs_f64().max(1e-9));
        println!("  Direct scene.fill():     {:.1}ms ({:.0} fps)",
            direct_result.avg.as_secs_f64() * 1000.0, 1.0 / direct_result.avg.as_secs_f64().max(1e-9));
    }

    // ── 10b. Same-device A/B comparison (eliminates device difference) ──
    println!("\n--- Same-Device Comparison ({gpu_frames} frames, {canvas_w}x{canvas_h}, blocking poll) ---\n");
    bench_same_device_comparison(&primitives, font_ref, gpu_sequence, canvas_w, canvas_h);

    // ── 10c. Render loop simulation (raster vs vector path) ────────────
    println!("\n--- Render Loop Simulation ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    bench_render_loop_simulation(
        &cached_fragment, canvas_w, canvas_h, &content,
        gpu_sequence, canvas_w, canvas_h,
    );

    // ── 10. Primitive scaling test ──────────────────────────────────────
    println!("\n--- Primitive Scaling (how vello cost grows, {canvas_w}x{canvas_h}) ---\n");
    bench_primitive_scaling(canvas_w, canvas_h);

    // ── 11. Minimal scene test (isolate vello fixed overhead) ─────────────
    println!("\n--- Minimal Scene Test (vello fixed overhead, {gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    bench_minimal_scene(canvas_w, canvas_h, gpu_frames);

    // ── 12. GPU Blit Path Benchmark (render-once, blit-per-frame) ────────
    println!("\n--- GPU Blit Path ({gpu_frames} frames, {canvas_w}x{canvas_h}) ---\n");
    bench_gpu_blit_path(&cached_fragment, &content, gpu_sequence, canvas_w, canvas_h);

    // ── 9. Resolution scaling test ───────────────────────────────────────
    println!("\n--- Resolution Scaling Test (Area AA, 50 frames each) ---\n");
    let short_seq = &sequence[..50.min(sequence.len())];
    for (w, h, label) in [
        (640, 480, "640x480 (small panel)"),
        (1280, 720, "1280x720 (medium)"),
        (1920, 1080, "1920x1080 (full HD)"),
        (2560, 1440, "2560x1440 (QHD)"),
        (3840, 2160, "3840x2160 (4K / Retina)"),
    ] {
        let result = bench_gpu_pipeline(
            &cached_fragment,
            &content,
            short_seq,
            w,
            h,
            vello::AaConfig::Area,
            vello::AaSupport::area_only(),
            label,
        );
        println!(
            "  {}: avg {:.1}ms ({:.0} fps)",
            label,
            result.avg.as_secs_f64() * 1000.0,
            1.0 / result.avg.as_secs_f64().max(1e-9),
        );
    }
    println!();
}
