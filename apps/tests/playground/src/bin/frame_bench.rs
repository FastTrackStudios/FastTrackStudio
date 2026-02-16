//! Headless benchmark for Frame tab raster-snapshot caching.
//!
//! Measures vector-replay vs raster-blit render paths, simulating pan/zoom.
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

use anyrender::{ImageRenderer, PaintScene};
use anyrender_vello::VelloImageRenderer;
use frame_ui::{
    anyrender_renderer::{BlendMix, FillPaint, Rgba},
    build_paint_primitives, paint_primitives_into_scene_with, PaintPrimitive, TextFontRef,
};
use kurbo::Affine;
use peniko::{Blob, Color, Fill, ImageAlphaType, ImageBrush, ImageData, ImageFormat};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Helpers duplicated from main.rs (not exported)
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
            PaintPrimitive::LayerStart { x, y, width, height, .. }
            | PaintPrimitive::ClipStart { x, y, width, height, .. }
            | PaintPrimitive::Rect { x, y, width, height, .. }
            | PaintPrimitive::Path { x, y, width, height, .. } => (*x, *y, *width, *height),
            PaintPrimitive::Text { x, y, font_size, .. } => (*x, *y, 200.0, *font_size as f64),
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
// Synthetic fixture when no real Figma file is available
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

/// Generates a sequence of (pan_x, pan_y, zoom) values simulating a user
/// panning right then zooming in, then panning left while zooming out.
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
// Benchmark routines
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

/// Benchmark: replay full vector scene via append_scene for each frame.
fn bench_vector_replay(
    scene_fragment: &anyrender::Scene,
    content: &PrimitiveBounds,
    viewport: (f64, f64, f64, f64), // (x, y, w, h)
    sequence: &[(f64, f64, f64)],
) -> BenchResult {
    let pad = 16.0;
    let (bx, by, _bw, _bh) = viewport;
    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((bx + pad + pan_x, by + pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let start = Instant::now();
        let mut frame_scene = anyrender::Scene::new();
        // Background fill
        frame_scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &kurbo::Rect::new(bx, by, bx + _bw, by + _bh),
        );
        frame_scene.append_scene(scene_fragment.clone(), transform);
        let elapsed = start.elapsed();
        times.push(elapsed);
    }

    summarize("Vector replay (append_scene)", times)
}

/// Benchmark: draw cached raster image for each frame.
fn bench_raster_blit(
    raster: &ImageBrush,
    content: &PrimitiveBounds,
    viewport: (f64, f64, f64, f64),
    sequence: &[(f64, f64, f64)],
) -> BenchResult {
    let pad = 16.0;
    let (bx, by, _bw, _bh) = viewport;
    let content_w = (content.max_x - content.min_x).max(1.0);
    let content_h = (content.max_y - content.min_y).max(1.0);
    let mut times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let img_transform = Affine::translate((bx + pad + pan_x, by + pad + pan_y))
            * Affine::scale(zoom)
            * Affine::scale_non_uniform(
                content_w / raster.image.width as f64,
                content_h / raster.image.height as f64,
            );

        let start = Instant::now();
        let mut frame_scene = anyrender::Scene::new();
        frame_scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(5, 6, 12),
            None,
            &kurbo::Rect::new(bx, by, bx + _bw, by + _bh),
        );
        frame_scene.draw_image(raster.as_ref(), img_transform);
        let elapsed = start.elapsed();
        times.push(elapsed);
    }

    summarize("Raster blit (draw_image)", times)
}

/// Benchmark: full GPU render via VelloImageRenderer for both paths.
fn bench_gpu_render(
    primitives: &[PaintPrimitive],
    raster: &ImageBrush,
    content: &PrimitiveBounds,
    font_ref: Option<TextFontRef<'_>>,
    sequence: &[(f64, f64, f64)],
    canvas_w: u32,
    canvas_h: u32,
) -> (BenchResult, BenchResult) {
    let pad = 16.0;
    let content_w = (content.max_x - content.min_x).max(1.0);
    let content_h = (content.max_y - content.min_y).max(1.0);

    // GPU vector path
    let mut renderer = VelloImageRenderer::new(canvas_w, canvas_h);
    let mut buf = Vec::new();
    let mut vector_times = Vec::with_capacity(sequence.len());

    for &(pan_x, pan_y, zoom) in sequence {
        let transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::translate((-content.min_x, -content.min_y));

        let start = Instant::now();
        renderer.render_to_vec(
            |painter| {
                painter.fill(
                    Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(5, 6, 12),
                    None,
                    &kurbo::Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
                );
                paint_primitives_into_scene_with(painter, primitives, transform, font_ref);
            },
            &mut buf,
        );
        vector_times.push(start.elapsed());
    }

    // GPU raster path
    let mut raster_times = Vec::with_capacity(sequence.len());
    for &(pan_x, pan_y, zoom) in sequence {
        let img_transform = Affine::translate((pad + pan_x, pad + pan_y))
            * Affine::scale(zoom)
            * Affine::scale_non_uniform(
                content_w / raster.image.width as f64,
                content_h / raster.image.height as f64,
            );

        let start = Instant::now();
        renderer.render_to_vec(
            |painter| {
                painter.fill(
                    Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgb8(5, 6, 12),
                    None,
                    &kurbo::Rect::new(0.0, 0.0, canvas_w as f64, canvas_h as f64),
                );
                painter.draw_image(raster.as_ref(), img_transform);
            },
            &mut buf,
        );
        raster_times.push(start.elapsed());
    }

    (
        summarize("GPU vector (full render_to_vec)", vector_times),
        summarize("GPU raster (image blit render_to_vec)", raster_times),
    )
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let num_frames: usize = std::env::var("FRAME_BENCH_FRAMES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(200);

    println!("=== Frame Tab Raster Snapshot Benchmark ===\n");

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
        println!("  Loaded {} primitives from Figma document\n", primitives.len());
    } else {
        let count = 2000;
        println!("No FRAME_BENCH_FIXTURE set, generating {count} synthetic rectangles\n");
        primitives = build_synthetic_primitives(count);
    }

    let content = primitive_bounds(&primitives);
    let content_w = (content.max_x - content.min_x).max(1.0);
    let content_h = (content.max_y - content.min_y).max(1.0);
    println!("Content bounds: {:.0}x{:.0} (from ({:.0},{:.0}) to ({:.0},{:.0}))",
        content_w, content_h, content.min_x, content.min_y, content.max_x, content.max_y);

    // ── Build vector scene fragment ──────────────────────────────────────
    let font_bytes = try_load_system_text_font();
    let font_ref = font_bytes.map(|b| TextFontRef {
        bytes: b.as_slice(),
        index: 0,
    });

    println!("Building vector scene fragment...");
    let build_start = Instant::now();
    let mut scene_fragment = anyrender::Scene::new();
    paint_primitives_into_scene_with(&mut scene_fragment, &primitives, Affine::IDENTITY, font_ref);
    let vector_build_time = build_start.elapsed();
    let vector_cmd_count = scene_fragment.commands.len();
    println!("  Vector scene: {} commands, built in {:.1}ms",
        vector_cmd_count, vector_build_time.as_secs_f64() * 1000.0);

    // ── Build raster snapshot ────────────────────────────────────────────
    let raster_w = (content_w.ceil() as u32).clamp(1, 4096);
    let raster_h = (content_h.ceil() as u32).clamp(1, 4096);
    let scale_x = raster_w as f64 / content_w;
    let scale_y = raster_h as f64 / content_h;

    println!("Building raster snapshot ({raster_w}x{raster_h})...");
    let raster_start = Instant::now();
    let raster_transform = Affine::scale_non_uniform(scale_x, scale_y)
        * Affine::translate((-content.min_x, -content.min_y));
    let mut img_renderer = VelloImageRenderer::new(raster_w, raster_h);
    let mut rgba_buf = Vec::new();
    img_renderer.render_to_vec(
        |painter| {
            paint_primitives_into_scene_with(painter, &primitives, raster_transform, font_ref);
        },
        &mut rgba_buf,
    );
    let raster_build_time = raster_start.elapsed();
    println!("  Raster snapshot: {}x{} ({:.1} KB), built in {:.1}ms",
        raster_w, raster_h,
        rgba_buf.len() as f64 / 1024.0,
        raster_build_time.as_secs_f64() * 1000.0);

    let raster_brush = ImageBrush::new(ImageData {
        data: Blob::from(rgba_buf),
        format: ImageFormat::Rgba8,
        alpha_type: ImageAlphaType::AlphaPremultiplied,
        width: raster_w,
        height: raster_h,
    });

    // Verify raster path uses fewer commands
    let mut raster_scene_test = anyrender::Scene::new();
    raster_scene_test.draw_image(raster_brush.as_ref(), Affine::IDENTITY);
    let raster_cmd_count = raster_scene_test.commands.len();
    println!("  Raster draw_image: {} command(s) vs {} vector commands ({:.0}x reduction)\n",
        raster_cmd_count, vector_cmd_count,
        vector_cmd_count as f64 / raster_cmd_count.max(1) as f64);

    // ── Simulate pan/zoom ────────────────────────────────────────────────
    let sequence = pan_zoom_sequence(num_frames);
    let viewport = (0.0, 40.0, 1920.0, 1040.0); // simulated window bounds

    println!("--- Recording-backend benchmarks ({num_frames} frames, no GPU) ---\n");

    let vector_result = bench_vector_replay(&scene_fragment, &content, viewport, &sequence);
    vector_result.print();
    println!();

    let raster_result = bench_raster_blit(&raster_brush, &content, viewport, &sequence);
    raster_result.print();
    println!();

    let speedup = vector_result.avg.as_secs_f64() / raster_result.avg.as_secs_f64().max(1e-9);
    println!("  Recording-backend speedup: {:.1}x faster with raster\n", speedup);

    // ── GPU benchmarks ───────────────────────────────────────────────────
    let gpu_frames = num_frames.min(50); // GPU renders are slower, use fewer frames
    let gpu_sequence = &sequence[..gpu_frames];
    let canvas_w = 1920u32;
    let canvas_h = 1080u32;

    println!("--- GPU benchmarks ({gpu_frames} frames, actual VelloImageRenderer) ---\n");

    let (gpu_vector, gpu_raster) = bench_gpu_render(
        &primitives,
        &raster_brush,
        &content,
        font_ref,
        gpu_sequence,
        canvas_w,
        canvas_h,
    );
    gpu_vector.print();
    println!();
    gpu_raster.print();
    println!();

    let gpu_speedup = gpu_vector.avg.as_secs_f64() / gpu_raster.avg.as_secs_f64().max(1e-9);
    println!("  GPU speedup: {:.1}x faster with raster\n", gpu_speedup);

    // ── Summary ──────────────────────────────────────────────────────────
    println!("=== Summary ===");
    println!("  Primitives:        {}", primitives.len());
    println!("  Vector commands:   {}", vector_cmd_count);
    println!("  Raster commands:   {}", raster_cmd_count);
    println!("  Vector build:      {:.1}ms", vector_build_time.as_secs_f64() * 1000.0);
    println!("  Raster build:      {:.1}ms", raster_build_time.as_secs_f64() * 1000.0);
    println!("  Recording speedup: {:.1}x", speedup);
    println!("  GPU speedup:       {:.1}x", gpu_speedup);
    println!("  Target: raster path should be >5x faster during pan/zoom");
}
