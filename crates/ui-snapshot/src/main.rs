//! ui-snapshot — headless rendering + fuzzy PNG diff for fts-ui components.
//!
//! Uses the same Blitz pipeline as the panels ship with (Dioxus VirtualDom
//! → DioxusDocument → paint_scene) but routes output to a CPU
//! `anyrender_vello_cpu::VelloCpuImageRenderer` so the result is deterministic
//! across machines. Committed reference PNGs live under
//! `tests/reference/<scene>.png`; diffs are emitted to `target/ui-snapshots/`.
//!
//! Commands:
//!   ui-snapshot check        # render all scenes, fail on pixel diff
//!   ui-snapshot update       # regenerate + overwrite reference PNGs
//!   ui-snapshot render <name>  # render a single scene to target/ui-snapshots/

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyrender::{PaintScene as _, render_to_buffer};
use anyrender_vello_cpu::VelloCpuImageRenderer;
use blitz_dom::{Document, DocumentConfig};
use blitz_paint::paint_scene;
use blitz_traits::shell::{ColorScheme, Viewport};
use clap::{Parser, Subcommand};
use dioxus::prelude::*;
use dioxus_native::{DioxusDocument, prelude::*};
use kurbo::Rect;
use peniko::{Color as PColor, Fill};

mod scenes;

const TAILWIND_CSS: &str =
    include_str!("../../fts-extensions/assets/tailwind.css");
const FTS_THEME_CSS: &str =
    include_str!("../../fts-extensions/assets/fts-theme.css");
const COLOR_SCHEME_LIGHT: &str = ":root { color-scheme: light; }";

/// Scene descriptor — pairs a scene id with the component that paints it.
struct Scene {
    name: &'static str,
    width: u32,
    height: u32,
    /// Background: we paint a theme-aware background first, then the tree.
    background: PColor,
    render: fn() -> Element,
}

const SCENES: &[Scene] = &[
    Scene {
        name: "icons-default",
        width: 800,
        height: 400,
        background: PColor::WHITE,
        render: scenes::icons_default,
    },
];

#[derive(Parser)]
#[command(name = "ui-snapshot")]
struct Cli {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Render all scenes and diff against committed references.
    Check {
        /// Per-scene fuzzy tolerance (0.0–1.0). Matches Blitz WPT runner default.
        #[arg(long, default_value_t = 0.1)]
        tolerance: f32,
    },
    /// Regenerate all committed reference PNGs.
    Update,
    /// Render a single scene to target/ui-snapshots/<name>.png.
    Render { name: String },
}

fn main() -> std::process::ExitCode {
    let cli = Cli::parse();
    match cli.cmd {
        Command::Check { tolerance } => check(tolerance),
        Command::Update => update(),
        Command::Render { name } => render_one(&name),
    }
}

// ── rendering ────────────────────────────────────────────────────────────────

/// Render a Dioxus component to a tightly-packed RGBA8 buffer via Blitz + Vello CPU.
fn render_scene(scene: &Scene) -> Vec<u8> {
    let width = scene.width;
    let height = scene.height;

    // Wrap the scene's component in a page shell that attaches the Tailwind +
    // FTS theme stylesheets. This matches how the real REAPER panel sets up
    // its document (see reaper-dioxus/src/ui_test_panel.rs).
    let page_component = scene.render;
    let vdom = VirtualDom::new_with_props(Page, PageProps { inner: page_component });

    let viewport = Viewport::new(width, height, 1.0, ColorScheme::Light);
    let mut doc = DioxusDocument::new(
        vdom,
        DocumentConfig {
            viewport: Some(viewport),
            ..Default::default()
        },
    );

    doc.initial_build();
    doc.inner_mut().resolve(0.0);

    let bg = scene.background;
    let buffer = render_to_buffer::<VelloCpuImageRenderer, _>(
        |canvas| {
            // Opaque background — matches a document's initial paint.
            canvas.fill(
                Fill::NonZero,
                Default::default(),
                bg,
                Default::default(),
                &Rect::new(0.0, 0.0, width as f64, height as f64),
            );
            // `doc.inner()` returns a DocGuard (Ref-like); deref to &BaseDocument.
            paint_scene(canvas, &*doc.inner(), 1.0, width, height);
        },
        width,
        height,
    );
    buffer
}

#[derive(Clone, PartialEq, Props)]
struct PageProps {
    inner: fn() -> Element,
}

#[component]
fn Page(props: PageProps) -> Element {
    let inner = props.inner;
    rsx! {
        document::Style { {TAILWIND_CSS} }
        document::Style { {FTS_THEME_CSS} }
        // Deterministic color scheme — matches the live panel init chain.
        document::Style { {COLOR_SCHEME_LIGHT} }
        {inner()}
    }
}

fn write_png(path: &Path, buffer: &[u8], width: u32, height: u32) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create output dir");
    }
    let file = fs::File::create(path).expect("create png");
    let mut encoder = png::Encoder::new(file, width, height);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_depth(png::BitDepth::Eight);
    let mut w = encoder.write_header().expect("png header");
    w.write_image_data(buffer).expect("png data");
    w.finish().expect("png finish");
}

// ── commands ────────────────────────────────────────────────────────────────

fn reference_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/reference")
        .join(format!("{name}.png"))
}

fn output_path(name: &str) -> PathBuf {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    ws.join("target/ui-snapshots").join(format!("{name}.png"))
}

fn update() -> std::process::ExitCode {
    for scene in SCENES {
        println!("render {} ({}×{})...", scene.name, scene.width, scene.height);
        let buf = render_scene(scene);
        let path = reference_path(scene.name);
        write_png(&path, &buf, scene.width, scene.height);
        println!("  wrote {}", path.display());
    }
    std::process::ExitCode::SUCCESS
}

fn render_one(name: &str) -> std::process::ExitCode {
    let Some(scene) = SCENES.iter().find(|s| s.name == name) else {
        eprintln!("no scene named {name}");
        return std::process::ExitCode::FAILURE;
    };
    let buf = render_scene(scene);
    let path = output_path(name);
    write_png(&path, &buf, scene.width, scene.height);
    println!("wrote {}", path.display());
    std::process::ExitCode::SUCCESS
}

fn check(tolerance: f32) -> std::process::ExitCode {
    let mut any_failed = false;
    for scene in SCENES {
        print!("check {} ... ", scene.name);
        std::io::stdout().flush().ok();

        let buf = render_scene(scene);
        let actual_path = output_path(scene.name);
        write_png(&actual_path, &buf, scene.width, scene.height);

        let reference_path = reference_path(scene.name);
        if !reference_path.exists() {
            println!("NO REFERENCE — run `ui-snapshot update` to create");
            any_failed = true;
            continue;
        }

        match diff_png(&actual_path, &reference_path, tolerance) {
            Ok(diff_count) if diff_count == 0 => println!("ok"),
            Ok(diff_count) => {
                println!("FAIL ({diff_count} differing pixels above tolerance {tolerance})");
                let diff_path = actual_path.with_file_name(format!("{}-diff.png", scene.name));
                println!("    actual:    {}", actual_path.display());
                println!("    reference: {}", reference_path.display());
                println!("    diff:      {}", diff_path.display());
                any_failed = true;
            }
            Err(e) => {
                println!("ERROR: {e}");
                any_failed = true;
            }
        }
    }
    if any_failed {
        std::process::ExitCode::FAILURE
    } else {
        std::process::ExitCode::SUCCESS
    }
}

/// Runs `dify::diff::run` between two PNGs. Writes a `<actual>-diff.png` if any
/// pixels exceed the tolerance. Returns the count of differing pixels (0 means
/// identical or within tolerance).
fn diff_png(actual: &Path, reference: &Path, tolerance: f32) -> Result<usize, String> {
    let diff_path = actual.with_file_name(format!(
        "{}-diff.png",
        actual.file_stem().and_then(|s| s.to_str()).unwrap_or("scene")
    ));
    let left = actual.to_string_lossy().into_owned();
    let right = reference.to_string_lossy().into_owned();
    let out = diff_path.to_string_lossy().into_owned();
    let params = dify::diff::RunParams {
        left: &left,
        right: &right,
        output: &out,
        threshold: tolerance,
        output_image_base: Some(dify::cli::OutputImageBase::LeftImage),
        do_not_check_dimensions: false,
        detect_anti_aliased_pixels: true,
        blend_factor_of_unchanged_pixels: None,
        block_out_areas: None,
    };
    match dify::diff::run(&params) {
        // `Some(n)` = n differing pixels above threshold; `None` = no diff.
        Ok(Some(n)) if n > 0 => Ok(n as usize),
        Ok(_) => Ok(0),
        Err(e) => Err(format!("{e:?}")),
    }
}
