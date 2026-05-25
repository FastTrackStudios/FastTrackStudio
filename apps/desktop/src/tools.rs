//! Tools page — offline file utilities.
//!
//! Two cards today:
//!
//! 1. **DAW file converter** — wraps `daw::file::convert`, which takes a
//!    foreign session file (.ptx / .ptf / .pts / .als / .aaf / .dawproject)
//!    and writes the equivalent .rpp next to it. The conversion runs on a
//!    blocking thread so the UI stays responsive on large sessions. The
//!    reverse direction (.rpp → foreign) is not offered: Pro Tools and
//!    Ableton both ship proprietary formats with no public writer.
//!
//! 2. **Keyflow chart render** — wraps the keyflow engraver. Loads a `.kf`
//!    source file, lays it out, and writes one SVG per page. Output is
//!    pure-Rust (no headless browser, no external CLI) so it works the
//!    same on any machine the desktop app builds on.

use std::path::PathBuf;

use dioxus::prelude::*;
use keyflow::engraver::export::svg::{SvgExportConfig, SvgSerializer};
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutResult, LayoutMode};
use keyflow::engraver::style::MStyle;
use keyflow::text::chart::parse_chart;

// ---------------------------------------------------------------------------
// DAW file conversion
// ---------------------------------------------------------------------------

/// Extensions accepted as conversion *inputs*. The display strings are what
/// the native file picker shows in the filter row.
const CONVERT_INPUT_EXTS: &[&str] = &["ptx", "ptf", "pts", "als", "aaf", "dawproject"];

#[derive(Clone, PartialEq)]
enum ConvertStatus {
    Idle,
    Converting,
    Done(String),
    Error(String),
}

#[component]
fn ConverterCard() -> Element {
    let mut input_path: Signal<Option<PathBuf>> = use_signal(|| None);
    let mut status = use_signal(|| ConvertStatus::Idle);

    let pick = move |_| {
        spawn(async move {
            if let Some(handle) = rfd::AsyncFileDialog::new()
                .add_filter("Supported sessions", CONVERT_INPUT_EXTS)
                .pick_file()
                .await
            {
                input_path.set(Some(handle.path().to_path_buf()));
                status.set(ConvertStatus::Idle);
            }
        });
    };

    let convert = move |_| {
        let Some(path) = input_path() else { return };
        status.set(ConvertStatus::Converting);
        spawn(async move {
            let out = path.with_extension("rpp");
            let in_path = path.clone();
            let out_path = out.clone();
            // `daw::file::convert` does file I/O + CPU work; run off the UI
            // thread so the window stays responsive on large sessions.
            let result = tokio::task::spawn_blocking(move || {
                daw::file::convert(&in_path, &out_path).map_err(|e| e.to_string())
            })
            .await;
            match result {
                Ok(Ok(())) => status.set(ConvertStatus::Done(out.to_string_lossy().to_string())),
                Ok(Err(e)) => status.set(ConvertStatus::Error(e)),
                Err(e) => status.set(ConvertStatus::Error(format!(
                    "conversion task panicked: {e}"
                ))),
            }
        });
    };

    let selected_label = match input_path() {
        Some(p) => p
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| p.to_string_lossy().to_string()),
        None => "No file selected".to_string(),
    };
    let converting = status() == ConvertStatus::Converting;

    rsx! {
        div { class: "border border-neutral-300 rounded-lg p-4 flex flex-col gap-3",
            h2 { class: "text-lg font-semibold", "DAW file converter" }
            p { class: "text-sm opacity-70",
                "Convert any supported session into a Reaper project (.rpp). The .rpp is written next to the source file. Supported inputs: .ptx / .ptf / .pts (Pro Tools), .als (Ableton), .aaf, .dawproject."
            }
            div { class: "flex items-center gap-3",
                button {
                    class: "px-3 py-2 rounded bg-neutral-200 hover:bg-neutral-300 text-sm",
                    onclick: pick,
                    "Choose session file…"
                }
                span { class: "text-sm truncate opacity-80", "{selected_label}" }
            }
            button {
                class: "px-3 py-2 rounded bg-blue-600 text-white text-sm disabled:opacity-50 w-fit",
                disabled: input_path().is_none() || converting,
                onclick: convert,
                {if converting { "Converting…" } else { "Convert to .rpp" }}
            }
            {match status() {
                ConvertStatus::Done(path) => rsx! { p { class: "text-sm text-green-600", "✓ Saved: {path}" } },
                ConvertStatus::Error(e) => rsx! { p { class: "text-sm text-red-600", "✗ {e}" } },
                _ => rsx! {}
            }}
        }
    }
}

// ---------------------------------------------------------------------------
// Keyflow chart rendering
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq)]
enum ChartStatus {
    Idle,
    Rendering,
    Done { pages: usize, dir: String },
    Error(String),
}

#[component]
fn ChartCard() -> Element {
    let mut input_path: Signal<Option<PathBuf>> = use_signal(|| None);
    let mut status = use_signal(|| ChartStatus::Idle);

    let pick = move |_| {
        spawn(async move {
            if let Some(handle) = rfd::AsyncFileDialog::new()
                .add_filter("Keyflow chart", &["kf"])
                .pick_file()
                .await
            {
                input_path.set(Some(handle.path().to_path_buf()));
                status.set(ChartStatus::Idle);
            }
        });
    };

    let render = move |_| {
        let Some(path) = input_path() else { return };
        status.set(ChartStatus::Rendering);
        spawn(async move {
            let result = tokio::task::spawn_blocking(move || render_chart_to_svgs(&path)).await;
            match result {
                Ok(Ok((pages, dir))) => status.set(ChartStatus::Done { pages, dir }),
                Ok(Err(e)) => status.set(ChartStatus::Error(e)),
                Err(e) => status.set(ChartStatus::Error(format!(
                    "chart render task panicked: {e}"
                ))),
            }
        });
    };

    let selected_label = match input_path() {
        Some(p) => p
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| p.to_string_lossy().to_string()),
        None => "No .kf selected".to_string(),
    };
    let rendering = status() == ChartStatus::Rendering;

    rsx! {
        div { class: "border border-neutral-300 rounded-lg p-4 flex flex-col gap-3",
            h2 { class: "text-lg font-semibold", "Keyflow chart render" }
            p { class: "text-sm opacity-70",
                "Render a Keyflow source file (.kf) into one or more page-sized SVGs using the engraver. SVGs are written next to the source file as <name>.p1.svg, <name>.p2.svg, …"
            }
            div { class: "flex items-center gap-3",
                button {
                    class: "px-3 py-2 rounded bg-neutral-200 hover:bg-neutral-300 text-sm",
                    onclick: pick,
                    "Choose .kf file…"
                }
                span { class: "text-sm truncate opacity-80", "{selected_label}" }
            }
            button {
                class: "px-3 py-2 rounded bg-blue-600 text-white text-sm disabled:opacity-50 w-fit",
                disabled: input_path().is_none() || rendering,
                onclick: render,
                {if rendering { "Rendering…" } else { "Render chart" }}
            }
            {match status() {
                ChartStatus::Done { pages, dir } => rsx! {
                    p { class: "text-sm text-green-600", "✓ Rendered {pages} page(s) → {dir}" }
                },
                ChartStatus::Error(e) => rsx! { p { class: "text-sm text-red-600", "✗ {e}" } },
                _ => rsx! {}
            }}
        }
    }
}

/// Parse a `.kf` file, lay it out as paginated A4, and write one SVG per
/// page next to the source. Returns `(page_count, output_dir)`.
///
/// Mirrors `keyflow-cli`'s `Commands::Svg` pipeline so the desktop output
/// matches what `kf svg` produces from the command line.
fn render_chart_to_svgs(path: &std::path::Path) -> Result<(usize, String), String> {
    let source =
        std::fs::read_to_string(path).map_err(|e| format!("read {}: {e}", path.display()))?;
    let chart = parse_chart(&source).map_err(|e| format!("parse: {e:#?}"))?;

    let font_bundle = ChartFontBundle::new()?;
    // `ChartLayoutEngine::new` borrows `&'static MStyle`; the CLI leaks one
    // box of the default style and we follow the same trick. The leak is
    // a single allocation for the lifetime of the desktop process.
    let style: &'static MStyle = Box::leak(Box::new(MStyle::new()));
    let engine = font_bundle.create_layout_engine(style);
    let layout: ChartLayoutResult = engine.layout_chart_with_config(
        &chart,
        &LayoutMode::paginated_a4(),
        &ChartLayoutConfig::master_rhythm().with_page_offsets(true),
    );

    let stem = path
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "chart".to_string());
    let dir = path
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());

    for (i, page) in layout.pages.iter().enumerate() {
        let config = with_embedded_fonts(
            SvgExportConfig::for_page(page.x_offset, page.y_offset, page.width, page.height),
            &font_bundle,
        );
        let mut serializer = SvgSerializer::new(config);
        let svg = serializer.serialize(&layout.scene);
        let out = dir.join(format!("{stem}.p{}.svg", i + 1));
        std::fs::write(&out, svg).map_err(|e| format!("write {}: {e}", out.display()))?;
    }
    Ok((layout.pages.len(), dir.to_string_lossy().to_string()))
}

/// Embed every named font the chart pipeline references so the resulting SVG
/// doesn't need the viewer to have any of them installed.
fn with_embedded_fonts(mut config: SvgExportConfig, bundle: &ChartFontBundle) -> SvgExportConfig {
    let leland = bundle.symbol_font_data().as_ref().clone();
    let leland_text = bundle.leland_text_font_data().as_ref().clone();
    let musejazz_text = bundle.text_font_data().as_ref().clone();
    let musejazz = bundle.musejazz_font_data().as_ref().clone();
    let chicago = bundle.chicago_font_data().as_ref().clone();
    let bravura = bundle.bravura_font_data().as_ref().clone();
    let freesans = bundle.freesans_font_data().as_ref().clone();

    config = config
        .with_embedded_font("Leland", leland.clone())
        .with_embedded_font("Bravura", bravura)
        .with_embedded_font("Leland Text", leland_text)
        .with_embedded_font("MuseJazz Text", musejazz_text)
        .with_embedded_font("MuseJazz", musejazz)
        .with_embedded_font("Chicago", chicago)
        .with_embedded_font("FreeSans", freesans)
        .with_embedded_font("Music", leland);
    config
}

// ---------------------------------------------------------------------------
// Page entry
// ---------------------------------------------------------------------------

#[component]
pub fn ToolsPage() -> Element {
    rsx! {
        div { class: "p-6 max-w-2xl mx-auto flex flex-col gap-4",
            h1 { class: "text-2xl font-bold", "Tools" }
            ConverterCard {}
            ChartCard {}
        }
    }
}
