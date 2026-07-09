//! Keyflow Playground
//!
//! A standalone desktop app whose entire UI is the keyflow live chart editor:
//! syntax-highlighted source on the left, live engraved preview on the right,
//! plus PDF export. No gateway / REAPER / session wiring, so it launches
//! straight into the editor.
//!
//! Rendering note: the preview is **inline SVG** produced by the engraver, not
//! the WGPU overlay used by `keyflow_ui::ChartView`. The transparent-WebView-
//! over-WGPU overlay does not composite on Linux/webkitgtk (the WebView is
//! transparent but the GPU surface behind it never shows), so we engrave to SVG
//! and drop it into the DOM — which renders identically on every platform and
//! is also what the PDF export is built from.

use std::rc::Rc;

use dioxus::desktop::tao::dpi::LogicalSize;
use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;

use keyflow::engraver::export::svg::{SvgExportConfig, SvgSerializer};
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutResult, LayoutMode};
use keyflow::engraver::style::MStyle;
use keyflow::text::chart::parse_chart;
use keyflow_ui::components::HighlightedEditor;
use keyflow_ui::CHART_SOURCE;

fn main() {
    let filter = tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        tracing_subscriber::EnvFilter::new("info,wgpu_core=warn,wgpu_hal=warn,naga=warn")
    });
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_ansi(false)
        .init();

    tracing::info!("Starting Keyflow Playground");

    let cfg = Config::new().with_window(
        WindowBuilder::new()
            .with_title("Keyflow Playground")
            .with_inner_size(LogicalSize::new(1500.0, 950.0)),
    );

    LaunchBuilder::desktop().with_cfg(cfg).launch(App);
}

/// Engraving context reused across renders. The font bundle is expensive to
/// build (parses several embedded font files), so we build it once; the
/// `&'static MStyle` is leaked a single time for the engine's borrow.
#[derive(Clone)]
struct Engraver {
    bundle: Rc<ChartFontBundle>,
    style: &'static MStyle,
}

impl Engraver {
    fn new() -> Result<Self, String> {
        let bundle = Rc::new(ChartFontBundle::new()?);
        let style: &'static MStyle = Box::leak(Box::new(MStyle::new()));
        Ok(Self { bundle, style })
    }

    /// Parse + lay out the source as paginated A4 and serialize one SVG per page.
    fn page_svgs(&self, source: &str) -> Result<Vec<String>, String> {
        let chart = parse_chart(source).map_err(|e| format!("parse error: {e:#?}"))?;
        let engine = self.bundle.create_layout_engine(self.style);
        let layout: ChartLayoutResult = engine.layout_chart_with_config(
            &chart,
            &LayoutMode::paginated_a4(),
            &ChartLayoutConfig::master_rhythm().with_page_offsets(true),
        );
        let svgs = layout
            .pages
            .iter()
            .map(|page| {
                let config = self.embed_fonts(SvgExportConfig::for_page(
                    page.x_offset,
                    page.y_offset,
                    page.width,
                    page.height,
                ));
                SvgSerializer::new(config).serialize(&layout.scene)
            })
            .collect();
        Ok(svgs)
    }

    /// Render the source to a single PDF (one A4 page per chart page).
    fn pdf_bytes(&self, source: &str) -> Result<Vec<u8>, String> {
        use keyflow::engraver::export::pdf::PdfSerializer;

        let svgs = self.page_svgs(source)?;
        if svgs.is_empty() {
            return Err("nothing to export (empty chart)".to_string());
        }
        let symbol = self.bundle.symbol_font_data();
        let leland_text = self.bundle.leland_text_font_data();
        let musejazz_text = self.bundle.text_font_data();
        let musejazz = self.bundle.musejazz_font_data();
        let chicago = self.bundle.chicago_font_data();
        let bravura = self.bundle.bravura_font_data();
        let freesans = self.bundle.freesans_font_data();
        PdfSerializer::serialize_from_svg(
            &svgs,
            &[
                ("Leland", symbol.as_slice()),
                ("Music", symbol.as_slice()),
                ("Bravura", bravura.as_slice()),
                ("Leland Text", leland_text.as_slice()),
                ("MuseJazz Text", musejazz_text.as_slice()),
                ("MuseJazz", musejazz.as_slice()),
                ("Chicago", chicago.as_slice()),
                ("FreeSans", freesans.as_slice()),
            ],
        )
        .map_err(|e| format!("pdf export failed: {e}"))
    }

    /// Embed every named font the chart pipeline references so the SVG/PDF is
    /// self-contained (mirrors the desktop `ChartCard` tool).
    fn embed_fonts(&self, config: SvgExportConfig) -> SvgExportConfig {
        let leland = self.bundle.symbol_font_data().as_ref().clone();
        config
            .with_embedded_font("Leland", leland.clone())
            .with_embedded_font("Bravura", self.bundle.bravura_font_data().as_ref().clone())
            .with_embedded_font(
                "Leland Text",
                self.bundle.leland_text_font_data().as_ref().clone(),
            )
            .with_embedded_font(
                "MuseJazz Text",
                self.bundle.text_font_data().as_ref().clone(),
            )
            .with_embedded_font("MuseJazz", self.bundle.musejazz_font_data().as_ref().clone())
            .with_embedded_font("Chicago", self.bundle.chicago_font_data().as_ref().clone())
            .with_embedded_font("FreeSans", self.bundle.freesans_font_data().as_ref().clone())
            .with_embedded_font("Music", leland)
    }
}

#[component]
fn App() -> Element {
    // Build the engraver once. If fonts fail to load we surface the error in
    // the preview pane rather than panicking the app.
    let engraver = use_hook(|| Engraver::new());
    let engraver_for_export = engraver.clone();

    // Re-engrave whenever the source changes. `HighlightedEditor` debounces its
    // `on_change` (~150ms), so this doesn't fire on every keystroke.
    let render = use_memo(move || {
        let source = CHART_SOURCE.read().clone();
        match &engraver {
            Ok(eng) => eng.page_svgs(&source),
            Err(e) => Err(format!("engraver init failed: {e}")),
        }
    });

    let mut export_status = use_signal(String::new);

    rsx! {
        document::Stylesheet { href: asset!("/assets/tailwind.css") }
        div { class: "flex h-screen w-screen bg-background text-foreground overflow-hidden",

            // ── Left: source editor ────────────────────────────────────────
            div { class: "flex flex-col w-1/2 min-w-0 border-r border-border",
                div { class: "flex items-center justify-between px-3 py-2 border-b border-border shrink-0",
                    span { class: "text-sm font-medium", "Keyflow Source" }
                    button {
                        class: "px-3 py-1 text-xs rounded-md bg-primary text-primary-foreground hover:opacity-90",
                        onclick: move |_| {
                            let source = CHART_SOURCE.peek().clone();
                            match &engraver_for_export {
                                Ok(eng) => match eng.pdf_bytes(&source) {
                                    Ok(bytes) => {
                                        if let Some(path) = rfd::FileDialog::new()
                                            .set_file_name("chart.pdf")
                                            .add_filter("PDF", &["pdf"])
                                            .save_file()
                                        {
                                            match std::fs::write(&path, &bytes) {
                                                Ok(()) => export_status
                                                    .set(format!("Saved {}", path.display())),
                                                Err(e) => export_status.set(format!("Write failed: {e}")),
                                            }
                                        }
                                    }
                                    Err(e) => export_status.set(e),
                                },
                                Err(e) => export_status.set(format!("engraver init failed: {e}")),
                            }
                        },
                        "Export PDF"
                    }
                }
                // Must be a flex column: HighlightedEditor's root is `flex-1`
                // with an `absolute inset-0` textarea, so it needs a flex parent
                // with a definite height or the textarea collapses to nothing.
                div { class: "flex-1 min-h-0 flex flex-col",
                    HighlightedEditor {
                        source: CHART_SOURCE.read().clone(),
                        on_change: move |v: String| *CHART_SOURCE.write() = v,
                    }
                }
                if !export_status.read().is_empty() {
                    div { class: "px-3 py-1.5 text-xs text-muted-foreground border-t border-border shrink-0 truncate",
                        "{export_status}"
                    }
                }
            }

            // ── Right: live engraved preview (inline SVG) ──────────────────
            div { class: "w-1/2 min-w-0 overflow-auto bg-neutral-200 p-6 flex flex-col items-center gap-6",
                match &*render.read() {
                    Ok(pages) if !pages.is_empty() => rsx! {
                        for (i, svg) in pages.iter().enumerate() {
                            div {
                                key: "{i}",
                                class: "bg-white shadow-lg",
                                dangerous_inner_html: "{svg}",
                            }
                        }
                    },
                    Ok(_) => rsx! {
                        p { class: "text-sm text-neutral-500 mt-8", "Empty chart — start typing on the left." }
                    },
                    Err(e) => rsx! {
                        pre { class: "text-xs text-red-600 whitespace-pre-wrap font-mono mt-8 max-w-full",
                            "{e}"
                        }
                    },
                }
            }
        }
    }
}
