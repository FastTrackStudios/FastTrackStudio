//! Chart Editor Component
//!
//! Interactive split-view editor with live chart preview.

use dioxus::prelude::*;
use keyflow::text::highlighting::{HighlightKind, Highlighter};

/// Preview mode - Snippet (content-sized) or Page (A4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreviewMode {
    Snippet,
    Page,
}

/// Static chart renderer for non-interactive display.
///
/// Renders the chart as a static SVG image: generates the SVG once on mount from
/// the initial `source` value and displays it as an `<img>` on a clean white page
/// (no pan/zoom controls or gray background). Content changes do not trigger
/// re-renders — intentional for chart browsers and preview cards. Invalid or
/// incomplete source falls back to a blank white page rather than an error state.
#[component]
pub fn StaticChartRenderer(
    source: Signal<String>,
    mode: Signal<PreviewMode>,
    canvas_id: Option<String>,
    fixed_layout_width: Option<f64>,
) -> Element {
    let _ = canvas_id; // unused in SVG path
    let svg_url: Signal<Option<String>> = use_signal(|| None);

    use_effect(move || {
        // peek() — no reactive subscription, so this only runs once on mount
        let source_text = source.peek().clone();
        let is_snippet = *mode.peek() == PreviewMode::Snippet;
        let layout_width = fixed_layout_width.unwrap_or(800.0);
        let mut url_out = svg_url;

        spawn(async move {
            let svg = (|| -> Result<String, String> {
                use crate::renderer::ChartLayoutManager;
                let mut manager = ChartLayoutManager::new()?;
                let chart = if source_text.trim().is_empty() {
                    None
                } else {
                    keyflow::parse(&source_text)
                        .ok()
                        .filter(|c| !c.sections.is_empty())
                };
                if let Some(chart) = chart {
                    manager.layout_chart_for_export(&chart, layout_width, is_snippet);
                    manager.export_to_svg()
                } else {
                    Ok(r#"<svg xmlns="http://www.w3.org/2000/svg" width="595" height="50"><rect width="100%" height="100%" fill="white"/></svg>"#.to_string())
                }
            })();

            #[cfg(target_arch = "wasm32")]
            if let Ok(svg_str) = svg {
                use wasm_bindgen::JsValue;
                use web_sys::{Blob, BlobPropertyBag, Url};
                let parts = js_sys::Array::new();
                parts.push(&JsValue::from_str(&svg_str));
                let opts = BlobPropertyBag::new();
                opts.set_type("image/svg+xml;charset=utf-8");
                if let Ok(blob) = Blob::new_with_str_sequence_and_options(&parts, &opts) {
                    if let Ok(url) = Url::create_object_url_with_blob(&blob) {
                        url_out.set(Some(url));
                    }
                }
            }
        });
    });

    rsx! {
        div {
            class: "relative w-full h-full",
            style: "background: white;",
            if let Some(url) = svg_url.read().as_ref() {
                img {
                    src: "{url}",
                    class: "w-full h-full object-contain",
                    alt: "Chart preview",
                }
            }
        }
    }
}

/// Syntax-highlighted code editor for keyflow notation.
///
/// Uses a layered approach with a transparent textarea for input and
/// a highlighted display layer behind it. Both layers must have
/// identical text styling for proper alignment.
#[component]
pub fn HighlightedEditor(
    value: String,
    on_change: EventHandler<String>,
    placeholder: &'static str,
    /// Optional unique ID for the textarea element. Defaults to "keyflow-editor-textarea".
    textarea_id: Option<String>,
) -> Element {
    // Track scroll position to sync layers
    let mut scroll_top = use_signal(|| 0.0_f64);
    let mut scroll_left = use_signal(|| 0.0_f64);

    // Unique ID for the textarea to query scroll position
    let textarea_id = textarea_id.unwrap_or_else(|| "keyflow-editor-textarea".to_string());

    // Common text styling - MUST match exactly between textarea and highlight layer
    // Using explicit line-height to ensure pixel-perfect alignment
    let text_style = "font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; font-size: 14px; line-height: 21px; white-space: pre; tab-size: 4;";

    rsx! {
        div {
            class: "relative w-full h-full",

            // Highlighted display layer (behind textarea)
            div {
                id: "keyflow-editor-highlight",
                class: "absolute inset-0 p-4 overflow-hidden pointer-events-none bg-background",
                style: "{text_style}",

                // Inner container with scroll offset
                div {
                    style: "transform: translate(-{scroll_left}px, -{scroll_top}px);",

                    HighlightedCode { source: value.clone() }
                }
            }

            // Transparent textarea for actual input (on top)
            textarea {
                id: "{textarea_id}",
                class: "absolute inset-0 w-full h-full p-4 resize-none focus:outline-none bg-transparent text-transparent caret-foreground z-10 overflow-auto",
                style: "{text_style}",
                value: "{value}",
                spellcheck: false,
                placeholder: "{placeholder}",
                oninput: {
                    let textarea_id = textarea_id.clone();
                    move |evt| {
                        on_change.call(evt.value());
                        // Update scroll position after input
                        #[cfg(target_arch = "wasm32")]
                        {
                            use wasm_bindgen::JsCast;
                            if let Some(window) = web_sys::window() {
                                if let Some(document) = window.document() {
                                    if let Some(elem) = document.get_element_by_id(&textarea_id) {
                                        if let Ok(html_elem) = elem.dyn_into::<web_sys::HtmlElement>() {
                                            scroll_top.set(html_elem.scroll_top() as f64);
                                            scroll_left.set(html_elem.scroll_left() as f64);
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                onscroll: {
                    let textarea_id = textarea_id.clone();
                    move |_evt| {
                        // Sync scroll with highlighted layer
                        #[cfg(target_arch = "wasm32")]
                        {
                            use wasm_bindgen::JsCast;
                            if let Some(window) = web_sys::window() {
                                if let Some(document) = window.document() {
                                    if let Some(elem) = document.get_element_by_id(&textarea_id) {
                                        if let Ok(html_elem) = elem.dyn_into::<web_sys::HtmlElement>() {
                                            scroll_top.set(html_elem.scroll_top() as f64);
                                            scroll_left.set(html_elem.scroll_left() as f64);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Renders highlighted keyflow source code.
///
/// Renders each line as plain text with inline spans for highlighting.
/// Uses newline characters to separate lines (matching textarea behavior).
#[component]
fn HighlightedCode(source: String) -> Element {
    // Split into lines but preserve the structure
    let lines: Vec<&str> = source.split('\n').collect();

    rsx! {
        // Use a single pre-like container to match textarea rendering
        div {
            class: "text-foreground",

            for (idx, line) in lines.iter().enumerate() {
                // Render each line
                HighlightedLine { line: line.to_string() }
                // Add newline between lines (except after last)
                if idx < lines.len() - 1 {
                    "\n"
                }
            }
        }
    }
}

/// Renders a single highlighted line of keyflow notation.
#[component]
fn HighlightedLine(line: String) -> Element {
    let spans = Highlighter::highlight_line(&line);

    // Empty line - render empty span to preserve height
    if line.is_empty() {
        return rsx! { span { "" } };
    }

    if spans.is_empty() {
        // No highlighting - render as plain text
        return rsx! { span { "{line}" } };
    }

    // Build the highlighted segments
    let mut segments: Vec<Element> = Vec::new();
    let mut last_end = 0;

    for span in &spans {
        let start = span.span.start;
        let end = span.span.start + span.span.len;

        // Add any unhighlighted text before this span
        if start > last_end {
            let text = &line[last_end..start];
            segments.push(rsx! { span { "{text}" } });
        }

        // Add the highlighted span
        let text = &line[start..end.min(line.len())];
        let class = highlight_class(span.kind);
        segments.push(rsx! {
            span { class: "{class}", "{text}" }
        });

        last_end = end;
    }

    // Add any remaining text
    if last_end < line.len() {
        let text = &line[last_end..];
        segments.push(rsx! { span { "{text}" } });
    }

    // Return inline spans (no block element wrapper)
    rsx! {
        for segment in segments {
            {segment}
        }
    }
}

/// Export button component for downloading chart as SVG or PDF.
#[component]
pub fn ExportButton(source: Signal<String>) -> Element {
    let mut is_exporting = use_signal(|| false);
    let mut show_dropdown = use_signal(|| false);

    let mut do_export = move |format: &'static str| {
        #[cfg(target_arch = "wasm32")]
        {
            use crate::renderer::ChartLayoutManager;
            use wasm_bindgen::JsCast;

            is_exporting.set(true);
            show_dropdown.set(false);
            let source_text = source.read().clone();

            spawn(async move {
                // Parse the chart
                let chart = match keyflow::parse(source_text.as_str()) {
                    Ok(c) => c,
                    Err(e) => {
                        tracing::error!("Failed to parse chart for export: {}", e);
                        is_exporting.set(false);
                        return;
                    }
                };

                // Create layout manager and layout the chart
                let mut manager = match ChartLayoutManager::new() {
                    Ok(m) => m,
                    Err(e) => {
                        tracing::error!("Failed to create layout manager: {}", e);
                        is_exporting.set(false);
                        return;
                    }
                };

                // Layout in page mode for export (A4, no page offsets for clean export)
                manager.layout_chart_for_export(&chart, 595.0, false);

                // Export based on format
                let title = chart.metadata.title.as_deref().unwrap_or("chart");
                let base_filename = title.replace(' ', "_");

                let (content, mime_type, extension): (Vec<u8>, &str, &str) = match format {
                    "pdf" => {
                        // Use SVG-to-PDF conversion for vector output
                        match manager.export_multi_page_pdf_via_svg() {
                            Ok(bytes) => (bytes, "application/pdf", "pdf"),
                            Err(e) => {
                                tracing::error!("Failed to export PDF: {}", e);
                                is_exporting.set(false);
                                return;
                            }
                        }
                    }
                    _ => {
                        // SVG export - use per-page export for LilyPond-style output
                        match manager.export_pages_to_svg() {
                            Ok(pages) => {
                                if pages.len() == 1 {
                                    // Single page - export directly
                                    (
                                        pages.into_iter().next().unwrap().into_bytes(),
                                        "image/svg+xml",
                                        "svg",
                                    )
                                } else {
                                    // Multi-page - create a zip file containing all pages
                                    match create_svg_zip(&pages, &base_filename) {
                                        Ok(zip_bytes) => (zip_bytes, "application/zip", "zip"),
                                        Err(e) => {
                                            tracing::error!("Failed to create zip: {}", e);
                                            is_exporting.set(false);
                                            return;
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                tracing::error!("Failed to export SVG: {}", e);
                                is_exporting.set(false);
                                return;
                            }
                        }
                    }
                };

                // Trigger download
                if let Some(window) = web_sys::window() {
                    if let Some(document) = window.document() {
                        // Create blob from bytes
                        let uint8_array = js_sys::Uint8Array::from(content.as_slice());
                        let array = js_sys::Array::new();
                        array.push(&uint8_array);

                        let options = web_sys::BlobPropertyBag::new();
                        options.set_type(mime_type);

                        if let Ok(blob) =
                            web_sys::Blob::new_with_u8_array_sequence_and_options(&array, &options)
                        {
                            if let Ok(url) = web_sys::Url::create_object_url_with_blob(&blob) {
                                // Create download link
                                if let Ok(link) = document.create_element("a") {
                                    let link = link.unchecked_into::<web_sys::HtmlAnchorElement>();
                                    link.set_href(&url);

                                    // Use pre-computed filename
                                    let filename = format!("{}.{}", base_filename, extension);
                                    link.set_download(&filename);

                                    // Trigger download
                                    link.click();

                                    // Cleanup
                                    let _ = web_sys::Url::revoke_object_url(&url);
                                }
                            }
                        }
                    }
                }

                is_exporting.set(false);
            });
        }

        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = format;
            tracing::warn!("Export is only available in the browser");
        }
    };

    let export_svg = move |_| do_export("svg");
    let export_pdf = move |_| do_export("pdf");

    rsx! {
        div {
            class: "relative",

            // Main button with dropdown
            div {
                class: "flex",

                // Export button
                button {
                    class: "flex items-center gap-1.5 px-3 py-1.5 rounded-l-md text-xs font-medium bg-primary text-primary-foreground hover:bg-primary/90 transition-colors disabled:opacity-50",
                    disabled: *is_exporting.read(),
                    onclick: export_pdf,  // Default to PDF

                    if *is_exporting.read() {
                        lucide_dioxus::LoaderCircle { class: "w-3.5 h-3.5 animate-spin" }
                        "Exporting..."
                    } else {
                        lucide_dioxus::Download { class: "w-3.5 h-3.5" }
                        "Export"
                    }
                }

                // Dropdown toggle
                button {
                    class: "flex items-center px-1.5 py-1.5 rounded-r-md text-xs font-medium bg-primary text-primary-foreground hover:bg-primary/90 transition-colors border-l border-primary-foreground/20 disabled:opacity-50",
                    disabled: *is_exporting.read(),
                    aria_label: "Choose export format",
                    onclick: move |_| {
                        let current = *show_dropdown.read();
                        show_dropdown.set(!current);
                    },

                    lucide_dioxus::ChevronDown { class: "w-3.5 h-3.5" }
                }
            }

            // Dropdown menu
            if *show_dropdown.read() {
                div {
                    class: "absolute top-full right-0 mt-1 bg-popover border border-border rounded-md shadow-lg z-50 min-w-[120px]",

                    button {
                        class: "w-full flex items-center gap-2 px-3 py-2 text-xs text-left hover:bg-accent transition-colors rounded-t-md",
                        onclick: export_pdf,

                        lucide_dioxus::FileText { class: "w-3.5 h-3.5" }
                        "PDF"
                    }

                    button {
                        class: "w-full flex items-center gap-2 px-3 py-2 text-xs text-left hover:bg-accent transition-colors rounded-b-md",
                        onclick: export_svg,

                        lucide_dioxus::FileCode { class: "w-3.5 h-3.5" }
                        "SVG"
                    }
                }
            }
        }
    }
}

/// Map highlight kinds to Tailwind CSS classes.
///
/// Design decisions:
/// - Root + Accidental use the same color (Ab = same color for A and b)
/// - Quality + Extension use the same color (maj9 = same color for maj and 9)
/// - Barlines (MeasureSeparator) are gray/muted
/// - Unknown/unparsed text is muted, not red (avoids visual noise)
fn highlight_class(kind: HighlightKind) -> &'static str {
    match kind {
        // Chord components - Root and Accidental same color
        HighlightKind::Root => "text-sky-400 font-semibold",
        HighlightKind::Accidental => "text-sky-400", // Same as Root
        HighlightKind::ScaleDegree => "text-purple-400 font-semibold",
        HighlightKind::RomanNumeral => "text-purple-400 font-semibold",

        // Quality and Extension same color
        HighlightKind::Quality => "text-amber-400",
        HighlightKind::Extension => "text-amber-400", // Same as Quality
        HighlightKind::Modifier => "text-yellow-300",

        // Bass note - slightly different shade
        HighlightKind::Bass => "text-sky-300",
        HighlightKind::BassSlash => "text-gray-500",

        // Rhythm notation
        HighlightKind::Duration => "text-violet-400",
        HighlightKind::SlashRhythm => "text-gray-400",
        HighlightKind::Push => "text-rose-400",
        HighlightKind::Pull => "text-rose-400",
        HighlightKind::Triplet => "text-rose-300",
        HighlightKind::Dot => "text-violet-400",

        // Structure - Barlines are gray
        HighlightKind::MeasureSeparator => "text-gray-500",
        HighlightKind::Repeat => "text-indigo-400 font-bold",
        HighlightKind::Section => "text-emerald-400 font-semibold",
        HighlightKind::SectionBracket => "text-emerald-400",
        HighlightKind::MeasureCount => "text-emerald-300",
        HighlightKind::SectionComment => "text-emerald-200 italic",

        // Special
        HighlightKind::Rest => "text-gray-400 italic",
        HighlightKind::Space => "text-gray-500",
        HighlightKind::MemoryRecall => "text-gray-400",
        HighlightKind::Dynamic => "text-red-400 italic",

        // Metadata
        HighlightKind::Title => "text-green-400 font-semibold",
        HighlightKind::Artist => "text-green-300",
        HighlightKind::Tempo => "text-orange-400",
        HighlightKind::TempoArrow => "text-orange-300",
        HighlightKind::Key => "text-violet-400",
        HighlightKind::TimeSignature => "text-cyan-400",

        // Comments - muted gray
        HighlightKind::Comment => "text-gray-500 italic",
        HighlightKind::CommentMarker => "text-gray-500",

        // Melody and tracks
        HighlightKind::MelodyBlock => "text-teal-400",
        HighlightKind::TrackMarker => "text-fuchsia-400 font-semibold",

        // Commands and cues - muted (these are config lines like /push = triplet)
        HighlightKind::Command => "text-gray-500",
        HighlightKind::TextCue => "text-gray-400 italic",

        // Whitespace and unknown - muted, not distracting
        HighlightKind::Whitespace => "",
        HighlightKind::Unknown => "text-gray-500", // Muted instead of red
    }
}

/// Create a zip file containing multiple SVG pages.
///
/// Each SVG is stored as `{base_filename}_page{N}.svg` where N is 1-indexed.
#[cfg(target_arch = "wasm32")]
fn create_svg_zip(pages: &[String], base_filename: &str) -> Result<Vec<u8>, String> {
    use std::io::{Cursor, Write};
    use zip::ZipWriter;
    use zip::write::SimpleFileOptions;

    let mut buffer = Cursor::new(Vec::new());
    let mut zip = ZipWriter::new(&mut buffer);

    let options = SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .compression_level(Some(6));

    for (i, svg_content) in pages.iter().enumerate() {
        let filename = format!("{}_page{}.svg", base_filename, i + 1);
        zip.start_file(&filename, options)
            .map_err(|e| format!("Failed to create zip entry: {e}"))?;
        zip.write_all(svg_content.as_bytes())
            .map_err(|e| format!("Failed to write SVG to zip: {e}"))?;
    }

    zip.finish()
        .map_err(|e| format!("Failed to finalize zip: {e}"))?;

    Ok(buffer.into_inner())
}
