//! Chart Renderer Module
//!
//! Lays out keyflow charts and exports them to SVG / PDF (vector, via SVG).
//! The site renders charts as static SVG images — there is no WebGPU/canvas
//! rendering path.

use keyflow::Chart;
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::ChartLayoutMode;
use keyflow::engraver::layout::chart::{
    Breakpoint, ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult,
};
use keyflow::engraver::style::MStyle;

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

/// Chart layout and SVG/PDF export engine for the web.
///
/// Manages fonts and the layout engine, and serializes the laid-out scene to
/// SVG (displayed as an `<img>`) or to PDF via SVG.
pub struct ChartLayoutManager {
    /// Font bundle (single source of truth for all chart fonts)
    font_bundle: ChartFontBundle,
    /// Layout engine
    layout_engine: ChartLayoutEngine,
    /// Cached layout result
    layout_result: Option<ChartLayoutResult>,
    /// Last rendered chart hash (for cache invalidation)
    last_chart_hash: u64,
}

impl ChartLayoutManager {
    /// Create a new chart layout manager with embedded fonts.
    pub fn new() -> Result<Self, String> {
        // Load font bundle (single source of truth)
        let font_bundle = ChartFontBundle::new()?;

        // Create layout engine with correct font wiring via bundle
        let style = Box::leak(Box::new(MStyle::new()));
        let layout_engine = font_bundle.create_layout_engine(style);

        Ok(Self {
            font_bundle,
            layout_engine,
            layout_result: None,
            last_chart_hash: 0,
        })
    }

    /// Layout a chart for export (no page offsets, positioned at origin).
    ///
    /// # Arguments
    /// * `chart` - The parsed chart to layout
    /// * `viewport_width` - Width of the viewport in CSS pixels
    /// * `snippet_mode` - If true, use snippet mode (content-sized). If false, use A4 paginated mode.
    pub fn layout_chart_for_export(
        &mut self,
        chart: &Chart,
        viewport_width: f64,
        snippet_mode: bool,
    ) {
        let mode = if snippet_mode {
            crate::components::LayoutMode::Snippet
        } else {
            crate::components::LayoutMode::Page
        };
        self.layout_chart_with_options(chart, viewport_width, mode, false);
    }

    /// Layout a chart with full control over options.
    ///
    /// # Arguments
    /// * `chart` - The parsed chart to layout
    /// * `viewport_width` - Width of the viewport in CSS pixels
    /// * `layout_mode` - Snippet (content-sized), Page (A4 paginated), or Responsive (iReal Pro breakpoint).
    /// * `use_page_offsets` - If true, add 20pt offset for multi-page viewing. If false, position at origin.
    fn layout_chart_with_options(
        &mut self,
        chart: &Chart,
        viewport_width: f64,
        layout_mode: crate::components::LayoutMode,
        use_page_offsets: bool,
    ) {
        // Simple hash based on chart data, mode, and offset setting
        let chart_hash = self.compute_chart_hash_with_options(chart, layout_mode, use_page_offsets);

        // Skip if already laid out
        if self.layout_result.is_some() && chart_hash == self.last_chart_hash {
            return;
        }

        let viewport_pt = (viewport_width / DPI_SCALE).max(240.0);

        let (mode, config) = match layout_mode {
            crate::components::LayoutMode::Snippet => {
                let config = ChartLayoutConfig::snippet().with_page_offsets(use_page_offsets);
                let mode = ChartLayoutMode::Snippet {
                    page_width: viewport_pt,
                };
                (mode, config)
            }
            crate::components::LayoutMode::Page => {
                let config = ChartLayoutConfig::master_rhythm().with_page_offsets(use_page_offsets);
                let mode = ChartLayoutMode::paginated_a4();
                (mode, config)
            }
            crate::components::LayoutMode::Responsive => {
                // Vertical-only scroll: width snaps to viewport, height grows.
                // Breakpoint picks per-class spatium/chord sizing.
                let breakpoint = Breakpoint::from_viewport_pt(viewport_pt);
                let config = ChartLayoutConfig::responsive_for(breakpoint);
                let mode = ChartLayoutMode::ContinuousScroll { width: viewport_pt };
                (mode, config)
            }
        };

        // Perform layout
        let result = self
            .layout_engine
            .layout_chart_with_config(chart, &mode, &config);

        self.layout_result = Some(result);
        self.last_chart_hash = chart_hash;
    }

    /// Compute a hash of the chart including all layout options.
    ///
    /// This hash is used for cache invalidation - if the hash changes, we re-layout.
    /// We hash the debug representation of the chart which includes all content.
    fn compute_chart_hash_with_options(
        &self,
        chart: &Chart,
        layout_mode: crate::components::LayoutMode,
        use_page_offsets: bool,
    ) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        (layout_mode as u8).hash(&mut hasher);

        // Hash key chart fields for cache invalidation.
        // Chart doesn't derive Debug, so we hash its accessible sub-fields.
        format!("{:?}", chart.metadata).hash(&mut hasher);
        chart.sections.len().hash(&mut hasher);
        format!("{:?}", chart.tempo).hash(&mut hasher);
        format!("{:?}", chart.time_signature).hash(&mut hasher);
        format!("{:?}", chart.key_changes).hash(&mut hasher);
        format!("{:?}", chart.tempo_changes).hash(&mut hasher);
        format!("{:?}", chart.settings).hash(&mut hasher);
        // Hash each section's debug output for content changes
        for section in &chart.sections {
            format!("{:?}", section).hash(&mut hasher);
        }
        use_page_offsets.hash(&mut hasher);

        hasher.finish()
    }

    /// Export the current layout to SVG.
    ///
    /// Returns the SVG string or an error if no layout is available.
    pub fn export_to_svg(&self) -> Result<String, String> {
        use keyflow::engraver::export::{SvgExportConfig, SvgSerializer};

        let layout = self
            .layout_result
            .as_ref()
            .ok_or_else(|| "No layout available for export".to_string())?;

        // Get dimensions from the scene's computed bounds or use A4 defaults
        let bounds = layout.scene.compute_bounds();
        let (width, height) = if bounds.width() > 0.0 && bounds.height() > 0.0 {
            // Add some padding
            (bounds.x1 + 20.0, bounds.y1 + 20.0)
        } else {
            // A4 dimensions in points
            (595.0, 842.0)
        };

        let config = SvgExportConfig {
            width,
            height,
            view_box: None, // Default viewBox
            include_semantic_ids: true,
            embed_glyphs: false,
            precision: 2,
            pretty_print: true,
            background: Some(peniko::Color::WHITE),
            default_stroke_width: 0.5,
            embedded_fonts: vec![
                // SMuFL music symbol font
                (
                    "Bravura".to_string(),
                    self.font_bundle.symbol_font_data().as_ref().clone(),
                ),
                // Chord symbol fonts
                (
                    "MuseJazzText".to_string(),
                    self.font_bundle.text_font_data().as_ref().clone(),
                ),
                (
                    "MuseJazz".to_string(),
                    self.font_bundle.text_font_data().as_ref().clone(),
                ),
                // Text fonts - FreeSans with all aliases used in the chart
                (
                    "FreeSans".to_string(),
                    self.font_bundle.aux_font_data().as_ref().clone(),
                ),
                (
                    "title-bold".to_string(),
                    self.font_bundle.aux_font_data().as_ref().clone(),
                ),
                (
                    "part-name-bold".to_string(),
                    self.font_bundle.aux_font_data().as_ref().clone(),
                ),
                (
                    "sans-serif".to_string(),
                    self.font_bundle.aux_font_data().as_ref().clone(),
                ),
            ],
        };

        let mut serializer = SvgSerializer::new(config);
        Ok(serializer.serialize(&layout.scene))
    }

    /// Export each page as a separate SVG string (LilyPond-style).
    ///
    /// Returns a vector of SVG strings, one per page. Each SVG has:
    /// - Proper dimensions matching the page size
    /// - viewBox set to show only that page's content
    /// - Semantic IDs on elements for editability
    ///
    /// This is the recommended export format for high-quality vector output
    /// that can be edited in vector graphics software or converted to PDF.
    pub fn export_pages_to_svg(&self) -> Result<Vec<String>, String> {
        use keyflow::engraver::export::{SvgExportConfig, SvgSerializer};

        let layout = self
            .layout_result
            .as_ref()
            .ok_or_else(|| "No layout available for export".to_string())?;

        let page_info = self.get_page_info();

        if page_info.is_empty() {
            // Single page / snippet mode - export the whole scene
            return self.export_to_svg().map(|svg| vec![svg]);
        }

        let mut svg_pages = Vec::with_capacity(page_info.len());

        for (_page_num, page_x, page_y, page_width, page_height) in page_info {
            // Create config for this page with viewBox set to clip to this page's area
            // Include embedded fonts so SVGs are self-contained
            let config = SvgExportConfig::for_page(page_x, page_y, page_width, page_height)
                // SMuFL music symbol font
                .with_embedded_font(
                    "Bravura",
                    self.font_bundle.symbol_font_data().as_ref().clone(),
                )
                // Chord symbol fonts
                .with_embedded_font(
                    "MuseJazzText",
                    self.font_bundle.text_font_data().as_ref().clone(),
                )
                .with_embedded_font(
                    "MuseJazz",
                    self.font_bundle.text_font_data().as_ref().clone(),
                )
                // Text fonts - FreeSans with all aliases used in the chart
                .with_embedded_font(
                    "FreeSans",
                    self.font_bundle.aux_font_data().as_ref().clone(),
                )
                .with_embedded_font(
                    "title-bold",
                    self.font_bundle.aux_font_data().as_ref().clone(),
                )
                .with_embedded_font(
                    "part-name-bold",
                    self.font_bundle.aux_font_data().as_ref().clone(),
                )
                .with_embedded_font(
                    "sans-serif",
                    self.font_bundle.aux_font_data().as_ref().clone(),
                );

            let mut serializer = SvgSerializer::new(config);
            let svg = serializer.serialize(&layout.scene);
            svg_pages.push(svg);
        }

        Ok(svg_pages)
    }

    /// Get page information for multi-page PDF export.
    ///
    /// Returns a vector of (page_number, x_offset, y_offset, width, height) for each page.
    /// Uses the offsets stored in each PageLayout, which were calculated during layout
    /// using the correct page gap and offset values.
    fn get_page_info(&self) -> Vec<(u32, f64, f64, f64, f64)> {
        self.layout_result
            .as_ref()
            .map(|layout| {
                layout
                    .pages
                    .iter()
                    .map(|page| {
                        (
                            page.number,
                            page.x_offset,
                            page.y_offset,
                            page.width,
                            page.height,
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Export to PDF using SVG as an intermediate format (vector-based).
    ///
    /// This approach converts the scene graph to SVG, then uses svg2pdf to
    /// convert the SVG to PDF. This preserves vector quality and ensures
    /// fonts are properly embedded.
    ///
    /// # Returns
    /// PDF bytes ready to be downloaded.
    fn export_to_pdf_via_svg(&self) -> Result<Vec<u8>, String> {
        use keyflow::engraver::export::{PdfSerializer, SvgExportConfig, SvgSerializer};

        let layout = self
            .layout_result
            .as_ref()
            .ok_or_else(|| "No layout available for export".to_string())?;

        // Generate SVG for each page
        let mut svg_pages = Vec::new();
        let page_info = self.get_page_info();

        if page_info.is_empty() {
            // Single page - export entire scene
            let bounds = layout.scene.compute_bounds();
            let (width, height) = if bounds.width() > 0.0 && bounds.height() > 0.0 {
                (bounds.width(), bounds.height())
            } else {
                (595.0, 842.0)
            };

            let config = SvgExportConfig {
                width,
                height,
                view_box: None,
                include_semantic_ids: false,
                embed_glyphs: false,
                precision: 2,
                pretty_print: false,
                background: Some(peniko::Color::WHITE),
                default_stroke_width: 0.5,
                embedded_fonts: Vec::new(),
            };

            let mut serializer = SvgSerializer::new(config);
            let svg = serializer.serialize(&layout.scene);
            svg_pages.push(svg);
        } else {
            // Multi-page - extract and export each page separately
            // For now, we export the entire scene and let svg2pdf handle pagination
            // A proper implementation would create separate scene nodes per page
            let bounds = layout.scene.compute_bounds();
            let config = SvgExportConfig {
                width: bounds.width(),
                height: bounds.height(),
                view_box: None,
                include_semantic_ids: false,
                embed_glyphs: false,
                precision: 2,
                pretty_print: false,
                background: Some(peniko::Color::WHITE),
                default_stroke_width: 0.5,
                embedded_fonts: Vec::new(),
            };

            let mut serializer = SvgSerializer::new(config);
            let svg = serializer.serialize(&layout.scene);
            svg_pages.push(svg);
        }

        // Build font data for svg2pdf
        // Note: Font names here are for logging - fontdb extracts actual family names from font files
        // The important thing is that all required font data is loaded
        let fonts: Vec<(&str, &[u8])> = vec![
            ("Bravura", self.font_bundle.symbol_font_data().as_slice()),
            // MuseJazz font file has internal family name "MuseJazz Text" (with space)
            (
                "MuseJazz Text",
                self.font_bundle.text_font_data().as_slice(),
            ),
            ("FreeSans", self.font_bundle.aux_font_data().as_slice()),
        ];

        PdfSerializer::serialize_from_svg(&svg_pages, &fonts)
            .map_err(|e| format!("SVG to PDF export failed: {e}"))
    }

    /// Export to multi-page PDF using SVG as an intermediate format.
    ///
    /// For each page in the layout:
    /// 1. Extracts the page's scene content
    /// 2. Converts to SVG
    /// 3. Combines all SVGs into a multi-page PDF
    ///
    /// # Returns
    /// PDF bytes ready to be downloaded.
    pub fn export_multi_page_pdf_via_svg(&self) -> Result<Vec<u8>, String> {
        use keyflow::engraver::export::{PdfSerializer, SvgExportConfig, SvgSerializer};

        let layout = self
            .layout_result
            .as_ref()
            .ok_or_else(|| "No layout available for export".to_string())?;

        let page_info = self.get_page_info();

        if page_info.is_empty() {
            // Fall back to single-page export
            return self.export_to_pdf_via_svg();
        }

        // For multi-page export, we need to extract each page's scene content
        // Currently the scene has all pages side-by-side
        // We'll generate SVGs with viewport transforms for each page

        let mut svg_pages = Vec::new();

        for (_page_num, page_x, page_y, page_width, page_height) in &page_info {
            // Create SVG with viewBox set to this page's area
            let config = SvgExportConfig::for_page(*page_x, *page_y, *page_width, *page_height);

            // Create a translated view of the scene for this page
            // The SVG serializer will output the full scene, but we set the viewBox
            // to show only this page's content
            let svg_content = {
                let mut serializer = SvgSerializer::new(config);
                let base_svg = serializer.serialize(&layout.scene);

                // Modify the SVG's viewBox to show only this page
                // Replace the viewBox in the SVG header
                let viewbox = format!(
                    "viewBox=\"{:.2} {:.2} {:.2} {:.2}\"",
                    page_x, page_y, page_width, page_height
                );

                // Find and replace the viewBox attribute
                if let Some(start) = base_svg.find("viewBox=\"") {
                    if let Some(end) = base_svg[start..].find('"').and_then(|s| {
                        base_svg[start + s + 1..]
                            .find('"')
                            .map(|e| start + s + 1 + e + 1)
                    }) {
                        let mut modified = base_svg[..start].to_string();
                        modified.push_str(&viewbox);
                        modified.push_str(&base_svg[end..]);
                        modified
                    } else {
                        base_svg
                    }
                } else {
                    base_svg
                }
            };

            svg_pages.push(svg_content);
        }

        // Build font data for svg2pdf
        // Note: Font names here are for logging - fontdb extracts actual family names from font files
        // The important thing is that all required font data is loaded
        let fonts: Vec<(&str, &[u8])> = vec![
            ("Bravura", self.font_bundle.symbol_font_data().as_slice()),
            // MuseJazz font file has internal family name "MuseJazz Text" (with space)
            (
                "MuseJazz Text",
                self.font_bundle.text_font_data().as_slice(),
            ),
            ("FreeSans", self.font_bundle.aux_font_data().as_slice()),
        ];

        PdfSerializer::serialize_from_svg(&svg_pages, &fonts)
            .map_err(|e| format!("SVG to PDF export failed: {e}"))
    }
}
