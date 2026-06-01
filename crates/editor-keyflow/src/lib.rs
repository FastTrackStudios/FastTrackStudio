//! Keyflow chart → inline SVG. Public API mirrors
//! [`editor_mermaid::render_svg`] and `editor_typst::Compiler`
//! so the markdown integration layer treats all three fence
//! renderers the same way: one `render_svg(source)` →
//! `Result<String, RenderError>` call, no setup, no state.
//!
//! Under the hood we drive keyflow's `engraver` through its
//! **CPU-only `svg` tier** — the layout engine plus the
//! `SvgSerializer`, with the wgpu/vello GPU stack and the PDF
//! deps compiled out. That subset is pure-Rust
//! (kurbo/peniko/skrifa/smufl) and compiles for
//! `wasm32-unknown-unknown`, so `.kf` fences render fully in
//! the browser like typst and mermaid.
//!
//! ## Self-contained output
//!
//! Music engraving leans on specific fonts (Leland/Bravura for
//! SMuFL glyphs, MuseJazz/Chicago for text). We embed those
//! font bytes into the SVG as base64 `@font-face` blocks
//! (`with_embedded_font`), so the emitted SVG renders correctly
//! in any browser without the fonts being installed. The bytes
//! themselves are `include_bytes!`-baked into `engraver`, so no
//! filesystem access is needed at runtime.

use engraver::api;
use engraver::export::svg::{SvgExportConfig, SvgSerializer};
use engraver::fonts::ChartFontBundle;
use engraver::layout::chart::LayoutMode;
use thiserror::Error;

/// Layout width (points) for the continuous-scroll render. The
/// emitted SVG carries this as its intrinsic width; the editor
/// scales it to the container with CSS, so this only sets how
/// wide the engraver lets a system grow before wrapping.
const LAYOUT_WIDTH: f64 = 800.0;

/// Padding (points) kept around the content when cropping a snippet's viewBox
/// to its bounds — a little breathing room so glyph edges and chord symbols
/// above the staff aren't flush against the embed border.
const SNIPPET_PADDING: f64 = 6.0;

#[derive(Debug, Error)]
pub enum RenderError {
    /// The keyflow source didn't parse, or layout/serialization
    /// failed. Body carries the human-readable error. The caller
    /// typically falls back to showing the raw source so the
    /// user can fix it.
    #[error("keyflow render failed: {0}")]
    Render(String),
}

/// Render keyflow chart source to an inline SVG string. The
/// result is safe to drop into `dangerous_inner_html` — it
/// begins with `<svg …>` and is self-contained (fonts embedded).
///
/// Uses continuous-scroll layout (a single scene-sized image,
/// no pagination) which is the right shape for an inline
/// document embed. Returns [`RenderError::Render`] when the
/// source can't be parsed or laid out.
pub fn render_svg(source: &str) -> Result<String, RenderError> {
    let mode = LayoutMode::ContinuousScroll {
        width: LAYOUT_WIDTH,
    };
    let result =
        api::chart::layout_text(source, &mode).map_err(|e| RenderError::Render(e.to_string()))?;
    let fonts = ChartFontBundle::new().map_err(RenderError::Render)?;

    // Shrink-wrap the SVG viewBox to what's actually drawn. The engraver's
    // `total_width`/`total_height` describe a print page box — content plus A4
    // margins, inter-system spacing, and below-staff reserve — which for an
    // inline snippet leaves the music marooned in mostly-empty space (a
    // one-system chart is ~20pt of music in a ~180pt box). Cropping to the
    // content bounds (with a little padding) makes the embed size to its
    // content; the editor's CSS then scales that to the container.
    let (vx, vy, vw, vh) = match result.content_bounds() {
        Some(b) => (
            b.x0 - SNIPPET_PADDING,
            b.y0 - SNIPPET_PADDING,
            b.width() + 2.0 * SNIPPET_PADDING,
            b.height() + 2.0 * SNIPPET_PADDING,
        ),
        None => (0.0, 0.0, result.total_width, result.total_height),
    };
    let config = with_embedded_fonts(&fonts, SvgExportConfig::for_page(vx, vy, vw, vh));
    let mut serializer = SvgSerializer::new(config);
    Ok(serializer.serialize(&result.scene))
}

/// Embed the engraving fonts into the SVG config. Family names
/// mirror keyflow-cli's exporter so every `font-family` the
/// scene references — SMuFL music glyphs, chord-symbol text, and
/// document text, plus their legacy aliases — resolves to baked
/// bytes.
fn with_embedded_fonts(fonts: &ChartFontBundle, config: SvgExportConfig) -> SvgExportConfig {
    let leland = fonts.symbol_font_data().as_ref().clone();
    let leland_text = fonts.leland_text_font_data().as_ref().clone();
    let musejazz_text = fonts.text_font_data().as_ref().clone();
    let musejazz = fonts.musejazz_font_data().as_ref().clone();
    let chicago = fonts.chicago_font_data().as_ref().clone();
    let bravura = fonts.bravura_font_data().as_ref().clone();
    let freesans = fonts.freesans_font_data().as_ref().clone();

    config
        // SMuFL music font (Leland) + legacy "Bravura" alias.
        .with_embedded_font("Leland", leland)
        .with_embedded_font("Bravura", bravura)
        // Leland Text companion + aliases.
        .with_embedded_font("Leland Text", leland_text.clone())
        .with_embedded_font("LelandText", leland_text.clone())
        .with_embedded_font("Edwin", leland_text)
        // MuseJazz music + MuseJazz Text chord-symbol font.
        .with_embedded_font("MuseJazz", musejazz)
        .with_embedded_font("MuseJazz Text", musejazz_text.clone())
        .with_embedded_font("MuseJazzText", musejazz_text)
        // Chicago — default document / title text.
        .with_embedded_font("Chicago", chicago.clone())
        .with_embedded_font("ChicagoFLF", chicago.clone())
        .with_embedded_font("FreeSans", freesans)
        .with_embedded_font("sans-serif", chicago)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE: &str = "Test Chart - Demo\n4/4 120bpm #C\n\nVS\nC | F | G | C\n";

    #[test]
    fn renders_a_simple_chart() {
        let svg = render_svg(SIMPLE).expect("render ok");
        assert!(svg.contains("<svg"), "got: {}", &svg[..svg.len().min(80)]);
    }

    #[test]
    fn embeds_fonts_for_self_contained_output() {
        let svg = render_svg(SIMPLE).expect("render ok");
        assert!(svg.contains("@font-face"), "expected embedded fonts");
    }

    /// A one-system chart is a few measures of music; its SVG must crop to that,
    /// not to the engraver's print page box (which pads it with margins +
    /// trailing system spacing into ~5× the height). Guards the snippet crop.
    #[test]
    fn crops_to_content_not_page_box() {
        let svg = render_svg("1 4 6 5").expect("render ok");
        let head: String = svg.chars().take(400).collect();
        let attr = |name: &str| -> f64 {
            head.split(&format!("{name}=\""))
                .nth(1)
                .and_then(|s| s.split('"').next())
                .and_then(|v| v.parse::<f64>().ok())
                .unwrap_or(f64::NAN)
        };
        let h = attr("height");
        // Four bars of single-line chord/staff content crop to well under 80pt;
        // the un-cropped page box was ~178pt. A regression (reverting to the
        // page box) would blow past this.
        assert!(
            h < 80.0,
            "snippet height should crop to content (~33pt), got {h}"
        );
    }
}
