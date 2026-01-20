//! SVG export for the scene graph.
//!
//! Converts SceneNode trees into SVG documents with semantic `data-*` attributes
//! for editable vector output. Produces LilyPond-style SVGs suitable for
//! post-processing and editing.

use std::fmt::Write;

use kurbo::Affine;

use crate::engraver::scene::id::SemanticId;
use crate::engraver::scene::node::SceneNode;
use crate::engraver::scene::paint::PaintCommand;
use crate::engraver::scene::transform::{get_translation, is_identity};

/// Configuration for SVG export.
#[derive(Debug, Clone)]
pub struct SvgExportConfig {
    /// SVG width in points
    pub width: f64,
    /// SVG height in points
    pub height: f64,
    /// Whether to include semantic `data-*` attributes
    pub include_semantic_ids: bool,
    /// Whether to embed SMuFL glyph definitions
    pub embed_glyphs: bool,
    /// Decimal precision for coordinates
    pub precision: u8,
    /// Whether to pretty-print the SVG with indentation
    pub pretty_print: bool,
    /// Background color (None for transparent)
    pub background: Option<vello::peniko::Color>,
    /// Default stroke width
    pub default_stroke_width: f64,
}

impl Default for SvgExportConfig {
    fn default() -> Self {
        Self {
            width: 612.0,  // US Letter width in points (8.5" × 72)
            height: 792.0, // US Letter height in points (11" × 72)
            include_semantic_ids: true,
            embed_glyphs: false,
            precision: 2,
            pretty_print: true,
            background: None,
            default_stroke_width: 0.5,
        }
    }
}

/// SVG serializer that converts scene graphs to SVG strings.
pub struct SvgSerializer {
    config: SvgExportConfig,
    output: String,
    indent_level: usize,
    /// Glyphs used in this document (for <defs>)
    used_glyphs: std::collections::HashSet<char>,
}

impl SvgSerializer {
    /// Create a new SVG serializer with the given configuration.
    #[must_use]
    pub fn new(config: SvgExportConfig) -> Self {
        Self {
            config,
            output: String::with_capacity(4096),
            indent_level: 0,
            used_glyphs: std::collections::HashSet::new(),
        }
    }

    /// Serialize a scene graph to SVG.
    #[must_use]
    pub fn serialize(&mut self, scene: &SceneNode) -> String {
        self.output.clear();
        self.used_glyphs.clear();

        // First pass: collect used glyphs
        self.collect_glyphs(scene);

        // Write SVG header
        self.write_header();

        // Write glyph definitions if enabled
        if self.config.embed_glyphs && !self.used_glyphs.is_empty() {
            self.write_glyph_defs();
        }

        // Write background if set
        if let Some(bg) = self.config.background {
            self.write_background(bg);
        }

        // Write scene content
        self.write_node(scene, Affine::IDENTITY);

        // Close SVG
        self.write_footer();

        std::mem::take(&mut self.output)
    }

    /// Collect all glyphs used in the scene.
    fn collect_glyphs(&mut self, node: &SceneNode) {
        for cmd in &node.commands {
            if let PaintCommand::Glyph { codepoint, .. } = cmd {
                self.used_glyphs.insert(*codepoint);
            }
        }
        for child in &node.children {
            self.collect_glyphs(child);
        }
    }

    /// Write the SVG header.
    fn write_header(&mut self) {
        let width = self.format_coord(self.config.width);
        let height = self.format_coord(self.config.height);

        writeln!(self.output, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();

        writeln!(
            self.output,
            r#"<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="{width}" height="{height}" viewBox="0 0 {width} {height}">"#
        )
        .unwrap();

        self.indent_level += 1;
    }

    /// Write the SVG footer.
    fn write_footer(&mut self) {
        self.indent_level -= 1;
        writeln!(self.output, "</svg>").unwrap();
    }

    /// Write background rectangle.
    fn write_background(&mut self, color: vello::peniko::Color) {
        self.write_indent();
        let width = self.format_coord(self.config.width);
        let height = self.format_coord(self.config.height);
        let color_str = color_to_svg(color);
        writeln!(
            self.output,
            r#"<rect width="{width}" height="{height}" fill="{color_str}"/>"#
        )
        .unwrap();
    }

    /// Write glyph definitions for SMuFL characters.
    fn write_glyph_defs(&mut self) {
        self.write_indent();
        writeln!(self.output, "<defs>").unwrap();
        self.indent_level += 1;

        // Clone glyphs to avoid borrow issue
        let glyphs: Vec<char> = self.used_glyphs.iter().copied().collect();

        // For each used glyph, create a symbol definition
        // In a full implementation, these would be actual glyph paths
        for codepoint in glyphs {
            self.write_indent();
            let id = format!("glyph-{:04X}", codepoint as u32);
            writeln!(
                self.output,
                r#"<symbol id="{id}"><text>{}</text></symbol>"#,
                codepoint
            )
            .unwrap();
        }

        self.indent_level -= 1;
        self.write_indent();
        writeln!(self.output, "</defs>").unwrap();
    }

    /// Write a scene node and its children.
    fn write_node(&mut self, node: &SceneNode, parent_transform: Affine) {
        if !node.visible {
            return;
        }

        let combined_transform = parent_transform * node.transform;
        let has_transform = !is_identity(&node.transform);
        let has_id = self.config.include_semantic_ids && node.id.is_some();
        let has_metadata = !node.metadata.is_empty();

        // Determine if we need a group element
        let needs_group = has_transform || has_id || has_metadata || !node.children.is_empty();

        if needs_group {
            self.write_group_start(node);
        }

        // Write paint commands
        for cmd in &node.commands {
            self.write_paint_command(cmd, combined_transform);
        }

        // Write children
        for child in &node.children {
            self.write_node(child, combined_transform);
        }

        if needs_group {
            self.write_group_end();
        }
    }

    /// Write the opening tag of a group element.
    fn write_group_start(&mut self, node: &SceneNode) {
        self.write_indent();
        write!(self.output, "<g").unwrap();

        // Add semantic ID attributes
        if self.config.include_semantic_ids {
            if let Some(id) = &node.id {
                self.write_semantic_attrs(id);
            }
        }

        // Add metadata attributes
        for (key, value) in &node.metadata {
            write!(self.output, r#" data-{key}="{value}""#).unwrap();
        }

        // Add transform if non-identity
        if !is_identity(&node.transform) {
            let transform_str = transform_to_svg(&node.transform);
            if !transform_str.is_empty() {
                write!(self.output, r#" transform="{transform_str}""#).unwrap();
            }
        }

        writeln!(self.output, ">").unwrap();
        self.indent_level += 1;
    }

    /// Write the closing tag of a group element.
    fn write_group_end(&mut self) {
        self.indent_level -= 1;
        self.write_indent();
        writeln!(self.output, "</g>").unwrap();
    }

    /// Write semantic ID as data attributes.
    fn write_semantic_attrs(&mut self, id: &SemanticId) {
        for (attr, value) in id.to_svg_attributes() {
            write!(self.output, r#" {attr}="{value}""#).unwrap();
        }
    }

    /// Write a paint command as SVG.
    fn write_paint_command(&mut self, cmd: &PaintCommand, _transform: Affine) {
        self.write_indent();

        match cmd {
            PaintCommand::Fill { path, color, .. } => {
                let d = path_to_svg_d(path, self.config.precision);
                let fill = color_to_svg(*color);
                writeln!(self.output, r#"<path d="{d}" fill="{fill}"/>"#).unwrap();
            }

            PaintCommand::Stroke {
                path, color, width, ..
            } => {
                let d = path_to_svg_d(path, self.config.precision);
                let stroke = color_to_svg(*color);
                let w = self.format_coord(*width);
                writeln!(
                    self.output,
                    r#"<path d="{d}" fill="none" stroke="{stroke}" stroke-width="{w}"/>"#
                )
                .unwrap();
            }

            PaintCommand::Glyph {
                codepoint,
                position,
                size,
                color,
            } => {
                let x = self.format_coord(position.x);
                let y = self.format_coord(position.y);
                let font_size = self.format_coord(*size);
                let fill = color_to_svg(*color);
                writeln!(
                    self.output,
                    r#"<text x="{x}" y="{y}" font-size="{font_size}" fill="{fill}" font-family="Bravura">{}</text>"#,
                    codepoint
                )
                .unwrap();
            }

            PaintCommand::Text {
                text,
                font_family,
                font_size,
                position,
                color,
                anchor,
                weight,
                style,
            } => {
                let x = self.format_coord(position.x);
                let y = self.format_coord(position.y);
                let size = self.format_coord(*font_size);
                let fill = color_to_svg(*color);
                let anchor_str = text_anchor_to_svg(*anchor);
                let weight_str = font_weight_to_svg(*weight);
                let style_str = font_style_to_svg(*style);

                write!(
                    self.output,
                    r#"<text x="{x}" y="{y}" font-size="{size}" font-family="{font_family}""#
                )
                .unwrap();
                write!(self.output, r#" fill="{fill}""#).unwrap();

                if anchor_str != "start" {
                    write!(self.output, r#" text-anchor="{anchor_str}""#).unwrap();
                }
                if weight_str != "normal" {
                    write!(self.output, r#" font-weight="{weight_str}""#).unwrap();
                }
                if style_str != "normal" {
                    write!(self.output, r#" font-style="{style_str}""#).unwrap();
                }

                writeln!(self.output, ">{}</text>", escape_xml(text)).unwrap();
            }

            PaintCommand::Line {
                start,
                end,
                width,
                color,
                ..
            } => {
                let x1 = self.format_coord(start.x);
                let y1 = self.format_coord(start.y);
                let x2 = self.format_coord(end.x);
                let y2 = self.format_coord(end.y);
                let w = self.format_coord(*width);
                let stroke = color_to_svg(*color);
                writeln!(
                    self.output,
                    r#"<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{stroke}" stroke-width="{w}"/>"#
                )
                .unwrap();
            }

            PaintCommand::Rect {
                rect,
                fill,
                stroke,
                stroke_width,
                corner_radius,
            } => {
                let x = self.format_coord(rect.x0);
                let y = self.format_coord(rect.y0);
                let w = self.format_coord(rect.width());
                let h = self.format_coord(rect.height());

                let fill_str = fill.map_or("none".to_string(), color_to_svg);
                let mut attrs =
                    format!(r#"x="{x}" y="{y}" width="{w}" height="{h}" fill="{fill_str}""#);

                if let Some(stroke_color) = stroke {
                    let stroke_str = color_to_svg(*stroke_color);
                    let sw = self.format_coord(*stroke_width);
                    write!(attrs, r#" stroke="{stroke_str}" stroke-width="{sw}""#).unwrap();
                }

                if let Some(radius) = corner_radius {
                    let r = self.format_coord(*radius);
                    write!(attrs, r#" rx="{r}""#).unwrap();
                }

                writeln!(self.output, r#"<rect {attrs}/>"#).unwrap();
            }

            PaintCommand::Circle {
                center,
                radius,
                fill,
                stroke,
                stroke_width,
            } => {
                let cx = self.format_coord(center.x);
                let cy = self.format_coord(center.y);
                let r = self.format_coord(*radius);

                let fill_str = fill.map_or("none".to_string(), color_to_svg);
                let mut attrs = format!(r#"cx="{cx}" cy="{cy}" r="{r}" fill="{fill_str}""#);

                if let Some(stroke_color) = stroke {
                    let stroke_str = color_to_svg(*stroke_color);
                    let sw = self.format_coord(*stroke_width);
                    write!(attrs, r#" stroke="{stroke_str}" stroke-width="{sw}""#).unwrap();
                }

                writeln!(self.output, r#"<circle {attrs}/>"#).unwrap();
            }

            PaintCommand::Ellipse {
                center,
                radius_x,
                radius_y,
                fill,
                stroke,
                stroke_width,
            } => {
                let cx = self.format_coord(center.x);
                let cy = self.format_coord(center.y);
                let rx = self.format_coord(*radius_x);
                let ry = self.format_coord(*radius_y);

                let fill_str = fill.map_or("none".to_string(), color_to_svg);
                let mut attrs =
                    format!(r#"cx="{cx}" cy="{cy}" rx="{rx}" ry="{ry}" fill="{fill_str}""#);

                if let Some(stroke_color) = stroke {
                    let stroke_str = color_to_svg(*stroke_color);
                    let sw = self.format_coord(*stroke_width);
                    write!(attrs, r#" stroke="{stroke_str}" stroke-width="{sw}""#).unwrap();
                }

                writeln!(self.output, r#"<ellipse {attrs}/>"#).unwrap();
            }
        }
    }

    /// Write indentation.
    fn write_indent(&mut self) {
        if self.config.pretty_print {
            for _ in 0..self.indent_level {
                self.output.push_str("  ");
            }
        }
    }

    /// Format a coordinate with the configured precision.
    fn format_coord(&self, value: f64) -> String {
        format!("{:.prec$}", value, prec = self.config.precision as usize)
    }
}

/// Convert a Color to SVG color string.
fn color_to_svg(color: vello::peniko::Color) -> String {
    let rgba = color.to_rgba8();
    if rgba.a == 255 {
        format!("#{:02x}{:02x}{:02x}", rgba.r, rgba.g, rgba.b)
    } else {
        format!(
            "rgba({},{},{},{:.2})",
            rgba.r,
            rgba.g,
            rgba.b,
            rgba.a as f64 / 255.0
        )
    }
}

/// Convert an affine transform to SVG transform string.
fn transform_to_svg(transform: &Affine) -> String {
    let [a, b, c, d, e, f] = transform.as_coeffs();

    // Check for simple translation
    let translation = get_translation(transform);
    let tx = translation.x;
    let ty = translation.y;

    if (tx.abs() > 0.001 || ty.abs() > 0.001)
        && (a - 1.0).abs() < 0.001
        && (d - 1.0).abs() < 0.001
        && b.abs() < 0.001
        && c.abs() < 0.001
    {
        return format!("translate({:.2},{:.2})", tx, ty);
    }

    // General matrix
    format!(
        "matrix({:.4},{:.4},{:.4},{:.4},{:.2},{:.2})",
        a, b, c, d, e, f
    )
}

/// Convert a BezPath to SVG path data.
fn path_to_svg_d(path: &kurbo::BezPath, precision: u8) -> String {
    use kurbo::PathEl;

    let prec = precision as usize;
    let mut d = String::new();

    for el in path.elements() {
        match el {
            PathEl::MoveTo(p) => {
                write!(d, "M{:.prec$},{:.prec$}", p.x, p.y).unwrap();
            }
            PathEl::LineTo(p) => {
                write!(d, "L{:.prec$},{:.prec$}", p.x, p.y).unwrap();
            }
            PathEl::QuadTo(p1, p2) => {
                write!(
                    d,
                    "Q{:.prec$},{:.prec$},{:.prec$},{:.prec$}",
                    p1.x, p1.y, p2.x, p2.y
                )
                .unwrap();
            }
            PathEl::CurveTo(p1, p2, p3) => {
                write!(
                    d,
                    "C{:.prec$},{:.prec$},{:.prec$},{:.prec$},{:.prec$},{:.prec$}",
                    p1.x, p1.y, p2.x, p2.y, p3.x, p3.y
                )
                .unwrap();
            }
            PathEl::ClosePath => {
                d.push('Z');
            }
        }
    }

    d
}

/// Convert text anchor to SVG attribute value.
fn text_anchor_to_svg(anchor: crate::engraver::scene::paint::TextAnchor) -> &'static str {
    use crate::engraver::scene::paint::TextAnchor;
    match anchor {
        TextAnchor::Start => "start",
        TextAnchor::Middle => "middle",
        TextAnchor::End => "end",
    }
}

/// Convert font weight to SVG attribute value.
fn font_weight_to_svg(weight: crate::engraver::scene::paint::FontWeight) -> String {
    use crate::engraver::scene::paint::FontWeight;
    match weight {
        FontWeight::Normal => "normal".to_string(),
        FontWeight::Bold => "bold".to_string(),
        FontWeight::Light => "300".to_string(),
        FontWeight::Custom(w) => w.to_string(),
    }
}

/// Convert font style to SVG attribute value.
fn font_style_to_svg(style: crate::engraver::scene::paint::FontStyle) -> &'static str {
    use crate::engraver::scene::paint::FontStyle;
    match style {
        FontStyle::Normal => "normal",
        FontStyle::Italic => "italic",
        FontStyle::Oblique => "oblique",
    }
}

/// Escape XML special characters.
fn escape_xml(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '&' => escaped.push_str("&amp;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&apos;"),
            _ => escaped.push(c),
        }
    }
    escaped
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engraver::scene::id::ElementType;
    use kurbo::{Point, Rect};
    use vello::peniko::Color;

    #[test]
    fn test_empty_scene() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::group(SemanticId::page(1));
        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"<svg xmlns="http://www.w3.org/2000/svg""#));
        assert!(svg.contains("</svg>"));
        assert!(svg.contains(r#"data-type="page""#));
    }

    #[test]
    fn test_semantic_ids() {
        let config = SvgExportConfig {
            include_semantic_ids: true,
            ..Default::default()
        };
        let mut serializer = SvgSerializer::new(config);

        let mut scene = SceneNode::group(SemanticId::measure(1));
        scene.add_child(SceneNode::leaf(
            SemanticId::new(ElementType::Note, 42).with_attribute("pitch", "C4"),
            vec![],
        ));

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"data-type="measure""#));
        assert!(svg.contains(r#"data-measure="1""#));
        assert!(svg.contains(r#"data-type="note""#));
        assert!(svg.contains(r#"data-pitch="C4""#));
    }

    #[test]
    fn test_line_command() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::anonymous_leaf(vec![PaintCommand::line(
            Point::new(0.0, 0.0),
            Point::new(100.0, 0.0),
            Color::BLACK,
            1.0,
        )]);

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"<line"#));
        assert!(svg.contains(r#"x1="0.00""#));
        assert!(svg.contains(r#"x2="100.00""#));
        assert!(svg.contains("stroke=\"#000000\""));
    }

    #[test]
    fn test_rect_command() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
            Rect::new(10.0, 20.0, 50.0, 40.0),
            Color::BLACK,
        )]);

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"<rect"#));
        assert!(svg.contains(r#"x="10.00""#));
        assert!(svg.contains(r#"y="20.00""#));
        assert!(svg.contains(r#"width="40.00""#));
        assert!(svg.contains(r#"height="20.00""#));
    }

    #[test]
    fn test_text_command() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::anonymous_leaf(vec![PaintCommand::text(
            "Hello",
            "Arial",
            12.0,
            Point::new(10.0, 20.0),
            Color::BLACK,
        )]);

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"<text"#));
        assert!(svg.contains(r#"font-family="Arial""#));
        assert!(svg.contains(">Hello</text>"));
    }

    #[test]
    fn test_glyph_command() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::anonymous_leaf(vec![PaintCommand::glyph(
            '\u{E0A4}', // SMuFL black notehead
            Point::new(50.0, 100.0),
            20.0,
            Color::BLACK,
        )]);

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"<text"#));
        assert!(svg.contains(r#"font-family="Bravura""#));
    }

    #[test]
    fn test_transform() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let mut scene = SceneNode::group(SemanticId::page(1));
        scene.transform = Affine::translate((100.0, 50.0));

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"transform="translate(100.00,50.00)""#));
    }

    #[test]
    fn test_invisible_node_excluded() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let mut scene = SceneNode::group(SemanticId::page(1));
        let mut invisible = SceneNode::leaf(
            SemanticId::new(ElementType::Note, 1),
            vec![PaintCommand::filled_rect(
                Rect::new(0.0, 0.0, 10.0, 10.0),
                Color::BLACK,
            )],
        );
        invisible.visible = false;
        scene.add_child(invisible);

        let svg = serializer.serialize(&scene);

        // Should not contain the note element
        assert!(!svg.contains(r#"data-type="note""#));
    }

    #[test]
    fn test_escape_xml() {
        assert_eq!(escape_xml("a < b"), "a &lt; b");
        assert_eq!(escape_xml("a > b"), "a &gt; b");
        assert_eq!(escape_xml("a & b"), "a &amp; b");
        assert_eq!(escape_xml(r#"a "b""#), "a &quot;b&quot;");
    }

    #[test]
    fn test_color_to_svg() {
        assert_eq!(color_to_svg(Color::BLACK), "#000000");
        assert_eq!(color_to_svg(Color::WHITE), "#ffffff");
        assert_eq!(
            color_to_svg(Color::from_rgba8(255, 0, 0, 128)),
            "rgba(255,0,0,0.50)"
        );
    }

    #[test]
    fn test_metadata_attributes() {
        let config = SvgExportConfig::default();
        let mut serializer = SvgSerializer::new(config);

        let scene = SceneNode::leaf(SemanticId::new(ElementType::Clef, 1), vec![])
            .with_metadata("clef-type", "Treble");

        let svg = serializer.serialize(&scene);

        assert!(svg.contains(r#"data-clef-type="Treble""#));
    }
}
