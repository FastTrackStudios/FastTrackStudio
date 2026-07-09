//! Pure-Rust icon generation for FTS rig types.
//!
//! Loads the REAPER base icon from an embedded `.icns`, applies a color tint
//! overlay, and draws a badge pill with text — matching the Swift `icon_gen.rs`
//! output but in pure Rust using `tiny-skia` + `ab_glyph`.

use std::io::Cursor;
use std::path::Path;

use ab_glyph::{Font, FontRef, PxScale, ScaleFont};
use tiny_skia::*;

/// RGB color + badge text for a rig type.
#[derive(Debug, Clone)]
pub struct RigAppearance {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub badge: &'static str,
    pub display_name: &'static str,
}

/// All known rig types with their appearances.
pub const RIG_TYPES: &[(&str, RigAppearance)] = &[
    (
        "reaper",
        RigAppearance { r: 0.545, g: 0.361, b: 0.965, badge: "FTS", display_name: "FTS-REAPER" },
    ),
    (
        "live",
        RigAppearance { r: 0.15, g: 0.65, b: 0.45, badge: "LIVE", display_name: "FTS-LIVE" },
    ),
    (
        "guitar",
        RigAppearance { r: 0.231, g: 0.510, b: 0.965, badge: "GUITAR", display_name: "FTS-GUITAR" },
    ),
    (
        "bass",
        RigAppearance { r: 0.918, g: 0.702, b: 0.031, badge: "BASS", display_name: "FTS-BASS" },
    ),
    (
        "keys",
        RigAppearance { r: 0.133, g: 0.773, b: 0.369, badge: "KEYS", display_name: "FTS-KEYS" },
    ),
    (
        "drums",
        RigAppearance { r: 0.937, g: 0.267, b: 0.267, badge: "DRUMS", display_name: "FTS-DRUMS" },
    ),
    (
        "drum-enhancement",
        RigAppearance { r: 0.976, g: 0.451, b: 0.086, badge: "DRUM\nENHANCE", display_name: "FTS-DRUM-ENHANCE" },
    ),
    (
        "vocals",
        RigAppearance { r: 0.925, g: 0.282, b: 0.600, badge: "VOCALS", display_name: "FTS-VOCALS" },
    ),
    (
        "session",
        RigAppearance { r: 0.400, g: 0.620, b: 0.900, badge: "TRACKS", display_name: "FTS-TRACKS" },
    ),
];

/// The embedded REAPER base icon (.icns).
const BASE_ICNS: &[u8] = include_bytes!("../assets/reaper-base.icns");

/// Load the largest image from the embedded REAPER .icns and scale to `size`.
fn load_base_icon(size: u32) -> anyhow::Result<Pixmap> {
    let family = icns::IconFamily::read(Cursor::new(BASE_ICNS))?;

    // Find the largest icon type available
    let icon_types = [
        icns::IconType::RGBA32_512x512_2x,
        icns::IconType::RGBA32_512x512,
        icns::IconType::RGBA32_256x256_2x,
        icns::IconType::RGBA32_256x256,
        icns::IconType::RGBA32_128x128,
    ];

    let mut best_image = None;
    for ty in &icon_types {
        if let Ok(img) = family.get_icon_with_type(*ty) {
            best_image = Some(img);
            break;
        }
    }

    let img = best_image.ok_or_else(|| anyhow::anyhow!("No suitable icon found in .icns"))?;
    let w = img.width();
    let h = img.height();

    // Convert icns Image to an image::RgbaImage for resizing
    let rgba = image::RgbaImage::from_raw(w, h, img.data().to_vec())
        .ok_or_else(|| anyhow::anyhow!("Failed to create image from icns data"))?;

    // Resize to target size
    let resized = image::imageops::resize(
        &rgba,
        size,
        size,
        image::imageops::FilterType::Lanczos3,
    );

    // Convert to tiny_skia Pixmap
    let mut pixmap = Pixmap::new(size, size)
        .ok_or_else(|| anyhow::anyhow!("Failed to create pixmap"))?;

    // Copy pixels, converting from unpremultiplied RGBA to premultiplied
    for (i, pixel) in resized.pixels().enumerate() {
        let [r, g, b, a] = pixel.0;
        let color = ColorU8::from_rgba(r, g, b, a).premultiply();
        pixmap.pixels_mut()[i] = color;
    }

    Ok(pixmap)
}

/// Generate a single rig icon as a PNG at the given size.
/// Composites a color tint + badge onto the REAPER base icon.
pub fn generate_icon(appearance: &RigAppearance, size: u32) -> anyhow::Result<Vec<u8>> {
    let sz = size as f32;
    let mut pixmap = load_base_icon(size)?;

    // ── Apply color tint (SourceAtop — only tints existing opaque pixels) ──
    let tint_color = Color::from_rgba(appearance.r, appearance.g, appearance.b, 0.3).unwrap();
    let mut tint_pixmap = Pixmap::new(size, size).unwrap();
    tint_pixmap.fill(tint_color);

    pixmap.draw_pixmap(
        0, 0,
        tint_pixmap.as_ref(),
        &PixmapPaint {
            blend_mode: BlendMode::SourceAtop,
            ..Default::default()
        },
        Transform::identity(),
        None,
    );

    // ── Badge pill ──
    let lines: Vec<&str> = appearance.badge.lines().collect();
    let line_count = lines.len();
    let is_multiline = line_count > 1;

    let badge_w = sz * 0.70;
    let badge_h = if is_multiline {
        sz * 0.18 * line_count as f32
    } else {
        sz * 0.22
    };
    let badge_x = (sz - badge_w) / 2.0;
    let badge_y = sz - badge_h - sz * 0.08; // inset from bottom (Y=0 is top in tiny-skia)
    let badge_radius = if is_multiline { badge_h * 0.25 } else { badge_h / 2.0 };

    let badge_rect = rounded_rect(badge_x, badge_y, badge_w, badge_h, badge_radius);

    // Badge shadow
    let mut shadow_paint = Paint::default();
    shadow_paint.set_color(Color::from_rgba(0.0, 0.0, 0.0, 0.5).unwrap());
    shadow_paint.anti_alias = true;
    let shadow_rect = rounded_rect(badge_x, badge_y - 1.0, badge_w, badge_h, badge_radius);
    // Draw slightly offset for shadow effect
    for offset in [2.0_f32, 1.0] {
        shadow_paint.set_color(Color::from_rgba(0.0, 0.0, 0.0, 0.15).unwrap());
        let sr = rounded_rect(badge_x, badge_y - offset, badge_w, badge_h, badge_radius);
        pixmap.fill_path(&sr, &shadow_paint, FillRule::Winding, Transform::identity(), None);
    }

    // Badge fill
    let badge_color = Color::from_rgba(appearance.r, appearance.g, appearance.b, 0.95).unwrap();
    let mut paint = Paint::default();
    paint.set_color(badge_color);
    paint.anti_alias = true;
    pixmap.fill_path(&badge_rect, &paint, FillRule::Winding, Transform::identity(), None);

    // Badge border
    let border_color = Color::from_rgba(
        (appearance.r * 0.5).min(1.0),
        (appearance.g * 0.5).min(1.0),
        (appearance.b * 0.5).min(1.0),
        0.8,
    ).unwrap();
    let mut stroke = Stroke::default();
    stroke.width = (sz * 0.012).max(1.0);
    paint.set_color(border_color);
    pixmap.stroke_path(&badge_rect, &paint, &stroke, Transform::identity(), None);

    // ── Badge text ──
    let font_data = include_bytes!("../assets/fonts/Inter-Bold.ttf");
    let font = FontRef::try_from_slice(font_data)?;

    let font_size = if is_multiline {
        badge_h * 0.30
    } else {
        badge_h * 0.55
    };
    let scale = PxScale::from(font_size);
    let scaled_font = font.as_scaled(scale);

    let line_height = scaled_font.height();
    let total_text_height = line_height * line_count as f32;
    let text_start_y = badge_y + (badge_h - total_text_height) / 2.0 + scaled_font.ascent();

    for (i, line) in lines.iter().enumerate() {
        let y = text_start_y + i as f32 * line_height;
        draw_text_centered(&mut pixmap, &scaled_font, line, sz / 2.0, y);
    }

    // ── Encode to PNG ──
    let img = image::RgbaImage::from_raw(size, size, pixmap.data().to_vec())
        .ok_or_else(|| anyhow::anyhow!("Failed to create image"))?;

    let mut png_bytes = Vec::new();
    let cursor = Cursor::new(&mut png_bytes);
    let encoder = image::codecs::png::PngEncoder::new(cursor);
    image::ImageEncoder::write_image(
        encoder,
        img.as_raw(),
        size,
        size,
        image::ExtendedColorType::Rgba8,
    )?;

    Ok(png_bytes)
}

/// Generate all rig icons to a directory.
pub fn generate_all_icons(output_dir: &Path, sizes: &[u32]) -> anyhow::Result<()> {
    std::fs::create_dir_all(output_dir)?;

    for (rig_type, appearance) in RIG_TYPES {
        for &size in sizes {
            let png_data = generate_icon(appearance, size)?;
            let filename = if sizes.len() == 1 {
                format!("{rig_type}.png")
            } else {
                format!("{rig_type}_{size}.png")
            };
            let path = output_dir.join(&filename);
            std::fs::write(&path, &png_data)?;
            println!("  Generated {filename} ({size}x{size})");
        }
    }

    Ok(())
}

/// Draw white text centered horizontally at (cx, baseline_y).
fn draw_text_centered(
    pixmap: &mut Pixmap,
    font: &ab_glyph::PxScaleFont<&FontRef>,
    text: &str,
    cx: f32,
    baseline_y: f32,
) {
    let mut total_width = 0.0_f32;
    let mut prev: Option<ab_glyph::GlyphId> = None;
    for ch in text.chars() {
        let glyph_id = font.glyph_id(ch);
        if let Some(p) = prev {
            total_width += font.kern(p, glyph_id);
        }
        total_width += font.h_advance(glyph_id);
        prev = Some(glyph_id);
    }

    let start_x = cx - total_width / 2.0;
    let w = pixmap.width();
    let h = pixmap.height();
    let pixels = pixmap.pixels_mut();

    let mut x = start_x;
    prev = None;
    for ch in text.chars() {
        let glyph_id = font.glyph_id(ch);
        if let Some(p) = prev {
            x += font.kern(p, glyph_id);
        }

        let glyph = glyph_id.with_scale_and_position(font.scale(), ab_glyph::point(x, baseline_y));
        if let Some(outlined) = font.outline_glyph(glyph) {
            let bounds = outlined.px_bounds();
            outlined.draw(|gx, gy, coverage| {
                let px = bounds.min.x as i32 + gx as i32;
                let py = bounds.min.y as i32 + gy as i32;
                if px >= 0 && py >= 0 && (px as u32) < w && (py as u32) < h {
                    let alpha = (coverage * 255.0) as u8;
                    if alpha > 0 {
                        let idx = (py as u32 * w + px as u32) as usize;
                        let existing = pixels[idx];
                        let sa = alpha as f32 / 255.0;
                        let da = existing.alpha() as f32 / 255.0;
                        let out_a = sa + da * (1.0 - sa);
                        if out_a > 0.0 {
                            let blend = |src: u8, dst: u8| -> u8 {
                                ((src as f32 * sa + dst as f32 * da * (1.0 - sa)) / out_a) as u8
                            };
                            let nr = blend(255, existing.red());
                            let ng = blend(255, existing.green());
                            let nb = blend(255, existing.blue());
                            let na = (out_a * 255.0) as u8;
                            pixels[idx] = ColorU8::from_rgba(nr, ng, nb, na).premultiply();
                        }
                    }
                }
            });
        }

        x += font.h_advance(glyph_id);
        prev = Some(glyph_id);
    }
}

/// Create a rounded rectangle path.
fn rounded_rect(x: f32, y: f32, w: f32, h: f32, r: f32) -> tiny_skia::Path {
    let mut pb = PathBuilder::new();
    pb.move_to(x + r, y);
    pb.line_to(x + w - r, y);
    pb.quad_to(x + w, y, x + w, y + r);
    pb.line_to(x + w, y + h - r);
    pb.quad_to(x + w, y + h, x + w - r, y + h);
    pb.line_to(x + r, y + h);
    pb.quad_to(x, y + h, x, y + h - r);
    pb.line_to(x, y + r);
    pb.quad_to(x, y, x + r, y);
    pb.close();
    pb.finish().unwrap()
}
