//! Offline icon generation for wrapper .app bundles.
//!
//! Shells out to a Swift snippet that uses the same native macOS APIs
//! (NSImage, NSBezierPath, NSFont boldSystemFont, NSShadow) as the runtime
//! `dock_icon.rs`, so the generated `.icns` icons are pixel-identical to
//! what appears in the Dock at launch time.
//!
//! Pipeline: Swift renders a composited NSImage → writes a TIFF →
//! `sips` converts to `.icns`.

use std::path::Path;
use std::process::{Command, Stdio};

/// RGB + badge for a rig type. Mirrors `dock_icon.rs` in reaper-extension.
pub struct RigAppearance {
    pub r: f64,
    pub g: f64,
    pub b: f64,
    pub badge: &'static str,
}

pub fn rig_appearance(rig_type: &str) -> Option<RigAppearance> {
    Some(match rig_type {
        "guitar" => RigAppearance {
            r: 0.231,
            g: 0.510,
            b: 0.965,
            badge: "GUITAR",
        },
        "bass" => RigAppearance {
            r: 0.918,
            g: 0.702,
            b: 0.031,
            badge: "BASS",
        },
        "keys" => RigAppearance {
            r: 0.133,
            g: 0.773,
            b: 0.369,
            badge: "KEYS",
        },
        "drums" => RigAppearance {
            r: 0.937,
            g: 0.267,
            b: 0.267,
            badge: "DRUMS",
        },
        "drum-enhancement" => RigAppearance {
            r: 0.976,
            g: 0.451,
            b: 0.086,
            badge: "DRUM\nENHANCE",
        },
        "vocals" => RigAppearance {
            r: 0.925,
            g: 0.282,
            b: 0.600,
            badge: "VOCALS",
        },
        "session" => RigAppearance {
            r: 0.400,
            g: 0.620,
            b: 0.900,
            badge: "TRACKS",
        },
        "testing" => RigAppearance {
            r: 0.300,
            g: 0.300,
            b: 0.300,
            badge: "TEST",
        },
        "fts-folder" => RigAppearance {
            r: 0.200,
            g: 0.500,
            b: 0.850,
            badge: "FTS",
        },
        _ => return None,
    })
}

/// Generate a tinted+badged `.icns` for the given rig type using native
/// macOS rendering (Swift subprocess).
///
/// This produces icons identical to the runtime dock icon compositing in
/// `dock_icon.rs`, using the same NSBezierPath rounded rects, bold system
/// font, and NSShadow.
pub fn generate_rig_icon(
    base_icns: &Path,
    output_icns: &Path,
    rig_type: &str,
) -> Result<(), String> {
    let appearance =
        rig_appearance(rig_type).ok_or_else(|| format!("Unknown rig type: {rig_type}"))?;

    let swift_source = build_swift_source(
        base_icns,
        output_icns,
        appearance.r,
        appearance.g,
        appearance.b,
        appearance.badge,
        rig_type,
    );

    // Compile and run the Swift snippet
    // Use the CLT Swift directly with a clean environment to avoid
    // Nix SDK interference (Nix sets SDKROOT/DEVELOPER_DIR to its own
    // Apple SDK which lacks Swift support).
    let swift_bin = "/Library/Developer/CommandLineTools/usr/bin/swift";
    let output = Command::new(swift_bin)
        .args(["-e", &swift_source])
        .env_clear()
        .env("HOME", std::env::var("HOME").unwrap_or_default())
        .env("PATH", "/usr/bin:/bin:/usr/sbin:/sbin")
        .env("DEVELOPER_DIR", "/Library/Developer/CommandLineTools")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Failed to run swift: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Swift icon generation failed: {stderr}"));
    }

    Ok(())
}

fn build_swift_source(
    base_icns: &Path,
    output_icns: &Path,
    r: f64,
    g: f64,
    b: f64,
    badge: &str,
    rig_type: &str,
) -> String {
    let base_path = base_icns.display();
    let output_path = output_icns.display();
    // Escape newlines in badge text for Swift string literal
    let badge_escaped = badge.replace('\n', "\\n");
    let is_multiline = badge.contains('\n');
    let line_count = badge.lines().count();

    format!(
        r##"
import AppKit

// ── Load base icon ──────────────────────────────────────────
guard let base = NSImage(contentsOfFile: "{base_path}") else {{
    fputs("Failed to load base icon: {base_path}\n", stderr)
    exit(1)
}}

let size = base.size

// ── Render at multiple sizes for .iconset ───────────────────
// Standard macOS icon sizes (points and @2x pixel variants)
let iconSizes: [(String, CGFloat)] = [
    ("icon_16x16", 16),
    ("icon_16x16@2x", 32),
    ("icon_32x32", 32),
    ("icon_32x32@2x", 64),
    ("icon_128x128", 128),
    ("icon_128x128@2x", 256),
    ("icon_256x256", 256),
    ("icon_256x256@2x", 512),
    ("icon_512x512", 512),
    ("icon_512x512@2x", 1024),
]

let tmpDir = NSTemporaryDirectory() + "fts-icon-{rig_type}"
let iconsetPath = tmpDir + "/output.iconset"

// Clean up previous run
try? FileManager.default.removeItem(atPath: tmpDir)
try! FileManager.default.createDirectory(atPath: iconsetPath,
    withIntermediateDirectories: true)

for (name, px) in iconSizes {{
    let renderSize = NSSize(width: px, height: px)

    let composite = NSImage(size: renderSize)
    composite.lockFocus()

    let fullRect = NSRect(origin: .zero, size: renderSize)

    // Draw base icon scaled to this size
    base.draw(in: fullRect, from: .zero, operation: .sourceOver, fraction: 1.0)

    // ── Apply color tint (SourceAtop) ───────────────────────
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current?.compositingOperation = .sourceAtop

    let tint = NSColor(red: {r}, green: {g}, blue: {b}, alpha: 0.3)
    tint.setFill()
    NSBezierPath(rect: fullRect).fill()

    NSGraphicsContext.restoreGraphicsState()

    // ── Skip badge on very small icons ──────────────────────
    if px >= 64 {{
        let isMultiline = {is_multiline}
        let lineCount = CGFloat({line_count})

        let badgeW = renderSize.width * 0.55
        let badgeH: CGFloat = isMultiline
            ? renderSize.height * 0.14 * lineCount
            : renderSize.height * 0.16
        let badgeX = (renderSize.width - badgeW) / 2.0
        // Inset from bottom edge to stay within the icon's rounded squircle
        let badgeY = renderSize.height * 0.12
        let badgeRect = NSRect(x: badgeX, y: badgeY, width: badgeW, height: badgeH)
        let badgeRadius: CGFloat = isMultiline ? badgeH * 0.25 : badgeH / 2.0

        // Badge shadow
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowOffset = NSSize(width: 0, height: -1)
        shadow.shadowBlurRadius = 4.0
        shadow.shadowColor = NSColor(white: 0, alpha: 0.6)
        shadow.set()

        // Badge fill
        let badgeColor = NSColor(red: {r}, green: {g}, blue: {b}, alpha: 0.95)
        badgeColor.setFill()
        let badgePath = NSBezierPath(roundedRect: badgeRect,
            xRadius: badgeRadius, yRadius: badgeRadius)
        badgePath.fill()

        NSGraphicsContext.restoreGraphicsState()

        // Badge border
        let borderColor = NSColor(red: {r} * 0.5, green: {g} * 0.5,
            blue: {b} * 0.5, alpha: 0.8)
        borderColor.setStroke()
        badgePath.lineWidth = 1.5
        badgePath.stroke()

        // Badge text
        let fontSize: CGFloat = isMultiline ? badgeH * 0.30 : badgeH * 0.55
        let font = NSFont.boldSystemFont(ofSize: fontSize)
        let paraStyle = NSMutableParagraphStyle()
        paraStyle.alignment = .center

        let attrs: [NSAttributedString.Key: Any] = [
            .font: font,
            .foregroundColor: NSColor.white,
            .paragraphStyle: paraStyle,
        ]

        let badgeText = "{badge_escaped}"
        let textSize = badgeText.size(withAttributes: attrs)
        let textY = badgeY + (badgeH - textSize.height) / 2.0
        let textRect = NSRect(x: badgeX, y: textY,
            width: badgeW, height: textSize.height)
        badgeText.draw(in: textRect, withAttributes: attrs)
    }}

    composite.unlockFocus()

    // Write PNG
    guard let tiffData = composite.tiffRepresentation,
          let bitmapRep = NSBitmapImageRep(data: tiffData),
          let pngData = bitmapRep.representation(using: .png, properties: [:])
    else {{
        fputs("Failed to create PNG for \(name)\n", stderr)
        exit(1)
    }}

    let pngPath = iconsetPath + "/\(name).png"
    try! pngData.write(to: URL(fileURLWithPath: pngPath))
}}

// ── Pack into .icns ─────────────────────────────────────────
let outputPath = "{output_path}"
// Ensure parent directory exists
let parentDir = (outputPath as NSString).deletingLastPathComponent
try? FileManager.default.createDirectory(atPath: parentDir,
    withIntermediateDirectories: true)

let process = Process()
process.executableURL = URL(fileURLWithPath: "/usr/bin/iconutil")
process.arguments = ["-c", "icns", "-o", outputPath, iconsetPath]
try! process.run()
process.waitUntilExit()

if process.terminationStatus != 0 {{
    fputs("iconutil failed\n", stderr)
    exit(1)
}}

// Clean up
try? FileManager.default.removeItem(atPath: tmpDir)
"##,
        rig_type = rig_type,
    )
}
