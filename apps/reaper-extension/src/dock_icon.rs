//! Per-role appearance customization (dock icon + color theme).
//!
//! On startup, reads `FTS_DAW_ROLE` and applies:
//! - A custom macOS dock icon (`.icns`) so instances are visually distinct
//! - A REAPER color theme (`.ReaperThemeZip`) per role
//!
//! For Signal instances, the icon is dynamically composited with a color tint
//! and badge text based on `FTS_RIG_TYPE` (e.g., "GTR" in blue for guitar).
//!
//! Assets live in the FTS Library:
//! - Icons:  `Library/assets/icons/reaper/mac/`
//! - Themes: `Library/assets/themes/`

const ASSETS_DIR: &str = "/Users/codywright/Music/FastTrackStudio/Library/assets";

/// Base icon used for session instances.
const SESSION_ICON: &str = "REAPER-Icon-04.icns";

/// Base icon used for signal instances (tinted + badged per rig type).
const SIGNAL_ICON: &str = "reaper-icon-05.icns";

// ---------------------------------------------------------------------------
// Rig type appearance config
// ---------------------------------------------------------------------------

struct RigAppearance {
    /// RGB color (0.0–1.0) matching the dashboard card color.
    r: f64,
    g: f64,
    b: f64,
    /// Short badge text (2–4 chars) drawn on the icon.
    badge: &'static str,
}

fn rig_appearance(rig_type: &str) -> Option<RigAppearance> {
    Some(match rig_type {
        "guitar" => RigAppearance {
            r: 0.231,
            g: 0.510,
            b: 0.965,
            badge: "GUITAR",
        }, // #3b82f6
        "bass" => RigAppearance {
            r: 0.918,
            g: 0.702,
            b: 0.031,
            badge: "BASS",
        }, // #eab308
        "keys" => RigAppearance {
            r: 0.133,
            g: 0.773,
            b: 0.369,
            badge: "KEYS",
        }, // #22c55e
        "drums" => RigAppearance {
            r: 0.937,
            g: 0.267,
            b: 0.267,
            badge: "DRUMS",
        }, // #ef4444
        "drum-enhancement" => RigAppearance {
            r: 0.976,
            g: 0.451,
            b: 0.086,
            badge: "DRUM\nENHANCE",
        }, // #f97316
        "vocals" => RigAppearance {
            r: 0.925,
            g: 0.282,
            b: 0.600,
            badge: "VOCALS",
        }, // #ec4899
        _ => return None,
    })
}

// ---------------------------------------------------------------------------
// Dock icon
// ---------------------------------------------------------------------------

#[cfg(target_os = "macos")]
pub fn set_dock_icon_for_role(role: &str) {
    use cocoa::base::nil;
    use cocoa::foundation::NSString;
    use objc::{class, msg_send, sel, sel_impl};

    let icon_filename = match role.to_lowercase().as_str() {
        "session" => SESSION_ICON,
        "signal" => SIGNAL_ICON,
        _ => return,
    };

    let icon_path = format!("{}/icons/reaper/mac/{}", ASSETS_DIR, icon_filename);

    if !std::path::Path::new(&icon_path).exists() {
        tracing::warn!("Dock icon not found: {}", icon_path);
        return;
    }

    // For signal instances, try to create a tinted + badged icon per rig type.
    if role.eq_ignore_ascii_case("signal") {
        if let Ok(rig_type) = std::env::var("FTS_RIG_TYPE") {
            if set_tinted_dock_icon(&icon_path, &rig_type) {
                return;
            }
            // Fall through to plain icon if tinting fails
        }
    }

    // Plain icon (session, or signal without rig type / tinting failure)
    unsafe {
        let path_nsstring = NSString::alloc(nil).init_str(&icon_path);

        let image: cocoa::base::id = msg_send![class!(NSImage), alloc];
        let image: cocoa::base::id = msg_send![image, initWithContentsOfFile: path_nsstring];

        if image == nil {
            tracing::warn!("Failed to load dock icon from: {}", icon_path);
            return;
        }

        let app: cocoa::base::id = msg_send![class!(NSApplication), sharedApplication];
        let _: () = msg_send![app, setApplicationIconImage: image];

        tracing::info!("Dock icon set for role '{}' from {}", role, icon_filename);
    }
}

/// Composite a tinted + badged dock icon for a signal rig type.
///
/// Returns `true` on success, `false` if the caller should fall back to the
/// plain icon.
#[cfg(target_os = "macos")]
fn set_tinted_dock_icon(base_icon_path: &str, rig_type: &str) -> bool {
    use cocoa::base::{id, nil};
    use cocoa::foundation::{NSPoint, NSRect, NSSize, NSString};
    use objc::{class, msg_send, sel, sel_impl};

    let Some(appearance) = rig_appearance(rig_type) else {
        return false;
    };

    let RigAppearance { r, g, b, badge } = appearance;

    unsafe {
        // ── Load base icon ─────────────────────────────────────────
        let path_ns = NSString::alloc(nil).init_str(base_icon_path);
        let base: id = msg_send![class!(NSImage), alloc];
        let base: id = msg_send![base, initWithContentsOfFile: path_ns];
        if base == nil {
            tracing::warn!("Failed to load base icon for tinting: {}", base_icon_path);
            return false;
        }

        let size: NSSize = msg_send![base, size];

        // ── Create composite image ────────────────────────────────
        let composite: id = msg_send![class!(NSImage), alloc];
        let composite: id = msg_send![composite, initWithSize: size];

        let _: () = msg_send![composite, lockFocus];

        let origin = NSPoint::new(0.0, 0.0);
        let zero_rect = NSRect::new(origin, NSSize::new(0.0, 0.0));
        let full_rect = NSRect::new(origin, size);

        // Draw base icon
        let _: () = msg_send![base, drawInRect: full_rect
            fromRect: zero_rect
            operation: 2u64  // NSCompositingOperationSourceOver
            fraction: 1.0f64];

        // ── Apply color tint ──────────────────────────────────────
        // SourceAtop (5) draws only where the base has non-zero alpha,
        // preserving the icon's shape and transparency.
        let ctx: id = msg_send![class!(NSGraphicsContext), currentContext];

        let _: () = msg_send![class!(NSGraphicsContext), saveGraphicsState];
        let _: () = msg_send![ctx, setCompositingOperation: 5i64]; // SourceAtop

        let tint: id = msg_send![class!(NSColor),
            colorWithRed: r
            green: g
            blue: b
            alpha: 0.3f64];
        let _: () = msg_send![tint, setFill];

        let tint_path: id = msg_send![class!(NSBezierPath), bezierPathWithRect: full_rect];
        let _: () = msg_send![tint_path, fill];

        let _: () = msg_send![class!(NSGraphicsContext), restoreGraphicsState];

        // ── Draw badge pill ───────────────────────────────────────
        // Positioned at the bottom center of the icon.
        // Multi-line badges (containing '\n') get a taller pill.
        let is_multiline = badge.contains('\n');
        let line_count = badge.lines().count() as f64;
        let badge_w = size.width * 0.55;
        let badge_h = if is_multiline {
            size.height * 0.14 * line_count
        } else {
            size.height * 0.16
        };
        let badge_x = (size.width - badge_w) / 2.0;
        // Inset from bottom edge to stay within the icon's rounded squircle
        let badge_y = size.height * 0.12;
        let badge_rect =
            NSRect::new(NSPoint::new(badge_x, badge_y), NSSize::new(badge_w, badge_h));
        let badge_radius = if is_multiline {
            badge_h * 0.25
        } else {
            badge_h / 2.0
        };

        // Badge shadow
        let shadow: id = msg_send![class!(NSShadow), alloc];
        let shadow: id = msg_send![shadow, init];
        let shadow_offset = NSSize::new(0.0, -1.0);
        let _: () = msg_send![shadow, setShadowOffset: shadow_offset];
        let _: () = msg_send![shadow, setShadowBlurRadius: 4.0f64];
        let shadow_color: id =
            msg_send![class!(NSColor), colorWithWhite: 0.0f64 alpha: 0.6f64];
        let _: () = msg_send![shadow, setShadowColor: shadow_color];

        let _: () = msg_send![class!(NSGraphicsContext), saveGraphicsState];
        let _: () = msg_send![shadow, set];

        // Badge fill
        let badge_color: id = msg_send![class!(NSColor),
            colorWithRed: r
            green: g
            blue: b
            alpha: 0.95f64];
        let _: () = msg_send![badge_color, setFill];

        let badge_path: id = msg_send![class!(NSBezierPath),
            bezierPathWithRoundedRect: badge_rect
            xRadius: badge_radius
            yRadius: badge_radius];
        let _: () = msg_send![badge_path, fill];

        let _: () = msg_send![class!(NSGraphicsContext), restoreGraphicsState];

        // Badge border (darker shade of the rig color)
        let border_color: id = msg_send![class!(NSColor),
            colorWithRed: (r * 0.5)
            green: (g * 0.5)
            blue: (b * 0.5)
            alpha: 0.8f64];
        let _: () = msg_send![border_color, setStroke];
        let _: () = msg_send![badge_path, setLineWidth: 1.5f64];
        let _: () = msg_send![badge_path, stroke];

        // ── Draw badge text ───────────────────────────────────────
        let font_size = if is_multiline {
            badge_h * 0.30
        } else {
            badge_h * 0.55
        };
        let font: id = msg_send![class!(NSFont), boldSystemFontOfSize: font_size];
        let white: id = msg_send![class!(NSColor), whiteColor];

        // Centered paragraph style for multi-line text
        let para_style: id = msg_send![class!(NSMutableParagraphStyle), new];
        let _: () = msg_send![para_style, setAlignment: 1i64]; // NSTextAlignmentCenter

        // Build attributes dictionary
        let attrs: id = msg_send![class!(NSMutableDictionary), new];
        let font_key = NSString::alloc(nil).init_str("NSFont");
        let color_key = NSString::alloc(nil).init_str("NSColor");
        let para_key = NSString::alloc(nil).init_str("NSParagraphStyle");
        let _: () = msg_send![attrs, setObject: font forKey: font_key];
        let _: () = msg_send![attrs, setObject: white forKey: color_key];
        let _: () = msg_send![attrs, setObject: para_style forKey: para_key];

        // Measure text to vertically center it, draw in rect for horizontal centering
        let text_ns = NSString::alloc(nil).init_str(badge);
        let text_size: NSSize = msg_send![text_ns, sizeWithAttributes: attrs];
        let text_y = badge_y + (badge_h - text_size.height) / 2.0;
        let text_rect = NSRect::new(
            NSPoint::new(badge_x, text_y),
            NSSize::new(badge_w, text_size.height),
        );

        let _: () = msg_send![text_ns, drawInRect: text_rect withAttributes: attrs];

        // ── Finalize ──────────────────────────────────────────────
        let _: () = msg_send![composite, unlockFocus];

        let app: id = msg_send![class!(NSApplication), sharedApplication];
        let _: () = msg_send![app, setApplicationIconImage: composite];

        tracing::info!(
            "Dock icon set for rig type '{}' (badge: '{}', color: ({:.0},{:.0},{:.0}))",
            rig_type,
            badge,
            r * 255.0,
            g * 255.0,
            b * 255.0,
        );
    }

    true
}

#[cfg(not(target_os = "macos"))]
pub fn set_dock_icon_for_role(_role: &str) {}

// ---------------------------------------------------------------------------
// Color theme
// ---------------------------------------------------------------------------

/// Theme loading is handled pre-launch by the wrapper .app launcher binary,
/// which patches `lastthemefn5` in `reaper.ini` before exec'ing REAPER.
/// This avoids the "copying file" progress dialog that `OpenColorThemeFile`
/// triggers at runtime.
///
/// Theme mappings:
///   session → nvk_THEME_Light.ReaperThemeZip
///   signal  → Reapertips Theme.ReaperThemeZip
pub fn set_theme_for_role(_role: &str) {
    // No-op: theme is set by the launcher binary before REAPER starts.
    // See `cargo xtask setup-rigs` for the pre-launch theme patching.
}
