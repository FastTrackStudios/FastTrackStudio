//! Per-role appearance customization (dock icon + color theme).
//!
//! On startup, reads `FTS_DAW_ROLE` and applies:
//! - A custom macOS dock icon (`.icns`) so instances are visually distinct
//! - A REAPER color theme (`.ReaperThemeZip`) per role
//!
//! Assets live in the FTS Library:
//! - Icons:  `Library/assets/icons/reaper/mac/`
//! - Themes: `Library/assets/themes/`

const ASSETS_DIR: &str = "/Users/codywright/Music/FastTrackStudio/Library/assets";

// ---------------------------------------------------------------------------
// Dock icon
// ---------------------------------------------------------------------------

#[cfg(target_os = "macos")]
pub fn set_dock_icon_for_role(role: &str) {
    use cocoa::base::{id, nil};
    use cocoa::foundation::NSString;
    use objc::{class, msg_send, sel, sel_impl};

    let icon_filename = match role.to_lowercase().as_str() {
        "session" => "REAPER-Icon-04.icns",
        "signal" => "reaper-icon-05.icns",
        _ => return,
    };

    let icon_path = format!("{}/icons/reaper/mac/{}", ASSETS_DIR, icon_filename);

    if !std::path::Path::new(&icon_path).exists() {
        tracing::warn!("Dock icon not found: {}", icon_path);
        return;
    }

    unsafe {
        let path_nsstring = NSString::alloc(nil).init_str(&icon_path);

        // [[NSImage alloc] initWithContentsOfFile:path]
        let image: id = msg_send![class!(NSImage), alloc];
        let image: id = msg_send![image, initWithContentsOfFile: path_nsstring];

        if image == nil {
            tracing::warn!("Failed to load dock icon from: {}", icon_path);
            return;
        }

        // [[NSApplication sharedApplication] setApplicationIconImage:image]
        let app: id = msg_send![class!(NSApplication), sharedApplication];
        let _: () = msg_send![app, setApplicationIconImage: image];

        tracing::info!("Dock icon set for role '{}' from {}", role, icon_filename);
    }
}

#[cfg(not(target_os = "macos"))]
pub fn set_dock_icon_for_role(_role: &str) {}

// ---------------------------------------------------------------------------
// Color theme
// ---------------------------------------------------------------------------

/// Load a REAPER color theme based on the DAW role.
///
/// Uses the low-level `OpenColorThemeFile` API which must be called on the
/// main thread (we're already on the main thread during extension init).
pub fn set_theme_for_role(role: &str) {
    let theme_filename = match role.to_lowercase().as_str() {
        "session" => "nvk_THEME_Light.ReaperThemeZip",
        "signal" => "Reapertips Theme.ReaperThemeZip",
        _ => return,
    };

    let theme_path = format!("{}/themes/{}", ASSETS_DIR, theme_filename);

    if !std::path::Path::new(&theme_path).exists() {
        tracing::warn!("Theme file not found: {}", theme_path);
        return;
    }

    let c_path = match std::ffi::CString::new(theme_path.clone()) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Invalid theme path: {e}");
            return;
        }
    };

    let low = reaper_high::Reaper::get().medium_reaper().low();
    let ok = unsafe { low.OpenColorThemeFile(c_path.as_ptr()) };

    if ok {
        tracing::info!("Theme loaded for role '{}': {}", role, theme_filename);
    } else {
        tracing::warn!("Failed to load theme for role '{}': {}", role, theme_path);
    }
}
