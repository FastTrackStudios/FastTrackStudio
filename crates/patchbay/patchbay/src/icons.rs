//! Freedesktop icon resolution → `data:` URIs.
//!
//! Nodes carry `application.icon-name`; remotes can't read this host's
//! icon themes, so the service resolves names to image bytes and ships
//! them inline. Lookup is deliberately simple: hicolor + pixmaps across
//! the XDG data dirs — no theme inheritance, no index.theme parsing.

use std::collections::HashMap;
use std::path::PathBuf;

use parking_lot::Mutex;

/// Resolved-name cache (misses cached too — lookups hit the disk).
#[derive(Default)]
pub(crate) struct IconCache {
    cache: Mutex<HashMap<String, Option<String>>>,
}

impl IconCache {
    /// `data:` URI for a freedesktop icon name, if any theme dir has it.
    pub fn data_uri(&self, name: &str) -> Option<String> {
        if name.trim().is_empty() {
            return None;
        }
        if let Some(hit) = self.cache.lock().get(name) {
            return hit.clone();
        }
        let resolved = resolve(name).and_then(|p| {
            let mime = match p.extension().and_then(|e| e.to_str()) {
                Some("svg") => "image/svg+xml",
                Some("png") => "image/png",
                Some("xpm") => return None, // webviews don't render xpm
                _ => return None,
            };
            let bytes = std::fs::read(&p).ok()?;
            Some(format!("data:{mime};base64,{}", base64(&bytes)))
        });
        self.cache.lock().insert(name.to_string(), resolved.clone());
        resolved
    }
}

/// Search order: biggest crisp raster first, then scalable, then pixmaps.
const SIZES: &[&str] = &["128x128", "96x96", "64x64", "48x48", "256x256", "32x32"];

fn data_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Some(home) = dirs::data_dir() {
        dirs.push(home); // ~/.local/share
    }
    let xdg = std::env::var("XDG_DATA_DIRS")
        .unwrap_or_else(|_| "/usr/local/share:/usr/share".to_string());
    dirs.extend(xdg.split(':').filter(|s| !s.is_empty()).map(PathBuf::from));
    // NixOS system profile isn't always in XDG_DATA_DIRS for user units.
    dirs.push(PathBuf::from("/run/current-system/sw/share"));
    dirs
}

fn resolve(name: &str) -> Option<PathBuf> {
    // Some apps put a full path in the property.
    let as_path = PathBuf::from(name);
    if as_path.is_absolute() && as_path.is_file() {
        return Some(as_path);
    }
    for dir in data_dirs() {
        for size in SIZES {
            let p = dir.join(format!("icons/hicolor/{size}/apps/{name}.png"));
            if p.is_file() {
                return Some(p);
            }
        }
        let svg = dir.join(format!("icons/hicolor/scalable/apps/{name}.svg"));
        if svg.is_file() {
            return Some(svg);
        }
        for ext in ["png", "svg"] {
            let p = dir.join(format!("pixmaps/{name}.{ext}"));
            if p.is_file() {
                return Some(p);
            }
        }
    }
    None
}

fn base64(bytes: &[u8]) -> String {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let b = [chunk[0], *chunk.get(1).unwrap_or(&0), *chunk.get(2).unwrap_or(&0)];
        let n = u32::from_be_bytes([0, b[0], b[1], b[2]]);
        for i in 0..4 {
            if i <= chunk.len() {
                out.push(TABLE[(n >> (18 - 6 * i)) as usize & 63] as char);
            } else {
                out.push('=');
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::base64;

    #[test]
    fn base64_matches_known_vectors() {
        assert_eq!(base64(b""), "");
        assert_eq!(base64(b"f"), "Zg==");
        assert_eq!(base64(b"fo"), "Zm8=");
        assert_eq!(base64(b"foo"), "Zm9v");
        assert_eq!(base64(b"foobar"), "Zm9vYmFy");
    }
}
