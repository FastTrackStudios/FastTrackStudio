//! Persistence — save/load dock presets and layouts to JSON.
//!
//! Provides helpers for serializing the entire preset collection
//! and individual layouts to JSON strings or files.

use crate::layout::DockLayout;
use crate::preset::PresetCollection;

/// Serialize a preset collection to a pretty-printed JSON string.
pub fn presets_to_json(presets: &PresetCollection) -> Result<String, Box<dyn std::error::Error>> {
    Ok(facet_json::to_string_pretty(presets)?)
}

/// Deserialize a preset collection from a JSON string.
pub fn presets_from_json(json: &str) -> Result<PresetCollection, Box<dyn std::error::Error>> {
    Ok(facet_json::from_str(json)?)
}

/// Serialize a single layout to a JSON string.
pub fn layout_to_json(layout: &DockLayout) -> Result<String, Box<dyn std::error::Error>> {
    Ok(facet_json::to_string_pretty(layout)?)
}

/// Deserialize a single layout from a JSON string.
pub fn layout_from_json(json: &str) -> Result<DockLayout, Box<dyn std::error::Error>> {
    Ok(facet_json::from_str(json)?)
}

/// Save presets to a file. Creates parent directories if needed.
pub fn save_presets_to_file(
    presets: &PresetCollection,
    path: &std::path::Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let json = presets_to_json(presets)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, json)?;
    Ok(())
}

/// Load presets from a file. Returns None if the file doesn't exist.
pub fn load_presets_from_file(
    path: &std::path::Path,
) -> Result<Option<PresetCollection>, Box<dyn std::error::Error>> {
    if !path.exists() {
        return Ok(None);
    }
    let json = std::fs::read_to_string(path)?;
    let presets = presets_from_json(&json)?;
    Ok(Some(presets))
}

/// Get the default presets file path for the application.
///
/// Returns `~/.config/fts-control/dock-presets.json` on Unix
/// or `%APPDATA%/fts-control/dock-presets.json` on Windows.
pub fn default_presets_path() -> Option<std::path::PathBuf> {
    dirs_or_home().map(|dir| dir.join("dock-presets.json"))
}

fn dirs_or_home() -> Option<std::path::PathBuf> {
    // Try XDG config dir first, fall back to home
    if let Some(config) = std::env::var_os("XDG_CONFIG_HOME") {
        return Some(std::path::PathBuf::from(config).join("fts-control"));
    }
    if let Ok(home) = std::env::var("HOME") {
        return Some(
            std::path::PathBuf::from(home)
                .join(".config")
                .join("fts-control"),
        );
    }
    #[cfg(target_os = "windows")]
    if let Some(appdata) = std::env::var_os("APPDATA") {
        return Some(std::path::PathBuf::from(appdata).join("fts-control"));
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::defaults::default_presets;

    #[test]
    fn json_roundtrip() {
        let presets = default_presets();
        let json = presets_to_json(&presets).unwrap();
        let restored = presets_from_json(&json).unwrap();
        assert_eq!(presets.presets.len(), restored.presets.len());
        for (orig, rest) in presets.presets.iter().zip(restored.presets.iter()) {
            assert_eq!(orig.name, rest.name);
            assert_eq!(orig.layout.node_count(), rest.layout.node_count());
        }
    }

    #[test]
    fn file_roundtrip() {
        let dir = std::env::temp_dir().join("dock_proto_test");
        let path = dir.join("test-presets.json");

        let presets = default_presets();
        save_presets_to_file(&presets, &path).unwrap();

        let loaded = load_presets_from_file(&path).unwrap().unwrap();
        assert_eq!(loaded.presets.len(), presets.presets.len());

        // Cleanup
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn load_nonexistent_returns_none() {
        let path = std::path::Path::new("/tmp/dock_proto_test_nonexistent.json");
        let result = load_presets_from_file(path).unwrap();
        assert!(result.is_none());
    }
}
