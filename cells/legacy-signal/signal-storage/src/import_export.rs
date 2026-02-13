//! Preset import/export — serialize presets to/from JSON via facet-json.
//!
//! Uses [`facet_json`] for serialization instead of serde, consistent with
//! the signal-proto domain types which derive [`Facet`] rather than `Serialize`/`Deserialize`.

use std::path::Path;

use signal_proto::import_export::{
    BundleMetadata, ExportError, ImportError, PresetBundle, BUNDLE_FORMAT_VERSION,
};
use signal_proto::preset::Preset;

// ─────────────────────────────────────────────────────────────────────────────
// JSON serialization
// ─────────────────────────────────────────────────────────────────────────────

/// Export a single preset to a pretty-printed JSON string.
pub fn export_preset_json(preset: &Preset) -> Result<String, ExportError> {
    let bundle = PresetBundle::single(preset.clone(), BundleMetadata::new(now_iso8601()));
    facet_json::to_string_pretty(&bundle).map_err(|e| ExportError::Serialization(e.to_string()))
}

/// Import a single preset from a JSON string.
///
/// If the bundle contains multiple presets, returns the first one.
pub fn import_preset_json(json: &str) -> Result<Preset, ImportError> {
    let bundle: PresetBundle =
        facet_json::from_str(json).map_err(|e| ImportError::Deserialization(e.to_string()))?;

    if bundle.version > BUNDLE_FORMAT_VERSION {
        return Err(ImportError::IncompatibleVersion {
            found: bundle.version,
            expected: BUNDLE_FORMAT_VERSION,
        });
    }

    bundle
        .presets
        .into_iter()
        .next()
        .ok_or(ImportError::EmptyBundle)
}

/// Export multiple presets to a pretty-printed JSON bundle string.
pub fn export_bundle_json(presets: &[Preset]) -> Result<String, ExportError> {
    let bundle = PresetBundle::multiple(presets.to_vec(), BundleMetadata::new(now_iso8601()));
    facet_json::to_string_pretty(&bundle).map_err(|e| ExportError::Serialization(e.to_string()))
}

/// Import all presets from a JSON bundle string.
pub fn import_bundle_json(json: &str) -> Result<PresetBundle, ImportError> {
    let bundle: PresetBundle =
        facet_json::from_str(json).map_err(|e| ImportError::Deserialization(e.to_string()))?;

    if bundle.version > BUNDLE_FORMAT_VERSION {
        return Err(ImportError::IncompatibleVersion {
            found: bundle.version,
            expected: BUNDLE_FORMAT_VERSION,
        });
    }

    if bundle.presets.is_empty() {
        return Err(ImportError::EmptyBundle);
    }

    Ok(bundle)
}

// ─────────────────────────────────────────────────────────────────────────────
// File I/O
// ─────────────────────────────────────────────────────────────────────────────

/// Export a single preset to a JSON file. Creates parent directories if needed.
pub fn export_to_file(preset: &Preset, path: &Path) -> Result<(), ExportError> {
    let json = export_preset_json(preset)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ExportError::Io(e.to_string()))?;
    }
    std::fs::write(path, json).map_err(|e| ExportError::Io(e.to_string()))
}

/// Import a single preset from a JSON file.
pub fn import_from_file(path: &Path) -> Result<Preset, ImportError> {
    let json = std::fs::read_to_string(path).map_err(|e| ImportError::Io(e.to_string()))?;
    import_preset_json(&json)
}

/// Export multiple presets to a JSON bundle file.
pub fn export_bundle_to_file(presets: &[Preset], path: &Path) -> Result<(), ExportError> {
    let json = export_bundle_json(presets)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ExportError::Io(e.to_string()))?;
    }
    std::fs::write(path, json).map_err(|e| ExportError::Io(e.to_string()))
}

/// Import a preset bundle from a JSON file.
pub fn import_bundle_from_file(path: &Path) -> Result<PresetBundle, ImportError> {
    let json = std::fs::read_to_string(path).map_err(|e| ImportError::Io(e.to_string()))?;
    import_bundle_json(&json)
}

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Generate a simple ISO-8601 UTC timestamp.
fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339()
}
