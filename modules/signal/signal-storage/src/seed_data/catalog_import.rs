//! Import Neural DSP catalog from disk into domain `Preset`/`Snapshot` types.
//!
//! Reads the file-based catalog at `~/Music/FastTrackStudio/Library/` (produced
//! by `cargo xtask catalog`) and converts each NDSP plugin into a `Preset`
//! with `BlockType::Custom`, one `Snapshot` per factory preset.
//!
//! If the catalog directory is missing, returns an empty `Vec` gracefully.

use std::path::Path;

use signal_proto::catalog::{BlockMetadata, Catalog, SnapshotMetadata};
use signal_proto::metadata::Metadata;
use signal_proto::{seed_id, Block, BlockType, Preset, Snapshot};

/// Read the catalog from `library_path` and return one `Preset` per NDSP plugin.
///
/// Each plugin becomes a `Preset` with `BlockType::Custom`. Each factory preset
/// on disk becomes a `Snapshot` within that collection.
///
/// Returns an empty `Vec` if the catalog directory doesn't exist.
pub fn catalog_block_collections(library_path: &Path) -> Vec<Preset> {
    let catalog_path = library_path.join("catalog.json");
    if !catalog_path.exists() {
        return Vec::new();
    }

    let catalog_json = match std::fs::read_to_string(&catalog_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[signal-storage] Failed to read catalog.json: {e}");
            return Vec::new();
        }
    };

    let catalog: Catalog = match serde_json::from_str(&catalog_json) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("[signal-storage] Failed to parse catalog.json: {e}");
            return Vec::new();
        }
    };

    let mut presets = Vec::new();

    for plugin in &catalog.plugins {
        let block_dir = library_path
            .join("blocks/plugin/neural-dsp")
            .join(&plugin.slug);

        // Read block.json for plugin metadata
        let block_json_path = block_dir.join("block.json");
        let block_meta: Option<BlockMetadata> = std::fs::read_to_string(&block_json_path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok());

        let plugin_name = block_meta
            .as_ref()
            .map(|m| m.name.clone())
            .unwrap_or_else(|| plugin.name.clone());

        // Collect all snapshot JSONs recursively
        let snapshots_dir = block_dir.join("snapshots");
        let mut snapshot_metas = Vec::new();
        if snapshots_dir.exists() {
            collect_snapshot_metas(&snapshots_dir, &mut snapshot_metas);
        }

        if snapshot_metas.is_empty() {
            continue;
        }

        // Sort alphabetically by name for stable ordering
        snapshot_metas.sort_by(|a, b| a.name.cmp(&b.name));

        // Convert to domain Snapshots
        let domain_snapshots: Vec<Snapshot> = snapshot_metas
            .iter()
            .map(|meta| {
                // Include folder in seed key to avoid collisions — multiple
                // folders can contain presets with the same name (e.g., "Clean"
                // in both "Artists/Plini" and "Artists/Ryan Lerman").
                let seed_key = if meta.folder.is_empty() {
                    format!("ndsp-{}-{}", plugin.slug, meta.id)
                } else {
                    format!(
                        "ndsp-{}-{}-{}",
                        plugin.slug,
                        signal_proto::catalog::slugify(&meta.folder),
                        meta.id
                    )
                };
                let snapshot_id = seed_id(&seed_key);
                let mut metadata = Metadata::new().with_tag("Neural DSP");

                if !meta.folder.is_empty() {
                    metadata = metadata.with_folder(&meta.folder);
                }
                for tag in &meta.tags {
                    metadata = metadata.with_tag(tag);
                }

                Snapshot::new(snapshot_id, &meta.name, Block::from_parameters(vec![]))
                    .with_metadata(metadata)
            })
            .collect();

        // First snapshot is default, rest are additional
        let default = domain_snapshots[0].clone();
        let additional: Vec<Snapshot> = domain_snapshots.into_iter().skip(1).collect();

        let preset_id = seed_id(&format!("ndsp-{}", plugin.slug));
        let preset_metadata = Metadata::new()
            .with_tag("Neural DSP")
            .with_tag(&plugin_name);

        let preset = Preset::new(
            preset_id,
            &plugin_name,
            BlockType::Custom,
            default,
            additional,
        )
        .with_metadata(preset_metadata);

        presets.push(preset);
    }

    presets
}

/// Recursively walk a directory collecting `SnapshotMetadata` from `*.json` files.
fn collect_snapshot_metas(dir: &Path, out: &mut Vec<SnapshotMetadata>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_snapshot_metas(&path, out);
        } else if path.extension().map_or(false, |ext| ext == "json") {
            if let Ok(contents) = std::fs::read_to_string(&path) {
                if let Ok(meta) = serde_json::from_str::<SnapshotMetadata>(&contents) {
                    out.push(meta);
                }
            }
        }
    }
}
