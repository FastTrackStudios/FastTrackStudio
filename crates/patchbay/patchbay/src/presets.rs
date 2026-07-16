//! Preset + alias persistence — plain JSON under the fts config dir.
//!
//! Presets are connection memory (RaySession's jackpatch idea): links
//! remembered by stable (node.name, port.name) pairs, re-applied
//! incrementally against whatever half of the graph currently exists.

use std::fs;
use std::path::PathBuf;

use parking_lot::Mutex;
use patchbay_proto::{AliasEntry, PresetLink, RoutingPreset};

#[derive(Default, serde::Serialize, serde::Deserialize)]
struct FileFormat {
    #[serde(default)]
    presets: Vec<RoutingPreset>,
    #[serde(default)]
    aliases: Vec<AliasEntry>,
}

pub(crate) struct PresetStore {
    path: PathBuf,
    data: Mutex<FileFormat>,
}

fn config_path() -> PathBuf {
    // Override for tests / scratch instances so smoke runs never touch
    // the real config.
    if let Ok(p) = std::env::var("PATCHBAY_CONFIG") {
        return PathBuf::from(p);
    }
    dirs::config_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("fts/patchbay/patchbay.json")
}

impl PresetStore {
    pub fn open() -> Self {
        let path = config_path();
        let data = fs::read_to_string(&path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default();
        Self {
            path,
            data: Mutex::new(data),
        }
    }

    fn persist(&self, data: &FileFormat) {
        if let Some(dir) = self.path.parent() {
            let _ = fs::create_dir_all(dir);
        }
        match serde_json::to_string_pretty(data) {
            Ok(json) => {
                if let Err(e) = fs::write(&self.path, json) {
                    tracing::warn!("patchbay config write failed: {e}");
                }
            }
            Err(e) => tracing::warn!("patchbay config serialize failed: {e}"),
        }
    }

    pub fn presets(&self) -> Vec<RoutingPreset> {
        self.data.lock().presets.clone()
    }

    pub fn preset(&self, name: &str) -> Option<RoutingPreset> {
        self.data.lock().presets.iter().find(|p| p.name == name).cloned()
    }

    pub fn upsert_preset(&self, name: String, description: String, links: Vec<PresetLink>) -> RoutingPreset {
        let preset = RoutingPreset {
            name,
            description,
            links,
        };
        let mut data = self.data.lock();
        data.presets.retain(|p| p.name != preset.name);
        data.presets.push(preset.clone());
        data.presets.sort_by(|a, b| a.name.cmp(&b.name));
        self.persist(&data);
        preset
    }

    pub fn delete_preset(&self, name: &str) -> bool {
        let mut data = self.data.lock();
        let before = data.presets.len();
        data.presets.retain(|p| p.name != name);
        let removed = data.presets.len() != before;
        if removed {
            self.persist(&data);
        }
        removed
    }

    pub fn aliases(&self) -> Vec<AliasEntry> {
        self.data.lock().aliases.clone()
    }

    /// Empty alias clears the entry.
    pub fn set_alias(&self, target: String, alias: String) {
        let mut data = self.data.lock();
        data.aliases.retain(|a| a.target != target);
        if !alias.is_empty() {
            data.aliases.push(AliasEntry { target, alias });
        }
        self.persist(&data);
    }
}
