//! Preset + alias persistence — plain JSON under the fts config dir.
//!
//! Presets are connection memory (RaySession's jackpatch idea): links
//! remembered by stable (node.name, port.name) pairs, re-applied
//! incrementally against whatever half of the graph currently exists.

use std::fs;
use std::path::PathBuf;

use parking_lot::Mutex;
use patchbay_proto::{AliasEntry, CanvasView, ColorEntry, PresetLink, RoutingPreset, VirtualSink};

#[derive(Default, serde::Serialize, serde::Deserialize)]
struct FileFormat {
    #[serde(default)]
    presets: Vec<RoutingPreset>,
    #[serde(default)]
    aliases: Vec<AliasEntry>,
    #[serde(default)]
    latency_rules: Vec<patchbay_proto::LatencyRule>,
    #[serde(default)]
    colors: Vec<ColorEntry>,
    #[serde(default)]
    virtual_sinks: Vec<VirtualSink>,
    #[serde(default)]
    views: Vec<CanvasView>,
}

/// First-run channel names for a stock REAPER JACK client: the main
/// stereo pair + click, the studio's baseline output map. Pure aliases
/// (identity is names), so they apply whenever REAPER shows up and the
/// user extends/overwrites them like any other alias.
fn seed_defaults() -> Vec<AliasEntry> {
    [
        ("REAPER:out1", "Main Output ST L"),
        ("REAPER:out2", "Main Output ST R"),
        ("REAPER:out3", "Click"),
    ]
    .into_iter()
    .map(|(target, alias)| AliasEntry {
        target: target.into(),
        alias: alias.into(),
    })
    .collect()
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
        let data = match fs::read_to_string(&path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
        {
            Some(data) => data,
            // Fresh install: start from the REAPER baseline so the
            // main outs/click are named the first time it appears.
            None => FileFormat {
                aliases: seed_defaults(),
                ..FileFormat::default()
            },
        };
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

    pub fn latency_rules(&self) -> Vec<patchbay_proto::LatencyRule> {
        self.data.lock().latency_rules.clone()
    }

    pub fn set_latency_rule(&self, rule: patchbay_proto::LatencyRule) -> Vec<patchbay_proto::LatencyRule> {
        let mut data = self.data.lock();
        data.latency_rules.retain(|r| r.pattern != rule.pattern);
        data.latency_rules.push(rule);
        data.latency_rules.sort_by(|a, b| a.pattern.cmp(&b.pattern));
        self.persist(&data);
        data.latency_rules.clone()
    }

    pub fn remove_latency_rule(&self, pattern: &str) -> Option<Vec<patchbay_proto::LatencyRule>> {
        let mut data = self.data.lock();
        let before = data.latency_rules.len();
        data.latency_rules.retain(|r| r.pattern != pattern);
        if data.latency_rules.len() == before {
            return None;
        }
        self.persist(&data);
        Some(data.latency_rules.clone())
    }

    pub fn virtual_sinks(&self) -> Vec<VirtualSink> {
        self.data.lock().virtual_sinks.clone()
    }

    pub fn add_virtual_sink(&self, sink: VirtualSink) {
        let mut data = self.data.lock();
        data.virtual_sinks.retain(|s| s.name != sink.name);
        data.virtual_sinks.push(sink);
        data.virtual_sinks.sort_by(|a, b| a.name.cmp(&b.name));
        self.persist(&data);
    }

    pub fn remove_virtual_sink(&self, name: &str) -> bool {
        let mut data = self.data.lock();
        let before = data.virtual_sinks.len();
        data.virtual_sinks.retain(|s| s.name != name);
        let removed = data.virtual_sinks.len() != before;
        if removed {
            self.persist(&data);
        }
        removed
    }

    /// Does this alias target already have a value? (Used by the
    /// non-destructive auto chanmap import.)
    pub fn has_alias(&self, target: &str) -> bool {
        self.data.lock().aliases.iter().any(|a| a.target == target)
    }

    pub fn views(&self) -> Vec<CanvasView> {
        self.data.lock().views.clone()
    }

    pub fn save_view(&self, view: CanvasView) {
        let mut data = self.data.lock();
        data.views.retain(|v| v.name != view.name);
        data.views.push(view);
        data.views.sort_by(|a, b| a.name.cmp(&b.name));
        self.persist(&data);
    }

    pub fn delete_view(&self, name: &str) -> bool {
        let mut data = self.data.lock();
        let before = data.views.len();
        data.views.retain(|v| v.name != name);
        let removed = data.views.len() != before;
        if removed {
            self.persist(&data);
        }
        removed
    }

    pub fn colors(&self) -> Vec<ColorEntry> {
        self.data.lock().colors.clone()
    }

    /// Empty color clears the entry.
    pub fn set_color(&self, target: String, color: String) {
        let mut data = self.data.lock();
        data.colors.retain(|c| c.target != target);
        if !color.is_empty() {
            data.colors.push(ColorEntry { target, color });
        }
        self.persist(&data);
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
