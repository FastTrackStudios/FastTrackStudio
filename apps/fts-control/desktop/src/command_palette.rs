use std::collections::HashMap;

use actions_proto::ActionDefinition;
use input::{
    config::{load_default_config, load_user_config},
    KeymapConfig,
};
use session::session_actions;

use actions_proto::ids::standalone as standalone_ids;

#[derive(Clone)]
pub(crate) struct PaletteEntry {
    pub(crate) id: String,
    pub(crate) name: String,
    pub(crate) description: String,
    pub(crate) shortcut: String,
    pub(crate) when_clause: String,
    pub(crate) mappings: Vec<String>,
}

pub(crate) fn build_input_config() -> KeymapConfig {
    let mut merged = load_default_config().unwrap_or_default();

    let mut app_overlay = KeymapConfig::default();
    app_overlay.keymap.insert(
        "normal".to_string(),
        HashMap::from([
            (
                "Cmd+Comma".to_string(),
                standalone_ids::OPEN_SETTINGS.as_str().to_string(),
            ),
            (
                "Cmd+Shift+D".to_string(),
                standalone_ids::TOGGLE_DARK_MODE.as_str().to_string(),
            ),
            (
                "Cmd+Shift+P".to_string(),
                standalone_ids::COMMAND_PALETTE.as_str().to_string(),
            ),
        ]),
    );
    merged = KeymapConfig::merge(merged, app_overlay);

    if let Ok(Some(user)) = load_user_config() {
        merged = KeymapConfig::merge(merged, user);
    }

    merged
}

fn collect_mappings_by_action(config: &KeymapConfig) -> HashMap<String, Vec<String>> {
    let mut by_action: HashMap<String, Vec<String>> = HashMap::new();

    for (mode, bindings) in &config.keymap {
        for (keys, action) in bindings {
            by_action
                .entry(action.clone())
                .or_default()
                .push(format!("{mode}: {keys}"));
        }
    }

    for (mode, layers) in &config.keymap_context {
        for layer in layers {
            for (keys, action) in &layer.bindings {
                by_action
                    .entry(action.clone())
                    .or_default()
                    .push(format!("{mode} [{}]: {keys}", layer.when));
            }
        }
    }

    by_action
}

pub(crate) fn build_palette_entries(config: &KeymapConfig) -> Vec<PaletteEntry> {
    let mut actions: Vec<ActionDefinition> = session_actions::definitions();
    actions.extend(actions_standalone::common_action_definitions());

    let mappings = collect_mappings_by_action(config);
    let mut entries = Vec::with_capacity(actions.len());
    for action in actions {
        entries.push(PaletteEntry {
            id: action.id.as_str().to_string(),
            name: action.name,
            description: action.description,
            shortcut: action.shortcut_hint.unwrap_or_default(),
            when_clause: action.when.unwrap_or_default(),
            mappings: mappings
                .get(action.id.as_str())
                .cloned()
                .unwrap_or_default(),
        });
    }

    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries
}
