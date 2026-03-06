use once_cell::sync::Lazy;
use reaper_medium::CommandId;
use std::collections::HashMap;
use std::sync::RwLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ActionSection {
    Main,
}

pub struct ActionDef {
    pub command_id: &'static str,
    pub display_name: String,
    pub handler: fn(),
    pub appears_in_menu: bool,
    pub section: ActionSection,
    pub toggle_state: Option<fn() -> bool>,
}

static COMMAND_IDS: Lazy<RwLock<HashMap<String, CommandId>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

pub fn register_actions(_actions: &[ActionDef], _namespace: &str) {}

pub fn remember_command_id(action_id: impl Into<String>, command_id: CommandId) {
    if let Ok(mut map) = COMMAND_IDS.write() {
        map.insert(action_id.into(), command_id);
    }
}

pub fn get_command_id(action_id: &str) -> Option<CommandId> {
    COMMAND_IDS
        .read()
        .ok()
        .and_then(|map| map.get(action_id).copied())
}
