//! Menu registration for FastTrackStudio REAPER Extension
//!
//! Registers a custom menu in REAPER's Extensions menu and populates it with registered actions.

use reaper_high::Reaper;
use reaper_medium::{Hmenu, HookCustomMenu, MenuHookFlag, ReaperStr};
use std::collections::BTreeMap;
use swell_ui::{
    menu_tree::{anonymous_menu, item, menu, separator, Entry},
    Menu,
};
use tracing::{debug, error, warn};

use crate::action_registry::{get_all_registered_actions, MenuActionDef};

/// Register the extension menu with REAPER
pub fn register_extension_menu() -> anyhow::Result<()> {
    let reaper = std::panic::catch_unwind(Reaper::get).map_err(|_| {
        anyhow::anyhow!("Reaper::get() panicked - Reaper not available globally yet")
    })?;

    reaper.medium_reaper().add_extensions_main_menu();
    reaper
        .medium_session()
        .plugin_register_add_hook_custom_menu::<FastTrackStudioMenuHook>()?;

    Ok(())
}

/// Hook implementation for FastTrackStudio menu
pub struct FastTrackStudioMenuHook;

impl HookCustomMenu for FastTrackStudioMenuHook {
    fn call(menuidstr: &ReaperStr, menu: Hmenu, flag: MenuHookFlag) {
        let result = std::panic::catch_unwind(|| {
            debug!(
                menu_id = %menuidstr.to_str(),
                flag = ?flag,
                "Menu hook called"
            );

            if flag != MenuHookFlag::Init || menuidstr.to_str() != "Main extensions" {
                return;
            }

            debug!("Building FastTrackStudio menu");

            // Build the pure menu structure
            let mut pure_menu = extension_menu();

            // Assign command IDs recursively
            for entry in &mut pure_menu.entries {
                assign_command_ids_recursively(entry);
            }

            // Add the menu entries to the SWELL menu
            let swell_menu = Menu::new(menu.as_ptr());
            swell_ui::menu_tree::add_all_entries_of_menu(swell_menu, &pure_menu);

            debug!("FastTrackStudio menu populated successfully");
        });

        if let Err(e) = result {
            error!(error = ?e, "Panic in menu hook callback");
        }
    }
}

/// Creates the pure menu structure for FastTrackStudio
fn extension_menu() -> swell_ui::menu_tree::Menu<String> {
    let actions = get_all_registered_actions();
    debug!(
        total_actions = actions.len(),
        "Building menu from registered actions"
    );

    if actions.is_empty() {
        warn!("No actions registered - menu will be empty");
        return anonymous_menu(vec![menu(
            "FastTrackStudio".to_string(),
            vec![item("No actions available".to_string(), "".to_string())],
        )]);
    }

    #[derive(Default)]
    struct MenuNode<'a> {
        children: BTreeMap<String, MenuNode<'a>>,
        actions: Vec<&'a MenuActionDef>,
    }

    fn insert_path<'a>(root: &mut MenuNode<'a>, path: &[String], action: &'a MenuActionDef) {
        let mut node = root;
        for segment in path {
            node = node.children.entry(segment.clone()).or_default();
        }
        node.actions.push(action);
    }

    fn node_to_entries(node: &MenuNode<'_>) -> Vec<Entry<String>> {
        let mut entries = Vec::new();

        for (name, child) in &node.children {
            let child_entries = node_to_entries(child);
            entries.push(menu(name.clone(), child_entries));
        }

        let mut action_entries: Vec<Entry<String>> = node
            .actions
            .iter()
            .map(|action| item(action.display_name.clone(), action.action_id.clone()))
            .collect();

        action_entries.sort_by(|a, b| match (a, b) {
            (Entry::Item(ia), Entry::Item(ib)) => ia.text.cmp(&ib.text),
            _ => std::cmp::Ordering::Equal,
        });

        entries.extend(action_entries);
        entries
    }

    // Group actions by full menu path hierarchy
    let mut root = MenuNode::default();
    let mut ungrouped: Vec<&MenuActionDef> = Vec::new();

    for action in &actions {
        if let Some(ref path) = action.menu_path {
            let mut segments: Vec<String> = path
                .split('/')
                .filter(|s| !s.trim().is_empty())
                .map(|s| s.trim().to_string())
                .collect();

            if matches!(segments.first().map(String::as_str), Some("FTS")) {
                segments.remove(0);
            }

            if segments.is_empty() {
                ungrouped.push(action);
            } else {
                insert_path(&mut root, &segments, action);
            }
        } else {
            ungrouped.push(action);
        }
    }

    // Build menu entries
    let mut menu_entries = node_to_entries(&root);

    // Add ungrouped actions directly
    if !ungrouped.is_empty() {
        if !menu_entries.is_empty() {
            menu_entries.push(separator());
        }
        for action in ungrouped {
            menu_entries.push(item(action.display_name.clone(), action.action_id.clone()));
        }
    }

    // Wrap in "FastTrackStudio" menu
    anonymous_menu(vec![menu("FastTrackStudio".to_string(), menu_entries)])
}

/// Assign command IDs to menu items by looking them up in REAPER
fn assign_command_ids_recursively(entry: &mut Entry<String>) {
    match entry {
        Entry::Menu(m) => {
            m.id = 0;
            for e in &mut m.entries {
                assign_command_ids_recursively(e);
            }
        }
        Entry::Item(i) => {
            if i.result.is_empty() {
                i.id = 0;
                return;
            }

            let command_id = std::panic::catch_unwind(|| {
                let reaper = Reaper::get();
                let lookup_name = format!("_{}", i.result);
                reaper.medium_reaper().named_command_lookup(lookup_name)
            });

            match command_id {
                Ok(Some(cmd_id)) => {
                    i.id = cmd_id.get();
                    debug!(
                        command_id = %i.result,
                        reaper_cmd_id = cmd_id.get(),
                        "Found REAPER command ID for menu item"
                    );
                }
                Ok(None) => {
                    warn!(
                        command_id = %i.result,
                        "Could not find REAPER command ID for menu item"
                    );
                    i.id = 0;
                }
                Err(_) => {
                    warn!(
                        command_id = %i.result,
                        "Panic when looking up REAPER command ID"
                    );
                    i.id = 0;
                }
            }
        }
        _ => {}
    }
}
