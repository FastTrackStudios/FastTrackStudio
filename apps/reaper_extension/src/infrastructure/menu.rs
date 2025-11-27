//! Menu registration for FastTrackStudio REAPER Extension
//!
//! Registers a custom menu in REAPER's Extensions menu and populates it with registered actions.

use reaper_high::Reaper;
use reaper_medium::{HookCustomMenu, Hmenu, MenuHookFlag, ReaperStr};
use swell_ui::{Menu, menu_tree::{anonymous_menu, menu, item, separator, Entry}};
use tracing::{debug, error, warn};
use std::collections::HashMap;

use crate::infrastructure::action_registry::get_all_registered_actions;

/// Register the extension menu with REAPER
/// Must be called after actions are registered and REAPER is woken up
pub fn register_extension_menu() -> anyhow::Result<()> {
    // Safely get Reaper instance - if it panics, return an error
    let reaper = std::panic::catch_unwind(|| Reaper::get())
        .map_err(|_| anyhow::anyhow!("Reaper::get() panicked - Reaper not available globally yet"))?;
    
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
        // Wrap the entire callback in panic handling to prevent crashes
        let result = std::panic::catch_unwind(|| {
            debug!(
                menu_id = %menuidstr.to_str(),
                flag = ?flag,
                "Menu hook called"
            );
            
            if flag != MenuHookFlag::Init || menuidstr.to_str() != "Main extensions" {
                debug!(
                    menu_id = %menuidstr.to_str(),
                    flag = ?flag,
                    "Skipping menu hook (wrong menu or flag)"
                );
                return;
            }
            
            debug!("Building FastTrackStudio menu");
            
            // Build the pure menu structure using swell-ui's menu_tree
            let mut pure_menu = extension_menu();
            
            // Count menu items for logging (before assigning IDs)
            let item_count = count_menu_items(&pure_menu);
            
            debug!(
                item_count = item_count,
                "Built menu structure, assigning command IDs"
            );
            
            // Assign command IDs recursively (this is where Reaper::get() is called,
            // but it's safe here because we're in the hook callback)
            for entry in &mut pure_menu.entries {
                assign_command_ids_recursively(entry);
            }
            
            debug!("Command IDs assigned, adding entries to SWELL menu");
            
            // Add the menu entries to the SWELL menu
            let swell_menu = Menu::new(menu.as_ptr());
            swell_ui::menu_tree::add_all_entries_of_menu(swell_menu, &pure_menu);
            
            debug!(
                menu_items = item_count,
                "✅ FastTrackStudio menu populated successfully"
            );
        });
        
        if let Err(e) = result {
            error!(
                error = ?e,
                "❌ Panic in menu hook callback - menu population failed"
            );
        }
    }
}

/// Extract category and subcategory from command ID
/// Returns (category, subcategory) tuple
/// Examples:
///   "FTS_LIVE_SETLIST_PLAY" -> ("Live", "Tracks")
///   "FTS_VM_CREATE_GROUP" -> ("Visibility Manager", None)
///   "FTS_LYRICS_READ_FROM_REAPER" -> ("Lyrics", None)
///   "FTS_DEV_LOG_PROJECTS" -> ("Dev", None)
///   "FTS_DEBUG_TEST" -> ("General", None)
fn extract_category_from_command_id(command_id: &str) -> (String, Option<String>) {
    if command_id.starts_with("FTS_LIVE_") {
        // All FTS_LIVE_ actions are currently tracks-related
        // In the future, we could parse FTS_LIVE_TRACKS_* vs FTS_LIVE_OTHER_* etc.
        ("Live".to_string(), Some("Tracks".to_string()))
    } else if command_id.starts_with("FTS_VM_") {
        ("Visibility Manager".to_string(), None)
    } else if command_id.starts_with("FTS_LYRICS_") {
        ("Lyrics".to_string(), None)
    } else if command_id.starts_with("FTS_DEV_") {
        ("Dev".to_string(), None)
    } else {
        ("General".to_string(), None)
    }
}

/// Creates the pure menu structure for FastTrackStudio
/// Groups actions into submenus based on their command ID prefixes
fn extension_menu() -> swell_ui::menu_tree::Menu<String> {
    // Get all registered actions that should appear in menu
    let actions = get_all_registered_actions();
    debug!(total_actions = actions.len(), "Building menu from registered actions");
    
    let menu_actions: Vec<_> = actions
        .iter()
        .filter(|action| {
            let should_include = action.appears_in_menu;
            if !should_include {
                debug!(
                    command_id = %action.command_id,
                    appears_in_menu = action.appears_in_menu,
                    "Action filtered out from menu"
                );
            }
            should_include
        })
        .collect();
    
    debug!(menu_action_count = menu_actions.len(), "Actions to include in menu");
    
    if menu_actions.is_empty() {
        warn!("No actions marked for menu display");
        return anonymous_menu(vec![]);
    }
    
    // Group actions by category and subcategory
    // Structure: category -> subcategory -> actions
    let mut categorized: HashMap<String, HashMap<Option<String>, Vec<&crate::infrastructure::action_registry::ActionDef>>> = HashMap::new();
    
    for action in &menu_actions {
        let (category, subcategory) = extract_category_from_command_id(action.command_id);
        categorized
            .entry(category)
            .or_default()
            .entry(subcategory)
            .or_default()
            .push(action);
    }
    
    // Build menu entries: nested submenus for categories with subcategories, then General actions directly
    let mut menu_entries = Vec::new();
    
    // Define category order (non-General categories first, then General, then Dev)
    let category_order = vec!["Live", "Visibility Manager", "Lyrics", "Dev"];
    
    // Add categorized submenus
    for category_name in &category_order {
        if let Some(subcategories) = categorized.remove(*category_name) {
            if !subcategories.is_empty() {
                // If there's only one subcategory and it's not None, create nested structure
                // Otherwise, flatten
                let mut category_menu_entries = Vec::new();
                
                for (subcategory_opt, actions_in_subcategory) in subcategories {
                    if !actions_in_subcategory.is_empty() {
                        let mut subcategory_items = Vec::new();
                        
                        for action in actions_in_subcategory {
                            let entry = item(
                                action.display_name.clone(),
                                action.command_id.to_string(),
                            );
                            
                            debug!(
                                command_id = %action.command_id,
                                category = %category_name,
                                subcategory = ?subcategory_opt,
                                "Adding action to subcategory"
                            );
                            
                            subcategory_items.push(entry);
                        }
                        
                        // Sort items alphabetically by display name
                        subcategory_items.sort_by(|a, b| {
                            match (a, b) {
                                (Entry::Item(ia), Entry::Item(ib)) => {
                                    ia.text.cmp(&ib.text)
                                }
                                _ => std::cmp::Ordering::Equal
                            }
                        });
                        
                        // If there's a subcategory name, create a nested menu
                        if let Some(subcategory_name) = subcategory_opt {
                            category_menu_entries.push(menu(subcategory_name, subcategory_items));
                        } else {
                            // No subcategory, add items directly to category menu
                            category_menu_entries.extend(subcategory_items);
                        }
                    }
                }
                
                if !category_menu_entries.is_empty() {
                    menu_entries.push(menu(category_name.to_string(), category_menu_entries));
                }
            }
        }
    }
    
    // Add separator if we have categorized menus and General actions
    if !menu_entries.is_empty() && categorized.contains_key("General") {
        menu_entries.push(separator());
    }
    
    // Add General actions directly (not in a submenu)
    if let Some(general_subcategories) = categorized.remove("General") {
        for (_, general_actions) in general_subcategories {
            for action in general_actions {
        let entry = item(
            action.display_name.clone(),
            action.command_id.to_string(),
        );
        
        debug!(
            command_id = %action.command_id,
                    "Adding General action to menu"
        );
        
        menu_entries.push(entry);
            }
        }
    }
    
    // Add any remaining uncategorized actions
    for (category_name, subcategories) in categorized {
        let mut category_menu_entries = Vec::new();
        
        for (subcategory_opt, actions_in_subcategory) in subcategories {
            if !actions_in_subcategory.is_empty() {
                let mut subcategory_items = Vec::new();
                
                for action in actions_in_subcategory {
                    let entry = item(
                        action.display_name.clone(),
                        action.command_id.to_string(),
                    );
                    
                    debug!(
                        command_id = %action.command_id,
                        category = %category_name,
                        subcategory = ?subcategory_opt,
                        "Adding uncategorized action"
                    );
                    
                    subcategory_items.push(entry);
                }
                
                if let Some(subcategory_name) = subcategory_opt {
                    category_menu_entries.push(menu(subcategory_name, subcategory_items));
                } else {
                    category_menu_entries.extend(subcategory_items);
                }
            }
        }
        
        if !category_menu_entries.is_empty() {
            menu_entries.push(menu(category_name, category_menu_entries));
        }
    }
    
    if menu_entries.is_empty() {
        warn!("No menu entries after filtering - returning empty menu");
        return anonymous_menu(vec![]);
    }
    
    // Wrap in "FastTrackStudio" menu
    let root_menu = anonymous_menu(vec![menu("FastTrackStudio".to_string(), menu_entries)]);
    
    root_menu
}

/// Assign command IDs to menu items by looking them up in REAPER
/// This is called from the menu hook when Reaper is guaranteed to be available
fn assign_command_ids_recursively(entry: &mut Entry<String>) {
    match entry {
        Entry::Menu(m) => {
            m.id = 0;
            for e in &mut m.entries {
                assign_command_ids_recursively(e);
            }
        }
        Entry::Item(i) => {
            // Use std::panic::catch_unwind to safely handle Reaper::get() failures
            let command_id = std::panic::catch_unwind(|| {
                let reaper = Reaper::get();
                let lookup_name = format!("_{}", i.result);
                reaper
                    .medium_reaper()
                    .named_command_lookup(lookup_name)
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
                        "Panic when looking up REAPER command ID (Reaper not available?)"
                    );
                    i.id = 0;
                }
            }
        }
        _ => {}
    }
}

/// Count the number of menu items recursively
fn count_menu_items(menu: &swell_ui::menu_tree::Menu<String>) -> usize {
    menu.entries.iter().fold(0, |acc, entry| {
        acc + match entry {
            Entry::Menu(m) => count_menu_items(m),
            Entry::Item(_) => 1,
            _ => 0,
        }
    })
}

