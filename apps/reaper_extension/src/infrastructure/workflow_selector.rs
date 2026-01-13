//! Profile and Workflow Selector Toolbar Buttons
//!
//! Provides two toolbar buttons for selecting:
//! - **Profile Selector**: Keybind profiles (FastTrackStudio, Logic, Reaper, ReaVim)
//! - **Workflow Selector**: Active workflows (Tempo Mapping, Slip Editing, etc.)
//!
//! Both use popup menus when clicked and display as double-wide text icons.

use reaper_high::Reaper;
use reaper_low::{raw, Swell};
use std::ffi::CString;
use tracing::{debug, info};

use crate::input::handler::InputHandler;
use crate::input::keybinds;
use crate::input::mouse_modifiers::manager as mouse_manager;
use crate::input::workflows;

// region: --- Menu IDs

/// Profile menu IDs
mod profile_menu {
    pub const FTS: u32 = 1;
    pub const LOGIC: u32 = 2;
    pub const REAPER: u32 = 3;
    pub const REAVIM: u32 = 4;
    pub const TOGGLE: u32 = 101;
}

/// Workflow menu IDs (dynamically assigned starting from 200)
const WORKFLOW_MENU_BASE: u32 = 200;
const WORKFLOW_NONE: u32 = 199;

// endregion: --- Menu IDs

// region: --- Profile Selector

/// Get the current keybind profile name for display
pub fn current_profile_name() -> &'static str {
    let preset = keybinds::active_preset_name();
    match preset.to_lowercase().as_str() {
        "fastrackstudio" => "FastTrackStudio",
        "logic" => "Logic",
        "reaper" => "Reaper",
        "reavim" => "ReaVim",
        _ => "Reaper",
    }
}

/// Get the display text for the profile selector button
pub fn profile_button_text() -> String {
    current_profile_name().to_string()
}

/// Show the profile selector popup menu
pub fn show_profile_menu() -> Option<u32> {
    let swell = Swell::get();
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    let main_hwnd = medium.get_main_hwnd();
    let mut cursor_pos = raw::POINT { x: 0, y: 0 };
    unsafe {
        swell.GetCursorPos(&mut cursor_pos);
    }

    let menu = swell.CreatePopupMenu();
    if menu.is_null() {
        return None;
    }

    let current_preset = keybinds::active_preset_name();
    let is_enabled = InputHandler::is_enabled();

    unsafe {
        add_menu_item(menu, profile_menu::FTS, "FastTrackStudio", current_preset == "fastrackstudio");
        add_menu_item(menu, profile_menu::LOGIC, "Logic", current_preset == "logic");
        add_menu_item(menu, profile_menu::REAPER, "Reaper", current_preset == "reaper");
        add_menu_item(menu, profile_menu::REAVIM, "ReaVim", current_preset == "reavim");

        // Separator
        let mut sep_info = raw::MENUITEMINFO {
            fMask: raw::MIIM_TYPE,
            fType: raw::MF_SEPARATOR,
            ..Default::default()
        };
        swell.InsertMenuItem(menu, -1, 1, &mut sep_info);

        // "Enable" with checkmark when enabled
        add_menu_item(menu, profile_menu::TOGGLE, "Enable", is_enabled);
    }

    let result = unsafe {
        swell.TrackPopupMenu(
            menu,
            raw::TPM_RETURNCMD as i32,
            cursor_pos.x,
            cursor_pos.y,
            0,
            main_hwnd.as_ptr(),
            std::ptr::null(),
        )
    };

    unsafe {
        swell.DestroyMenu(menu);
    }

    if result == 0 {
        return None;
    }

    Some(result as u32)
}

/// Handle profile menu selection
pub fn handle_profile_selection(menu_id: u32) {
    let reaper = Reaper::get();

    match menu_id {
        profile_menu::FTS => {
            keybinds::set_preset("fastrackstudio");
            mouse_manager::set_profile("fastrackstudio");
            reaper.show_console_msg("Profile: FastTrackStudio\n");
            info!("Profile changed to: FastTrackStudio");
        }
        profile_menu::LOGIC => {
            keybinds::set_preset("logic");
            mouse_manager::set_profile("logic");
            reaper.show_console_msg("Profile: Logic\n");
            info!("Profile changed to: Logic");
        }
        profile_menu::REAPER => {
            keybinds::set_preset("reaper");
            mouse_manager::set_profile("reaper");
            reaper.show_console_msg("Profile: Reaper\n");
            info!("Profile changed to: Reaper");
        }
        profile_menu::REAVIM => {
            keybinds::set_preset("reavim");
            mouse_manager::set_profile("reavim");
            reaper.show_console_msg("Profile: ReaVim\n");
            info!("Profile changed to: ReaVim");
        }
        profile_menu::TOGGLE => {
            let new_state = InputHandler::toggle();
            let status = if new_state { "enabled" } else { "disabled" };
            reaper.show_console_msg(format!("FTS-Input: {}\n", status));
            info!("FTS-Input toggled: {}", status);
        }
        _ => {
            debug!("Unknown profile menu selection: {}", menu_id);
            return;
        }
    }

    let _ = reaper.wake_up();
    update_profile_button();
}

/// Action handler for profile selector
pub fn profile_selector_handler() {
    info!("Profile selector action triggered");
    if let Some(menu_id) = show_profile_menu() {
        handle_profile_selection(menu_id);
    }
}

/// Get toggle state for profile selector (returns true if FTS-Input is enabled)
pub fn get_profile_selector_state() -> bool {
    InputHandler::is_enabled()
}

// endregion: --- Profile Selector

// region: --- Workflow Selector

/// Get the current workflow name for display (uses short name if applicable)
pub fn current_workflow_name() -> String {
    workflows::get_active_workflow_display_name().unwrap_or_else(|| "None".to_string())
}

/// Get the display text for the workflow selector button
pub fn workflow_button_text() -> String {
    current_workflow_name()
}

/// Show the workflow selector popup menu
pub fn show_workflow_menu() -> Option<u32> {
    let swell = Swell::get();
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    let main_hwnd = medium.get_main_hwnd();
    let mut cursor_pos = raw::POINT { x: 0, y: 0 };
    unsafe {
        swell.GetCursorPos(&mut cursor_pos);
    }

    let menu = swell.CreatePopupMenu();
    if menu.is_null() {
        return None;
    }

    let active_workflow = workflows::get_active_workflow_id();
    let available = workflows::list_workflows();

    unsafe {
        // "None" option to deactivate workflow
        add_menu_item(menu, WORKFLOW_NONE, "None", active_workflow.is_none());

        if !available.is_empty() {
            // Separator
            let mut sep_info = raw::MENUITEMINFO {
                fMask: raw::MIIM_TYPE,
                fType: raw::MF_SEPARATOR,
                ..Default::default()
            };
            swell.InsertMenuItem(menu, -1, 1, &mut sep_info);

            // Add each workflow
            for (idx, (id, name, _desc)) in available.iter().enumerate() {
                let menu_id = WORKFLOW_MENU_BASE + idx as u32;
                let is_active = active_workflow.as_ref().map(|a| a == id).unwrap_or(false);
                add_menu_item(menu, menu_id, name, is_active);
            }
        }
    }

    let result = unsafe {
        swell.TrackPopupMenu(
            menu,
            raw::TPM_RETURNCMD as i32,
            cursor_pos.x,
            cursor_pos.y,
            0,
            main_hwnd.as_ptr(),
            std::ptr::null(),
        )
    };

    unsafe {
        swell.DestroyMenu(menu);
    }

    if result == 0 {
        return None;
    }

    Some(result as u32)
}

/// Handle workflow menu selection
pub fn handle_workflow_selection(menu_id: u32) {
    let reaper = Reaper::get();

    if menu_id == WORKFLOW_NONE {
        // Deactivate current workflow
        if workflows::get_active_workflow_id().is_some() {
            workflows::deactivate();
            reaper.show_console_msg("Workflow: None\n");
            info!("Workflow deactivated");
        }
    } else if menu_id >= WORKFLOW_MENU_BASE {
        // Activate a specific workflow
        let available = workflows::list_workflows();
        let idx = (menu_id - WORKFLOW_MENU_BASE) as usize;

        if let Some((id, name, _)) = available.get(idx) {
            // Deactivate current first
            if workflows::get_active_workflow_id().is_some() {
                workflows::deactivate();
            }

            // Activate new workflow
            if let Err(e) = workflows::activate(id) {
                tracing::warn!(error = %e, "Failed to activate workflow");
                reaper.show_console_msg(format!("Failed to activate workflow: {}\n", e));
                return;
            }
            reaper.show_console_msg(format!("Workflow: {}\n", name));
            info!(workflow = %name, "Workflow activated");
        }
    } else {
        debug!("Unknown workflow menu selection: {}", menu_id);
        return;
    }

    let _ = reaper.wake_up();
    update_workflow_button();
}

/// Action handler for workflow selector
pub fn workflow_selector_handler() {
    info!("Workflow selector action triggered");
    if let Some(menu_id) = show_workflow_menu() {
        handle_workflow_selection(menu_id);
    }
}

/// Get toggle state for workflow selector (returns true if a workflow is active)
pub fn get_workflow_selector_state() -> bool {
    workflows::get_active_workflow_id().is_some()
}

// endregion: --- Workflow Selector

// region: --- Toolbar Button Management

/// Update the profile selector toolbar button text (preserves position)
pub fn update_profile_button() {
    use crate::infrastructure::toolbar::{self, ToolbarButton, ToolbarTarget};

    if !toolbar::is_available() {
        return;
    }

    let new_text = profile_button_text();
    debug!(text = %new_text, "Updating profile selector toolbar button");

    // Update in-place (preserves position on toolbar)
    let button = ToolbarButton::new("FTS_PROFILE_SELECTOR", &new_text)
        .on_toolbar(ToolbarTarget::Floating(32))
        .double_wide();

    if let Err(e) = toolbar::update_button(&button, "__profile_selector__") {
        tracing::warn!(error = %e, "Failed to update profile selector button");
    }
}

/// Update the workflow selector toolbar button text (preserves position)
pub fn update_workflow_button() {
    use crate::infrastructure::toolbar::{self, ToolbarButton, ToolbarTarget};

    if !toolbar::is_available() {
        return;
    }

    let new_text = workflow_button_text();
    debug!(text = %new_text, "Updating workflow selector toolbar button");

    // Update in-place (preserves position on toolbar)
    let button = ToolbarButton::new("FTS_WORKFLOW_SELECTOR", &new_text)
        .on_toolbar(ToolbarTarget::Floating(32))
        .double_wide();

    if let Err(e) = toolbar::update_button(&button, "__workflow_selector__") {
        tracing::warn!(error = %e, "Failed to update workflow selector button");
    }
}

/// Add both toolbar buttons to Floating toolbar 32
/// Call this during initialization after actions are registered
/// Order: Profile selector, then Workflow selector (left to right)
pub fn add_toolbar_buttons() {
    use crate::infrastructure::toolbar::{self, ToolbarButton, ToolbarTarget};

    if !toolbar::is_available() {
        info!("Dynamic toolbar API not available - skipping selector buttons");
        return;
    }

    // Workflow selector button (added first, will be on right after profile is added)
    let workflow_text = workflow_button_text();
    info!(text = %workflow_text, "Adding workflow selector toolbar button");

    let workflow_button = ToolbarButton::new("FTS_WORKFLOW_SELECTOR", &workflow_text)
        .on_toolbar(ToolbarTarget::Floating(32))
        .double_wide();

    match toolbar::add_button(&workflow_button, "__workflow_selector__") {
        Ok(_) => info!("Workflow selector button added"),
        Err(e) => tracing::warn!(error = %e, "Failed to add workflow selector button"),
    }

    // Profile selector button (added second, but will appear on left)
    let profile_text = profile_button_text();
    info!(text = %profile_text, "Adding profile selector toolbar button");

    let profile_button = ToolbarButton::new("FTS_PROFILE_SELECTOR", &profile_text)
        .on_toolbar(ToolbarTarget::Floating(32))
        .double_wide();

    match toolbar::add_button(&profile_button, "__profile_selector__") {
        Ok(_) => info!("Profile selector button added"),
        Err(e) => tracing::warn!(error = %e, "Failed to add profile selector button"),
    }
}

// Backwards compatibility alias
pub fn add_toolbar_button() {
    add_toolbar_buttons();
}

// endregion: --- Toolbar Button Management

// region: --- Helpers

/// Helper to add a menu item with optional checkmark
unsafe fn add_menu_item(menu: raw::HMENU, id: u32, text: &str, checked: bool) {
    let swell = Swell::get();
    let text_cstring = CString::new(text).unwrap_or_default();

    let mut mi = raw::MENUITEMINFO {
        fMask: raw::MIIM_TYPE | raw::MIIM_DATA | raw::MIIM_ID | raw::MIIM_STATE,
        fType: raw::MF_STRING,
        wID: id,
        fState: if checked { raw::MF_CHECKED } else { 0 },
        dwTypeData: text_cstring.as_ptr() as *mut i8,
        ..Default::default()
    };
    // SAFETY: Valid parameters passed to Swell InsertMenuItem
    unsafe {
        swell.InsertMenuItem(menu, -1, 1, &mut mi);
    }
}

// endregion: --- Helpers
