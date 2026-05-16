//! REAPER Action Registry Implementation
//!
//! Registers actions with REAPER's action system using `reaper_high::Reaper::register_action`.
//! Tracks registered actions so they can be unregistered when a guest disconnects.
//!
//! When a registered action is triggered, all subscribers receive an
//! `ActionEvent::Triggered` event. Guests handle action logic — the host
//! is domain-agnostic.
//!
//! Actions registered with `show_in_menu: true` are automatically added to the
//! Extensions > FastTrackStudio menu. The menu hierarchy is derived from the
//! command name prefix (e.g., `FTS_SESSION_*` → Session submenu).

use crate::main_thread;
use daw_control::lock::LockExt;
use daw_proto::{
    ActionExecutionResult, ActionInfo, ActionListFilter, ActionListRequest, ActionListResponse,
    ActionOrigin, ActionRegistration, ActionSection, DawResult,
};
use reaper_high::{Reaper, RegisteredAction};
use reaper_medium::{
    CommandId, Handle, Hmenu, HookCustomMenu, MenuHookFlag, OwnedGaccelRegister, ReaperStr,
    SectionContext, SectionId,
};
use std::collections::{BTreeMap, HashMap};
use std::ffi::{CStr, CString};
use std::ptr::null_mut;
use std::sync::Mutex;
use tokio::sync::broadcast;
use tracing::{debug, info, warn};
use vox::Tx;

/// Owned action keepalive — drops `RegisteredAction` and removes the
/// manually-registered gaccel when unregistered. `Handle<gaccel_register_t>`
/// wraps a NonNull<reaper_low::raw::gaccel_register_t> which is owned by
/// REAPER itself (the medium session keeps the backing storage alive). The
/// pointer is only deref'd on the REAPER main thread through
/// `plugin_register_remove_gaccel`, so we can safely impl Send/Sync for
/// the carrier type.
struct OwnedAction {
    action: RegisteredAction,
    /// `None` when REAPER itself already registered the gaccel (e.g. the
    /// command name appeared in `reaper-menu.ini`'s Main toolbar before our
    /// extension's spawned task ran). In that case the action is in REAPER's
    /// list already and we never minted a handle to remove.
    gaccel_handle: Option<Handle<reaper_low::raw::gaccel_register_t>>,
}

// SAFETY: `OwnedAction` is only mutated / dropped via main_thread::query,
// which serialises on REAPER's main thread. The `Handle` and
// `RegisteredAction` interior pointers are never deref'd from other
// threads — we only need Send so the OnceLock<Mutex<..>> map can hold them.
unsafe impl Send for OwnedAction {}

/// Tracks actions registered through this service.
///
/// Maps command_name → command_id for actions we've registered.
/// Used for unregistration and to avoid double-registering.
static REGISTERED_ACTIONS: std::sync::OnceLock<Mutex<HashMap<String, u32>>> =
    std::sync::OnceLock::new();

/// Live `RegisteredAction` + manual gaccel handle keepalives. Dropping
/// the action removes it from `reaper_high`'s internal command map (so
/// REAPER's hook lookup won't dispatch through it any more), and the
/// gaccel handle lets us call `plugin_register_remove_gaccel` to take
/// the action OUT of REAPER's action list.
static OWNED_ACTIONS: std::sync::OnceLock<Mutex<HashMap<String, OwnedAction>>> =
    std::sync::OnceLock::new();

fn owned_actions() -> &'static Mutex<HashMap<String, OwnedAction>> {
    OWNED_ACTIONS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Broadcast channel for action trigger events.
///
/// Each subscriber gets their own `broadcast::Receiver` which forwards
/// events to their vox `Tx<ActionEvent>`.
static ACTION_BROADCASTER: std::sync::OnceLock<broadcast::Sender<String>> =
    std::sync::OnceLock::new();

/// Menu metadata for actions that should appear in the Extensions menu.
static MENU_ACTIONS: std::sync::OnceLock<Mutex<Vec<MenuActionDef>>> = std::sync::OnceLock::new();

/// Toggle state for toggleable actions.
///
/// Maps command_name → current on/off state. REAPER queries toggle state
/// synchronously on the main thread, so we store it here for instant access.
/// Guests update this via `set_toggle_state`.
static TOGGLE_STATES: std::sync::OnceLock<Mutex<HashMap<String, bool>>> =
    std::sync::OnceLock::new();

const SHARED_TOGGLE_EXTSTATE_SECTION: &str = "FastTrackStudio.ActionToggleState";

/// Action metadata stored for menu building.
#[derive(Clone)]
struct MenuActionDef {
    /// REAPER command name (e.g., "FTS_SESSION_TOGGLE_PLAYBACK")
    command_name: String,
    /// Display name shown in menu (the description from registration)
    display_name: String,
    /// Menu group derived from command name (e.g., "Session")
    group: String,
}

pub(crate) fn registered_actions() -> &'static Mutex<HashMap<String, u32>> {
    REGISTERED_ACTIONS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Subscribe directly to action trigger broadcasts.
///
/// Returns a receiver that yields command names whenever an action is triggered.
/// Useful for in-process extensions (LocalCaller) that want to avoid the vox
/// streaming round-trip.
pub fn subscribe_action_broadcasts() -> broadcast::Receiver<String> {
    action_broadcaster().subscribe()
}

fn action_broadcaster() -> &'static broadcast::Sender<String> {
    ACTION_BROADCASTER.get_or_init(|| {
        let (tx, _rx) = broadcast::channel::<String>(64);
        tx
    })
}

fn menu_actions() -> &'static Mutex<Vec<MenuActionDef>> {
    MENU_ACTIONS.get_or_init(|| Mutex::new(Vec::new()))
}

pub(crate) fn toggle_states() -> &'static Mutex<HashMap<String, bool>> {
    TOGGLE_STATES.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Read toggle state for a command. Called from REAPER's main thread
/// via the `ActionKind::Toggleable` closure.
fn read_toggle_state(command_name: &str) -> bool {
    let state = toggle_states()
        .lock()
        .unwrap()
        .get(command_name)
        .copied()
        .unwrap_or(false);
    // Trace every REAPER toggleaction hook query so we can confirm
    // REAPER is actually consulting our state when repainting the
    // action list / toolbars. If this trace never fires, REAPER never
    // queries our hook and the indicator can never update.
    tracing::trace!(target: "toggle_hook", "{} -> {}", command_name, state);
    state
}

/// Derive a menu group name from a REAPER command name.
///
/// `FTS_SESSION_TOGGLE_PLAYBACK` → "Session"
/// `FTS_TRANSPORT_PLAY_STOP` → "Transport"
/// `FTS_MARKERS_REGIONS_INSERT_MARKER` → "Markers Regions"
///
/// Convention: strip "FTS_" prefix, then take segments until we hit
/// a lowercase-starting word (action names are all-caps in the prefix part,
/// but after titlecasing they become mixed). Since the raw command name is
/// ALL_CAPS, we use a heuristic: known domain prefixes get titlecased.
fn derive_menu_group(command_name: &str) -> String {
    let name = command_name.strip_prefix("FTS_").unwrap_or(command_name);

    // Known domain prefixes (order matters — longest match first)
    let known_domains = [
        "MARKERS_REGIONS",
        "DYNAMIC_TEMPLATE",
        "VISIBILITY_MANAGER",
        "AUTO_COLOR",
        "REAPER_EXTENSION",
        "TRANSPORT",
        "SESSION",
        "SIGNAL",
        "SYNC",
        "DAW",
    ];

    for domain in &known_domains {
        if name.starts_with(domain) {
            return titlecase_underscored(domain);
        }
    }

    // Fallback: use first segment
    name.split('_')
        .next()
        .map(titlecase_underscored)
        .unwrap_or_default()
}

/// Titlecase an underscored string: "MARKERS_REGIONS" → "Markers Regions"
fn titlecase_underscored(s: &str) -> String {
    s.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(c) => {
                    let upper: String = c.to_uppercase().collect();
                    let lower: String = chars.as_str().to_lowercase();
                    format!("{upper}{lower}")
                }
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Notify all subscribers that an action was triggered.
///
/// Called from the REAPER action handler closure on the main thread.
/// Uses a broadcast channel so this is non-blocking.
fn notify_action_triggered(command_name: String) {
    let tx = action_broadcaster();
    if tx.receiver_count() == 0 {
        return;
    }
    let _ = tx.send(command_name);
}

fn flip_toggle_state(command_name: &str) -> bool {
    let mut states = toggle_states().lock_recoverable("action_registry");
    let state = states.entry(command_name.to_string()).or_insert(false);
    *state = !*state;
    *state
}

fn named_command_lookup(command_name: &str) -> Option<CommandId> {
    let medium = Reaper::get().medium_reaper();
    medium.named_command_lookup(command_name).or_else(|| {
        if command_name.starts_with('_') {
            None
        } else {
            medium.named_command_lookup(format!("_{command_name}"))
        }
    })
}

fn normalize_command_name(command_name: &str) -> &str {
    command_name.strip_prefix('_').unwrap_or(command_name)
}

fn read_shared_toggle_state(command_name: &str) -> Option<bool> {
    let section = CString::new(SHARED_TOGGLE_EXTSTATE_SECTION).ok()?;
    let key = CString::new(normalize_command_name(command_name)).ok()?;
    let value = unsafe {
        let ptr = Reaper::get()
            .medium_reaper()
            .low()
            .GetExtState(section.as_ptr(), key.as_ptr());
        if ptr.is_null() {
            return None;
        }
        CStr::from_ptr(ptr).to_string_lossy().into_owned()
    };

    match value.as_str() {
        "1" | "true" | "on" => Some(true),
        "0" | "false" | "off" => Some(false),
        _ => None,
    }
}

fn delete_shared_toggle_state(command_name: &str) {
    let Ok(section) = CString::new(SHARED_TOGGLE_EXTSTATE_SECTION) else {
        return;
    };
    let Ok(key) = CString::new(normalize_command_name(command_name)) else {
        return;
    };
    unsafe {
        Reaper::get()
            .medium_reaper()
            .low()
            .DeleteExtState(section.as_ptr(), key.as_ptr(), false);
    }
}

fn write_shared_toggle_state(command_name: &str, is_on: bool) {
    let Ok(section) = CString::new(SHARED_TOGGLE_EXTSTATE_SECTION) else {
        return;
    };
    let Ok(key) = CString::new(normalize_command_name(command_name)) else {
        return;
    };
    let value = if is_on { c"1".as_ptr() } else { c"0".as_ptr() };
    unsafe {
        Reaper::get().medium_reaper().low().SetExtState(
            section.as_ptr(),
            key.as_ptr(),
            value,
            false,
        );
    }
}

fn read_reaper_toggle_state(
    medium: &reaper_medium::Reaper,
    section_id: u32,
    section_context: SectionContext<'_>,
    command_id: CommandId,
) -> Option<bool> {
    let hook_state = unsafe {
        medium
            .low()
            .GetToggleCommandStateThroughHooks(section_context.to_raw(), command_id.to_raw())
    };
    if hook_state != -1 {
        return Some(hook_state != 0);
    }

    medium.get_toggle_command_state_ex(SectionId::new(section_id), command_id)
}

fn write_reaper_toggle_state(section_id: u32, command_id: CommandId, is_on: bool) {
    let medium = Reaper::get().medium_reaper();
    let state = i32::from(is_on);
    if !medium
        .low()
        .SetToggleCommandState(section_id as i32, command_id.get() as i32, state)
    {
        debug!(
            "SetToggleCommandState({}, {}, {}) returned false",
            section_id,
            command_id.get(),
            state
        );
    }
    medium
        .low()
        .RefreshToolbar2(section_id as i32, command_id.get() as i32);
}

fn execute_main_action(medium: &reaper_medium::Reaper, command_id: CommandId) {
    unsafe {
        medium
            .low()
            .KBD_OnMainActionEx(command_id.to_raw(), 0, -1, 0, null_mut(), null_mut());
    }
}

fn action_toggle_state(
    medium: &reaper_medium::Reaper,
    section_id: u32,
    section_context: SectionContext<'_>,
    command_id: CommandId,
    command_name: Option<&str>,
    toggles: &HashMap<String, bool>,
) -> Option<bool> {
    if let Some(state) = command_name
        .map(normalize_command_name)
        .and_then(|name| toggles.get(name).copied())
    {
        return Some(state);
    }
    if let Some(state) = command_name.and_then(read_shared_toggle_state) {
        return Some(state);
    }

    read_reaper_toggle_state(medium, section_id, section_context, command_id)
}

fn is_sws_action(command_name: Option<&str>, description: &str) -> bool {
    let normalized = command_name.map(normalize_command_name);
    let name_matches = normalized.is_some_and(|name| {
        name.starts_with("SWS")
            || name.starts_with("S&M")
            || name.starts_with("BR_")
            || name.starts_with("FNG_")
            || name.starts_with("NF_")
            || name.starts_with("SN_")
            || name.starts_with("XENAKIOS")
            || name.starts_with("PADRE")
            || name.starts_with("AUTORENDER")
    });
    let desc_matches = description.starts_with("SWS")
        || description.starts_with("S&M")
        || description.starts_with("BR:")
        || description.starts_with("FNG:")
        || description.starts_with("NF:")
        || description.starts_with("SN:")
        || description.starts_with("Xenakios");
    name_matches || desc_matches
}

fn action_provider(command_name: Option<&str>, description: &str) -> (String, Vec<String>) {
    let mut tags = Vec::new();
    let normalized = command_name.map(normalize_command_name);
    if let Some(name) = normalized {
        if name.starts_with("FTS") {
            tags.push("fasttrackstudio".to_string());
            return ("fts".to_string(), tags);
        }
        if name.starts_with("RS") {
            tags.push("script".to_string());
            return ("reascript".to_string(), tags);
        }
        let sws_tags = [
            ("SWS", "sws"),
            ("S&M", "s&m"),
            ("BR_", "br"),
            ("FNG_", "fng"),
            ("NF_", "nf"),
            ("SN_", "sn"),
            ("XENAKIOS", "xenakios"),
            ("PADRE", "padre"),
            ("AUTORENDER", "autorender"),
        ];
        for (prefix, tag) in sws_tags {
            if name.starts_with(prefix) {
                tags.push(tag.to_string());
                return ("sws".to_string(), tags);
            }
        }
    }
    if description.starts_with("SWS") {
        tags.push("sws".to_string());
        return ("sws".to_string(), tags);
    }
    if description.starts_with("S&M") {
        tags.push("s&m".to_string());
        return ("sws".to_string(), tags);
    }
    for (prefix, tag) in [
        ("BR:", "br"),
        ("FNG:", "fng"),
        ("NF:", "nf"),
        ("SN:", "sn"),
        ("Xenakios", "xenakios"),
    ] {
        if description.starts_with(prefix) {
            tags.push(tag.to_string());
            return ("sws".to_string(), tags);
        }
    }
    if command_name.is_some() {
        ("extension".to_string(), tags)
    } else {
        ("reaper".to_string(), tags)
    }
}

fn classify_action(command_name: Option<&str>, description: &str) -> ActionOrigin {
    if let Some(name) = command_name.map(normalize_command_name)
        && name.starts_with("FTS")
    {
        return ActionOrigin::Fts;
    }
    if is_sws_action(command_name, description) {
        return ActionOrigin::Sws;
    }
    if command_name.is_some() {
        ActionOrigin::Extension
    } else {
        ActionOrigin::Reaper
    }
}

fn action_section_label(section: ActionSection) -> (u32, String) {
    (section.unique_id(), section.name())
}

fn section_context_for<'a>(
    _section_id: u32,
    raw: &'a reaper_medium::KbdSectionInfo,
) -> SectionContext<'a> {
    SectionContext::Sec(raw)
}

fn action_matches_filter(info: &ActionInfo, filter: ActionListFilter) -> bool {
    match filter {
        ActionListFilter::All => true,
        ActionListFilter::Reaper => info.origin == ActionOrigin::Reaper,
        ActionListFilter::NonReaper => info.origin != ActionOrigin::Reaper,
        ActionListFilter::Sws => info.origin == ActionOrigin::Sws,
        ActionListFilter::Fts => info.origin == ActionOrigin::Fts,
        ActionListFilter::Registered => {
            info.registered_by_fts
                || (info.origin == ActionOrigin::Fts && info.command_name.is_some())
        }
    }
}

fn action_matches_query(info: &ActionInfo, query: Option<&str>) -> bool {
    let Some(query) = query else {
        return true;
    };
    info.description.to_ascii_lowercase().contains(query)
        || info
            .command_name
            .as_ref()
            .is_some_and(|name| name.to_ascii_lowercase().contains(query))
}

fn find_action_info(command_id: CommandId, section: ActionSection) -> Option<ActionInfo> {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let registered = registered_actions()
        .lock_recoverable("action_registry")
        .clone();
    let toggles = toggle_states().lock_recoverable("action_registry").clone();
    let (section_id, section_name) = action_section_label(section);
    let section = reaper.section_by_id(SectionId::new(section_id));

    section
        .with_raw(|s| {
            for i in 0..s.action_list_cnt() {
                let Some(cmd) = s.get_action_by_index(i).map(|a| a.cmd()) else {
                    continue;
                };
                if cmd != command_id {
                    continue;
                }

                let section_context = section_context_for(section_id, s);
                let description = unsafe {
                    medium.kbd_get_text_from_cmd(cmd, section_context, |name| name.to_string())
                }
                .unwrap_or_default();
                let command_name =
                    medium.reverse_named_command_lookup(cmd, |name| name.to_string());
                let normalized = command_name.as_deref().map(normalize_command_name);
                let registered_by_fts = normalized
                    .is_some_and(|name| registered.contains_key(name))
                    || registered.values().any(|id| *id == cmd.get());
                let toggle_state = action_toggle_state(
                    medium,
                    section_id,
                    section_context,
                    cmd,
                    command_name.as_deref(),
                    &toggles,
                );
                let origin = if registered_by_fts {
                    ActionOrigin::Fts
                } else {
                    classify_action(command_name.as_deref(), &description)
                };
                let (provider, provider_tags) = if registered_by_fts {
                    ("fts".to_string(), vec!["fasttrackstudio".to_string()])
                } else {
                    action_provider(command_name.as_deref(), &description)
                };

                return Some(ActionInfo {
                    command_id: cmd.get(),
                    section_id,
                    section_name: section_name.clone(),
                    command_name,
                    description,
                    origin,
                    provider,
                    provider_tags,
                    registered_by_fts,
                    toggle_state,
                });
            }
            None
        })
        .flatten()
}

// ============================================================================
// Extensions Menu
// ============================================================================

/// Register the Extensions menu hook with REAPER.
///
/// Call once during plugin initialization after `ReaperSession` is created.
/// The hook is invoked each time REAPER shows the Extensions menu,
/// dynamically building it from all registered actions with `show_in_menu`.
pub fn register_extension_menu(session: &mut reaper_medium::ReaperSession) {
    Reaper::get().medium_reaper().add_extensions_main_menu();
    if let Err(e) = session.plugin_register_add_hook_custom_menu::<FtsMenuHook>() {
        warn!("Failed to register menu hook: {:?}", e);
    } else {
        info!("Extensions menu hook registered");
    }
}

/// REAPER menu hook implementation for FastTrackStudio.
struct FtsMenuHook;

impl HookCustomMenu for FtsMenuHook {
    fn call(menuidstr: &ReaperStr, hmenu: Hmenu, flag: MenuHookFlag) {
        let result = std::panic::catch_unwind(|| {
            if flag != MenuHookFlag::Init || menuidstr.to_str() != "Main extensions" {
                return;
            }
            build_extension_menu(hmenu);
        });
        if let Err(e) = result {
            warn!("Panic in menu hook: {:?}", e);
        }
    }
}

/// Build the Extensions > FastTrackStudio menu from registered actions.
fn build_extension_menu(hmenu: Hmenu) {
    let actions = menu_actions().lock_recoverable("action_registry").clone();
    if actions.is_empty() {
        return;
    }

    let swell = reaper_low::Swell::get();

    // Group actions by their derived menu group
    let mut groups: BTreeMap<String, Vec<MenuActionDef>> = BTreeMap::new();
    for action in &actions {
        groups
            .entry(action.group.clone())
            .or_default()
            .push(action.clone());
    }

    // Create "FastTrackStudio" submenu
    let fts_menu = swell.CreatePopupMenu();

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    for (group_name, group_actions) in &mut groups {
        // Create submenu for this group
        let submenu = swell.CreatePopupMenu();

        // Sort actions by display name
        group_actions.sort_by(|a, b| a.display_name.cmp(&b.display_name));

        for action in group_actions.iter() {
            let lookup = format!("_{}", action.command_name);
            if let Some(cmd_id) = medium.named_command_lookup(lookup) {
                let mut text_buf: Vec<u8> = action.display_name.as_bytes().to_vec();
                text_buf.push(0);
                let mut mii = reaper_low::raw::MENUITEMINFO {
                    fMask: 0x40 | 0x04, // MIIM_TYPE | MIIM_ID
                    fType: 0,           // MFT_STRING
                    wID: cmd_id.get(),
                    hSubMenu: std::ptr::null_mut(),
                    dwTypeData: text_buf.as_mut_ptr() as *mut _,
                    ..unsafe { std::mem::zeroed() }
                };
                let count = unsafe { swell.GetMenuItemCount(submenu) };
                unsafe {
                    swell.InsertMenuItem(submenu, count, 1, &mut mii);
                }
            }
        }

        // Add the group submenu to the FTS menu
        let mut label_buf: Vec<u8> = group_name.as_bytes().to_vec();
        label_buf.push(0);
        let mut mii = reaper_low::raw::MENUITEMINFO {
            fMask: 0x10 | 0x40 | 0x04, // MIIM_SUBMENU | MIIM_TYPE | MIIM_ID
            fType: 0,
            wID: 0,
            hSubMenu: submenu,
            dwTypeData: label_buf.as_mut_ptr() as *mut _,
            ..unsafe { std::mem::zeroed() }
        };
        let count = unsafe { swell.GetMenuItemCount(fts_menu) };
        unsafe {
            swell.InsertMenuItem(fts_menu, count, 1, &mut mii);
        }
    }

    // Insert "FastTrackStudio" into the Extensions menu
    let parent = hmenu.as_ptr();
    let mut label = b"FastTrackStudio\0".to_vec();

    // Add separator before our menu if there are already items
    let existing = unsafe { swell.GetMenuItemCount(parent) };
    if existing > 0 {
        let mut sep = reaper_low::raw::MENUITEMINFO {
            fMask: 0x40,  // MIIM_TYPE
            fType: 0x800, // MFT_SEPARATOR
            ..unsafe { std::mem::zeroed() }
        };
        unsafe {
            swell.InsertMenuItem(parent, existing, 1, &mut sep);
        }
    }

    let mut mii = reaper_low::raw::MENUITEMINFO {
        fMask: 0x10 | 0x40 | 0x04, // MIIM_SUBMENU | MIIM_TYPE | MIIM_ID
        fType: 0,
        wID: 0,
        hSubMenu: fts_menu,
        dwTypeData: label.as_mut_ptr() as *mut _,
        ..unsafe { std::mem::zeroed() }
    };
    let pos = unsafe { swell.GetMenuItemCount(parent) };
    unsafe {
        swell.InsertMenuItem(parent, pos, 1, &mut mii);
    }

    debug!(
        "Built FastTrackStudio menu ({} actions in {} groups)",
        actions.len(),
        groups.len()
    );
}

// ============================================================================
// Action Registry Service
// ============================================================================

/// REAPER action registry implementation.
#[derive(Clone)]
pub struct ReaperActionRegistry;

impl ReaperActionRegistry {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ReaperActionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derive_menu_group_session() {
        assert_eq!(derive_menu_group("FTS_SESSION_TOGGLE_PLAYBACK"), "Session");
    }

    #[test]
    fn derive_menu_group_transport() {
        assert_eq!(derive_menu_group("FTS_TRANSPORT_PLAY"), "Transport");
    }

    #[test]
    fn derive_menu_group_markers_regions() {
        assert_eq!(
            derive_menu_group("FTS_MARKERS_REGIONS_INSERT_MARKER"),
            "Markers Regions"
        );
    }

    #[test]
    fn derive_menu_group_signal() {
        assert_eq!(derive_menu_group("FTS_SIGNAL_NEXT_SONG"), "Signal");
    }

    #[test]
    fn derive_menu_group_sync() {
        assert_eq!(derive_menu_group("FTS_SYNC_TOGGLE_LINK"), "Sync");
    }

    #[test]
    fn derive_menu_group_dynamic_template() {
        assert_eq!(
            derive_menu_group("FTS_DYNAMIC_TEMPLATE_SORT_ALL"),
            "Dynamic Template"
        );
    }

    #[test]
    fn derive_menu_group_visibility_manager() {
        assert_eq!(
            derive_menu_group("FTS_VISIBILITY_MANAGER_TOGGLE_DRUMS"),
            "Visibility Manager"
        );
    }

    #[test]
    fn derive_menu_group_auto_color() {
        assert_eq!(derive_menu_group("FTS_AUTO_COLOR_COLOR_ALL"), "Auto Color");
    }

    #[test]
    fn derive_menu_group_daw() {
        assert_eq!(derive_menu_group("FTS_DAW_SOMETHING"), "Daw");
    }

    #[test]
    fn derive_menu_group_unknown_prefix_falls_back_to_first_segment() {
        assert_eq!(derive_menu_group("UNKNOWN_PREFIX"), "Unknown");
    }

    #[test]
    fn titlecase_underscored_multi_word() {
        assert_eq!(titlecase_underscored("MARKERS_REGIONS"), "Markers Regions");
    }

    #[test]
    fn titlecase_underscored_single_word() {
        assert_eq!(titlecase_underscored("TRANSPORT"), "Transport");
    }

    #[test]
    fn titlecase_underscored_two_words() {
        assert_eq!(titlecase_underscored("AUTO_COLOR"), "Auto Color");
    }

    #[test]
    fn titlecase_underscored_empty() {
        assert_eq!(titlecase_underscored(""), "");
    }
}

// ============================================================================
// architect::rpc port —
// ============================================================================
//
// TODO: Wire to the existing helpers above (REGISTERED_ACTIONS, action_broadcaster,
// MENU_ACTIONS, etc.) — currently stubbed with todo!() pending full port.

impl ActionRegistration for crate::Reaper {
    // NOTE: these are stubbed (returning a fake non-zero command id / Ok) so
    // extensions that try to register actions don't panic the host. The full
    // gaccel/REAPER integration is still pending — see TODO above.
    fn register_action(
        &self,
        _command_name: &str,
        _description: &str,
        _show_in_menu: bool,
        _toggleable: bool,
    ) -> u32 {
        1
    }

    fn register(&self, _cmd_name: &str, _description: &str) -> DawResult<u32> {
        Ok(1)
    }

    fn register_in_menu(&self, _cmd_name: &str, _description: &str) -> DawResult<u32> {
        Ok(1)
    }

    fn register_toggle(&self, _cmd_name: &str, _description: &str) -> DawResult<u32> {
        Ok(1)
    }

    fn register_toggle_in_menu(&self, _cmd_name: &str, _description: &str) -> DawResult<u32> {
        Ok(1)
    }

    fn unregister(&self, _cmd_name: &str) -> DawResult<()> {
        Ok(())
    }

    fn is_registered(&self, _command_name: &str) -> bool {
        false
    }

    fn lookup_command_id(&self, _command_name: &str) -> Option<u32> {
        None
    }

    fn is_in_action_list(&self, _command_name: &str) -> bool {
        false
    }

    fn list_actions(&self, _request: ActionListRequest) -> ActionListResponse {
        ActionListResponse::default()
    }

    fn execute_command(&self, _command_id: u32) {}

    fn execute_named_action(&self, _command_name: &str) -> bool {
        false
    }

    fn execute_action(&self, action_id: &str) -> ActionExecutionResult {
        ActionExecutionResult {
            requested_action: action_id.to_string(),
            executed: false,
            command_id: None,
            command_name: None,
            description: None,
            origin: None,
            provider: None,
            provider_tags: Vec::new(),
            registered_by_fts: false,
            toggle_state_before: None,
            toggle_state_after: None,
        }
    }

    fn set_toggle_state(&self, _command_name: &str, _is_on: bool) {}

    fn get_toggle_state(&self, _command_name: &str) -> Option<bool> {
        None
    }
}
