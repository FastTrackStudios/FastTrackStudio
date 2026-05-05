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
use daw_proto::{
    ActionEvent, ActionExecutionResult, ActionInfo, ActionListFilter, ActionListRequest,
    ActionListResponse, ActionOrigin, ActionRegistryService, ActionSection,
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
    let mut states = toggle_states().lock().unwrap();
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
    let registered = registered_actions().lock().unwrap().clone();
    let toggles = toggle_states().lock().unwrap().clone();
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
    let actions = menu_actions().lock().unwrap().clone();
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

impl ActionRegistryService for ReaperActionRegistry {
    async fn register_action(
        &self,
        command_name: String,
        description: String,
        show_in_menu: bool,
        toggleable: bool,
    ) -> u32 {
        // Check if already registered by us
        {
            let map = registered_actions().lock().unwrap();
            if let Some(&cmd_id) = map.get(&command_name) {
                debug!(
                    "Action '{}' already registered (cmd_id={})",
                    command_name, cmd_id
                );
                return cmd_id;
            }
        }

        // If toggleable, initialize toggle state to off
        if toggleable {
            toggle_states()
                .lock()
                .unwrap()
                .insert(command_name.clone(), false);
        }

        let name_for_query = command_name.clone();
        let desc_for_query = description.clone();

        let result = main_thread::query(move || {
            let reaper = Reaper::get();
            let desc_static: &'static str = Box::leak(desc_for_query.clone().into_boxed_str());

            // register_action needs 'static string args — leak the strings
            // since actions live for the process lifetime anyway.
            let name_static: &'static str = Box::leak(name_for_query.clone().into_boxed_str());

            // Capture command_name for the trigger notification
            let trigger_name = name_for_query.clone();

            let kind = if toggleable {
                // For toggleable actions, read state from the shared toggle map.
                // Guests update this via set_toggle_state().
                let state_key = name_for_query.clone();
                reaper_high::ActionKind::Toggleable(Box::new(move || read_toggle_state(&state_key)))
            } else {
                reaper_high::ActionKind::NotToggleable
            };

            let action = reaper.register_action(
                name_static,
                desc_static,
                None, // no default key binding
                move || {
                    if toggleable {
                        let state = flip_toggle_state(&trigger_name);
                        write_shared_toggle_state(&trigger_name, state);
                        if let Some(cmd_id) = named_command_lookup(&trigger_name) {
                            write_reaper_toggle_state(0, cmd_id, state);
                        }
                        debug!("Toggle state for '{}' -> {}", trigger_name, state);
                    }

                    // Notify all subscribers that this action was triggered.
                    // Consumers handle domain logic; fake-toggle state is
                    // host-managed so REAPER's UI updates synchronously.
                    info!("Action triggered: {}", trigger_name);
                    notify_action_triggered(trigger_name.clone());
                },
                kind,
            );

            let cmd_id = action.command_id();

            // reaper_high::register_action only stores the command in its internal
            // map and calls plugin_register_add_command_id. It does NOT register the
            // gaccel (action list entry) when the session is already awake — that
            // only happens during wake_up(). It also does not deduplicate against
            // gaccels that REAPER itself preregistered while parsing reaper-menu.ini
            // toolbar references (REAPER allocates a cmd_id for any `_NAME` token
            // it sees in a Main toolbar, before our extension's spawned task gets
            // a chance to run). Skip the manual gaccel if REAPER already lists the
            // action; otherwise register it and keep the handle for unregister.
            let already_listed = reaper
                .main_section()
                .with_raw(|s| {
                    (0..s.action_list_cnt()).any(|i| {
                        s.get_action_by_index(i)
                            .map(|a| a.cmd() == cmd_id)
                            .unwrap_or(false)
                    })
                })
                .unwrap_or(false);
            let gaccel_handle = if already_listed {
                None
            } else {
                let gaccel = OwnedGaccelRegister::without_key_binding(cmd_id, desc_static);
                let mut session = reaper.medium_session();
                match session.plugin_register_add_gaccel(gaccel) {
                    Ok(h) => Some(h),
                    Err(e) => {
                        warn!(
                            "Failed to register gaccel for '{}': {:?}",
                            name_for_query, e
                        );
                        None
                    }
                }
            };

            if toggleable {
                // REAPER can be slow to surface newly registered toggle actions in the
                // action list unless their toolbar state is refreshed at least once.
                write_shared_toggle_state(&name_for_query, false);
                write_reaper_toggle_state(0, cmd_id, false);
            }

            let cmd_id_val = cmd_id.get();
            debug!(
                "Registered action '{}' → cmd_id={} (\"{}\")",
                name_for_query, cmd_id_val, desc_for_query
            );

            // Park the RegisteredAction + (optional) gaccel handle in
            // OWNED_ACTIONS keyed by command_name so unregister can drop them.
            owned_actions().lock().unwrap().insert(
                name_for_query.clone(),
                OwnedAction {
                    action,
                    gaccel_handle,
                },
            );

            cmd_id_val
        })
        .await;

        match result {
            Some(cmd_id) if cmd_id > 0 => {
                registered_actions()
                    .lock()
                    .unwrap()
                    .insert(command_name.clone(), cmd_id);

                // Store menu metadata if this action should appear in the menu
                if show_in_menu {
                    let group = derive_menu_group(&command_name);
                    menu_actions().lock().unwrap().push(MenuActionDef {
                        command_name: command_name.clone(),
                        display_name: description.clone(),
                        group,
                    });
                }

                debug!("Action '{}' registered: cmd_id={}", command_name, cmd_id);
                cmd_id
            }
            _ => {
                warn!("Failed to register action '{}'", command_name);
                0
            }
        }
    }

    async fn unregister_action(&self, command_name: String) -> bool {
        let removed = registered_actions()
            .lock()
            .unwrap()
            .remove(&command_name)
            .is_some();

        if removed {
            // Tear down with REAPER on the main thread: drop the
            // RegisteredAction (removes the hook entry) AND remove the
            // gaccel handle (takes the entry out of the action list +
            // NamedCommandLookup table).
            let name_for_query = command_name.clone();
            let _ = main_thread::query(move || {
                let owned = owned_actions().lock().unwrap().remove(&name_for_query);
                if let Some(owned) = owned {
                    let reaper = Reaper::get();
                    if let Some(handle) = owned.gaccel_handle {
                        let mut session = reaper.medium_session();
                        session.plugin_register_remove_gaccel(handle);
                    }
                    // Explicit unregister of the action — drops the
                    // command from reaper_high's command_by_id map. We
                    // intentionally do NOT call `plugin_register("-command_id", …)`
                    // here: REAPER's named-command allocations are sticky for
                    // the lifetime of the process. Freeing the slot creates a
                    // window where toolbar/menu entries still reference the old
                    // cmd_id while a re-register would mint a new one, leaving
                    // the toolbar pointed at a dead id. Letting the cmd_id
                    // remain in REAPER's table means re-register rebinds the
                    // closure to the original id (see `register_action` —
                    // `plugin_register_add_command_id` returns the existing id
                    // when the name is already known).
                    owned.action.unregister();
                }
                toggle_states().lock().unwrap().remove(&name_for_query);
                delete_shared_toggle_state(&name_for_query);
            })
            .await;

            // Also remove from menu metadata
            menu_actions()
                .lock()
                .unwrap()
                .retain(|a| a.command_name != command_name);

            // Clear any toggle state recorded for this action so a later
            // re-register starts fresh (no stale on/off carrying over).
            toggle_states().lock().unwrap().remove(&command_name);

            info!("Unregistered action '{}' (full teardown)", command_name);
        } else {
            debug!(
                "Action '{}' not found in our registry (may not have been registered by us)",
                command_name
            );
        }

        removed
    }

    async fn is_registered(&self, command_name: String) -> bool {
        main_thread::query(move || named_command_lookup(&command_name).is_some())
            .await
            .unwrap_or(false)
    }

    async fn is_in_action_list(&self, command_name: String) -> bool {
        main_thread::query(move || {
            let reaper = Reaper::get();

            // First resolve the command name to a command ID
            let cmd_id = match named_command_lookup(&command_name) {
                Some(id) => id,
                None => return false,
            };

            // Enumerate the main section's action list to see if this command ID
            // actually has a gaccel entry (i.e., appears in Actions > Show action list)
            let section = reaper.main_section();
            section
                .with_raw(|s| {
                    (0..s.action_list_cnt()).any(|i| {
                        s.get_action_by_index(i)
                            .map(|a| a.cmd() == cmd_id)
                            .unwrap_or(false)
                    })
                })
                .unwrap_or(false)
        })
        .await
        .unwrap_or(false)
    }

    async fn lookup_command_id(&self, command_name: String) -> Option<u32> {
        main_thread::query(move || named_command_lookup(&command_name).map(|id| id.get()))
            .await
            .flatten()
    }

    async fn list_actions(&self, request: ActionListRequest) -> ActionListResponse {
        main_thread::query(move || {
            let reaper = Reaper::get();
            let medium = reaper.medium_reaper();
            let registered = registered_actions().lock().unwrap().clone();
            let toggles = toggle_states().lock().unwrap().clone();
            let query = request.query.as_ref().map(|q| q.to_ascii_lowercase());
            let limit = request.limit.unwrap_or(u32::MAX) as usize;
            let (section_id, section_name) = action_section_label(request.section);
            let mut actions = Vec::new();
            let mut total_count = 0_u32;

            let section = reaper.section_by_id(SectionId::new(section_id));
            let _ = section.with_raw(|s| {
                for i in 0..s.action_list_cnt() {
                    let Some(cmd) = s.get_action_by_index(i).map(|a| a.cmd()) else {
                        continue;
                    };

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

                    let info = ActionInfo {
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
                    };

                    if action_matches_filter(&info, request.filter)
                        && action_matches_query(&info, query.as_deref())
                    {
                        if actions.len() >= limit {
                            total_count += 1;
                            continue;
                        }
                        actions.push(info);
                        total_count += 1;
                    }
                }
            });

            ActionListResponse {
                total_count,
                actions,
            }
        })
        .await
        .unwrap_or_default()
    }

    async fn subscribe_actions(&self, tx: Tx<ActionEvent>) {
        let mut rx = action_broadcaster().subscribe();
        info!(
            "Action event subscriber added (receivers: {})",
            action_broadcaster().receiver_count()
        );

        moire::task::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(command_name) => {
                        let event = ActionEvent::Triggered { command_name };
                        if tx.send(event).await.is_err() {
                            debug!("Action event subscriber disconnected");
                            break;
                        }
                    }
                    Err(broadcast::error::RecvError::Lagged(count)) => {
                        debug!("Action event subscriber lagged by {count} messages");
                    }
                    Err(broadcast::error::RecvError::Closed) => {
                        info!("Action broadcast channel closed");
                        break;
                    }
                }
            }
        });
    }

    async fn execute_command(&self, command_id: u32) {
        main_thread::query(move || {
            execute_main_action(&Reaper::get().medium_reaper(), CommandId::new(command_id));
        })
        .await;
    }

    async fn execute_named_action(&self, command_name: String) -> bool {
        main_thread::query(move || {
            let reaper = Reaper::get();
            let medium = reaper.medium_reaper();
            if let Some(cmd_id) = named_command_lookup(&command_name) {
                execute_main_action(&medium, cmd_id);
                true
            } else {
                warn!("Named action not found: {}", command_name);
                false
            }
        })
        .await
        .unwrap_or(false)
    }

    async fn execute_action(&self, action_id: String) -> ActionExecutionResult {
        let fallback_action = action_id.clone();
        main_thread::query(move || {
            let reaper = Reaper::get();
            let medium = reaper.medium_reaper();
            let requested_action = action_id.clone();

            let command_id = if let Ok(id) = action_id.parse::<u32>() {
                if id == 0 {
                    None
                } else {
                    Some(CommandId::new(id))
                }
            } else {
                named_command_lookup(&action_id)
            };

            let Some(command_id) = command_id else {
                return ActionExecutionResult {
                    requested_action,
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
                };
            };

            let before = find_action_info(command_id, ActionSection::Main);
            execute_main_action(medium, command_id);
            let after = find_action_info(command_id, ActionSection::Main);
            let info = after.as_ref().or(before.as_ref());

            ActionExecutionResult {
                requested_action,
                executed: true,
                command_id: Some(command_id.get()),
                command_name: info.and_then(|info| info.command_name.clone()),
                description: info.map(|info| info.description.clone()),
                origin: info.map(|info| info.origin),
                provider: info.map(|info| info.provider.clone()),
                provider_tags: info
                    .map(|info| info.provider_tags.clone())
                    .unwrap_or_default(),
                registered_by_fts: info.is_some_and(|info| info.registered_by_fts),
                toggle_state_before: before.and_then(|info| info.toggle_state),
                toggle_state_after: after.and_then(|info| info.toggle_state),
            }
        })
        .await
        .unwrap_or_else(|| ActionExecutionResult {
            requested_action: fallback_action,
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
        })
    }

    async fn set_toggle_state(&self, command_name: String, is_on: bool) {
        let was_known = {
            let mut states = toggle_states().lock().unwrap();
            if states.contains_key(&command_name) {
                states.insert(command_name.clone(), is_on);
                debug!("Toggle state for '{}' set to {}", command_name, is_on);
                true
            } else {
                debug!(
                    "Ignoring set_toggle_state for '{}' — not registered as toggleable",
                    command_name
                );
                false
            }
        };

        if !was_known {
            let _ = main_thread::query(move || {
                let Some(cmd_id) = named_command_lookup(&command_name) else {
                    return;
                };
                let Some(info) = find_action_info(cmd_id, ActionSection::Main) else {
                    return;
                };
                if info.origin != ActionOrigin::Fts {
                    return;
                }
                if info.toggle_state.is_none() && read_shared_toggle_state(&command_name).is_none()
                {
                    return;
                }
                write_shared_toggle_state(&command_name, is_on);
                write_reaper_toggle_state(0, cmd_id, is_on);
            })
            .await;
            return;
        }

        // Look up the cmd_id and tell REAPER to repaint any toolbar /
        // menu cells showing this action's toggle indicator. REAPER
        // queries the toggleaction hook lazily on repaint — without
        // this nudge the indicator may keep showing the previous value
        // until the user mouses over the action list. Mirrors SWS's
        // RefreshToolbar2() / NSEEL_PROC_executeToolbarRefresh pattern.
        let cmd_id = registered_actions()
            .lock()
            .unwrap()
            .get(&command_name)
            .copied();
        if let Some(cmd_id) = cmd_id {
            // Hop to the main thread; RefreshToolbar2 must be called
            // there per the REAPER plugin SDK contract.
            let _ = main_thread::query(move || {
                write_shared_toggle_state(&command_name, is_on);
                write_reaper_toggle_state(0, CommandId::new(cmd_id), is_on);
            })
            .await;
        }
    }

    async fn get_toggle_state(&self, command_name: String) -> Option<bool> {
        main_thread::query(move || {
            let normalized = normalize_command_name(&command_name);
            let toggles = toggle_states().lock().unwrap();
            if let Some(state) = toggles.get(normalized).copied() {
                return Some(state);
            }
            if let Some(state) = read_shared_toggle_state(normalized) {
                return Some(state);
            }

            let medium = Reaper::get().medium_reaper();
            let command_id = named_command_lookup(normalized)?;
            Reaper::get()
                .main_section()
                .with_raw(|section| {
                    read_reaper_toggle_state(&medium, 0, SectionContext::Sec(section), command_id)
                })
                .flatten()
        })
        .await
        .flatten()
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
