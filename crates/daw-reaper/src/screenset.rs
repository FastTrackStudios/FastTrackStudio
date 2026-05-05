//! REAPER implementation of the FTS screenset service.

use std::ffi::CString;
use std::time::{SystemTime, UNIX_EPOCH};

use daw_proto::{
    CaptureScreensetRequest, Screenset, ScreensetMonitor, ScreensetOptions, ScreensetRect,
    ScreensetResult, ScreensetScope, ScreensetService, ScreensetSummary, ScreensetWindow,
};
use display_info::DisplayInfo;
use reaper_high::Reaper;
use reaper_low::raw;
use reaper_medium::{CommandId, ProjectContext};

use crate::main_thread;
use crate::safe_wrappers::ext_state as sw;

/// Stable id for REAPER's main window inside a captured screenset.
const MAIN_WINDOW_ID: &str = "reaper.main";

const SECTION_GLOBAL: &str = "FTS.Screensets";
const REGISTRY_KEY: &str = "registry";

/// REAPER-backed FTS screenset storage and realtime apply service.
#[derive(Clone, Default)]
pub struct ReaperScreenset;

impl ReaperScreenset {
    pub fn new() -> Self {
        Self
    }
}

fn now_unix() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or_default()
}

fn screenset_key(id: &str) -> String {
    format!("screenset:{id}")
}

fn summary_for(screenset: &Screenset) -> ScreensetSummary {
    ScreensetSummary {
        id: screenset.id.clone(),
        name: screenset.name.clone(),
        description: screenset.description.clone(),
        updated_at_unix: screenset.updated_at_unix,
        tags: screenset.tags.clone(),
        window_count: screenset.windows.len() as u32,
        monitor_count: screenset.monitors.len() as u32,
        action_count: screenset.actions_on_apply.len() as u32,
    }
}

fn validate_id(id: &str) -> Result<(), String> {
    if id.trim().is_empty() {
        return Err("screenset id cannot be empty".to_string());
    }
    if id.contains('\0') || id.contains(':') {
        return Err("screenset id cannot contain NUL or ':'".to_string());
    }
    Ok(())
}

fn cstring(value: &str) -> Result<CString, String> {
    CString::new(value).map_err(|_| "screenset values cannot contain NUL bytes".to_string())
}

fn get_global_value(section: &str, key: &str) -> Option<String> {
    let section = cstring(section).ok()?;
    let key = cstring(key).ok()?;
    sw::get_ext_state(Reaper::get().medium_reaper().low(), &section, &key)
}

fn set_global_value(section: &str, key: &str, value: &str, persist: bool) -> Result<(), String> {
    let section = cstring(section)?;
    let key = cstring(key)?;
    let value = cstring(value)?;
    sw::set_ext_state(
        Reaper::get().medium_reaper().low(),
        &section,
        &key,
        &value,
        persist,
    );
    Ok(())
}

fn delete_global_value(section: &str, key: &str, persist: bool) -> Result<(), String> {
    let section = cstring(section)?;
    let key = cstring(key)?;
    sw::delete_ext_state(Reaper::get().medium_reaper().low(), &section, &key, persist);
    Ok(())
}

fn load_registry() -> Vec<ScreensetSummary> {
    get_global_value(SECTION_GLOBAL, REGISTRY_KEY)
        .and_then(|json| facet_json::from_str::<Vec<ScreensetSummary>>(&json).ok())
        .unwrap_or_default()
}

fn save_registry(registry: &[ScreensetSummary], persist: bool) -> Result<(), String> {
    let json = facet_json::to_string(registry)
        .map_err(|err| format!("encode screenset registry: {err}"))?;
    set_global_value(SECTION_GLOBAL, REGISTRY_KEY, &json, persist)
}

fn load_screenset(id: &str) -> Option<Screenset> {
    get_global_value(SECTION_GLOBAL, &screenset_key(id))
        .and_then(|json| facet_json::from_str::<Screenset>(&json).ok())
}

fn save_screenset_immediate(
    mut screenset: Screenset,
    options: ScreensetOptions,
) -> Result<String, String> {
    if options.scope != ScreensetScope::Global {
        return Err("project-scoped FTS screensets are not implemented yet".to_string());
    }
    screenset.id = screenset.id.trim().to_string();
    validate_id(&screenset.id)?;
    if screenset.name.trim().is_empty() {
        screenset.name = screenset.id.clone();
    }
    screenset.schema_version = 1;
    screenset.updated_at_unix = now_unix();

    let json = facet_json::to_string(&screenset)
        .map_err(|err| format!("encode screenset '{}': {err}", screenset.id))?;
    set_global_value(
        SECTION_GLOBAL,
        &screenset_key(&screenset.id),
        &json,
        options.persist,
    )?;

    let summary = summary_for(&screenset);
    let mut registry = load_registry();
    registry.retain(|row| row.id != summary.id);
    registry.push(summary);
    registry.sort_by(|a, b| a.id.cmp(&b.id));
    save_registry(&registry, options.persist)?;
    Ok(screenset.id)
}

/// Build a stable id for a monitor that survives reboots and dock-undock
/// events. We can't trust the OS-assigned monitor index because adding /
/// removing displays renumbers them. Combining the panel name with its
/// resolution is good enough for the laptop ↔ multi-monitor switch case.
fn monitor_id(name: &str, width: u32, height: u32) -> String {
    let trimmed = name.trim();
    let safe = if trimmed.is_empty() { "display" } else { trimmed };
    format!("{safe}@{width}x{height}")
}

/// Capture the current host monitor topology.
///
/// `display-info` is cross-platform (Windows / macOS / Linux X11+Wayland).
/// On a host with no displays — e.g. headless CI — we return an empty
/// vec rather than failing, so the screenset itself still saves.
fn capture_monitors() -> Vec<ScreensetMonitor> {
    let displays = match DisplayInfo::all() {
        Ok(displays) => displays,
        Err(err) => {
            tracing::warn!("display-info enumeration failed: {err}");
            return Vec::new();
        }
    };
    displays
        .into_iter()
        .map(|d| ScreensetMonitor {
            id: monitor_id(&d.name, d.width, d.height),
            name: d.name,
            x: d.x,
            y: d.y,
            width: d.width,
            height: d.height,
            scale: d.scale_factor,
            primary: d.is_primary,
        })
        .collect()
}

/// Read the bounds of an HWND via REAPER's SWELL-compatible `GetWindowRect`.
/// Returns `None` when SWELL isn't loaded or the call fails.
unsafe fn window_rect(hwnd: raw::HWND) -> Option<ScreensetRect> {
    if !reaper_low::Swell::is_available_globally() {
        tracing::debug!("screenset: SWELL unavailable, skipping window_rect");
        return None;
    }
    let mut r = raw::RECT::default();
    let ok = unsafe { reaper_low::Swell::get().GetWindowRect(hwnd, &mut r) };
    if !ok {
        tracing::debug!("screenset: GetWindowRect returned false for hwnd {hwnd:?}");
        return None;
    }
    let width = (r.right - r.left).max(0) as u32;
    let height = (r.bottom - r.top).max(0) as u32;
    Some(ScreensetRect {
        x: r.left as i32,
        y: r.top as i32,
        width,
        height,
    })
}

/// Pick the monitor that contains a window's center point, falling back to
/// the primary display if the window is wholly off-screen. Returns the id
/// we stamped on the corresponding [`ScreensetMonitor`].
fn monitor_for(rect: &ScreensetRect, monitors: &[ScreensetMonitor]) -> Option<String> {
    if monitors.is_empty() {
        return None;
    }
    let cx = rect.x + (rect.width as i32) / 2;
    let cy = rect.y + (rect.height as i32) / 2;
    for m in monitors {
        let mx2 = m.x + m.width as i32;
        let my2 = m.y + m.height as i32;
        if cx >= m.x && cx < mx2 && cy >= m.y && cy < my2 {
            return Some(m.id.clone());
        }
    }
    monitors
        .iter()
        .find(|m| m.primary)
        .or_else(|| monitors.first())
        .map(|m| m.id.clone())
}

/// Capture REAPER's main window into a [`ScreensetWindow`].
fn capture_main_window(monitors: &[ScreensetMonitor]) -> Option<ScreensetWindow> {
    let hwnd = Reaper::get().medium_reaper().get_main_hwnd().as_ptr();
    if hwnd.is_null() {
        return None;
    }
    let bounds = unsafe { window_rect(hwnd) };
    let monitor_id = bounds
        .as_ref()
        .and_then(|rect| monitor_for(rect, monitors));
    Some(ScreensetWindow {
        id: MAIN_WINDOW_ID.to_string(),
        title: "REAPER".to_string(),
        monitor_id,
        bounds,
        visible: true,
        // The main window is never docked into another container.
        dock_id: None,
        floating: false,
    })
}

/// Apply a window's stored geometry. Currently we only restore position +
/// size for the REAPER main window; per-panel placement requires the dock
/// host adapter (handled separately via `dock_layout_blob`).
fn apply_main_window(window: &ScreensetWindow) -> Result<(), String> {
    if window.id != MAIN_WINDOW_ID {
        return Ok(());
    }
    let Some(rect) = window.bounds else {
        return Ok(());
    };
    let hwnd = Reaper::get().medium_reaper().get_main_hwnd().as_ptr();
    if hwnd.is_null() {
        return Err("main window unavailable".to_string());
    }
    if !reaper_low::Swell::is_available_globally() {
        return Err("SWELL unavailable in this REAPER build".to_string());
    }
    // SWP_NOZORDER (0x4) | SWP_NOACTIVATE (0x10): keep current Z-order
    // and don't steal focus from the foreground app while we're restoring.
    const FLAGS: i32 = 0x4 | 0x10;
    unsafe {
        reaper_low::Swell::get().SetWindowPos(
            hwnd,
            std::ptr::null_mut(),
            rect.x,
            rect.y,
            rect.width as i32,
            rect.height as i32,
            FLAGS,
        );
    }
    Ok(())
}

fn resolve_command_id(command: &str) -> Result<CommandId, String> {
    if let Ok(command_id) = command.parse::<u32>() {
        return Ok(CommandId::new(command_id));
    }
    Reaper::get()
        .action_by_command_name(command)
        .command_id()
        .map_err(|err| format!("action not found '{command}': {err}"))
}

fn apply_screenset_immediate(id: &str) -> Result<String, String> {
    validate_id(id)?;
    let screenset = load_screenset(id).ok_or_else(|| format!("screenset not found: {id}"))?;
    // Restore window geometry first so any actions that paint into the
    // restored layout (e.g. "show mixer") observe the new positions.
    for window in &screenset.windows {
        if let Err(err) = apply_main_window(window) {
            tracing::warn!("apply window '{}': {err}", window.id);
        }
    }
    let reaper = Reaper::get().medium_reaper();
    for action in &screenset.actions_on_apply {
        let command_id = resolve_command_id(action)?;
        reaper.main_on_command_ex(command_id, 0, ProjectContext::CurrentProject);
    }
    set_global_value(SECTION_GLOBAL, "active", &screenset.id, true)?;
    Ok(screenset.id)
}

impl ScreensetService for ReaperScreenset {
    async fn capture_screenset(&self, request: CaptureScreensetRequest) -> ScreensetResult {
        main_thread::query(move || {
            let monitors = capture_monitors();
            let mut windows = Vec::new();
            if let Some(main) = capture_main_window(&monitors) {
                windows.push(main);
            }
            let screenset = Screenset {
                id: request.id,
                name: request.name,
                description: request.description,
                schema_version: 1,
                updated_at_unix: now_unix(),
                tags: request.tags,
                monitors,
                windows,
                // The dock-layout blob is owned by the dock host adapter and
                // attached separately by callers that have access to it.
                dock_layout_blob: Vec::new(),
                actions_on_apply: request.actions_on_apply,
            };
            match save_screenset_immediate(screenset, request.options) {
                Ok(id) => ScreensetResult::ok(id),
                Err(err) => ScreensetResult::error(err),
            }
        })
        .await
        .unwrap_or_else(|| ScreensetResult::error("Main thread dispatcher not available"))
    }

    async fn save_screenset(
        &self,
        screenset: Screenset,
        options: ScreensetOptions,
    ) -> ScreensetResult {
        main_thread::query(move || match save_screenset_immediate(screenset, options) {
            Ok(id) => ScreensetResult::ok(id),
            Err(err) => ScreensetResult::error(err),
        })
        .await
        .unwrap_or_else(|| ScreensetResult::error("Main thread dispatcher not available"))
    }

    async fn list_screensets(&self, options: ScreensetOptions) -> Vec<ScreensetSummary> {
        main_thread::query(move || {
            if options.scope != ScreensetScope::Global {
                return Vec::new();
            }
            load_registry()
        })
        .await
        .unwrap_or_default()
    }

    async fn get_screenset(&self, id: String, options: ScreensetOptions) -> Option<Screenset> {
        main_thread::query(move || {
            if options.scope != ScreensetScope::Global || validate_id(&id).is_err() {
                return None;
            }
            load_screenset(&id)
        })
        .await
        .flatten()
    }

    async fn apply_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult {
        main_thread::query(move || {
            if options.scope != ScreensetScope::Global {
                return ScreensetResult::error(
                    "project-scoped FTS screensets are not implemented yet",
                );
            }
            match apply_screenset_immediate(&id) {
                Ok(id) => ScreensetResult::ok(id),
                Err(err) => ScreensetResult::error(err),
            }
        })
        .await
        .unwrap_or_else(|| ScreensetResult::error("Main thread dispatcher not available"))
    }

    async fn delete_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult {
        main_thread::query(move || {
            if options.scope != ScreensetScope::Global {
                return ScreensetResult::error(
                    "project-scoped FTS screensets are not implemented yet",
                );
            }
            if let Err(err) = validate_id(&id) {
                return ScreensetResult::error(err);
            }
            if let Err(err) =
                delete_global_value(SECTION_GLOBAL, &screenset_key(&id), options.persist)
            {
                return ScreensetResult::error(err);
            }
            let mut registry = load_registry();
            registry.retain(|row| row.id != id);
            if let Err(err) = save_registry(&registry, options.persist) {
                return ScreensetResult::error(err);
            }
            ScreensetResult::ok(id)
        })
        .await
        .unwrap_or_else(|| ScreensetResult::error("Main thread dispatcher not available"))
    }
}
