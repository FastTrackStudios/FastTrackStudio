//! REAPER implementation of the `WindowManager` service.
//!
//! Layouts are stored in our own JSON files (one per layout name) and
//! applied by driving REAPER's public dock/window APIs:
//! - `DockIsChildOfDock` reads the current state
//! - `DockWindowAddEx` / `Dock_UpdateDockID` moves windows between dockers
//! - `SetWindowPos` (via SWELL) positions floating windows
//! - The "Toolbar: Open/close floating toolbar N" action shows/hides toolbars
//!
//! We deliberately do **not** read or write REAPER's
//! `reaper-screensets.ini` — its per-window `poslist*_data` blobs are
//! produced by plugin-registered `screenset_register` callbacks and
//! aren't safe to round-trip from outside REAPER.

use std::collections::HashMap;
use std::ffi::c_void;
use std::path::PathBuf;

use daw_proto::window_manager::{
    WindowLayout, WindowLayoutOptions, WindowLayoutResult, WindowLayoutSummary, WindowManager,
};
use reaper_high::Reaper as HighReaper;
use reaper_low::Swell;
use reaper_low::raw;

// ─── Window enumeration ─────────────────────────────────────────────────────
//
// REAPER's floating toolbars and panels surface in one of two places:
// - As top-level windows (when free-floating)
// - As descendants of REAPER's main HWND (when docked or hosted in a
//   docker pane)
//
// `for_each_reaper_window` walks both so callers can find a toolbar by
// title regardless of its current dock state.

/// Read a window's title via SWELL. Returns empty string on failure.
fn window_text(hwnd: raw::HWND) -> String {
    if hwnd.is_null() {
        return String::new();
    }
    let mut buf = vec![0u8; 256];
    let written =
        unsafe { Swell::get().GetWindowText(hwnd, buf.as_mut_ptr() as *mut _, buf.len() as _) };
    if written <= 0 {
        return String::new();
    }
    let nul = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    String::from_utf8_lossy(&buf[..nul]).to_string()
}

fn for_each_top_level_window<F>(mut visit: F)
where
    F: FnMut(raw::HWND, String) -> bool,
{
    unsafe extern "C" fn cb<F>(hwnd: raw::HWND, lp: raw::LPARAM) -> raw::BOOL
    where
        F: FnMut(raw::HWND, String) -> bool,
    {
        let cb_ptr = lp as *mut F;
        let title = window_text(hwnd);
        if unsafe { (*cb_ptr)(hwnd, title) } {
            1
        } else {
            0
        }
    }
    unsafe {
        Swell::get().EnumWindows(Some(cb::<F>), &mut visit as *mut F as raw::LPARAM);
    }
}

fn for_each_child_window<F>(parent: raw::HWND, mut visit: F)
where
    F: FnMut(raw::HWND, String) -> bool,
{
    if parent.is_null() {
        return;
    }
    unsafe extern "C" fn cb<F>(hwnd: raw::HWND, lp: raw::LPARAM) -> raw::BOOL
    where
        F: FnMut(raw::HWND, String) -> bool,
    {
        let cb_ptr = lp as *mut F;
        let title = window_text(hwnd);
        if unsafe { (*cb_ptr)(hwnd, title) } {
            1
        } else {
            0
        }
    }
    unsafe {
        Swell::get().EnumChildWindows(parent, Some(cb::<F>), &mut visit as *mut F as raw::LPARAM);
    }
}

/// Walk both top-level windows *and* the descendants of REAPER's main
/// HWND. Toolbars surface in one or the other depending on whether
/// they're floating or docked.
fn for_each_reaper_window<F>(mut visit: F)
where
    F: FnMut(raw::HWND, String) -> bool,
{
    let mut keep_going = true;
    for_each_top_level_window(|hwnd, title| {
        if !keep_going {
            return false;
        }
        keep_going = visit(hwnd, title);
        keep_going
    });
    if !keep_going {
        return;
    }
    let main = unsafe { reaper_low::Reaper::get().GetMainHwnd() };
    for_each_child_window(main, |hwnd, title| {
        if !keep_going {
            return false;
        }
        keep_going = visit(hwnd, title);
        keep_going
    });
}

/// Build a map of window title → HWND for every visible REAPER window.
/// Refresh per-call: HWNDs change when toolbars close and reopen.
pub fn toolbar_hwnds() -> HashMap<String, raw::HWND> {
    let mut out = HashMap::new();
    for_each_reaper_window(|hwnd, title| {
        if !title.is_empty() {
            // Top-level entries arrive first; keep them over child
            // duplicates so a floating toolbar wins over a docker-pane
            // child with the same title.
            out.entry(title).or_insert(hwnd);
        }
        true
    });
    out
}

// ─── Toolbar state queries ──────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DockState {
    /// Window is docked in a fixed REAPER docker.
    Docked { docker_id: i32 },
    /// Window is docked into a free-positioned ("floating") docker frame.
    FloatingDocker { docker_id: i32 },
    /// Window is a regular floating window — not in any docker.
    Floating,
}

/// Resolve the dock state of any HWND via REAPER's `DockIsChildOfDock`.
pub fn current_dock_state(hwnd: raw::HWND) -> Option<DockState> {
    if hwnd.is_null() {
        return None;
    }
    let reaper = reaper_low::Reaper::get();
    let mut is_floating: bool = false;
    let dock_id = unsafe { reaper.DockIsChildOfDock(hwnd, &mut is_floating) };
    if dock_id < 0 {
        Some(DockState::Floating)
    } else if is_floating {
        Some(DockState::FloatingDocker { docker_id: dock_id })
    } else {
        Some(DockState::Docked { docker_id: dock_id })
    }
}

/// Whether the window is currently shown on screen.
pub fn is_window_visible(hwnd: raw::HWND) -> bool {
    if hwnd.is_null() {
        return false;
    }
    unsafe { Swell::get().IsWindowVisible(hwnd) }
}

/// Read a window's absolute screen rect via SWELL's `GetWindowRect`.
fn window_rect(hwnd: raw::HWND) -> Option<raw::RECT> {
    if hwnd.is_null() {
        return None;
    }
    let mut rect = raw::RECT {
        left: 0,
        top: 0,
        right: 0,
        bottom: 0,
    };
    let ok = unsafe { Swell::get().GetWindowRect(hwnd, &mut rect) };
    if ok { Some(rect) } else { None }
}

/// Primary screen size in pixels via `GetSystemMetrics`. SM_CXSCREEN=0,
/// SM_CYSCREEN=1 (Windows + SWELL agree on these).
fn primary_screen_size() -> (i32, i32) {
    let swell = Swell::get();
    let w = swell.GetSystemMetrics(0);
    let h = swell.GetSystemMetrics(1);
    (w.max(1), h.max(1))
}

/// Match `<Mode> <slot>` titles like `Organize 1`. Used to filter
/// discovery results down to mode toolbars.
fn is_mode_toolbar_title(title: &str) -> bool {
    let Some((mode_word, slot_word)) = title.rsplit_once(' ') else {
        return false;
    };
    if slot_word.parse::<u32>().is_err() {
        return false;
    }
    matches!(
        mode_word,
        "Organize" | "Write" | "Produce" | "Record" | "Edit" | "Mix" | "Master" | "Live"
    )
}

// ─── Toolbar identity + visibility actions ──────────────────────────────────
//
// REAPER assigns the 32 floating toolbar slots to three different
// `Toolbar: Open/close toolbar N` action ranges (decoded by probing the
// action list on REAPER 7.66):
//
//   slot  1..= 8 → cmd 41679..41686
//   slot  9..=16 → cmd 41936..41943
//   slot 17..=32 → cmd 42713..42728
//
// These actions are toggleable — `GetToggleCommandStateEx` returns 0/1
// for hidden/shown — so we can read state before firing to make the
// toggle idempotent.

/// Number of floating toolbars REAPER ships.
const REAPER_FLOATING_TOOLBAR_COUNT: u32 = 32;

/// Map a 1-based floating-toolbar slot to its `Toolbar: Open/close
/// toolbar N` REAPER command ID. Returns `None` for out-of-range slots.
fn toolbar_toggle_command_id(slot: u32) -> Option<u32> {
    match slot {
        1..=8 => Some(41678 + slot),
        9..=16 => Some(41927 + slot),
        17..=32 => Some(42696 + slot),
        _ => None,
    }
}

/// Decode a `<Mode> <N>` title into the 1-based floating-toolbar slot
/// that the renamer assigned at startup. Mode order matches
/// `Mode::ALL` in session::mode_actions; slot = mode_idx*3 + n.
fn slot_for_mode_toolbar_title(title: &str) -> Option<u32> {
    let (mode_word, slot_word) = title.rsplit_once(' ')?;
    let n: u32 = slot_word.parse().ok()?;
    if !(1..=3).contains(&n) {
        return None;
    }
    let mode_idx = match mode_word {
        "Organize" => 0,
        "Write" => 1,
        "Produce" => 2,
        "Record" => 3,
        "Edit" => 4,
        "Mix" => 5,
        "Master" => 6,
        "Live" => 7,
        _ => return None,
    };
    Some((mode_idx as u32) * 3 + n)
}

/// Resolve a layout name to its 0-based mode index (`Organize` → 0,
/// ..., `Live` → 7). Case-sensitive — matches the names we register
/// via `mode_defs` exactly.
fn mode_index_for_layout_name(name: &str) -> Option<u32> {
    match name {
        "Organize" => Some(0),
        "Write" => Some(1),
        "Produce" => Some(2),
        "Record" => Some(3),
        "Edit" => Some(4),
        "Mix" => Some(5),
        "Master" => Some(6),
        "Live" => Some(7),
        _ => None,
    }
}

/// Build the full list of mode-toolbar titles in slot order (`Organize 1`
/// at slot 1, ..., `Live 3` at slot 24). Used to know which toolbars are
/// "mode managed" for hide-others-on-apply logic.
fn all_mode_toolbar_titles() -> Vec<(u32, String)> {
    let modes = [
        "Organize", "Write", "Produce", "Record", "Edit", "Mix", "Master", "Live",
    ];
    let mut out = Vec::with_capacity(modes.len() * 3);
    for (mode_idx, mode) in modes.iter().enumerate() {
        for n in 1..=3u32 {
            let slot = (mode_idx as u32) * 3 + n;
            out.push((slot, format!("{mode} {n}")));
        }
    }
    out
}

/// Is the toolbar at this slot currently visible? Reads REAPER's
/// toggle-command state for the matching open/close action.
fn is_toolbar_slot_visible(slot: u32) -> Option<bool> {
    let cmd = toolbar_toggle_command_id(slot)?;
    let state = unsafe { reaper_low::Reaper::get().GetToggleCommandStateEx(0, cmd as i32) };
    if state < 0 { None } else { Some(state != 0) }
}

/// Toggle a floating toolbar's visibility via REAPER's `Toolbar:
/// Open/close toolbar N` action. No-op when the toolbar is already in
/// the requested state.
fn set_toolbar_slot_visible(slot: u32, show: bool) -> Result<(), String> {
    let Some(cmd_id) = toolbar_toggle_command_id(slot) else {
        return Err(format!("toolbar slot {slot} out of range 1..=32"));
    };
    if let Some(current) = is_toolbar_slot_visible(slot)
        && current == show
    {
        return Ok(());
    }
    unsafe {
        reaper_low::Reaper::get().Main_OnCommand(cmd_id as i32, 0);
    }
    Ok(())
}

// ─── Mode docker layout config ──────────────────────────────────────────────
//
// REAPER's 16 dockers are user-configured to physical screen positions,
// so we can't infer top/left/right from the docker ID alone. The user
// names which docker ID corresponds to each position via a small JSON
// config at `<resource>/fasttrackstudio/mode_docker_layout.json`. The
// file is loaded fresh on each `apply_layout` call so edits take effect
// without a restart. Falls back to a `Default` instance when missing.

const MODE_DOCKER_LAYOUT_PATH: &str = "fasttrackstudio/mode_docker_layout.json";

fn load_mode_docker_layout() -> daw_proto::window_manager::ModeDockerLayout {
    use daw_proto::window_manager::ModeDockerLayout;

    let Some(resource) = Some(HighReaper::get().resource_path()) else {
        return ModeDockerLayout::default();
    };
    let path = PathBuf::from(resource.as_str()).join(MODE_DOCKER_LAYOUT_PATH);
    match std::fs::read_to_string(&path) {
        Ok(contents) => match facet_json::from_str::<ModeDockerLayout>(&contents) {
            Ok(layout) => layout,
            Err(err) => {
                tracing::warn!(
                    path = %path.display(),
                    error = %err,
                    "mode_docker_layout.json failed to parse — using defaults"
                );
                ModeDockerLayout::default()
            }
        },
        Err(_) => ModeDockerLayout::default(),
    }
}

/// Resolve the target docker ID for a mode-toolbar slot (1..=3) using
/// the user's configured top/left/right mapping.
fn docker_id_for_slot_position(
    slot_in_mode: u32,
    layout: &daw_proto::window_manager::ModeDockerLayout,
) -> Option<i32> {
    match slot_in_mode {
        1 => Some(layout.top),
        2 => Some(layout.left),
        3 => Some(layout.right),
        _ => None,
    }
}

/// Force-dock a toolbar HWND into the given docker via REAPER's
/// `DockWindowAdd` (which accepts a numeric docker ID directly, unlike
/// `DockWindowAddEx` which keys off an ident string). No-op when
/// `hwnd` is null.
fn dock_window_to(hwnd: raw::HWND, title: &str, docker_id: i32) {
    if hwnd.is_null() {
        return;
    }
    let mut name_buf: Vec<u8> = title.as_bytes().to_vec();
    name_buf.push(0);
    unsafe {
        reaper_low::Reaper::get().DockWindowAdd(
            hwnd,
            name_buf.as_ptr() as *const _,
            docker_id,
            true,
        );
    }
}

// ─── Storage (JSON files, one per layout) ───────────────────────────────────
//
// Layouts live under `<reaper_resource_path>/fasttrackstudio/layouts/`
// with one `<name>.json` per layout. Storing per-file keeps each layout
// independently editable, diffable, and avoids any contention with
// REAPER's own ini files. `facet_json` round-trips the same proto types
// the RPC surface uses.

const LAYOUTS_SUBDIR: &str = "fasttrackstudio/layouts";

fn layouts_dir() -> Option<PathBuf> {
    let resource = HighReaper::get().resource_path();
    Some(PathBuf::from(resource.as_str()).join(LAYOUTS_SUBDIR))
}

fn layout_path(name: &str) -> Option<PathBuf> {
    if name.is_empty() {
        return None;
    }
    // Strict whitelist to keep names safe as filesystem paths and as
    // future config keys: alphanumerics, space, dash, underscore.
    if !name
        .chars()
        .all(|c| c.is_alphanumeric() || c == ' ' || c == '-' || c == '_')
    {
        return None;
    }
    Some(layouts_dir()?.join(format!("{name}.json")))
}

fn ensure_layouts_dir() -> std::io::Result<PathBuf> {
    let dir = layouts_dir().ok_or_else(|| std::io::Error::other("no REAPER resource path"))?;
    std::fs::create_dir_all(&dir)?;
    Ok(dir)
}

fn load_layout_from_disk(name: &str) -> Option<WindowLayout> {
    let path = layout_path(name)?;
    let contents = std::fs::read_to_string(&path).ok()?;
    facet_json::from_str::<WindowLayout>(&contents).ok()
}

fn write_layout_to_disk(layout: &WindowLayout) -> std::io::Result<()> {
    let path = layout_path(&layout.name).ok_or_else(|| {
        std::io::Error::other(format!(
            "layout name '{}' invalid (must be alnum/space/-/_)",
            layout.name
        ))
    })?;
    ensure_layouts_dir()?;
    let json = facet_json::to_string(layout)
        .map_err(|e| std::io::Error::other(format!("serialize layout: {e}")))?;
    std::fs::write(&path, json)
}

fn list_layouts_from_disk() -> Vec<WindowLayoutSummary> {
    let Some(dir) = layouts_dir() else {
        return Vec::new();
    };
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("json") {
            continue;
        }
        let Ok(contents) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(layout) = facet_json::from_str::<WindowLayout>(&contents) else {
            tracing::warn!(
                path = %path.display(),
                "skipping unreadable FTS layout file"
            );
            continue;
        };
        out.push(summary_for(&layout));
    }
    out.sort_by(|a, b| a.name.cmp(&b.name));
    out
}

fn summary_for(layout: &WindowLayout) -> WindowLayoutSummary {
    WindowLayoutSummary {
        name: layout.name.clone(),
        description: layout.description.clone(),
        toolbar_count: layout.toolbars.len() as u32,
        action_count: layout.actions_on_apply.len() as u32,
    }
}

// ─── Diagnostics ────────────────────────────────────────────────────────────

pub fn debug_dump_top_level_windows() {
    let mut top_count = 0usize;
    for_each_top_level_window(|hwnd, title| {
        if !title.is_empty() {
            tracing::info!(
                hwnd = ?(hwnd as *const c_void),
                title = %title,
                scope = "top",
                "REAPER window"
            );
            top_count += 1;
        }
        true
    });
    let main = unsafe { reaper_low::Reaper::get().GetMainHwnd() };
    let mut child_count = 0usize;
    for_each_child_window(main, |hwnd, title| {
        if !title.is_empty() {
            tracing::info!(
                hwnd = ?(hwnd as *const c_void),
                title = %title,
                scope = "child-of-main",
                "REAPER window"
            );
            child_count += 1;
        }
        true
    });
    tracing::info!(top_count, child_count, "Window enumeration complete");
}

/// Diagnostic: scan REAPER command IDs in the toolbar action range and
/// log the human-readable name for each one. Finds the actual
/// "Toolbar: Open/close floating toolbar N" command IDs on the user's
/// REAPER build without us having to hardcode/guess them.
pub fn debug_log_toolbar_command_names() {
    use reaper_medium::SectionContext;

    let medium = HighReaper::get().medium_reaper();
    // First-8 toolbar open/close actions cluster around 41679..41686;
    // toolbars 9-32 are elsewhere in REAPER 7's action space. Scan
    // wider and log anything whose name contains "toolbar".
    let mut hits = 0usize;
    for cmd_id in 41600u32..43500u32 {
        let name: Option<String> = unsafe {
            medium.kbd_get_text_from_cmd(
                reaper_medium::CommandId::new(cmd_id),
                SectionContext::MainSection,
                |cstr| {
                    cstr.as_c_str()
                        .to_str()
                        .unwrap_or("(invalid utf-8)")
                        .to_string()
                },
            )
        };
        if let Some(name) = name
            && name.to_lowercase().contains("toolbar")
        {
            tracing::info!(command_id = cmd_id, name = %name, "Toolbar action");
            hits += 1;
        }
    }
    tracing::info!(hits, "Toolbar command-id probe complete");
}

pub fn debug_dump_toolbar_states() {
    let hwnds = toolbar_hwnds();
    let mut shown = 0usize;
    for (title, hwnd) in &hwnds {
        if !is_mode_toolbar_title(title) {
            continue;
        }
        let dock = current_dock_state(*hwnd);
        let visible = is_window_visible(*hwnd);
        tracing::info!(
            title = %title,
            hwnd = ?(*hwnd as *const c_void),
            visible,
            dock = ?dock,
            "Toolbar state"
        );
        shown += 1;
    }
    tracing::info!(toolbars = shown, "Toolbar state probe complete");
}

// ─── WindowManager service impl (stubbed) ───────────────────────────────────
//
// The persistent storage + apply/save algorithms land in follow-up tasks
// (#10, #11, #12). Returning explicit "not yet implemented" errors keeps
// the trait satisfied without pretending broken behaviour is success.

/// Last layout applied this process lifetime. REAPER doesn't expose
/// "current layout" through its API, so this is our memory of it.
static CURRENT_LAYOUT_NAME: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

fn record_current_layout(name: &str) {
    if let Ok(mut guard) = CURRENT_LAYOUT_NAME.lock() {
        *guard = Some(name.to_string());
    }
}

impl WindowManager for crate::Reaper {
    fn apply_layout(&self, name: String, _options: WindowLayoutOptions) -> WindowLayoutResult {
        // Layouts are derived from the renaming convention: mode at
        // index N owns toolbar slots `(N*3 + 1)..=(N*3 + 3)`. No disk
        // lookup, no save flow — the mode name fully determines the
        // toolbar set.
        let Some(mode_idx) = mode_index_for_layout_name(&name) else {
            return WindowLayoutResult::error(format!(
                "no mode named '{name}' (expected Organize/Write/Produce/Record/Edit/Mix/Master/Live)"
            ));
        };
        let docker_layout = load_mode_docker_layout();
        let mode_first_slot = mode_idx * 3 + 1;
        let want_slots: [u32; 3] = [mode_first_slot, mode_first_slot + 1, mode_first_slot + 2];

        let mut shown = 0usize;
        let mut hidden = 0usize;
        let mut errored = 0usize;
        for (slot, title) in all_mode_toolbar_titles() {
            let want_visible = want_slots.contains(&slot);
            match set_toolbar_slot_visible(slot, want_visible) {
                Ok(()) => {
                    if want_visible {
                        shown += 1;
                    } else {
                        hidden += 1;
                        continue;
                    }
                }
                Err(err) => {
                    tracing::warn!(slot, error = %err, "toolbar visibility toggle failed");
                    errored += 1;
                    continue;
                }
            }

            // Slot-in-mode is 1..=3 (top/left/right). Find the toolbar's
            // HWND (which only exists after the toggle action above
            // showed it) and force-dock it to the configured docker.
            let slot_in_mode = slot - mode_first_slot + 1;
            let Some(docker_id) = docker_id_for_slot_position(slot_in_mode, &docker_layout) else {
                continue;
            };
            let hwnds = toolbar_hwnds();
            let Some(&hwnd) = hwnds.get(&title) else {
                tracing::warn!(
                    title = %title,
                    "shown toolbar's HWND not found after toggle — skipping dock"
                );
                continue;
            };
            dock_window_to(hwnd, &title, docker_id);
        }
        tracing::info!(
            layout = %name,
            top_docker = docker_layout.top,
            left_docker = docker_layout.left,
            right_docker = docker_layout.right,
            shown,
            hidden,
            errored,
            "WindowManager: layout applied"
        );

        record_current_layout(&name);
        WindowLayoutResult::ok(name)
    }

    fn list_layouts(&self) -> Vec<WindowLayoutSummary> {
        list_layouts_from_disk()
    }

    fn current_layout(&self) -> Option<WindowLayoutSummary> {
        let name = CURRENT_LAYOUT_NAME.lock().ok()?.clone()?;
        let layout = load_layout_from_disk(&name)?;
        Some(summary_for(&layout))
    }

    fn get_layout(&self, name: String) -> Option<WindowLayout> {
        load_layout_from_disk(&name)
    }

    fn save_layout(&self, mut layout: WindowLayout) -> WindowLayoutResult {
        use daw_proto::window_manager::{LayoutPlacement, LayoutToolbar, MonitorRect};

        if layout.name.is_empty() {
            return WindowLayoutResult::error("layout name cannot be empty");
        }

        // Snapshot the current state of every mode toolbar we can find.
        // Toolbars not present in the enumeration (e.g. closed) are
        // omitted — `apply` treats absent entries as "leave untouched"
        // so we don't accidentally hide things that weren't part of the
        // capture.
        let hwnds = toolbar_hwnds();
        let (screen_w, screen_h) = primary_screen_size();
        let screen_w_f = screen_w as f32;
        let screen_h_f = screen_h as f32;
        let mut toolbars: Vec<LayoutToolbar> = Vec::new();
        for (title, hwnd) in &hwnds {
            if !is_mode_toolbar_title(title) {
                continue;
            }
            let visible = is_window_visible(*hwnd);
            let placement = if !visible {
                LayoutPlacement::Hidden
            } else {
                match current_dock_state(*hwnd) {
                    Some(DockState::Docked { docker_id }) => LayoutPlacement::Docked { docker_id },
                    Some(DockState::FloatingDocker { docker_id }) => {
                        LayoutPlacement::FloatingDocker { docker_id }
                    }
                    Some(DockState::Floating) | None => {
                        let rect = window_rect(*hwnd).unwrap_or(raw::RECT {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        });
                        let w = (rect.right - rect.left).max(0) as f32;
                        let h = (rect.bottom - rect.top).max(0) as f32;
                        LayoutPlacement::Floating {
                            rect: MonitorRect {
                                x_frac: rect.left as f32 / screen_w_f,
                                y_frac: rect.top as f32 / screen_h_f,
                                w_frac: w / screen_w_f,
                                h_frac: h / screen_h_f,
                            },
                        }
                    }
                }
            };
            toolbars.push(LayoutToolbar {
                toolbar_name: title.clone(),
                placement,
            });
        }
        toolbars.sort_by(|a, b| a.toolbar_name.cmp(&b.toolbar_name));
        layout.toolbars = toolbars;

        match write_layout_to_disk(&layout) {
            Ok(()) => {
                tracing::info!(
                    name = %layout.name,
                    toolbars = layout.toolbars.len(),
                    "WindowManager: layout saved"
                );
                WindowLayoutResult::ok(layout.name)
            }
            Err(err) => WindowLayoutResult::error(format!("write layout: {err}")),
        }
    }

    fn delete_layout(&self, name: String) -> WindowLayoutResult {
        let Some(path) = layout_path(&name) else {
            return WindowLayoutResult::error(format!(
                "layout name '{name}' invalid (alnum/space/-/_ only)"
            ));
        };
        match std::fs::remove_file(&path) {
            Ok(()) => WindowLayoutResult::ok(name),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                WindowLayoutResult::error(format!("layout '{name}' does not exist"))
            }
            Err(e) => WindowLayoutResult::error(format!("delete failed: {e}")),
        }
    }
}
