//! REAPER Toolbar Service Implementation
//!
//! Manages dynamic toolbar buttons using REAPER's GetCustomMenuOrToolbarItem /
//! AddCustomMenuOrToolbarItem / DeleteCustomMenuOrToolbarItem API.
//!
//! Operations are deferred and applied from the timer callback to avoid
//! re-entrancy issues inside REAPER callbacks.

use daw_proto::toolbar::{
    ToolbarButton, ToolbarIcon, ToolbarItemInfo, ToolbarPlacement, ToolbarResult, ToolbarService,
    ToolbarSnapshot, ToolbarSnapshotSource, ToolbarTarget, TrackedButton,
};
use reaper_high::Reaper;
use reaper_medium::{CommandId, MenuOrToolbarItem, PositionDescriptor, UiRefreshBehavior};
use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;
use tracing::{debug, info, warn};

/// REAPER implementation of the ToolbarService.
#[derive(Clone)]
pub struct ReaperToolbar {
    // State is shared via statics because toolbar operations must be deferred
    // to the timer callback (main thread).
}

impl ReaperToolbar {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for ReaperToolbar {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Deferred operation queue
// ============================================================================

#[derive(Debug, Clone)]
enum DeferredOp {
    Add {
        button: ToolbarButton,
        workflow_id: String,
    },
    Remove {
        target: ToolbarTarget,
        command_name: String,
    },
    Update {
        button: ToolbarButton,
        workflow_id: String,
    },
    Move {
        target: ToolbarTarget,
        command_name: String,
        position: u32,
    },
    SetIcon {
        target: ToolbarTarget,
        command_name: String,
        icon: Option<ToolbarIcon>,
    },
    RemoveWorkflow {
        workflow_id: String,
    },
}

#[derive(Default)]
struct ToolbarState {
    added_buttons: HashMap<(String, String), String>,
}

static STATE: std::sync::OnceLock<Mutex<ToolbarState>> = std::sync::OnceLock::new();
static QUEUE: std::sync::OnceLock<Mutex<VecDeque<DeferredOp>>> = std::sync::OnceLock::new();

fn state() -> &'static Mutex<ToolbarState> {
    STATE.get_or_init(|| Mutex::new(ToolbarState::default()))
}

fn queue() -> &'static Mutex<VecDeque<DeferredOp>> {
    QUEUE.get_or_init(|| Mutex::new(VecDeque::new()))
}

fn enqueue(op: DeferredOp) {
    if let Ok(mut q) = queue().lock() {
        q.push_back(op);
    }
}

// ============================================================================
// Public API for timer callback
// ============================================================================

/// Process all deferred toolbar operations. Call from the timer callback.
pub fn process_deferred_ops() {
    let ops: Vec<DeferredOp> = match queue().lock() {
        Ok(mut q) => q.drain(..).collect(),
        Err(_) => return,
    };

    for op in ops {
        let result = match op {
            DeferredOp::Add {
                button,
                workflow_id,
            } => add_button_immediate(&button, &workflow_id).map(|_| ()),
            DeferredOp::Remove {
                target,
                command_name,
            } => remove_button_immediate(&target, &command_name),
            DeferredOp::Update {
                button,
                workflow_id,
            } => update_button_immediate(&button, &workflow_id).map(|_| ()),
            DeferredOp::Move {
                target,
                command_name,
                position,
            } => move_button_immediate(&target, &command_name, position).map(|_| ()),
            DeferredOp::SetIcon {
                target,
                command_name,
                icon,
            } => set_button_icon_immediate(&target, &command_name, icon).map(|_| ()),
            DeferredOp::RemoveWorkflow { workflow_id } => {
                remove_workflow_buttons_immediate(&workflow_id)
            }
        };

        if let Err(error) = result {
            warn!(%error, "deferred toolbar operation failed");
        }
    }
}

// ============================================================================
// ToolbarTarget helpers
// ============================================================================

fn target_to_str(target: &ToolbarTarget) -> String {
    match target {
        ToolbarTarget::Main => "Main toolbar".to_string(),
        ToolbarTarget::Floating(n) => format!("Floating toolbar {}", (*n).clamp(1, 32)),
    }
}

fn target_from_str(value: &str) -> ToolbarTarget {
    if value == "Main toolbar" {
        return ToolbarTarget::Main;
    }
    if let Some(num) = value.strip_prefix("Floating toolbar ") {
        if let Ok(n) = num.parse::<u8>() {
            if (1..=32).contains(&n) {
                return ToolbarTarget::Floating(n);
            }
        }
    }
    ToolbarTarget::Main
}

fn toolbar_targets() -> impl Iterator<Item = ToolbarTarget> {
    std::iter::once(ToolbarTarget::Main).chain((1..=32).map(ToolbarTarget::Floating))
}

// ============================================================================
// Immediate operations (run on main thread from timer callback)
// ============================================================================

fn is_api_available() -> bool {
    Reaper::get()
        .medium_reaper()
        .low()
        .pointers()
        .GetCustomMenuOrToolbarItem
        .is_some()
}

fn resolve_command_id(command_name: &str) -> Result<CommandId, String> {
    Reaper::get()
        .action_by_command_name(command_name)
        .command_id()
        .map_err(|e| format!("Command not found: {command_name} - {e}"))
}

fn position_descriptor(placement: &ToolbarPlacement) -> PositionDescriptor {
    match placement {
        ToolbarPlacement::Append => PositionDescriptor::Append,
        ToolbarPlacement::Position(position) => PositionDescriptor::AtPos(*position),
    }
}

fn icon_path(icon: Option<&ToolbarIcon>) -> Option<&camino::Utf8Path> {
    icon.map(|icon| camino::Utf8Path::new(icon.value.as_str()))
}

fn add_toolbar_command_item(
    toolbar_name: &str,
    position: PositionDescriptor,
    command_id: CommandId,
    flags: u32,
    label: &str,
    icon: Option<&ToolbarIcon>,
    refresh: UiRefreshBehavior,
) -> Result<(), String> {
    Reaper::get()
        .medium_reaper()
        .add_custom_menu_or_toolbar_item_command(
            toolbar_name,
            position,
            command_id,
            flags,
            label,
            icon_path(icon),
            refresh,
        )
        .map_err(|e| format!("Failed to add toolbar item: {e}"))
}

fn add_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let command_id = resolve_command_id(&button.command_name)?;
    let toolbar_name = target_to_str(&button.target);

    if scan_toolbar_for_command(&toolbar_name, command_id).is_none() {
        add_toolbar_command_item(
            toolbar_name.as_str(),
            position_descriptor(&button.placement),
            command_id,
            button.flags,
            button.label.as_str(),
            button.icon.as_ref(),
            UiRefreshBehavior::Refresh,
        )?;

        info!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            "added toolbar button"
        );
    }

    if let Ok(mut s) = state().lock() {
        s.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    }

    Ok(command_id)
}

fn update_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let command_id = resolve_command_id(&button.command_name)?;
    let toolbar_name = target_to_str(&button.target);

    if let Some(position) = scan_toolbar_for_command(&toolbar_name, command_id) {
        let medium = Reaper::get().medium_reaper();
        medium
            .delete_custom_menu_or_toolbar_item(
                toolbar_name.as_str(),
                position,
                UiRefreshBehavior::NoRefresh,
            )
            .map_err(|e| format!("Failed to remove toolbar item: {e}"))?;

        let placement = match button.placement {
            ToolbarPlacement::Append => PositionDescriptor::AtPos(position),
            ToolbarPlacement::Position(position) => PositionDescriptor::AtPos(position),
        };
        add_toolbar_command_item(
            toolbar_name.as_str(),
            placement,
            command_id,
            button.flags,
            button.label.as_str(),
            button.icon.as_ref(),
            UiRefreshBehavior::Refresh,
        )?;

        debug!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            position,
            "updated toolbar button"
        );
    } else {
        return add_button_immediate(button, workflow_id);
    }

    if let Ok(mut s) = state().lock() {
        s.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    }

    Ok(command_id)
}

struct ToolbarCommandDetails {
    position: u32,
    command_id: CommandId,
    flags: u32,
    label: String,
    icon: Option<ToolbarIcon>,
}

fn get_toolbar_command_details(
    toolbar_name: &str,
    command_id: CommandId,
) -> Option<ToolbarCommandDetails> {
    let medium = Reaper::get().medium_reaper();
    let mut pos = 0;

    loop {
        let result =
            medium.get_custom_menu_or_toolbar_item(toolbar_name, pos, |item| match item? {
                MenuOrToolbarItem::Command(cmd) if cmd.command_id == command_id => {
                    Some(Some(ToolbarCommandDetails {
                        position: pos,
                        command_id: cmd.command_id,
                        flags: cmd.toolbar_flags,
                        label: cmd.label.to_string(),
                        icon: cmd.icon_file_name.map(|value| ToolbarIcon {
                            value: value.to_string(),
                            ..Default::default()
                        }),
                    }))
                }
                _ => Some(None),
            });

        match result {
            Some(Some(details)) => return Some(details),
            Some(None) => pos += 1,
            None => return None,
        }
    }
}

fn move_button_immediate(
    target: &ToolbarTarget,
    command_name: &str,
    position: u32,
) -> Result<CommandId, String> {
    let command_id = resolve_command_id(command_name)?;
    let toolbar_name = target_to_str(target);
    let details = get_toolbar_command_details(toolbar_name.as_str(), command_id)
        .ok_or_else(|| format!("Toolbar button not found: {command_name}"))?;
    let medium = Reaper::get().medium_reaper();
    medium
        .delete_custom_menu_or_toolbar_item(
            toolbar_name.as_str(),
            details.position,
            UiRefreshBehavior::NoRefresh,
        )
        .map_err(|e| format!("Failed to remove toolbar item: {e}"))?;
    add_toolbar_command_item(
        toolbar_name.as_str(),
        PositionDescriptor::AtPos(position),
        details.command_id,
        details.flags,
        details.label.as_str(),
        details.icon.as_ref(),
        UiRefreshBehavior::Refresh,
    )?;
    Ok(command_id)
}

fn set_button_icon_immediate(
    target: &ToolbarTarget,
    command_name: &str,
    icon: Option<ToolbarIcon>,
) -> Result<CommandId, String> {
    let command_id = resolve_command_id(command_name)?;
    let toolbar_name = target_to_str(target);
    let details = get_toolbar_command_details(toolbar_name.as_str(), command_id)
        .ok_or_else(|| format!("Toolbar button not found: {command_name}"))?;
    let medium = Reaper::get().medium_reaper();
    medium
        .delete_custom_menu_or_toolbar_item(
            toolbar_name.as_str(),
            details.position,
            UiRefreshBehavior::NoRefresh,
        )
        .map_err(|e| format!("Failed to remove toolbar item: {e}"))?;
    add_toolbar_command_item(
        toolbar_name.as_str(),
        PositionDescriptor::AtPos(details.position),
        details.command_id,
        details.flags,
        details.label.as_str(),
        icon.as_ref(),
        UiRefreshBehavior::Refresh,
    )?;
    Ok(command_id)
}

fn remove_button_immediate(target: &ToolbarTarget, command_name: &str) -> Result<(), String> {
    let command_id = resolve_command_id(command_name)?;
    let toolbar_name = target_to_str(target);

    if let Some(position) = scan_toolbar_for_command(&toolbar_name, command_id) {
        Reaper::get()
            .medium_reaper()
            .delete_custom_menu_or_toolbar_item(
                toolbar_name.as_str(),
                position,
                UiRefreshBehavior::Refresh,
            )
            .map_err(|e| format!("Failed to delete toolbar item: {e}"))?;

        info!(
            command = %command_name,
            toolbar = %toolbar_name,
            position,
            "removed toolbar button"
        );
    }

    if let Ok(mut s) = state().lock() {
        s.added_buttons
            .remove(&(toolbar_name, command_name.to_string()));
    }

    Ok(())
}

fn remove_workflow_buttons_immediate(workflow_id: &str) -> Result<(), String> {
    let buttons = state()
        .lock()
        .ok()
        .map(|s| {
            s.added_buttons
                .iter()
                .filter(|(_, owner)| owner.as_str() == workflow_id)
                .map(|((toolbar_name, command_name), _)| {
                    (toolbar_name.clone(), command_name.clone())
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    for (toolbar_name, command_name) in buttons {
        let target = target_from_str(&toolbar_name);
        remove_button_immediate(&target, &command_name)?;
    }

    Ok(())
}

fn scan_toolbar_for_command(toolbar_name: &str, command_id: CommandId) -> Option<u32> {
    let medium = Reaper::get().medium_reaper();
    let mut pos = 0;

    loop {
        let result =
            medium.get_custom_menu_or_toolbar_item(toolbar_name, pos, |item| match item? {
                MenuOrToolbarItem::Command(cmd) if cmd.command_id == command_id => Some(Some(pos)),
                _ => Some(None),
            });

        match result {
            Some(Some(found)) => return Some(found),
            Some(None) => pos += 1,
            None => return None,
        }
    }
}

fn command_name_for(command_id: CommandId) -> Option<String> {
    Reaper::get()
        .medium_reaper()
        .reverse_named_command_lookup(command_id, |name| format!("_{name}"))
}

fn snapshot_live_toolbar(target: ToolbarTarget) -> ToolbarSnapshot {
    let toolbar_name = target_to_str(&target);
    let medium = Reaper::get().medium_reaper();
    let mut items = Vec::new();
    let mut pos = 0;

    loop {
        let item = medium.get_custom_menu_or_toolbar_item(toolbar_name.as_str(), pos, |item| {
            item.map(|item| match item {
                MenuOrToolbarItem::Separator => ToolbarItemInfo {
                    position: pos,
                    kind: "separator".to_string(),
                    raw: Some("-1".to_string()),
                    ..Default::default()
                },
                MenuOrToolbarItem::SubMenuStart(submenu) => ToolbarItemInfo {
                    position: pos,
                    kind: "submenu-start".to_string(),
                    label: submenu.label.to_string(),
                    raw: Some("-2".to_string()),
                    ..Default::default()
                },
                MenuOrToolbarItem::SubMenuEnd => ToolbarItemInfo {
                    position: pos,
                    kind: "submenu-end".to_string(),
                    raw: Some("-3".to_string()),
                    ..Default::default()
                },
                MenuOrToolbarItem::Command(command) => {
                    let command_id = command.command_id;
                    ToolbarItemInfo {
                        position: pos,
                        kind: "command".to_string(),
                        command_id: Some(command_id.get()),
                        command_name: command_name_for(command_id),
                        label: command.label.to_string(),
                        flags: command.toolbar_flags,
                        icon: command.icon_file_name.map(|name| name.to_string()),
                        raw: None,
                    }
                }
            })
        });

        match item {
            Some(item) => {
                items.push(item);
                pos += 1;
            }
            None => break,
        }
    }

    ToolbarSnapshot {
        toolbar_name,
        source: ToolbarSnapshotSource::Live,
        items,
    }
}

// ============================================================================
// ToolbarService trait implementation
// ============================================================================

impl ToolbarService for ReaperToolbar {
    async fn add_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult {
        debug!(command = %button.command_name, "toolbar add_button service call");
        if !is_api_available() {
            return ToolbarResult::error("Dynamic toolbar API not available");
        }
        enqueue(DeferredOp::Add {
            button,
            workflow_id,
        });
        ToolbarResult::ok(0)
    }

    async fn update_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult {
        if !is_api_available() {
            return ToolbarResult::error("Dynamic toolbar API not available");
        }
        enqueue(DeferredOp::Update {
            button,
            workflow_id,
        });
        ToolbarResult::ok(0)
    }

    async fn remove_button(&self, target: ToolbarTarget, command_name: String) -> ToolbarResult {
        if !is_api_available() {
            return ToolbarResult::ok(0);
        }
        enqueue(DeferredOp::Remove {
            target,
            command_name,
        });
        ToolbarResult::ok(0)
    }

    async fn move_button(
        &self,
        target: ToolbarTarget,
        command_name: String,
        position: u32,
    ) -> ToolbarResult {
        debug!(%command_name, position, "toolbar move_button service call");
        if !is_api_available() {
            return ToolbarResult::error("Dynamic toolbar API not available");
        }
        enqueue(DeferredOp::Move {
            target,
            command_name,
            position,
        });
        ToolbarResult::ok(0)
    }

    async fn set_button_icon(
        &self,
        target: ToolbarTarget,
        command_name: String,
        icon: Option<ToolbarIcon>,
    ) -> ToolbarResult {
        if !is_api_available() {
            return ToolbarResult::error("Dynamic toolbar API not available");
        }
        enqueue(DeferredOp::SetIcon {
            target,
            command_name,
            icon,
        });
        ToolbarResult::ok(0)
    }

    async fn remove_workflow_buttons(&self, workflow_id: String) -> ToolbarResult {
        if !is_api_available() {
            return ToolbarResult::ok(0);
        }
        enqueue(DeferredOp::RemoveWorkflow { workflow_id });
        ToolbarResult::ok(0)
    }

    async fn is_available(&self) -> bool {
        is_api_available()
    }

    async fn get_tracked_buttons(&self) -> Vec<TrackedButton> {
        crate::main_thread::query(|| {
            toolbar_targets()
                .map(snapshot_live_toolbar)
                .flat_map(|snapshot| {
                    let toolbar_name = snapshot.toolbar_name;
                    snapshot.items.into_iter().map(move |item| TrackedButton {
                        toolbar_name: toolbar_name.clone(),
                        command_name: facet_json::to_string(&item).unwrap_or_default(),
                        workflow_id: "__fts_live_toolbar_item".to_string(),
                    })
                })
                .collect()
        })
        .await
        .unwrap_or_default()
    }
}
