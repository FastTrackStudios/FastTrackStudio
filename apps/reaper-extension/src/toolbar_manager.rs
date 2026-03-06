//! Dynamic toolbar manager for REAPER extension workflows.
//!
//! Operations are queued and applied from the timer callback to avoid
//! re-entrancy issues inside REAPER callbacks.

use reaper_high::Reaper;
use reaper_medium::{CommandId, MenuOrToolbarItem, PositionDescriptor, UiRefreshBehavior};
use std::collections::{HashMap, VecDeque};
use std::sync::{Mutex, OnceLock};
use tracing::{debug, info, warn};

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
    RemoveWorkflow {
        workflow_id: String,
    },
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum ToolbarTarget {
    #[default]
    Main,
    Floating(u8),
}

impl ToolbarTarget {
    pub fn as_str(&self) -> String {
        match self {
            Self::Main => "Main toolbar".to_string(),
            Self::Floating(n) => format!("Floating toolbar {}", (*n).clamp(1, 32)),
        }
    }

    pub fn from_str(value: &str) -> Option<Self> {
        if value == "Main toolbar" {
            return Some(Self::Main);
        }

        if let Some(num) = value.strip_prefix("Floating toolbar ") {
            let n = num.parse::<u8>().ok()?;
            if (1..=32).contains(&n) {
                return Some(Self::Floating(n));
            }
        }

        None
    }
}

pub mod flags {
    pub const NORMAL: u32 = 0;
    pub const TEXT_ICON: u32 = 1 << 0;
    pub const DOUBLE_WIDE: u32 = 1 << 1;
}

pub mod icons {
    pub const TEXT: &str = "text";
    pub const TEXT_WIDE: &str = "text_wide";
}

#[derive(Debug, Clone)]
pub struct ToolbarButton {
    pub command_name: String,
    pub label: String,
    pub icon: Option<String>,
    pub target: ToolbarTarget,
    pub toolbar_flags: u32,
}

impl ToolbarButton {
    pub fn new(command_name: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            command_name: command_name.into(),
            label: label.into(),
            icon: None,
            target: ToolbarTarget::Main,
            toolbar_flags: flags::NORMAL,
        }
    }

    pub fn on_toolbar(mut self, target: ToolbarTarget) -> Self {
        self.target = target;
        self
    }

    pub fn with_icon(mut self, icon: impl Into<String>) -> Self {
        self.icon = Some(icon.into());
        self
    }

    pub fn with_flags(mut self, flags: u32) -> Self {
        self.toolbar_flags = flags;
        self
    }

    pub fn text_icon(mut self) -> Self {
        self.icon = Some(icons::TEXT.to_string());
        self.toolbar_flags |= flags::TEXT_ICON;
        self
    }

    pub fn double_wide(mut self) -> Self {
        self.icon = Some(icons::TEXT_WIDE.to_string());
        self.toolbar_flags |= flags::DOUBLE_WIDE;
        self
    }
}

#[derive(Default)]
struct ToolbarState {
    added_buttons: HashMap<(String, String), String>,
}

static STATE: OnceLock<Mutex<ToolbarState>> = OnceLock::new();
static QUEUE: OnceLock<Mutex<VecDeque<DeferredOp>>> = OnceLock::new();

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

pub fn is_available() -> bool {
    Reaper::get()
        .medium_reaper()
        .low()
        .pointers()
        .GetCustomMenuOrToolbarItem
        .is_some()
}

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
            DeferredOp::RemoveWorkflow { workflow_id } => {
                remove_workflow_buttons_immediate(&workflow_id)
            }
        };

        if let Err(error) = result {
            warn!(%error, "deferred toolbar operation failed");
        }
    }
}

pub fn add_button(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available".to_string());
    }

    let command_id = resolve_command_id(&button.command_name)?;

    enqueue(DeferredOp::Add {
        button: button.clone(),
        workflow_id: workflow_id.to_string(),
    });

    Ok(command_id)
}

pub fn update_button(button: &ToolbarButton, workflow_id: &str) -> Result<(), String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available".to_string());
    }

    enqueue(DeferredOp::Update {
        button: button.clone(),
        workflow_id: workflow_id.to_string(),
    });

    Ok(())
}

pub fn remove_button(target: &ToolbarTarget, command_name: &str) -> Result<(), String> {
    if !is_available() {
        return Ok(());
    }

    enqueue(DeferredOp::Remove {
        target: target.clone(),
        command_name: command_name.to_string(),
    });

    Ok(())
}

pub fn remove_workflow_buttons(workflow_id: &str) -> Result<(), String> {
    if !is_available() {
        return Ok(());
    }

    enqueue(DeferredOp::RemoveWorkflow {
        workflow_id: workflow_id.to_string(),
    });

    Ok(())
}

pub fn get_tracked_buttons() -> Vec<(String, String, String)> {
    state()
        .lock()
        .ok()
        .map(|s| {
            s.added_buttons
                .iter()
                .map(|((toolbar, command), workflow)| {
                    (toolbar.clone(), command.clone(), workflow.clone())
                })
                .collect()
        })
        .unwrap_or_default()
}

fn resolve_command_id(command_name: &str) -> Result<CommandId, String> {
    Reaper::get()
        .action_by_command_name(command_name)
        .command_id()
        .map_err(|e| format!("Command not found: {command_name} - {e}"))
}

fn add_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let command_id = resolve_command_id(&button.command_name)?;
    let toolbar_name = button.target.as_str();

    if scan_toolbar_for_command(&toolbar_name, command_id).is_none() {
        let icon_path = button.icon.as_deref().map(camino::Utf8Path::new);
        Reaper::get()
            .medium_reaper()
            .add_custom_menu_or_toolbar_item_command(
                toolbar_name.as_str(),
                PositionDescriptor::Append,
                command_id,
                button.toolbar_flags,
                button.label.as_str(),
                icon_path,
                UiRefreshBehavior::Refresh,
            )
            .map_err(|e| format!("Failed to add toolbar item: {e}"))?;

        info!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            "added toolbar button"
        );
    }

    if let Ok(mut state) = state().lock() {
        state.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    }

    Ok(command_id)
}

fn update_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let command_id = resolve_command_id(&button.command_name)?;
    let toolbar_name = button.target.as_str();

    if let Some(position) = scan_toolbar_for_command(&toolbar_name, command_id) {
        let medium = Reaper::get().medium_reaper();
        medium
            .delete_custom_menu_or_toolbar_item(
                toolbar_name.as_str(),
                position,
                UiRefreshBehavior::NoRefresh,
            )
            .map_err(|e| format!("Failed to remove toolbar item: {e}"))?;

        let icon_path = button.icon.as_deref().map(camino::Utf8Path::new);
        medium
            .add_custom_menu_or_toolbar_item_command(
                toolbar_name.as_str(),
                PositionDescriptor::AtPos(position),
                command_id,
                button.toolbar_flags,
                button.label.as_str(),
                icon_path,
                UiRefreshBehavior::Refresh,
            )
            .map_err(|e| format!("Failed to re-add toolbar item: {e}"))?;

        debug!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            position,
            "updated toolbar button"
        );
    } else {
        return add_button_immediate(button, workflow_id);
    }

    if let Ok(mut state) = state().lock() {
        state.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    }

    Ok(command_id)
}

fn remove_button_immediate(target: &ToolbarTarget, command_name: &str) -> Result<(), String> {
    let command_id = resolve_command_id(command_name)?;
    let toolbar_name = target.as_str();

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

    if let Ok(mut state) = state().lock() {
        state
            .added_buttons
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
        let target = ToolbarTarget::from_str(&toolbar_name).unwrap_or_default();
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
