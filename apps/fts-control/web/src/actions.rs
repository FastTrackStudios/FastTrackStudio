//! Actions module for the web app
//!
//! Provides:
//! - Local standalone actions (web-specific functionality)
//! - Integration with remote hosts (REAPER instances) via ActionsServiceClient
//! - Aggregated action list for command palette

#![cfg(target_arch = "wasm32")]

use actions_proto::{
    ActionCategory, ActionDefinition, ActionId, ActionResult, ActionsServiceClient,
};
use actions_standalone::StandaloneActions;

use dioxus::prelude::*;
use roam::ErasedCaller;
use std::sync::Arc;

/// Represents a host that provides actions
#[derive(Clone, Debug, PartialEq)]
pub enum ActionSource {
    /// Local standalone actions (web app specific)
    Standalone,
    /// Remote host (e.g., REAPER extension)
    Remote { name: String, address: String },
}

impl ActionSource {
    pub fn display_name(&self) -> String {
        match self {
            Self::Standalone => "Standalone".to_string(),
            Self::Remote { name, .. } => name.clone(),
        }
    }
}

/// An action with its source information
#[derive(Clone)]
pub struct SourcedAction {
    pub definition: ActionDefinition,
    pub source: ActionSource,
}

/// Manager for all action sources (standalone + remote hosts)
pub struct ActionManager {
    /// Local standalone actions
    standalone: Arc<StandaloneActions>,
    /// Connected remote hosts
    remote_hosts: Vec<RemoteHost>,
}

struct RemoteHost {
    source: ActionSource,
    client: ActionsServiceClient,
    cached_actions: Vec<ActionDefinition>,
}

impl ActionManager {
    /// Create a new action manager with standalone actions initialized
    pub async fn new() -> Self {
        let standalone = StandaloneActions::new();

        // Register web-specific standalone actions
        register_web_actions(&standalone).await;

        Self {
            standalone,
            remote_hosts: Vec::new(),
        }
    }

    /// Add a remote host connection
    pub async fn add_remote_host(&mut self, name: String, address: String, handle: ErasedCaller) {
        let client = ActionsServiceClient::new(handle);

        // Query actions from the remote host
        let cached_actions = match client.get_all_actions().await {
            Ok(actions) => {
                tracing::info!(host = %name, count = actions.len(), "Got actions from remote host");
                actions
            }
            Err(e) => {
                tracing::warn!(host = %name, error = ?e, "Failed to get actions from remote host");
                Vec::new()
            }
        };

        self.remote_hosts.push(RemoteHost {
            source: ActionSource::Remote { name, address },
            client,
            cached_actions,
        });
    }

    /// Remove a remote host
    pub fn remove_remote_host(&mut self, address: &str) {
        self.remote_hosts.retain(|h| {
            if let ActionSource::Remote { address: addr, .. } = &h.source {
                addr != address
            } else {
                true
            }
        });
    }

    /// Get all available sources
    pub fn sources(&self) -> Vec<ActionSource> {
        let mut sources = vec![ActionSource::Standalone];
        for host in &self.remote_hosts {
            sources.push(host.source.clone());
        }
        sources
    }

    /// Get all actions from a specific source
    pub async fn get_actions_for_source(&self, source: &ActionSource) -> Vec<ActionDefinition> {
        match source {
            ActionSource::Standalone => self.standalone.get_all_actions().await,
            ActionSource::Remote { address, .. } => {
                for host in &self.remote_hosts {
                    if let ActionSource::Remote { address: addr, .. } = &host.source {
                        if addr == address {
                            return host.cached_actions.clone();
                        }
                    }
                }
                Vec::new()
            }
        }
    }

    /// Get all actions from all sources
    pub async fn get_all_actions(&self) -> Vec<SourcedAction> {
        let mut actions = Vec::new();

        // Standalone actions
        for def in self.standalone.get_all_actions().await {
            actions.push(SourcedAction {
                definition: def,
                source: ActionSource::Standalone,
            });
        }

        // Remote host actions
        for host in &self.remote_hosts {
            for def in &host.cached_actions {
                actions.push(SourcedAction {
                    definition: def.clone(),
                    source: host.source.clone(),
                });
            }
        }

        actions
    }

    /// Execute an action
    pub async fn execute(&self, action_id: &ActionId, source: &ActionSource) -> ActionResult {
        match source {
            ActionSource::Standalone => self.standalone.execute(action_id.clone()).await,
            ActionSource::Remote { address, .. } => {
                for host in &self.remote_hosts {
                    if let ActionSource::Remote { address: addr, .. } = &host.source {
                        if addr == address {
                            return match host.client.execute(action_id.clone()).await {
                                Ok(result) => result,
                                Err(e) => ActionResult::failure(format!("RPC error: {e:?}")),
                            };
                        }
                    }
                }
                ActionResult::failure("Host not found")
            }
        }
    }

    /// Refresh actions from a remote host
    pub async fn refresh_remote_host(&mut self, address: &str) {
        for host in &mut self.remote_hosts {
            if let ActionSource::Remote { address: addr, .. } = &host.source {
                if addr == address {
                    match host.client.get_all_actions().await {
                        Ok(actions) => {
                            host.cached_actions = actions;
                        }
                        Err(e) => {
                            tracing::warn!(address = %addr, error = ?e, "Failed to refresh actions");
                        }
                    }
                    break;
                }
            }
        }
    }
}

/// Register web-specific standalone actions
async fn register_web_actions(standalone: &StandaloneActions) {
    // Toggle dark mode
    standalone
        .register(
            ActionDefinition::new(
                "fts.web.toggle_dark_mode",
                "Toggle Dark Mode",
                "Switch between light and dark themes",
            )
            .with_category(ActionCategory::View)
            .with_shortcut("Cmd+Shift+D"),
            |_id| {
                tracing::info!("Toggle dark mode action triggered");
                ActionResult::success_with_message("Dark mode toggled")
            },
        )
        .await;

    // Open command palette (meta action)
    standalone
        .register(
            ActionDefinition::new(
                "fts.web.command_palette",
                "Command Palette",
                "Open the command palette",
            )
            .with_category(ActionCategory::General)
            .with_shortcut("Cmd+Shift+P"),
            |_id| ActionResult::success(),
        )
        .await;

    // Reconnect to gateway
    standalone
        .register(
            ActionDefinition::new("fts.web.reconnect", "Reconnect", "Reconnect to the gateway")
                .with_category(ActionCategory::General),
            |_id| {
                tracing::info!("Reconnect action triggered");
                ActionResult::success_with_message("Reconnecting...")
            },
        )
        .await;

    // Show connection info
    standalone
        .register(
            ActionDefinition::new(
                "fts.web.connection_info",
                "Connection Info",
                "Show current connection information",
            )
            .with_category(ActionCategory::General),
            |_id| {
                tracing::info!("Connection info action triggered");
                ActionResult::success()
            },
        )
        .await;
}

// ============================================================================
// Command Palette UI Component
// ============================================================================

/// Props for the CommandPalette component
#[derive(Props, Clone, PartialEq)]
pub struct CommandPaletteProps {
    /// Whether the palette is open
    pub is_open: Signal<bool>,
    /// The action manager (wrapped in Option for initialization)
    pub manager: Signal<Option<Arc<async_lock::RwLock<ActionManager>>>>,
    /// Currently selected source
    pub selected_source: Signal<ActionSource>,
}

/// Command palette component
#[component]
pub fn CommandPalette(mut props: CommandPaletteProps) -> Element {
    let mut search_query = use_signal(|| String::new());
    let mut actions = use_signal(|| Vec::<SourcedAction>::new());
    let mut selected_index = use_signal(|| 0usize);
    let mut sources = use_signal(|| vec![ActionSource::Standalone]);

    // Load actions when opened or source changes
    use_effect(move || {
        let is_open = *props.is_open.read();
        let source = props.selected_source.read().clone();

        if is_open {
            spawn(async move {
                if let Some(manager_lock) = props.manager.read().as_ref() {
                    let manager = manager_lock.read().await;

                    // Update sources list
                    sources.set(manager.sources());

                    // Get actions for selected source
                    let source_actions = manager.get_actions_for_source(&source).await;
                    actions.set(
                        source_actions
                            .into_iter()
                            .map(|def| SourcedAction {
                                definition: def,
                                source: source.clone(),
                            })
                            .collect(),
                    );
                }
            });
        }
    });

    // Don't render if closed
    if !*props.is_open.read() {
        return rsx! {};
    }

    // Filter actions based on search query (computed each render)
    let filtered_actions: Vec<SourcedAction> = {
        let query = search_query.read().to_lowercase();
        let all_actions = actions.read();

        if query.is_empty() {
            all_actions.clone()
        } else {
            all_actions
                .iter()
                .filter(|a| {
                    a.definition.name.to_lowercase().contains(&query)
                        || a.definition.description.to_lowercase().contains(&query)
                        || a.definition.id.as_str().to_lowercase().contains(&query)
                })
                .cloned()
                .collect()
        }
    };

    // Execute the selected action
    let mut execute_selected = move || {
        let idx = *selected_index.read();
        let all_actions = actions.read();
        let query = search_query.read().to_lowercase();

        let filtered: Vec<_> = if query.is_empty() {
            all_actions.iter().collect()
        } else {
            all_actions
                .iter()
                .filter(|a| {
                    a.definition.name.to_lowercase().contains(&query)
                        || a.definition.description.to_lowercase().contains(&query)
                        || a.definition.id.as_str().to_lowercase().contains(&query)
                })
                .collect()
        };

        if let Some(action) = filtered.get(idx) {
            let action_id = action.definition.id.clone();
            let source = action.source.clone();

            spawn(async move {
                if let Some(manager_lock) = props.manager.read().as_ref() {
                    let manager = manager_lock.read().await;
                    let result = manager.execute(&action_id, &source).await;
                    if result.success {
                        tracing::info!(action = %action_id, "Action executed successfully");
                    } else {
                        tracing::warn!(action = %action_id, message = ?result.message, "Action failed");
                    }
                }
            });

            props.is_open.set(false);
        }
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 bg-black/50 z-40",
            onclick: move |_| props.is_open.set(false),
        }

        // Palette container
        div {
            class: "fixed top-1/4 left-1/2 -translate-x-1/2 w-full max-w-2xl bg-gray-800 rounded-xl shadow-2xl z-50 overflow-hidden",
            onkeydown: move |e: KeyboardEvent| {
                match e.key() {
                    Key::Escape => {
                        props.is_open.set(false);
                    }
                    Key::ArrowDown => {
                        let max = filtered_actions.len().saturating_sub(1);
                        let current = *selected_index.read();
                        selected_index.set((current + 1).min(max));
                    }
                    Key::ArrowUp => {
                        let current = *selected_index.read();
                        selected_index.set(current.saturating_sub(1));
                    }
                    Key::Enter => {
                        execute_selected();
                    }
                    _ => {}
                }
            },

            // Source selector
            div { class: "flex border-b border-gray-700 px-4 py-2 gap-2",
                for source in sources.read().iter().cloned() {
                    {
                        let is_selected = *props.selected_source.read() == source;
                        let source_for_click = source.clone();
                        let source_for_display = source.clone();
                        rsx! {
                            button {
                                class: if is_selected {
                                    "px-3 py-1 rounded-full text-sm bg-blue-600 text-white"
                                } else {
                                    "px-3 py-1 rounded-full text-sm bg-gray-700 text-gray-300 hover:bg-gray-600"
                                },
                                onclick: move |_| props.selected_source.set(source_for_click.clone()),
                                "{source_for_display.display_name()}"
                            }
                        }
                    }
                }
            }

            // Search input
            div { class: "p-4 border-b border-gray-700",
                input {
                    class: "w-full bg-gray-900 text-white px-4 py-3 rounded-lg outline-none focus:ring-2 focus:ring-blue-500",
                    placeholder: "Search actions...",
                    autofocus: true,
                    value: "{search_query}",
                    oninput: move |e| {
                        search_query.set(e.value().clone());
                        selected_index.set(0);
                    },
                }
            }

            // Action list
            div { class: "max-h-96 overflow-y-auto",
                if filtered_actions.is_empty() {
                    div { class: "p-4 text-gray-400 text-center",
                        "No actions found"
                    }
                } else {
                    for (i, action) in filtered_actions.iter().enumerate() {
                        {
                            let is_selected = i == *selected_index.read();
                            let action_id = action.definition.id.clone();
                            let action_name = action.definition.name.clone();
                            let action_desc = action.definition.description.clone();
                            let action_shortcut = action.definition.shortcut_hint.clone();
                            let source = action.source.clone();

                            rsx! {
                                div {
                                    class: if is_selected {
                                        "px-4 py-3 bg-blue-600 cursor-pointer"
                                    } else {
                                        "px-4 py-3 hover:bg-gray-700 cursor-pointer"
                                    },
                                    onclick: move |_| {
                                        let action_id = action_id.clone();
                                        let source = source.clone();
                                        spawn(async move {
                                            if let Some(manager_lock) = props.manager.read().as_ref() {
                                                let manager = manager_lock.read().await;
                                                manager.execute(&action_id, &source).await;
                                            }
                                        });
                                        props.is_open.set(false);
                                    },

                                    div { class: "flex justify-between items-center",
                                        div {
                                            div { class: "font-medium", "{action_name}" }
                                            div { class: "text-sm text-gray-400", "{action_desc}" }
                                        }
                                        if let Some(shortcut) = action_shortcut {
                                            span { class: "text-xs text-gray-500 bg-gray-900 px-2 py-1 rounded",
                                                "{shortcut}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Footer
            div { class: "p-2 border-t border-gray-700 text-xs text-gray-500 flex justify-between",
                span { "↑↓ Navigate" }
                span { "↵ Select" }
                span { "Esc Close" }
            }
        }
    }
}
