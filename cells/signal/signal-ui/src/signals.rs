//! Rig Control Global Signals
//!
//! This module defines global Dioxus signals for rig state that UI components
//! can read and subscribe to. The signals are updated by the rig service via
//! the `use_rig_subscription()` hook.

use crate::components::rig_grid::node_graph::{Node, NodeParameter, NodePosition, ParameterType};
use crate::prelude::*;
use std::collections::HashMap;
use uuid::Uuid;

// Re-export service types for convenience (signal-control re-exports at crate root)
pub use signal_control::{
    PresetInfo, PresetSnapshotInfo, ProfileInfo, ProfileSceneInfo, RigInfo, SetlistInfo, SongInfo,
};

// ─────────────────────────────────────────────────────────────────────────────
// Rig Editor Sub-Tab
// ─────────────────────────────────────────────────────────────────────────────

/// Which sub-tab is active within the Rig Editor panel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RigEditorTab {
    /// Live performance view.
    Performance,
    /// Unified editor — node graph (Flow/Compact) or block grid (Grid).
    #[default]
    Edit,
    /// Design — module/block creation and editing workspace.
    Design,
    /// Song/setlist editor — manage songs, sections, and setlists.
    SongEditor,
    /// Advanced inspector — raw chunk data, parameter tables, debug info.
    Advanced,
}

impl RigEditorTab {
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Performance => "Performance",
            Self::Edit => "Edit",
            Self::Design => "Design",
            Self::SongEditor => "Songs",
            Self::Advanced => "Advanced",
        }
    }

    pub const fn all() -> &'static [RigEditorTab] {
        &[
            RigEditorTab::Performance,
            RigEditorTab::Edit,
            RigEditorTab::Design,
            RigEditorTab::SongEditor,
            RigEditorTab::Advanced,
        ]
    }
}

/// Active rig editor sub-tab.
pub static RIG_EDITOR_TAB: GlobalSignal<RigEditorTab> = Signal::global(RigEditorTab::default);

// ─────────────────────────────────────────────────────────────────────────────
// Rig State
// ─────────────────────────────────────────────────────────────────────────────

/// Current profile loaded in the rig
pub static RIG_PROFILE: GlobalSignal<Option<ProfileInfo>> = Signal::global(|| None);

/// All available profiles
pub static RIG_AVAILABLE_PROFILES: GlobalSignal<Vec<ProfileInfo>> = Signal::global(Vec::new);

/// Current rig information
pub static RIG_INFO: GlobalSignal<Option<RigInfo>> = Signal::global(|| None);

/// Currently loaded preset
pub static RIG_CURRENT_PRESET: GlobalSignal<Option<PresetInfo>> = Signal::global(|| None);

/// Currently active preset snapshot ID (scene within preset)
pub static RIG_CURRENT_PRESET_SNAPSHOT_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// Last applied snapshot ID (for tracking which scene was most recently activated)
pub static RIG_LAST_APPLIED_SNAPSHOT: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// All available presets
pub static RIG_AVAILABLE_PRESETS: GlobalSignal<Vec<PresetInfo>> = Signal::global(Vec::new);

/// Preloaded presets (presets that have been loaded into memory for fast switching)
pub static RIG_PRELOADED_PRESETS: GlobalSignal<Vec<PresetInfo>> = Signal::global(Vec::new);

/// Current setlist
pub static RIG_CURRENT_SETLIST: GlobalSignal<Option<SetlistInfo>> = Signal::global(|| None);

/// All available setlists
pub static RIG_AVAILABLE_SETLISTS: GlobalSignal<Vec<SetlistInfo>> = Signal::global(Vec::new);

/// Songs in the current setlist
pub static RIG_SETLIST_SONGS: GlobalSignal<Vec<SongInfo>> = Signal::global(Vec::new);

/// Current song (in performance mode)
pub static RIG_CURRENT_SONG: GlobalSignal<Option<SongInfo>> = Signal::global(|| None);

/// Current song index
pub static RIG_SONG_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

/// Current scene (in performance mode)
pub static RIG_CURRENT_SCENE: GlobalSignal<Option<ProfileSceneInfo>> = Signal::global(|| None);

/// Current scene index
pub static RIG_SCENE_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

/// Current modules materialized from preset for UI display
pub static RIG_MODULES: GlobalSignal<Vec<signal_control::module::Module>> =
    Signal::global(Vec::new);

/// Live bound FX chain used for parameter read/write round-trips.
pub static RIG_FX_CHAIN: GlobalSignal<Option<daw_control::FxChain>> = Signal::global(|| None);

/// Mapping from node graph node IDs to DAW plugin GUIDs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeFxBinding {
    pub fx_guid: String,
    pub module_name: String,
    pub plugin_name: String,
}

pub static RIG_NODE_FX_BINDINGS: GlobalSignal<HashMap<Uuid, NodeFxBinding>> =
    Signal::global(HashMap::new);

/// The rig service client — stored globally so any dock panel can dispatch actions
/// without needing a context provider wrapper.
pub static RIG_SERVICE: GlobalSignal<Option<signal_control::SignalControl>> =
    Signal::global(|| None);

/// Whether the DB connection upgrade has been attempted.
/// Prevents multiple spawned tasks from racing.
static DB_INIT_STARTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Initialize the rig service.
///
/// Immediately sets `RIG_SERVICE` to a mock guitar rig so UI can render
/// right away. Then spawns an async task to connect to SQLite at
/// `~/Music/FastTrackStudio/signal.db`, run migrations, and upgrade
/// `RIG_SERVICE` to a DB-backed `SignalControl`.
///
/// If the DB connection fails, the mock service stays in place and a
/// warning is logged. Call this from any component that needs rig data —
/// it's idempotent.
pub fn init_rig_service() {
    if RIG_SERVICE.read().is_some() {
        return; // already initialized
    }
    // Start with mock data immediately so UI can render
    // (commented-out original: was the only line before DB support)
    // *RIG_SERVICE.write() = Some(signal_control::SignalControl::mock_guitar());
    *RIG_SERVICE.write() = Some(signal_control::SignalControl::mock_guitar());

    // Spawn DB connection upgrade (once)
    if !DB_INIT_STARTED.swap(true, std::sync::atomic::Ordering::SeqCst) {
        spawn(async move {
            let db_path = match signal_storage::LocalConfig::default_path() {
                Ok(config) => config.database_path.to_string_lossy().to_string(),
                Err(e) => {
                    tracing::warn!("Could not determine DB path, staying with mock data: {e}");
                    return;
                }
            };

            match signal_control::SignalControl::connect_db(&db_path).await {
                Ok(ctl) => {
                    tracing::info!("Upgraded RIG_SERVICE to DB-backed SignalControl");
                    *RIG_SERVICE.write() = Some(ctl);
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to connect to signal DB at {db_path}, using mock data: {e}"
                    );
                }
            }
        });
    }
}

/// Bind the rig to a live DAW FX chain. Fetches the FxTree, runs discovery,
/// and populates `RIG_MODULES` from the discovered modules.
///
/// Call this when a DAW is connected and the user selects a track.
pub async fn bind_fx_chain(
    chain: &daw_control::FxChain,
    track_guid: &str,
    track_name: &str,
) -> eyre::Result<()> {
    let binding = signal_control::fx_binding::FxRigBinding::from_chain(chain, track_guid).await?;
    let modules = binding.to_signal_modules();
    let n_modules = binding.modules().len();
    let n_unassigned = binding.unassigned().len();

    *RIG_MODULES.write() = modules;
    *RIG_FX_CHAIN.write() = Some(chain.clone());
    *RIG_FX_BINDING_STATUS.write() = format!(
        "Bound: {} ({} modules, {} unassigned)",
        track_name, n_modules, n_unassigned
    );
    *RIG_FX_BINDING.write() = Some(binding);
    sync_node_graph_from_binding(chain).await?;
    Ok(())
}

/// Refresh the FX binding — re-fetches the FxTree and re-runs discovery.
///
/// Call this when the FX chain may have changed (e.g. after adding/removing FX).
pub async fn refresh_fx_binding(chain: &daw_control::FxChain) -> eyre::Result<()> {
    // Clone the binding out to avoid holding the write guard across await points
    let mut binding = match RIG_FX_BINDING.read().clone() {
        Some(b) => b,
        None => return Ok(()),
    };

    binding.refresh(chain).await?;
    let modules = binding.to_signal_modules();
    let n_modules = binding.modules().len();
    let n_unassigned = binding.unassigned().len();
    let track_guid = binding.track_guid.clone();

    *RIG_MODULES.write() = modules;
    *RIG_FX_CHAIN.write() = Some(chain.clone());
    *RIG_FX_BINDING_STATUS.write() = format!(
        "Bound: {} ({} modules, {} unassigned)",
        track_guid, n_modules, n_unassigned
    );
    *RIG_FX_BINDING.write() = Some(binding);
    sync_node_graph_from_binding(chain).await?;
    Ok(())
}

/// Unbind the FX chain, clearing the binding and its status.
pub fn unbind_fx_chain() {
    *RIG_FX_BINDING.write() = None;
    *RIG_FX_CHAIN.write() = None;
    RIG_NODE_FX_BINDINGS.write().clear();
    *RIG_FX_BINDING_STATUS.write() = "Not bound".to_string();
}

/// Refresh a single node's parameter values from DAW.
pub async fn refresh_node_parameters(node_id: Uuid) -> eyre::Result<()> {
    let binding = match RIG_NODE_FX_BINDINGS.read().get(&node_id).cloned() {
        Some(b) => b,
        None => return Ok(()),
    };
    let Some(chain) = RIG_FX_CHAIN.read().clone() else {
        return Ok(());
    };
    let Some(handle) = chain.by_guid(&binding.fx_guid).await? else {
        return Ok(());
    };
    let params = handle.parameters().await.unwrap_or_default();
    let params_simple: Vec<_> = params
        .iter()
        .map(|p| {
            (
                p.index,
                p.name.clone(),
                p.value,
                p.is_toggle,
                p.formatted.clone(),
            )
        })
        .collect();
    let mapped = map_fx_params_to_node_params(&params_simple);
    let mut graph = RIG_NODE_GRAPH.write();
    if let Some(node) = graph.find_node_mut(node_id) {
        node.parameters = mapped;
    }
    Ok(())
}

async fn sync_node_graph_from_binding(chain: &daw_control::FxChain) -> eyre::Result<()> {
    let Some(binding) = RIG_FX_BINDING.read().clone() else {
        return Ok(());
    };

    let mut updates: Vec<ModuleSyncUpdate> = Vec::new();
    for module in binding.modules() {
        let mut plugins = Vec::new();
        let mut stack: Vec<_> = module.blocks.iter().collect();
        while let Some(block) = stack.pop() {
            if block.children.is_empty() {
                plugins.push(&block.fx);
            } else {
                stack.extend(block.children.iter());
            }
        }
        let mut node_updates = Vec::new();
        for plugin in plugins {
            let params = match chain.by_guid(&plugin.guid).await? {
                Some(handle) => handle.parameters().await.unwrap_or_default(),
                None => Vec::new(),
            };
            let params_simple: Vec<_> = params
                .iter()
                .map(|p| {
                    (
                        p.index,
                        p.name.clone(),
                        p.value,
                        p.is_toggle,
                        p.formatted.clone(),
                    )
                })
                .collect();
            node_updates.push(NodeSyncUpdate {
                fx_guid: plugin.guid.clone(),
                plugin_name: plugin.name.clone(),
                plugin_label: plugin.plugin_name.clone(),
                enabled: plugin.enabled,
                params: map_fx_params_to_node_params(&params_simple),
            });
        }
        updates.push(ModuleSyncUpdate {
            module_name: module.container_name.clone(),
            module_key: module.module_type.display_name().to_ascii_uppercase(),
            node_updates,
        });
    }

    let mut graph = RIG_NODE_GRAPH.write();
    let mut bindings = HashMap::new();
    for update in updates {
        let Some(module) = graph.modules.iter_mut().find(|m| {
            m.name.eq_ignore_ascii_case(&update.module_name)
                || m.name.eq_ignore_ascii_case(&update.module_key)
        }) else {
            continue;
        };

        let required = update.node_updates.len();
        if required == 0 {
            module.nodes.clear();
            continue;
        }

        while module.nodes.len() < required {
            let idx = module.nodes.len();
            let offset_x = 12.0 + (idx as f64 * 210.0);
            let pos = NodePosition::new(offset_x, 12.0);
            module.add_node(Node::new("Plugin", module.block_type, pos));
        }
        module.nodes.truncate(required);

        for (idx, node_update) in update.node_updates.into_iter().enumerate() {
            if let Some(node) = module.nodes.get_mut(idx) {
                node.name = if node_update.plugin_name.is_empty() {
                    node_update.plugin_label.clone()
                } else {
                    node_update.plugin_name.clone()
                };
                node.bypassed = !node_update.enabled;
                node.parameters = node_update.params;
                bindings.insert(
                    node.id,
                    NodeFxBinding {
                        fx_guid: node_update.fx_guid,
                        module_name: module.name.clone(),
                        plugin_name: node.name.clone(),
                    },
                );
            }
        }
    }
    *RIG_NODE_FX_BINDINGS.write() = bindings;
    Ok(())
}

#[derive(Debug, Clone)]
struct ModuleSyncUpdate {
    module_name: String,
    module_key: String,
    node_updates: Vec<NodeSyncUpdate>,
}

#[derive(Debug, Clone)]
struct NodeSyncUpdate {
    fx_guid: String,
    plugin_name: String,
    plugin_label: String,
    enabled: bool,
    params: Vec<NodeParameter>,
}

fn map_fx_params_to_node_params(params: &[(u32, String, f64, bool, String)]) -> Vec<NodeParameter> {
    params
        .iter()
        .map(|p| {
            let id = format!("param_{}", p.0);
            let mut np = NodeParameter::new(
                id,
                p.1.clone(),
                signal_control::normalized::NormalizedF64::new(p.2),
            );
            if p.3 {
                np = np.with_param_type(ParameterType::Toggle);
            }
            if !p.4.trim().is_empty() {
                np = np.with_formatted_display(p.4.clone());
            }
            if p.4.contains('%') {
                np = np.with_unit("%").with_range(0.0, 100.0);
            }
            np
        })
        .collect()
}

/// FX chain binding — when set, modules are populated from the live DAW FX chain
/// instead of (or merged with) mock data.
pub static RIG_FX_BINDING: GlobalSignal<Option<signal_control::fx_binding::FxRigBinding>> =
    Signal::global(|| None);

/// Human-readable binding status: "Not bound", "Bound: TrackName (N modules)", etc.
pub static RIG_FX_BINDING_STATUS: GlobalSignal<String> = Signal::global(|| "Not bound".to_string());

/// Connection status
pub static RIG_CONNECTED: GlobalSignal<bool> = Signal::global(|| false);

/// Loading status
pub static RIG_LOADING: GlobalSignal<bool> = Signal::global(|| false);

/// The node graph for the Flow/FlowCompact view.
///
/// Initialized with `sample_guitar_rig()` on first access. Modified by
/// drag interactions, wire creation/deletion, and the module browser.
pub static RIG_NODE_GRAPH: GlobalSignal<crate::components::rig_grid::node_graph::NodeGraph> =
    Signal::global(|| crate::components::rig_grid::node_graph::NodeGraph::sample_guitar_rig());

/// Saved rig snapshots — captured parameter states that can be recalled later.
pub static RIG_SNAPSHOTS: GlobalSignal<Vec<crate::components::rig_grid::node_graph::RigSnapshot>> =
    Signal::global(Vec::new);

/// Currently selected slot in the grid editor dock panel.
///
/// Written by `RigGridEditorPanel` when the user clicks a block/module;
/// read by `RigDetailEditorPanel` to show the detail view. Uses the full
/// `CompositionSlot` so the detail panel has all the info it needs.
pub static RIG_GRID_SELECTED_SLOT: GlobalSignal<
    Option<crate::components::module_editor::module_editor_view::CompositionSlot>,
> = Signal::global(|| None);

/// Current grid selection (Block or Module) — written by `RigGridEditorPanel`,
/// read by `RigDetailEditorPanel`. Carries the full `GridSelection` enum so the
/// detail panel can distinguish between block-level and module-level selection.
pub static RIG_GRID_SELECTION: GlobalSignal<
    Option<crate::components::module_editor::grid_view::GridSelection>,
> = Signal::global(|| None);

/// Chain override for the dock-panel grid editor.
///
/// Written by `RigDetailEditorPanel` when the user assigns a block preset.
/// Read by `RigGridEditorPanelInner` to reflect the update in the grid.
/// Cleared when `RIG_MODULES` changes (new preset loaded). `None` means
/// use the computed chain from `RIG_MODULES`.
pub static RIG_GRID_CHAIN_OVERRIDE: GlobalSignal<
    Option<Vec<crate::components::module_editor::module_editor_view::CompositionSlot>>,
> = Signal::global(|| None);

/// Currently selected entity on the node graph canvas.
///
/// Set by `NodeGraphView` when the user clicks a node or module. Read by
/// `NodePropertyPanel` to display the selected entity's properties.
/// `None` means nothing is selected.
pub static RIG_SELECTED_ENTITY: GlobalSignal<Option<SelectedEntity>> = Signal::global(|| None);

/// What kind of entity is selected on the node graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectedEntity {
    /// A standalone node or a node inside a module.
    Node(Uuid),
    /// A module container.
    Module(Uuid),
}
