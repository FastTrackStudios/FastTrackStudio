//! Rig grid context provider — scoped state for the rig grid subtree.
//!
//! Replaces direct `GlobalSignal` reads for signals that are local to the
//! rig grid component subtree:
//!
//! - `RIG_GRID_SELECTED_SLOT` — selected block/module in the 2D grid editor
//! - `RIG_SELECTED_ENTITY` — selected node/module on the node graph canvas
//!
//! ## Usage
//!
//! Wrap the rig grid subtree with `RigGridStateProvider`:
//!
//! ```ignore
//! RigGridStateProvider {
//!     // all descendants can call use_rig_grid_state()
//!     NodePropertyPanel {}
//! }
//! ```
//!
//! Descendants read/write via the hook:
//!
//! ```ignore
//! let grid_state = use_rig_grid_state();
//! let selected = grid_state.selected_entity();
//! grid_state.set_selected_entity(Some(SelectedEntity::Node(id)));
//! ```

use crate::components::module_editor::module_editor_view::CompositionSlot;
use crate::prelude::*;
use crate::signals::SelectedEntity;

/// Scoped rig grid state provided via Dioxus context.
///
/// Holds reactive signals for selection state that is local to the rig
/// grid subtree. Components use `use_rig_grid_state()` to obtain this.
#[derive(Clone)]
pub struct RigGridState {
    selected_slot: Signal<Option<CompositionSlot>>,
    selected_entity: Signal<Option<SelectedEntity>>,
}

impl RigGridState {
    /// Create a new `RigGridState` with default (empty) selections.
    fn new() -> Self {
        Self {
            selected_slot: Signal::new(None),
            selected_entity: Signal::new(None),
        }
    }

    // ── Selected Slot (grid editor) ─────────────────────────────

    /// Read the currently selected composition slot.
    pub fn selected_slot(&self) -> Option<CompositionSlot> {
        self.selected_slot.read().clone()
    }

    /// Get a read reference to the selected slot signal.
    pub fn selected_slot_signal(&self) -> &Signal<Option<CompositionSlot>> {
        &self.selected_slot
    }

    /// Set the selected composition slot.
    pub fn set_selected_slot(&self, slot: Option<CompositionSlot>) {
        let mut sig = self.selected_slot;
        *sig.write() = slot;
    }

    // ── Selected Entity (node graph) ────────────────────────────

    /// Read the currently selected entity (node or module).
    pub fn selected_entity(&self) -> Option<SelectedEntity> {
        *self.selected_entity.read()
    }

    /// Get a read reference to the selected entity signal.
    pub fn selected_entity_signal(&self) -> &Signal<Option<SelectedEntity>> {
        &self.selected_entity
    }

    /// Set the selected entity.
    pub fn set_selected_entity(&self, entity: Option<SelectedEntity>) {
        let mut sig = self.selected_entity;
        *sig.write() = entity;
    }

    /// Check whether any entity is currently selected.
    pub fn has_selection(&self) -> bool {
        self.selected_entity.read().is_some()
    }
}

/// Hook to access the rig grid state from context.
///
/// # Panics
///
/// Panics if called outside of a `RigGridStateProvider` subtree.
pub fn use_rig_grid_state() -> RigGridState {
    use_context::<RigGridState>()
}

/// Provider component that initializes rig grid scoped state and injects
/// it into the Dioxus context tree.
///
/// Place this around any subtree that needs access to rig grid selection
/// state (e.g., `RigLayout`, `RigGridPanel`, `NodePropertyDockPanel`).
#[component]
pub fn RigGridStateProvider(children: Element) -> Element {
    use_context_provider(RigGridState::new);
    children
}
