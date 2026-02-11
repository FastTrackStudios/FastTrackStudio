//! Panel registration for signal/rig domain.
//!
//! Registers all signal-ui panels with the dock renderer registry,
//! decoupling panel definitions from the central app binary.

use dock_dioxus::PanelRendererRegistry;
use dock_proto::PanelId;

use crate::components::snapshot_test_harness::SnapshotTestHarness;
use crate::layouts::rig_editor::RigEditorPanel;
use crate::layouts::rig_layout::{
    PresetBrowserPanel, ProfileBrowserPanel, RigDetailEditorPanel, RigGridEditorPanel,
    RigGridPanel, SceneGridDockPanel, SongPartsPanel, SongSelectorPanel,
};
use daw_ui::FxBrowserDockPanel;

use crate::prelude::*;

/// Register all signal/rig panels with the renderer registry.
pub fn register_panels(registry: &mut PanelRendererRegistry) {
    registry.register(PanelId::RigGrid, || {
        rsx! { RigGridPanel {} }
    });
    registry.register(PanelId::PresetBrowser, || {
        rsx! { PresetBrowserPanel {} }
    });
    registry.register(PanelId::ProfileBrowser, || {
        rsx! { ProfileBrowserPanel {} }
    });
    registry.register(PanelId::SongParts, || {
        rsx! { SongPartsPanel {} }
    });
    registry.register(PanelId::SongSelector, || {
        rsx! { SongSelectorPanel {} }
    });
    registry.register(PanelId::SceneGrid, || {
        rsx! { SceneGridDockPanel {} }
    });
    registry.register(PanelId::FxBrowser, || {
        rsx! { FxBrowserDockPanel {} }
    });
    registry.register(PanelId::RigEditor, || {
        rsx! { RigEditorPanel {} }
    });
    registry.register(PanelId::RigGridEditor, || {
        rsx! { RigGridEditorPanel {} }
    });
    registry.register(PanelId::RigDetailEditor, || {
        rsx! { RigDetailEditorPanel {} }
    });
    registry.register(PanelId::SnapshotTest, || {
        rsx! { SnapshotTestHarness {} }
    });
}
