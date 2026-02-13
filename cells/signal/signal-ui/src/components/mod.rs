pub mod advanced_inspector;
pub mod automation_lane;
pub mod block_editor;
pub mod daw_preset_panel;
pub mod daw_snapshot_panel;
pub mod fx_binding_status;
pub mod midi_learn;
pub mod module_editor;
pub mod morph_slider;
pub mod performance;
pub mod preset_browser;
pub mod preset_editor;
pub mod profile_editor;
pub mod rig_grid;
pub mod shared;
pub mod snapshot_test_harness;
pub mod song_editor;

// Re-export morph slider
pub use morph_slider::MorphSlider;

// Re-export MIDI learn components
pub use midi_learn::{
    handle_midi_cc, MidiActivityIndicator, MidiCcMappingList, MidiCcSettings, MidiLearnButton,
};

// Re-export shared components
pub use shared::{CreateEntityModal, EntityKind};

// Re-export commonly used components
pub use rig_grid::{
    block_type_color,
    GridBlock,
    GridConnection,
    GridPosition,
    GuitarRigGrid,
    GuitarRigLeftSidebar,
    GuitarRigProfileSidebar,
    GuitarRigRightSidebar,
    // Page layout components
    GuitarRigTopBar,
    ModuleBrowserModal,
    ModuleViewMode,
    // Node-based system
    Node,
    NodeGraph,
    NodePosition,
    // Property panel
    NodePropertyPanel,
    NodeSize,
    NodeWidget,
    RigViewMode,
    // Version history
    VersionHistoryPanel,
    GRID_COLS,
    GRID_ROWS,
};
