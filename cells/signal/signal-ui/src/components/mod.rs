pub mod morph_slider;
pub mod rig_grid;
pub mod snapshot_slots;

// Re-export morph slider
pub use morph_slider::MorphSlider;

// Re-export snapshot slots
pub use snapshot_slots::{SnapshotSlots, SnapshotSlotsPanel, SnapshotSlotsState, SnapshotSlot, RIG_SNAPSHOT_SLOTS};

// Re-export morph slider
pub use morph_slider::MorphSlider;

// Re-export commonly used components
pub use rig_grid::{
    GuitarRigGrid, ModuleViewMode, RigViewMode, block_type_color,
    GridBlock, GridPosition, GridConnection,
    GRID_COLS, GRID_ROWS,
    // Page layout components
    GuitarRigTopBar, GuitarRigLeftSidebar, GuitarRigRightSidebar, GuitarRigProfileSidebar, ModuleBrowserModal,
    // Node-based system
    Node, NodeGraph, NodePosition, NodeSize, NodeWidget,
};
