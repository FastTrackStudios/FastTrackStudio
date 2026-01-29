//! Guitar Rig Grid UI - Quad Cortex/Helix inspired signal flow grid.
//!
//! This module provides a polished grid UI for displaying guitar rig signal flow
//! with three view modes: Grid (signal flow), Macro (knobs), and Detail (parameters).
//!
//! # View Modes
//!
//! - **Grid**: 14x6 signal flow grid with blocks, routing lines, and I/O jacks (Quad Cortex style)
//! - **Macro**: Quick-access macro knobs in a compact layout
//! - **Detail**: Full parameter editing for each block
//!
//! # Grid Components
//!
//! - `SignalFlowGridView`: Main 14x6 grid with signal routing (compact mode)
//! - `GridBlock`, `GridConnection`, `GridJack`: Grid data model
//!
//! # Module Components
//!
//! - `GuitarRigGrid`: Main entry component with view mode switching
//! - `ModuleGroup`: Container for a single module with header and view-mode content
//! - `ModuleHeader`: Name, preset dropdown, bypass toggle, view mode selector
//! - `ModuleDetailView`: Full parameter display
//! - `ModuleMacroView`: Macro knob grid
//!
//! # Page Layout Components
//!
//! - `GuitarRigTopBar`: Top navigation bar with view mode selector
//! - `GuitarRigLeftSidebar`: Preset browser and profile selector
//! - `GuitarRigRightSidebar`: Scenes and songs navigation
//! - `ModuleBrowserModal`: Modal for browsing and adding modules

pub mod view_mode;
pub mod block_colors;
pub mod grid_model;
pub mod signal_flow_grid;
pub mod module_compact_view;
pub mod module_macro_view;
pub mod module_detail_view;
pub mod module_header;
pub mod module_group;
pub mod guitar_rig_grid;
pub mod top_bar;
pub mod left_sidebar;
pub mod right_sidebar;
pub mod module_browser_modal;
// Node-based system
pub mod node_graph;
pub mod node_canvas;
pub mod node_focus_dialog;
pub mod performance_view;

// Re-export main components
pub use view_mode::{ModuleViewMode, GlobalViewOverride};
pub use block_colors::block_type_color;
pub use grid_model::{GridBlock, GridConnection, GridJack, GridPosition, SignalFlowGrid, GRID_COLS, GRID_ROWS};
pub use signal_flow_grid::SignalFlowGridView;
pub use module_compact_view::ModuleCompactView;
pub use module_macro_view::ModuleMacroView;
pub use module_detail_view::ModuleDetailView;
pub use module_header::ModuleHeader;
pub use module_group::ModuleGroup;
pub use guitar_rig_grid::GuitarRigGrid;

// Page layout components
pub use top_bar::GuitarRigTopBar;
pub use left_sidebar::GuitarRigLeftSidebar;
pub use right_sidebar::GuitarRigRightSidebar;
pub use module_browser_modal::ModuleBrowserModal;

// Node-based system
pub use node_graph::{Module, Node, NodeGraph, NodePosition, NodeSize, NodeWidget, Wire};
pub use node_canvas::NodeCanvas;
pub use node_focus_dialog::NodeFocusDialog;
pub use performance_view::{PerformanceView, DetailLevel, PerformanceModule};
