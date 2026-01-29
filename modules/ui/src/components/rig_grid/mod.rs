//! Guitar Rig Grid UI - Quad Cortex/Helix inspired module grid.
//!
//! This module provides a polished grid UI for displaying guitar rig modules
//! with three view modes: Detail, Macro, and Compact.
//!
//! # View Modes
//!
//! - **Detail**: Shows all parameters for each block using audio-controls widgets
//! - **Macro**: Shows only quick-access macro knobs in a compact grid
//! - **Compact**: Shows blocks as colored pills (Quad Cortex style) for rapid switching
//!
//! # Components
//!
//! - `GuitarRigGrid`: Main entry component replacing RigControlView for guitar rigs
//! - `ModuleGroup`: Container for a single module with header and view-mode content
//! - `ModuleHeader`: Name, preset dropdown, bypass toggle, view mode selector
//! - `ModuleDetailView`: Full parameter display using BlockView widgets
//! - `ModuleMacroView`: Macro knob grid
//! - `ModuleCompactView`: Colored block pills

pub mod view_mode;
pub mod block_colors;
pub mod module_compact_view;
pub mod module_macro_view;
pub mod module_detail_view;
pub mod module_header;
pub mod module_group;
pub mod guitar_rig_grid;

// Re-export main components
pub use view_mode::{ModuleViewMode, GlobalViewOverride};
pub use block_colors::block_type_color;
pub use module_compact_view::ModuleCompactView;
pub use module_macro_view::ModuleMacroView;
pub use module_detail_view::ModuleDetailView;
pub use module_header::ModuleHeader;
pub use module_group::ModuleGroup;
pub use guitar_rig_grid::GuitarRigGrid;
