//! Dock Protocol — domain types for modular layout/docking system.
//!
//! Pure domain types with serde serialization. Zero UI dependencies.
//!
//! - Layout tree: binary splits + tile leaves with tab groups
//! - Panel registry: known panel types
//! - Presets: named layout snapshots (screensets)
//! - Drop zones: drag-and-drop panel rearrangement
//! - Builder: fluent API for constructing layouts
//! - Defaults: built-in screenset presets
//! - Persistence: JSON save/load for presets and layouts

pub mod builder;
pub mod defaults;
pub mod drop_zone;
pub mod id;
pub mod layout;
pub mod panel;
pub mod persistence;
pub mod preset;
pub mod registry;
pub mod tab_group;
pub mod tree;

// Re-export core types at crate root
pub use builder::DockLayoutBuilder;
pub use defaults::default_presets;
pub use drop_zone::DropZone;
pub use id::*;
pub use layout::{DockLayout, FlatNode};
pub use panel::PanelId;
pub use persistence::{load_presets_from_file, save_presets_to_file};
pub use preset::{DockPreset, PresetCollection};
pub use registry::{DockPosition, PanelConstraints, PanelDescriptor, PanelRegistry};
pub use tab_group::TabGroup;
pub use tree::{DockNode, SplitDirection};
