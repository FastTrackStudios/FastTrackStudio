//! Rig Control - Re-exports from rig-control crate
//!
//! This module provides backward compatibility by re-exporting rig control
//! UI components and signals from the rig-control crate.
//!
//! TODO: Re-enable when rig-control roam service issues are fixed

/* TEMPORARILY DISABLED
// Re-export everything from rig-control's UI module
#[cfg(feature = "dioxus")]
pub use rig_control::ui::*;

// Re-export service types for backward compatibility
pub use rig_control::MockRigControlService;

// Re-export domain types under "core" namespace for backward compatibility
pub mod core {
    pub use rig_control::block::BlockType;
    pub use rig_control::tags::{Tag as PresetTag, TagRegistry, TagCategory, TagFilter};
    pub use rig_control::category::{PresetCategory, BaseTone};
}

// Re-export service types under "service" namespace for backward compatibility
pub mod service {
    pub use rig_control::service::*;
}
*/
