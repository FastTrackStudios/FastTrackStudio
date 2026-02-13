//! Context providers for dependency injection in Dioxus components

pub mod rig;
pub mod rig_grid;

pub use rig::{
    detect_service_mode, use_rig_service, RigService, RigServiceCtx, RigServiceMode,
    RigServiceProvider,
};
pub use rig_grid::{use_rig_grid_state, RigGridState, RigGridStateProvider};
