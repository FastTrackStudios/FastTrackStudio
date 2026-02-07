//! Context providers for dependency injection in Dioxus components

pub mod rig;

pub use rig::{use_rig_service, RigService, RigServiceCtx, RigServiceProvider};
