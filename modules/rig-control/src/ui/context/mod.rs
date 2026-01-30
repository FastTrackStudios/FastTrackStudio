//! Context providers for dependency injection in Dioxus components

#[macro_use]
pub mod macros;

pub mod rig;

pub use rig::{use_rig_service, RigServiceCtx, RigServiceProvider};
