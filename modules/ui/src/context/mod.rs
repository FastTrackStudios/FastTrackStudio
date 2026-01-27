//! Context providers for dependency injection in Dioxus components

#[macro_use]
pub mod macros;

pub mod rig;
pub mod setlist;

pub use rig::{use_rig_service, RigServiceCtx, RigServiceProvider};
pub use setlist::{use_setlist_service, SetlistServiceCtx, SetlistServiceProvider};
