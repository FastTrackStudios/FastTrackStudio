//! Context providers for dependency injection in Dioxus components

#[macro_use]
pub mod macros;

pub mod setlist;

pub use setlist::{use_setlist_service, SetlistServiceCtx, SetlistServiceProvider};
