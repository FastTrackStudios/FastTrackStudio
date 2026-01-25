//! Context providers for dependency injection in Dioxus components

pub mod setlist;

pub use setlist::{use_setlist_service, SetlistServiceCtx, SetlistServiceProvider};
