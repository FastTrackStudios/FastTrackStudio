//! Infrastructure layer for setlist
//!
//! This module contains infrastructure concerns like API state, Dioxus integration,
//! and other external integrations.

pub mod api_trait;
pub mod commands;
#[cfg(feature = "dioxus")]
pub mod dioxus;
#[cfg(feature = "dioxus")]
pub mod dioxus_impl;
#[cfg(feature = "iroh")]
pub mod stream;
pub mod traits;

#[cfg(feature = "reaper")]
pub mod reaper;

// Re-export command types (shared between iroh and reaper features)
pub use commands::{LyricsState, NavigationCommand, TransportCommand};

// Re-export traits (shared between iroh and reaper features)
pub use traits::{CommandAdapter, SeekAdapter, SetlistBuilder};

#[cfg(not(target_arch = "wasm32"))]
pub use traits::{SetlistCommandHandler, SetlistStateProvider};
