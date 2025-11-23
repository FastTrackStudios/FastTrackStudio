//! Infrastructure layer for setlist
//! 
//! This module contains infrastructure concerns like API state, Dioxus integration,
//! and other external integrations.

pub mod api_trait;
#[cfg(feature = "dioxus")]
pub mod dioxus;
#[cfg(feature = "dioxus")]
pub mod dioxus_impl;
#[cfg(not(target_arch = "wasm32"))]
pub mod stream;

