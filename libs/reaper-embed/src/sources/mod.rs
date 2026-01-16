//! Pre-built embed sources
//!
//! This module provides ready-to-use implementations of `EmbedSource`:
//!
//! - [`vello::VelloEmbedSource`] - For custom Vello scene rendering
//! - [`dioxus::DioxusEmbedSource`] - For Dioxus component rendering (requires `dioxus` feature)

pub mod vello;

#[cfg(feature = "dioxus")]
pub mod dioxus;
