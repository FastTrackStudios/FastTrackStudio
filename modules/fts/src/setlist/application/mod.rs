//! Application helpers for the setlist domain.
//!
//! Instead of exposing a standalone provider, we augment the `Setlist` struct with
//! higher-level workflows (sample generation, persistence, etc.) so it can be embedded
//! directly inside other application states.

mod app;

pub use app::*;
