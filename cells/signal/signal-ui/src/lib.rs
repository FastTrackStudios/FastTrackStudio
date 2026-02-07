//! Signal UI — Dioxus components for rig control.
//!
//! Provides the reactive UI layer for signal control:
//! - Global signals for rig state
//! - Hooks for service subscription and actions
//! - Context providers for dependency injection
//! - Components for the guitar rig grid interface

pub mod components;
pub mod context;
pub mod hooks;
pub mod layouts;
pub mod prelude;
pub mod signals;

// Re-exports for desktop app
pub use context::rig::{use_rig_service, RigService, RigServiceProvider};
pub use hooks::rig_actions::{use_rig_actions, RigActions};
pub use hooks::rig_state::use_rig_subscription;
pub use layouts::rig_layout::RigLayout;
pub use signal_control::SignalControl;
pub use signals::*;
