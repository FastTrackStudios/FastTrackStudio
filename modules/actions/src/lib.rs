//! Actions crate - Action definitions and ActionGroup trait
//!
//! This crate provides the foundation for user-invocable actions in FastTrackStudio.
//! Actions are commands that can be triggered via:
//! - REAPER's action system (keyboard shortcuts, menus, toolbar buttons)
//! - Remote clients via roam RPC
//! - Lua scripts (future)
//!
//! # Architecture
//!
//! - `Action`: A single invocable action with execution capability
//! - `ActionDefinition`: Static metadata for an action (id, name, shortcut)
//! - `ActionGroup`: Trait for types that provide a group of related actions
//! - `ActionRegistry`: Central registry of all actions (in microservice crate)
//!
//! # Example
//!
//! ```ignore
//! use actions::{ActionDefinition, ActionGroup, ActionId};
//!
//! // Transport state struct implements ActionGroup
//! impl ActionGroup for Transport {
//!     fn action_definitions() -> &'static [ActionDefinition] {
//!         &[
//!             ActionDefinition {
//!                 id: ActionId::new("fts.transport.play"),
//!                 name: "Play",
//!                 description: "Start playback",
//!                 default_shortcut: None,
//!             },
//!         ]
//!     }
//! }
//! ```

// region:    --- Modules

mod action;
mod error;
mod group;

pub use action::{Action, ActionCategory, ActionDefinition, ActionId, Shortcut};
pub use error::{Error, Result};
pub use group::ActionGroup;

#[cfg(feature = "roam")]
mod service;

#[cfg(feature = "roam")]
pub use service::*;

// endregion: --- Modules
