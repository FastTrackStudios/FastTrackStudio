//! Typed action ID constants and the [`StaticActionId`] wrapper.
//!
//! Each domain crate declares its own action IDs via [`declare_actions!`](crate::declare_actions).
//! This module provides `StaticActionId` (the const-constructible wrapper) and
//! constants for actions owned by `actions-proto` itself (e.g., standalone app actions).
//!
//! ```ignore
//! // Domain crate declares its own action IDs:
//! // session::session_actions::TOGGLE_PLAYBACK
//!
//! // Standalone actions are defined here:
//! use actions_proto::ids::standalone;
//! standalone::TOGGLE_DARK_MODE.to_id()  // ActionId
//! ```

use crate::ActionId;

/// A compile-time action identifier backed by a `&'static str`.
///
/// Use `.to_id()` to convert to an owned `ActionId` for dispatch,
/// or `.as_str()` to get the raw string for matching.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StaticActionId(pub &'static str);

impl StaticActionId {
    pub const fn new(id: &'static str) -> Self {
        Self(id)
    }

    pub const fn as_str(&self) -> &'static str {
        self.0
    }

    pub fn to_id(&self) -> ActionId {
        ActionId::new(self.0)
    }
}

impl From<StaticActionId> for ActionId {
    fn from(s: StaticActionId) -> Self {
        ActionId::new(s.0)
    }
}

impl PartialEq<ActionId> for StaticActionId {
    fn eq(&self, other: &ActionId) -> bool {
        self.0 == other.as_str()
    }
}

impl PartialEq<StaticActionId> for ActionId {
    fn eq(&self, other: &StaticActionId) -> bool {
        self.as_str() == other.0
    }
}

impl std::fmt::Display for StaticActionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Standalone app actions (settings, dark mode, command palette).
pub mod standalone {
    use super::StaticActionId;

    pub const OPEN_SETTINGS: StaticActionId =
        StaticActionId::new("fts.standalone.open_settings");
    pub const TOGGLE_DARK_MODE: StaticActionId =
        StaticActionId::new("fts.standalone.toggle_dark_mode");
    pub const COMMAND_PALETTE: StaticActionId =
        StaticActionId::new("fts.standalone.command_palette");
    pub const SHOW_ABOUT: StaticActionId =
        StaticActionId::new("fts.standalone.show_about");
}
