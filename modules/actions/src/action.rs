//! Action types and definitions

use std::fmt;
use std::sync::Arc;

use crate::Result;

// region:    --- ActionId (Newtype)

/// Unique identifier for an action.
///
/// Action IDs follow the pattern `{namespace}.{group}.{action}`.
/// Examples:
/// - `fts.transport.play`
/// - `fts.tracks.mute_selected`
/// - `fts.setlist.next_song`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ActionId(pub &'static str);

impl ActionId {
    #[must_use]
    pub const fn new(id: &'static str) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        self.0
    }

    /// Returns the namespace part (e.g., "fts" from "fts.transport.play")
    #[must_use]
    pub fn namespace(&self) -> Option<&str> {
        self.0.split('.').next()
    }

    /// Returns the group part (e.g., "transport" from "fts.transport.play")
    #[must_use]
    pub fn group(&self) -> Option<&str> {
        self.0.split('.').nth(1)
    }

    /// Returns the action name part (e.g., "play" from "fts.transport.play")
    #[must_use]
    pub fn name(&self) -> Option<&str> {
        self.0.split('.').nth(2)
    }
}

impl fmt::Debug for ActionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ActionId({})", self.0)
    }
}

impl fmt::Display for ActionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// endregion: --- ActionId (Newtype)

// region:    --- ActionCategory

/// Category of an action for organization in menus/UI
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ActionCategory {
    #[default]
    General,
    Transport,
    Tracks,
    Editing,
    Navigation,
    View,
    Project,
    Recording,
    Mixing,
    Automation,
}

impl fmt::Display for ActionCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::General => write!(f, "General"),
            Self::Transport => write!(f, "Transport"),
            Self::Tracks => write!(f, "Tracks"),
            Self::Editing => write!(f, "Editing"),
            Self::Navigation => write!(f, "Navigation"),
            Self::View => write!(f, "View"),
            Self::Project => write!(f, "Project"),
            Self::Recording => write!(f, "Recording"),
            Self::Mixing => write!(f, "Mixing"),
            Self::Automation => write!(f, "Automation"),
        }
    }
}

// endregion: --- ActionCategory

// region:    --- Shortcut

/// Keyboard shortcut for an action
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shortcut {
    /// Key code or character
    pub key: String,
    /// Whether Ctrl/Cmd is required
    pub ctrl: bool,
    /// Whether Shift is required
    pub shift: bool,
    /// Whether Alt/Option is required
    pub alt: bool,
}

impl Shortcut {
    #[must_use]
    pub fn new(key: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            ctrl: false,
            shift: false,
            alt: false,
        }
    }

    #[must_use]
    pub const fn with_ctrl(mut self) -> Self {
        self.ctrl = true;
        self
    }

    #[must_use]
    pub const fn with_shift(mut self) -> Self {
        self.shift = true;
        self
    }

    #[must_use]
    pub const fn with_alt(mut self) -> Self {
        self.alt = true;
        self
    }
}

impl fmt::Display for Shortcut {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        if self.ctrl {
            parts.push("Ctrl");
        }
        if self.alt {
            parts.push("Alt");
        }
        if self.shift {
            parts.push("Shift");
        }
        parts.push(&self.key);
        write!(f, "{}", parts.join("+"))
    }
}

// endregion: --- Shortcut

// region:    --- ActionDefinition

/// Static definition of an action - metadata only, no execution logic.
///
/// This is what `ActionGroup::action_definitions()` returns.
/// The actual execution callbacks are provided separately by the service.
#[derive(Debug, Clone)]
pub struct ActionDefinition {
    /// Unique identifier
    pub id: ActionId,
    /// Human-readable name for menus
    pub name: &'static str,
    /// Tooltip/help text
    pub description: &'static str,
    /// Default keyboard shortcut (can be overridden by user)
    pub default_shortcut: Option<Shortcut>,
    /// Category for organization
    pub category: ActionCategory,
}

impl ActionDefinition {
    #[must_use]
    pub const fn new(id: ActionId, name: &'static str, description: &'static str) -> Self {
        Self {
            id,
            name,
            description,
            default_shortcut: None,
            category: ActionCategory::General,
        }
    }

    #[must_use]
    pub const fn with_category(mut self, category: ActionCategory) -> Self {
        self.category = category;
        self
    }

    #[must_use]
    pub fn with_shortcut(mut self, shortcut: Shortcut) -> Self {
        self.default_shortcut = Some(shortcut);
        self
    }
}

// endregion: --- ActionDefinition

// region:    --- Action

/// A fully-bound action with execution capability.
///
/// This is created by combining an `ActionDefinition` with an executor callback.
/// Used by the registry to store actions that can be invoked.
pub struct Action {
    /// The action definition (metadata)
    pub definition: ActionDefinition,
    /// The execution callback
    pub execute: Arc<dyn Fn() -> Result<()> + Send + Sync>,
}

impl Action {
    /// Create a new action from a definition and executor
    pub fn new<F>(definition: ActionDefinition, execute: F) -> Self
    where
        F: Fn() -> Result<()> + Send + Sync + 'static,
    {
        Self {
            definition,
            execute: Arc::new(execute),
        }
    }

    /// Execute the action
    pub fn invoke(&self) -> Result<()> {
        (self.execute)()
    }

    /// Get the action ID
    #[must_use]
    pub fn id(&self) -> ActionId {
        self.definition.id
    }

    /// Get the action name
    #[must_use]
    pub fn name(&self) -> &'static str {
        self.definition.name
    }
}

impl fmt::Debug for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Action")
            .field("definition", &self.definition)
            .field("execute", &"<fn>")
            .finish()
    }
}

// endregion: --- Action
