//! Actions Protocol - Shared types and service definitions
//!
//! This crate defines the protocol for the actions system, allowing cells
//! to provide actions and hosts to aggregate them for command palettes.
//!
//! ## Architecture
//!
//! ```text
//!                                    ┌─────────────────────────────────────┐
//!                                    │         Command Palette             │
//!                                    │  (web app / desktop app)            │
//!                                    │                                     │
//!                                    │  ┌─────────────────────────────┐    │
//!                                    │  │ Host Selector:              │    │
//!                                    │  │  ○ Standalone               │    │
//!                                    │  │  ● REAPER @ 192.168.1.10    │    │
//!                                    │  │  ○ REAPER @ localhost       │    │
//!                                    │  └─────────────────────────────┘    │
//!                                    └──────────────┬──────────────────────┘
//!                                                   │
//!                          ┌────────────────────────┼────────────────────────┐
//!                          │                        │                        │
//!                          ▼                        ▼                        ▼
//!                 ┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
//!                 │   Standalone    │      │ REAPER Host 1   │      │ REAPER Host 2   │
//!                 │ (ActionsService)│      │ (ActionsService)│      │ (ActionsService)│
//!                 │                 │      │                 │      │                 │
//!                 │ Local actions   │      │ Aggregates from │      │ Aggregates from │
//!                 │ for web/desktop │      │ session, daw,   │      │ session, daw,   │
//!                 │                 │      │ etc. cells      │      │ etc. cells      │
//!                 └─────────────────┘      └─────────────────┘      └─────────────────┘
//! ```
//!
//! ## Key Traits
//!
//! - `DefinesActions`: Implemented by cells that provide actions
//! - `ActionsService`: Implemented by hosts to aggregate and expose all actions

#![deny(unsafe_code)]

use facet::Facet;
use roam::service;

/// Unique identifier for an action.
///
/// Action IDs follow the pattern `{namespace}.{cell}.{action}`.
/// Examples:
/// - `fts.session.log_hello`
/// - `fts.transport.play`
/// - `fts.standalone.open_settings` (for standalone app actions)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
pub struct ActionId(pub String);

impl ActionId {
    /// Create a new ActionId
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Get the action ID as a string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns the namespace part (e.g., "fts" from "fts.session.log_hello")
    pub fn namespace(&self) -> Option<&str> {
        self.0.split('.').next()
    }

    /// Returns the cell/source part (e.g., "session" from "fts.session.log_hello")
    pub fn source(&self) -> Option<&str> {
        self.0.split('.').nth(1)
    }

    /// Returns the action name part (e.g., "log_hello" from "fts.session.log_hello")
    pub fn name(&self) -> Option<&str> {
        self.0.split('.').nth(2)
    }

    /// Convert to REAPER command ID format (e.g., "FTS_SESSION_LOG_HELLO")
    pub fn to_command_id(&self) -> String {
        self.0.replace('.', "_").to_uppercase()
    }
}

impl std::fmt::Display for ActionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for ActionId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for ActionId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// Category for organizing actions in menus/palettes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Facet)]
#[repr(u8)]
pub enum ActionCategory {
    #[default]
    General,
    Transport,
    Session,
    Project,
    Tracks,
    View,
    Settings,
    Dev,
}

impl std::fmt::Display for ActionCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::General => write!(f, "General"),
            Self::Transport => write!(f, "Transport"),
            Self::Session => write!(f, "Session"),
            Self::Project => write!(f, "Project"),
            Self::Tracks => write!(f, "Tracks"),
            Self::View => write!(f, "View"),
            Self::Settings => write!(f, "Settings"),
            Self::Dev => write!(f, "Dev"),
        }
    }
}

/// Definition of an action (metadata only, no execution logic).
#[derive(Debug, Clone, Facet)]
pub struct ActionDefinition {
    /// Unique identifier for the action
    pub id: ActionId,
    /// Human-readable name for display in menus/palettes
    pub name: String,
    /// Description/tooltip for the action
    pub description: String,
    /// Category for organizing in menus/palettes
    pub category: ActionCategory,
    /// Menu path (e.g., "FTS/Session") - None means don't show in menu
    pub menu_path: Option<String>,
    /// Keyboard shortcut hint (e.g., "Cmd+Shift+P") - display only
    pub shortcut_hint: Option<String>,
}

impl ActionDefinition {
    /// Create a new action definition
    pub fn new(
        id: impl Into<ActionId>,
        name: impl Into<String>,
        description: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            description: description.into(),
            category: ActionCategory::General,
            menu_path: None,
            shortcut_hint: None,
        }
    }

    /// Set the category
    pub fn with_category(mut self, category: ActionCategory) -> Self {
        self.category = category;
        self
    }

    /// Set the menu path
    pub fn with_menu_path(mut self, path: impl Into<String>) -> Self {
        self.menu_path = Some(path.into());
        self
    }

    /// Set a keyboard shortcut hint
    pub fn with_shortcut(mut self, shortcut: impl Into<String>) -> Self {
        self.shortcut_hint = Some(shortcut.into());
        self
    }

    /// Get the REAPER command ID for this action
    pub fn command_id(&self) -> String {
        self.id.to_command_id()
    }

    /// Get the display name with FTS prefix
    pub fn display_name(&self) -> String {
        format!("FTS: {}", self.name)
    }
}

/// Result of executing an action
#[derive(Debug, Clone, Facet)]
pub struct ActionResult {
    /// Whether the action executed successfully
    pub success: bool,
    /// Optional message (error message on failure, info on success)
    pub message: Option<String>,
}

impl ActionResult {
    /// Create a successful result
    pub fn success() -> Self {
        Self {
            success: true,
            message: None,
        }
    }

    /// Create a successful result with a message
    pub fn success_with_message(message: impl Into<String>) -> Self {
        Self {
            success: true,
            message: Some(message.into()),
        }
    }

    /// Create a failure result
    pub fn failure(message: impl Into<String>) -> Self {
        Self {
            success: false,
            message: Some(message.into()),
        }
    }
}

/// Event emitted when actions change
#[derive(Debug, Clone)]
pub enum ActionEvent {
    /// Actions were added (cell/source registered)
    Added {
        source: String,
        actions: Vec<ActionDefinition>,
    },
    /// Actions were removed (cell/source unregistered)
    Removed {
        source: String,
        action_ids: Vec<ActionId>,
    },
}

// ============================================================================
// DefinesActions - Service trait for cells that provide actions
// ============================================================================

/// Service trait for cells that define actions.
///
/// Any cell that wants to provide actions should implement this trait.
/// The host (via `ActionsRegistry`) will query each connected cell for
/// actions using this interface.
///
/// ## Example
///
/// ```ignore
/// impl DefinesActions for MyServiceImpl {
///     async fn get_actions(&self, _cx: &Context) -> Vec<ActionDefinition> {
///         vec![
///             ActionDefinition::new(
///                 "fts.mycell.my_action",
///                 "My Action",
///                 "Does something cool",
///             )
///             .with_category(ActionCategory::General)
///             .with_menu_path("FTS/MyCell"),
///         ]
///     }
///
///     async fn execute_action(&self, _cx: &Context, action_id: ActionId) -> ActionResult {
///         match action_id.as_str() {
///             "fts.mycell.my_action" => {
///                 // Do the thing
///                 ActionResult::success()
///             }
///             _ => ActionResult::failure("Unknown action"),
///         }
///     }
/// }
/// ```
#[service]
pub trait DefinesActions {
    /// Get all actions defined by this cell.
    async fn get_actions(&self) -> Vec<ActionDefinition>;

    /// Execute an action by ID.
    ///
    /// Returns `ActionResult` indicating success/failure.
    async fn execute_action(&self, action_id: ActionId) -> ActionResult;
}

// ============================================================================
// ActionsService - Aggregated service exposed by hosts
// ============================================================================

/// Service trait for hosts that aggregate actions from multiple sources.
///
/// This is exposed by:
/// - REAPER extension (aggregates actions from cells like session, daw, etc.)
/// - Standalone apps (provides local actions for web/desktop)
///
/// Command palettes connect to this service to get all available actions
/// and execute them.
///
/// ## Example
///
/// ```ignore
/// // In a command palette UI
/// let actions = actions_client.get_all_actions().await?;
///
/// // Display in palette, user selects one...
/// let result = actions_client.execute(selected_action_id).await?;
/// ```
#[service]
pub trait ActionsService {
    /// Get all available actions from all sources.
    ///
    /// Returns actions aggregated from all cells/sources.
    async fn get_all_actions(&self) -> Vec<ActionDefinition>;

    /// Execute an action by ID.
    ///
    /// Routes the execution to the appropriate cell/source.
    async fn execute(&self, action_id: ActionId) -> ActionResult;
}
