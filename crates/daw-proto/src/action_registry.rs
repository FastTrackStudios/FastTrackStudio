//! Action Registry — Dynamic REAPER Action Registration
//!
//! Allows integrated extensions and external clients to register custom REAPER
//! actions at runtime.
//! Actions appear in REAPER's action list and can be bound to keyboard shortcuts.
//!
//! When a registered action is triggered (by user hotkey, toolbar button, or
//! script), the host notifies subscribers via a subscription stream. Integrated
//! extensions can subscribe in-process; external clients can subscribe through
//! the socket bridge.
//!
//! # Example (via daw-control)
//!
//! ```rust,ignore
//! let actions = daw.action_registry();
//!
//! // Register a custom action
//! let cmd_id = actions.register("FTS_SIGNAL_ARM", "FTS: Arm Signal Chain").await?;
//!
//! // Subscribe to action triggers
//! let (tx, rx) = daw_control::channel();
//! actions.subscribe_actions(tx).await;
//! while let Some(event) = rx.recv().await {
//!     match event {
//!         ActionEvent::Triggered { command_name } => {
//!             // Handle the action
//!         }
//!     }
//! }
//! ```

use facet::Facet;
use vox::{Tx, service};

/// Events pushed to subscribers when their registered actions are triggered.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ActionEvent {
    /// A registered action was triggered by the user (hotkey, toolbar, script).
    Triggered {
        /// The command name that was registered (e.g., "FTS_SIGNAL_NEXT_SONG").
        command_name: String,
    },
}

/// Action list category used when enumerating REAPER's action list.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub enum ActionListFilter {
    /// Return every action in the main action section.
    All,
    /// Return REAPER built-in actions.
    Reaper,
    /// Return extension, script, and custom actions.
    NonReaper,
    /// Return actions that look like SWS/S&M actions.
    Sws,
    /// Return FastTrackStudio actions.
    Fts,
    /// Return actions registered through this action registry instance.
    Registered,
}

/// Best-effort source classification for an action-list entry.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub enum ActionOrigin {
    /// A built-in REAPER action.
    Reaper,
    /// An SWS/S&M extension action.
    Sws,
    /// A FastTrackStudio action.
    Fts,
    /// Any other named extension, script, or custom action.
    Extension,
}

/// Request parameters for action-list enumeration.
#[derive(Debug, Clone, Facet)]
pub struct ActionListRequest {
    /// Which class of actions to return.
    pub filter: ActionListFilter,
    /// Optional case-insensitive search over description and command name.
    pub query: Option<String>,
    /// Optional maximum number of rows to return.
    pub limit: Option<u32>,
}

impl Default for ActionListRequest {
    fn default() -> Self {
        Self {
            filter: ActionListFilter::All,
            query: None,
            limit: None,
        }
    }
}

/// One action from REAPER's main action list.
#[derive(Debug, Clone, Facet)]
pub struct ActionInfo {
    /// Numeric REAPER command ID.
    pub command_id: u32,
    /// Named command identifier, if REAPER has one.
    ///
    /// REAPER's `ReverseNamedCommandLookup` returns names without the leading
    /// underscore; built-in actions usually have no named identifier.
    pub command_name: Option<String>,
    /// Text shown in REAPER's action list.
    pub description: String,
    /// Best-effort action source classification.
    pub origin: ActionOrigin,
    /// True if this action was registered by the FastTrackStudio registry.
    pub registered_by_fts: bool,
    /// Stored toggle state for FastTrackStudio toggle actions.
    pub toggle_state: Option<bool>,
}

/// Response from action-list enumeration.
#[derive(Debug, Clone, Facet)]
pub struct ActionListResponse {
    /// Total number of actions matching the requested filter/query before limit.
    pub total_count: u32,
    /// Returned action rows after applying limit.
    pub actions: Vec<ActionInfo>,
}

impl Default for ActionListResponse {
    fn default() -> Self {
        Self {
            total_count: 0,
            actions: Vec::new(),
        }
    }
}

/// Detailed result from executing a REAPER action.
#[derive(Debug, Clone, Facet)]
pub struct ActionExecutionResult {
    /// Identifier supplied by the caller.
    pub requested_action: String,
    /// Whether a command was resolved and dispatched.
    pub executed: bool,
    /// Numeric REAPER command ID, if resolved.
    pub command_id: Option<u32>,
    /// Named command identifier, if REAPER has one.
    pub command_name: Option<String>,
    /// Text shown in REAPER's action list, if known.
    pub description: Option<String>,
    /// Best-effort source classification, if known.
    pub origin: Option<ActionOrigin>,
    /// True if this action was registered by the FastTrackStudio registry.
    pub registered_by_fts: bool,
    /// Stored toggle state before dispatch for FastTrackStudio toggle actions.
    pub toggle_state_before: Option<bool>,
    /// Stored toggle state after dispatch for FastTrackStudio toggle actions.
    pub toggle_state_after: Option<bool>,
}

/// Dynamic action registration for DAW extensions and clients.
///
/// Extensions use this service to register REAPER actions at runtime.
/// Registered actions appear in REAPER's action list (Actions > Show action list)
/// and can be assigned keyboard shortcuts by the user.
///
/// After registering, call [`subscribe_actions`] to receive trigger events.
/// The subscriber handles action-specific domain logic.
#[service]
pub trait ActionRegistryService {
    /// Register a new REAPER action.
    ///
    /// - `command_name`: Unique identifier (e.g., "FTS_SIGNAL_ARM"). Must be
    ///   globally unique across all extensions.
    /// - `description`: Human-readable label shown in REAPER's action list.
    /// - `show_in_menu`: If true, the action appears in REAPER's Extensions >
    ///   FastTrackStudio menu. The menu hierarchy is derived from the command
    ///   name prefix (e.g., `FTS_SESSION_*` → Session submenu).
    /// - `toggleable`: If true, REAPER shows an on/off indicator for this action.
    ///   The host flips fake-toggle state synchronously when the action runs;
    ///   use [`set_toggle_state`] to force the state from extension code.
    ///
    /// Returns the numeric command ID assigned by REAPER, or 0 on failure.
    async fn register_action(
        &self,
        command_name: String,
        description: String,
        show_in_menu: bool,
        toggleable: bool,
    ) -> u32;

    /// Unregister a previously registered action.
    ///
    /// Returns `true` if the action was found and unregistered.
    async fn unregister_action(&self, command_name: String) -> bool;

    /// Check if an action is registered (either by us or any other extension).
    ///
    /// Uses REAPER's `NamedCommandLookup` — works for any registered command,
    /// not just ones registered through this service.
    async fn is_registered(&self, command_name: String) -> bool;

    /// Look up the numeric command ID for a named action.
    ///
    /// Returns `None` if the action is not registered.
    async fn lookup_command_id(&self, command_name: String) -> Option<u32>;

    /// Subscribe to action trigger events.
    ///
    /// Streams `ActionEvent::Triggered` whenever a REAPER action registered
    /// through this service is triggered by the user. The guest receives
    /// events for ALL actions registered through this service, not just
    /// its own — filter by `command_name` if needed.
    async fn subscribe_actions(&self, tx: Tx<ActionEvent>);

    /// Check if an action is actually present in REAPER's action list (main section).
    ///
    /// Unlike [`is_registered`], which only checks `NamedCommandLookup` (the command
    /// ID registry), this enumerates REAPER's action list to verify the action's
    /// gaccel entry exists. An action can have a command ID without being in the
    /// action list if the gaccel registration was missed.
    async fn is_in_action_list(&self, command_name: String) -> bool;

    /// Enumerate actions in REAPER's main action list.
    ///
    /// This is the programmatic equivalent of the Actions window for the main
    /// section. Results include built-in REAPER commands, SWS/S&M actions,
    /// scripts/custom actions, and FastTrackStudio registered actions.
    async fn list_actions(&self, request: ActionListRequest) -> ActionListResponse;

    /// Execute a native DAW command by numeric ID.
    ///
    /// This maps to `Main_OnCommandEx(command_id, 0, current_project)`.
    /// Use for REAPER built-in actions (e.g., 40044 for play/stop,
    /// 40157 for insert marker, 40306 for insert region).
    async fn execute_command(&self, command_id: u32);

    /// Execute a named action (custom or native).
    ///
    /// Looks up the command by name (e.g., "_FTS_SIGNAL_ARM") and executes it.
    /// Returns `true` if the command was found and executed.
    async fn execute_named_action(&self, command_name: String) -> bool;

    /// Execute any REAPER action identifier and return resolved metadata.
    ///
    /// Accepts either a numeric command ID or a named command identifier.
    async fn execute_action(&self, action_id: String) -> ActionExecutionResult;

    /// Set the toggle state for a toggleable action.
    ///
    /// Guests call this to update the on/off state that REAPER displays.
    /// REAPER queries toggle state synchronously on the main thread, so the
    /// host stores the state and returns it immediately when asked.
    ///
    /// Has no effect if the action was not registered as toggleable.
    async fn set_toggle_state(&self, command_name: String, is_on: bool);

    /// Get the current toggle state for a toggleable action.
    ///
    /// Returns `None` if the action is not registered or not toggleable.
    async fn get_toggle_state(&self, command_name: String) -> Option<bool>;
}
