//! ActionGroup trait - types that provide a set of related actions

use crate::ActionDefinition;

/// Trait for types that define a group of related actions.
///
/// This trait is implemented by domain state types (like `Transport`, `Tracks`, etc.)
/// to declare what actions they support. The trait only provides action DEFINITIONS
/// (metadata like id, name, shortcut) - the actual execution callbacks are provided
/// separately by the service implementations.
///
/// # Design Philosophy
///
/// - **State types define actions**: The domain struct declares what actions exist
/// - **Services provide execution**: `ReaperTransport`, `MockTransport` provide the callbacks
/// - **Registry binds them**: `MicroServiceRegistry` combines definitions + executors
///
/// # Example
///
/// ```ignore
/// use actions::{ActionDefinition, ActionGroup, ActionId, ActionCategory};
///
/// #[derive(Clone, Default)]
/// pub struct Transport {
///     pub play_state: PlayState,
///     pub tempo: Tempo,
///     // ... pure state
/// }
///
/// impl ActionGroup for Transport {
///     const GROUP_NAME: &'static str = "Transport";
///     const MENU_PATH: &'static str = "FastTrack/Transport";
///
///     fn action_definitions() -> &'static [ActionDefinition] {
///         static DEFINITIONS: &[ActionDefinition] = &[
///             ActionDefinition::new(
///                 ActionId::new("fts.transport.play"),
///                 "Play",
///                 "Start playback",
///             ).with_category(ActionCategory::Transport),
///             ActionDefinition::new(
///                 ActionId::new("fts.transport.stop"),
///                 "Stop",
///                 "Stop playback",
///             ).with_category(ActionCategory::Transport),
///         ];
///         DEFINITIONS
///     }
/// }
/// ```
pub trait ActionGroup {
    /// Human-readable name for this group (e.g., "Transport", "Tracks")
    const GROUP_NAME: &'static str;

    /// Menu path for REAPER's Swell menus (e.g., "FastTrack/Transport")
    const MENU_PATH: &'static str;

    /// Returns the static list of action definitions for this group.
    ///
    /// These are just metadata - the execution callbacks are provided separately
    /// when registering with the `MicroServiceRegistry`.
    fn action_definitions() -> &'static [ActionDefinition];
}
