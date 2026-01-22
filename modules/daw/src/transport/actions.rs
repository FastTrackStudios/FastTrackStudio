//! ActionGroup implementation for Transport
//!
//! Defines the user-invocable actions for transport control.

use actions::{ActionCategory, ActionDefinition, ActionGroup, ActionId};

use super::Transport;

// region:    --- Action Definitions

/// All transport action definitions.
///
/// These are static definitions that describe what actions exist.
/// The actual execution callbacks are provided by the service implementations
/// (ReaperTransport, MockTransport) when registering with the MicroServiceRegistry.
static TRANSPORT_ACTIONS: &[ActionDefinition] = &[
    // -- Playback Control
    ActionDefinition {
        id: ActionId::new("fts.transport.play"),
        name: "Play",
        description: "Start playback",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.pause"),
        name: "Pause",
        description: "Pause playback",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.stop"),
        name: "Stop",
        description: "Stop playback",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.play_pause"),
        name: "Play/Pause",
        description: "Toggle between play and pause",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.play_stop"),
        name: "Play/Stop",
        description: "Toggle between play and stop",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
    // -- Recording
    ActionDefinition {
        id: ActionId::new("fts.transport.record"),
        name: "Record",
        description: "Start recording",
        default_shortcut: None,
        category: ActionCategory::Recording,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.stop_recording"),
        name: "Stop Recording",
        description: "Stop recording",
        default_shortcut: None,
        category: ActionCategory::Recording,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.toggle_recording"),
        name: "Toggle Recording",
        description: "Toggle recording on/off",
        default_shortcut: None,
        category: ActionCategory::Recording,
    },
    // -- Navigation
    ActionDefinition {
        id: ActionId::new("fts.transport.goto_start"),
        name: "Go to Start",
        description: "Move playhead to project start",
        default_shortcut: None,
        category: ActionCategory::Navigation,
    },
    ActionDefinition {
        id: ActionId::new("fts.transport.goto_end"),
        name: "Go to End",
        description: "Move playhead to project end",
        default_shortcut: None,
        category: ActionCategory::Navigation,
    },
    // -- Loop
    ActionDefinition {
        id: ActionId::new("fts.transport.toggle_loop"),
        name: "Toggle Loop",
        description: "Toggle loop playback on/off",
        default_shortcut: None,
        category: ActionCategory::Transport,
    },
];

// endregion: --- Action Definitions

// region:    --- ActionGroup Implementation

impl ActionGroup for Transport {
    const GROUP_NAME: &'static str = "Transport";
    const MENU_PATH: &'static str = "FastTrack/Transport";

    fn action_definitions() -> &'static [ActionDefinition] {
        TRANSPORT_ACTIONS
    }
}

// endregion: --- ActionGroup Implementation
