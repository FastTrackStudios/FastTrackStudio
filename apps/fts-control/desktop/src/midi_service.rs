//! MIDI device integration service.
//!
//! Connects to system MIDI input devices via `midir`, parses incoming messages
//! with `helgoboss-midi`, and dispatches matched actions through the app's
//! action system. An mpsc channel bridges the CoreMIDI callback thread to the
//! tokio/Dioxus runtime.

use std::sync::Mutex;

use dioxus::prelude::*;
use helgoboss_midi::{RawShortMessage, ShortMessage, ShortMessageFactory, StructuredShortMessage, U7};
use midir::{MidiInput, MidiInputConnection};
use signal_proto::midi_actions::{CcThreshold, MidiActionMap, MidiActionTrigger};
use tokio::sync::mpsc;
use tracing::{debug, info, warn};

use crate::actions::dispatch_action;
use crate::persistence::{load_midi_config, save_midi_config};

// ── Constants ───────────────────────────────────────────────────────────────

const MAX_MONITOR_ENTRIES: usize = 64;

// ── Global signals ──────────────────────────────────────────────────────────

/// Available MIDI input devices.
pub static MIDI_DEVICES: GlobalSignal<Vec<MidiDeviceInfo>> = Signal::global(Vec::new);

/// Name of the currently connected MIDI port (if any).
pub static MIDI_CONNECTED_PORT: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Current connection state.
pub static MIDI_CONNECTION_STATE: GlobalSignal<MidiConnectionState> =
    Signal::global(|| MidiConnectionState::Disconnected);

/// Last received MIDI message (for debug display in the UI).
pub static MIDI_LAST_MESSAGE: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Scrolling MIDI monitor log (newest first, capped at MAX_MONITOR_ENTRIES).
pub static MIDI_MONITOR_LOG: GlobalSignal<Vec<MidiMonitorEntry>> = Signal::global(Vec::new);

/// MIDI learn state: when `Some(action_id)`, the next incoming MIDI message
/// will be bound to that action instead of being dispatched.
pub static MIDI_LEARN_TARGET: GlobalSignal<Option<String>> = Signal::global(|| None);

/// The active action map (shared with the callback thread).
static MIDI_ACTION_MAP: Mutex<Option<MidiActionMap>> = Mutex::new(None);

/// The active MIDI connection handle. Dropping this closes the port.
static MIDI_CONNECTION: Mutex<Option<MidiInputConnection<()>>> = Mutex::new(None);

/// Sender for the action dispatch channel (set once at service start).
static MIDI_ACTION_TX: Mutex<Option<mpsc::UnboundedSender<MidiDispatch>>> = Mutex::new(None);

// ── Types ───────────────────────────────────────────────────────────────────

/// Information about a discovered MIDI input port.
#[derive(Debug, Clone, PartialEq)]
pub struct MidiDeviceInfo {
    pub port_index: usize,
    pub name: String,
}

/// Connection state for the MIDI service.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MidiConnectionState {
    Disconnected,
    Connected,
}

/// An entry in the MIDI monitor log.
#[derive(Debug, Clone, PartialEq)]
pub struct MidiMonitorEntry {
    /// Human-readable description (e.g. "CC 101 ch1 val=127").
    pub message: String,
    /// The action that was dispatched (if any).
    pub matched_action: Option<String>,
}

/// A message from the MIDI callback thread → tokio dispatch task.
enum MidiDispatch {
    /// An action matched a MIDI trigger — dispatch it.
    Action(String),
    /// A MIDI message was received — log it to the monitor.
    Monitor {
        description: String,
        trigger: Option<MidiActionTrigger>,
    },
}

// ── Public API ──────────────────────────────────────────────────────────────

/// Enumerate available MIDI input ports and update `MIDI_DEVICES`.
pub fn refresh_devices() {
    let devices = match MidiInput::new("fts-control-enumerate") {
        Ok(midi_in) => {
            let ports = midi_in.ports();
            ports
                .iter()
                .enumerate()
                .filter_map(|(i, port)| {
                    midi_in.port_name(port).ok().map(|name| MidiDeviceInfo {
                        port_index: i,
                        name,
                    })
                })
                .collect()
        }
        Err(e) => {
            warn!("Failed to create MidiInput for enumeration: {e}");
            Vec::new()
        }
    };
    debug!("MIDI devices found: {}", devices.len());
    *MIDI_DEVICES.write() = devices;
}

/// Connect to a MIDI input port by name.
///
/// Creates a fresh `MidiInput` (midir consumes it on connect), finds the
/// matching port, and installs the message callback that parses and dispatches.
pub fn connect_device(port_name: &str) {
    // Disconnect any existing connection first
    disconnect_device();

    let midi_in = match MidiInput::new("fts-control-input") {
        Ok(m) => m,
        Err(e) => {
            warn!("Failed to create MidiInput: {e}");
            *MIDI_CONNECTION_STATE.write() = MidiConnectionState::Disconnected;
            return;
        }
    };

    let ports = midi_in.ports();
    let target = ports.iter().find(|p| {
        midi_in
            .port_name(p)
            .map(|n| n == port_name)
            .unwrap_or(false)
    });

    let Some(port) = target else {
        warn!("MIDI port '{port_name}' not found");
        *MIDI_CONNECTION_STATE.write() = MidiConnectionState::Disconnected;
        return;
    };

    let port_name_owned = port_name.to_string();

    // The callback runs on a CoreMIDI thread — send through the mpsc channel.
    let connection = midi_in.connect(
        port,
        "fts-control",
        move |_timestamp, data, _| {
            handle_midi_message(data);
        },
        (),
    );

    match connection {
        Ok(conn) => {
            info!("Connected to MIDI device: {port_name_owned}");
            *MIDI_CONNECTION.lock().unwrap() = Some(conn);
            *MIDI_CONNECTED_PORT.write() = Some(port_name_owned.clone());
            *MIDI_CONNECTION_STATE.write() = MidiConnectionState::Connected;

            // Persist selection
            let mut config = load_midi_config();
            config.selected_port_name = Some(port_name_owned);
            save_midi_config(&config);
        }
        Err(e) => {
            warn!("Failed to connect to MIDI device '{port_name_owned}': {e}");
            *MIDI_CONNECTION_STATE.write() = MidiConnectionState::Disconnected;
        }
    }
}

/// Disconnect the current MIDI device (if connected).
pub fn disconnect_device() {
    let had_connection = MIDI_CONNECTION.lock().unwrap().take().is_some();
    if had_connection {
        info!("Disconnected MIDI device");
    }
    *MIDI_CONNECTED_PORT.write() = None;
    *MIDI_CONNECTION_STATE.write() = MidiConnectionState::Disconnected;
}

/// Start MIDI learn mode for the given action ID.
/// The next MIDI message received will be bound to this action.
pub fn start_learn(action_id: &str) {
    info!("MIDI learn started for action: {action_id}");
    *MIDI_LEARN_TARGET.write() = Some(action_id.to_string());
}

/// Cancel MIDI learn mode without binding.
pub fn cancel_learn() {
    *MIDI_LEARN_TARGET.write() = None;
}

/// Clear the MIDI monitor log.
pub fn clear_monitor() {
    MIDI_MONITOR_LOG.write().clear();
}

// ── Message handling (runs on CoreMIDI thread) ──────────────────────────────

fn handle_midi_message(data: &[u8]) {
    if data.len() < 2 {
        return;
    }

    // Parse raw bytes into a structured MIDI message
    let byte1 = U7::new(data.get(1).copied().unwrap_or(0));
    let byte2 = U7::new(data.get(2).copied().unwrap_or(0));
    let raw = match RawShortMessage::from_bytes((data[0], byte1, byte2)) {
        Ok(msg) => msg,
        Err(_) => return,
    };

    let structured = raw.to_structured();
    let tx = MIDI_ACTION_TX.lock().unwrap();
    let Some(tx) = tx.as_ref() else { return };

    // Build debug description and extract the primary trigger for learn/monitor
    let debug_desc = format_midi_message(&structured);
    let triggers = triggers_from_structured(&structured);
    let primary_trigger = triggers.first().cloned();

    // Send monitor entry (always, regardless of learn mode)
    let _ = tx.send(MidiDispatch::Monitor {
        description: debug_desc,
        trigger: primary_trigger,
    });

    // Look up action in the map and dispatch
    let action_map = MIDI_ACTION_MAP.lock().unwrap();
    let Some(map) = action_map.as_ref() else {
        return;
    };

    for trigger in &triggers {
        if let Some(action_id) = map.find(trigger) {
            let _ = tx.send(MidiDispatch::Action(action_id.to_string()));
            return; // Fire only the first matching action
        }
    }
}

/// Convert a structured MIDI message into candidate `MidiActionTrigger`s.
///
/// Returns multiple triggers to try (channel-specific first, then omni), so
/// the action map can match either.
fn triggers_from_structured(msg: &StructuredShortMessage) -> Vec<MidiActionTrigger> {
    let mut triggers = Vec::with_capacity(4);

    match msg {
        StructuredShortMessage::NoteOn {
            channel,
            key_number,
            velocity,
        } => {
            let ch = u8::from(*channel);
            let note = u8::from(*key_number);
            let vel = u8::from(*velocity);
            if vel > 0 {
                triggers.push(MidiActionTrigger::NoteOn {
                    channel: Some(ch),
                    note,
                });
                triggers.push(MidiActionTrigger::NoteOn {
                    channel: None,
                    note,
                });
            } else {
                triggers.push(MidiActionTrigger::NoteOff {
                    channel: Some(ch),
                    note,
                });
                triggers.push(MidiActionTrigger::NoteOff {
                    channel: None,
                    note,
                });
            }
        }
        StructuredShortMessage::NoteOff {
            channel,
            key_number,
            ..
        } => {
            let ch = u8::from(*channel);
            let note = u8::from(*key_number);
            triggers.push(MidiActionTrigger::NoteOff {
                channel: Some(ch),
                note,
            });
            triggers.push(MidiActionTrigger::NoteOff {
                channel: None,
                note,
            });
        }
        StructuredShortMessage::ControlChange {
            channel,
            controller_number,
            control_value,
        } => {
            let ch = u8::from(*channel);
            let cc = u8::from(*controller_number);
            let val = u8::from(*control_value);

            if val >= 64 {
                triggers.push(MidiActionTrigger::ControlChange {
                    channel: Some(ch),
                    cc,
                    threshold: CcThreshold::ButtonHigh,
                });
                triggers.push(MidiActionTrigger::ControlChange {
                    channel: None,
                    cc,
                    threshold: CcThreshold::ButtonHigh,
                });
            }
            if val > 0 {
                triggers.push(MidiActionTrigger::ControlChange {
                    channel: Some(ch),
                    cc,
                    threshold: CcThreshold::ButtonAny,
                });
                triggers.push(MidiActionTrigger::ControlChange {
                    channel: None,
                    cc,
                    threshold: CcThreshold::ButtonAny,
                });
            }
        }
        StructuredShortMessage::ProgramChange {
            channel,
            program_number,
        } => {
            let ch = u8::from(*channel);
            let prog = u8::from(*program_number);
            triggers.push(MidiActionTrigger::ProgramChange {
                channel: Some(ch),
                program: prog,
            });
            triggers.push(MidiActionTrigger::ProgramChange {
                channel: None,
                program: prog,
            });
        }
        _ => {}
    }

    triggers
}

/// Format a MIDI message for the debug display.
fn format_midi_message(msg: &StructuredShortMessage) -> String {
    match msg {
        StructuredShortMessage::NoteOn {
            channel,
            key_number,
            velocity,
        } => format!(
            "NoteOn ch{} note={} vel={}",
            u8::from(*channel) + 1,
            u8::from(*key_number),
            u8::from(*velocity)
        ),
        StructuredShortMessage::NoteOff {
            channel,
            key_number,
            velocity,
        } => format!(
            "NoteOff ch{} note={} vel={}",
            u8::from(*channel) + 1,
            u8::from(*key_number),
            u8::from(*velocity)
        ),
        StructuredShortMessage::ControlChange {
            channel,
            controller_number,
            control_value,
        } => format!(
            "CC {} ch{} val={}",
            u8::from(*controller_number),
            u8::from(*channel) + 1,
            u8::from(*control_value)
        ),
        StructuredShortMessage::ProgramChange {
            channel,
            program_number,
        } => format!(
            "PC {} ch{}",
            u8::from(*program_number),
            u8::from(*channel) + 1
        ),
        other => format!("{:?}", other),
    }
}

// ── Service entry point ─────────────────────────────────────────────────────

/// Run the MIDI service. Call from a Dioxus `use_future` or `spawn` context
/// so that `dispatch_action()` (which calls Dioxus `spawn()`) works correctly.
pub async fn run_midi_service() {
    let config = load_midi_config();
    if !config.enabled {
        info!("MIDI service disabled in config");
        return;
    }

    // Install the action map
    *MIDI_ACTION_MAP.lock().unwrap() = Some(config.action_map.clone());

    // Set up the mpsc channel for callback→dispatch bridging
    let (tx, mut rx) = mpsc::unbounded_channel::<MidiDispatch>();
    *MIDI_ACTION_TX.lock().unwrap() = Some(tx);

    // Initial device enumeration
    refresh_devices();

    // Auto-connect if a port was previously selected
    if let Some(ref port_name) = config.selected_port_name {
        info!("Auto-connecting to saved MIDI port: {port_name}");
        connect_device(port_name);
    }

    // Dispatch loop — receives actions from the CoreMIDI callback thread
    while let Some(dispatch) = rx.recv().await {
        match dispatch {
            MidiDispatch::Action(action_id) => {
                debug!("MIDI → dispatch action: {action_id}");
                // Annotate the most recent monitor entry with the matched action
                {
                    let mut log = MIDI_MONITOR_LOG.write();
                    if let Some(entry) = log.first_mut() {
                        entry.matched_action = Some(action_id.clone());
                    }
                }
                dispatch_action(&action_id);
            }
            MidiDispatch::Monitor {
                description,
                trigger,
            } => {
                // Update last message
                *MIDI_LAST_MESSAGE.write() = Some(description.clone());

                // Check if we're in MIDI learn mode
                let learn_target = MIDI_LEARN_TARGET.peek().clone();
                if let (Some(action_id), Some(trigger)) = (learn_target, trigger) {
                    // Bind this trigger to the learn target action
                    info!("MIDI learn: binding {description} → {action_id}");
                    let mut map_guard = MIDI_ACTION_MAP.lock().unwrap();
                    if let Some(map) = map_guard.as_mut() {
                        map.set(trigger, &action_id);
                        // Persist
                        let mut cfg = load_midi_config();
                        cfg.action_map = map.clone();
                        save_midi_config(&cfg);
                    }
                    *MIDI_LEARN_TARGET.write() = None;

                    // Add monitor entry showing the learn binding
                    let mut log = MIDI_MONITOR_LOG.write();
                    log.insert(
                        0,
                        MidiMonitorEntry {
                            message: format!("{description}  [LEARNED]"),
                            matched_action: Some(action_id),
                        },
                    );
                    log.truncate(MAX_MONITOR_ENTRIES);
                } else {
                    // Normal monitor: add entry to log
                    let mut log = MIDI_MONITOR_LOG.write();
                    log.insert(
                        0,
                        MidiMonitorEntry {
                            message: description,
                            matched_action: None,
                        },
                    );
                    log.truncate(MAX_MONITOR_ENTRIES);
                }
            }
        }
    }
}

/// Update the action map at runtime (e.g., after user edits mappings).
pub fn update_action_map(map: MidiActionMap) {
    *MIDI_ACTION_MAP.lock().unwrap() = Some(map);
}
