//! MIDI settings tab — device selection, connection status, action mappings,
//! MIDI learn, and live MIDI monitor.

use dioxus::prelude::*;

use crate::midi_service::{
    cancel_learn, clear_monitor, connect_device, disconnect_device, refresh_devices, start_learn,
    update_action_map, MidiConnectionState, MIDI_CONNECTED_PORT, MIDI_CONNECTION_STATE,
    MIDI_DEVICES, MIDI_LEARN_TARGET, MIDI_MONITOR_LOG,
};
use crate::persistence::{load_midi_config, save_midi_config};

// Glass design tokens (inline style values)
const GLASS_CARD: &str = "border: 1px solid rgba(255,255,255,0.06); background: rgba(24,24,27,0.4);";
const GLASS_BTN: &str = "background: rgba(255,255,255,0.08);";
const GLASS_INPUT: &str = "background: rgba(255,255,255,0.06); border: 1px solid rgba(255,255,255,0.08);";
const GLASS_ROW: &str = "background: rgba(255,255,255,0.03);";
const GLASS_ROW_LEARN: &str = "background: rgba(120,53,15,0.3); border: 1px solid rgba(180,83,9,0.4);";

/// MIDI settings tab component.
#[component]
pub(crate) fn SignalMidiTab() -> Element {
    let mut config = use_signal(|| load_midi_config());

    // Track which device is selected in the dropdown (may differ from connected)
    let mut selected_port = use_signal(|| {
        MIDI_CONNECTED_PORT
            .peek()
            .clone()
            .unwrap_or_default()
    });

    // Reload config when learn completes (action map may have changed)
    let learn_target = MIDI_LEARN_TARGET.read().clone();
    use_effect(move || {
        if MIDI_LEARN_TARGET.read().is_none() {
            config.set(load_midi_config());
        }
    });

    rsx! {
        div { class: "h-full flex flex-col overflow-hidden",
            div { class: "flex-1 overflow-y-auto p-4 space-y-4",
                // ── Header + Refresh ──────────────────────────────────────────
                div { class: "flex items-center justify-between",
                    h2 { class: "text-lg font-semibold text-zinc-100", "MIDI Configuration" }
                    button {
                        class: "px-3 py-1.5 text-xs font-medium text-zinc-300 rounded-md transition-colors",
                        style: GLASS_BTN,
                        onclick: move |_| {
                            refresh_devices();
                        },
                        "Refresh"
                    }
                }

                // ── Device selector + connection ──────────────────────────────
                div {
                    class: "rounded-lg p-4 space-y-3",
                    style: GLASS_CARD,
                    div { class: "flex items-center gap-3",
                        label { class: "text-sm text-zinc-400 w-16 flex-shrink-0", "Device" }
                        select {
                            class: "flex-1 rounded-md px-3 py-1.5 text-sm text-zinc-100 outline-none",
                            style: GLASS_INPUT,
                            value: "{selected_port}",
                            onchange: move |e| {
                                selected_port.set(e.value());
                            },
                            option { value: "", "Select a MIDI device..." }
                            for device in MIDI_DEVICES.read().iter() {
                                option {
                                    value: "{device.name}",
                                    "{device.name}"
                                }
                            }
                        }
                    }

                    div { class: "flex items-center justify-between",
                        div { class: "flex items-center gap-2",
                            span { class: "text-sm text-zinc-400", "Status:" }
                            match *MIDI_CONNECTION_STATE.read() {
                                MidiConnectionState::Connected => rsx! {
                                    span { class: "inline-block w-2 h-2 rounded-full bg-emerald-400" }
                                    span { class: "text-sm text-emerald-300", "Connected" }
                                    if let Some(port) = MIDI_CONNECTED_PORT.read().as_ref() {
                                        span { class: "text-xs text-zinc-500 ml-1", "({port})" }
                                    }
                                },
                                MidiConnectionState::Disconnected => rsx! {
                                    span { class: "inline-block w-2 h-2 rounded-full bg-zinc-500" }
                                    span { class: "text-sm text-zinc-400", "Disconnected" }
                                },
                            }
                        }

                        match *MIDI_CONNECTION_STATE.read() {
                            MidiConnectionState::Connected => rsx! {
                                button {
                                    class: "px-3 py-1.5 text-xs font-medium text-red-300 rounded-md transition-colors",
                                    style: "background: rgba(127,29,29,0.3); border: 1px solid rgba(153,27,27,0.5);",
                                    onclick: move |_| disconnect_device(),
                                    "Disconnect"
                                }
                            },
                            MidiConnectionState::Disconnected => rsx! {
                                button {
                                    class: "px-3 py-1.5 text-xs font-medium text-emerald-300 rounded-md transition-colors disabled:opacity-40 disabled:cursor-not-allowed",
                                    style: "background: rgba(6,78,59,0.3); border: 1px solid rgba(6,95,70,0.5);",
                                    disabled: selected_port().is_empty(),
                                    onclick: move |_| {
                                        let port = selected_port();
                                        if !port.is_empty() {
                                            connect_device(&port);
                                        }
                                    },
                                    "Connect"
                                }
                            },
                        }
                    }
                }

                // ── MIDI Learn banner ─────────────────────────────────────────
                if let Some(ref target) = learn_target {
                    div {
                        class: "rounded-lg p-3 flex items-center justify-between",
                        style: "border: 1px solid rgba(180,83,9,0.6); background: rgba(120,53,15,0.2);",
                        div { class: "flex items-center gap-2",
                            span { class: "inline-block w-2 h-2 rounded-full bg-amber-400 animate-pulse" }
                            span { class: "text-sm text-amber-200",
                                "MIDI Learn: send a MIDI message to assign to "
                            }
                            span { class: "text-sm text-amber-100 font-medium",
                                "{format_action_name(target)}"
                            }
                        }
                        button {
                            class: "px-2 py-1 text-xs text-zinc-400 rounded transition-colors",
                            onclick: move |_| cancel_learn(),
                            "Cancel"
                        }
                    }
                }

                // ── Action Mappings ───────────────────────────────────────────
                div {
                    class: "rounded-lg p-4 space-y-3",
                    style: GLASS_CARD,
                    div { class: "flex items-center justify-between mb-2",
                        h3 { class: "text-sm font-medium text-zinc-200", "Action Mappings" }
                        button {
                            class: "px-2 py-1 text-xs text-zinc-400 rounded transition-colors",
                            onclick: move |_| {
                                let default_map = signal_proto::midi_actions::MidiActionMap::with_defaults();
                                update_action_map(default_map.clone());
                                let mut cfg = config();
                                cfg.action_map = default_map;
                                save_midi_config(&cfg);
                                config.set(cfg);
                            },
                            "Reset to Defaults"
                        }
                    }

                    div { class: "space-y-1",
                        for binding in config().action_map.bindings().iter() {
                            {
                                let action_id = binding.action_id.clone();
                                let is_learning = learn_target.as_deref() == Some(action_id.as_str());
                                rsx! {
                                    div {
                                        class: "flex items-center justify-between py-1.5 px-2 rounded",
                                        style: if is_learning { GLASS_ROW_LEARN } else { GLASS_ROW },
                                        div { class: "flex items-center gap-2 flex-1 min-w-0",
                                            span { class: "text-xs text-zinc-300 font-mono flex-shrink-0",
                                                "{format_trigger(&binding.trigger)}"
                                            }
                                            span { class: "text-xs text-zinc-600", "\u{2192}" }
                                            span { class: "text-xs text-zinc-200 truncate",
                                                "{format_action_name(&binding.action_id)}"
                                            }
                                        }
                                        button {
                                            class: "px-2 py-0.5 text-xs text-cyan-400 rounded transition-colors flex-shrink-0 ml-2",
                                            onclick: {
                                                let aid = action_id.clone();
                                                move |_| start_learn(&aid)
                                            },
                                            "Learn"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ── MIDI Monitor ──────────────────────────────────────────────
                div {
                    class: "rounded-lg p-4 space-y-2",
                    style: GLASS_CARD,
                    div { class: "flex items-center justify-between mb-1",
                        h3 { class: "text-sm font-medium text-zinc-200", "MIDI Monitor" }
                        button {
                            class: "px-2 py-1 text-xs text-zinc-400 rounded transition-colors",
                            onclick: move |_| clear_monitor(),
                            "Clear"
                        }
                    }

                    div { class: "max-h-48 overflow-y-auto space-y-0.5 font-mono text-xs",
                        {
                            let log = MIDI_MONITOR_LOG.read();
                            if log.is_empty() {
                                rsx! {
                                    div { class: "text-zinc-600 py-2 text-center",
                                        "No MIDI messages received"
                                    }
                                }
                            } else {
                                rsx! {
                                    for (i, entry) in log.iter().enumerate() {
                                        div {
                                            key: "{i}",
                                            class: "flex items-center gap-2 py-0.5 px-1",
                                            span { class: "text-zinc-400", "{entry.message}" }
                                            if let Some(ref action) = entry.matched_action {
                                                span { class: "text-emerald-400", "\u{2192} {format_action_name(action)}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Format a `MidiActionTrigger` for display.
fn format_trigger(trigger: &signal_proto::midi_actions::MidiActionTrigger) -> String {
    use signal_proto::midi_actions::{CcThreshold, MidiActionTrigger};

    match trigger {
        MidiActionTrigger::NoteOn { channel, note } => {
            let ch = channel.map(|c| format!("ch{}", c + 1)).unwrap_or_else(|| "any".into());
            format!("NoteOn {} ({})", note, ch)
        }
        MidiActionTrigger::NoteOff { channel, note } => {
            let ch = channel.map(|c| format!("ch{}", c + 1)).unwrap_or_else(|| "any".into());
            format!("NoteOff {} ({})", note, ch)
        }
        MidiActionTrigger::ControlChange { channel, cc, threshold } => {
            let ch = channel.map(|c| format!("ch{}", c + 1)).unwrap_or_else(|| "any".into());
            let thresh = match threshold {
                CcThreshold::ButtonHigh => ">=64",
                CcThreshold::ButtonAny => ">0",
            };
            format!("CC {} ({}) {}", cc, ch, thresh)
        }
        MidiActionTrigger::ProgramChange { channel, program } => {
            let ch = channel.map(|c| format!("ch{}", c + 1)).unwrap_or_else(|| "any".into());
            format!("PC {} ({})", program, ch)
        }
    }
}

/// Format an action ID into a human-readable name.
fn format_action_name(action_id: &str) -> String {
    match action_id {
        "fts.signal.next_song" => "Next Collection".into(),
        "fts.signal.previous_song" => "Previous Collection".into(),
        "fts.signal.next_section" => "Next Section".into(),
        "fts.signal.previous_section" => "Previous Section".into(),
        id if id.starts_with("fts.signal.load_variant.") => {
            if let Some(n) = id.strip_prefix("fts.signal.load_variant.") {
                format!("Load Variant {n}")
            } else {
                action_id.into()
            }
        }
        other => other.into(),
    }
}
