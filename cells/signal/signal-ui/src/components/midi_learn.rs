//! MIDI Learn and CC control components.
//!
//! Provides UI components for MIDI CC mapping, expression pedal assignment,
//! and activity monitoring:
//!
//! - [`MidiLearnButton`] — toggleable button that enters MIDI learn mode
//! - [`MidiCcSettings`] — panel for editing curve, range, and CC assignment
//! - [`MidiActivityIndicator`] — small visual showing incoming CC values
//!
//! # Usage
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────┐
//! │  [MIDI Learn]  ●  Ch1 CC#11  [Linear ▾]  0.0 ─ 1.0    │
//! └─────────────────────────────────────────────────────────┘
//! ```

use crate::prelude::*;
use crate::signals::{MIDI_CC_MAPPINGS, MIDI_LAST_CC, MIDI_LEARN_ACTIVE, MIDI_LEARN_STATE};
use signal_control::midi::{CcCurve, MidiCcMapping, MidiLearnState, MidiTarget};

// ─────────────────────────────────────────────────────────────────────────────
// MidiLearnButton
// ─────────────────────────────────────────────────────────────────────────────

/// Props for the MIDI learn toggle button.
#[derive(Props, Clone, PartialEq)]
pub struct MidiLearnButtonProps {
    /// What the learned CC will control.
    pub target: MidiTarget,

    /// Optional compact mode (smaller text, no label).
    #[props(default = false)]
    pub compact: bool,
}

/// A toggleable button that enters MIDI learn mode.
///
/// When activated, the button pulses to indicate listening state. The next
/// incoming CC message (visible via `MIDI_LAST_CC`) is captured and used to
/// create a new mapping or update an existing one for the given target.
///
/// ```text
/// Idle:      [MIDI Learn]        — neutral gray
/// Listening: [● Listening...]    — pulsing red
/// Captured:  [Ch1 CC#11 ✓]      — green flash, then returns to idle
/// ```
#[component]
pub fn MidiLearnButton(props: MidiLearnButtonProps) -> Element {
    let learn_state = MIDI_LEARN_STATE.read();
    let is_listening = learn_state.is_listening();
    let is_captured = learn_state.is_captured();

    let target = props.target.clone();
    let compact = props.compact;

    // Determine button appearance based on state
    let (label, btn_class) = if is_listening {
        (
            if compact {
                "...".to_string()
            } else {
                "Listening...".to_string()
            },
            "flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium \
             bg-red-900/60 text-red-300 border border-red-500/60 \
             hover:bg-red-800/60 transition-colors animate-pulse",
        )
    } else if is_captured {
        let captured_text = if let MidiLearnState::Captured {
            channel, cc_number, ..
        } = &*learn_state
        {
            if compact {
                format!("CC#{cc_number}")
            } else {
                format!("Ch{} CC#{}", channel + 1, cc_number)
            }
        } else {
            "Captured".to_string()
        };
        (
            captured_text,
            "flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium \
             bg-green-900/60 text-green-300 border border-green-500/60 \
             transition-colors",
        )
    } else {
        (
            if compact {
                "MIDI".to_string()
            } else {
                "MIDI Learn".to_string()
            },
            "flex items-center gap-1.5 px-2.5 py-1 rounded-md text-xs font-medium \
             bg-zinc-800 text-zinc-400 border border-zinc-700 \
             hover:bg-zinc-700 hover:text-zinc-300 transition-colors",
        )
    };

    rsx! {
        button {
            class: btn_class,
            title: "Click to enter MIDI learn mode",
            onclick: move |_| {
                if is_listening {
                    // Cancel learn mode
                    *MIDI_LEARN_ACTIVE.write() = false;
                    *MIDI_LEARN_STATE.write() = MidiLearnState::Idle;
                } else {
                    // Enter learn mode
                    *MIDI_LEARN_ACTIVE.write() = true;
                    *MIDI_LEARN_STATE.write() = MidiLearnState::Listening {
                        target: target.clone(),
                    };
                }
            },
            // Listening indicator dot
            if is_listening {
                span {
                    class: "w-2 h-2 rounded-full bg-red-400",
                }
            }
            span { "{label}" }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiCcSettings
// ─────────────────────────────────────────────────────────────────────────────

/// Props for the MIDI CC settings panel.
#[derive(Props, Clone, PartialEq)]
pub struct MidiCcSettingsProps {
    /// The mapping to display/edit.
    pub mapping: MidiCcMapping,

    /// Callback when the mapping is updated.
    pub on_update: Callback<MidiCcMapping>,

    /// Callback when the mapping is removed.
    pub on_remove: Callback<uuid::Uuid>,
}

/// Panel displaying MIDI CC mapping settings: curve, min/max range, CC number.
///
/// ```text
/// ┌──────────────────────────────────────────────────┐
/// │  Ch1 CC#11 → Morph Slider                   [x] │
/// │  Curve: [Linear ▾]                               │
/// │  Range: [0.0 ━━━━━━━━━━━━━━━━━━━━━━ 1.0]        │
/// └──────────────────────────────────────────────────┘
/// ```
#[component]
pub fn MidiCcSettings(props: MidiCcSettingsProps) -> Element {
    let mapping = props.mapping.clone();
    let mapping_id = mapping.id;
    let on_update = props.on_update.clone();
    let on_remove = props.on_remove.clone();

    let header_text = mapping.summary();
    let current_curve = mapping.curve;
    let min_pct = (mapping.min_value * 100.0).round() as i64;
    let max_pct = (mapping.max_value * 100.0).round() as i64;

    rsx! {
        div { class: "flex flex-col gap-2 p-2.5 rounded-lg bg-zinc-800/60 border border-zinc-700/50",
            // Header row: summary + remove button
            div { class: "flex items-center justify-between",
                span { class: "text-xs font-medium text-zinc-300", "{header_text}" }
                button {
                    class: "w-5 h-5 flex items-center justify-center rounded text-zinc-500 \
                            hover:text-red-400 hover:bg-red-900/30 transition-colors text-xs",
                    title: "Remove mapping",
                    onclick: move |_| {
                        on_remove.call(mapping_id);
                    },
                    "x"
                }
            }

            // Curve selector
            div { class: "flex items-center gap-2",
                span { class: "text-[11px] text-zinc-500 w-12", "Curve" }
                div { class: "flex gap-1",
                    for curve in CcCurve::all().iter() {
                        {
                            let c = *curve;
                            let is_selected = c == current_curve;
                            let mapping_for_curve = mapping.clone();
                            let on_update_for_curve = on_update.clone();
                            let btn_class = if is_selected {
                                "px-2 py-0.5 rounded text-[10px] font-medium \
                                 bg-blue-900/50 text-blue-300 border border-blue-700/50"
                            } else {
                                "px-2 py-0.5 rounded text-[10px] font-medium \
                                 bg-zinc-700/50 text-zinc-400 border border-zinc-600/30 \
                                 hover:bg-zinc-600/50 transition-colors"
                            };
                            rsx! {
                                button {
                                    key: "{c:?}",
                                    class: btn_class,
                                    onclick: move |_| {
                                        let mut updated = mapping_for_curve.clone();
                                        updated.curve = c;
                                        on_update_for_curve.call(updated);
                                    },
                                    "{c.display_name()}"
                                }
                            }
                        }
                    }
                }
            }

            // Range sliders
            div { class: "flex items-center gap-2",
                span { class: "text-[11px] text-zinc-500 w-12", "Range" }
                div { class: "flex-1 flex items-center gap-2",
                    span { class: "text-[10px] text-zinc-500 font-mono w-8 text-right",
                        "{min_pct}%"
                    }
                    div { class: "flex-1 flex items-center gap-1",
                        {
                            let mapping_for_min = mapping.clone();
                            let on_update_for_min = on_update.clone();
                            rsx! {
                                input {
                                    r#type: "range",
                                    class: "flex-1 h-1 appearance-none bg-zinc-600 rounded cursor-pointer",
                                    min: "0",
                                    max: "100",
                                    value: "{min_pct}",
                                    oninput: move |evt| {
                                        if let Ok(val) = evt.value().parse::<f64>() {
                                            let mut updated = mapping_for_min.clone();
                                            updated.min_value = val / 100.0;
                                            on_update_for_min.call(updated);
                                        }
                                    },
                                }
                            }
                        }
                        {
                            let mapping_for_max = mapping.clone();
                            let on_update_for_max = on_update.clone();
                            rsx! {
                                input {
                                    r#type: "range",
                                    class: "flex-1 h-1 appearance-none bg-zinc-600 rounded cursor-pointer",
                                    min: "0",
                                    max: "100",
                                    value: "{max_pct}",
                                    oninput: move |evt| {
                                        if let Ok(val) = evt.value().parse::<f64>() {
                                            let mut updated = mapping_for_max.clone();
                                            updated.max_value = val / 100.0;
                                            on_update_for_max.call(updated);
                                        }
                                    },
                                }
                            }
                        }
                    }
                    span { class: "text-[10px] text-zinc-500 font-mono w-8",
                        "{max_pct}%"
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiActivityIndicator
// ─────────────────────────────────────────────────────────────────────────────

/// Props for the MIDI activity indicator.
#[derive(Props, Clone, PartialEq)]
pub struct MidiActivityIndicatorProps {
    /// Optional filter: only show activity for this channel/CC pair.
    /// If `None`, shows activity for any incoming CC.
    #[props(default)]
    pub filter_channel: Option<u8>,

    #[props(default)]
    pub filter_cc: Option<u8>,

    /// Display mode: "dot" (small circle) or "bar" (horizontal bar).
    #[props(default = "dot".to_string())]
    pub mode: String,
}

/// Small visual indicator showing incoming MIDI CC activity.
///
/// - **Dot mode**: A small colored circle that lights up on CC activity.
/// - **Bar mode**: A thin horizontal bar showing the current CC value.
///
/// Reads from `MIDI_LAST_CC` global signal to determine activity.
#[component]
pub fn MidiActivityIndicator(props: MidiActivityIndicatorProps) -> Element {
    let last_cc = MIDI_LAST_CC.read();

    // Determine if we should show activity and what value
    let (is_active, value_normalized) = match *last_cc {
        Some((ch, cc, val)) => {
            let channel_match = props.filter_channel.map(|f| f == ch).unwrap_or(true);
            let cc_match = props.filter_cc.map(|f| f == cc).unwrap_or(true);
            if channel_match && cc_match {
                (true, val as f64 / 127.0)
            } else {
                (false, 0.0)
            }
        }
        None => (false, 0.0),
    };

    let value_pct = (value_normalized * 100.0).round();

    if props.mode == "bar" {
        // Bar mode: thin horizontal level meter
        let bar_color = if is_active {
            "bg-green-400"
        } else {
            "bg-zinc-600"
        };

        rsx! {
            div {
                class: "h-1.5 w-12 rounded-full bg-zinc-700/50 overflow-hidden",
                title: if is_active { format!("CC value: {value_pct}%") } else { "No MIDI activity".to_string() },
                div {
                    class: "h-full rounded-full transition-all duration-75 {bar_color}",
                    style: "width: {value_pct}%",
                }
            }
        }
    } else {
        // Dot mode: small colored indicator circle
        let dot_class = if is_active {
            "w-2 h-2 rounded-full bg-green-400 shadow-[0_0_4px_rgba(74,222,128,0.6)] transition-all duration-75"
        } else {
            "w-2 h-2 rounded-full bg-zinc-600 transition-all duration-300"
        };

        rsx! {
            span {
                class: dot_class,
                title: if is_active { format!("CC value: {value_pct}%") } else { "No MIDI activity".to_string() },
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiCcMappingList
// ─────────────────────────────────────────────────────────────────────────────

/// A list of all active MIDI CC mappings with edit/remove controls.
///
/// Reads from `MIDI_CC_MAPPINGS` global signal. Useful as a section in
/// a settings panel or sidebar.
#[component]
pub fn MidiCcMappingList() -> Element {
    let mappings = MIDI_CC_MAPPINGS.read();

    rsx! {
        div { class: "flex flex-col gap-2",
            // Header
            div { class: "flex items-center justify-between",
                span { class: "text-xs font-semibold text-zinc-400 uppercase tracking-wide",
                    "MIDI Mappings"
                }
                span { class: "text-[10px] text-zinc-500",
                    "{mappings.len()} active"
                }
            }

            if mappings.is_empty() {
                div { class: "text-xs text-zinc-500 italic py-2",
                    "No MIDI CC mappings. Use MIDI Learn to assign controllers."
                }
            }

            for mapping in mappings.iter() {
                {
                    let m = mapping.clone();
                    let m_id = m.id;
                    rsx! {
                        MidiCcSettings {
                            key: "{m_id}",
                            mapping: m,
                            on_update: move |updated: MidiCcMapping| {
                                let mut mappings = MIDI_CC_MAPPINGS.write();
                                if let Some(existing) = mappings.iter_mut().find(|m| m.id == updated.id) {
                                    *existing = updated;
                                }
                            },
                            on_remove: move |id: uuid::Uuid| {
                                MIDI_CC_MAPPINGS.write().retain(|m| m.id != id);
                            },
                        }
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Helper: process incoming MIDI CC for learn mode and active mappings
// ─────────────────────────────────────────────────────────────────────────────

/// Process an incoming MIDI CC message.
///
/// Updates `MIDI_LAST_CC`, handles MIDI learn capture, and evaluates active
/// mappings that target the morph slider.
///
/// Call this from whatever MIDI input handler receives raw CC events.
pub fn handle_midi_cc(channel: u8, cc_number: u8, value: u8) {
    // Update last CC signal for activity indicators
    *MIDI_LAST_CC.write() = Some((channel, cc_number, value));

    // Handle MIDI learn capture
    let learn_state = MIDI_LEARN_STATE.read().clone();
    if let MidiLearnState::Listening { target } = learn_state {
        // Capture this CC as the learned assignment
        *MIDI_LEARN_STATE.write() = MidiLearnState::Captured {
            target: target.clone(),
            channel,
            cc_number,
        };
        *MIDI_LEARN_ACTIVE.write() = false;

        // Create mapping and add it (replacing any existing mapping for the same target)
        let new_mapping = MidiCcMapping::new(channel, cc_number, target.clone());
        let mut mappings = MIDI_CC_MAPPINGS.write();

        // Remove any existing mapping for the same target
        mappings.retain(|m| m.target != target);
        mappings.push(new_mapping);
        return;
    }

    // Evaluate active mappings for morph slider control
    let mappings = MIDI_CC_MAPPINGS.read();
    for mapping in mappings.iter() {
        if mapping.channel == channel && mapping.cc_number == cc_number {
            if let MidiTarget::MorphSlider = &mapping.target {
                let position = mapping.process(value);
                *crate::components::daw_snapshot_panel::MORPH_POSITION.write() = position;
            }
            // Parameter targets would be handled here when DAW parameter
            // write-back is wired up.
        }
    }
}
