//! Audio settings — device / channel / buffer / sample-rate picker with
//! persistence, mirroring the guitar TUI's audio config.
//!
//! signal-ui is domain-agnostic and cannot depend on the audio backend
//! (`signal-sampler` / `daw`). So this module defines plain data types
//! ([`AudioDevice`], [`AudioPrefs`]) and an [`AudioSettingsBridge`] the host app
//! provides via Dioxus context: the app enumerates devices and loads/saves the
//! preferences (e.g. through `RigManager`'s styx file), while this module owns
//! only the presentation.
//!
//! Controls are built from the themed `lumen_blocks` component library (Button,
//! Dropdown) so they follow the active theme rather than hard-coded colors.

use dioxus::prelude::*;
use lumen_blocks::components::button::{Button, ButtonVariant};
use lumen_blocks::components::dropdown::{Dropdown, DropdownContent, DropdownItem, DropdownTrigger};

/// One selectable audio device — plain mirror of the backend's `DeviceInfo`.
#[derive(Clone, PartialEq, Debug)]
pub struct AudioDevice {
    pub name: String,
    pub channels: u16,
    pub default_sample_rate: u32,
}

/// Audio I/O preferences — plain mirror of the backend's `RigAudioPrefs`.
///
/// Empty-string / `0` mean "use the system/backend default" (not `Option`), to
/// match the persisted styx representation.
#[derive(Clone, PartialEq, Debug)]
pub struct AudioPrefs {
    pub input_device: String,
    pub input_channel: usize,
    pub output_device: String,
    pub sample_rate: u32,
    pub buffer_size: u32,
}

impl Default for AudioPrefs {
    fn default() -> Self {
        Self {
            input_device: String::new(),
            input_channel: 0,
            output_device: String::new(),
            sample_rate: 48_000,
            buffer_size: 256,
        }
    }
}

/// Host-provided bridge: enumerated devices, the persisted prefs, and a save
/// callback. The app builds this (once) and injects it via
/// `use_context_provider`. The settings modal reads/edits a copy and calls
/// [`on_save`](AudioSettingsBridge::on_save) to persist.
#[derive(Clone, PartialEq)]
pub struct AudioSettingsBridge {
    pub inputs: Vec<AudioDevice>,
    pub outputs: Vec<AudioDevice>,
    pub prefs: AudioPrefs,
    pub on_save: Callback<AudioPrefs>,
}

/// One footswitch stack (folder) in the performance grid — a named rotation of
/// patches. Plain mirror of the backend's `RigStack` + live cursor/active state.
#[derive(Clone, PartialEq, Debug)]
pub struct PerfStack {
    /// Folder / footswitch name (e.g. "Clean", "Lead").
    pub name: String,
    /// Name of the patch at the current rotation cursor.
    pub current_patch: String,
    /// Cursor position within the rotation (0-based).
    pub position: usize,
    /// Number of patches in the rotation.
    pub patch_count: usize,
    /// Whether the current patch's chain is loaded (preloaded / ready).
    pub available: bool,
    /// Whether this stack holds the currently-active patch (highlight it).
    pub is_active: bool,
}

/// The live performance model: the active profile's footswitch stacks + the
/// global function-switch state (FX bypass, volume boost, tempo).
#[derive(Clone, PartialEq, Debug, Default)]
pub struct PerformanceModel {
    pub profile_name: String,
    pub stacks: Vec<PerfStack>,
    /// Global time/FX bypass engaged.
    pub fx_bypass: bool,
    /// Volume boost engaged.
    pub boost: bool,
    /// Current tempo (BPM) — drives the tap-tempo blink.
    pub tempo_bpm: u32,
}

/// One block in the live active-patch FX chain — the UI's view of engine state.
#[derive(Clone, PartialEq, Debug)]
pub struct LiveBlock {
    /// Stable id used to address the block (bypass / param edits).
    pub id: String,
    /// Block type (for coloring).
    pub block_type: signal::BlockType,
    /// Display name.
    pub name: String,
    /// Whether the block is currently bypassed.
    pub bypassed: bool,
    /// Primary dialable param name (e.g. "mix" / "depth"), if any.
    pub param_name: Option<String>,
    /// Current primary-param value + range (for the inspector knob).
    pub param_value: f32,
    pub param_min: f32,
    pub param_max: f32,
}

/// Host-provided handle to the live audio rig. The app owns the actual
/// `ProfileRig`/`GuitarRig` (which can't live in signal-ui); this bridge lets
/// the UI read meters, drive the footswitch stacks, and re-open the device.
#[derive(Clone, PartialEq)]
pub struct RigAudioHandle {
    /// Open the audio device using the persisted prefs (and reload the profile).
    pub start: Callback<()>,
    /// Close the audio device.
    pub stop: Callback<()>,
    /// Whether the rig is currently running.
    pub is_running: Callback<(), bool>,
    /// Latest input peak (linear 0..1).
    pub input_peak: Callback<(), f32>,
    /// Latest output peak (linear 0..1).
    pub output_peak: Callback<(), f32>,
    /// Display name of the currently-active patch/amp, if any.
    pub loaded_amp: Callback<(), Option<String>>,
    /// Current performance model (profile + footswitch stacks + live state).
    pub perf_model: Callback<(), PerformanceModel>,
    /// Press a footswitch stack (by index): activate current / rotate next.
    pub press_stack: Callback<usize>,
    /// Toggle the global time/FX bypass.
    pub toggle_fx: Callback<()>,
    /// Toggle the volume boost.
    pub toggle_boost: Callback<()>,
    /// Tap tempo (records a tap; sets the patch tempo).
    pub tap_tempo: Callback<()>,
    /// The live active-patch FX chain (blocks + bypass + params).
    pub active_chain: Callback<(), Vec<LiveBlock>>,
    /// Toggle a block's bypass (by id).
    pub toggle_block_bypass: Callback<String>,
    /// Set a block's param: (block id, param name, value).
    pub set_block_param: Callback<(String, String, f32)>,
}

/// Selectable buffer sizes (frames) — matches the guitar TUI's `BUFFERS`.
const BUFFER_SIZES: &[u32] = &[32, 64, 128, 256, 512, 1024];

/// Selectable sample rates (Hz). `0` = device native.
const SAMPLE_RATES: &[(u32, &str)] = &[
    (0, "Device native"),
    (44_100, "44.1 kHz"),
    (48_000, "48 kHz"),
    (88_200, "88.2 kHz"),
    (96_000, "96 kHz"),
];

/// Max input channels to offer when the selected device's channel count is
/// unknown (e.g. the "system default" entry).
const DEFAULT_MAX_CHANNELS: u16 = 8;

/// Human label for a device value (empty string → "System default").
fn device_label(name: &str) -> String {
    if name.is_empty() {
        "System default".to_string()
    } else {
        name.to_string()
    }
}

/// Modal audio-settings page. Edits a local copy of the prefs and persists via
/// the bridge's `on_save` when the user clicks Save.
#[component]
pub fn AudioSettingsModal(bridge: AudioSettingsBridge, on_close: EventHandler<()>) -> Element {
    // Local edit state, seeded from the persisted prefs.
    let mut input_device = use_signal(|| bridge.prefs.input_device.clone());
    let mut input_channel = use_signal(|| bridge.prefs.input_channel);
    let mut output_device = use_signal(|| bridge.prefs.output_device.clone());
    let mut sample_rate = use_signal(|| bridge.prefs.sample_rate);
    let mut buffer_size = use_signal(|| bridge.prefs.buffer_size);

    // Channel count for the currently-selected input device.
    let sel_input = input_device();
    let max_channels = bridge
        .inputs
        .iter()
        .find(|d| d.name == sel_input)
        .map(|d| d.channels)
        .filter(|c| *c > 0)
        .unwrap_or(DEFAULT_MAX_CHANNELS);

    let inputs = bridge.inputs.clone();
    let outputs = bridge.outputs.clone();
    let on_save = bridge.on_save;

    let sample_rate_label = SAMPLE_RATES
        .iter()
        .find(|(hz, _)| *hz == sample_rate())
        .map(|(_, l)| l.to_string())
        .unwrap_or_else(|| format!("{} Hz", sample_rate()));

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center",
            style: "background: rgba(0,0,0,0.55);",
            onclick: move |_| on_close.call(()),

            // Panel (stopPropagation so clicks inside don't close it)
            div {
                class: "w-[460px] max-w-[92vw] rounded-lg border border-border bg-popover text-popover-foreground shadow-2xl",
                onclick: move |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-5 py-3 border-b border-border",
                    h2 { class: "text-sm font-semibold", "Audio Settings" }
                    Button {
                        variant: ButtonVariant::Ghost,
                        is_icon_button: true,
                        aria_label: "Close".to_string(),
                        on_click: move |_| on_close.call(()),
                        "×"
                    }
                }

                // Body
                div { class: "px-5 py-4 space-y-3",

                    // Input device
                    SettingRow { label: "Input device",
                        Dropdown {
                            DropdownTrigger {
                                Button { variant: ButtonVariant::Outline, full_width: true,
                                    span { class: "truncate", "{device_label(&sel_input)}" }
                                }
                            }
                            DropdownContent { width: "w-56".to_string(), class: "max-h-72 overflow-y-auto",
                                DropdownItem {
                                    value: String::new(),
                                    index: 0,
                                    on_select: move |v: String| { input_device.set(v); input_channel.set(0); },
                                    "System default"
                                }
                                for (i, d) in inputs.iter().enumerate() {
                                    DropdownItem {
                                        key: "{i}",
                                        value: d.name.clone(),
                                        index: i + 1,
                                        on_select: move |v: String| { input_device.set(v); input_channel.set(0); },
                                        "{d.name} ({d.channels} ch)"
                                    }
                                }
                            }
                        }
                    }

                    // Input channel
                    SettingRow { label: "Input channel",
                        Dropdown {
                            DropdownTrigger {
                                Button { variant: ButtonVariant::Outline, full_width: true,
                                    "Channel {input_channel() + 1}"
                                }
                            }
                            DropdownContent { width: "w-56".to_string(), class: "max-h-72 overflow-y-auto",
                                for ch in 0..max_channels as usize {
                                    DropdownItem {
                                        key: "{ch}",
                                        value: ch,
                                        index: ch,
                                        on_select: move |v: usize| input_channel.set(v),
                                        "Channel {ch + 1}"
                                    }
                                }
                            }
                        }
                    }

                    // Output device
                    SettingRow { label: "Output device",
                        Dropdown {
                            DropdownTrigger {
                                Button { variant: ButtonVariant::Outline, full_width: true,
                                    span { class: "truncate", "{device_label(&output_device())}" }
                                }
                            }
                            DropdownContent { width: "w-56".to_string(), class: "max-h-72 overflow-y-auto",
                                DropdownItem {
                                    value: String::new(),
                                    index: 0,
                                    on_select: move |v: String| output_device.set(v),
                                    "System default"
                                }
                                for (i, d) in outputs.iter().enumerate() {
                                    DropdownItem {
                                        key: "{i}",
                                        value: d.name.clone(),
                                        index: i + 1,
                                        on_select: move |v: String| output_device.set(v),
                                        "{d.name} ({d.channels} ch)"
                                    }
                                }
                            }
                        }
                    }

                    // Sample rate
                    SettingRow { label: "Sample rate",
                        Dropdown {
                            DropdownTrigger {
                                Button { variant: ButtonVariant::Outline, full_width: true, "{sample_rate_label}" }
                            }
                            DropdownContent { width: "w-56".to_string(),
                                for (idx, (hz, label)) in SAMPLE_RATES.iter().copied().enumerate() {
                                    DropdownItem {
                                        key: "{hz}",
                                        value: hz,
                                        index: idx,
                                        on_select: move |v: u32| sample_rate.set(v),
                                        "{label}"
                                    }
                                }
                            }
                        }
                    }

                    // Buffer size
                    SettingRow { label: "Buffer size",
                        Dropdown {
                            DropdownTrigger {
                                Button { variant: ButtonVariant::Outline, full_width: true, "{buffer_size()} frames" }
                            }
                            DropdownContent { width: "w-56".to_string(),
                                for (idx, frames) in BUFFER_SIZES.iter().copied().enumerate() {
                                    DropdownItem {
                                        key: "{frames}",
                                        value: frames,
                                        index: idx,
                                        on_select: move |v: u32| buffer_size.set(v),
                                        "{frames} frames"
                                    }
                                }
                            }
                        }
                    }
                }

                // Footer
                div { class: "flex items-center justify-end gap-2 px-5 py-3 border-t border-border",
                    Button {
                        variant: ButtonVariant::Ghost,
                        on_click: move |_| on_close.call(()),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: move |_| {
                            let prefs = AudioPrefs {
                                input_device: input_device(),
                                input_channel: input_channel(),
                                output_device: output_device(),
                                sample_rate: sample_rate(),
                                buffer_size: buffer_size(),
                            };
                            on_save.call(prefs);
                            on_close.call(());
                        },
                        "Save"
                    }
                }
            }
        }
    }
}

/// A labelled settings row: label on the left, control on the right.
#[component]
fn SettingRow(label: String, children: Element) -> Element {
    rsx! {
        div { class: "flex items-center justify-between gap-4",
            label { class: "text-sm text-muted-foreground", "{label}" }
            div { class: "w-56", {children} }
        }
    }
}
