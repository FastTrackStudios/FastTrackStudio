//! Guitar rig showcase — the default guitar rig *template* in the interactive
//! module/wire grid, with a left-hand preset list, a top bar with sidebar
//! toggles, and audio settings (quick input picker + full modal).
//!
//! This is the static-RSX entry point the desktop app builds on. It takes the
//! canonical [`guitar_rig_template`](signal::defaults::guitar::guitar_rig_template)
//! (11 modules — Source, Dynamics, Special, Drive, Volume, Pre-FX, Amp,
//! Modulation, Time, Motion, Mastering — Amp and Time using parallel splits)
//! and renders every block as a dashed template placeholder in [`RigGridPanel`].
//!
//! Interactive controls use the themed `lumen_blocks` components (Button,
//! Dropdown) so they follow the active theme.

use std::time::Duration;

use dioxus::prelude::*;
use lumen_blocks::components::button::{Button, ButtonVariant};
use lumen_blocks::components::dropdown::{Dropdown, DropdownContent, DropdownItem, DropdownTrigger};

use signal::defaults::guitar::guitar_rig_template;
use signal::{BlockType, Preset, Signal};
use signal_browser::grid_conversion::template_to_grid_slots;

use crate::components::{block_color, BlockColor, GridSelection, GridSlot};
use crate::views::{
    AudioPrefs, AudioSettingsBridge, AudioSettingsModal, LiveBlock, PerfStack, PerformanceModel,
    RigAudioHandle, RigGridPanel,
};

/// Stable Uuid derived from a block's string id (so the grid keeps a consistent
/// identity across polls without re-diffing every frame).
fn slot_uuid(id: &str) -> uuid::Uuid {
    use std::hash::{Hash, Hasher};
    let mut h1 = std::collections::hash_map::DefaultHasher::new();
    id.hash(&mut h1);
    let mut h2 = std::collections::hash_map::DefaultHasher::new();
    (id, 0x9e37_79b9u64).hash(&mut h2);
    let mut bytes = [0u8; 16];
    bytes[..8].copy_from_slice(&h1.finish().to_le_bytes());
    bytes[8..].copy_from_slice(&h2.finish().to_le_bytes());
    uuid::Uuid::from_bytes(bytes)
}

/// Overlay the live rig's active blocks onto the guitar-rig-template grid slots:
/// the template is the full canvas (every module + slot); a live block *resolves*
/// its matching slot (matched by name) — filling in real bypass state, its param,
/// and its live id (in `preset_id`) for control. Unmatched slots stay dashed
/// template placeholders.
fn resolve_template(base: &[GridSlot], live: &[LiveBlock]) -> Vec<GridSlot> {
    let mut slots = base.to_vec();
    for slot in slots.iter_mut() {
        let Some(slot_name) = slot.block_preset_name.clone() else {
            continue;
        };
        if let Some(b) = live.iter().find(|b| b.name.eq_ignore_ascii_case(&slot_name)) {
            slot.is_template = false;
            slot.bypassed = b.bypassed;
            slot.id = slot_uuid(&b.id);
            slot.preset_id = Some(b.id.clone());
            slot.plugin_name = Some(b.name.clone());
            slot.parameters = b
                .param_name
                .as_ref()
                .map(|n| vec![(n.clone(), b.param_value)])
                .unwrap_or_default();
        }
    }
    slots
}

/// Top-level UI mode.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// Build the rig — module/wire grid + preset sidebar.
    Edit,
    /// Play the rig — footswitch folder grid.
    Perform,
}

/// Map a linear peak (0..1) to a perceptual meter level (0..1) via a sqrt curve,
/// so quiet-but-present signal is clearly visible.
fn meter_level(peak: f32) -> f64 {
    (peak.max(0.0).sqrt() as f64).min(1.0)
}

/// Guitar-relevant block types offered in the preset sidebar selector.
const PRESET_BLOCK_TYPES: &[(BlockType, &str)] = &[
    (BlockType::Amp, "Amp"),
    (BlockType::Drive, "Drive"),
    (BlockType::Reverb, "Reverb"),
    (BlockType::Delay, "Delay"),
    (BlockType::Compressor, "Comp"),
    (BlockType::Eq, "EQ"),
];

/// Display label for an audio-device value ("" → "Default input").
fn input_label(name: &str) -> String {
    if name.is_empty() {
        "Default input".to_string()
    } else {
        name.to_string()
    }
}

/// Static showcase: preset list + the default guitar rig template grid.
#[component]
pub fn GuitarRigView() -> Element {
    let module_count = 11;
    // The full guitar-rig template — the grid canvas that the live rig resolves into.
    let base_slots = use_hook(|| template_to_grid_slots(&guitar_rig_template()));

    // UI mode + sidebar visibility + audio-settings modal state.
    let mut mode = use_signal(|| Mode::Edit);
    let mut left_open = use_signal(|| true);
    let mut right_open = use_signal(|| false);
    let mut audio_open = use_signal(|| false);

    // Audio capability injected by the host app (device lists + persistence).
    let ctx_bridge = use_hook(try_consume_context::<AudioSettingsBridge>);
    // Live rig transport + meters (host-provided).
    let rig_handle = use_hook(try_consume_context::<RigAudioHandle>);

    // Shared, editable prefs — both the quick picker and the modal read/write
    // this one signal so they never disagree. Seeded from the persisted prefs.
    let mut prefs = use_signal(|| {
        ctx_bridge
            .as_ref()
            .map(|b| b.prefs.clone())
            .unwrap_or_default()
    });

    // Apply = update shared state, persist, and re-open the live rig so device /
    // buffer changes take effect immediately (the engine is always running).
    let apply = {
        let ctx_bridge = ctx_bridge.clone();
        let rig_handle = rig_handle.clone();
        use_callback(move |p: AudioPrefs| {
            prefs.set(p.clone());
            if let Some(b) = &ctx_bridge {
                b.on_save.call(p);
            }
            if let Some(h) = &rig_handle {
                h.start.call(());
            }
        })
    };

    // A per-render bridge carrying the *current* shared prefs, handed to the
    // modal + quick picker so their edits round-trip through `apply`.
    let live_bridge = ctx_bridge.as_ref().map(|b| AudioSettingsBridge {
        inputs: b.inputs.clone(),
        outputs: b.outputs.clone(),
        prefs: prefs(),
        on_save: apply,
    });

    let mut running = use_signal(|| false);
    let mut in_level = use_signal(|| 0.0f64);
    let mut out_level = use_signal(|| 0.0f64);
    let mut perf = use_signal(PerformanceModel::default);
    // The live FX chain of the active patch, and the currently-selected block.
    let mut live_blocks = use_signal(Vec::<LiveBlock>::new);
    let mut selected_block = use_signal(|| None::<String>);

    // Poll meters + performance + the live FX chain while the rig runs.
    {
        let rig_handle = rig_handle.clone();
        use_future(move || {
            let rig_handle = rig_handle.clone();
            async move {
                loop {
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    if let Some(h) = &rig_handle {
                        running.set(h.is_running.call(()));
                        in_level.set(meter_level(h.input_peak.call(())));
                        out_level.set(meter_level(h.output_peak.call(())));
                        perf.set(h.perf_model.call(()));
                        live_blocks.set(h.active_chain.call(()));
                    }
                }
            }
        });
    }

    let block_count = live_blocks().len();
    // The template grid with live blocks resolved into their matching slots.
    let slots = {
        let base = base_slots.clone();
        use_memo(move || resolve_template(&base, &live_blocks()))
    };

    rsx! {
        div { class: "flex flex-col h-full bg-background text-foreground",
            // Top bar
            header {
                class: "flex items-center gap-2 px-3 py-2 border-b border-border bg-card",

                // Left: toggle preset sidebar
                Button {
                    variant: if left_open() { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                    is_icon_button: true,
                    aria_label: "Toggle preset list".to_string(),
                    on_click: move |_| left_open.toggle(),
                    "☰"
                }

                div { class: "flex flex-col ml-1 mr-2",
                    span { class: "text-[10px] font-semibold uppercase tracking-[2px] text-muted-foreground",
                        "Guitar Rig"
                    }
                    span { class: "text-sm font-bold",
                        if !perf().profile_name.is_empty() { "{perf().profile_name}" } else { "Default Template" }
                    }
                }

                // Edit | Perform mode toggle
                div { class: "flex items-center rounded-md border border-border overflow-hidden",
                    Button {
                        variant: if mode() == Mode::Edit { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                        on_click: move |_| mode.set(Mode::Edit),
                        "Edit"
                    }
                    Button {
                        variant: if mode() == Mode::Perform { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                        on_click: move |_| mode.set(Mode::Perform),
                        "Perform"
                    }
                }

                div { class: "flex-1" }

                // Live IN/OUT meters (engine is always running)
                if rig_handle.is_some() {
                    div { class: "flex items-center gap-2 mr-2",
                        MeterPair { input: in_level(), output: out_level() }
                    }
                }

                // Quick audio input picker
                if let Some(bridge) = live_bridge.clone() {
                    QuickInputPicker { bridge }
                }

                span { class: "text-xs text-muted-foreground mx-1",
                    "{module_count} modules · {block_count} slots"
                }

                // Audio settings
                Button {
                    variant: if audio_open() { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                    on_click: move |_| audio_open.toggle(),
                    "Audio"
                }

                // Right: toggle right sidebar (inspector — coming soon)
                Button {
                    variant: if right_open() { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                    is_icon_button: true,
                    aria_label: "Toggle right panel".to_string(),
                    on_click: move |_| right_open.toggle(),
                    "⚙"
                }
            }

            // Body: Edit (the live module/wire grid) or Perform (footswitch folders)
            if mode() == Mode::Edit {
                // Focusable wrapper so Space toggles the selected block's bypass.
                div {
                    class: "flex-1 min-h-0 flex flex-row overflow-hidden outline-none",
                    tabindex: "0",
                    onkeydown: {
                        let rig_handle = rig_handle.clone();
                        move |e: KeyboardEvent| {
                            if e.code() == Code::Space {
                                e.prevent_default();
                                if let (Some(h), Some(id)) = (&rig_handle, selected_block()) {
                                    h.toggle_block_bypass.call(id);
                                }
                            }
                        }
                    },
                    if left_open() {
                        PresetSidebar {}
                    }
                    div { class: "flex-1 min-w-0 min-h-0 flex flex-col overflow-hidden",
                        RigGridPanel {
                            initial_slots: slots(),
                            on_selection_change: move |sel: Option<GridSelection>| {
                                selected_block.set(match sel {
                                    Some(GridSelection::Block(uuid)) => slots()
                                        .iter()
                                        .find(|s| s.id == uuid)
                                        .and_then(|s| s.preset_id.clone()),
                                    _ => None,
                                });
                            },
                            on_param_change: {
                                let rig_handle = rig_handle.clone();
                                move |(uuid, name, value): (uuid::Uuid, String, f32)| {
                                    if let Some(h) = &rig_handle {
                                        if let Some(id) = slots().iter().find(|s| s.id == uuid).and_then(|s| s.preset_id.clone()) {
                                            h.set_block_param.call((id, name, value));
                                        }
                                    }
                                }
                            },
                        }
                    }
                }
            } else {
                div { class: "flex-1 min-h-0 overflow-hidden p-4",
                    if let Some(h) = rig_handle.clone() {
                        PerformGrid {
                            model: perf(),
                            on_press: h.press_stack,
                            on_toggle_fx: h.toggle_fx,
                            on_toggle_boost: h.toggle_boost,
                            on_tap_tempo: h.tap_tempo,
                        }
                    } else {
                        div { class: "flex items-center justify-center h-full",
                            span { class: "text-sm text-muted-foreground italic", "Audio backend not connected." }
                        }
                    }
                }
            }
        }

        // Audio settings modal
        if audio_open() {
            if let Some(bridge) = live_bridge.clone() {
                AudioSettingsModal {
                    bridge: bridge,
                    on_close: move |_| audio_open.set(false),
                }
            } else {
                AudioUnavailableModal { on_close: move |_| audio_open.set(false) }
            }
        }
    }
}

/// Compact IN / OUT level meters for the top bar (confirms signal passthrough).
#[component]
fn MeterPair(input: f64, output: f64) -> Element {
    rsx! {
        div { class: "flex items-center gap-3",
            MeterBar { label: "IN", level: input }
            MeterBar { label: "OUT", level: output }
        }
    }
}

/// A single horizontal level meter with an explicit, always-visible fill.
#[component]
fn MeterBar(label: &'static str, level: f64) -> Element {
    let clamped = level.clamp(0.0, 1.0);
    let pct = (clamped * 100.0) as u32;
    let color = if clamped > 0.9 {
        "#ef4444"
    } else if clamped > 0.7 {
        "#eab308"
    } else {
        "#22c55e"
    };
    rsx! {
        div { class: "flex items-center gap-1.5",
            span { class: "text-[10px] font-semibold text-muted-foreground w-7 text-right", "{label}" }
            div { class: "relative w-32 h-3 rounded bg-black/50 overflow-hidden border border-border",
                div {
                    class: "absolute inset-y-0 left-0 transition-[width] duration-75",
                    style: "width: {pct}%; background-color: {color};",
                }
            }
        }
    }
}

/// Compact top-bar audio input picker — sets the input device and persists.
#[component]
fn QuickInputPicker(bridge: AudioSettingsBridge) -> Element {
    let current = bridge.prefs.input_device.clone();
    let inputs = bridge.inputs.clone();
    let on_save = bridge.on_save;
    let base = bridge.prefs.clone();

    rsx! {
        Dropdown {
            DropdownTrigger {
                Button { variant: ButtonVariant::Outline,
                    span { class: "max-w-[180px] truncate text-xs", "🎸 {input_label(&current)}" }
                }
            }
            DropdownContent { align: "end".to_string(), width: "w-64".to_string(), class: "max-h-80 overflow-y-auto",
                DropdownItem {
                    value: String::new(),
                    index: 0,
                    on_select: {
                        let base = base.clone();
                        move |v: String| on_save.call(AudioPrefs { input_device: v, input_channel: 0, ..base.clone() })
                    },
                    "Default input"
                }
                for (i, d) in inputs.iter().enumerate() {
                    DropdownItem {
                        key: "{i}",
                        value: d.name.clone(),
                        index: i + 1,
                        on_select: {
                            let base = base.clone();
                            move |v: String| on_save.call(AudioPrefs { input_device: v, input_channel: 0, ..base.clone() })
                        },
                        "{d.name} ({d.channels} ch)"
                    }
                }
            }
        }
    }
}

/// Fallback shown when no audio backend bridge is provided by the host app.
#[component]
fn AudioUnavailableModal(on_close: EventHandler<()>) -> Element {
    rsx! {
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center",
            style: "background: rgba(0,0,0,0.55);",
            onclick: move |_| on_close.call(()),
            div {
                class: "w-96 max-w-[92vw] rounded-lg border border-border bg-popover text-popover-foreground shadow-2xl p-6",
                onclick: move |e| e.stop_propagation(),
                h2 { class: "text-sm font-semibold mb-2", "Audio Settings" }
                p { class: "text-xs text-muted-foreground",
                    "No audio backend is connected in this build, so devices can't be "
                    "enumerated. Run the desktop app to configure audio."
                }
                div { class: "flex justify-end mt-4",
                    Button { variant: ButtonVariant::Outline, on_click: move |_| on_close.call(()), "Close" }
                }
            }
        }
    }
}

/// Tile background + text color for a folder (footswitch), by name.
fn folder_color(name: &str) -> (&'static str, &'static str) {
    match name.to_ascii_lowercase().as_str() {
        "clean" => ("#38bdf8", "#082f49"),   // light blue / dark text
        "crunch" => ("#2563eb", "#ffffff"),  // darker blue / white
        "drive" => ("#f97316", "#ffffff"),   // orange / white
        "lead" => ("#ef4444", "#ffffff"),    // red / white
        "ambient" => ("#06b6d4", "#04222a"), // cyan / dark text
        _ => ("#3f3f46", "#e4e4e7"),         // zinc fallback
    }
}

/// Perform-mode footswitch grid: a full-height 4×2 grid — five colored folder
/// tiles (Clean/Crunch/Drive/Lead/Ambient) plus Tap Tempo, FX Toggle, and
/// Volume Boost function switches.
#[component]
fn PerformGrid(
    model: PerformanceModel,
    on_press: Callback<usize>,
    on_toggle_fx: Callback<()>,
    on_toggle_boost: Callback<()>,
    on_tap_tempo: Callback<()>,
) -> Element {
    let stacks = model.stacks;
    let fx_sub = if model.fx_bypass { "Bypassed" } else { "Active" };
    let boost_sub = if model.boost { "+6 dB" } else { "Off" };
    rsx! {
        div { class: "grid grid-cols-4 grid-rows-2 gap-3 h-full",
            // Folders (positions 1–5): Clean, Crunch, Drive, Lead, Ambient.
            for i in 0..5usize {
                if let Some(stack) = stacks.get(i).cloned() {
                    StackTile { key: "s{i}", index: i, stack, on_press }
                } else {
                    div { key: "s{i}", class: "rounded-xl border-2 border-dashed border-border/30" }
                }
            }
            // Position 6: Tap Tempo (white, blinking at tempo).
            TapTempoTile { tempo_bpm: model.tempo_bpm, on_tap: on_tap_tempo }
            // Position 7: FX Toggle (pink).
            FnTile {
                title: "FX Toggle".to_string(),
                subtitle: fx_sub.to_string(),
                bg: "#ec4899".to_string(),
                text: "#ffffff".to_string(),
                active: model.fx_bypass,
                onclick: on_toggle_fx,
            }
            // Position 8: Volume Boost (white).
            FnTile {
                title: "Boost".to_string(),
                subtitle: boost_sub.to_string(),
                bg: "#fafafa".to_string(),
                text: "#0a0a0a".to_string(),
                active: model.boost,
                onclick: on_toggle_boost,
            }
        }
    }
}

/// One colored footswitch folder tile.
#[component]
fn StackTile(index: usize, stack: PerfStack, on_press: Callback<usize>) -> Element {
    let (bg, text) = folder_color(&stack.name);
    let state_cls = if stack.is_active {
        "ring-4 ring-white/80 shadow-xl opacity-100"
    } else {
        "opacity-[0.22] saturate-50 hover:opacity-60"
    };
    rsx! {
        button {
            class: format!(
                "relative flex flex-col items-center justify-center gap-1 rounded-xl transition-all h-full {state_cls}"
            ),
            style: "background-color: {bg}; color: {text};",
            onclick: move |_| on_press.call(index),
            // Amber dot while the current patch is still loading.
            if !stack.available {
                span { class: "absolute top-2 right-2 w-2.5 h-2.5 rounded-full",
                    style: "background-color: #fde047;" }
            }
            span { class: "text-2xl font-bold tracking-wide", "{stack.name}" }
            span { class: "text-sm font-semibold opacity-90", "{stack.current_patch}" }
            if stack.patch_count > 1 {
                span { class: "text-[11px] font-mono opacity-75", "{stack.position + 1}/{stack.patch_count}" }
            }
        }
    }
}

/// A function-switch tile (FX Toggle, Volume Boost).
#[component]
fn FnTile(
    title: String,
    subtitle: String,
    bg: String,
    text: String,
    active: bool,
    onclick: Callback<()>,
) -> Element {
    let state_cls = if active {
        "ring-4 ring-white/80 shadow-xl opacity-100"
    } else {
        "opacity-[0.3] saturate-50 hover:opacity-70"
    };
    rsx! {
        button {
            class: format!(
                "flex flex-col items-center justify-center gap-1 rounded-xl transition-all h-full {state_cls}"
            ),
            style: "background-color: {bg}; color: {text};",
            onclick: move |_| onclick.call(()),
            span { class: "text-xl font-bold tracking-wide", "{title}" }
            span { class: "text-xs opacity-80", "{subtitle}" }
        }
    }
}

/// Tap Tempo tile — white, with a light blinking at the current tempo.
#[component]
fn TapTempoTile(tempo_bpm: u32, on_tap: Callback<()>) -> Element {
    let mut lit = use_signal(|| false);
    // Blink the indicator at the tempo (toggle twice per beat).
    use_future(move || async move {
        loop {
            let bpm = tempo_bpm.max(40) as u64;
            let half_ms = (60_000 / bpm / 2).max(60);
            tokio::time::sleep(Duration::from_millis(half_ms)).await;
            lit.toggle();
        }
    });
    let dot_color = if lit() { "#ef4444" } else { "#d4d4d8" };
    rsx! {
        button {
            class: "relative flex flex-col items-center justify-center gap-1 rounded-xl transition-all h-full opacity-90 hover:opacity-100",
            style: "background-color: #fafafa; color: #0a0a0a;",
            onclick: move |_| on_tap.call(()),
            span { class: "absolute top-2 right-2 w-3 h-3 rounded-full transition-colors",
                style: "background-color: {dot_color};" }
            span { class: "text-lg font-bold tracking-wide", "Tap Tempo" }
            span { class: "text-[11px] text-zinc-500", "{tempo_bpm} BPM · hold: tuner" }
        }
    }
}

/// Left-hand preset list. Lists presets for the selected block type, pulled from
/// the seeded [`Signal`] controller (via context). Self-contained: if no
/// controller is provided, or the preset library is empty, it shows a graceful
/// empty state rather than panicking.
#[component]
fn PresetSidebar() -> Element {
    let controller = use_hook(try_consume_context::<Signal>);

    let mut block_type = use_signal(|| BlockType::Amp);
    let mut presets = use_signal(Vec::<Preset>::new);
    let mut selected = use_signal(|| None::<usize>);
    let mut loading = use_signal(|| controller.is_some());

    // Reload the preset list whenever the selected block type changes.
    {
        let controller = controller.clone();
        use_effect(move || {
            let selected_type = block_type();
            let Some(signal) = controller.clone() else {
                return;
            };
            loading.set(true);
            spawn(async move {
                let list = signal
                    .block_presets()
                    .list(selected_type)
                    .await
                    .unwrap_or_default();
                presets.set(list);
                selected.set(None);
                loading.set(false);
            });
        });
    }

    let items = presets();
    let sel = selected();

    rsx! {
        aside { class: "w-64 flex-shrink-0 flex flex-col border-r border-border bg-card min-h-0",
            // Title
            div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                h3 { class: "text-[10px] font-semibold text-muted-foreground uppercase tracking-wider", "Presets" }
            }

            // Block-type selector
            div { class: "flex flex-wrap gap-1 px-2 py-2 border-b border-border flex-shrink-0",
                for (bt, label) in PRESET_BLOCK_TYPES.iter().copied() {
                    Button {
                        key: "{label}",
                        variant: if block_type() == bt { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                        on_click: move |_| block_type.set(bt),
                        "{label}"
                    }
                }
            }

            // Preset list
            div { class: "flex-1 overflow-y-auto min-h-0",
                if controller.is_none() {
                    div { class: "p-4 text-xs text-muted-foreground italic", "No Signal controller connected." }
                } else if loading() {
                    div { class: "p-4 text-xs text-muted-foreground italic", "Loading presets…" }
                } else if items.is_empty() {
                    div { class: "p-4 text-xs text-muted-foreground italic",
                        "No presets found for this block type."
                    }
                } else {
                    // Keyed by index: the seeded catalog can contain presets with
                    // colliding ids, which would panic a name/id-keyed list.
                    for (i, preset) in items.iter().enumerate() {
                        {
                            let name = preset.name().to_string();
                            let variants = preset.snapshots().len();
                            let is_sel = sel == Some(i);
                            let color: BlockColor = block_color(preset.block_type().as_str());
                            rsx! {
                                button {
                                    key: "{i}",
                                    class: if is_sel {
                                        "w-full text-left px-3 py-2 border-b border-border/50 bg-accent text-accent-foreground"
                                    } else {
                                        "w-full text-left px-3 py-2 border-b border-border/50 hover:bg-accent/50 text-foreground"
                                    },
                                    onclick: move |_| selected.set(Some(i)),
                                    div { class: "flex items-center gap-2",
                                        span {
                                            class: "w-2 h-2 rounded-full flex-shrink-0",
                                            style: "background-color: {color.bg};",
                                        }
                                        span { class: "text-sm truncate", "{name}" }
                                        span { class: "text-[10px] text-muted-foreground ml-auto flex-shrink-0",
                                            "{variants}"
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
