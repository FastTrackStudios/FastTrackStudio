//! Playground — preset-level macro bar with hierarchical sub-macros.
//!
//! Run with: `dx serve --hotpatch` from this directory for fast UI iteration.

#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;
#[allow(unused_imports)]
use lumen_blocks::components::dropdown::{
    Dropdown, DropdownContent, DropdownItem, DropdownTrigger,
};
use macromod::macro_bank::{MacroBank, MacroKnob};
use macromod::{EasingCurve, MacroBinding, ResponseCurve};
use signal_ui::views::MiniKnob;
use std::collections::HashMap;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const MAIN_CSS: Asset = asset!("/assets/main.css");

// ============================================================================
// SearchDropdown — filterable dropdown with auto-focused search
// ============================================================================

#[derive(Clone, PartialEq)]
struct SearchDropdownOption {
    value: String,
    label: String,
    /// Optional colored dot shown before the label.
    color: Option<String>,
}

#[derive(Props, Clone, PartialEq)]
struct SearchDropdownProps {
    /// Text shown on the trigger button.
    trigger_label: String,
    /// Available options.
    options: Vec<SearchDropdownOption>,
    /// Called when an option is selected.
    on_select: EventHandler<String>,
    /// Show the search input (default true). Set to false for short lists.
    #[props(default = true)]
    searchable: bool,
    /// Extra classes on the trigger button.
    #[props(default)]
    trigger_class: String,
}

#[component]
fn SearchDropdown(props: SearchDropdownProps) -> Element {
    let mut open = use_signal(|| false);
    let mut query = use_signal(String::new);

    let filtered: Vec<&SearchDropdownOption> = if query().is_empty() {
        props.options.iter().collect()
    } else {
        let q = query().to_lowercase();
        props
            .options
            .iter()
            .filter(|o| o.label.to_lowercase().contains(&q))
            .collect()
    };

    rsx! {
        div { class: "relative inline-block",
            // Trigger
            button {
                class: format!(
                    "flex items-center gap-1.5 px-2.5 py-1.5 text-[11px] \
                     bg-background border border-border rounded-md \
                     hover:bg-accent hover:text-accent-foreground \
                     transition-colors cursor-pointer {}",
                    props.trigger_class
                ),
                onclick: move |evt: MouseEvent| {
                    evt.stop_propagation();
                    open.toggle();
                    query.set(String::new());
                },
                span { "{props.trigger_label}" }
                span { class: "text-[9px] text-muted-foreground ml-0.5", "\u{25BE}" }
            }

            // Popover
            if open() {
                // Backdrop to close on click-away
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| open.set(false),
                }
                div {
                    class: "absolute left-0 mt-1 z-50 \
                            rounded-lg border border-border bg-popover shadow-lg \
                            text-popover-foreground",
                    style: "min-width: 200px;",
                    onclick: move |evt: MouseEvent| evt.stop_propagation(),

                    // Search input
                    if props.searchable {
                        div { class: "p-1.5 border-b border-border",
                            input {
                                class: "w-full px-2 py-1.5 text-xs bg-background \
                                        border border-border rounded-md \
                                        placeholder:text-muted-foreground/50 \
                                        focus:outline-none focus:border-ring \
                                        text-foreground",
                                r#type: "text",
                                placeholder: "Search...",
                                autofocus: true,
                                value: "{query}",
                                oninput: move |evt: FormEvent| {
                                    query.set(evt.value().to_string());
                                },
                                onkeydown: move |evt: KeyboardEvent| {
                                    if evt.key() == Key::Escape {
                                        open.set(false);
                                    }
                                },
                            }
                        }
                    }

                    // Options list
                    div { class: "max-h-52 overflow-y-auto p-1",
                        if filtered.is_empty() {
                            div { class: "px-2 py-3 text-[11px] text-muted-foreground text-center",
                                "No matches"
                            }
                        }
                        for opt in filtered.iter() {
                            {
                                let val = opt.value.clone();
                                let color = opt.color.clone();
                                rsx! {
                                    button {
                                        class: "w-full flex items-center gap-2 px-2 py-1.5 \
                                                text-xs text-left rounded-md \
                                                hover:bg-accent hover:text-accent-foreground \
                                                transition-colors cursor-pointer",
                                        onclick: move |_| {
                                            props.on_select.call(val.clone());
                                            open.set(false);
                                        },
                                        if let Some(ref c) = color {
                                            span {
                                                class: "w-2 h-2 rounded-full shrink-0",
                                                style: "background: {c};",
                                            }
                                        }
                                        "{opt.label}"
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

fn main() {
    #[cfg(feature = "desktop")]
    {
        let config = Config::new().with_window(
            WindowBuilder::new()
                .with_title("Preset Macro Bar Playground")
                .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1400, 800)),
        );

        dioxus::LaunchBuilder::desktop()
            .with_cfg(config)
            .launch(App);
    }

    #[cfg(not(feature = "desktop"))]
    dioxus::launch(App);
}

// ============================================================================
// Mock data — preset-level macro bar representing a full signal chain
// ============================================================================

/// Helper: create a knob with specific value, color, and bindings.
fn knob(
    id: &str,
    label: &str,
    value: f32,
    color: &str,
    bindings: Vec<(&str, &str, f32, f32)>,
) -> MacroKnob {
    let mut k = MacroKnob::new(id, label);
    k.set_value(value);
    k.color = Some(color.into());
    for (block, param, min, max) in bindings {
        k.bindings
            .push(MacroBinding::from_ids(block, param, min, max));
    }
    k
}

/// Helper: create a parent knob that directly drives its children's values.
///
/// Each entry in `child_ranges` is `(child_knob, min, max)` — when the parent
/// is at 0% the child is set to `min`, at 100% the child is set to `max`.
/// Moving the parent physically moves the child knob values.
fn parent_knob(
    id: &str,
    label: &str,
    value: f32,
    color: &str,
    child_ranges: Vec<(MacroKnob, f32, f32)>,
) -> MacroKnob {
    let mut k = MacroKnob::new(id, label);
    k.set_value(value);
    k.color = Some(color.into());
    let mut children = Vec::new();
    for (child, min, max) in child_ranges {
        k.bindings
            .push(MacroBinding::from_ids("self", &child.id, min, max));
        children.push(child);
    }
    k.children = children;
    k
}

// ============================================================================
// Bypass activation rules — which children are active at which parent ranges
// ============================================================================

/// Describes when a child knob is active based on the parent's value.
/// Each rule has one or more active ranges — the child is un-bypassed when
/// the parent's value falls within ANY of these ranges.
#[derive(Clone)]
struct BypassRule {
    child_id: String,
    /// Ranges where this child is active: `(min_inclusive, max_exclusive)`.
    active_ranges: Vec<(f32, f32)>,
}

/// Holds bypass activation rules for parent knobs that toggle children on/off.
/// Keyed by parent knob ID.
type BypassRules = HashMap<String, Vec<BypassRule>>;

/// Apply bypass rules: for each parent in the rules, set child bypass states
/// based on the parent's current value.
fn apply_bypass_rules(bank: &mut MacroBank, rules: &BypassRules) {
    for parent_knob in &mut bank.knobs {
        if let Some(parent_rules) = rules.get(&parent_knob.id) {
            let v = parent_knob.value;
            for rule in parent_rules {
                let active = rule.active_ranges.iter().any(|&(lo, hi)| v >= lo && v < hi);
                if let Some(child) = parent_knob
                    .children
                    .iter_mut()
                    .find(|c| c.id == rule.child_id)
                {
                    child.bypassed = !active;
                }
            }
        }
    }
}

/// Build bypass rules for the Drive macro:
///   0%:       all off
///   1–30%:    Drive 1
///   30–60%:   Drive 1 + 2
///   60–90%:   Drive 1 + 3
///   90–100%:  Drive 1 + 2 + 3
fn drive_bypass_rules() -> Vec<BypassRule> {
    vec![
        BypassRule {
            child_id: "drive-1".into(),
            active_ranges: vec![(0.01, 1.01)], // on from 1% to 100%
        },
        BypassRule {
            child_id: "drive-2".into(),
            active_ranges: vec![(0.3, 0.6), (0.9, 1.01)], // on at 30–60% and 90–100%
        },
        BypassRule {
            child_id: "drive-3".into(),
            active_ranges: vec![(0.6, 1.01)], // on from 60% to 100%
        },
    ]
}

fn build_bypass_rules() -> BypassRules {
    let mut rules = HashMap::new();
    rules.insert("drive".into(), drive_bypass_rules());
    rules
}

/// Build a mock MacroBank representing a full preset signal chain.
///
/// Parent macros with children (Drive, Shape, Delay, Reverb) don't bind
/// to block params directly — they bind to their sub-macros as min/max
/// range scalers. The children hold the actual block parameter bindings.
fn mock_preset_macro_bar() -> MacroBank {
    let mut bank = MacroBank::new();
    let rules = build_bypass_rules();

    // Input — no children
    bank.add(knob(
        "input",
        "Input",
        0.75,
        "#6B7280",
        vec![("input", "gain", 0.0, 1.0)],
    ));

    // Gate — no children
    bank.add(knob(
        "gate",
        "Gate",
        0.3,
        "#EF4444",
        vec![
            ("gate", "threshold", 0.0, 1.0),
            ("gate", "release", 0.0, 1.0),
        ],
    ));

    // Compression — no children
    bank.add(knob(
        "comp",
        "Comp",
        0.5,
        "#3B82F6",
        vec![("comp", "ratio", 0.0, 1.0), ("comp", "makeup", 0.0, 1.0)],
    ));

    // Drive — parent moves 3 sub-drive knobs AND toggles their bypass:
    //   0–30%:  Drive 1 only        (light breakup)
    //   30–60%: Drive 1 + 2         (crunch)
    //   60–100%: Drive 1 + 2 + 3    (full saturation)
    bank.add(parent_knob(
        "drive",
        "Drive",
        0.6,
        "#F97316",
        vec![
            (
                knob(
                    "drive-1",
                    "Drive 1",
                    0.7,
                    "#FB923C",
                    vec![("drive", "stage1", 0.0, 1.0)],
                ),
                0.0,
                0.8,
            ),
            (
                knob(
                    "drive-2",
                    "Drive 2",
                    0.4,
                    "#FDBA74",
                    vec![("drive", "stage2", 0.0, 1.0)],
                ),
                0.1,
                0.6,
            ),
            (
                knob(
                    "drive-3",
                    "Drive 3",
                    0.2,
                    "#FED7AA",
                    vec![("drive", "stage3", 0.0, 1.0)],
                ),
                0.0,
                0.4,
            ),
        ],
    ));

    // Gain — no children
    bank.add(knob(
        "gain",
        "Gain",
        0.5,
        "#EAB308",
        vec![("amp", "gain", 0.0, 1.0)],
    ));

    // Shape — bipolar tilt EQ (-100% to +100%, center = flat)
    //   Internal 0.0 = -100% (bass heavy): Low=0.9, Mid=0.35, High=0.1
    //   Internal 0.5 =    0% (flat):       Low=0.5, Mid=0.5,  High=0.5
    //   Internal 1.0 = +100% (treble heavy): Low=0.1, Mid=0.35, High=0.9
    //
    // Binding min/max: min is value at parent=0.0, max is value at parent=1.0
    //   Low:  0.9 → 0.1  (inverted — cuts as shape goes positive)
    //   Mid:  0.35 → 0.35 ... but we want a slight mid scoop at extremes,
    //         so we use a curve. For now: 0.35 → 0.35 keeps mids slightly cut
    //         at both extremes (the child starts at 0.5 = flat at center)
    //   High: 0.1 → 0.9  (normal — boosts as shape goes positive)
    {
        let mut shape = parent_knob(
            "shape",
            "Shape",
            0.5,
            "#22C55E",
            vec![
                (
                    knob(
                        "shape-low",
                        "Low",
                        0.5,
                        "#4ADE80",
                        vec![("eq", "low", 0.0, 1.0)],
                    ),
                    0.9,
                    0.1,
                ),
                (
                    knob(
                        "shape-mid",
                        "Mid",
                        0.5,
                        "#86EFAC",
                        vec![("eq", "mid", 0.0, 1.0)],
                    ),
                    0.35,
                    0.35,
                ),
                (
                    knob(
                        "shape-high",
                        "High",
                        0.5,
                        "#BBF7D0",
                        vec![("eq", "high", 0.0, 1.0)],
                    ),
                    0.1,
                    0.9,
                ),
            ],
        );
        shape.bipolar = true;
        bank.add(shape);
    }

    // Modulation — no children
    bank.add(knob(
        "mod",
        "Mod",
        0.35,
        "#A855F7",
        vec![("mod", "rate", 0.0, 1.0), ("mod", "depth", 0.0, 1.0)],
    ));

    // Boost — no children
    bank.add(knob(
        "boost",
        "Boost",
        0.45,
        "#EC4899",
        vec![("boost", "level", 0.0, 1.0)],
    ));

    // NOTE: MacroBank::MAX_KNOBS is 8 and we already have 8.
    // For the preset bar we bypass the add() limit and push directly.

    // Delay — parent drives all delay sub-knobs
    bank.knobs.push(parent_knob(
        "delay",
        "Delay",
        0.5,
        "#06B6D4",
        vec![
            (
                knob(
                    "delay-type",
                    "Type",
                    0.3,
                    "#22D3EE",
                    vec![("delay", "type", 0.0, 1.0)],
                ),
                0.0,
                0.5,
            ),
            (
                knob(
                    "delay-time",
                    "Time",
                    0.5,
                    "#67E8F9",
                    vec![("delay", "time", 0.0, 1.0)],
                ),
                0.1,
                0.8,
            ),
            (
                knob(
                    "delay-fb",
                    "Feedback",
                    0.4,
                    "#A5F3FC",
                    vec![("delay", "feedback", 0.0, 1.0)],
                ),
                0.0,
                0.65,
            ),
            (
                knob(
                    "delay-mix",
                    "Mix",
                    0.5,
                    "#CFFAFE",
                    vec![("delay", "mix", 0.0, 1.0)],
                ),
                0.0,
                0.7,
            ),
            (
                knob(
                    "delay-level",
                    "Level",
                    0.7,
                    "#67E8F9",
                    vec![("delay", "level", 0.0, 1.0)],
                ),
                0.3,
                1.0,
            ),
        ],
    ));

    // Reverb — parent drives all reverb sub-knobs
    bank.knobs.push(parent_knob(
        "reverb",
        "Reverb",
        0.4,
        "#0EA5E9",
        vec![
            (
                knob(
                    "reverb-type",
                    "Type",
                    0.2,
                    "#38BDF8",
                    vec![("reverb", "type", 0.0, 1.0)],
                ),
                0.0,
                0.5,
            ),
            (
                knob(
                    "reverb-time",
                    "Time",
                    0.6,
                    "#7DD3FC",
                    vec![("reverb", "time", 0.0, 1.0)],
                ),
                0.1,
                0.9,
            ),
            (
                knob(
                    "reverb-fb",
                    "Feedback",
                    0.3,
                    "#BAE6FD",
                    vec![("reverb", "feedback", 0.0, 1.0)],
                ),
                0.0,
                0.5,
            ),
            (
                knob(
                    "reverb-mix",
                    "Mix",
                    0.45,
                    "#E0F2FE",
                    vec![("reverb", "mix", 0.0, 1.0)],
                ),
                0.0,
                0.7,
            ),
            (
                knob(
                    "reverb-level",
                    "Level",
                    0.65,
                    "#7DD3FC",
                    vec![("reverb", "level", 0.0, 1.0)],
                ),
                0.3,
                1.0,
            ),
        ],
    ));

    // Motion — no children
    bank.knobs.push(knob(
        "motion",
        "Motion",
        0.2,
        "#8B5CF6",
        vec![("motion", "amount", 0.0, 1.0)],
    ));

    // Output — no children
    bank.knobs.push(knob(
        "output",
        "Output",
        0.8,
        "#6B7280",
        vec![("output", "level", 0.0, 1.0)],
    ));

    // Apply initial bypass states based on current parent values
    apply_bypass_rules(&mut bank, &rules);

    bank
}

/// Build mock block parameters that correspond to our macro bar bindings.
fn mock_block_params() -> Vec<signal::BlockParameter> {
    vec![
        signal::BlockParameter::new("input-gain", "Input Gain", 0.75),
        signal::BlockParameter::new("gate-threshold", "Gate Threshold", 0.3),
        signal::BlockParameter::new("gate-release", "Gate Release", 0.3),
        signal::BlockParameter::new("comp-ratio", "Compression Ratio", 0.5),
        signal::BlockParameter::new("comp-makeup", "Compression Makeup", 0.5),
        signal::BlockParameter::new("drive-master", "Drive Master", 0.6),
        signal::BlockParameter::new("drive-stage1", "Drive Stage 1", 0.7),
        signal::BlockParameter::new("drive-stage2", "Drive Stage 2", 0.4),
        signal::BlockParameter::new("drive-stage3", "Drive Stage 3", 0.2),
        signal::BlockParameter::new("amp-gain", "Amp Gain", 0.5),
        signal::BlockParameter::new("eq-master", "EQ Master", 0.5),
        signal::BlockParameter::new("eq-low", "EQ Low", 0.5),
        signal::BlockParameter::new("eq-mid", "EQ Mid", 0.6),
        signal::BlockParameter::new("eq-high", "EQ High", 0.4),
        signal::BlockParameter::new("mod-rate", "Mod Rate", 0.35),
        signal::BlockParameter::new("mod-depth", "Mod Depth", 0.35),
        signal::BlockParameter::new("boost-level", "Boost Level", 0.45),
        signal::BlockParameter::new("delay-master-mix", "Delay Master Mix", 0.5),
        signal::BlockParameter::new("delay-type", "Delay Type", 0.3),
        signal::BlockParameter::new("delay-time", "Delay Time", 0.5),
        signal::BlockParameter::new("delay-feedback", "Delay Feedback", 0.4),
        signal::BlockParameter::new("delay-mix", "Delay Mix", 0.5),
        signal::BlockParameter::new("delay-level", "Delay Level", 0.7),
        signal::BlockParameter::new("reverb-master-mix", "Reverb Master Mix", 0.4),
        signal::BlockParameter::new("reverb-type", "Reverb Type", 0.2),
        signal::BlockParameter::new("reverb-time", "Reverb Time", 0.6),
        signal::BlockParameter::new("reverb-feedback", "Reverb Feedback", 0.3),
        signal::BlockParameter::new("reverb-mix", "Reverb Mix", 0.45),
        signal::BlockParameter::new("reverb-level", "Reverb Level", 0.65),
        signal::BlockParameter::new("motion-amount", "Motion Amount", 0.2),
        signal::BlockParameter::new("output-level", "Output Level", 0.8),
    ]
}

fn mock_block() -> signal::Block {
    let bank = mock_preset_macro_bar();
    let params = mock_block_params();
    let mut block = signal::Block::from_parameters(params);
    block.macro_bank = Some(bank);
    block
}

// ============================================================================
// App
// ============================================================================

#[component]
fn App() -> Element {
    let block = use_signal(mock_block);
    let bypass_rules = use_signal(build_bypass_rules);
    let selected_macro_id: Signal<Option<String>> = use_signal(|| None);

    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        document::Stylesheet { href: MAIN_CSS }

        div { class: "min-h-screen bg-[#0a0a0a] text-zinc-200 flex flex-col",
            // ── Header ──
            div { class: "flex items-center gap-3 px-6 py-3 border-b border-zinc-800",
                h1 { class: "text-sm font-semibold text-zinc-300", "Preset Macro Bar" }
                div { class: "flex-1" }
                span { class: "text-[10px] text-zinc-600",
                    "Hover macros with \u{25BE} to see sub-macros \u{00B7} Click to edit assignments"
                }
            }

            // ── Macro Bar ──
            MacroBar {
                block: block,
                selected_macro_id: selected_macro_id,
                bypass_rules: bypass_rules,
            }

            // ── Assignment Detail Panel ──
            AssignmentPanel {
                block: block,
                selected_macro_id: selected_macro_id,
            }
        }
    }
}

// ============================================================================
// MacroBar — horizontal strip of macro knobs
// ============================================================================

#[component]
fn MacroBar(
    block: Signal<signal::Block>,
    selected_macro_id: Signal<Option<String>>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    let bank = block().macro_bank.clone().unwrap_or_default();

    rsx! {
        div { class: "shrink-0 px-6 py-4 border-b border-zinc-800/50 bg-zinc-950/30 overflow-visible",
            div { class: "flex items-start gap-1 overflow-visible",
                for knob in bank.knobs.iter() {
                    MacroCell {
                        knob: knob.clone(),
                        block: block,
                        selected_macro_id: selected_macro_id,
                        bypass_rules: bypass_rules,
                    }
                }
            }
        }
    }
}

// ============================================================================
// MacroCell — individual knob cell with pure-CSS hover dropdown for children
// ============================================================================

#[component]
fn MacroCell(
    knob: MacroKnob,
    block: Signal<signal::Block>,
    selected_macro_id: Signal<Option<String>>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    let kid = knob.id.clone();
    let knob_label = knob.label.clone();
    let knob_color = knob.color.clone();
    let knob_bindings = knob.bindings.clone();
    let value = knob.value;
    let has_children = knob.has_children();
    let is_selected = selected_macro_id() == Some(kid.clone());
    let binding_count = knob.bindings.len();
    let readout = knob.format_value();
    let children_data: Vec<MacroKnob> = knob.children.clone();
    let mut editing_label = use_signal(|| false);

    // "group" on the outer div enables group-hover on descendants
    rsx! {
        div {
            class: "relative group/macro",

            // Main knob cell
            div {
                class: if is_selected {
                    "flex flex-col items-center gap-1 w-[76px] py-2 rounded-xl \
                     bg-zinc-800/80 border border-zinc-600 cursor-pointer transition-all"
                } else {
                    "flex flex-col items-center gap-1 w-[76px] py-2 rounded-xl \
                     hover:bg-zinc-800/40 cursor-pointer border border-transparent transition-all"
                },
                onclick: {
                    let kid = kid.clone();
                    move |_| {
                        selected_macro_id.set(Some(kid.clone()));
                    }
                },

                // Label (above knob) — double-click to rename
                if editing_label() {
                    input {
                        class: "text-[10px] font-medium text-center w-16 \
                                bg-zinc-700 border border-zinc-500 rounded \
                                px-0.5 py-0 outline-none text-zinc-200",
                        r#type: "text",
                        autofocus: true,
                        value: "{knob_label}",
                        oninput: {
                            let kid = kid.clone();
                            move |evt: FormEvent| {
                                let mut blk = block();
                                if let Some(ref mut bk) = blk.macro_bank {
                                    if let Some(k) = bk.get_knob_mut(&kid) {
                                        k.label = evt.value().to_string();
                                    }
                                }
                                block.set(blk);
                            }
                        },
                        onkeydown: move |evt: KeyboardEvent| {
                            if evt.key() == Key::Enter || evt.key() == Key::Escape {
                                editing_label.set(false);
                            }
                        },
                        onfocusout: move |_| {
                            editing_label.set(false);
                        },
                    }
                } else {
                    div { class: "flex items-center gap-0.5 justify-center w-full",
                        span {
                            class: "text-[10px] font-medium truncate max-w-[60px] cursor-text",
                            style: if let Some(ref c) = knob_color {
                                format!("color: {};", c)
                            } else {
                                "color: #94a3b8;".into()
                            },
                            ondoubleclick: move |_| {
                                editing_label.set(true);
                            },
                            "{knob_label}"
                        }
                        if has_children {
                            span {
                                class: "text-[8px] text-zinc-600 \
                                        group-hover/macro:text-zinc-400 transition-colors",
                                "\u{25BE}"
                            }
                        }
                    }
                }

                // Knob — when a parent with children changes, propagate
                // its value through its bindings to move child knobs,
                // and apply bypass rules to toggle children on/off.
                MiniKnob {
                    value,
                    color: knob_color.clone(),
                    on_change: {
                        let kid = kid.clone();
                        let bindings = knob_bindings.clone();
                        let has_kids = has_children;
                        move |new_val: f32| {
                            let mut blk = block();
                            if let Some(ref mut bk) = blk.macro_bank {
                                if let Some(k) = bk.get_knob_mut(&kid) {
                                    k.set_value(new_val);
                                }
                                // Propagate: parent value drives child knob values
                                if has_kids {
                                    for binding in &bindings {
                                        let child_id = &binding.target.param_id;
                                        let child_val = binding.min
                                            + (binding.max - binding.min) * new_val;
                                        if let Some(child) = bk.get_knob_mut(child_id) {
                                            child.set_value(child_val);
                                        }
                                    }
                                }
                                // Apply bypass rules (toggle children on/off by range)
                                apply_bypass_rules(bk, &bypass_rules());
                            }
                            block.set(blk);
                        }
                    },
                }

                // Value readout
                span {
                    class: "text-[9px] font-mono tabular-nums text-zinc-400",
                    "{readout}"
                }

                // Binding count badge
                if binding_count > 0 {
                    span {
                        class: "text-[8px] text-zinc-500 bg-zinc-800 px-1 py-0.5 rounded-full",
                        {
                            let suffix = if binding_count != 1 { "s" } else { "" };
                            format!("{binding_count} bind{suffix}")
                        }
                    }
                }
            }

            // Sub-macro dropdown — pure CSS show/hide via group-hover
            if has_children {
                SubMacroDropdown {
                    parent_id: kid.clone(),
                    children_knobs: children_data,
                    block: block,
                    selected_macro_id: selected_macro_id,
                    bypass_rules: bypass_rules,
                }
            }
        }
    }
}

// ============================================================================
// SubMacroDropdown — CSS-driven hover dropdown with bypass pads + knobs
// ============================================================================

#[component]
fn SubMacroDropdown(
    parent_id: String,
    children_knobs: Vec<MacroKnob>,
    block: Signal<signal::Block>,
    selected_macro_id: Signal<Option<String>>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    // Which children have bypass rules defined?
    let rules = bypass_rules();
    let parent_rules = rules.get(&parent_id);
    let has_bypass = |child_id: &str| -> bool {
        parent_rules
            .map(|r| r.iter().any(|rule| rule.child_id == child_id))
            .unwrap_or(false)
    };
    rsx! {
        // Invisible bridge: fills the gap between parent cell and dropdown
        div {
            class: "absolute top-full left-0 w-full h-2 \
                    opacity-0 group-hover/macro:opacity-100 \
                    pointer-events-none group-hover/macro:pointer-events-auto",
        }
        // The actual dropdown panel
        div {
            class: "absolute top-full left-1/2 -translate-x-1/2 mt-2 z-50 \
                    rounded-xl border border-zinc-700/80 bg-zinc-900/95 \
                    shadow-xl backdrop-blur-sm p-2 \
                    opacity-0 scale-95 translate-y-[-4px] \
                    group-hover/macro:opacity-100 group-hover/macro:scale-100 \
                    group-hover/macro:translate-y-0 \
                    pointer-events-none group-hover/macro:pointer-events-auto \
                    transition-all duration-150 ease-out",
            // Child knobs in a horizontal row
            div { class: "flex items-start gap-1",
                for child in children_knobs.iter() {
                    {
                        let child_id = child.id.clone();
                        let child_label = child.label.clone();
                        let child_color = child.color.clone();
                        let child_value = child.value;
                        let child_bypassed = child.bypassed;
                        let child_has_bypass = has_bypass(&child_id);
                        let child_readout = child.format_value();
                        let is_child_selected = selected_macro_id() == Some(child_id.clone());
                        let mut editing_child_label = use_signal(|| false);

                        // Only dim when this child has bypass rules and is bypassed
                        let cell_opacity = if child_has_bypass && child_bypassed { "opacity-40" } else { "" };

                        rsx! {
                            div {
                                class: format!(
                                    "flex flex-col items-center gap-1 w-[72px] py-1.5 rounded-lg \
                                     cursor-pointer transition-all {} {}",
                                    if is_child_selected {
                                        "bg-zinc-700/80 border border-zinc-500"
                                    } else {
                                        "hover:bg-zinc-700/40 border border-transparent"
                                    },
                                    cell_opacity,
                                ),
                                onclick: {
                                    let child_id = child_id.clone();
                                    move |_| {
                                        selected_macro_id.set(Some(child_id.clone()));
                                    }
                                },

                                // Bypass pad button — only shown for children with bypass rules
                                if child_has_bypass {
                                    {
                                        let pad_color = child_color.as_deref().unwrap_or("#94a3b8");
                                        let child_id = child_id.clone();
                                        rsx! {
                                            button {
                                                class: if child_bypassed {
                                                    "w-[52px] h-[18px] rounded-md text-[8px] font-bold \
                                                     border border-zinc-600 bg-zinc-800/60 text-zinc-600 \
                                                     hover:border-zinc-500 transition-all uppercase tracking-wider"
                                                } else {
                                                    "w-[52px] h-[18px] rounded-md text-[8px] font-bold \
                                                     border border-transparent text-zinc-900 \
                                                     hover:brightness-110 transition-all uppercase tracking-wider"
                                                },
                                                style: if child_bypassed {
                                                    String::new()
                                                } else {
                                                    format!("background: {};", pad_color)
                                                },
                                                onclick: {
                                                    let child_id = child_id.clone();
                                                    move |evt: MouseEvent| {
                                                        evt.stop_propagation();
                                                        let mut blk = block();
                                                        if let Some(ref mut bk) = blk.macro_bank {
                                                            if let Some(k) = bk.get_knob_mut(&child_id) {
                                                                k.bypassed = !k.bypassed;
                                                            }
                                                        }
                                                        block.set(blk);
                                                    }
                                                },
                                                if child_bypassed { "OFF" } else { "ON" }
                                            }
                                        }
                                    }
                                }

                                // Label — double-click to rename
                                if editing_child_label() {
                                    input {
                                        class: "text-[9px] font-medium text-center w-14 \
                                                bg-zinc-600 border border-zinc-500 rounded \
                                                px-0.5 py-0 outline-none text-zinc-200",
                                        r#type: "text",
                                        autofocus: true,
                                        value: "{child_label}",
                                        oninput: {
                                            let child_id = child_id.clone();
                                            move |evt: FormEvent| {
                                                let mut blk = block();
                                                if let Some(ref mut bk) = blk.macro_bank {
                                                    if let Some(k) = bk.get_knob_mut(&child_id) {
                                                        k.label = evt.value().to_string();
                                                    }
                                                }
                                                block.set(blk);
                                            }
                                        },
                                        onkeydown: move |evt: KeyboardEvent| {
                                            if evt.key() == Key::Enter || evt.key() == Key::Escape {
                                                editing_child_label.set(false);
                                            }
                                        },
                                        onfocusout: move |_| {
                                            editing_child_label.set(false);
                                        },
                                    }
                                } else {
                                    span {
                                        class: "text-[9px] font-medium truncate max-w-[56px] cursor-text",
                                        style: if let Some(ref c) = child_color {
                                            format!("color: {};", c)
                                        } else {
                                            "color: #94a3b8;".into()
                                        },
                                        ondoubleclick: move |_| {
                                            editing_child_label.set(true);
                                        },
                                        "{child_label}"
                                    }
                                }

                                // Knob
                                MiniKnob {
                                    value: child_value,
                                    color: child_color.clone(),
                                    on_change: {
                                        let child_id = child_id.clone();
                                        move |new_val: f32| {
                                            let mut blk = block();
                                            if let Some(ref mut bk) = blk.macro_bank {
                                                if let Some(k) = bk.get_knob_mut(&child_id) {
                                                    k.set_value(new_val);
                                                }
                                            }
                                            block.set(blk);
                                        }
                                    },
                                }

                                // Value readout
                                span {
                                    class: "text-[8px] font-mono tabular-nums text-zinc-500",
                                    "{child_readout}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// AssignmentPanel — detail panel for selected macro's bindings
// ============================================================================

#[component]
fn AssignmentPanel(
    block: Signal<signal::Block>,
    selected_macro_id: Signal<Option<String>>,
) -> Element {
    let bank = block().macro_bank.clone().unwrap_or_default();

    let selected_knob = selected_macro_id().and_then(|id| bank.get_knob(&id).cloned());

    let unbound_params: Vec<(String, String)> = {
        let bound_ids: Vec<String> = selected_knob
            .as_ref()
            .map(|k| {
                k.bindings
                    .iter()
                    .map(|b| b.target.param_id.clone())
                    .collect()
            })
            .unwrap_or_default();
        block()
            .parameters()
            .iter()
            .filter(|p| !bound_ids.contains(&p.id().to_string()))
            .map(|p| (p.id().to_string(), p.name().to_string()))
            .collect()
    };

    rsx! {
        div { class: "flex-1 overflow-y-auto px-6 py-4",
            if let Some(ref knob) = selected_knob {
                // Header
                div { class: "flex items-center gap-3 mb-4",
                    span { class: "text-sm font-semibold text-zinc-200",
                        "\"{knob.label}\" Assignments"
                    }
                    {
                        let count = knob.bindings.len();
                        rsx! {
                            span { class: "text-xs text-zinc-500", "({count} bound)" }
                        }
                    }
                }

                // Assignment cards
                div { class: "flex flex-col gap-2",
                    if knob.bindings.is_empty() {
                        div { class: "flex flex-col items-center justify-center py-8 \
                                     rounded-xl border border-dashed border-zinc-800 bg-zinc-900/30",
                            span { class: "text-zinc-600 text-xs", "No assignments yet" }
                            span { class: "text-zinc-700 text-[10px] mt-1",
                                "Assign this macro to block parameters."
                            }
                        }
                    }

                    for (pos, binding) in knob.bindings.iter().enumerate() {
                        {
                            let param_name = block()
                                .parameters()
                                .iter()
                                .find(|p| p.id() == binding.target.param_id)
                                .map(|p| p.name().to_string())
                                .unwrap_or_else(|| binding.target.param_id.clone());
                            let output_val = knob.compute_binding_value(binding);
                            let output_pct = (output_val * 100.0) as i32;
                            let output_clamped = output_pct.max(0).min(100);
                            let current_curve = match binding.curve {
                                ResponseCurve::Easing(ec) => ec,
                                _ => EasingCurve::Linear,
                            };
                            let kid = knob.id.clone();
                            let b_min = binding.min;
                            let b_max = binding.max;

                            rsx! {
                                div {
                                    class: "flex items-center gap-4 px-4 py-3 \
                                            rounded-xl border border-border bg-card",

                                    // Parameter name
                                    div { class: "w-32 shrink-0",
                                        div { class: "text-xs font-medium text-card-foreground truncate",
                                            "{param_name}"
                                        }
                                        // Output bar
                                        div { class: "flex items-center gap-1.5 mt-1",
                                            div { class: "flex-1 h-1 bg-muted rounded-full overflow-hidden",
                                                div {
                                                    class: "h-full bg-primary/70 rounded-full transition-all duration-100",
                                                    style: "width: {output_clamped}%",
                                                }
                                            }
                                            span { class: "text-[9px] font-mono tabular-nums text-muted-foreground",
                                                "{output_pct}%"
                                            }
                                        }
                                    }

                                    // Min knob
                                    div { class: "flex flex-col items-center gap-0.5",
                                        span { class: "text-[9px] text-muted-foreground", "Min" }
                                        MiniKnob {
                                            value: b_min,
                                            on_change: {
                                                let kid = kid.clone();
                                                move |v: f32| {
                                                    let mut blk = block();
                                                    if let Some(ref mut bk) = blk.macro_bank {
                                                        if let Some(k) = bk.get_knob_mut(&kid) {
                                                            if let Some(b) = k.bindings.get_mut(pos) {
                                                                b.min = v;
                                                            }
                                                        }
                                                    }
                                                    block.set(blk);
                                                }
                                            },
                                        }
                                        span { class: "text-[9px] font-mono tabular-nums text-muted-foreground",
                                            "{b_min:.2}"
                                        }
                                    }

                                    // Max knob
                                    div { class: "flex flex-col items-center gap-0.5",
                                        span { class: "text-[9px] text-muted-foreground", "Max" }
                                        MiniKnob {
                                            value: b_max,
                                            on_change: {
                                                let kid = kid.clone();
                                                move |v: f32| {
                                                    let mut blk = block();
                                                    if let Some(ref mut bk) = blk.macro_bank {
                                                        if let Some(k) = bk.get_knob_mut(&kid) {
                                                            if let Some(b) = k.bindings.get_mut(pos) {
                                                                b.max = v;
                                                            }
                                                        }
                                                    }
                                                    block.set(blk);
                                                }
                                            },
                                        }
                                        span { class: "text-[9px] font-mono tabular-nums text-muted-foreground",
                                            "{b_max:.2}"
                                        }
                                    }

                                    // Curve dropdown
                                    {
                                        let curve_opts: Vec<SearchDropdownOption> = EasingCurve::ALL
                                            .iter()
                                            .map(|c| SearchDropdownOption {
                                                value: c.display_name().to_string(),
                                                label: c.display_name().to_string(),
                                                color: None,
                                            })
                                            .collect();
                                        let kid = kid.clone();
                                        rsx! {
                                            div { class: "shrink-0",
                                                SearchDropdown {
                                                    trigger_label: current_curve.display_name().to_string(),
                                                    options: curve_opts,
                                                    searchable: false,
                                                    on_select: move |selected: String| {
                                                        let curve = EasingCurve::ALL.iter()
                                                            .find(|c| c.display_name() == selected.as_str())
                                                            .copied()
                                                            .unwrap_or(EasingCurve::Linear);
                                                        let mut blk = block();
                                                        if let Some(ref mut bk) = blk.macro_bank {
                                                            if let Some(k) = bk.get_knob_mut(&kid) {
                                                                if let Some(b) = k.bindings.get_mut(pos) {
                                                                    b.curve = ResponseCurve::Easing(curve);
                                                                }
                                                            }
                                                        }
                                                        block.set(blk);
                                                    },
                                                }
                                            }
                                        }
                                    }

                                    div { class: "flex-1" }

                                    // Delete button
                                    button {
                                        class: "text-muted-foreground hover:text-destructive \
                                                text-xs w-6 h-6 flex items-center justify-center \
                                                rounded hover:bg-destructive/10 transition-colors shrink-0",
                                        onclick: {
                                            let kid = kid.clone();
                                            move |_| {
                                                let mut blk = block();
                                                if let Some(ref mut bk) = blk.macro_bank {
                                                    if let Some(k) = bk.get_knob_mut(&kid) {
                                                        if pos < k.bindings.len() {
                                                            k.bindings.remove(pos);
                                                        }
                                                    }
                                                }
                                                block.set(blk);
                                            }
                                        },
                                        "\u{00D7}"
                                    }
                                }
                            }
                        }
                    }
                }

                // Add Assignment
                {
                    let param_opts: Vec<SearchDropdownOption> = unbound_params
                        .iter()
                        .map(|(pid, pname)| SearchDropdownOption {
                            value: pid.clone(),
                            label: pname.clone(),
                            color: None,
                        })
                        .collect();
                    let kid = knob.id.clone();
                    let has_params = !param_opts.is_empty();
                    rsx! {
                        div { class: "mt-3",
                            if has_params {
                                SearchDropdown {
                                    trigger_label: "+ Add Assignment".to_string(),
                                    options: param_opts,
                                    searchable: true,
                                    trigger_class: "text-primary".to_string(),
                                    on_select: move |pid: String| {
                                        let mut blk = block();
                                        if let Some(ref mut bk) = blk.macro_bank {
                                            if let Some(k) = bk.get_knob_mut(&kid) {
                                                k.bindings.push(
                                                    MacroBinding::from_ids("self", pid.clone(), 0.0, 1.0),
                                                );
                                            }
                                        }
                                        block.set(blk);
                                    },
                                }
                            } else {
                                span { class: "text-xs text-muted-foreground/50 italic",
                                    "All parameters assigned"
                                }
                            }
                        }
                    }
                }
            } else {
                div { class: "flex flex-col items-center justify-center h-full text-center gap-2 py-12",
                    span { class: "text-muted-foreground text-sm",
                        "Select a macro knob to edit its assignments"
                    }
                    span { class: "text-muted-foreground/50 text-xs",
                        "Click a knob above, or double-click its name to rename"
                    }
                }
            }
        }
    }
}
