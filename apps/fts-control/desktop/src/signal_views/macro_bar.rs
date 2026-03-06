//! Macro bar — horizontal strip of macro knobs for the active preset.
//!
//! Ported from the playground example (`apps/tests/playground/src/main.rs`).
//! Shows a row of `MacroCell` knobs; parent knobs with children show a
//! CSS-driven hover dropdown with sub-macro knobs and bypass pads.

use dioxus::prelude::*;
use macromod::macro_bank::{MacroBank, MacroKnob};
use macromod::MacroBinding;
use signal_ui::views::MiniKnob;
use std::collections::HashMap;
use std::rc::Rc;

// ============================================================================
// Bypass activation rules — which children are active at which parent ranges
// ============================================================================

/// Describes when a child knob is active based on the parent's value.
#[derive(Clone)]
pub(crate) struct BypassRule {
    pub child_id: String,
    pub active_ranges: Vec<(f32, f32)>,
}

/// Holds bypass activation rules for parent knobs that toggle children on/off.
pub(crate) type BypassRules = HashMap<String, Vec<BypassRule>>;

/// Apply bypass rules: for each parent in the rules, set child bypass states
/// based on the parent's current value.
pub(crate) fn apply_bypass_rules(bank: &mut MacroBank, rules: &BypassRules) {
    for parent_knob in &mut bank.knobs {
        if let Some(parent_rules) = rules.get(&parent_knob.id) {
            let v = parent_knob.value;
            for rule in parent_rules {
                let active = rule.active_ranges.iter().any(|&(lo, hi)| v >= lo && v < hi);
                if let Some(child) = parent_knob.children.iter_mut().find(|c| c.id == rule.child_id)
                {
                    child.bypassed = !active;
                }
            }
        }
    }
}

// ============================================================================
// Mock macro bank builder (for initial development)
// ============================================================================

/// Helper: create a knob with bindings to block parameters.
fn knob(id: &str, label: &str, value: f32, color: &str, bindings: Vec<(&str, &str, f32, f32)>) -> MacroKnob {
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

fn drive_bypass_rules() -> Vec<BypassRule> {
    vec![
        BypassRule {
            child_id: "drive-1".into(),
            active_ranges: vec![(0.01, 1.01)],
        },
        BypassRule {
            child_id: "drive-2".into(),
            active_ranges: vec![(0.3, 0.6), (0.9, 1.01)],
        },
        BypassRule {
            child_id: "drive-3".into(),
            active_ranges: vec![(0.6, 1.01)],
        },
    ]
}

pub(crate) fn build_bypass_rules() -> BypassRules {
    let mut rules = HashMap::new();
    rules.insert("drive".into(), drive_bypass_rules());
    rules
}

/// Build a mock MacroBank representing a full preset signal chain.
pub(crate) fn mock_preset_macro_bar() -> MacroBank {
    let mut bank = MacroBank::new();
    let rules = build_bypass_rules();

    bank.add(knob("input", "Input", 0.75, "#6B7280", vec![("input", "gain", 0.0, 1.0)]));
    bank.add(parent_knob(
        "gate", "Gate", 0.3, "#EF4444",
        vec![
            (knob("gate-threshold", "Threshold", 0.4, "#F87171", vec![("gate", "threshold", 0.0, 1.0)]), 0.0, 0.8),
            (knob("gate-ratio", "Ratio", 0.5, "#FCA5A5", vec![("gate", "ratio", 0.0, 1.0)]), 0.0, 0.7),
            (knob("gate-range", "Range", 0.6, "#FECACA", vec![("gate", "range", 0.0, 1.0)]), 0.0, 0.9),
            (knob("gate-attack", "Attack", 0.2, "#FEE2E2", vec![("gate", "attack", 0.0, 1.0)]), 0.0, 0.5),
            (knob("gate-release", "Release", 0.4, "#F87171", vec![("gate", "release", 0.0, 1.0)]), 0.1, 0.8),
            (knob("gate-hold", "Hold", 0.3, "#FCA5A5", vec![("gate", "hold", 0.0, 1.0)]), 0.0, 0.6),
        ],
    ));
    bank.add(parent_knob(
        "pre-comp", "Pre-Comp", 0.5, "#3B82F6",
        vec![
            (knob("pre-comp-threshold", "Threshold", 0.5, "#60A5FA", vec![("pre-comp", "threshold", 0.0, 1.0)]), 0.0, 0.8),
            (knob("pre-comp-attack", "Attack", 0.3, "#93C5FD", vec![("pre-comp", "attack", 0.0, 1.0)]), 0.0, 0.6),
            (knob("pre-comp-release", "Release", 0.5, "#BFDBFE", vec![("pre-comp", "release", 0.0, 1.0)]), 0.1, 0.8),
            (knob("pre-comp-makeup", "Makeup", 0.4, "#DBEAFE", vec![("pre-comp", "makeup", 0.0, 1.0)]), 0.0, 0.7),
        ],
    ));
    bank.add(parent_knob(
        "drive", "Drive", 0.6, "#F97316",
        vec![
            (knob("drive-1", "Drive 1", 0.7, "#FB923C", vec![("drive", "stage1", 0.0, 1.0)]), 0.0, 0.8),
            (knob("drive-2", "Drive 2", 0.4, "#FDBA74", vec![("drive", "stage2", 0.0, 1.0)]), 0.1, 0.6),
            (knob("drive-3", "Drive 3", 0.2, "#FED7AA", vec![("drive", "stage3", 0.0, 1.0)]), 0.0, 0.4),
        ],
    ));
    bank.add(knob("gain", "Gain", 0.5, "#EAB308", vec![("amp", "gain", 0.0, 1.0)]));

    {
        let mut shape = parent_knob(
            "shape", "Shape", 0.5, "#22C55E",
            vec![
                (knob("shape-low", "Low", 0.5, "#4ADE80", vec![("eq", "low", 0.0, 1.0)]), 0.9, 0.1),
                (knob("shape-mid", "Mid", 0.5, "#86EFAC", vec![("eq", "mid", 0.0, 1.0)]), 0.35, 0.35),
                (knob("shape-high", "High", 0.5, "#BBF7D0", vec![("eq", "high", 0.0, 1.0)]), 0.1, 0.9),
            ],
        );
        shape.bipolar = true;
        bank.add(shape);
    }

    bank.add(parent_knob(
        "comp", "Comp", 0.4, "#60A5FA",
        vec![
            (knob("comp-threshold", "Threshold", 0.4, "#93C5FD", vec![("comp", "threshold", 0.0, 1.0)]), 0.0, 0.8),
            (knob("comp-attack", "Attack", 0.3, "#BFDBFE", vec![("comp", "attack", 0.0, 1.0)]), 0.0, 0.6),
            (knob("comp-release", "Release", 0.5, "#DBEAFE", vec![("comp", "release", 0.0, 1.0)]), 0.1, 0.8),
            (knob("comp-makeup", "Makeup", 0.4, "#EFF6FF", vec![("comp", "makeup", 0.0, 1.0)]), 0.0, 0.7),
        ],
    ));

    // Beyond MAX_KNOBS — push directly
    bank.knobs.push(knob("mod", "Mod", 0.35, "#A855F7", vec![
        ("mod", "rate", 0.0, 1.0),
        ("mod", "depth", 0.0, 1.0),
    ]));
    bank.knobs.push(knob("boost", "Boost", 0.45, "#EC4899", vec![("boost", "level", 0.0, 1.0)]));
    bank.knobs.push(parent_knob(
        "delay", "Delay", 0.5, "#06B6D4",
        vec![
            // Row 1: Delay 1
            (knob("delay-type1", "Type 1", 0.3, "#22D3EE", vec![("delay1", "type", 0.0, 1.0)]), 0.0, 0.5),
            (knob("delay-time1", "Time 1", 0.5, "#67E8F9", vec![("delay1", "time", 0.0, 1.0)]), 0.1, 0.8),
            (knob("delay-fb1", "FB 1", 0.4, "#A5F3FC", vec![("delay1", "feedback", 0.0, 1.0)]), 0.0, 0.65),
            (knob("delay-hipass", "HiPass", 0.2, "#CFFAFE", vec![("delay", "hipass", 0.0, 1.0)]), 0.0, 0.5),
            (knob("delay-mix", "Mix", 0.5, "#67E8F9", vec![("delay", "mix", 0.0, 1.0)]), 0.0, 0.7),
            // Row 2: Delay 2
            (knob("delay-type2", "Type 2", 0.3, "#22D3EE", vec![("delay2", "type", 0.0, 1.0)]), 0.0, 0.5),
            (knob("delay-time2", "Time 2", 0.5, "#67E8F9", vec![("delay2", "time", 0.0, 1.0)]), 0.1, 0.8),
            (knob("delay-fb2", "FB 2", 0.4, "#A5F3FC", vec![("delay2", "feedback", 0.0, 1.0)]), 0.0, 0.65),
            (knob("delay-lopass", "LoPass", 0.8, "#CFFAFE", vec![("delay", "lopass", 0.0, 1.0)]), 0.0, 0.5),
            (knob("delay-ducking", "Ducking", 0.3, "#67E8F9", vec![("delay", "ducking", 0.0, 1.0)]), 0.0, 0.6),
        ],
    ));
    bank.knobs.push(parent_knob(
        "reverb", "Reverb", 0.4, "#0EA5E9",
        vec![
            // Row 1: Reverb 1
            (knob("reverb-type1", "Type 1", 0.2, "#38BDF8", vec![("reverb1", "type", 0.0, 1.0)]), 0.0, 0.5),
            (knob("reverb-time1", "Time 1", 0.6, "#7DD3FC", vec![("reverb1", "time", 0.0, 1.0)]), 0.1, 0.9),
            (knob("reverb-predelay1", "Pre-Dly 1", 0.3, "#BAE6FD", vec![("reverb1", "predelay", 0.0, 1.0)]), 0.0, 0.5),
            (knob("reverb-character1", "Char 1", 0.5, "#E0F2FE", vec![("reverb1", "character", 0.0, 1.0)]), 0.0, 0.8),
            (knob("reverb-mix", "Mix", 0.45, "#7DD3FC", vec![("reverb", "mix", 0.0, 1.0)]), 0.0, 0.7),
            // Row 2: Reverb 2
            (knob("reverb-type2", "Type 2", 0.2, "#38BDF8", vec![("reverb2", "type", 0.0, 1.0)]), 0.0, 0.5),
            (knob("reverb-time2", "Time 2", 0.6, "#7DD3FC", vec![("reverb2", "time", 0.0, 1.0)]), 0.1, 0.9),
            (knob("reverb-predelay2", "Pre-Dly 2", 0.3, "#BAE6FD", vec![("reverb2", "predelay", 0.0, 1.0)]), 0.0, 0.5),
            (knob("reverb-character2", "Char 2", 0.5, "#E0F2FE", vec![("reverb2", "character", 0.0, 1.0)]), 0.0, 0.8),
            (knob("reverb-ducking", "Ducking", 0.3, "#7DD3FC", vec![("reverb", "ducking", 0.0, 1.0)]), 0.0, 0.6),
        ],
    ));
    bank.knobs.push(knob("motion", "Motion", 0.2, "#8B5CF6", vec![("motion", "amount", 0.0, 1.0)]));
    bank.knobs.push(knob("output", "Output", 0.8, "#6B7280", vec![("output", "level", 0.0, 1.0)]));

    apply_bypass_rules(&mut bank, &rules);
    bank
}

// ============================================================================
// MacroBar — horizontal strip of macro knobs
// ============================================================================

#[component]
pub(crate) fn MacroBar(
    bank: Signal<MacroBank>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    let current_bank = bank();

    rsx! {
        div { class: "shrink-0 px-3 py-2 border-b border-zinc-800/50 bg-zinc-950/30 overflow-visible",
            div { class: "flex items-start w-full overflow-visible",
                for knob in current_bank.knobs.iter() {
                    MacroCell {
                        knob: knob.clone(),
                        bank: bank,
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
    bank: Signal<MacroBank>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    let kid = knob.id.clone();
    let knob_label = knob.label.clone();
    let knob_color = knob.color.clone();
    let knob_bindings = knob.bindings.clone();
    let value = knob.value;
    let has_children = knob.has_children();
    let readout = knob.format_value();
    let children_data: Vec<MacroKnob> = knob.children.clone();

    rsx! {
        div {
            class: "relative group/macro flex-1 min-w-0",

            // Main knob cell
            div {
                class: "flex flex-col items-center gap-0.5 py-1.5 rounded-xl \
                         hover:bg-zinc-800/40 cursor-pointer border border-transparent transition-all",

                // Label (above knob)
                div { class: "flex items-center gap-0.5 justify-center w-full",
                    span {
                        class: "text-[10px] font-medium truncate max-w-[56px]",
                        style: if let Some(ref c) = knob_color {
                            format!("color: {};", c)
                        } else {
                            "color: #94a3b8;".into()
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

                // Knob
                MiniKnob {
                    value,
                    color: knob_color.clone(),
                    on_change: {
                        let kid = kid.clone();
                        let bindings = knob_bindings.clone();
                        let has_kids = has_children;
                        move |new_val: f32| {
                            let mut bk = bank();
                            if let Some(k) = bk.get_knob_mut(&kid) {
                                k.set_value(new_val);
                            }
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
                            apply_bypass_rules(&mut bk, &bypass_rules());
                            bank.set(bk);
                        }
                    },
                }

                // Value readout
                span {
                    class: "text-[9px] font-mono tabular-nums text-zinc-400",
                    "{readout}"
                }

            }

            // Sub-macro dropdown — pure CSS show/hide via group-hover
            if has_children {
                if kid == "delay" {
                    DualRowDropdown {
                        prefix: String::from("delay"),
                        headers: vec![String::from("Type"), String::from("Time"), String::from("Feedback"), String::from("Filter"), String::from("Level")],
                        children_knobs: children_data,
                        bank: bank,
                    }
                } else if kid == "reverb" {
                    DualRowDropdown {
                        prefix: String::from("reverb"),
                        headers: vec![String::from("Type"), String::from("Time"), String::from("Pre-Delay"), String::from("Character"), String::from("Level")],
                        children_knobs: children_data,
                        bank: bank,
                    }
                } else {
                    SubMacroDropdown {
                        parent_id: kid.clone(),
                        children_knobs: children_data,
                        bank: bank,
                        bypass_rules: bypass_rules,
                    }
                }
            }
        }
    }
}

// ============================================================================
// DropdownPanel — viewport-aware wrapper that nudges horizontally to stay visible
// ============================================================================

const VIEWPORT_PADDING: f64 = 8.0;

/// Wrapper for hover dropdowns. Measures itself on mount and applies a
/// horizontal offset so the panel never overflows the window edges.
#[component]
fn DropdownPanel(children: Element) -> Element {
    let mut offset_x = use_signal(|| 0.0_f64);
    let mut mounted_el: Signal<Option<Rc<MountedData>>> = use_signal(|| None);

    // Measure and correct on mount
    let measure = move || {
        if let Some(el) = mounted_el.read().as_ref() {
            let el = el.clone();
            spawn(async move {
                // Get window width from webview JS
                let mut eval = document::eval("dioxus.send(window.innerWidth);");
                let window_w: f64 = match eval.recv::<f64>().await {
                    Ok(val) => val,
                    Err(_) => 1920.0,
                };

                if let Ok(rect) = el.get_client_rect().await {
                    let left = rect.origin.x;
                    let right = left + rect.size.width;

                    let mut nudge = 0.0;
                    if right > window_w - VIEWPORT_PADDING {
                        nudge = (window_w - VIEWPORT_PADDING) - right;
                    }
                    if left + nudge < VIEWPORT_PADDING {
                        nudge = VIEWPORT_PADDING - left;
                    }
                    if nudge.abs() > 0.5 {
                        offset_x.set(nudge);
                    }
                }
            });
        }
    };

    rsx! {
        // Invisible bridge: fills the gap between parent cell and dropdown
        div {
            class: "absolute top-full left-0 w-full h-2 \
                    opacity-0 group-hover/macro:opacity-100 \
                    pointer-events-none group-hover/macro:pointer-events-auto",
        }
        // The panel
        div {
            class: "absolute top-full left-1/2 -translate-x-1/2 mt-2 z-50 \
                    rounded-xl border border-zinc-700/80 bg-zinc-900/95 \
                    shadow-xl backdrop-blur-sm p-2 \
                    opacity-0 scale-95 translate-y-[-4px] \
                    group-hover/macro:opacity-100 group-hover/macro:scale-100 \
                    group-hover/macro:translate-y-0 \
                    pointer-events-none group-hover/macro:pointer-events-auto \
                    transition-all duration-150 ease-out",
            style: format!("margin-left: {off}px;", off = offset_x()),
            onmounted: move |evt: MountedEvent| {
                mounted_el.set(Some(evt.data()));
                measure();
            },
            {children}
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
    bank: Signal<MacroBank>,
    bypass_rules: Signal<BypassRules>,
) -> Element {
    let rules = bypass_rules();
    let parent_rules = rules.get(&parent_id);
    let has_bypass = |child_id: &str| -> bool {
        parent_rules
            .map(|r| r.iter().any(|rule| rule.child_id == child_id))
            .unwrap_or(false)
    };

    rsx! {
        DropdownPanel {
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

                        let cell_opacity = if child_has_bypass && child_bypassed { "opacity-40" } else { "" };

                        rsx! {
                            div {
                                class: format!(
                                    "flex flex-col items-center gap-1 w-[68px] py-1.5 rounded-lg \
                                     cursor-pointer transition-all \
                                     hover:bg-zinc-700/40 border border-transparent {}",
                                    cell_opacity,
                                ),

                                // Bypass pad button
                                if child_has_bypass {
                                    {
                                        let pad_color = child_color.as_deref().unwrap_or("#94a3b8");
                                        let child_id = child_id.clone();
                                        rsx! {
                                            button {
                                                class: if child_bypassed {
                                                    "w-[48px] h-[16px] rounded-md text-[8px] font-bold \
                                                     border border-zinc-600 bg-zinc-800/60 text-zinc-600 \
                                                     hover:border-zinc-500 transition-all uppercase tracking-wider"
                                                } else {
                                                    "w-[48px] h-[16px] rounded-md text-[8px] font-bold \
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
                                                        let mut bk = bank();
                                                        if let Some(k) = bk.get_knob_mut(&child_id) {
                                                            k.bypassed = !k.bypassed;
                                                        }
                                                        bank.set(bk);
                                                    }
                                                },
                                                if child_bypassed { "OFF" } else { "ON" }
                                            }
                                        }
                                    }
                                }

                                // Label
                                span {
                                    class: "text-[9px] font-medium truncate max-w-[56px]",
                                    style: if let Some(ref c) = child_color {
                                        format!("color: {};", c)
                                    } else {
                                        "color: #94a3b8;".into()
                                    },
                                    "{child_label}"
                                }

                                // Knob
                                MiniKnob {
                                    value: child_value,
                                    color: child_color.clone(),
                                    on_change: {
                                        let child_id = child_id.clone();
                                        move |new_val: f32| {
                                            let mut bk = bank();
                                            if let Some(k) = bk.get_knob_mut(&child_id) {
                                                k.set_value(new_val);
                                            }
                                            bank.set(bk);
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
// DualRowDropdown — 2-row grid for dual-layer effects (Delay, Reverb, etc.)
// with linkable Type & Time columns
// ============================================================================

#[component]
fn DualRowDropdown(
    /// Knob ID prefix, e.g. "delay" or "reverb" — used for link mirror lookups.
    prefix: String,
    /// Column headers (must be exactly 5).
    headers: Vec<String>,
    children_knobs: Vec<MacroKnob>,
    bank: Signal<MacroBank>,
) -> Element {
    let mut type_linked = use_signal(|| false);
    let mut time_linked = use_signal(|| false);
    let prefix_clone = prefix.clone();

    let row1: Vec<&MacroKnob> = children_knobs[0..5].iter().collect();
    let row2: Vec<&MacroKnob> = children_knobs[5..10].iter().collect();

    rsx! {
        DropdownPanel {
            // CSS Grid: 5 columns
            div {
                class: "grid gap-x-1 gap-y-0",
                style: "grid-template-columns: repeat(5, 68px);",

                // ── Header row ──
                for header in headers.iter() {
                    div {
                        class: "text-center text-[9px] font-semibold text-zinc-500 \
                                uppercase tracking-wider py-1",
                        "{header}"
                    }
                }

                // ── Row 1 ──
                for child in row1.iter() {
                    {dual_row_knob_cell(child, &prefix, bank, type_linked, time_linked)}
                }

                // ── Link buttons row ──
                // Type link
                div { class: "flex items-center justify-center py-0.5",
                    button {
                        class: if (type_linked)() {
                            "text-[12px] leading-none px-1 py-0.5 rounded \
                             text-cyan-400 bg-cyan-900/40 hover:bg-cyan-800/50 transition-all"
                        } else {
                            "text-[12px] leading-none px-1 py-0.5 rounded \
                             text-zinc-600 hover:text-zinc-400 hover:bg-zinc-700/40 transition-all"
                        },
                        onclick: move |_| type_linked.set(!(type_linked)()),
                        "\u{1F517}"
                    }
                }
                // Time link
                div { class: "flex items-center justify-center py-0.5",
                    button {
                        class: if (time_linked)() {
                            "text-[12px] leading-none px-1 py-0.5 rounded \
                             text-cyan-400 bg-cyan-900/40 hover:bg-cyan-800/50 transition-all"
                        } else {
                            "text-[12px] leading-none px-1 py-0.5 rounded \
                             text-zinc-600 hover:text-zinc-400 hover:bg-zinc-700/40 transition-all"
                        },
                        onclick: move |_| time_linked.set(!(time_linked)()),
                        "\u{1F517}"
                    }
                }
                // Empty cells for columns 3-5 (no link buttons)
                div {}
                div {}
                div {}

                // ── Row 2 ──
                for child in row2.iter() {
                    {dual_row_knob_cell(child, &prefix_clone, bank, type_linked, time_linked)}
                }
            }
        }
    }
}

/// Render a single knob cell in a dual-row grid, with link mirroring logic.
fn dual_row_knob_cell(
    child: &MacroKnob,
    prefix: &str,
    mut bank: Signal<MacroBank>,
    type_linked: Signal<bool>,
    time_linked: Signal<bool>,
) -> Element {
    let child_id = child.id.clone();
    let child_label = child.label.clone();
    let child_color = child.color.clone();
    let child_value = child.value;
    let child_readout = child.format_value();
    let prefix = prefix.to_string();

    rsx! {
        div {
            class: "flex flex-col items-center gap-1 py-1.5 rounded-lg \
                     cursor-pointer transition-all \
                     hover:bg-zinc-700/40 border border-transparent",

            // Label
            span {
                class: "text-[9px] font-medium truncate max-w-[56px]",
                style: if let Some(ref c) = child_color {
                    format!("color: {};", c)
                } else {
                    "color: #94a3b8;".into()
                },
                "{child_label}"
            }

            // Knob
            MiniKnob {
                value: child_value,
                color: child_color.clone(),
                on_change: {
                    let child_id = child_id.clone();
                    let prefix = prefix.clone();
                    move |new_val: f32| {
                        let mut bk = bank();
                        if let Some(k) = bk.get_knob_mut(&child_id) {
                            k.set_value(new_val);
                        }
                        // Mirror linked knobs
                        let mirror_id = linked_mirror(&child_id, &prefix, (type_linked)(), (time_linked)());
                        if let Some(mid) = mirror_id {
                            if let Some(mk) = bk.get_knob_mut(&mid) {
                                mk.set_value(new_val);
                            }
                        }
                        bank.set(bk);
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

/// Given a knob ID, prefix, and link states, return the mirror knob ID if linked.
fn linked_mirror(id: &str, prefix: &str, type_linked: bool, time_linked: bool) -> Option<String> {
    if type_linked {
        let t1 = format!("{prefix}-type1");
        let t2 = format!("{prefix}-type2");
        if id == t1 { return Some(t2); }
        if id == t2 { return Some(t1); }
    }
    if time_linked {
        let t1 = format!("{prefix}-time1");
        let t2 = format!("{prefix}-time2");
        if id == t1 { return Some(t2); }
        if id == t2 { return Some(t1); }
    }
    None
}
