//! Module background component — visual-only container background with title bar.

use dioxus::prelude::*;

use super::layout::GROUP_TITLE_H;
use super::types::ModuleVisualState;

// ─────────────────────────────────────────────────────────────────────────────
// ModuleBackground component
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(super) struct ModuleBackgroundProps {
    pub name: String,
    pub bg_color: String,
    pub fg_color: String,
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
    pub visual_state: ModuleVisualState,
}

/// Module group background with title bar.
///
/// Visual only — `pointer-events: none`. Interaction (select, drag) is
/// handled by the title bar hit zone layer in the orchestrator.
#[component]
pub(super) fn ModuleBackground(props: ModuleBackgroundProps) -> Element {
    let bg = format!(
        "left: {}px; top: {}px; width: {}px; height: {}px; \
         background-color: {}12; border: 1px solid {}30; border-radius: 10px;",
        props.x, props.y, props.w, props.h, props.bg_color, props.bg_color,
    );
    let title_style = format!(
        "background-color: {}20; border-bottom: 1px solid {}25; \
         border-radius: 10px 10px 0 0; height: {}px;",
        props.bg_color, props.bg_color, GROUP_TITLE_H,
    );
    let opacity = props.visual_state.opacity();
    let extra_style = props.visual_state.extra_style();
    let transition = props.visual_state.transition();
    let selection_glow = props.visual_state.selection_glow(&props.bg_color);

    rsx! {
        div {
            key: "grp-{props.name}",
            class: "absolute overflow-hidden",
            style: "position: absolute; {bg} z-index: 1; pointer-events: none; opacity: {opacity}; transition: {transition}; {extra_style} {selection_glow}",
            // Title bar — visual only (interaction handled in Layer 3)
            div {
                class: "flex items-center gap-1.5 px-2",
                style: "{title_style} pointer-events: none;",
                div {
                    class: "w-2 h-2 rounded-full flex-shrink-0",
                    style: "background-color: {props.bg_color};",
                }
                span {
                    class: "text-[8px] font-semibold tracking-wide whitespace-nowrap opacity-80",
                    style: "color: {props.fg_color};",
                    "{props.name}"
                }
            }
        }
    }
}
