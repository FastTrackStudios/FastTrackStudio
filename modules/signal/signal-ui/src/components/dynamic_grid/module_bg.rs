//! Container background components — visual-only backgrounds with title bars.
//!
//! Supports three nesting levels: Engine (outermost), Layer (middle), Module (innermost).

use dioxus::prelude::*;

use super::layout::{ContainerLevel, GROUP_TITLE_H};
use super::types::ModuleVisualState;

// ─────────────────────────────────────────────────────────────────────────────
// Module-level background (innermost, existing)
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
            style: "position: absolute; {bg} z-index: 3; pointer-events: none; opacity: {opacity}; transition: {transition}; {extra_style} {selection_glow}",
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

// ─────────────────────────────────────────────────────────────────────────────
// Container background (generic for Engine/Layer levels)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(super) struct ContainerBackgroundProps {
    pub name: String,
    pub bg_color: String,
    pub fg_color: String,
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
    pub level: ContainerLevel,
}

#[component]
pub(super) fn ContainerBackground(props: ContainerBackgroundProps) -> Element {
    let (bg_alpha, border_alpha, border_style, radius, z_index, font_class, label_opacity) =
        match props.level {
            ContainerLevel::Engine => (
                "04",
                "10",
                "solid",
                "10px",
                1,
                "text-[7px] font-semibold uppercase tracking-wider",
                "0.35",
            ),
            ContainerLevel::Layer => (
                "03",
                "0a",
                "dashed",
                "8px",
                2,
                "text-[7px] font-medium tracking-wide",
                "0.30",
            ),
            ContainerLevel::Module => {
                // Module level should use ModuleBackground instead, but handle gracefully
                (
                    "12",
                    "30",
                    "solid",
                    "10px",
                    3,
                    "text-[8px] font-semibold",
                    "0.80",
                )
            }
        };

    let bg = format!(
        "left: {}px; top: {}px; width: {}px; height: {}px; \
         background-color: {}{bg_alpha}; border: 1px {border_style} {}{border_alpha}; border-radius: {radius};",
        props.x, props.y, props.w, props.h, props.bg_color, props.bg_color,
    );

    rsx! {
        div {
            key: "container-{props.name}",
            class: "absolute",
            style: "position: absolute; {bg} z-index: {z_index}; pointer-events: none;",
            // Floating corner label — no title bar, no height cost
            span {
                class: "{font_class} whitespace-nowrap",
                style: "position: absolute; top: 2px; left: 6px; color: {props.fg_color}; opacity: {label_opacity};",
                "{props.name}"
            }
        }
    }
}
