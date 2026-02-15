//! Container background components — visual-only backgrounds with title bars.
//!
//! Supports three nesting levels: Engine (outermost), Layer (middle), Module (innermost).

use dioxus::prelude::*;

use super::layout::{ContainerLevel, ENGINE_TITLE_H, GROUP_TITLE_H, LAYER_TITLE_H};
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
    let (bg_alpha, border_alpha, border_style, radius, title_h, z_index, dot_size, font_class) =
        match props.level {
            ContainerLevel::Engine => (
                "06",
                "15",
                "solid",
                "14px",
                ENGINE_TITLE_H,
                1,
                "w-2.5 h-2.5",
                "text-[9px] font-bold",
            ),
            ContainerLevel::Layer => (
                "08",
                "20",
                "dashed",
                "12px",
                LAYER_TITLE_H,
                2,
                "w-2 h-2",
                "text-[8px] font-semibold",
            ),
            ContainerLevel::Module => {
                // Module level should use ModuleBackground instead, but handle gracefully
                (
                    "12",
                    "30",
                    "solid",
                    "10px",
                    GROUP_TITLE_H,
                    3,
                    "w-2 h-2",
                    "text-[8px] font-semibold",
                )
            }
        };

    let bg = format!(
        "left: {}px; top: {}px; width: {}px; height: {}px; \
         background-color: {}{bg_alpha}; border: 1px {border_style} {}{border_alpha}; border-radius: {radius};",
        props.x, props.y, props.w, props.h, props.bg_color, props.bg_color,
    );
    let title_style = format!(
        "background-color: {}10; border-bottom: 1px {border_style} {}15; \
         border-radius: {radius} {radius} 0 0; height: {title_h}px;",
        props.bg_color, props.bg_color,
    );

    rsx! {
        div {
            key: "container-{props.name}",
            class: "absolute overflow-hidden",
            style: "position: absolute; {bg} z-index: {z_index}; pointer-events: none;",
            div {
                class: "flex items-center gap-1.5 px-2.5",
                style: "{title_style} pointer-events: none;",
                div {
                    class: "{dot_size} rounded-full flex-shrink-0 opacity-60",
                    style: "background-color: {props.bg_color};",
                }
                span {
                    class: "{font_class} tracking-wide whitespace-nowrap opacity-60",
                    style: "color: {props.fg_color};",
                    "{props.name}"
                }
            }
        }
    }
}
