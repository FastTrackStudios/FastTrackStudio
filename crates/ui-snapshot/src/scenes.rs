//! Scenes — one Dioxus component per snapshot. Keep them self-contained and
//! deterministic (no time, no randomness, no animated content).

use dioxus::prelude::*;
use fts_ui::lucide_dioxus;

/// Row of default-color icons + row of theme-tinted icons + sizes.
/// Exercises: SVG element rendering, currentColor attribute substitution,
/// CSS `color` cascade into the SVG source, per-path stroke-width.
pub fn icons_default() -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground",
            div { class: "flex flex-col gap-6",
                // Default (foreground) row
                div { class: "flex items-center gap-4",
                    lucide_dioxus::Check        { size: 24 }
                    lucide_dioxus::X            { size: 24 }
                    lucide_dioxus::Search       { size: 24 }
                    lucide_dioxus::House        { size: 24 }
                    lucide_dioxus::Settings     { size: 24 }
                    lucide_dioxus::Bell         { size: 24 }
                    lucide_dioxus::Heart        { size: 24 }
                    lucide_dioxus::Star         { size: 24 }
                    lucide_dioxus::ChevronRight { size: 24 }
                    lucide_dioxus::ChevronDown  { size: 24 }
                }
                // Theme-tinted row (tailwind utility)
                div { class: "flex items-center gap-4",
                    span { class: "text-destructive",       lucide_dioxus::CircleAlert   { size: 24 } }
                    span { class: "text-primary",           lucide_dioxus::Info          { size: 24 } }
                    span { class: "text-chart-2",           lucide_dioxus::CircleCheck   { size: 24 } }
                    span { class: "text-chart-4",           lucide_dioxus::TriangleAlert { size: 24 } }
                    span { class: "text-muted-foreground",  lucide_dioxus::Circle        { size: 24 } }
                }
                // Inline style (control — proves CSS → SVG chain works)
                div { class: "flex items-center gap-4",
                    span { style: "color: #dc2626;", lucide_dioxus::CircleAlert   { size: 24 } }
                    span { style: "color: #2563eb;", lucide_dioxus::Info          { size: 24 } }
                    span { style: "color: #16a34a;", lucide_dioxus::CircleCheck   { size: 24 } }
                }
                // Size scale (ensures stroke-width scales, viewBox correct)
                div { class: "flex items-end gap-4",
                    lucide_dioxus::Star { size: 12 }
                    lucide_dioxus::Star { size: 16 }
                    lucide_dioxus::Star { size: 24 }
                    lucide_dioxus::Star { size: 32 }
                    lucide_dioxus::Star { size: 48 }
                }
            }
        }
    }
}
