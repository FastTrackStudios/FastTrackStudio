//! Empty grid cell component — placeholder with hover-to-reveal "+" button.

use dioxus::prelude::*;

// ─────────────────────────────────────────────────────────────────────────────
// EmptyGridCell component
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(super) struct EmptyGridCellProps {
    pub col: usize,
    pub row: usize,
    /// Another block is being dragged over this cell.
    pub is_drag_target: bool,
    /// Any drag interaction is in progress (block, group, or wire).
    pub is_any_drag: bool,
    /// The block picker dropdown is currently open at this cell.
    pub picker_open_here: bool,
    /// Called when the "+" button is clicked (idle state only).
    pub on_click: EventHandler<MouseEvent>,
}

/// Empty grid cell — three visual states:
/// 1. **Drop target**: cyan dashed highlight when a dragged block hovers over.
/// 2. **Passive drop zone**: faint dashed border during any drag.
/// 3. **Idle**: invisible until hovered (CSS `:hover`), click opens the block picker.
#[component]
pub(super) fn EmptyGridCell(props: EmptyGridCellProps) -> Element {
    let col = props.col;
    let row = props.row;

    rsx! {
        div {
            key: "empty-{col}-{row}",
            class: "relative aspect-square",
            if props.is_drag_target {
                // Active drop target highlight
                div {
                    class: "absolute inset-0 flex items-center justify-center \
                         rounded-lg border-2 border-dashed border-cyan-400/60 \
                         bg-cyan-400/10",
                    span {
                        class: "text-cyan-400/60 text-xs font-mono",
                        "drop"
                    }
                }
            } else if props.is_any_drag {
                // Passive drop zone during drag
                div {
                    class: "absolute inset-0 flex items-center justify-center \
                         rounded-lg border border-dashed \
                         border-zinc-700/30 bg-zinc-800/5",
                }
            } else {
                // Idle: invisible until hovered via CSS :hover.
                // picker_open_here forces visibility while the picker is open at this cell.
                div {
                    class: if props.picker_open_here {
                        "group absolute inset-0 flex items-center justify-center \
                         rounded-lg border border-dashed cursor-pointer \
                         border-zinc-600/40 bg-zinc-800/20"
                    } else {
                        "group absolute inset-0 flex items-center justify-center \
                         rounded-lg border border-dashed cursor-pointer \
                         border-transparent bg-transparent \
                         hover:border-zinc-600/40 hover:bg-zinc-800/20"
                    },
                    onclick: move |evt| props.on_click.call(evt),
                    span {
                        class: if props.picker_open_here {
                            "text-zinc-600 text-sm opacity-70"
                        } else {
                            "text-zinc-600 text-sm opacity-0 group-hover:opacity-70"
                        },
                        "+"
                    }
                }
            }
        }
    }
}
