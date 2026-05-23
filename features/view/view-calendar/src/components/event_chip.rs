//! Absolutely-positioned event block used in the week/day time grid.
//!
//! Month-view chips have their own small renderer because they sit
//! inside a CSS grid track (no absolute positioning) — see
//! `month_view::MonthChip`.

use dioxus::prelude::*;

use crate::types::CalendarEvent;

use super::drag::{DragKind, DragState, use_drag_context};
use super::style::chip_palette;

#[derive(Props, Clone, PartialEq)]
pub struct EventChipProps {
    pub event: CalendarEvent,
    /// CSS `style` string handling absolute positioning. The
    /// time-grid view computes `top: Xpx; height: Ypx; left: Z%;
    /// width: W%;` and passes it whole.
    pub position_style: String,
    #[props(default = false)]
    pub readonly: bool,
    pub on_click: EventHandler<()>,
}

#[component]
pub fn EventChip(props: EventChipProps) -> Element {
    let ctx = use_drag_context();
    let mut drag = ctx.state;
    let event = props.event.clone();
    let event_id = event.id;

    let palette = chip_palette(event.color);
    let is_dragging = drag
        .read()
        .as_ref()
        .is_some_and(|d| d.event == event_id && d.committed);
    // Hide the chip almost entirely during drag — the ghost block
    // shows where it will land. Faint outline keeps the spot legible
    // until drop, then snaps back into place.
    let drag_style = if is_dragging {
        "opacity: 0.15; pointer-events: none;"
    } else {
        ""
    };

    let on_click = props.on_click;
    let style = format!("{}; {drag_style}", props.position_style);
    let time_label = format_time_range(&event);

    // Adaptive layout — short events lose the time row, very short
    // events collapse to a single horizontal line so the title
    // still survives a 15-min slot.
    let duration_min = event.duration_minutes();
    let dense = duration_min < 50; // ≤40px tall: drop time row, tighter padding
    let inline = duration_min < 25; // ≤20px tall: title + time on one row

    let body = palette.body;
    let hover = palette.hover;
    let padding = if inline {
        "px-2 py-0"
    } else if dense {
        "px-2 py-0.5"
    } else {
        "px-2 py-1"
    };

    rsx! {
        div {
            class: "absolute rounded-md cursor-grab active:cursor-grabbing select-none overflow-hidden shadow-sm transition-colors {padding} {body} {hover}",
            style: "{style}",
            onpointerdown: move |e: Event<PointerData>| {
                if props.readonly { return; }
                e.stop_propagation();
                let p = e.data().page_coordinates();
                drag.set(Some(DragState {
                    event: event_id,
                    kind: DragKind::Move,
                    orig_start: event.start,
                    orig_end: event.end,
                    color: event.color,
                    title: event.title.clone(),
                    start_page_x: p.x,
                    start_page_y: p.y,
                    committed: false,
                }));
            },
            onclick: move |e: MouseEvent| {
                e.stop_propagation();
                on_click.call(());
            },
            if inline {
                div { class: "flex items-baseline gap-1.5 text-[11px] leading-4 truncate",
                    span { class: "font-semibold truncate", "{event.title}" }
                    span { class: "opacity-85 shrink-0 text-[10px]", "{time_label}" }
                }
            } else if dense {
                div { class: "text-[11px] leading-4 font-semibold truncate", "{event.title}" }
            } else {
                div { class: "text-[11px] leading-4 font-semibold truncate", "{event.title}" }
                div { class: "text-[10px] leading-3 opacity-85 truncate", "{time_label}" }
            }
            // Resize handles — top + bottom edges.
            if !props.readonly {
                ResizeHandle {
                    event_id,
                    orig_start: event.start,
                    orig_end: event.end,
                    color: event.color,
                    title: event.title.clone(),
                    edge: ResizeEdge::Top,
                }
                ResizeHandle {
                    event_id,
                    orig_start: event.start,
                    orig_end: event.end,
                    color: event.color,
                    title: event.title.clone(),
                    edge: ResizeEdge::Bottom,
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum ResizeEdge {
    Top,
    Bottom,
}

#[derive(Props, Clone, PartialEq)]
struct ResizeHandleProps {
    event_id: crate::types::EventId,
    orig_start: chrono::DateTime<chrono::Utc>,
    orig_end: chrono::DateTime<chrono::Utc>,
    color: crate::types::ColorTag,
    title: String,
    edge: ResizeEdge,
}

#[component]
fn ResizeHandle(props: ResizeHandleProps) -> Element {
    let ctx = use_drag_context();
    let mut drag = ctx.state;
    let event_id = props.event_id;
    let orig_start = props.orig_start;
    let orig_end = props.orig_end;
    let color = props.color;
    let title = props.title.clone();
    let edge = props.edge;
    let position = match edge {
        ResizeEdge::Top => "top-0",
        ResizeEdge::Bottom => "bottom-0",
    };

    rsx! {
        div {
            class: "absolute left-0 right-0 {position} h-1.5 cursor-ns-resize hover:bg-white/40 z-10",
            onpointerdown: move |e: Event<PointerData>| {
                e.stop_propagation();
                let p = e.data().page_coordinates();
                drag.set(Some(DragState {
                    event: event_id,
                    kind: match edge {
                        ResizeEdge::Top => DragKind::ResizeStart,
                        ResizeEdge::Bottom => DragKind::ResizeEnd,
                    },
                    orig_start,
                    orig_end,
                    color,
                    title: title.clone(),
                    start_page_x: p.x,
                    start_page_y: p.y,
                    // Resize handles always commit immediately — the
                    // tiny 6px handle has no "click vs drag"
                    // ambiguity to resolve.
                    committed: true,
                }));
            },
        }
    }
}

fn format_time_range(ev: &CalendarEvent) -> String {
    use chrono::Timelike;
    let s = ev.start.naive_utc();
    let e = ev.end.naive_utc();
    let fmt = |h: u32, m: u32| {
        let (h12, suf) = if h == 0 {
            (12, "AM")
        } else if h < 12 {
            (h, "AM")
        } else if h == 12 {
            (12, "PM")
        } else {
            (h - 12, "PM")
        };
        if m == 0 {
            format!("{h12}{suf}")
        } else {
            format!("{h12}:{m:02}{suf}")
        }
    };
    format!(
        "{} – {}",
        fmt(s.hour(), s.minute()),
        fmt(e.hour(), e.minute())
    )
}
