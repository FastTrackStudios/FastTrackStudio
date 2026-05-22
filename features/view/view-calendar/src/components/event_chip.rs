//! Absolutely-positioned event block used in the week/day time grid.
//!
//! Month-view chips have their own small renderer because they sit
//! inside a CSS grid track (no absolute positioning) — see
//! `month_view::MonthChip`.

use dioxus::prelude::*;

use crate::types::CalendarEvent;

use super::drag::{DT_MIME, DragKind, DragState, use_drag_context};

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

    let stem = event.color.stem();
    let is_dragging = drag.read().is_some_and(|d| d.event == event_id);
    let opacity = if is_dragging { "opacity: 0.4;" } else { "" };

    let on_click = props.on_click;
    let bg = format!(
        "bg-{stem}-500/30 text-{stem}-50 border-l-2 border-{stem}-500 hover:bg-{stem}-500/40"
    );
    let style = format!("{}; {opacity}", props.position_style);
    let time_label = format_time_range(&event);

    rsx! {
        div {
            class: "absolute rounded-sm px-1.5 py-0.5 cursor-pointer select-none overflow-hidden {bg}",
            style: "{style}",
            draggable: !props.readonly,
            ondragstart: move |e: Event<DragData>| {
                if props.readonly { return; }
                let dt = e.data().data_transfer();
                let _ = dt.set_data(DT_MIME, &event_id.to_string());
                drag.set(Some(DragState {
                    event: event_id,
                    kind: DragKind::Move,
                    orig_start: event.start,
                    orig_end: event.end,
                }));
            },
            ondragend: move |_| drag.set(None),
            onclick: move |e: MouseEvent| {
                e.stop_propagation();
                on_click.call(());
            },
            div { class: "text-[11px] leading-4 font-medium truncate", "{event.title}" }
            div { class: "text-[10px] leading-3 opacity-80 truncate", "{time_label}" }
            // Resize handle — bottom edge.
            if !props.readonly {
                ResizeHandle {
                    event_id,
                    orig_start: event.start,
                    orig_end: event.end,
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ResizeHandleProps {
    event_id: crate::types::EventId,
    orig_start: chrono::DateTime<chrono::Utc>,
    orig_end: chrono::DateTime<chrono::Utc>,
}

#[component]
fn ResizeHandle(props: ResizeHandleProps) -> Element {
    let ctx = use_drag_context();
    let mut drag = ctx.state;
    let event_id = props.event_id;
    let orig_start = props.orig_start;
    let orig_end = props.orig_end;

    rsx! {
        div {
            class: "absolute left-0 right-0 bottom-0 h-1 cursor-ns-resize hover:bg-foreground/40",
            draggable: true,
            ondragstart: move |e: Event<DragData>| {
                e.stop_propagation();
                let dt = e.data().data_transfer();
                let _ = dt.set_data(DT_MIME, &event_id.to_string());
                drag.set(Some(DragState {
                    event: event_id,
                    kind: DragKind::ResizeEnd,
                    orig_start,
                    orig_end,
                }));
            },
            ondragend: move |_| drag.set(None),
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
