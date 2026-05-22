//! Day view — 1-column [`TimeGridView`].

use chrono::NaiveDate;
use dioxus::prelude::*;

use crate::store::CalendarMutation;
use crate::types::{CalendarEvent, EventId};

use super::time_grid::TimeGridView;

#[derive(Props, Clone, PartialEq)]
pub struct DayViewProps {
    pub anchor: NaiveDate,
    pub events: Vec<CalendarEvent>,
    #[props(default = false)]
    pub readonly: bool,
    pub on_event: EventHandler<CalendarMutation>,
    pub on_open_editor: EventHandler<EventId>,
}

#[component]
pub fn DayView(props: DayViewProps) -> Element {
    rsx! {
        TimeGridView {
            days: vec![props.anchor],
            events: props.events,
            readonly: props.readonly,
            on_event: props.on_event,
            on_open_editor: props.on_open_editor,
        }
    }
}
