//! Sidebar mini-month picker — thin wrapper over fts-ui's `Calendar`.

use chrono::{DateTime, Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn MiniMonth(selected: DateTime<Utc>, on_select: EventHandler<DateTime<Utc>>) -> Element {
    let selected_local: NaiveDate = selected.with_timezone(&Local).date_naive();
    rsx! {
        div { class: "rounded-lg border border-border bg-card text-card-foreground p-1",
            Calendar {
                selected: selected_local,
                on_select: move |d: NaiveDate| {
                    if let Some(ndt) = d.and_hms_opt(0, 0, 0) {
                        if let Some(local_dt) = Local.from_local_datetime(&ndt).single() {
                            on_select.call(local_dt.with_timezone(&Utc));
                        }
                    }
                },
            }
        }
    }
}
