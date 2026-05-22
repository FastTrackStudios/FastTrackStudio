//! `/views/table` — stub demo of the `view-table` crate.

use chrono::NaiveDate;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use view::table::{
    CellValue, Column, ColumnType, Row, SelectOption, Table, TableMutation, TableState,
    store::apply,
};

#[component]
pub fn TableView() -> Element {
    let mut state = use_signal(seed_state);

    let on_event = EventHandler::new(move |mu: TableMutation| {
        state.with_mut(|s| apply(s, &mu));
    });

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3",
            Heading { level: HeadingLevel::H1, "Table" }
            Text { variant: TextVariant::Muted,
                "Click headers to sort. Type in the filter row to narrow. Use the Columns menu to hide columns or group by one. Double-click a cell to edit."
            }
            div { class: "flex-1 min-h-0 border border-border/60 rounded-lg overflow-hidden",
                Table { state: state.read().clone(), on_event }
            }
        }
    }
}

fn seed_state() -> TableState {
    let title = Column::new("Title", ColumnType::Text);
    let mut status = Column::new("Status", ColumnType::Select);
    status.options = vec![
        SelectOption {
            value: "todo".into(),
            color: Some("slate".into()),
        },
        SelectOption {
            value: "doing".into(),
            color: Some("amber".into()),
        },
        SelectOption {
            value: "done".into(),
            color: Some("emerald".into()),
        },
        SelectOption {
            value: "blocked".into(),
            color: Some("rose".into()),
        },
    ];
    let due = Column::new("Due", ColumnType::Date);
    let points = Column::new("Points", ColumnType::Number);
    let urgent = Column::new("Urgent", ColumnType::Checkbox);

    let row = |t: &str, s: &str, d: (i32, u32, u32), p: f64, u: bool| {
        Row::new()
            .with(title.id, CellValue::Text(t.into()))
            .with(status.id, CellValue::Select(s.into()))
            .with(
                due.id,
                CellValue::Date(NaiveDate::from_ymd_opt(d.0, d.1, d.2).unwrap()),
            )
            .with(points.id, CellValue::Number(p))
            .with(urgent.id, CellValue::Checkbox(u))
    };

    let rows = vec![
        row("Ship view-table", "doing", (2026, 5, 23), 5.0, true),
        row("Wire heatmap", "todo", (2026, 5, 25), 3.0, false),
        row("Persist via CRDT", "todo", (2026, 6, 1), 8.0, false),
        row("Cookbook MVP", "blocked", (2026, 6, 10), 13.0, false),
        row("Inbox triage", "done", (2026, 5, 20), 2.0, false),
    ];

    TableState {
        columns: vec![title, status, due, points, urgent],
        rows,
        ..Default::default()
    }
}
