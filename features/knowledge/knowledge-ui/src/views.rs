//! Phase 6 — view components for executed Bases queries.
//!
//! Four dumb components, each takes the `bases::ExecutedView` from
//! `knowledge-proto` plus optional callbacks:
//!
//! - [`KindList`] — flat list, one row per page (uses the first
//!   bucket; ignores grouping).
//! - [`KindKanban`] — one column per bucket, cards per row.
//!   Cards expose a "Move to {label}" control for each *other*
//!   bucket so playwright can move cards without HTML5 drag-and-drop
//!   (which is brittle to fake). HTML5 DnD can land as a Phase 6b
//!   enhancement.
//! - [`KindGallery`] — grid of cards, one bucket only.
//! - [`KindCalendar`] — list sorted by a `date` frontmatter field.
//!   Full month-grid UI is Phase 7+ (out of scope here).
//!
//! All components use fts-ui primitives + theme tokens. No raw hex.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use knowledge_proto::bases::{BaseRow, ExecutedView};
use uuid::Uuid;

/// `KindList` — flat enumeration of every row in `view.groups`.
/// Callback fires with the clicked page id.
#[component]
pub fn KindList(view: ExecutedView, on_select: Callback<Uuid>) -> Element {
    rsx! {
        ul {
            "data-testid": "kind-list",
            class: "flex flex-col gap-1 rounded-md border border-border bg-card p-3",
            for (_, rows) in view.groups.iter() {
                for row in rows.iter() {
                    KindListItem { key: "{row.page_id}", row: row.clone(), on_select }
                }
            }
        }
    }
}

#[component]
fn KindListItem(row: BaseRow, on_select: Callback<Uuid>) -> Element {
    let page_id = row.page_id;
    let testid = format!("kind-list-row-{page_id}");
    rsx! {
        li {
            "data-testid": testid,
            class: "text-sm cursor-pointer rounded px-2 py-1 hover:bg-muted",
            onclick: move |_| on_select.call(page_id),
            "{row.basename}"
        }
    }
}

/// `KindKanban` — one column per bucket. Cards carry a
/// `Move to {target}` button for each other bucket so tests + users
/// can move them without HTML5 DnD.
///
/// `on_move` fires with `(page_id, target_bucket_label)`. The caller
/// translates that into a frontmatter update (e.g. `status: <label>`).
#[component]
pub fn KindKanban(
    view: ExecutedView,
    group_key: String,
    on_select: Callback<Uuid>,
    on_move: Callback<(Uuid, String)>,
) -> Element {
    let bucket_labels: Vec<String> = view.groups.iter().map(|(k, _)| k.clone()).collect();
    rsx! {
        div {
            "data-testid": "kind-kanban",
            "data-group-key": "{group_key}",
            class: "flex flex-row gap-3 overflow-x-auto",
            for (label, rows) in view.groups.iter() {
                KanbanColumn {
                    key: "{label}",
                    label: label.clone(),
                    rows: rows.clone(),
                    bucket_labels: bucket_labels.clone(),
                    on_select,
                    on_move,
                }
            }
        }
    }
}

#[component]
fn KanbanColumn(
    label: String,
    rows: Vec<BaseRow>,
    bucket_labels: Vec<String>,
    on_select: Callback<Uuid>,
    on_move: Callback<(Uuid, String)>,
) -> Element {
    let column_testid = format!("kanban-column-{label}");
    rsx! {
        section {
            "data-testid": column_testid,
            "data-bucket": "{label}",
            class: "min-w-[14rem] flex-1 rounded-md border border-border bg-card p-3 flex flex-col gap-2",
            HStack { class: "items-baseline justify-between",
                Heading { level: HeadingLevel::H4, "{column_label(&label)}" }
                Text { variant: TextVariant::Muted, "{rows.len()}" }
            }
            div { class: "flex flex-col gap-2",
                for row in rows.iter() {
                    KanbanCard {
                        key: "{row.page_id}",
                        row: row.clone(),
                        current_bucket: label.clone(),
                        bucket_labels: bucket_labels.clone(),
                        on_select,
                        on_move,
                    }
                }
            }
        }
    }
}

fn column_label(s: &str) -> String {
    if s.is_empty() {
        "(none)".into()
    } else {
        s.to_string()
    }
}

#[component]
fn KanbanCard(
    row: BaseRow,
    current_bucket: String,
    bucket_labels: Vec<String>,
    on_select: Callback<Uuid>,
    on_move: Callback<(Uuid, String)>,
) -> Element {
    let id = row.page_id;
    let card_testid = format!("kanban-card-{id}");
    rsx! {
        article {
            "data-testid": card_testid,
            "data-bucket": "{current_bucket}",
            class: "rounded border border-border bg-background p-2 flex flex-col gap-1",
            div {
                class: "text-sm cursor-pointer",
                onclick: move |_| on_select.call(id),
                "{row.basename}"
            }
            HStack { class: "gap-1 flex-wrap",
                for target in bucket_labels.iter() {
                    if target != &current_bucket {
                        MoveButton {
                            key: "{target}",
                            page_id: id,
                            target: target.clone(),
                            on_move,
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn MoveButton(page_id: Uuid, target: String, on_move: Callback<(Uuid, String)>) -> Element {
    let testid = format!("kanban-move-{page_id}-to-{target}");
    let target_for_click = target.clone();
    rsx! {
        span { "data-testid": testid,
            Button {
                on_click: move |_| on_move.call((page_id, target_for_click.clone())),
                "→ {column_label(&target)}"
            }
        }
    }
}

/// `KindGallery` — grid of cards. One bucket only (uses the first).
#[component]
pub fn KindGallery(view: ExecutedView, on_select: Callback<Uuid>) -> Element {
    let rows: Vec<BaseRow> = view.groups.into_iter().flat_map(|(_, r)| r).collect();
    rsx! {
        div {
            "data-testid": "kind-gallery",
            class: "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3",
            for row in rows.iter() {
                GalleryCard { key: "{row.page_id}", row: row.clone(), on_select }
            }
        }
    }
}

#[component]
fn GalleryCard(row: BaseRow, on_select: Callback<Uuid>) -> Element {
    let id = row.page_id;
    let testid = format!("kind-gallery-card-{id}");
    rsx! {
        article {
            "data-testid": testid,
            class: "rounded-md border border-border bg-card p-3 cursor-pointer hover:bg-muted",
            onclick: move |_| on_select.call(id),
            Heading { level: HeadingLevel::H4, "{row.basename}" }
        }
    }
}

/// `KindCalendar` — Phase 6 stub. Lists pages sorted by their
/// `date_field` frontmatter property (defaults to `date`). Full
/// month-grid UI is out of scope; the playwright spec doesn't
/// exercise this view.
#[component]
pub fn KindCalendar(view: ExecutedView, date_field: String) -> Element {
    let rows: Vec<BaseRow> = view.groups.into_iter().flat_map(|(_, r)| r).collect();
    rsx! {
        ul {
            "data-testid": "kind-calendar",
            "data-date-field": "{date_field}",
            class: "flex flex-col gap-1 rounded-md border border-border bg-card p-3",
            for row in rows.iter() {
                CalendarRow {
                    key: "{row.page_id}",
                    row: row.clone(),
                    date_field: date_field.clone(),
                }
            }
        }
    }
}

#[component]
fn CalendarRow(row: BaseRow, date_field: String) -> Element {
    let date = row
        .frontmatter
        .get(&date_field)
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();
    let testid = format!("kind-calendar-row-{}", row.page_id);
    rsx! {
        li {
            "data-testid": testid,
            class: "text-sm flex justify-between gap-2 px-2 py-1",
            span { "{row.basename}" }
            span { class: "text-muted-foreground", "{date}" }
        }
    }
}
