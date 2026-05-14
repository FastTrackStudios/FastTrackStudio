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

/// `KindKanban` — column per bucket, cards per row. Built on
/// fts-ui primitives (Card / Badge / Item). Cards are draggable;
/// columns are drop targets. No move buttons — the kanban is a
/// purely visual drag interface. Use `KindList` for keyboard-
/// driven reordering instead.
///
/// `on_move` fires with `(page_id, target_bucket_label)`. The
/// caller translates that into a frontmatter update.
#[component]
pub fn KindKanban(
    view: ExecutedView,
    group_key: String,
    on_select: Callback<Uuid>,
    on_move: Callback<(Uuid, String)>,
) -> Element {
    rsx! {
        div {
            "data-testid": "kind-kanban",
            "data-group-key": "{group_key}",
            class: "flex flex-row gap-4 overflow-x-auto pb-2 -mx-1 px-1",
            for (label, rows) in view.groups.iter() {
                KanbanColumn {
                    key: "{label}",
                    label: label.clone(),
                    rows: rows.clone(),
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
    on_select: Callback<Uuid>,
    on_move: Callback<(Uuid, String)>,
) -> Element {
    let column_testid = format!("kanban-column-{label}");
    // Per-column DnD drop target. Loro avoids Obsidian's file-
    // watcher race so we don't need TaskNotes'
    // `suppressRenderUntil` workaround.
    let label_for_drop = label.clone();
    let count = rows.len();
    let bucket_variant = column_badge_variant(&label);
    rsx! {
        section {
            "data-testid": column_testid,
            "data-bucket": "{label}",
            class: "min-w-[18rem] w-72 shrink-0",
            ondragover: move |e| e.prevent_default(),
            ondrop: move |e| {
                e.prevent_default();
                let dt = e.data().data_transfer();
                let text = dt.get_data("text/plain").unwrap_or_default();
                if !text.is_empty() {
                    if let Ok(page_id) = text.parse::<Uuid>() {
                        on_move.call((page_id, label_for_drop.clone()));
                    }
                }
            },
            Card { class: "h-full flex flex-col",
                CardHeader { class: "py-3 pb-2",
                    HStack { class: "items-center justify-between",
                        HStack { class: "items-center gap-2",
                            StatusDot { color: bucket_dot_color(&label), size: StatusDotSize::Small }
                            Heading { level: HeadingLevel::H4, "{column_label(&label)}" }
                        }
                        Badge { variant: bucket_variant, "{count}" }
                    }
                }
                CardContent { class: "pt-0 pb-3",
                    if rows.is_empty() {
                        div { class: "rounded-md border border-dashed border-border/60 p-3 text-center text-xs text-muted-foreground",
                            "Drop here"
                        }
                    } else {
                        div { class: "flex flex-col gap-2",
                            for row in rows.iter() {
                                KanbanCard {
                                    key: "{row.page_id}",
                                    row: row.clone(),
                                    current_bucket: label.clone(),
                                    on_select,
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn column_label(s: &str) -> String {
    if s.is_empty() {
        return "(none)".into();
    }
    // Pretty-print snake_case → "Snake case".
    let mut chars = s.chars();
    let first = chars
        .next()
        .map(|c| c.to_uppercase().to_string())
        .unwrap_or_default();
    let rest: String = chars.map(|c| if c == '_' { ' ' } else { c }).collect();
    format!("{first}{rest}")
}

/// Map common bucket labels to a `StatusDot` color so columns
/// get a small leading swatch that mirrors the
/// `EnumWithMetadata.color` declaration in the task schema.
fn bucket_dot_color(label: &str) -> StatusDotColor {
    match label {
        "todo" => StatusDotColor::Neutral,
        "in_progress" => StatusDotColor::Warning,
        "blocked" => StatusDotColor::Danger,
        "done" => StatusDotColor::Success,
        _ => StatusDotColor::Neutral,
    }
}

fn column_badge_variant(label: &str) -> BadgeVariant {
    match label {
        "in_progress" => BadgeVariant::Default,
        "done" => BadgeVariant::Secondary,
        _ => BadgeVariant::Outline,
    }
}

#[component]
fn KanbanCard(row: BaseRow, current_bucket: String, on_select: Callback<Uuid>) -> Element {
    let id = row.page_id;
    let card_testid = format!("kanban-card-{id}");
    let priority = row
        .frontmatter
        .get("priority")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let priority_color = priority_dot(&priority);
    rsx! {
        article {
            "data-testid": card_testid,
            "data-bucket": "{current_bucket}",
            class: "group/card relative rounded-md border border-border bg-background hover:bg-accent/30 hover:border-border/80 transition-colors cursor-grab active:cursor-grabbing shadow-xs",
            draggable: true,
            ondragstart: move |e| {
                let dt = e.data().data_transfer();
                let _ = dt.set_data("text/plain", &id.to_string());
                let _ = dt.set_effect_allowed("move");
            },
            // Drag handle (visual; the whole card is draggable).
            // Shows on hover only — keeps the resting card clean.
            span {
                class: "absolute left-1.5 top-2 text-muted-foreground/40 text-xs opacity-0 group-hover/card:opacity-100 transition-opacity select-none",
                "⋮⋮"
            }
            div { class: "p-3 pl-5 flex flex-col gap-2",
                HStack { class: "items-start gap-2",
                    if let Some(color) = priority_color {
                        span { class: "mt-1.5",
                            StatusDot { color, size: StatusDotSize::Small }
                        }
                    }
                    div {
                        class: "flex-1 text-sm leading-snug cursor-pointer min-w-0",
                        onclick: move |_| on_select.call(id),
                        "{row.basename}"
                    }
                }
            }
        }
    }
}

fn priority_dot(priority: &str) -> Option<StatusDotColor> {
    match priority {
        "urgent" => Some(StatusDotColor::Danger),
        "high" => Some(StatusDotColor::Warning),
        "low" => Some(StatusDotColor::Neutral),
        _ => None,
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
