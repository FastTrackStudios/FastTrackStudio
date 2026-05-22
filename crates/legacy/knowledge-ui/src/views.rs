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

/// One kanban drop event. `before` is the page id of the card
/// the dragged card was hovering over at the moment of drop, or
/// `None` if the drop landed in the bucket's empty space (= append
/// to the end). The caller is responsible for computing a fresh
/// `sort_order` from `before`'s neighbors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KanbanDrop {
    pub page_id: Uuid,
    pub target_bucket: String,
    pub before: Option<Uuid>,
}

/// Shared drag state — held in Dioxus context so the column +
/// card components can coordinate without prop drilling.
#[derive(Clone, Copy)]
struct KanbanDragState {
    /// Page id of the card currently being dragged. `None` =
    /// nothing in flight.
    dragging_id: Signal<Option<Uuid>>,
    /// Page id of the card the dragged item is currently
    /// hovering over (the future `before` neighbor on drop).
    drag_over_card: Signal<Option<Uuid>>,
    /// Bucket label of the column the pointer is currently in.
    /// Used by column headers to render the highlight ring.
    drag_over_bucket: Signal<Option<String>>,
}

/// `KindKanban` — column per bucket, cards per row. dnd-kit-style
/// interactions: visible-on-hover drag handle, source-card ghost
/// while dragging, drop-position indicator above the target
/// card, animated column highlight on drag-over. Within-column
/// reorder works because the column reads `drag_over_card` on
/// drop and the caller turns that into a `LexoRank::between(prev,
/// before)` write.
#[component]
pub fn KindKanban(
    view: ExecutedView,
    group_key: String,
    on_select: Callback<Uuid>,
    on_move: Callback<KanbanDrop>,
    #[props(default)] on_add: Option<Callback<(String, String)>>,
) -> Element {
    let dragging_id: Signal<Option<Uuid>> = use_signal(|| None);
    let drag_over_card: Signal<Option<Uuid>> = use_signal(|| None);
    let drag_over_bucket: Signal<Option<String>> = use_signal(|| None);
    use_context_provider(|| KanbanDragState {
        dragging_id,
        drag_over_card,
        drag_over_bucket,
    });
    let labels: Vec<String> = view.groups.iter().map(|(k, _)| k.clone()).collect();
    let mut mobile_bucket: Signal<String> =
        use_signal(|| labels.first().cloned().unwrap_or_default());
    let mobile_value = mobile_bucket.read().clone();

    rsx! {
        div {
            "data-testid": "kind-kanban",
            "data-group-key": "{group_key}",
            "data-dragging": if dragging_id.read().is_some() { "true" } else { "false" },
            class: "flex flex-col gap-3",
            // Mobile tabs — sm:hidden.
            if labels.len() > 1 {
                div {
                    class: "sm:hidden",
                    "data-testid": "kanban-mobile-tabs",
                    Tabs {
                        value: Some(mobile_value.clone()),
                        on_change: Callback::new(move |v: String| mobile_bucket.set(v)),
                        TabList { class: "w-full",
                            for (i, label) in labels.iter().enumerate() {
                                TabTrigger {
                                    key: "{label}",
                                    value: label.clone(),
                                    index: i,
                                    "{column_label(label)}"
                                }
                            }
                        }
                    }
                }
            }
            // Board: stacks on mobile, 2-col on md, 4-col on xl.
            // The reference design uses md:grid-cols-2 — we go a
            // step further at xl to keep all four buckets in one
            // row on big screens.
            div { class: "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-4 sm:gap-6",
                for (label, rows) in view.groups.iter() {
                    div {
                        key: "{label}",
                        class: if labels.len() > 1 && *label != *mobile_value {
                            "hidden md:block"
                        } else {
                            "block"
                        },
                        KanbanColumn {
                            label: label.clone(),
                            rows: rows.clone(),
                            on_select,
                            on_move,
                            on_add,
                        }
                    }
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
    on_move: Callback<KanbanDrop>,
    on_add: Option<Callback<(String, String)>>,
) -> Element {
    let column_testid = format!("kanban-column-{label}");
    let mut state = use_context::<KanbanDragState>();
    let label_for_over = label.clone();
    let label_for_drop = label.clone();
    let label_for_class = label.clone();
    let count = rows.len();
    let bucket_variant = column_badge_variant(&label);

    let is_drag_over = state
        .drag_over_bucket
        .read()
        .as_ref()
        .map(|s| s == &label_for_class)
        .unwrap_or(false);

    rsx! {
        section {
            "data-testid": column_testid,
            "data-bucket": "{label}",
            "data-drag-over": if is_drag_over { "true" } else { "false" },
            class: "h-full",
            ondragover: move |e| {
                e.prevent_default();
                // Update the bucket highlight only when it
                // actually changes — avoids signal-write churn
                // on every mousemove.
                let same = state
                    .drag_over_bucket
                    .peek()
                    .as_ref()
                    .map(|s| s == &label_for_over)
                    .unwrap_or(false);
                if !same {
                    state.drag_over_bucket.set(Some(label_for_over.clone()));
                }
            },
            ondrop: move |e| {
                e.prevent_default();
                let dt = e.data().data_transfer();
                let text = dt.get_data("text/plain").unwrap_or_default();
                if !text.is_empty() {
                    if let Ok(page_id) = text.parse::<Uuid>() {
                        let before = *state.drag_over_card.peek();
                        on_move.call(KanbanDrop {
                            page_id,
                            target_bucket: label_for_drop.clone(),
                            before,
                        });
                    }
                }
                state.dragging_id.set(None);
                state.drag_over_card.set(None);
                state.drag_over_bucket.set(None);
            },
            Card {
                class: if is_drag_over {
                    "h-full flex flex-col ring-2 ring-primary/60 shadow-md transition-all duration-150"
                } else {
                    "h-full flex flex-col transition-all duration-150"
                },
                CardHeader { class: "py-3 pb-2 sticky top-0 bg-card/95 backdrop-blur z-10 border-b border-border/40",
                    HStack { class: "items-center justify-between",
                        HStack { class: "items-center gap-2",
                            StatusDot { color: bucket_dot_color(&label), size: StatusDotSize::Small }
                            Heading { level: HeadingLevel::H4, "{column_label(&label)}" }
                        }
                        Badge { variant: bucket_variant, "{count}" }
                    }
                }
                CardContent { class: "pt-2 pb-3 flex flex-col gap-2",
                    if let Some(on_add_cb) = on_add {
                        ColumnAddInput {
                            bucket: label.clone(),
                            on_submit: on_add_cb,
                        }
                    }
                    if rows.is_empty() {
                        // Drop placeholder grows + tints when a
                        // drag is in this column.
                        div {
                            class: if is_drag_over {
                                "rounded-md border-2 border-dashed border-primary/60 bg-primary/10 p-6 text-center text-xs text-primary transition-all"
                            } else {
                                "rounded-md border border-dashed border-border/60 p-4 text-center text-xs text-muted-foreground transition-all"
                            },
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

#[component]
fn ColumnAddInput(bucket: String, on_submit: Callback<(String, String)>) -> Element {
    let mut value = use_signal(String::new);
    let testid = format!("kanban-add-input-{bucket}");
    let submit_testid = format!("kanban-add-submit-{bucket}");
    let bucket_for_submit = bucket.clone();
    let mut submit = move |_: ()| {
        let v = value.read().trim().to_string();
        if v.is_empty() {
            return;
        }
        on_submit.call((bucket_for_submit.clone(), v));
        value.set(String::new());
    };
    let mut submit_for_enter = submit.clone();
    rsx! {
        div { class: "flex items-center gap-1.5",
            input {
                "data-testid": testid,
                r#type: "text",
                class: "flex-1 h-9 sm:h-8 rounded-md border border-border bg-background px-2.5 text-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
                value: "{value}",
                placeholder: "+ Add task",
                oninput: move |e| value.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { submit_for_enter(()); }
                },
            }
            span { "data-testid": submit_testid,
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| submit(()),
                    "↵"
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
    let due = row
        .frontmatter
        .get("due")
        .and_then(|v| v.as_str())
        .map(String::from);
    let tags: Vec<String> = row
        .frontmatter
        .get("tags")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();
    let priority_color = priority_dot(&priority);

    let mut state = use_context::<KanbanDragState>();
    let is_dragging = *state.dragging_id.read() == Some(id);
    let is_drop_target = *state.drag_over_card.read() == Some(id) && !is_dragging;

    // Outer class composes:
    //  - resting card style
    //  - hover bg shift
    //  - cursor states (grab/grabbing)
    //  - ghost effect (opacity-40) when this is the source
    //  - drop indicator (a top accent bar) when another card is
    //    about to land just before this one
    let card_class = if is_dragging {
        "group/card relative rounded-md border border-border bg-background opacity-40 cursor-grabbing shadow-xs"
    } else if is_drop_target {
        "group/card relative rounded-md border border-border bg-background hover:bg-accent/30 transition-all cursor-grab shadow-xs ring-1 ring-primary/50"
    } else {
        "group/card relative rounded-md border border-border bg-background hover:bg-accent/30 hover:border-border/80 transition-all cursor-grab shadow-xs"
    };

    rsx! {
        article {
            "data-testid": card_testid,
            "data-bucket": "{current_bucket}",
            "data-dragging": if is_dragging { "true" } else { "false" },
            "data-drop-target": if is_drop_target { "true" } else { "false" },
            class: card_class,
            draggable: true,
            ondragstart: move |e| {
                let dt = e.data().data_transfer();
                let _ = dt.set_data("text/plain", &id.to_string());
                let _ = dt.set_effect_allowed("move");
                state.dragging_id.set(Some(id));
            },
            ondragend: move |_| {
                state.dragging_id.set(None);
                state.drag_over_card.set(None);
                state.drag_over_bucket.set(None);
            },
            ondragenter: move |_| {
                // The card the dragged item is hovering over.
                // The column's `ondrop` reads this and passes it
                // as `before` so the caller can compute a
                // LexoRank between this card and its prev sibling.
                if !is_dragging {
                    let same = *state.drag_over_card.peek() == Some(id);
                    if !same {
                        state.drag_over_card.set(Some(id));
                    }
                }
            },
            // Drop-position indicator: a thin highlighted bar at
            // the top of the card the dragged item is about to
            // land before. Mirrors dnd-kit's slot animation.
            if is_drop_target {
                span {
                    class: "absolute -top-1 left-0 right-0 h-0.5 rounded-full bg-primary shadow-[0_0_6px_rgba(0,0,0,0.15)]",
                }
            }
            // Drag-handle slot. The whole card is draggable for
            // touch-friendliness, but the visible ⋮⋮ glyph on
            // hover makes the affordance obvious.
            span {
                class: "absolute left-1.5 top-2 text-muted-foreground/50 text-xs opacity-0 group-hover/card:opacity-100 transition-opacity select-none pointer-events-none",
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
                if !tags.is_empty() {
                    HStack { class: "gap-1 flex-wrap",
                        for tag in tags.iter().take(3) {
                            Badge {
                                variant: BadgeVariant::Outline,
                                class: "text-[10px] font-normal",
                                "{tag}"
                            }
                        }
                        if tags.len() > 3 {
                            Badge {
                                variant: BadgeVariant::Outline,
                                class: "text-[10px] font-normal",
                                "+{tags.len() - 3}"
                            }
                        }
                    }
                }
                if due.is_some() {
                    HStack { class: "items-center gap-1.5 pt-1 border-t border-border/40",
                        Text {
                            variant: TextVariant::Muted,
                            class: "text-xs",
                            "📅 {due.as_deref().unwrap_or(\"\")}"
                        }
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
