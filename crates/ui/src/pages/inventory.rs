//! `/inventory` — the gear register.
//!
//! Inventory items are the instruments, amps, mics, cables,
//! outboard, and computers the org owns. They live as markdown
//! pages in the vault (`type: item`) and reference their physical
//! [`location`](crate::pages::locations) by id so location renames
//! don't break the link.
//!
//! This page lists the org's items (name + category + condition /
//! status + location) and offers a friction-light "Add item" form
//! (name + category). Editing, tags, value, serials, and rename
//! live in the CLI for now; this is the read + create slice,
//! mirroring the locations page.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use inventory_proto::{Item, Status};

use crate::optimistic::{use_optimistic_list, RowState};
use crate::orgs::{OrgMeta, OrgSelection};

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

/// Common categories offered in the form's picker. `category` is
/// free-form on the model; these cover the usual gear without
/// forcing the set.
const CATEGORIES: &[&str] = &[
    "guitar", "amp", "mic", "cable", "outboard", "computer", "other",
];

/// Map a free-form status string onto a status-badge variant.
/// Unknown values fall back to neutral.
fn status_variant(status: &str) -> StatusBadgeVariant {
    match Status::from_str(status) {
        Some(Status::InUse) => StatusBadgeVariant::Success,
        Some(Status::InRepair | Status::Loaned) => StatusBadgeVariant::Warning,
        Some(Status::Missing) => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}

#[component]
pub fn InventoryView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we list / create into (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut name = use_signal(String::new);
    let mut category = use_signal(|| "other".to_string());

    // Authoritative list: optimistic create, write-through to the org's
    // InventoryService. Initial snapshot loaded below; no refetch on create.
    let list = use_optimistic_list::<Item, _>(|i| i.id);

    let loader = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_inventory(&s).await,
            None => Ok(Vec::new()),
        }
    });
    use_effect(move || {
        if let Some(Ok(rows)) = &*loader.read_unchecked() {
            list.set(rows.clone());
        }
    });

    // Create a provisional item (client-minted id; backend assigns its own
    // on write — the whole row is replaced when it returns), show it
    // instantly, then write through.
    let mut create = move || {
        let n = name.read().trim().to_string();
        if n.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        let c = category.read().clone();
        name.set(String::new());
        let provisional = Item {
            path: String::new(),
            id: uuid::Uuid::new_v4(),
            name: n.clone(),
            category: c.clone(),
            location_id: None,
            condition: inventory_proto::Condition::Good.as_str().to_owned(),
            status: inventory_proto::Status::Stored.as_str().to_owned(),
            manufacturer: None,
            model: None,
            serial: None,
            purchase_date: None,
            value: None,
            tasks: inventory_proto::StringList::default(),
            tags: inventory_proto::StringList::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
        };
        list.create(provisional, async move {
            crate::feeds::create_item(&s, &n, &c, None).await
        });
    };

    let rows = list.items().read().clone();
    let load_err: Option<String> = match &*loader.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Inventory" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{rows.len()} items" }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Instruments, amps, mics, cables, and gear the org owns.",
            }

            // ── Add item ───────────────────────────────────────────
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3 sm:flex-row sm:items-center",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Item name…",
                    value: "{name}",
                    oninput: move |e| name.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            create();
                        }
                    },
                }
                select {
                    class: "{INPUT_CLS}",
                    value: "{category}",
                    onchange: move |e| category.set(e.value()),
                    for c in CATEGORIES {
                        option { value: "{c}", "{c}" }
                    }
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| create(),
                    "Add"
                }
            }

            if let Some(err) = load_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load inventory: {err}"
                }
            }

            // ── The register ───────────────────────────────────────
            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No items yet — add your first piece of gear above." }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for item in rows {
                        ItemRow { key: "{item.id}", state: list.state(item.id), item }
                    }
                }
            }
        }
    }
}

/// One item in the register: name + category + condition + status
/// badge + optional value / location. `state` reflects optimistic
/// write-through: dimmed while `Pending`, a destructive ring while
/// `Failed`.
#[component]
fn ItemRow(item: Item, state: RowState) -> Element {
    let name = item.name.clone();
    let category = item.category.clone();
    let condition = item.condition.clone();
    let status = item.status.clone();
    let value = item.value;
    let located = item.location_id.is_some();

    let state_cls = match state {
        RowState::Settled => "border-border bg-card/40",
        RowState::Pending => "border-border bg-card/40 opacity-60",
        RowState::Failed => "border-destructive/40 bg-destructive/10",
    };

    rsx! {
        div { class: "flex items-start gap-3 rounded-lg border px-3 py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "break-words text-sm font-medium", "{name}" }
                div { class: "flex flex-wrap items-center gap-2 text-[11px] text-muted-foreground",
                    if !category.is_empty() {
                        span { class: "rounded bg-muted px-1.5 py-px", "{category}" }
                    }
                    span { "cond: {condition}" }
                    if let Some(v) = value {
                        span { "≈ {v}" }
                    }
                    if located {
                        span { "located" }
                    }
                }
            }
            div { class: "flex shrink-0 items-center gap-2",
                StatusBadge { variant: status_variant(&status), label: status.clone() }
            }
        }
    }
}
