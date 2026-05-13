//! `InventoryTable` — Snipe-IT-style dense table view.
//!
//! Columns:
//!   [icon][name+model][status][owner][location][warranty][serial][actions]
//!
//! Row "Open" button → `on_open`. A `ContextMenu` is wired on each row
//! exposing checkout / checkin / open / retire / copy-serial actions.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Eye;
use fts_ui::prelude::*;
use inventory_proto::InventoryItem;
use location_proto::Location;
use person_proto::Person;
use uuid::Uuid;

use super::common::{category_icon, category_label, status_to_badge_variant};
use super::warranty::WarrantyBadge;

#[component]
pub fn InventoryTable(
    items: Vec<InventoryItem>,
    people_by_id: HashMap<Uuid, Person>,
    locations_by_id: HashMap<Uuid, Location>,
    on_open: EventHandler<Uuid>,
    on_checkout: EventHandler<Uuid>,
    on_checkin: EventHandler<Uuid>,
    on_retire: EventHandler<Uuid>,
) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No items match these filters.",
                icon: rsx! { div { class: "h-8 w-8" } },
            }
        };
    }
    rsx! {
        Table {
            TableHeader {
                TableRow {
                    TableHead { "" }
                    TableHead { "Name" }
                    TableHead { "Status" }
                    TableHead { "Assigned" }
                    TableHead { "Location" }
                    TableHead { "Warranty" }
                    TableHead { "Serial" }
                    TableHead { "" }
                }
            }
            TableBody {
                for item in items.iter().cloned() {
                    InventoryTableRow {
                        key: "{item.id}",
                        item: item.clone(),
                        people_by_id: people_by_id.clone(),
                        locations_by_id: locations_by_id.clone(),
                        on_open: on_open,
                        on_checkout: on_checkout,
                        on_checkin: on_checkin,
                        on_retire: on_retire,
                    }
                }
            }
        }
    }
}

#[component]
fn InventoryTableRow(
    item: InventoryItem,
    people_by_id: HashMap<Uuid, Person>,
    locations_by_id: HashMap<Uuid, Location>,
    on_open: EventHandler<Uuid>,
    on_checkout: EventHandler<Uuid>,
    on_checkin: EventHandler<Uuid>,
    on_retire: EventHandler<Uuid>,
) -> Element {
    let id = item.id;
    let owner_name = item
        .owner_id
        .and_then(|i| people_by_id.get(&i).map(|p| p.name.clone()))
        .unwrap_or_else(|| "—".to_string());
    let location_name = item
        .location_id
        .and_then(|i| locations_by_id.get(&i).map(|l| l.name.clone()))
        .unwrap_or_else(|| "Unfiled".to_string());
    let sub = item.sub_location.clone().unwrap_or_default();
    let model = item.model.clone().unwrap_or_default();
    let manufacturer = item.manufacturer.clone().unwrap_or_default();
    let subtitle = if !manufacturer.is_empty() && !model.is_empty() {
        format!("{manufacturer} · {model}")
    } else if !manufacturer.is_empty() {
        manufacturer
    } else {
        model
    };
    let serial = item
        .serial_number
        .clone()
        .unwrap_or_else(|| "—".to_string());

    rsx! {
        ContextMenu {
            ContextMenuTrigger {
                TableRow {
                    TableCell {
                        div { class: "rounded-md bg-muted p-1.5 text-foreground/80",
                            { category_icon(item.category.as_deref(), 16) }
                        }
                    }
                    TableCell {
                        div { class: "flex flex-col",
                            span { class: "font-medium text-sm", "{item.name}" }
                            if !subtitle.is_empty() {
                                span { class: "text-xs text-muted-foreground", "{subtitle}" }
                            }
                        }
                    }
                    TableCell {
                        StatusBadge {
                            variant: status_to_badge_variant(&item.status),
                            label: item.status.clone(),
                        }
                    }
                    TableCell {
                        span { class: "text-sm", "{owner_name}" }
                    }
                    TableCell {
                        div { class: "flex flex-col",
                            span { class: "text-sm", "{location_name}" }
                            if !sub.is_empty() {
                                span { class: "text-xs text-muted-foreground", "{sub}" }
                            }
                        }
                    }
                    TableCell {
                        WarrantyBadge { warranty_until: item.warranty_until }
                    }
                    TableCell {
                        Kbd { "{serial}" }
                    }
                    TableCell {
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| on_open.call(id),
                            Eye { size: 14 }
                        }
                    }
                }
            }
            ContextMenuContent {
                ContextMenuLabel { "{item.name}" }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "checkout",
                    index: 0usize,
                    on_select: move |_: String| on_checkout.call(id),
                    "Checkout…"
                }
                ContextMenuItem {
                    value: "checkin",
                    index: 1usize,
                    on_select: move |_: String| on_checkin.call(id),
                    "Check in"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "open",
                    index: 2usize,
                    on_select: move |_: String| on_open.call(id),
                    "Open details"
                }
                ContextMenuItem {
                    value: "retire",
                    index: 3usize,
                    on_select: move |_: String| on_retire.call(id),
                    "Retire"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "copy-serial",
                    index: 4usize,
                    on_select: {
                        let serial = item.serial_number.clone();
                        move |_: String| {
                            tracing::debug!(?serial, "copy serial");
                        }
                    },
                    "Copy serial"
                }
            }
        }
    }
}

/// Per-category swatch tint (Tailwind class) for icon backgrounds.
pub fn category_swatch_class(category: Option<&str>) -> &'static str {
    match category.unwrap_or("other") {
        "audio-interface" => "bg-emerald-500/10 text-emerald-500",
        "microphone" => "bg-rose-500/10 text-rose-500",
        "outboard" => "bg-purple-500/10 text-purple-500",
        "computer" => "bg-blue-500/10 text-blue-500",
        "instrument" => "bg-amber-500/10 text-amber-500",
        "cable" => "bg-zinc-500/10 text-zinc-400",
        "software-license" => "bg-cyan-500/10 text-cyan-500",
        "vehicle" => "bg-orange-500/10 text-orange-500",
        _ => "bg-muted text-muted-foreground",
    }
}

/// Tiny chip rendering "Category · subLocation" for hover/inline hints.
pub fn category_chip_label(category: Option<&str>) -> String {
    category_label(category)
}
