//! `InventoryItemDetailSheet` — right-side sheet with overview / history
//! / tags / notes tabs. Primary action button flips between Checkout
//! and Checkin based on the current status.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ArrowLeftRight, LogOut, X};
use fts_ui::prelude::*;
use inventory_proto::{CheckoutEvent, InventoryItem};
use location_proto::Location;
use person_proto::Person;
use uuid::Uuid;

use super::common::{category_label, format_value_cents, status_to_badge_variant};

#[component]
pub fn InventoryItemDetailSheet(
    open: bool,
    item: Option<InventoryItem>,
    history: Vec<CheckoutEvent>,
    people_by_id: HashMap<Uuid, Person>,
    locations_by_id: HashMap<Uuid, Location>,
    on_close: EventHandler<()>,
    on_checkout: EventHandler<Uuid>,
    on_checkin: EventHandler<Uuid>,
) -> Element {
    let mut tab = use_signal(|| "overview".to_string());

    let Some(item) = item else {
        return rsx! { div {} };
    };
    let id = item.id;
    let is_checked_out = item.status == "checked-out";
    let location_name = item
        .location_id
        .and_then(|i| locations_by_id.get(&i).map(|l| l.name.clone()))
        .unwrap_or_else(|| "Unfiled".to_string());
    let owner_name = item
        .owner_id
        .and_then(|i| people_by_id.get(&i).map(|p| p.name.clone()))
        .unwrap_or_else(|| "—".to_string());
    let subtitle = match (&item.manufacturer, &item.model) {
        (Some(m), Some(mo)) => format!("{m} · {mo}"),
        (Some(m), None) => m.clone(),
        (None, Some(mo)) => mo.clone(),
        _ => "—".to_string(),
    };
    let location_full = match item.sub_location.as_deref() {
        Some(s) if !s.is_empty() => format!("{location_name} · {s}"),
        _ => location_name.clone(),
    };
    let acquired = item
        .acquired_at
        .map(|d| d.format("%Y-%m-%d").to_string())
        .unwrap_or_else(|| "—".to_string());
    let warranty = item
        .warranty_until
        .map(|d| d.format("%Y-%m-%d").to_string())
        .unwrap_or_else(|| "—".to_string());

    rsx! {
        Sheet {
            open: open,
            side: SheetSide::Right,
            on_close: move |_| on_close.call(()),
            SheetHeader {
                HStack { class: "items-start justify-between gap-3",
                    VStack { class: "gap-0.5",
                        SheetTitle { "{item.name}" }
                        SheetDescription { "{subtitle}" }
                    }
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| on_close.call(()),
                        X { size: 14 }
                    }
                }
                HStack { class: "gap-2 items-center mt-2",
                    StatusBadge {
                        variant: status_to_badge_variant(&item.status),
                        label: item.status.clone(),
                    }
                    if is_checked_out {
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| on_checkin.call(id),
                            ArrowLeftRight { size: 14 }
                            " Check in"
                        }
                    } else {
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| on_checkout.call(id),
                            LogOut { size: 14 }
                            " Checkout"
                        }
                    }
                }
            }

            div { class: "p-4",
                Tabs {
                    value: Some(tab.read().clone()),
                    on_change: move |v: String| tab.set(v),
                    TabList {
                        TabTrigger { value: "overview".to_string(), index: 0usize, "Overview" }
                        TabTrigger { value: "history".to_string(), index: 1usize, "History" }
                        TabTrigger { value: "tags".to_string(), index: 2usize, "Tags" }
                        TabTrigger { value: "notes".to_string(), index: 3usize, "Notes" }
                    }
                    TabContent { value: "overview".to_string(), index: 0usize,
                        div { class: "flex flex-col gap-2 pt-3",
                            KeyValueRow { label: "Category".to_string(), value: category_label(item.category.as_deref()) }
                            KeyValueRow { label: "Manufacturer".to_string(), value: item.manufacturer.clone().unwrap_or_else(|| "—".into()) }
                            KeyValueRow { label: "Model".to_string(), value: item.model.clone().unwrap_or_else(|| "—".into()) }
                            KeyValueRow { label: "Serial".to_string(), value: item.serial_number.clone().unwrap_or_else(|| "—".into()) }
                            KeyValueRow { label: "QR".to_string(), value: item.qr_code.clone().unwrap_or_else(|| "—".into()) }
                            KeyValueRow { label: "Owner".to_string(), value: owner_name, mono: false }
                            KeyValueRow { label: "Location".to_string(), value: location_full, mono: false }
                            KeyValueRow { label: "Value".to_string(), value: format_value_cents(item.value_cents) }
                            KeyValueRow { label: "Vendor".to_string(), value: item.vendor.clone().unwrap_or_else(|| "—".into()), mono: false }
                            KeyValueRow { label: "PO".to_string(), value: item.purchase_order.clone().unwrap_or_else(|| "—".into()) }
                            KeyValueRow { label: "Acquired".to_string(), value: acquired }
                            KeyValueRow { label: "Warranty".to_string(), value: warranty }
                        }
                    }
                    TabContent { value: "history".to_string(), index: 1usize,
                        div { class: "flex flex-col gap-2 pt-3",
                            if history.is_empty() {
                                Text { variant: TextVariant::Muted, "No checkout history yet." }
                            } else {
                                for ev in history.iter().cloned() {
                                    HistoryRow {
                                        key: "{ev.id}",
                                        event: ev,
                                        people_by_id: people_by_id.clone(),
                                    }
                                }
                            }
                        }
                    }
                    TabContent { value: "tags".to_string(), index: 2usize,
                        div { class: "flex flex-wrap gap-2 pt-3",
                            if item.tags.is_empty() {
                                Text { variant: TextVariant::Muted, "No tags." }
                            } else {
                                for tag in item.tags.iter().cloned() {
                                    Badge { variant: BadgeVariant::Secondary, "{tag}" }
                                }
                            }
                        }
                    }
                    TabContent { value: "notes".to_string(), index: 3usize,
                        div { class: "pt-3",
                            Text { "{item.notes.clone().unwrap_or_else(|| String::from(\"(no notes)\"))}" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn HistoryRow(event: CheckoutEvent, people_by_id: HashMap<Uuid, Person>) -> Element {
    let who = event
        .person_id
        .and_then(|i| people_by_id.get(&i).map(|p| p.name.clone()))
        .unwrap_or_else(|| "—".to_string());
    let when = event.checked_out_at.format("%Y-%m-%d %H:%M").to_string();
    let returned = event
        .returned_at
        .map(|d| d.format("%Y-%m-%d %H:%M").to_string())
        .unwrap_or_else(|| "open".to_string());
    let note = event.note.clone().unwrap_or_default();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{who}" }
                ItemDescription { "out " "{when}" " · back " "{returned}" }
                if !note.is_empty() {
                    ItemDescription { "{note}" }
                }
            }
            ItemActions {
                if event.returned_at.is_some() {
                    Badge { variant: BadgeVariant::Secondary, "returned" }
                } else {
                    Badge { variant: BadgeVariant::Default, "open" }
                }
            }
        }
    }
}
