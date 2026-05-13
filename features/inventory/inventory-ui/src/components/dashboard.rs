//! `InventoryDashboard` — page-level composition: header, stat cards,
//! filters, view-switcher, table or grid view.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Boxes, LayoutGrid, ListChecks, Table as TableIcon, Wrench};
use fts_ui::prelude::*;
use inventory_proto::{CheckoutEvent, InventoryItem};
use location_proto::Location;
use person_proto::Person;
use uuid::Uuid;

use super::card::InventoryCard;
use super::filters::{InventoryFilterState, InventoryFilters};
use super::table::InventoryTable;

#[derive(Clone, Copy, PartialEq)]
enum ViewMode {
    Table,
    Grid,
}

#[component]
pub fn InventoryDashboard(
    items: Vec<InventoryItem>,
    events: Vec<CheckoutEvent>,
    people_by_id: HashMap<Uuid, Person>,
    locations_by_id: HashMap<Uuid, Location>,
    status: String,
    on_open: EventHandler<Uuid>,
    on_checkout: EventHandler<Uuid>,
    on_checkin: EventHandler<Uuid>,
    on_retire: EventHandler<Uuid>,
) -> Element {
    let mut filters = use_signal(InventoryFilterState::default);
    let mut view = use_signal(|| ViewMode::Table);
    let _ = events; // reserved for future "checked out this week" widget.

    // Derive the manufacturer dropdown options from the data.
    let mut manufacturers: Vec<String> = items
        .iter()
        .filter_map(|i| i.manufacturer.clone())
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    manufacturers.sort();

    let total = items.len();
    let active = items.iter().filter(|i| i.status == "active").count();
    let checked_out = items.iter().filter(|i| i.status == "checked-out").count();
    let in_repair = items.iter().filter(|i| i.status == "in-repair").count();

    let f = filters.read().clone();
    let filtered: Vec<InventoryItem> = items
        .iter()
        .filter(|i| {
            f.status.as_deref().is_none_or(|s| i.status == s)
                && f.category
                    .as_deref()
                    .is_none_or(|c| i.category.as_deref() == Some(c))
                && f.manufacturer
                    .as_deref()
                    .is_none_or(|m| i.manufacturer.as_deref() == Some(m))
                && (f.search.trim().is_empty() || matches_search(i, f.search.trim()))
        })
        .cloned()
        .collect();

    rsx! {
        VStack { class: "gap-6",
            HStack { class: "items-start gap-3",
                div { class: "rounded-md bg-emerald-500/10 p-2 text-emerald-500",
                    Boxes { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Inventory" }
                    Text { variant: TextVariant::Muted,
                        "Gear, hardware, and software licences across every studio."
                    }
                }
                Spacer {}
                HStack { class: "gap-2 items-center",
                    StatusDot { color: StatusDotColor::Success, size: StatusDotSize::Small }
                    Text { variant: TextVariant::Muted, "{status}" }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total" }
                            Boxes { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{total}" }
                        Text { variant: TextVariant::Muted, "items tracked" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Active" }
                            ListChecks { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{active}" }
                        Text { variant: TextVariant::Muted, "in service" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Checked out" }
                            ListChecks { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{checked_out}" }
                        Text { variant: TextVariant::Muted, "out with someone" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "In repair" }
                            Wrench { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{in_repair}" }
                        Text { variant: TextVariant::Muted, "being fixed" }
                    }
                }
            }

            HStack { class: "items-center gap-2 flex-wrap",
                InventoryFilters {
                    value: filters.read().clone(),
                    manufacturers: manufacturers,
                    on_change: move |v: InventoryFilterState| filters.set(v),
                }
                Spacer {}
                HStack { class: "gap-1",
                    Button {
                        variant: if *view.read() == ViewMode::Table { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| view.set(ViewMode::Table),
                        TableIcon { size: 14 }
                        " Table"
                    }
                    Button {
                        variant: if *view.read() == ViewMode::Grid { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| view.set(ViewMode::Grid),
                        LayoutGrid { size: 14 }
                        " Grid"
                    }
                }
            }

            Divider {}

            SectionHeader {
                label: "Items",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }

            match *view.read() {
                ViewMode::Table => rsx! {
                    InventoryTable {
                        items: filtered.clone(),
                        people_by_id: people_by_id.clone(),
                        locations_by_id: locations_by_id.clone(),
                        on_open: on_open,
                        on_checkout: on_checkout,
                        on_checkin: on_checkin,
                        on_retire: on_retire,
                    }
                },
                ViewMode::Grid => rsx! {
                    div { class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3",
                        for item in filtered.iter().cloned() {
                            InventoryCard {
                                key: "{item.id}",
                                item: item,
                                locations_by_id: locations_by_id.clone(),
                                on_open: on_open,
                            }
                        }
                    }
                },
            }
        }
    }
}

fn matches_search(i: &InventoryItem, q: &str) -> bool {
    let q = q.to_lowercase();
    let hay = format!(
        "{} {} {} {} {}",
        i.name,
        i.manufacturer.as_deref().unwrap_or(""),
        i.model.as_deref().unwrap_or(""),
        i.serial_number.as_deref().unwrap_or(""),
        i.notes.as_deref().unwrap_or(""),
    )
    .to_lowercase();
    hay.contains(&q)
}
