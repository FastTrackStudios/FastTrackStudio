//! Location feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the location domain:
//!
//! - [`LocationList`]  — full collection view, dispatches `on_delete`
//! - [`LocationRow`]   — single-row presentation (composable into other lists)
//! - [`LocationCreateForm`] — minimal new-location form, emits the create payload

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Boxes, MapPin, Package, Plus, Trash2};
use fts_ui::prelude::*;
use location_proto::{Location, LocationCreate};
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn LocationList(items: Vec<Location>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No locations yet. Add one above.",
                icon: rsx! { MapPin { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for location in items.iter().cloned() {
                LocationRow {
                    key: "{location.id}",
                    location: location.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn LocationRow(location: Location, on_delete: EventHandler<Uuid>) -> Element {
    let id = location.id;
    let locality_parts: Vec<String> = [
        location.city.clone(),
        location.state.clone(),
        location.country_code.clone(),
    ]
    .into_iter()
    .flatten()
    .filter(|s| !s.trim().is_empty())
    .collect();
    let locality = locality_parts.join(" · ");
    let kind = location.kind.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{location.name}" }
                if !locality.is_empty() {
                    ItemDescription { "{locality}" }
                }
            }
            ItemActions { class: "gap-2",
                if let Some(k) = kind {
                    Badge { variant: BadgeVariant::Secondary, "{k}" }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn LocationCreateForm(on_submit: EventHandler<LocationCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut city = use_signal(String::new);
    let mut state = use_signal(String::new);
    let mut country_code = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add a location" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: city,
                        placeholder: "City",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: state,
                        placeholder: "State",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: country_code,
                        placeholder: "CC",
                        class: "w-24",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let payload = LocationCreate {
                                name: n,
                                kind: None,
                                address1: None,
                                address2: None,
                                city: trim_to_option(city.read().clone()),
                                state: trim_to_option(state.read().clone()),
                                postal_code: None,
                                country_code: trim_to_option(country_code.read().clone()),
                                contact_name: None,
                                contact_email: None,
                                parent_id: None,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            city.set(String::new());
                            state.set(String::new());
                            country_code.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add location"
                    }
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

/// Purpose-built locations dashboard. Page header + place stats + kind filter
/// + create form + list.
#[component]
pub fn LocationDashboard(
    items: Vec<Location>,
    status: String,
    on_create: EventHandler<LocationCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut kind_filter = use_signal(|| "all".to_string());

    let count = items.len();
    let mut by_kind: BTreeMap<String, usize> = BTreeMap::new();
    for l in &items {
        let k = l.kind.clone().unwrap_or_else(|| "unspecified".into());
        *by_kind.entry(k).or_insert(0) += 1;
    }
    let mut by_country: BTreeMap<String, usize> = BTreeMap::new();
    for l in &items {
        if let Some(cc) = l.country_code.clone() {
            *by_country.entry(cc).or_insert(0) += 1;
        }
    }
    let sub_locations = items.iter().filter(|l| l.parent_id.is_some()).count();

    let kinds: Vec<String> = by_kind.keys().cloned().collect();
    let current = kind_filter.read().clone();
    let filtered: Vec<Location> = if current == "all" {
        items.clone()
    } else {
        items
            .iter()
            .filter(|l| l.kind.clone().unwrap_or_else(|| "unspecified".into()) == current)
            .cloned()
            .collect()
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Locations",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-sky-500/10 p-2 text-sky-500",
                    MapPin { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Locations dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Studios, venues, offices, client sites — every physical place your work happens."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Places" }
                            MapPin { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{count}" }
                        Text { variant: TextVariant::Muted, "tracked" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Kinds" }
                            Boxes { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{by_kind.len()}" }
                        Text { variant: TextVariant::Muted, "distinct" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Countries" }
                            Package { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{by_country.len()}" }
                        Text { variant: TextVariant::Muted, "{sub_locations} sub-location(s)" }
                    }
                }
            }

            if kinds.len() > 1 {
                HStack { class: "gap-2 flex-wrap items-center",
                    Text { variant: TextVariant::Muted, "Filter:" }
                    Button {
                        variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| kind_filter.set("all".into()),
                        "All"
                    }
                    for k in kinds.iter().cloned() {
                        Button {
                            key: "{k}",
                            variant: if current == k { ButtonVariant::Primary } else { ButtonVariant::Outline },
                            size: ButtonSize::Small,
                            on_click: {
                                let k = k.clone();
                                move |_| kind_filter.set(k.clone())
                            },
                            "{k}"
                        }
                    }
                }
            }

            LocationCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Places",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            LocationList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}
