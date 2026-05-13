//! `InventoryFilters` — toolbar with status / category / manufacturer
//! pickers + a search input. Stateless beyond the popover internals
//! managed by fts-ui's combobox.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Search, X};
use fts_ui::prelude::*;
use inventory_proto::{INVENTORY_CATEGORIES, INVENTORY_STATUSES};

use super::common::category_label;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InventoryFilterState {
    pub status: Option<String>,
    pub category: Option<String>,
    pub manufacturer: Option<String>,
    pub search: String,
}

impl InventoryFilterState {
    pub fn is_empty(&self) -> bool {
        self.status.is_none()
            && self.category.is_none()
            && self.manufacturer.is_none()
            && self.search.trim().is_empty()
    }
}

#[component]
pub fn InventoryFilters(
    value: InventoryFilterState,
    manufacturers: Vec<String>,
    on_change: EventHandler<InventoryFilterState>,
) -> Element {
    let value_for_search = value.clone();
    let on_change_search = on_change.clone();
    let value_for_clear = value.clone();
    let on_change_clear = on_change.clone();

    rsx! {
        Toolbar { class: "flex flex-wrap items-center gap-2",
            div { class: "flex items-center gap-2 rounded-md border bg-background px-2 py-1 min-w-60",
                Search { size: 14 }
                input {
                    class: "flex-1 bg-transparent text-sm outline-none placeholder:text-muted-foreground",
                    placeholder: "Search by name, notes, serial…",
                    value: "{value.search}",
                    oninput: move |e| {
                        let mut v = value_for_search.clone();
                        v.search = e.value();
                        on_change_search.call(v);
                    },
                }
            }

            StatusPicker {
                value: value.status.clone(),
                on_change: {
                    let value = value.clone();
                    let on_change = on_change.clone();
                    move |s: Option<String>| {
                        let mut v = value.clone();
                        v.status = s;
                        on_change.call(v);
                    }
                },
            }

            CategoryPicker {
                value: value.category.clone(),
                on_change: {
                    let value = value.clone();
                    let on_change = on_change.clone();
                    move |s: Option<String>| {
                        let mut v = value.clone();
                        v.category = s;
                        on_change.call(v);
                    }
                },
            }

            ManufacturerPicker {
                value: value.manufacturer.clone(),
                options: manufacturers,
                on_change: {
                    let value = value.clone();
                    let on_change = on_change.clone();
                    move |s: Option<String>| {
                        let mut v = value.clone();
                        v.manufacturer = s;
                        on_change.call(v);
                    }
                },
            }

            if !value.is_empty() {
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| {
                        let _ = &value_for_clear;
                        on_change_clear.call(InventoryFilterState::default());
                    },
                    X { size: 12 }
                    " Clear"
                }
            }
        }
    }
}

#[component]
fn StatusPicker(value: Option<String>, on_change: EventHandler<Option<String>>) -> Element {
    let current = value.clone().unwrap_or_else(|| "Status".to_string());
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{current}"
                }
            }
            PopoverContent { class: "p-1 min-w-44",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(None),
                    "All statuses"
                }
                for s in INVENTORY_STATUSES.iter() {
                    button {
                        key: "{s}",
                        class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                        onclick: {
                            let s = s.to_string();
                            move |_| on_change.call(Some(s.clone()))
                        },
                        "{s}"
                    }
                }
            }
        }
    }
}

#[component]
fn CategoryPicker(value: Option<String>, on_change: EventHandler<Option<String>>) -> Element {
    let current = value
        .as_deref()
        .map(|c| category_label(Some(c)))
        .unwrap_or_else(|| "Category".to_string());
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{current}"
                }
            }
            PopoverContent { class: "p-1 min-w-56",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(None),
                    "All categories"
                }
                for s in INVENTORY_CATEGORIES.iter() {
                    button {
                        key: "{s}",
                        class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                        onclick: {
                            let s = s.to_string();
                            move |_| on_change.call(Some(s.clone()))
                        },
                        "{category_label(Some(s))}"
                    }
                }
            }
        }
    }
}

#[component]
fn ManufacturerPicker(
    value: Option<String>,
    options: Vec<String>,
    on_change: EventHandler<Option<String>>,
) -> Element {
    let current = value.clone().unwrap_or_else(|| "Manufacturer".to_string());
    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{current}"
                }
            }
            PopoverContent { class: "p-1 min-w-56 max-h-80 overflow-auto",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(None),
                    "All manufacturers"
                }
                for s in options.iter() {
                    button {
                        key: "{s}",
                        class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                        onclick: {
                            let s = s.clone();
                            move |_| on_change.call(Some(s.clone()))
                        },
                        "{s}"
                    }
                }
            }
        }
    }
}
