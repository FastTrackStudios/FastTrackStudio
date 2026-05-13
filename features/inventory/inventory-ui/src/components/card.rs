//! `InventoryCard` — grid-mode card. Icon swatch keyed by category,
//! title, subtitle, footer with status + location + warranty.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use inventory_proto::InventoryItem;
use location_proto::Location;
use uuid::Uuid;

use super::common::{category_icon, status_to_badge_variant};
use super::table::category_swatch_class;
use super::warranty::WarrantyBadge;

#[component]
pub fn InventoryCard(
    item: InventoryItem,
    locations_by_id: HashMap<Uuid, Location>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let id = item.id;
    let swatch = category_swatch_class(item.category.as_deref());
    let location_name = item
        .location_id
        .and_then(|i| locations_by_id.get(&i).map(|l| l.name.clone()))
        .unwrap_or_else(|| "Unfiled".to_string());
    let subtitle = match (&item.manufacturer, &item.model) {
        (Some(m), Some(mo)) => format!("{m} · {mo}"),
        (Some(m), None) => m.clone(),
        (None, Some(mo)) => mo.clone(),
        _ => String::new(),
    };

    rsx! {
        Card { class: "cursor-pointer hover:border-foreground/30 transition-colors",
            div {
                onclick: move |_| on_open.call(id),
                CardHeader { class: "flex flex-row items-start gap-3",
                    div { class: "rounded-md p-2 {swatch}",
                        { category_icon(item.category.as_deref(), 22) }
                    }
                    div { class: "flex flex-col gap-0.5",
                        CardTitle { "{item.name}" }
                        if !subtitle.is_empty() {
                            CardDescription { "{subtitle}" }
                        }
                    }
                }
                CardFooter { class: "flex items-center justify-between gap-2",
                    HStack { class: "gap-2 items-center",
                        StatusBadge {
                            variant: status_to_badge_variant(&item.status),
                            label: item.status.clone(),
                        }
                        Text { variant: TextVariant::Muted, "{location_name}" }
                    }
                    WarrantyBadge { warranty_until: item.warranty_until }
                }
            }
        }
    }
}
