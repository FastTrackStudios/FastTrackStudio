//! Shared helpers for inventory-ui components: badge variants, icons,
//! warranty colouring, value formatting.

use chrono::{DateTime, Duration, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Boxes, Cable, Car, Cpu, FileKey, Mic, Music, Package, SlidersHorizontal,
};
use fts_ui::prelude::*;

/// Snipe-IT-style mapping of inventory status strings → fts-ui badge variant.
pub fn status_to_badge_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" => StatusBadgeVariant::Success,
        "checked-out" => StatusBadgeVariant::Warning,
        "in-repair" => StatusBadgeVariant::Warning,
        "retired" => StatusBadgeVariant::Neutral,
        "sold" => StatusBadgeVariant::Neutral,
        "lost" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}

/// Category → lucide icon. Pick by best fit; falls back to a generic box.
pub fn category_icon(category: Option<&str>, size: u32) -> Element {
    let size = size as usize;
    match category.unwrap_or("") {
        "audio-interface" => rsx! { SlidersHorizontal { size: size } },
        "microphone" => rsx! { Mic { size: size } },
        "outboard" => rsx! { SlidersHorizontal { size: size } },
        "computer" => rsx! { Cpu { size: size } },
        "instrument" => rsx! { Music { size: size } },
        "cable" => rsx! { Cable { size: size } },
        "software-license" => rsx! { FileKey { size: size } },
        "vehicle" => rsx! { Car { size: size } },
        "other" => rsx! { Package { size: size } },
        _ => rsx! { Boxes { size: size } },
    }
}

/// "How urgent is this warranty?" — used for badge tints + sorting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarrantyHealth {
    /// More than 90 days remaining.
    Healthy,
    /// 0–90 days remaining.
    Soon,
    /// Already expired.
    Expired,
    /// No warranty info.
    Unknown,
}

pub fn warranty_health(warranty: Option<DateTime<Utc>>) -> WarrantyHealth {
    match warranty {
        None => WarrantyHealth::Unknown,
        Some(when) => {
            let now = Utc::now();
            if when < now {
                WarrantyHealth::Expired
            } else if when - now < Duration::days(90) {
                WarrantyHealth::Soon
            } else {
                WarrantyHealth::Healthy
            }
        }
    }
}

/// Tailwind class for a warranty pill background — green/amber/red/grey.
pub fn warranty_class(warranty: Option<DateTime<Utc>>) -> &'static str {
    match warranty_health(warranty) {
        WarrantyHealth::Healthy => "bg-emerald-500/10 text-emerald-500",
        WarrantyHealth::Soon => "bg-amber-500/10 text-amber-500",
        WarrantyHealth::Expired => "bg-red-500/10 text-red-500",
        WarrantyHealth::Unknown => "bg-muted text-muted-foreground",
    }
}

/// "$1,234.56" from i64 cents — None → "—".
pub fn format_value_cents(cents: Option<i64>) -> String {
    match cents {
        None => "—".to_string(),
        Some(c) => {
            let abs = c.unsigned_abs();
            let dollars = abs / 100;
            let pennies = abs % 100;
            let sign = if c < 0 { "-" } else { "" };
            // Tiny grouping helper, no fancy locales.
            let s = dollars.to_string();
            let bytes = s.as_bytes();
            let mut out = String::new();
            for (i, b) in bytes.iter().enumerate() {
                if i > 0 && (bytes.len() - i) % 3 == 0 {
                    out.push(',');
                }
                out.push(*b as char);
            }
            format!("{sign}${out}.{pennies:02}")
        }
    }
}

/// Human label for a category slug — title-cased and re-spaced.
pub fn category_label(category: Option<&str>) -> String {
    match category {
        None => "Uncategorized".to_string(),
        Some(c) => c
            .split('-')
            .map(|w| {
                let mut chars = w.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().chain(chars).collect(),
                }
            })
            .collect::<Vec<_>>()
            .join(" "),
    }
}
