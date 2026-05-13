//! `WarrantyBadge` — pill that visualises remaining warranty.

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ShieldAlert, ShieldCheck, ShieldOff};

use super::common::{WarrantyHealth, warranty_class, warranty_health};

#[component]
pub fn WarrantyBadge(warranty_until: Option<DateTime<Utc>>) -> Element {
    let class = warranty_class(warranty_until);
    let (icon, label) = match warranty_health(warranty_until) {
        WarrantyHealth::Healthy => (
            rsx! { ShieldCheck { size: 12 } },
            label_for(warranty_until, "covered"),
        ),
        WarrantyHealth::Soon => (
            rsx! { ShieldAlert { size: 12 } },
            label_for(warranty_until, "soon"),
        ),
        WarrantyHealth::Expired => (rsx! { ShieldOff { size: 12 } }, "Expired".to_string()),
        WarrantyHealth::Unknown => (rsx! { ShieldOff { size: 12 } }, "No warranty".to_string()),
    };
    let cls = format!(
        "inline-flex items-center gap-1 rounded-full px-2 py-0.5 text-xs font-medium {class}"
    );
    rsx! {
        span { class: "{cls}",
            {icon}
            "{label}"
        }
    }
}

fn label_for(warranty: Option<DateTime<Utc>>, _phase: &str) -> String {
    match warranty {
        None => "—".to_string(),
        Some(when) => {
            let days = (when - Utc::now()).num_days();
            if days >= 0 {
                format!("{days}d left")
            } else {
                format!("{}d ago", -days)
            }
        }
    }
}
