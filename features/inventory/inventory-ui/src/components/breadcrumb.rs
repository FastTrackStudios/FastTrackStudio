//! `LocationBreadcrumb` — render a chain of locations as breadcrumbs.
//!
//! v1 takes a flat `Vec<Location>` (root → leaf). Real wiring will use
//! a `LocationRepoLoro` lookup with parent-edge traversal — left as a
//! `FUTURE` once `LocationRepoLoro` queries land in `task-ui`.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use location_proto::Location;

#[component]
pub fn LocationBreadcrumb(chain: Vec<Location>) -> Element {
    if chain.is_empty() {
        return rsx! {
            Text { variant: TextVariant::Muted, "Unfiled" }
        };
    }
    rsx! {
        Breadcrumb {
            BreadcrumbList {
                for (i, loc) in chain.iter().enumerate() {
                    if i > 0 { BreadcrumbSeparator {} }
                    BreadcrumbItem {
                        BreadcrumbPage { "{loc.name}" }
                    }
                }
            }
        }
    }
}
