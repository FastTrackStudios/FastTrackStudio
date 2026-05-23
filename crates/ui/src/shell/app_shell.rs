//! Two-column desktop layout / single-column mobile layout.
//! Wraps the active route via `Outlet`.

use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::data::organizations;
use crate::routes::Route;
use crate::shell::mobile::{BottomTabBar, MobileHeader};
use crate::shell::sidebar::DesktopSidebar;

#[component]
pub fn AppShell() -> Element {
    let orgs = organizations();
    let current = use_route::<Route>();

    rsx! {
        div { class: "min-h-screen bg-background text-foreground lg:grid lg:h-screen lg:grid-cols-[18rem_1fr] lg:overflow-hidden",
            div { class: "hidden lg:flex lg:h-screen lg:flex-col lg:overflow-hidden",
                SidebarProvider {
                    DesktopSidebar { orgs: orgs.clone(), current: current.clone() }
                }
            }

            div { class: "flex min-h-screen flex-col lg:h-screen lg:min-h-0 lg:overflow-y-auto",
                MobileHeader {}
                main { class: "flex-1 pb-24 lg:pb-0",
                    SuspenseBoundary {
                        fallback: |_| rsx! { RouteFallback {} },
                        Outlet::<Route> {}
                    }
                }
                BottomTabBar { current }
            }
        }
    }
}

#[component]
fn RouteFallback() -> Element {
    rsx! {
        div { class: "flex h-64 items-center justify-center text-sm text-muted-foreground",
            "Loading…"
        }
    }
}
