//! Two-column desktop layout / single-column mobile layout.
//! Wraps the active route via `Outlet`.

use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::chrome::{FleetingFab, FleetingModal, TopBar, provide_chrome_contexts};
use crate::routes::Route;
use crate::shell::mobile::{BottomTabBar, MobileHeader};
use crate::shell::sidebar::DesktopSidebar;

#[component]
pub fn AppShell() -> Element {
    let current = use_route::<Route>();

    // Quick-capture + data-refresh signals for the persistent chrome.
    provide_chrome_contexts();

    rsx! {
        // Publishes this client's presence entry (route activity, idle,
        // manual status) on the org channel joined at the app root.
        // Renders nothing; lives here because it needs `use_route`.
        crate::presence::PresencePublisher {}
        div { class: "min-h-screen bg-background text-foreground lg:grid lg:h-screen lg:grid-cols-[18rem_1fr] lg:overflow-hidden",
            div { class: "hidden lg:flex lg:h-screen lg:flex-col lg:overflow-hidden",
                SidebarProvider {
                    DesktopSidebar { current: current.clone() }
                }
            }

            div { class: "flex min-h-screen flex-col lg:h-screen lg:min-h-0 lg:overflow-y-auto",
                MobileHeader {}
                TopBar {}
                main { class: "flex-1 pb-24 lg:pb-0",
                    SuspenseBoundary {
                        fallback: |_| rsx! { RouteFallback {} },
                        Outlet::<Route> {}
                    }
                }
                BottomTabBar { current }
                FleetingFab {}
            }
        }
        // Single global capture modal, toggled from any fleeting button.
        FleetingModal {}
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
