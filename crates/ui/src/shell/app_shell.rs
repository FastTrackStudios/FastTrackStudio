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
        // App-wide notice queue (architect::Notifications, provided by
        // `use_app_reactive` at the app root). Mutations + the vault
        // DocumentSession report failures here so they outlive the
        // screen that caused them.
        NotificationTray {}
    }
}

/// Fixed bottom-right toast stack over the notification queue.
/// Errors render destructive; info renders muted. Dismiss is
/// per-notice.
#[component]
fn NotificationTray() -> Element {
    let notices = architect::use_notifications();
    let list = notices.list();
    if list.is_empty() {
        return rsx! {};
    }
    rsx! {
        div { class: "pointer-events-none fixed bottom-20 right-4 z-50 flex w-80 flex-col gap-2 lg:bottom-4",
            for n in list {
                div {
                    key: "{n.id}",
                    class: if n.level == architect::NoticeLevel::Error {
                        "pointer-events-auto flex items-start justify-between gap-2 rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive shadow-md backdrop-blur"
                    } else {
                        "pointer-events-auto flex items-start justify-between gap-2 rounded-md border border-border bg-background/95 px-3 py-2 text-sm shadow-md backdrop-blur"
                    },
                    span { class: "min-w-0 break-words", "{n.message}" }
                    button {
                        class: "shrink-0 text-muted-foreground hover:text-foreground",
                        aria_label: "Dismiss",
                        onclick: move |_| notices.dismiss(n.id),
                        "×"
                    }
                }
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
