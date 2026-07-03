//! Two-column desktop layout / single-column mobile layout.
//! Wraps the active route via `Outlet`.

use dioxus::prelude::*;

use crate::chrome::{FleetingFab, FleetingModal, TopBar, provide_chrome_contexts};
use crate::routes::Route;
use crate::shell::mobile::{BottomTabBar, MobileHeader};

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
        // One-shot start-page redirect from the user's prefs entity.
        StartPageRedirect {}
        // Mobile is the primary platform: below `md` the chrome is the
        // top app bar + bottom tab bar. At `md`+ the desktop shell is
        // Obsidian-shaped (plans/vault-views.md): icon rail (shortcut
        // ribbon) → vault explorer (the main sidebar) → the open view.
        div { class: "min-h-screen bg-background text-foreground md:grid md:h-screen md:grid-cols-[3rem_17rem_1fr] md:overflow-hidden",
            div { class: "hidden md:block",
                crate::shell::rail::IconRail { current: current.clone() }
            }
            div { class: "hidden border-r border-border/60 md:flex md:h-screen md:flex-col md:overflow-hidden",
                crate::shell::explorer::VaultExplorer {}
            }

            div { class: "flex min-h-screen flex-col md:h-screen md:min-h-0 md:overflow-y-auto",
                MobileHeader {}
                TopBar {}
                // Bottom padding keeps content clear of the fixed tab
                // bar (56px + safe area) plus breathing room.
                main { class: "flex-1 pb-[calc(6rem+env(safe-area-inset-bottom,0px))] md:pb-0",
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
        div { class: "pointer-events-none fixed bottom-20 right-4 z-50 flex w-80 max-w-[calc(100vw-2rem)] flex-col gap-2 md:bottom-4",
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

/// Redirect `/` to the user's preferred start page, once per session,
/// when their prefs load (renders nothing). Deep links and manual
/// navigation are never hijacked: the redirect only fires while the
/// current route is still the root and no redirect has happened yet.
#[component]
fn StartPageRedirect() -> Element {
    let prefs = use_context::<crate::prefs::PrefsCtx>().prefs;
    let nav = use_navigator();
    let route = use_route::<Route>();
    let mut done = use_signal(|| false);

    use_effect(move || {
        let target = prefs.read().default_page.clone();
        if *done.peek() || target.is_empty() {
            return;
        }
        // `use_route` in an effect: read once via the captured value —
        // only fire from the root route.
        if matches!(route, Route::HomeRoute {}) {
            done.set(true);
            nav.replace(target.as_str());
        }
    });
    rsx! {}
}
