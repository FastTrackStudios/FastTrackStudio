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
    // Ctrl+P command-palette visibility (same pattern as FleetingOpen).
    crate::palette::provide_palette_context();
    // Obsidian-style route tabs (the strip lives in the TopBar) —
    // restores the persisted strip and seeds it with the boot route.
    crate::tabs::provide_tabs(current.clone());
    // Shell panel state: the vault explorer + the right (backlinks)
    // panel, toggled from the top bar (Obsidian-style).
    let explorer = use_context_provider(|| Signal::new(crate::chrome::ExplorerOpen(true)));
    let _ = use_context_provider(|| Signal::new(crate::chrome::RightPanelOpen(true)));
    // Zen mode (Ctrl+Shift+Z): render NO desktop chrome — the open
    // view gets the whole viewport. Only the rendering is gated; the
    // explorer/right-panel signals keep their values, so exiting zen
    // restores exactly what was showing. Desktop-only: everything zen
    // hides is `md:`-gated already, so mobile is unaffected.
    let zen = use_context::<crate::chrome::ZenMode>().0;

    rsx! {
        // Publishes this client's presence entry (route activity, idle,
        // manual status) on the org channel joined at the app root.
        // Renders nothing; lives here because it needs `use_route`.
        crate::presence::PresencePublisher {}
        // One-shot start-page redirect from the user's prefs entity.
        StartPageRedirect {}
        // Mobile is the primary platform: below `md` the chrome is the
        // top app bar + bottom tab bar. At `md`+ the desktop shell is
        // Obsidian-shaped (plans/vault-views.md): one full-width top
        // bar over everything (sidebar toggles, timer, capture — and
        // where tabs will live), then icon rail → vault explorer →
        // the open view.
        div { class: "min-h-screen bg-background text-foreground md:flex md:h-screen md:flex-col md:overflow-hidden",
            if !zen() {
                TopBar {}
            }
            div { class: "md:flex md:min-h-0 md:flex-1",
            if !zen() {
                div { class: "hidden md:block",
                    crate::shell::rail::IconRail { current: current.clone() }
                }
            }
            // Everything right of the icon rail: the [vault explorer |
            // open view] row, with the IDE status line spanning BENEATH
            // it. The rail runs full-height beside the status bar, but the
            // vault explorer stops just above it (VS Code-style).
            div { class: "flex flex-col md:min-h-0 md:flex-1 md:overflow-hidden",
                div { class: "flex flex-col md:min-h-0 md:flex-1 md:flex-row",
                    if explorer.read().0 && !zen() {
                        div { class: "hidden w-[17rem] shrink-0 border-r border-border/60 md:flex md:min-h-0 md:flex-col md:overflow-hidden",
                            crate::shell::explorer::VaultExplorer {}
                        }
                    }
                    div { class: "flex min-h-screen flex-col md:min-h-0 md:flex-1 md:overflow-hidden",
                        MobileHeader {}
                        // Bottom padding keeps content clear of the fixed
                        // tab bar (56px + safe area). On desktop `main` is
                        // the scroll container.
                        main { class: "flex-1 pb-[calc(6rem+env(safe-area-inset-bottom,0px))] md:min-h-0 md:overflow-y-auto md:pb-0",
                            SuspenseBoundary {
                                fallback: |_| rsx! { RouteFallback {} },
                                Outlet::<Route> {}
                            }
                        }
                        BottomTabBar { current }
                        FleetingFab {}
                    }
                }
                // IDE status line — spans the explorer + view (right of the
                // full-height rail), pinned to the base.
                if !zen() {
                    crate::chrome::StatusBar {}
                }
            }
            }
        }
        // Zen's only chrome: the hover-revealed exit button in the
        // top-left corner.
        if zen() {
            crate::chrome::ZenExitOverlay {}
        }
        // Single global capture modal, toggled from any fleeting button.
        FleetingModal {}
        // Ctrl+P command palette — pages + vault notes, fuzzy-ranked.
        // Mounts its own document-level hotkey listener.
        crate::palette::CommandPalette {}
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
