//! Mobile chrome — the primary-platform shell below `md`.
//!
//! Three pieces, all hidden at `md:` and up (where the desktop
//! sidebar takes over):
//!
//! - [`MobileHeader`] — sticky top app bar: page title, the
//!   [`crate::presence::ConnectionBadge`], and an avatar button that
//!   opens the **account & status** bottom sheet (the desktop
//!   [`crate::auth::AccountSwitcher`]'s content via
//!   [`crate::auth::AccountSheetBody`] — same `AuthCtx` /
//!   `PresenceLocal` logic, touch-sized presentation).
//! - [`BottomTabBar`] — fixed bottom bar (safe-area padded): four
//!   primary destinations + a "More" tab opening the full nav as a
//!   bottom sheet, with the org switcher and the presence roster.
//! - [`BottomSheet`] — the shared mobile sheet primitive (fts-ui's
//!   `Sheet` only slides from the sides; mobile wants bottom sheets).
//!
//! Signal hygiene: every `use_signal` here is owned by a non-keyed,
//! shell-lifetime component (see `crate::collab` docs for the keyed-
//! child rule).

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Menu;

use crate::auth::{AccountSheetBody, AuthCtx, Avatar};
use crate::nav::{NavTab, nav_tabs, primary_mobile_tabs, route_title, tabs_match};
use crate::presence::{ConnectionBadge, PresenceLocal};
use crate::routes::Route;
use crate::shell::org_switcher::OrgSwitcher;

// ── bottom sheet primitive ──────────────────────────────────────────

/// Mobile bottom sheet: scrim + a rounded panel pinned to the bottom
/// edge, safe-area padded, scrollable past `85vh`. Closes on scrim
/// tap, Escape, or the explicit close button (≥44px hit area).
/// Children are only mounted while open.
#[component]
pub fn BottomSheet(
    open: bool,
    on_close: EventHandler<()>,
    title: String,
    children: Element,
) -> Element {
    if !open {
        return rsx! {};
    }
    rsx! {
        div {
            class: "fixed inset-0 z-50 bg-black/60 supports-[backdrop-filter]:backdrop-blur-xs md:hidden",
            onclick: move |_| on_close.call(()),
        }
        div {
            class: "fixed inset-x-0 bottom-0 z-50 flex max-h-[85vh] flex-col rounded-t-2xl border-t border-border bg-background text-foreground shadow-2xl outline-none md:hidden",
            style: "padding-bottom: env(safe-area-inset-bottom, 0px);",
            tabindex: "-1",
            // `autofocus` only fires on initial page load, not when the
            // sheet is inserted dynamically — focus on mount so Escape
            // lands on the panel (same trick as the fleeting modal).
            onmounted: move |e: Event<MountedData>| {
                spawn(async move {
                    let _ = e.data().set_focus(true).await;
                });
            },
            onclick: move |e| e.stop_propagation(),
            onkeydown: move |e| {
                if e.key() == Key::Escape {
                    on_close.call(());
                }
            },
            // Grab handle.
            div { class: "mx-auto mt-2 h-1.5 w-10 shrink-0 rounded-full bg-muted" }
            div { class: "flex shrink-0 items-center justify-between gap-2 px-4 pb-1 pt-2",
                h2 { class: "text-sm font-semibold uppercase tracking-widest text-muted-foreground",
                    "{title}"
                }
                button {
                    r#type: "button",
                    class: "flex h-11 w-11 items-center justify-center rounded-full text-muted-foreground active:bg-accent active:text-foreground",
                    aria_label: "Close",
                    onclick: move |_| on_close.call(()),
                    "✕"
                }
            }
            div { class: "min-h-0 flex-1 overflow-y-auto overflow-x-hidden px-4 pb-4",
                {children}
            }
        }
    }
}

// ── top app bar ─────────────────────────────────────────────────────

#[component]
pub fn MobileHeader() -> Element {
    let route = use_route::<Route>();
    let title = route_title(&route);
    let mut account_open = use_signal(|| false);

    // Avatar trigger state — same identity the desktop switcher shows.
    let ctx = use_context::<AuthCtx>();
    let local = use_context::<PresenceLocal>();
    let account = ctx.active.read().clone();
    let (name, email) = account.as_ref().map_or_else(
        || ("Signing in…".to_owned(), String::new()),
        |a| (a.name.clone(), a.email.clone()),
    );
    let effective = local.effective_status();
    let dot = effective.dot_class();

    rsx! {
        header {
            class: "sticky top-0 z-20 flex items-center gap-3 border-b border-border bg-background/95 px-4 py-2 backdrop-blur md:hidden",
            style: "padding-top: max(0.5rem, env(safe-area-inset-top, 0px));",
            div {
                class: "flex h-9 w-9 shrink-0 items-center justify-center rounded-xl bg-primary text-base font-black text-primary-foreground",
                "T"
            }
            div { class: "flex min-w-0 flex-col leading-tight",
                span { class: "text-xs uppercase tracking-[0.2em] text-muted-foreground", "Task" }
                span { class: "truncate text-base font-semibold text-foreground", "{title}" }
            }
            div { class: "ml-auto flex shrink-0 items-center gap-1",
                ConnectionBadge {}
                // Account & status — opens the bottom sheet.
                button {
                    r#type: "button",
                    class: "flex h-11 w-11 items-center justify-center rounded-full active:bg-accent",
                    aria_label: "Account & status",
                    onclick: move |_| account_open.set(true),
                    span { class: "relative",
                        Avatar { name: name.clone(), email: email.clone(), size: 32 }
                        span { class: "absolute -bottom-0.5 -right-0.5 h-2.5 w-2.5 rounded-full border-2 border-background {dot}",
                            title: "{effective.label()}",
                        }
                    }
                }
            }
        }
        BottomSheet {
            open: account_open(),
            title: "Account & status".to_string(),
            on_close: move |()| account_open.set(false),
            AccountSheetBody { on_done: move |()| account_open.set(false) }
        }
    }
}

// ── bottom tab bar + "More" sheet ───────────────────────────────────

#[component]
pub fn BottomTabBar(current: Route) -> Element {
    let mut more_open = use_signal(|| false);
    let primary = primary_mobile_tabs();
    rsx! {
        nav {
            class: "fixed inset-x-0 bottom-0 z-30 border-t border-border bg-background/95 backdrop-blur md:hidden",
            style: "padding-bottom: env(safe-area-inset-bottom, 0px);",
            ul { class: "mx-auto grid max-w-md grid-cols-5",
                for tab in primary.iter() {
                    li { key: "{tab.label}",
                        TabBarItem { tab: tab.clone(), active: tabs_match(&current, tab) }
                    }
                }
                li {
                    button {
                        r#type: "button",
                        class: "flex min-h-[56px] w-full flex-col items-center justify-center gap-1 py-2 text-muted-foreground active:text-foreground",
                        aria_label: "More sections",
                        onclick: move |_| more_open.set(true),
                        Menu { size: 20 }
                        span { class: "text-[10px] font-semibold uppercase tracking-widest", "More" }
                    }
                }
            }
        }
        BottomSheet {
            open: more_open(),
            title: "All sections".to_string(),
            on_close: move |()| more_open.set(false),
            MoreSheetBody { on_navigate: move |()| more_open.set(false) }
        }
    }
}

/// The "More" sheet content: the full nav (everything the desktop
/// sidebar lists), the org switcher, and the presence roster. Only
/// mounted while the sheet is open, so the roster's polling future
/// runs only then.
#[component]
fn MoreSheetBody(on_navigate: EventHandler<()>) -> Element {
    rsx! {
        div { class: "flex flex-col gap-4 pb-2",
            ul { class: "grid grid-cols-2 gap-x-2",
                for tab in nav_tabs() {
                    li { key: "{tab.label}",
                        Link {
                            to: tab.route.clone(),
                            class: "flex min-h-[44px] items-center gap-3 rounded-lg px-3 py-2.5 text-sm text-foreground active:bg-accent",
                            onclick: move |_| on_navigate.call(()),
                            span { class: "flex h-5 w-5 shrink-0 items-center justify-center", {(tab.icon)()} }
                            span { class: "truncate", "{tab.label}" }
                        }
                    }
                }
            }
            section {
                h3 { class: "px-1 pb-1 text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                    "Organization"
                }
                OrgSwitcher { compact: false }
            }
            // Who's online — the same roster the desktop sidebar shows.
            crate::presence::PresenceRoster {}
        }
    }
}

#[component]
fn TabBarItem(tab: NavTab, active: bool) -> Element {
    let class = if active {
        "flex min-h-[56px] w-full flex-col items-center justify-center gap-1 py-2 text-primary"
    } else {
        "flex min-h-[56px] w-full flex-col items-center justify-center gap-1 py-2 text-muted-foreground active:text-foreground"
    };
    rsx! {
        Link { to: tab.route.clone(), class,
            span { class: "flex h-5 w-5 items-center justify-center", {(tab.icon)()} }
            span { class: "text-[10px] font-semibold uppercase tracking-widest", "{tab.label}" }
        }
    }
}
