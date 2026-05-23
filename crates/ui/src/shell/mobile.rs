//! Mobile chrome — sticky top header + bottom tab bar with
//! "More" sheet. Ported from the federation-era shell.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Menu;
use fts_ui::prelude::*;

use crate::nav::{NavTab, nav_tabs, primary_mobile_tabs, route_title, tabs_match};
use crate::routes::Route;
use crate::shell::org_switcher::OrgSwitcher;

#[component]
pub fn MobileHeader() -> Element {
    let route = use_route::<Route>();
    let title = route_title(&route);
    rsx! {
        header {
            class: "sticky top-0 z-20 flex items-center gap-3 border-b border-border bg-background/95 px-4 py-3 backdrop-blur lg:hidden",
            div {
                class: "flex h-9 w-9 items-center justify-center rounded-xl bg-primary text-base font-black text-primary-foreground",
                "T"
            }
            div { class: "flex flex-col leading-tight",
                span { class: "text-xs uppercase tracking-[0.2em] text-muted-foreground", "Task" }
                span { class: "text-base font-semibold text-foreground", "{title}" }
            }
            div { class: "ml-auto", OrgSwitcher { compact: true } }
        }
    }
}

#[component]
pub fn BottomTabBar(current: Route) -> Element {
    let mut more_open = use_signal(|| false);
    let primary = primary_mobile_tabs();
    rsx! {
        nav {
            class: "fixed inset-x-0 bottom-0 z-30 border-t border-border bg-background/95 backdrop-blur lg:hidden",
            style: "padding-bottom: env(safe-area-inset-bottom, 0px);",
            ul { class: "mx-auto grid max-w-md grid-cols-5",
                for tab in primary.iter() {
                    li { key: "{tab.label}",
                        TabBarItem { tab: tab.clone(), active: tabs_match(&current, tab) }
                    }
                }
                li {
                    button {
                        class: "flex min-h-[56px] w-full flex-col items-center justify-center gap-1 py-2 text-muted-foreground active:text-foreground",
                        onclick: move |_| more_open.set(true),
                        Menu { size: 20 }
                        span { class: "text-[10px] font-semibold uppercase tracking-widest", "More" }
                    }
                }
            }
        }
        Sheet {
            open: more_open(),
            side: SheetSide::Right,
            on_close: move |()| more_open.set(false),
            SheetHeader {
                SheetTitle { "All sections" }
                SheetDescription { "Jump to any view." }
            }
            div { class: "mt-4 flex-1 overflow-y-auto",
                ul { class: "flex flex-col gap-1",
                    for tab in nav_tabs().into_iter() {
                        li { key: "{tab.label}",
                            Link {
                                to: tab.route.clone(),
                                class: "flex items-center gap-3 rounded-lg px-3 py-2.5 text-sm text-foreground hover:bg-accent hover:text-accent-foreground",
                                onclick: move |_| more_open.set(false),
                                span { class: "flex h-5 w-5 items-center justify-center", {(tab.icon)()} }
                                span { "{tab.label}" }
                            }
                        }
                    }
                }
            }
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
