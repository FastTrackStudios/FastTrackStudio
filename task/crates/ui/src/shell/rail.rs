//! The icon rail — Obsidian's ribbon for this app. A slim icon-only
//! strip of shortcuts on the far left; the vault explorer beside it
//! is the main sidebar. Labels become tooltips; active state is a
//! subtle filled pill.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Feather;

use crate::nav::{nav_tabs, tabs_match};
use crate::routes::Route;

#[component]
pub fn IconRail(current: Route) -> Element {
    let nav = use_navigator();
    rsx! {
        div { class: "flex h-screen w-12 flex-col items-center gap-0.5 border-r border-border/60 bg-card/40 py-2",
            // Brand chip — also the way home.
            button {
                r#type: "button",
                class: "mb-1 flex h-8 w-8 items-center justify-center rounded-lg bg-primary text-sm font-black text-primary-foreground",
                title: "Task",
                onclick: move |_| {
                    nav.push(Route::HomeRoute {});
                },
                "T"
            }
            div { class: "flex min-h-0 flex-1 flex-col items-center gap-0.5 overflow-y-auto",
                for tab in nav_tabs() {
                    {
                        let is_active = tabs_match(&current, &tab);
                        let route = tab.route.clone();
                        let icon = tab.icon;
                        let cls = if is_active {
                            "flex h-8 w-8 shrink-0 items-center justify-center rounded-lg bg-accent text-foreground"
                        } else {
                            "flex h-8 w-8 shrink-0 items-center justify-center rounded-lg text-muted-foreground hover:bg-accent/50 hover:text-foreground"
                        };
                        rsx! {
                            button {
                                key: "{tab.label}",
                                r#type: "button",
                                class: "{cls}",
                                title: "{tab.label}",
                                onclick: move |_| {
                                    nav.push(route.clone());
                                },
                                {icon()}
                            }
                        }
                    }
                }
            }
            // Quick capture at the rail's foot — icon-only, rail-sized.
            {
                let mut open = crate::chrome::use_fleeting_open();
                rsx! {
                    button {
                        r#type: "button",
                        class: "flex h-8 w-8 shrink-0 items-center justify-center rounded-lg bg-primary/15 text-primary transition-colors hover:bg-primary/25",
                        title: "Capture a fleeting note",
                        onclick: move |_| open.set(true),
                        Feather { size: 15 }
                    }
                }
            }
        }
    }
}
