//! Desktop sidebar — ported from the federation-era `task-ui`
//! shell (commit 78c5622). Workspace nav up top, org switcher
//! at the bottom.

use dioxus::prelude::*;
use dioxus_router::Navigator;
use fts_ui::prelude::*;

use crate::nav::{NavTab, nav_tabs, tabs_match};
use crate::routes::Route;
use crate::shell::org_switcher::OrgSwitcher;

#[component]
pub fn DesktopSidebar(current: Route) -> Element {
    let nav = use_navigator();
    rsx! {
        Sidebar { class: "flex h-screen w-72 flex-col overflow-hidden",
            SidebarHeader {
                HStack { gap: "3", class: "px-2 py-1",
                    div {
                        class: "flex h-10 w-10 items-center justify-center rounded-xl bg-primary font-black text-primary-foreground",
                        "T"
                    }
                    div { class: "flex flex-col",
                        span { class: "text-base font-semibold text-foreground leading-tight", "Task" }
                        span { class: "text-xs text-muted-foreground", "Local-first command center" }
                    }
                }
            }
            SidebarSeparator {}
            SidebarGroup {
                SidebarGroupLabel { "Workspace" }
                SidebarGroupContent {
                    SidebarMenu {
                        for tab in nav_tabs() {
                            {render_sidebar_item(tab, &current, nav)}
                        }
                    }
                }
            }
            SidebarSeparator {}
            div { class: "flex-1 min-h-0 overflow-y-auto",
                // Org-wide presence roster: live peers + agents
                // mid-turn + open timers, humans first.
                crate::presence::PresenceRoster {}
            }
            SidebarFooter {
                div { class: "px-1 pb-2",
                    crate::chrome::FleetingButton {}
                }
                // Presence identity: display name + DND/Available
                // picker, published org-wide by the shell's publisher.
                div { class: "px-1 pb-2",
                    crate::presence::PresenceStatusPicker {}
                }
                div { class: "px-1 pb-1 pt-1",
                    SectionHeader { label: "Organization", size: SectionHeaderSize::Small }
                }
                OrgSwitcher { compact: false }
            }
        }
    }
}

fn render_sidebar_item(tab: NavTab, current: &Route, nav: Navigator) -> Element {
    let is_active = tabs_match(current, &tab);
    let route = tab.route.clone();
    let icon = tab.icon;
    let label = tab.label;
    rsx! {
        SidebarMenuItem { key: "{label}",
            SidebarMenuButton {
                is_active,
                on_click: move |()| {
                    nav.push(route.clone());
                },
                span { class: "flex h-4 w-4 items-center justify-center", {icon()} }
                span { "{label}" }
            }
        }
    }
}
