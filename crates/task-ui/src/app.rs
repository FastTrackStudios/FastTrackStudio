use dioxus::prelude::*;

use crate::data::{organizations, Organization};
use crate::views::{AllProjects, Inbox, ProjectOverview};

/// Source of truth for the app's top-level routes. The router will pick
/// the matching component for each URL, mount it under `AppShell`, and
/// (when the `wasm-split` feature is enabled in the consuming app) load
/// each variant's code lazily so the initial wasm payload only contains
/// the route the user actually visits.
#[derive(Clone, Debug, PartialEq, Routable)]
#[rustfmt::skip]
pub enum Route {
    #[layout(AppShell)]
        #[route("/")]
        DashboardRoute {},

        #[route("/projects")]
        ProjectsRoute {},

        #[route("/inbox")]
        InboxRoute {},

        #[route("/settings")]
        SettingsRoute {},

        #[route("/timer-demo")]
        TimerDemoRoute {},
}

#[component]
fn DashboardRoute() -> Element {
    rsx! { ProjectOverview {} }
}

#[component]
fn ProjectsRoute() -> Element {
    rsx! { AllProjects {} }
}

#[component]
fn InboxRoute() -> Element {
    rsx! { Inbox {} }
}

#[component]
fn TimerDemoRoute() -> Element {
    rsx! { crate::timer_demo::TimerDemo {} }
}

#[component]
fn SettingsRoute() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-2 p-6 lg:p-10",
            h1 { class: "text-3xl font-bold", "Settings" }
            p { class: "text-slate-400", "Not implemented yet." }
        }
    }
}

/// Top-level component that the platform launchers mount.
#[component]
pub fn App() -> Element {
    rsx! { Router::<Route> {} }
}

// ── App shell (layout) ─────────────────────────────────────────────────

/// Wraps every route with the mobile-first chrome: top header on phones,
/// sidebar on desktop, bottom tab bar on phones. `Outlet::<Route>` is
/// where the route component renders.
#[component]
fn AppShell() -> Element {
    let orgs = organizations();
    let active_org = use_signal(|| orgs[0].clone());

    rsx! {
        div { class: "min-h-screen bg-slate-950 text-slate-100 lg:grid lg:grid-cols-[18rem_1fr]",
            DesktopSidebar { orgs: orgs.clone(), active_org }

            div { class: "flex min-h-screen flex-col",
                MobileHeader { active_org }

                main { class: "flex-1 pb-24 lg:pb-0",
                    // Suspense boundary required by dioxus-router when
                    // wasm-split is on — otherwise the entire page would
                    // suspend on first navigation.
                    SuspenseBoundary {
                        fallback: |_| rsx! { RouteFallback {} },
                        Outlet::<Route> {}
                    }
                }

                BottomTabBar {}
            }
        }
    }
}

#[component]
fn RouteFallback() -> Element {
    rsx! {
        div { class: "flex h-64 items-center justify-center text-sm text-slate-500",
            "Loading…"
        }
    }
}

// ── Mobile header ───────────────────────────────────────────────────────

#[component]
fn MobileHeader(active_org: Signal<Organization>) -> Element {
    let route = use_route::<Route>();
    let title = route_title(&route);
    rsx! {
        header {
            class: "sticky top-0 z-20 flex items-center gap-3 border-b border-slate-800 bg-slate-950/95 px-4 py-3 backdrop-blur lg:hidden",
            div { class: "flex h-9 w-9 items-center justify-center rounded-xl bg-cyan-400 text-base font-black text-slate-950", "T" }
            div { class: "flex flex-col leading-tight",
                span { class: "text-xs uppercase tracking-[0.2em] text-slate-500", "Task" }
                span { class: "text-base font-semibold text-white", "{title}" }
            }
            div { class: "ml-auto", OrgSwitcher { active_org, compact: true } }
        }
    }
}

// ── Bottom tab bar (mobile) ────────────────────────────────────────────

#[component]
fn BottomTabBar() -> Element {
    let current = use_route::<Route>();
    rsx! {
        nav {
            class: "fixed inset-x-0 bottom-0 z-30 border-t border-slate-800 bg-slate-950/95 backdrop-blur lg:hidden",
            style: "padding-bottom: env(safe-area-inset-bottom, 0px);",
            ul { class: "mx-auto grid max-w-md grid-cols-4",
                for tab in NAV_TABS.iter() {
                    li { key: "{tab.label}",
                        TabBarItem { tab: tab.clone(), active: tabs_match(&current, &tab) }
                    }
                }
            }
        }
    }
}

#[component]
fn TabBarItem(tab: NavTab, active: bool) -> Element {
    let class = if active {
        "flex min-h-[56px] w-full flex-col items-center justify-center gap-0.5 py-2 text-cyan-300"
    } else {
        "flex min-h-[56px] w-full flex-col items-center justify-center gap-0.5 py-2 text-slate-400 active:text-cyan-200"
    };
    rsx! {
        Link { to: tab.route.clone(), class,
            span { class: "text-xl leading-none", "{tab.icon}" }
            span { class: "text-[10px] font-semibold uppercase tracking-widest", "{tab.label}" }
        }
    }
}

// ── Desktop sidebar ────────────────────────────────────────────────────

#[component]
fn DesktopSidebar(orgs: Vec<Organization>, active_org: Signal<Organization>) -> Element {
    let current = use_route::<Route>();
    rsx! {
        aside { class: "hidden flex-col gap-6 border-r border-slate-800 bg-slate-900/90 p-4 lg:flex lg:min-h-screen",
            div { class: "flex items-center gap-3",
                div { class: "flex h-10 w-10 items-center justify-center rounded-xl bg-cyan-400 font-black text-slate-950", "T" }
                div {
                    h1 { class: "text-lg font-semibold", "Task" }
                    p { class: "text-xs text-slate-400", "Local-first command center" }
                    p { class: "mt-1 text-[10px] text-cyan-400 font-mono tracking-widest uppercase", "live · task-dev" }
                }
            }

            nav { class: "flex flex-col gap-1",
                for tab in NAV_TABS.iter() {
                    SidebarItem { key: "{tab.label}", tab: tab.clone(), active: tabs_match(&current, &tab) }
                }
            }

            div { class: "mt-auto pt-6",
                p { class: "px-2 pb-2 text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "Organization" }
                OrgSwitcher { orgs, active_org, compact: false }
            }
        }
    }
}

#[component]
fn SidebarItem(tab: NavTab, active: bool) -> Element {
    let class = if active {
        "flex w-full items-center gap-3 rounded-xl bg-cyan-400 px-3 py-2.5 text-left font-semibold text-slate-950 shadow-lg shadow-cyan-950/30"
    } else {
        "flex w-full items-center gap-3 rounded-xl px-3 py-2.5 text-left text-slate-300 hover:bg-slate-800 hover:text-white"
    };
    rsx! {
        Link { to: tab.route.clone(), class,
            span { class: "w-5 text-center text-lg", "{tab.icon}" }
            span { "{tab.label}" }
        }
    }
}

// ── Nav tab table ──────────────────────────────────────────────────────

#[derive(Clone, PartialEq)]
struct NavTab {
    label: &'static str,
    icon: &'static str,
    route: Route,
}

fn nav_tabs() -> Vec<NavTab> {
    vec![
        NavTab { label: "Home", icon: "▦", route: Route::DashboardRoute {} },
        NavTab { label: "Projects", icon: "▤", route: Route::ProjectsRoute {} },
        NavTab { label: "Inbox", icon: "✉", route: Route::InboxRoute {} },
        NavTab { label: "Timer demo", icon: "⏱", route: Route::TimerDemoRoute {} },
        NavTab { label: "Settings", icon: "⚙", route: Route::SettingsRoute {} },
    ]
}

thread_local! {
    static NAV_TABS_CACHE: Vec<NavTab> = nav_tabs();
}

// Helper macro to iterate at the call site without thread_local plumbing.
#[allow(non_upper_case_globals)]
const NAV_TABS: NavTabsConst = NavTabsConst;
struct NavTabsConst;
impl NavTabsConst {
    fn iter(&self) -> std::vec::IntoIter<NavTab> {
        nav_tabs().into_iter()
    }
}

fn tabs_match(current: &Route, tab: &NavTab) -> bool {
    std::mem::discriminant(current) == std::mem::discriminant(&tab.route)
}

fn route_title(route: &Route) -> &'static str {
    match route {
        Route::DashboardRoute {} => "Home",
        Route::ProjectsRoute {} => "Projects",
        Route::InboxRoute {} => "Inbox",
        Route::SettingsRoute {} => "Settings",
        Route::TimerDemoRoute {} => "Timer demo",
    }
}

// ── Org switcher ───────────────────────────────────────────────────────

#[component]
fn OrgSwitcher(
    #[props(default = Vec::new())] orgs: Vec<Organization>,
    active_org: Signal<Organization>,
    #[props(default = false)] compact: bool,
) -> Element {
    let mut open = use_signal(|| false);
    let orgs = if orgs.is_empty() { organizations() } else { orgs };
    let active = active_org();

    let trigger_class = if compact {
        "flex items-center gap-2 rounded-full border border-slate-700 bg-slate-900 px-2.5 py-1.5 text-xs font-semibold text-slate-100 active:bg-slate-800"
    } else {
        "flex w-full items-center gap-3 rounded-xl border border-slate-700 bg-slate-950/60 px-3 py-2.5 text-left text-sm font-semibold text-slate-100 hover:bg-slate-900"
    };

    rsx! {
        div { class: "relative",
            button {
                class: trigger_class,
                onclick: move |_| {
                    let next = !open();
                    open.set(next);
                },
                span { class: "flex h-7 w-7 items-center justify-center rounded-lg bg-cyan-400 text-xs font-black text-slate-950", "{active.monogram}" }
                if !compact {
                    div { class: "flex min-w-0 flex-1 flex-col leading-tight",
                        span { class: "truncate", "{active.name}" }
                        span { class: "text-xs font-normal text-slate-400", "{active.role}" }
                    }
                } else {
                    span { class: "max-w-[6.5rem] truncate", "{active.name}" }
                }
                span { class: "text-slate-500", "▾" }
            }

            if open() {
                div {
                    class: if compact {
                        "fixed inset-x-3 top-16 z-40 rounded-2xl border border-slate-700 bg-slate-900 p-2 shadow-2xl shadow-black/60"
                    } else {
                        "absolute bottom-full left-0 right-0 z-40 mb-2 rounded-2xl border border-slate-700 bg-slate-900 p-2 shadow-2xl shadow-black/60"
                    },
                    p { class: "px-2 pb-2 text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "Switch organization" }
                    ul { class: "flex flex-col gap-1",
                        for org in orgs.iter() {
                            li { key: "{org.id}",
                                OrgOption {
                                    org: org.clone(),
                                    active: org.id == active.id,
                                    on_pick: {
                                        let picked = org.clone();
                                        move |_| {
                                            active_org.set(picked.clone());
                                            open.set(false);
                                        }
                                    },
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn OrgOption(org: Organization, active: bool, on_pick: EventHandler<MouseEvent>) -> Element {
    let row_class = if active {
        "flex w-full items-center gap-3 rounded-xl bg-cyan-400/15 px-3 py-2.5 text-left"
    } else {
        "flex w-full items-center gap-3 rounded-xl px-3 py-2.5 text-left hover:bg-slate-800 active:bg-slate-800"
    };
    rsx! {
        button { class: row_class, onclick: move |evt| on_pick.call(evt),
            span { class: "flex h-9 w-9 shrink-0 items-center justify-center rounded-lg bg-cyan-400 text-sm font-black text-slate-950", "{org.monogram}" }
            div { class: "flex min-w-0 flex-1 flex-col leading-tight",
                span { class: "truncate text-sm font-semibold text-white", "{org.name}" }
                span { class: "truncate text-xs text-slate-400", "{org.role}" }
            }
            if active {
                span { class: "text-cyan-300", "●" }
            }
        }
    }
}
