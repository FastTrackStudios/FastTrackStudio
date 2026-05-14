use std::collections::HashMap;

use dioxus::prelude::*;
use dioxus_router::Navigator;
use fts_ui::lucide_dioxus::{
    BookOpen, FolderKanban, House, Inbox as InboxIcon, Menu, Palette, Settings as SettingsIcon,
};
use fts_ui::prelude::*;
use fts_ui::primitives::{ContentAlign, ContentSide};
use uuid::Uuid;

use crate::data::{Organization, organizations};
use crate::theming::{OrgThemeOverrides, ProjectThemeOverrides, state_from_preset_name};

/// Source of truth for the app's top-level routes.
#[derive(Clone, Debug, PartialEq, Routable)]
#[rustfmt::skip]
pub enum Route {
    #[layout(AppShell)]
        #[route("/")]
        DashboardRoute {},

        #[route("/projects")]
        ProjectsRoute {},

        #[route("/knowledge")]
        KnowledgeRoute {},

        #[route("/tasks-kanban")]
        TasksKanbanRoute {},

        #[route("/servers")]
        ServersRoute {},

        #[route("/federated-tasks")]
        FederatedTasksRoute {},

        #[route("/inbox")]
        InboxRoute {},

        #[route("/settings")]
        SettingsRoute {},

        #[route("/vox-test")]
        VoxTestRoute {},
}

#[component]
fn DashboardRoute() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Home" }
            Text { variant: TextVariant::Muted,
                "Task vertical slice — projects + tasks. Use the sidebar to navigate."
            }
        }
    }
}

#[component]
fn ProjectsRoute() -> Element {
    rsx! { crate::feature_routes::project::ProjectView {} }
}

#[component]
fn KnowledgeRoute() -> Element {
    rsx! { crate::feature_routes::knowledge::KnowledgeView {} }
}

#[component]
fn TasksKanbanRoute() -> Element {
    rsx! { crate::feature_routes::tasks_kanban::TasksKanbanView {} }
}

#[component]
fn ServersRoute() -> Element {
    rsx! { crate::feature_routes::servers::ServersView {} }
}

#[component]
fn FederatedTasksRoute() -> Element {
    rsx! { crate::feature_routes::federated_tasks::FederatedTasksView {} }
}

#[component]
fn InboxRoute() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Inbox" }
            Text { variant: TextVariant::Muted, "Coming soon." }
        }
    }
}

#[component]
fn VoxTestRoute() -> Element {
    let vox_status = use_context::<crate::vox_session::VoxStatusCtx>();
    let on_reconnect = move |_| {
        crate::vox_session::spawn_session_bootstrap(vox_status.0);
    };
    let status = vox_status.0.read().clone();
    let (label, variant) = match &status {
        crate::vox_session::VoxStatus::Idle => ("Idle", StatusBadgeVariant::Neutral),
        crate::vox_session::VoxStatus::Connecting { .. } => {
            ("Connecting…", StatusBadgeVariant::Warning)
        }
        crate::vox_session::VoxStatus::Connected { .. } => {
            ("Connected", StatusBadgeVariant::Success)
        }
        crate::vox_session::VoxStatus::Failed { .. } => ("Failed", StatusBadgeVariant::Danger),
    };
    let detail = match &status {
        crate::vox_session::VoxStatus::Idle => "Waiting for the bootstrap to run…".to_string(),
        crate::vox_session::VoxStatus::Connecting { url } => format!("Opening WS to {url}"),
        crate::vox_session::VoxStatus::Connected { url } => {
            format!("Vox handshake completed against {url}.")
        }
        crate::vox_session::VoxStatus::Failed { stage, error } => {
            format!("{stage} failed: {error}")
        }
    };
    rsx! {
        div { class: "mx-auto max-w-3xl flex flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Vox session test" }
                StatusBadge { variant, label: label.to_string() }
            }
            div {
                class: "rounded-md border border-border bg-card p-4 text-sm whitespace-pre-wrap",
                "{detail}"
            }
            HStack { class: "gap-2",
                Button { on_click: on_reconnect, "Reconnect" }
            }
        }
    }
}

#[component]
fn SettingsRoute() -> Element {
    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-2 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Settings" }
            Text { variant: TextVariant::Muted, "Not implemented yet." }
        }
    }
}

/// Top-level component that the platform launchers mount.
#[component]
pub fn App() -> Element {
    let orgs = organizations();
    let initial_org = orgs[0].clone();
    let active_org: Signal<Organization> = use_context_provider(move || Signal::new(initial_org));

    let org_overrides: OrgThemeOverrides = use_context_provider(|| OrgThemeOverrides {
        map: Signal::new(HashMap::<String, String>::new()),
        mode: Signal::new(ThemeMode::Dark),
    });

    let _project_overrides: ProjectThemeOverrides =
        use_context_provider(|| ProjectThemeOverrides {
            map: Signal::new(HashMap::<Uuid, String>::new()),
        });

    let vox_status = use_context_provider(crate::vox_session::VoxStatusCtx::new);
    use_hook(move || crate::vox_session::spawn_session_bootstrap(vox_status.0));

    let mut theme_state = use_signal(|| {
        let org = active_org.read().clone();
        state_from_preset_name(org.theme_preset, ThemeMode::Dark)
    });

    use_effect(move || {
        let org = active_org.read().clone();
        let resolved_name: String = org_overrides
            .map
            .read()
            .get(org.id)
            .cloned()
            .unwrap_or_else(|| org.theme_preset.to_string());
        let preset = theme_preset(&resolved_name).unwrap_or_else(default_theme_preset);
        theme_state.write().set_preset(preset);
    });

    use_effect(move || {
        let mode = *org_overrides.mode.read();
        if theme_state.peek().mode != mode {
            theme_state.write().set_mode(mode);
        }
    });

    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                Router::<Route> {}
            }
        }
    }
}

#[component]
fn AppShell() -> Element {
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

#[component]
fn MobileHeader() -> Element {
    let route = use_route::<Route>();
    let title = route_title(&route);
    rsx! {
        header {
            class: "sticky top-0 z-20 flex items-center gap-3 border-b border-border bg-background/95 px-4 py-3 backdrop-blur lg:hidden",
            div { class: "flex h-9 w-9 items-center justify-center rounded-xl bg-primary text-base font-black text-primary-foreground", "T" }
            div { class: "flex flex-col leading-tight",
                span { class: "text-xs uppercase tracking-[0.2em] text-muted-foreground", "Task" }
                span { class: "text-base font-semibold text-foreground", "{title}" }
            }
            div { class: "ml-auto", OrgSwitcher { compact: true } }
        }
    }
}

#[component]
fn BottomTabBar(current: Route) -> Element {
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
            on_close: move |_| more_open.set(false),
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

#[component]
fn DesktopSidebar(orgs: Vec<Organization>, current: Route) -> Element {
    let nav = use_navigator();
    rsx! {
        Sidebar { class: "flex h-screen w-72 flex-col overflow-hidden",
            SidebarHeader {
                HStack { gap: "3", class: "px-2 py-1",
                    div { class: "flex h-10 w-10 items-center justify-center rounded-xl bg-primary font-black text-primary-foreground", "T" }
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
            div { class: "flex-1 min-h-0 overflow-y-auto" }
            SidebarFooter {
                div { class: "px-1 pb-1 pt-2",
                    SectionHeader { label: "Organization", size: SectionHeaderSize::Small }
                }
                OrgSwitcher { orgs, compact: false }
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
                on_click: move |_| {
                    nav.push(route.clone());
                },
                span { class: "flex h-4 w-4 items-center justify-center", {icon()} }
                span { "{label}" }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
#[allow(unpredictable_function_pointer_comparisons)]
struct NavTab {
    label: &'static str,
    icon: fn() -> Element,
    route: Route,
}

fn icon_house() -> Element {
    rsx! { House { size: 16 } }
}
fn icon_inbox() -> Element {
    rsx! { InboxIcon { size: 16 } }
}
fn icon_folder_kanban() -> Element {
    rsx! { FolderKanban { size: 16 } }
}
fn icon_settings() -> Element {
    rsx! { SettingsIcon { size: 16 } }
}
fn icon_knowledge() -> Element {
    rsx! { BookOpen { size: 16 } }
}

fn nav_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Home",
            icon: icon_house,
            route: Route::DashboardRoute {},
        },
        NavTab {
            label: "Inbox",
            icon: icon_inbox,
            route: Route::InboxRoute {},
        },
        NavTab {
            label: "Projects",
            icon: icon_folder_kanban,
            route: Route::ProjectsRoute {},
        },
        NavTab {
            label: "Knowledge",
            icon: icon_knowledge,
            route: Route::KnowledgeRoute {},
        },
        NavTab {
            label: "Tasks kanban",
            icon: icon_folder_kanban,
            route: Route::TasksKanbanRoute {},
        },
        NavTab {
            label: "Servers",
            icon: icon_settings,
            route: Route::ServersRoute {},
        },
        NavTab {
            label: "Federated",
            icon: icon_folder_kanban,
            route: Route::FederatedTasksRoute {},
        },
        NavTab {
            label: "Vox test",
            icon: icon_settings,
            route: Route::VoxTestRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

fn primary_mobile_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Home",
            icon: icon_house,
            route: Route::DashboardRoute {},
        },
        NavTab {
            label: "Inbox",
            icon: icon_inbox,
            route: Route::InboxRoute {},
        },
        NavTab {
            label: "Projects",
            icon: icon_folder_kanban,
            route: Route::ProjectsRoute {},
        },
        NavTab {
            label: "Knowledge",
            icon: icon_knowledge,
            route: Route::KnowledgeRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

fn tabs_match(current: &Route, tab: &NavTab) -> bool {
    std::mem::discriminant(current) == std::mem::discriminant(&tab.route)
}

fn route_title(route: &Route) -> &'static str {
    match route {
        Route::DashboardRoute {} => "Home",
        Route::ProjectsRoute {} => "Projects",
        Route::KnowledgeRoute {} => "Knowledge",
        Route::TasksKanbanRoute {} => "Tasks kanban",
        Route::ServersRoute {} => "Servers",
        Route::FederatedTasksRoute {} => "Federated",
        Route::InboxRoute {} => "Inbox",
        Route::SettingsRoute {} => "Settings",
        Route::VoxTestRoute {} => "Vox test",
    }
}

#[component]
fn OrgSwitcher(
    #[props(default = Vec::new())] orgs: Vec<Organization>,
    #[props(default = false)] compact: bool,
) -> Element {
    let mut active_org = use_context::<Signal<Organization>>();
    let mut org_overrides = use_context::<OrgThemeOverrides>();
    let mut open = use_signal(|| false);
    let mut theme_open = use_signal(|| false);
    let orgs = if orgs.is_empty() {
        organizations()
    } else {
        orgs
    };
    let active = active_org();

    let mut switcher_state = use_signal(|| {
        let name = org_overrides
            .map
            .read()
            .get(active.id)
            .cloned()
            .unwrap_or_else(|| active.theme_preset.to_string());
        let mode = *org_overrides.mode.read();
        state_from_preset_name(&name, mode)
    });

    use_effect(move || {
        let org = active_org.read().clone();
        let name = org_overrides
            .map
            .read()
            .get(org.id)
            .cloned()
            .unwrap_or_else(|| org.theme_preset.to_string());
        let prev_mode = switcher_state.peek().mode;
        if switcher_state.peek().preset == name {
            return;
        }
        switcher_state.set(state_from_preset_name(&name, prev_mode));
    });

    let active_id_for_effect: &'static str = active.id;
    use_effect(move || {
        let name = switcher_state.read().preset.clone();
        let prev = org_overrides.map.read().get(active_id_for_effect).cloned();
        if prev.as_deref() != Some(name.as_str()) {
            let mut m = org_overrides.map.write();
            m.insert(active_id_for_effect.to_string(), name);
        }
    });

    let mut org_mode = org_overrides.mode;
    use_effect(move || {
        let mode = switcher_state.read().mode;
        if *org_mode.peek() != mode {
            org_mode.set(mode);
        }
    });

    rsx! {
        HStack { class: if compact { "items-center gap-1" } else { "items-center gap-1 w-full" },
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                class: if compact { "" } else { "w-full flex-1" },
                DropdownTrigger { class: if compact { "" } else { "w-full" },
                    button {
                        r#type: "button",
                        class: "flex items-center gap-2 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs font-semibold text-foreground hover:bg-accent",
                        "{active.name}"
                    }
                }
                DropdownContent {
                    side: if compact { "bottom" } else { "top" },
                    align: "end",
                    width: "w-64",
                    DropdownLabel { "Switch organization" }
                    for (idx, org) in orgs.iter().enumerate() {
                        {
                            let org_for_select = org.clone();
                            rsx! {
                                DropdownItem {
                                    key: "{org.id}",
                                    value: org.id.to_string(),
                                    index: idx,
                                    on_select: move |_| {
                                        active_org.set(org_for_select.clone());
                                        open.set(false);
                                    },
                                    "{org.name}"
                                }
                            }
                        }
                    }
                }
            }
            Popover {
                open: theme_open(),
                is_modal: false,
                on_open_change: move |o| theme_open.set(o),
                PopoverTrigger { class: "inline-flex",
                    button {
                        r#type: "button",
                        class: "inline-flex h-8 w-8 items-center justify-center rounded-md border border-border bg-card text-muted-foreground hover:bg-accent hover:text-accent-foreground",
                        title: "Organization theme",
                        onclick: move |_| {
                            let v = !*theme_open.read();
                            theme_open.set(v);
                        },
                        Palette { size: 14 }
                    }
                }
                PopoverContent {
                    side: ContentSide::Top,
                    align: ContentAlign::End,
                    class: "w-[17rem] p-3 max-h-[70vh] overflow-y-auto",
                    div { class: "flex flex-col gap-2",
                        span { class: "text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                            "Theme · {active.name}"
                        }
                        ThemeSwitcher { state: switcher_state }
                    }
                }
            }
        }
    }
}
