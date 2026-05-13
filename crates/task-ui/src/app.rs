use std::collections::HashMap;

use dioxus::prelude::*;
use dioxus_router::Navigator;
use fts_ui::lucide_dioxus::{
    BotMessageSquare, CalendarDays, ChefHat, ChevronDown, DollarSign, Dumbbell, FileText,
    FlaskConical, FolderKanban, House, Inbox as InboxIcon, Mail, MapPin, Menu, MessageCircle,
    MessagesSquare, Package, Palette, Settings as SettingsIcon, Timer as TimerIcon, Users, Video,
};
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::data::{Organization, organizations};
use crate::theming::{OrgThemeOverrides, ProjectThemeOverrides, state_from_preset_name};
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

        #[route("/test")]
        TestRoute {},

        // Per-feature multiplayer routes. Each one shares the same
        // workspace doc id via `sync::WORKSPACE_DOC_ID`, so edits in
        // any tab on any feature land on the same authoritative
        // server doc.
        #[route("/agent")]
        AgentRoute {},
        #[route("/calendar")]
        CalendarRoute {},
        #[route("/chat")]
        ChatRoute {},
        #[route("/conference")]
        ConferenceRoute {},
        #[route("/cookbook")]
        CookbookRoute {},
        #[route("/email")]
        EmailRoute {},
        #[route("/finance")]
        FinanceRoute {},
        #[route("/fitness")]
        FitnessRoute {},
        #[route("/inventory")]
        InventoryRoute {},
        #[route("/invoice")]
        InvoiceRoute {},
        #[route("/locations")]
        LocationsRoute {},
        #[route("/people")]
        PeopleRoute {},
        #[route("/projects-live")]
        ProjectsLiveRoute {},
        #[route("/threads")]
        ThreadsRoute {},
        #[route("/timer")]
        TimerRoute {},
        #[route("/agents/runs")]
        AgentRunsRoute {},
        #[route("/settings/integrations")]
        IntegrationsSettingsRoute {},
        #[route("/settings/webhooks")]
        WebhooksRoute {},
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

/// Minimal sanity-check route — no signals, no async, no Loro, no
/// WebSocket. If `/test` renders, the app shell + router are fine
/// and the issue is in a feature wiring; if it doesn't, the problem
/// is upstream (wasm bundle bootstrap, JS runtime, etc.).
#[component]
fn TestRoute() -> Element {
    let mut counter = use_signal(|| 0i32);
    rsx! {
        div { class: "mx-auto flex max-w-md flex-col gap-4 p-8",
            Heading { level: HeadingLevel::H1, "Hello from /test" }
            Text { variant: TextVariant::Muted,
                "If you can see this and the counter increments, Dioxus + the router + wasm are all healthy."
            }
            HStack { gap: "3".to_string(),
                Button {
                    on_click: move |_| counter += 1,
                    "Click me"
                }
                span { class: "text-2xl font-mono text-foreground", "{counter}" }
            }
        }
    }
}

#[component]
fn AgentRoute() -> Element {
    rsx! { crate::feature_routes::agent::AgentRunView {} }
}
#[component]
fn CalendarRoute() -> Element {
    rsx! { crate::feature_routes::calendar::CalendarEventView {} }
}
#[component]
fn ChatRoute() -> Element {
    rsx! { crate::feature_routes::chat::MessageView {} }
}
#[component]
fn ConferenceRoute() -> Element {
    rsx! { crate::feature_routes::conference::MeetingView {} }
}
#[component]
fn CookbookRoute() -> Element {
    rsx! { crate::feature_routes::cookbook::RecipeView {} }
}
#[component]
fn EmailRoute() -> Element {
    rsx! { crate::feature_routes::email::EmailView {} }
}
#[component]
fn FinanceRoute() -> Element {
    rsx! { crate::feature_routes::finance::RevenueView {} }
}
#[component]
fn FitnessRoute() -> Element {
    rsx! { crate::feature_routes::fitness::WorkoutSessionView {} }
}
#[component]
fn InventoryRoute() -> Element {
    rsx! { crate::feature_routes::inventory::InventoryView {} }
}
#[component]
fn InvoiceRoute() -> Element {
    rsx! { crate::feature_routes::invoice::InvoiceView {} }
}
#[component]
fn LocationsRoute() -> Element {
    rsx! { crate::feature_routes::location::LocationView {} }
}
#[component]
fn PeopleRoute() -> Element {
    rsx! { crate::feature_routes::person::PersonView {} }
}
#[component]
fn ProjectsLiveRoute() -> Element {
    rsx! { crate::feature_routes::project::ProjectView {} }
}
#[component]
fn ThreadsRoute() -> Element {
    rsx! { crate::feature_routes::threads::CommentView {} }
}
#[component]
fn TimerRoute() -> Element {
    rsx! { crate::feature_routes::timer::TimeEntryView {} }
}
#[component]
fn AgentRunsRoute() -> Element {
    rsx! { crate::feature_routes::agent::AgentRunBoardView {} }
}
#[component]
fn IntegrationsSettingsRoute() -> Element {
    rsx! { crate::feature_routes::agent::IntegrationSettingsView {} }
}
#[component]
fn WebhooksRoute() -> Element {
    rsx! { crate::feature_routes::agent::WebhookEventLogView {} }
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
    // App-level context: active org (lifted from AppShell so the theme
    // layer at the root can react to org switches without the route
    // tree owning the source of truth).
    let orgs = organizations();
    let initial_org = orgs[0].clone();
    let active_org: Signal<Organization> = use_context_provider(move || Signal::new(initial_org));

    // Per-org runtime preset override (user picks a different preset
    // for an org via the picker; persists for the session only).
    let org_overrides: OrgThemeOverrides = use_context_provider(|| OrgThemeOverrides {
        map: Signal::new(HashMap::<String, String>::new()),
    });

    // Per-project preset override (in-memory for v1).
    let _project_overrides: ProjectThemeOverrides =
        use_context_provider(|| ProjectThemeOverrides {
            map: Signal::new(HashMap::<Uuid, String>::new()),
        });

    // Derive the initial theme state from the active org's static default.
    let mut theme_state = use_signal(|| {
        let org = active_org.read().clone();
        state_from_preset_name(org.theme_preset, ThemeMode::Light)
    });

    // Re-apply preset whenever the active org or the per-org override map
    // changes. Preserves the user's current mode (light/dark) across the
    // swap so flipping orgs doesn't accidentally toggle dark mode.
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

    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                Router::<Route> {}
            }
        }
    }
}

// ── App shell (layout) ─────────────────────────────────────────────────

/// Wraps every route with the mobile-first chrome: top header on phones,
/// sidebar on desktop, bottom tab bar on phones. `Outlet::<Route>` is
/// where the route component renders.
#[component]
fn AppShell() -> Element {
    let orgs = organizations();
    let current = use_route::<Route>();

    rsx! {
        div { class: "min-h-screen bg-background text-foreground lg:grid lg:grid-cols-[18rem_1fr]",
            // Desktop sidebar (lg+)
            div { class: "hidden lg:block",
                SidebarProvider {
                    DesktopSidebar { orgs: orgs.clone(), current: current.clone() }
                }
            }

            // Main column
            div { class: "flex min-h-screen flex-col",
                MobileHeader {}

                main { class: "flex-1 pb-24 lg:pb-0",
                    // Suspense boundary required by dioxus-router when
                    // wasm-split is on — otherwise the entire page would
                    // suspend on first navigation.
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

// ── Mobile header ───────────────────────────────────────────────────────

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

// ── Bottom tab bar (mobile) ────────────────────────────────────────────

/// Mobile bottom bar — limited to the 4 most-used tabs plus a "More" slot
/// that opens a sheet listing every available destination.
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

// ── Desktop sidebar ────────────────────────────────────────────────────

#[component]
fn DesktopSidebar(orgs: Vec<Organization>, current: Route) -> Element {
    let nav = use_navigator();
    rsx! {
        Sidebar { class: "min-h-screen w-72".to_string(),
            SidebarHeader {
                HStack { gap: "3".to_string(), class: "px-2 py-1".to_string(),
                    div { class: "flex h-10 w-10 items-center justify-center rounded-xl bg-primary font-black text-primary-foreground", "T" }
                    div { class: "flex flex-col",
                        span { class: "text-base font-semibold text-foreground leading-tight", "Task" }
                        span { class: "text-xs text-muted-foreground", "Local-first command center" }
                        span { class: "mt-0.5 text-[10px] text-primary font-mono tracking-widest uppercase", "live · task-dev" }
                    }
                }
            }
            SidebarSeparator {}
            SidebarContent {
                SidebarGroup {
                    SidebarGroupLabel { "Workspace" }
                    SidebarGroupContent {
                        SidebarMenu {
                            for tab in core_nav_tabs() {
                                {render_sidebar_item(tab, &current, nav)}
                            }
                        }
                    }
                }
                SidebarGroup {
                    SidebarGroupLabel { "Features" }
                    SidebarGroupContent {
                        SidebarMenu {
                            for tab in feature_nav_tabs() {
                                {render_sidebar_item(tab, &current, nav)}
                            }
                        }
                    }
                }
                SidebarGroup {
                    SidebarGroupLabel { "System" }
                    SidebarGroupContent {
                        SidebarMenu {
                            for tab in system_nav_tabs() {
                                {render_sidebar_item(tab, &current, nav)}
                            }
                        }
                    }
                }
            }
            SidebarFooter {
                div { class: "px-1 pb-1 pt-2",
                    SectionHeader { label: "Organization".to_string(), size: SectionHeaderSize::Small }
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

// ── Nav tab table ──────────────────────────────────────────────────────

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
fn icon_bot_message() -> Element {
    rsx! { BotMessageSquare { size: 16 } }
}
fn icon_calendar() -> Element {
    rsx! { CalendarDays { size: 16 } }
}
fn icon_message_circle() -> Element {
    rsx! { MessageCircle { size: 16 } }
}
fn icon_video() -> Element {
    rsx! { Video { size: 16 } }
}
fn icon_chef() -> Element {
    rsx! { ChefHat { size: 16 } }
}
fn icon_mail() -> Element {
    rsx! { Mail { size: 16 } }
}
fn icon_dollar() -> Element {
    rsx! { DollarSign { size: 16 } }
}
fn icon_dumbbell() -> Element {
    rsx! { Dumbbell { size: 16 } }
}
fn icon_package() -> Element {
    rsx! { Package { size: 16 } }
}
fn icon_file_text() -> Element {
    rsx! { FileText { size: 16 } }
}
fn icon_map_pin() -> Element {
    rsx! { MapPin { size: 16 } }
}
fn icon_users() -> Element {
    rsx! { Users { size: 16 } }
}
fn icon_messages_square() -> Element {
    rsx! { MessagesSquare { size: 16 } }
}
fn icon_timer() -> Element {
    rsx! { TimerIcon { size: 16 } }
}
fn icon_flask() -> Element {
    rsx! { FlaskConical { size: 16 } }
}
fn icon_settings() -> Element {
    rsx! { SettingsIcon { size: 16 } }
}

fn core_nav_tabs() -> Vec<NavTab> {
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
    ]
}

fn feature_nav_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Agent",
            icon: icon_bot_message,
            route: Route::AgentRoute {},
        },
        NavTab {
            label: "Calendar",
            icon: icon_calendar,
            route: Route::CalendarRoute {},
        },
        NavTab {
            label: "Chat",
            icon: icon_message_circle,
            route: Route::ChatRoute {},
        },
        NavTab {
            label: "Conference",
            icon: icon_video,
            route: Route::ConferenceRoute {},
        },
        NavTab {
            label: "Cookbook",
            icon: icon_chef,
            route: Route::CookbookRoute {},
        },
        NavTab {
            label: "Email",
            icon: icon_mail,
            route: Route::EmailRoute {},
        },
        NavTab {
            label: "Finance",
            icon: icon_dollar,
            route: Route::FinanceRoute {},
        },
        NavTab {
            label: "Fitness",
            icon: icon_dumbbell,
            route: Route::FitnessRoute {},
        },
        NavTab {
            label: "Inventory",
            icon: icon_package,
            route: Route::InventoryRoute {},
        },
        NavTab {
            label: "Invoice",
            icon: icon_file_text,
            route: Route::InvoiceRoute {},
        },
        NavTab {
            label: "Locations",
            icon: icon_map_pin,
            route: Route::LocationsRoute {},
        },
        NavTab {
            label: "People",
            icon: icon_users,
            route: Route::PeopleRoute {},
        },
        NavTab {
            label: "Projects (live)",
            icon: icon_folder_kanban,
            route: Route::ProjectsLiveRoute {},
        },
        NavTab {
            label: "Threads",
            icon: icon_messages_square,
            route: Route::ThreadsRoute {},
        },
        NavTab {
            label: "Timer",
            icon: icon_timer,
            route: Route::TimerRoute {},
        },
    ]
}

fn system_nav_tabs() -> Vec<NavTab> {
    vec![
        NavTab {
            label: "Agent runs",
            icon: icon_bot_message,
            route: Route::AgentRunsRoute {},
        },
        NavTab {
            label: "Integrations",
            icon: icon_settings,
            route: Route::IntegrationsSettingsRoute {},
        },
        NavTab {
            label: "Webhooks",
            icon: icon_flask,
            route: Route::WebhooksRoute {},
        },
        NavTab {
            label: "Timer demo",
            icon: icon_timer,
            route: Route::TimerDemoRoute {},
        },
        NavTab {
            label: "Test",
            icon: icon_flask,
            route: Route::TestRoute {},
        },
        NavTab {
            label: "Settings",
            icon: icon_settings,
            route: Route::SettingsRoute {},
        },
    ]
}

fn nav_tabs() -> Vec<NavTab> {
    let mut all = core_nav_tabs();
    all.extend(feature_nav_tabs());
    all.extend(system_nav_tabs());
    all
}

/// Mobile bottom-tab shortlist — the 4 most-used destinations. Everything
/// else lives in the "More" sheet.
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
            label: "Calendar",
            icon: icon_calendar,
            route: Route::CalendarRoute {},
        },
        NavTab {
            label: "Timer",
            icon: icon_timer,
            route: Route::TimerRoute {},
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
        Route::InboxRoute {} => "Inbox",
        Route::SettingsRoute {} => "Settings",
        Route::TimerDemoRoute {} => "Timer demo",
        Route::TestRoute {} => "Test",
        Route::AgentRoute {} => "Agent",
        Route::CalendarRoute {} => "Calendar",
        Route::ChatRoute {} => "Chat",
        Route::ConferenceRoute {} => "Conference",
        Route::CookbookRoute {} => "Cookbook",
        Route::EmailRoute {} => "Email",
        Route::FinanceRoute {} => "Finance",
        Route::FitnessRoute {} => "Fitness",
        Route::InventoryRoute {} => "Inventory",
        Route::InvoiceRoute {} => "Invoices",
        Route::LocationsRoute {} => "Locations",
        Route::PeopleRoute {} => "People",
        Route::ProjectsLiveRoute {} => "Projects (live)",
        Route::ThreadsRoute {} => "Threads",
        Route::TimerRoute {} => "Timer",
        Route::AgentRunsRoute {} => "Agent runs",
        Route::IntegrationsSettingsRoute {} => "Integrations",
        Route::WebhooksRoute {} => "Webhooks",
    }
}

// ── Org switcher ───────────────────────────────────────────────────────

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

    // A scratch theme state that drives `ThemeSwitcher` inside the
    // popover. We sync it from the resolved org preset on each open,
    // and write back to `OrgThemeOverrides` when the user changes the
    // preset so the top-level App effect picks it up.
    let mut switcher_state = use_signal(|| {
        let name = org_overrides
            .map
            .read()
            .get(active.id)
            .cloned()
            .unwrap_or_else(|| active.theme_preset.to_string());
        state_from_preset_name(&name, ThemeMode::Light)
    });

    // When the active org changes, refresh the switcher state so the
    // popover always reflects the org currently being themed.
    use_effect(move || {
        let org = active_org.read().clone();
        let name = org_overrides
            .map
            .read()
            .get(org.id)
            .cloned()
            .unwrap_or_else(|| org.theme_preset.to_string());
        let prev_mode = switcher_state.read().mode;
        switcher_state.set(state_from_preset_name(&name, prev_mode));
    });

    // Propagate switcher-driven preset changes to the org overrides
    // map. The top-level `App` effect watches this map and updates the
    // real `ThemeProvider` state.
    let active_id_for_effect: &'static str = active.id;
    use_effect(move || {
        let name = switcher_state.read().preset.clone();
        let prev = org_overrides.map.read().get(active_id_for_effect).cloned();
        if prev.as_deref() != Some(name.as_str()) {
            let mut m = org_overrides.map.write();
            m.insert(active_id_for_effect.to_string(), name);
        }
    });

    rsx! {
        HStack { class: if compact { "items-center gap-1".to_string() } else { "items-center gap-1 w-full".to_string() },
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                class: if compact { "".to_string() } else { "w-full flex-1".to_string() },
                DropdownTrigger { class: if compact { "".to_string() } else { "w-full".to_string() },
                    {render_trigger(active.clone(), compact)}
                }
                DropdownContent {
                    align: if compact { "end".to_string() } else { "start".to_string() },
                    width: if compact { "w-64".to_string() } else { "w-[16rem]".to_string() },
                    DropdownLabel { "Switch organization" }
                    for (idx, org) in orgs.iter().enumerate() {
                        {
                            let org_for_select = org.clone();
                            let is_active = org.id == active.id;
                            rsx! {
                                DropdownItem {
                                    key: "{org.id}",
                                    value: org.id.to_string(),
                                    index: idx,
                                    on_select: move |_| {
                                        active_org.set(org_for_select.clone());
                                        open.set(false);
                                    },
                                    HStack { gap: "3".to_string(), class: "w-full".to_string(),
                                        Avatar { size: AvatarSize::Medium,
                                            AvatarFallback {
                                                class: "bg-primary text-primary-foreground font-bold".to_string(),
                                                "{org.monogram}"
                                            }
                                        }
                                        div { class: "flex min-w-0 flex-1 flex-col leading-tight",
                                            span { class: "truncate text-sm font-semibold text-foreground", "{org.name}" }
                                            span { class: "truncate text-xs text-muted-foreground", "{org.role}" }
                                        }
                                        if is_active {
                                            span { class: "text-primary", "●" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Org theme picker.
            Popover {
                open: theme_open(),
                is_modal: false,
                on_open_change: move |o| theme_open.set(o),
                PopoverTrigger { class: "inline-flex".to_string(),
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
                PopoverContent { class: "w-[22rem] p-3".to_string(),
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

fn render_trigger(active: Organization, compact: bool) -> Element {
    if compact {
        rsx! {
            button {
                class: "flex items-center gap-2 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs font-semibold text-foreground hover:bg-accent",
                r#type: "button",
                Avatar { size: AvatarSize::Small,
                    AvatarFallback { class: "bg-primary text-primary-foreground text-[10px] font-bold".to_string(), "{active.monogram}" }
                }
                span { class: "max-w-[6.5rem] truncate", "{active.name}" }
                ChevronDown { size: 14 }
            }
        }
    } else {
        rsx! {
            button {
                class: "flex w-full items-center gap-3 rounded-xl border border-border bg-card px-3 py-2.5 text-left text-sm font-semibold text-foreground hover:bg-accent",
                r#type: "button",
                Avatar { size: AvatarSize::Medium,
                    AvatarFallback { class: "bg-primary text-primary-foreground font-bold".to_string(), "{active.monogram}" }
                }
                div { class: "flex min-w-0 flex-1 flex-col leading-tight",
                    span { class: "truncate", "{active.name}" }
                    span { class: "text-xs font-normal text-muted-foreground", "{active.role}" }
                }
                ChevronDown { size: 16 }
            }
        }
    }
}
