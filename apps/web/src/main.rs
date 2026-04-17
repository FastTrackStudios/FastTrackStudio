//! Task Web — browser-based task management UI.
//!
//! Compiled to WASM, served by task-server. Communicates via Vox WebSocket RPC.
//! Uses the same fts-ui components as the desktop app.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use serde::{Deserialize, Serialize};

mod components;
mod workflows;

// ── Active project context ──────────────────────────────────────────────────

/// Global org list — fetched once, available everywhere.
static ORG_LIST: GlobalSignal<Vec<OrgInfo>> = Signal::global(|| Vec::new());

/// Look up org display info from the global list.
fn org_display(slug: &str) -> (String, u32) {
    let orgs = ORG_LIST.read();
    if let Some(org) = orgs.iter().find(|o| o.slug == slug) {
        (
            if org.emoji.is_empty() { "📁".to_string() } else { org.emoji.clone() },
            org.hue,
        )
    } else {
        ("📁".to_string(), 200)
    }
}

/// Display name for a project — uses workflow_stage if set, otherwise title.
fn display_name(p: &ApiProject) -> &str {
    p.workflow_stage.as_deref().unwrap_or(&p.title)
}

/// Build a project route using the slug if available, falling back to title.
fn project_link(p: &ApiProject) -> Route {
    let id = if p.slug.is_empty() { slug::slugify(&p.title) } else { p.slug.clone() };
    Route::ProjectDetailPage { title: id }
}

fn project_link_from_title(title: &str) -> Route {
    Route::ProjectDetailPage { title: slug::slugify(title) }
}

// ── Data types (from Vox RPC responses) ─────────────────────────────────────

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct ApiTask {
    title: String,
    status: String,
    priority: String,
    assignee: Option<String>,
    due: Option<String>,
    projects: Vec<String>,
    tags: Vec<String>,
    body: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
struct ApiProject {
    title: String,
    #[serde(default)]
    slug: String,
    status: String,
    area: Option<String>,
    due: Option<String>,
    team: Vec<String>,
    is_overdue: bool,
    #[serde(default)]
    hue: u32,
    project_type: Option<String>,
    organization: Option<String>,
    description: Option<String>,
    repo: Option<String>,
    workflow: Option<String>,
    workflow_stage: Option<String>,
    parent: Option<String>,
    #[serde(default)]
    references: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
struct ApiChange {
    entity_type: String,
    entity_id: String,
    field: Option<String>,
    old_value: Option<String>,
    new_value: Option<String>,
    changed_by: Option<String>,
    changed_at: String,
}

// ── Routes ──────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[layout(Shell)]
    #[route("/")]                       CommandCenter {},
    #[route("/tasks")]                  TasksPage {},
    #[route("/tasks/:title")]           TaskDetailPage { title: String },
    #[route("/projects")]               ProjectsPage {},
    #[route("/projects/:title")]        ProjectDetailPage { title: String },
    #[route("/activity")]               ActivityPage {},
}

// ── App ─────────────────────────────────────────────────────────────────────

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    dioxus::launch(App);
}

/// Global sidebar columns — persists across route changes.
/// Updated by ProjectDetailPage when it loads, read by Shell for rendering.
static SIDEBAR_COLUMNS: GlobalSignal<Vec<AncestryColumn>> = Signal::global(|| Vec::new());

#[component]
fn App() -> Element {
    let mut base_url = use_signal(|| String::new());
    let mut resolved = use_signal(|| false);

    use_context_provider(|| base_url);

    // Auto-discover the API server and load org list on mount
    use_effect(move || {
        spawn(async move {
            let url = discover_api_server().await;
            web_sys::console::log_1(&format!("[Task] Discovered API server: {url}").into());
            base_url.set(url.clone());

            // Fetch org list into global signal
            if let Ok(result) = rpc_call(&url, "list_orgs", serde_json::json!({})).await {
                if let Ok(orgs) = serde_json::from_value::<Vec<OrgInfo>>(result) {
                    *ORG_LIST.write() = orgs;
                }
            }

            resolved.set(true);
        });
    });

    rsx! {
        // Dark mode by default — toggle by adding/removing "dark" class
        div { class: "dark",
            document::Link { rel: "stylesheet", href: TAILWIND_CSS }
            if *resolved.read() {
                Router::<Route> {}
            } else {
                div { class: "flex items-center justify-center h-screen bg-background text-foreground text-sm text-muted-foreground",
                    "Connecting to server..."
                }
            }
        }
    }
}

// ── Server discovery ────────────────────────────────────────────────────────

#[allow(unused_variables)]
async fn discover_api_server() -> String {
    #[cfg(target_arch = "wasm32")]
    {
        let window = web_sys::window().unwrap();
        let location = window.location();

        if let Ok(search) = location.search() {
            if let Some(api) = search
                .trim_start_matches('?')
                .split('&')
                .find_map(|pair| pair.strip_prefix("api="))
            {
                return api.to_string();
            }
        }

        let origin = location.origin().unwrap_or_default();
        let hostname = location.hostname().unwrap_or_else(|_| "localhost".to_string());
        let protocol = location.protocol().unwrap_or_else(|_| "http:".to_string());

        web_sys::console::log_1(&format!("[Task] Probing same origin: {origin}").into());
        if probe_health(&origin).await {
            return origin;
        }

        let dev_base = format!("{protocol}//{hostname}:3456");
        web_sys::console::log_1(&format!("[Task] Probing dev server: {dev_base}").into());
        if probe_health(&dev_base).await {
            return dev_base;
        }

        web_sys::console::warn_1(&"[Task] No server found, using localhost fallback".into());
        "http://localhost:3456".to_string()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        "http://localhost:3456".to_string()
    }
}

#[allow(unused_variables)]
async fn probe_health(base_url: &str) -> bool {
    #[cfg(target_arch = "wasm32")]
    {
        let Ok(resp) = gloo_net::http::Request::get(&format!("{base_url}/api/health"))
            .send()
            .await
        else {
            return false;
        };
        resp.text().await.map(|t| t.trim() == "ok").unwrap_or(false)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        false
    }
}

// ── Vox WebSocket RPC ───────────────────────────────────────────────────────

#[allow(unused_variables)]
async fn rpc_call(base_url: &str, method: &str, params: serde_json::Value) -> Result<serde_json::Value, String> {
    #[cfg(target_arch = "wasm32")]
    {
        use gloo_net::websocket::futures::WebSocket;
        use gloo_net::websocket::Message;
        use futures::{SinkExt, StreamExt};

        let ws_url = format!(
            "{}/vox",
            base_url.replace("http://", "ws://").replace("https://", "wss://")
        );

        let ws = WebSocket::open(&ws_url).map_err(|e| {
            let msg = format!("WS open failed: {e:?}");
            web_sys::console::error_1(&msg.clone().into());
            msg
        })?;
        let (mut write, mut read) = ws.split();

        let request = serde_json::json!({"method": method, "params": params});
        write
            .send(Message::Text(request.to_string()))
            .await
            .map_err(|e| {
                let msg = format!("WS send failed: {e:?}");
                web_sys::console::error_1(&msg.clone().into());
                msg
            })?;

        match read.next().await {
            Some(Ok(Message::Text(s))) => {
                let v: serde_json::Value = serde_json::from_str(&s).map_err(|e| e.to_string())?;
                if let Some(err) = v.get("error").and_then(|e| {
                    if e.is_null() { None } else { e.as_str() }
                }) {
                    web_sys::console::error_1(&format!("[Task] RPC {method} error: {err}").into());
                    return Err(err.to_string());
                }
                Ok(v.get("result").cloned().unwrap_or(serde_json::Value::Null))
            }
            Some(Err(e)) => {
                let msg = format!("WS read error: {e:?}");
                web_sys::console::error_1(&msg.clone().into());
                Err(msg)
            }
            _ => {
                web_sys::console::error_1(&format!("[Task] RPC {method}: connection closed").into());
                Err("connection closed".into())
            }
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("rpc_call only works in WASM".into())
    }
}

// ── Shell (sidebar + content) ───────────────────────────────────────────────

/// Sidebar column data from the server.
#[derive(Debug, Clone, Deserialize, PartialEq)]
struct AncestryColumn {
    title: String,
    slug: String,
    hue: u32,
    #[serde(default)]
    children: Vec<AncestryChild>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct AncestryChild {
    title: String,
    slug: String,
    display_name: String,
    hue: u32,
}

#[component]
fn Shell() -> Element {
    let base_url: Signal<String> = use_context();
    let route = use_route::<Route>();

    // Read the current project slug from route (just for highlighting, no fetch)
    let current_project_slug = match route {
        Route::ProjectDetailPage { ref title } => Some(title.clone()),
        _ => None,
    };
    let slug_for_render = current_project_slug.clone();

    // Clear sidebar when navigating away from project pages
    {
        let on_project_page = current_project_slug.is_some();
        use_effect(move || {
            if !on_project_page && !SIDEBAR_COLUMNS.peek().is_empty() {
                *SIDEBAR_COLUMNS.write() = Vec::new();
            }
        });
    }

    let notifications = use_resource(move || {
        let base = base_url.read().clone();
        async move {
            if base.is_empty() { return Vec::new(); }
            match rpc_call(&base, "list_notifications", serde_json::json!({})).await {
                Ok(result) => serde_json::from_value::<Vec<components::notification_bell::NotificationItem>>(result).unwrap_or_default(),
                Err(_) => Vec::new(),
            }
        }
    });

    let notifs = notifications.read().as_ref().cloned().unwrap_or_default();
    let columns = SIDEBAR_COLUMNS.read().clone();

    rsx! {
        div { class: "flex h-screen bg-background text-foreground",
            // Global sidebar
            nav { class: "w-52 bg-sidebar border-r border-sidebar-border flex flex-col p-4 shrink-0",
                div { class: "flex items-center justify-between mb-6",
                    div { class: "flex items-center gap-2",
                        div { class: "flex size-7 items-center justify-center rounded-lg bg-primary text-primary-foreground text-xs font-bold",
                            "T"
                        }
                        span { class: "text-sm font-semibold tracking-tight", "Task" }
                    }
                    components::notification_bell::NotificationBell {
                        notifications: notifs,
                    }
                }

                div { class: "flex flex-col gap-1 flex-1",
                    NavItem { to: Route::CommandCenter {}, label: "Command Center" }
                    NavItem { to: Route::TasksPage {}, label: "Tasks" }
                    NavItem { to: Route::ProjectsPage {}, label: "Projects" }
                    NavItem { to: Route::ActivityPage {}, label: "Activity" }
                }

                // Bottom section: org switcher + user account
                div { class: "mt-auto flex flex-col gap-2 pt-4 border-t border-sidebar-border",
                    OrgSwitcher {}
                    UserMenu {}
                }
            }

            // Project columns — derived from URL, fetched from server
            for (col_idx, col) in columns.iter().enumerate() {
                if !col.children.is_empty() {
                    nav { class: "w-44 bg-sidebar/50 border-r border-sidebar-border flex flex-col overflow-y-auto shrink-0",
                        Link {
                            to: Route::ProjectDetailPage { title: col.slug.clone() },
                            class: "flex items-center gap-2 p-3 border-b border-sidebar-border hover:bg-sidebar-accent/50 transition-colors",
                            ProjectIcon { title: col.title.clone(), hue: col.hue, size: "size-5".to_string() }
                            span { class: "text-[11px] font-semibold truncate", "{col.title}" }
                        }
                        div { class: "flex flex-col py-1",
                            for child in col.children.iter() {
                                {
                                    // A child is "selected" if it matches the next column's project
                                    let next_col = columns.get(col_idx + 1);
                                    let is_selected = next_col.map_or(false, |nc| nc.slug == child.slug);
                                    // Or if it's the current page and there's no next column
                                    let is_current = slug_for_render.as_deref() == Some(&child.slug)
                                        && next_col.is_none();
                                    let highlighted = is_selected || is_current;
                                    let item_class = if highlighted {
                                        "flex items-center gap-2 px-3 py-1.5 text-[11px] bg-sidebar-accent text-sidebar-accent-foreground font-medium"
                                    } else {
                                        "flex items-center gap-2 px-3 py-1.5 text-[11px] text-sidebar-foreground/70 hover:text-sidebar-foreground hover:bg-sidebar-accent/50 transition-colors"
                                    };
                                    rsx! {
                                        Link {
                                            to: Route::ProjectDetailPage { title: child.slug.clone() },
                                            class: item_class,
                                            span { class: "size-1.5 rounded-full",
                                                class: if highlighted { "bg-primary" } else { "bg-muted-foreground/30" },
                                            }
                                            span { class: "truncate", "{child.display_name}" }
                                            if is_selected {
                                                svg {
                                                    class: "size-3 ml-auto text-muted-foreground shrink-0",
                                                    xmlns: "http://www.w3.org/2000/svg",
                                                    view_box: "0 0 24 24",
                                                    fill: "none",
                                                    stroke: "currentColor",
                                                    stroke_width: "2",
                                                    path { d: "m9 18 6-6-6-6" }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            main { class: "flex-1 overflow-y-auto p-8",
                div { class: "max-w-4xl mx-auto",
                    Outlet::<Route> {}
                }
            }
        }
    }
}

#[component]
fn NavItem(to: Route, label: &'static str) -> Element {
    rsx! {
        Link {
            to: to,
            class: "block px-3 py-2 rounded-lg text-sm text-sidebar-foreground/70 hover:text-sidebar-foreground hover:bg-sidebar-accent transition-colors",
            "{label}"
        }
    }
}

// ── Organization Switcher ────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct OrgInfo {
    slug: String,
    name: String,
    description: String,
    #[serde(default)]
    members: Vec<String>,
    /// Emoji icon for the org (e.g. "🎵", "💻", "🎸")
    #[serde(default)]
    emoji: String,
    /// Hue for color generation (0-360)
    #[serde(default)]
    hue: u32,
}

#[component]
fn OrgSwitcher() -> Element {
    let mut open = use_signal(|| false);

    // Read from global signal — no fetch needed
    let org_list = ORG_LIST.read().clone();
    let mut selected = use_signal(|| "all".to_string()); // "all" or org slug

    let current_org = {
        let sel = selected.read().clone();
        org_list.iter().find(|o| o.slug == sel).cloned()
    };
    let current_label = current_org.as_ref().map(|o| o.name.clone()).unwrap_or("All Organizations".to_string());
    let current_emoji = current_org.as_ref().map(|o| o.emoji.clone()).unwrap_or("∗".to_string());
    let current_hue = current_org.as_ref().map(|o| o.hue).unwrap_or(0);
    let total_members: usize = org_list.iter().map(|o| o.members.len()).sum();

    rsx! {
        div { class: "relative",
            button {
                class: "flex items-center gap-2 w-full px-2 py-1.5 rounded-lg text-xs hover:bg-sidebar-accent transition-colors text-left",
                onclick: move |_| { let v = *open.read(); open.set(!v); },
                {
                    let bg = if *selected.read() == "all" {
                        "var(--sidebar-primary)".to_string()
                    } else {
                        format!("oklch(0.45 0.12 {current_hue})")
                    };
                    rsx! {
                        div {
                            class: "flex size-6 items-center justify-center rounded-md text-[12px] shrink-0",
                            style: "background: {bg}; color: white;",
                            "{current_emoji}"
                        }
                    }
                }
                div { class: "flex-1 min-w-0",
                    p { class: "text-xs font-medium truncate", "{current_label}" }
                }
                svg {
                    class: "size-3 text-muted-foreground shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "m7 15 5 5 5-5" }
                    path { d: "m7 9 5-5 5 5" }
                }
            }

            if *open.read() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| open.set(false),
                }
                div { class: "absolute bottom-full left-0 mb-1 w-full rounded-lg border border-border bg-popover shadow-md z-50 py-1 max-h-64 overflow-y-auto",
                    // "All" option
                    button {
                        class: if *selected.read() == "all" {
                            "flex items-center gap-2 w-full px-3 py-2 text-xs bg-accent text-left"
                        } else {
                            "flex items-center gap-2 w-full px-3 py-2 text-xs hover:bg-accent transition-colors text-left"
                        },
                        onclick: move |_| { selected.set("all".to_string()); open.set(false); },
                        div { class: "flex size-5 items-center justify-center rounded bg-primary text-primary-foreground text-[9px] font-bold shrink-0",
                            "∗"
                        }
                        div { class: "min-w-0",
                            p { class: "font-medium", "All Organizations" }
                            p { class: "text-[10px] text-muted-foreground",
                                { format!("{} orgs · {} people", org_list.len(), total_members) }
                            }
                        }
                    }

                    div { class: "border-t border-border my-1" }

                    for org in org_list.iter() {
                        {
                            let slug = org.slug.clone();
                            let is_selected = *selected.read() == slug;
                            rsx! {
                                button {
                                    class: if is_selected {
                                        "flex items-center gap-2 w-full px-3 py-2 text-xs bg-accent text-left"
                                    } else {
                                        "flex items-center gap-2 w-full px-3 py-2 text-xs hover:bg-accent transition-colors text-left"
                                    },
                                    onclick: move |_| { selected.set(slug.clone()); open.set(false); },
                                    {
                                        let org_bg = format!("oklch(0.45 0.12 {})", org.hue);
                                        rsx! {
                                            div {
                                                class: "flex size-5 items-center justify-center rounded text-[10px] shrink-0",
                                                style: "background: {org_bg}; color: white;",
                                                "{org.emoji}"
                                            }
                                        }
                                    }
                                    div { class: "min-w-0",
                                        p { class: "font-medium truncate", "{org.name}" }
                                        p { class: "text-[10px] text-muted-foreground truncate",
                                            { format!("{} members", org.members.len()) }
                                        }
                                    }
                                    if is_selected {
                                        svg {
                                            class: "size-3 text-primary ml-auto shrink-0",
                                            xmlns: "http://www.w3.org/2000/svg",
                                            view_box: "0 0 24 24",
                                            fill: "none",
                                            stroke: "currentColor",
                                            stroke_width: "3",
                                            path { d: "M20 6 9 17l-5-5" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── User Menu ───────────────────────────────────────────────────────────────

#[component]
fn UserMenu() -> Element {
    let mut open = use_signal(|| false);

    // Mock current user — in production this comes from auth session
    let user_name = "Cody Wright";
    let user_email = "cody@fasttrackstudio.com";
    let user_initials = "CW";

    rsx! {
        div { class: "relative",
            button {
                class: "flex items-center gap-2 w-full px-2 py-1.5 rounded-lg text-xs hover:bg-sidebar-accent transition-colors text-left",
                onclick: move |_| { let v = *open.read(); open.set(!v); },
                UserAvatar { name: user_name.to_string(), size: "size-6".to_string() }
                div { class: "flex-1 min-w-0",
                    p { class: "text-xs font-medium truncate", "{user_name}" }
                    p { class: "text-[10px] text-muted-foreground truncate", "{user_email}" }
                }
                svg {
                    class: "size-3 text-muted-foreground shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "m9 18 6-6-6-6" }
                }
            }

            if *open.read() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| open.set(false),
                }
                div { class: "absolute bottom-full left-0 mb-1 w-full rounded-lg border border-border bg-popover shadow-md z-50 py-1",
                    div { class: "px-3 py-2 border-b border-border",
                        p { class: "text-xs font-medium", "{user_name}" }
                        p { class: "text-[10px] text-muted-foreground", "{user_email}" }
                    }
                    button {
                        class: "flex items-center gap-2 w-full px-3 py-2 text-xs hover:bg-accent transition-colors text-left",
                        svg {
                            class: "size-3.5 text-muted-foreground",
                            xmlns: "http://www.w3.org/2000/svg",
                            view_box: "0 0 24 24",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            circle { cx: "12", cy: "12", r: "3" }
                            path { d: "M12 1v2M12 21v2M4.22 4.22l1.42 1.42M18.36 18.36l1.42 1.42M1 12h2M21 12h2M4.22 19.78l1.42-1.42M18.36 5.64l1.42-1.42" }
                        }
                        "Settings"
                    }
                    button {
                        class: "flex items-center gap-2 w-full px-3 py-2 text-xs text-destructive hover:bg-accent transition-colors text-left",
                        svg {
                            class: "size-3.5",
                            xmlns: "http://www.w3.org/2000/svg",
                            view_box: "0 0 24 24",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            path { d: "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" }
                            path { d: "m16 17 5-5-5-5" }
                            path { d: "M21 12H9" }
                        }
                        "Sign out"
                    }
                }
            }
        }
    }
}

#[component]
fn SyncButton() -> Element {
    let base_url: Signal<String> = use_context();
    let mut syncing = use_signal(|| false);

    rsx! {
        Button {
            variant: ButtonVariant::Secondary,
            size: ButtonSize::Small,
            class: "w-full".to_string(),
            disabled: *syncing.read(),
            on_click: move |_| {
                syncing.set(true);
                let base = base_url.read().clone();
                spawn(async move {
                    let _ = rpc_call(&base, "trigger_sync", serde_json::json!({})).await;
                    syncing.set(false);
                });
            },
            if *syncing.read() { "Syncing..." } else { "Sync Now" }
        }
    }
}

// ── Command Center ──────────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ProjectSummary {
    project: ApiProject,
    total_tasks: usize,
    done_tasks: usize,
    #[serde(default)]
    open_tasks: usize,
    #[serde(default)]
    in_progress: usize,
    #[serde(default)]
    urgent_count: usize,
    #[serde(default)]
    overdue_count: usize,
    #[serde(default)]
    notifications: usize,
    next_task: Option<ApiTask>,
}

// ── Reusable UI helpers ─────────────────────────────────────────────────────

/// Project icon — colored circle with first letter of the title.
#[component]
fn ProjectIcon(title: String, hue: u32, #[props(default = "size-8".to_string())] size: String) -> Element {
    let letter = title.chars().next().unwrap_or('?').to_uppercase().to_string();
    let bg = format!("oklch(0.55 0.15 {hue})");
    let fg = format!("oklch(0.95 0.03 {hue})");
    rsx! {
        div {
            class: "{size} rounded-lg flex items-center justify-center text-xs font-bold shrink-0",
            style: "background: {bg}; color: {fg};",
            "{letter}"
        }
    }
}

/// User avatar — colored circle with initials derived from name.
#[component]
fn UserAvatar(name: String, #[props(default = "size-6".to_string())] size: String) -> Element {
    let initials: String = name.split_whitespace()
        .filter_map(|w| w.chars().next())
        .take(2)
        .collect::<String>()
        .to_uppercase();
    let initials = if initials.is_empty() { name.chars().take(2).collect::<String>().to_uppercase() } else { initials };
    // Derive hue from name
    let hue: u32 = name.bytes().fold(5381u32, |h, b| h.wrapping_mul(33).wrapping_add(b as u32)) % 360;
    let bg = format!("oklch(0.45 0.12 {hue})");
    rsx! {
        div {
            class: "{size} rounded-full flex items-center justify-center text-[10px] font-medium text-white shrink-0",
            style: "background: {bg};",
            "{initials}"
        }
    }
}

/// Returns the first uncompleted subtask from a markdown body, if any.
fn next_subtask(body: &str) -> Option<String> {
    body.lines().find_map(|line| {
        let trimmed = line.trim().trim_start_matches('-').trim();
        trimmed.strip_prefix("[ ]").map(|rest| rest.trim().to_string())
    })
}

#[component]
fn CommandCenter() -> Element {
    let base_url: Signal<String> = use_context();

    let data = use_resource(move || {
        let base = base_url.read().clone();
        async move {
            if base.is_empty() {
                return Vec::new();
            }
            match rpc_call(&base, "command_center", serde_json::json!({})).await {
                Ok(result) => serde_json::from_value::<Vec<ProjectSummary>>(result).unwrap_or_default(),
                Err(_) => Vec::new(),
            }
        }
    });

    let loading = data.read().is_none();

    rsx! {
        div { class: "flex flex-col gap-6",
            h2 { class: "text-lg font-semibold tracking-tight", "Command Center" }

            if loading {
                div { class: "grid grid-cols-1 lg:grid-cols-2 gap-4",
                    Skeleton { class: "h-44".to_string() }
                    Skeleton { class: "h-44".to_string() }
                    Skeleton { class: "h-44".to_string() }
                    Skeleton { class: "h-44".to_string() }
                }
            } else if data.read().as_ref().map_or(true, |d| d.is_empty()) {
                EmptyState { message: "No active projects.".to_string() }
            } else {
                div { class: "grid grid-cols-1 lg:grid-cols-2 gap-4",
                    for summary in data.read().as_ref().unwrap().iter() {
                        ProjectCard2 { summary: summary.clone() }
                    }
                }
            }
        }
    }
}

#[component]
fn ProjectCard2(summary: ProjectSummary) -> Element {
    let progress = if summary.total_tasks > 0 {
        (summary.done_tasks as f64 / summary.total_tasks as f64 * 100.0) as u32
    } else {
        0
    };

    let subtask = summary.next_task.as_ref()
        .and_then(|t| t.body.as_deref())
        .and_then(next_subtask);

    let project_link_title = summary.project.title.clone();

    rsx! {
        Card {
            CardHeader { class: "pb-2".to_string(),
                div { class: "flex items-start justify-between",
                    Link {
                        to: project_link_from_title(&project_link_title),
                        class: "flex items-center gap-3 hover:opacity-80 transition-opacity",
                        ProjectIcon { title: summary.project.title.clone(), hue: summary.project.hue }
                        div {
                            CardTitle { class: "text-base".to_string(), "{summary.project.title}" }
                            div { class: "flex items-center gap-1.5 mt-0.5",
                                if let Some(ref org) = summary.project.organization {
                                    {
                                        let (emoji, hue) = org_display(org);
                                        let bg = format!("oklch(0.45 0.12 {hue})");
                                        rsx! {
                                            span {
                                                class: "inline-flex items-center gap-1 h-4 rounded px-1.5 text-[9px] font-medium",
                                                style: "background: {bg}; color: white;",
                                                span { class: "text-[10px]", "{emoji}" }
                                                "{org}"
                                            }
                                        }
                                    }
                                }
                                if let Some(ref area) = summary.project.area {
                                    span { class: "text-[10px] text-muted-foreground", "{area}" }
                                }
                                if let Some(ref pt) = summary.project.project_type {
                                    span { class: "text-[10px] text-muted-foreground",
                                        { format!("· {}", workflow_label(pt)) }
                                    }
                                }
                            }
                        }
                    }
                    div { class: "flex items-center gap-2",
                        if summary.project.is_overdue {
                            span { class: "inline-flex items-center h-5 rounded-full bg-destructive/10 text-destructive px-2 text-[10px] font-medium",
                                "Overdue"
                            }
                        }
                        // Notification bell
                        if summary.notifications > 0 {
                            div { class: "relative",
                                svg {
                                    class: "size-4 text-muted-foreground",
                                    xmlns: "http://www.w3.org/2000/svg",
                                    view_box: "0 0 24 24",
                                    fill: "none",
                                    stroke: "currentColor",
                                    stroke_width: "2",
                                    stroke_linecap: "round",
                                    stroke_linejoin: "round",
                                    path { d: "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9" }
                                    path { d: "M10.3 21a1.94 1.94 0 0 0 3.4 0" }
                                }
                                span { class: "absolute -top-1.5 -right-1.5 flex size-4 items-center justify-center rounded-full bg-destructive text-[9px] font-bold text-destructive-foreground",
                                    "{summary.notifications}"
                                }
                            }
                        }
                    }
                }

                // Stats row
                div { class: "flex items-center gap-3 mt-2 text-[11px] text-muted-foreground",
                    if let Some(ref due) = summary.project.due {
                        span { "Due {due}" }
                    }
                    if summary.in_progress > 0 {
                        span { class: "inline-flex items-center gap-1",
                            span { class: "size-1.5 rounded-full bg-chart-2" }
                            { format!("{} in progress", summary.in_progress) }
                        }
                    }
                    if summary.urgent_count > 0 {
                        span { class: "inline-flex items-center gap-1 text-destructive",
                            { format!("{} urgent", summary.urgent_count) }
                        }
                    }
                }

                // Progress bar
                div { class: "mt-3 flex items-center gap-3",
                    div { class: "flex-1 h-1.5 rounded-full bg-secondary overflow-hidden",
                        div {
                            class: "h-full rounded-full bg-primary transition-all duration-500",
                            style: "width: {progress}%",
                        }
                    }
                    span { class: "text-[11px] text-muted-foreground tabular-nums shrink-0",
                        "{summary.done_tasks}/{summary.total_tasks}"
                    }
                }
            }

            // Next action
            CardContent { class: "pt-0".to_string(),
                if let Some(ref task) = summary.next_task {
                    div { class: "rounded-lg bg-accent/50 px-3 py-2.5",
                        div { class: "flex items-center justify-between",
                            span { class: "text-[10px] font-medium uppercase tracking-wider text-muted-foreground", "Next" }
                            if let Some(ref assignee) = task.assignee {
                                UserAvatar { name: assignee.clone() }
                            }
                        }
                        Link {
                            to: Route::TaskDetailPage { title: task.title.clone() },
                            class: "text-sm font-medium mt-1 hover:underline block",
                            "{task.title}"
                        }
                        if let Some(ref st) = subtask {
                            p { class: "text-xs text-muted-foreground mt-0.5", "→ {st}" }
                        }
                    }
                } else {
                    div { class: "rounded-lg bg-primary/5 px-3 py-2 text-xs text-muted-foreground italic",
                        "All tasks complete"
                    }
                }
            }
        }
    }
}

// ── Tasks Page ──────────────────────────────────────────────────────────────

#[component]
fn TasksPage() -> Element {
    let base_url: Signal<String> = use_context();
    let search = use_signal(String::new);
    let refresh = use_signal(|| 0u64);

    // Tracks titles currently being completed (optimistic UI)
    let mut pending = use_signal(|| std::collections::HashSet::<String>::new());

    let tasks = use_resource(move || {
        let base = base_url.read().clone();
        let _rev = refresh();
        async move {
            if base.is_empty() {
                return Vec::new();
            }
            let result = match rpc_call(&base, "list_tasks", serde_json::json!({})).await {
                Ok(result) => serde_json::from_value::<Vec<ApiTask>>(result).unwrap_or_default(),
                Err(_) => Vec::new(),
            };
            // Clear optimistic pending set now that we have fresh server data
            pending.write().clear();
            result
        }
    });

    // Provide shared state to children
    use_context_provider(|| refresh);
    use_context_provider(|| pending);

    let filtered: Vec<ApiTask> = match tasks.read().as_ref() {
        Some(list) => {
            let q = search.read().to_lowercase();
            let pending_set = pending.read();
            let mut out: Vec<ApiTask> = list.iter()
                .filter(|t| q.is_empty() || t.title.to_lowercase().contains(&q))
                .cloned()
                .collect();
            // Done + pending-completion sort to bottom
            out.sort_by_key(|t| {
                if t.status == "Done" || pending_set.contains(&t.title) { 1 } else { 0 }
            });
            out
        }
        None => Vec::new(),
    };

    let loading = tasks.read().is_none();

    rsx! {
        div { class: "flex flex-col gap-6",
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold tracking-tight", "Tasks" }
                span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                    "{filtered.len()}"
                }
            }

            div { class: "flex items-center gap-3",
                div { class: "flex-1",
                    Input {
                        value: search,
                        placeholder: "Search tasks...".to_string(),
                        size: InputSize::Small,
                    }
                }
                NewTaskButton {}
            }

            if loading {
                div { class: "flex flex-col gap-3 py-8",
                    SkeletonText {}
                    SkeletonText {}
                    SkeletonText {}
                }
            } else if filtered.is_empty() {
                EmptyState { message: "No tasks found.".to_string() }
            } else {
                div { class: "rounded-2xl border border-border bg-card overflow-hidden",
                    div { class: "divide-y divide-border",
                        for task in &filtered {
                            TaskRow { key: "{task.title}", task: task.clone() }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn TaskRow(task: ApiTask) -> Element {
    let base_url: Signal<String> = use_context();
    let mut refresh: Signal<u64> = use_context();
    let mut pending: Signal<std::collections::HashSet<String>> = use_context();

    let confirmed_done = task.status == "Done";
    let is_pending = pending.read().contains(&task.title);
    let visually_done = confirmed_done || is_pending;

    let check_class = if is_pending {
        "flex size-4 items-center justify-center rounded-[6px] border border-muted-foreground/50 bg-muted-foreground/30 text-muted-foreground cursor-wait shrink-0 transition-all duration-300"
    } else if confirmed_done {
        "flex size-4 items-center justify-center rounded-[6px] border border-primary bg-primary text-primary-foreground shrink-0 transition-all duration-300"
    } else {
        "flex size-4 items-center justify-center rounded-[6px] border border-input cursor-pointer hover:border-ring shrink-0 transition-all duration-300"
    };

    let title_class = if visually_done {
        "text-sm font-medium line-through text-muted-foreground transition-all duration-300"
    } else {
        "text-sm font-medium text-foreground transition-all duration-300"
    };

    let row_class = if is_pending {
        "flex items-center gap-3 px-4 py-3 transition-all duration-300 opacity-50 animate-pulse"
    } else if confirmed_done {
        "flex items-center gap-3 px-4 py-3 transition-all duration-300 opacity-40 hover:opacity-60"
    } else {
        "flex items-center gap-3 px-4 py-3 transition-all duration-300 hover:bg-accent/50"
    };

    rsx! {
        div { class: row_class,
            button {
                r#type: "button",
                class: check_class,
                disabled: visually_done,
                onclick: {
                    let title = task.title.clone();
                    move |_| {
                        // Optimistic: add to pending set immediately
                        pending.write().insert(title.clone());
                        let base = base_url.read().clone();
                        let title = title.clone();
                        spawn(async move {
                            let _ = rpc_call(&base, "complete_task", serde_json::json!({"title": title})).await;
                            // Server confirmed — trigger re-fetch (which clears pending)
                            refresh += 1;
                        });
                    }
                },
                if visually_done {
                    svg {
                        class: "size-3",
                        xmlns: "http://www.w3.org/2000/svg",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "3",
                        path { d: "M20 6 9 17l-5-5" }
                    }
                }
            }

            div { class: "flex-1 min-w-0",
                Link {
                    to: Route::TaskDetailPage { title: task.title.clone() },
                    class: "{title_class} hover:underline cursor-pointer",
                    "{task.title}"
                }
                if !task.projects.is_empty() {
                    div { class: "flex items-center gap-1.5 mt-0.5 text-xs text-muted-foreground",
                        for proj in &task.projects {
                            span { "{proj}" }
                        }
                    }
                }
            }

            div { class: "flex items-center gap-2 shrink-0",
                for tag in &task.tags {
                    span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                        "#{tag}"
                    }
                }
                if let Some(ref due) = task.due {
                    span { class: "text-xs text-muted-foreground", "{due}" }
                }
                span { class: "text-xs text-muted-foreground", "{task.priority}" }
                if let Some(ref assignee) = task.assignee {
                    UserAvatar { name: assignee.clone() }
                }
            }
        }
    }
}

#[component]
fn NewTaskButton() -> Element {
    let base_url: Signal<String> = use_context();
    let mut refresh: Signal<u64> = use_context();
    let mut show_form = use_signal(|| false);
    let mut title = use_signal(String::new);
    let mut assignee = use_signal(String::new);
    let mut priority = use_signal(|| "Normal".to_string());
    let mut due = use_signal(String::new);

    if !*show_form.read() {
        return rsx! {
            Button {
                variant: ButtonVariant::Primary,
                size: ButtonSize::Small,
                on_click: move |_| show_form.set(true),
                "+ New Task"
            }
        };
    }

    rsx! {
        div { class: "flex flex-col gap-2 p-3 rounded-xl border border-border bg-card",
            Input {
                value: title,
                placeholder: "Task title...".to_string(),
                size: InputSize::Small,
            }
            div { class: "grid grid-cols-3 gap-2",
                Input {
                    value: assignee,
                    placeholder: "Assignee".to_string(),
                    size: InputSize::Small,
                }
                div { class: "relative",
                    select {
                        class: "w-full h-8 rounded-lg border border-input bg-input/30 px-2 text-xs appearance-none cursor-pointer",
                        value: "{priority}",
                        onchange: move |evt: FormEvent| priority.set(evt.value().to_string()),
                        option { value: "Urgent", "Urgent" }
                        option { value: "High", "High" }
                        option { value: "Normal", selected: true, "Normal" }
                        option { value: "Low", "Low" }
                    }
                }
                Input {
                    value: due,
                    placeholder: "Due (YYYY-MM-DD)".to_string(),
                    size: InputSize::Small,
                }
            }
            div { class: "flex items-center gap-2",
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: move |_| {
                        let base = base_url.read().clone();
                        let t = title.read().clone();
                        let a = assignee.read().clone();
                        let p = priority.read().clone();
                        let d = due.read().clone();
                        if !t.is_empty() {
                            spawn(async move {
                                let mut params = serde_json::json!({"title": t, "priority": p});
                                if !a.is_empty() {
                                    params["assignee"] = serde_json::json!(a);
                                }
                                if !d.is_empty() {
                                    params["due"] = serde_json::json!(d);
                                }
                                let _ = rpc_call(&base, "create_task", params).await;
                                refresh += 1;
                            });
                        }
                        show_form.set(false);
                        title.set(String::new());
                        assignee.set(String::new());
                        due.set(String::new());
                    },
                    "Create"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| {
                        show_form.set(false);
                        title.set(String::new());
                    },
                    "Cancel"
                }
            }
        }
    }
}

// ── Task Detail Page ────────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiSubtask {
    text: String,
    done: bool,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiComment {
    id: String,
    author: String,
    body: String,
    created_at: Option<String>,
    resolved: bool,
    reply_to: Option<String>,
    #[serde(default)]
    mentions: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct TaskDetail {
    task: ApiTask,
    #[serde(default)]
    subtasks: Vec<ApiSubtask>,
    subtask_progress: Option<f64>,
    #[serde(default)]
    comments: Vec<ApiComment>,
}

#[component]
fn TaskDetailPage(title: ReadSignal<String>) -> Element {
    let base_url: Signal<String> = use_context();
    let mut refresh = use_signal(|| 0u64);

    let detail = use_resource(move || {
        let base = base_url.read().clone();
        let t = title();
        let _rev = refresh();
        async move {
            if base.is_empty() || t.is_empty() { return None; }
            match rpc_call(&base, "task_detail", serde_json::json!({"title": t})).await {
                Ok(result) => serde_json::from_value::<TaskDetail>(result).ok(),
                Err(_) => None,
            }
        }
    });

    let d = detail.read();
    let data = d.as_ref().and_then(|o| o.as_ref());

    if detail.read().is_none() {
        return rsx! {
            div { class: "flex flex-col gap-4",
                Skeleton { class: "h-12".to_string() }
                Skeleton { class: "h-40".to_string() }
            }
        };
    }

    let Some(data) = data else {
        return rsx! {
            EmptyState { message: format!("Task \"{}\" not found.", title) }
        };
    };

    let task = &data.task;
    let done = task.status == "Done";

    let status_color = match task.status.as_str() {
        "Open" => "bg-chart-1",
        "InProgress" => "bg-chart-2",
        "Done" => "bg-primary",
        "OnHold" => "bg-muted-foreground",
        "Planned" => "bg-chart-4",
        _ => "bg-secondary",
    };

    let priority_color = match task.priority.as_str() {
        "Urgent" => "text-destructive",
        "High" => "text-chart-1",
        _ => "text-muted-foreground",
    };

    rsx! {
        div { class: "flex flex-col gap-6",
            // Back link
            Link {
                to: Route::TasksPage {},
                class: "text-xs text-muted-foreground hover:text-foreground transition-colors inline-flex items-center gap-1",
                svg {
                    class: "size-3",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    path { d: "m15 18-6-6 6-6" }
                }
                "Tasks"
            }

            // Header
            div { class: "flex items-start justify-between",
                div { class: "flex-1",
                    div { class: "flex items-center gap-3",
                        span { class: "size-3 rounded-full {status_color}" }
                        h1 { class: if done { "text-xl font-semibold tracking-tight line-through text-muted-foreground" } else { "text-xl font-semibold tracking-tight" },
                            "{task.title}"
                        }
                    }
                    div { class: "flex items-center gap-3 mt-2 text-sm text-muted-foreground",
                        span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                            "{task.status}"
                        }
                        span { class: "{priority_color} text-xs font-medium", "{task.priority}" }
                        if let Some(ref due) = task.due {
                            span { "Due {due}" }
                        }
                        for proj in &task.projects {
                            span { class: "text-xs", "{proj}" }
                        }
                    }
                }
                div { class: "flex items-center gap-2",
                    if let Some(ref assignee) = task.assignee {
                        UserAvatar { name: assignee.clone(), size: "size-8".to_string() }
                    }
                    if !done {
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: {
                                let base = base_url.read().clone();
                                let t = task.title.clone();
                                move |_| {
                                    let base = base.clone();
                                    let t = t.clone();
                                    spawn(async move {
                                        let _ = rpc_call(&base, "complete_task", serde_json::json!({"title": t})).await;
                                        refresh += 1;
                                    });
                                }
                            },
                            "Complete"
                        }
                    }
                }
            }

            // Subtasks
            if !data.subtasks.is_empty() {
                div { class: "rounded-xl border border-border bg-card overflow-hidden",
                    div { class: "flex items-center justify-between px-4 py-2 border-b border-border",
                        span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider", "Subtasks" }
                        if let Some(progress) = data.subtask_progress {
                            span { class: "text-xs text-muted-foreground tabular-nums",
                                { format!("{}%", (progress * 100.0) as u32) }
                            }
                        }
                    }
                    // Progress bar
                    if let Some(progress) = data.subtask_progress {
                        {
                            let pct = (progress * 100.0) as u32;
                            rsx! {
                                div { class: "h-1 bg-secondary",
                                    div {
                                        class: "h-full bg-primary transition-all duration-300",
                                        style: "width: {pct}%",
                                    }
                                }
                            }
                        }
                    }
                    div { class: "divide-y divide-border",
                        for st in data.subtasks.iter() {
                            div { class: "flex items-center gap-3 px-4 py-2",
                                if st.done {
                                    span { class: "size-4 rounded-[6px] border border-primary bg-primary text-primary-foreground flex items-center justify-center shrink-0",
                                        svg {
                                            class: "size-2.5",
                                            xmlns: "http://www.w3.org/2000/svg",
                                            view_box: "0 0 24 24",
                                            fill: "none",
                                            stroke: "currentColor",
                                            stroke_width: "3",
                                            path { d: "M20 6 9 17l-5-5" }
                                        }
                                    }
                                    span { class: "text-sm text-muted-foreground line-through", "{st.text}" }
                                } else {
                                    span { class: "size-4 rounded-[6px] border border-input shrink-0" }
                                    span { class: "text-sm", "{st.text}" }
                                }
                            }
                        }
                    }
                }
            }

            // Tags
            if !task.tags.is_empty() {
                div { class: "flex flex-wrap gap-1.5",
                    for tag in task.tags.iter() {
                        span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                            "#{tag}"
                        }
                    }
                }
            }

            // Body (markdown content, excluding subtasks/comments)
            if let Some(ref body) = task.body {
                if !body.is_empty() {
                    {
                        let display_body: String = body.lines()
                            .filter(|l| !l.trim().starts_with("- [") && !l.trim().starts_with("## Comments") && !l.trim().starts_with("**@"))
                            .take_while(|l| l.trim() != "## Comments")
                            .collect::<Vec<_>>()
                            .join("\n")
                            .trim()
                            .to_string();
                        if !display_body.is_empty() {
                            rsx! {
                                div { class: "rounded-xl border border-border bg-card px-4 py-3",
                                    pre { class: "text-sm text-muted-foreground whitespace-pre-wrap font-sans", "{display_body}" }
                                }
                            }
                        } else {
                            rsx! {}
                        }
                    }
                }
            }

            // Comments
            if !data.comments.is_empty() {
                div { class: "rounded-xl border border-border bg-card overflow-hidden",
                    div { class: "flex items-center gap-2 px-4 py-2 border-b border-border",
                        span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider", "Comments" }
                        span { class: "text-xs text-muted-foreground",
                            { format!("{}", data.comments.iter().filter(|c| c.reply_to.is_none()).count()) }
                        }
                    }
                    div { class: "divide-y divide-border",
                        for comment in data.comments.iter() {
                            {
                                let indent = if comment.reply_to.is_some() { "pl-8" } else { "" };
                                rsx! {
                                    div { class: "flex items-start gap-3 px-4 py-2.5 {indent}",
                                        UserAvatar { name: comment.author.clone(), size: "size-6".to_string() }
                                        div { class: "flex-1 min-w-0",
                                            div { class: "flex items-center gap-2",
                                                span { class: "text-xs font-medium", "{comment.author}" }
                                                if let Some(ref at) = comment.created_at {
                                                    span { class: "text-[10px] text-muted-foreground", "{at}" }
                                                }
                                                if comment.resolved {
                                                    span { class: "text-[10px] text-muted-foreground", "✅" }
                                                }
                                            }
                                            p { class: "text-sm text-muted-foreground mt-0.5", "{comment.body}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── Projects Page ───────────────────────────────────────────────────────────

#[component]
fn ProjectsPage() -> Element {
    let base_url: Signal<String> = use_context();

    let projects = use_resource(move || {
        let base = base_url.read().clone();
        async move {
            if base.is_empty() {
                return Vec::new();
            }
            match rpc_call(&base, "list_active_projects", serde_json::json!({})).await {
                Ok(result) => serde_json::from_value::<Vec<ApiProject>>(result).unwrap_or_default(),
                Err(_) => Vec::new(),
            }
        }
    });

    let loading = projects.read().is_none();

    rsx! {
        div { class: "flex flex-col gap-6",
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold tracking-tight", "Projects" }
                if let Some(ref list) = *projects.read() {
                    span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                        "{list.len()}"
                    }
                }
            }

            if loading {
                div { class: "grid grid-cols-2 gap-4",
                    Skeleton { class: "h-32".to_string() }
                    Skeleton { class: "h-32".to_string() }
                }
            } else if projects.read().as_ref().map_or(true, |p| p.is_empty()) {
                EmptyState { message: "No active projects.".to_string() }
            } else {
                div { class: "grid grid-cols-1 lg:grid-cols-2 gap-4",
                    for project in projects.read().as_ref().unwrap().iter() {
                        ProjectCard { project: project.clone() }
                    }
                }
            }
        }
    }
}

#[component]
fn ProjectCard(project: ApiProject) -> Element {
    let title = project.title.clone();
    rsx! {
        Link {
            to: project_link_from_title(&title),
            class: "block hover:ring-2 hover:ring-ring/30 rounded-xl transition-all",
            Card {
                CardHeader {
                    div { class: "flex items-center justify-between",
                        div { class: "flex items-center gap-3",
                            ProjectIcon { title: project.title.clone(), hue: project.hue }
                            div {
                                CardTitle { "{project.title}" }
                                if let Some(ref area) = project.area {
                                    CardDescription { "{area}" }
                                }
                            }
                        }
                        if project.is_overdue {
                            span { class: "inline-flex items-center h-5 rounded-full bg-destructive/10 text-destructive px-2 text-xs font-medium",
                                "Overdue"
                            }
                        }
                    }
                }
                CardContent {
                    div { class: "flex items-center justify-between",
                        div { class: "flex items-center gap-4 text-xs text-muted-foreground",
                            if let Some(ref due) = project.due {
                                span { "Due: {due}" }
                            }
                            span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 font-medium",
                                "{project.status}"
                            }
                        }
                        if !project.team.is_empty() {
                            div { class: "flex -space-x-1.5",
                                for member in project.team.iter().take(4) {
                                    UserAvatar { name: member.clone(), size: "size-5".to_string() }
                                }
                                if project.team.len() > 4 {
                                    div { class: "size-5 rounded-full flex items-center justify-center bg-muted text-[9px] text-muted-foreground font-medium",
                                        { format!("+{}", project.team.len() - 4) }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── Project Detail Page ─────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct TasksByStatus {
    #[serde(default)]
    open: Vec<ApiTask>,
    #[serde(default)]
    in_progress: Vec<ApiTask>,
    #[serde(default)]
    planned: Vec<ApiTask>,
    #[serde(default)]
    on_hold: Vec<ApiTask>,
    #[serde(default)]
    done: Vec<ApiTask>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiSession {
    title: String,
    session_type: String,
    date: Option<String>,
    #[serde(default)]
    attendees: Vec<String>,
    location: Option<String>,
    body: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiTrack {
    title: String,
    key: Option<String>,
    tempo: Option<u32>,
    duration_seconds: Option<u32>,
    take_count: Option<u32>,
    best_take: Option<u32>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiInputChannel {
    channel: u32,
    source: String,
    mic: Option<String>,
    #[serde(default)]
    di: bool,
    #[serde(default)]
    phantom: bool,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ApiInputList {
    channel_count: Option<u32>,
    #[serde(default)]
    channels: Vec<ApiInputChannel>,
    monitor_type: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Default)]
struct WorkflowData {
    #[serde(default)]
    sessions: Vec<ApiSession>,
    #[serde(default)]
    tracks: Vec<ApiTrack>,
    total_duration_minutes: Option<u32>,
    input_list: Option<ApiInputList>,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ChildProject {
    project: ApiProject,
    total_tasks: usize,
    done_tasks: usize,
}

#[derive(Debug, Clone, Deserialize, PartialEq)]
struct ProjectDetail {
    project: ApiProject,
    total_tasks: usize,
    done_tasks: usize,
    tasks_by_status: TasksByStatus,
    all_tasks: Vec<ApiTask>,
    #[serde(default)]
    workflow: WorkflowData,
    #[serde(default)]
    children: Vec<ChildProject>,
    #[serde(default)]
    referenced_projects: Vec<ApiProject>,
}

#[component]
fn ProjectDetailPage(title: ReadSignal<String>) -> Element {
    let base_url: Signal<String> = use_context();

    let detail = use_resource(move || {
        let base = base_url.read().clone();
        let t = title();
        async move {
            if base.is_empty() || t.is_empty() {
                return None;
            }
            match rpc_call(&base, "project_detail", serde_json::json!({"title": t})).await {
                Ok(result) => serde_json::from_value::<ProjectDetail>(result).ok(),
                Err(_) => None,
            }
        }
    });

    // Fetch ancestry for the sidebar — writes to SIDEBAR_COLUMNS global
    let _ancestry = use_resource(move || {
        let base = base_url.read().clone();
        let t = title();
        async move {
            if base.is_empty() || t.is_empty() { return; }
            match rpc_call(&base, "project_ancestry", serde_json::json!({"title": t})).await {
                Ok(result) => {
                    if let Ok(columns) = serde_json::from_value::<Vec<AncestryColumn>>(result) {
                        *SIDEBAR_COLUMNS.write() = columns;
                    }
                }
                Err(_) => {}
            }
        }
    });

    // Single read of the resource
    let d = detail.read();
    let data = match d.as_ref() {
        None => {
            return rsx! {
                div { class: "flex flex-col gap-4",
                    Skeleton { class: "h-20".to_string() }
                    Skeleton { class: "h-60".to_string() }
                }
            };
        }
        Some(None) => {
            return rsx! {
                EmptyState { message: format!("Project \"{}\" not found.", title()) }
            };
        }
        Some(Some(data)) => data,
    };

    let progress = if data.total_tasks > 0 {
        (data.done_tasks as f64 / data.total_tasks as f64 * 100.0) as u32
    } else {
        0
    };

    let p = &data.project;
    let tbs = &data.tasks_by_status;

    rsx! {
        div { class: "flex flex-col gap-6",
            // Breadcrumb
            div { class: "flex items-center gap-1.5 text-xs text-muted-foreground",
                Link {
                    to: Route::ProjectsPage {},
                    class: "hover:text-foreground transition-colors",
                    "Projects"
                }
                if let Some(ref parent_title) = p.parent {
                    span { "/" }
                    Link {
                        to: project_link_from_title(parent_title),
                        class: "hover:text-foreground transition-colors",
                        "{parent_title}"
                    }
                }
                span { "/" }
                span { class: "text-foreground", "{p.title}" }
            }

            // Header
            div { class: "flex items-start justify-between",
                div { class: "flex items-center gap-4",
                    ProjectIcon { title: p.title.clone(), hue: p.hue, size: "size-12".to_string() }
                    div {
                        div { class: "flex items-center gap-2",
                            h1 { class: "text-xl font-semibold tracking-tight", "{p.title}" }
                            if let Some(ref pt) = p.project_type {
                                span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-[10px] font-medium",
                                    { workflow_label(pt) }
                                }
                            }
                        }
                        div { class: "flex items-center gap-3 mt-1 text-sm text-muted-foreground",
                            if let Some(ref area) = p.area {
                                span { "{area}" }
                            }
                            if let Some(ref due) = p.due {
                                span { "Due {due}" }
                            }
                            if p.is_overdue {
                                span { class: "text-destructive font-medium", "Overdue" }
                            }
                        }
                    }
                }
                if !p.team.is_empty() {
                    div { class: "flex -space-x-2",
                        for member in p.team.iter() {
                            UserAvatar { name: member.clone(), size: "size-8".to_string() }
                        }
                    }
                }
            }

            // Description
            if let Some(ref desc) = p.description {
                p { class: "text-sm text-muted-foreground", "{desc}" }
            }

            // Workflow-specific info panel
            WorkflowPanel { project: p.clone(), tasks: data.all_tasks.clone(), workflow: data.workflow.clone() }

            // Progress
            div { class: "flex items-center gap-4",
                div { class: "flex-1 h-2 rounded-full bg-secondary overflow-hidden",
                    div {
                        class: "h-full rounded-full bg-primary transition-all duration-500",
                        style: "width: {progress}%",
                    }
                }
                span { class: "text-sm text-muted-foreground tabular-nums",
                    { format!("{}/{} tasks · {}%", data.done_tasks, data.total_tasks, progress) }
                }
            }

            // Child projects (songs, stages, etc.)
            if !data.children.is_empty() {
                div { class: "rounded-xl border border-border bg-card overflow-hidden",
                    div { class: "flex items-center gap-2 px-4 py-2 border-b border-border",
                        span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider",
                            if p.project_type.as_deref() == Some("audio-production") { "Songs" }
                            else if p.project_type.as_deref() == Some("video-production") { "Sequences" }
                            else { "Sub-Projects" }
                        }
                        span { class: "text-xs text-muted-foreground",
                            { format!("{}", data.children.len()) }
                        }
                    }
                    div { class: "divide-y divide-border",
                        for child in data.children.iter() {
                            {
                                let child_progress = if child.total_tasks > 0 {
                                    (child.done_tasks as f64 / child.total_tasks as f64 * 100.0) as u32
                                } else { 0 };
                                let status_class = match child.project.status.as_str() {
                                    "active" => "bg-chart-2",
                                    "completed" => "bg-primary",
                                    "planning" => "bg-chart-4",
                                    _ => "bg-muted-foreground/30",
                                };
                                rsx! {
                                    Link {
                                        to: project_link(&child.project),
                                        class: "flex items-center gap-3 px-4 py-2.5 hover:bg-accent/30 transition-colors",
                                        span { class: "size-2 rounded-full {status_class} shrink-0" }
                                        div { class: "flex-1 min-w-0",
                                            { let name = display_name(&child.project); rsx! { span { class: "text-sm font-medium truncate block", "{name}" } } }
                                        }
                                        div { class: "flex items-center gap-2 shrink-0",
                                            div { class: "w-16 h-1.5 rounded-full bg-secondary overflow-hidden",
                                                div {
                                                    class: "h-full rounded-full bg-primary",
                                                    style: "width: {child_progress}%",
                                                }
                                            }
                                            span { class: "text-[10px] text-muted-foreground tabular-nums w-6 text-right",
                                                { format!("{child_progress}%") }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Referenced projects (linked, not children)
            if !data.referenced_projects.is_empty() {
                div { class: "rounded-xl border border-border bg-card overflow-hidden",
                    div { class: "flex items-center gap-2 px-4 py-2 border-b border-border",
                        svg {
                            class: "size-3.5 text-muted-foreground",
                            xmlns: "http://www.w3.org/2000/svg",
                            view_box: "0 0 24 24",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            path { d: "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" }
                            path { d: "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" }
                        }
                        span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider", "Referenced" }
                    }
                    div { class: "divide-y divide-border",
                        for ref_proj in data.referenced_projects.iter() {
                            Link {
                                to: project_link(ref_proj),
                                class: "flex items-center gap-3 px-4 py-2 hover:bg-accent/30 transition-colors",
                                ProjectIcon { title: ref_proj.title.clone(), hue: ref_proj.hue, size: "size-5".to_string() }
                                span { class: "text-sm flex-1 truncate",
                                    { display_name(ref_proj) }
                                }
                                if let Some(ref pt) = ref_proj.project_type {
                                    span { class: "text-[10px] text-muted-foreground",
                                        { workflow_label(pt) }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Status columns — show non-empty groups
            div { class: "grid grid-cols-1 lg:grid-cols-2 gap-4",
                if !tbs.in_progress.is_empty() {
                    StatusGroup { label: "In Progress", tasks: tbs.in_progress.clone(), accent: "bg-chart-2" }
                }
                if !tbs.open.is_empty() {
                    StatusGroup { label: "Open", tasks: tbs.open.clone(), accent: "bg-chart-1" }
                }
                if !tbs.planned.is_empty() {
                    StatusGroup { label: "Planned", tasks: tbs.planned.clone(), accent: "bg-chart-4" }
                }
                if !tbs.on_hold.is_empty() {
                    StatusGroup { label: "On Hold", tasks: tbs.on_hold.clone(), accent: "bg-muted-foreground" }
                }
            }

            // Completed tasks (collapsed)
            if !tbs.done.is_empty() {
                div { class: "mt-2",
                    details { class: "group",
                        summary { class: "cursor-pointer text-sm text-muted-foreground hover:text-foreground transition-colors select-none",
                            { format!("{} completed tasks", tbs.done.len()) }
                        }
                        div { class: "mt-3 rounded-xl border border-border bg-card overflow-hidden",
                            div { class: "divide-y divide-border",
                                for task in tbs.done.iter() {
                                    div { class: "flex items-center gap-3 px-4 py-2 opacity-40",
                                        svg {
                                            class: "size-3.5 text-primary shrink-0",
                                            xmlns: "http://www.w3.org/2000/svg",
                                            view_box: "0 0 24 24",
                                            fill: "none",
                                            stroke: "currentColor",
                                            stroke_width: "2.5",
                                            path { d: "M20 6 9 17l-5-5" }
                                        }
                                        span { class: "text-sm line-through text-muted-foreground", "{task.title}" }
                                        if let Some(ref assignee) = task.assignee {
                                            div { class: "ml-auto",
                                                UserAvatar { name: assignee.clone(), size: "size-5".to_string() }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Extension-based workflow panel — dispatches to the registered workflow
/// extension for this project type, rendering stats + named panels.
#[component]
fn WorkflowPanel(project: ApiProject, tasks: Vec<ApiTask>, #[props(default)] workflow: WorkflowData) -> Element {
    let pt = project.project_type.as_deref().unwrap_or("generic");
    let ext = workflows::resolve(pt);

    let Some(ext) = ext else {
        return rsx! {};
    };

    let ctx = workflows::WorkflowContext {
        project: project.clone(),
        tasks,
        workflow,
    };

    let panels = ext.panels();

    rsx! {
        // Stats summary
        { ext.stats_panel(&ctx) }

        // Named panels — each rendered in a card, collapsible if flagged
        for panel in panels.iter() {
            if panel.collapsed {
                details { class: "group",
                    summary { class: "cursor-pointer text-sm text-muted-foreground hover:text-foreground transition-colors select-none",
                        "{panel.label}"
                    }
                    div { class: "mt-2 rounded-xl border border-border bg-card overflow-hidden",
                        { (panel.render)(&ctx) }
                    }
                }
            } else {
                div { class: "rounded-xl border border-border bg-card overflow-hidden",
                    div { class: "flex items-center gap-2 px-4 py-2 border-b border-border",
                        span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider", "{panel.label}" }
                    }
                    { (panel.render)(&ctx) }
                }
            }
        }
    }
}

fn workflow_label(project_type: &str) -> &'static str {
    match project_type {
        "album" => "Album",
        "ep" => "EP",
        "lp" => "LP",
        "single" => "Single",
        "song" => "Song",
        "mixtape" => "Mixtape",
        "soundtrack" => "Soundtrack",
        "podcast" => "Podcast",
        "audio-production" => "Audio Production",
        "video-production" => "Video Production",
        "music-video" => "Music Video",
        "documentary" => "Documentary",
        "short-film" => "Short Film",
        "livestream" => "Livestream",
        "promo" => "Promo",
        "performance-event" => "Performance Event",
        "concert" => "Concert",
        "festival" => "Festival",
        "rehearsal" => "Rehearsal",
        "showcase" => "Showcase",
        "code-repository" => "Code Repository",
        "software" => "Software",
        "website" => "Website",
        "app" => "App",
        "plugin" => "Plugin",
        "stage" => "Stage",
        _ => "Project",
    }
}

#[component]
fn StatusGroup(label: &'static str, tasks: Vec<ApiTask>, accent: &'static str) -> Element {
    rsx! {
        div { class: "rounded-xl border border-border bg-card overflow-hidden",
            // Group header with accent bar
            div { class: "flex items-center gap-2 px-4 py-2 border-b border-border",
                span { class: "size-2 rounded-full {accent}" }
                span { class: "text-xs font-medium text-muted-foreground uppercase tracking-wider", "{label}" }
                span { class: "text-xs text-muted-foreground", { format!("{}", tasks.len()) } }
            }
            // Tasks
            div { class: "divide-y divide-border",
                for task in tasks.iter() {
                    div { class: "px-4 py-2.5",
                        div { class: "flex items-center justify-between",
                            span { class: "text-sm font-medium", "{task.title}" }
                            div { class: "flex items-center gap-2 shrink-0",
                                if let Some(ref due) = task.due {
                                    span { class: "text-[11px] text-muted-foreground", "{due}" }
                                }
                                if let Some(ref assignee) = task.assignee {
                                    UserAvatar { name: assignee.clone(), size: "size-5".to_string() }
                                }
                            }
                        }
                        // Show next subtask if there is one
                        if let Some(ref body) = task.body {
                            if let Some(st) = next_subtask(body) {
                                p { class: "text-xs text-muted-foreground mt-0.5", "→ {st}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── Activity Page ───────────────────────────────────────────────────────────

#[component]
fn ActivityPage() -> Element {
    let base_url: Signal<String> = use_context();

    let changes = use_resource(move || {
        let base = base_url.read().clone();
        async move {
            if base.is_empty() {
                return Vec::new();
            }
            match rpc_call(&base, "activity", serde_json::json!({"limit": 50})).await {
                Ok(result) => serde_json::from_value::<Vec<ApiChange>>(result).unwrap_or_default(),
                Err(_) => Vec::new(),
            }
        }
    });

    let loading = changes.read().is_none();

    rsx! {
        div { class: "flex flex-col gap-6",
            h2 { class: "text-lg font-semibold tracking-tight", "Activity" }

            if loading {
                div { class: "flex flex-col gap-2",
                    SkeletonText {}
                    SkeletonText {}
                    SkeletonText {}
                }
            } else if changes.read().as_ref().map_or(true, |c| c.is_empty()) {
                EmptyState { message: "No activity yet.".to_string() }
            } else {
                div { class: "flex flex-col gap-2",
                    for change in changes.read().as_ref().unwrap().iter() {
                        div { class: "flex items-center gap-3 px-3 py-2 rounded-xl border border-border bg-card/50 text-sm",
                            div { class: "flex-1",
                                span { class: "font-medium", "{change.entity_id}" }
                                if let Some(ref field) = change.field {
                                    span { class: "text-muted-foreground", " · {field}" }
                                    if let Some(ref new_val) = change.new_value {
                                        span { class: "text-muted-foreground", " → {new_val}" }
                                    }
                                }
                            }
                            if let Some(ref by) = change.changed_by {
                                span { class: "text-xs text-muted-foreground", "{by}" }
                            }
                            span { class: "text-xs text-muted-foreground", "{change.changed_at}" }
                        }
                    }
                }
            }
        }
    }
}
