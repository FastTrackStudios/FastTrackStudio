//! Task — desktop app.

use std::sync::Arc;

#[cfg(feature = "dev")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use vault_core::{parse_capture, Priority, Project, Status, Task, VaultServiceImpl};
use task_ui::{InboxView, ProjectDashboard, TaskCard, TaskList, TodayView, UpcomingView};

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[layout(Shell)]
    #[route("/")]                    Today {},
    #[route("/inbox")]               Inbox {},
    #[route("/upcoming")]            Upcoming {},
    #[route("/projects")]            Dashboard {},
    #[route("/tasks")]               Tasks {},
    #[route("/tasks/new")]           NewTask {},
    #[route("/projects/:title")]     ProjectDetail { title: String },
    #[route("/showcase")]            ShowcasePage {},
}

#[cfg(feature = "dev")]
fn main() {
    if std::env::var("WEBKIT_DISABLE_COMPOSITING_MODE").is_err() {
        unsafe { std::env::set_var("WEBKIT_DISABLE_COMPOSITING_MODE", "1"); }
    }
    unsafe { std::env::set_var("GTK_THEME", "Adwaita:dark"); }

    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    let cfg = Config::new()
        .with_window(
            WindowBuilder::new()
                .with_title("Task")
                .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1200.0, 800.0)),
        )
        .with_menu(None);

    LaunchBuilder::desktop().with_cfg(cfg).launch(App);
}

#[component]
fn App() -> Element {
    let vault_path = std::env::var("TASK_VAULT").ok();

    match vault_path {
        None => rsx! {
            document::Stylesheet { href: asset!("/assets/tailwind.css") }
            div { class: "min-h-screen bg-background flex items-center justify-center",
                Card {
                    CardHeader {
                        CardTitle { "No vault configured" }
                        CardDescription {
                            "Set the "
                            Kbd { "TASK_VAULT" }
                            " environment variable to your vault path."
                        }
                    }
                }
            }
        },
        Some(path) => {
            let svc = Arc::new(VaultServiceImpl::new(&path));
            use_context_provider(|| svc.clone());

            let mut tasks = use_context_provider(|| Signal::new(Vec::<Task>::new()));
            let mut projects = use_context_provider(|| Signal::new(Vec::<Project>::new()));

            let svc_bg = svc.clone();
            use_coroutine(move |_: UnboundedReceiver<()>| {
                let svc_bg = svc_bg.clone();
                async move {
                    *tasks.write() = svc_bg.list_tasks().await;
                    *projects.write() = svc_bg.list_projects().await;

                    let _handle = svc_bg.watch().ok();
                    let mut rx = svc_bg.subscribe();
                    loop {
                        if rx.changed().await.is_err() { break; }
                        *tasks.write() = svc_bg.list_tasks().await;
                        *projects.write() = svc_bg.list_projects().await;
                    }
                }
            });

            rsx! {
                document::Stylesheet { href: asset!("/assets/tailwind.css") }
                Router::<Route> {}
            }
        }
    }
}

// ── Shell ────────────────────────────────────────────────────────────────────

#[component]
fn Shell() -> Element {
    let tasks: Signal<Vec<Task>> = use_context();

    // Selected task signal for the detail Sheet overlay.
    let mut selected_task: Signal<Option<String>> = use_context_provider(|| Signal::new(Option::<String>::None));

    // Compute counts for sidebar badges.
    let today = chrono::Local::now().date_naive();
    let today_count = tasks.read().iter().filter(|t| {
        t.has_started() && !t.is_complete()
            && t.status != Status::Cancelled && t.status != Status::Archived
            && (t.due.map(|d| d == today).unwrap_or(false)
                || t.scheduled.map(|d| d == today).unwrap_or(false)
                || t.is_overdue()
                || t.status == Status::InProgress)
    }).count();

    let inbox_count = tasks.read().iter().filter(|t| {
        t.has_started() && t.projects.is_empty() && t.due.is_none() && t.scheduled.is_none()
            && matches!(t.status, Status::None | Status::Open | Status::InProgress)
    }).count();

    // Look up the currently selected task for the Sheet.
    let current_task: Option<Task> = {
        let sel = selected_task.read();
        match sel.as_ref() {
            Some(title) => tasks.read().iter().find(|t| t.title == *title).cloned(),
            None => None,
        }
    };

    rsx! {
        SidebarProvider {
            Sidebar { side: SidebarSide::Left,
                SidebarHeader {
                    div { class: "flex items-center gap-2 px-1",
                        div { class: "flex size-7 items-center justify-center rounded-lg bg-primary text-primary-foreground text-xs font-bold",
                            "T"
                        }
                        span { class: "text-sm font-semibold tracking-tight", "Task" }
                    }
                }

                SidebarContent {
                    SidebarGroup {
                        SidebarGroupLabel { "Views" }
                        SidebarGroupContent {
                            SidebarMenu {
                                SidebarNavItem { to: Route::Today {}, label: "Today", count: today_count }
                                SidebarNavItem { to: Route::Inbox {}, label: "Inbox", count: inbox_count }
                                SidebarNavItem { to: Route::Upcoming {}, label: "Upcoming", count: 0 }
                            }
                        }
                    }

                    SidebarSeparator {}

                    SidebarGroup {
                        SidebarGroupLabel { "Manage" }
                        SidebarGroupContent {
                            SidebarMenu {
                                SidebarNavItem { to: Route::Dashboard {}, label: "Projects", count: 0 }
                                SidebarNavItem { to: Route::Tasks {}, label: "All Tasks", count: 0 }
                                SidebarNavItem { to: Route::ShowcasePage {}, label: "Showcase", count: 0 }
                            }
                        }
                    }
                }

                SidebarFooter {
                    Link { to: Route::NewTask {},
                        Button {
                            variant: ButtonVariant::Primary,
                            class: "w-full".to_string(),
                            "+ New Task"
                        }
                    }
                }
            }

            SidebarInset {
                main { class: "flex-1 overflow-y-auto p-8",
                    div { class: "max-w-4xl mx-auto",
                        Outlet::<Route> {}
                    }
                }
            }
        }

        // ── Task Detail Sheet (slide-over) ──────────────────────────────────
        TaskDetailSheet { task: current_task, selected_task: selected_task }
    }
}

#[derive(Props, Clone, PartialEq)]
struct SidebarNavItemProps {
    to: Route,
    label: &'static str,
    count: usize,
}

#[component]
fn SidebarNavItem(props: SidebarNavItemProps) -> Element {
    rsx! {
        SidebarMenuItem {
            Link { to: props.to,
                SidebarMenuButton {
                    "{props.label}"
                }
            }
            if props.count > 0 {
                SidebarMenuBadge { "{props.count}" }
            }
        }
    }
}

// ── Task Detail Sheet ───────────────────────────────────────────────────────

#[component]
fn TaskDetailSheet(task: Option<Task>, mut selected_task: Signal<Option<String>>) -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();

    let is_open = task.is_some();

    match task {
        None => rsx! {
            Sheet { open: false, side: SheetSide::Right }
        },
        Some(t) => {
            let status_badge = match t.status {
                Status::InProgress => StatusBadgeVariant::Success,
                Status::Done => StatusBadgeVariant::Success,
                Status::OnHold => StatusBadgeVariant::Warning,
                Status::Cancelled => StatusBadgeVariant::Danger,
                _ => StatusBadgeVariant::Neutral,
            };

            rsx! {
                Sheet {
                    open: is_open,
                    on_close: move |_| selected_task.set(None),
                    side: SheetSide::Right,

                    SheetHeader {
                        div { class: "flex items-center justify-between",
                            SheetTitle { "{t.title}" }
                            StatusBadge { variant: status_badge, label: format!("{:?}", t.status) }
                        }
                    }

                    if t.is_overdue() {
                        Alert { variant: AlertVariant::Destructive,
                            AlertTitle { "Overdue" }
                            AlertDescription { "This task is past its due date." }
                        }
                    }

                    div { class: "grid grid-cols-2 gap-x-6 gap-y-3 mt-4",
                        KeyValueRow { label: "Priority".to_string(), value: format!("{:?}", t.priority) }
                        if let Some(due) = t.due {
                            KeyValueRow { label: "Due".to_string(), value: due.to_string() }
                        }
                        if let Some(scheduled) = t.scheduled {
                            KeyValueRow { label: "Scheduled".to_string(), value: scheduled.to_string() }
                        }
                        if let Some(est) = t.time_estimate {
                            KeyValueRow { label: "Estimate".to_string(), value: format!("{est} min") }
                        }
                        KeyValueRow { label: "Urgency".to_string(), value: t.urgency_score().to_string(), bold: true }
                    }

                    if !t.projects.is_empty() || !t.tags.is_empty() || !t.contexts.is_empty() {
                        Divider { class: "my-4".to_string() }
                        div { class: "flex flex-wrap gap-2",
                            for p in &t.projects {
                                Badge { variant: BadgeVariant::Default, "{p.0}" }
                            }
                            for tag in &t.tags {
                                Badge { variant: BadgeVariant::Secondary, "#{tag}" }
                            }
                            for ctx in &t.contexts {
                                Badge { variant: BadgeVariant::Outline, "@{ctx}" }
                            }
                        }
                    }

                    if !t.is_complete() {
                        div { class: "mt-6",
                            Button {
                                variant: ButtonVariant::Primary,
                                on_click: {
                                    let title = t.title.clone();
                                    move |_| {
                                        let title = title.clone();
                                        let svc = svc.clone();
                                        spawn(async move {
                                            if svc.complete_task(title).await.is_ok() {
                                                *tasks.write() = svc.list_tasks().await;
                                            }
                                        });
                                        selected_task.set(None);
                                    }
                                },
                                "Mark Complete"
                            }
                        }
                    }
                }
            }
        },
    }
}

// ── Page Components ──────────────────────────────────────────────────────────

#[component]
fn Today() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let mut selected_task: Signal<Option<String>> = use_context();
    rsx! {
        TodayView {
            tasks: tasks.read().clone(),
            on_complete: move |title: String| {
                let svc = svc.clone();
                spawn(async move {
                    if svc.complete_task(title).await.is_ok() {
                        *tasks.write() = svc.list_tasks().await;
                    }
                });
            },
            on_tap: move |title: String| {
                selected_task.set(Some(title));
            },
        }
    }
}

#[component]
fn Inbox() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let mut selected_task: Signal<Option<String>> = use_context();
    rsx! {
        InboxView {
            tasks: tasks.read().clone(),
            on_complete: move |title: String| {
                let svc = svc.clone();
                spawn(async move {
                    if svc.complete_task(title).await.is_ok() {
                        *tasks.write() = svc.list_tasks().await;
                    }
                });
            },
            on_tap: move |title: String| {
                selected_task.set(Some(title));
            },
        }
    }
}

#[component]
fn Upcoming() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    rsx! {
        UpcomingView {
            tasks: tasks.read().clone(),
            on_complete: move |title: String| {
                let svc = svc.clone();
                spawn(async move {
                    if svc.complete_task(title).await.is_ok() {
                        *tasks.write() = svc.list_tasks().await;
                    }
                });
            },
        }
    }
}

#[component]
fn Dashboard() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let projects: Signal<Vec<Project>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();
    rsx! {
        ProjectDashboard {
            tasks: tasks.read().clone(),
            projects: projects.read().clone(),
            on_project_tap: move |title: String| {
                nav.push(Route::ProjectDetail { title });
            },
            on_complete_task: move |title: String| {
                let svc = svc.clone();
                spawn(async move {
                    if svc.complete_task(title).await.is_ok() {
                        *tasks.write() = svc.list_tasks().await;
                    }
                });
            },
        }
    }
}

#[component]
fn Tasks() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let mut selected_task: Signal<Option<String>> = use_context();
    let search = use_signal(String::new);
    let status_filter = use_signal(String::new);
    let priority_filter = use_signal(String::new);

    let filtered: Vec<Task> = tasks
        .read()
        .iter()
        .filter(|t| {
            let q = search.read().to_lowercase();
            let text_ok = q.is_empty() || t.title.to_lowercase().contains(&q);
            let sf = status_filter.read();
            let status_ok = sf.is_empty() || match sf.as_str() {
                "open" => t.status == Status::Open || t.status == Status::None,
                "in-progress" => t.status == Status::InProgress,
                "on-hold" => t.status == Status::OnHold,
                "done" => t.status == Status::Done,
                _ => true,
            };
            let pf = priority_filter.read();
            let prio_ok = pf.is_empty() || match pf.as_str() {
                "urgent" => t.priority == Priority::Urgent,
                "high" => t.priority == Priority::High,
                "normal" => t.priority == Priority::Normal,
                "low" => t.priority == Priority::Low,
                _ => true,
            };
            text_ok && status_ok && prio_ok
        })
        .cloned()
        .collect();

    let mut sorted = filtered;
    sorted.sort_by_key(|t| std::cmp::Reverse(t.urgency_score()));

    rsx! {
        div { class: "flex flex-col gap-6",
            // Header
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold tracking-tight", "All Tasks" }
                span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                    "{sorted.len()} tasks"
                }
            }

            // Filters bar
            div { class: "flex items-center gap-3",
                div { class: "flex-1",
                    Input {
                        value: search,
                        placeholder: "Search tasks...".to_string(),
                        size: InputSize::Small,
                    }
                }
                Select {
                    value: status_filter,
                    placeholder: "All statuses".to_string(),
                    SelectContent {
                        SelectItem { value: "".to_string(), "All statuses" }
                        SelectSeparator {}
                        SelectItem { value: "open".to_string(), "Open" }
                        SelectItem { value: "in-progress".to_string(), "In Progress" }
                        SelectItem { value: "on-hold".to_string(), "On Hold" }
                        SelectItem { value: "done".to_string(), "Done" }
                    }
                }
                Select {
                    value: priority_filter,
                    placeholder: "All priorities".to_string(),
                    SelectContent {
                        SelectItem { value: "".to_string(), "All priorities" }
                        SelectSeparator {}
                        SelectItem { value: "urgent".to_string(), "Urgent" }
                        SelectItem { value: "high".to_string(), "High" }
                        SelectItem { value: "normal".to_string(), "Normal" }
                        SelectItem { value: "low".to_string(), "Low" }
                    }
                }
            }

            // Task list
            div { class: "rounded-2xl ring-1 ring-foreground/10 bg-card text-card-foreground overflow-hidden",
                if sorted.is_empty() {
                    div { class: "flex flex-col items-center justify-center gap-2 p-8 text-sm text-muted-foreground",
                        "No tasks match your filters."
                    }
                } else {
                    div { class: "divide-y divide-border",
                        for task in &sorted {
                            {
                                let tc = task.title.clone();
                                let tt = task.title.clone();
                                let svc = svc.clone();
                                rsx! {
                                    TaskCard {
                                        key: "{task.title}",
                                        task: task.clone(),
                                        on_complete: move |_| {
                                            let title = tc.clone();
                                            let svc = svc.clone();
                                            spawn(async move {
                                                if svc.complete_task(title).await.is_ok() {
                                                    *tasks.write() = svc.list_tasks().await;
                                                }
                                            });
                                        },
                                        on_tap: move |_| {
                                            selected_task.set(Some(tt.clone()));
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
}

#[component]
fn NewTask() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let input = use_signal(String::new);
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();

    // Live preview of parsed capture.
    let preview = {
        let raw = input.read();
        if raw.trim().is_empty() {
            None
        } else {
            Some(parse_capture(&raw))
        }
    };

    rsx! {
        VStack { gap: "5".to_string(),
            Breadcrumb {
                BreadcrumbList {
                    BreadcrumbItem { span { class: "hover:text-foreground transition-colors cursor-pointer text-muted-foreground", onclick: move |_| nav.go_back(), "Tasks" } }
                    BreadcrumbSeparator {}
                    BreadcrumbItem { BreadcrumbPage { "New Task" } }
                }
            }

            Card {
                CardHeader {
                    CardTitle { "Create a new task" }
                    CardDescription { "Use natural language — e.g. \"Buy groceries tomorrow !high #errands\"" }
                }
                CardContent {
                    Field {
                        FieldLabel { "Task" }
                        Input {
                            value: input,
                            placeholder: "What needs to be done?".to_string(),
                        }
                        FieldDescription { "Supports #tags, @contexts, [[Projects]], !priority, and date expressions." }
                    }

                    // Live preview
                    if let Some(ref captured) = preview {
                        div { class: "mt-4 rounded-md border border-border p-3 text-sm space-y-1",
                            p { class: "font-medium", "Preview" }
                            div { class: "grid grid-cols-2 gap-x-4 gap-y-1 text-muted-foreground",
                                span { "Title" }
                                span { class: "text-foreground", "{captured.title}" }
                                if let Some(ref prio) = captured.priority {
                                    span { "Priority" }
                                    span { class: "text-foreground", "{prio:?}" }
                                }
                                if let Some(due) = captured.due {
                                    span { "Due" }
                                    span { class: "text-foreground", "{due}" }
                                }
                            }
                            if !captured.tags.is_empty() || !captured.contexts.is_empty() || !captured.projects.is_empty() {
                                div { class: "flex flex-wrap gap-1 mt-2",
                                    for p in &captured.projects {
                                        Badge { variant: BadgeVariant::Default, "{p.0}" }
                                    }
                                    for tag in &captured.tags {
                                        Badge { variant: BadgeVariant::Secondary, "#{tag}" }
                                    }
                                    for ctx in &captured.contexts {
                                        Badge { variant: BadgeVariant::Outline, "@{ctx}" }
                                    }
                                }
                            }
                        }
                    }
                }
                CardFooter {
                    HStack { gap: "2".to_string(),
                        Button {
                            variant: ButtonVariant::Primary,
                            on_click: move |_| {
                                let captured = parse_capture(&input.read());
                                if captured.title.is_empty() { return; }
                                let task = Task {
                                    title: captured.title,
                                    priority: captured.priority.unwrap_or_default(),
                                    due: captured.due,
                                    tags: captured.tags,
                                    contexts: captured.contexts,
                                    projects: captured.projects,
                                    ..Default::default()
                                };
                                let svc = svc.clone();
                                spawn(async move {
                                    if svc.create_task(task).await.is_ok() {
                                        *tasks.write() = svc.list_tasks().await;
                                    }
                                });
                                nav.go_back();
                            },
                            "Create Task"
                        }
                        Button {
                            variant: ButtonVariant::Ghost,
                            on_click: move |_| nav.go_back(),
                            "Cancel"
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn ShowcasePage() -> Element {
    rsx! { fts_ui::showcase::Showcase {} }
}

#[component]
fn ProjectDetail(title: String) -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let mut selected_task: Signal<Option<String>> = use_context();
    let nav = use_navigator();

    let project_tasks: Vec<Task> = tasks
        .read()
        .iter()
        .filter(|t| t.projects.iter().any(|p| p.0 == title))
        .cloned()
        .collect();

    let completed = project_tasks.iter().filter(|t| t.is_complete()).count();
    let total = project_tasks.len();

    rsx! {
        VStack { gap: "5".to_string(),
            Breadcrumb {
                BreadcrumbList {
                    BreadcrumbItem { span { class: "hover:text-foreground transition-colors cursor-pointer text-muted-foreground", onclick: move |_| nav.go_back(), "Projects" } }
                    BreadcrumbSeparator {}
                    BreadcrumbItem { BreadcrumbPage { "{title}" } }
                }
            }

            div { class: "flex items-center justify-between",
                Heading { level: HeadingLevel::H3, "{title}" }
                Badge { variant: BadgeVariant::Secondary, "{completed}/{total} complete" }
            }

            TaskList {
                tasks: project_tasks,
                on_complete: move |task_title: String| {
                    let svc = svc.clone();
                    spawn(async move {
                        if svc.complete_task(task_title).await.is_ok() {
                            *tasks.write() = svc.list_tasks().await;
                        }
                    });
                },
                on_tap: move |title: String| {
                    selected_task.set(Some(title));
                },
            }
        }
    }
}
