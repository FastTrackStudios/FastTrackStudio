//! Task — mobile app.

use std::sync::Arc;

use dioxus::prelude::*;
use task_core::{parse_capture, Project, Task, VaultServiceImpl};
use task_ui::{InboxView, ProjectDashboard, TodayView, UpcomingView};

#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[layout(Shell)]
    #[route("/")]             Today {},
    #[route("/inbox")]        Inbox {},
    #[route("/upcoming")]     Upcoming {},
    #[route("/projects")]     Projects {},
    #[route("/tasks/new")]    NewTask {},
    #[route("/tasks/:title")] TaskDetail { title: String },
}

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    let vault_path = std::env::var("TASK_VAULT").ok();

    match vault_path {
        None => rsx! {
            div { class: "setup",
                h2 { "No vault configured" }
                p { "Set " code { "TASK_VAULT" } " to your vault path." }
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
                    // Initial load
                    *tasks.write() = svc_bg.list_tasks().await;
                    *projects.write() = svc_bg.list_projects().await;

                    // Watch for vault changes and reload
                    let _handle = svc_bg.watch().ok();
                    let mut rx = svc_bg.subscribe();
                    loop {
                        if rx.changed().await.is_err() {
                            break;
                        }
                        *tasks.write() = svc_bg.list_tasks().await;
                        *projects.write() = svc_bg.list_projects().await;
                    }
                }
            });

            rsx! { Router::<Route> {} }
        }
    }
}

// r[impl views.today]
#[component]
fn Shell() -> Element {
    rsx! {
        div { class: "shell",
            div { class: "shell__content", Outlet::<Route> {} }
            nav { class: "shell__tab-bar",
                Link { to: Route::Today {}, "Today" }
                Link { to: Route::Inbox {}, "Inbox" }
                Link { to: Route::Upcoming {}, "Upcoming" }
                Link { to: Route::Projects {}, "Projects" }
                Link { to: Route::NewTask {}, "+" }
            }
        }
    }
}

// r[impl views.today]
#[component]
fn Today() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();
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
                nav.push(Route::TaskDetail { title });
            },
        }
    }
}

// r[impl views.inbox]
#[component]
fn Inbox() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();
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
                nav.push(Route::TaskDetail { title });
            },
        }
    }
}

// r[impl views.upcoming]
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

// r[impl project.dashboard]
#[component]
fn Projects() -> Element {
    let tasks: Signal<Vec<Task>> = use_context();
    let projects: Signal<Vec<Project>> = use_context();
    rsx! {
        ProjectDashboard {
            tasks: tasks.read().clone(),
            projects: projects.read().clone(),
        }
    }
}

// r[impl capture.quick-add]
#[component]
fn NewTask() -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let mut input = use_signal(String::new);
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();
    rsx! {
        div { class: "new-task",
            h2 { "New Task" }
            input {
                placeholder: "Buy groceries tomorrow !high #errands",
                autofocus: true,
                value: "{input}",
                oninput: move |e| *input.write() = e.value(),
            }
            button {
                onclick: move |_| {
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
                "Add"
            }
        }
    }
}

// r[impl task.id]
#[component]
fn TaskDetail(title: String) -> Element {
    let mut tasks: Signal<Vec<Task>> = use_context();
    let svc = use_context::<Arc<VaultServiceImpl>>();
    let nav = use_navigator();
    let task = tasks.read().iter().find(|t| t.title == title).cloned();

    match task {
        None => rsx! { div { "Task not found." } },
        Some(t) => rsx! {
            div { class: "task-detail",
                button { onclick: move |_| nav.go_back(), "← Back" }
                h1 { "{t.title}" }
                p { "Status: " span { "{t.status:?}" } }
                p { "Priority: " span { "{t.priority:?}" } }
                if let Some(due) = t.due {
                    p { "Due: {due}" }
                }
                if let Some(scheduled) = t.scheduled {
                    p { "Scheduled: {scheduled}" }
                }
                if !t.projects.is_empty() {
                    p { "Projects: " for p in &t.projects { span { "{p.0} " } } }
                }
                if !t.contexts.is_empty() {
                    p { "Contexts: " for c in &t.contexts { span { "@{c} " } } }
                }
                if let Some(est) = t.time_estimate {
                    p { "Estimate: {est} min" }
                }
                if !t.is_complete() {
                    button {
                        onclick: move |_| {
                            let title = t.title.clone();
                            let svc = svc.clone();
                            spawn(async move {
                                if svc.complete_task(title).await.is_ok() {
                                    *tasks.write() = svc.list_tasks().await;
                                }
                            });
                            nav.go_back();
                        },
                        "Mark Complete"
                    }
                }
            }
        },
    }
}
