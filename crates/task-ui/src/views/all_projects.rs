use dioxus::prelude::*;

use crate::data::{active_projects, Project, Task, TaskStatus};

#[component]
pub fn AllProjects() -> Element {
    let projects = active_projects();
    let project_count = projects.len();
    let open_tasks: usize = projects.iter().map(|p| p.open_count()).sum();
    let done_tasks: usize = projects.iter().map(|p| p.done_count()).sum();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-8 p-6 lg:p-10",
            section { class: "flex flex-col gap-6 lg:flex-row lg:items-end lg:justify-between",
                div { class: "space-y-2",
                    p { class: "text-sm font-semibold uppercase tracking-[0.3em] text-cyan-300", "All projects" }
                    h1 { class: "text-4xl font-black tracking-tight text-white", "Every project, every task" }
                    p { class: "max-w-2xl text-slate-400", "One screen for the full state of work. Each project shows its full task list grouped by status." }
                }
                div { class: "flex flex-wrap gap-3 text-center",
                    Stat { label: "Projects", value: "{project_count}" }
                    Stat { label: "Open", value: "{open_tasks}" }
                    Stat { label: "Done", value: "{done_tasks}" }
                }
            }

            section { class: "flex flex-col gap-6",
                for project in projects {
                    ProjectSection { key: "{project.id}", project }
                }
            }
        }
    }
}

#[component]
fn ProjectSection(project: Project) -> Element {
    let open = project.open_count();
    let done = project.done_count();

    rsx! {
        article { class: "rounded-2xl border border-slate-800 bg-slate-900/70 p-6 shadow-lg shadow-black/20",
            header { class: "mb-5 flex flex-col gap-3 border-b border-slate-800 pb-4 sm:flex-row sm:items-end sm:justify-between",
                div { class: "space-y-1",
                    h2 { class: "text-2xl font-bold text-white", "{project.name}" }
                    p { class: "text-sm text-slate-400", "{project.summary}" }
                }
                div { class: "flex gap-2 text-xs font-semibold uppercase tracking-[0.2em] text-slate-400",
                    span { class: "rounded-full border border-slate-700 px-3 py-1", "{open} open" }
                    span { class: "rounded-full border border-slate-700 px-3 py-1", "{done} done" }
                }
            }
            if project.tasks.is_empty() {
                p { class: "rounded-xl bg-slate-950/70 p-4 text-sm text-slate-500", "No tasks yet." }
            } else {
                ul { class: "space-y-2",
                    for task in &project.tasks {
                        TaskRow { key: "{task.id}", task: task.clone() }
                    }
                }
            }
        }
    }
}

#[component]
fn TaskRow(task: Task) -> Element {
    rsx! {
        li { class: "flex items-start gap-4 rounded-xl bg-slate-950/60 p-3 hover:bg-slate-950",
            span {
                class: "mt-0.5 shrink-0 rounded-full border px-2 py-0.5 text-[10px] font-semibold uppercase tracking-widest {task.status.badge_class()}",
                "{task.status.label()}"
            }
            div { class: "flex flex-col",
                span {
                    class: if task.status == TaskStatus::Done {
                        "text-sm text-slate-400 line-through"
                    } else {
                        "text-sm text-slate-100"
                    },
                    "{task.title}"
                }
                if !task.note.is_empty() {
                    span { class: "text-xs text-slate-500", "{task.note}" }
                }
            }
        }
    }
}

#[component]
fn Stat(label: &'static str, value: String) -> Element {
    rsx! {
        div { class: "rounded-2xl border border-slate-800 bg-slate-950/70 px-5 py-3",
            div { class: "text-2xl font-black text-white", "{value}" }
            div { class: "text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "{label}" }
        }
    }
}
