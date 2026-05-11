use dioxus::prelude::*;

use crate::data::active_projects;

#[component]
pub fn ProjectOverview() -> Element {
    let projects = active_projects();
    let project_count = projects.len();
    let task_count: usize = projects.iter().map(|project| project.tasks.len()).sum();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-8 p-6 lg:p-10",
            section { class: "rounded-3xl border border-slate-800 bg-slate-900/70 p-6 shadow-2xl shadow-black/30",
                div { class: "flex flex-col gap-6 lg:flex-row lg:items-end lg:justify-between",
                    div { class: "space-y-3",
                        p { class: "text-sm font-semibold uppercase tracking-[0.3em] text-cyan-300", "Task web baseline" }
                        h1 { class: "max-w-3xl text-4xl font-black tracking-tight text-white lg:text-5xl", "Projects, agent work, and operational status in one shell" }
                        p { class: "max-w-2xl text-slate-300", "This first Dioxus web slice proves the app shell, navigation, Tailwind asset pipeline, and deployable static bundle without touching production Task data." }
                    }
                    div { class: "grid grid-cols-2 gap-3 text-center",
                        StatCard { label: "Projects", value: "{project_count}" }
                        StatCard { label: "Tasks", value: "{task_count}" }
                    }
                }
            }

            section { class: "grid gap-4 md:grid-cols-2 xl:grid-cols-3",
                for project in projects {
                    article { key: "{project.id}", class: "rounded-2xl border border-slate-800 bg-slate-900 p-5 shadow-lg shadow-black/20",
                        div { class: "mb-5 flex items-start justify-between gap-4",
                            div {
                                h2 { class: "text-xl font-bold text-white", "{project.name}" }
                                p { class: "text-sm text-slate-400", "Next up" }
                            }
                            span { class: "rounded-full border border-cyan-400/30 bg-cyan-400/10 px-3 py-1 text-xs font-semibold text-cyan-200", "Active" }
                        }
                        if let Some(task) = project.first_task() {
                            div { class: "flex items-center gap-3 rounded-xl bg-slate-950/70 p-3",
                                span { class: "h-2.5 w-2.5 rounded-full bg-emerald-400 shadow-[0_0_16px_rgba(52,211,153,0.8)]" }
                                span { class: "text-sm text-slate-200", "{task.title}" }
                            }
                        } else {
                            p { class: "rounded-xl bg-slate-950/70 p-3 text-sm text-slate-500", "No tasks yet." }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn StatCard(label: &'static str, value: String) -> Element {
    rsx! {
        div { class: "rounded-2xl border border-slate-800 bg-slate-950/70 px-5 py-4",
            div { class: "text-3xl font-black text-white", "{value}" }
            div { class: "text-xs font-semibold uppercase tracking-[0.2em] text-slate-500", "{label}" }
        }
    }
}
