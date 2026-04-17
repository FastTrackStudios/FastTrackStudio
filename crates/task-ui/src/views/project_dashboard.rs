use dioxus::prelude::*;
use fts_ui::prelude::*;
use task_core::{next_task, Project, ProjectStats, Task};

use crate::components::ProjectCard;

#[derive(Props, Clone, PartialEq)]
pub struct ProjectDashboardProps {
    pub projects: Vec<Project>,
    pub tasks: Vec<Task>,
    /// Called when a project card is clicked. Receives project title.
    #[props(default)]
    pub on_project_tap: Option<EventHandler<String>>,
    /// Called when the "complete next task" action is triggered. Receives task title.
    #[props(default)]
    pub on_complete_task: Option<EventHandler<String>>,
}

#[component]
pub fn ProjectDashboard(props: ProjectDashboardProps) -> Element {
    let mut active: Vec<&Project> = props
        .projects
        .iter()
        .filter(|p| p.is_active() && !p.is_archived())
        .collect();

    // Sort: overdue first, then by urgency of next task
    active.sort_by(|a, b| {
        b.is_overdue()
            .cmp(&a.is_overdue())
            .then_with(|| {
                let a_urgency = next_task(&a.title, &props.tasks)
                    .map(|t| t.urgency_score())
                    .unwrap_or(0);
                let b_urgency = next_task(&b.title, &props.tasks)
                    .map(|t| t.urgency_score())
                    .unwrap_or(0);
                b_urgency.cmp(&a_urgency)
            })
    });

    // Summary stats
    let total_projects = active.len();
    let total_tasks: usize = active.iter().map(|p| {
        props.tasks.iter().filter(|t| t.projects.iter().any(|pp| pp.0 == p.title)).count()
    }).sum();
    let completed_tasks: usize = active.iter().map(|p| {
        props.tasks.iter().filter(|t| t.projects.iter().any(|pp| pp.0 == p.title) && t.is_complete()).count()
    }).sum();

    rsx! {
        div { class: "flex flex-col gap-6",
            // Header
            div { class: "flex items-center justify-between",
                h2 { class: "text-lg font-semibold tracking-tight", "Projects" }
                div { class: "flex items-center gap-3",
                    span { class: "text-xs text-muted-foreground tabular-nums",
                        "{completed_tasks}/{total_tasks} tasks done"
                    }
                    span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                        "{total_projects} active"
                    }
                }
            }

            if active.is_empty() {
                EmptyState { message: "No active projects.".to_string() }
            } else {
                div { class: "grid gap-4 grid-cols-1 lg:grid-cols-2",
                    for project in active {
                        {
                            let project_tasks: Vec<&Task> = props.tasks.iter()
                                .filter(|t| t.projects.iter().any(|p| p.0 == project.title))
                                .collect();
                            let stats = ProjectStats::from_tasks(&project_tasks);
                            let nt = next_task(&project.title, &props.tasks).cloned();
                            let pct = stats.completion_percent();
                            let proj = project.clone();
                            let proj_title = project.title.clone();
                            let next_task_title = nt.as_ref().map(|t| t.title.clone());
                            let on_project_tap = props.on_project_tap.clone();
                            let on_complete_task = props.on_complete_task.clone();
                            rsx! {
                                ProjectCard {
                                    key: "{proj.title}",
                                    project: proj,
                                    next_task: nt,
                                    completion_percent: pct,
                                    completed_count: stats.completed_task_count,
                                    total_count: stats.total(),
                                    on_tap: move |_| {
                                        if let Some(ref h) = on_project_tap {
                                            h.call(proj_title.clone());
                                        }
                                    },
                                    on_complete_next: move |_| {
                                        if let (Some(ref h), Some(ref title)) = (&on_complete_task, &next_task_title) {
                                            h.call(title.clone());
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
