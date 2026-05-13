use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::data::{Project, Task, TaskStatus, active_projects};

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
                    Text { variant: TextVariant::Small, class: "uppercase tracking-[0.3em] text-primary font-semibold".to_string(), "All projects" }
                    Heading { level: HeadingLevel::H1, "Every project, every task" }
                    Text { variant: TextVariant::Muted, class: "max-w-2xl".to_string(),
                        "One screen for the full state of work. Each project shows its full task list grouped by status."
                    }
                }
                HStack { gap: "3".to_string(), class: "flex-wrap".to_string(),
                    Stat { label: "Projects", value: format!("{}", project_count) }
                    Stat { label: "Open", value: format!("{}", open_tasks) }
                    Stat { label: "Done", value: format!("{}", done_tasks) }
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
        Card {
            CardHeader { class: "flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between".to_string(),
                div { class: "flex flex-col gap-1",
                    CardTitle { class: "text-2xl".to_string(), "{project.name}" }
                    CardDescription { "{project.summary}" }
                }
                HStack { gap: "2".to_string(),
                    Badge { variant: BadgeVariant::Outline, "{open} open" }
                    Badge { variant: BadgeVariant::Outline, "{done} done" }
                }
            }
            CardContent {
                if project.tasks.is_empty() {
                    EmptyState { message: "No tasks yet.".to_string() }
                } else {
                    ItemGroup {
                        for task in &project.tasks {
                            TaskRow { key: "{task.id}", task: task.clone() }
                        }
                    }
                }
            }
        }
    }
}

fn status_variant(status: TaskStatus) -> StatusBadgeVariant {
    match status {
        TaskStatus::Todo => StatusBadgeVariant::Neutral,
        TaskStatus::Doing => StatusBadgeVariant::Success,
        TaskStatus::Blocked => StatusBadgeVariant::Warning,
        TaskStatus::Done => StatusBadgeVariant::Neutral,
    }
}

#[component]
fn TaskRow(task: Task) -> Element {
    let variant = status_variant(task.status);
    let title_class = if task.status == TaskStatus::Done {
        "line-through text-muted-foreground"
    } else {
        ""
    };
    rsx! {
        Item { variant: ItemVariant::Muted,
            ItemContent {
                HStack { gap: "3".to_string(), class: "items-center".to_string(),
                    StatusBadge {
                        variant,
                        label: task.status.label().to_string(),
                        class: "text-[10px] uppercase tracking-widest px-2 py-0.5".to_string(),
                    }
                    ItemTitle { class: "{title_class}", "{task.title}" }
                }
                if !task.note.is_empty() {
                    ItemDescription { "{task.note}" }
                }
            }
        }
    }
}

#[component]
fn Stat(label: &'static str, value: String) -> Element {
    rsx! {
        Card { class: "min-w-28".to_string(),
            CardContent { class: "p-4 pt-4".to_string(),
                div { class: "text-2xl font-extrabold text-foreground", "{value}" }
                Text { variant: TextVariant::Small, class: "uppercase tracking-[0.2em] text-muted-foreground font-semibold".to_string(), "{label}" }
            }
        }
    }
}
