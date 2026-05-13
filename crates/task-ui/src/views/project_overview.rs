use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::data::active_projects;

#[component]
pub fn ProjectOverview() -> Element {
    let projects = active_projects();
    let project_count = projects.len();
    let task_count: usize = projects.iter().map(|project| project.tasks.len()).sum();

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-8 p-6 lg:p-10",
            Card {
                CardHeader {
                    Text { variant: TextVariant::Small, class: "uppercase tracking-[0.3em] text-primary font-semibold", "Task web baseline" }
                    CardTitle { class: "max-w-3xl text-3xl font-bold lg:text-4xl",
                        "Projects, agent work, and operational status in one shell"
                    }
                    CardDescription { class: "max-w-2xl",
                        "This first Dioxus web slice proves the app shell, navigation, theme pipeline, and deployable static bundle without touching production Task data."
                    }
                }
                CardContent {
                    HStack { gap: "3", class: "flex-wrap",
                        StatCard { label: "Projects", value: format!("{}", project_count) }
                        StatCard { label: "Tasks", value: format!("{}", task_count) }
                    }
                }
            }

            SectionHeader { label: "Active projects" }

            section { class: "grid gap-4 md:grid-cols-2 xl:grid-cols-3",
                for project in projects {
                    Card { key: "{project.id}",
                        CardHeader { class: "flex flex-row items-start justify-between gap-3",
                            div { class: "flex flex-col gap-1",
                                CardTitle { "{project.name}" }
                                CardDescription { "Next up" }
                            }
                            StatusBadge {
                                variant: StatusBadgeVariant::Success,
                                label: "Active",
                            }
                        }
                        CardContent {
                            if let Some(task) = project.first_task() {
                                Item { variant: ItemVariant::Muted,
                                    ItemMedia { class: "size-7 rounded-full bg-green-500/20 text-green-500",
                                        span { class: "block size-2 rounded-full bg-current" }
                                    }
                                    ItemContent {
                                        ItemTitle { "{task.title}" }
                                    }
                                }
                            } else {
                                EmptyState { message: "No tasks yet." }
                            }
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
        Card { class: "min-w-32",
            CardContent { class: "p-4 pt-4",
                div { class: "text-3xl font-extrabold text-foreground", "{value}" }
                Text { variant: TextVariant::Small, class: "uppercase tracking-[0.2em] text-muted-foreground font-semibold", "{label}" }
            }
        }
    }
}
