use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::data::active_projects;

#[component]
pub fn ProjectOverview() -> Element {
    let projects = active_projects();

    rsx! {
        div { class: "flex flex-col gap-6 p-6",
            div { class: "flex flex-col gap-1",
                Heading { level: HeadingLevel::H1, "Active Projects" }
                Text { variant: TextVariant::Muted,
                    "All currently active projects and their first task."
                }
            }

            div { class: "grid gap-4 md:grid-cols-2 lg:grid-cols-3",
                for project in projects {
                    Card { key: "{project.id}",
                        CardHeader {
                            CardTitle { "{project.name}" }
                            CardDescription { "Next up" }
                        }
                        CardContent {
                            if let Some(task) = project.first_task() {
                                div { class: "flex items-center gap-2",
                                    StatusDot {
                                        color: StatusDotColor::Success,
                                        size: StatusDotSize::Small,
                                    }
                                    Text { "{task.title}" }
                                }
                            } else {
                                Text { variant: TextVariant::Muted, "No tasks yet." }
                            }
                        }
                    }
                }
            }
        }
    }
}
