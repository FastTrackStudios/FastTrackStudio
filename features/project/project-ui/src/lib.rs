//! Project feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`ProjectList`]       — full collection view, dispatches `on_delete`
//! - [`ProjectRow`]        — single-row presentation (composable into other lists)
//! - [`ProjectCreateForm`] — minimal new-project form, emits the create payload
//! - [`ProjectDashboard`]  — purpose-built dashboard for the live route
//! - [`pm`]                — task-management components (list / board / sheet
//!                           / palette / header / priority badge)

pub mod pm;
pub use pm::*;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Activity, CheckCheck, FolderKanban, Plus, Trash2};
use fts_ui::prelude::*;
use project_proto::{Project, ProjectCreate};
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn ProjectList(items: Vec<Project>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No projects yet. Add one above.",
                icon: rsx! { FolderKanban { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for project in items.iter().cloned() {
                ProjectRow {
                    key: "{project.id}",
                    project: project.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn ProjectRow(project: Project, on_delete: EventHandler<Uuid>) -> Element {
    let id = project.id;
    let kind = project
        .project_type
        .clone()
        .unwrap_or_else(|| "project".into());
    let meta = format!("{} · {}", project.status, kind);
    let status_variant = match project.status.as_str() {
        "active" => StatusBadgeVariant::Success,
        "archived" | "completed" => StatusBadgeVariant::Neutral,
        "blocked" | "cancelled" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Warning,
    };
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{project.name}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                StatusBadge { variant: status_variant, label: project.status.clone() }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn ProjectCreateForm(on_submit: EventHandler<ProjectCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut description = use_signal(String::new);
    let mut project_type = use_signal(String::new);
    let mut color = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add a project" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: project_type,
                        placeholder: "audio-production",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: color,
                        placeholder: "#06b6d4",
                        class: "flex-1 min-w-40",
                    }
                }
                Input {
                    value: description,
                    placeholder: "Description (optional)",
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let payload = ProjectCreate {
                                name: n,
                                description: trim_to_option(description.read().clone()),
                                status: "active".into(),
                                project_type: trim_to_option(project_type.read().clone()),
                                color: trim_to_option(color.read().clone()),
                                owner: None,
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            description.set(String::new());
                            project_type.set(String::new());
                            color.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add project"
                    }
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

/// Purpose-built projects dashboard. Page header + status breakdown stats +
/// status filter + create form + list.
#[component]
pub fn ProjectDashboard(
    items: Vec<Project>,
    status: String,
    on_create: EventHandler<ProjectCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut status_filter = use_signal(|| "all".to_string());

    let count = items.len();
    let mut by_status: BTreeMap<String, usize> = BTreeMap::new();
    for p in &items {
        *by_status.entry(p.status.clone()).or_insert(0) += 1;
    }
    let active = by_status.get("active").copied().unwrap_or(0);
    let completed = by_status.get("completed").copied().unwrap_or(0);
    let blocked_or_hold = by_status.get("on-hold").copied().unwrap_or(0)
        + by_status.get("blocked").copied().unwrap_or(0);

    let statuses: Vec<String> = by_status.keys().cloned().collect();
    let current = status_filter.read().clone();
    let filtered: Vec<Project> = if current == "all" {
        items.clone()
    } else {
        items
            .iter()
            .filter(|p| p.status == current)
            .cloned()
            .collect()
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Projects",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-cyan-500/10 p-2 text-cyan-500",
                    FolderKanban { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Projects dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Containers for the work that ships — tasks, cycles, and milestones nested inside."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total" }
                            FolderKanban { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{count}" }
                        Text { variant: TextVariant::Muted, "{by_status.len()} statuses" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Active" }
                            Activity { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{active}" }
                        Text { variant: TextVariant::Muted, "in flight" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Stalled" }
                            Activity { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{blocked_or_hold}" }
                        Text { variant: TextVariant::Muted, "on-hold / blocked" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Completed" }
                            CheckCheck { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{completed}" }
                        Text { variant: TextVariant::Muted, "shipped" }
                    }
                }
            }

            if statuses.len() > 1 {
                HStack { class: "gap-2 flex-wrap items-center",
                    Text { variant: TextVariant::Muted, "Status:" }
                    Button {
                        variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                        size: ButtonSize::Small,
                        on_click: move |_| status_filter.set("all".into()),
                        "All"
                    }
                    for s in statuses.iter().cloned() {
                        Button {
                            key: "{s}",
                            variant: if current == s { ButtonVariant::Primary } else { ButtonVariant::Outline },
                            size: ButtonSize::Small,
                            on_click: {
                                let s = s.clone();
                                move |_| status_filter.set(s.clone())
                            },
                            "{s}"
                        }
                    }
                }
            }

            ProjectCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Projects",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            ProjectList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}
