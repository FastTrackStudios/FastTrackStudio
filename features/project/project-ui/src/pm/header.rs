//! `ProjectHeaderCard` — the page-level project banner.
//!
//! Renders the project's identity (colored swatch, name, description),
//! summary stats (progress + done/total + lead avatar + status), and
//! the two control rows: section tabs (Overview / Tasks / …) and a
//! view toggle (List / Board) plus the "+ New task" button.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronDown, ChevronRight, Palette, Plus, Target};
use fts_ui::prelude::*;
use project_proto::{Project, ProjectUpdate};

use super::common::{color_swatch_class, status_to_badge_variant};
use super::theme::ProjectThemeSettings;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProjectTab {
    Overview,
    Tasks,
    Cycles,
    Milestones,
}

impl ProjectTab {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Overview => "overview",
            Self::Tasks => "tasks",
            Self::Cycles => "cycles",
            Self::Milestones => "milestones",
        }
    }
    pub fn from_str(s: &str) -> Self {
        match s {
            "tasks" => Self::Tasks,
            "cycles" => Self::Cycles,
            "milestones" => Self::Milestones,
            _ => Self::Overview,
        }
    }
}

/// The two render modes the spec calls out for v1 — we ship both.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskView {
    List,
    Board,
}

impl TaskView {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::List => "list",
            Self::Board => "board",
        }
    }
    pub fn from_str(s: &str) -> Self {
        match s {
            "board" => Self::Board,
            _ => Self::List,
        }
    }
}

/// Quick stats for the header's progress ring. The caller computes
/// these from its in-memory task list (cheap; component stays dumb).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct TaskStats {
    pub total: u32,
    pub done: u32,
    pub in_progress: u32,
    pub blocked: u32,
}

impl TaskStats {
    pub fn progress(self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            (self.done as f64 / self.total as f64) * 100.0
        }
    }
}

#[component]
pub fn ProjectHeaderCard(
    project: Project,
    task_stats: TaskStats,
    active_tab: ProjectTab,
    active_view: TaskView,
    /// Currently-active project-local theme preset name. `None` ⇒ inherit
    /// the organization theme.
    #[props(default)]
    project_theme: Option<String>,
    on_tab_change: EventHandler<ProjectTab>,
    on_view_change: EventHandler<TaskView>,
    on_edit: EventHandler<ProjectUpdate>,
    on_new_task: EventHandler<()>,
    /// Emits the new project-local preset name (or `None` to clear and
    /// inherit the org theme). When omitted the picker is hidden.
    #[props(default)]
    on_theme_change: Option<EventHandler<Option<String>>>,
) -> Element {
    let mut expanded = use_signal(|| false);
    let mut theme_open = use_signal(|| false);
    let swatch = color_swatch_class(project.color.as_deref());
    let owner = project.owner.clone();
    let status_variant = status_to_badge_variant(&project.status);
    let status_label = project.status.clone();
    let strike = matches!(project.status.as_str(), "done" | "completed" | "archived");

    let description = project.description.clone().unwrap_or_default();
    let desc_lines = description.lines().count();
    let has_more = desc_lines > 2 || description.len() > 180;
    let name_init = project.name.clone();

    rsx! {
        Card {
            CardContent { class: "flex flex-col gap-4 py-5".to_string(),
                HStack { class: "items-start gap-4 flex-wrap",
                    div { class: "h-12 w-12 rounded-xl shrink-0 {swatch}" }

                    div { class: "flex-1 min-w-0",
                        InlineEdit {
                            value: name_init,
                            on_commit: move |new: String| {
                                on_edit
                                    .call(ProjectUpdate {
                                        name: Some(new),
                                        ..Default::default()
                                    });
                            },
                            class: if strike {
                                "text-2xl font-bold line-through text-muted-foreground".to_string()
                            } else {
                                "text-2xl font-bold".to_string()
                            },
                            placeholder: "Project name".to_string(),
                        }
                        if !description.is_empty() {
                            div { class: "mt-1 text-sm text-muted-foreground",
                                p {
                                    class: if *expanded.read() {
                                        "whitespace-pre-wrap"
                                    } else {
                                        "line-clamp-2"
                                    },
                                    "{description}"
                                }
                                if has_more {
                                    button {
                                        r#type: "button",
                                        class: "text-xs font-medium text-primary hover:underline mt-0.5 inline-flex items-center gap-1",
                                        onclick: move |_| {
                                            let v = !*expanded.read();
                                            expanded.set(v);
                                        },
                                        if *expanded.read() {
                                            ChevronDown { size: 12 } "Less"
                                        } else {
                                            ChevronRight { size: 12 } "More"
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Right-side stats cluster
                    HStack { class: "items-center gap-3 flex-wrap",
                        div { class: "flex flex-col items-end gap-1 min-w-[8rem]",
                            div { class: "text-[10px] uppercase tracking-widest text-muted-foreground",
                                "{task_stats.done}/{task_stats.total} done"
                            }
                            Progress { value: task_stats.progress() }
                        }
                        if task_stats.in_progress > 0 {
                            Badge { variant: BadgeVariant::Secondary,
                                "{task_stats.in_progress} in progress"
                            }
                        }
                        if task_stats.blocked > 0 {
                            Badge { variant: BadgeVariant::Destructive,
                                "{task_stats.blocked} blocked"
                            }
                        }
                        StatusBadge { variant: status_variant, label: status_label }
                        if let Some(name) = owner.as_deref() {
                            HStack { class: "items-center gap-2",
                                Avatar { size: AvatarSize::Small,
                                    AvatarFallback {
                                        class: "bg-muted text-foreground text-[10px]".to_string(),
                                        "{name.chars().next().unwrap_or('?').to_uppercase().to_string()}"
                                    }
                                }
                                span { class: "text-xs text-muted-foreground", "{name}" }
                            }
                        }
                    }
                }

                Divider {}

                // Section tabs row
                HStack { class: "items-center justify-between gap-3 flex-wrap",
                    Tabs {
                        value: Some(active_tab.as_str().to_string()),
                        on_change: move |v: String| on_tab_change.call(ProjectTab::from_str(&v)),
                        TabList {
                            TabTrigger {
                                value: ProjectTab::Overview.as_str().to_string(),
                                index: 0usize,
                                "Overview"
                            }
                            TabTrigger {
                                value: ProjectTab::Tasks.as_str().to_string(),
                                index: 1usize,
                                "Tasks"
                            }
                            TabTrigger {
                                value: ProjectTab::Cycles.as_str().to_string(),
                                index: 2usize,
                                "Cycles"
                            }
                            TabTrigger {
                                value: ProjectTab::Milestones.as_str().to_string(),
                                index: 3usize,
                                Target { size: 12 }
                                " Milestones"
                            }
                        }
                    }

                    HStack { class: "items-center gap-2",
                        SegmentedControl {
                            size: SegmentedControlSize::Small,
                            value: active_view.as_str().to_string(),
                            on_change: move |v: String| on_view_change.call(TaskView::from_str(&v)),
                            options: vec![
                                ("list".into(), "List".into()),
                                ("board".into(), "Board".into()),
                            ],
                        }
                        if let Some(handler) = on_theme_change.clone() {
                            Popover {
                                open: theme_open(),
                                is_modal: false,
                                on_open_change: move |o| theme_open.set(o),
                                PopoverTrigger { class: "inline-flex".to_string(),
                                    button {
                                        r#type: "button",
                                        class: "inline-flex h-8 w-8 items-center justify-center rounded-md border border-border bg-card text-muted-foreground hover:bg-accent hover:text-accent-foreground",
                                        title: "Project theme",
                                        onclick: move |_| {
                                            let v = !*theme_open.read();
                                            theme_open.set(v);
                                        },
                                        Palette { size: 14 }
                                    }
                                }
                                PopoverContent { class: "w-[26rem] p-3".to_string(),
                                    ProjectThemeSettings {
                                        current: project_theme.clone(),
                                        on_change: move |v| handler.call(v),
                                    }
                                }
                            }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| on_new_task.call(()),
                            Plus { size: 14 }
                            " New task"
                        }
                    }
                }
            }
        }
    }
}
