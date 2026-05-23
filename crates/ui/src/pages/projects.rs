//! `/projects` — card-grid project browser.
//!
//! Renders every project the active org owns as a card with
//! title, status badge, priority chip, and tag chips.
//! Subprojects nest inside their parent's card as a compact
//! list with a status badge each.
//!
//! Wired against a hardcoded in-page fixture matching the
//! codywright vault as seeded. Replace the fixture with a
//! vox-backed loader (`/org/<slug>/vox` → project list) when
//! the project RPC lands; the component's data shape is
//! storage-agnostic.

use dioxus::prelude::*;
use fts_ui::prelude::*;

/// Trimmed view of `project::ProjectInfo` — only the fields
/// the card needs. Replaces the full entity until we wire the
/// remote loader through vox.
#[derive(Clone, PartialEq)]
struct ProjectCard {
    title: &'static str,
    /// `active` / `on-hold` / `done` / `cancelled`.
    status: &'static str,
    /// `p0`..`p4` / `urgent` / `high` / `normal` / `low`.
    priority: &'static str,
    /// Parent project title for sub-projects; `None` for
    /// top-level.
    parent: Option<&'static str>,
    tags: &'static [&'static str],
    summary: &'static str,
}

const PROJECTS: &[ProjectCard] = &[
    ProjectCard {
        title: "Health",
        status: "active",
        priority: "high",
        parent: None,
        tags: &["health", "ongoing", "parent"],
        summary: "Umbrella for body, energy, and recovery. Decomposes into Fitness, Nutrition, Sleep Tracking.",
    },
    ProjectCard {
        title: "Fitness",
        status: "active",
        priority: "normal",
        parent: Some("Health"),
        tags: &["fitness", "health"],
        summary: "Workout routines, gym sessions, body metrics.",
    },
    ProjectCard {
        title: "Nutrition",
        status: "active",
        priority: "normal",
        parent: Some("Health"),
        tags: &["nutrition", "calorie-intake", "health"],
        summary: "Daily calorie + macro tracking, supplement log.",
    },
    ProjectCard {
        title: "Sleep Tracking",
        status: "active",
        priority: "normal",
        parent: Some("Health"),
        tags: &["sleep", "recovery", "health"],
        summary: "Nightly sleep duration + quality. Targets 7.5h from Daily Plan.",
    },
    ProjectCard {
        title: "Mealplan",
        status: "active",
        priority: "normal",
        parent: None,
        tags: &["mealplan", "personal"],
        summary: "Personal meal-planning. Recipes from the Cookbook wiki.",
    },
    ProjectCard {
        title: "Scheduling",
        status: "active",
        priority: "normal",
        parent: None,
        tags: &["scheduling"],
        summary: "Day templates + event types + availability schedules.",
    },
    ProjectCard {
        title: "Inbox Zero",
        status: "active",
        priority: "high",
        parent: None,
        tags: &["inbox", "processing", "ongoing"],
        summary: "End every day with Inbox empty — fleeting notes processed or promoted.",
    },
    ProjectCard {
        title: "The Temporal Contract",
        status: "active",
        priority: "normal",
        parent: None,
        tags: &["self-awareness", "time-tracking", "meta"],
        summary: "Fidelity log of how well I actually adhere to the Daily Plan time blocks.",
    },
    ProjectCard {
        title: "Bible Study",
        status: "active",
        priority: "high",
        parent: None,
        tags: &["bible", "spiritual", "study", "ongoing"],
        summary: "Reading plan + passage notes. Anchored on the 6:30 AM spiritual block.",
    },
];

#[component]
pub fn ProjectsView() -> Element {
    let top_level: Vec<&ProjectCard> = PROJECTS.iter().filter(|p| p.parent.is_none()).collect();
    let total = PROJECTS.len();
    let top_count = top_level.len();

    rsx! {
        div { class: "mx-auto w-full max-w-6xl flex flex-col gap-6 p-6 lg:p-10",
            div { class: "flex items-start justify-between gap-4",
                div { class: "flex flex-col gap-1",
                    Heading { level: HeadingLevel::H1, "Projects" }
                    Text {
                        variant: TextVariant::Muted,
                        "{top_count} top-level · {total} total. Subprojects nest under their parent."
                    }
                }
            }
            div { class: "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4",
                for parent in top_level.iter() {
                    {
                        let parent = *parent;
                        let kids: Vec<ProjectCard> = PROJECTS
                            .iter()
                            .filter(|p| p.parent == Some(parent.title))
                            .cloned()
                            .collect();
                        rsx! {
                            ProjectCardView {
                                key: "{parent.title}",
                                p: parent.clone(),
                                subprojects: kids,
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ProjectCardProps {
    p: ProjectCard,
    subprojects: Vec<ProjectCard>,
}

#[component]
fn ProjectCardView(props: ProjectCardProps) -> Element {
    let p = &props.p;
    let kids = props.subprojects.clone();
    rsx! {
        Card {
            CardHeader {
                div { class: "flex items-start justify-between gap-2",
                    CardTitle { "{p.title}" }
                    StatusBadge {
                        variant: status_variant(p.status),
                        label: p.status.to_string(),
                    }
                }
                CardDescription { "{p.summary}" }
            }
            CardContent {
                div { class: "flex flex-wrap items-center gap-1.5 text-xs",
                    PriorityChip { priority: p.priority }
                    for tag in p.tags.iter() {
                        Badge {
                            variant: BadgeVariant::Secondary,
                            "{tag}"
                        }
                    }
                }
                if !kids.is_empty() {
                    div { class: "mt-4 flex flex-col gap-2 border-t border-border/40 pt-3",
                        Text {
                            variant: TextVariant::Muted,
                            class: "text-xs uppercase tracking-wider",
                            "Subprojects",
                        }
                        for kid in kids.iter() {
                            div {
                                key: "{kid.title}",
                                class: "flex items-center justify-between gap-2 rounded-md bg-muted/30 px-3 py-2 text-sm",
                                span { class: "font-medium", "{kid.title}" }
                                StatusBadge {
                                    variant: status_variant(kid.status),
                                    label: kid.status.to_string(),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct PriorityChipProps {
    priority: &'static str,
}

#[component]
fn PriorityChip(props: PriorityChipProps) -> Element {
    let cls = match props.priority {
        "high" | "urgent" | "p0" | "p1" => {
            "rounded-full px-2 py-0.5 bg-destructive/15 text-destructive font-medium"
        }
        "normal" | "p2" => "rounded-full px-2 py-0.5 bg-muted/60 text-muted-foreground",
        _ => "rounded-full px-2 py-0.5 bg-muted/40 text-muted-foreground",
    };
    let label = props.priority;
    rsx! {
        span { class: "{cls}", "{label}" }
    }
}

/// Map a status string to the canonical `StatusBadgeVariant`.
/// `active` → Success, `on-hold` / `paused` → Warning,
/// `cancelled` / `blocked` → Danger, anything else → Neutral.
fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" => StatusBadgeVariant::Success,
        "on-hold" | "paused" => StatusBadgeVariant::Warning,
        "cancelled" | "blocked" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}
