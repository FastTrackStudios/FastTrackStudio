//! Grouped list view — Overdue, Today, This week, Later, No
//! due date, Done. Each section is collapsible.

use chrono::{Duration, NaiveDate};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::ChevronRight;
use uuid::Uuid;

use task_proto::{Priority, Status};

use crate::display::TaskDisplay;
use crate::{TaskInfo, TaskMutation};

use super::row::TaskRow;

#[derive(Props, Clone, PartialEq)]
pub struct TaskListProps {
    pub tasks: Vec<TaskInfo>,
    pub on_toggle: EventHandler<Uuid>,
    pub on_open: EventHandler<Uuid>,
    pub on_event: EventHandler<TaskMutation>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Group {
    Overdue,
    Today,
    ThisWeek,
    Later,
    NoDate,
    Done,
}

impl Group {
    fn label(self) -> &'static str {
        match self {
            Self::Overdue => "Overdue",
            Self::Today => "Today",
            Self::ThisWeek => "This week",
            Self::Later => "Later",
            Self::NoDate => "No due date",
            Self::Done => "Done",
        }
    }
}

const ORDER: &[Group] = &[
    Group::Overdue,
    Group::Today,
    Group::ThisWeek,
    Group::Later,
    Group::NoDate,
    Group::Done,
];

fn classify(t: &TaskInfo, today: NaiveDate) -> Group {
    if t.status_enum() == Status::Done {
        return Group::Done;
    }
    match t.due_date() {
        None => Group::NoDate,
        Some(d) if d < today => Group::Overdue,
        Some(d) if d == today => Group::Today,
        Some(d) if d <= today + Duration::days(7) => Group::ThisWeek,
        Some(_) => Group::Later,
    }
}

#[component]
pub fn TaskList(props: TaskListProps) -> Element {
    let today = chrono::Local::now().date_naive();
    let mut groups: Vec<(Group, Vec<TaskInfo>)> = ORDER.iter().map(|g| (*g, Vec::new())).collect();
    for t in &props.tasks {
        let g = classify(t, today);
        if let Some(slot) = groups.iter_mut().find(|(k, _)| *k == g) {
            slot.1.push(t.clone());
        }
    }
    // Sort each group: due ascending, then priority high→low,
    // then title.
    for (_, bucket) in &mut groups {
        bucket.sort_by(|a, b| {
            a.due_date()
                .cmp(&b.due_date())
                .then_with(|| {
                    let rank = |p| match p {
                        Priority::Critical => 0,
                        Priority::High => 1,
                        Priority::Normal => 2,
                        Priority::Low => 3,
                        Priority::None => 4,
                    };
                    rank(a.priority_enum()).cmp(&rank(b.priority_enum()))
                })
                .then_with(|| a.title.to_lowercase().cmp(&b.title.to_lowercase()))
        });
    }

    // Two lanes on wide screens, split by urgency horizon: the left
    // lane is what deserves attention now (overdue, today, undated),
    // the right lane is what's scheduled to come (this week, later,
    // done). One lane on mobile — the same order, stacked.
    let (now_lane, later_lane): (Vec<_>, Vec<_>) = groups
        .into_iter()
        .filter(|(_, items)| !items.is_empty())
        .partition(|(g, _)| matches!(g, Group::Overdue | Group::Today | Group::NoDate));

    let lane = |groups: Vec<(Group, Vec<TaskInfo>)>| {
        rsx! {
            div { class: "flex min-w-0 flex-col gap-3",
                for (group, items) in groups.into_iter() {
                    Section {
                        key: "{group.label()}",
                        label: group.label(),
                        count: items.len(),
                        // The Done section starts collapsed —
                        // typical Things 3 / Todoist behaviour.
                        initially_open: group != Group::Done,
                        TaskList_Children { items, on_toggle: props.on_toggle, on_open: props.on_open, on_event: props.on_event }
                    }
                }
            }
        }
    };

    rsx! {
        if later_lane.is_empty() || now_lane.is_empty() {
            {lane(if now_lane.is_empty() { later_lane } else { now_lane })}
        } else {
            div { class: "grid grid-cols-1 gap-3 lg:grid-cols-2 lg:gap-x-10",
                {lane(now_lane)}
                {lane(later_lane)}
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct TaskListChildrenProps {
    items: Vec<TaskInfo>,
    on_toggle: EventHandler<Uuid>,
    on_open: EventHandler<Uuid>,
    on_event: EventHandler<TaskMutation>,
}

#[allow(non_snake_case)]
#[component]
fn TaskList_Children(props: TaskListChildrenProps) -> Element {
    // Subtasks nest under their parent when both are in the bucket —
    // the same domain arrangement the CLI list prints.
    let arranged = task_proto::arrange_families(props.items.clone(), |t| t.id, TaskDisplay::parent);
    rsx! {
        div { class: "flex flex-col gap-0.5 pl-1",
            for (depth, t) in arranged.into_iter() {
                div {
                    key: "{t.id}",
                    // Subtasks hang off a hairline rail — family
                    // membership as structure, not fat indentation.
                    class: if depth > 0 { "ml-2.5 border-l border-border/60 pl-2.5" } else { "" },
                    TaskRow {
                        task: t,
                        on_toggle: props.on_toggle,
                        on_open: props.on_open,
                        on_event: props.on_event,
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct SectionProps {
    label: &'static str,
    count: usize,
    initially_open: bool,
    children: Element,
}

#[component]
fn Section(props: SectionProps) -> Element {
    let mut open = use_signal(|| props.initially_open);
    let icon_rotation = if open() { "rotate-90" } else { "" };
    rsx! {
        div { class: "flex flex-col gap-1",
            button {
                r#type: "button",
                class: "flex items-center gap-1 text-xs font-semibold uppercase tracking-wider text-muted-foreground hover:text-foreground py-1",
                onclick: move |_| open.toggle(),
                span { class: "transition-transform {icon_rotation}",
                    ChevronRight { size: 12 }
                }
                span { "{props.label}" }
                span { class: "ml-1 rounded-full bg-muted/50 px-1.5 py-0 text-[10px] text-muted-foreground",
                    "{props.count}"
                }
            }
            if open() {
                {props.children}
            }
        }
    }
}
