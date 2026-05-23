//! `/tasks` — list + kanban over an in-memory seed.
//!
//! Wires [`task_ui::TasksApp`] against a `Signal<TaskState>`
//! and the reducer's `apply`. Persistence to the file-backed
//! `task` crate is a follow-up; the demo seed is enough to QA
//! the UI today.

use chrono::Duration;
use dioxus::prelude::*;
use task_ui::TaskMutation;
use task_ui::model::{Priority, Status};
use task_ui::store::{TaskState, apply};
use task_ui::{TaskInfo, TasksApp};

#[component]
pub fn TasksView() -> Element {
    let mut state = use_signal(seed_state);
    let tasks = state.read().tasks.clone();

    rsx! {
        TasksApp {
            tasks,
            on_event: move |mu: TaskMutation| apply(&mut state.write(), &mu),
        }
    }
}

fn seed_state() -> TaskState {
    let today = chrono::Local::now().date_naive();
    let mk = |title: &str, status: Status, priority: Priority, due_offset: Option<i64>| {
        let mut t = TaskInfo::new(title);
        t.status = status.as_str().to_string();
        t.priority = priority.as_str().to_string();
        t.due = due_offset.map(|d| (today + Duration::days(d)).format("%Y-%m-%d").to_string());
        t
    };

    let mut tasks = vec![
        {
            let mut t = mk(
                "Ship the Tasks UI",
                Status::InProgress,
                Priority::High,
                Some(0),
            );
            t.contexts = vec!["dev".into()];
            t.projects = vec!["[[Task]]".into()];
            t
        },
        {
            let mut t = mk(
                "Review PR #92 calendar UX",
                Status::Open,
                Priority::Normal,
                Some(-1),
            );
            t.contexts = vec!["dev".into()];
            t
        },
        {
            let mut t = mk(
                "Wire vault-live into Tasks store",
                Status::Open,
                Priority::Normal,
                Some(3),
            );
            t.projects = vec!["[[Task]]".into()];
            t
        },
        mk("Pick up groceries", Status::Open, Priority::Low, Some(0)),
        {
            let mut t = mk("Plan Q3 roadmap", Status::Waiting, Priority::High, Some(7));
            t.contexts = vec!["work".into()];
            t.projects = vec!["[[Roadmap]]".into()];
            t
        },
        mk("Call dentist", Status::Open, Priority::Normal, None),
        {
            let mut t = mk(
                "Fix scheduling RRULE parser",
                Status::Open,
                Priority::Critical,
                Some(-3),
            );
            t.contexts = vec!["dev".into()];
            t.projects = vec!["[[Scheduling]]".into()];
            t
        },
        {
            let mut t = mk(
                "Read TaskNotes Bases docs",
                Status::Done,
                Priority::Low,
                Some(-2),
            );
            t.completed_date = Some(today - Duration::days(2));
            t
        },
    ];

    // Stable demo-friendly ordering by reverse-insertion time.
    for t in &mut tasks {
        t.date_modified = Some(chrono::Utc::now());
    }

    TaskState { tasks }
}
