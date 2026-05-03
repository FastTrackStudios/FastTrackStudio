//! Placeholder in-memory model. Will be replaced by a task-core-backed
//! data source once the UI/server wiring lands.

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Task {
    pub id: &'static str,
    pub title: &'static str,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Project {
    pub id: &'static str,
    pub name: &'static str,
    pub tasks: Vec<Task>,
}

impl Project {
    pub fn first_task(&self) -> Option<&Task> {
        self.tasks.first()
    }
}

pub fn active_projects() -> Vec<Project> {
    vec![
        Project {
            id: "obs",
            name: "The Observatory",
            tasks: vec![
                Task { id: "t1", title: "Sync vault to Nextcloud" },
                Task { id: "t2", title: "Audit ACLs" },
            ],
        },
        Project {
            id: "task",
            name: "Task App",
            tasks: vec![
                Task { id: "t1", title: "Scaffold Dioxus apps" },
                Task { id: "t2", title: "Wire task-core to UI" },
            ],
        },
        Project {
            id: "ftaudio",
            name: "FastTrack Audio",
            tasks: vec![Task { id: "t1", title: "Draft Q2 release notes" }],
        },
    ]
}
