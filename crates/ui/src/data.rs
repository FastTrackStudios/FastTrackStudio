//! Placeholder in-memory model. Will be replaced by a task-core-backed
//! data source once the UI/server wiring lands.

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TaskStatus {
    Todo,
    Doing,
    Blocked,
    Done,
}

impl TaskStatus {
    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            TaskStatus::Todo => "todo",
            TaskStatus::Doing => "doing",
            TaskStatus::Blocked => "blocked",
            TaskStatus::Done => "done",
        }
    }

    /// Tailwind classes used to color the status badge in the UI.
    /// Keep one source of truth here so views never hand-color statuses.
    #[must_use]
    pub fn badge_class(self) -> &'static str {
        match self {
            TaskStatus::Todo => "border-slate-700 bg-slate-800/70 text-slate-200",
            TaskStatus::Doing => "border-cyan-400/40 bg-cyan-400/10 text-cyan-200",
            TaskStatus::Blocked => "border-amber-400/40 bg-amber-400/10 text-amber-200",
            TaskStatus::Done => "border-emerald-400/40 bg-emerald-400/10 text-emerald-200",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Task {
    pub id: &'static str,
    pub title: &'static str,
    pub status: TaskStatus,
    /// Free-form context shown under the title in list views.
    pub note: &'static str,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Project {
    pub id: &'static str,
    pub name: &'static str,
    pub summary: &'static str,
    pub tasks: Vec<Task>,
}

impl Project {
    #[must_use]
    pub fn first_task(&self) -> Option<&Task> {
        self.tasks.iter().find(|t| t.status != TaskStatus::Done)
    }

    #[must_use]
    pub fn open_count(&self) -> usize {
        self.tasks
            .iter()
            .filter(|t| t.status != TaskStatus::Done)
            .count()
    }

    #[must_use]
    pub fn done_count(&self) -> usize {
        self.tasks
            .iter()
            .filter(|t| t.status == TaskStatus::Done)
            .count()
    }
}

#[must_use]
pub fn active_projects() -> Vec<Project> {
    vec![
        Project {
            id: "obs",
            name: "The Observatory",
            summary: "Nextcloud-backed agent vault and ACL surface.",
            tasks: vec![
                Task {
                    id: "obs-1",
                    title: "Sync vault to Nextcloud",
                    status: TaskStatus::Doing,
                    note: "Curator owns rsync schedule",
                },
                Task {
                    id: "obs-2",
                    title: "Audit ACLs",
                    status: TaskStatus::Todo,
                    note: "Cross-check against LLDAP groups",
                },
                Task {
                    id: "obs-3",
                    title: "Document recovery flow",
                    status: TaskStatus::Blocked,
                    note: "Waiting on restic repository decision",
                },
            ],
        },
        Project {
            id: "task",
            name: "Task App",
            summary: "Local-first command center across web / desktop / mobile.",
            tasks: vec![
                Task {
                    id: "task-1",
                    title: "Scaffold Dioxus apps",
                    status: TaskStatus::Done,
                    note: "apps/web + apps/desktop + apps/mobile live",
                },
                Task {
                    id: "task-2",
                    title: "Wire task-core to UI",
                    status: TaskStatus::Doing,
                    note: "Currently using placeholder model",
                },
                Task {
                    id: "task-3",
                    title: "Inbox triage flow",
                    status: TaskStatus::Todo,
                    note: "Drop, defer, convert-to-task actions",
                },
                Task {
                    id: "task-4",
                    title: "Ship dev tunnel",
                    status: TaskStatus::Done,
                    note: "task-dev.starcommand.live → dx serve",
                },
            ],
        },
        Project {
            id: "ftaudio",
            name: "FastTrack Audio",
            summary: "Quarterly release work for the audio production stack.",
            tasks: vec![
                Task {
                    id: "fa-1",
                    title: "Draft Q2 release notes",
                    status: TaskStatus::Todo,
                    note: "Pull commits since v0.34.0 tag",
                },
                Task {
                    id: "fa-2",
                    title: "Schedule pre-release listening session",
                    status: TaskStatus::Todo,
                    note: "Engineer + producer + lead vocalist",
                },
            ],
        },
    ]
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InboxSource {
    Email,
    Webhook,
    Talk,
    Manual,
}

impl InboxSource {
    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            InboxSource::Email => "Email",
            InboxSource::Webhook => "Webhook",
            InboxSource::Talk => "Nextcloud Talk",
            InboxSource::Manual => "Captured",
        }
    }

    #[must_use]
    pub fn icon(self) -> &'static str {
        match self {
            InboxSource::Email => "✉",
            InboxSource::Webhook => "⚡",
            InboxSource::Talk => "💬",
            InboxSource::Manual => "✎",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InboxItem {
    pub id: &'static str,
    pub title: &'static str,
    pub source: InboxSource,
    pub from: &'static str,
    /// Human-readable receipt time. The real backend will produce
    /// timestamps and the view will format them.
    pub received: &'static str,
    pub summary: &'static str,
    /// If the source already hints which project this belongs to, the
    /// inbox view surfaces that as a one-click "file to project" action.
    pub project_hint: Option<&'static str>,
}

#[must_use]
pub fn inbox_items() -> Vec<InboxItem> {
    vec![
        InboxItem {
            id: "inb-1",
            title: "Re: vault snapshot diff",
            source: InboxSource::Email,
            from: "amy@fasttrackaudio.com",
            received: "12 min ago",
            summary: "Found two stale shares in Observatory/clients — okay to prune?",
            project_hint: Some("obs"),
        },
        InboxItem {
            id: "inb-2",
            title: "Forgejo run #119 failed",
            source: InboxSource::Webhook,
            from: "ci@git.starcommand.live",
            received: "1 hr ago",
            summary: "validate-staging on nix-fleet — flake check exit 1.",
            project_hint: None,
        },
        InboxItem {
            id: "inb-3",
            title: "Cody: pick a port for task-dev tunnel",
            source: InboxSource::Talk,
            from: "starcommand",
            received: "yesterday",
            summary: "Resolved → 8765. Capture so the decision lands in the wiki.",
            project_hint: Some("task"),
        },
        InboxItem {
            id: "inb-4",
            title: "Pre-release listening session window",
            source: InboxSource::Manual,
            from: "self",
            received: "yesterday",
            summary: "Nail down a date that works for engineer + producer + lead.",
            project_hint: Some("ftaudio"),
        },
        InboxItem {
            id: "inb-5",
            title: "wasm32 target missing on THEBATTLESHIP",
            source: InboxSource::Manual,
            from: "self",
            received: "2 days ago",
            summary: "rustup target add wasm32-unknown-unknown so just task-web-dev runs clean.",
            project_hint: Some("task"),
        },
    ]
}

/// Look up the project name for a hint id, if any. Used by the inbox
/// view to render the project chip on items the source already tagged.
#[must_use]
pub fn project_name(id: &str) -> Option<&'static str> {
    active_projects()
        .into_iter()
        .find(|p| p.id == id)
        .map(|p| p.name)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Organization {
    pub id: &'static str,
    pub name: &'static str,
    /// Short 1–2 letter monogram for the avatar.
    pub monogram: &'static str,
    /// Free-form role copy ("Owner", "Member", etc).
    pub role: &'static str,
    /// Default fts-ui theme preset name for this organization.
    /// The user can override it at runtime through the in-app theme picker
    /// (see `task_ui::theming::OrgThemeOverrides`).
    pub theme_preset: &'static str,
}

#[must_use]
pub fn organizations() -> Vec<Organization> {
    vec![
        Organization {
            id: "personal",
            name: "Personal",
            monogram: "P",
            role: "Owner",
            theme_preset: "default",
        },
        Organization {
            id: "work",
            name: "Work",
            monogram: "W",
            role: "Member",
            theme_preset: "violet-bloom",
        },
        Organization {
            id: "side-project",
            name: "Side Project",
            monogram: "SP",
            role: "Owner",
            theme_preset: "supabase",
        },
    ]
}
