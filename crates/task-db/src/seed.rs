//! Idempotent demo-data seeder.
//!
//! Provides reproducible fixture data across the SeaORM domain entities so the
//! server can boot into a known state and the CLI / UI / real-time tests have
//! a stable target. Every seeded row uses a deterministic UUID derived from
//! [`DEMO_NAMESPACE`] + a stable string key — re-running [`seed_demo_data`]
//! against an already-seeded database is a no-op.
//!
//! The seeder reaches into the SeaORM `ActiveModel`s directly rather than the
//! crudcrate `*Api*` create surface because the API layer auto-generates new
//! UUIDs on create (idempotency-hostile). This is admin-only seed code; the
//! rest of the app continues to go through the generated CRUD surfaces.

use chrono::{Duration, NaiveDate, Utc};
use sea_orm::{ActiveValue::Set, DatabaseConnection, DbErr, EntityTrait};
use uuid::Uuid;

use task_core::calendar_event::{self, CalendarEventStatus};
use task_core::comment;
use task_core::people::{self, ContactMethod, ContactMethodList, ProviderRef, ProviderRefList};
use task_core::project::{self, ProjectStatus};
use task_core::task::{
    self, EmailRefList, Priority, RecurrenceAnchor, ReminderList, Status, StringList,
    TaskDependencyList, TaskRelationList, TimeEntry, TimeEntryList, WikiLink, WikiLinkList,
};

/// Namespace for demo-data UUID derivation. Stable; do not change without
/// running `reset_demo_data` first.
pub const DEMO_NAMESPACE: Uuid = Uuid::from_u128(0x0d_e7_5e_ed_0d_e7_5e_ed_0d_e7_5e_ed_0d_e7_5e_ed);

/// Per-entity counts for a seed run.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DemoSeedSummary {
    pub projects_created: usize,
    pub projects_unchanged: usize,
    pub tasks_created: usize,
    pub tasks_unchanged: usize,
    pub calendar_events_created: usize,
    pub calendar_events_unchanged: usize,
    pub people_created: usize,
    pub people_unchanged: usize,
    pub comments_created: usize,
    pub comments_unchanged: usize,
}

impl DemoSeedSummary {
    pub fn total_created(&self) -> usize {
        self.projects_created
            + self.tasks_created
            + self.calendar_events_created
            + self.people_created
            + self.comments_created
    }

    pub fn total_unchanged(&self) -> usize {
        self.projects_unchanged
            + self.tasks_unchanged
            + self.calendar_events_unchanged
            + self.people_unchanged
            + self.comments_unchanged
    }
}

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

/// Idempotently seed every entity flavor that has a server-side `*ServiceImpl`.
pub async fn seed_demo_data(db: &DatabaseConnection) -> Result<DemoSeedSummary, DbErr> {
    let mut summary = DemoSeedSummary::default();
    seed_projects(db, &mut summary).await?;
    seed_tasks(db, &mut summary).await?;
    seed_calendar_events(db, &mut summary).await?;
    seed_people(db, &mut summary).await?;
    seed_comments(db, &mut summary).await?;
    Ok(summary)
}

/// Delete every row created by [`seed_demo_data`] (by deterministic id).
pub async fn reset_demo_data(db: &DatabaseConnection) -> Result<DemoSeedSummary, DbErr> {
    let mut summary = DemoSeedSummary::default();

    for key in COMMENT_KEYS {
        let id = demo_id(key);
        if comment::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.comments_created += 1;
        }
    }
    for key in PEOPLE_KEYS {
        let id = demo_id(key);
        if people::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.people_created += 1;
        }
    }
    for key in CALENDAR_EVENT_KEYS {
        let id = demo_id(key);
        if calendar_event::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.calendar_events_created += 1;
        }
    }
    for key in TASK_KEYS {
        let id = demo_id(key);
        if task::Entity::delete_by_id(id).exec(db).await?.rows_affected > 0 {
            summary.tasks_created += 1;
        }
    }
    for key in PROJECT_KEYS {
        let id = demo_id(key);
        if project::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.projects_created += 1;
        }
    }
    Ok(summary)
}

// ── Stable keys ──────────────────────────────────────────────────────────────

const PROJECT_KEYS: &[&str] = &[
    "project:task-app",
    "project:fasttrack-album",
    "project:venue-prep",
    "project:on-hold-r-and-d",
    "project:archived-2024",
    "project:personal-todo",
];

const TASK_KEYS: &[&str] = &[
    // Active, varied
    "task:fix-auth-bug",
    "task:design-portal-ui",
    "task:write-readme",
    "task:write-tests",
    "task:onboard-codywright",
    "task:overdue-tax-filing",
    "task:due-today-call-client",
    "task:scheduled-future-deploy",
    "task:running-timer-perf-pass",
    "task:billable-mix-mastering",
    "task:weekly-recurring-standup",
    // Inbox bucket
    "task:inbox-misc-idea",
    "task:inbox-research-loro",
    // Done
    "task:done-publish-blog",
    "task:done-q1-review",
    // Cancelled / archived
    "task:cancelled-old-spike",
    // Body content
    "task:body-rich-spec",
];

const CALENDAR_EVENT_KEYS: &[&str] = &[
    "event:standup-today",
    "event:client-meeting-tomorrow",
    "event:past-retro",
    "event:offsite-allday",
];

const PEOPLE_KEYS: &[&str] = &[
    "person:cody-wright",
    "person:amy-wright",
    "person:tom-brooks",
    "person:carter-whitlock",
];

const COMMENT_KEYS: &[&str] = &[
    "comment:auth-bug-1",
    "comment:auth-bug-2-reply",
    "comment:design-resolved",
];

// ── Project fixtures ────────────────────────────────────────────────────────

async fn seed_projects(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let today = Utc::now().date_naive();

    let projects = [
        {
            let mut p = project_base("project:task-app", "Task App", ProjectStatus::Active);
            p.description = Set(Some(
                "The task management system this codebase is.".to_string(),
            ));
            p.area = Set(Some("Engineering".to_string()));
            p.organization = Set(Some("FastTrack Studios".to_string()));
            p.project_type = Set(Some("Product".to_string()));
            p.workflow = Set(Some("kanban".to_string()));
            p.workflow_stage = Set(Some("In Progress".to_string()));
            p.identifier = Set(Some("TASK".to_string()));
            p.lead = Set(Some("codywright".to_string()));
            p.default_assignee = Set(Some("codywright".to_string()));
            p.emoji = Set(Some("🛠".to_string()));
            p.start = Set(Some(today - Duration::days(120)));
            p.due = Set(Some(today + Duration::days(60)));
            p.tags = Set(StringList::from(vec![
                "internal".to_string(),
                "product".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "codywright".to_string(),
                "amywright".to_string(),
            ]));
            p
        },
        {
            let mut p = project_base(
                "project:fasttrack-album",
                "Montreal Album",
                ProjectStatus::Active,
            );
            p.description = Set(Some(
                "Audio production: mastering + sequencing.".to_string(),
            ));
            p.area = Set(Some("Music".to_string()));
            p.project_type = Set(Some("Audio".to_string()));
            p.emoji = Set(Some("🎧".to_string()));
            p.start = Set(Some(today - Duration::days(45)));
            p.due = Set(Some(today + Duration::days(30)));
            p.tags = Set(StringList::from(vec!["billable".to_string()]));
            p
        },
        {
            let mut p = project_base(
                "project:venue-prep",
                "Campus Jax Show Prep",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Operations".to_string()));
            p.project_type = Set(Some("Event".to_string()));
            p.emoji = Set(Some("🎤".to_string()));
            p.start = Set(Some(today));
            p.due = Set(Some(today + Duration::days(14)));
            p
        },
        {
            let mut p = project_base(
                "project:on-hold-r-and-d",
                "CRDT Research Spike",
                ProjectStatus::OnHold,
            );
            p.area = Set(Some("Engineering".to_string()));
            p.description = Set(Some("Loro CRDT integration exploration.".to_string()));
            p
        },
        {
            let mut p = project_base(
                "project:archived-2024",
                "2024 Q4 Retrospective",
                ProjectStatus::Archived,
            );
            p.area = Set(Some("Operations".to_string()));
            p
        },
        {
            let mut p = project_base(
                "project:personal-todo",
                "Personal Todos",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Personal".to_string()));
            p.emoji = Set(Some("📝".to_string()));
            p
        },
    ];

    for p in projects {
        let id = match &p.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if project::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.projects_unchanged += 1;
        } else {
            project::Entity::insert(p).exec(db).await?;
            summary.projects_created += 1;
        }
    }
    Ok(())
}

fn project_base(key: &str, title: &str, status: ProjectStatus) -> project::ActiveModel {
    project::ActiveModel {
        id: Set(demo_id(key)),
        title: Set(title.to_string()),
        status: Set(status),
        up: Set(WikiLinkList::default()),
        tags: Set(StringList::default()),
        team: Set(StringList::default()),
        references: Set(WikiLinkList::default()),
        favorited_by: Set(StringList::default()),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        ..Default::default()
    }
}

// ── Task fixtures ────────────────────────────────────────────────────────────

async fn seed_tasks(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let tasks = [
        // Active, varied
        task_active(
            "task:fix-auth-bug",
            "Fix login session timeout",
            Priority::High,
            Status::InProgress,
            Some("Task App"),
            vec!["backend", "bug"],
            Some(today + Duration::days(2)),
            Some("codywright"),
            now,
        ),
        task_active(
            "task:design-portal-ui",
            "Design portal landing page",
            Priority::Normal,
            Status::Open,
            Some("Task App"),
            vec!["design"],
            Some(today + Duration::days(5)),
            Some("amywright"),
            now,
        ),
        task_active(
            "task:write-readme",
            "Write README for new repo layout",
            Priority::Low,
            Status::Open,
            Some("Task App"),
            vec!["docs"],
            None,
            Some("codywright"),
            now,
        ),
        task_active(
            "task:write-tests",
            "Add integration tests for realtime",
            Priority::High,
            Status::InProgress,
            Some("Task App"),
            vec!["testing"],
            Some(today + Duration::days(3)),
            Some("codywright"),
            now,
        ),
        task_active(
            "task:onboard-codywright",
            "Onboard new dev",
            Priority::Normal,
            Status::Open,
            Some("Task App"),
            vec!["ops"],
            Some(today + Duration::days(7)),
            None,
            now,
        ),
        // Overdue
        task_active(
            "task:overdue-tax-filing",
            "File quarterly taxes",
            Priority::High,
            Status::Open,
            Some("Personal Todos"),
            vec!["finance"],
            Some(today - Duration::days(3)),
            Some("codywright"),
            now,
        ),
        // Due today
        task_active(
            "task:due-today-call-client",
            "Call client about Montreal Album scope",
            Priority::High,
            Status::Open,
            Some("Montreal Album"),
            vec!["billable"],
            Some(today),
            Some("codywright"),
            now,
        ),
        // Scheduled future
        {
            let mut t = task_active(
                "task:scheduled-future-deploy",
                "Deploy v0.3.0",
                Priority::Normal,
                Status::Open,
                Some("Task App"),
                vec!["release"],
                None,
                Some("codywright"),
                now,
            );
            t.scheduled = Set(Some(today + Duration::days(10)));
            t
        },
        // Running timer
        {
            let mut t = task_active(
                "task:running-timer-perf-pass",
                "Profile slow query path",
                Priority::Normal,
                Status::InProgress,
                Some("Task App"),
                vec!["perf"],
                None,
                Some("codywright"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-perf-pass-1".to_string(),
                user: Some("codywright".to_string()),
                start_time: now - Duration::minutes(45),
                end_time: None,
                ..Default::default()
            }]));
            t
        },
        // Billable mix completed entry
        {
            let mut t = task_active(
                "task:billable-mix-mastering",
                "Master track 3",
                Priority::Normal,
                Status::InProgress,
                Some("Montreal Album"),
                vec!["billable", "audio"],
                Some(today + Duration::days(4)),
                Some("codywright"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-mastering-1".to_string(),
                user: Some("codywright".to_string()),
                start_time: now - Duration::hours(3),
                end_time: Some(now - Duration::hours(1)),
                billable: true,
                billable_rate: Some(15000),
                description: Some("Mastering pass + reference compare".to_string()),
                ..Default::default()
            }]));
            t
        },
        // Recurring
        {
            let mut t = task_active(
                "task:weekly-recurring-standup",
                "Weekly team standup notes",
                Priority::Low,
                Status::Open,
                Some("Task App"),
                vec!["recurring"],
                None,
                Some("codywright"),
                now,
            );
            t.recurrence = Set(Some("FREQ=WEEKLY;BYDAY=MO".to_string()));
            t.recurrence_anchor = Set(RecurrenceAnchor::Scheduled);
            t
        },
        // Inbox
        {
            let mut t = task_active(
                "task:inbox-misc-idea",
                "Read article about effect-ts",
                Priority::Low,
                Status::Open,
                None,
                vec!["inbox"],
                None,
                None,
                now,
            );
            t.issue_type = Set(Some("inbox".to_string()));
            t
        },
        {
            let mut t = task_active(
                "task:inbox-research-loro",
                "Research Loro snapshot serialization",
                Priority::Normal,
                Status::Open,
                None,
                vec!["inbox", "research"],
                None,
                None,
                now,
            );
            t.issue_type = Set(Some("inbox".to_string()));
            t
        },
        // Done
        {
            let mut t = task_active(
                "task:done-publish-blog",
                "Publish blog post on SQLite migration",
                Priority::Normal,
                Status::Done,
                Some("Task App"),
                vec!["docs"],
                None,
                Some("codywright"),
                now,
            );
            t.completed_date = Set(Some(today - Duration::days(2)));
            t
        },
        {
            let mut t = task_active(
                "task:done-q1-review",
                "Q1 review writeup",
                Priority::Normal,
                Status::Done,
                Some("Personal Todos"),
                vec![],
                None,
                Some("codywright"),
                now,
            );
            t.completed_date = Set(Some(today - Duration::days(14)));
            t
        },
        // Cancelled
        task_with_status(
            "task:cancelled-old-spike",
            "Spike abandoned auth proposal",
            Status::Cancelled,
            Some("CRDT Research Spike"),
            now,
        ),
        // Rich body
        {
            let mut t = task_active(
                "task:body-rich-spec",
                "Write spec: realtime sync v2",
                Priority::High,
                Status::InProgress,
                Some("Task App"),
                vec!["spec"],
                Some(today + Duration::days(7)),
                Some("codywright"),
                now,
            );
            t.body = Set(
                "## Goals\n\n- Authoritative server\n- Optimistic clients\n- Conflict resolution\n\n## Open questions\n\n- Snapshot vs delta?\n".to_string(),
            );
            t
        },
    ];

    for t in tasks {
        let id = match &t.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if task::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.tasks_unchanged += 1;
        } else {
            task::Entity::insert(t).exec(db).await?;
            summary.tasks_created += 1;
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn task_active(
    key: &str,
    title: &str,
    priority: Priority,
    status: Status,
    project: Option<&str>,
    tags: Vec<&str>,
    due: Option<NaiveDate>,
    assignee: Option<&str>,
    now: chrono::DateTime<Utc>,
) -> task::ActiveModel {
    task::ActiveModel {
        id: Set(demo_id(key)),
        title: Set(title.to_string()),
        priority: Set(priority),
        status: Set(status),
        projects: Set(project
            .map(|p| WikiLinkList::from(vec![WikiLink(p.to_string())]))
            .unwrap_or_default()),
        contexts: Set(StringList::default()),
        tags: Set(StringList::from(
            tags.into_iter().map(|t| t.to_string()).collect::<Vec<_>>(),
        )),
        areas: Set(WikiLinkList::default()),
        due: Set(due),
        scheduled: Set(None),
        date_created: Set(Some(now)),
        date_modified: Set(Some(now)),
        time_entries: Set(TimeEntryList::default()),
        completed_instances: Set(StringList::default()),
        skipped_instances: Set(StringList::default()),
        blocked_by: Set(TaskDependencyList::default()),
        blocking: Set(WikiLinkList::default()),
        reminders: Set(ReminderList::default()),
        assignee: Set(assignee.map(str::to_string)),
        assignees: Set(StringList::default()),
        created_by: Set(Some("codywright".to_string())),
        relations: Set(TaskRelationList::default()),
        subscribers: Set(StringList::default()),
        reactions: Set(Default::default()),
        is_draft: Set(false),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        recurrence_anchor: Set(RecurrenceAnchor::Scheduled),
        body: Set(String::new()),
        ..Default::default()
    }
}

fn task_with_status(
    key: &str,
    title: &str,
    status: Status,
    project: Option<&str>,
    now: chrono::DateTime<Utc>,
) -> task::ActiveModel {
    task_active(
        key,
        title,
        Priority::Normal,
        status,
        project,
        vec![],
        None,
        None,
        now,
    )
}

// ── Calendar event fixtures ─────────────────────────────────────────────────

async fn seed_calendar_events(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let events = [
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:standup-today")),
            id: Set(Some("standup-today".to_string())),
            title: Set("Daily standup".to_string()),
            start: Set(now.date_naive().and_hms_opt(9, 30, 0).unwrap().and_utc()),
            end: Set(Some(
                now.date_naive().and_hms_opt(9, 45, 0).unwrap().and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            attendees: Set(StringList::from(vec![
                "codywright".to_string(),
                "amywright".to_string(),
            ])),
            spaces: Set(WikiLinkList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:client-meeting-tomorrow")),
            id: Set(Some("client-mtg".to_string())),
            title: Set("Client check-in: Montreal Album".to_string()),
            start: Set((now + Duration::days(1))
                .date_naive()
                .and_hms_opt(15, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now + Duration::days(1))
                    .date_naive()
                    .and_hms_opt(16, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:past-retro")),
            id: Set(Some("retro-past".to_string())),
            title: Set("Sprint retro".to_string()),
            start: Set((now - Duration::days(7))
                .date_naive()
                .and_hms_opt(14, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now - Duration::days(7))
                    .date_naive()
                    .and_hms_opt(15, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(false),
            status: Set(CalendarEventStatus::Confirmed),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
        calendar_event::ActiveModel {
            uuid: Set(demo_id("event:offsite-allday")),
            id: Set(Some("offsite".to_string())),
            title: Set("Team offsite".to_string()),
            start: Set((now + Duration::days(20))
                .date_naive()
                .and_hms_opt(0, 0, 0)
                .unwrap()
                .and_utc()),
            end: Set(Some(
                (now + Duration::days(21))
                    .date_naive()
                    .and_hms_opt(0, 0, 0)
                    .unwrap()
                    .and_utc(),
            )),
            all_day: Set(true),
            status: Set(CalendarEventStatus::Tentative),
            spaces: Set(WikiLinkList::default()),
            attendees: Set(StringList::default()),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
            ..Default::default()
        },
    ];

    for e in events {
        let id = match &e.uuid {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if calendar_event::Entity::find_by_id(id)
            .one(db)
            .await?
            .is_some()
        {
            summary.calendar_events_unchanged += 1;
        } else {
            calendar_event::Entity::insert(e).exec(db).await?;
            summary.calendar_events_created += 1;
        }
    }
    Ok(())
}

// ── People fixtures ─────────────────────────────────────────────────────────

async fn seed_people(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let now = Utc::now();
    let people = [
        people::ActiveModel {
            uuid: Set(demo_id("person:cody-wright")),
            id: Set(Some("codywright".to_string())),
            display_name: Set("Cody Wright".to_string()),
            given_name: Set(Some("Cody".to_string())),
            family_name: Set(Some("Wright".to_string())),
            organization: Set(Some("FastTrack Studios".to_string())),
            title: Set(Some("Engineer".to_string())),
            contact_methods: Set(ContactMethodList::from(vec![
                ContactMethod {
                    kind: "email".to_string(),
                    value: "cody@fasttrackaudio.com".to_string(),
                    label: Some("work".to_string()),
                    primary: true,
                },
                ContactMethod {
                    kind: "phone".to_string(),
                    value: "+1-555-0100".to_string(),
                    label: Some("mobile".to_string()),
                    primary: false,
                },
            ])),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:amy-wright")),
            id: Set(Some("amywright".to_string())),
            display_name: Set("Amy Wright".to_string()),
            given_name: Set(Some("Amy".to_string())),
            family_name: Set(Some("Wright".to_string())),
            organization: Set(Some("FastTrack Studios".to_string())),
            contact_methods: Set(ContactMethodList::from(vec![ContactMethod {
                kind: "email".to_string(),
                value: "amy@fasttrackaudio.com".to_string(),
                label: None,
                primary: true,
            }])),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:tom-brooks")),
            id: Set(Some("tombrooks".to_string())),
            display_name: Set("Tom Brooks".to_string()),
            given_name: Set(Some("Tom".to_string())),
            family_name: Set(Some("Brooks".to_string())),
            contact_methods: Set(ContactMethodList::default()),
            provider_refs: Set(ProviderRefList::from(vec![ProviderRef {
                provider: "nextcloud".to_string(),
                account: Some("starcommand".to_string()),
                collection: Some("addressbook-default".to_string()),
                href: None,
                etag: None,
                uid: Some("tombrooks-uid".to_string()),
            }])),
            notes: Set(Some("Sound engineer, Campus Jax shows.".to_string())),
            ..Default::default()
        },
        people::ActiveModel {
            uuid: Set(demo_id("person:carter-whitlock")),
            id: Set(Some("carterwhitlock".to_string())),
            display_name: Set("Carter Whitlock".to_string()),
            contact_methods: Set(ContactMethodList::default()),
            provider_refs: Set(ProviderRefList::default()),
            notes: Set(None),
            ..Default::default()
        },
    ];

    for p in people {
        let id = match &p.uuid {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if people::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.people_unchanged += 1;
        } else {
            people::Entity::insert(p).exec(db).await?;
            summary.people_created += 1;
        }
    }
    let _ = now;
    Ok(())
}

// ── Comment fixtures ────────────────────────────────────────────────────────

async fn seed_comments(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug_id = demo_id("task:fix-auth-bug");
    let design_id = demo_id("task:design-portal-ui");

    let comments = [
        comment::ActiveModel {
            id: Set(demo_id("comment:auth-bug-1")),
            entity_id: Set(auth_bug_id),
            entity_type: Set("task".to_string()),
            author: Set("codywright".to_string()),
            body: Set("Repro'd: session cookie expires after 1h instead of 24h.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(None),
            resolved: Set(false),
            resolved_by: Set(None),
            mentions: Set(serde_json::json!([]).into()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::hours(6)),
            updated_at: Set(now - Duration::hours(6)),
        },
        comment::ActiveModel {
            id: Set(demo_id("comment:auth-bug-2-reply")),
            entity_id: Set(auth_bug_id),
            entity_type: Set("task".to_string()),
            author: Set("amywright".to_string()),
            body: Set("Likely the JWT exp claim — taking a look now.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(Some(demo_id("comment:auth-bug-1"))),
            resolved: Set(false),
            resolved_by: Set(None),
            mentions: Set(serde_json::json!([]).into()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::hours(5)),
            updated_at: Set(now - Duration::hours(5)),
        },
        comment::ActiveModel {
            id: Set(demo_id("comment:design-resolved")),
            entity_id: Set(design_id),
            entity_type: Set("task".to_string()),
            author: Set("amywright".to_string()),
            body: Set("Mocks attached, signed off.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(None),
            resolved: Set(true),
            resolved_by: Set(Some("codywright".to_string())),
            mentions: Set(serde_json::json!([]).into()),
            external_id: Set(None),
            deleted_at: Set(None),
            created_at: Set(now - Duration::days(1)),
            updated_at: Set(now - Duration::days(1)),
        },
    ];

    for c in comments {
        let id = match &c.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if comment::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.comments_unchanged += 1;
        } else {
            comment::Entity::insert(c).exec(db).await?;
            summary.comments_created += 1;
        }
    }
    Ok(())
}
