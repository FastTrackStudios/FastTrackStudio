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

// Org slugs match the auth-seed organizations in `apps/server/src/main.rs`
// (`seed_auth_data`). Cross-org collaboration is modeled by shared usernames
// across `Project::team` and `Task::assignees` — e.g. cody is a member of all
// five orgs, tom is in fta/fts/jf/tbm, marcus is in fta/tbm. A project
// owned by org A with assignees from org B + B's also-member-of-A users
// is a cross-org collab.
const ORG_FTA: &str = "fasttrackaudio";
const ORG_FTS: &str = "fasttrackstudio";
const ORG_JF: &str = "just-friends";
const ORG_TBM: &str = "tombrooksmusic";
const ORG_PERSONAL: &str = "personal";

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
    // Cross-org collaborations
    "project:tom-solo-ep",
    "project:band-tour-2026",
];

const TASK_KEYS: &[&str] = &[
    // Task App (FTS) — cody/tom/kai/luna
    "task:fix-auth-bug",
    "task:design-portal-ui",
    "task:write-readme",
    "task:write-tests",
    "task:onboard-luna",
    "task:scheduled-future-deploy",
    "task:running-timer-perf-pass",
    "task:weekly-recurring-standup",
    // Montreal Album (FTA) — cody/amy/carter
    "task:billable-mix-mastering",
    "task:due-today-call-client",
    "task:montreal-track-sequencing",
    // Personal (PERSONAL) — cody only
    "task:overdue-tax-filing",
    "task:body-rich-spec",
    // Inbox bucket — unrouted, no project
    "task:inbox-misc-idea",
    "task:inbox-research-loro",
    // Done
    "task:done-publish-blog",
    "task:done-q1-review",
    // Cancelled
    "task:cancelled-old-spike",
    // Tom solo EP (TBM, cross-org) — cody contributing as fta engineer
    "task:tom-ep-tracking-bass",
    "task:tom-ep-mixing-collab",
    "task:tom-ep-master-prep",
    // JF Tour 2026 (cross-org) — bri lead, tom + amy + carter from other orgs
    "task:tour-book-venues",
    "task:tour-stage-plot",
    "task:tour-merch-design",
    // Venue prep (JF) — bri lead
    "task:venue-input-list",
    "task:venue-runner-coordination",
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
        // FastTrackStudio (software org) — Task App, led by cody, Tom is a
        // contributor though his primary org is FTA. tom + cody overlap into
        // both fta/fts so this is a clean intra-org collaboration.
        {
            let mut p = project_base("project:task-app", "Task App", ProjectStatus::Active);
            p.description = Set(Some(
                "The task management system this codebase is.".to_string(),
            ));
            p.area = Set(Some("Engineering".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p.project_type = Set(Some("Product".to_string()));
            p.workflow = Set(Some("kanban".to_string()));
            p.workflow_stage = Set(Some("In Progress".to_string()));
            p.identifier = Set(Some("TASK".to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.default_assignee = Set(Some("cody".to_string()));
            p.emoji = Set(Some("🛠".to_string()));
            p.start = Set(Some(today - Duration::days(120)));
            p.due = Set(Some(today + Duration::days(60)));
            p.tags = Set(StringList::from(vec![
                "internal".to_string(),
                "product".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "tom".to_string(),
                "kai".to_string(),
                "luna".to_string(),
            ]));
            p
        },
        // FastTrackAudio (music) — Montreal Album, billable. cody/amy/carter.
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
            p.organization = Set(Some(ORG_FTA.to_string()));
            p.project_type = Set(Some("Audio".to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.default_assignee = Set(Some("cody".to_string()));
            p.emoji = Set(Some("🎧".to_string()));
            p.start = Set(Some(today - Duration::days(45)));
            p.due = Set(Some(today + Duration::days(30)));
            p.tags = Set(StringList::from(vec![
                "billable".to_string(),
                "client-work".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
            ]));
            p
        },
        // Just Friends (band) — venue prep. Cross-org by nature: cody/amy/
        // carter/tom/bri are all in jf, but tom's primary is fta + tbm.
        {
            let mut p = project_base(
                "project:venue-prep",
                "Campus Jax Show Prep",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_JF.to_string()));
            p.project_type = Set(Some("Event".to_string()));
            p.lead = Set(Some("bri".to_string()));
            p.default_assignee = Set(Some("bri".to_string()));
            p.emoji = Set(Some("🎤".to_string()));
            p.start = Set(Some(today));
            p.due = Set(Some(today + Duration::days(14)));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
                "tom".to_string(),
                "bri".to_string(),
            ]));
            p
        },
        // FTS — research spike, on hold.
        {
            let mut p = project_base(
                "project:on-hold-r-and-d",
                "CRDT Research Spike",
                ProjectStatus::OnHold,
            );
            p.area = Set(Some("Engineering".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.description = Set(Some("Loro CRDT integration exploration.".to_string()));
            p.team = Set(StringList::from(vec![
                "cody".to_string(),
                "kai".to_string(),
            ]));
            p
        },
        // FTS — archived retro from last quarter.
        {
            let mut p = project_base(
                "project:archived-2024",
                "2024 Q4 Retrospective",
                ProjectStatus::Archived,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_FTS.to_string()));
            p
        },
        // Personal — single-member.
        {
            let mut p = project_base(
                "project:personal-todo",
                "Personal Todos",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Personal".to_string()));
            p.organization = Set(Some(ORG_PERSONAL.to_string()));
            p.lead = Set(Some("cody".to_string()));
            p.emoji = Set(Some("📝".to_string()));
            p.team = Set(StringList::from(vec!["cody".to_string()]));
            p
        },
        // ── Cross-org collaboration #1 ─────────────────────────────────────
        // TomBrooksMusic owns this solo EP; cody (primary: fta) and marcus
        // (primary: fta) collaborate as engineers. Both are members of tbm
        // through the auth org-membership table, so this is genuine cross-org
        // work — fta engineers contracted into tbm's release.
        {
            let mut p = project_base(
                "project:tom-solo-ep",
                "Tom Brooks: Solo EP",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Music".to_string()));
            p.organization = Set(Some(ORG_TBM.to_string()));
            p.project_type = Set(Some("Audio".to_string()));
            p.lead = Set(Some("tom".to_string()));
            p.default_assignee = Set(Some("tom".to_string()));
            p.emoji = Set(Some("🎸".to_string()));
            p.start = Set(Some(today - Duration::days(20)));
            p.due = Set(Some(today + Duration::days(45)));
            p.tags = Set(StringList::from(vec![
                "billable".to_string(),
                "cross-org".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "tom".to_string(),
                "cody".to_string(),
                "marcus".to_string(),
            ]));
            p
        },
        // ── Cross-org collaboration #2 ─────────────────────────────────────
        // Just Friends planning a 2026 tour — coordinates across fta (cody/amy/
        // carter handling stage/sound) and tbm (tom doing setlist/logistics).
        // Bri (jf-only, tour manager) leads.
        {
            let mut p = project_base(
                "project:band-tour-2026",
                "Just Friends 2026 Tour",
                ProjectStatus::Active,
            );
            p.area = Set(Some("Operations".to_string()));
            p.organization = Set(Some(ORG_JF.to_string()));
            p.project_type = Set(Some("Event".to_string()));
            p.lead = Set(Some("bri".to_string()));
            p.default_assignee = Set(Some("bri".to_string()));
            p.emoji = Set(Some("🚐".to_string()));
            p.start = Set(Some(today + Duration::days(30)));
            p.due = Set(Some(today + Duration::days(180)));
            p.tags = Set(StringList::from(vec![
                "tour".to_string(),
                "cross-org".to_string(),
            ]));
            p.team = Set(StringList::from(vec![
                "bri".to_string(),
                "cody".to_string(),
                "amy".to_string(),
                "carter".to_string(),
                "tom".to_string(),
            ]));
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
            Some("cody"),
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
            Some("amy"),
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
            Some("cody"),
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
            Some("cody"),
            now,
        ),
        task_active(
            "task:onboard-luna",
            "Onboard Luna (FTS new hire)",
            Priority::Normal,
            Status::Open,
            Some("Task App"),
            vec!["ops"],
            Some(today + Duration::days(7)),
            Some("tom"),
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
            Some("cody"),
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
            Some("cody"),
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
                Some("cody"),
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
                Some("cody"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-perf-pass-1".to_string(),
                user: Some("cody".to_string()),
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
                Some("cody"),
                now,
            );
            t.time_entries = Set(TimeEntryList::from(vec![TimeEntry {
                id: "te-mastering-1".to_string(),
                user: Some("cody".to_string()),
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
                Some("cody"),
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
                Some("cody"),
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
                Some("cody"),
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
        // Rich body — kept on personal projects so it doesn't clutter the
        // Task App project tasks list.
        {
            let mut t = task_active(
                "task:body-rich-spec",
                "Write spec: realtime sync v2",
                Priority::High,
                Status::InProgress,
                Some("Personal Todos"),
                vec!["spec"],
                Some(today + Duration::days(7)),
                Some("cody"),
                now,
            );
            t.body = Set(
                "## Goals\n\n- Authoritative server\n- Optimistic clients\n- Conflict resolution\n\n## Open questions\n\n- Snapshot vs delta?\n".to_string(),
            );
            t
        },
        // ── Montreal Album: more depth ─────────────────────────────────────
        task_active(
            "task:montreal-track-sequencing",
            "Sequence album tracklist",
            Priority::Normal,
            Status::Open,
            Some("Montreal Album"),
            vec!["billable", "audio"],
            Some(today + Duration::days(10)),
            Some("amy"),
            now,
        ),
        // ── Tom Solo EP (TBM, cross-org) ───────────────────────────────────
        // tom = lead, cody (fta) tracks bass, marcus (fta) co-mixes.
        task_active(
            "task:tom-ep-tracking-bass",
            "Track bass for Tom's EP",
            Priority::Normal,
            Status::InProgress,
            Some("Tom Brooks: Solo EP"),
            vec!["billable", "cross-org", "audio"],
            Some(today + Duration::days(7)),
            Some("cody"),
            now,
        ),
        {
            let mut t = task_active(
                "task:tom-ep-mixing-collab",
                "Mix passes — split with Marcus",
                Priority::Normal,
                Status::Open,
                Some("Tom Brooks: Solo EP"),
                vec!["billable", "cross-org", "audio"],
                Some(today + Duration::days(20)),
                Some("marcus"),
                now,
            );
            t.assignees = Set(StringList::from(vec![
                "marcus".to_string(),
                "cody".to_string(),
            ]));
            t.subscribers = Set(StringList::from(vec!["tom".to_string()]));
            t
        },
        task_active(
            "task:tom-ep-master-prep",
            "Prepare reference mixes for mastering",
            Priority::Low,
            Status::Open,
            Some("Tom Brooks: Solo EP"),
            vec!["cross-org", "audio"],
            Some(today + Duration::days(35)),
            Some("tom"),
            now,
        ),
        // ── JF 2026 Tour (cross-org) ───────────────────────────────────────
        task_active(
            "task:tour-book-venues",
            "Book 12-city tour venues",
            Priority::High,
            Status::InProgress,
            Some("Just Friends 2026 Tour"),
            vec!["tour", "ops"],
            Some(today + Duration::days(21)),
            Some("bri"),
            now,
        ),
        {
            let mut t = task_active(
                "task:tour-stage-plot",
                "Draft stage plot + input list",
                Priority::Normal,
                Status::Open,
                Some("Just Friends 2026 Tour"),
                vec!["tour", "audio"],
                Some(today + Duration::days(40)),
                Some("carter"),
                now,
            );
            // Carter (jf+fta) is lead, but cody (fta) reviews; subscribe him.
            t.subscribers = Set(StringList::from(vec!["cody".to_string()]));
            t
        },
        task_active(
            "task:tour-merch-design",
            "Design tour merch (shirts, posters)",
            Priority::Low,
            Status::Open,
            Some("Just Friends 2026 Tour"),
            vec!["tour", "design"],
            Some(today + Duration::days(60)),
            Some("amy"),
            now,
        ),
        // ── Venue prep (JF) ────────────────────────────────────────────────
        task_active(
            "task:venue-input-list",
            "Confirm Campus Jax input list",
            Priority::High,
            Status::InProgress,
            Some("Campus Jax Show Prep"),
            vec!["audio"],
            Some(today + Duration::days(3)),
            Some("carter"),
            now,
        ),
        task_active(
            "task:venue-runner-coordination",
            "Coordinate day-of runners + load-in",
            Priority::Normal,
            Status::Open,
            Some("Campus Jax Show Prep"),
            vec!["ops"],
            Some(today + Duration::days(7)),
            Some("bri"),
            now,
        ),
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
        created_by: Set(Some("cody".to_string())),
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
                "cody".to_string(),
                "amy".to_string(),
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
            id: Set(Some("cody".to_string())),
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
            id: Set(Some("amy".to_string())),
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
            author: Set("cody".to_string()),
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
            author: Set("amy".to_string()),
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
            author: Set("amy".to_string()),
            body: Set("Mocks attached, signed off.".to_string()),
            time_start: Set(None),
            time_end: Set(None),
            reply_to: Set(None),
            resolved: Set(true),
            resolved_by: Set(Some("cody".to_string())),
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
