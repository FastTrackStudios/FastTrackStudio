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

use task_core::activity;
use task_core::attachment;
use task_core::calendar_event::{self, CalendarEventStatus};
use task_core::comment;
use task_core::cycle::{self, CycleStatus, CycleTaskList};
use task_core::email;
use task_core::email::EmailStringList;
use task_core::expense::{self, ExpenseStatus};
use task_core::invoice::{self, InvoiceLine, InvoiceLineList, InvoiceStatus, Payment, PaymentList};
use task_core::notification;
use task_core::people::{self, ContactMethod, ContactMethodList, ProviderRef, ProviderRefList};
use task_core::project::{self, ProjectStatus};
use task_core::reaction;
use task_core::task::{
    self, EmailRefList, Priority, RecurrenceAnchor, ReminderList, Status, StringList,
    TaskDependencyList, TaskRelationList, TimeEntry, TimeEntryList, WikiLink, WikiLinkList,
};
use task_core::views::{self, ViewDisplay, ViewFilters};

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
    pub reactions_created: usize,
    pub reactions_unchanged: usize,
    pub notifications_created: usize,
    pub notifications_unchanged: usize,
    pub saved_views_created: usize,
    pub saved_views_unchanged: usize,
    pub cycles_created: usize,
    pub cycles_unchanged: usize,
    pub activities_created: usize,
    pub activities_unchanged: usize,
    pub expenses_created: usize,
    pub expenses_unchanged: usize,
    pub invoices_created: usize,
    pub invoices_unchanged: usize,
    pub email_refs_created: usize,
    pub email_refs_unchanged: usize,
    pub attachments_created: usize,
    pub attachments_unchanged: usize,
}

impl DemoSeedSummary {
    pub fn total_created(&self) -> usize {
        self.projects_created
            + self.tasks_created
            + self.calendar_events_created
            + self.people_created
            + self.comments_created
            + self.reactions_created
            + self.notifications_created
            + self.saved_views_created
            + self.cycles_created
            + self.activities_created
            + self.expenses_created
            + self.invoices_created
            + self.email_refs_created
            + self.attachments_created
    }

    pub fn total_unchanged(&self) -> usize {
        self.projects_unchanged
            + self.tasks_unchanged
            + self.calendar_events_unchanged
            + self.people_unchanged
            + self.comments_unchanged
            + self.reactions_unchanged
            + self.notifications_unchanged
            + self.saved_views_unchanged
            + self.cycles_unchanged
            + self.activities_unchanged
            + self.expenses_unchanged
            + self.invoices_unchanged
            + self.email_refs_unchanged
            + self.attachments_unchanged
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
    seed_reactions(db, &mut summary).await?;
    seed_notifications(db, &mut summary).await?;
    seed_saved_views(db, &mut summary).await?;
    seed_cycles(db, &mut summary).await?;
    seed_activities(db, &mut summary).await?;
    seed_expenses(db, &mut summary).await?;
    seed_invoices(db, &mut summary).await?;
    seed_email_refs(db, &mut summary).await?;
    seed_attachments(db, &mut summary).await?;
    Ok(summary)
}

/// Delete every row created by [`seed_demo_data`] (by deterministic id).
pub async fn reset_demo_data(db: &DatabaseConnection) -> Result<DemoSeedSummary, DbErr> {
    let mut summary = DemoSeedSummary::default();

    for key in ATTACHMENT_KEYS {
        let id = demo_id(key);
        if attachment::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.attachments_created += 1;
        }
    }
    for key in EMAIL_REF_KEYS {
        let id = demo_id(key);
        if email::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.email_refs_created += 1;
        }
    }
    for key in INVOICE_KEYS {
        let id = demo_id(key);
        if invoice::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.invoices_created += 1;
        }
    }
    for key in EXPENSE_KEYS {
        let id = demo_id(key);
        if expense::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.expenses_created += 1;
        }
    }
    for key in ACTIVITY_KEYS {
        let id = demo_id(key);
        if activity::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.activities_created += 1;
        }
    }
    for key in CYCLE_KEYS {
        let id = demo_id(key);
        if cycle::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.cycles_created += 1;
        }
    }
    for key in SAVED_VIEW_KEYS {
        let id = demo_id(key);
        if views::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.saved_views_created += 1;
        }
    }
    for key in NOTIFICATION_KEYS {
        let id = demo_id(key);
        if notification::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.notifications_created += 1;
        }
    }
    for key in REACTION_KEYS {
        let id = demo_id(key);
        if reaction::Entity::delete_by_id(id)
            .exec(db)
            .await?
            .rows_affected
            > 0
        {
            summary.reactions_created += 1;
        }
    }
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

const REACTION_KEYS: &[&str] = &[
    "reaction:fix-auth-thumbs-cody",
    "reaction:design-portal-fire-amy",
    "reaction:billable-mix-tada-cody",
    "reaction:auth-bug-comment-thumbs-cody",
];

const NOTIFICATION_KEYS: &[&str] = &[
    "notification:assigned-fix-auth",
    "notification:mentioned-design-portal",
    "notification:overdue-tax-filing",
];

const SAVED_VIEW_KEYS: &[&str] = &["view:my-today", "view:inbox-triage"];

const CYCLE_KEYS: &[&str] = &["cycle:sprint-2026-w19"];

const ACTIVITY_KEYS: &[&str] = &[
    "activity:fix-auth-created",
    "activity:fix-auth-status-inprogress",
    "activity:design-portal-created",
    "activity:design-portal-resolved",
    "activity:billable-mix-time-logged",
    "activity:done-publish-blog-completed",
    "activity:done-q1-review-completed",
    "activity:tour-book-venues-priority",
    "activity:venue-input-list-created",
    "activity:running-timer-perf-pass-started",
];

const EXPENSE_KEYS: &[&str] = &[
    "expense:montreal-studio-rental",
    "expense:montreal-mastering-software",
    "expense:tour-van-deposit",
    "expense:tour-printed-merch",
    "expense:misc-coffee-meeting",
];

const INVOICE_KEYS: &[&str] = &["invoice:montreal-mar", "invoice:tom-ep-feb-paid"];

const ATTACHMENT_KEYS: &[&str] = &[
    "attachment:montreal-master-wav",
    "attachment:montreal-stems-zip",
    "attachment:tom-ep-mix-notes-pdf",
    "attachment:tour-stage-plot-png",
];

const EMAIL_REF_KEYS: &[&str] = &[
    "email:fix-auth-stack-trace",
    "email:montreal-client-revisions",
    "email:tour-venue-confirmation",
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
            properties: Set(Default::default()),
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
            properties: Set(Default::default()),
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
            properties: Set(Default::default()),
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

// ── Reaction fixtures ───────────────────────────────────────────────────────

async fn seed_reactions(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let billable = demo_id("task:billable-mix-mastering");
    let auth_comment = demo_id("comment:auth-bug-1");

    let reactions = [
        reaction::ActiveModel {
            id: Set(demo_id("reaction:fix-auth-thumbs-cody")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            emoji: Set("👍".to_string()),
            user: Set("cody".to_string()),
            created_at: Set(now - Duration::hours(2)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:design-portal-fire-amy")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            emoji: Set("🔥".to_string()),
            user: Set("amy".to_string()),
            created_at: Set(now - Duration::hours(3)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:billable-mix-tada-cody")),
            entity_id: Set(billable),
            entity_type: Set("task".to_string()),
            emoji: Set("🎉".to_string()),
            user: Set("cody".to_string()),
            created_at: Set(now - Duration::hours(1)),
        },
        reaction::ActiveModel {
            id: Set(demo_id("reaction:auth-bug-comment-thumbs-cody")),
            entity_id: Set(auth_comment),
            entity_type: Set("comment".to_string()),
            emoji: Set("👍".to_string()),
            user: Set("amy".to_string()),
            created_at: Set(now - Duration::hours(4)),
        },
    ];

    for r in reactions {
        let id = match &r.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if reaction::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.reactions_unchanged += 1;
        } else {
            reaction::Entity::insert(r).exec(db).await?;
            summary.reactions_created += 1;
        }
    }
    Ok(())
}

// ── Notification fixtures ───────────────────────────────────────────────────

async fn seed_notifications(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let overdue_tax = demo_id("task:overdue-tax-filing");

    let notifications = [
        // Unread — assignment notification
        notification::ActiveModel {
            id: Set(demo_id("notification:assigned-fix-auth")),
            recipient: Set("cody".to_string()),
            kind: Set("assigned".to_string()),
            message: Set("You were assigned: Fix login session timeout".to_string()),
            actor: Set(Some("amy".to_string())),
            entity_id: Set(Some(auth_bug)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Task App".to_string())),
            read_at: Set(None),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::hours(8)),
        },
        // Read
        notification::ActiveModel {
            id: Set(demo_id("notification:mentioned-design-portal")),
            recipient: Set("cody".to_string()),
            kind: Set("mention".to_string()),
            message: Set("Amy mentioned you in: Design portal landing page".to_string()),
            actor: Set(Some("amy".to_string())),
            entity_id: Set(Some(design)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Task App".to_string())),
            read_at: Set(Some(now - Duration::hours(20))),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::days(1)),
        },
        // Read — system overdue reminder
        notification::ActiveModel {
            id: Set(demo_id("notification:overdue-tax-filing")),
            recipient: Set("cody".to_string()),
            kind: Set("overdue".to_string()),
            message: Set("Task is overdue: File quarterly taxes".to_string()),
            actor: Set(None),
            entity_id: Set(Some(overdue_tax)),
            entity_type: Set(Some("task".to_string())),
            project: Set(Some("Personal Todos".to_string())),
            read_at: Set(Some(now - Duration::hours(2))),
            snoozed_till: Set(None),
            created_at: Set(now - Duration::days(2)),
        },
    ];

    for n in notifications {
        let id = match &n.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if notification::Entity::find_by_id(id)
            .one(db)
            .await?
            .is_some()
        {
            summary.notifications_unchanged += 1;
        } else {
            notification::Entity::insert(n).exec(db).await?;
            summary.notifications_created += 1;
        }
    }
    Ok(())
}

// ── Saved-view fixtures ─────────────────────────────────────────────────────

async fn seed_saved_views(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive().to_string();

    let my_today_filters = ViewFilters {
        assignee: vec!["cody".to_string()],
        status: vec!["open".to_string(), "in_progress".to_string()],
        due_before: Some(today.clone()),
        ..Default::default()
    };
    let my_today_display = ViewDisplay {
        layout: Some("list".to_string()),
        order_by: Some("priority".to_string()),
        order_direction: Some("desc".to_string()),
        ..Default::default()
    };

    let inbox_filters = ViewFilters {
        tags: vec!["inbox".to_string()],
        ..Default::default()
    };
    let inbox_display = ViewDisplay {
        layout: Some("list".to_string()),
        order_by: Some("created".to_string()),
        order_direction: Some("desc".to_string()),
        ..Default::default()
    };

    let saved_views = [
        views::ActiveModel {
            id: Set(demo_id("view:my-today")),
            title: Set("My Today".to_string()),
            description: Set(Some(
                "Open + in-progress tasks assigned to me, due today or earlier.".to_string(),
            )),
            project: Set(None),
            filters: Set(my_today_filters),
            display: Set(my_today_display),
            created_by: Set(Some("cody".to_string())),
            is_shared: Set(false),
            sort_order: Set(Some(1.0)),
            created_at: Set(now - Duration::days(30)),
            updated_at: Set(now - Duration::days(1)),
        },
        views::ActiveModel {
            id: Set(demo_id("view:inbox-triage")),
            title: Set("Inbox Triage".to_string()),
            description: Set(Some(
                "Untriaged inbox items needing project routing.".to_string(),
            )),
            project: Set(None),
            filters: Set(inbox_filters),
            display: Set(inbox_display),
            created_by: Set(Some("cody".to_string())),
            is_shared: Set(true),
            sort_order: Set(Some(2.0)),
            created_at: Set(now - Duration::days(60)),
            updated_at: Set(now - Duration::days(7)),
        },
    ];

    for v in saved_views {
        let id = match &v.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if views::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.saved_views_unchanged += 1;
        } else {
            views::Entity::insert(v).exec(db).await?;
            summary.saved_views_created += 1;
        }
    }
    Ok(())
}

// ── Cycle fixtures ──────────────────────────────────────────────────────────

async fn seed_cycles(db: &DatabaseConnection, summary: &mut DemoSeedSummary) -> Result<(), DbErr> {
    let today = Utc::now().date_naive();
    let cycles = [cycle::ActiveModel {
        id: Set(demo_id("cycle:sprint-2026-w19")),
        title: Set("Sprint 2026-W19".to_string()),
        description: Set(Some(
            "Mid-quarter sprint focused on auth + portal UI.".to_string(),
        )),
        start_date: Set(Some(today - Duration::days(7))),
        end_date: Set(Some(today + Duration::days(7))),
        owned_by: Set(Some("cody".to_string())),
        tasks: Set(CycleTaskList::from(vec![
            "Fix login session timeout".to_string(),
            "Design portal landing page".to_string(),
            "Add integration tests for realtime".to_string(),
            "Profile slow query path".to_string(),
        ])),
        status: Set(CycleStatus::Active),
        total_tasks: Set(Some(4)),
        completed_tasks: Set(Some(0)),
        sort_order: Set(Some(1.0)),
    }];

    for c in cycles {
        let id = match &c.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if cycle::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.cycles_unchanged += 1;
        } else {
            cycle::Entity::insert(c).exec(db).await?;
            summary.cycles_created += 1;
        }
    }
    Ok(())
}

// ── Activity-log fixtures ───────────────────────────────────────────────────

async fn seed_activities(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let auth_bug = demo_id("task:fix-auth-bug");
    let design = demo_id("task:design-portal-ui");
    let billable = demo_id("task:billable-mix-mastering");
    let done_blog = demo_id("task:done-publish-blog");
    let done_q1 = demo_id("task:done-q1-review");
    let tour_book = demo_id("task:tour-book-venues");
    let venue_input = demo_id("task:venue-input-list");
    let perf_pass = demo_id("task:running-timer-perf-pass");

    let activities = [
        activity::ActiveModel {
            id: Set(demo_id("activity:fix-auth-created")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(6)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:fix-auth-status-inprogress")),
            entity_id: Set(auth_bug),
            entity_type: Set("task".to_string()),
            verb: Set("updated".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("open".to_string())),
            new_value: Set(Some("in_progress".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(5)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:design-portal-created")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(4)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:design-portal-resolved")),
            entity_id: Set(design),
            entity_type: Set("task".to_string()),
            verb: Set("commented".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(Some("Mocks attached, signed off.".to_string())),
            actor: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(1)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:billable-mix-time-logged")),
            entity_id: Set(billable),
            entity_type: Set("task".to_string()),
            verb: Set("time_logged".to_string()),
            field: Set(Some("time_entries".to_string())),
            old_value: Set(None),
            new_value: Set(Some("120m".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::hours(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:done-publish-blog-completed")),
            entity_id: Set(done_blog),
            entity_type: Set("task".to_string()),
            verb: Set("completed".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("in_progress".to_string())),
            new_value: Set(Some("done".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:done-q1-review-completed")),
            entity_id: Set(done_q1),
            entity_type: Set("task".to_string()),
            verb: Set("completed".to_string()),
            field: Set(Some("status".to_string())),
            old_value: Set(Some("open".to_string())),
            new_value: Set(Some("done".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(7)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:tour-book-venues-priority")),
            entity_id: Set(tour_book),
            entity_type: Set("task".to_string()),
            verb: Set("updated".to_string()),
            field: Set(Some("priority".to_string())),
            old_value: Set(Some("normal".to_string())),
            new_value: Set(Some("high".to_string())),
            actor: Set(Some("bri".to_string())),
            created_at: Set(now - Duration::days(3)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:venue-input-list-created")),
            entity_id: Set(venue_input),
            entity_type: Set("task".to_string()),
            verb: Set("created".to_string()),
            field: Set(None),
            old_value: Set(None),
            new_value: Set(None),
            actor: Set(Some("carter".to_string())),
            created_at: Set(now - Duration::days(2)),
        },
        activity::ActiveModel {
            id: Set(demo_id("activity:running-timer-perf-pass-started")),
            entity_id: Set(perf_pass),
            entity_type: Set("task".to_string()),
            verb: Set("time_started".to_string()),
            field: Set(Some("time_entries".to_string())),
            old_value: Set(None),
            new_value: Set(Some("running".to_string())),
            actor: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::minutes(45)),
        },
    ];

    for a in activities {
        let id = match &a.id {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if activity::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.activities_unchanged += 1;
        } else {
            activity::Entity::insert(a).exec(db).await?;
            summary.activities_created += 1;
        }
    }
    Ok(())
}

// ── Expense fixtures ────────────────────────────────────────────────────────

async fn seed_expenses(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let expenses = [
        expense::ActiveModel {
            uuid: Set(demo_id("expense:montreal-studio-rental")),
            id: Set("EXP-2026-0001".to_string()),
            number: Set(1),
            status: Set(ExpenseStatus::Open),
            date: Set(today - Duration::days(10)),
            amount_cents: Set(85000),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Montreal Album".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("studio".to_string())),
            vendor: Set(Some("Eastside Audio".to_string())),
            description: Set("Studio day rate — vocals tracking".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(10))),
            date_modified: Set(Some(now - Duration::days(10))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:montreal-mastering-software")),
            id: Set("EXP-2026-0002".to_string()),
            number: Set(2),
            status: Set(ExpenseStatus::Paid),
            date: Set(today - Duration::days(20)),
            amount_cents: Set(29900),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Montreal Album".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("software".to_string())),
            vendor: Set(Some("FabFilter".to_string())),
            description: Set("Pro-L 2 mastering limiter license".to_string()),
            receipt: Set(None),
            reference: Set(Some("CC-2025-1129".to_string())),
            reimbursable: Set(false),
            notes: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(20))),
            date_modified: Set(Some(now - Duration::days(20))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:tour-van-deposit")),
            id: Set("EXP-2026-0003".to_string()),
            number: Set(3),
            status: Set(ExpenseStatus::Open),
            date: Set(today - Duration::days(2)),
            amount_cents: Set(150000),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Just Friends 2026 Tour".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("travel".to_string())),
            vendor: Set(Some("Sprinter Rentals".to_string())),
            description: Set("Sprinter van deposit (3 weeks)".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(None),
            created_by: Set(Some("bri".to_string())),
            date_created: Set(Some(now - Duration::days(2))),
            date_modified: Set(Some(now - Duration::days(2))),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:tour-printed-merch")),
            id: Set("EXP-2026-0004".to_string()),
            number: Set(4),
            status: Set(ExpenseStatus::Draft),
            date: Set(today),
            amount_cents: Set(67500),
            currency_code: Set("USD".to_string()),
            project: Set(Some(WikiLink("Just Friends 2026 Tour".to_string()))),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("merch".to_string())),
            vendor: Set(Some("Threadhouse".to_string())),
            description: Set("Tour t-shirt run — 100 units".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(true),
            notes: Set(Some("Awaiting design approval.".to_string())),
            created_by: Set(Some("amy".to_string())),
            date_created: Set(Some(now)),
            date_modified: Set(Some(now)),
            body: Set(String::new()),
        },
        expense::ActiveModel {
            uuid: Set(demo_id("expense:misc-coffee-meeting")),
            id: Set("EXP-2026-0005".to_string()),
            number: Set(5),
            status: Set(ExpenseStatus::Cancelled),
            date: Set(today - Duration::days(15)),
            amount_cents: Set(2400),
            currency_code: Set("USD".to_string()),
            project: Set(None),
            client: Set(None),
            deliverable: Set(None),
            category: Set(Some("meals".to_string())),
            vendor: Set(Some("Workshop Cafe".to_string())),
            description: Set("Coffee meeting — duplicate entry".to_string()),
            receipt: Set(None),
            reference: Set(None),
            reimbursable: Set(false),
            notes: Set(Some(
                "Voided — already filed under expense 0001.".to_string(),
            )),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(15))),
            date_modified: Set(Some(now - Duration::days(14))),
            body: Set(String::new()),
        },
    ];

    for e in expenses {
        let id = match &e.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if expense::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.expenses_unchanged += 1;
        } else {
            expense::Entity::insert(e).exec(db).await?;
            summary.expenses_created += 1;
        }
    }
    Ok(())
}

// ── Invoice fixtures ────────────────────────────────────────────────────────

async fn seed_invoices(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let today = now.date_naive();

    let montreal_lines = InvoiceLineList::from(vec![
        InvoiceLine {
            id: "line-1".to_string(),
            task_title: "Master track 3".to_string(),
            description: "Mastering pass + reference compare".to_string(),
            hours: 2.0,
            rate_cents: 15000,
            tax_rate_percent: None,
            discount_percent: None,
        },
        InvoiceLine {
            id: "line-2".to_string(),
            task_title: "Sequence album tracklist".to_string(),
            description: "Sequencing + crossfade prep".to_string(),
            hours: 1.5,
            rate_cents: 15000,
            tax_rate_percent: None,
            discount_percent: None,
        },
    ]);

    let tom_ep_lines = InvoiceLineList::from(vec![InvoiceLine {
        id: "line-1".to_string(),
        task_title: "Track bass for Tom's EP".to_string(),
        description: "Bass tracking + edits".to_string(),
        hours: 6.0,
        rate_cents: 12000,
        tax_rate_percent: None,
        discount_percent: None,
    }]);

    let tom_ep_payment = PaymentList::from(vec![Payment {
        id: "pay-1".to_string(),
        amount_cents: 72000,
        received_at: now - Duration::days(10),
        method: "ach".to_string(),
        reference: Some("ACH-20260214".to_string()),
        recorded_by: Some("cody".to_string()),
        notes: None,
    }]);

    let invoices = [
        invoice::ActiveModel {
            uuid: Set(demo_id("invoice:montreal-mar")),
            id: Set("INV-2026-0001".to_string()),
            number: Set(1),
            status: Set(InvoiceStatus::Sent),
            client: Set(WikiLink("Montreal Records".to_string())),
            issue_date: Set(today - Duration::days(5)),
            due_date: Set(today + Duration::days(25)),
            currency_code: Set("USD".to_string()),
            line_items: Set(montreal_lines),
            tax_rate_percent: Set(None),
            discount_percent: Set(None),
            po_number: Set(None),
            public_notes: Set(Some("Net 30. Thanks for the work!".to_string())),
            private_notes: Set(None),
            payments: Set(PaymentList::default()),
            entry_ids: Set(StringList::from(vec!["te-mastering-1".to_string()])),
            sent_at: Set(Some(now - Duration::days(5))),
            paid_at: Set(None),
            cancelled_at: Set(None),
            cancelled_reason: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(5))),
            date_modified: Set(Some(now - Duration::days(5))),
        },
        invoice::ActiveModel {
            uuid: Set(demo_id("invoice:tom-ep-feb-paid")),
            id: Set("INV-2026-0002".to_string()),
            number: Set(2),
            status: Set(InvoiceStatus::Paid),
            client: Set(WikiLink("TomBrooksMusic".to_string())),
            issue_date: Set(today - Duration::days(40)),
            due_date: Set(today - Duration::days(10)),
            currency_code: Set("USD".to_string()),
            line_items: Set(tom_ep_lines),
            tax_rate_percent: Set(None),
            discount_percent: Set(None),
            po_number: Set(None),
            public_notes: Set(None),
            private_notes: Set(None),
            payments: Set(tom_ep_payment),
            entry_ids: Set(StringList::default()),
            sent_at: Set(Some(now - Duration::days(40))),
            paid_at: Set(Some(now - Duration::days(10))),
            cancelled_at: Set(None),
            cancelled_reason: Set(None),
            created_by: Set(Some("cody".to_string())),
            date_created: Set(Some(now - Duration::days(40))),
            date_modified: Set(Some(now - Duration::days(10))),
        },
    ];

    for inv in invoices {
        let id = match &inv.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if invoice::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.invoices_unchanged += 1;
        } else {
            invoice::Entity::insert(inv).exec(db).await?;
            summary.invoices_created += 1;
        }
    }
    Ok(())
}

// ── Email-ref fixtures ──────────────────────────────────────────────────────

async fn seed_email_refs(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let emails = [
        email::ActiveModel {
            uuid: Set(demo_id("email:fix-auth-stack-trace")),
            message_id: Set("<auth-bug-trace@fasttrackstudio.com>".to_string()),
            subject: Set("[bug] Auth session timeout — stack trace".to_string()),
            from: Set("amy@fasttrackstudio.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "cody@fasttrackstudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(2)),
            snippet: Set(Some(
                "Reproduced again this morning — stack trace attached.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("INBOX".to_string())),
            imap_uid: Set(Some(1042)),
            nc_db_id: Set(Some(9001)),
            has_attachments: Set(true),
            attachment_count: Set(1),
            linked_by: Set(Some("amy".to_string())),
            linked_at: Set(Some(now - Duration::days(2))),
            user_tags: Set(EmailStringList::from(vec!["bug".to_string()])),
        },
        email::ActiveModel {
            uuid: Set(demo_id("email:montreal-client-revisions")),
            message_id: Set("<mtl-revisions-2@montrealrecords.com>".to_string()),
            subject: Set("Montreal Album — round 2 revisions".to_string()),
            from: Set("a&r@montrealrecords.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "cody@fasttrackaudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(3)),
            snippet: Set(Some(
                "A few notes on track 3 — see timestamps below.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("Clients/Montreal".to_string())),
            imap_uid: Set(Some(204)),
            nc_db_id: Set(Some(9002)),
            has_attachments: Set(false),
            attachment_count: Set(0),
            linked_by: Set(Some("cody".to_string())),
            linked_at: Set(Some(now - Duration::days(3))),
            user_tags: Set(EmailStringList::from(vec![
                "client".to_string(),
                "billable".to_string(),
            ])),
        },
        email::ActiveModel {
            uuid: Set(demo_id("email:tour-venue-confirmation")),
            message_id: Set("<venue-confirm-9281@campusjax.com>".to_string()),
            subject: Set("Campus Jax — show confirmation".to_string()),
            from: Set("booking@campusjax.com".to_string()),
            to: Set(EmailStringList::from(vec![
                "bri@fasttrackstudio.com".to_string(),
            ])),
            date: Set(now - Duration::days(1)),
            snippet: Set(Some(
                "Confirmed for the date — load-in starts at 4pm.".to_string(),
            )),
            account_id: Set(Some(1)),
            mailbox: Set(Some("INBOX".to_string())),
            imap_uid: Set(Some(2008)),
            nc_db_id: Set(Some(9003)),
            has_attachments: Set(false),
            attachment_count: Set(0),
            linked_by: Set(Some("bri".to_string())),
            linked_at: Set(Some(now - Duration::days(1))),
            user_tags: Set(EmailStringList::from(vec!["tour".to_string()])),
        },
    ];

    for e in emails {
        let id = match &e.uuid {
            Set(value) => *value,
            _ => unreachable!(),
        };
        if email::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.email_refs_unchanged += 1;
        } else {
            email::Entity::insert(e).exec(db).await?;
            summary.email_refs_created += 1;
        }
    }
    Ok(())
}

// ── Attachment fixtures ─────────────────────────────────────────────────────

async fn seed_attachments(
    db: &DatabaseConnection,
    summary: &mut DemoSeedSummary,
) -> Result<(), DbErr> {
    let now = Utc::now();
    let mix_master = demo_id("task:billable-mix-mastering");
    let track_seq = demo_id("task:montreal-track-sequencing");
    let tom_mixing = demo_id("task:tom-ep-mixing-collab");
    let tour_stage = demo_id("task:tour-stage-plot");

    let attachments = [
        attachment::ActiveModel {
            id: Set(demo_id("attachment:montreal-master-wav")),
            owner_id: Set(mix_master),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Montreal Album/masters/master-v3.wav".to_string()),
            label: Set(Some("master-v3.wav".to_string())),
            mime: Set(Some("audio/wav".to_string())),
            size_bytes: Set(Some(48_123_456)),
            checksum: Set(Some(
                "9f0c3f2c2b6e5b48b7c0a8d8f9a3b1e8d5c4a2b1e0f9d8c7b6a5f4e3d2c1b0a9".to_string(),
            )),
            uploader: Set(Some("cody".to_string())),
            created_at: Set(now - Duration::days(2)),
            updated_at: Set(now - Duration::days(2)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:montreal-stems-zip")),
            owner_id: Set(track_seq),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Montreal Album/stems/stems-bundle.zip".to_string()),
            label: Set(Some("stems-bundle.zip".to_string())),
            mime: Set(Some("application/zip".to_string())),
            size_bytes: Set(Some(312_456_789)),
            checksum: Set(Some(
                "1a2b3c4d5e6f70819293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f9".to_string(),
            )),
            uploader: Set(Some("amy".to_string())),
            created_at: Set(now - Duration::days(4)),
            updated_at: Set(now - Duration::days(4)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:tom-ep-mix-notes-pdf")),
            owner_id: Set(tom_mixing),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/Tom EP/notes/mix-notes-feb.pdf".to_string()),
            label: Set(Some("Mix notes — Feb".to_string())),
            mime: Set(Some("application/pdf".to_string())),
            size_bytes: Set(Some(184_320)),
            checksum: Set(Some(
                "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210".to_string(),
            )),
            uploader: Set(Some("tom".to_string())),
            created_at: Set(now - Duration::days(1)),
            updated_at: Set(now - Duration::days(1)),
        },
        attachment::ActiveModel {
            id: Set(demo_id("attachment:tour-stage-plot-png")),
            owner_id: Set(tour_stage),
            owner_type: Set("task".to_string()),
            source: Set("nextcloud".to_string()),
            path: Set("Projects/JF Tour 2026/plots/stage-plot-v1.png".to_string()),
            label: Set(Some("stage-plot-v1.png".to_string())),
            mime: Set(Some("image/png".to_string())),
            size_bytes: Set(Some(2_456_789)),
            checksum: Set(Some(
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".to_string(),
            )),
            uploader: Set(Some("bri".to_string())),
            created_at: Set(now - Duration::hours(8)),
            updated_at: Set(now - Duration::hours(8)),
        },
    ];

    for a in attachments {
        let id = match &a.id {
            Set(v) => *v,
            _ => unreachable!(),
        };
        if attachment::Entity::find_by_id(id).one(db).await?.is_some() {
            summary.attachments_unchanged += 1;
        } else {
            attachment::Entity::insert(a).exec(db).await?;
            summary.attachments_created += 1;
        }
    }
    Ok(())
}
