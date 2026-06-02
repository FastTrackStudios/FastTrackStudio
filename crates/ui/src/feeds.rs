//! Multi-org data feeds.
//!
//! Each fetcher takes the resolved list of org slugs (from
//! [`crate::orgs::selected_slugs`]) and fans out **concurrently** —
//! establishing each org's service client and concatenating the rows.
//! "All" mode passes every hosted slug; single-org mode passes one.
//! Per-org failures are tolerated in multi-org mode (a down/empty org
//! doesn't blank the whole view); an error only surfaces if *nothing*
//! came back.

#[cfg(target_arch = "wasm32")]
use project::ProjectInfo;
#[cfg(target_arch = "wasm32")]
use task::TaskInfo as DbTask;

/// Active projects across the selected orgs (concurrent fan-out).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_projects(slugs: &[String]) -> Result<Vec<ProjectInfo>, String> {
    let futs = slugs.iter().cloned().map(|slug| async move {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(&slug).await {
            Ok(client) => client
                .list()
                .await
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Tasks across the selected orgs (concurrent fan-out).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_tasks(slugs: &[String]) -> Result<Vec<DbTask>, String> {
    Ok(fetch_tasks_tagged(slugs)
        .await?
        .into_iter()
        .map(|(_, t)| t)
        .collect())
}

/// Tasks across the selected orgs, each paired with the slug of the org
/// it came from — so mutations can be routed back to the right org's
/// `TaskService` when viewing "All".
#[cfg(target_arch = "wasm32")]
pub async fn fetch_tasks_tagged(slugs: &[String]) -> Result<Vec<(String, DbTask)>, String> {
    let futs = slugs.iter().cloned().map(|slug| async move {
        match crate::vox_clients::establish_for::<task::TaskServiceClient>(&slug).await {
            Ok(client) => client
                .list()
                .await
                .map(|rows| {
                    rows.into_iter()
                        .map(|t| (slug.clone(), t))
                        .collect::<Vec<_>>()
                })
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Fetch one org's day-plan templates (drives the calendar schedule
/// overlay), in the order the backend lists them.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_day_templates(slug: &str) -> Result<Vec<scheduling_proto::DayTemplate>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayTemplatesClient>(slug).await?;
    client
        .list_day_templates()
        .await
        .map_err(|e| format!("{slug}: day templates: {e:?}"))
}

/// The saved per-date plan for `date` (ISO `YYYY-MM-DD`), or `None`
/// when the date hasn't been edited (caller materializes a default).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_day_plan(
    slug: &str,
    date: &str,
) -> Result<Option<scheduling_proto::DayPlan>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .get_day_plan(date.to_string())
        .await
        .map_err(|e| format!("{slug}: day plan {date}: {e:?}"))
}

/// Save (replacing) a per-date plan.
#[cfg(target_arch = "wasm32")]
pub async fn save_day_plan(slug: &str, plan: scheduling_proto::DayPlan) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .upsert_day_plan(plan)
        .await
        .map_err(|e| format!("{slug}: save day plan: {e:?}"))
}

/// Delete a per-date plan, reverting that date to the template.
#[cfg(target_arch = "wasm32")]
pub async fn delete_day_plan(slug: &str, date: &str) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .delete_day_plan(date.to_string())
        .await
        .map_err(|e| format!("{slug}: delete day plan {date}: {e:?}"))
}

/// All persisted calendar events for the org.
#[cfg(target_arch = "wasm32")]
pub async fn list_events(slug: &str) -> Result<Vec<scheduling_proto::CalEvent>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::CalendarEventsClient>(slug).await?;
    client
        .list_events()
        .await
        .map_err(|e| format!("{slug}: list events: {e:?}"))
}

/// Save (replacing) one calendar event.
#[cfg(target_arch = "wasm32")]
pub async fn upsert_event(slug: &str, event: scheduling_proto::CalEvent) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::CalendarEventsClient>(slug).await?;
    client
        .upsert_event(event)
        .await
        .map_err(|e| format!("{slug}: save event: {e:?}"))
}

/// Delete one calendar event.
#[cfg(target_arch = "wasm32")]
pub async fn delete_event(slug: &str, id: &str) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::CalendarEventsClient>(slug).await?;
    client
        .delete_event(id.to_string())
        .await
        .map_err(|e| format!("{slug}: delete event: {e:?}"))
}

// ── Bookings (Cal.com-style booking half) ───────────────────────────

/// All bookable event types for the org (30-min consults, etc.).
#[cfg(target_arch = "wasm32")]
pub async fn fetch_event_types(slug: &str) -> Result<Vec<scheduling_proto::EventType>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::EventTypesClient>(slug).await?;
    client
        .list_event_types()
        .await
        .map_err(|e| format!("{slug}: list event types: {e:?}"))
}

/// Create (upsert) a bookable event type. The backend derives the
/// vault `path` from the slug/id, so we leave it empty here.
#[cfg(target_arch = "wasm32")]
pub async fn create_event_type(slug: &str, title: &str, duration_min: u16) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::EventTypesClient>(slug).await?;
    let id = uuid::Uuid::new_v4().to_string();
    let url_slug = slugify(title);
    let event_type = scheduling_proto::EventType {
        path: String::new(),
        id: scheduling_proto::EventTypeId(id),
        title: title.to_owned(),
        slug: url_slug,
        description: None,
        duration_min,
        buffer_min: 0,
        location: scheduling_proto::EventTypeLocation::Tbd,
        schedule_id: None,
        published: true,
    };
    client
        .upsert_event_type(event_type)
        .await
        .map_err(|e| format!("{slug}: create event type: {e:?}"))
}

/// All bookings for the org (every status), oldest start first.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_bookings(slug: &str) -> Result<Vec<scheduling_proto::Booking>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::BookingsClient>(slug).await?;
    client
        .list_bookings()
        .await
        .map_err(|e| format!("{slug}: list bookings: {e:?}"))
}

/// Cancel a booking by id (sets status to `Cancelled`).
#[cfg(target_arch = "wasm32")]
pub async fn cancel_booking(slug: &str, id: &str) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::BookingsClient>(slug).await?;
    client
        .update_booking_status(
            scheduling_proto::BookingId(id.to_owned()),
            scheduling_proto::BookingStatus::Cancelled,
        )
        .await
        .map_err(|e| format!("{slug}: cancel booking: {e:?}"))
}

/// Lowercase, hyphenate, strip non-url-safe chars for an event-type slug.
#[cfg(target_arch = "wasm32")]
fn slugify(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_dash = false;
    for ch in s.trim().chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
            prev_dash = false;
        } else if !prev_dash && !out.is_empty() {
            out.push('-');
            prev_dash = true;
        }
    }
    out.trim_matches('-').to_owned()
}

/// Every inbox item (open + processed + archived), oldest first.
/// Consumers filter by `status` / `resurface_on` for the daily queue.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_inbox(slug: &str) -> Result<Vec<inbox_proto::InboxItem>, String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .list_inbox()
        .await
        .map_err(|e| format!("{slug}: list inbox: {e:?}"))
}

/// Capture or update one inbox item (keyed by id). Capture, snooze,
/// process, and archive all flow through here.
#[cfg(target_arch = "wasm32")]
pub async fn upsert_inbox_item(slug: &str, item: inbox_proto::InboxItem) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .upsert_inbox_item(item)
        .await
        .map_err(|e| format!("{slug}: save inbox item: {e:?}"))
}

/// Delete one inbox item.
#[cfg(target_arch = "wasm32")]
pub async fn delete_inbox_item(slug: &str, id: &str) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .delete_inbox_item(id.to_string())
        .await
        .map_err(|e| format!("{slug}: delete inbox item: {e:?}"))
}

/// Promote an inbox item into a Task — `title` is the headline, `details`
/// the markdown body. Returns the created task (its `path` is the
/// provenance back-link to store in `processed_into`).
#[cfg(target_arch = "wasm32")]
pub async fn create_task(slug: &str, title: &str, details: &str) -> Result<task::TaskInfo, String> {
    let client = crate::vox_clients::establish_for::<task::TaskServiceClient>(slug).await?;
    let t = task::TaskInfo {
        id: uuid::Uuid::nil(),
        path: String::new(),
        title: title.to_owned(),
        status: "open".into(),
        priority: "normal".into(),
        due: None,
        scheduled: None,
        tags: task::model::StringList(vec!["task".into()]),
        contexts: task::model::StringList::default(),
        projects: task::model::StringList::default(),
        project_id: None,
        milestone_id: None,
        time_estimate: None,
        time_entries: task::model::TimeEntries::default(),
        recurrence: None,
        recurrence_anchor: None,
        complete_instances: task::model::StringList::default(),
        completed_date: None,
        agent_profile: String::new(),
        dispatched_agent_tasks: task::model::StringList::default(),
        date_created: None,
        date_modified: None,
        details: details.to_owned(),
        workflow: None,
    };
    client
        .create(t)
        .await
        .map_err(|e| format!("{slug}: create task: {e:?}"))
}

/// Promote an inbox item into an atomic note: write `markdown` to
/// `path` (vault-relative, e.g. `Wiki/Atomic/<slug>.md`) in the org's
/// `"default"` vault. `CreateOnly` so a re-promote doesn't clobber.
#[cfg(target_arch = "wasm32")]
pub async fn create_wiki_note(slug: &str, path: &str, markdown: &str) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    client
        .put_file(
            "default".to_owned(),
            path.to_owned(),
            markdown.as_bytes().to_vec(),
            vault_proto::IfMatch::CreateOnly,
        )
        .await
        .map(|_| ())
        .map_err(|e| format!("{slug}: write note: {e:?}"))
}

// ── Locations ───────────────────────────────────────────────────────

/// Every location in the org's vault (studios / rooms / storage /
/// venues / homes), in the order the backend lists them.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_locations(slug: &str) -> Result<Vec<locations_proto::Location>, String> {
    let client =
        crate::vox_clients::establish_for::<locations_proto::LocationsServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list locations: {e:?}"))
}

/// Create one location. The backend assigns the `id` (nil here) and
/// the vault `path` (empty here, defaulting to `locations/<slug>.md`).
/// Returns the persisted location.
#[cfg(target_arch = "wasm32")]
pub async fn create_location(
    slug: &str,
    name: &str,
    kind: &str,
    address: Option<String>,
) -> Result<locations_proto::Location, String> {
    let client =
        crate::vox_clients::establish_for::<locations_proto::LocationsServiceClient>(slug).await?;
    let loc = locations_proto::Location {
        path: String::new(),
        id: uuid::Uuid::nil(),
        name: name.to_owned(),
        kind: kind.to_owned(),
        parent_id: None,
        address,
        tags: locations_proto::model::Tags::default(),
        same_as: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    client
        .create(loc)
        .await
        .map_err(|e| format!("{slug}: create location: {e:?}"))
}

// ── Milestones ──────────────────────────────────────────────────────

/// Every milestone in the org's vault (project-scoped checkpoints),
/// in the order the backend lists them. Filter client-side by
/// `project_id` / `status` as needed.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_milestones(slug: &str) -> Result<Vec<milestone_proto::Milestone>, String> {
    let client =
        crate::vox_clients::establish_for::<milestone_proto::MilestoneServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list milestones: {e:?}"))
}

/// Create one milestone. `project_id` is required (a milestone
/// always lives inside a project); the backend derives the vault
/// `path` (empty here) and assigns the `id` (nil here). Returns
/// the persisted milestone.
#[cfg(target_arch = "wasm32")]
pub async fn create_milestone(
    slug: &str,
    title: &str,
    project_id: uuid::Uuid,
    due_date: Option<chrono::NaiveDate>,
) -> Result<milestone_proto::Milestone, String> {
    let client =
        crate::vox_clients::establish_for::<milestone_proto::MilestoneServiceClient>(slug).await?;
    let ms = milestone_proto::Milestone {
        path: String::new(),
        id: uuid::Uuid::nil(),
        title: title.to_owned(),
        project_id,
        goal_id: None,
        status: "open".to_owned(),
        due_date,
        tags: milestone_proto::Tags::default(),
        forge_ref: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    client
        .create(ms)
        .await
        .map_err(|e| format!("{slug}: create milestone: {e:?}"))
}

// ── Fitness ─────────────────────────────────────────────────────────

/// Every tracked body metric (weight / body-fat / measurements)
/// in the org's vault, in the order the backend lists them. Each
/// carries its full entry time series inline.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_body_metrics(
    slug: &str,
) -> Result<Vec<fitness_proto::body::BodyMetric>, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::body::BodyServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list body metrics: {e:?}"))
}

/// Create one body metric (e.g. a "Weight" series tracked in
/// kg). The backend assigns the `id` (nil here) and the vault
/// `path` (empty here). Returns the persisted metric.
#[cfg(target_arch = "wasm32")]
pub async fn create_body_metric(
    slug: &str,
    name: &str,
    kind: &str,
    unit: &str,
) -> Result<fitness_proto::body::BodyMetric, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::body::BodyServiceClient>(slug).await?;
    let metric = fitness_proto::body::BodyMetric {
        path: String::new(),
        id: uuid::Uuid::nil(),
        name: name.to_owned(),
        kind: kind.to_owned(),
        unit: unit.to_owned(),
        goal: None,
        tags: fitness_proto::body::Tags::default(),
        entries: fitness_proto::body::Entries::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    client
        .create(metric)
        .await
        .map_err(|e| format!("{slug}: create body metric: {e:?}"))
}

/// Log a single reading against an existing metric. `value` is
/// in the metric's default unit; `date` is the calendar day of
/// the reading. Returns the updated metric.
#[cfg(target_arch = "wasm32")]
pub async fn log_body_entry(
    slug: &str,
    metric_id: uuid::Uuid,
    value: f64,
    date: chrono::NaiveDate,
) -> Result<fitness_proto::body::BodyMetric, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::body::BodyServiceClient>(slug).await?;
    let entry = fitness_proto::body::BodyEntry {
        id: uuid::Uuid::nil(),
        date,
        value,
        unit: None,
        note: None,
    };
    client
        .log_entry(metric_id.to_string(), entry)
        .await
        .map_err(|e| format!("{slug}: log body entry: {e:?}"))
}

/// Every exercise in the org's catalog, in the order the
/// backend lists them.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_exercises(
    slug: &str,
) -> Result<Vec<fitness_proto::exercises::Exercise>, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::exercises::ExercisesServiceClient>(slug)
            .await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list exercises: {e:?}"))
}

/// Add one exercise to the catalog (name + movement category).
/// The backend assigns the `id` (nil here) and the vault `path`
/// (empty here). Returns the persisted exercise.
#[cfg(target_arch = "wasm32")]
pub async fn create_exercise(
    slug: &str,
    name: &str,
    category: &str,
) -> Result<fitness_proto::exercises::Exercise, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::exercises::ExercisesServiceClient>(slug)
            .await?;
    let exercise = fitness_proto::exercises::Exercise {
        path: String::new(),
        id: uuid::Uuid::nil(),
        name: name.to_owned(),
        aliases: fitness_proto::exercises::StringList::default(),
        description: None,
        category: category.to_owned(),
        primary_muscles: fitness_proto::exercises::StringList::default(),
        secondary_muscles: fitness_proto::exercises::StringList::default(),
        equipment: fitness_proto::exercises::StringList::default(),
        mechanics: None,
        force: None,
        instructions: fitness_proto::exercises::StringList::default(),
        video_url: None,
        image_url: None,
        tags: fitness_proto::exercises::StringList::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    client
        .create(exercise)
        .await
        .map_err(|e| format!("{slug}: create exercise: {e:?}"))
}

/// Every logged workout session in the org's vault, in the order
/// the backend lists them.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_workouts(
    slug: &str,
) -> Result<Vec<fitness_proto::workouts::WorkoutSession>, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::workouts::WorkoutsServiceClient>(slug)
            .await?;
    client
        .list_sessions()
        .await
        .map_err(|e| format!("{slug}: list workout sessions: {e:?}"))
}

/// Every daily intake log in the org's vault, in the order the
/// backend lists them. Each carries its consumed entries inline.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_intake(slug: &str) -> Result<Vec<fitness_proto::intake::IntakeLog>, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::intake::IntakeServiceClient>(slug)
            .await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list intake logs: {e:?}"))
}

// ── Timer ─────────────────────────────────────────────────────────

/// The currently-running session for `user_id` in this org, if any.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_active_timer(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<Option<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .active_timer(user_id)
        .await
        .map_err(|e| format!("{slug}: active timer: {e:?}"))
}

/// Recent sessions for `user_id`, newest first.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_recent_sessions(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<Vec<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    let filter = timer_proto::WorkSessionFilter {
        user_id: Some(user_id),
        ..Default::default()
    };
    let mut sessions = client
        .list_sessions(filter)
        .await
        .map_err(|e| format!("{slug}: list sessions: {e:?}"))?;
    sessions.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    Ok(sessions)
}

/// Sessions across several orgs, each tagged with its org slug, newest
/// first. Powers the multi-org timer / finances / invoices views.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_sessions_multi(slugs: &[String]) -> Vec<(String, timer_proto::WorkSession)> {
    let mut out = Vec::new();
    for slug in slugs {
        if let Ok(sessions) = fetch_org_sessions(slug).await {
            out.extend(sessions.into_iter().map(|s| (slug.clone(), s)));
        }
    }
    out.sort_by(|a, b| b.1.start_time.cmp(&a.1.start_time));
    out
}

/// Every session in the org (all members), newest first — the time-log
/// view. Lets the operator see contractors' logged time too, not just
/// their own.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_org_sessions(slug: &str) -> Result<Vec<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    let mut sessions = client
        .list_sessions(timer_proto::WorkSessionFilter::default())
        .await
        .map_err(|e| format!("{slug}: list sessions: {e:?}"))?;
    sessions.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    Ok(sessions)
}

/// Start a timer; returns the new open session.
#[cfg(target_arch = "wasm32")]
pub async fn start_timer(
    slug: &str,
    req: timer_proto::StartTimerRequest,
) -> Result<timer_proto::WorkSession, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .start_timer(req)
        .await
        .map_err(|e| format!("{slug}: start timer: {e:?}"))
}

/// Stop `user_id`'s running timer; returns the closed session.
#[cfg(target_arch = "wasm32")]
pub async fn stop_timer(
    slug: &str,
    user_id: uuid::Uuid,
) -> Result<timer_proto::WorkSession, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .stop_timer(user_id)
        .await
        .map_err(|e| format!("{slug}: stop timer: {e:?}"))
}

/// Edit an existing session — only the `Some(_)` fields change. The
/// backend re-snapshots the rate afterward.
#[cfg(target_arch = "wasm32")]
pub async fn update_session(
    slug: &str,
    req: timer_proto::service::UpdateSessionRequest,
) -> Result<timer_proto::WorkSession, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .update_session(req)
        .await
        .map_err(|e| format!("{slug}: update session: {e:?}"))
}

/// Permanently delete a session.
#[cfg(target_arch = "wasm32")]
pub async fn delete_session(slug: &str, id: uuid::Uuid) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .delete_session(id)
        .await
        .map_err(|e| format!("{slug}: delete session: {e:?}"))
}

// ── finance / invoicing ─────────────────────────────────────────────

#[cfg(target_arch = "wasm32")]
async fn invoicing(slug: &str) -> Result<finance_proto::InvoicingClient, String> {
    crate::vox_clients::establish_for::<finance_proto::InvoicingClient>(slug).await
}

/// All invoices in an org, newest first.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_invoices(slug: &str) -> Result<Vec<finance_proto::Invoice>, String> {
    invoicing(slug)
        .await?
        .list_invoices()
        .await
        .map_err(|e| format!("{slug}: list invoices: {e:?}"))
}

/// Per-project billable time not yet invoiced, in an org.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_uninvoiced(slug: &str) -> Result<Vec<finance_proto::UninvoicedGroup>, String> {
    invoicing(slug)
        .await?
        .uninvoiced()
        .await
        .map_err(|e| format!("{slug}: uninvoiced: {e:?}"))
}

/// Generate + persist a draft invoice from a project's billable time.
#[cfg(target_arch = "wasm32")]
pub async fn generate_invoice(
    slug: &str,
    req: finance_proto::GenerateInvoice,
) -> Result<finance_proto::Invoice, String> {
    invoicing(slug)
        .await?
        .generate_invoice(req)
        .await
        .map_err(|e| format!("{slug}: generate invoice: {e:?}"))
}

/// Issue an invoice (assign number, lock).
#[cfg(target_arch = "wasm32")]
pub async fn invoice_mark_sent(
    slug: &str,
    id: uuid::Uuid,
) -> Result<finance_proto::Invoice, String> {
    invoicing(slug)
        .await?
        .mark_sent(id)
        .await
        .map_err(|e| format!("{slug}: mark sent: {e:?}"))
}

/// Record a payment against an invoice.
#[cfg(target_arch = "wasm32")]
pub async fn invoice_record_payment(
    slug: &str,
    id: uuid::Uuid,
    amount_minor: i64,
    date: String,
) -> Result<finance_proto::Invoice, String> {
    invoicing(slug)
        .await?
        .record_invoice_payment(id, amount_minor, date)
        .await
        .map_err(|e| format!("{slug}: record payment: {e:?}"))
}

/// Delete a draft invoice (un-bills its sessions).
#[cfg(target_arch = "wasm32")]
pub async fn invoice_delete(slug: &str, id: uuid::Uuid) -> Result<(), String> {
    invoicing(slug)
        .await?
        .delete_invoice(id)
        .await
        .map_err(|e| format!("{slug}: delete invoice: {e:?}"))
}

/// Invoices across several orgs, slug-tagged, newest first.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_invoices_multi(slugs: &[String]) -> Vec<(String, finance_proto::Invoice)> {
    let mut out = Vec::new();
    for slug in slugs {
        if let Ok(rows) = fetch_invoices(slug).await {
            out.extend(rows.into_iter().map(|i| (slug.clone(), i)));
        }
    }
    out.sort_by(|a, b| b.1.created_at.cmp(&a.1.created_at));
    out
}

/// Uninvoiced groups across several orgs, slug-tagged.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_uninvoiced_multi(
    slugs: &[String],
) -> Vec<(String, finance_proto::UninvoicedGroup)> {
    let mut out = Vec::new();
    for slug in slugs {
        if let Ok(rows) = fetch_uninvoiced(slug).await {
            out.extend(rows.into_iter().map(|g| (slug.clone(), g)));
        }
    }
    out
}

/// Fetch one org's vault markdown as `WikiFile`s for the knowledge
/// graph: pull the manifest, then read every `.md` file concurrently
/// over the one socket. Pure graph-building happens caller-side.
#[cfg(target_arch = "wasm32")]
pub async fn fetch_wiki_files(slug: &str) -> Result<Vec<view_knowledge_graph::WikiFile>, String> {
    use view_knowledge_graph::WikiFile;

    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    let manifest = client
        .manifest("default".to_owned())
        .await
        .map_err(|e| format!("manifest: {e:?}"))?;
    let md_paths: Vec<String> = manifest
        .files
        .into_iter()
        .map(|f| f.path)
        .filter(|p| p.ends_with(".md"))
        .collect();

    let futs = md_paths.into_iter().map(|path| {
        let c = client.clone();
        async move {
            let bytes = c.get_file("default".to_owned(), path.clone()).await.ok()?;
            let name = std::path::Path::new(&path)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(&path)
                .to_string();
            Some(WikiFile {
                name,
                path,
                content: String::from_utf8_lossy(&bytes.0).into_owned(),
            })
        }
    });
    Ok(futures_util::future::join_all(futs)
        .await
        .into_iter()
        .flatten()
        .collect())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_wiki_files(_slug: &str) -> Result<Vec<view_knowledge_graph::WikiFile>, String> {
    Err("native client not wired yet".to_owned())
}

/// Locate a single project by id across the selected orgs, returning it
/// together with the slug of the org that owns it. Used by the project
/// detail page so it works regardless of which org is in view.
#[cfg(target_arch = "wasm32")]
pub async fn find_project(id: &str, slugs: &[String]) -> Result<(ProjectInfo, String), String> {
    let uuid = uuid::Uuid::parse_str(id).map_err(|_| "invalid project id".to_owned())?;
    let mut last_err = None;
    for slug in slugs {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await {
            Ok(client) => match client.get(uuid).await {
                Ok(p) => return Ok((p, slug.clone())),
                Err(e) => last_err = Some(format!("{slug}: {e:?}")),
            },
            Err(e) => last_err = Some(format!("{slug}: {e}")),
        }
    }
    Err(last_err.unwrap_or_else(|| "project not found in any hosted org".to_owned()))
}

/// Flatten per-org results: concat the successes; surface an error only
/// if every org failed *and* nothing came back.
#[cfg(target_arch = "wasm32")]
fn collect<T>(results: Vec<Result<Vec<T>, String>>) -> Result<Vec<T>, String> {
    let mut out = Vec::new();
    let mut last_err = None;
    for r in results {
        match r {
            Ok(rows) => out.extend(rows),
            Err(e) => last_err = Some(e),
        }
    }
    if out.is_empty() {
        if let Some(e) = last_err {
            return Err(e);
        }
    }
    Ok(out)
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_projects(_slugs: &[String]) -> Result<Vec<project::ProjectInfo>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_tasks(_slugs: &[String]) -> Result<Vec<task::TaskInfo>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_tasks_tagged(
    _slugs: &[String],
) -> Result<Vec<(String, task::TaskInfo)>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_day_templates(
    _slug: &str,
) -> Result<Vec<scheduling_proto::DayTemplate>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_day_plan(
    _slug: &str,
    _date: &str,
) -> Result<Option<scheduling_proto::DayPlan>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn save_day_plan(_slug: &str, _plan: scheduling_proto::DayPlan) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn delete_day_plan(_slug: &str, _date: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn list_events(_slug: &str) -> Result<Vec<scheduling_proto::CalEvent>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn upsert_event(_slug: &str, _event: scheduling_proto::CalEvent) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn delete_event(_slug: &str, _id: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_event_types(_slug: &str) -> Result<Vec<scheduling_proto::EventType>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_event_type(
    _slug: &str,
    _title: &str,
    _duration_min: u16,
) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_bookings(_slug: &str) -> Result<Vec<scheduling_proto::Booking>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn cancel_booking(_slug: &str, _id: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_inbox(_slug: &str) -> Result<Vec<inbox_proto::InboxItem>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn upsert_inbox_item(_slug: &str, _item: inbox_proto::InboxItem) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn delete_inbox_item(_slug: &str, _id: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_task(
    _slug: &str,
    _title: &str,
    _details: &str,
) -> Result<task::TaskInfo, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_wiki_note(_slug: &str, _path: &str, _markdown: &str) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_locations(_slug: &str) -> Result<Vec<locations_proto::Location>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_location(
    _slug: &str,
    _name: &str,
    _kind: &str,
    _address: Option<String>,
) -> Result<locations_proto::Location, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_milestones(_slug: &str) -> Result<Vec<milestone_proto::Milestone>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_milestone(
    _slug: &str,
    _title: &str,
    _project_id: uuid::Uuid,
    _due_date: Option<chrono::NaiveDate>,
) -> Result<milestone_proto::Milestone, String> {
    Err("native client not wired yet".to_owned())
}

// ── Fitness (native stubs) ──────────────────────────────────────────

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_body_metrics(
    _slug: &str,
) -> Result<Vec<fitness_proto::body::BodyMetric>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_body_metric(
    _slug: &str,
    _name: &str,
    _kind: &str,
    _unit: &str,
) -> Result<fitness_proto::body::BodyMetric, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn log_body_entry(
    _slug: &str,
    _metric_id: uuid::Uuid,
    _value: f64,
    _date: chrono::NaiveDate,
) -> Result<fitness_proto::body::BodyMetric, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_exercises(
    _slug: &str,
) -> Result<Vec<fitness_proto::exercises::Exercise>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn create_exercise(
    _slug: &str,
    _name: &str,
    _category: &str,
) -> Result<fitness_proto::exercises::Exercise, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_workouts(
    _slug: &str,
) -> Result<Vec<fitness_proto::workouts::WorkoutSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_intake(_slug: &str) -> Result<Vec<fitness_proto::intake::IntakeLog>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_active_timer(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<Option<timer_proto::WorkSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_recent_sessions(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<Vec<timer_proto::WorkSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_org_sessions(_slug: &str) -> Result<Vec<timer_proto::WorkSession>, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_sessions_multi(_slugs: &[String]) -> Vec<(String, timer_proto::WorkSession)> {
    Vec::new()
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn start_timer(
    _slug: &str,
    _req: timer_proto::StartTimerRequest,
) -> Result<timer_proto::WorkSession, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn stop_timer(
    _slug: &str,
    _user_id: uuid::Uuid,
) -> Result<timer_proto::WorkSession, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn update_session(
    _slug: &str,
    _req: timer_proto::service::UpdateSessionRequest,
) -> Result<timer_proto::WorkSession, String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn delete_session(_slug: &str, _id: uuid::Uuid) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_invoices(_slug: &str) -> Result<Vec<finance_proto::Invoice>, String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_uninvoiced(_slug: &str) -> Result<Vec<finance_proto::UninvoicedGroup>, String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn generate_invoice(
    _slug: &str,
    _req: finance_proto::GenerateInvoice,
) -> Result<finance_proto::Invoice, String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn invoice_mark_sent(
    _slug: &str,
    _id: uuid::Uuid,
) -> Result<finance_proto::Invoice, String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn invoice_record_payment(
    _slug: &str,
    _id: uuid::Uuid,
    _amount_minor: i64,
    _date: String,
) -> Result<finance_proto::Invoice, String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn invoice_delete(_slug: &str, _id: uuid::Uuid) -> Result<(), String> {
    Err("native client not wired yet".to_owned())
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_invoices_multi(_slugs: &[String]) -> Vec<(String, finance_proto::Invoice)> {
    Vec::new()
}
#[cfg(not(target_arch = "wasm32"))]
pub async fn fetch_uninvoiced_multi(
    _slugs: &[String],
) -> Vec<(String, finance_proto::UninvoicedGroup)> {
    Vec::new()
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn find_project(
    _id: &str,
    _slugs: &[String],
) -> Result<(project::ProjectInfo, String), String> {
    Err("native client not wired yet".to_owned())
}
