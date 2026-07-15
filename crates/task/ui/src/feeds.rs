//! Multi-org data feeds.
//!
//! Each fetcher takes the resolved list of org slugs (from
//! [`crate::orgs::selected_slugs`]) and fans out **concurrently** —
//! establishing each org's service client and concatenating the rows.
//! "All" mode passes every hosted slug; single-org mode passes one.
//! Per-org failures are tolerated in multi-org mode (a down/empty org
//! doesn't blank the whole view); an error only surfaces if *nothing*
//! came back.

use project::ProjectInfo;
use task::TaskInfo as DbTask;

/// Active projects across the selected orgs (concurrent fan-out).
pub async fn fetch_projects(slugs: &[String]) -> Result<Vec<ProjectInfo>, String> {
    let futs = slugs.iter().map(|slug| async move {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await {
            Ok(client) => client
                .list()
                .await
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Goals across the selected orgs (concurrent fan-out).
pub async fn fetch_goals(slugs: &[String]) -> Result<Vec<goal::Goal>, String> {
    let futs = slugs.iter().map(|slug| async move {
        match crate::vox_clients::establish_for::<goal::GoalServiceClient>(slug).await {
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
pub async fn fetch_tasks(slugs: &[String]) -> Result<Vec<DbTask>, String> {
    Ok(fetch_tasks_tagged(slugs)
        .await?
        .into_iter()
        .map(|(_, t)| t)
        .collect())
}

/// Projects across the selected orgs, each paired with the slug of the
/// org it came from — feeds the shared project store so mutations and
/// the detail page can route back to the owning org.
pub async fn fetch_projects_tagged(slugs: &[String]) -> Result<Vec<(String, ProjectInfo)>, String> {
    let futs = slugs.iter().map(|slug| async move {
        match crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await {
            Ok(client) => client
                .list()
                .await
                .map(|rows| {
                    rows.into_iter()
                        .map(|p| (slug.clone(), p))
                        .collect::<Vec<_>>()
                })
                .map_err(|e| format!("{slug}: list: {e:?}")),
            Err(e) => Err(format!("{slug}: {e}")),
        }
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Tasks across the selected orgs, each paired with the slug of the org
/// it came from — so mutations can be routed back to the right org's
/// `TaskService` when viewing "All".
pub async fn fetch_tasks_tagged(slugs: &[String]) -> Result<Vec<(String, DbTask)>, String> {
    let futs = slugs.iter().map(|slug| async move {
        match crate::vox_clients::establish_for::<task::TaskServiceClient>(slug).await {
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
pub async fn save_day_plan(slug: &str, plan: scheduling_proto::DayPlan) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .upsert_day_plan(plan)
        .await
        .map_err(|e| format!("{slug}: save day plan: {e:?}"))
}

/// Delete a per-date plan, reverting that date to the template.
pub async fn delete_day_plan(slug: &str, date: &str) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::DayPlansClient>(slug).await?;
    client
        .delete_day_plan(date.to_string())
        .await
        .map_err(|e| format!("{slug}: delete day plan {date}: {e:?}"))
}

/// All persisted calendar events for the org.
pub async fn list_events(slug: &str) -> Result<Vec<scheduling_proto::CalEvent>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::CalendarEventsClient>(slug).await?;
    client
        .list_events()
        .await
        .map_err(|e| format!("{slug}: list events: {e:?}"))
}

/// Save (replacing) one calendar event.
pub async fn upsert_event(slug: &str, event: scheduling_proto::CalEvent) -> Result<(), String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::CalendarEventsClient>(slug).await?;
    client
        .upsert_event(event)
        .await
        .map_err(|e| format!("{slug}: save event: {e:?}"))
}

/// Delete one calendar event.
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
pub async fn fetch_event_types(slug: &str) -> Result<Vec<scheduling_proto::EventType>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::EventTypesClient>(slug).await?;
    client
        .list_event_types()
        .await
        .map_err(|e| format!("{slug}: list event types: {e:?}"))
}

/// Create (upsert) a bookable event type, returning the persisted draft
/// so optimistic stores can reconcile against it. The backend derives
/// the vault `path` from the slug/id; the caller builds the entity (see
/// `stores::draft_event_type`).
pub async fn create_event_type(
    slug: &str,
    event_type: scheduling_proto::EventType,
) -> Result<scheduling_proto::EventType, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::EventTypesClient>(slug).await?;
    client
        .upsert_event_type(event_type.clone())
        .await
        .map(|()| event_type)
        .map_err(|e| format!("{slug}: create event type: {e:?}"))
}

/// All bookings for the org (every status), oldest start first.
pub async fn fetch_bookings(slug: &str) -> Result<Vec<scheduling_proto::Booking>, String> {
    let client =
        crate::vox_clients::establish_for::<scheduling_proto::BookingsClient>(slug).await?;
    client
        .list_bookings()
        .await
        .map_err(|e| format!("{slug}: list bookings: {e:?}"))
}

/// Cancel a booking by id (sets status to `Cancelled`).
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
pub fn slugify(s: &str) -> String {
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
pub async fn fetch_inbox(slug: &str) -> Result<Vec<inbox_proto::InboxItem>, String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .list_inbox()
        .await
        .map_err(|e| format!("{slug}: list inbox: {e:?}"))
}

/// Capture or update one inbox item (keyed by id). Capture, snooze,
/// process, and archive all flow through here.
pub async fn upsert_inbox_item(slug: &str, item: inbox_proto::InboxItem) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .upsert_inbox_item(item)
        .await
        .map_err(|e| format!("{slug}: save inbox item: {e:?}"))
}

/// Delete one inbox item.
pub async fn delete_inbox_item(slug: &str, id: &str) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<inbox_proto::InboxClient>(slug).await?;
    client
        .delete_inbox_item(id.to_string())
        .await
        .map_err(|e| format!("{slug}: delete inbox item: {e:?}"))
}

// ── Threads (conversations on tasks/projects) ────────────────────────
//
// Single cross-target impl — the architect-generated `ThreadsServiceClient`
// is one API, established via `vox_clients::establish_for` (which already
// compiles for both wasm + native). No per-target duplication.

/// Threads anchored to `(entity_type, entity_id)`, newest first.
pub async fn fetch_threads(
    slug: &str,
    entity_type: &str,
    entity_id: uuid::Uuid,
) -> Result<Vec<threads::Thread>, String> {
    let client = crate::vox_clients::establish_for::<threads::ThreadsServiceClient>(slug).await?;
    client
        .list_threads(entity_type.to_string(), entity_id)
        .await
        .map_err(|e| format!("{slug}: list threads: {e:?}"))
}

/// Messages of one thread, oldest first.
pub async fn fetch_thread_messages(
    slug: &str,
    thread_id: uuid::Uuid,
) -> Result<Vec<threads::Message>, String> {
    let client = crate::vox_clients::establish_for::<threads::ThreadsServiceClient>(slug).await?;
    client
        .list_messages(thread_id)
        .await
        .map_err(|e| format!("{slug}: list messages: {e:?}"))
}

/// Open a new thread.
pub async fn create_thread(
    slug: &str,
    req: threads::CreateThreadRequest,
) -> Result<threads::Thread, String> {
    let client = crate::vox_clients::establish_for::<threads::ThreadsServiceClient>(slug).await?;
    client
        .create_thread(req)
        .await
        .map_err(|e| format!("{slug}: create thread: {e:?}"))
}

/// Post a message to a thread.
pub async fn post_thread_message(
    slug: &str,
    req: threads::PostMessageRequest,
) -> Result<threads::Message, String> {
    let client = crate::vox_clients::establish_for::<threads::ThreadsServiceClient>(slug).await?;
    client
        .post_message(req)
        .await
        .map_err(|e| format!("{slug}: post message: {e:?}"))
}

/// Repos *connected* (project-bound) in this org — the `/repos`
/// "connected" view, distinct from the raw forge catalog.
pub async fn fetch_connected_repos(slug: &str) -> Result<Vec<git_proto::RepoId>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::connections::RepoConnectionsClient>(slug)
            .await?;
    client
        .list_connected_repos()
        .await
        .map_err(|e| format!("{slug}: list connected repos: {e:?}"))
}

/// Comments on a forge issue — the issue's conversation, rendered under
/// it in the `/repos` view. Works for PRs too (Gitea shares the index).
pub async fn fetch_issue_comments(
    slug: &str,
    repo: git_proto::RepoId,
    number: u64,
) -> Result<Vec<git_proto::Comment>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::issues::IssueTrackerClient>(slug).await?;
    client
        .list_comments(repo, git_proto::IssueId(number))
        .await
        .map_err(|e| format!("{slug}: list comments #{number}: {e:?}"))
}

/// Repos bound to a specific project (its connected repos).
pub async fn fetch_repos_for_project(
    slug: &str,
    project_id: uuid::Uuid,
) -> Result<Vec<git_proto::RepoId>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::connections::RepoConnectionsClient>(slug)
            .await?;
    client
        .repos_for_project(project_id.to_string())
        .await
        .map_err(|e| format!("{slug}: repos for project: {e:?}"))
}

/// Update a project (write-through to its markdown). Used to change the
/// project type from the detail page.
pub async fn create_project(
    slug: &str,
    project: project::ProjectInfo,
) -> Result<project::ProjectInfo, String> {
    let client = crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await?;
    client
        .create(project)
        .await
        .map_err(|e| format!("{slug}: create project: {e:?}"))
}

pub async fn update_project(
    slug: &str,
    project: project::ProjectInfo,
) -> Result<project::ProjectInfo, String> {
    let client = crate::vox_clients::establish_for::<project::ProjectServiceClient>(slug).await?;
    client
        .update(project)
        .await
        .map_err(|e| format!("{slug}: update project: {e:?}"))
}

/// Pull requests on a connected repo.
pub async fn fetch_pull_requests(
    slug: &str,
    repo: git_proto::RepoId,
) -> Result<Vec<git_proto::PullRequest>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::reviews::ReviewSurfaceClient>(slug).await?;
    client
        .list_pull_requests(repo)
        .await
        .map_err(|e| format!("{slug}: list pull requests: {e:?}"))
}

/// Reviews on a PR (summary state + body per reviewer).
pub async fn fetch_pr_reviews(
    slug: &str,
    repo: git_proto::RepoId,
    pr: u64,
) -> Result<Vec<git_proto::Review>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::reviews::ReviewSurfaceClient>(slug).await?;
    client
        .list_reviews(repo, git_proto::PullRequestId(pr))
        .await
        .map_err(|e| format!("{slug}: list reviews #{pr}: {e:?}"))
}

/// Post a comment to an issue or PR conversation (PRs share the issue
/// index). Authored as the server's configured forge identity.
pub async fn post_issue_comment(
    slug: &str,
    repo: git_proto::RepoId,
    number: u64,
    body: String,
) -> Result<git_proto::Comment, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::issues::IssueTrackerClient>(slug).await?;
    client
        .add_comment(repo, git_proto::IssueId(number), body)
        .await
        .map_err(|e| format!("{slug}: add comment #{number}: {e:?}"))
}

/// Open or close an issue (state-only update).
pub async fn set_issue_state(
    slug: &str,
    repo: git_proto::RepoId,
    number: u64,
    state: git_proto::IssueState,
) -> Result<git_proto::Issue, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::issues::IssueTrackerClient>(slug).await?;
    let update = git_proto::IssueUpdate {
        state: Some(state),
        ..Default::default()
    };
    client
        .update_issue(repo, git_proto::IssueId(number), update)
        .await
        .map_err(|e| format!("{slug}: set state #{number}: {e:?}"))
}

/// Merge a pull request.
pub async fn merge_pull_request(
    slug: &str,
    repo: git_proto::RepoId,
    number: u64,
    method: git_proto::MergeMethod,
) -> Result<Option<String>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::reviews::ReviewSurfaceClient>(slug).await?;
    client
        .merge_pull_request(repo, git_proto::PullRequestId(number), method)
        .await
        .map_err(|e| format!("{slug}: merge PR #{number}: {e:?}"))
}

/// Promote an inbox item into a Task — `title` is the headline, `details`
/// the markdown body. Returns the created task (its `path` is the
/// provenance back-link to store in `processed_into`).
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
pub async fn fetch_locations(slug: &str) -> Result<Vec<locations_proto::Location>, String> {
    let client =
        crate::vox_clients::establish_for::<locations_proto::LocationsServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list locations: {e:?}"))
}

/// Create one location from a caller-built draft (see
/// `stores::draft_location` — the backend assigns the real `id` and
/// vault `path`). Returns the persisted location.
pub async fn create_location(
    slug: &str,
    loc: locations_proto::Location,
) -> Result<locations_proto::Location, String> {
    let client =
        crate::vox_clients::establish_for::<locations_proto::LocationsServiceClient>(slug).await?;
    client
        .create(loc)
        .await
        .map_err(|e| format!("{slug}: create location: {e:?}"))
}

// ── Scripture ───────────────────────────────────────────────────────

/// Installed Bible translations for the org (bundled editions first).
pub async fn fetch_translations(
    slug: &str,
) -> Result<Vec<scripture_proto::TranslationInfo>, String> {
    let client =
        crate::vox_clients::establish_for::<scripture_proto::ScriptureServiceClient>(slug).await?;
    client
        .translations()
        .await
        .map_err(|e| format!("{slug}: translations: {e:?}"))
}

/// One chapter of one translation. `book` accepts any spelling.
pub async fn fetch_chapter(
    slug: &str,
    translation: &str,
    book: &str,
    chapter: u16,
) -> Result<scripture_proto::ChapterView, String> {
    let client =
        crate::vox_clients::establish_for::<scripture_proto::ScriptureServiceClient>(slug).await?;
    // The generated vox client takes owned `String` args.
    client
        .chapter(translation.to_owned(), book.to_owned(), chapter)
        .await
        .map_err(|e| format!("{slug}: chapter {book} {chapter}: {e:?}"))
}

/// Compare a verse/range across translations (empty list ⇒ all).
pub async fn fetch_comparison(
    slug: &str,
    reference: &str,
    translations: Vec<String>,
) -> Result<scripture_proto::ComparisonView, String> {
    let client =
        crate::vox_clients::establish_for::<scripture_proto::ScriptureServiceClient>(slug).await?;
    client
        .compare(reference.to_owned(), translations)
        .await
        .map_err(|e| format!("{slug}: compare {reference}: {e:?}"))
}

/// Per-verse backlinks for a chapter — vault notes that link each verse.
pub async fn fetch_chapter_backlinks(
    slug: &str,
    book: &str,
    chapter: u16,
) -> Result<Vec<scripture_proto::VerseBacklinks>, String> {
    let client =
        crate::vox_clients::establish_for::<scripture_proto::ScriptureServiceClient>(slug).await?;
    client
        .chapter_backlinks(book.to_owned(), chapter)
        .await
        .map_err(|e| format!("{slug}: backlinks {book} {chapter}: {e:?}"))
}

// ── Inventory ───────────────────────────────────────────────────────

/// Every inventory item in the org's vault (`type: item` gear /
/// equipment pages), in the order the backend lists them.
pub async fn fetch_inventory(slug: &str) -> Result<Vec<inventory_proto::Item>, String> {
    let client =
        crate::vox_clients::establish_for::<inventory_proto::InventoryServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list inventory: {e:?}"))
}

/// Create one inventory item from a caller-built draft (see
/// `stores::draft_item` — the backend assigns the real `id` and vault
/// `path`). Returns the persisted item.
pub async fn create_item(
    slug: &str,
    item: inventory_proto::Item,
) -> Result<inventory_proto::Item, String> {
    let client =
        crate::vox_clients::establish_for::<inventory_proto::InventoryServiceClient>(slug).await?;
    client
        .create(item)
        .await
        .map_err(|e| format!("{slug}: create item: {e:?}"))
}

/// Move an item along its lifecycle (in-use / stored / loaned /
/// in-repair / missing / retired). Returns the updated item.
pub async fn set_item_status(
    slug: &str,
    id: &str,
    status: &str,
) -> Result<inventory_proto::Item, String> {
    let client =
        crate::vox_clients::establish_for::<inventory_proto::InventoryServiceClient>(slug).await?;
    client
        .set_status(id.to_owned(), status.to_owned())
        .await
        .map_err(|e| format!("{slug}: set item status: {e:?}"))
}

// ── Milestones ──────────────────────────────────────────────────────

/// Every milestone in the org's vault (project-scoped checkpoints),
/// in the order the backend lists them. Filter client-side by
/// `project_id` / `status` as needed.
pub async fn fetch_milestones(slug: &str) -> Result<Vec<milestone_proto::Milestone>, String> {
    let client =
        crate::vox_clients::establish_for::<milestone_proto::MilestoneServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list milestones: {e:?}"))
}

/// Create one milestone from a caller-built draft (see
/// `stores::draft_milestone` — the backend derives the vault `path` and
/// assigns the real `id`). Returns the persisted milestone.
pub async fn create_milestone(
    slug: &str,
    ms: milestone_proto::Milestone,
) -> Result<milestone_proto::Milestone, String> {
    let client =
        crate::vox_clients::establish_for::<milestone_proto::MilestoneServiceClient>(slug).await?;
    client
        .create(ms)
        .await
        .map_err(|e| format!("{slug}: create milestone: {e:?}"))
}

// ── Fitness ─────────────────────────────────────────────────────────

/// Every tracked body metric (weight / body-fat / measurements)
/// in the org's vault, in the order the backend lists them. Each
/// carries its full entry time series inline.
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

/// Create one body metric from a caller-built draft (see
/// `stores::draft_body_metric` — the backend assigns the real `id` and
/// vault `path`). Returns the persisted metric.
pub async fn create_body_metric(
    slug: &str,
    metric: fitness_proto::body::BodyMetric,
) -> Result<fitness_proto::body::BodyMetric, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::body::BodyServiceClient>(slug).await?;
    client
        .create(metric)
        .await
        .map_err(|e| format!("{slug}: create body metric: {e:?}"))
}

/// Log a single reading against an existing metric. `value` is
/// in the metric's default unit; `date` is the calendar day of
/// the reading. Returns the updated metric.
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

/// Add one exercise to the catalog from a caller-built draft (see
/// `stores::draft_exercise` — the backend assigns the real `id` and
/// vault `path`). Returns the persisted exercise.
pub async fn create_exercise(
    slug: &str,
    exercise: fitness_proto::exercises::Exercise,
) -> Result<fitness_proto::exercises::Exercise, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::exercises::ExercisesServiceClient>(slug)
            .await?;
    client
        .create(exercise)
        .await
        .map_err(|e| format!("{slug}: create exercise: {e:?}"))
}

/// Every logged workout session in the org's vault, in the order
/// the backend lists them.
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
pub async fn fetch_intake(slug: &str) -> Result<Vec<fitness_proto::intake::IntakeLog>, String> {
    let client =
        crate::vox_clients::establish_for::<fitness_proto::intake::IntakeServiceClient>(slug)
            .await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list intake logs: {e:?}"))
}

// ── Mealplan ────────────────────────────────────────────────────────

/// Every recipe in the org's cookbook (`<wiki>/Cookbook/*.cook`),
/// in the order the backend lists them.
pub async fn fetch_recipes(slug: &str) -> Result<Vec<cookbook_proto::Recipe>, String> {
    let client =
        crate::vox_clients::establish_for::<cookbook_proto::CookbookServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list recipes: {e:?}"))
}

/// Create one recipe from a caller-built draft (see
/// `stores::draft_recipe` — identity is the vault-relative `path`; the
/// backend parses the cooklang `source`). Returns the persisted recipe.
pub async fn create_recipe(
    slug: &str,
    recipe: cookbook_proto::Recipe,
) -> Result<cookbook_proto::Recipe, String> {
    let client =
        crate::vox_clients::establish_for::<cookbook_proto::CookbookServiceClient>(slug).await?;
    client
        .create(recipe)
        .await
        .map_err(|e| format!("{slug}: create recipe: {e:?}"))
}

/// Import a recipe from a web URL — the server fetches the page,
/// extracts the recipe, and synthesizes a cooklang `.cook` draft (not
/// yet saved). Returns the parsed draft for review.
pub async fn import_recipe(slug: &str, url: String) -> Result<cookbook_proto::Recipe, String> {
    let client =
        crate::vox_clients::establish_for::<cookbook_proto::CookbookServiceClient>(slug).await?;
    client
        .import(url)
        .await
        .map_err(|e| format!("{slug}: import recipe: {e:?}"))
}

/// Save edits to a recipe's `.cook` source. The server writes the
/// source verbatim then re-parses, so the returned recipe carries fresh
/// structured steps / ingredients / timers.
pub async fn update_recipe(
    slug: &str,
    recipe: cookbook_proto::Recipe,
) -> Result<cookbook_proto::Recipe, String> {
    let client =
        crate::vox_clients::establish_for::<cookbook_proto::CookbookServiceClient>(slug).await?;
    client
        .update(recipe)
        .await
        .map_err(|e| format!("{slug}: update recipe: {e:?}"))
}

/// Every pantry item in the org's vault (food-on-hand pages), in
/// the order the backend lists them.
pub async fn fetch_pantry(slug: &str) -> Result<Vec<pantry_proto::PantryItem>, String> {
    let client =
        crate::vox_clients::establish_for::<pantry_proto::PantryServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list pantry: {e:?}"))
}

/// Create one pantry item from a caller-built draft (see
/// `stores::draft_pantry_item` — the backend assigns the real `id` and
/// vault `path`). Returns the persisted item.
pub async fn create_pantry_item(
    slug: &str,
    item: pantry_proto::PantryItem,
) -> Result<pantry_proto::PantryItem, String> {
    let client =
        crate::vox_clients::establish_for::<pantry_proto::PantryServiceClient>(slug).await?;
    client
        .create(item)
        .await
        .map_err(|e| format!("{slug}: create pantry item: {e:?}"))
}

/// Cook a recipe directly: the server computes the pantry deductions
/// for `servings` and consumes them from stock, returning a receipt of
/// what was deducted (matched + convertible + in-stock ingredients) and
/// what was skipped (with the reason).
pub async fn cook_recipe(
    slug: &str,
    recipe_path: String,
    servings: u32,
) -> Result<mealplan_proto::CookReceipt, String> {
    let client =
        crate::vox_clients::establish_for::<mealplan_proto::MealplanServiceClient>(slug).await?;
    client
        .cook_recipe(recipe_path, servings)
        .await
        .map_err(|e| format!("{slug}: cook recipe: {e:?}"))
}

/// "Can I cook this right now?" — the server checks the recipe (and any
/// nested recipes) against current pantry stock for `servings` and
/// returns the full `Fulfillment`: whether it's cookable, the
/// have/need partition, and the per-shortage substitution suggestions.
/// All derivation is server-side — this is a thin client call.
pub async fn can_cook(
    slug: &str,
    recipe_path: String,
    servings: u32,
) -> Result<mealplan_proto::Fulfillment, String> {
    let client =
        crate::vox_clients::establish_for::<mealplan_proto::MealplanServiceClient>(slug).await?;
    client
        .can_cook(recipe_path, servings)
        .await
        .map_err(|e| format!("{slug}: can cook: {e:?}"))
}

/// Every planned meal in the org's vault, in the order the
/// backend lists them.
pub async fn fetch_meal_plans(slug: &str) -> Result<Vec<mealplan_proto::Meal>, String> {
    let client =
        crate::vox_clients::establish_for::<mealplan_proto::MealplanServiceClient>(slug).await?;
    client
        .list()
        .await
        .map_err(|e| format!("{slug}: list meal plans: {e:?}"))
}

// ── Timer ─────────────────────────────────────────────────────────

/// The currently-running session for `user_id` in this org, if any.
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
pub async fn fetch_org_sessions(slug: &str) -> Result<Vec<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    let mut sessions = client
        .list_sessions(timer_proto::WorkSessionFilter::default())
        .await
        .map_err(|e| format!("{slug}: list sessions: {e:?}"))?;
    sessions.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    Ok(sessions)
}

/// Sessions logged against one project (all members), newest first.
/// `open_only = true` narrows to running timers — the project
/// overview's "active now" view; `false` returns the full history for
/// budget aggregation.
pub async fn fetch_project_sessions(
    slug: &str,
    project_id: uuid::Uuid,
    open_only: bool,
) -> Result<Vec<timer_proto::WorkSession>, String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    let filter = timer_proto::WorkSessionFilter {
        project_id: Some(project_id),
        open: open_only.then_some(true),
        ..Default::default()
    };
    let mut sessions = client
        .list_sessions(filter)
        .await
        .map_err(|e| format!("{slug}: list sessions: {e:?}"))?;
    sessions.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    Ok(sessions)
}

/// Start a timer; returns the new open session.
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
pub async fn delete_session(slug: &str, id: uuid::Uuid) -> Result<(), String> {
    let client = crate::vox_clients::establish_for::<timer_proto::TimerServiceClient>(slug).await?;
    client
        .delete_session(id)
        .await
        .map_err(|e| format!("{slug}: delete session: {e:?}"))
}

// ── finance / invoicing ─────────────────────────────────────────────

async fn invoicing(slug: &str) -> Result<finance_proto::InvoicingClient, String> {
    crate::vox_clients::establish_for::<finance_proto::InvoicingClient>(slug).await
}

/// All invoices in an org, newest first.
pub async fn fetch_invoices(slug: &str) -> Result<Vec<finance_proto::Invoice>, String> {
    invoicing(slug)
        .await?
        .list_invoices()
        .await
        .map_err(|e| format!("{slug}: list invoices: {e:?}"))
}

/// Per-project billable time not yet invoiced, in an org.
pub async fn fetch_uninvoiced(slug: &str) -> Result<Vec<finance_proto::UninvoicedGroup>, String> {
    invoicing(slug)
        .await?
        .uninvoiced()
        .await
        .map_err(|e| format!("{slug}: uninvoiced: {e:?}"))
}

/// Generate + persist a draft invoice from a project's billable time.
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
pub async fn invoice_delete(slug: &str, id: uuid::Uuid) -> Result<(), String> {
    invoicing(slug)
        .await?
        .delete_invoice(id)
        .await
        .map_err(|e| format!("{slug}: delete invoice: {e:?}"))
}

// ── finance / ledger ────────────────────────────────────────────────

async fn ledger(slug: &str) -> Result<finance_proto::LedgerClient, String> {
    crate::vox_clients::establish_for::<finance_proto::LedgerClient>(slug).await
}

/// Resolve the org's (single) finance book id, if one exists yet.
async fn ledger_book_id(
    client: &finance_proto::LedgerClient,
    slug: &str,
) -> Result<Option<uuid::Uuid>, String> {
    let books = client
        .books()
        .await
        .map_err(|e| format!("{slug}: books: {e:?}"))?;
    Ok(books.first().map(|b| b.id))
}

/// Every account in an org's (single) book, paired with its current
/// balance. Returns `(account, balance)` rows. Empty when the org has
/// no book / accounts yet.
pub async fn fetch_ledger_accounts(
    slug: &str,
) -> Result<Vec<(finance_proto::Account, finance_proto::AccountBalance)>, String> {
    let client = ledger(slug).await?;
    let Some(book_id) = ledger_book_id(&client, slug).await? else {
        return Ok(Vec::new());
    };
    let accounts = client
        .accounts(book_id)
        .await
        .map_err(|e| format!("{slug}: accounts: {e:?}"))?;
    let balances = client
        .balances(book_id, None)
        .await
        .map_err(|e| format!("{slug}: balances: {e:?}"))?;
    let out = accounts
        .into_iter()
        .map(|a| {
            let bal = balances
                .iter()
                .find(|b| b.account_id == a.id)
                .cloned()
                .unwrap_or_else(|| finance_proto::AccountBalance {
                    account_id: a.id,
                    balance_minor: a.opening_balance_minor,
                    currency: a.currency.clone(),
                });
            (a, bal)
        })
        .collect();
    Ok(out)
}

/// Recent ledger transactions across every account in an org's book,
/// newest first. Pulls each account's history and de-dupes by
/// transaction id (a double-entry txn touches ≥2 accounts).
pub async fn fetch_ledger_transactions(
    slug: &str,
) -> Result<Vec<finance_proto::Transaction>, String> {
    let client = ledger(slug).await?;
    let Some(book_id) = ledger_book_id(&client, slug).await? else {
        return Ok(Vec::new());
    };
    let accounts = client
        .accounts(book_id)
        .await
        .map_err(|e| format!("{slug}: accounts: {e:?}"))?;
    let mut seen = std::collections::HashSet::new();
    let mut out: Vec<finance_proto::Transaction> = Vec::new();
    for a in accounts {
        let txns = client
            .account_transactions(a.id, None, None, 100)
            .await
            .map_err(|e| format!("{slug}: account transactions: {e:?}"))?;
        for t in txns {
            if seen.insert(t.id) {
                out.push(t);
            }
        }
    }
    out.sort_by(|a, b| b.created_at.cmp(&a.created_at));
    Ok(out)
}

/// Invoices across several orgs, slug-tagged, newest first.
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
/// Every `.base` file in an org's vault (vault-relative paths), sorted.
pub async fn fetch_bases(slug: &str) -> Result<Vec<String>, String> {
    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    let manifest = client
        .manifest("default".to_owned())
        .await
        .map_err(|e| format!("manifest: {e:?}"))?;
    let mut bases: Vec<String> = manifest
        .files
        .into_iter()
        .map(|f| f.path)
        .filter(|p| {
            std::path::Path::new(p)
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("base"))
        })
        .collect();
    bases.sort();
    Ok(bases)
}

/// The executed views of one `.base` file — server-rendered tables.
pub async fn fetch_base_views(
    slug: &str,
    base_path: &str,
) -> Result<Vec<vault_proto::BaseView>, String> {
    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    client
        .base_views("default".to_owned(), base_path.to_owned())
        .await
        .map_err(|e| format!("{base_path}: {e:?}"))
}

/// Every typed link touching `node_token` (a `kind:id` NodeRef token,
/// e.g. `sermon:god-restores-broken-people`) — the timestamped notes for
/// the watch view.
pub async fn fetch_links_for(
    slug: &str,
    node_token: &str,
) -> Result<Vec<links_proto::TypedLink>, String> {
    let node = links_proto::NodeRef::parse(node_token)
        .ok_or_else(|| format!("bad node token: {node_token}"))?;
    let client = crate::vox_clients::establish_for::<links_proto::LinksServiceClient>(slug).await?;
    client
        .links_for(node)
        .await
        .map_err(|e| format!("{slug}: links_for: {e:?}"))
}

/// One verse's text (`translation` defaults handled by the caller).
/// `reference` is a human/OSIS-ish ref like `John 3:16` or `1John 3:16`.
pub async fn fetch_verse_text(
    slug: &str,
    translation: &str,
    reference: &str,
) -> Result<String, String> {
    let client =
        crate::vox_clients::establish_for::<scripture_proto::ScriptureServiceClient>(slug).await?;
    client
        .verse(translation.to_owned(), reference.to_owned())
        .await
        .map(|v| v.text)
        .map_err(|e| format!("{slug}: verse {reference}: {e:?}"))
}

/// A resource's transcript cues (`resources/<rel_path>`), for the watch
/// view's synced transcript. Empty vec on a missing sidecar.
pub async fn fetch_transcript(
    slug: &str,
    rel_path: &str,
) -> Result<Vec<resources_proto::TranscriptSegment>, String> {
    let client =
        crate::vox_clients::establish_for::<resources_proto::ResourcesServiceClient>(slug).await?;
    // A missing / unreadable transcript just means no cues — never fatal.
    Ok(client
        .transcript(rel_path.to_owned())
        .await
        .map(|doc| doc.segments)
        .unwrap_or_default())
}

/// Save a watched video to the library as a `type: video` vault note
/// (`Videos/<id>.md`) so `[[id]]` resolves and it shows in a Videos base.
/// `CreateOnly`, so re-saving an existing video is a no-op (the title
/// stays whatever you renamed it to).
pub async fn save_video_note(
    slug: &str,
    video_id: &str,
    url: &str,
    title: &str,
) -> Result<(), String> {
    let title = if title.trim().is_empty() {
        video_id
    } else {
        title
    };
    let md = format!(
        "---\ntitle: {title}\ntype: video\nkind: video\nvideo_id: {video_id}\nurl: {url}\ntags: [video]\n---\n\n# {title}\n\nTimestamped notes are typed links on `video:{video_id}`. Watch + annotate at `/watch?v={video_id}&node=video:{video_id}`.\n"
    );
    let client = crate::vox_clients::establish_for::<vault_proto::VaultSyncClient>(slug).await?;
    client
        .put_file(
            "default".to_owned(),
            format!("Videos/{video_id}.md"),
            md.into_bytes(),
            vault_proto::IfMatch::CreateOnly,
        )
        .await
        .map(|_| ())
        .map_err(|e| format!("{slug}: save video: {e:?}"))
}

/// Persist one typed link (the watch view's "add note at current time").
pub async fn create_link(
    slug: &str,
    link: links_proto::TypedLink,
) -> Result<links_proto::TypedLink, String> {
    let client = crate::vox_clients::establish_for::<links_proto::LinksServiceClient>(slug).await?;
    client
        .create(link)
        .await
        .map_err(|e| format!("{slug}: create link: {e:?}"))
}

/// Every typed link in an org's graph — the verse↔song↔sermon↔topic web
/// for the `/connections` page. Fetches at the lowest confidence and
/// includes private links; the page filters client-side.
pub async fn fetch_link_graph(slug: &str) -> Result<Vec<links_proto::TypedLink>, String> {
    let client = crate::vox_clients::establish_for::<links_proto::LinksServiceClient>(slug).await?;
    client
        .graph(links_proto::Confidence::Speculative, true)
        .await
        .map_err(|e| format!("{slug}: link graph: {e:?}"))
}

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
        .filter(|p| {
            std::path::Path::new(p)
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("md"))
        })
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

/// Fetch the **curated wiki** graph for one org — the server-built
/// 4-signal relevance graph over `<org>/wiki/Knowledge/` (the
/// `wiki_proto` Graph service), adapted to the renderer's
/// [`view_knowledge_graph::WikiGraph`] model. Louvain clusters ride
/// along as the legend's community summaries.
pub async fn fetch_wiki_service_graph(
    slug: &str,
) -> Result<view_knowledge_graph::WikiGraph, String> {
    use std::collections::HashMap;

    use view_knowledge_graph as vkg;

    let client =
        crate::vox_clients::establish_for::<wiki_proto::service::graph::GraphClient>(slug).await?;
    let opts = wiki_proto::graph::GraphOpts {
        query: String::new(),
        node_type: String::new(),
        limit: 0,
        weights: None,
    };
    let graph = client
        .build_graph("default".to_owned(), opts)
        .await
        .map_err(|e| format!("build_graph: {e:?}"))?;
    // Clusters are advisory (legend colors + summaries) — an error
    // here degrades to an unclustered graph, not a failed page.
    let clusters = client
        .clusters("default".to_owned())
        .await
        .unwrap_or_default();

    // Cluster membership → dense u32 community ids (0 = unclustered).
    let mut community_of: HashMap<&str, u32> = HashMap::new();
    for (i, c) in clusters.iter().enumerate() {
        for m in &c.members {
            community_of.insert(m.as_str(), (i + 1) as u32);
        }
    }

    let nodes: Vec<vkg::GraphNode> = graph
        .nodes
        .iter()
        .map(|n| vkg::GraphNode {
            id: n.id.clone(),
            label: n.label.clone(),
            kind: {
                let k = n.node_type.trim().to_ascii_lowercase();
                if k.is_empty() { "other".to_owned() } else { k }
            },
            path: n.id.clone(),
            link_count: n.link_count,
            community: community_of.get(n.id.as_str()).copied().unwrap_or(0),
        })
        .collect();
    let degree_of: HashMap<&str, u32> = graph
        .nodes
        .iter()
        .map(|n| (n.id.as_str(), n.link_count))
        .collect();
    let label_of: HashMap<&str, &str> = graph
        .nodes
        .iter()
        .map(|n| (n.id.as_str(), n.label.as_str()))
        .collect();
    let edges: Vec<vkg::GraphEdge> = graph
        .edges
        .iter()
        .map(|e| vkg::GraphEdge::wikilink(e.source.clone(), e.target.clone(), e.weight))
        .collect();
    let communities: Vec<vkg::CommunityInfo> = clusters
        .iter()
        .enumerate()
        .map(|(i, c)| {
            let mut members: Vec<&str> = c.members.iter().map(String::as_str).collect();
            members.sort_by_key(|m| std::cmp::Reverse(degree_of.get(m).copied().unwrap_or(0)));
            vkg::CommunityInfo {
                id: (i + 1) as u32,
                node_count: c.members.len(),
                cohesion: c.cohesion,
                top_nodes: members
                    .into_iter()
                    .take(5)
                    .map(|m| label_of.get(m).copied().unwrap_or(m).to_owned())
                    .collect(),
            }
        })
        .collect();

    Ok(vkg::WikiGraph {
        nodes,
        edges,
        communities,
    })
}

/// Locate a single project by id across the selected orgs, returning it
/// together with the slug of the org that owns it. Used by the project
/// detail page so it works regardless of which org is in view.
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

// ── Fitness (native stubs) ──────────────────────────────────────────

// ── Mealplan (native stubs) ─────────────────────────────────────────

// ── Agents ────────────────────────────────────────────────────────

/// Agent sessions across the selected orgs (concurrent fan-out).
///
/// Each session carries its owning org slug so the `/agents` page can
/// show provenance in multi-org "All" mode. Archived sessions are
/// included so the listing is a faithful mirror of the backend.
pub async fn fetch_agent_sessions(
    slugs: &[String],
) -> Result<Vec<(String, agent_proto::session::Session)>, String> {
    let futs = slugs.iter().map(|slug| async move {
        let client = crate::vox_clients::establish_for::<
            agent_proto::service::sessions::SessionsClient,
        >(slug)
        .await?;
        let filter = agent_proto::service::sessions::SessionFilter {
            project_id: String::new(),
            backend_id: String::new(),
            profile_id: String::new(),
            include_archived: true,
            only_pinned: false,
            limit: 0,
            cursor: String::new(),
        };
        let page = client
            .list_sessions(filter)
            .await
            .map_err(|e| format!("{slug}: list agent sessions: {e:?}"))?;
        Ok::<_, String>(
            page.sessions
                .into_iter()
                .map(|s| (slug.clone(), s))
                .collect::<Vec<_>>(),
        )
    });
    collect(futures_util::future::join_all(futs).await)
}

/// Agent sessions attached to one project, filtered server-side
/// (`SessionFilter.project_id` is an exact match in the backend).
/// Archived sessions are excluded — this powers "active now" style
/// views, and an archived session can't be active.
pub async fn fetch_project_agent_sessions(
    slug: &str,
    project_id: &str,
) -> Result<Vec<agent_proto::session::Session>, String> {
    let client =
        crate::vox_clients::establish_for::<agent_proto::service::sessions::SessionsClient>(slug)
            .await?;
    let filter = agent_proto::service::sessions::SessionFilter {
        project_id: project_id.to_owned(),
        backend_id: String::new(),
        profile_id: String::new(),
        include_archived: false,
        only_pinned: false,
        limit: 0,
        cursor: String::new(),
    };
    let page = client
        .list_sessions(filter)
        .await
        .map_err(|e| format!("{slug}: list agent sessions: {e:?}"))?;
    Ok(page.sessions)
}

// ── Email ───────────────────────────────────────────────────────────

/// Every mail account the org's `EmailSync` backend serves. An org
/// with no configured mailbox returns an empty list (operational but
/// unconfigured) — the `/email` page renders that as an empty state
/// rather than an error.
pub async fn fetch_email_accounts(slug: &str) -> Result<Vec<email_proto::Account>, String> {
    let client = crate::vox_clients::establish_for::<email_proto::EmailSyncClient>(slug).await?;
    client
        .accounts()
        .await
        .map_err(|e| format!("{slug}: list accounts: {e:?}"))
}

/// Recent envelopes (header summaries) for one account's `INBOX`,
/// newest first. `count` caps the slice. Returns an empty list for an
/// empty mailbox; surfaces backend errors verbatim so the page can show
/// them inline.
pub async fn fetch_email_envelopes(
    slug: &str,
    account: &str,
    count: u32,
) -> Result<Vec<email_proto::Envelope>, String> {
    let client = crate::vox_clients::establish_for::<email_proto::EmailSyncClient>(slug).await?;
    let mut envelopes = client
        .fetch_envelopes(
            account.to_owned(),
            "INBOX".to_owned(),
            email_proto::SeqRange::Recent(count),
        )
        .await
        .map_err(|e| format!("{slug}: fetch envelopes: {e:?}"))?;
    // Newest first — the backend's `Recent` ordering isn't guaranteed
    // across implementations, so sort defensively on the date.
    envelopes.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
    Ok(envelopes)
}

// ── Git / forge ─────────────────────────────────────────────────────

/// Every repo the org's forge backend can address, in the order the
/// catalog lists them. Backed by `RepoCatalog::list_repos`; when the
/// forge is unconfigured (no token) the backend returns an
/// auth/forge error the caller renders as an empty list.
pub async fn fetch_repos(slug: &str) -> Result<Vec<git_proto::Repo>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::repo::RepoCatalogClient>(slug).await?;
    client
        .list_repos()
        .await
        .map_err(|e| format!("{slug}: list repos: {e:?}"))
}

/// Issues for one repo (all states), via `IssueTracker::list_issues`
/// with a default (unfiltered) filter. The `/repos` page calls this
/// per repo to show each repo's open work inline.
pub async fn fetch_issues(
    slug: &str,
    repo: git_proto::RepoId,
) -> Result<Vec<git_proto::issues::Issue>, String> {
    let client =
        crate::vox_clients::establish_for::<git_proto::issues::IssueTrackerClient>(slug).await?;
    client
        .list_issues(repo, git_proto::issues::IssueFilter::default())
        .await
        .map_err(|e| format!("{slug}: list issues: {e:?}"))
}
