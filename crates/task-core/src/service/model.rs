//! Vox service definitions for task-core.
//!
//! The core surface is intentionally split by product area so clients can bind
//! only the APIs they need while still sharing one implementation.

use chrono::{DateTime, Utc};
use uuid::Uuid;
use vox::Tx;

use crate::attachment::Attachment;
use crate::calendar_event::{CalendarEvent, CalendarEventStatus};
use crate::email::EmailRef;
use crate::expense::{ExpenseFilter, ExpenseReport};
use crate::index::{ChangeRow, ConflictRow};
use crate::invoice::Invoice;
use crate::people::{
    OrganizationContext, OrganizationRecord, Person, PersonContext, ProviderConflict,
};
use crate::project::{Project, ProjectDashboardEntry, ProjectStats};
use crate::provider::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, MailAccount, MailMessage,
    MailMessageDetail, MailTag, Mailbox,
};
use crate::query::Query;
use crate::task::WikiLink;
use crate::task::{Task, TimeEntry};
use crate::track::TrackApi;

#[vox::service]
pub trait TaskService {
    /// Execute a query and return matching tasks sorted by the query's sort.
    async fn execute_query(&self, query: Query) -> Vec<Task>;

    /// Compute the urgency score for a single task.
    async fn urgency_score(&self, task: Task) -> i32;

    /// Mark a task complete. Handles recurrence logic and sets completedDate.
    async fn complete_task(&self, title: String) -> Result<Task, VaultError>;

    /// Search tasks by text query (uses FTS5 index).
    async fn search_tasks(&self, query: String) -> Vec<Task>;

    /// Get tasks assigned to a specific user.
    async fn tasks_for_user(&self, username: String) -> Vec<Task>;

    /// Subscribe to live task ops on this server. The server forwards
    /// matching `TaskOp`s into `output` until either the receiver disconnects
    /// or the subscription is dropped.
    async fn subscribe(&self, filter: TaskSubscriptionFilter, output: Tx<TaskOp>);

    /// Apply one CRDT op to the server's authoritative state, persist the
    /// resulting task (and its CRDT snapshot when the realtime feature is
    /// enabled), and broadcast the op to subscribers.
    async fn apply_op(&self, op: TaskOp) -> Result<(), VaultError>;
}

/// Filter applied server-side to ops fanned out via [`TaskService::subscribe`].
///
/// All fields are AND-combined. `None`/empty means "match anything".
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TaskSubscriptionFilter {
    /// Restrict to ops on a single task.
    pub task_id: Option<Uuid>,
    /// Restrict to tasks that include this project (matched by project title).
    pub project: Option<String>,
}

/// One CRDT operation suitable for streaming over Vox to subscribers.
///
/// The wire format is intentionally narrow: scalar field changes, raw Loro
/// update bytes for body/list edits, and create/delete bookends. Body edits
/// are carried as raw Loro update payloads so peers can merge them without
/// re-encoding the surrounding `Task`.
#[derive(Debug, Clone, facet::Facet)]
#[repr(u8)]
pub enum TaskOp {
    /// A scalar metadata field changed.
    FieldChanged {
        task_id: Uuid,
        field: String,
        value: Option<String>,
        peer: Option<u64>,
    } = 0,
    /// Raw Loro update bytes — body edits and other container-level mutations.
    BodyUpdate {
        task_id: Uuid,
        update_bytes: Vec<u8>,
    } = 1,
    /// A new task was created. `snapshot` is a full Loro snapshot.
    Created { task_id: Uuid, snapshot: Vec<u8> } = 2,
    /// A task was deleted (soft or hard).
    Deleted { task_id: Uuid } = 3,
}

impl TaskOp {
    /// The task id this op targets.
    pub fn task_id(&self) -> Uuid {
        match self {
            TaskOp::FieldChanged { task_id, .. }
            | TaskOp::BodyUpdate { task_id, .. }
            | TaskOp::Created { task_id, .. }
            | TaskOp::Deleted { task_id } => *task_id,
        }
    }
}

// SAFETY: `TaskOp` has no lifetime parameters; `Ref<'a> = TaskOp` is sound
// because the type is fully owned and 'static. This is the same pattern the
// vox primitives (`String`, `i64`, …) use to enable `SelfRef::get()`.
unsafe impl vox_types::Reborrow for TaskOp {
    type Ref<'a> = TaskOp;
}

unsafe impl vox_types::Reborrow for TaskSubscriptionFilter {
    type Ref<'a> = TaskSubscriptionFilter;
}

#[vox::service]
pub trait InboxService {
    /// Capture raw text into the untriaged inbox.
    async fn capture(&self, request: InboxCaptureRequest) -> Result<InboxItem, VaultError>;

    /// Return untriaged inbox items.
    async fn list_inbox(&self) -> Vec<InboxItem>;

    /// Classify/promote an inbox item into a commitment, idea, or task.
    async fn promote(&self, request: InboxPromoteRequest) -> Result<InboxItem, VaultError>;

    /// Return review buckets for today's operational sweep.
    async fn daily_review(&self) -> ReviewReport;

    /// Return review buckets for a weekly planning sweep.
    async fn weekly_review(&self) -> ReviewReport;

    /// Return review buckets for a monthly planning sweep.
    async fn monthly_review(&self) -> ReviewReport;

    /// Return review buckets scoped to a project.
    async fn project_review(&self, project_title: String) -> ReviewReport;
}

#[vox::service]
pub trait ProjectService {
    /// Return task count stats for a project.
    async fn project_stats(&self, project_title: String) -> ProjectStats;

    /// Return the active project dashboard sorted by urgency.
    async fn project_dashboard(&self) -> Vec<ProjectDashboardEntry>;

    /// Return the next actionable task for a project.
    async fn next_task(&self, project_title: String) -> Option<Task>;

    /// Get all tasks for a project.
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task>;

    /// Return one project plus tasks and storage-backed knowledge/files context.
    async fn project_context(
        &self,
        project_title: String,
        include_files: bool,
        depth: String,
    ) -> Result<Option<ProjectKnowledgeContext>, VaultError>;
}

#[vox::service]
pub trait TimeService {
    /// Start a timer on a task. The core enforces one active timer per vault.
    async fn start_timer(&self, request: TimeStartRequest) -> Result<TimeEntry, VaultError>;

    /// Stop the active timer, optionally scoped to a task.
    async fn stop_timer(&self, task_ref: Option<String>) -> Result<TimedTaskEntry, VaultError>;

    /// Log a completed time entry.
    async fn log_time(&self, request: TimeLogRequest) -> Result<TimeEntry, VaultError>;

    /// Return the active timer if one is running.
    async fn active_timer(&self) -> Option<TimedTaskEntry>;

    /// List time entries joined with task, project, client, and rate context.
    async fn list_time_entries(&self, filter: TimeEntryFilter) -> Vec<TimeEntryContext>;

    /// Edit a time entry by id.
    async fn edit_time_entry(
        &self,
        entry_id: String,
        patch: TimeEntryPatch,
        actor: Option<String>,
    ) -> Result<TimedTaskEntry, VaultError>;

    /// Delete a time entry by id.
    async fn delete_time_entry(
        &self,
        entry_id: String,
        actor: Option<String>,
    ) -> Result<(), VaultError>;
}

#[vox::service]
pub trait PeopleService {
    /// Sync CardDAV contacts and return normalized people.
    async fn list_people(&self, addressbook: Option<String>) -> Result<Vec<Person>, VaultError>;

    /// Sync CardDAV contacts and return organizations grouped from contacts.
    async fn list_organizations(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<OrganizationRecord>, VaultError>;

    /// Return one person plus related tasks/projects/events/communication refs.
    async fn person_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<PersonContext>, VaultError>;

    /// Return one organization plus related people/tasks/projects/events/communication refs.
    async fn organization_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<OrganizationContext>, VaultError>;

    /// Compare two versions of a provider-backed contact and return changed fields.
    async fn detect_person_conflict(
        &self,
        local: Person,
        remote: Person,
    ) -> Result<Option<ProviderConflict>, VaultError>;

    /// Compare two versions of a provider-backed organization and return changed fields.
    async fn detect_organization_conflict(
        &self,
        local: OrganizationRecord,
        remote: OrganizationRecord,
    ) -> Result<Option<ProviderConflict>, VaultError>;
}

#[vox::service]
pub trait OperatingService {
    /// Return a derived life/business operating model snapshot.
    async fn operating_model(&self) -> OperatingModelReport;
}

#[vox::service]
pub trait InvoiceService {
    async fn create_invoice_from_entries(
        &self,
        request: InvoiceCreateRequest,
    ) -> Result<Invoice, VaultError>;

    async fn finance_report(&self) -> BusinessFinanceReport;
    async fn send_invoice(
        &self,
        invoice_id: String,
        actor: Option<String>,
    ) -> Result<Invoice, VaultError>;
    async fn record_invoice_payment(
        &self,
        request: InvoicePaymentRequest,
    ) -> Result<Invoice, VaultError>;
    async fn cancel_invoice(
        &self,
        invoice_id: String,
        reason: Option<String>,
        actor: Option<String>,
    ) -> Result<Invoice, VaultError>;
}

#[vox::service]
pub trait ExpenseService {
    /// Return a roll-up expense report.
    async fn expense_report(&self, filter: ExpenseFilter) -> ExpenseReport;
}

#[vox::service]
pub trait ActivityService {
    async fn recent_activity(&self, limit: u32) -> Result<Vec<ChangeRow>, VaultError>;
    async fn list_sync_states(&self) -> Result<Vec<ProviderSyncState>, VaultError>;
    async fn list_conflicts(
        &self,
        open_only: bool,
        limit: u32,
    ) -> Result<Vec<ConflictRow>, VaultError>;
    async fn resolve_conflict(
        &self,
        conflict_id: i64,
        resolver: Option<String>,
        how: String,
    ) -> Result<(), VaultError>;
}

#[vox::service]
pub trait AttachmentService {
    /// List every attachment hung off a given owner entity (task/project/etc).
    async fn list_for_entity(
        &self,
        owner_type: String,
        owner_id: Uuid,
    ) -> Result<Vec<Attachment>, VaultError>;

    /// Upload bytes to the configured provider (when applicable) and persist
    /// the metadata row. The bytes parameter currently carries the full file;
    /// chunked streaming via `Tx<Vec<u8>>` is a planned follow-up.
    async fn upload(&self, request: AttachmentUploadRequest) -> Result<Attachment, VaultError>;

    /// Load metadata + bytes for an attachment. Returns
    /// `provider_not_configured` when the row is provider-backed but the
    /// provider isn't wired into the server.
    async fn download(&self, id: Uuid) -> Result<AttachmentDownloadResponse, VaultError>;

    /// Drop the attachment row; if it's provider-backed, also DELETE the
    /// remote object on a best-effort basis.
    async fn delete(&self, id: Uuid) -> Result<(), VaultError>;
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct AttachmentUploadRequest {
    /// Owner entity type — "task" / "project" / "comment" / "calendar_event"
    /// / "person".
    pub owner_type: String,
    pub owner_id: Uuid,
    /// Provider-relative path. Defaulted by the service when empty.
    pub path: String,
    pub label: Option<String>,
    pub mime: Option<String>,
    /// File bytes. The full payload travels in one frame today; chunked
    /// streaming via `Tx<Vec<u8>>` is a planned follow-up.
    pub bytes: Vec<u8>,
    pub uploader: Option<String>,
    /// Storage source — defaults to `nextcloud` when empty.
    pub source: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct AttachmentDownloadResponse {
    pub metadata: Attachment,
    pub bytes: Vec<u8>,
}

#[vox::service]
pub trait MailService {
    async fn list_accounts(&self) -> Result<Vec<MailAccount>, VaultError>;
    async fn list_mailboxes(&self, account_id: i64) -> Result<Vec<Mailbox>, VaultError>;
    async fn list_messages(
        &self,
        request: MailListMessagesRequest,
    ) -> Result<Vec<MailMessage>, VaultError>;
    async fn get_message(&self, id: i64) -> Result<MailMessageDetail, VaultError>;
    async fn get_body(&self, id: i64) -> Result<String, VaultError>;
    async fn create_mailbox(
        &self,
        request: MailCreateMailboxRequest,
    ) -> Result<Mailbox, VaultError>;
    async fn delete_mailbox(&self, mailbox_id: i64) -> Result<(), VaultError>;
    async fn move_message(&self, request: MailMoveMessageRequest) -> Result<(), VaultError>;
    async fn list_tags(&self) -> Result<Vec<MailTag>, VaultError>;
    async fn create_tag(&self, request: MailCreateTagRequest) -> Result<MailTag, VaultError>;
    async fn delete_tag(&self, request: MailDeleteTagRequest) -> Result<(), VaultError>;
    async fn set_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError>;
    async fn remove_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError>;
    async fn link_email(&self, request: EmailLinkRequest) -> Result<EmailLinkResponse, VaultError>;
    async fn unlink_email(&self, request: EmailUnlinkRequest) -> Result<(), VaultError>;
    async fn list_linked_emails(
        &self,
        request: EmailListRequest,
    ) -> Result<Vec<EmailRef>, VaultError>;
    async fn linked_message_ids(&self) -> Vec<String>;
}

#[vox::service]
pub trait ConversationService {
    /// List conversations across the configured channel provider.
    async fn list_conversations(&self) -> Result<Vec<ChannelConversation>, VaultError>;

    /// Read recent messages from a conversation.
    async fn recent_messages(
        &self,
        conversation_id: String,
        limit: u32,
    ) -> Result<Vec<ChannelMessage>, VaultError>;

    /// Send or reply to a message in a conversation.
    async fn send_message(
        &self,
        request: ChannelSendMessageRequest,
    ) -> Result<ChannelMessage, VaultError>;
}

#[vox::service]
pub trait CalendarService {
    /// Get tasks due on or before a date (YYYY-MM-DD).
    async fn tasks_due_by(&self, date: String) -> Vec<Task>;

    /// Get tasks scheduled between two dates, inclusive (YYYY-MM-DD).
    async fn scheduled_between(&self, from: String, to: String) -> Result<Vec<Task>, VaultError>;

    /// List calendar events whose start/end overlap an RFC3339 time range.
    async fn events_between(
        &self,
        from: String,
        to: String,
    ) -> Result<Vec<CalendarEvent>, VaultError>;

    /// Trigger a Nextcloud sync cycle. Returns sync stats.
    async fn trigger_sync(&self) -> Result<SyncStats, VaultError>;

    /// Get the last sync result.
    async fn sync_status(&self) -> Option<SyncStats>;

    /// Describe what a sync would touch without mutating providers.
    async fn sync_plan(&self) -> SyncPlan;

    /// Discover CalDAV principal, calendar home, and available calendars.
    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError>;

    /// Discover CardDAV principal, addressbook home, and available addressbooks.
    async fn discover_carddav(&self) -> Result<CardDavDiscovery, VaultError>;

    /// Fetch specific calendar objects by href using CalDAV calendar-multiget.
    async fn calendar_multiget(
        &self,
        request: CalDavMultigetRequest,
    ) -> Result<Vec<CalDavObject>, VaultError>;

    /// Incrementally sync a calendar collection using a previous sync-token.
    async fn calendar_sync_collection(
        &self,
        request: CalDavSyncCollectionRequest,
    ) -> Result<CalDavSyncCollectionResponse, VaultError>;

    /// Fetch specific vCards by href using CardDAV addressbook-multiget.
    async fn addressbook_multiget(
        &self,
        request: CardDavMultigetRequest,
    ) -> Result<Vec<CardDavObject>, VaultError>;

    /// Incrementally sync an addressbook using a previous sync-token.
    async fn addressbook_sync_collection(
        &self,
        request: CardDavSyncCollectionRequest,
    ) -> Result<CardDavSyncCollectionResponse, VaultError>;

    /// Put one raw VCALENDAR object, optionally guarded by ETag preconditions.
    async fn put_calendar_object(&self, request: CalDavPutObjectRequest) -> Result<(), VaultError>;

    /// Delete one raw calendar object, optionally guarded by If-Match.
    async fn delete_calendar_object(
        &self,
        request: CalDavDeleteObjectRequest,
    ) -> Result<(), VaultError>;

    /// Put one raw VCARD object, optionally guarded by ETag preconditions.
    async fn put_addressbook_object(
        &self,
        request: CardDavPutObjectRequest,
    ) -> Result<(), VaultError>;

    /// Delete one raw vCard object, optionally guarded by If-Match.
    async fn delete_addressbook_object(
        &self,
        request: CardDavDeleteObjectRequest,
    ) -> Result<(), VaultError>;

    /// Send an iTIP VCALENDAR payload through the CalDAV scheduling outbox.
    async fn send_calendar_schedule(
        &self,
        request: CalDavScheduleRequest,
    ) -> Result<CalDavScheduleResponse, VaultError>;

    /// Query busy VEVENT intervals for a calendar.
    async fn calendar_free_busy(
        &self,
        request: CalDavFreeBusyRequest,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError>;

    /// List remote Nextcloud Deck boards.
    async fn list_deck_boards(&self) -> Result<Vec<RemoteDeckBoard>, VaultError>;

    /// List remote Nextcloud Deck stacks for a board.
    async fn list_deck_stacks(&self, board_id: u64) -> Result<Vec<RemoteDeckStack>, VaultError>;
}

#[vox::service]
pub trait PropertyService {
    /// List every registered property definition.
    async fn list_definitions(&self) -> Result<Vec<PropertyDefinitionView>, VaultError>;

    /// Register or update a property definition. Idempotent on `name`.
    async fn define_property(
        &self,
        name: String,
        kind: String,
        description: Option<String>,
        options: String,
    ) -> Result<PropertyDefinitionView, VaultError>;

    /// Delete a property definition by name. Existing entity properties
    /// are not touched — definitions are advisory metadata.
    async fn delete_definition(&self, name: String) -> Result<(), VaultError>;

    /// Read all properties off one entity. `owner_type` is the entity
    /// kind: "task" | "project" | "calendar_event" | "person" | "comment"
    /// | "asset" | "location". Returns the JSON object verbatim
    /// (Obsidian frontmatter shape).
    async fn get_properties(
        &self,
        owner_type: String,
        owner_id: Uuid,
    ) -> Result<String, VaultError>;

    /// Set one property on one entity. JSON-encoded `value`. Auto-creates
    /// a Text property definition if `name` isn't registered yet.
    async fn set_property(
        &self,
        owner_type: String,
        owner_id: Uuid,
        name: String,
        value: String,
    ) -> Result<(), VaultError>;

    /// Remove a property from one entity.
    async fn unset_property(
        &self,
        owner_type: String,
        owner_id: Uuid,
        name: String,
    ) -> Result<(), VaultError>;

    /// Find every entity of `owner_type` whose property `name` JSON-equals
    /// `value` (encoded as JSON). Backed by SQLite `json_extract`.
    async fn find_by_property(
        &self,
        owner_type: String,
        name: String,
        value: String,
    ) -> Result<Vec<Uuid>, VaultError>;
}

/// Wire-friendly snapshot of a property definition.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct PropertyDefinitionView {
    pub id: Uuid,
    pub name: String,
    /// One of: "text" | "number" | "checkbox" | "date" | "datetime"
    /// | "list" | "tags".
    pub kind: String,
    pub description: Option<String>,
    /// JSON-encoded options blob.
    pub options: String,
    pub created_at: String,
    pub updated_at: String,
}

/// A project-type / workflow binding.
///
/// Exposes the `Integration` entity through a workflow-oriented lens:
/// when `Project::project_type` matches an Integration's `name`, that
/// integration's `statuses` and `task_templates` define the workflow's
/// status set and standard task scaffolding.
///
/// Status gating is *advisory* at this layer — `Task::status` is a fixed
/// SeaORM enum (Open/InProgress/Done/Cancelled/...). Integrations
/// declare their workflow's named statuses, which clients map onto the
/// canonical enum at presentation time.
#[vox::service]
pub trait ProjectTypeService {
    /// Every registered project type / workflow.
    async fn list_types(&self) -> Result<Vec<ProjectTypeView>, VaultError>;

    /// Look up a single type by `name`.
    async fn get_type(&self, name: String) -> Result<Option<ProjectTypeView>, VaultError>;

    /// Register a new type or upsert an existing one (matched by `name`).
    /// All list payloads are JSON-encoded to keep the wire shape
    /// Vox/Facet-friendly.
    async fn register_type(&self, spec: ProjectTypeSpec) -> Result<ProjectTypeView, VaultError>;

    /// Delete a type by name. Projects referencing it keep their
    /// `project_type` string; the lookup just returns None afterwards.
    async fn delete_type(&self, name: String) -> Result<(), VaultError>;

    /// Resolve the integration matching a project's `project_type`. Returns
    /// `None` when the project has no type or no matching integration is
    /// registered.
    async fn get_active_for_project(
        &self,
        project_id: Uuid,
    ) -> Result<Option<ProjectTypeView>, VaultError>;
}

/// Argument bundle for [`ProjectTypeService::register_type`]. Collapses
/// to a single Facet-friendly struct so the trait method stays under
/// Vox's tuple-arity limit.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ProjectTypeSpec {
    pub name: String,
    /// JSON-encoded `Vec<StatusDef>`.
    pub statuses_json: String,
    /// JSON-encoded `Vec<TaskTemplate>`.
    pub task_templates_json: String,
    /// JSON-encoded `Vec<ProjectTemplate>` — leave empty if none.
    pub project_templates_json: String,
    pub area_conventions: Vec<String>,
    pub context_conventions: Vec<String>,
}

/// Wire-friendly snapshot of a project-type integration. Lists are sent
/// as JSON-encoded strings to dodge Vox/Facet limitations on nested
/// custom types.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ProjectTypeView {
    pub id: Uuid,
    pub name: String,
    /// JSON-encoded `Vec<StatusDef>`.
    pub statuses_json: String,
    /// JSON-encoded `Vec<TaskTemplate>` (project-template-free standalone
    /// task scaffolding).
    pub task_templates_json: String,
    /// JSON-encoded `Vec<ProjectTemplate>`.
    pub project_templates_json: String,
    pub area_conventions: Vec<String>,
    pub context_conventions: Vec<String>,
}

/// Materializable project templates — the runtime API on top of
/// `ProjectTemplate` entries declared inside `Integration` rows.
///
/// One template plus a target project name produces a fresh project and
/// its scaffolded tasks. Re-running with the same project name is
/// idempotent (deterministic ids derived from
/// `(integration_name, template_name, project_name)`).
#[vox::service]
pub trait TemplateService {
    /// Every project template registered across all integrations.
    async fn list_templates(&self) -> Result<Vec<TemplateView>, VaultError>;

    /// Look up a single template by the integration that owns it and the
    /// template's name.
    async fn get_template(
        &self,
        integration_name: String,
        template_name: String,
    ) -> Result<Option<TemplateView>, VaultError>;

    /// Materialize a project + scaffolded tasks from a template.
    /// `project_type` defaults to the integration's name.
    async fn materialize(
        &self,
        request: MaterializeRequest,
    ) -> Result<MaterializeResult, VaultError>;
}

/// Wire-friendly snapshot of a `ProjectTemplate` from an integration.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TemplateView {
    pub integration_name: String,
    pub name: String,
    pub description: Option<String>,
    /// JSON-encoded `Vec<TaskTemplate>` — what tasks materialize creates.
    pub task_templates_json: String,
}

/// Argument bundle for [`TemplateService::materialize`].
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MaterializeRequest {
    pub integration_name: String,
    pub template_name: String,
    /// Title of the project to create. Reused as the deterministic-id
    /// salt so re-running the same call is idempotent.
    pub project_name: String,
    /// Override `Project::area` if set; otherwise inherit from the
    /// integration's `area_conventions[0]`.
    pub area: Option<String>,
    /// Override `Project::organization` if set.
    pub organization: Option<String>,
    /// Override `Project::lead` if set.
    pub lead: Option<String>,
}

/// Result of materializing a project + its tasks.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MaterializeResult {
    pub project_id: Uuid,
    pub project_title: String,
    pub task_ids: Vec<Uuid>,
    pub created_project: bool,
    pub created_task_count: u32,
    pub unchanged_task_count: u32,
}

/// Specialized workflow service for the `audio-production` integration.
///
/// Tracks are first-class workflow deliverables: ordered, status-bearing
/// rows hung off a project. The service drives the production workflow
/// (Composing → Demo → Tracking → Editing → Mixing → Mastering →
/// Approved → Released), encapsulates revision bookkeeping for mix
/// submissions, and links optional mix-bounce attachments via the
/// existing attachment infrastructure.
#[vox::service]
pub trait AudioProductionService {
    /// Tracks belonging to a project, ordered by sequence.
    async fn list_tracks(&self, project_id: Uuid) -> Result<Vec<TrackApi>, VaultError>;

    /// One track by id.
    async fn get_track(&self, id: Uuid) -> Result<Option<TrackApi>, VaultError>;

    /// Append a track to a project. Sequence auto-increments past the
    /// highest existing sequence in that project when omitted.
    async fn add_track(&self, request: AddTrackRequest) -> Result<TrackApi, VaultError>;

    /// Apply a partial update.
    async fn update_track(&self, id: Uuid, patch: TrackPatch) -> Result<TrackApi, VaultError>;

    /// Move a track to a new status. Validates the transition is one of
    /// the known [`TrackStatus`](crate::track::TrackStatus) values;
    /// returns `ParseError` on unknown.
    async fn transition_status(&self, id: Uuid, new_status: String)
    -> Result<TrackApi, VaultError>;

    /// Mark a track as submitted for mix review:
    /// - Status becomes `Mixing` (kept as `Mastering` if already there).
    /// - `revision_number` increments.
    /// - When a `mix_path` is supplied, an `attachment` row is inserted
    ///   with `owner_type = "track"`, `source = "nextcloud"`, and the
    ///   given path. Bytes are NOT uploaded — that flows through
    ///   [`AttachmentService::upload`] separately.
    async fn submit_mix(&self, request: SubmitMixRequest) -> Result<TrackApi, VaultError>;

    /// Approve a mix — sets status to `Approved` and stamps
    /// `approved_by` / `approved_at` (RFC3339) into the track's
    /// `properties` blob so it's reachable through `PropertyService`.
    async fn approve_mix(&self, id: Uuid, actor: String) -> Result<TrackApi, VaultError>;
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct AddTrackRequest {
    pub project_id: Uuid,
    pub title: String,
    /// Optional. If omitted, the service appends after the current max.
    pub sequence: Option<u32>,
    pub bpm: Option<f64>,
    pub key: Option<String>,
    pub artist: Option<String>,
    pub created_by: Option<String>,
}

/// Free-form patch — only `Some(_)` fields are applied.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TrackPatch {
    pub title: Option<String>,
    pub sequence: Option<u32>,
    pub bpm: Option<f64>,
    pub key: Option<String>,
    pub duration_ms: Option<i64>,
    pub time_signature: Option<String>,
    pub daw_session_path: Option<String>,
    pub stems_path: Option<String>,
    pub reference_url: Option<String>,
    pub isrc: Option<String>,
    pub artist: Option<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SubmitMixRequest {
    pub track_id: Uuid,
    /// Free-text mix notes, captured on the attachment label when no
    /// dedicated label is supplied.
    pub notes: Option<String>,
    /// Path to record as the new mix-bounce attachment. Set to `None`
    /// when there's no bounce to attach.
    pub mix_path: Option<String>,
    pub mix_label: Option<String>,
    pub mix_mime: Option<String>,
    pub uploader: Option<String>,
}

#[vox::service]
pub trait SystemService {
    /// Fast live capability snapshot for this task-server instance.
    async fn capabilities(&self) -> SystemCapabilities;

    /// Operational health checks for configured providers.
    async fn health(&self, deep: bool) -> SystemHealth;
}

/// Sync operation statistics.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SyncStats {
    pub timestamp: String,
    pub calendar_pushed: u32,
    pub calendar_pulled: u32,
    pub deck_pushed: u32,
    pub deck_pulled: u32,
    pub files_created: u32,
    pub files_updated: u32,
    pub errors: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SyncPlanItem {
    pub provider: String,
    pub operation: String,
    pub collection: String,
    pub direction: String,
    pub configured: bool,
    pub destructive: bool,
    pub detail: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SyncPlan {
    pub generated_at: String,
    pub safe_to_run: bool,
    #[facet(default)]
    pub items: Vec<SyncPlanItem>,
    #[facet(default)]
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SystemCapabilities {
    pub package: String,
    pub version: String,
    pub protocol_version: u32,
    pub min_cli_version: String,
    pub min_server_version: String,
    pub services: Vec<String>,
    pub features: Vec<String>,
    pub nextcloud: NextcloudCapability,
    pub vault: VaultCapability,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct NextcloudCapability {
    pub configured: bool,
    pub url: Option<String>,
    pub username: Option<String>,
    pub projects_path: Option<String>,
    pub task_calendar: Option<String>,
    pub event_calendar: Option<String>,
    pub deck_enabled: bool,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct VaultCapability {
    pub root: String,
    pub exists: bool,
    pub index_available: bool,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SystemHealth {
    pub ok: bool,
    pub degraded: bool,
    pub deep: bool,
    pub checks: Vec<HealthCheck>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ProviderSyncState {
    pub provider: String,
    pub account: Option<String>,
    pub collection: String,
    pub sync_token: Option<String>,
    pub cursor: Option<String>,
    pub etag: Option<String>,
    pub last_success_at: Option<String>,
    pub last_failure_at: Option<String>,
    pub last_error: Option<String>,
    pub updated_at: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct HealthCheck {
    pub name: String,
    pub code: String,
    pub severity: String,
    pub ok: bool,
    pub configured: bool,
    pub detail: String,
    pub hint: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct RemoteDeckBoard {
    pub id: u64,
    pub title: String,
    pub archived: bool,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct RemoteDeckStack {
    pub id: u64,
    pub title: String,
    pub card_count: u32,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavDiscovery {
    pub principal_url: String,
    pub calendar_home_set: String,
    pub schedule_inbox_url: Option<String>,
    pub schedule_outbox_url: Option<String>,
    pub calendar_user_addresses: Vec<String>,
    pub calendars: Vec<CalDavCalendarInfo>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavCalendarInfo {
    pub href: String,
    pub name: String,
    pub display_name: Option<String>,
    pub sync_token: Option<String>,
    pub ctag: Option<String>,
    pub components: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavDiscovery {
    pub principal_url: String,
    pub addressbook_home_set: String,
    pub addressbooks: Vec<CardDavAddressBookInfo>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavAddressBookInfo {
    pub href: String,
    pub name: String,
    pub display_name: Option<String>,
    pub description: Option<String>,
    pub sync_token: Option<String>,
    pub ctag: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavObject {
    pub href: String,
    pub etag: Option<String>,
    pub status: String,
    pub address_data: Option<String>,
    pub contact: Option<CardDavContact>,
    pub deleted: bool,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavContact {
    pub uid: Option<String>,
    pub full_name: Option<String>,
    pub family_name: Option<String>,
    pub given_name: Option<String>,
    pub additional_names: Option<String>,
    pub prefixes: Option<String>,
    pub suffixes: Option<String>,
    pub organization: Option<String>,
    pub title: Option<String>,
    pub emails: Vec<String>,
    pub phones: Vec<String>,
    pub urls: Vec<String>,
    pub note: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavObject {
    pub href: String,
    pub etag: Option<String>,
    pub status: String,
    pub component: Option<String>,
    pub calendar_data: Option<String>,
    pub details: Option<CalDavObjectDetails>,
    pub task: Option<Task>,
    pub event: Option<CalendarEvent>,
    pub deleted: bool,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavObjectDetails {
    pub product_id: Option<String>,
    pub method: Option<String>,
    pub timezones: Vec<CalDavTimezone>,
    pub events: Vec<CalDavEventInstance>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavTimezone {
    pub tzid: String,
    pub calendar_data: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavEventInstance {
    pub uid: Option<String>,
    pub summary: Option<String>,
    pub status: Option<String>,
    pub recurrence_id: Option<String>,
    pub dtstart: Option<String>,
    pub dtend: Option<String>,
    pub dtstart_timezone: Option<String>,
    pub dtend_timezone: Option<String>,
    pub recurrence_id_timezone: Option<String>,
    pub rrules: Vec<String>,
    pub rdates: Vec<String>,
    pub exdates: Vec<String>,
    pub organizer: Option<CalDavParticipant>,
    pub attendees: Vec<CalDavParticipant>,
    pub alarms: Vec<CalDavAlarm>,
    pub raw_properties: Vec<CalDavProperty>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavParticipant {
    pub value: String,
    pub cn: Option<String>,
    pub role: Option<String>,
    pub partstat: Option<String>,
    pub rsvp: Option<String>,
    pub cutype: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavAlarm {
    pub action: Option<String>,
    pub trigger: Option<String>,
    pub description: Option<String>,
    pub summary: Option<String>,
    pub attendees: Vec<CalDavParticipant>,
    pub raw_properties: Vec<CalDavProperty>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavProperty {
    pub name: String,
    pub value: String,
    pub parameters: Vec<CalDavParameter>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavParameter {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavMultigetRequest {
    pub calendar: String,
    pub hrefs: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavSyncCollectionRequest {
    pub calendar: String,
    pub sync_token: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavSyncCollectionResponse {
    pub sync_token: Option<String>,
    pub objects: Vec<CalDavObject>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavSyncCollectionResponse {
    pub sync_token: Option<String>,
    pub objects: Vec<CardDavObject>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavPutObjectRequest {
    pub calendar: String,
    pub href: String,
    pub calendar_data: String,
    pub if_match: Option<String>,
    pub if_none_match: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavMultigetRequest {
    pub addressbook: String,
    pub hrefs: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavSyncCollectionRequest {
    pub addressbook: String,
    pub sync_token: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavPutObjectRequest {
    pub addressbook: String,
    pub href: String,
    pub address_data: String,
    pub if_match: Option<String>,
    pub if_none_match: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CardDavDeleteObjectRequest {
    pub addressbook: String,
    pub href: String,
    pub if_match: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavDeleteObjectRequest {
    pub calendar: String,
    pub href: String,
    pub if_match: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavScheduleRequest {
    pub outbox_url: Option<String>,
    pub calendar_data: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavScheduleResponse {
    pub status: u16,
    pub body: String,
}

#[derive(Debug, Clone, facet::Facet)]
pub struct CalDavFreeBusyRequest {
    pub calendar: String,
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

impl Default for CalDavFreeBusyRequest {
    fn default() -> Self {
        let now = Utc::now();
        Self {
            calendar: String::new(),
            start: now,
            end: now,
        }
    }
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalDavFreeBusyInterval {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
    pub busy_type: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CalendarEventPatch {
    pub title: Option<String>,
    pub description: Option<Option<String>>,
    pub location: Option<Option<String>>,
    pub venue: Option<Option<WikiLink>>,
    pub spaces: Option<Vec<WikiLink>>,
    pub start: Option<DateTime<Utc>>,
    pub end: Option<Option<DateTime<Utc>>>,
    pub all_day: Option<bool>,
    pub status: Option<CalendarEventStatus>,
    pub recurrence: Option<Option<String>>,
    pub attendees: Option<Vec<String>>,
    pub body: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FileEntry {
    pub path: String,
    pub name: String,
    pub kind: String,
    pub content_type: Option<String>,
    pub content_length: Option<u64>,
    pub etag: Option<String>,
    pub last_modified: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ProjectFileSummary {
    pub path: String,
    pub name: String,
    pub kind: String,
    pub role: String,
    pub content_type: Option<String>,
    pub content_length: Option<u64>,
    pub last_modified: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ProjectKnowledgeContext {
    pub project: Project,
    pub project_path: String,
    #[facet(default)]
    pub tasks: Vec<Task>,
    pub next_action: Option<Task>,
    #[facet(default)]
    pub files: Vec<ProjectFileSummary>,
    #[facet(default)]
    pub notes: Vec<ProjectFileSummary>,
    #[facet(default)]
    pub decisions: Vec<ProjectFileSummary>,
    #[facet(default)]
    pub deliverables: Vec<ProjectFileSummary>,
    #[facet(default)]
    pub references: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FileReadResponse {
    pub content_base64: String,
    pub content_type: Option<String>,
    pub etag: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FileWriteRequest {
    pub path: String,
    pub content_base64: String,
    pub content_type: Option<String>,
    pub if_match: Option<String>,
    pub if_none_match: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FileCopyMoveRequest {
    pub from: String,
    pub to: String,
    pub overwrite: bool,
}

/// Patch for editing time entries. Only `Some(_)` fields are applied. For
/// `end_time`, `Some(None)` clears the end time and makes the timer running.
#[derive(Debug, Default, Clone, facet::Facet)]
pub struct TimeEntryPatch {
    pub start_time: Option<DateTime<Utc>>,
    pub end_time: Option<Option<DateTime<Utc>>>,
    pub description: Option<String>,
    pub billable: Option<bool>,
    pub billable_rate: Option<u32>,
    pub user: Option<String>,
    pub tags: Option<Vec<String>>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TimeStartRequest {
    pub task_ref: String,
    pub description: Option<String>,
    pub billable: bool,
    pub billable_rate: Option<u32>,
    pub user: Option<String>,
}

#[derive(Debug, Clone, facet::Facet)]
pub struct TimeLogRequest {
    pub task_ref: String,
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
    pub description: Option<String>,
    pub billable: bool,
    pub billable_rate: Option<u32>,
    pub user: Option<String>,
}

impl Default for TimeLogRequest {
    fn default() -> Self {
        let now = Utc::now();
        Self {
            task_ref: String::new(),
            start: now,
            end: now,
            description: None,
            billable: false,
            billable_rate: None,
            user: None,
        }
    }
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InboxCaptureRequest {
    pub text: String,
    pub actor: Option<String>,
    /// Optional capture source such as cli, email, voice, web, or agent.
    pub source: Option<String>,
    /// Optional initial classification. Defaults to inbox.
    pub kind: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InboxPromoteRequest {
    /// Task id or title.
    pub reference: String,
    /// commitment, idea, task, waiting, reference, someday.
    pub kind: Option<String>,
    pub project: Option<String>,
    pub status: Option<String>,
    pub assignee: Option<String>,
    pub due: Option<String>,
    pub scheduled: Option<String>,
    #[facet(default)]
    pub add_tags: Vec<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InboxItem {
    pub id: Option<String>,
    pub title: String,
    pub kind: String,
    pub status: String,
    pub priority: String,
    #[facet(default)]
    pub projects: Vec<String>,
    #[facet(default)]
    pub tags: Vec<String>,
    #[facet(default)]
    pub contexts: Vec<String>,
    pub due: Option<String>,
    pub scheduled: Option<String>,
    pub assignee: Option<String>,
    pub source: Option<String>,
    pub body: String,
}

#[derive(Debug, Clone, facet::Facet)]
pub struct ReviewReport {
    pub generated_at: DateTime<Utc>,
    /// Local date used as the review anchor.
    pub today: String,
    /// Inclusive local date horizon for upcoming work.
    pub horizon_end: String,
    pub stale_after_days: u32,
    #[facet(default)]
    pub inbox: Vec<InboxItem>,
    #[facet(default)]
    pub commitments: Vec<Task>,
    #[facet(default)]
    pub ideas: Vec<Task>,
    #[facet(default)]
    pub someday: Vec<Task>,
    #[facet(default)]
    pub waiting: Vec<Task>,
    #[facet(default)]
    pub overdue: Vec<Task>,
    #[facet(default)]
    pub due_today: Vec<Task>,
    #[facet(default)]
    pub scheduled_today: Vec<Task>,
    #[facet(default)]
    pub upcoming: Vec<Task>,
    #[facet(default)]
    pub unscheduled: Vec<Task>,
    #[facet(default)]
    pub stale: Vec<Task>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct OperatingAreaStatus {
    pub name: String,
    pub open_tasks: u32,
    pub active_projects: u32,
    pub overdue_tasks: u32,
    pub due_today_tasks: u32,
    pub waiting_tasks: u32,
    pub stale_tasks: u32,
    pub routine_tasks: u32,
    pub habit_tasks: u32,
    pub goal_tasks: u32,
    pub next_action: Option<Task>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct OperatingGoal {
    pub title: String,
    pub area: Option<String>,
    pub project: Option<String>,
    pub status: String,
    pub due: Option<String>,
    pub next_action: Option<Task>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct OperatingRoutine {
    pub title: String,
    pub area: Option<String>,
    pub kind: String,
    pub recurrence: Option<String>,
    pub due: Option<String>,
    pub scheduled: Option<String>,
    pub status: String,
}

#[derive(Debug, Clone, facet::Facet)]
pub struct OperatingModelReport {
    pub generated_at: DateTime<Utc>,
    pub today: String,
    #[facet(default)]
    pub areas: Vec<OperatingAreaStatus>,
    #[facet(default)]
    pub goals: Vec<OperatingGoal>,
    #[facet(default)]
    pub routines: Vec<OperatingRoutine>,
    #[facet(default)]
    pub habits: Vec<OperatingRoutine>,
    #[facet(default)]
    pub active_projects: Vec<Project>,
    #[facet(default)]
    pub inbox: Vec<InboxItem>,
    #[facet(default)]
    pub review: ReviewReport,
    pub open_tasks: u32,
    pub overdue_tasks: u32,
    pub due_today_tasks: u32,
    pub waiting_tasks: u32,
    pub stale_tasks: u32,
    pub unscheduled_tasks: u32,
    pub active_timers: u32,
    pub upcoming_events: u32,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct BusinessFinanceClientSummary {
    pub client_name: String,
    pub unbilled_minutes: u32,
    pub unbilled_cents: u64,
    pub open_invoice_cents: u64,
    pub overdue_invoice_cents: u64,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InvoiceAgingBucket {
    pub name: String,
    pub invoice_count: u32,
    pub balance_cents: u64,
}

#[derive(Debug, Clone, facet::Facet)]
pub struct BusinessFinanceReport {
    pub generated_at: DateTime<Utc>,
    pub today: String,
    pub billable_minutes: u32,
    pub unbilled_minutes: u32,
    pub unbilled_cents: u64,
    pub invoiced_cents: u64,
    pub paid_cents: u64,
    pub open_invoice_cents: u64,
    pub overdue_invoice_cents: u64,
    #[facet(default)]
    pub clients: Vec<BusinessFinanceClientSummary>,
    #[facet(default)]
    pub aging: Vec<InvoiceAgingBucket>,
    #[facet(default)]
    pub draft_invoices: Vec<Invoice>,
    #[facet(default)]
    pub open_invoices: Vec<Invoice>,
    #[facet(default)]
    pub unbilled_entries: Vec<TimeEntryContext>,
}

impl Default for BusinessFinanceReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            billable_minutes: 0,
            unbilled_minutes: 0,
            unbilled_cents: 0,
            invoiced_cents: 0,
            paid_cents: 0,
            open_invoice_cents: 0,
            overdue_invoice_cents: 0,
            clients: Vec::new(),
            aging: Vec::new(),
            draft_invoices: Vec::new(),
            open_invoices: Vec::new(),
            unbilled_entries: Vec::new(),
        }
    }
}

impl Default for OperatingModelReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            areas: Vec::new(),
            goals: Vec::new(),
            routines: Vec::new(),
            habits: Vec::new(),
            active_projects: Vec::new(),
            inbox: Vec::new(),
            review: ReviewReport::default(),
            open_tasks: 0,
            overdue_tasks: 0,
            due_today_tasks: 0,
            waiting_tasks: 0,
            stale_tasks: 0,
            unscheduled_tasks: 0,
            active_timers: 0,
            upcoming_events: 0,
        }
    }
}

impl Default for ReviewReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            horizon_end: String::new(),
            stale_after_days: 0,
            inbox: Vec::new(),
            commitments: Vec::new(),
            ideas: Vec::new(),
            someday: Vec::new(),
            waiting: Vec::new(),
            overdue: Vec::new(),
            due_today: Vec::new(),
            scheduled_today: Vec::new(),
            upcoming: Vec::new(),
            unscheduled: Vec::new(),
            stale: Vec::new(),
        }
    }
}

/// Filter for querying time entries across the vault.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TimeEntryFilter {
    pub task_ref: Option<String>,
    pub user: Option<String>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub tag: Option<String>,
    pub from: Option<DateTime<Utc>>,
    pub to: Option<DateTime<Utc>>,
    pub billable_only: bool,
}

/// A time entry joined with the task/project/client context needed for
/// reporting and invoicing.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TimeEntryContext {
    pub task_title: String,
    pub task_projects: Vec<String>,
    pub client_name: Option<String>,
    pub project_rate: Option<u32>,
    pub client_rate: Option<u32>,
    pub entry: TimeEntry,
}

impl TimeEntryContext {
    /// Effective rate given a caller fallback, using the Invoice Ninja-style
    /// cascade: entry override, project default, client default, fallback.
    pub fn effective_rate(&self, fallback: Option<u32>) -> u32 {
        crate::client::resolve_rate(
            self.entry.billable_rate,
            self.project_rate,
            self.client_rate,
            fallback,
        )
    }
}

/// A time entry paired with its owning task.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct TimedTaskEntry {
    pub task_title: String,
    pub entry: TimeEntry,
}

/// Patch for editing project metadata. Only `Some(_)` fields are applied. For
/// optional string fields, passing `"clear"` or `""` clears the value.
#[derive(Debug, Default, Clone, facet::Facet)]
pub struct ProjectPatch {
    pub status: Option<String>,
    pub description: Option<String>,
    pub body: Option<String>,
    pub area: Option<String>,
    pub organization: Option<String>,
    pub project_type: Option<String>,
    pub workflow: Option<String>,
    pub workflow_stage: Option<String>,
    pub identifier: Option<String>,
    pub lead: Option<String>,
    pub default_assignee: Option<String>,
    pub emoji: Option<String>,
    pub repo: Option<String>,
    pub dev_path: Option<String>,
    pub client: Option<String>,
    /// Cents/hr; pass 0 to clear.
    pub default_rate: Option<u32>,
    pub due: Option<String>,
    pub start: Option<String>,

    pub add_tag: Vec<String>,
    pub remove_tag: Vec<String>,
    pub add_email_tag: Vec<String>,
    pub remove_email_tag: Vec<String>,
    pub add_team: Vec<String>,
    pub remove_team: Vec<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InvoiceCreateRequest {
    pub client_name: String,
    pub from: Option<DateTime<Utc>>,
    pub to: Option<DateTime<Utc>>,
    pub fallback_rate: Option<u32>,
    pub tax_rate_percent: Option<f64>,
    pub discount_percent: Option<f64>,
    pub po_number: Option<String>,
    pub public_notes: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct InvoicePaymentRequest {
    pub invoice_id: String,
    pub amount_cents: u64,
    pub method: Option<String>,
    pub reference: Option<String>,
    pub notes: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailListMessagesRequest {
    pub mailbox_id: i64,
    pub filter: Option<String>,
    pub limit: u32,
    pub cursor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailCreateMailboxRequest {
    pub account_id: i64,
    pub name: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailMoveMessageRequest {
    pub message_id: i64,
    pub dest_folder_id: i64,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailCreateTagRequest {
    pub display_name: String,
    pub color: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailDeleteTagRequest {
    pub account_id: i64,
    pub tag_id: i64,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MailMessageTagRequest {
    pub message_id: i64,
    pub imap_label: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct EmailLinkRequest {
    /// "task" or "project".
    pub target_type: String,
    pub reference: String,
    pub email: EmailRef,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct EmailUnlinkRequest {
    /// "task" or "project".
    pub target_type: String,
    pub reference: String,
    pub message_id: String,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct EmailListRequest {
    /// "task" or "project".
    pub target_type: String,
    pub reference: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct EmailLinkResponse {
    pub target_type: String,
    pub title: String,
    pub email_count: u32,
}

/// Specialized workflow service for the `cooking` integration.
///
/// Mirrors Mealie's surface: recipes (with ordered ingredient + step
/// children), cookbooks (named collections, m2m to recipe), meal plans
/// (one slot per `(date, meal_type, organization)` — `set_meal_plan_entry`
/// upserts on that triple), and shopping lists (with optional
/// generate-from-meal-plan).
///
/// Recipe ingredients and steps are JSON-encoded on the wire (one
/// `Vec<RecipeIngredientSpec>` / `Vec<RecipeStepSpec>` per call) to fit
/// inside Vox's tuple-arity budget without exposing one-method-per-child
/// CRUD endpoints.
#[vox::service]
pub trait CookingService {
    // Recipes
    async fn list_recipes(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<crate::recipe::RecipeApi>, VaultError>;
    async fn get_recipe(&self, id: Uuid) -> Result<Option<RecipeWithDetails>, VaultError>;
    async fn create_recipe(
        &self,
        request: CreateRecipeRequest,
    ) -> Result<RecipeWithDetails, VaultError>;
    async fn update_recipe(
        &self,
        id: Uuid,
        patch: RecipePatch,
    ) -> Result<crate::recipe::RecipeApi, VaultError>;
    async fn delete_recipe(&self, id: Uuid) -> Result<(), VaultError>;
    /// Validate `0.0..=5.0` and persist. `ParseError` outside that range.
    async fn rate_recipe(
        &self,
        id: Uuid,
        rating: f32,
    ) -> Result<crate::recipe::RecipeApi, VaultError>;
    /// Stamp `last_made` (defaults to today).
    async fn mark_made(
        &self,
        id: Uuid,
        on_date: Option<chrono::NaiveDate>,
    ) -> Result<crate::recipe::RecipeApi, VaultError>;
    /// Mealie-style URL import. Fetches the page, runs schema.org
    /// JSON-LD + OpenGraph extractors, and persists the result via
    /// `create_recipe`. Image bytes are not fetched in this bead — the
    /// `image_url` is stored as-is on the recipe row.
    async fn import_recipe(
        &self,
        request: ImportRecipeRequest,
    ) -> Result<RecipeWithDetails, VaultError>;
    /// Same fetch+parse pipeline as `import_recipe`, without the
    /// database insert. Returns the assembled `CreateRecipeRequest` as
    /// JSON so callers can review it before committing.
    async fn preview_recipe_import(&self, url: String) -> Result<RecipeImportPreview, VaultError>;

    // Cookbooks
    async fn list_cookbooks(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<crate::cookbook::CookbookApi>, VaultError>;
    async fn get_cookbook(&self, id: Uuid) -> Result<Option<CookbookWithRecipes>, VaultError>;
    async fn create_cookbook(
        &self,
        name: String,
        description: Option<String>,
        organization: Option<String>,
    ) -> Result<crate::cookbook::CookbookApi, VaultError>;
    async fn add_recipe_to_cookbook(
        &self,
        cookbook_id: Uuid,
        recipe_id: Uuid,
    ) -> Result<(), VaultError>;
    async fn remove_recipe_from_cookbook(
        &self,
        cookbook_id: Uuid,
        recipe_id: Uuid,
    ) -> Result<(), VaultError>;

    // Meal plan
    async fn list_meal_plan(
        &self,
        request: MealPlanRangeRequest,
    ) -> Result<Vec<crate::meal_plan::MealPlanEntryApi>, VaultError>;
    /// Upsert on `(date, meal_type, organization)` — see `MealPlanEntry`
    /// docs.
    async fn set_meal_plan_entry(
        &self,
        request: SetMealPlanEntryRequest,
    ) -> Result<crate::meal_plan::MealPlanEntryApi, VaultError>;
    async fn delete_meal_plan_entry(&self, id: Uuid) -> Result<(), VaultError>;

    // Shopping lists
    async fn list_shopping_lists(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<crate::shopping_list::ShoppingListApi>, VaultError>;
    async fn get_shopping_list(
        &self,
        id: Uuid,
    ) -> Result<Option<ShoppingListWithItems>, VaultError>;
    async fn create_shopping_list(
        &self,
        name: String,
        organization: Option<String>,
    ) -> Result<crate::shopping_list::ShoppingListApi, VaultError>;
    /// Append one shopping_list_item per RecipeIngredient across every
    /// meal-plan entry in the requested range. Items inherit `food`,
    /// `unit`, `quantity` and back-reference `recipe_id` + `meal_plan_id`.
    /// Existing items in the list are kept (not deduped).
    async fn generate_from_meal_plan(
        &self,
        request: GenerateShoppingListRequest,
    ) -> Result<ShoppingListWithItems, VaultError>;
    async fn check_item(&self, item_id: Uuid, checked: bool) -> Result<(), VaultError>;
    async fn add_shopping_list_item(
        &self,
        request: AddShoppingItemRequest,
    ) -> Result<(), VaultError>;
}

/// Wire-friendly Recipe-with-children view. `ingredients_json` and
/// `steps_json` are JSON-encoded `Vec<RecipeIngredientApi>` /
/// `Vec<RecipeStepApi>`, ordered.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct RecipeWithDetails {
    pub recipe: crate::recipe::RecipeApi,
    pub ingredients_json: String,
    pub steps_json: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CookbookWithRecipes {
    pub cookbook: crate::cookbook::CookbookApi,
    /// JSON-encoded `Vec<RecipeApi>`, ordered by sequence.
    pub recipes_json: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ShoppingListWithItems {
    pub list: crate::shopping_list::ShoppingListApi,
    /// JSON-encoded `Vec<ShoppingListItemApi>`, ordered by sequence.
    pub items_json: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CreateRecipeRequest {
    pub name: String,
    pub description: Option<String>,
    pub organization: Option<String>,
    pub prep_time_minutes: Option<u32>,
    pub cook_time_minutes: Option<u32>,
    pub servings: Option<u32>,
    pub source_url: Option<String>,
    pub created_by: Option<String>,
    /// Original image URL — populated by the recipe importer. Stored
    /// as-is on the recipe row; byte fetch + Nextcloud PUT is deferred.
    pub image_url: Option<String>,
    /// Free-form yield string ("12 cookies", "1 loaf"). When provided
    /// alongside `servings`, both are persisted.
    pub yield_label: Option<String>,
    /// Optional JSON-encoded object that will be merged into the
    /// recipe row's `properties` blob (cuisine, category, keywords,
    /// …). Empty/`None` leaves `properties` as the default `{}`.
    pub properties_json: Option<String>,
    /// JSON-encoded `Vec<RecipeIngredientSpec>`. Empty string means
    /// "no ingredients".
    pub ingredients_json: String,
    /// JSON-encoded `Vec<RecipeStepSpec>`.
    pub steps_json: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct RecipePatch {
    pub name: Option<String>,
    pub description: Option<String>,
    pub prep_time_minutes: Option<u32>,
    pub cook_time_minutes: Option<u32>,
    pub servings: Option<u32>,
    pub source_url: Option<String>,
    pub yield_label: Option<String>,
    pub notes: Option<String>,
    /// Fully replaces ingredients when Some. JSON-encoded
    /// `Vec<RecipeIngredientSpec>`.
    pub ingredients_json: Option<String>,
    /// Fully replaces steps when Some. JSON-encoded
    /// `Vec<RecipeStepSpec>`.
    pub steps_json: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct MealPlanRangeRequest {
    pub organization: Option<String>,
    pub from_date: chrono::NaiveDate,
    pub to_date: chrono::NaiveDate,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SetMealPlanEntryRequest {
    pub date: chrono::NaiveDate,
    /// Parsed via [`crate::meal_plan::MealType::parse`]. Unknown values
    /// return `ParseError`.
    pub meal_type: String,
    pub organization: Option<String>,
    pub recipe_id: Option<Uuid>,
    pub title: Option<String>,
    pub servings_planned: Option<u32>,
    pub notes: Option<String>,
    pub created_by: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct GenerateShoppingListRequest {
    pub list_id: Uuid,
    pub organization: Option<String>,
    pub from_date: chrono::NaiveDate,
    pub to_date: chrono::NaiveDate,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct ImportRecipeRequest {
    /// Source URL to fetch and parse.
    pub url: String,
    /// Owning organization — applied to the persisted recipe.
    pub organization: Option<String>,
    /// Author/created_by — applied to the persisted recipe when the
    /// schema.org metadata didn't surface its own author.
    pub created_by: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct RecipeImportPreview {
    /// JSON-encoded [`CreateRecipeRequest`] — exactly what would have
    /// been passed to [`CookingService::create_recipe`].
    pub draft_json: String,
    /// Final URL (after any redirects) the importer fetched.
    pub source_url: String,
    /// Which strategy populated the draft: `"json-ld"` or
    /// `"opengraph"`.
    pub strategy: String,
    /// Free-form warnings. JSON-encoded `Vec<String>` for transport
    /// simplicity.
    pub warnings_json: String,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct AddShoppingItemRequest {
    pub list_id: Uuid,
    pub food: String,
    pub quantity: Option<f64>,
    pub unit: Option<String>,
    pub note: Option<String>,
    pub label: Option<String>,
}

// ── Food + FoodProduct ──────────────────────────────────────────────

/// Canonical ingredient catalog + branded products. The `food_id` link
/// on `RecipeIngredient` is populated by name-match on insert (see
/// `crate::food::find_food_by_name`), so most callers only touch this
/// service directly when curating the catalog or attaching barcoded
/// products.
#[vox::service]
pub trait FoodService {
    // ── Foods (canonical ingredients) ──────────────────────────────
    async fn list_foods(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<crate::food::FoodApi>, VaultError>;
    async fn get_food(&self, id: Uuid) -> Result<Option<crate::food::FoodApi>, VaultError>;
    /// Look up a Food by canonical name OR any alias
    /// (case-insensitive). Returns the first match within the
    /// organization (or global when `organization = None`).
    async fn find_food_by_name(
        &self,
        organization: Option<String>,
        name_or_alias: String,
    ) -> Result<Option<crate::food::FoodApi>, VaultError>;
    async fn create_food(
        &self,
        request: CreateFoodRequest,
    ) -> Result<crate::food::FoodApi, VaultError>;
    async fn update_food(
        &self,
        id: Uuid,
        patch: FoodPatch,
    ) -> Result<crate::food::FoodApi, VaultError>;
    async fn delete_food(&self, id: Uuid) -> Result<(), VaultError>;
    /// Add an alias to an existing Food. Idempotent — duplicates
    /// (case-insensitive) are silently dropped.
    async fn add_food_alias(
        &self,
        food_id: Uuid,
        alias: String,
    ) -> Result<crate::food::FoodApi, VaultError>;

    // ── Food products (branded) ────────────────────────────────────
    async fn list_food_products(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<crate::food_product::FoodProductApi>, VaultError>;
    async fn get_food_product(
        &self,
        id: Uuid,
    ) -> Result<Option<crate::food_product::FoodProductApi>, VaultError>;
    async fn get_food_product_by_barcode(
        &self,
        organization: Option<String>,
        barcode: String,
    ) -> Result<Option<crate::food_product::FoodProductApi>, VaultError>;
    async fn create_food_product(
        &self,
        request: CreateFoodProductRequest,
    ) -> Result<crate::food_product::FoodProductApi, VaultError>;
    async fn update_food_product(
        &self,
        id: Uuid,
        patch: FoodProductPatch,
    ) -> Result<crate::food_product::FoodProductApi, VaultError>;

    /// Manually link a `RecipeIngredient` row to a `Food`. Used when
    /// the auto name-match on recipe insert didn't find a hit and the
    /// user wires the link by hand later.
    async fn link_recipe_ingredient(
        &self,
        recipe_ingredient_id: Uuid,
        food_id: Uuid,
    ) -> Result<(), VaultError>;
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CreateFoodRequest {
    pub name: String,
    pub aliases: Vec<String>,
    pub category: Option<String>,
    pub default_unit: Option<String>,
    pub organization: Option<String>,
    /// JSON-encoded [`crate::nutrition::NutritionFacts`].
    pub nutrition_json: Option<String>,
    pub notes: Option<String>,
    pub created_by: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FoodPatch {
    pub name: Option<String>,
    pub category: Option<String>,
    pub default_unit: Option<String>,
    /// `Some([])` clears; `None` leaves unchanged.
    pub aliases: Option<Vec<String>>,
    pub nutrition_json: Option<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct CreateFoodProductRequest {
    pub food_id: Uuid,
    pub barcode: Option<String>,
    pub brand: Option<String>,
    pub name: String,
    pub package_size_g: Option<f64>,
    pub package_size_label: Option<String>,
    pub source: String,
    pub external_id: Option<String>,
    pub nutrition_json: Option<String>,
    pub image_url: Option<String>,
    pub organization: Option<String>,
}

#[derive(Debug, Clone, Default, facet::Facet)]
pub struct FoodProductPatch {
    pub brand: Option<String>,
    pub name: Option<String>,
    pub package_size_g: Option<f64>,
    pub package_size_label: Option<String>,
    pub nutrition_json: Option<String>,
    pub image_url: Option<String>,
}

/// Errors returned by vault operations.
#[derive(Debug, facet::Facet, thiserror::Error)]
#[repr(C)]
pub enum VaultError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("parse error: {0}")]
    ParseError(String),
    #[error("io error: {0}")]
    IoError(String),
}
