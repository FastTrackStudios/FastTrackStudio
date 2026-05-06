//! Vox service definitions for task-core.
//!
//! The core surface is intentionally split by product area so clients can bind
//! only the APIs they need while still sharing one implementation.

use chrono::{DateTime, Utc};

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
