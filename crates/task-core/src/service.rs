//! Vox service definitions for task-core.
//!
//! The core surface is intentionally split by product area so clients can bind
//! only the APIs they need while still sharing one implementation.

use chrono::{DateTime, Utc};

use crate::calendar_event::{CalendarEvent, CalendarEventStatus};
use crate::client::Client;
use crate::email::EmailRef;
use crate::index::{ChangeRow, ConflictRow};
use crate::invoice::Invoice;
use crate::project::{Project, ProjectStats};
use crate::provider::{MailAccount, MailMessage, MailMessageDetail, MailTag, Mailbox};
use crate::query::Query;
use crate::task::{Task, TimeEntry};

#[vox::service]
pub trait TaskService {
    /// Return all tasks in the vault.
    async fn list_tasks(&self) -> Vec<Task>;

    /// Execute a query and return matching tasks sorted by the query's sort.
    async fn execute_query(&self, query: Query) -> Vec<Task>;

    /// Compute the urgency score for a single task.
    async fn urgency_score(&self, task: Task) -> i32;

    /// Create a new task. Returns the created task with generated id/dates filled in.
    async fn create_task(&self, task: Task) -> Result<Task, VaultError>;

    /// Update an existing task. Returns the updated task.
    async fn update_task(&self, task: Task) -> Result<Task, VaultError>;

    /// Mark a task complete. Handles recurrence logic and sets completedDate.
    async fn complete_task(&self, title: String) -> Result<Task, VaultError>;

    /// Delete a task by title.
    async fn delete_task(&self, title: String) -> Result<(), VaultError>;

    /// Search tasks by text query (uses FTS5 index).
    async fn search_tasks(&self, query: String) -> Vec<Task>;

    /// Get tasks assigned to a specific user.
    async fn tasks_for_user(&self, username: String) -> Vec<Task>;
}

#[vox::service]
pub trait ProjectService {
    /// Return all projects in the vault.
    async fn list_projects(&self) -> Vec<Project>;

    /// Update project metadata.
    async fn update_project(
        &self,
        title: String,
        patch: ProjectPatch,
        actor: Option<String>,
    ) -> Result<Project, VaultError>;

    /// Return task count stats for a project.
    async fn project_stats(&self, project_title: String) -> ProjectStats;

    /// Return the next actionable task for a project.
    async fn next_task(&self, project_title: String) -> Option<Task>;

    /// Get all tasks for a project.
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task>;
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
    async fn delete_time_entry(&self, entry_id: String, actor: Option<String>) -> Result<(), VaultError>;
}

#[vox::service]
pub trait ClientService {
    async fn list_clients(&self) -> Vec<Client>;
    async fn save_client(&self, client: Client) -> Result<Client, VaultError>;
    async fn find_client(&self, name: String) -> Option<Client>;
}

#[vox::service]
pub trait InvoiceService {
    async fn create_invoice_from_entries(
        &self,
        request: InvoiceCreateRequest,
    ) -> Result<Invoice, VaultError>;

    async fn list_invoices(&self) -> Vec<Invoice>;
    async fn get_invoice(&self, invoice_id: String) -> Option<Invoice>;
    async fn send_invoice(&self, invoice_id: String, actor: Option<String>) -> Result<Invoice, VaultError>;
    async fn record_invoice_payment(&self, request: InvoicePaymentRequest) -> Result<Invoice, VaultError>;
    async fn cancel_invoice(
        &self,
        invoice_id: String,
        reason: Option<String>,
        actor: Option<String>,
    ) -> Result<Invoice, VaultError>;
}

#[vox::service]
pub trait ActivityService {
    async fn recent_activity(&self, limit: u32) -> Result<Vec<ChangeRow>, VaultError>;
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

    /// Create a first-class calendar event.
    async fn create_event(&self, event: CalendarEvent) -> Result<CalendarEvent, VaultError>;

    /// Update mutable calendar event fields by id or title.
    async fn update_event(
        &self,
        event_ref: String,
        patch: CalendarEventPatch,
    ) -> Result<CalendarEvent, VaultError>;

    /// Delete a calendar event by id or title.
    async fn delete_event(&self, event_ref: String) -> Result<(), VaultError>;

    /// Trigger a Nextcloud sync cycle. Returns sync stats.
    async fn trigger_sync(&self) -> Result<SyncStats, VaultError>;

    /// Get the last sync result.
    async fn sync_status(&self) -> Option<SyncStats>;

    /// Discover CalDAV principal, calendar home, and available calendars.
    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError>;

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

    /// Put one raw VCALENDAR object, optionally guarded by ETag preconditions.
    async fn put_calendar_object(&self, request: CalDavPutObjectRequest) -> Result<(), VaultError>;

    /// Delete one raw calendar object, optionally guarded by If-Match.
    async fn delete_calendar_object(
        &self,
        request: CalDavDeleteObjectRequest,
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
pub trait FileService {
    /// List files/directories under the configured Nextcloud project root.
    async fn list_files(&self, path: String, depth: String) -> Result<Vec<FileEntry>, VaultError>;

    /// Return metadata for one file/directory.
    async fn stat_file(&self, path: String) -> Result<Option<FileEntry>, VaultError>;

    /// Read a file as base64 content.
    async fn read_file(&self, path: String) -> Result<Option<FileReadResponse>, VaultError>;

    /// Write a file from base64 content.
    async fn write_file(&self, request: FileWriteRequest) -> Result<(), VaultError>;

    /// Create a directory.
    async fn create_dir(&self, path: String) -> Result<(), VaultError>;

    /// Delete a file or directory.
    async fn delete_file(&self, path: String) -> Result<(), VaultError>;

    /// Copy a file or directory.
    async fn copy_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError>;

    /// Move or rename a file or directory.
    async fn move_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError>;
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
pub struct CalDavPutObjectRequest {
    pub calendar: String,
    pub href: String,
    pub calendar_data: String,
    pub if_match: Option<String>,
    pub if_none_match: Option<String>,
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
