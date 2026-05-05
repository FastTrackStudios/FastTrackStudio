pub mod agent;
pub mod asset;
pub mod calendar_event;
pub mod capture;
pub mod client;
pub mod cycle;
pub mod email;
pub mod expense;
pub mod invoice;
pub mod location;
pub mod module;
pub mod people;
pub mod project;
pub mod query;
pub mod revenue;
pub mod rrule;
pub mod task;
pub mod team;
pub mod views;
pub mod workflows;

#[cfg(feature = "server")]
pub mod index;
#[cfg(feature = "server")]
pub mod integration;
#[cfg(feature = "server")]
pub mod project_vault;
#[cfg(feature = "server")]
pub mod provider;
#[cfg(feature = "server")]
pub mod service;
#[cfg(feature = "server")]
pub mod service_impl;
#[cfg(feature = "server")]
pub mod vault;
#[cfg(feature = "server")]
pub mod watch;

#[cfg(feature = "caldav")]
pub mod caldav;

#[cfg(feature = "realtime")]
pub mod crdt;

pub use agent::{
    AgentPlan, AgentPlanEdge, AgentPlanNode, AgentPlanNodeKind, AgentPlanNodeStatus,
    build_agent_plan,
};
pub use asset::{
    Asset, AssetBucket, AssetConflict, AssetCreateRequest, AssetFilter, AssetMaintenanceRecord,
    AssetMaintenanceRequest, AssetPatch, AssetRepairRequest, AssetRepairResponse, AssetReport,
    AssetReservationRecord, AssetReservationResponse, AssetReserveRequest, AssetStatus,
    format_asset_id,
};
pub use calendar_event::{CalendarEvent, CalendarEventStatus};
pub use capture::{CaptureInput, parse_capture};
pub use client::{Client, resolve_rate};
pub use cycle::{Cycle, CycleStatus};
pub use email::EmailRef;
pub use expense::{
    Expense, ExpenseBucket, ExpenseCreateRequest, ExpensePatch, ExpenseReport, ExpenseStatus,
    format_expense_id,
};
pub use invoice::{Invoice, InvoiceLine, InvoiceStatus, Payment, format_invoice_id};
pub use location::{Location, Space, VenueDefault, render_location_body};
pub use module::{Module, ModuleStatus};
pub use people::{
    CommunicationRef, ContactMethod, OrganizationContext, OrganizationRecord, Person,
    PersonContext, ProviderConflict, ProviderConflictField, ProviderRef, Relationship,
};
pub use project::{
    Project, ProjectDashboardBucket, ProjectDashboardEntry, ProjectStats, ProjectStatus, next_task,
    project_dashboard,
};
pub use query::{Filter, Group, GroupedTasks, Query, Sort, TaskGroup};
pub use revenue::{
    Revenue, RevenueBucket, RevenueCreateRequest, RevenueFilter, RevenueReport, format_revenue_id,
};
pub use task::{
    DependencyRelType, Priority, Reaction, RecurrenceAnchor, RelationType, Reminder,
    ReminderAnchor, Status, Task, TaskDependency, TaskRelation, TimeEntry, WikiLink,
};
pub use views::{SavedView, ViewDisplay, ViewFilters};

#[cfg(feature = "server")]
pub use integration::{
    Integration, ProjectTemplate, StatusDef, TaskTemplate, list_integrations, load_integration,
};
#[cfg(feature = "server")]
pub use project_vault::{
    ProjectWithTasks, create_project, save_project_metadata, save_project_task, scan_project_vault,
};
#[cfg(feature = "server")]
pub use provider::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, CommunicationChannelProvider,
    LocalProvider, NextcloudConfig, NextcloudProvider, ProjectBundle, ProjectProvider,
    ProjectRegistry, ProviderEvent, ProviderInfo, S3Config, S3Provider, VaultProvider,
    WebDavConfig, WebDavProvider,
};
#[cfg(feature = "server")]
pub use service::{
    ActivityService, ActivityServiceDispatcher, BusinessFinanceClientSummary,
    BusinessFinanceReport, CalDavCalendarInfo, CalDavDeleteObjectRequest, CalDavDiscovery,
    CalDavFreeBusyInterval, CalDavFreeBusyRequest, CalDavMultigetRequest, CalDavObject,
    CalDavPutObjectRequest, CalDavScheduleRequest, CalDavScheduleResponse,
    CalDavSyncCollectionRequest, CalDavSyncCollectionResponse, CalendarEventPatch, CalendarService,
    CalendarServiceDispatcher, CardDavAddressBookInfo, CardDavContact, CardDavDeleteObjectRequest,
    CardDavDiscovery, CardDavMultigetRequest, CardDavObject, CardDavPutObjectRequest,
    CardDavSyncCollectionRequest, CardDavSyncCollectionResponse, ClientService,
    ClientServiceDispatcher, ConversationService, ConversationServiceDispatcher, EmailLinkRequest,
    EmailLinkResponse, EmailListRequest, EmailUnlinkRequest, FileCopyMoveRequest, FileEntry,
    FileReadResponse, FileService, FileServiceDispatcher, FileWriteRequest, HealthCheck,
    InboxCaptureRequest, InboxItem, InboxPromoteRequest, InboxService, InboxServiceDispatcher,
    InvoiceAgingBucket, InvoiceCreateRequest, InvoicePaymentRequest, InvoiceService,
    InvoiceServiceDispatcher, MailCreateMailboxRequest, MailCreateTagRequest, MailDeleteTagRequest,
    MailListMessagesRequest, MailMessageTagRequest, MailMoveMessageRequest, MailService,
    MailServiceDispatcher, NextcloudCapability, OperatingAreaStatus, OperatingGoal,
    OperatingModelReport, OperatingRoutine, OperatingService, OperatingServiceDispatcher,
    PeopleService, PeopleServiceDispatcher, ProjectFileSummary, ProjectKnowledgeContext,
    ProjectPatch, ProjectService, ProjectServiceDispatcher, ProviderSyncState, RemoteDeckBoard,
    RemoteDeckStack, ReviewReport, SyncPlan, SyncPlanItem, SyncStats, SystemCapabilities,
    SystemHealth, SystemService, SystemServiceDispatcher, TaskService, TaskServiceDispatcher,
    TimeEntryContext, TimeEntryFilter, TimeEntryPatch, TimeLogRequest, TimeService,
    TimeServiceDispatcher, TimeStartRequest, TimedTaskEntry, VaultCapability, VaultError,
    activity_service_service_descriptor, calendar_service_service_descriptor,
    client_service_service_descriptor, conversation_service_service_descriptor,
    file_service_service_descriptor, inbox_service_service_descriptor,
    invoice_service_service_descriptor, mail_service_service_descriptor,
    operating_service_service_descriptor, people_service_service_descriptor,
    project_service_service_descriptor, system_service_service_descriptor,
    task_service_service_descriptor, time_service_service_descriptor,
};
#[cfg(feature = "server")]
pub use service_impl::{VaultKind, VaultServiceImpl, VaultSource};
#[cfg(feature = "server")]
pub use vault::Vault;
#[cfg(feature = "server")]
pub use watch::WatchHandle;

#[cfg(feature = "caldav")]
#[cfg(feature = "realtime")]
pub use caldav::{
    CalDavClient, CalDavConfig, ics_to_task, task_to_ics, task_to_vtodo, vtodo_to_task,
};
