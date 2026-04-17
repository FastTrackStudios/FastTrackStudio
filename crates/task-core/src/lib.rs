pub mod task;
pub mod project;
pub mod client;
pub mod invoice;
pub mod query;
pub mod rrule;
pub mod capture;
pub mod workflows;
pub mod views;
pub mod cycle;
pub mod module;
pub mod team;

#[cfg(feature = "server")]
pub mod service;
#[cfg(feature = "server")]
pub mod vault;
#[cfg(feature = "server")]
pub mod service_impl;
#[cfg(feature = "server")]
pub mod watch;
#[cfg(feature = "server")]
pub mod integration;
#[cfg(feature = "server")]
pub mod project_vault;
#[cfg(feature = "server")]
pub mod provider;
#[cfg(feature = "server")]
pub mod index;

#[cfg(feature = "caldav")]
pub mod caldav;

#[cfg(feature = "realtime")]
pub mod crdt;

pub use task::{
    DependencyRelType, Priority, RecurrenceAnchor, Reminder, ReminderAnchor, Status, Task,
    TaskDependency, TaskRelation, RelationType, Reaction, TimeEntry, WikiLink,
};
pub use project::{next_task, Project, ProjectStatus, ProjectStats};
pub use client::{resolve_rate, Client};
pub use invoice::{format_invoice_id, Invoice, InvoiceLine, InvoiceStatus, Payment};
pub use views::{SavedView, ViewFilters, ViewDisplay};
pub use cycle::{Cycle, CycleStatus};
pub use module::{Module, ModuleStatus};
pub use query::{Filter, Group, GroupedTasks, TaskGroup, Query, Sort};
pub use capture::{CaptureInput, parse_capture};

#[cfg(feature = "server")]
pub use service::{VaultError, VaultService, VaultServiceDispatcher, SyncStats, vault_service_service_descriptor};
#[cfg(feature = "server")]
pub use vault::Vault;
#[cfg(feature = "server")]
pub use service_impl::{
    TimeEntryContext, TimeEntryFilter, TimeEntryPatch, VaultServiceImpl, VaultKind, VaultSource,
};
#[cfg(feature = "server")]
pub use watch::WatchHandle;
#[cfg(feature = "server")]
pub use integration::{
    Integration, ProjectTemplate, StatusDef, TaskTemplate,
    load_integration, list_integrations,
};
#[cfg(feature = "server")]
pub use project_vault::{
    ProjectWithTasks, scan_project_vault, save_project_task,
    save_project_metadata, create_project,
};
#[cfg(feature = "server")]
pub use provider::{
    ProjectProvider, ProjectBundle, ProviderInfo, ProviderEvent,
    ProjectRegistry, LocalProvider, VaultProvider,
    S3Provider, S3Config, WebDavProvider, WebDavConfig,
    NextcloudProvider, NextcloudConfig,
};

#[cfg(feature = "caldav")]

#[cfg(feature = "realtime")]
pub use caldav::{
    task_to_vtodo, vtodo_to_task, task_to_ics, ics_to_task,
    CalDavClient, CalDavConfig,
};
