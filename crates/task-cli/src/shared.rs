//! Cross-cutting helpers shared across command modules.
//!
//! Holds:
//! - [`RemoteVoxConfig`] — connection-bearing handle to remote services.
//! - [`ServerProfiles`] — TSV-backed server profile config.
//! - JSON adapters (`model_to_api`, `api_to_model`) bridging facet/serde models
//!   and serde-only API DTOs.
//! - `remote_*_with_client` helpers that wrap repo CRUD + adapter calls.
//! - URL helpers for normalizing/encoding remote endpoints.

use chrono::{Datelike, NaiveDate, Utc};
use serde::{Serialize, de::DeserializeOwned};
use task_core::expense::{ExpenseCreateRequest, ExpensePatch};
use task_core::{CalendarEvent, Client, ExpenseStatus, Invoice, Project, Task, WikiLink};
use uuid::Uuid;

// ── Server profiles (TSV-backed config) ─────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub(crate) struct ServerProfiles {
    pub(crate) default: Option<String>,
    pub(crate) servers: Vec<ServerProfile>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ServerProfile {
    pub(crate) name: String,
    pub(crate) url: String,
    pub(crate) session_token: Option<String>,
    pub(crate) organization_id: Option<String>,
}

impl ServerProfiles {
    pub(crate) fn resolve(&self, name_or_url: &str) -> Option<ServerProfile> {
        let requested_url = normalize_profile_url(name_or_url);
        self.servers
            .iter()
            .find(|profile| profile.name == name_or_url)
            .or_else(|| {
                self.servers
                    .iter()
                    .find(|profile| normalize_profile_url(&profile.url) == requested_url)
            })
            .cloned()
            .or_else(|| {
                if name_or_url == "default" {
                    self.current()
                } else {
                    None
                }
            })
    }

    pub(crate) fn current(&self) -> Option<ServerProfile> {
        self.default
            .as_deref()
            .and_then(|name| self.servers.iter().find(|p| p.name == name))
            .cloned()
    }
}

pub(crate) fn server_profiles_path() -> eyre::Result<std::path::PathBuf> {
    let base = std::env::var("TASK_CONFIG_DIR")
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            std::env::var("HOME")
                .ok()
                .map(|home| std::path::PathBuf::from(home).join(".config/task"))
        })
        .ok_or_else(|| eyre::eyre!("Set HOME or TASK_CONFIG_DIR to store server profiles."))?;
    Ok(base.join("servers.tsv"))
}

pub(crate) fn load_server_profiles() -> eyre::Result<ServerProfiles> {
    let path = server_profiles_path()?;
    let Ok(content) = std::fs::read_to_string(path) else {
        return Ok(ServerProfiles::default());
    };
    let mut profiles = ServerProfiles::default();
    for line in content.lines() {
        let parts: Vec<_> = line.split('\t').collect();
        match parts.as_slice() {
            ["default", name] => profiles.default = Some((*name).to_string()),
            ["server", name, url, token, org] => profiles.servers.push(ServerProfile {
                name: (*name).to_string(),
                url: (*url).to_string(),
                session_token: if token.is_empty() {
                    None
                } else {
                    Some((*token).to_string())
                },
                organization_id: if org.is_empty() {
                    None
                } else {
                    Some((*org).to_string())
                },
            }),
            _ => {}
        }
    }
    Ok(profiles)
}

pub(crate) fn save_server_profiles(profiles: &ServerProfiles) -> eyre::Result<()> {
    let path = server_profiles_path()?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut content = String::new();
    if let Some(default) = &profiles.default {
        content.push_str(&format!("default\t{}\n", tsv_escape(default)));
    }
    for profile in &profiles.servers {
        content.push_str(&format!(
            "server\t{}\t{}\t{}\t{}\n",
            tsv_escape(&profile.name),
            tsv_escape(&profile.url),
            tsv_escape(profile.session_token.as_deref().unwrap_or("")),
            tsv_escape(profile.organization_id.as_deref().unwrap_or(""))
        ));
    }
    std::fs::write(path, content)?;
    Ok(())
}

pub(crate) fn tsv_escape(value: &str) -> String {
    value.replace(['\t', '\n', '\r'], " ")
}

// ── URL helpers ─────────────────────────────────────────────────────────────

pub(crate) fn normalize_profile_url(url: &str) -> String {
    url.trim().trim_end_matches('/').to_string()
}

pub(crate) fn normalize_vox_url(server: &str) -> String {
    let trimmed = server.trim().trim_end_matches('/');
    if trimmed.starts_with("ws://") || trimmed.starts_with("wss://") {
        trimmed.to_string()
    } else if let Some(rest) = trimmed.strip_prefix("https://") {
        format!("wss://{}/vox", rest.trim_end_matches("/vox"))
    } else if let Some(rest) = trimmed.strip_prefix("http://") {
        format!("ws://{}/vox", rest.trim_end_matches("/vox"))
    } else {
        format!("ws://{}/vox", trimmed.trim_end_matches("/vox"))
    }
}

pub(crate) fn append_query_param(url: &mut String, key: &str, value: &str) {
    let separator = if url.contains('?') { '&' } else { '?' };
    url.push(separator);
    url.push_str(key);
    url.push('=');
    url.push_str(&percent_encode_query_value(value));
}

pub(crate) fn percent_encode_query_value(value: &str) -> String {
    let mut out = String::new();
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                out.push(byte as char);
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}

// ── Remote Vox connection handle ────────────────────────────────────────────

#[derive(Debug, Clone)]
pub(crate) struct RemoteVoxConfig {
    pub(crate) vox_url: String,
    pub(crate) display_url: String,
    pub(crate) profile_name: Option<String>,
}

impl RemoteVoxConfig {
    pub(crate) fn new(
        server: String,
        session_token: Option<String>,
        organization_id: Option<String>,
    ) -> eyre::Result<Self> {
        let profile = load_server_profiles()
            .ok()
            .and_then(|config| config.resolve(&server));
        let server_url = profile
            .as_ref()
            .map(|profile| profile.url.clone())
            .unwrap_or(server);
        let token = session_token
            .or_else(|| {
                profile
                    .as_ref()
                    .and_then(|profile| profile.session_token.clone())
            })
            .filter(|s| !s.is_empty())
            .ok_or_else(|| {
                eyre::eyre!("Remote mode requires --session-token or TASK_SESSION_TOKEN.")
            })?;
        let organization_id = organization_id.or_else(|| {
            profile
                .as_ref()
                .and_then(|profile| profile.organization_id.clone())
        });
        let base_vox_url = normalize_vox_url(&server_url);
        let mut vox_url = base_vox_url.clone();
        append_query_param(&mut vox_url, "token", &token);
        let mut display_url = base_vox_url;
        append_query_param(&mut display_url, "token", "<redacted>");
        if let Some(org) = organization_id.filter(|s| !s.is_empty()) {
            append_query_param(&mut vox_url, "organization_id", &org);
            append_query_param(&mut display_url, "organization_id", &org);
        }
        Ok(Self {
            vox_url,
            display_url,
            profile_name: profile.map(|profile| profile.name),
        })
    }

    pub(crate) async fn task_repo(&self) -> eyre::Result<task_core::task::TaskRepoClient> {
        self.connect().await
    }

    pub(crate) async fn project_repo(&self) -> eyre::Result<task_core::project::ProjectRepoClient> {
        self.connect().await
    }

    pub(crate) async fn client_repo(&self) -> eyre::Result<task_core::client::ClientRepoClient> {
        self.connect().await
    }

    pub(crate) async fn expense_repo(&self) -> eyre::Result<task_core::expense::ExpenseRepoClient> {
        self.connect().await
    }

    pub(crate) async fn invoice_repo(&self) -> eyre::Result<task_core::invoice::InvoiceRepoClient> {
        self.connect().await
    }

    pub(crate) async fn calendar_event_repo(
        &self,
    ) -> eyre::Result<task_core::calendar_event::CalendarEventRepoClient> {
        self.connect().await
    }

    pub(crate) async fn asset_repo(&self) -> eyre::Result<task_core::asset::AssetRepoClient> {
        self.connect().await
    }

    pub(crate) async fn task(&self) -> eyre::Result<task_core::service::TaskServiceClient> {
        self.connect().await
    }

    pub(crate) async fn inbox(&self) -> eyre::Result<task_core::service::InboxServiceClient> {
        self.connect().await
    }

    pub(crate) async fn project(&self) -> eyre::Result<task_core::service::ProjectServiceClient> {
        self.connect().await
    }

    pub(crate) async fn time(&self) -> eyre::Result<task_core::service::TimeServiceClient> {
        self.connect().await
    }

    pub(crate) async fn people(&self) -> eyre::Result<task_core::service::PeopleServiceClient> {
        self.connect().await
    }

    pub(crate) async fn conversation(
        &self,
    ) -> eyre::Result<task_core::service::ConversationServiceClient> {
        self.connect().await
    }

    pub(crate) async fn operating(
        &self,
    ) -> eyre::Result<task_core::service::OperatingServiceClient> {
        self.connect().await
    }

    pub(crate) async fn invoice(&self) -> eyre::Result<task_core::service::InvoiceServiceClient> {
        self.connect().await
    }

    pub(crate) async fn activity(&self) -> eyre::Result<task_core::service::ActivityServiceClient> {
        self.connect().await
    }

    pub(crate) async fn mail(&self) -> eyre::Result<task_core::service::MailServiceClient> {
        self.connect().await
    }

    pub(crate) async fn calendar(&self) -> eyre::Result<task_core::service::CalendarServiceClient> {
        self.connect().await
    }

    pub(crate) async fn system(&self) -> eyre::Result<task_core::service::SystemServiceClient> {
        self.connect().await
    }

    pub(crate) async fn attachment(
        &self,
    ) -> eyre::Result<task_core::service::AttachmentServiceClient> {
        self.connect().await
    }

    pub(crate) async fn property(&self) -> eyre::Result<task_core::service::PropertyServiceClient> {
        self.connect().await
    }

    pub(crate) async fn audio(
        &self,
    ) -> eyre::Result<task_core::service::AudioProductionServiceClient> {
        self.connect().await
    }

    pub(crate) async fn cooking(&self) -> eyre::Result<task_core::service::CookingServiceClient> {
        self.connect().await
    }

    pub(crate) async fn food(&self) -> eyre::Result<task_core::service::FoodServiceClient> {
        self.connect().await
    }

    pub(crate) async fn connect<C>(&self) -> eyre::Result<C>
    where
        C: vox::FromVoxSession,
    {
        vox::connect(&self.vox_url)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("Remote Vox connection failed: {e}"))
    }
}

// ── JSON adapters ───────────────────────────────────────────────────────────

/// Re-encode a facet/serde model through `serde_json::Value` to land it in the
/// matching API DTO type. Used to bridge `task_core` domain models and the
/// serde-only API request/response structs generated by crudcrate.
pub(crate) fn model_to_api<T, U>(value: &T) -> eyre::Result<U>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(serde_json::to_value(value)?).map_err(Into::into)
}

pub(crate) fn api_to_model<T, U>(value: T) -> eyre::Result<U>
where
    T: Serialize,
    U: DeserializeOwned,
{
    model_to_api(&value)
}

// ── Task lookup ─────────────────────────────────────────────────────────────

pub(crate) fn find_task_in(tasks: Vec<Task>, reference: &str) -> eyre::Result<Task> {
    tasks
        .into_iter()
        .find(|t| t.matches_reference(reference))
        .ok_or_else(|| eyre::eyre!("Task not found: {reference}"))
}

// ── Repo client wrappers ────────────────────────────────────────────────────

pub(crate) async fn remote_find_task(
    remote: &RemoteVoxConfig,
    reference: &str,
) -> eyre::Result<Task> {
    let client = remote.task_repo().await?;
    remote_find_task_with_client(&client, reference).await
}

pub(crate) async fn remote_find_task_with_client(
    client: &task_core::task::TaskRepoClient,
    reference: &str,
) -> eyre::Result<Task> {
    let tasks = remote_list_tasks_with_client(client).await?;
    find_task_in(tasks, reference)
}

pub(crate) async fn remote_list_tasks_with_client(
    client: &task_core::task::TaskRepoClient,
) -> eyre::Result<Vec<Task>> {
    client
        .list_tasks(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_update_task_with_client(
    client: &task_core::task::TaskRepoClient,
    task: &Task,
) -> eyre::Result<Task> {
    let update: task_core::task::TaskApiUpdate = model_to_api(task)?;
    let updated = client.update_task(task.id.to_string(), update).await?;
    api_to_model(updated)
}

pub(crate) async fn remote_list_projects_with_client(
    client: &task_core::project::ProjectRepoClient,
) -> eyre::Result<Vec<Project>> {
    client
        .list_projects(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_find_project_with_client(
    client: &task_core::project::ProjectRepoClient,
    reference: &str,
) -> eyre::Result<Project> {
    remote_list_projects_with_client(client)
        .await?
        .into_iter()
        .find(|project| project.title.eq_ignore_ascii_case(reference))
        .ok_or_else(|| eyre::eyre!("Project not found: {reference}"))
}

pub(crate) async fn remote_update_project_with_client(
    client: &task_core::project::ProjectRepoClient,
    project: &Project,
) -> eyre::Result<Project> {
    let update: task_core::project::ProjectApiUpdate = model_to_api(project)?;
    let updated = client
        .update_project(project.id.to_string(), update)
        .await?;
    api_to_model(updated)
}

pub(crate) async fn remote_list_clients_with_client(
    client: &task_core::client::ClientRepoClient,
) -> eyre::Result<Vec<Client>> {
    client
        .list_clients(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_find_client_with_client(
    client: &task_core::client::ClientRepoClient,
    name: &str,
) -> eyre::Result<Option<Client>> {
    Ok(remote_list_clients_with_client(client)
        .await?
        .into_iter()
        .find(|client| client.name.eq_ignore_ascii_case(name)))
}

pub(crate) async fn remote_save_client_with_client(
    client: &task_core::client::ClientRepoClient,
    item: &Client,
) -> eyre::Result<Client> {
    if item.id == Uuid::nil() {
        let create: task_core::client::ClientApiCreate = model_to_api(item)?;
        api_to_model(client.create_client(create).await?)
    } else {
        let update: task_core::client::ClientApiUpdate = model_to_api(item)?;
        api_to_model(client.update_client(item.id.to_string(), update).await?)
    }
}

pub(crate) async fn remote_list_invoices_with_client(
    client: &task_core::invoice::InvoiceRepoClient,
) -> eyre::Result<Vec<Invoice>> {
    client
        .list_invoices(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_find_invoice_with_client(
    client: &task_core::invoice::InvoiceRepoClient,
    id: &str,
) -> eyre::Result<Option<Invoice>> {
    Ok(remote_list_invoices_with_client(client)
        .await?
        .into_iter()
        .find(|invoice| invoice.id.eq_ignore_ascii_case(id) || invoice.uuid.to_string() == id))
}

pub(crate) async fn remote_list_expenses_with_client(
    client: &task_core::expense::ExpenseRepoClient,
) -> eyre::Result<Vec<task_core::Expense>> {
    client
        .list_expenses(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_find_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    id: &str,
) -> eyre::Result<Option<task_core::Expense>> {
    Ok(remote_list_expenses_with_client(client)
        .await?
        .into_iter()
        .find(|expense| expense.id.eq_ignore_ascii_case(id) || expense.uuid.to_string() == id))
}

pub(crate) async fn remote_list_calendar_events_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
) -> eyre::Result<Vec<CalendarEvent>> {
    client
        .list_calendar_events(None, None, None, Some(10_000))
        .await?
        .into_iter()
        .map(api_to_model)
        .collect()
}

pub(crate) async fn remote_find_calendar_event_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
    reference: &str,
) -> eyre::Result<CalendarEvent> {
    remote_list_calendar_events_with_client(client)
        .await?
        .into_iter()
        .find(|event| {
            event.uuid.to_string() == reference
                || event.id.as_deref() == Some(reference)
                || event.title.eq_ignore_ascii_case(reference)
        })
        .ok_or_else(|| eyre::eyre!("Calendar event not found: {reference}"))
}

pub(crate) async fn remote_update_calendar_event_with_client(
    client: &task_core::calendar_event::CalendarEventRepoClient,
    event: &CalendarEvent,
) -> eyre::Result<CalendarEvent> {
    let update: task_core::calendar_event::CalendarEventApiUpdate = model_to_api(event)?;
    api_to_model(
        client
            .update_calendar_event(event.uuid.to_string(), update)
            .await?,
    )
}

pub(crate) async fn remote_create_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    request: ExpenseCreateRequest,
) -> eyre::Result<task_core::Expense> {
    let now = Utc::now();
    let date = request.date.unwrap_or_else(|| now.date_naive());
    let number = remote_list_expenses_with_client(client)
        .await?
        .into_iter()
        .filter(|expense| expense.date.year() == date.year())
        .map(|expense| expense.number)
        .max()
        .unwrap_or(0)
        + 1;
    let expense = task_core::Expense {
        uuid: Uuid::new_v4(),
        id: task_core::expense::format_expense_id(date.year(), number),
        number,
        status: request
            .status
            .as_deref()
            .and_then(task_core::expense::parse_expense_status)
            .unwrap_or(ExpenseStatus::Draft),
        date,
        amount_cents: request.amount_cents,
        currency_code: request.currency_code.unwrap_or_else(|| "USD".into()),
        project: request.project.map(WikiLink),
        client: request.client.map(WikiLink),
        deliverable: request.deliverable,
        category: request.category,
        vendor: request.vendor,
        description: request.description,
        receipt: request.receipt,
        reference: request.reference,
        reimbursable: request.reimbursable,
        notes: request.notes,
        created_by: request.actor,
        date_created: Some(now),
        date_modified: Some(now),
        body: String::new(),
    };
    let create: task_core::expense::ExpenseApiCreate = model_to_api(&expense)?;
    api_to_model(client.create_expense(create).await?)
}

pub(crate) async fn remote_update_expense_with_client(
    client: &task_core::expense::ExpenseRepoClient,
    id: &str,
    patch: ExpensePatch,
) -> eyre::Result<task_core::Expense> {
    let mut expense = remote_find_expense_with_client(client, id)
        .await?
        .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
    if let Some(status) = patch.status.as_deref() {
        expense.status = task_core::expense::parse_expense_status(status)
            .ok_or_else(|| eyre::eyre!("invalid expense status: {status}"))?;
    }
    if let Some(date) = patch.date.as_deref() {
        expense.date = date.parse::<NaiveDate>()?;
    }
    if let Some(amount) = patch.amount_cents {
        expense.amount_cents = amount;
    }
    if let Some(currency) = patch.currency_code {
        expense.currency_code = currency;
    }
    if let Some(project) = patch.project {
        expense.project = if project.trim().is_empty() {
            None
        } else {
            Some(WikiLink(project))
        };
    }
    if let Some(client_name) = patch.client {
        expense.client = if client_name.trim().is_empty() {
            None
        } else {
            Some(WikiLink(client_name))
        };
    }
    if let Some(value) = patch.deliverable {
        expense.deliverable = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.category {
        expense.category = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.vendor {
        expense.vendor = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.description {
        expense.description = value;
    }
    if let Some(value) = patch.receipt {
        expense.receipt = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.reference {
        expense.reference = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    if let Some(value) = patch.reimbursable {
        expense.reimbursable = value;
    }
    if let Some(value) = patch.notes {
        expense.notes = if value.trim().is_empty() {
            None
        } else {
            Some(value)
        };
    }
    expense.date_modified = Some(Utc::now());
    let update: task_core::expense::ExpenseApiUpdate = model_to_api(&expense)?;
    api_to_model(
        client
            .update_expense(expense.uuid.to_string(), update)
            .await?,
    )
}
