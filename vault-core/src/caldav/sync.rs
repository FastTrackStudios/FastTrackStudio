//! CalDAV sync client — fetch and push tasks to/from a CalDAV server.
//!
//! Uses `libdav` for the CalDAV protocol and `icalendar` for VTODO parsing.
//! Compatible with Nextcloud Tasks, Nextcloud Deck (via CalDAV bridge),
//! Thunderbird, Apple Reminders, and any CalDAV server.

use std::path::{Path, PathBuf};

use crate::service::VaultError;
use crate::task::Task;
use super::vtodo::{ics_to_task, task_to_ics};

/// Configuration for a CalDAV connection.
#[derive(Debug, Clone)]
pub struct CalDavConfig {
    /// CalDAV server URL (e.g. "https://cloud.example.com/remote.php/dav")
    pub server_url: String,
    /// Username for authentication
    pub username: String,
    /// Password or app token
    pub password: String,
    /// Calendar path (e.g. "calendars/user/tasks/")
    pub calendar_path: String,
    /// Local cache directory for .ics files
    pub cache_dir: PathBuf,
}

/// CalDAV client for syncing tasks with a remote server.
pub struct CalDavClient {
    config: CalDavConfig,
    http: reqwest::Client,
}

impl CalDavClient {
    /// Create a new CalDAV client.
    pub fn new(config: CalDavConfig) -> Self {
        let http = reqwest::Client::new();
        Self { config, http }
    }

    /// Full URL to the calendar collection.
    fn calendar_url(&self) -> String {
        format!(
            "{}/{}",
            self.config.server_url.trim_end_matches('/'),
            self.config.calendar_path.trim_start_matches('/')
        )
    }

    /// Fetch all VTODO items from the remote calendar.
    pub async fn fetch_tasks(&self) -> Result<Vec<Task>, VaultError> {
        let url = self.calendar_url();

        // REPORT request with calendar-query to get all VTODOs
        let body = r#"<?xml version="1.0" encoding="utf-8"?>
<c:calendar-query xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:prop>
    <d:getetag/>
    <c:calendar-data/>
  </d:prop>
  <c:filter>
    <c:comp-filter name="VCALENDAR">
      <c:comp-filter name="VTODO"/>
    </c:comp-filter>
  </c:filter>
</c:calendar-query>"#;

        let response = self
            .http
            .request(reqwest::Method::from_bytes(b"REPORT").unwrap(), &url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("Content-Type", "application/xml; charset=utf-8")
            .header("Depth", "1")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV request failed: {e}")))?;

        let status = response.status();
        if !status.is_success() && status.as_u16() != 207 {
            return Err(VaultError::IoError(format!(
                "CalDAV REPORT failed with status {status}"
            )));
        }

        let xml = response
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Parse the multistatus response — extract calendar-data elements
        let tasks = parse_multistatus_vtodos(&xml);
        Ok(tasks)
    }

    /// Push a task to the remote calendar as a VTODO.
    pub async fn push_task(&self, task: &Task) -> Result<(), VaultError> {
        let uid = task
            .id
            .as_deref()
            .ok_or_else(|| VaultError::ParseError("Task has no ID".into()))?;

        let url = format!("{}/{}.ics", self.calendar_url(), uid);
        let ics = task_to_ics(task);

        let response = self
            .http
            .put(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("Content-Type", "text/calendar; charset=utf-8")
            .body(ics)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV PUT failed: {e}")))?;

        let status = response.status();
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV PUT failed with status {status}"
            )));
        }

        Ok(())
    }

    /// Delete a task from the remote calendar.
    pub async fn delete_task(&self, uid: &str) -> Result<(), VaultError> {
        let url = format!("{}/{}.ics", self.calendar_url(), uid);

        let response = self
            .http
            .delete(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV DELETE failed: {e}")))?;

        let status = response.status();
        if !status.is_success() && status.as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "CalDAV DELETE failed with status {status}"
            )));
        }

        Ok(())
    }

    /// Sync local tasks with remote: fetch remote, merge, push changes.
    /// Returns the merged task list.
    pub async fn sync(&self, local_tasks: &[Task]) -> Result<Vec<Task>, VaultError> {
        let remote_tasks = self.fetch_tasks().await?;

        // Build a map of remote tasks by UID
        let mut remote_map: std::collections::HashMap<String, Task> = remote_tasks
            .into_iter()
            .filter_map(|t| t.id.clone().map(|id| (id, t)))
            .collect();

        let mut merged: Vec<Task> = Vec::new();

        // For each local task with an ID, check if remote has a newer version
        for local in local_tasks {
            if let Some(ref id) = local.id {
                if let Some(remote) = remote_map.remove(id) {
                    // Both exist — keep whichever was modified more recently
                    let merged_task = match (local.date_modified, remote.date_modified) {
                        (Some(lm), Some(rm)) if rm > lm => remote,
                        _ => local.clone(),
                    };
                    merged.push(merged_task);
                } else {
                    // Only local — push to remote
                    self.push_task(local).await?;
                    merged.push(local.clone());
                }
            } else {
                // No ID — local only, not synced
                merged.push(local.clone());
            }
        }

        // Remaining remote tasks (not in local) — pull them in
        for (_id, remote_task) in remote_map {
            merged.push(remote_task);
        }

        Ok(merged)
    }

    /// Save fetched .ics files to the local cache directory.
    pub async fn cache_locally(&self) -> Result<Vec<Task>, VaultError> {
        let tasks = self.fetch_tasks().await?;

        std::fs::create_dir_all(&self.config.cache_dir)
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        for task in &tasks {
            if let Some(ref id) = task.id {
                let path = self.config.cache_dir.join(format!("{}.ics", id));
                let ics = task_to_ics(task);
                std::fs::write(&path, ics)
                    .map_err(|e| VaultError::IoError(e.to_string()))?;
            }
        }

        Ok(tasks)
    }

    /// Load tasks from the local .ics cache.
    pub fn load_cached(cache_dir: &Path) -> Vec<Task> {
        let mut tasks = Vec::new();
        if let Ok(entries) = std::fs::read_dir(cache_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("ics") {
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        if let Some(task) = ics_to_task(&content) {
                            tasks.push(task);
                        }
                    }
                }
            }
        }
        tasks
    }
}

/// Parse a WebDAV multistatus XML response and extract VTODO tasks.
fn parse_multistatus_vtodos(xml: &str) -> Vec<Task> {
    let mut tasks = Vec::new();

    // Simple extraction: find all <cal:calendar-data> or <C:calendar-data> blocks
    // containing VTODO data. This is a pragmatic approach — a full XML parser
    // would be better for production but this handles the common case.
    for chunk in xml.split("BEGIN:VCALENDAR") {
        if chunk.contains("BEGIN:VTODO") {
            let ics = format!("BEGIN:VCALENDAR{}", chunk.split("END:VCALENDAR").next().unwrap_or(""));
            let ics = format!("{}END:VCALENDAR", ics);
            if let Some(task) = ics_to_task(&ics) {
                tasks.push(task);
            }
        }
    }

    tasks
}
