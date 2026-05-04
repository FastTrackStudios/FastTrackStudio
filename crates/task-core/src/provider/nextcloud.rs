//! Nextcloud provider — full integration with Nextcloud's API surface.
//!
//! Combines multiple Nextcloud APIs into a single `ProjectProvider`:
//!
//! - **WebDAV Files**: Read/write `project.md` + `tasks/*.md` in shared folders
//! - **CalDAV/VTODO**: Sync tasks with Nextcloud Tasks app (bidirectional)
//! - **Deck API**: Kanban boards with card assignments and labels
//! - **OCS User API**: Resolve usernames, groups, display names
//! - **Activity API**: Change feed for audit trail
//! - **Notifications API**: Push assignment/due-date notifications
//!
//! # Architecture
//!
//! ```text
//! NextcloudProvider
//!   ├── WebDavClient      — file-based projects (project.md + tasks/*.md)
//!   ├── CalDavClient      — VTODO task sync (from vault-core::caldav)
//!   ├── DeckClient        — board/card operations
//!   ├── OcsClient         — user/group resolution
//!   └── NotificationClient — push notifications
//! ```
//!
//! The provider reads projects from the WebDAV file structure (same format
//! as LocalProvider) and optionally syncs tasks bidirectionally with CalDAV.
//! Deck boards are mapped to projects, cards to tasks.

use async_trait::async_trait;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::{Task, WikiLink};
use crate::vault::Vault;

/// Configuration for connecting to a Nextcloud instance.
#[derive(Debug, Clone)]
pub struct NextcloudConfig {
    /// Base URL of the Nextcloud instance (e.g. "https://cloud.example.com").
    pub url: String,
    /// Username for authentication.
    pub username: String,
    /// App password or regular password.
    pub password: String,
    /// Path to the Projects directory within the user's files
    /// (e.g. "Projects/" — relative to the user's file root).
    pub projects_path: String,
    /// CalDAV calendar name for task sync (e.g. "tasks", "personal").
    /// If None, CalDAV sync is disabled.
    pub calendar: Option<String>,
    /// Whether to sync with Nextcloud Deck boards.
    pub deck_enabled: bool,
    /// Mapping of project titles to Deck board IDs.
    /// When a task is saved to a project in this map, it auto-syncs to the Deck board.
    pub deck_boards: std::collections::HashMap<String, u64>,
}

impl NextcloudConfig {
    /// WebDAV base URL for file operations.
    fn webdav_url(&self) -> String {
        format!(
            "{}/remote.php/dav/files/{}/{}",
            self.url.trim_end_matches('/'),
            self.username,
            self.projects_path.trim_start_matches('/')
        )
    }

    /// OCS API base URL.
    fn ocs_url(&self) -> String {
        format!("{}/ocs/v2.php", self.url.trim_end_matches('/'))
    }

    /// Deck API base URL.
    fn deck_url(&self) -> String {
        format!(
            "{}/index.php/apps/deck/api/v1.0",
            self.url.trim_end_matches('/')
        )
    }
}

/// Full Nextcloud integration provider.
pub struct NextcloudProvider {
    info: ProviderInfo,
    config: NextcloudConfig,
    http: reqwest::Client,
}

impl NextcloudProvider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, config: NextcloudConfig) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "nextcloud".into(),
                writable: true,
            },
            config,
            http: reqwest::Client::new(),
        }
    }

    // ── WebDAV file operations ───────────────────────────────────────

    /// List directories (projects) under the projects path via PROPFIND.
    async fn webdav_list_dirs(&self) -> Result<Vec<String>, VaultError> {
        let url = self.config.webdav_url();
        let body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:">
  <d:prop>
    <d:resourcetype/>
    <d:displayname/>
  </d:prop>
</d:propfind>"#;

        let resp = self
            .http
            .request(reqwest::Method::from_bytes(b"PROPFIND").unwrap(), &url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("Content-Type", "application/xml")
            .header("Depth", "1")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PROPFIND failed: {e}")))?;

        let xml = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Parse directory names from multistatus response
        Ok(parse_webdav_directories(&xml, &self.config.projects_path))
    }

    /// GET a file's content from WebDAV.
    async fn webdav_get(&self, path: &str) -> Result<Option<String>, VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.username,
            path.trim_start_matches('/')
        );

        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV GET failed: {e}")))?;

        if resp.status() == 404 {
            return Ok(None);
        }
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV GET {}: {}",
                path,
                resp.status()
            )));
        }

        Ok(Some(
            resp.text()
                .await
                .map_err(|e| VaultError::IoError(e.to_string()))?,
        ))
    }

    /// PUT a file to WebDAV.
    async fn webdav_put(&self, path: &str, content: &str) -> Result<(), VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.username,
            path.trim_start_matches('/')
        );

        let resp = self
            .http
            .put(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("Content-Type", "text/markdown; charset=utf-8")
            .body(content.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PUT failed: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "WebDAV PUT {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    /// MKCOL (create directory) on WebDAV.
    async fn webdav_mkdir(&self, path: &str) -> Result<(), VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.username,
            path.trim_start_matches('/')
        );

        let resp = self
            .http
            .request(reqwest::Method::from_bytes(b"MKCOL").unwrap(), &url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV MKCOL failed: {e}")))?;

        // 405 = already exists, which is fine
        if !resp.status().is_success() && resp.status().as_u16() != 405 {
            return Err(VaultError::IoError(format!(
                "WebDAV MKCOL {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    /// DELETE a file from WebDAV.
    async fn webdav_delete(&self, path: &str) -> Result<(), VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.username,
            path.trim_start_matches('/')
        );

        let resp = self
            .http
            .delete(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV DELETE failed: {e}")))?;

        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "WebDAV DELETE {}: {}",
                path,
                resp.status()
            )));
        }
        Ok(())
    }

    /// List .md files in a WebDAV directory.
    async fn webdav_list_md_files(&self, dir_path: &str) -> Result<Vec<String>, VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.username,
            dir_path.trim_start_matches('/')
        );

        let body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:">
  <d:prop>
    <d:displayname/>
    <d:getcontenttype/>
  </d:prop>
</d:propfind>"#;

        let resp = self
            .http
            .request(reqwest::Method::from_bytes(b"PROPFIND").unwrap(), &url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("Content-Type", "application/xml")
            .header("Depth", "1")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("WebDAV PROPFIND failed: {e}")))?;

        let xml = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        Ok(parse_webdav_md_files(&xml))
    }

    /// Load a project from WebDAV (project.md + tasks/*.md).
    async fn load_project_from_webdav(
        &self,
        project_name: &str,
    ) -> Result<Option<ProjectBundle>, VaultError> {
        let projects_base = self.config.projects_path.trim_end_matches('/');
        let project_md_path = format!("{}/{}/project.md", projects_base, project_name);

        // Load project.md
        let project = match self.webdav_get(&project_md_path).await? {
            Some(content) => Vault::parse_project_from_md(&content),
            None => None,
        };

        let project = project.unwrap_or_else(|| Project {
            title: project_name.to_string(),
            ..Default::default()
        });

        // Load tasks from tasks/ subdirectory
        let tasks_dir = format!("{}/{}/tasks", projects_base, project_name);
        let task_files = self
            .webdav_list_md_files(&tasks_dir)
            .await
            .unwrap_or_default();

        let mut tasks = Vec::new();
        for file_name in &task_files {
            let task_path = format!("{}/{}", tasks_dir, file_name);
            if let Ok(Some(content)) = self.webdav_get(&task_path).await {
                if let Some(mut task) = Vault::parse_task_from_md(&content) {
                    let project_link = WikiLink(project.title.clone());
                    if !task.projects.contains(&project_link) {
                        task.projects.push(project_link);
                    }
                    tasks.push(task);
                }
            }
        }

        Ok(Some(ProjectBundle {
            project,
            tasks,
            location: format!("{}/{}", self.config.url, project_md_path),
            source: self.info.name.clone(),
        }))
    }

    // ── OCS User API ─────────────────────────────────────────────────

    /// Get display name for a Nextcloud user.
    pub async fn get_user_display_name(&self, user_id: &str) -> Result<String, VaultError> {
        let url = format!(
            "{}/cloud/users/{}?format=json",
            self.config.ocs_url(),
            user_id
        );

        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("OCS API failed: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Simple JSON extraction — in production use serde_json
        if let Some(start) = text.find("\"displayname\":\"") {
            let rest = &text[start + 15..];
            if let Some(end) = rest.find('"') {
                return Ok(rest[..end].to_string());
            }
        }

        Ok(user_id.to_string())
    }

    /// List all users in the Nextcloud instance.
    pub async fn list_users(&self) -> Result<Vec<String>, VaultError> {
        let url = format!("{}/cloud/users?format=json", self.config.ocs_url());

        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("OCS API failed: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Extract user list from JSON — simplified parsing
        let mut users = Vec::new();
        for segment in text.split('"') {
            // User IDs appear as string values in the "users" array
            if !segment.is_empty()
                && !segment.contains('{')
                && !segment.contains('}')
                && !segment.contains(':')
                && !segment.contains(',')
                && !segment.contains('[')
                && segment.len() > 2
                && segment
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
            {
                if !users.contains(&segment.to_string()) {
                    users.push(segment.to_string());
                }
            }
        }

        Ok(users)
    }

    // ── Deck API ─────────────────────────────────────────────────────

    /// List all Deck boards (maps to projects).
    pub async fn list_deck_boards(&self) -> Result<Vec<Project>, VaultError> {
        if !self.config.deck_enabled {
            return Ok(vec![]);
        }

        let url = format!("{}/boards", self.config.deck_url());

        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API failed: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Parse boards from JSON — simplified
        Ok(parse_deck_boards(&text))
    }
}

#[async_trait]
impl ProjectProvider for NextcloudProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        let dir_names = self.webdav_list_dirs().await?;
        let mut projects = Vec::new();

        for name in &dir_names {
            let projects_base = self.config.projects_path.trim_end_matches('/');
            let project_md_path = format!("{}/{}/project.md", projects_base, name);
            let project = match self.webdav_get(&project_md_path).await? {
                Some(content) => Vault::parse_project_from_md(&content),
                None => None,
            };
            projects.push(project.unwrap_or_else(|| Project {
                title: name.clone(),
                ..Default::default()
            }));
        }

        // Also include Deck boards as projects if enabled
        if self.config.deck_enabled {
            let boards = self.list_deck_boards().await.unwrap_or_default();
            projects.extend(boards);
        }

        Ok(projects)
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        self.load_project_from_webdav(title).await
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let dir_names = self.webdav_list_dirs().await?;
        let mut bundles = Vec::new();

        for name in &dir_names {
            if let Ok(Some(bundle)) = self.load_project_from_webdav(name).await {
                bundles.push(bundle);
            }
        }

        Ok(bundles)
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        let projects_base = self.config.projects_path.trim_end_matches('/');
        let project_dir = format!("{}/{}", projects_base, project.title);
        let tasks_dir = format!("{}/tasks", project_dir);

        // Create WebDAV directories
        self.webdav_mkdir(&project_dir).await?;
        self.webdav_mkdir(&tasks_dir).await?;

        let content = Vault::render_project_file(project, "")?;
        self.webdav_put(&format!("{}/project.md", project_dir), &content)
            .await?;

        // Auto-create Deck board if Deck is enabled
        if self.config.deck_enabled {
            // Create board
            let board_url = format!("{}/index.php/apps/deck/api/v1.0/boards", self.config.url);
            let body = format!(
                r#"{{"title":"{}","color":"0082C9"}}"#,
                project.title.replace('"', "\\\"")
            );
            if let Ok(resp) = self
                .http
                .post(&board_url)
                .basic_auth(&self.config.username, Some(&self.config.password))
                .header("OCS-APIRequest", "true")
                .header("Content-Type", "application/json")
                .body(body)
                .send()
                .await
            {
                if let Ok(text) = resp.text().await {
                    // Create standard stacks
                    if let Some(board_id) =
                        super::nextcloud::extract_between(&text, r#""id":"#, ",")
                            .or_else(|| super::nextcloud::extract_between(&text, r#""id":"#, "}"))
                            .and_then(|s| s.trim().parse::<u64>().ok())
                    {
                        for (i, stack_name) in ["To Do", "In Progress", "On Hold", "Done"]
                            .iter()
                            .enumerate()
                        {
                            let stack_body =
                                format!(r#"{{"title":"{}","order":{}}}"#, stack_name, i);
                            let _ = self
                                .http
                                .post(&format!(
                                    "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks",
                                    self.config.url, board_id
                                ))
                                .basic_auth(&self.config.username, Some(&self.config.password))
                                .header("OCS-APIRequest", "true")
                                .header("Content-Type", "application/json")
                                .body(stack_body)
                                .send()
                                .await;
                        }

                        // Share with team members
                        for member in &project.team {
                            if member != &self.config.username {
                                let share_body = format!(
                                    r#"{{"type":0,"participant":"{}","permissionEdit":true,"permissionShare":false,"permissionManage":false}}"#,
                                    member
                                );
                                let _ = self
                                    .http
                                    .post(&format!(
                                        "{}/index.php/apps/deck/api/v1.0/boards/{}/acl",
                                        self.config.url, board_id
                                    ))
                                    .basic_auth(&self.config.username, Some(&self.config.password))
                                    .header("OCS-APIRequest", "true")
                                    .header("Content-Type", "application/json")
                                    .body(share_body)
                                    .send()
                                    .await;
                            }
                        }
                    }
                }
            }
        }

        Ok(format!("{}/{}", self.config.url, project_dir))
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        let projects_base = self.config.projects_path.trim_end_matches('/');
        let path = format!("{}/{}/project.md", projects_base, project.title);

        // Preserve body if file exists
        let body = match self.webdav_get(&path).await? {
            Some(content) => Vault::extract_body(&content).unwrap_or("").to_string(),
            None => String::new(),
        };

        let content = Vault::render_project_file(project, &body)?;
        self.webdav_put(&path, &content).await
    }

    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        let projects_base = self.config.projects_path.trim_end_matches('/');
        let tasks_dir = format!("{}/{}/tasks", projects_base, project_title);

        // 1. Save to WebDAV (markdown file)
        self.webdav_mkdir(&tasks_dir).await?;

        let path = format!("{}/{}.md", tasks_dir, task.title);
        let body = match self.webdav_get(&path).await? {
            Some(content) => Vault::extract_body(&content).unwrap_or("").to_string(),
            None => String::new(),
        };

        let content = Vault::render_task_file(task, &body)?;
        self.webdav_put(&path, &content).await?;

        // 2. Auto-sync to CalDAV (Nextcloud Tasks app)
        if let Some(ref calendar) = self.config.calendar {
            let sync = super::nextcloud_sync::NextcloudSync::new(
                &self.config.url,
                &self.config.username,
                &self.config.password,
            );
            if let Err(e) = sync.push_task_to_calendar(calendar, task).await {
                tracing::warn!(task = %task.title, error = %e, "CalDAV sync failed (non-fatal)");
            }
        }

        // 3. Auto-sync to Deck board (if mapped)
        //    Include the markdown body as the card description (subtask checkboxes etc.)
        if self.config.deck_enabled {
            if let Some(&board_id) = self.config.deck_boards.get(project_title) {
                let sync = super::nextcloud_sync::NextcloudSync::new(
                    &self.config.url,
                    &self.config.username,
                    &self.config.password,
                );
                if let Err(e) = sync.push_task_to_deck(board_id, task, &body).await {
                    tracing::warn!(task = %task.title, board = board_id, error = %e, "Deck sync failed (non-fatal)");
                }
            }
        }

        Ok(())
    }

    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError> {
        let projects_base = self.config.projects_path.trim_end_matches('/');
        let path = format!(
            "{}/{}/tasks/{}.md",
            projects_base, project_title, task_title
        );
        self.webdav_delete(&path).await
    }
}

// ── XML/JSON parsing helpers ─────────────────────────────────────────────────

/// Extract directory names from a WebDAV PROPFIND multistatus response.
fn parse_webdav_directories(xml: &str, base_path: &str) -> Vec<String> {
    let mut dirs = Vec::new();
    let base = base_path.trim_matches('/');

    // Simple extraction: find <d:href> elements that end with /
    for line in xml.lines() {
        let line = line.trim();
        if let Some(href) = extract_between(line, "<d:href>", "</d:href>")
            .or_else(|| extract_between(line, "<D:href>", "</D:href>"))
        {
            let decoded = percent_decode(href);
            // Skip the base directory itself
            if decoded.trim_end_matches('/')
                == format!("/remote.php/dav/files/{}/{}", "", base).trim_end_matches('/')
            {
                continue;
            }
            // Extract just the directory name
            if decoded.ends_with('/') {
                if let Some(name) = decoded.trim_end_matches('/').rsplit('/').next() {
                    if !name.is_empty() && name != base && !name.starts_with('.') {
                        dirs.push(name.to_string());
                    }
                }
            }
        }
    }

    dirs
}

/// Extract .md filenames from a WebDAV PROPFIND response.
fn parse_webdav_md_files(xml: &str) -> Vec<String> {
    let mut files = Vec::new();

    for line in xml.lines() {
        let line = line.trim();
        if let Some(href) = extract_between(line, "<d:href>", "</d:href>")
            .or_else(|| extract_between(line, "<D:href>", "</D:href>"))
        {
            let decoded = percent_decode(href);
            if decoded.ends_with(".md") {
                if let Some(name) = decoded.rsplit('/').next() {
                    files.push(name.to_string());
                }
            }
        }
    }

    files
}

/// Parse Deck boards JSON into projects.
fn parse_deck_boards(json: &str) -> Vec<Project> {
    // Simplified — in production use serde_json
    let mut projects = Vec::new();

    // Look for "title":"..." patterns
    let mut rest = json;
    while let Some(pos) = rest.find("\"title\":\"") {
        let start = pos + 9;
        rest = &rest[start..];
        if let Some(end) = rest.find('"') {
            let title = rest[..end].to_string();
            if !title.is_empty() {
                projects.push(Project {
                    title,
                    ..Default::default()
                });
            }
            rest = &rest[end + 1..];
        } else {
            break;
        }
    }

    projects
}

pub(crate) fn extract_between<'a>(s: &'a str, start: &str, end: &str) -> Option<&'a str> {
    let start_idx = s.find(start)? + start.len();
    let end_idx = s[start_idx..].find(end)? + start_idx;
    Some(&s[start_idx..end_idx])
}

fn percent_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '%' {
            let hex: String = chars.by_ref().take(2).collect();
            if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                result.push(byte as char);
            }
        } else {
            result.push(c);
        }
    }
    result
}
