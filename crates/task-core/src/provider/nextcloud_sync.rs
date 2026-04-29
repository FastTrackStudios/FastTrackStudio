//! Nextcloud Tasks + Deck bidirectional sync.
//!
//! Syncs vault-core tasks with:
//! - **Nextcloud Tasks** via CalDAV/VTODO
//! - **Nextcloud Deck** via REST API (boards→projects, cards→tasks)

use chrono::Utc;

use crate::project::Project;
use crate::service::VaultError;
use crate::task::{Priority, Status, Task, WikiLink};
// Deck API response types use serde for JSON deserialization (complex nested structures).
// Domain types (Task, Project) use facet throughout.
use serde::{Deserialize, Serialize};

// ── Inline ICS generation (no icalendar crate dependency) ────────────────────

/// Generate a VTODO .ics string from a Task.
fn task_to_ics_inline(task: &Task) -> String {
    let uid = task.id.as_deref().unwrap_or(&task.title);
    let safe_uid = uid.replace(' ', "-").replace('/', "-");
    let now = Utc::now().format("%Y%m%dT%H%M%SZ");
    let status_str = match task.status {
        Status::None | Status::Open | Status::Planned => "NEEDS-ACTION",
        Status::InProgress | Status::OnHold => "IN-PROCESS",
        Status::Done | Status::Archived => "COMPLETED",
        Status::Cancelled => "CANCELLED",
    };
    let priority_num = match task.priority {
        Priority::Urgent => 1,
        Priority::High => 3,
        Priority::Normal => 5,
        Priority::Low => 9,
        Priority::None => 0,
    };

    let mut lines = vec![
        "BEGIN:VCALENDAR".to_string(),
        "VERSION:2.0".to_string(),
        "PRODID:-//Task//vault-core//EN".to_string(),
        "BEGIN:VTODO".to_string(),
        format!("UID:{safe_uid}"),
        format!("DTSTAMP:{now}"),
        format!("SUMMARY:{}", task.title),
        format!("STATUS:{status_str}"),
    ];

    if let Some(created) = task.date_created {
        lines.push(format!("CREATED:{}", created.format("%Y%m%dT%H%M%SZ")));
    }
    if let Some(modified) = task.date_modified {
        lines.push(format!(
            "LAST-MODIFIED:{}",
            modified.format("%Y%m%dT%H%M%SZ")
        ));
    }

    if priority_num > 0 {
        lines.push(format!("PRIORITY:{priority_num}"));
    }
    if let Some(due) = task.due {
        lines.push(format!("DUE;VALUE=DATE:{}", due.format("%Y%m%d")));
    }
    if let Some(scheduled) = task.scheduled {
        lines.push(format!("DTSTART;VALUE=DATE:{}", scheduled.format("%Y%m%d")));
    }
    if let Some(ref completed) = task.completed_date {
        lines.push(format!("COMPLETED:{}T000000Z", completed.format("%Y%m%d")));
    }
    if task.status == Status::Done {
        lines.push("PERCENT-COMPLETE:100".to_string());
    }
    if !task.tags.is_empty() {
        lines.push(format!("CATEGORIES:{}", task.tags.join(",")));
    }
    if let Some(ref assignee) = task.assignee {
        lines.push(format!("ATTENDEE;CN={assignee}:mailto:{assignee}@local"));
    }
    if let Some(ref rrule) = task.recurrence {
        lines.push(format!("RRULE:{rrule}"));
    }
    // Description: encode projects + contexts
    let mut desc_parts = Vec::new();
    for p in &task.projects {
        desc_parts.push(format!("Project: {}", p.0));
    }
    for c in &task.contexts {
        desc_parts.push(format!("Context: @{c}"));
    }
    if let Some(est) = task.time_estimate {
        desc_parts.push(format!("Estimate: {est} min"));
    }
    if !desc_parts.is_empty() {
        lines.push(format!("DESCRIPTION:{}", desc_parts.join("\\n")));
    }

    lines.push("END:VTODO".to_string());
    lines.push("END:VCALENDAR".to_string());
    lines.join("\r\n")
}

/// Parse a VTODO .ics string into a Task.
fn ics_to_task_inline(ics: &str) -> Option<Task> {
    if !ics.contains("VTODO") {
        return None;
    }

    let mut task = Task::default();

    for line in ics.lines() {
        let line = line.trim_end_matches('\r');
        if let Some(val) = line.strip_prefix("UID:") {
            task.id = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("SUMMARY:") {
            task.title = val.to_string();
        } else if let Some(val) = line.strip_prefix("STATUS:") {
            task.status = match val {
                "NEEDS-ACTION" => Status::Open,
                "IN-PROCESS" => Status::InProgress,
                "COMPLETED" => Status::Done,
                "CANCELLED" => Status::Cancelled,
                _ => Status::Open,
            };
        } else if let Some(val) = line.strip_prefix("PRIORITY:") {
            task.priority = match val.parse::<u8>().unwrap_or(0) {
                1..=2 => Priority::Urgent,
                3..=4 => Priority::High,
                5..=6 => Priority::Normal,
                7..=9 => Priority::Low,
                _ => Priority::None,
            };
        } else if line.starts_with("DUE") {
            // Handle DUE;VALUE=DATE:20260415 or DUE:20260415T000000Z
            if let Some(date_str) = line.rsplit(':').next() {
                let date_str = &date_str[..8.min(date_str.len())];
                task.due = chrono::NaiveDate::parse_from_str(date_str, "%Y%m%d").ok();
            }
        } else if line.starts_with("DTSTART") {
            if let Some(date_str) = line.rsplit(':').next() {
                let date_str = &date_str[..8.min(date_str.len())];
                task.scheduled = chrono::NaiveDate::parse_from_str(date_str, "%Y%m%d").ok();
            }
        } else if let Some(val) = line.strip_prefix("CATEGORIES:") {
            task.tags = val
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        } else if line.starts_with("ATTENDEE") {
            // Extract CN= value as assignee
            if let Some(cn_start) = line.find("CN=") {
                let rest = &line[cn_start + 3..];
                let cn = rest
                    .split(|c: char| c == ':' || c == ';')
                    .next()
                    .unwrap_or("");
                if !cn.is_empty() {
                    task.assignee = Some(cn.to_string());
                }
            }
        } else if let Some(val) = line.strip_prefix("RRULE:") {
            task.recurrence = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("COMPLETED:") {
            let date_str = &val[..8.min(val.len())];
            task.completed_date = chrono::NaiveDate::parse_from_str(date_str, "%Y%m%d").ok();
        } else if let Some(val) = line.strip_prefix("CREATED:") {
            task.date_created = chrono::NaiveDateTime::parse_from_str(val, "%Y%m%dT%H%M%SZ")
                .ok()
                .map(|dt| dt.and_utc());
        } else if let Some(val) = line.strip_prefix("LAST-MODIFIED:") {
            task.date_modified = chrono::NaiveDateTime::parse_from_str(val, "%Y%m%dT%H%M%SZ")
                .ok()
                .map(|dt| dt.and_utc());
        } else if let Some(val) = line.strip_prefix("DESCRIPTION:") {
            for part in val.split("\\n") {
                if let Some(proj) = part.strip_prefix("Project: ") {
                    task.projects.push(WikiLink(proj.trim().to_string()));
                } else if let Some(ctx) = part.strip_prefix("Context: @") {
                    task.contexts.push(ctx.trim().to_string());
                } else if let Some(est) = part.strip_prefix("Estimate: ") {
                    task.time_estimate = est.strip_suffix(" min").and_then(|s| s.parse().ok());
                }
            }
        }
    }

    if task.title.is_empty() {
        return None;
    }

    Some(task)
}

/// Nextcloud sync client — handles CalDAV + Deck API interactions.
pub struct NextcloudSync {
    base_url: String,
    username: String,
    password: String,
    http: reqwest::Client,
}

impl NextcloudSync {
    pub fn new(base_url: &str, username: &str, password: &str) -> Self {
        Self {
            base_url: base_url.trim_end_matches('/').to_string(),
            username: username.to_string(),
            password: password.to_string(),
            http: reqwest::Client::new(),
        }
    }

    fn auth(&self, req: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        req.basic_auth(&self.username, Some(&self.password))
    }

    // ── CalDAV / Nextcloud Tasks ─────────────────────────────────────

    /// Push a task to the Nextcloud Tasks app as a VTODO on the given calendar.
    pub async fn push_task_to_calendar(
        &self,
        calendar: &str,
        task: &Task,
    ) -> Result<(), VaultError> {
        let uid = task.id.as_deref().unwrap_or(&task.title);
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        let ics = task_to_ics_inline(task);

        let resp = self
            .auth(self.http.put(&url))
            .header("Content-Type", "text/calendar; charset=utf-8")
            .body(ics)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV PUT: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV PUT {}: {}",
                safe_uid,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Pull all tasks from a Nextcloud Tasks calendar.
    pub async fn pull_tasks_from_calendar(&self, calendar: &str) -> Result<Vec<Task>, VaultError> {
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/",
            self.base_url, self.username, calendar
        );

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

        let resp = self
            .auth(
                self.http
                    .request(reqwest::Method::from_bytes(b"REPORT").unwrap(), &url)
                    .header("Content-Type", "application/xml; charset=utf-8")
                    .header("Depth", "1"),
            )
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV REPORT: {e}")))?;

        let xml = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Extract VCALENDAR blocks and parse VTODOs
        let mut tasks = Vec::new();
        for chunk in xml.split("BEGIN:VCALENDAR") {
            if chunk.contains("BEGIN:VTODO") {
                let ics = format!(
                    "BEGIN:VCALENDAR{}END:VCALENDAR",
                    chunk.split("END:VCALENDAR").next().unwrap_or("")
                );
                if let Some(task) = ics_to_task_inline(&ics) {
                    tasks.push(task);
                }
            }
        }

        Ok(tasks)
    }

    /// Delete a task from the calendar.
    pub async fn delete_task_from_calendar(
        &self,
        calendar: &str,
        uid: &str,
    ) -> Result<(), VaultError> {
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        self.auth(self.http.delete(&url))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV DELETE: {e}")))?;

        Ok(())
    }

    /// Sync local tasks with Nextcloud Tasks calendar.
    /// Pushes all local tasks, pulls remote-only tasks.
    pub async fn sync_calendar(
        &self,
        calendar: &str,
        local_tasks: &[Task],
    ) -> Result<Vec<Task>, VaultError> {
        // Push all local tasks to calendar
        for task in local_tasks {
            self.push_task_to_calendar(calendar, task).await?;
        }

        // Pull all remote tasks
        let remote_tasks = self.pull_tasks_from_calendar(calendar).await?;

        // Merge: start with local, add remote-only tasks
        let mut merged = local_tasks.to_vec();
        for remote in remote_tasks {
            let exists = merged
                .iter()
                .any(|t| t.id == remote.id || t.title == remote.title);
            if !exists {
                merged.push(remote);
            }
        }

        Ok(merged)
    }

    // ── Deck API ─────────────────────────────────────────────────────

    /// List all Deck boards.
    pub async fn list_boards(&self) -> Result<Vec<DeckBoard>, VaultError> {
        let url = format!("{}/index.php/apps/deck/api/v1.0/boards", self.base_url);

        let resp = self
            .auth(self.http.get(&url))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        Ok(parse_deck_boards_json(&text))
    }

    /// List stacks (columns) in a board.
    pub async fn list_stacks(&self, board_id: u64) -> Result<Vec<DeckStack>, VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks",
            self.base_url, board_id
        );

        let resp = self
            .auth(self.http.get(&url))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        Ok(parse_deck_stacks_json(&text))
    }

    /// Create a card in a Deck board stack.
    pub async fn create_card(
        &self,
        board_id: u64,
        stack_id: u64,
        title: &str,
        description: &str,
        due_date: Option<&str>,
    ) -> Result<u64, VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards",
            self.base_url, board_id, stack_id
        );

        let mut body = format!(
            r#"{{"title":"{}","type":"plain","order":999"#,
            escape_json(title)
        );
        if !description.is_empty() {
            body.push_str(&format!(r#","description":"{}""#, escape_json(description)));
        }
        if let Some(due) = due_date {
            body.push_str(&format!(r#","duedate":"{}T00:00:00+00:00""#, due));
        }
        body.push('}');

        let resp = self
            .auth(self.http.post(&url))
            .header("OCS-APIRequest", "true")
            .header("Content-Type", "application/json")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API create card: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        // Extract card ID from response
        extract_json_id(&text)
            .ok_or_else(|| VaultError::ParseError("No card ID in response".into()))
    }

    /// Update a Deck card in place without changing its identity.
    pub async fn update_card(
        &self,
        board_id: u64,
        stack_id: u64,
        card: &DeckCard,
        title: &str,
        description: &str,
        due_date: Option<&str>,
    ) -> Result<(), VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}",
            self.base_url, board_id, stack_id, card.id
        );

        let body = DeckCardUpdate {
            title,
            description,
            card_type: "plain",
            order: card.order,
            duedate: due_date.map(|due| format!("{due}T00:00:00+00:00")),
            owner: card.owner.as_deref().unwrap_or(&self.username),
        };

        let resp = self
            .auth(self.http.put(&url))
            .header("OCS-APIRequest", "true")
            .json(&body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API update card: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Deck API update card {} failed: {}",
                card.id,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Move an existing Deck card to another stack when its task status changes.
    pub async fn move_card(
        &self,
        board_id: u64,
        from_stack_id: u64,
        to_stack_id: u64,
        card_id: u64,
    ) -> Result<(), VaultError> {
        if from_stack_id == to_stack_id {
            return Ok(());
        }

        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}/reorder",
            self.base_url, board_id, from_stack_id, card_id
        );

        let resp = self
            .auth(self.http.put(&url))
            .header("OCS-APIRequest", "true")
            .json(&serde_json::json!({
                "order": 999,
                "stackId": to_stack_id,
            }))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API move card: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Deck API move card {} failed: {}",
                card_id,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Assign a user to a Deck card.
    pub async fn assign_card(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
        user_id: &str,
    ) -> Result<(), VaultError> {
        let assign_url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}/assignUser",
            self.base_url, board_id, stack_id, card_id
        );

        let body = format!(r#"{{"userId":"{}"}}"#, user_id);

        self.auth(self.http.put(&assign_url))
            .header("OCS-APIRequest", "true")
            .header("Content-Type", "application/json")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck assign: {e}")))?;

        Ok(())
    }

    /// Remove a user assignment from a Deck card.
    pub async fn unassign_card(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
        user_id: &str,
    ) -> Result<(), VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}/unassignUser",
            self.base_url, board_id, stack_id, card_id
        );

        let resp = self
            .auth(self.http.put(&url))
            .header("OCS-APIRequest", "true")
            .json(&serde_json::json!({ "userId": user_id }))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck unassign: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Deck unassign card {} failed: {}",
                card_id,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Convert Deck board stacks + cards to vault-core tasks.
    /// Card descriptions are stored in `task.body`.
    pub async fn deck_board_to_tasks(
        &self,
        board_id: u64,
    ) -> Result<(Project, Vec<Task>), VaultError> {
        let boards = self.list_boards().await?;
        let board = boards
            .iter()
            .find(|b| b.id == board_id)
            .ok_or_else(|| VaultError::NotFound(format!("Board {board_id}")))?;

        let project = Project {
            title: board.title.clone(),
            ..Default::default()
        };

        let stacks = self.list_stacks(board_id).await?;
        let mut tasks = Vec::new();

        for stack in &stacks {
            let status = match stack.title.to_lowercase().as_str() {
                "to do" | "todo" | "backlog" => Status::Open,
                "doing" | "in progress" | "active" => Status::InProgress,
                "done" | "completed" | "finished" => Status::Done,
                "on hold" | "waiting" | "blocked" => Status::OnHold,
                _ => Status::Open,
            };

            for card in &stack.cards {
                let mut task = Task {
                    title: card.title.clone(),
                    status: status.clone(),
                    ..Default::default()
                };
                task.projects.push(WikiLink(board.title.clone()));

                if let Some(ref assignee) = card.assigned_user {
                    task.assignee = Some(assignee.clone());
                }

                if let Some(ref due) = card.due_date {
                    if let Ok(d) = chrono::NaiveDate::parse_from_str(due, "%Y-%m-%dT%H:%M:%S%z") {
                        task.due = Some(d);
                    } else if let Ok(d) = chrono::NaiveDate::parse_from_str(&due[..10], "%Y-%m-%d")
                    {
                        task.due = Some(d);
                    }
                }

                // Parse tags from description (lines starting with #)
                if !card.description.is_empty() {
                    for word in card.description.split_whitespace() {
                        if word.starts_with('#') && word.len() > 1 {
                            let tag = word.trim_start_matches('#').to_string();
                            if !tag.is_empty() && !task.tags.contains(&tag) {
                                task.tags.push(tag);
                            }
                        }
                    }
                }

                task.external_source = Some("deck".to_string());
                task.external_id = Some(card.id.to_string());

                // Store card description as task body (strip tag-only lines)
                task.body = card
                    .description
                    .lines()
                    .filter(|line| {
                        let trimmed = line.trim();
                        !trimmed
                            .split_whitespace()
                            .all(|w| w.starts_with('#') && w.len() > 1)
                            || trimmed.is_empty()
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .trim()
                    .to_string();

                tasks.push(task);
            }
        }

        Ok((project, tasks))
    }

    /// Push a task to Deck as a card. Existing cards are updated/moved in
    /// place using Deck ids first, then a title match fallback.
    /// The card description includes the full markdown body (with subtask checkboxes).
    /// Pass an empty string for `body` if there is no body content.
    pub async fn push_task_to_deck(
        &self,
        board_id: u64,
        task: &Task,
        body: &str,
    ) -> Result<(), VaultError> {
        let stacks = self.list_stacks(board_id).await?;

        let stack = stacks
            .iter()
            .find(|s| stack_matches_status(&s.title, &task.status))
            .or_else(|| stacks.first())
            .ok_or_else(|| VaultError::NotFound("No stacks in board".into()))?;

        let due_str = task.due.map(|d| d.to_string());

        // Build description: markdown body first, then tags
        let mut desc_parts = Vec::new();
        if !body.trim().is_empty() {
            desc_parts.push(body.trim().to_string());
        }
        if !task.tags.is_empty() {
            let tags_line = task
                .tags
                .iter()
                .map(|t| format!("#{t}"))
                .collect::<Vec<_>>()
                .join(" ");
            desc_parts.push(tags_line);
        }
        let description = desc_parts.join("\n\n");

        if let Some(existing) = find_matching_deck_card(&stacks, task) {
            if existing.stack_id != stack.id {
                self.move_card(board_id, existing.stack_id, stack.id, existing.card.id)
                    .await?;
            }
            self.update_card(
                board_id,
                stack.id,
                existing.card,
                &task.title,
                &description,
                due_str.as_deref(),
            )
            .await?;
            self.sync_card_assignment(
                board_id,
                stack.id,
                existing.card.id,
                existing.card.assigned_user.as_deref(),
                task.assignee.as_deref(),
            )
            .await;
            return Ok(());
        }

        let card_id = self
            .create_card(
                board_id,
                stack.id,
                &task.title,
                &description,
                due_str.as_deref(),
            )
            .await?;

        // Assign user if set
        if let Some(ref assignee) = task.assignee {
            let _ = self
                .assign_card(board_id, stack.id, card_id, assignee)
                .await;
        }

        Ok(())
    }

    async fn sync_card_assignment(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
        current: Option<&str>,
        desired: Option<&str>,
    ) {
        if current == desired {
            return;
        }
        if let Some(user) = current {
            let _ = self.unassign_card(board_id, stack_id, card_id, user).await;
        }
        if let Some(user) = desired {
            let _ = self.assign_card(board_id, stack_id, card_id, user).await;
        }
    }

    /// Bidirectional sync between local `.md` files and Nextcloud (CalDAV + Deck).
    ///
    /// 1. Push local tasks → CalDAV + Deck
    /// 2. Pull Deck cards → create/update `.md` files for new/changed tasks
    /// 3. Pull CalDAV VTODOs → create `.md` files for tasks created in Nextcloud Tasks
    ///
    /// `webdav_base` is the WebDAV URL for the project's tasks/ directory
    /// (e.g. "Projects/Montreal Album/tasks/").
    pub async fn full_sync(
        &self,
        calendar: &str,
        deck_board_id: Option<u64>,
        local_tasks: &[Task],
        webdav_tasks_path: Option<&str>,
    ) -> Result<SyncResult, VaultError> {
        let mut result = SyncResult::default();

        // ── Phase 1: Push local → remote ─────────────────────────────

        for task in local_tasks {
            match self.push_task_to_calendar(calendar, task).await {
                Ok(_) => result.calendar_pushed += 1,
                Err(e) => result
                    .errors
                    .push(format!("CalDAV push '{}': {}", task.title, e)),
            }
        }

        if let Some(board_id) = deck_board_id {
            for task in local_tasks {
                match self.push_task_to_deck(board_id, task, "").await {
                    Ok(_) => result.deck_pushed += 1,
                    Err(e) => result
                        .errors
                        .push(format!("Deck push '{}': {}", task.title, e)),
                }
            }
        }

        // ── Phase 2: Pull remote → local (reverse sync) ─────────────

        let local_titles: std::collections::HashSet<String> =
            local_tasks.iter().map(|t| t.title.clone()).collect();

        // Pull from Deck (richer data: assignees, stack=status, description=body)
        if let Some(board_id) = deck_board_id {
            match self.deck_board_to_tasks(board_id).await {
                Ok((project, remote_tasks)) => {
                    result.deck_pulled = remote_tasks.len();

                    for remote_task in &remote_tasks {
                        if !local_titles.contains(&remote_task.title) {
                            // New task from Deck — write .md file (body is in task.body)
                            if let Some(tasks_path) = webdav_tasks_path {
                                match self
                                    .write_task_to_webdav(
                                        tasks_path,
                                        remote_task,
                                        &project.title,
                                        &remote_task.body,
                                    )
                                    .await
                                {
                                    Ok(_) => result.files_created += 1,
                                    Err(e) => result
                                        .errors
                                        .push(format!("Write '{}': {}", remote_task.title, e)),
                                }
                            }
                        } else {
                            // Exists locally — check if Deck version has updates
                            if let Some(local) =
                                local_tasks.iter().find(|t| t.title == remote_task.title)
                            {
                                if local.status != remote_task.status
                                    || local.assignee != remote_task.assignee
                                {
                                    let mut updated = local.clone();
                                    updated.status = remote_task.status.clone();
                                    if remote_task.assignee.is_some() {
                                        updated.assignee = remote_task.assignee.clone();
                                    }
                                    if let Some(tasks_path) = webdav_tasks_path {
                                        // Preserve existing body when updating status/assignee
                                        match self
                                            .write_task_to_webdav(
                                                tasks_path,
                                                &updated,
                                                &project.title,
                                                "",
                                            )
                                            .await
                                        {
                                            Ok(_) => result.files_updated += 1,
                                            Err(e) => result
                                                .errors
                                                .push(format!("Update '{}': {}", updated.title, e)),
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Err(e) => result.errors.push(format!("Deck pull: {e}")),
            }
        }

        // Pull from CalDAV (catches tasks created in Nextcloud Tasks app)
        match self.pull_tasks_from_calendar(calendar).await {
            Ok(remote_tasks) => {
                result.calendar_pulled = remote_tasks.len();

                for remote_task in &remote_tasks {
                    if !local_titles.contains(&remote_task.title) {
                        // Check if it was already created from Deck above
                        if result.files_created > 0 {
                            // Skip if we might have already written it
                            continue;
                        }
                        if let Some(tasks_path) = webdav_tasks_path {
                            match self
                                .write_task_to_webdav(tasks_path, remote_task, "", "")
                                .await
                            {
                                Ok(_) => result.files_created += 1,
                                Err(e) => result
                                    .errors
                                    .push(format!("Write CalDAV '{}': {}", remote_task.title, e)),
                            }
                        }
                    }
                }
            }
            Err(e) => result.errors.push(format!("CalDAV pull: {e}")),
        }

        Ok(result)
    }

    /// Write a task as a `.md` file to WebDAV with markdown body content.
    /// Pass an empty string for `body` if there is no body content.
    async fn write_task_to_webdav(
        &self,
        tasks_path: &str,
        task: &Task,
        project_title: &str,
        body: &str,
    ) -> Result<(), VaultError> {
        let url = format!(
            "{}/remote.php/dav/files/{}/{}",
            self.base_url,
            self.username,
            tasks_path.trim_start_matches('/')
        );

        // Ensure directory exists
        let _ = self
            .auth(
                self.http
                    .request(reqwest::Method::from_bytes(b"MKCOL").unwrap(), &url),
            )
            .send()
            .await;

        // Build the task — ensure it has the project link
        let mut task = task.clone();
        if !project_title.is_empty() {
            let link = WikiLink(project_title.to_string());
            if !task.projects.contains(&link) {
                task.projects.push(link);
            }
        }

        // Render to markdown with body (subtask checkboxes, notes, etc.)
        let content = crate::vault::Vault::render_task_file(&task, body)?;

        // Write via WebDAV PUT
        let encoded_title = task.title.replace(' ', "%20").replace('/', "-");
        let file_url = format!(
            "{}/remote.php/dav/files/{}/{}/{}.md",
            self.base_url,
            self.username,
            tasks_path.trim_start_matches('/').trim_end_matches('/'),
            encoded_title,
        );

        self.auth(
            self.http
                .put(&file_url)
                .header("Content-Type", "text/markdown; charset=utf-8")
                .body(content),
        )
        .send()
        .await
        .map_err(|e| VaultError::IoError(format!("WebDAV PUT: {e}")))?;

        Ok(())
    }

    // ── Deck Comments ───────────────────────────────────────────────

    /// List all comments on a Deck card.
    pub async fn list_card_comments(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
    ) -> Result<Vec<DeckComment>, VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}/comments",
            self.base_url, board_id, stack_id, card_id
        );

        let resp = self
            .auth(self.http.get(&url))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck comments GET: {e}")))?;

        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let comments = parse_deck_comments_json(&text);
        Ok(comments)
    }

    /// Add a comment to a Deck card.
    pub async fn add_card_comment(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
        message: &str,
    ) -> Result<(), VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}/comments",
            self.base_url, board_id, stack_id, card_id
        );

        let body = format!(r#"{{"message":"{}"}}"#, escape_json(message));

        let resp = self
            .auth(self.http.post(&url))
            .header("OCS-APIRequest", "true")
            .header("Content-Type", "application/json")
            .body(body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck comment POST: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Deck comment POST failed: {}",
                resp.status()
            )));
        }

        Ok(())
    }

    /// Pull comments from a Deck card and append them to the task body as a
    /// `## Comments` section. Existing comments in the body are replaced.
    pub async fn sync_comments_to_body(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
        task: &mut Task,
    ) -> Result<(), VaultError> {
        let comments = self.list_card_comments(board_id, stack_id, card_id).await?;

        if comments.is_empty() {
            return Ok(());
        }

        // Build the comments section
        let mut section = String::from("## Comments");
        for c in &comments {
            // Extract just the date portion (YYYY-MM-DD) from the datetime string
            let date = if c.created_at.len() >= 10 {
                &c.created_at[..10]
            } else {
                &c.created_at
            };
            section.push_str(&format!("\n> **{}** ({}): {}", c.author, date, c.message));
        }

        // Strip any existing ## Comments section from the body
        let body_without_comments = if let Some(idx) = task.body.find("## Comments") {
            task.body[..idx].trim_end().to_string()
        } else {
            task.body.trim_end().to_string()
        };

        // Append the new comments section
        if body_without_comments.is_empty() {
            task.body = section;
        } else {
            task.body = format!("{}\n\n{}", body_without_comments, section);
        }

        Ok(())
    }
}

// ── Data types ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct DeckComment {
    pub id: u64,
    pub author: String,
    pub message: String,
    pub created_at: String,
}

#[derive(Debug, Clone, Default)]
pub struct SyncResult {
    /// Tasks pushed to CalDAV.
    pub calendar_pushed: usize,
    /// Tasks found in CalDAV.
    pub calendar_pulled: usize,
    /// Tasks pushed to Deck.
    pub deck_pushed: usize,
    /// Cards found in Deck.
    pub deck_pulled: usize,
    /// New .md files created from remote tasks.
    pub files_created: usize,
    /// Existing .md files updated from remote changes.
    pub files_updated: usize,
    /// Errors encountered (non-fatal, sync continues).
    pub errors: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DeckBoard {
    pub id: u64,
    pub title: String,
    #[serde(default)]
    pub archived: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DeckStack {
    pub id: u64,
    pub title: String,
    #[serde(default)]
    pub cards: Vec<DeckCard>,
}

/// Helper to extract the first assigned user's uid from the `assignedUsers` array.
/// Deck returns `[{"participant": {"uid": "..."}, ...}]`.
#[derive(Debug, Clone, Deserialize)]
struct AssignedUser {
    participant: AssignedUserParticipant,
}

#[derive(Debug, Clone, Deserialize)]
struct AssignedUserParticipant {
    uid: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DeckCard {
    pub id: u64,
    pub title: String,
    #[serde(default)]
    pub description: String,
    #[serde(
        rename = "assignedUsers",
        default,
        deserialize_with = "deserialize_assigned_user"
    )]
    pub assigned_user: Option<String>,
    #[serde(rename = "duedate", default)]
    pub due_date: Option<String>,
    #[serde(default)]
    pub order: i64,
    #[serde(default, deserialize_with = "deserialize_card_owner")]
    pub owner: Option<String>,
}

#[derive(Debug, Serialize)]
struct DeckCardUpdate<'a> {
    title: &'a str,
    description: &'a str,
    #[serde(rename = "type")]
    card_type: &'a str,
    order: i64,
    duedate: Option<String>,
    owner: &'a str,
}

#[derive(Debug, Clone, Copy)]
struct DeckCardRef<'a> {
    stack_id: u64,
    card: &'a DeckCard,
}

fn find_matching_deck_card<'a>(stacks: &'a [DeckStack], task: &Task) -> Option<DeckCardRef<'a>> {
    if task.external_source.as_deref() == Some("deck") {
        if let Some(card_id) = task
            .external_id
            .as_deref()
            .and_then(|id| id.parse::<u64>().ok())
        {
            if let Some(found) = stacks.iter().find_map(|stack| {
                stack
                    .cards
                    .iter()
                    .find(|card| card.id == card_id)
                    .map(|card| DeckCardRef {
                        stack_id: stack.id,
                        card,
                    })
            }) {
                return Some(found);
            }
        }
    }

    stacks.iter().find_map(|stack| {
        stack
            .cards
            .iter()
            .find(|card| card.title == task.title)
            .map(|card| DeckCardRef {
                stack_id: stack.id,
                card,
            })
    })
}

fn stack_matches_status(stack_title: &str, status: &Status) -> bool {
    let normalized = stack_title.trim().to_ascii_lowercase();
    match status {
        Status::Done | Status::Archived => {
            matches!(normalized.as_str(), "done" | "completed" | "finished")
        }
        Status::InProgress => {
            matches!(normalized.as_str(), "doing" | "in progress" | "active")
        }
        Status::OnHold => {
            matches!(normalized.as_str(), "on hold" | "waiting" | "blocked")
        }
        Status::Planned => matches!(normalized.as_str(), "planned"),
        _ => matches!(normalized.as_str(), "to do" | "todo" | "backlog"),
    }
}

fn deserialize_assigned_user<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let users: Vec<AssignedUser> = Vec::deserialize(deserializer).unwrap_or_default();
    Ok(users.into_iter().next().map(|u| u.participant.uid))
}

fn deserialize_card_owner<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let value = serde_json::Value::deserialize(deserializer)?;
    Ok(match value {
        serde_json::Value::String(owner) => Some(owner),
        serde_json::Value::Object(map) => map
            .get("uid")
            .or_else(|| map.get("primaryKey"))
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()),
        _ => None,
    })
}

/// Response from creating a card -- we only need the id.
#[derive(Debug, Deserialize)]
struct CardCreateResponse {
    id: u64,
}

// ── JSON parsing via serde_json ─────────────────────────────────────────────

fn parse_deck_boards_json(json: &str) -> Vec<DeckBoard> {
    serde_json::from_str(json).unwrap_or_default()
}

fn parse_deck_stacks_json(json: &str) -> Vec<DeckStack> {
    serde_json::from_str(json).unwrap_or_default()
}

fn parse_deck_comments_json(json: &str) -> Vec<DeckComment> {
    #[derive(Deserialize)]
    struct RawComment {
        id: u64,
        #[serde(rename = "actorId")]
        actor_id: String,
        message: String,
        #[serde(rename = "creationDateTime")]
        creation_date_time: String,
    }

    let raw: Vec<RawComment> = serde_json::from_str(json).unwrap_or_default();
    raw.into_iter()
        .map(|r| DeckComment {
            id: r.id,
            author: r.actor_id,
            message: r.message,
            created_at: r.creation_date_time,
        })
        .collect()
}

fn extract_json_id(json: &str) -> Option<u64> {
    serde_json::from_str::<CardCreateResponse>(json)
        .ok()
        .map(|r| r.id)
}

fn escape_json(s: &str) -> String {
    // Use serde_json to properly escape, then strip the surrounding quotes
    let escaped = serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s));
    escaped[1..escaped.len() - 1].to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn card(id: u64, title: &str) -> DeckCard {
        DeckCard {
            id,
            title: title.to_string(),
            description: String::new(),
            assigned_user: None,
            due_date: None,
            order: 0,
            owner: None,
        }
    }

    #[test]
    fn deck_upsert_matching_prefers_external_id_over_title() {
        let stacks = vec![DeckStack {
            id: 10,
            title: "To Do".to_string(),
            cards: vec![card(7, "Old title"), card(8, "Same title")],
        }];
        let mut task = Task {
            title: "Same title".to_string(),
            external_source: Some("deck".to_string()),
            external_id: Some("7".to_string()),
            ..Default::default()
        };

        let found = find_matching_deck_card(&stacks, &task).expect("card by id");
        assert_eq!(found.card.id, 7);

        task.external_id = Some("404".to_string());
        let found = find_matching_deck_card(&stacks, &task).expect("card by title");
        assert_eq!(found.card.id, 8);
    }

    #[test]
    fn deck_stack_matching_maps_task_statuses() {
        assert!(stack_matches_status("In Progress", &Status::InProgress));
        assert!(stack_matches_status("Done", &Status::Done));
        assert!(stack_matches_status("On Hold", &Status::OnHold));
        assert!(stack_matches_status("To Do", &Status::Open));
    }

    #[test]
    fn calendar_task_roundtrip_preserves_tracking_fields() {
        let created = chrono::DateTime::parse_from_rfc3339("2026-04-28T10:00:00Z")
            .unwrap()
            .to_utc();
        let modified = chrono::DateTime::parse_from_rfc3339("2026-04-29T12:30:00Z")
            .unwrap()
            .to_utc();
        let task = Task {
            id: Some("calendar-tracking-1".to_string()),
            title: "Calendar tracked task".to_string(),
            status: Status::InProgress,
            priority: Priority::High,
            due: chrono::NaiveDate::from_ymd_opt(2026, 5, 1),
            scheduled: chrono::NaiveDate::from_ymd_opt(2026, 4, 30),
            date_created: Some(created),
            date_modified: Some(modified),
            tags: vec!["calendar".to_string(), "sync".to_string()],
            projects: vec![WikiLink("Personal".to_string())],
            contexts: vec!["office".to_string()],
            time_estimate: Some(45),
            assignee: Some("agent".to_string()),
            recurrence: Some("FREQ=WEEKLY;BYDAY=WE".to_string()),
            ..Default::default()
        };

        let ics = task_to_ics_inline(&task);
        assert!(ics.contains("CREATED:20260428T100000Z"));
        assert!(ics.contains("LAST-MODIFIED:20260429T123000Z"));
        assert!(ics.contains("DTSTART;VALUE=DATE:20260430"));
        assert!(ics.contains("DUE;VALUE=DATE:20260501"));

        let parsed = ics_to_task_inline(&ics).expect("calendar task should parse");
        assert_eq!(parsed.id, task.id);
        assert_eq!(parsed.title, task.title);
        assert_eq!(parsed.status, task.status);
        assert_eq!(parsed.priority, task.priority);
        assert_eq!(parsed.due, task.due);
        assert_eq!(parsed.scheduled, task.scheduled);
        assert_eq!(parsed.date_created, task.date_created);
        assert_eq!(parsed.date_modified, task.date_modified);
        assert_eq!(parsed.tags, task.tags);
        assert_eq!(parsed.projects, task.projects);
        assert_eq!(parsed.contexts, task.contexts);
        assert_eq!(parsed.assignee, task.assignee);
        assert_eq!(parsed.recurrence, task.recurrence);
    }
}
