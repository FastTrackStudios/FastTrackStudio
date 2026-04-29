//! Nextcloud Tasks + Deck bidirectional sync.
//!
//! Syncs vault-core tasks with:
//! - **Nextcloud Tasks** via CalDAV/VTODO
//! - **Nextcloud Deck** via REST API (boards→projects, cards→tasks)

use chrono::{DateTime, NaiveDate, Utc};

use crate::calendar_event::{CalendarEvent, CalendarEventStatus};
use crate::project::Project;
use crate::service::{
    CalDavAlarm, CalDavCalendarInfo, CalDavDiscovery, CalDavEventInstance,
    CalDavFreeBusyInterval, CalDavObject, CalDavObjectDetails, CalDavParameter,
    CalDavParticipant, CalDavProperty, CalDavScheduleResponse, CalDavSyncCollectionResponse,
    CalDavTimezone, VaultError,
};
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

fn event_to_ics_inline(event: &CalendarEvent) -> String {
    let uid = event.id.as_deref().unwrap_or(&event.title);
    let safe_uid = uid.replace(' ', "-").replace('/', "-");
    let now = Utc::now().format("%Y%m%dT%H%M%SZ");
    let status = match event.status {
        CalendarEventStatus::Confirmed => "CONFIRMED",
        CalendarEventStatus::Tentative => "TENTATIVE",
        CalendarEventStatus::Cancelled => "CANCELLED",
    };
    let mut lines = vec![
        "BEGIN:VCALENDAR".to_string(),
        "VERSION:2.0".to_string(),
        "PRODID:-//Task//calendar//EN".to_string(),
        "BEGIN:VEVENT".to_string(),
        format!("UID:{safe_uid}"),
        format!("DTSTAMP:{now}"),
        format!("SUMMARY:{}", escape_ics_text(&event.title)),
        format!("STATUS:{status}"),
    ];
    if event.all_day {
        lines.push(format!(
            "DTSTART;VALUE=DATE:{}",
            event.start.date_naive().format("%Y%m%d")
        ));
        if let Some(end) = event.end {
            lines.push(format!(
                "DTEND;VALUE=DATE:{}",
                end.date_naive().format("%Y%m%d")
            ));
        }
    } else {
        lines.push(format!("DTSTART:{}", event.start.format("%Y%m%dT%H%M%SZ")));
        if let Some(end) = event.end {
            lines.push(format!("DTEND:{}", end.format("%Y%m%dT%H%M%SZ")));
        }
    }
    if let Some(created) = event.date_created {
        lines.push(format!("CREATED:{}", created.format("%Y%m%dT%H%M%SZ")));
    }
    if let Some(modified) = event.date_modified {
        lines.push(format!(
            "LAST-MODIFIED:{}",
            modified.format("%Y%m%dT%H%M%SZ")
        ));
    }
    if let Some(location) = &event.location {
        lines.push(format!("LOCATION:{}", escape_ics_text(location)));
    }
    if let Some(description) = event.description.as_ref().or_else(|| {
        if event.body.is_empty() {
            None
        } else {
            Some(&event.body)
        }
    }) {
        lines.push(format!("DESCRIPTION:{}", escape_ics_text(description)));
    }
    if let Some(rrule) = &event.recurrence {
        lines.push(format!("RRULE:{rrule}"));
    }
    for attendee in &event.attendees {
        lines.push(format!("ATTENDEE;CN={}:mailto:{}", attendee, attendee));
    }
    if let Some(source) = &event.external_source {
        lines.push(format!(
            "X-TASK-EXTERNAL-SOURCE:{}",
            escape_ics_text(source)
        ));
    }
    if let Some(id) = &event.external_id {
        lines.push(format!("X-TASK-EXTERNAL-ID:{}", escape_ics_text(id)));
    }
    lines.push("END:VEVENT".to_string());
    lines.push("END:VCALENDAR".to_string());
    lines.join("\r\n")
}

fn ics_to_event_inline(ics: &str) -> Option<CalendarEvent> {
    if !ics.contains("VEVENT") {
        return None;
    }
    let mut event = CalendarEvent::default();
    for raw in unfold_ics_lines(ics) {
        let line = raw.trim_end_matches('\r');
        if let Some(val) = line.strip_prefix("UID:") {
            event.id = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("SUMMARY:") {
            event.title = unescape_ics_text(val);
        } else if let Some(val) = line.strip_prefix("STATUS:") {
            event.status = match val {
                "TENTATIVE" => CalendarEventStatus::Tentative,
                "CANCELLED" => CalendarEventStatus::Cancelled,
                _ => CalendarEventStatus::Confirmed,
            };
        } else if line.starts_with("DTSTART") {
            event.all_day = line.contains("VALUE=DATE");
            if let Some(value) = line.rsplit(':').next() {
                event.start = parse_ics_datetime(value, event.all_day)?;
            }
        } else if line.starts_with("DTEND") {
            let all_day = line.contains("VALUE=DATE");
            if let Some(value) = line.rsplit(':').next() {
                event.end = parse_ics_datetime(value, all_day);
            }
        } else if let Some(val) = line.strip_prefix("DESCRIPTION:") {
            let value = unescape_ics_text(val);
            event.description = Some(value.clone());
            event.body = value;
        } else if let Some(val) = line.strip_prefix("LOCATION:") {
            event.location = Some(unescape_ics_text(val));
        } else if let Some(val) = line.strip_prefix("RRULE:") {
            event.recurrence = Some(val.to_string());
        } else if let Some(val) = line.strip_prefix("CREATED:") {
            event.date_created = parse_ics_datetime(val, false);
        } else if let Some(val) = line.strip_prefix("LAST-MODIFIED:") {
            event.date_modified = parse_ics_datetime(val, false);
        } else if line.starts_with("ATTENDEE") {
            if let Some(cn_start) = line.find("CN=") {
                let rest = &line[cn_start + 3..];
                let cn = rest
                    .split(|c: char| c == ':' || c == ';')
                    .next()
                    .unwrap_or("");
                if !cn.is_empty() {
                    event.attendees.push(cn.to_string());
                }
            }
        } else if let Some(val) = line.strip_prefix("X-TASK-EXTERNAL-SOURCE:") {
            event.external_source = Some(unescape_ics_text(val));
        } else if let Some(val) = line.strip_prefix("X-TASK-EXTERNAL-ID:") {
            event.external_id = Some(unescape_ics_text(val));
        }
    }
    if event.title.is_empty() {
        return None;
    }
    Some(event)
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

    fn calendar_collection_url(&self, calendar: &str) -> String {
        if calendar.starts_with("http://") || calendar.starts_with("https://") {
            return ensure_trailing_slash(calendar);
        }
        if calendar.starts_with('/') {
            return format!("{}{}", self.base_url, ensure_trailing_slash(calendar));
        }
        format!(
            "{}/remote.php/dav/calendars/{}/{}/",
            self.base_url, self.username, calendar
        )
    }

    fn absolute_dav_url(&self, href: &str) -> String {
        if href.starts_with("http://") || href.starts_with("https://") {
            href.to_string()
        } else if href.starts_with('/') {
            format!("{}{}", self.base_url, href)
        } else {
            format!("{}/{}", self.base_url, href)
        }
    }

    fn calendar_object_url(&self, calendar: &str, href: &str) -> String {
        if href.starts_with("http://") || href.starts_with("https://") || href.starts_with('/') {
            return self.absolute_dav_url(href);
        }
        format!("{}{}", self.calendar_collection_url(calendar), href)
    }

    pub async fn discover_calendars(&self) -> Result<CalDavDiscovery, VaultError> {
        let root = format!("{}/remote.php/dav/", self.base_url);
        let principal_body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:">
  <d:prop><d:current-user-principal/></d:prop>
</d:propfind>"#;
        let principal_xml = self.propfind(&root, "0", principal_body).await?;
        let principal_url = extract_href_from_prop(&principal_xml, "current-user-principal")
            .unwrap_or_else(|| format!("/remote.php/dav/principals/users/{}/", self.username));

        let home_body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:prop>
    <c:calendar-home-set/>
    <c:schedule-inbox-URL/>
    <c:schedule-outbox-URL/>
    <c:calendar-user-address-set/>
  </d:prop>
</d:propfind>"#;
        let principal_url_abs = self.absolute_dav_url(&principal_url);
        let home_xml = self.propfind(&principal_url_abs, "0", home_body).await?;
        let calendar_home_set = extract_href_from_prop(&home_xml, "calendar-home-set")
            .unwrap_or_else(|| format!("/remote.php/dav/calendars/{}/", self.username));
        let schedule_inbox_url = extract_href_from_prop(&home_xml, "schedule-inbox-URL");
        let schedule_outbox_url = extract_href_from_prop(&home_xml, "schedule-outbox-URL");
        let calendar_user_addresses = extract_elements(&home_xml, "calendar-user-address-set")
            .into_iter()
            .next()
            .map(|set| {
                extract_elements(&set, "href")
                    .into_iter()
                    .filter_map(|href| extract_first_text(&href, "href"))
                    .collect()
            })
            .unwrap_or_default();

        let calendars_body = r#"<?xml version="1.0" encoding="utf-8"?>
<d:propfind xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav" xmlns:cs="http://calendarserver.org/ns/">
  <d:prop>
    <d:displayname/>
    <d:resourcetype/>
    <d:sync-token/>
    <cs:getctag/>
    <c:supported-calendar-component-set/>
  </d:prop>
</d:propfind>"#;
        let calendar_home_abs = self.absolute_dav_url(&calendar_home_set);
        let calendars_xml = self
            .propfind(&calendar_home_abs, "1", calendars_body)
            .await?;
        let calendars = parse_calendar_home_multistatus(&calendars_xml);

        Ok(CalDavDiscovery {
            principal_url,
            calendar_home_set,
            schedule_inbox_url,
            schedule_outbox_url,
            calendar_user_addresses,
            calendars,
        })
    }

    pub async fn calendar_multiget(
        &self,
        calendar: &str,
        hrefs: &[String],
    ) -> Result<Vec<CalDavObject>, VaultError> {
        if hrefs.is_empty() {
            return Ok(Vec::new());
        }
        let url = self.calendar_collection_url(calendar);
        let href_xml = hrefs
            .iter()
            .map(|href| format!("  <d:href>{}</d:href>", escape_xml(href)))
            .collect::<Vec<_>>()
            .join("\n");
        let body = format!(
            r#"<?xml version="1.0" encoding="utf-8"?>
<c:calendar-multiget xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:prop>
    <d:getetag/>
    <c:calendar-data/>
  </d:prop>
{href_xml}
</c:calendar-multiget>"#
        );
        let xml = self
            .report(&url, "1", &body, "CalDAV calendar-multiget")
            .await?;
        Ok(parse_caldav_objects(&xml))
    }

    pub async fn sync_calendar_collection(
        &self,
        calendar: &str,
        sync_token: Option<&str>,
    ) -> Result<CalDavSyncCollectionResponse, VaultError> {
        let url = self.calendar_collection_url(calendar);
        let token = sync_token.unwrap_or("");
        let body = format!(
            r#"<?xml version="1.0" encoding="utf-8"?>
<d:sync-collection xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:sync-token>{}</d:sync-token>
  <d:sync-level>1</d:sync-level>
  <d:prop>
    <d:getetag/>
    <c:calendar-data/>
  </d:prop>
</d:sync-collection>"#,
            escape_xml(token)
        );
        let xml = self
            .report(&url, "1", &body, "CalDAV sync-collection")
            .await?;
        Ok(CalDavSyncCollectionResponse {
            sync_token: extract_first_text(&xml, "sync-token"),
            objects: parse_caldav_objects(&xml),
        })
    }

    pub async fn put_calendar_object(
        &self,
        calendar: &str,
        href: &str,
        calendar_data: &str,
        if_match: Option<&str>,
        if_none_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let url = self.calendar_object_url(calendar, href);
        let mut req = self
            .auth(self.http.put(&url))
            .header("Content-Type", "text/calendar; charset=utf-8");
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }
        if let Some(etag) = if_none_match {
            req = req.header("If-None-Match", etag);
        }
        let resp = req
            .body(calendar_data.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV object PUT: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV object PUT {url}: {}",
                resp.status()
            )));
        }
        Ok(())
    }

    pub async fn delete_calendar_object(
        &self,
        calendar: &str,
        href: &str,
        if_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let url = self.calendar_object_url(calendar, href);
        let mut req = self.auth(self.http.delete(&url));
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV object DELETE: {e}")))?;
        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "CalDAV object DELETE {url}: {}",
                resp.status()
            )));
        }
        Ok(())
    }

    pub async fn send_calendar_schedule(
        &self,
        outbox_url: &str,
        calendar_data: &str,
    ) -> Result<CalDavScheduleResponse, VaultError> {
        let url = self.absolute_dav_url(outbox_url);
        let resp = self
            .auth(
                self.http
                    .post(&url)
                    .header("Content-Type", "text/calendar; charset=utf-8"),
            )
            .body(calendar_data.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV schedule POST: {e}")))?;
        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV schedule POST {url}: {status}: {body}"
            )));
        }
        Ok(CalDavScheduleResponse {
            status: status.as_u16(),
            body,
        })
    }

    pub async fn calendar_free_busy(
        &self,
        calendar: &str,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError> {
        let url = self.calendar_collection_url(calendar);
        let body = format!(
            r#"<?xml version="1.0" encoding="utf-8"?>
<c:free-busy-query xmlns:c="urn:ietf:params:xml:ns:caldav">
  <c:time-range start="{}" end="{}"/>
</c:free-busy-query>"#,
            start.format("%Y%m%dT%H%M%SZ"),
            end.format("%Y%m%dT%H%M%SZ")
        );
        let xml = self.report(&url, "1", &body, "CalDAV free-busy").await?;
        Ok(parse_free_busy_intervals(&xml))
    }

    async fn propfind(&self, url: &str, depth: &str, body: &str) -> Result<String, VaultError> {
        let method = reqwest::Method::from_bytes(b"PROPFIND").unwrap();
        let resp = self
            .auth(
                self.http
                    .request(method, url)
                    .header("Content-Type", "application/xml; charset=utf-8")
                    .header("Depth", depth),
            )
            .body(body.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV PROPFIND: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV PROPFIND {url}: {}",
                resp.status()
            )));
        }
        resp.text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))
    }

    async fn report(
        &self,
        url: &str,
        depth: &str,
        body: &str,
        label: &str,
    ) -> Result<String, VaultError> {
        let method = reqwest::Method::from_bytes(b"REPORT").unwrap();
        let resp = self
            .auth(
                self.http
                    .request(method, url)
                    .header("Content-Type", "application/xml; charset=utf-8")
                    .header("Depth", depth),
            )
            .body(body.to_string())
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("{label}: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "{label} {url}: {}",
                resp.status()
            )));
        }
        resp.text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))
    }

    /// Push a task to the Nextcloud Tasks app as a VTODO on the given calendar.
    pub async fn push_task_to_calendar(
        &self,
        calendar: &str,
        task: &Task,
    ) -> Result<(), VaultError> {
        self.put_task_to_calendar(calendar, task, None, None).await
    }

    pub async fn put_task_to_calendar(
        &self,
        calendar: &str,
        task: &Task,
        if_match: Option<&str>,
        if_none_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let uid = task.id.as_deref().unwrap_or(&task.title);
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        let ics = task_to_ics_inline(task);

        let mut req = self
            .auth(self.http.put(&url))
            .header("Content-Type", "text/calendar; charset=utf-8");
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }
        if let Some(etag) = if_none_match {
            req = req.header("If-None-Match", etag);
        }

        let resp = req
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
        self.delete_task_from_calendar_with_etag(calendar, uid, None)
            .await
    }

    pub async fn delete_task_from_calendar_with_etag(
        &self,
        calendar: &str,
        uid: &str,
        if_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        let mut req = self.auth(self.http.delete(&url));
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }

        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV DELETE: {e}")))?;
        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "CalDAV DELETE {}: {}",
                safe_uid,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Push a VEVENT to a CalDAV calendar.
    pub async fn push_event_to_calendar(
        &self,
        calendar: &str,
        event: &CalendarEvent,
    ) -> Result<(), VaultError> {
        self.put_event_to_calendar(calendar, event, None, None)
            .await
    }

    pub async fn put_event_to_calendar(
        &self,
        calendar: &str,
        event: &CalendarEvent,
        if_match: Option<&str>,
        if_none_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let uid = event.id.as_deref().unwrap_or(&event.title);
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        let mut req = self
            .auth(self.http.put(&url))
            .header("Content-Type", "text/calendar; charset=utf-8");
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }
        if let Some(etag) = if_none_match {
            req = req.header("If-None-Match", etag);
        }

        let resp = req
            .body(event_to_ics_inline(event))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV VEVENT PUT: {e}")))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "CalDAV VEVENT PUT {}: {}",
                safe_uid,
                resp.status()
            )));
        }

        Ok(())
    }

    /// Pull VEVENT calendar items from a CalDAV calendar.
    pub async fn pull_events_from_calendar(
        &self,
        calendar: &str,
    ) -> Result<Vec<CalendarEvent>, VaultError> {
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
      <c:comp-filter name="VEVENT"/>
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
            .map_err(|e| VaultError::IoError(format!("CalDAV VEVENT REPORT: {e}")))?;

        let xml = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        Ok(extract_vcalendars(&xml)
            .into_iter()
            .filter_map(|ics| ics_to_event_inline(&ics))
            .collect())
    }

    pub async fn delete_event_from_calendar(
        &self,
        calendar: &str,
        uid: &str,
    ) -> Result<(), VaultError> {
        self.delete_event_from_calendar_with_etag(calendar, uid, None)
            .await
    }

    pub async fn delete_event_from_calendar_with_etag(
        &self,
        calendar: &str,
        uid: &str,
        if_match: Option<&str>,
    ) -> Result<(), VaultError> {
        let safe_uid = uid.replace(' ', "-").replace('/', "-");
        let url = format!(
            "{}/remote.php/dav/calendars/{}/{}/{}.ics",
            self.base_url, self.username, calendar, safe_uid
        );

        let mut req = self.auth(self.http.delete(&url));
        if let Some(etag) = if_match {
            req = req.header("If-Match", etag);
        }

        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("CalDAV VEVENT DELETE: {e}")))?;
        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "CalDAV VEVENT DELETE {}: {}",
                safe_uid,
                resp.status()
            )));
        }

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

    pub async fn delete_card(
        &self,
        board_id: u64,
        stack_id: u64,
        card_id: u64,
    ) -> Result<(), VaultError> {
        let url = format!(
            "{}/index.php/apps/deck/api/v1.0/boards/{}/stacks/{}/cards/{}",
            self.base_url, board_id, stack_id, card_id
        );

        let resp = self
            .auth(self.http.delete(&url))
            .header("OCS-APIRequest", "true")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("Deck API delete card: {e}")))?;

        if !resp.status().is_success() && resp.status().as_u16() != 404 {
            return Err(VaultError::IoError(format!(
                "Deck API delete card {} failed: {}",
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

fn extract_vcalendars(xml: &str) -> Vec<String> {
    xml.split("BEGIN:VCALENDAR")
        .filter(|chunk| chunk.contains("END:VCALENDAR"))
        .map(|chunk| {
            let body = chunk.split("END:VCALENDAR").next().unwrap_or("");
            format!("BEGIN:VCALENDAR{}END:VCALENDAR", body)
        })
        .collect()
}

fn ensure_trailing_slash(value: &str) -> String {
    if value.ends_with('/') {
        value.to_string()
    } else {
        format!("{value}/")
    }
}

fn escape_xml(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

fn unescape_xml(value: &str) -> String {
    value
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
        .replace("&apos;", "'")
        .replace("&amp;", "&")
}

fn element_local_name(tag: &str) -> &str {
    let tag = tag
        .trim()
        .trim_start_matches('/')
        .split_whitespace()
        .next()
        .unwrap_or("");
    tag.rsplit_once(':').map(|(_, local)| local).unwrap_or(tag)
}

fn opening_tag_end(xml: &str, from: usize, local_name: &str) -> Option<usize> {
    let tag_end = xml[from..].find('>')? + from;
    let tag = &xml[from + 1..tag_end];
    if tag.starts_with('/') || element_local_name(tag) != local_name {
        return None;
    }
    Some(tag_end)
}

fn extract_elements(xml: &str, local_name: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut pos = 0;
    while let Some(rel) = xml[pos..].find('<') {
        let start = pos + rel;
        let Some(open_end) = opening_tag_end(xml, start, local_name) else {
            pos = start + 1;
            continue;
        };
        let open_tag = &xml[start + 1..open_end];
        if open_tag.trim_end().ends_with('/') {
            out.push(xml[start..=open_end].to_string());
            pos = open_end + 1;
            continue;
        }
        let mut scan = open_end + 1;
        let mut depth = 1usize;
        while let Some(next_rel) = xml[scan..].find('<') {
            let next = scan + next_rel;
            let Some(next_end) = xml[next..].find('>').map(|i| next + i) else {
                break;
            };
            let tag = &xml[next + 1..next_end];
            let local = element_local_name(tag);
            if local == local_name {
                if tag.trim_start().starts_with('/') {
                    depth -= 1;
                    if depth == 0 {
                        out.push(xml[start..=next_end].to_string());
                        pos = next_end + 1;
                        break;
                    }
                } else if !tag.trim_end().ends_with('/') {
                    depth += 1;
                }
            }
            scan = next_end + 1;
        }
        if scan >= xml.len() {
            break;
        }
    }
    out
}

fn extract_first_text(xml: &str, local_name: &str) -> Option<String> {
    let element = extract_elements(xml, local_name).into_iter().next()?;
    let open_end = element.find('>')?;
    let close_start = element.rfind("</")?;
    Some(unescape_xml(element[open_end + 1..close_start].trim())).filter(|value| !value.is_empty())
}

fn extract_href_from_prop(xml: &str, prop_name: &str) -> Option<String> {
    extract_elements(xml, prop_name)
        .into_iter()
        .find_map(|prop| extract_first_text(&prop, "href"))
        .or_else(|| extract_first_text(xml, "href"))
}

fn parse_calendar_home_multistatus(xml: &str) -> Vec<CalDavCalendarInfo> {
    extract_elements(xml, "response")
        .into_iter()
        .filter_map(|response| {
            if !response.contains(":calendar") && !response.contains("<calendar") {
                return None;
            }
            let href = extract_first_text(&response, "href")?;
            let name = href
                .trim_end_matches('/')
                .rsplit('/')
                .next()
                .unwrap_or("")
                .to_string();
            if name.is_empty() {
                return None;
            }
            let component_set = extract_elements(&response, "supported-calendar-component-set")
                .into_iter()
                .next()
                .unwrap_or_default();
            let components = ["VEVENT", "VTODO", "VJOURNAL", "VFREEBUSY"]
                .into_iter()
                .filter(|component| {
                    component_set.contains(&format!("name=\"{component}\""))
                        || component_set.contains(&format!("name='{component}'"))
                })
                .map(str::to_string)
                .collect();
            Some(CalDavCalendarInfo {
                href,
                name,
                display_name: extract_first_text(&response, "displayname"),
                sync_token: extract_first_text(&response, "sync-token"),
                ctag: extract_first_text(&response, "getctag"),
                components,
            })
        })
        .collect()
}

fn parse_caldav_objects(xml: &str) -> Vec<CalDavObject> {
    extract_elements(xml, "response")
        .into_iter()
        .filter_map(|response| {
            let href = extract_first_text(&response, "href")?;
            let status = extract_first_text(&response, "status").unwrap_or_default();
            let deleted = status.contains(" 404 ") || status.ends_with(" 404");
            let calendar_data = extract_first_text(&response, "calendar-data");
            let component = calendar_data.as_deref().and_then(calendar_component);
            let details = calendar_data.as_deref().map(parse_caldav_object_details);
            let task = calendar_data
                .as_deref()
                .filter(|ics| ics.contains("BEGIN:VTODO"))
                .and_then(ics_to_task_inline);
            let event = calendar_data
                .as_deref()
                .filter(|ics| ics.contains("BEGIN:VEVENT"))
                .and_then(ics_to_event_inline);
            Some(CalDavObject {
                href,
                etag: extract_first_text(&response, "getetag"),
                status,
                component,
                calendar_data,
                details,
                task,
                event,
                deleted,
            })
        })
        .collect()
}

fn calendar_component(ics: &str) -> Option<String> {
    ["VEVENT", "VTODO", "VJOURNAL", "VFREEBUSY"]
        .into_iter()
        .find(|component| ics.contains(&format!("BEGIN:{component}")))
        .map(str::to_string)
}

#[derive(Debug, Clone)]
struct ParsedIcsProperty {
    name: String,
    value: String,
    parameters: Vec<CalDavParameter>,
}

fn parse_caldav_object_details(ics: &str) -> CalDavObjectDetails {
    let calendar_props = parse_top_level_ics_properties(ics);
    CalDavObjectDetails {
        product_id: property_value(&calendar_props, "PRODID"),
        method: property_value(&calendar_props, "METHOD"),
        timezones: extract_ics_blocks(ics, "VTIMEZONE")
            .into_iter()
            .filter_map(|block| {
                let tzid = parse_ics_properties(&block)
                    .into_iter()
                    .find(|prop| prop.name == "TZID")
                    .map(|prop| prop.value)?;
                Some(CalDavTimezone {
                    tzid,
                    calendar_data: block,
                })
            })
            .collect(),
        events: extract_ics_blocks(ics, "VEVENT")
            .into_iter()
            .map(|block| parse_caldav_event_instance(&block))
            .collect(),
    }
}

fn parse_caldav_event_instance(block: &str) -> CalDavEventInstance {
    let props = parse_ics_properties(block);
    CalDavEventInstance {
        uid: property_value(&props, "UID"),
        summary: property_value(&props, "SUMMARY").map(|value| unescape_ics_text(&value)),
        status: property_value(&props, "STATUS"),
        recurrence_id: property_value(&props, "RECURRENCE-ID"),
        dtstart: property_value(&props, "DTSTART"),
        dtend: property_value(&props, "DTEND"),
        dtstart_timezone: property_parameter_value(&props, "DTSTART", "TZID"),
        dtend_timezone: property_parameter_value(&props, "DTEND", "TZID"),
        recurrence_id_timezone: property_parameter_value(&props, "RECURRENCE-ID", "TZID"),
        rrules: property_values(&props, "RRULE"),
        rdates: property_values(&props, "RDATE"),
        exdates: property_values(&props, "EXDATE"),
        organizer: props
            .iter()
            .find(|prop| prop.name == "ORGANIZER")
            .map(parse_participant),
        attendees: props
            .iter()
            .filter(|prop| prop.name == "ATTENDEE")
            .map(parse_participant)
            .collect(),
        alarms: extract_ics_blocks(block, "VALARM")
            .into_iter()
            .map(|alarm| parse_caldav_alarm(&alarm))
            .collect(),
        raw_properties: props
            .into_iter()
            .filter(|prop| {
                !matches!(
                    prop.name.as_str(),
                    "BEGIN" | "END"
                        | "UID"
                        | "SUMMARY"
                        | "STATUS"
                        | "RECURRENCE-ID"
                        | "DTSTART"
                        | "DTEND"
                        | "RRULE"
                        | "RDATE"
                        | "EXDATE"
                        | "ORGANIZER"
                        | "ATTENDEE"
                )
            })
            .map(into_caldav_property)
            .collect(),
    }
}

fn parse_caldav_alarm(block: &str) -> CalDavAlarm {
    let props = parse_ics_properties(block);
    CalDavAlarm {
        action: property_value(&props, "ACTION"),
        trigger: property_value(&props, "TRIGGER"),
        description: property_value(&props, "DESCRIPTION").map(|value| unescape_ics_text(&value)),
        summary: property_value(&props, "SUMMARY").map(|value| unescape_ics_text(&value)),
        attendees: props
            .iter()
            .filter(|prop| prop.name == "ATTENDEE")
            .map(parse_participant)
            .collect(),
        raw_properties: props
            .into_iter()
            .filter(|prop| {
                !matches!(
                    prop.name.as_str(),
                    "BEGIN" | "END" | "ACTION" | "TRIGGER" | "DESCRIPTION" | "SUMMARY" | "ATTENDEE"
                )
            })
            .map(into_caldav_property)
            .collect(),
    }
}

fn parse_participant(prop: &ParsedIcsProperty) -> CalDavParticipant {
    CalDavParticipant {
        value: prop.value.clone(),
        cn: parameter_value(prop, "CN"),
        role: parameter_value(prop, "ROLE"),
        partstat: parameter_value(prop, "PARTSTAT"),
        rsvp: parameter_value(prop, "RSVP"),
        cutype: parameter_value(prop, "CUTYPE"),
    }
}

fn parse_top_level_ics_properties(ics: &str) -> Vec<ParsedIcsProperty> {
    let mut depth = 0usize;
    let mut props = Vec::new();
    for line in unfold_ics_lines(ics) {
        let Some(prop) = parse_ics_property(&line) else {
            continue;
        };
        if prop.name == "BEGIN" {
            depth += 1;
            continue;
        }
        if prop.name == "END" {
            depth = depth.saturating_sub(1);
            continue;
        }
        if depth == 1 {
            props.push(prop);
        }
    }
    props
}

fn parse_ics_properties(block: &str) -> Vec<ParsedIcsProperty> {
    unfold_ics_lines(block)
        .into_iter()
        .filter_map(|line| parse_ics_property(&line))
        .collect()
}

fn parse_ics_property(line: &str) -> Option<ParsedIcsProperty> {
    let line = line.trim_end_matches('\r');
    let (left, value) = line.split_once(':')?;
    let mut parts = left.split(';');
    let name = parts.next()?.to_ascii_uppercase();
    let parameters = parts
        .filter_map(|part| {
            let (name, value) = part.split_once('=')?;
            Some(CalDavParameter {
                name: name.to_ascii_uppercase(),
                value: value.trim_matches('"').to_string(),
            })
        })
        .collect();
    Some(ParsedIcsProperty {
        name,
        value: value.to_string(),
        parameters,
    })
}

fn property_value(props: &[ParsedIcsProperty], name: &str) -> Option<String> {
    props
        .iter()
        .find(|prop| prop.name == name)
        .map(|prop| prop.value.clone())
}

fn property_values(props: &[ParsedIcsProperty], name: &str) -> Vec<String> {
    props
        .iter()
        .filter(|prop| prop.name == name)
        .map(|prop| prop.value.clone())
        .collect()
}

fn parameter_value(prop: &ParsedIcsProperty, name: &str) -> Option<String> {
    prop.parameters
        .iter()
        .find(|param| param.name == name)
        .map(|param| param.value.clone())
}

fn property_parameter_value(
    props: &[ParsedIcsProperty],
    property_name: &str,
    parameter_name: &str,
) -> Option<String> {
    props
        .iter()
        .find(|prop| prop.name == property_name)
        .and_then(|prop| parameter_value(prop, parameter_name))
}

fn into_caldav_property(prop: ParsedIcsProperty) -> CalDavProperty {
    CalDavProperty {
        name: prop.name,
        value: prop.value,
        parameters: prop.parameters,
    }
}

fn extract_ics_blocks(ics: &str, component: &str) -> Vec<String> {
    let begin = format!("BEGIN:{component}");
    let end = format!("END:{component}");
    let mut blocks = Vec::new();
    let mut current: Vec<String> = Vec::new();
    let mut depth = 0usize;

    for line in unfold_ics_lines(ics) {
        let clean = line.trim_end_matches('\r').to_string();
        if clean == begin {
            depth += 1;
            current.push(clean);
            continue;
        }
        if depth > 0 {
            current.push(clean.clone());
            if clean == begin {
                depth += 1;
            } else if clean == end {
                depth -= 1;
                if depth == 0 {
                    blocks.push(current.join("\r\n"));
                    current.clear();
                }
            }
        }
    }

    blocks
}

fn parse_free_busy_intervals(xml: &str) -> Vec<CalDavFreeBusyInterval> {
    let mut intervals = Vec::new();
    for ics in extract_vcalendars(xml) {
        let mut busy_type = None;
        for line in unfold_ics_lines(&ics) {
            if let Some(value) = line.strip_prefix("FBTYPE:") {
                busy_type = Some(value.to_string());
            }
            if let Some((params, value)) = line
                .strip_prefix("FREEBUSY")
                .and_then(|rest| rest.split_once(':'))
            {
                let kind = params
                    .split(';')
                    .find_map(|part| part.strip_prefix("FBTYPE="))
                    .map(str::to_string)
                    .or_else(|| busy_type.clone());
                if let Some((start, end)) = value.split_once('/') {
                    if let (Some(start), Some(end)) = (
                        parse_ics_datetime(start, false),
                        parse_ics_datetime(end, false),
                    ) {
                        intervals.push(CalDavFreeBusyInterval {
                            start,
                            end,
                            busy_type: kind,
                        });
                    }
                }
            }
        }
    }
    intervals
}

fn parse_ics_datetime(value: &str, all_day: bool) -> Option<DateTime<Utc>> {
    if all_day {
        return NaiveDate::parse_from_str(value.get(..8)?, "%Y%m%d")
            .ok()
            .and_then(|d| d.and_hms_opt(0, 0, 0))
            .map(|dt| dt.and_utc());
    }
    chrono::NaiveDateTime::parse_from_str(value, "%Y%m%dT%H%M%SZ")
        .ok()
        .map(|dt| dt.and_utc())
        .or_else(|| {
            DateTime::parse_from_str(value, "%Y%m%dT%H%M%S%z")
                .ok()
                .map(|dt| dt.to_utc())
        })
        .or_else(|| {
            chrono::NaiveDateTime::parse_from_str(value, "%Y%m%dT%H%M%S")
                .ok()
                .map(|dt| dt.and_utc())
        })
}

fn unfold_ics_lines(ics: &str) -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();
    for line in ics.lines() {
        if line.starts_with(' ') || line.starts_with('\t') {
            if let Some(last) = lines.last_mut() {
                last.push_str(line.trim_start());
            }
        } else {
            lines.push(line.to_string());
        }
    }
    lines
}

fn escape_ics_text(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\n', "\\n")
        .replace(',', "\\,")
        .replace(';', "\\;")
}

fn unescape_ics_text(value: &str) -> String {
    value
        .replace("\\n", "\n")
        .replace("\\,", ",")
        .replace("\\;", ";")
        .replace("\\\\", "\\")
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
    use crate::provider::live_nextcloud_credentials;

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

    #[test]
    fn calendar_event_roundtrip_preserves_vevent_fields() {
        let start = chrono::DateTime::parse_from_rfc3339("2026-05-01T19:00:00Z")
            .unwrap()
            .to_utc();
        let end = chrono::DateTime::parse_from_rfc3339("2026-05-01T21:00:00Z")
            .unwrap()
            .to_utc();
        let event = CalendarEvent {
            id: Some("event-1".to_string()),
            title: "Album release meeting".to_string(),
            description: Some("Confirm release plan".to_string()),
            location: Some("Studio A".to_string()),
            start,
            end: Some(end),
            status: CalendarEventStatus::Tentative,
            recurrence: Some("FREQ=WEEKLY;COUNT=2".to_string()),
            attendees: vec!["agent".to_string(), "codywright".to_string()],
            date_created: Some(start),
            date_modified: Some(end),
            ..Default::default()
        };

        let ics = event_to_ics_inline(&event);
        assert!(ics.contains("BEGIN:VEVENT"));
        assert!(ics.contains("DTSTART:20260501T190000Z"));
        assert!(ics.contains("DTEND:20260501T210000Z"));

        let parsed = ics_to_event_inline(&ics).expect("event should parse");
        assert_eq!(parsed.id, event.id);
        assert_eq!(parsed.title, event.title);
        assert_eq!(parsed.description, event.description);
        assert_eq!(parsed.location, event.location);
        assert_eq!(parsed.start, event.start);
        assert_eq!(parsed.end, event.end);
        assert_eq!(parsed.status, event.status);
        assert_eq!(parsed.recurrence, event.recurrence);
        assert_eq!(parsed.attendees, event.attendees);
    }

    #[test]
    fn caldav_discovery_parser_extracts_calendars() {
        let xml = r#"<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav" xmlns:cs="http://calendarserver.org/ns/">
  <d:response>
    <d:href>/remote.php/dav/calendars/agent/</d:href>
    <d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat>
  </d:response>
  <d:response>
    <d:href>/remote.php/dav/calendars/agent/tasks/</d:href>
    <d:propstat>
      <d:prop>
        <d:displayname>Tasks</d:displayname>
        <d:resourcetype><d:collection/><c:calendar/></d:resourcetype>
        <d:sync-token>http://nextcloud/ns/sync/42</d:sync-token>
        <cs:getctag>7</cs:getctag>
        <c:supported-calendar-component-set>
          <c:comp name="VTODO"/>
          <c:comp name="VEVENT"/>
        </c:supported-calendar-component-set>
      </d:prop>
      <d:status>HTTP/1.1 200 OK</d:status>
    </d:propstat>
  </d:response>
</d:multistatus>"#;

        let calendars = parse_calendar_home_multistatus(xml);
        assert_eq!(calendars.len(), 1);
        assert_eq!(calendars[0].name, "tasks");
        assert_eq!(calendars[0].display_name.as_deref(), Some("Tasks"));
        assert_eq!(
            calendars[0].sync_token.as_deref(),
            Some("http://nextcloud/ns/sync/42")
        );
        assert_eq!(calendars[0].components, vec!["VEVENT", "VTODO"]);
    }

    #[test]
    fn caldav_multiget_parser_extracts_tasks_events_and_deletes() {
        let event = CalendarEvent {
            id: Some("event-1".to_string()),
            title: "Calendar event".to_string(),
            start: chrono::DateTime::parse_from_rfc3339("2026-05-01T19:00:00Z")
                .unwrap()
                .to_utc(),
            ..Default::default()
        };
        let event_ics = escape_xml(&event_to_ics_inline(&event));
        let task = Task {
            id: Some("task-1".to_string()),
            title: "Calendar task".to_string(),
            ..Default::default()
        };
        let task_ics = escape_xml(&task_to_ics_inline(&task));
        let xml = format!(
            r#"<d:multistatus xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:response>
    <d:href>/remote.php/dav/calendars/agent/tasks/task-1.ics</d:href>
    <d:propstat><d:prop><d:getetag>"abc"</d:getetag><c:calendar-data>{task_ics}</c:calendar-data></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>
  </d:response>
  <d:response>
    <d:href>/remote.php/dav/calendars/agent/personal/event-1.ics</d:href>
    <d:propstat><d:prop><d:getetag>"def"</d:getetag><c:calendar-data>{event_ics}</c:calendar-data></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>
  </d:response>
  <d:response>
    <d:href>/remote.php/dav/calendars/agent/tasks/deleted.ics</d:href>
    <d:status>HTTP/1.1 404 Not Found</d:status>
  </d:response>
</d:multistatus>"#
        );

        let objects = parse_caldav_objects(&xml);
        assert_eq!(objects.len(), 3);
        assert_eq!(objects[0].component.as_deref(), Some("VTODO"));
        assert_eq!(
            objects[0].task.as_ref().map(|t| t.title.as_str()),
            Some("Calendar task")
        );
        assert_eq!(objects[1].component.as_deref(), Some("VEVENT"));
        assert_eq!(
            objects[1].event.as_ref().map(|event| event.title.as_str()),
            Some("Calendar event")
        );
        assert!(objects[2].deleted);
    }

    #[test]
    fn caldav_sync_collection_parser_extracts_token() {
        let xml = r#"<d:multistatus xmlns:d="DAV:">
  <d:sync-token>http://nextcloud/ns/sync/99</d:sync-token>
  <d:response><d:href>/remote.php/dav/calendars/agent/tasks/a.ics</d:href><d:status>HTTP/1.1 404 Not Found</d:status></d:response>
</d:multistatus>"#;

        assert_eq!(
            extract_first_text(xml, "sync-token").as_deref(),
            Some("http://nextcloud/ns/sync/99")
        );
        let objects = parse_caldav_objects(xml);
        assert_eq!(objects.len(), 1);
        assert!(objects[0].deleted);
    }

    #[test]
    fn caldav_free_busy_parser_extracts_intervals() {
        let xml = r#"<c:schedule-response xmlns:c="urn:ietf:params:xml:ns:caldav">
  <c:response>
    <c:calendar-data>BEGIN:VCALENDAR
BEGIN:VFREEBUSY
FREEBUSY;FBTYPE=BUSY:20260501T190000Z/20260501T200000Z
END:VFREEBUSY
END:VCALENDAR</c:calendar-data>
  </c:response>
</c:schedule-response>"#;

        let intervals = parse_free_busy_intervals(xml);
        assert_eq!(intervals.len(), 1);
        assert_eq!(intervals[0].busy_type.as_deref(), Some("BUSY"));
    }

    #[test]
    fn caldav_object_details_extracts_recurrence_exceptions_alarms_and_participants() {
        let ics = r#"BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Task Test//EN
METHOD:REQUEST
BEGIN:VTIMEZONE
TZID:America/Los_Angeles
BEGIN:STANDARD
DTSTART:20261101T020000
TZOFFSETFROM:-0700
TZOFFSETTO:-0800
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:event-1
SUMMARY:Weekly review
DTSTART;TZID=America/Los_Angeles:20260501T090000
DTEND;TZID=America/Los_Angeles:20260501T100000
RRULE:FREQ=WEEKLY;COUNT=3
EXDATE;TZID=America/Los_Angeles:20260508T090000
RDATE;TZID=America/Los_Angeles:20260509T090000
ORGANIZER;CN=Cody:mailto:cody@example.com
ATTENDEE;CN=Agent;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=FALSE:mailto:agent@example.com
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER:-PT15M
DESCRIPTION:Review starts soon
END:VALARM
END:VEVENT
BEGIN:VEVENT
UID:event-1
RECURRENCE-ID;TZID=America/Los_Angeles:20260515T090000
SUMMARY:Weekly review moved
DTSTART;TZID=America/Los_Angeles:20260515T110000
DTEND;TZID=America/Los_Angeles:20260515T120000
END:VEVENT
END:VCALENDAR"#;

        let details = parse_caldav_object_details(ics);
        assert_eq!(details.product_id.as_deref(), Some("-//Task Test//EN"));
        assert_eq!(details.method.as_deref(), Some("REQUEST"));
        assert_eq!(details.timezones.len(), 1);
        assert_eq!(details.timezones[0].tzid, "America/Los_Angeles");
        assert_eq!(details.events.len(), 2);

        let master = &details.events[0];
        assert_eq!(master.uid.as_deref(), Some("event-1"));
        assert_eq!(
            master.dtstart_timezone.as_deref(),
            Some("America/Los_Angeles")
        );
        assert_eq!(master.rrules, vec!["FREQ=WEEKLY;COUNT=3"]);
        assert_eq!(master.exdates, vec!["20260508T090000"]);
        assert_eq!(master.rdates, vec!["20260509T090000"]);
        assert_eq!(
            master.organizer.as_ref().and_then(|p| p.cn.as_deref()),
            Some("Cody")
        );
        assert_eq!(master.attendees[0].partstat.as_deref(), Some("ACCEPTED"));
        assert_eq!(master.alarms.len(), 1);
        assert_eq!(master.alarms[0].trigger.as_deref(), Some("-PT15M"));

        let override_event = &details.events[1];
        assert_eq!(
            override_event.recurrence_id.as_deref(),
            Some("20260515T090000")
        );
        assert_eq!(
            override_event.recurrence_id_timezone.as_deref(),
            Some("America/Los_Angeles")
        );
        assert_eq!(
            override_event.summary.as_deref(),
            Some("Weekly review moved")
        );
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials"]
    async fn nextcloud_caldav_discovery_and_sync_smoke() {
        let credentials = live_nextcloud_credentials();
        let client =
            NextcloudSync::new(&credentials.url, &credentials.username, &credentials.password);

        let discovery = client
            .discover_calendars()
            .await
            .expect("discover calendars");
        assert!(!discovery.principal_url.is_empty());
        assert!(!discovery.calendar_home_set.is_empty());
        assert!(!discovery.calendars.is_empty());

        let calendar = discovery
            .calendars
            .iter()
            .find(|calendar| calendar.components.iter().any(|c| c == "VTODO"))
            .or_else(|| discovery.calendars.first())
            .expect("calendar");
        let sync = client
            .sync_calendar_collection(&calendar.name, calendar.sync_token.as_deref())
            .await
            .expect("sync collection");
        assert!(sync.sync_token.is_some());
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials"]
    async fn nextcloud_caldav_vtodo_crud_smoke() {
        let credentials = live_nextcloud_credentials();
        let client =
            NextcloudSync::new(&credentials.url, &credentials.username, &credentials.password);
        let discovery = client.discover_calendars().await.expect("discover calendars");
        let calendar = discovery
            .calendars
            .iter()
            .find(|calendar| calendar.components.iter().any(|component| component == "VTODO"))
            .expect("a VTODO-capable calendar");
        let suffix = unix_suffix();
        let uid = format!("task-caldav-vtodo-smoke-{suffix}");
        let href = calendar_object_href(&calendar.href, &uid);
        let ics = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Task//Live Smoke//EN\r\nBEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Live CalDAV VTODO smoke {suffix}\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        );

        client
            .put_calendar_object(&calendar.name, &href, &ics, None, Some("*"))
            .await
            .expect("put vtodo");
        let objects = client
            .calendar_multiget(&calendar.name, std::slice::from_ref(&href))
            .await
            .expect("multiget vtodo");
        assert_eq!(objects.len(), 1);
        assert_eq!(objects[0].component.as_deref(), Some("VTODO"));
        let expected_title = format!("Live CalDAV VTODO smoke {suffix}");
        assert_eq!(
            objects[0].task.as_ref().map(|task| task.title.as_str()),
            Some(expected_title.as_str())
        );
        client
            .delete_calendar_object(&calendar.name, &href, objects[0].etag.as_deref())
            .await
            .expect("delete vtodo");
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials"]
    async fn nextcloud_caldav_vevent_crud_smoke() {
        let credentials = live_nextcloud_credentials();
        let calendar = credentials
            .event_calendar
            .as_deref()
            .unwrap_or(credentials.calendar.as_str());
        let client =
            NextcloudSync::new(&credentials.url, &credentials.username, &credentials.password);
        let suffix = unix_suffix();
        let uid = format!("task-caldav-vevent-smoke-{suffix}");
        let href = format!(
            "/remote.php/dav/calendars/{}/{}/{}.ics",
            credentials.username, calendar, uid
        );
        let ics = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Task//Live Smoke//EN\r\nBEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:Live CalDAV VEVENT smoke {suffix}\r\nDTSTART:20260501T190000Z\r\nDTEND:20260501T200000Z\r\nSTATUS:CONFIRMED\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
        );

        client
            .put_calendar_object(calendar, &href, &ics, None, Some("*"))
            .await
            .expect("put vevent");
        let objects = client
            .calendar_multiget(calendar, std::slice::from_ref(&href))
            .await
            .expect("multiget vevent");
        assert_eq!(objects.len(), 1);
        assert_eq!(objects[0].component.as_deref(), Some("VEVENT"));
        let expected_title = format!("Live CalDAV VEVENT smoke {suffix}");
        assert_eq!(
            objects[0].event.as_ref().map(|event| event.title.as_str()),
            Some(expected_title.as_str())
        );
        assert_eq!(objects[0].details.as_ref().map(|d| d.events.len()), Some(1));
        client
            .delete_calendar_object(calendar, &href, objects[0].etag.as_deref())
            .await
            .expect("delete vevent");
    }

    #[tokio::test]
    #[ignore = "requires live Nextcloud credentials and Deck access"]
    async fn nextcloud_deck_board_stack_card_smoke() {
        let credentials = live_nextcloud_credentials();
        let client =
            NextcloudSync::new(&credentials.url, &credentials.username, &credentials.password);
        let board = client
            .list_boards()
            .await
            .expect("list deck boards")
            .into_iter()
            .find(|board| !board.archived)
            .expect("at least one active Deck board");
        let stack = client
            .list_stacks(board.id)
            .await
            .expect("list deck stacks")
            .into_iter()
            .next()
            .expect("at least one Deck stack");

        let title = format!("Live Deck smoke {}", unix_suffix());
        let card_id = client
            .create_card(board.id, stack.id, &title, "Created by Task live smoke test", None)
            .await
            .expect("create deck card");
        let stacks = client
            .list_stacks(board.id)
            .await
            .expect("list deck stacks after create");
        assert!(stacks
            .iter()
            .flat_map(|stack| stack.cards.iter())
            .any(|card| card.id == card_id));
        client
            .delete_card(board.id, stack.id, card_id)
            .await
            .expect("delete deck card");
    }

    fn unix_suffix() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }

    fn calendar_object_href(calendar_href: &str, uid: &str) -> String {
        format!("{}/{}.ics", calendar_href.trim_end_matches('/'), uid)
    }
}
