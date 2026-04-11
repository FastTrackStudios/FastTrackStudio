//! VTODO ↔ Task conversion.
//!
//! Maps between vault-core's `Task` struct and iCalendar `VTODO` components
//! per RFC 5545. This enables compatibility with Nextcloud Tasks, Thunderbird,
//! Apple Reminders, GNOME To Do, and any CalDAV-capable app.

use chrono::{NaiveDate, Utc};
use icalendar::{Calendar, CalendarComponent, Component, EventLike, Todo, TodoStatus};

use crate::task::{Priority, Status, Task, WikiLink};

// ── Task → VTODO ─────────────────────────────────────────────────────────────

/// Convert a vault-core `Task` to an iCalendar `Todo` (VTODO component).
pub fn task_to_vtodo(task: &Task) -> Todo {
    let mut todo = Todo::new();

    // UID
    if let Some(ref id) = task.id {
        todo.uid(id);
    }

    // SUMMARY
    todo.summary(&task.title);

    // STATUS
    todo.status(status_to_ical(&task.status));

    // PRIORITY (RFC 5545: 1=highest, 9=lowest, 0=undefined)
    let ical_priority = match task.priority {
        Priority::Urgent => 1,
        Priority::High => 3,
        Priority::Normal => 5,
        Priority::Low => 9,
        Priority::None => 0,
    };
    if ical_priority > 0 {
        todo.add_property("PRIORITY", ical_priority.to_string());
    }

    // DUE
    if let Some(due) = task.due {
        todo.due(due);
    }

    // DTSTART (scheduled date)
    if let Some(scheduled) = task.scheduled {
        todo.starts(scheduled);
    }

    // COMPLETED
    if let Some(completed) = task.completed_date {
        // COMPLETED must be UTC datetime
        let dt = completed
            .and_hms_opt(0, 0, 0)
            .map(|ndt| ndt.and_utc())
            .unwrap_or_else(Utc::now);
        todo.completed(dt);
    }

    // CATEGORIES (tags) — must use append_multi_property since CATEGORIES is a multi-prop
    if !task.tags.is_empty() {
        todo.append_multi_property(icalendar::Property::new("CATEGORIES", task.tags.join(",")));
    }

    // DESCRIPTION (contexts, projects, and other metadata as structured text)
    let mut desc_parts: Vec<String> = Vec::new();
    if !task.projects.is_empty() {
        desc_parts.push(format!(
            "Projects: {}",
            task.projects.iter().map(|p| p.0.as_str()).collect::<Vec<_>>().join(", ")
        ));
    }
    if !task.contexts.is_empty() {
        desc_parts.push(format!("Contexts: {}", task.contexts.join(", ")));
    }
    if let Some(est) = task.time_estimate {
        desc_parts.push(format!("Estimate: {} min", est));
    }
    if !desc_parts.is_empty() {
        todo.description(&desc_parts.join("\n"));
    }

    // RRULE (recurrence — already in RFC 5545 format)
    if let Some(ref rrule) = task.recurrence {
        todo.add_property("RRULE", rrule);
    }

    // RELATED-TO (blocked_by dependencies)
    for dep in &task.blocked_by {
        todo.add_property("RELATED-TO", &dep.uid);
    }

    // PERCENT-COMPLETE
    if task.status == Status::Done {
        todo.percent_complete(100);
    }

    // X-VAULT-AREAS (custom property for areas)
    if !task.areas.is_empty() {
        let areas_str = task.areas.iter().map(|a| a.0.as_str()).collect::<Vec<_>>().join(",");
        todo.add_property("X-VAULT-AREAS", areas_str);
    }

    // X-VAULT-SORT-ORDER (custom property)
    if let Some(ref order) = task.sort_order {
        todo.add_property("X-VAULT-SORT-ORDER", order);
    }

    // X-VAULT-EXTERNAL (external source tracking)
    if let Some(ref src) = task.external_source {
        todo.add_property("X-VAULT-EXTERNAL-SOURCE", src);
    }
    if let Some(ref eid) = task.external_id {
        todo.add_property("X-VAULT-EXTERNAL-ID", eid);
    }

    // Timestamp
    todo.timestamp(Utc::now());

    todo.done()
}

/// Convert a `Task` to a complete `.ics` file string.
pub fn task_to_ics(task: &Task) -> String {
    let todo = task_to_vtodo(task);
    let mut cal = Calendar::new();
    cal.push(todo);
    cal.done().to_string()
}

// ── VTODO → Task ─────────────────────────────────────────────────────────────

/// Convert an iCalendar `Todo` (VTODO) to a vault-core `Task`.
pub fn vtodo_to_task(todo: &Todo) -> Task {
    let mut task = Task::default();

    // UID → id
    task.id = todo.property_value("UID").map(|s| s.to_string());

    // SUMMARY → title
    task.title = todo
        .property_value("SUMMARY")
        .unwrap_or("Untitled")
        .to_string();

    // STATUS
    task.status = todo
        .get_status()
        .map(|s| ical_to_status(&s))
        .unwrap_or(Status::Open);

    // PRIORITY
    task.priority = todo
        .property_value("PRIORITY")
        .and_then(|s| s.parse::<u8>().ok())
        .map(ical_priority_to_priority)
        .unwrap_or(Priority::None);

    // DUE
    task.due = todo.get_due().and_then(|d| d.try_into().ok());

    // DTSTART → scheduled
    task.scheduled = todo
        .property_value("DTSTART")
        .and_then(|s| NaiveDate::parse_from_str(s, "%Y%m%d").ok());

    // COMPLETED
    task.completed_date = todo
        .get_completed()
        .map(|dt| dt.date_naive());

    // CATEGORIES → tags (stored in multi_properties)
    if let Some(props) = todo.multi_properties().get("CATEGORIES") {
        for prop in props {
            let val: &str = prop.value();
            for cat in val.split(',') {
                let cat = cat.trim().to_string();
                if !cat.is_empty() {
                    task.tags.push(cat);
                }
            }
        }
    }

    // DESCRIPTION → parse contexts, projects, estimate
    if let Some(desc) = todo.property_value("DESCRIPTION") {
        for line in desc.lines() {
            if let Some(projects_str) = line.strip_prefix("Projects: ") {
                task.projects = projects_str
                    .split(", ")
                    .map(|s| WikiLink(s.trim().to_string()))
                    .collect();
            } else if let Some(contexts_str) = line.strip_prefix("Contexts: ") {
                task.contexts = contexts_str
                    .split(", ")
                    .map(|s| s.trim().to_string())
                    .collect();
            } else if let Some(est_str) = line.strip_prefix("Estimate: ") {
                task.time_estimate = est_str
                    .strip_suffix(" min")
                    .and_then(|s| s.parse().ok());
            }
        }
    }

    // RRULE → recurrence
    task.recurrence = todo.property_value("RRULE").map(|s| s.to_string());

    // RELATED-TO → blocked_by
    // Note: RELATED-TO can appear multiple times; we collect all
    if let Some(rel) = todo.property_value("RELATED-TO") {
        task.blocked_by.push(crate::task::TaskDependency {
            uid: rel.to_string(),
            reltype: crate::task::DependencyRelType::FinishToStart,
            gap: None,
        });
    }

    // X-VAULT-AREAS
    if let Some(areas) = todo.property_value("X-VAULT-AREAS") {
        task.areas = areas
            .split(',')
            .map(|s| WikiLink(s.trim().to_string()))
            .collect();
    }

    // X-VAULT-SORT-ORDER
    task.sort_order = todo
        .property_value("X-VAULT-SORT-ORDER")
        .map(|s| s.to_string());

    // X-VAULT-EXTERNAL-SOURCE / X-VAULT-EXTERNAL-ID
    task.external_source = todo
        .property_value("X-VAULT-EXTERNAL-SOURCE")
        .map(|s| s.to_string());
    task.external_id = todo
        .property_value("X-VAULT-EXTERNAL-ID")
        .map(|s| s.to_string());

    // CREATED / LAST-MODIFIED
    task.date_created = todo
        .property_value("CREATED")
        .and_then(|s| {
            chrono::NaiveDateTime::parse_from_str(s, "%Y%m%dT%H%M%SZ")
                .ok()
                .map(|ndt| ndt.and_utc())
        });
    task.date_modified = todo
        .property_value("LAST-MODIFIED")
        .and_then(|s| {
            chrono::NaiveDateTime::parse_from_str(s, "%Y%m%dT%H%M%SZ")
                .ok()
                .map(|ndt| ndt.and_utc())
        });

    task
}

/// Parse an `.ics` file string and extract the first Task from it.
pub fn ics_to_task(ics: &str) -> Option<Task> {
    let cal: Calendar = ics.parse().ok()?;
    for component in cal.iter() {
        if let CalendarComponent::Todo(todo) = component {
            return Some(vtodo_to_task(todo));
        }
    }
    None
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn status_to_ical(status: &Status) -> TodoStatus {
    match status {
        Status::None | Status::Open | Status::Planned => TodoStatus::NeedsAction,
        Status::InProgress | Status::OnHold => TodoStatus::InProcess,
        Status::Done => TodoStatus::Completed,
        Status::Cancelled => TodoStatus::Cancelled,
        Status::Archived => TodoStatus::Completed,
    }
}

fn ical_to_status(status: &TodoStatus) -> Status {
    match status {
        TodoStatus::NeedsAction => Status::Open,
        TodoStatus::InProcess => Status::InProgress,
        TodoStatus::Completed => Status::Done,
        TodoStatus::Cancelled => Status::Cancelled,
    }
}

fn ical_priority_to_priority(p: u8) -> Priority {
    match p {
        0 => Priority::None,
        1..=2 => Priority::Urgent,
        3..=4 => Priority::High,
        5..=6 => Priority::Normal,
        7..=9 => Priority::Low,
        _ => Priority::None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_basic_task() {
        let task = Task {
            id: Some("test-uid-123".into()),
            title: "Buy groceries".into(),
            status: Status::Open,
            priority: Priority::High,
            due: NaiveDate::from_ymd_opt(2026, 4, 15),
            tags: vec!["errands".into(), "shopping".into()],
            contexts: vec!["home".into()],
            projects: vec![WikiLink("Personal".into())],
            time_estimate: Some(30),
            ..Default::default()
        };

        let ics = task_to_ics(&task);
        assert!(ics.contains("VTODO"));
        assert!(ics.contains("Buy groceries"));

        let parsed = ics_to_task(&ics).expect("should parse back");
        assert_eq!(parsed.title, "Buy groceries");
        assert_eq!(parsed.status, Status::Open);
        assert_eq!(parsed.priority, Priority::High);
        assert_eq!(parsed.tags, vec!["errands", "shopping"]);
        assert_eq!(parsed.id, Some("test-uid-123".into()));
    }

    #[test]
    fn roundtrip_completed_task() {
        let task = Task {
            title: "Done task".into(),
            status: Status::Done,
            completed_date: NaiveDate::from_ymd_opt(2026, 4, 10),
            ..Default::default()
        };

        let ics = task_to_ics(&task);
        let parsed = ics_to_task(&ics).expect("should parse");
        assert_eq!(parsed.status, Status::Done);
        assert!(parsed.completed_date.is_some());
    }

    #[test]
    fn roundtrip_recurring_task() {
        let task = Task {
            title: "Daily standup".into(),
            recurrence: Some("FREQ=DAILY;BYDAY=MO,TU,WE,TH,FR".into()),
            ..Default::default()
        };

        let ics = task_to_ics(&task);
        assert!(ics.contains("RRULE:FREQ=DAILY"));

        let parsed = ics_to_task(&ics).expect("should parse");
        assert!(parsed.recurrence.unwrap().contains("FREQ=DAILY"));
    }
}
