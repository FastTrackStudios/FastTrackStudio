// r[impl task.recurrence]
use chrono::{NaiveDate, TimeZone, Utc};
use rrule::RRuleSet;

use crate::task::{RecurrenceAnchor, Task};

/// Returns true if today has not already been completed for this recurring task.
pub fn is_due_today(completed_instances: &[String]) -> bool {
    let today = chrono::Local::now()
        .date_naive()
        .format("%Y-%m-%d")
        .to_string();
    !completed_instances.contains(&today)
}

/// Returns true if today has been explicitly skipped.
// r[impl task.recurrence.skipped]
pub fn is_skipped_today(skipped_instances: &[String]) -> bool {
    let today = chrono::Local::now()
        .date_naive()
        .format("%Y-%m-%d")
        .to_string();
    skipped_instances.contains(&today)
}

// r[impl task.recurrence.next]
/// Compute the next occurrence date for a recurring task.
/// Returns None if the task has no recurrence rule.
pub fn next_occurrence(task: &Task) -> Option<NaiveDate> {
    let rule_str = task.recurrence.as_deref()?;

    // Determine anchor date
    let anchor: NaiveDate = match task.recurrence_anchor {
        RecurrenceAnchor::Completion => task
            .completed_instances
            .last()
            .and_then(|s| s.parse::<NaiveDate>().ok())
            .or_else(|| {
                task.date_created
                    .map(|dt| dt.date_naive())
            })
            .unwrap_or_else(|| chrono::Local::now().date_naive()),
        RecurrenceAnchor::Scheduled => task
            .scheduled
            .or_else(|| task.date_created.map(|dt| dt.date_naive()))
            .unwrap_or_else(|| chrono::Local::now().date_naive()),
    };

    let today = chrono::Local::now().date_naive();

    // Build the rrule starting from the anchor
    // rrule crate expects "DTSTART:...\nRRULE:..." format
    let anchor_dt = Utc.from_utc_datetime(&anchor.and_hms_opt(0, 0, 0)?);
    let dtstart = anchor_dt.format("DTSTART:%Y%m%dT%H%M%SZ\n").to_string();
    let full_rule = format!("{}{}", dtstart, rule_str);

    let rrule_set: RRuleSet = full_rule.parse().ok()?;

    // Find first occurrence strictly after today
    let today_dt = Utc.from_utc_datetime(&today.and_hms_opt(0, 0, 0)?);
    rrule_set
        .into_iter()
        .find(|dt| dt > &today_dt)
        .map(|dt| dt.date_naive())
}
