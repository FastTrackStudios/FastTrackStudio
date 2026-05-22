//! RRULE expansion.
//!
//! Events carry an optional [`CalendarEvent::recurrence`] string
//! (RFC-5545 RRULE, e.g. `"FREQ=WEEKLY;BYDAY=MO,WE"`). At render
//! time the view component calls [`expand_in_range`] to turn each
//! master into a fan of instances that fall inside the visible
//! window. Each instance is the master cloned with its `start`/`end`
//! shifted to the occurrence's time; the `id` is preserved so any
//! mutation triggered from an instance flows back to the master.
//!
//! For v1, editing or dragging an instance edits the *whole
//! series*. Per-instance exceptions ("this and future" / "just
//! this") are a follow-up that needs an `exdates` + an
//! "override events" table on the state.

use chrono::{DateTime, Utc};
use rrule::{RRule, RRuleSet, Tz, Unvalidated};

use crate::types::CalendarEvent;

/// Maximum instances to return per master. Guards against runaway
/// "FREQ=SECONDLY"-style rules; the upper bound is matched against
/// the visible range so a year of weekly events expands without
/// hitting the cap.
const MAX_INSTANCES: u16 = 1024;

/// Expand `event` into all instances that overlap
/// `[range_start, range_end)`. Non-recurring events return
/// `vec![event]` when they overlap and `vec![]` otherwise. Bad
/// RRULE strings degrade to a single-instance render (the master)
/// so a typo doesn't make the event disappear.
#[must_use]
pub fn expand_in_range(
    event: &CalendarEvent,
    range_start: DateTime<Utc>,
    range_end: DateTime<Utc>,
) -> Vec<CalendarEvent> {
    let overlaps_master = event.end > range_start && event.start < range_end;
    let Some(rule_str) = event.recurrence.as_deref() else {
        return if overlaps_master {
            vec![event.clone()]
        } else {
            vec![]
        };
    };

    // Parse + validate. Fall back to the bare master on any error.
    let rrule: RRule<Unvalidated> = match rule_str.parse() {
        Ok(r) => r,
        Err(_) => {
            return if overlaps_master {
                vec![event.clone()]
            } else {
                vec![]
            };
        }
    };

    let dt_start = utc_to_rrule(event.start);
    let validated = match rrule.validate(dt_start) {
        Ok(r) => r,
        Err(_) => {
            return if overlaps_master {
                vec![event.clone()]
            } else {
                vec![]
            };
        }
    };

    let set = RRuleSet::new(dt_start)
        .rrule(validated)
        .after(utc_to_rrule(range_start))
        .before(utc_to_rrule(range_end));

    let duration = event.end - event.start;
    let result = set.all(MAX_INSTANCES);

    result
        .dates
        .into_iter()
        .map(|dt| {
            let start = rrule_to_utc(dt);
            let mut inst = event.clone();
            inst.start = start;
            inst.end = start + duration;
            inst
        })
        .collect()
}

/// Apply [`expand_in_range`] to each event in `events` and
/// flatten the results.
#[must_use]
pub fn expand_all(
    events: &[CalendarEvent],
    range_start: DateTime<Utc>,
    range_end: DateTime<Utc>,
) -> Vec<CalendarEvent> {
    events
        .iter()
        .flat_map(|ev| expand_in_range(ev, range_start, range_end))
        .collect()
}

fn utc_to_rrule(dt: DateTime<Utc>) -> DateTime<Tz> {
    dt.with_timezone(&Tz::UTC)
}

fn rrule_to_utc(dt: DateTime<Tz>) -> DateTime<Utc> {
    dt.with_timezone(&Utc)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Duration, TimeZone};

    fn at(y: i32, m: u32, d: u32, h: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(y, m, d, h, 0, 0).unwrap()
    }

    #[test]
    fn no_recurrence_returns_master_when_in_range() {
        let mut ev = CalendarEvent::new("standup", at(2026, 5, 4, 9), at(2026, 5, 4, 10));
        ev.recurrence = None;
        let out = expand_in_range(&ev, at(2026, 5, 1, 0), at(2026, 5, 8, 0));
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].start, ev.start);
    }

    #[test]
    fn weekly_expands_within_window() {
        let mut ev = CalendarEvent::new("standup", at(2026, 5, 4, 9), at(2026, 5, 4, 10));
        ev.recurrence = Some("FREQ=WEEKLY;BYDAY=MO,WE,FR".into());
        let out = expand_in_range(&ev, at(2026, 5, 4, 0), at(2026, 5, 11, 0));
        // Mon May 4, Wed May 6, Fri May 8 = 3 instances.
        assert_eq!(out.len(), 3);
        assert_eq!(out[1].start - out[0].start, Duration::days(2));
        assert!(out.iter().all(|i| i.id == ev.id));
    }

    #[test]
    fn invalid_rrule_falls_back_to_master() {
        let mut ev = CalendarEvent::new("standup", at(2026, 5, 4, 9), at(2026, 5, 4, 10));
        ev.recurrence = Some("NOT A REAL RULE".into());
        let out = expand_in_range(&ev, at(2026, 5, 1, 0), at(2026, 5, 8, 0));
        assert_eq!(out.len(), 1);
    }
}
