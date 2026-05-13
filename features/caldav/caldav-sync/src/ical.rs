//! iCal ↔ `CalendarEvent` mapping.
//!
//! Two directions:
//!
//! - [`parse_calendar_data`] takes the raw `BEGIN:VCALENDAR…END:VCALENDAR`
//!   string fast-dav-rs hands us and returns every VEVENT it
//!   contains as a [`CalendarEventCreate`] payload. Synthesizes a
//!   deterministic local `Uuid` from the iCal `UID` via uuid v5
//!   so the same remote event always maps to the same local row
//!   across syncs.
//! - [`event_to_ical`] emits one VCALENDAR wrapping the given
//!   [`CalendarEvent`] as a single VEVENT — suitable for `PUT`ing
//!   onto a CalDAV collection.
//!
//! What's intentionally minimal: ATTENDEE / ORGANIZER / RRULE
//! emission isn't wired yet (icalendar 0.17's typed wrappers don't
//! cover every property we care about; the raw `add_property` path
//! works but is verbose). The fields land in the wire struct via
//! `attendees: Vec<String>` / `organizer: Option<String>` / `rrule:
//! Option<String>` from `CalendarEvent`, and we can wire emission
//! per field as the demo needs it.

use calendar_proto::CalendarEventCreate;
use chrono::{DateTime, NaiveDate, Utc};
use icalendar::{
    Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime, Event, EventLike,
    EventStatus,
};
use uuid::Uuid;

use crate::SyncError;

/// Namespace for `Uuid::new_v5` so the same iCal UID always maps
/// to the same local CalendarEvent.id across syncs. Treat this
/// constant as part of the on-disk schema — changing it
/// renumbers every imported event.
const ICAL_UID_NAMESPACE: Uuid = Uuid::from_u128(0x9c5c_1c3c_caa0_4f60_9b1a_4a31_2c93_24b3);

/// Pull every VEVENT out of a VCALENDAR payload. Non-event
/// components (VTODO, VJOURNAL, …) are skipped.
pub fn parse_calendar_data(data: &str) -> Result<Vec<ParsedEvent>, SyncError> {
    let calendar: Calendar = data
        .parse()
        .map_err(|e| SyncError::Internal(format!("ical parse: {e}")))?;

    let mut out = Vec::new();
    for component in calendar.components {
        if let CalendarComponent::Event(event) = component {
            if let Some(parsed) = event_from_ical(&event) {
                out.push(parsed);
            }
        }
    }
    Ok(out)
}

/// Result of parsing one VEVENT — the architect-shaped payload
/// plus the source UID so callers can keep their own bookkeeping
/// (logging, dedup, mapping the same event across two accounts).
#[derive(Debug, Clone)]
pub struct ParsedEvent {
    pub source_uid: String,
    pub create: CalendarEventCreate,
}

fn event_from_ical(event: &Event) -> Option<ParsedEvent> {
    let uid = event.get_uid()?.to_string();
    let title = event
        .get_summary()
        .map(|s| s.to_string())
        .unwrap_or_else(|| "(untitled)".to_string());

    let (start_at, end_at, all_day) = dates_from_event(event)?;

    let description = event.get_description().map(str::to_string);
    let location_text = event.get_location().map(str::to_string);
    let status = event
        .get_status()
        .map(event_status_to_str)
        .unwrap_or("confirmed")
        .to_string();

    Some(ParsedEvent {
        source_uid: uid,
        create: CalendarEventCreate {
            title,
            description,
            start_at,
            end_at,
            all_day,
            location_id: None,
            location_text,
            // RRULE pass-through: read the raw property string if
            // present; icalendar 0.17 doesn't have a typed RRULE
            // accessor on `EventLike`.
            rrule: event.property_value("RRULE").map(str::to_string),
            organizer: event.property_value("ORGANIZER").map(str::to_string),
            // ATTENDEE can appear multiple times — collect them all.
            attendees: event
                .properties()
                .iter()
                .filter(|(k, _)| k.as_str() == "ATTENDEE")
                .map(|(_, v)| v.value().to_string())
                .collect(),
            calendar_id: None,
            status,
            tags: Vec::new(),
        },
    })
}

/// Render a [`calendar_proto::CalendarEvent`] into a single-event
/// VCALENDAR string. Suitable for `PUT`ing to a CalDAV collection
/// path. The UID is the event's local id stringified — that's
/// what the server will echo back on subsequent pulls, and our
/// v5-uuid scheme means it'll round-trip to the same local id.
pub fn event_to_ical(event: &calendar_proto::CalendarEvent) -> String {
    let mut e = Event::new();
    e.uid(&event.id.to_string());
    e.summary(&event.title);
    if let Some(desc) = &event.description {
        e.description(desc);
    }
    if let Some(loc) = &event.location_text {
        e.location(loc);
    }
    set_date_or_datetime(&mut e, true, event.start_at, event.all_day);
    set_date_or_datetime(&mut e, false, event.end_at, event.all_day);
    if let Some(rrule) = &event.rrule {
        e.add_property("RRULE", rrule);
    }
    if let Some(org) = &event.organizer {
        e.add_property("ORGANIZER", org);
    }
    for attendee in &event.attendees {
        e.add_property("ATTENDEE", attendee);
    }
    e.status(str_to_event_status(&event.status));
    e.last_modified(event.updated_at);

    let mut cal = Calendar::new();
    cal.push(e.done());
    cal.to_string()
}

/// Map an iCal UID onto a local `Uuid` deterministically. Same UID
/// → same Uuid every time. Use this when reconciling a parsed
/// event against the local `CalendarEventRepo` — `repo.get(uuid)`
/// either finds the existing row to update or returns NotFound
/// for the create path.
pub fn local_id_for_uid(uid: &str) -> Uuid {
    Uuid::new_v5(&ICAL_UID_NAMESPACE, uid.as_bytes())
}

// ── Helpers ───────────────────────────────────────────────────────────

fn dates_from_event(event: &Event) -> Option<(DateTime<Utc>, DateTime<Utc>, bool)> {
    let start = event.get_start()?;
    let end = event.get_end()?;
    let (start_at, all_day_a) = dpt_to_utc(start);
    let (end_at, all_day_b) = dpt_to_utc(end);
    Some((start_at, end_at, all_day_a || all_day_b))
}

fn dpt_to_utc(dpt: DatePerhapsTime) -> (DateTime<Utc>, bool) {
    match dpt {
        DatePerhapsTime::Date(d) => (date_to_utc_midnight(d), true),
        DatePerhapsTime::DateTime(cdt) => (cdt_to_utc(cdt), false),
    }
}

/// Best-effort conversion to `DateTime<Utc>`. `Floating` and
/// `WithTimezone` variants don't carry enough info on their own —
/// `Floating` is "no timezone, follow the attendee", `WithTimezone`
/// requires the VTIMEZONE calendar component we'd have to look up.
/// Both fall back to treating the naive time as UTC; the demo
/// caller can refine when it wires a tz database.
fn cdt_to_utc(cdt: CalendarDateTime) -> DateTime<Utc> {
    match cdt {
        CalendarDateTime::Utc(dt) => dt,
        CalendarDateTime::Floating(naive)
        | CalendarDateTime::WithTimezone {
            date_time: naive, ..
        } => naive.and_utc(),
    }
}

fn date_to_utc_midnight(d: NaiveDate) -> DateTime<Utc> {
    d.and_hms_opt(0, 0, 0)
        .map(|naive| naive.and_utc())
        .unwrap_or_else(Utc::now)
}

fn set_date_or_datetime(event: &mut Event, is_start: bool, dt: DateTime<Utc>, all_day: bool) {
    if all_day {
        let date = dt.date_naive();
        if is_start {
            event.starts(date);
        } else {
            event.ends(date);
        }
    } else if is_start {
        event.starts(dt);
    } else {
        event.ends(dt);
    }
}

fn event_status_to_str(status: EventStatus) -> &'static str {
    match status {
        EventStatus::Tentative => "tentative",
        EventStatus::Confirmed => "confirmed",
        EventStatus::Cancelled => "cancelled",
    }
}

fn str_to_event_status(s: &str) -> EventStatus {
    match s {
        "tentative" => EventStatus::Tentative,
        "cancelled" => EventStatus::Cancelled,
        _ => EventStatus::Confirmed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    const SAMPLE: &str = "\
BEGIN:VCALENDAR\r
VERSION:2.0\r
PRODID:-//Test//EN\r
BEGIN:VEVENT\r
UID:test-event-1@example.com\r
SUMMARY:Studio time\r
DESCRIPTION:Tracking new material\r
LOCATION:Studio A\r
DTSTART:20260612T140000Z\r
DTEND:20260612T160000Z\r
STATUS:CONFIRMED\r
END:VEVENT\r
END:VCALENDAR\r
";

    #[test]
    fn parses_a_minimal_vevent() {
        let parsed = parse_calendar_data(SAMPLE).unwrap();
        assert_eq!(parsed.len(), 1);
        let e = &parsed[0];
        assert_eq!(e.source_uid, "test-event-1@example.com");
        assert_eq!(e.create.title, "Studio time");
        assert_eq!(e.create.location_text.as_deref(), Some("Studio A"));
        assert_eq!(
            e.create.description.as_deref(),
            Some("Tracking new material")
        );
        assert_eq!(e.create.status, "confirmed");
        assert!(!e.create.all_day);
        assert_eq!(
            e.create.start_at,
            Utc.with_ymd_and_hms(2026, 6, 12, 14, 0, 0).unwrap()
        );
    }

    #[test]
    fn deterministic_local_id() {
        let a = local_id_for_uid("event-42@example.com");
        let b = local_id_for_uid("event-42@example.com");
        let c = local_id_for_uid("event-43@example.com");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn round_trip_event_to_ical_and_back() {
        use calendar_proto::CalendarEvent;
        let id = Uuid::new_v4();
        let start = Utc.with_ymd_and_hms(2026, 9, 1, 17, 30, 0).unwrap();
        let end = Utc.with_ymd_and_hms(2026, 9, 1, 18, 30, 0).unwrap();
        let original = CalendarEvent {
            id,
            title: "Mix review".into(),
            description: Some("With the client".into()),
            start_at: start,
            end_at: end,
            all_day: false,
            location_id: None,
            location_text: Some("Studio B".into()),
            rrule: None,
            organizer: None,
            attendees: Vec::new(),
            calendar_id: None,
            status: "confirmed".into(),
            tags: Vec::new(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };

        let ical = event_to_ical(&original);
        let parsed = parse_calendar_data(&ical)
            .expect("parse")
            .pop()
            .expect("one event");

        assert_eq!(parsed.source_uid, id.to_string());
        assert_eq!(parsed.create.title, original.title);
        assert_eq!(parsed.create.description, original.description);
        assert_eq!(parsed.create.location_text, original.location_text);
        assert_eq!(parsed.create.start_at, start);
        assert_eq!(parsed.create.end_at, end);
        assert_eq!(parsed.create.status, "confirmed");
    }
}
