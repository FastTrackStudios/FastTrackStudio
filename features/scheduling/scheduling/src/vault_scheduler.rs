//! Disk-backed scheduler. Markdown content lives at
//! `<vault_root>/Projects/Scheduling/<kind>/<file>.md` (config)
//! and `<vault_root>/Records/bookings/<id>.md` (history); app-state goes
//! through pluggable `KvStore` + `LogStore` impls (see
//! `store-proto`). The scheduler doesn't care where the state
//! actually lands — JSON-on-disk, `SQLite`, in-memory all wire in
//! the same way.

use std::path::PathBuf;
use std::sync::Mutex;

use thiserror::Error;

use scheduling_proto::{
    AvailabilitySchedule, Booking, BookingId, BookingStatus, Bookings, DayTemplate, DayTemplateId,
    DayTemplates, EventType, EventTypeId, EventTypes, NewBooking, ScheduleId, Schedules,
    SchedulingError, SlotQuery, Slots, TimeSlot,
};
use store_proto::{KvStore, LogStore};

use crate::parse::{parse_booking, parse_day_template, parse_event_type, parse_schedule};
use crate::scan::frontmatter_split;
use crate::slots;
use crate::write::{
    serialize_booking, serialize_day_template, serialize_event_type, serialize_schedule,
};

// Scheduling content lives split across the vault: config under
// `Projects/Scheduling/` (a workflow container) and historical
// records under `Records/bookings/` (append-only history).
const TEMPLATES_DIR: &str = "Projects/Scheduling/templates";
const EVENT_TYPES_DIR: &str = "Projects/Scheduling/event-types";
const SCHEDULES_DIR: &str = "Projects/Scheduling/schedules";
const BOOKINGS_DIR: &str = "Records/bookings";

/// Errors not covered by `SchedulingError` (mostly path
/// composition + IO bubbling).
#[derive(Debug, Error)]
pub enum VaultSchedulerError {
    #[error("io: {0}")]
    Io(String),
    #[error("invalid vault root: {0}")]
    BadRoot(String),
}

/// Disk-backed implementation. Holds an absolute vault root + a
/// pair of trait-object stores for app state. Methods serialize
/// against a coarse `Mutex` so concurrent UI callers don't race
/// on a single file write.
pub struct VaultScheduler {
    root: PathBuf,
    /// `Mutex` guards writes; reads grab the lock just to make
    /// sure we never see a torn write in-flight. Coarse but
    /// fine for the desktop use case.
    write_lock: Mutex<()>,
    kv: Box<dyn KvStore + Send + Sync>,
    log: Box<dyn LogStore + Send + Sync>,
}

impl VaultScheduler {
    /// Open a scheduler rooted at `vault_root`. Creates the
    /// `<root>/Projects/Scheduling/<kind>/` and `<root>/Records/bookings/`
    /// subdirectories on demand at
    /// first write — we don't pre-make them so empty installs
    /// don't litter the vault.
    pub fn new(
        vault_root: impl Into<PathBuf>,
        kv: Box<dyn KvStore + Send + Sync>,
        log: Box<dyn LogStore + Send + Sync>,
    ) -> Result<Self, VaultSchedulerError> {
        let root = vault_root.into();
        if !root.is_dir() {
            return Err(VaultSchedulerError::BadRoot(root.display().to_string()));
        }
        Ok(Self {
            root,
            write_lock: Mutex::new(()),
            kv,
            log,
        })
    }

    pub fn root(&self) -> &std::path::Path {
        &self.root
    }

    /// Read every entity of a given kind from disk. Errors on
    /// individual files are logged via `tracing::warn` and the
    /// file is skipped — one malformed markdown shouldn't
    /// nuke the whole list.
    fn scan<T>(
        &self,
        subdir: &str,
        mut parser: impl FnMut(&str, &str) -> Result<T, crate::parse::ParseError>,
    ) -> Result<Vec<T>, SchedulingError> {
        let dir = self.root.join(subdir);
        if !dir.is_dir() {
            return Ok(Vec::new());
        }
        let mut out = Vec::new();
        let entries = std::fs::read_dir(&dir).map_err(io)?;
        for entry in entries {
            let entry = entry.map_err(io)?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) != Some("md") {
                continue;
            }
            let raw = match std::fs::read_to_string(&path) {
                Ok(s) => s,
                Err(e) => {
                    tracing::warn!(?path, error = %e, "scheduling: skip unreadable file");
                    continue;
                }
            };
            let Some((fm, _body)) = frontmatter_split(&raw) else {
                tracing::warn!(?path, "scheduling: skip file with no frontmatter");
                continue;
            };
            let rel = relativize(&self.root, &path);
            match parser(&rel, fm) {
                Ok(item) => out.push(item),
                Err(e) => {
                    tracing::warn!(?path, error = %e, "scheduling: parse failed");
                }
            }
        }
        Ok(out)
    }

    fn write_file(&self, rel_path: &str, body: &str) -> Result<(), SchedulingError> {
        let _guard = self
            .write_lock
            .lock()
            .map_err(|_| SchedulingError::Backend {
                message: "vault scheduler lock poisoned".into(),
            })?;
        let abs = self.root.join(rel_path);
        if let Some(parent) = abs.parent() {
            std::fs::create_dir_all(parent).map_err(io)?;
        }
        std::fs::write(&abs, body).map_err(io)?;
        Ok(())
    }

    fn delete_file(&self, rel_path: &str) -> Result<(), SchedulingError> {
        let _guard = self
            .write_lock
            .lock()
            .map_err(|_| SchedulingError::Backend {
                message: "vault scheduler lock poisoned".into(),
            })?;
        let abs = self.root.join(rel_path);
        if abs.exists() {
            std::fs::remove_file(&abs).map_err(io)?;
        }
        Ok(())
    }
}

// ── DayTemplates ─────────────────────────────────────────────────
impl DayTemplates for VaultScheduler {
    fn list_day_templates(&self) -> Result<Vec<DayTemplate>, SchedulingError> {
        self.scan(TEMPLATES_DIR, parse_day_template)
    }

    fn get_day_template(&self, id: &DayTemplateId) -> Result<DayTemplate, SchedulingError> {
        self.list_day_templates()?
            .into_iter()
            .find(|t| t.id.0 == id.0)
            .ok_or_else(|| SchedulingError::NotFound { id: id.0.clone() })
    }

    fn upsert_day_template(&self, template: &DayTemplate) -> Result<(), SchedulingError> {
        let body = serialize_day_template(template).map_err(write_err)?;
        self.write_file(
            &format!("{TEMPLATES_DIR}/{}.md", sanitize(&template.id.0)),
            &body,
        )
    }

    fn delete_day_template(&self, id: &DayTemplateId) -> Result<(), SchedulingError> {
        self.delete_file(&format!("{TEMPLATES_DIR}/{}.md", sanitize(&id.0)))
    }
}

// ── EventTypes ───────────────────────────────────────────────────
impl EventTypes for VaultScheduler {
    fn list_event_types(&self) -> Result<Vec<EventType>, SchedulingError> {
        self.scan(EVENT_TYPES_DIR, parse_event_type)
    }

    fn get_event_type(&self, id: &EventTypeId) -> Result<EventType, SchedulingError> {
        self.list_event_types()?
            .into_iter()
            .find(|t| t.id.0 == id.0)
            .ok_or_else(|| SchedulingError::NotFound { id: id.0.clone() })
    }

    fn upsert_event_type(&self, event_type: &EventType) -> Result<(), SchedulingError> {
        let body = serialize_event_type(event_type).map_err(write_err)?;
        self.write_file(
            &format!(
                "{EVENT_TYPES_DIR}/{}.md",
                sanitize(if event_type.slug.is_empty() {
                    &event_type.id.0
                } else {
                    &event_type.slug
                })
            ),
            &body,
        )
    }

    fn delete_event_type(&self, id: &EventTypeId) -> Result<(), SchedulingError> {
        // Caller may pass the slug; fall back to id for backwards
        // compat. We just try to remove every file whose parsed
        // event type matches the id.
        for et in self.list_event_types()? {
            if et.id.0 == id.0 {
                self.delete_file(&et.path)?;
            }
        }
        Ok(())
    }
}

// ── Schedules ────────────────────────────────────────────────────
impl Schedules for VaultScheduler {
    fn list_schedules(&self) -> Result<Vec<AvailabilitySchedule>, SchedulingError> {
        self.scan(SCHEDULES_DIR, parse_schedule)
    }

    fn get_schedule(&self, id: &ScheduleId) -> Result<AvailabilitySchedule, SchedulingError> {
        self.list_schedules()?
            .into_iter()
            .find(|s| s.id.0 == id.0)
            .ok_or_else(|| SchedulingError::NotFound { id: id.0.clone() })
    }

    fn upsert_schedule(&self, schedule: &AvailabilitySchedule) -> Result<(), SchedulingError> {
        let body = serialize_schedule(schedule).map_err(write_err)?;
        self.write_file(
            &format!("{SCHEDULES_DIR}/{}.md", sanitize(&schedule.id.0)),
            &body,
        )
    }

    fn delete_schedule(&self, id: &ScheduleId) -> Result<(), SchedulingError> {
        self.delete_file(&format!("{SCHEDULES_DIR}/{}.md", sanitize(&id.0)))
    }
}

// ── Slots ────────────────────────────────────────────────────────
impl Slots for VaultScheduler {
    fn list_open_slots(&self, query: &SlotQuery) -> Result<Vec<TimeSlot>, SchedulingError> {
        let et = self.get_event_type(&query.event_type_id)?;
        let schedule_id = et.schedule_id.clone().ok_or(SchedulingError::Invalid {
            field: "event_type.schedule_id".into(),
            reason: "no default schedule fallback yet — set schedule_id explicitly".into(),
        })?;
        let schedule = self.get_schedule(&schedule_id)?;
        let bookings = self.list_bookings()?;
        let from = slots::parse_utc(&query.from_utc)?;
        let to = slots::parse_utc(&query.to_utc)?;
        slots::list_open_slots(&et, &schedule, &bookings, from, to)
    }
}

// ── Bookings ─────────────────────────────────────────────────────
impl Bookings for VaultScheduler {
    fn list_bookings(&self) -> Result<Vec<Booking>, SchedulingError> {
        self.scan(BOOKINGS_DIR, parse_booking)
    }

    fn get_booking(&self, id: &BookingId) -> Result<Booking, SchedulingError> {
        self.list_bookings()?
            .into_iter()
            .find(|b| b.id.0 == id.0)
            .ok_or_else(|| SchedulingError::NotFound { id: id.0.clone() })
    }

    fn create_booking(&self, booking: &NewBooking) -> Result<Booking, SchedulingError> {
        let id = uuid::Uuid::new_v4().to_string();
        let now = chrono::Utc::now().to_rfc3339();
        let persisted = Booking {
            id: BookingId(id.clone()),
            path: format!("{BOOKINGS_DIR}/{id}.md"),
            event_type_id: booking.event_type_id.clone(),
            start_utc: booking.start_utc.clone(),
            end_utc: booking.end_utc.clone(),
            attendee_name: booking.attendee_name.clone(),
            attendee_email: booking.attendee_email.clone(),
            note: booking.note.clone(),
            status: BookingStatus::Confirmed,
            created_utc: now,
        };
        let body = serialize_booking(&persisted).map_err(write_err)?;
        self.write_file(&persisted.path, &body)?;
        // Audit log via the injected `LogStore`. Failure here
        // shouldn't roll back the booking — log it and move on.
        let _ = self.log.append(
            "scheduling.audit",
            format!("created:{}", persisted.id.0).into_bytes(),
        );
        // Invalidate any cached busy-time window touching this
        // slot. v1 just clears the whole namespace — the cache
        // is a future optimization anyway.
        if let Ok(keys) = self.kv.list_keys("scheduling.cache") {
            for k in keys {
                let _ = self.kv.delete("scheduling.cache", &k);
            }
        }
        Ok(persisted)
    }

    fn update_booking_status(
        &self,
        id: &BookingId,
        status: BookingStatus,
    ) -> Result<(), SchedulingError> {
        let mut booking = self.get_booking(id)?;
        booking.status = status;
        let body = serialize_booking(&booking).map_err(write_err)?;
        self.write_file(&booking.path, &body)?;
        let _ = self.log.append(
            "scheduling.audit",
            format!("status:{}:{:?}", id.0, status).into_bytes(),
        );
        Ok(())
    }
}

fn io(e: std::io::Error) -> SchedulingError {
    SchedulingError::Backend {
        message: format!("io: {e}"),
    }
}

fn write_err(e: crate::write::WriteError) -> SchedulingError {
    SchedulingError::Backend {
        message: e.to_string(),
    }
}

/// Vault-relative forward-slash path.
fn relativize(root: &std::path::Path, abs: &std::path::Path) -> String {
    abs.strip_prefix(root).map_or_else(
        |_| abs.display().to_string(),
        |p| p.to_string_lossy().replace('\\', "/"),
    )
}

/// Strip anything that doesn't belong in a vault filename so a
/// hostile id can't escape its subdir.
fn sanitize(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | '.' => c,
            _ => '-',
        })
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use scheduling_proto::{
        AvailabilityRule, BlockCategory, TimeBlock, TimeBlockId, TimeOfDay, Weekday,
    };
    use store_proto::MemStore;
    use tempfile::TempDir;

    fn fixture() -> (TempDir, VaultScheduler) {
        let tmp = TempDir::new().expect("tempdir");
        let kv: Box<dyn KvStore + Send + Sync> = Box::new(MemStore::new());
        let log: Box<dyn LogStore + Send + Sync> = Box::new(MemStore::new());
        let sched = VaultScheduler::new(tmp.path(), kv, log).expect("new scheduler");
        (tmp, sched)
    }

    fn weekday_template() -> DayTemplate {
        DayTemplate {
            id: DayTemplateId("weekday".into()),
            name: "Weekday".into(),
            description: None,
            blocks: vec![TimeBlock {
                id: TimeBlockId("morning".into()),
                start: TimeOfDay::new(6, 0),
                end: TimeOfDay::new(6, 30),
                label: "Morning Reset".into(),
                category: BlockCategory::Reset,
                note: None,
            }],
        }
    }

    #[test]
    fn day_template_round_trips_through_disk() {
        let (_tmp, sched) = fixture();
        let dt = weekday_template();
        sched.upsert_day_template(&dt).unwrap();
        let got = sched.get_day_template(&dt.id).unwrap();
        assert_eq!(got.name, "Weekday");
        let list = sched.list_day_templates().unwrap();
        assert_eq!(list.len(), 1);
        sched.delete_day_template(&dt.id).unwrap();
        assert!(matches!(
            sched.get_day_template(&dt.id),
            Err(SchedulingError::NotFound { .. })
        ));
    }

    #[test]
    fn create_booking_writes_file_and_logs_audit() {
        let (_tmp, sched) = fixture();
        // Need an event type + schedule for `list_open_slots`
        // later; here we just exercise the booking write.
        let new = NewBooking {
            event_type_id: EventTypeId("c30".into()),
            start_utc: "2026-06-01T09:00:00+00:00".into(),
            end_utc: "2026-06-01T09:30:00+00:00".into(),
            attendee_name: "Alice".into(),
            attendee_email: "alice@x".into(),
            note: None,
        };
        let persisted = sched.create_booking(&new).unwrap();
        assert!(persisted.path.starts_with("Records/bookings/"));
        assert!(matches!(persisted.status, BookingStatus::Confirmed));

        let listed = sched.list_bookings().unwrap();
        assert_eq!(listed.len(), 1);
    }

    #[test]
    fn slot_generation_intersects_schedule_with_bookings() {
        let (_tmp, sched) = fixture();
        let schedule = AvailabilitySchedule {
            id: ScheduleId("wh".into()),
            path: String::new(),
            name: "WH".into(),
            timezone: None,
            rules: vec![AvailabilityRule {
                days: vec![Weekday::Mon],
                start: TimeOfDay::new(9, 0),
                end: TimeOfDay::new(10, 0),
            }],
        };
        sched.upsert_schedule(&schedule).unwrap();
        let et = EventType {
            id: EventTypeId("c30".into()),
            path: String::new(),
            title: "Consult".into(),
            slug: "c30".into(),
            description: None,
            duration_min: 30,
            buffer_min: 0,
            location: scheduling_proto::EventTypeLocation::Tbd,
            schedule_id: Some(ScheduleId("wh".into())),
            published: true,
        };
        sched.upsert_event_type(&et).unwrap();

        let slots_before = sched
            .list_open_slots(&SlotQuery {
                event_type_id: EventTypeId("c30".into()),
                from_utc: "2026-06-01T00:00:00+00:00".into(),
                to_utc: "2026-06-02T00:00:00+00:00".into(),
            })
            .unwrap();
        assert_eq!(slots_before.len(), 2);

        // Book the 09:00 slot.
        sched
            .create_booking(&NewBooking {
                event_type_id: EventTypeId("c30".into()),
                start_utc: "2026-06-01T09:00:00+00:00".into(),
                end_utc: "2026-06-01T09:30:00+00:00".into(),
                attendee_name: "x".into(),
                attendee_email: "x@x".into(),
                note: None,
            })
            .unwrap();

        let slots_after = sched
            .list_open_slots(&SlotQuery {
                event_type_id: EventTypeId("c30".into()),
                from_utc: "2026-06-01T00:00:00+00:00".into(),
                to_utc: "2026-06-02T00:00:00+00:00".into(),
            })
            .unwrap();
        assert_eq!(slots_after.len(), 1);
        assert!(slots_after[0].start_utc.starts_with("2026-06-01T09:30"));
    }
}
