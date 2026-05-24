//! File-backed [`WorkoutsService`] impl.

use std::sync::{Arc, Mutex};

use chrono::NaiveDate;
use uuid::Uuid;
use vault::Vault;

use crate::model::{LoggedSet, Routine, WorkoutSession};
use crate::parse::{looks_like_routine, looks_like_session, parse_routine, parse_session};
use crate::scan::{scan_routines, scan_sessions};
use crate::service::{WorkoutsError, WorkoutsService};
use crate::write::{
    default_routine_path, default_session_path, serialize_routine, serialize_session,
};
use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;

#[derive(Clone)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
}

impl Store {
    pub fn new(vault: Vault) -> Self {
        Self {
            inner: Arc::new(Mutex::new(vault)),
        }
    }

    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        Self { inner }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> WorkoutsError {
    WorkoutsError::Io(e.to_string())
}

fn find_routine_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault.pages.iter().position(|p| {
        looks_like_routine(p) && parse_routine(p).map(|r| r.id == id).unwrap_or(false)
    })
}

fn find_session_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault.pages.iter().position(|p| {
        looks_like_session(p) && parse_session(p).map(|s| s.id == id).unwrap_or(false)
    })
}

impl HasDispatcher for Store {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl WorkoutsService for Store {
    // ── Routines ────────────────────────────────────────
    fn list_routines(&self) -> Result<Vec<Routine>, WorkoutsError> {
        let guard = self.inner.lock().expect("workouts store poisoned");
        Ok(scan_routines(&guard))
    }

    fn get_routine(&self, id: &str) -> Result<Routine, WorkoutsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| WorkoutsError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("workouts store poisoned");
        for p in guard.pages.iter().filter(|p| looks_like_routine(p)) {
            if let Ok(r) = parse_routine(p) {
                if r.id == uuid {
                    return Ok(r);
                }
            }
        }
        Err(WorkoutsError::NotFound(id.to_string()))
    }

    fn create_routine(&self, mut routine: Routine) -> Result<Routine, WorkoutsError> {
        if routine.id.is_nil() {
            routine.id = Uuid::new_v4();
        }
        if routine.path.is_empty() {
            routine.path = default_routine_path(&routine.name, None);
        }
        let now = chrono::Utc::now();
        routine.date_created.get_or_insert(now);
        routine.date_modified = Some(now);
        let body = serialize_routine(&routine).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == routine.path) {
            return Err(WorkoutsError::AlreadyExists(routine.path));
        }
        vault::create_page(&mut guard, &routine.path, body).map_err(map_io)?;
        Ok(routine)
    }

    fn update_routine(&self, mut routine: Routine) -> Result<Routine, WorkoutsError> {
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        let idx = find_routine_idx(&guard, routine.id)
            .ok_or_else(|| WorkoutsError::NotFound(routine.id.to_string()))?;
        routine.path = guard.pages[idx].rel_path.clone();
        routine.date_modified = Some(chrono::Utc::now());
        let body = serialize_routine(&routine).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = routine.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(routine)
    }

    fn delete_routine(&self, id: &str) -> Result<(), WorkoutsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| WorkoutsError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        let idx = find_routine_idx(&guard, uuid)
            .ok_or_else(|| WorkoutsError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    // ── Sessions ────────────────────────────────────────
    fn list_sessions(&self) -> Result<Vec<WorkoutSession>, WorkoutsError> {
        let guard = self.inner.lock().expect("workouts store poisoned");
        Ok(scan_sessions(&guard))
    }

    fn get_session(&self, id: &str) -> Result<WorkoutSession, WorkoutsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| WorkoutsError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("workouts store poisoned");
        for p in guard.pages.iter().filter(|p| looks_like_session(p)) {
            if let Ok(s) = parse_session(p) {
                if s.id == uuid {
                    return Ok(s);
                }
            }
        }
        Err(WorkoutsError::NotFound(id.to_string()))
    }

    fn create_session(&self, mut session: WorkoutSession) -> Result<WorkoutSession, WorkoutsError> {
        if session.id.is_nil() {
            session.id = Uuid::new_v4();
        }
        if session.path.is_empty() {
            session.path = default_session_path(session.date, &session.name, None);
        }
        let now = chrono::Utc::now();
        session.date_created.get_or_insert(now);
        session.date_modified = Some(now);
        let body = serialize_session(&session).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == session.path) {
            return Err(WorkoutsError::AlreadyExists(session.path));
        }
        vault::create_page(&mut guard, &session.path, body).map_err(map_io)?;
        Ok(session)
    }

    fn update_session(&self, mut session: WorkoutSession) -> Result<WorkoutSession, WorkoutsError> {
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        let idx = find_session_idx(&guard, session.id)
            .ok_or_else(|| WorkoutsError::NotFound(session.id.to_string()))?;
        session.path = guard.pages[idx].rel_path.clone();
        session.date_modified = Some(chrono::Utc::now());
        let body = serialize_session(&session).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = session.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(session)
    }

    fn delete_session(&self, id: &str) -> Result<(), WorkoutsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| WorkoutsError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("workouts store poisoned");
        let idx = find_session_idx(&guard, uuid)
            .ok_or_else(|| WorkoutsError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn log_set(
        &self,
        session_id: &str,
        mut set: LoggedSet,
    ) -> Result<WorkoutSession, WorkoutsError> {
        let mut session = self.get_session(session_id)?;
        if set.id.is_nil() {
            set.id = Uuid::new_v4();
        }
        if set.order == 0 {
            // 0 sentinel: place at the end. Caller can
            // pass explicit non-zero `order` to insert
            // mid-session.
            set.order = session
                .logged_sets
                .iter()
                .map(|s| s.order)
                .max()
                .map(|m| m + 1)
                .unwrap_or(0);
        }
        session.logged_sets.push(set);
        self.update_session(session)
    }

    fn start_from_routine(
        &self,
        routine_id: &str,
        day_name: &str,
        date: &str,
    ) -> Result<WorkoutSession, WorkoutsError> {
        let routine = self.get_routine(routine_id)?;
        let day = routine
            .days
            .iter()
            .find(|d| d.name.eq_ignore_ascii_case(day_name))
            .ok_or_else(|| WorkoutsError::NotFound(format!("day {day_name} in routine")))?;
        let date: NaiveDate = date
            .parse()
            .map_err(|e| WorkoutsError::BadRequest(format!("date: {e}")))?;

        // Expand each slot's programmed `sets` count into
        // empty LoggedSet rows so the UI can step through
        // and fill actuals.
        let mut logged_sets = Vec::new();
        let mut order: u32 = 0;
        for slot in &day.slots {
            let reps_hint = slot
                .reps
                .as_deref()
                .and_then(|s| s.split(|c: char| !c.is_ascii_digit()).next())
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(0);
            let count = slot.sets.unwrap_or(1).max(1);
            for _ in 0..count {
                logged_sets.push(LoggedSet {
                    id: Uuid::new_v4(),
                    exercise_id: slot.exercise_id,
                    exercise_name: slot.exercise_name.clone(),
                    order,
                    reps: reps_hint,
                    weight_kg: slot.weight_kg.unwrap_or(0.0),
                    rir: slot.rir,
                    rpe: None,
                    completed: false,
                    note: slot.note.clone(),
                });
                order += 1;
            }
        }

        let session = WorkoutSession {
            path: String::new(),
            id: Uuid::nil(),
            name: format!("{} — {}", routine.name, day_name),
            date,
            routine_id: Some(routine.id),
            day_name: Some(day_name.to_string()),
            logged_sets: crate::model::LoggedSets(logged_sets),
            status: crate::model::SessionStatus::Planned.as_str().to_string(),
            duration_minutes: None,
            tags: crate::model::Tags::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
        };
        self.create_session(session)
    }
}
