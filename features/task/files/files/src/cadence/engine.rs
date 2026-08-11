//! The cadence state machine: when a root gets an **auto-snapshot**,
//! when its session ends in a **Session checkpoint**, and which
//! project-file saves ride along as **save points** (all three glossary
//! terms, `apps/task/CONTEXT.md`).
//!
//! The engine knows nothing about jj, the CAS, or the filesystem. It
//! takes activity hints and a clock and answers one question —
//! *what is due right now?* — which is exactly what makes the whole
//! cadence testable without sleeping through a 30-minute quiescence
//! window. [`crate::FilesBackend`] does the storage half: it asks
//! [`CadenceEngine::take_due`], performs each capture, and reports back.
//!
//! # The cadence itself
//!
//! - Activity (a watcher hint, or [`files_proto::FilesService::hint_activity`])
//!   opens a **session** on the root if none is open, and extends it.
//!   Sessions are per-root: concurrent writers share one (glossary).
//! - While the session has uncaptured activity, an auto-snapshot falls
//!   due once [`CadenceConfig::snapshot_debounce`] (default 10 min) has
//!   passed since the last capture. That interval *is* the debounce: a
//!   recording pass writing every few seconds coalesces into one
//!   snapshot per window, not one per write.
//! - Once activity stops for [`CadenceConfig::quiescence`] (default 30
//!   min), the session ends in one certified Session checkpoint and the
//!   root's cadence state is dropped. A quiescent root is silent: no
//!   further checkpoints until someone writes again.
//! - Quiescence outranks the snapshot window — a checkpoint full-scans
//!   the live tree anyway, so taking a snapshot on the way out would
//!   capture the same bytes twice.

use std::collections::HashMap;
use std::sync::Mutex;

use chrono::{DateTime, TimeDelta, Utc};
use files_proto::{RootFlavor, SavePoint};
use uuid::Uuid;

use super::clock::Clock;
use crate::ignore::IgnoreSet;
use std::sync::Arc;

/// The tunables of the cadence (spec #255: "~10 min" snapshots,
/// "default 30 min" quiescence).
#[derive(Debug, Clone, Copy)]
pub struct CadenceConfig {
    /// How long uncaptured activity coalesces before an auto-snapshot
    /// falls due.
    pub snapshot_debounce: TimeDelta,
    /// How long a root must go without activity before its session ends
    /// in a Session checkpoint.
    pub quiescence: TimeDelta,
    /// How many times the certifying scan re-reads a file that changed
    /// while it was being hashed before giving up and requeueing it
    /// (see [`crate::certify`]).
    pub certify_attempts: u32,
}

impl Default for CadenceConfig {
    fn default() -> Self {
        Self {
            snapshot_debounce: TimeDelta::minutes(10),
            quiescence: TimeDelta::minutes(30),
            certify_attempts: 3,
        }
    }
}

/// What a due capture is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DueKind {
    /// An ephemeral auto-snapshot, mid-session.
    Snapshot,
    /// The session-ending, scan-certified checkpoint.
    Checkpoint,
}

/// One capture the backend should perform now.
#[derive(Debug, Clone)]
pub struct Due {
    pub root_id: Uuid,
    pub kind: DueKind,
    /// The save points this capture carries: for a snapshot, the
    /// project-file saves since the last capture ("the nearest
    /// auto-snapshot"); for a checkpoint, every save point of the
    /// session it closes.
    pub save_points: Vec<SavePoint>,
}

#[derive(Debug)]
struct RootCadence {
    /// Last capture (session start, or the most recent auto-snapshot) —
    /// the origin the snapshot debounce window is measured from.
    last_capture_at: DateTime<Utc>,
    /// Most recent activity hint — the origin quiescence is measured
    /// from.
    last_activity_at: DateTime<Utc>,
    /// Activity has happened since `last_capture_at`.
    uncaptured_activity: bool,
    /// A capture for this root is being performed right now; nothing
    /// else falls due until it reports back.
    in_flight: bool,
    /// Save points since the last capture.
    pending_save_points: Vec<SavePoint>,
    /// Save points captured earlier in this session (already carried by
    /// a snapshot), kept so the session's checkpoint reports the whole
    /// session's saves.
    session_save_points: Vec<SavePoint>,
}

impl RootCadence {
    fn open(now: DateTime<Utc>) -> Self {
        Self {
            last_capture_at: now,
            last_activity_at: now,
            uncaptured_activity: true,
            in_flight: false,
            pending_save_points: Vec::new(),
            session_save_points: Vec::new(),
        }
    }

    fn all_save_points(&self) -> Vec<SavePoint> {
        let mut all = self.session_save_points.clone();
        all.extend(self.pending_save_points.iter().cloned());
        all
    }
}

/// Per-root cadence state plus the clock and config it runs on.
#[derive(Debug)]
pub struct CadenceEngine {
    config: CadenceConfig,
    clock: Arc<dyn Clock>,
    roots: Mutex<HashMap<Uuid, RootCadence>>,
}

impl CadenceEngine {
    #[must_use]
    pub fn new(config: CadenceConfig, clock: Arc<dyn Clock>) -> Self {
        Self {
            config,
            clock,
            roots: Mutex::new(HashMap::new()),
        }
    }

    #[must_use]
    pub fn config(&self) -> &CadenceConfig {
        &self.config
    }

    #[must_use]
    pub fn now(&self) -> DateTime<Utc> {
        self.clock.now()
    }

    /// Record activity on `root_id`. `paths` are root-relative; those
    /// the Ignore set covers are dropped before they can open a session
    /// (the whole point of the set: a `.rpp-bak` storm is not a working
    /// session). Returns how many hints survived the filter.
    ///
    /// A surviving path that names a project file for `flavor` also
    /// marks a save point.
    pub fn note_activity(
        &self,
        root_id: Uuid,
        paths: &[String],
        ignore: &IgnoreSet,
        flavor: RootFlavor,
    ) -> u32 {
        let now = self.clock.now();
        let live: Vec<&String> = paths.iter().filter(|p| !ignore.is_ignored(p)).collect();
        if live.is_empty() {
            return 0;
        }
        let mut roots = self.roots.lock().expect("cadence state poisoned");
        let state = roots
            .entry(root_id)
            .or_insert_with(|| RootCadence::open(now));
        state.last_activity_at = now;
        state.uncaptured_activity = true;
        for path in &live {
            if IgnoreSet::is_project_file(path, flavor) {
                state.pending_save_points.push(SavePoint {
                    path: (*path).clone(),
                    at: now,
                });
            }
        }
        u32::try_from(live.len()).unwrap_or(u32::MAX)
    }

    /// Everything due as of the engine's clock, marked in-flight so a
    /// second tick (or a second driver) can't perform the same capture
    /// twice. Every returned [`Due`] must be answered with
    /// [`CadenceEngine::completed`] or [`CadenceEngine::failed`].
    pub fn take_due(&self) -> Vec<Due> {
        let now = self.clock.now();
        let mut roots = self.roots.lock().expect("cadence state poisoned");
        let mut due = Vec::new();
        for (root_id, state) in roots.iter_mut() {
            if state.in_flight {
                continue;
            }
            let kind = if now - state.last_activity_at >= self.config.quiescence {
                DueKind::Checkpoint
            } else if state.uncaptured_activity
                && now - state.last_capture_at >= self.config.snapshot_debounce
            {
                DueKind::Snapshot
            } else {
                continue;
            };
            state.in_flight = true;
            due.push(Due {
                root_id: *root_id,
                kind,
                save_points: match kind {
                    DueKind::Snapshot => state.pending_save_points.clone(),
                    DueKind::Checkpoint => state.all_save_points(),
                },
            });
        }
        due
    }

    /// A capture succeeded. A snapshot restarts the debounce window and
    /// carries its save points into the session's history; a checkpoint
    /// ends the session outright — the root goes quiet until someone
    /// writes to it again.
    pub fn completed(&self, due: &Due) {
        let now = self.clock.now();
        let mut roots = self.roots.lock().expect("cadence state poisoned");
        match due.kind {
            DueKind::Checkpoint => {
                roots.remove(&due.root_id);
            }
            DueKind::Snapshot => {
                if let Some(state) = roots.get_mut(&due.root_id) {
                    state.in_flight = false;
                    state.last_capture_at = now;
                    state.uncaptured_activity = false;
                    let carried = due.save_points.len();
                    state
                        .session_save_points
                        .extend(state.pending_save_points.drain(..carried));
                    // Anything marked while the snapshot was in flight
                    // stays pending for the next capture.
                }
            }
        }
    }

    /// A capture failed. Nothing is consumed: the same capture falls
    /// due again on the next tick.
    pub fn failed(&self, due: &Due) {
        let mut roots = self.roots.lock().expect("cadence state poisoned");
        if let Some(state) = roots.get_mut(&due.root_id) {
            state.in_flight = false;
        }
    }

    /// End `root_id`'s session out of band — what an explicit
    /// "checkpoint now" does, since it certifies the same live tree the
    /// quiescence checkpoint would have. Returns the session's save
    /// points so the caller can record them on the checkpoint it just
    /// wrote.
    pub fn end_session(&self, root_id: Uuid) -> Vec<SavePoint> {
        let mut roots = self.roots.lock().expect("cadence state poisoned");
        roots
            .remove(&root_id)
            .map(|state| state.all_save_points())
            .unwrap_or_default()
    }

    /// Is a session open on `root_id`?
    #[must_use]
    pub fn session_open(&self, root_id: Uuid) -> bool {
        self.roots
            .lock()
            .expect("cadence state poisoned")
            .contains_key(&root_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cadence::clock::TestClock;

    fn engine() -> (Arc<TestClock>, CadenceEngine) {
        let clock = Arc::new(TestClock::default());
        (
            clock.clone(),
            CadenceEngine::new(CadenceConfig::default(), clock),
        )
    }

    fn write(engine: &CadenceEngine, root: Uuid, path: &str) {
        let n = engine.note_activity(
            root,
            &[path.to_string()],
            &IgnoreSet::seed(RootFlavor::Media),
            RootFlavor::Media,
        );
        assert_eq!(n, 1, "{path} should have survived the ignore set");
    }

    /// The acceptance criterion, at the state-machine level: writes
    /// every few minutes yield snapshots, and quiescence yields exactly
    /// one checkpoint.
    #[test]
    fn a_recording_storm_snapshots_then_checkpoints_once() {
        let (clock, engine) = engine();
        let root = Uuid::new_v4();

        let mut snapshots = 0;
        let mut checkpoints = 0;
        // A tracking day: a take lands every 3 minutes for 45 minutes,
        // then everyone goes home. The driver ticks every minute
        // throughout — 150 ticks, one cadence decision each.
        for minute in 0..150 {
            if minute <= 42 && minute % 3 == 0 {
                write(&engine, root, "Audio Files/take.wav");
            }
            for due in engine.take_due() {
                match due.kind {
                    DueKind::Snapshot => snapshots += 1,
                    DueKind::Checkpoint => checkpoints += 1,
                }
                engine.completed(&due);
            }
            clock.advance_minutes(1);
        }

        assert_eq!(
            snapshots, 5,
            "one snapshot per 10-minute window of uncaptured activity"
        );
        assert_eq!(
            checkpoints, 1,
            "exactly one Session checkpoint, at quiescence — not one per tick"
        );
        assert!(!engine.session_open(root), "the session closed with it");
    }

    #[test]
    fn ignored_paths_never_open_a_session() {
        let (clock, engine) = engine();
        let root = Uuid::new_v4();
        let ignore = IgnoreSet::seed(RootFlavor::Media);

        let accepted = engine.note_activity(
            root,
            &["El Artisa.rpp-bak".into(), "Audio/kick.reapeaks".into()],
            &ignore,
            RootFlavor::Media,
        );
        assert_eq!(accepted, 0);
        assert!(!engine.session_open(root), "backup churn is not a session");
        clock.advance_minutes(60);
        assert!(engine.take_due().is_empty());
    }

    #[test]
    fn a_project_file_save_marks_a_save_point_on_the_nearest_capture() {
        let (clock, engine) = engine();
        let root = Uuid::new_v4();

        write(&engine, root, "Audio Files/take.wav");
        write(&engine, root, "El Artisa.rpp");
        clock.advance_minutes(11);

        let due = engine.take_due();
        assert_eq!(due.len(), 1);
        assert_eq!(due[0].kind, DueKind::Snapshot);
        assert_eq!(
            due[0]
                .save_points
                .iter()
                .map(|s| s.path.as_str())
                .collect::<Vec<_>>(),
            ["El Artisa.rpp"],
            "only the project file marks a save point"
        );
        engine.completed(&due[0]);

        // The session's checkpoint still reports it: the save point is
        // session metadata, not snapshot-only.
        write(&engine, root, "Audio Files/take2.wav");
        clock.advance_minutes(31);
        let due = engine.take_due();
        assert_eq!(due[0].kind, DueKind::Checkpoint);
        assert_eq!(due[0].save_points.len(), 1);
    }

    #[test]
    fn a_failed_capture_is_retried_not_lost() {
        let (clock, engine) = engine();
        let root = Uuid::new_v4();
        write(&engine, root, "take.wav");
        clock.advance_minutes(11);

        let due = engine.take_due();
        assert_eq!(due.len(), 1);
        assert!(
            engine.take_due().is_empty(),
            "an in-flight capture is not handed out twice"
        );
        engine.failed(&due[0]);
        assert_eq!(
            engine.take_due().len(),
            1,
            "a failed capture falls due again"
        );
    }
}
