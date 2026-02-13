//! Snapshot automation types -- time-stamped events for a snapshot timeline.
//!
//! `SnapshotAutomation` holds a lane of `AutomationEvent`s, each carrying an
//! `AutomationAction` at a specific timestamp. During playback the engine walks
//! the sorted event list and fires actions as the transport reaches each
//! timestamp.
//!
//! # Event Types
//!
//! - **ApplySnapshot**: Instantly recall a snapshot at a point in time.
//! - **StartMorph**: Begin a timed morph between two snapshots (A -> B over a
//!   given duration).
//! - **SetMorphPosition**: Jump the morph slider to an explicit position
//!   (useful for automation curves drawn by hand).
//!
//! # Example
//!
//! ```
//! use signal_proto::automation::*;
//! use uuid::Uuid;
//!
//! let mut lane = SnapshotAutomation::new("Song Intro");
//! let snap_a = Uuid::new_v4();
//! let snap_b = Uuid::new_v4();
//!
//! lane.add_event(0, AutomationAction::ApplySnapshot { snapshot_id: snap_a });
//! lane.add_event(4000, AutomationAction::StartMorph {
//!     from_snapshot: snap_a,
//!     to_snapshot: snap_b,
//!     duration_ms: 2000,
//! });
//! lane.add_event(8000, AutomationAction::ApplySnapshot { snapshot_id: snap_b });
//!
//! assert_eq!(lane.events.len(), 3);
//! assert!(lane.events_in_range(3000, 5000).len() == 1);
//! ```

use uuid::Uuid;

// ---------------------------------------------------------------------------
// AutomationAction
// ---------------------------------------------------------------------------

/// The action payload carried by an automation event.
#[derive(Debug, Clone, PartialEq)]
pub enum AutomationAction {
    /// Instantly apply a snapshot at this point on the timeline.
    ApplySnapshot { snapshot_id: Uuid },

    /// Begin a timed morph between two snapshots.
    ///
    /// `duration_ms` is relative to the event's timestamp -- the morph
    /// completes at `timestamp_ms + duration_ms`.
    StartMorph {
        from_snapshot: Uuid,
        to_snapshot: Uuid,
        duration_ms: u64,
    },

    /// Set the morph slider to an explicit position in `[0.0, 1.0]`.
    ///
    /// Useful for hand-drawn automation curves or envelope followers.
    SetMorphPosition { position: f64 },
}

// ---------------------------------------------------------------------------
// AutomationEvent
// ---------------------------------------------------------------------------

/// A single time-stamped automation event.
#[derive(Debug, Clone, PartialEq)]
pub struct AutomationEvent {
    /// Unique ID for this event (used for selection, deletion, drag).
    pub id: Uuid,
    /// Absolute timestamp in milliseconds from the start of the timeline.
    pub timestamp_ms: u64,
    /// The action to perform at this timestamp.
    pub action: AutomationAction,
}

impl AutomationEvent {
    /// Create a new event with a fresh UUID.
    pub fn new(timestamp_ms: u64, action: AutomationAction) -> Self {
        Self {
            id: Uuid::new_v4(),
            timestamp_ms,
            action,
        }
    }

    /// Human-readable label for the event marker.
    pub fn label(&self) -> String {
        match &self.action {
            AutomationAction::ApplySnapshot { .. } => "Snap".to_string(),
            AutomationAction::StartMorph { duration_ms, .. } => {
                format!("Morph {duration_ms}ms")
            }
            AutomationAction::SetMorphPosition { position } => {
                format!("Pos {:.0}%", position * 100.0)
            }
        }
    }

    /// CSS-style color hint for rendering the event marker.
    pub fn color_hint(&self) -> &'static str {
        match &self.action {
            AutomationAction::ApplySnapshot { .. } => "#3b82f6", // blue-500
            AutomationAction::StartMorph { .. } => "#f97316",    // orange-500
            AutomationAction::SetMorphPosition { .. } => "#a855f7", // purple-500
        }
    }
}

// ---------------------------------------------------------------------------
// SnapshotAutomation
// ---------------------------------------------------------------------------

/// A complete automation lane holding time-stamped snapshot events.
///
/// Events are kept sorted by `timestamp_ms` after every mutation so that
/// playback can do a simple linear scan.
#[derive(Debug, Clone, PartialEq)]
pub struct SnapshotAutomation {
    /// Unique lane ID.
    pub id: Uuid,
    /// User-visible name for this automation lane.
    pub name: String,
    /// Sorted event list (ascending by `timestamp_ms`).
    pub events: Vec<AutomationEvent>,
    /// Whether the lane is currently recording new events.
    pub is_recording: bool,
    /// Current playback head position in milliseconds.
    pub playback_position_ms: u64,
}

impl SnapshotAutomation {
    /// Create a new empty automation lane.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            events: Vec::new(),
            is_recording: false,
            playback_position_ms: 0,
        }
    }

    /// Add an event and keep the list sorted.
    pub fn add_event(&mut self, timestamp_ms: u64, action: AutomationAction) -> Uuid {
        let event = AutomationEvent::new(timestamp_ms, action);
        let id = event.id;
        self.events.push(event);
        self.events.sort_by_key(|e| e.timestamp_ms);
        id
    }

    /// Remove an event by its ID.
    pub fn remove_event(&mut self, event_id: Uuid) -> bool {
        let before = self.events.len();
        self.events.retain(|e| e.id != event_id);
        self.events.len() < before
    }

    /// Move an event to a new timestamp, re-sorting the list.
    pub fn move_event(&mut self, event_id: Uuid, new_timestamp_ms: u64) -> bool {
        if let Some(event) = self.events.iter_mut().find(|e| e.id == event_id) {
            event.timestamp_ms = new_timestamp_ms;
            self.events.sort_by_key(|e| e.timestamp_ms);
            true
        } else {
            false
        }
    }

    /// Return events whose timestamp falls within `[start_ms, end_ms)`.
    pub fn events_in_range(&self, start_ms: u64, end_ms: u64) -> Vec<&AutomationEvent> {
        self.events
            .iter()
            .filter(|e| e.timestamp_ms >= start_ms && e.timestamp_ms < end_ms)
            .collect()
    }

    /// Total duration of the lane (timestamp of the last event, or 0).
    pub fn duration_ms(&self) -> u64 {
        self.events
            .last()
            .map(|e| {
                // If the last event is a morph, extend by its duration
                if let AutomationAction::StartMorph { duration_ms, .. } = &e.action {
                    e.timestamp_ms + duration_ms
                } else {
                    e.timestamp_ms
                }
            })
            .unwrap_or(0)
    }

    /// Reset the playback head to the beginning.
    pub fn rewind(&mut self) {
        self.playback_position_ms = 0;
    }

    /// Clear all events.
    pub fn clear(&mut self) {
        self.events.clear();
        self.playback_position_ms = 0;
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn snap_id() -> Uuid {
        Uuid::new_v4()
    }

    #[test]
    fn new_lane_is_empty() {
        let lane = SnapshotAutomation::new("Test");
        assert_eq!(lane.name, "Test");
        assert!(lane.events.is_empty());
        assert!(!lane.is_recording);
        assert_eq!(lane.playback_position_ms, 0);
        assert_eq!(lane.duration_ms(), 0);
    }

    #[test]
    fn add_events_sorted() {
        let mut lane = SnapshotAutomation::new("Test");
        let a = snap_id();
        let b = snap_id();

        // Insert out of order
        lane.add_event(5000, AutomationAction::ApplySnapshot { snapshot_id: b });
        lane.add_event(1000, AutomationAction::ApplySnapshot { snapshot_id: a });

        assert_eq!(lane.events.len(), 2);
        assert_eq!(lane.events[0].timestamp_ms, 1000);
        assert_eq!(lane.events[1].timestamp_ms, 5000);
    }

    #[test]
    fn remove_event() {
        let mut lane = SnapshotAutomation::new("Test");
        let id = lane.add_event(
            0,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        assert_eq!(lane.events.len(), 1);

        assert!(lane.remove_event(id));
        assert!(lane.events.is_empty());

        // Removing non-existent returns false
        assert!(!lane.remove_event(Uuid::new_v4()));
    }

    #[test]
    fn move_event_resorts() {
        let mut lane = SnapshotAutomation::new("Test");
        let id1 = lane.add_event(
            1000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        let _id2 = lane.add_event(
            2000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );

        // Move first event past the second
        assert!(lane.move_event(id1, 3000));
        assert_eq!(lane.events[0].timestamp_ms, 2000);
        assert_eq!(lane.events[1].timestamp_ms, 3000);
    }

    #[test]
    fn events_in_range() {
        let mut lane = SnapshotAutomation::new("Test");
        lane.add_event(
            1000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        lane.add_event(
            2000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        lane.add_event(
            3000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        lane.add_event(
            4000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );

        let in_range = lane.events_in_range(2000, 4000);
        assert_eq!(in_range.len(), 2);
        assert_eq!(in_range[0].timestamp_ms, 2000);
        assert_eq!(in_range[1].timestamp_ms, 3000);
    }

    #[test]
    fn duration_extends_for_morph() {
        let mut lane = SnapshotAutomation::new("Test");
        let a = snap_id();
        let b = snap_id();

        lane.add_event(
            5000,
            AutomationAction::StartMorph {
                from_snapshot: a,
                to_snapshot: b,
                duration_ms: 3000,
            },
        );

        // Duration = 5000 + 3000 = 8000
        assert_eq!(lane.duration_ms(), 8000);
    }

    #[test]
    fn event_labels_and_colors() {
        let apply = AutomationEvent::new(
            0,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        assert_eq!(apply.label(), "Snap");
        assert_eq!(apply.color_hint(), "#3b82f6");

        let morph = AutomationEvent::new(
            0,
            AutomationAction::StartMorph {
                from_snapshot: snap_id(),
                to_snapshot: snap_id(),
                duration_ms: 2000,
            },
        );
        assert_eq!(morph.label(), "Morph 2000ms");
        assert_eq!(morph.color_hint(), "#f97316");

        let pos = AutomationEvent::new(0, AutomationAction::SetMorphPosition { position: 0.75 });
        assert_eq!(pos.label(), "Pos 75%");
        assert_eq!(pos.color_hint(), "#a855f7");
    }

    #[test]
    fn rewind_and_clear() {
        let mut lane = SnapshotAutomation::new("Test");
        lane.add_event(
            1000,
            AutomationAction::ApplySnapshot {
                snapshot_id: snap_id(),
            },
        );
        lane.playback_position_ms = 5000;

        lane.rewind();
        assert_eq!(lane.playback_position_ms, 0);
        assert_eq!(lane.events.len(), 1);

        lane.clear();
        assert!(lane.events.is_empty());
        assert_eq!(lane.playback_position_ms, 0);
    }
}
