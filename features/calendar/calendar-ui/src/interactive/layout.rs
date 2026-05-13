//! Pure layout helpers for the interactive calendar.
//!
//! Two responsibilities:
//! 1. Convert a `DateTime<Utc>` start/end pair into pixel offsets inside a
//!    24h time-grid column (slot-height parametrised).
//! 2. Assign overlap "lanes" to a per-day slice of events so that visually
//!    overlapping events sit side-by-side instead of on top of each other.

use calendar_proto::CalendarEvent;
use chrono::{DateTime, Local, NaiveDate, TimeZone, Timelike, Utc};

/// Inclusive lane index + lane count for one event in a day column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LaneSlot {
    pub lane: usize,
    pub lanes: usize,
}

/// Slot height in pixels for the two density modes.
pub fn slot_px(compact: bool) -> f32 {
    if compact { 32.0 } else { 48.0 }
}

/// Total grid height for a 24h day at the given density (48 half-hour slots).
pub fn day_height_px(compact: bool) -> f32 {
    slot_px(compact) * 48.0
}

/// Convert a UTC datetime to (top_px, height_px) inside the day grid, clamped
/// to `[0, 24h)`. Anything outside the local day window is clipped.
pub fn event_offsets(
    day_local: NaiveDate,
    start: DateTime<Utc>,
    end: DateTime<Utc>,
    compact: bool,
) -> Option<(f32, f32)> {
    let slot = slot_px(compact);
    let day_start_local = Local
        .from_local_datetime(&day_local.and_hms_opt(0, 0, 0)?)
        .single()?;
    let day_end_local = day_start_local + chrono::Duration::days(1);
    let start_l = start.with_timezone(&Local);
    let end_l = end.with_timezone(&Local);
    if end_l <= day_start_local || start_l >= day_end_local {
        return None;
    }
    let visible_start = start_l.max(day_start_local);
    let visible_end = end_l.min(day_end_local);
    let mins_from_top = (visible_start - day_start_local).num_minutes().max(0) as f32;
    let mins_dur = (visible_end - visible_start).num_minutes().max(15) as f32;
    let top = (mins_from_top / 30.0) * slot;
    let height = (mins_dur / 30.0) * slot;
    Some((top, height))
}

/// Greedy lane assignment for a sorted (by start) day-slice of events.
///
/// Algorithm:
///  - Maintain a vec of lane end-times.
///  - For each event, place it in the first lane whose current end <= start.
///  - If none fits, append a new lane.
///  - Then back-fill the `lanes` total once everything is placed.
///
/// Returns slots in the same order as the input events.
pub fn assign_lanes(events: &[CalendarEvent]) -> Vec<LaneSlot> {
    let mut sorted: Vec<(usize, &CalendarEvent)> = events.iter().enumerate().collect();
    sorted.sort_by_key(|(_, e)| e.start_at);

    let mut lane_ends: Vec<DateTime<Utc>> = Vec::new();
    let mut placement: Vec<(usize, usize)> = Vec::with_capacity(events.len()); // (orig_idx, lane)

    for (orig_idx, ev) in sorted {
        let mut chosen: Option<usize> = None;
        for (i, end) in lane_ends.iter().enumerate() {
            if *end <= ev.start_at {
                chosen = Some(i);
                break;
            }
        }
        let lane = chosen.unwrap_or_else(|| {
            lane_ends.push(ev.end_at);
            lane_ends.len() - 1
        });
        lane_ends[lane] = ev.end_at;
        placement.push((orig_idx, lane));
    }

    let total = lane_ends.len().max(1);
    let mut out = vec![
        LaneSlot {
            lane: 0,
            lanes: total
        };
        events.len()
    ];
    for (orig_idx, lane) in placement {
        out[orig_idx] = LaneSlot { lane, lanes: total };
    }
    out
}

/// "now" line offset in pixels for a given day (returns None if `day_local`
/// isn't today in the local timezone).
pub fn now_offset_px(day_local: NaiveDate, compact: bool) -> Option<f32> {
    let now = Local::now();
    if now.date_naive() != day_local {
        return None;
    }
    let mins = (now.hour() * 60 + now.minute()) as f32;
    Some((mins / 30.0) * slot_px(compact))
}

/// Format a clock label (00:30 .. 23:30) for the hour gutter.
pub fn hour_label(hour: u32) -> String {
    format!("{:02}:00", hour)
}
