//! Derived workstream progress — the rollup engine.
//!
//! Pure function over the org's task list: nothing is stored,
//! the numbers are recomputed from `task::TaskInfo` rows on
//! every call. A task belongs to a workstream when
//! `task.workflow.workstream == Some(id)`.
//!
//! Semantics:
//! - `done` counts status `done` only (cancelled tasks stay in
//!   `total` but aren't "progress").
//! - `blocked` counts still-open attached tasks with at least
//!   one *unresolved* blocker. A blocker is resolved only when
//!   the referenced task exists in the provided list and is
//!   done / cancelled — the same rule `task issue ready` uses,
//!   so a workstream's blocked count matches what agents see.
//! - `estimate_points_sum` weights XS/S/M/L/XL as 1/2/3/5/8;
//!   `Points { value }` counts at face value; no estimate = 0.

use std::collections::HashMap;

use task::TaskInfo;
use task::model::Estimate;
use uuid::Uuid;
use workstream_proto::WorkstreamRollup;

/// Bucket weight for an estimate: XS/S/M/L/XL → 1/2/3/5/8,
/// `Points` at face value.
#[must_use]
pub fn estimate_points(e: Estimate) -> u32 {
    match e {
        Estimate::XS => 1,
        Estimate::S => 2,
        Estimate::M => 3,
        Estimate::L => 5,
        Estimate::XL => 8,
        Estimate::Points { value } => u32::from(value),
    }
}

/// Derive the progress rollup for one workstream from the org's
/// task list. `org_tasks` should be the *full* list (not
/// pre-filtered) so blocker references resolve across
/// workstream boundaries.
#[must_use]
pub fn rollup(workstream_id: Uuid, org_tasks: &[TaskInfo]) -> WorkstreamRollup {
    let by_id: HashMap<Uuid, &TaskInfo> = org_tasks.iter().map(|t| (t.id, t)).collect();
    let is_closed = |t: &TaskInfo| {
        matches!(
            task::Status::from_str(&t.status),
            Some(task::Status::Done | task::Status::Cancelled)
        )
    };

    let mut out = WorkstreamRollup::default();
    for t in org_tasks {
        let attached = t
            .workflow
            .as_ref()
            .and_then(|w| w.workstream)
            .is_some_and(|ws| ws == workstream_id);
        if !attached {
            continue;
        }
        out.total += 1;
        match task::Status::from_str(&t.status) {
            Some(task::Status::Done) => out.done += 1,
            Some(task::Status::InProgress) => out.in_progress += 1,
            _ => {}
        }
        if let Some(w) = &t.workflow {
            out.estimate_points_sum += w.estimate.map_or(0, estimate_points);
            // Blocked: still open, with >= 1 unresolved blocker.
            // Unresolved = the referenced task is missing from
            // the org list OR not yet done / cancelled.
            if !is_closed(t)
                && w.blockers
                    .iter()
                    .any(|bid| !by_id.get(bid).copied().is_some_and(is_closed))
            {
                out.blocked += 1;
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use task::model::{UuidList, WorkflowAttrs};

    fn ws_task(ws: Option<Uuid>, status: &str) -> TaskInfo {
        let mut t = task::parse_str("tasks/x.md", "x", "---\ntype: task\ntitle: x\n---\n")
            .expect("minimal task parses");
        t.id = Uuid::new_v4();
        t.status = status.into();
        if let Some(ws) = ws {
            t.workflow = Some(WorkflowAttrs {
                workstream: Some(ws),
                ..Default::default()
            });
        }
        t
    }

    fn with_estimate(mut t: TaskInfo, e: Estimate) -> TaskInfo {
        t.workflow
            .get_or_insert_with(WorkflowAttrs::default)
            .estimate = Some(e);
        t
    }

    #[test]
    fn rollup_counts_statuses_and_ignores_other_workstreams() {
        let ws = Uuid::new_v4();
        let other = Uuid::new_v4();
        let tasks = vec![
            ws_task(Some(ws), "done"),
            ws_task(Some(ws), "in-progress"),
            ws_task(Some(ws), "open"),
            ws_task(Some(other), "done"),
            ws_task(None, "open"),
        ];
        let r = rollup(ws, &tasks);
        assert_eq!(r.total, 3);
        assert_eq!(r.done, 1);
        assert_eq!(r.in_progress, 1);
        assert_eq!(r.blocked, 0);
    }

    #[test]
    fn rollup_blocked_requires_unresolved_blocker() {
        let ws = Uuid::new_v4();
        let done_blocker = ws_task(None, "done");
        let open_blocker = ws_task(None, "open");

        // Blocked: open task, blocker still open.
        let mut blocked = ws_task(Some(ws), "open");
        blocked.workflow.as_mut().unwrap().blockers = UuidList(vec![open_blocker.id]);
        // Not blocked: every blocker closed.
        let mut clear = ws_task(Some(ws), "open");
        clear.workflow.as_mut().unwrap().blockers = UuidList(vec![done_blocker.id]);
        // Blocked: dangling blocker reference counts as unresolved.
        let mut dangling = ws_task(Some(ws), "open");
        dangling.workflow.as_mut().unwrap().blockers = UuidList(vec![Uuid::new_v4()]);
        // Not blocked: the task itself is already done — a closed
        // task can't be "blocked" no matter its blocker list.
        let mut done = ws_task(Some(ws), "done");
        done.workflow.as_mut().unwrap().blockers = UuidList(vec![open_blocker.id]);

        let tasks = vec![done_blocker, open_blocker, blocked, clear, dangling, done];
        let r = rollup(ws, &tasks);
        assert_eq!(r.total, 4);
        assert_eq!(r.blocked, 2, "open-blocker + dangling-blocker tasks");
    }

    #[test]
    fn rollup_estimate_points_sum_uses_bucket_weights() {
        let ws = Uuid::new_v4();
        let tasks = vec![
            with_estimate(ws_task(Some(ws), "open"), Estimate::XS), // 1
            with_estimate(ws_task(Some(ws), "open"), Estimate::S),  // 2
            with_estimate(ws_task(Some(ws), "open"), Estimate::M),  // 3
            with_estimate(ws_task(Some(ws), "open"), Estimate::L),  // 5
            with_estimate(ws_task(Some(ws), "done"), Estimate::XL), // 8
            with_estimate(ws_task(Some(ws), "open"), Estimate::Points { value: 13 }),
            ws_task(Some(ws), "open"), // no estimate → 0
        ];
        let r = rollup(ws, &tasks);
        assert_eq!(r.estimate_points_sum, 1 + 2 + 3 + 5 + 8 + 13);
        assert_eq!(r.total, 7);
    }

    #[test]
    fn rollup_empty_workstream_is_all_zero() {
        let r = rollup(Uuid::new_v4(), &[]);
        assert_eq!(r, WorkstreamRollup::default());
    }
}
