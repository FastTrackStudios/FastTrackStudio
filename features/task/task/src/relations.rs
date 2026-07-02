//! Task-graph relations — merged edge view + reverse index.
//!
//! Two encodings coexist in the vault (and on the wire):
//!
//! - **Typed** — `workflow.relations: [{kind, target}]`, read as
//!   *source `<kind>`s target* (see [`RelationKind`]).
//! - **Legacy** — `workflow.blockers` ("each listed task blocks
//!   *me*", i.e. `b → blocks → me`) and `workflow.relates_to`
//!   (`me → relates → entry`).
//!
//! Everything in this module operates on the *merged* edge set
//! ([`edges`]) so callers never care which encoding a page
//! used. The reverse index answers "what blocks / duplicates /
//! implements THIS task" without each caller re-scanning the
//! org list.
//!
//! Pure functions over `&[TaskInfo]` — wasm-clean, no fs. The
//! server-side `TaskBackend` builds the index where the task
//! list already lives and serves it via
//! `TaskService::reverse_relations`.

use std::collections::HashMap;

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::model::{Relation, RelationKind, TaskInfo};

/// One incoming edge: `source` declares `kind` *targeting* the
/// queried task — e.g. `kind: blocks` means "`source` blocks
/// this task". Wire shape of `TaskService::reverse_relations`.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct ReverseRelation {
    pub kind: RelationKind,
    pub source: Uuid,
}

/// The merged, deduplicated edge set over both encodings:
/// `(source, kind, target)` triples where
///
/// - `workflow.relations` on `t` contributes `(t, kind, target)`
///   verbatim;
/// - `workflow.blockers = [b]` on `t` contributes
///   `(b, Blocks, t)` — the legacy list names *incoming*
///   blockers;
/// - `workflow.relates_to = [r]` on `t` contributes
///   `(t, Relates, r)`.
///
/// Self-edges are dropped. Order: stable over the input list.
#[must_use]
pub fn edges(tasks: &[TaskInfo]) -> Vec<(Uuid, RelationKind, Uuid)> {
    let mut out = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let mut push = |s: Uuid, k: RelationKind, t: Uuid| {
        if s != t && seen.insert((s, k, t)) {
            out.push((s, k, t));
        }
    };
    for t in tasks {
        let Some(w) = &t.workflow else { continue };
        for r in w.relations.iter() {
            push(t.id, r.kind, r.target);
        }
        for b in w.blockers.iter() {
            push(*b, RelationKind::Blocks, t.id);
        }
        for r in w.relates_to.iter() {
            push(t.id, RelationKind::Relates, *r);
        }
    }
    out
}

/// Outgoing edges of `id` (merged view): every `(kind, target)`
/// where `id` is the source — `id`'s own typed relations +
/// `relates_to`, plus a `blocks` edge for every *other* task
/// whose legacy `blockers` list names `id`.
#[must_use]
pub fn outgoing(id: Uuid, tasks: &[TaskInfo]) -> Vec<Relation> {
    edges(tasks)
        .into_iter()
        .filter(|(s, _, _)| *s == id)
        .map(|(_, kind, target)| Relation { kind, target })
        .collect()
}

/// Reverse (incoming) edges of `id`: who points at it, with what
/// kind. `kind: Blocks` entries answer "what blocks THIS" —
/// `id`'s own legacy `blockers` list surfaces here too (each
/// entry *is* an incoming blocks edge after normalization).
#[must_use]
pub fn reverse_relations_for(id: Uuid, tasks: &[TaskInfo]) -> Vec<ReverseRelation> {
    edges(tasks)
        .into_iter()
        .filter(|(_, _, t)| *t == id)
        .map(|(source, kind, _)| ReverseRelation { kind, source })
        .collect()
}

/// Full reverse index: target id → incoming edges. Build once
/// per task-list snapshot when answering many lookups (the
/// server does this inside `reverse_relations`).
#[must_use]
pub fn reverse_index(tasks: &[TaskInfo]) -> HashMap<Uuid, Vec<ReverseRelation>> {
    let mut idx: HashMap<Uuid, Vec<ReverseRelation>> = HashMap::new();
    for (source, kind, target) in edges(tasks) {
        idx.entry(target)
            .or_default()
            .push(ReverseRelation { kind, source });
    }
    idx
}

/// Merged blocker set per task: task id → the tasks blocking it
/// (sources of incoming `blocks` edges, both encodings). The
/// rollup engine consumes this for its `blocked` count.
#[must_use]
pub fn blockers_of(tasks: &[TaskInfo]) -> HashMap<Uuid, Vec<Uuid>> {
    let mut idx: HashMap<Uuid, Vec<Uuid>> = HashMap::new();
    for (source, kind, target) in edges(tasks) {
        if kind == RelationKind::Blocks {
            idx.entry(target).or_default().push(source);
        }
    }
    idx
}

/// Sugar for `task issue blocking`: every task `id` blocks —
/// targets of `id`'s outgoing `blocks` edges (typed `blocks`
/// relations on `id` + other tasks listing `id` in their legacy
/// `blockers`).
#[must_use]
pub fn blocking(id: Uuid, tasks: &[TaskInfo]) -> Vec<Uuid> {
    outgoing(id, tasks)
        .into_iter()
        .filter(|r| r.kind == RelationKind::Blocks)
        .map(|r| r.target)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{RelationList, UuidList, WorkflowAttrs};

    fn task(id: Uuid) -> TaskInfo {
        let mut t = crate::parse_str("tasks/x.md", "x", "---\ntype: task\ntitle: x\n---\n")
            .expect("minimal task parses");
        t.id = id;
        t.workflow = Some(WorkflowAttrs::default());
        t
    }

    #[test]
    fn edges_merge_typed_and_legacy_encodings() {
        let (a, b, c, d) = (
            Uuid::new_v4(),
            Uuid::new_v4(),
            Uuid::new_v4(),
            Uuid::new_v4(),
        );
        // a: typed `blocks c`, typed `implements d`.
        let mut ta = task(a);
        ta.workflow.as_mut().unwrap().relations = RelationList(vec![
            Relation {
                kind: RelationKind::Blocks,
                target: c,
            },
            Relation {
                kind: RelationKind::Implements,
                target: d,
            },
        ]);
        // b: legacy blockers [a] (=> a blocks b), relates_to [c].
        let mut tb = task(b);
        tb.workflow.as_mut().unwrap().blockers = UuidList(vec![a]);
        tb.workflow.as_mut().unwrap().relates_to = UuidList(vec![c]);

        let tasks = vec![ta, tb, task(c), task(d)];
        let result = edges(&tasks);
        assert!(result.contains(&(a, RelationKind::Blocks, c)), "typed");
        assert!(result.contains(&(a, RelationKind::Implements, d)), "typed");
        assert!(
            result.contains(&(a, RelationKind::Blocks, b)),
            "legacy blockers"
        );
        assert!(
            result.contains(&(b, RelationKind::Relates, c)),
            "legacy relates"
        );
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn duplicate_edges_across_encodings_dedupe() {
        let (a, b) = (Uuid::new_v4(), Uuid::new_v4());
        // a typed-blocks b AND b lists a as a legacy blocker —
        // the same edge twice.
        let mut ta = task(a);
        ta.workflow.as_mut().unwrap().relations = RelationList(vec![Relation {
            kind: RelationKind::Blocks,
            target: b,
        }]);
        let mut tb = task(b);
        tb.workflow.as_mut().unwrap().blockers = UuidList(vec![a]);

        let tasks = vec![ta, tb];
        assert_eq!(edges(&tasks), vec![(a, RelationKind::Blocks, b)]);
        assert_eq!(blocking(a, &tasks), vec![b]);
        assert_eq!(
            reverse_relations_for(b, &tasks),
            vec![ReverseRelation {
                kind: RelationKind::Blocks,
                source: a
            }]
        );
    }

    #[test]
    fn reverse_index_answers_what_points_at_this() {
        let (a, b, c) = (Uuid::new_v4(), Uuid::new_v4(), Uuid::new_v4());
        // a duplicates c; b implements c; c blocked by a (legacy).
        let mut ta = task(a);
        ta.workflow.as_mut().unwrap().relations = RelationList(vec![Relation {
            kind: RelationKind::Duplicate,
            target: c,
        }]);
        let mut tb = task(b);
        tb.workflow.as_mut().unwrap().relations = RelationList(vec![Relation {
            kind: RelationKind::Implements,
            target: c,
        }]);
        let mut tc = task(c);
        tc.workflow.as_mut().unwrap().blockers = UuidList(vec![a]);

        let tasks = vec![ta, tb, tc];
        let idx = reverse_index(&tasks);
        let incoming = &idx[&c];
        assert_eq!(incoming.len(), 3);
        assert!(incoming.contains(&ReverseRelation {
            kind: RelationKind::Duplicate,
            source: a
        }));
        assert!(incoming.contains(&ReverseRelation {
            kind: RelationKind::Implements,
            source: b
        }));
        assert!(
            incoming.contains(&ReverseRelation {
                kind: RelationKind::Blocks,
                source: a
            }),
            "own legacy blockers list = incoming blocks edges"
        );
        // blockers_of merges both encodings.
        assert_eq!(blockers_of(&tasks)[&c], vec![a]);
    }

    #[test]
    fn self_edges_are_dropped() {
        let a = Uuid::new_v4();
        let mut ta = task(a);
        ta.workflow.as_mut().unwrap().relations = RelationList(vec![Relation {
            kind: RelationKind::Relates,
            target: a,
        }]);
        ta.workflow.as_mut().unwrap().blockers = UuidList(vec![a]);
        assert!(edges(&[ta]).is_empty());
    }
}

/// Arrange a flat row list into parent→children display order:
/// roots keep their incoming order, each root is immediately
/// followed by its subtasks (`workflow.parent`, also in incoming
/// order), and the returned depth is `0` for roots / `1` for
/// subtasks. A child whose parent isn't in the list renders as a
/// root — filtered views (Relevant, status) must not orphan rows
/// invisibly.
///
/// Generic over the row type so the CLI (domain `TaskInfo`) and
/// task-ui (its view model) arrange identically — one behavior,
/// N renderers.
pub fn arrange_families<T>(
    rows: Vec<T>,
    id_of: impl Fn(&T) -> uuid::Uuid,
    parent_of: impl Fn(&T) -> Option<uuid::Uuid>,
) -> Vec<(u8, T)> {
    let present: std::collections::HashSet<uuid::Uuid> = rows.iter().map(&id_of).collect();
    let mut children: std::collections::HashMap<uuid::Uuid, Vec<T>> =
        std::collections::HashMap::new();
    let mut roots: Vec<T> = Vec::new();
    for row in rows {
        match parent_of(&row).filter(|p| present.contains(p)) {
            Some(p) => children.entry(p).or_default().push(row),
            None => roots.push(row),
        }
    }
    let mut out = Vec::with_capacity(present.len());
    for root in roots {
        let rid = id_of(&root);
        out.push((0, root));
        if let Some(kids) = children.remove(&rid) {
            out.extend(kids.into_iter().map(|k| (1, k)));
        }
    }
    out
}

#[cfg(test)]
mod family_tests {
    use super::arrange_families;
    use uuid::Uuid;

    #[test]
    fn children_follow_their_parent_and_orphans_stay_roots() {
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        let missing = Uuid::new_v4();
        // (id, parent, label)
        let rows = vec![
            (Uuid::new_v4(), Some(a), "a-kid-1"),
            (b, None, "b"),
            (a, None, "a"),
            (Uuid::new_v4(), Some(missing), "orphan"),
            (Uuid::new_v4(), Some(a), "a-kid-2"),
        ];
        let out = arrange_families(rows, |r| r.0, |r| r.1);
        let view: Vec<(u8, &str)> = out.iter().map(|(d, r)| (*d, r.2)).collect();
        assert_eq!(
            view,
            vec![
                (0, "b"),
                (0, "a"),
                (1, "a-kid-1"),
                (1, "a-kid-2"),
                (0, "orphan"),
            ]
        );
    }
}
