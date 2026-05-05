//! CRDT convergence tests (Loro).
//!
//! Verifies that concurrent edits from multiple users converge to the same
//! state regardless of operation order, and that concurrent writes to the
//! same field surface as detected conflicts.

#[cfg(feature = "realtime")]
mod crdt_tests {
    use task_core::crdt::{CONFLICT_FIELDS, CrdtDocument};
    use task_core::task::{Priority, Status, Task};

    fn base_task() -> Task {
        Task {
            id: Some("shared".into()),
            title: "Shared task".into(),
            status: Status::Open,
            priority: Priority::Normal,
            body: "## Subtasks\n- [ ] One\n- [ ] Two\n- [ ] Three\n- [ ] Four\n- [ ] Five".into(),
            ..Default::default()
        }
    }

    /// Broadcast every peer's updates to every other peer.
    fn all_to_all_sync(docs: &[CrdtDocument]) {
        for i in 0..docs.len() {
            for j in 0..docs.len() {
                if i == j {
                    continue;
                }
                let update = docs[j]
                    .export_updates_since(&docs[i].version_vector())
                    .unwrap();
                docs[i].import(&update).unwrap();
            }
        }
    }

    /// Five users write to five distinct fields — all peers converge.
    #[test]
    fn five_users_edit_different_fields() {
        let base = CrdtDocument::from_task(&base_task(), "t.md");
        let snapshot = base.export_snapshot().unwrap();

        let docs: Vec<CrdtDocument> = (0..5)
            .map(|i| {
                let d = CrdtDocument::from_snapshot(&snapshot, "t.md").unwrap();
                d.set_peer_id((i + 1) as u64).unwrap();
                d
            })
            .collect();

        docs[0].set_field("status", "InProgress");
        docs[1].set_field("assignee", "cody");
        docs[2].set_field("priority", "High");
        docs[3].set_field("due", "2026-05-01");
        docs[4].set_field("recurrence", "FREQ=WEEKLY");
        for d in &docs {
            d.commit();
        }

        all_to_all_sync(&docs);

        for d in &docs {
            let t = d.to_task();
            assert_eq!(t.status, Status::InProgress, "peer {}", d.peer_id());
            assert_eq!(t.assignee.as_deref(), Some("cody"));
            assert_eq!(t.priority, Priority::High);
            assert_eq!(t.due, chrono::NaiveDate::from_ymd_opt(2026, 5, 1));
            assert_eq!(t.recurrence.as_deref(), Some("FREQ=WEEKLY"));
        }
    }

    /// Same-field concurrent edit: LWW picks one winner; the loser is
    /// recoverable via the conflict detector.
    #[test]
    fn same_field_concurrent_edit_detected() {
        use task_core::crdt::sync::detect_field_conflicts;

        let base = CrdtDocument::from_task(&base_task(), "t.md");
        let snap = base.export_snapshot().unwrap();

        let a = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        a.set_peer_id(1).unwrap();
        let b = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        b.set_peer_id(2).unwrap();

        a.set_field("status", "Done");
        a.commit();
        b.set_field("status", "Cancelled");
        b.commit();

        let before_a = a.snapshot_fields();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_update).unwrap();

        let conflicts = detect_field_conflicts(&a, &before_a, 1);
        let status = conflicts.iter().find(|c| c.field == "status");
        assert!(
            status.is_some(),
            "expected status conflict, got {conflicts:?}"
        );
        assert_eq!(status.unwrap().losing_value.as_deref(), Some("Done"));
    }

    /// Concurrent subtask checks in the body text both survive the merge.
    #[test]
    fn concurrent_subtask_checks_converge() {
        let base = CrdtDocument::from_task(&base_task(), "t.md");
        let snap = base.export_snapshot().unwrap();

        let a = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        a.set_peer_id(1).unwrap();
        let b = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        b.set_peer_id(2).unwrap();

        let body_a = a.get_body().replace("- [ ] One", "- [x] One");
        a.set_body(&body_a);
        a.commit();

        let body_b = b.get_body().replace("- [ ] Three", "- [x] Three");
        b.set_body(&body_b);
        b.commit();

        let a_update = a.export_updates_since(&b.version_vector()).unwrap();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        b.import(&a_update).unwrap();
        a.import(&b_update).unwrap();

        for d in [&a, &b] {
            let body = d.get_body();
            assert!(
                body.contains("[x] One"),
                "peer {} body: {body}",
                d.peer_id()
            );
            assert!(body.contains("[x] Three"));
            assert!(body.contains("[ ] Two"));
        }
    }

    /// Merge order independence — the CRDT guarantee.
    #[test]
    fn merge_order_independent() {
        let base = CrdtDocument::from_task(&base_task(), "t.md");
        let snap = base.export_snapshot().unwrap();

        let a = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        a.set_peer_id(1).unwrap();
        let b = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        b.set_peer_id(2).unwrap();
        let c = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        c.set_peer_id(3).unwrap();

        a.set_field("status", "InProgress");
        b.set_field("assignee", "amy");
        c.set_field("priority", "Urgent");
        a.commit();
        b.commit();
        c.commit();

        let ua = a.export_updates_since(&loro::VersionVector::new()).unwrap();
        let ub = b.export_updates_since(&loro::VersionVector::new()).unwrap();
        let uc = c.export_updates_since(&loro::VersionVector::new()).unwrap();

        let r1 = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        r1.import(&ua).unwrap();
        r1.import(&ub).unwrap();
        r1.import(&uc).unwrap();

        let r2 = CrdtDocument::from_snapshot(&snap, "t.md").unwrap();
        r2.import(&uc).unwrap();
        r2.import(&ua).unwrap();
        r2.import(&ub).unwrap();

        let t1 = r1.to_task();
        let t2 = r2.to_task();
        assert_eq!(t1.status, t2.status);
        assert_eq!(t1.assignee, t2.assignee);
        assert_eq!(t1.priority, t2.priority);
        assert_eq!(t1.status, Status::InProgress);
        assert_eq!(t1.assignee.as_deref(), Some("amy"));
        assert_eq!(t1.priority, Priority::Urgent);
    }

    /// Every CONFLICT_FIELDS entry is reachable via set_field / get_field.
    #[test]
    fn all_conflict_fields_reachable() {
        let doc = CrdtDocument::from_task(&base_task(), "t.md");
        doc.set_peer_id(1).unwrap();
        for f in CONFLICT_FIELDS {
            doc.set_field(f, "probe");
        }
        doc.commit();
        for f in CONFLICT_FIELDS {
            assert_eq!(doc.get_field(f).as_deref(), Some("probe"));
        }
    }
}
