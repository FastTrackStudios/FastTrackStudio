//! CRDT convergence tests.
//!
//! Verifies that concurrent edits from multiple users converge
//! to the same state regardless of operation order.

#[cfg(feature = "realtime")]
mod crdt_tests {
    use task_core::crdt::metadata::MetadataDoc;
    use task_core::crdt::body::BodyDoc;
    use task_core::crdt::document::CrdtDocument;
    use task_core::task::{Task, Status, Priority};

    // ── Metadata: 5 users editing different fields concurrently ───

    #[test]
    fn five_users_edit_different_fields() {
        let task = Task {
            title: "Shared task".into(),
            status: Status::Open,
            priority: Priority::Normal,
            ..Default::default()
        };

        // Create initial document and save
        let mut doc_0 = MetadataDoc::new();
        doc_0.from_task(&task);
        let initial = doc_0.save();

        // 5 users fork from the same state
        let mut docs: Vec<MetadataDoc> = (0..5)
            .map(|_| MetadataDoc::load(&initial).unwrap())
            .collect();

        // Each user edits a different field
        docs[0].set_field("status", "InProgress");
        docs[1].set_field("assignee", "alice");
        docs[2].set_field("priority", "High");
        docs[3].set_field("title", "Updated task title");
        docs[4].set_field("external_source", "deck");

        // Merge all into doc_0 — save each fork, load into doc_0
        for i in 1..5 {
            let bytes = docs[i].save();
            let mut fork = MetadataDoc::load(&bytes).unwrap();
            docs[0].merge(fork.doc_mut()).unwrap();
        }

        // All changes should be preserved
        assert_eq!(docs[0].get_field("status").unwrap(), "InProgress");
        assert_eq!(docs[0].get_field("assignee").unwrap(), "alice");
        assert_eq!(docs[0].get_field("priority").unwrap(), "High");
        assert_eq!(docs[0].get_field("title").unwrap(), "Updated task title");
        assert_eq!(docs[0].get_field("external_source").unwrap(), "deck");
    }

    // ── Metadata: same field edited by 2 users (LWW) ─────────────

    #[test]
    fn same_field_concurrent_edit() {
        let mut doc_a = MetadataDoc::new();
        doc_a.set_field("status", "Open");
        let saved = doc_a.save();

        let mut doc_b = MetadataDoc::load(&saved).unwrap();

        // Both change status to different values
        doc_a.set_field("status", "InProgress");
        doc_b.set_field("status", "Done");

        // Merge — one wins (Automerge uses actor ID ordering)
        doc_a.merge(doc_b.doc_mut()).unwrap();
        let status = doc_a.get_field("status").unwrap();

        // Should be one of the two — not corrupted or lost
        assert!(
            status == "InProgress" || status == "Done",
            "Status should be one of the concurrent values, got: {status}"
        );
    }

    // ── Body: 5 users checking off different subtasks ────────────

    #[test]
    fn five_users_check_different_subtasks() {
        let initial_text = "## Subtasks\n\
                            - [ ] Task A\n\
                            - [ ] Task B\n\
                            - [ ] Task C\n\
                            - [ ] Task D\n\
                            - [ ] Task E";

        let doc_0 = BodyDoc::with_text(initial_text);
        let saved = doc_0.save();

        // 5 users fork from the same state
        let docs: Vec<BodyDoc> = (0..5).map(|_| BodyDoc::load(&saved).unwrap()).collect();

        // Each user checks off their subtask
        let replacements = [
            ("[ ] Task A", "[x] Task A"),
            ("[ ] Task B", "[x] Task B"),
            ("[ ] Task C", "[x] Task C"),
            ("[ ] Task D", "[x] Task D"),
            ("[ ] Task E", "[x] Task E"),
        ];

        for (i, (old, new)) in replacements.iter().enumerate() {
            let text = docs[i].text();
            if let Some(pos) = text.find(old) {
                docs[i].delete(pos as u32, old.len() as u32);
                docs[i].insert(pos as u32, new);
            }
        }

        // Sync all updates into doc_0
        for i in 1..5 {
            let sv = docs[0].state_vector();
            let update = docs[i].encode_update(&sv).unwrap();
            docs[0].apply_update(&update).unwrap();
        }

        let final_text = docs[0].text();

        // All 5 subtasks should be checked
        assert!(final_text.contains("[x] Task A"), "Task A should be checked");
        assert!(final_text.contains("[x] Task B"), "Task B should be checked");
        assert!(final_text.contains("[x] Task C"), "Task C should be checked");
        assert!(final_text.contains("[x] Task D"), "Task D should be checked");
        assert!(final_text.contains("[x] Task E"), "Task E should be checked");
    }

    // ── Full document: metadata + body concurrent edits ──────────

    #[test]
    fn full_document_concurrent_edits() {
        let task = Task {
            title: "Concert prep".into(),
            status: Status::Open,
            assignee: Some("alice".into()),
            body: "## Checklist\n- [ ] Book venue\n- [ ] Hire sound\n- [ ] Sell tickets".into(),
            ..Default::default()
        };

        let mut doc_a = CrdtDocument::from_task(&task, "concert.md");

        // Save and fork
        let (meta_bytes, body_bytes) = doc_a.save();
        let mut meta_b = MetadataDoc::load(&meta_bytes).unwrap();
        let body_b = BodyDoc::load(&body_bytes).unwrap();

        // User A: changes status
        doc_a.set_field("status", "InProgress");

        // User B: checks off a subtask
        let text_b = body_b.text();
        if let Some(pos) = text_b.find("[ ] Book venue") {
            body_b.delete(pos as u32, "[ ] Book venue".len() as u32);
            body_b.insert(pos as u32, "[x] Book venue");
        }

        // User B: changes assignee
        meta_b.set_field("assignee", "bob");

        // Merge metadata
        doc_a.merge_metadata(&mut meta_b).unwrap();

        // Merge body
        let sv_a = doc_a.body.state_vector();
        let update_b = body_b.encode_update(&sv_a).unwrap();
        doc_a.apply_body_update(&update_b).unwrap();

        // Verify convergence
        let result = doc_a.to_task();
        assert_eq!(result.status, Status::InProgress, "Status should be InProgress (from A)");
        assert_eq!(result.assignee, Some("bob".into()), "Assignee should be bob (from B)");
        assert!(result.body.contains("[x] Book venue"), "Book venue should be checked (from B)");
        assert!(result.body.contains("[ ] Hire sound"), "Hire sound should still be unchecked");
    }

    // ── Merge order independence ─────────────────────────────────

    #[test]
    fn merge_order_independent() {
        let task = Task {
            title: "Order test".into(),
            ..Default::default()
        };

        let mut doc_base = MetadataDoc::new();
        doc_base.from_task(&task);
        let saved = doc_base.save();

        // Three users make changes
        let mut doc_a = MetadataDoc::load(&saved).unwrap();
        let mut doc_b = MetadataDoc::load(&saved).unwrap();
        let mut doc_c = MetadataDoc::load(&saved).unwrap();

        doc_a.set_field("assignee", "alice");
        doc_b.set_field("priority", "Urgent");
        doc_c.set_field("external_source", "github");

        // Merge in order A, B, C
        let mut result_abc = MetadataDoc::load(&saved).unwrap();
        result_abc.merge(doc_a.doc_mut()).unwrap();
        result_abc.merge(doc_b.doc_mut()).unwrap();
        result_abc.merge(doc_c.doc_mut()).unwrap();

        // Merge in order C, A, B
        let mut doc_a2 = MetadataDoc::load(&saved).unwrap();
        let mut doc_b2 = MetadataDoc::load(&saved).unwrap();
        let mut doc_c2 = MetadataDoc::load(&saved).unwrap();
        doc_a2.set_field("assignee", "alice");
        doc_b2.set_field("priority", "Urgent");
        doc_c2.set_field("external_source", "github");

        let mut result_cab = MetadataDoc::load(&saved).unwrap();
        result_cab.merge(doc_c2.doc_mut()).unwrap();
        result_cab.merge(doc_a2.doc_mut()).unwrap();
        result_cab.merge(doc_b2.doc_mut()).unwrap();

        // Results should be identical regardless of merge order
        assert_eq!(
            result_abc.get_field("assignee"),
            result_cab.get_field("assignee"),
        );
        assert_eq!(
            result_abc.get_field("priority"),
            result_cab.get_field("priority"),
        );
        assert_eq!(
            result_abc.get_field("external_source"),
            result_cab.get_field("external_source"),
        );
    }
}
