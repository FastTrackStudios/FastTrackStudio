#![cfg(test)]

use agent_crdt::{AgentRunRepoLoro, CrdtDoc};
use agent_proto::{AgentRunCreate, AgentRunRepo, AgentRunUpdate};
use architect::Page;
use chrono::Utc;
use loro::ExportMode;

fn repo() -> AgentRunRepoLoro {
    AgentRunRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> AgentRunCreate {
    AgentRunCreate {
        name: "claude-code".into(),
        kind: "code-edit".into(),
        prompt: "refactor the agent feature".into(),
        status: "queued".into(),
        task_id: None,
        started_at: Some(Utc::now()),
        completed_at: None,
        result: None,
        error_message: None,
        tokens_used: None,
        cost_cents: None,
        tags: vec!["urgent".into(), "rust".into()],
    }
}

#[tokio::test]
async fn round_trip_all_fields() {
    let r = repo();
    let a = r.create(fixture()).await.unwrap();
    let got = r.get(a.id).await.unwrap();
    assert_eq!(got.name, "claude-code");
    assert_eq!(got.kind, "code-edit");
    assert_eq!(got.prompt, "refactor the agent feature");
    assert_eq!(got.status, "queued");
    assert_eq!(got.tags, vec!["urgent".to_string(), "rust".into()]);
}

#[tokio::test]
async fn update_status_to_done() {
    let r = repo();
    let a = r.create(fixture()).await.unwrap();
    let now = Utc::now();
    let updated = r
        .update(
            a.id,
            AgentRunUpdate {
                status: Some("done".into()),
                completed_at: Some(Some(now)),
                tokens_used: Some(Some(1234)),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.status, "done");
    assert!(updated.completed_at.is_some());
    assert_eq!(updated.tokens_used, Some(1234));
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(AgentRunCreate {
        name: "gpt-5".into(),
        ..fixture()
    })
    .await
    .unwrap();
    let ab = a.doc().export(ExportMode::all_updates()).unwrap();
    let bb = b.doc().export(ExportMode::all_updates()).unwrap();
    b.doc().import(&ab).unwrap();
    a.doc().import(&bb).unwrap();
    assert_eq!(
        a.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        b.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}
