#![cfg(test)]

use agent_crdt::{
    AgentConversationRepoLoro, AgentLogLineRepoLoro, AgentRunRepoLoro, CrdtDoc,
    GitRepoConnectionRepoLoro,
};
use agent_proto::{
    AgentConversationCreate, AgentConversationRepo, AgentLogLineCreate, AgentLogLineRepo,
    AgentRunCreate, AgentRunRepo, AgentRunUpdate, ChatModel, ChatModelRegistry, ChatStreamChunk,
    ChatStreamRequest, GitRepoConnectionCreate, GitRepoConnectionRepo, ModelInfo,
};
use agent_proto::integration::IntegrationError;
use async_trait::async_trait;
use futures_core::stream::BoxStream;
use std::sync::Arc;
use architect::Page;
use chrono::Utc;
use loro::ExportMode;
use uuid::Uuid;

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
        integration: None,
        external_id: None,
        external_url: None,
        log_cursor: None,
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

// ── Phase A additions: integration + log-line + git connection ───────

#[tokio::test]
async fn agent_run_integration_fields_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let runs = AgentRunRepoLoro::new(&doc);

    let run = runs
        .create(AgentRunCreate {
            integration: Some("hermes".into()),
            external_id: Some("hermes-42".into()),
            external_url: Some("https://hermes.example.com/tasks/42".into()),
            log_cursor: Some(17),
            ..fixture()
        })
        .await
        .unwrap();

    let got = runs.get(run.id).await.unwrap();
    assert_eq!(got.integration.as_deref(), Some("hermes"));
    assert_eq!(got.external_id.as_deref(), Some("hermes-42"));
    assert_eq!(
        got.external_url.as_deref(),
        Some("https://hermes.example.com/tasks/42")
    );
    assert_eq!(got.log_cursor, Some(17));

    // Patch the cursor — should survive the update path too.
    let bumped = runs
        .update(
            run.id,
            AgentRunUpdate {
                log_cursor: Some(Some(99)),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(bumped.log_cursor, Some(99));
}

#[tokio::test]
async fn agent_log_line_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let lines = AgentLogLineRepoLoro::new(&doc);

    let run_id = Uuid::new_v4();
    let at = Utc::now();
    let line = lines
        .create(AgentLogLineCreate {
            run_id,
            at,
            level: "info".into(),
            source: "hermes.tool".into(),
            text: "ran cargo build".into(),
            external_event_id: Some(123),
        })
        .await
        .unwrap();

    let got = lines.get(line.id).await.unwrap();
    assert_eq!(got.run_id, run_id);
    assert_eq!(got.level, "info");
    assert_eq!(got.source, "hermes.tool");
    assert_eq!(got.text, "ran cargo build");
    assert_eq!(got.external_event_id, Some(123));
}

#[tokio::test]
async fn git_repo_connection_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let conns = GitRepoConnectionRepoLoro::new(&doc);

    let conn = conns
        .create(GitRepoConnectionCreate {
            provider: "github".into(),
            owner: "Codys-Wright".into(),
            repo: "Task".into(),
            default_branch: "main".into(),
            project_id: None,
            webhook_secret_hash: "deadbeef".repeat(8),
            webhook_path: "gh-7f3a".into(),
            last_event_at: None,
        })
        .await
        .unwrap();

    let got = conns.get(conn.id).await.unwrap();
    assert_eq!(got.provider, "github");
    assert_eq!(got.owner, "Codys-Wright");
    assert_eq!(got.repo, "Task");
    assert_eq!(got.webhook_path, "gh-7f3a");
    assert_eq!(got.webhook_secret_hash.len(), 64);
}

#[tokio::test]
async fn log_lines_export_import_converges() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let la = AgentLogLineRepoLoro::new(&a);
    let lb = AgentLogLineRepoLoro::new(&b);

    let run_id = Uuid::new_v4();
    la.create(AgentLogLineCreate {
        run_id,
        at: Utc::now(),
        level: "stdout".into(),
        source: "stdout".into(),
        text: "from a".into(),
        external_event_id: Some(1),
    })
    .await
    .unwrap();
    lb.create(AgentLogLineCreate {
        run_id,
        at: Utc::now(),
        level: "stderr".into(),
        source: "stderr".into(),
        text: "from b".into(),
        external_event_id: Some(2),
    })
    .await
    .unwrap();

    let ab = la.doc().export(ExportMode::all_updates()).unwrap();
    let bb = lb.doc().export(ExportMode::all_updates()).unwrap();
    lb.doc().import(&ab).unwrap();
    la.doc().import(&bb).unwrap();

    let total_a = la
        .list(Page { index: 0, size: 100 }, None, None)
        .await
        .unwrap()
        .total;
    let total_b = lb
        .list(Page { index: 0, size: 100 }, None, None)
        .await
        .unwrap()
        .total;
    assert_eq!(total_a, 2);
    assert_eq!(total_b, 2);
}

// ── Phase A: AgentConversation + ChatModelRegistry ───────────────────

#[tokio::test]
async fn agent_conversation_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let convs = AgentConversationRepoLoro::new(&doc);

    let conv = convs
        .create(AgentConversationCreate {
            title: "Plan the migration".into(),
            system_prompt: Some("You are a helpful AI.".into()),
            default_model: "claude-opus-4-7".into(),
            temperature_milli: 700,
            max_tokens: Some(4096),
            tool_set: vec!["shell".into(), "edit".into()],
            agent_run_id: None,
            project_id: None,
            parent_conversation_id: None,
            branch_from_message_id: None,
            archived: false,
        })
        .await
        .unwrap();

    let got = convs.get(conv.id).await.unwrap();
    assert_eq!(got.title, "Plan the migration");
    assert_eq!(got.default_model, "claude-opus-4-7");
    assert_eq!(got.temperature_milli, 700);
    assert_eq!(got.max_tokens, Some(4096));
    assert_eq!(got.tool_set, vec!["shell".to_string(), "edit".to_string()]);
    assert!(got.system_prompt.is_some());
    assert!(!got.archived);
}

struct FakeProvider {
    name: &'static str,
    catalog: Vec<ModelInfo>,
}

#[async_trait]
impl ChatModel for FakeProvider {
    fn provider(&self) -> &'static str {
        self.name
    }
    fn models(&self) -> Vec<ModelInfo> {
        self.catalog.clone()
    }
    async fn complete_stream(
        &self,
        _req: ChatStreamRequest,
    ) -> Result<BoxStream<'static, Result<ChatStreamChunk, IntegrationError>>, IntegrationError>
    {
        Err(IntegrationError::Disabled)
    }
}

#[tokio::test]
async fn chat_model_registry_for_model() {
    let anthropic = Arc::new(FakeProvider {
        name: "anthropic",
        catalog: vec![ModelInfo {
            id: "claude-opus-4-7".into(),
            provider: "anthropic".into(),
            display: "Claude Opus 4.7".into(),
            reasoning: true,
            context_tokens: 200_000,
            input_cost_milli: 15_000,
            output_cost_milli: 75_000,
        }],
    });
    let ollama = Arc::new(FakeProvider {
        name: "ollama",
        catalog: vec![ModelInfo {
            id: "gemma3:4b".into(),
            provider: "ollama".into(),
            display: "Gemma 3 (4B)".into(),
            reasoning: false,
            context_tokens: 128_000,
            input_cost_milli: 0,
            output_cost_milli: 0,
        }],
    });

    let mut reg = ChatModelRegistry::new();
    reg.register(anthropic);
    reg.register(ollama);

    let claude = reg.for_model("claude-opus-4-7").expect("claude routes");
    assert_eq!(claude.provider(), "anthropic");
    let gemma = reg.for_model("gemma3:4b").expect("gemma routes");
    assert_eq!(gemma.provider(), "ollama");
    assert!(reg.for_model("does-not-exist").is_none());
    assert_eq!(reg.all_models().len(), 2);
}
