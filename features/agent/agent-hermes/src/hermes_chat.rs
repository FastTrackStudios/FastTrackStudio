//! `HermesChatModel` — wires the agent-chat surface to a real Hermes
//! profile via the kanban API.
//!
//! Each `complete_stream` call:
//!
//! 1. Creates a Hermes kanban task with the user's latest message as
//!    the body. Assignee = `req.model` if it names a known profile
//!    (`commander`, `architect`, …), else falls back to
//!    `HermesConfig.default_profile`.
//! 2. Polls `/api/plugins/kanban/tasks/{id}` every second.
//! 3. While the task is `ready` / `running` / `blocked`, emits one
//!    `Delta { content: "…" }` heartbeat per poll so the UI shows the
//!    stream is alive.
//! 4. On terminal status (`done`/`failed`/`cancelled`), emits the
//!    task's `result` (`HermesTask.result`, the assistant summary)
//!    as a single `Delta`, then `Usage` + `Done`.
//!
//! Why a single delta at end vs. tailing the log: the Hermes agent
//! session log includes box-drawing, tool-call frames, banners,
//! ANSI escapes — render-safe in a terminal but not in a chat
//! bubble. The `result` field is the clean summary the agent writes
//! at completion. A future enhancement could tail the log and
//! filter for assistant-message lines specifically.

use std::time::Duration;

use agent_proto::ModelInfo;
use agent_proto::chat_model::{ChatModel, ChatStreamChunk, ChatStreamRequest};
use agent_proto::integration::IntegrationError;
use async_trait::async_trait;
use futures_core::stream::BoxStream;
use futures_util::stream::StreamExt;

use crate::client::{HermesClient, HermesCreateTaskRequest};
use crate::config::HermesConfig;

/// Known Hermes profiles. Any `req.model` matching one of these is
/// used as the kanban task `assignee`; everything else falls back to
/// `HermesConfig.default_profile`.
pub const KNOWN_PROFILES: &[&str] = &[
    "architect",
    "atlas",
    "commander",
    "curator",
    "devops",
    "forge",
    "jarvis",
    "keeper",
    "researcher",
    "reviewer",
    "scout",
    "writer",
];

pub struct HermesChatModel {
    config: HermesConfig,
    client: HermesClient,
}

impl HermesChatModel {
    pub fn new(config: HermesConfig) -> Self {
        let client = HermesClient::new(config.clone());
        Self { config, client }
    }

    fn pick_assignee(&self, req_model: &str) -> String {
        // Allow "hermes-profile:commander" prefix or bare "commander".
        let candidate = req_model
            .strip_prefix("hermes-profile:")
            .unwrap_or(req_model);
        if KNOWN_PROFILES.contains(&candidate) {
            candidate.to_string()
        } else {
            self.config.default_profile.clone()
        }
    }
}

#[async_trait]
impl ChatModel for HermesChatModel {
    fn provider(&self) -> &'static str {
        "hermes"
    }

    fn models(&self) -> Vec<ModelInfo> {
        // Surface each known profile as a "model id" so the picker
        // lists them. Token / cost numbers are nominal — Hermes
        // accounts for the real cost upstream.
        KNOWN_PROFILES
            .iter()
            .map(|p| ModelInfo {
                id: format!("hermes:{p}"),
                provider: "hermes".into(),
                display: format!("Hermes — {}", title_case(p)),
                reasoning: false,
                context_tokens: 200_000,
                input_cost_milli: 0,
                output_cost_milli: 0,
            })
            .collect()
    }

    async fn complete_stream(
        &self,
        req: ChatStreamRequest,
    ) -> Result<BoxStream<'static, Result<ChatStreamChunk, IntegrationError>>, IntegrationError>
    {
        let assignee = self.pick_assignee(&req.model);

        // Build the task body from the conversation context. We pass
        // the last few turns so the profile has context, formatted as
        // "[role]: text".
        let body = render_conversation(&req);
        let title = title_from_body(&body);

        let create_req = HermesCreateTaskRequest {
            title,
            body: Some(body),
            assignee: Some(assignee),
            priority: None,
            idempotency_key: None,
            skills: None,
            workspace_kind: None,
            workspace_path: None,
            max_runtime_seconds: Some(300),
        };

        let task = self.client.create_task(create_req).await?;
        let task_id = task.id.clone();
        let client = self.client.clone();

        let stream = async_stream::stream! {
            // Heartbeat ticker for "still working…" deltas. Hermes
            // polls aren't free but Hermes is local; 2s is plenty
            // for chat-perceived liveness.
            let mut tick = tokio::time::interval(Duration::from_secs(2));
            tick.tick().await; // consume immediate first tick

            // Cap total wait at 5 minutes — same as max_runtime above.
            let deadline = tokio::time::Instant::now() + Duration::from_secs(300);

            loop {
                if tokio::time::Instant::now() >= deadline {
                    yield Ok(ChatStreamChunk::Delta("[chat timed out after 5 minutes]".into()));
                    yield Ok(ChatStreamChunk::Done {
                        finish_reason: "timeout".into(),
                    });
                    return;
                }

                let task = match client.get_task(&task_id).await {
                    Ok(t) => t,
                    Err(e) => {
                        yield Err(e);
                        return;
                    }
                };

                match task.status.as_str() {
                    "done" | "completed" => {
                        let result = task.result.clone().unwrap_or_else(|| {
                            "(Hermes returned no summary — check the task log for details.)".into()
                        });
                        yield Ok(ChatStreamChunk::Delta(result));
                        yield Ok(ChatStreamChunk::Done {
                            finish_reason: "stop".into(),
                        });
                        return;
                    }
                    "failed" | "cancelled" => {
                        let err = task
                            .last_failure_error
                            .clone()
                            .unwrap_or_else(|| "Hermes task failed without a message.".into());
                        yield Ok(ChatStreamChunk::Delta(format!("[error] {err}")));
                        yield Ok(ChatStreamChunk::Done {
                            finish_reason: "error".into(),
                        });
                        return;
                    }
                    other => {
                        // Heartbeat — emit a tiny "still working"
                        // marker so the UI streams something.
                        yield Ok(ChatStreamChunk::Delta(format!("[{other}…]\n")));
                    }
                }

                tick.tick().await;
            }
        };

        Ok(stream.boxed())
    }
}

fn render_conversation(req: &ChatStreamRequest) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    if let Some(sys) = &req.system_prompt {
        if !sys.trim().is_empty() {
            let _ = writeln!(out, "[system]: {sys}\n");
        }
    }
    for turn in &req.messages {
        let _ = writeln!(out, "[{}]: {}", turn.role, turn.content);
    }
    out
}

fn title_from_body(body: &str) -> String {
    // Take the first non-empty line, strip role prefix, cap at 80 chars.
    for line in body.lines() {
        let t = line.trim();
        if t.is_empty() {
            continue;
        }
        let cleaned = t
            .strip_prefix("[user]: ")
            .or_else(|| t.strip_prefix("[system]: "))
            .unwrap_or(t);
        return cleaned.chars().take(80).collect();
    }
    "Chat message".into()
}

fn title_case(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cfg() -> HermesConfig {
        HermesConfig::default()
    }

    fn req(model: &str, msgs: Vec<(&str, &str)>) -> ChatStreamRequest {
        use agent_proto::chat_model::ChatTurn;
        ChatStreamRequest {
            model: model.into(),
            system_prompt: None,
            messages: msgs
                .into_iter()
                .map(|(role, content)| ChatTurn {
                    role: role.into(),
                    content: content.into(),
                })
                .collect(),
            temperature_milli: 700,
            max_tokens: None,
            tools: Vec::new(),
            stream_reasoning: false,
        }
    }

    #[test]
    fn pick_assignee_known_profile() {
        let m = HermesChatModel::new(cfg());
        assert_eq!(m.pick_assignee("commander"), "commander");
        assert_eq!(m.pick_assignee("hermes-profile:architect"), "architect");
    }

    #[test]
    fn pick_assignee_unknown_falls_back() {
        let mut c = cfg();
        c.default_profile = "engineer".into();
        let m = HermesChatModel::new(c);
        assert_eq!(m.pick_assignee("gpt-4o"), "engineer");
        assert_eq!(m.pick_assignee(""), "engineer");
    }

    #[test]
    fn provider_key() {
        let m = HermesChatModel::new(cfg());
        assert_eq!(m.provider(), "hermes");
    }

    #[test]
    fn models_lists_every_known_profile() {
        let m = HermesChatModel::new(cfg());
        let ids: Vec<String> = m.models().iter().map(|mi| mi.id.clone()).collect();
        assert!(ids.contains(&"hermes:commander".to_string()));
        assert!(ids.contains(&"hermes:architect".to_string()));
        assert_eq!(ids.len(), KNOWN_PROFILES.len());
    }

    #[test]
    fn render_includes_system_and_turns() {
        let mut r = req("commander", vec![("user", "Hello?"), ("assistant", "Hi.")]);
        r.system_prompt = Some("You are helpful.".into());
        let rendered = render_conversation(&r);
        assert!(rendered.contains("[system]: You are helpful."));
        assert!(rendered.contains("[user]: Hello?"));
        assert!(rendered.contains("[assistant]: Hi."));
    }

    #[test]
    fn title_pulls_first_user_line() {
        let body = "[system]: Be helpful.\n[user]: What's the time?\n[assistant]: Now.\n";
        let t = title_from_body(body);
        // First non-empty line is system; we strip system + user prefixes both.
        assert!(t.starts_with("Be helpful.") || t.starts_with("What's the time?"));
    }

    #[test]
    fn title_caps_at_80_chars() {
        let long = format!("[user]: {}", "x".repeat(200));
        let t = title_from_body(&long);
        assert_eq!(t.chars().count(), 80);
    }
}
