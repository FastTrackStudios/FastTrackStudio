//! Agent-chat HTTP surface — `POST /send`, `GET /stream/{id}` (SSE),
//! `DELETE /stream/{id}`.
//!
//! v1 design: the server is authoritative for the assistant message
//! body — every chunk is flushed into the Loro doc via the
//! `MessageRepoLoro` so any sync client picks it up. The SSE channel
//! is *additive*: a UI peer can subscribe for low-latency token
//! deltas without paying the CRDT round-trip cost on every keystroke.
//!
//! Limits:
//! - No auth (matches the rest of the server in v1).
//! - History is the last 20 messages of the conversation, fetched via
//!   `MessageRepoLoro::list` and filtered client-side.
//! - The wasm web app does NOT yet POST `/send` directly; it runs a
//!   local simulation through the repo (see `feature_routes/agent_chat.rs`).

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use axum::Json;
use axum::Router;
use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::sse::{Event, KeepAlive, Sse};
use axum::routing::{delete, get, post};
use chat_proto::{Message, MessageCreate, MessageRepo, MessageUpdate};
use futures_util::Stream;
use futures_util::StreamExt;
use tokio::sync::{Mutex, broadcast};
use tokio_stream::wrappers::BroadcastStream;
use uuid::Uuid;

use agent_proto::ChatStreamChunk;
use agent_proto::chat_model::{ChatStreamRequest, ChatTurn};

use crate::AppState;

/// One active assistant-stream session — broadcast fanout for SSE
/// peers + a cancellation flag for `DELETE /stream/{id}`.
#[derive(Clone)]
pub struct ChatStreamSession {
    pub tx: broadcast::Sender<ChatStreamChunk>,
    pub cancel: Arc<std::sync::atomic::AtomicBool>,
}

/// In-memory registry of active streams keyed by assistant message id.
#[derive(Default)]
pub struct ChatStreamSessions {
    inner: Mutex<HashMap<Uuid, ChatStreamSession>>,
}

impl ChatStreamSessions {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn register(&self, id: Uuid, session: ChatStreamSession) {
        let mut map = self.inner.lock().await;
        map.insert(id, session);
    }

    pub async fn get(&self, id: Uuid) -> Option<ChatStreamSession> {
        let map = self.inner.lock().await;
        map.get(&id).cloned()
    }

    pub async fn remove(&self, id: Uuid) {
        let mut map = self.inner.lock().await;
        map.remove(&id);
    }
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/send", post(handle_send))
        .route("/stream/{message_id}", get(handle_stream))
        .route("/stream/{message_id}", delete(handle_stop))
}

#[derive(serde::Deserialize)]
pub struct SendRequest {
    pub conversation_id: Uuid,
    pub body: String,
    pub model: String,
    #[serde(default)]
    pub reasoning: bool,
    #[serde(default)]
    pub tool_set: Vec<String>,
}

#[derive(serde::Serialize)]
pub struct SendResponse {
    pub user_message_id: Uuid,
    pub assistant_message_id: Uuid,
    pub stream_url: String,
}

async fn handle_send(
    State(state): State<AppState>,
    Json(body): Json<SendRequest>,
) -> Result<Json<SendResponse>, (StatusCode, String)> {
    let conversation_id = body.conversation_id;

    // 1. User message
    let user_msg = state
        .message_repo
        .create(MessageCreate {
            channel_id: None,
            author: "user".to_string(),
            body: body.body.clone(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: Vec::new(),
            attachment_ids: Vec::new(),
            role: Some("user".to_string()),
            model: None,
            reasoning: None,
            tool_calls_json: None,
            finish_reason: None,
            tokens_input: None,
            tokens_output: None,
            cost_cents: None,
            streaming: false,
            agent_conversation_id: Some(conversation_id),
        })
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, format!("{e:?}")))?;

    // 2. Assistant placeholder
    let assistant_msg = state
        .message_repo
        .create(MessageCreate {
            channel_id: None,
            author: "assistant".to_string(),
            body: String::new(),
            reply_to: None,
            edited_at: None,
            deleted: false,
            mentions: Vec::new(),
            attachment_ids: Vec::new(),
            role: Some("assistant".to_string()),
            model: Some(body.model.clone()),
            reasoning: None,
            tool_calls_json: None,
            finish_reason: None,
            tokens_input: None,
            tokens_output: None,
            cost_cents: None,
            streaming: true,
            agent_conversation_id: Some(conversation_id),
        })
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, format!("{e:?}")))?;

    let (tx, _) = broadcast::channel::<ChatStreamChunk>(256);
    let cancel = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let session = ChatStreamSession {
        tx: tx.clone(),
        cancel: cancel.clone(),
    };
    state
        .chat_sessions
        .register(assistant_msg.id, session)
        .await;

    // 3. Spawn the streaming task.
    let state_clone = state.clone();
    let model_id = body.model.clone();
    let reasoning = body.reasoning;
    let assistant_id = assistant_msg.id;
    tokio::spawn(async move {
        let _ = run_chat_stream(
            state_clone,
            conversation_id,
            assistant_id,
            model_id,
            reasoning,
            cancel,
            tx,
        )
        .await;
    });

    Ok(Json(SendResponse {
        user_message_id: user_msg.id,
        assistant_message_id: assistant_msg.id,
        stream_url: format!("/api/agent-chat/stream/{}", assistant_msg.id),
    }))
}

async fn run_chat_stream(
    state: AppState,
    conversation_id: Uuid,
    assistant_id: Uuid,
    model_id: String,
    reasoning: bool,
    cancel: Arc<std::sync::atomic::AtomicBool>,
    tx: broadcast::Sender<ChatStreamChunk>,
) -> Result<(), String> {
    // Pull recent conversation history.
    let listing = state
        .message_repo
        .list(
            architect::Page {
                index: 0,
                size: 200,
            },
            None,
            None,
        )
        .await
        .map_err(|e| format!("{e:?}"))?;
    let mut history: Vec<Message> = listing
        .items
        .into_iter()
        .filter(|m| m.agent_conversation_id == Some(conversation_id))
        .collect();
    history.sort_by(|a, b| a.created_at.cmp(&b.created_at));
    let history: Vec<ChatTurn> = history
        .into_iter()
        .rev()
        .take(20)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .filter_map(|m| {
            let role = m.role.unwrap_or_else(|| "user".to_string());
            Some(ChatTurn {
                role,
                content: m.body,
            })
        })
        .collect();

    let model = state
        .chat_model_registry
        .for_model(&model_id)
        .or_else(|| {
            // Fall back to the first provider's first model.
            state
                .chat_model_registry
                .providers()
                .first()
                .and_then(|p| state.chat_model_registry.get(p))
        })
        .ok_or_else(|| "no chat model registered".to_string())?;

    let req = ChatStreamRequest {
        model: model_id.clone(),
        system_prompt: None,
        messages: history,
        temperature_milli: 700,
        max_tokens: None,
        tools: Vec::new(),
        stream_reasoning: reasoning,
    };

    let mut stream = model
        .complete_stream(req)
        .await
        .map_err(|e| format!("{e:?}"))?;

    let mut body_buf = String::new();
    let mut reasoning_buf = String::new();
    let mut last_flush = tokio::time::Instant::now();
    let mut tokens_in = 0u32;
    let mut tokens_out = 0u32;
    let mut cost_cents = 0u32;
    let mut finish_reason = "stop".to_string();

    while let Some(chunk) = stream.next().await {
        if cancel.load(std::sync::atomic::Ordering::Relaxed) {
            finish_reason = "stop".to_string();
            break;
        }
        let chunk = match chunk {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!(?e, "chat stream error");
                finish_reason = "error".to_string();
                break;
            }
        };
        // Fan out to SSE peers (best-effort).
        let _ = tx.send(chunk.clone());

        match &chunk {
            ChatStreamChunk::Delta(s) => body_buf.push_str(s),
            ChatStreamChunk::ReasoningDelta(s) => reasoning_buf.push_str(s),
            ChatStreamChunk::Usage {
                tokens_in: i,
                tokens_out: o,
                cost_cents: c,
            } => {
                tokens_in = *i;
                tokens_out = *o;
                cost_cents = *c;
            }
            ChatStreamChunk::Done {
                finish_reason: fr, ..
            } => {
                finish_reason = fr.clone();
                break;
            }
            _ => {}
        }

        // Flush to the doc at 1Hz or 200-char boundaries.
        if last_flush.elapsed() >= Duration::from_millis(1000) || body_buf.len() % 200 == 0 {
            let _ = state
                .message_repo
                .update(
                    assistant_id,
                    MessageUpdate {
                        body: Some(body_buf.clone()),
                        reasoning: Some(if reasoning_buf.is_empty() {
                            None
                        } else {
                            Some(reasoning_buf.clone())
                        }),
                        ..Default::default()
                    },
                )
                .await;
            last_flush = tokio::time::Instant::now();
        }
    }

    // Final flush.
    let _ = state
        .message_repo
        .update(
            assistant_id,
            MessageUpdate {
                body: Some(body_buf),
                reasoning: Some(if reasoning_buf.is_empty() {
                    None
                } else {
                    Some(reasoning_buf)
                }),
                streaming: Some(false),
                finish_reason: Some(Some(finish_reason)),
                tokens_input: Some(Some(tokens_in)),
                tokens_output: Some(Some(tokens_out)),
                cost_cents: Some(Some(cost_cents)),
                ..Default::default()
            },
        )
        .await;

    state.chat_sessions.remove(assistant_id).await;
    Ok(())
}

async fn handle_stream(
    State(state): State<AppState>,
    Path(message_id): Path<Uuid>,
) -> Result<Sse<impl Stream<Item = Result<Event, axum::Error>>>, (StatusCode, String)> {
    let session = state
        .chat_sessions
        .get(message_id)
        .await
        .ok_or((StatusCode::NOT_FOUND, "no active stream".to_string()))?;
    let rx = session.tx.subscribe();
    let stream = BroadcastStream::new(rx).map(|item| match item {
        Ok(chunk) => Ok(chunk_to_event(&chunk)),
        Err(_) => Ok(Event::default().event("lag")),
    });
    Ok(Sse::new(stream).keep_alive(KeepAlive::default()))
}

fn chunk_to_event(chunk: &ChatStreamChunk) -> Event {
    match chunk {
        ChatStreamChunk::Delta(s) => Event::default().event("delta").data(s.clone()),
        ChatStreamChunk::ReasoningDelta(s) => Event::default().event("reasoning").data(s.clone()),
        ChatStreamChunk::ToolCall {
            id,
            name,
            arguments_json,
        } => Event::default().event("tool").data(format!(
            "{{\"id\":\"{}\",\"name\":\"{}\",\"arguments_json\":{}}}",
            id,
            name,
            serde_json::to_string(arguments_json).unwrap_or_else(|_| "\"\"".to_string()),
        )),
        ChatStreamChunk::ToolResult { id, result_json } => Event::default()
            .event("tool_result")
            .data(format!("{{\"id\":\"{id}\",\"result_json\":{result_json}}}")),
        ChatStreamChunk::Usage {
            tokens_in,
            tokens_out,
            cost_cents,
        } => Event::default().event("usage").data(format!(
            "{{\"tokens_in\":{tokens_in},\"tokens_out\":{tokens_out},\"cost_cents\":{cost_cents}}}",
        )),
        ChatStreamChunk::Done { finish_reason } => Event::default()
            .event("done")
            .data(format!("{{\"finish_reason\":\"{finish_reason}\"}}")),
    }
}

async fn handle_stop(
    State(state): State<AppState>,
    Path(message_id): Path<Uuid>,
) -> Result<(), (StatusCode, String)> {
    if let Some(session) = state.chat_sessions.get(message_id).await {
        session
            .cancel
            .store(true, std::sync::atomic::Ordering::Relaxed);
        Ok(())
    } else {
        Err((StatusCode::NOT_FOUND, "no active stream".to_string()))
    }
}
