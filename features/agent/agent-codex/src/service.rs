//! Codex's per-capability impls.
//!
//! Codex provides:
//!
//! - [`Sessions`] — in-memory registry; sessions persist
//!   only for the backend's lifetime (file-backed
//!   persistence lands once we wire `wiki-live`-style state).
//! - [`TurnDispatch`] — wraps [`crate::chat::ChatHandle`]
//!   so each turn spawns `codex app-server` and streams
//!   events to the session's broadcast channel.
//! - [`Threads`] — message history rebuilt from
//!   accumulated `MessageDelta`s.
//! - [`Subscriptions`] — `subscribe_session` over the
//!   per-session broadcast channel. `subscribe_board` /
//!   `subscribe_global` close immediately (Codex doesn't
//!   own boards; global firehose is `subscribe_raw` on the
//!   backend handle instead).
//!
//! Codex *does not* implement: `ToolCalls`, Reasoning,
//! Attachments, Approvals, Questions, Kanban, Profiles,
//! Projects, Backends, `ExternalImport`. Other backends
//! (Hermes for kanban / approvals / profiles, the future
//! `agent-task` for projects) cover those.

use std::collections::HashMap;
use std::path::PathBuf;

use agent_proto::error::AgentError;
use agent_proto::event::AgentEvent;
use agent_proto::message::{ContentBlock, Message, Role};
use agent_proto::service::sessions::{CreateSession, SessionFilter, SessionPage, Sessions};
use agent_proto::service::subscriptions::Subscriptions;
use agent_proto::service::threads::Threads;
use agent_proto::service::turn_dispatch::{DispatchAck, DispatchTurn, TurnDispatch};
use agent_proto::session::{Session, SessionStatus, SourceTag, UsageStats};
use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use chrono::Utc;
use futures::StreamExt;
use tokio::sync::broadcast;
use uuid::Uuid;
use vox::Tx;

use crate::chat::ChatOpts;
use crate::{CodexBackend, SessionRow};

const BACKEND_ID: &str = "codex";

impl HasDispatcher for CodexBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

// ────────────────────── Sessions ──────────────────────
impl Sessions for CodexBackend {
    fn create_session(&self, args: CreateSession) -> Result<Session, AgentError> {
        let id = format!("sess-{}", Uuid::new_v4().simple());
        let now = Utc::now();
        let workspace_path = if args.workspace_path.is_empty() {
            std::env::current_dir()
                .map(|p| p.display().to_string())
                .unwrap_or_default()
        } else {
            args.workspace_path
        };
        let session = Session {
            id: id.clone(),
            title: if args.title.is_empty() {
                "Untitled".to_string()
            } else {
                args.title
            },
            project_id: args.project_id,
            profile_id: args.profile_id,
            backend_id: BACKEND_ID.to_string(),
            workspace_path,
            status: SessionStatus::Idle,
            source: SourceTag::Api,
            subagent_nickname: args.subagent_nickname,
            pinned: false,
            archived: false,
            created_at: now,
            updated_at: now,
            last_message_at: None,
            pending: None,
            usage: UsageStats {
                input_tokens: 0,
                output_tokens: 0,
                cache_read_tokens: 0,
                cache_write_tokens: 0,
                estimated_cost_usd: 0.0,
            },
            compression: None,
            worktree: None,
            composer_draft: agent_proto::session::ComposerDraft {
                text: String::new(),
                attachments: Vec::new(),
                updated_at: None,
            },
        };
        let (events_tx, _) = broadcast::channel::<AgentEvent>(256);
        let mut sessions = self.inner.sessions.blocking_lock();
        sessions.insert(
            id,
            SessionRow {
                session: session.clone(),
                events_tx,
                accumulated: HashMap::new(),
            },
        );
        Ok(session)
    }

    fn read_session(&self, session_id: &str) -> Result<Session, AgentError> {
        let sessions = self.inner.sessions.blocking_lock();
        sessions
            .get(session_id)
            .map(|row| row.session.clone())
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))
    }

    fn list_sessions(&self, filter: SessionFilter) -> Result<SessionPage, AgentError> {
        let sessions = self.inner.sessions.blocking_lock();
        let mut out: Vec<Session> = sessions
            .values()
            .filter(|row| {
                if !filter.project_id.is_empty() && row.session.project_id != filter.project_id {
                    return false;
                }
                if !filter.profile_id.is_empty() && row.session.profile_id != filter.profile_id {
                    return false;
                }
                if !filter.backend_id.is_empty() && row.session.backend_id != filter.backend_id {
                    return false;
                }
                if !filter.include_archived && row.session.archived {
                    return false;
                }
                if filter.only_pinned && !row.session.pinned {
                    return false;
                }
                true
            })
            .map(|row| row.session.clone())
            .collect();
        out.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
        if filter.limit > 0 {
            out.truncate(filter.limit as usize);
        }
        Ok(SessionPage {
            sessions: out,
            next_cursor: String::new(),
            has_more: false,
        })
    }

    fn rename_session(&self, session_id: &str, title: &str) -> Result<Session, AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get_mut(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        row.session.title = title.to_string();
        row.session.updated_at = Utc::now();
        Ok(row.session.clone())
    }

    fn pin_session(&self, session_id: &str, pinned: bool) -> Result<Session, AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get_mut(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        row.session.pinned = pinned;
        row.session.updated_at = Utc::now();
        Ok(row.session.clone())
    }

    fn archive_session(&self, session_id: &str, archived: bool) -> Result<Session, AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get_mut(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        row.session.archived = archived;
        row.session.updated_at = Utc::now();
        Ok(row.session.clone())
    }

    fn delete_session(&self, session_id: &str) -> Result<(), AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        if sessions.remove(session_id).is_none() {
            return Err(AgentError::SessionNotFound(session_id.to_string()));
        }
        Ok(())
    }

    fn save_composer_draft(
        &self,
        session_id: &str,
        text: &str,
        attachments: Vec<agent_proto::attachment::AttachmentRef>,
    ) -> Result<Session, AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get_mut(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        row.session.composer_draft.text = text.to_string();
        row.session.composer_draft.attachments = attachments;
        row.session.composer_draft.updated_at = Some(Utc::now());
        Ok(row.session.clone())
    }
}

// ────────────────────── TurnDispatch ──────────────────────
impl TurnDispatch for CodexBackend {
    fn dispatch_turn(&self, args: DispatchTurn) -> Result<DispatchAck, AgentError> {
        let (workspace_path, events_tx, model) = {
            let mut sessions = self.inner.sessions.blocking_lock();
            let row = sessions
                .get_mut(&args.session_id)
                .ok_or_else(|| AgentError::SessionNotFound(args.session_id.clone()))?;
            if matches!(row.session.status, SessionStatus::Running) {
                return Err(AgentError::SessionBusy(args.session_id.clone()));
            }
            row.session.status = SessionStatus::Running;
            row.session.updated_at = Utc::now();
            (
                row.session.workspace_path.clone(),
                row.events_tx.clone(),
                if args.model_override.is_empty() {
                    None
                } else {
                    Some(args.model_override.clone())
                },
            )
        };

        let backend = self.clone();
        let session_id = args.session_id.clone();
        let stream_id = format!("stream-{}", Uuid::new_v4().simple());
        let started_at = Utc::now();
        let text = args.text.clone();
        let opts = ChatOpts {
            codex_bin: None,
            codex_args: None,
            codex_home: None,
            model: model.clone(),
            effort: None,
            access_mode: None,
        };

        let stream_id_for_task = stream_id.clone();
        let started_at_for_task = started_at;
        tokio::runtime::Handle::current().spawn(async move {
            let _ = events_tx.send(AgentEvent::TurnStarted {
                session_id: session_id.clone(),
                stream_id: stream_id_for_task,
                at: started_at_for_task,
            });
            match backend
                .chat(PathBuf::from(workspace_path), text, opts)
                .await
            {
                Ok(mut handle) => {
                    while let Some(ev) = handle.events.next().await {
                        if let AgentEvent::MessageDelta {
                            message_id,
                            content_delta,
                            ..
                        } = &ev
                        {
                            let mut sessions = backend.inner.sessions.lock().await;
                            if let Some(row) = sessions.get_mut(&session_id) {
                                row.accumulated
                                    .entry(message_id.clone())
                                    .or_default()
                                    .push_str(content_delta);
                            }
                        }
                        let _ = events_tx.send(ev);
                    }
                    let mut sessions = backend.inner.sessions.lock().await;
                    if let Some(row) = sessions.get_mut(&session_id) {
                        row.session.status = SessionStatus::Idle;
                        row.session.updated_at = Utc::now();
                        row.session.last_message_at = Some(Utc::now());
                    }
                }
                Err(e) => {
                    let _ = events_tx.send(AgentEvent::TurnErrored {
                        session_id: session_id.clone(),
                        kind: "dispatch_error".to_string(),
                        message: e,
                        at: Utc::now(),
                    });
                    let mut sessions = backend.inner.sessions.lock().await;
                    if let Some(row) = sessions.get_mut(&session_id) {
                        row.session.status = SessionStatus::Errored;
                        row.session.updated_at = Utc::now();
                    }
                }
            }
        });

        Ok(DispatchAck {
            session_id: args.session_id,
            stream_id,
            turn_id: 0,
            started_at,
            effective_model: args.model_override,
            effective_backend_id: BACKEND_ID.to_string(),
            effective_profile_id: args.profile_override_id,
        })
    }

    fn cancel_turn(&self, session_id: &str) -> Result<(), AgentError> {
        let mut sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get_mut(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        row.session.status = SessionStatus::Cancelled;
        row.session.updated_at = Utc::now();
        let _ = row.events_tx.send(AgentEvent::TurnCancelled {
            session_id: session_id.to_string(),
            at: Utc::now(),
        });
        Ok(())
    }

    fn resume_session(&self, session_id: &str) -> Result<DispatchAck, AgentError> {
        Err(AgentError::Unsupported {
            backend: BACKEND_ID.to_string(),
            operation: format!("resume_session({session_id})"),
        })
    }
}

// ────────────────────── Threads ──────────────────────
impl Threads for CodexBackend {
    fn list_messages(
        &self,
        session_id: &str,
        _limit: u32,
        _before_cursor: &str,
    ) -> Result<Vec<Message>, AgentError> {
        let sessions = self.inner.sessions.blocking_lock();
        let row = sessions
            .get(session_id)
            .ok_or_else(|| AgentError::SessionNotFound(session_id.to_string()))?;
        let mut out = Vec::with_capacity(row.accumulated.len());
        for (message_id, text) in &row.accumulated {
            out.push(Message {
                id: message_id.clone(),
                session_id: session_id.to_string(),
                role: Role::Assistant,
                content: vec![ContentBlock::Text { text: text.clone() }],
                partial: false,
                errored: false,
                error_text: String::new(),
                reasoning: None,
                created_at: row.session.updated_at,
            });
        }
        Ok(out)
    }

    fn read_message(&self, message_id: &str) -> Result<Message, AgentError> {
        Err(AgentError::MessageNotFound(message_id.to_string()))
    }

    fn append_note(&self, _session_id: &str, _text: &str) -> Result<Message, AgentError> {
        Err(AgentError::Unsupported {
            backend: BACKEND_ID.to_string(),
            operation: "append_note".to_string(),
        })
    }
}

// ────────────────────── Subscriptions ──────────────────────
impl Subscriptions for CodexBackend {
    async fn subscribe_session(&self, session_id: String, tx: Tx<AgentEvent>) {
        let rx = {
            let sessions = self.inner.sessions.lock().await;
            let Some(row) = sessions.get(&session_id) else {
                let _ = tx.close(vox::Metadata::default()).await;
                return;
            };
            row.events_tx.subscribe()
        };
        let mut rx = rx;
        loop {
            match rx.recv().await {
                Ok(ev) => {
                    if tx.send(ev).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(_)) => {
                    if tx.send(AgentEvent::Resync).await.is_err() {
                        return;
                    }
                }
            }
        }
    }

    async fn subscribe_board(&self, _board_id: String, tx: Tx<AgentEvent>) {
        let _ = tx.close(vox::Metadata::default()).await;
    }

    async fn subscribe_global(&self, _tx: Tx<AgentEvent>) {
        // Backend exposes the raw firehose via
        // `CodexBackend::subscribe_raw()` for debug;
        // proto-shaped global is not wired today.
    }
}
