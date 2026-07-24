//! Backend router for the agent services — one `Sessions` /
//! `TurnDispatch` / `Threads` / `Subscriptions` surface over
//! multiple backends (Codex, Hermes).
//!
//! Sessions are owned by exactly one backend; ownership is decided
//! at `create_session` (the new `CreateSession.backend_id` field,
//! empty = the server default: Hermes when a gateway is configured
//! — it's the primary conversational agent — else Codex) and every
//! later call routes by looking the session up in each backend's
//! registry. `list_sessions` merges both registries so the sidebar
//! shows one timeline.

use agent_codex::CodexBackend;
use agent_hermes::HermesBackend;
use agent_proto::error::AgentError;
use agent_proto::event::AgentEvent;
use agent_proto::message::Message;
use agent_proto::service::discovery::{CapabilityFlag, Discovery, ModelInfo, SkillInfo};
use agent_proto::service::sessions::{CreateSession, SessionFilter, SessionPage, Sessions};
use agent_proto::service::subscriptions::Subscriptions;
use agent_proto::service::threads::Threads;
use agent_proto::service::turn_dispatch::{DispatchAck, DispatchTurn, TurnDispatch};
use agent_proto::session::Session;
use vox::Tx;

#[derive(Clone, architect::HasDispatcher)]
pub struct AgentRouter {
    codex: CodexBackend,
    hermes: Option<HermesBackend>,
}

/// Which backend owns a session.
enum Owner {
    Codex,
    Hermes,
}

impl AgentRouter {
    pub fn new(codex: CodexBackend, hermes: Option<HermesBackend>) -> Self {
        Self { codex, hermes }
    }

    /// Resolve a session's owning backend. Registries are
    /// in-memory maps, so probing both is cheap.
    fn owner(&self, session_id: &str) -> Result<Owner, AgentError> {
        if let Some(h) = &self.hermes {
            if h.read_session(session_id).is_ok() {
                return Ok(Owner::Hermes);
            }
        }
        if self.codex.read_session(session_id).is_ok() {
            return Ok(Owner::Codex);
        }
        Err(AgentError::SessionNotFound(session_id.to_string()))
    }

    fn hermes(&self) -> Result<&HermesBackend, AgentError> {
        self.hermes
            .as_ref()
            .ok_or_else(|| AgentError::BackendNotFound(agent_hermes::BACKEND_ID.to_string()))
    }
}

/// Route a by-session-id call to its owning backend.
macro_rules! route {
    ($self:ident, $sid:expr, $method:ident ( $($arg:expr),* )) => {
        match $self.owner($sid)? {
            Owner::Hermes => $self.hermes()?.$method($($arg),*),
            Owner::Codex => $self.codex.$method($($arg),*),
        }
    };
}

impl Sessions for AgentRouter {
    fn create_session(&self, args: CreateSession) -> Result<Session, AgentError> {
        match args.backend_id.as_str() {
            "hermes" => self.hermes()?.create_session(args),
            "codex" => self.codex.create_session(args),
            // Default backend: the conversational agent when a
            // Hermes gateway is configured, else Codex.
            "" => match &self.hermes {
                Some(h) => h.create_session(args),
                None => self.codex.create_session(args),
            },
            other => Err(AgentError::BackendNotFound(other.to_string())),
        }
    }

    fn read_session(&self, session_id: &str) -> Result<Session, AgentError> {
        route!(self, session_id, read_session(session_id))
    }

    fn list_sessions(&self, filter: SessionFilter) -> Result<SessionPage, AgentError> {
        let mut page = self.codex.list_sessions(filter.clone())?;
        if let Some(h) = &self.hermes {
            let hermes_page = h.list_sessions(filter.clone())?;
            page.sessions.extend(hermes_page.sessions);
        }
        page.sessions.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
        if filter.limit > 0 {
            page.sessions.truncate(filter.limit as usize);
        }
        Ok(page)
    }

    fn rename_session(&self, session_id: &str, title: &str) -> Result<Session, AgentError> {
        route!(self, session_id, rename_session(session_id, title))
    }

    fn pin_session(&self, session_id: &str, pinned: bool) -> Result<Session, AgentError> {
        route!(self, session_id, pin_session(session_id, pinned))
    }

    fn archive_session(&self, session_id: &str, archived: bool) -> Result<Session, AgentError> {
        route!(self, session_id, archive_session(session_id, archived))
    }

    fn delete_session(&self, session_id: &str) -> Result<(), AgentError> {
        route!(self, session_id, delete_session(session_id))
    }

    fn save_composer_draft(
        &self,
        session_id: &str,
        text: &str,
        attachments: Vec<agent_proto::attachment::AttachmentRef>,
    ) -> Result<Session, AgentError> {
        route!(
            self,
            session_id,
            save_composer_draft(session_id, text, attachments)
        )
    }
}

impl TurnDispatch for AgentRouter {
    fn dispatch_turn(&self, args: DispatchTurn) -> Result<DispatchAck, AgentError> {
        match self.owner(&args.session_id)? {
            Owner::Hermes => self.hermes()?.dispatch_turn(args),
            Owner::Codex => self.codex.dispatch_turn(args),
        }
    }

    fn cancel_turn(&self, session_id: &str) -> Result<(), AgentError> {
        route!(self, session_id, cancel_turn(session_id))
    }

    fn resume_session(&self, session_id: &str) -> Result<DispatchAck, AgentError> {
        route!(self, session_id, resume_session(session_id))
    }
}

impl Threads for AgentRouter {
    fn list_messages(
        &self,
        session_id: &str,
        limit: u32,
        before_cursor: &str,
    ) -> Result<Vec<Message>, AgentError> {
        route!(self, session_id, list_messages(session_id, limit, before_cursor))
    }

    fn read_message(&self, message_id: &str) -> Result<Message, AgentError> {
        if let Some(h) = &self.hermes {
            if let Ok(m) = h.read_message(message_id) {
                return Ok(m);
            }
        }
        self.codex.read_message(message_id)
    }

    fn append_note(&self, session_id: &str, text: &str) -> Result<Message, AgentError> {
        route!(self, session_id, append_note(session_id, text))
    }
}

impl Discovery for AgentRouter {
    fn list_models(&self, backend_id: &str) -> Result<Vec<ModelInfo>, AgentError> {
        let mut out = Vec::new();
        if backend_id.is_empty() || backend_id == "hermes" {
            if let Some(h) = &self.hermes {
                match h.list_models(backend_id) {
                    Ok(mut m) => out.append(&mut m),
                    Err(e) => tracing::warn!("hermes list_models: {e}"),
                }
            }
        }
        if backend_id.is_empty() || backend_id == "codex" {
            // Codex has no discovery API — its usual model set, default
            // first. Free-form overrides still work via DispatchTurn.
            for (i, id) in ["gpt-5.5-codex", "gpt-5.5", "o5-mini"].iter().enumerate() {
                out.push(ModelInfo {
                    backend_id: "codex".to_string(),
                    id: (*id).to_string(),
                    label: String::new(),
                    is_default: i == 0,
                    context_length: 0,
                });
            }
        }
        Ok(out)
    }

    fn list_skills(&self, backend_id: &str) -> Result<Vec<SkillInfo>, AgentError> {
        match &self.hermes {
            Some(h) if backend_id.is_empty() || backend_id == "hermes" => {
                h.list_skills(backend_id)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn list_capabilities(&self, backend_id: &str) -> Result<Vec<CapabilityFlag>, AgentError> {
        match &self.hermes {
            Some(h) if backend_id.is_empty() || backend_id == "hermes" => {
                h.list_capabilities(backend_id)
            }
            _ => Ok(Vec::new()),
        }
    }
}

impl Subscriptions for AgentRouter {
    async fn subscribe_session(&self, session_id: String, tx: Tx<AgentEvent>) {
        // Async ownership probe — the sync `owner()` path uses
        // `blocking_lock` and would panic inside the runtime.
        if let Some(h) = &self.hermes {
            if h.has_session(&session_id).await {
                h.subscribe_session(session_id, tx).await;
                return;
            }
        }
        if self.codex.has_session(&session_id).await {
            self.codex.subscribe_session(session_id, tx).await;
            return;
        }
        let _ = tx.close(vox::Metadata::default()).await;
    }

    async fn subscribe_board(&self, _board_id: String, tx: Tx<AgentEvent>) {
        let _ = tx.close(vox::Metadata::default()).await;
    }

    async fn subscribe_global(&self, tx: Tx<AgentEvent>) {
        let _ = tx.close(vox::Metadata::default()).await;
    }
}
