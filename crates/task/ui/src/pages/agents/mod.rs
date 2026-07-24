//! `/agents` — the agent command center.
//!
//! Three panes, CodexMonitor/t3code-inspired:
//!
//! - **Session rail** — searchable, pinned-first, hover actions,
//!   priority status pills, hash-colored subagent chips.
//! - **Chat pane** — a derived row timeline ([`timeline`]): turn
//!   folds ("N tool calls · 0:42") above settled assistant
//!   messages, live activity chips with status glyphs, streaming
//!   markdown, a ticking Working row; composer with queue-while-
//!   busy sends, `/` + `$` ranked autocomplete, a model chip fed by
//!   Discovery, and a CSS-only context ring.
//! - **Inspector** — session meta + actions, skill library,
//!   capability flags.
//!
//! All decidable behavior lives in [`logic`] / [`timeline`] as
//! pure, tested functions (the t3code `*.logic.ts` pattern); the
//! components are thin `rsx!` over them.

mod logic;
mod timeline;

use std::collections::HashMap;

use agent_proto::event::AgentEvent;
use agent_proto::message::{ContentBlock, Message, Role};
use agent_proto::question::QuestionRequest;
use agent_proto::service::discovery::{CapabilityFlag, ModelInfo, SkillInfo};
use agent_proto::session::{Session, SessionStatus};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Archive, Bot, Copy, Info, Pin, Trash2};
use fts_ui::prelude::*;

use logic::{
    context_free_percent, context_ring_style, fmt_elapsed, fmt_tokens, hash_hsl, rank_by,
    relative_time, status_pill,
};
use timeline::{ActivityLine, Row, ToolTone, TurnLog, fold_summary, push_line, settle_tool};

/// Duty-cycled pulse (t3code: `steps()` timing keeps the
/// compositor cheap) + halo styling utility classes can't express.
const AGENTS_CSS: &str = r#"
@keyframes agents-pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.35; }
}
.agents-dot-pulse {
  animation: agents-pulse 1.2s steps(6) infinite;
  box-shadow: 0 0 0 3px color-mix(in srgb, currentColor 25%, transparent);
}
@media (prefers-reduced-motion: reduce) {
  .agents-dot-pulse { animation: none; }
}
"#;

#[component]
pub fn AgentsView() -> Element {
    let selection = use_context::<Signal<crate::orgs::OrgSelection>>();
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();

    let mut sessions = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        if slugs.is_empty() {
            return Ok(Vec::new());
        }
        crate::feeds::fetch_agent_sessions(&slugs).await
    });

    let active = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    // Discovery — live models / skills / capabilities for the active org.
    let models = use_resource(move || async move {
        match active() {
            Some(s) => crate::feeds::fetch_agent_models(&s).await.unwrap_or_default(),
            None => Vec::new(),
        }
    });
    let skills = use_resource(move || async move {
        match active() {
            Some(s) => crate::feeds::fetch_agent_skills(&s).await.unwrap_or_default(),
            None => Vec::new(),
        }
    });
    let capabilities = use_resource(move || async move {
        match active() {
            Some(s) => crate::feeds::fetch_agent_capabilities(&s).await.unwrap_or_default(),
            None => Vec::new(),
        }
    });
    let model_list = models.read().clone().unwrap_or_default();
    let skill_list = skills.read().clone().unwrap_or_default();
    let cap_list = capabilities.read().clone().unwrap_or_default();

    let mut selected = use_signal(|| None::<(String, Session)>);
    let mut search = use_signal(String::new);
    let mut show_archived = use_signal(|| false);
    let mut show_inspector = use_signal(|| true);
    let mut new_backend = use_signal(String::new);
    let mut create_error = use_signal(String::new);

    let backends_available = use_memo(move || {
        let mut b: Vec<String> = models
            .read()
            .clone()
            .unwrap_or_default()
            .iter()
            .map(|m| m.backend_id.clone())
            .collect();
        b.sort();
        b.dedup();
        b
    });

    let on_new_chat = move |_| {
        let Some(slug) = active() else { return };
        let backend = new_backend.peek().clone();
        spawn(async move {
            match crate::feeds::create_agent_session(&slug, &backend, "").await {
                Ok(session) => {
                    create_error.set(String::new());
                    selected.set(Some((slug, session)));
                    sessions.restart();
                }
                Err(e) => create_error.set(e),
            }
        });
    };

    let rows: Vec<(String, Session)> = {
        let q = search.read().to_lowercase();
        let mut rows: Vec<(String, Session)> = match &*sessions.read_unchecked() {
            Some(Ok(rows)) => rows.clone(),
            _ => Vec::new(),
        };
        rows.retain(|(_, s)| {
            (show_archived() || !s.archived)
                && (q.is_empty() || s.title.to_lowercase().contains(&q))
        });
        rows.sort_by(|(_, a), (_, b)| {
            b.pinned.cmp(&a.pinned).then_with(|| {
                let ka = a.last_message_at.unwrap_or(a.created_at);
                let kb = b.last_message_at.unwrap_or(b.created_at);
                kb.cmp(&ka)
            })
        });
        rows
    };
    let fetch_err = match &*sessions.read_unchecked() {
        Some(Err(e)) => e.clone(),
        _ => String::new(),
    };
    let selected_id = selected.read().as_ref().map(|(_, s)| s.id.clone());

    let mutate = use_callback(move |(slug, id, action): (String, String, SessionAction)| {
        spawn(async move {
            let res: Result<(), String> = match action {
                SessionAction::Pin(v) => crate::feeds::pin_agent_session(&slug, &id, v)
                    .await
                    .map(|_| ()),
                SessionAction::Archive(v) => {
                    crate::feeds::archive_agent_session(&slug, &id, v).await.map(|_| ())
                }
                SessionAction::Rename(t) => {
                    crate::feeds::rename_agent_session(&slug, &id, &t).await.map(|_| ())
                }
                SessionAction::Delete => crate::feeds::delete_agent_session(&slug, &id).await,
            };
            if let Err(e) = res {
                create_error.set(e);
            } else {
                let touched_open = matches!(
                    selected.peek().as_ref(),
                    Some((_, s)) if s.id == id
                );
                if touched_open {
                    if let Ok(s) = crate::feeds::fetch_agent_sessions(&[slug.clone()]).await {
                        match s.into_iter().find(|(_, s)| s.id == id) {
                            Some(row) => selected.set(Some(row)),
                            None => selected.set(None),
                        }
                    }
                }
                sessions.restart();
            }
        });
    });

    rsx! {
        style { {AGENTS_CSS} }
        div { class: "flex h-full min-h-0 w-full",
            // ── Session rail ──
            div { class: "flex w-72 shrink-0 flex-col border-r border-border/60",
                div { class: "flex items-center justify-between gap-2 px-3 py-3",
                    div { class: "flex items-center gap-2",
                        Bot { size: 18 }
                        span { class: "text-sm font-semibold", "Agents" }
                    }
                    div { class: "flex items-center gap-1",
                        if backends_available().len() > 1 {
                            select {
                                class: "rounded-md border border-border/60 bg-card/40 px-1 py-0.5 text-[0.7rem] text-muted-foreground",
                                value: "{new_backend}",
                                onchange: move |e| new_backend.set(e.value()),
                                option { value: "", "auto" }
                                for b in backends_available().iter() {
                                    option { key: "{b}", value: "{b}", "{b}" }
                                }
                            }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            disabled: active().is_none(),
                            on_click: on_new_chat,
                            "New chat"
                        }
                    }
                }
                div { class: "px-3 pb-2",
                    input {
                        class: "w-full rounded-md border border-border/60 bg-card/30 px-2 py-1 text-xs text-foreground outline-none focus:border-primary/60",
                        placeholder: "Search conversations…",
                        value: "{search}",
                        oninput: move |e| search.set(e.value()),
                    }
                }
                if !create_error.read().is_empty() {
                    div { class: "mx-3 mb-2 rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs",
                        "{create_error}"
                    }
                }
                if !fetch_err.is_empty() {
                    div { class: "mx-3 mb-2 rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs",
                        "Couldn't reach the agent service: {fetch_err}"
                    }
                }
                div { class: "min-h-0 flex-1 overflow-y-auto px-1.5 pb-1",
                    if rows.is_empty() && fetch_err.is_empty() {
                        div { class: "flex flex-col items-center gap-2 px-4 py-10 text-center",
                            Bot { size: 24 }
                            Text { variant: TextVariant::Muted, class: "text-xs",
                                "No conversations yet — start one with New chat."
                            }
                        }
                    }
                    for (slug , s) in rows.iter() {
                        {session_row(slug, s, selected_id.as_deref(), selected, mutate)}
                    }
                }
                button {
                    r#type: "button",
                    class: "mx-3 mb-2 text-left text-[0.7rem] text-muted-foreground/70 hover:text-foreground",
                    onclick: move |_| {
                        let v = *show_archived.peek();
                        show_archived.set(!v);
                    },
                    if show_archived() { "Hide archived" } else { "Show archived" }
                }
            }

            // ── Chat pane ──
            if let Some((slug, session)) = selected.read().clone() {
                ChatPane {
                    key: "{session.id}",
                    slug,
                    session,
                    models: model_list.clone(),
                    skills: skill_list.clone(),
                    inspector_open: show_inspector(),
                    on_toggle_inspector: move |()| {
                        let v = *show_inspector.peek();
                        show_inspector.set(!v);
                    },
                    on_activity: move |()| sessions.restart(),
                }
            } else {
                div { class: "flex flex-1 flex-col items-center justify-center gap-3 text-center",
                    Bot { size: 32 }
                    Heading { level: HeadingLevel::H3, "Chat with your agents" }
                    Text { variant: TextVariant::Muted, class: "max-w-sm text-sm",
                        "Pick a conversation or start a new chat. Hermes answers by default; the inspector shows its skills and capabilities."
                    }
                }
            }

            // ── Inspector ──
            if show_inspector() {
                if let Some((slug, session)) = selected.read().clone() {
                    Inspector {
                        key: "insp-{session.id}",
                        slug,
                        session,
                        skills: skill_list.clone(),
                        capabilities: cap_list.clone(),
                        mutate,
                    }
                }
            }
        }
    }
}

/// One composer autocomplete row.
#[derive(Clone, PartialEq)]
struct CompletionRow {
    insert: String,
    label: String,
    detail: String,
}

/// Hermes slash commands the gateway understands (v0.19). Curated —
/// the gateway has no commands-discovery endpoint yet; keep in sync
/// with `hermes --help` when bumping the input.
const HERMES_COMMANDS: &[(&str, &str)] = &[
    ("/new", "Start a fresh context (clears the session's history)"),
    ("/model", "Show or switch the model for this session"),
    ("/skills", "List the agent's learned skills"),
    ("/learn", "Teach the agent a new skill from this conversation"),
    ("/journey", "Show what the agent has learned over time"),
    ("/compact", "Compress older context into a summary"),
    ("/status", "Agent + backend status"),
    ("/memory", "Inspect the agent's persistent memory"),
    ("/tools", "List available toolsets"),
    ("/help", "List available commands"),
];

/// The trigger token under the caret-at-end heuristic: `/` only as
/// the very first token (commands), `$` on the last whitespace-
/// separated token (skills). Returns (mode, query, token_start).
fn completion_trigger(text: &str) -> Option<(char, String, usize)> {
    if let Some(rest) = text.strip_prefix('/') {
        if !rest.contains(char::is_whitespace) {
            return Some(('/', rest.to_lowercase(), 0));
        }
    }
    let start = text
        .rfind(char::is_whitespace)
        .map_or(0, |i| i + text[i..].chars().next().map_or(1, char::len_utf8));
    let token = &text[start..];
    if let Some(q) = token.strip_prefix('$') {
        return Some(('$', q.to_lowercase(), start));
    }
    None
}

/// Rail/inspector session mutations.
#[derive(Clone, PartialEq)]
enum SessionAction {
    Pin(bool),
    Archive(bool),
    Rename(String),
    Delete,
}

/// One conversation row: status pill dot, title, backend chip,
/// hash-colored subagent chip, relative time, hover actions.
fn session_row(
    slug: &str,
    s: &Session,
    selected_id: Option<&str>,
    mut selected: Signal<Option<(String, Session)>>,
    mutate: Callback<(String, String, SessionAction)>,
) -> Element {
    let is_sel = selected_id == Some(s.id.as_str());
    let row_slug = slug.to_string();
    let row_session = s.clone();
    let title = if s.title.trim().is_empty() {
        "(untitled)".to_string()
    } else {
        s.title.clone()
    };
    let when = relative_time(s.last_message_at.unwrap_or(s.created_at));
    let cls = if is_sel {
        "group flex w-full flex-col gap-0.5 rounded-md bg-accent px-2 py-1.5 text-left"
    } else {
        "group flex w-full flex-col gap-0.5 rounded-md px-2 py-1.5 text-left hover:bg-accent/40"
    };
    let (pin_slug, pin_id, pinned) = (slug.to_string(), s.id.clone(), s.pinned);
    let (arc_slug, arc_id, archived) = (slug.to_string(), s.id.clone(), s.archived);
    let pill = status_pill(s.status);
    let subagent = (!s.subagent_nickname.is_empty()).then(|| {
        let (h, sat, l) = hash_hsl(&s.subagent_nickname);
        (
            s.subagent_nickname.clone(),
            format!("background: hsl({h} {sat}% {l}% / 0.18); color: hsl({h} {sat}% 70%);"),
        )
    });

    rsx! {
        div {
            key: "{slug}/{s.id}",
            role: "button",
            class: "{cls}",
            onclick: move |_| {
                selected.set(Some((row_slug.clone(), row_session.clone())));
            },
            div { class: "flex items-center justify-between gap-2",
                div { class: "flex min-w-0 items-center gap-1.5",
                    if s.pinned {
                        Pin { size: 10 }
                    }
                    span { class: "truncate text-sm text-foreground", "{title}" }
                }
                div { class: "flex shrink-0 items-center gap-1",
                    button {
                        r#type: "button",
                        class: "hidden rounded p-0.5 text-muted-foreground hover:text-foreground group-hover:block",
                        title: if pinned { "Unpin" } else { "Pin" },
                        onclick: move |e| {
                            e.stop_propagation();
                            mutate((pin_slug.clone(), pin_id.clone(), SessionAction::Pin(!pinned)));
                        },
                        Pin { size: 11 }
                    }
                    button {
                        r#type: "button",
                        class: "hidden rounded p-0.5 text-muted-foreground hover:text-foreground group-hover:block",
                        title: if archived { "Unarchive" } else { "Archive" },
                        onclick: move |e| {
                            e.stop_propagation();
                            mutate((arc_slug.clone(), arc_id.clone(), SessionAction::Archive(!archived)));
                        },
                        Archive { size: 11 }
                    }
                    if let Some(p) = &pill {
                        span {
                            class: if p.pulse {
                                format!("h-2 w-2 shrink-0 rounded-full agents-dot-pulse {}", p.dot)
                            } else {
                                format!("h-2 w-2 shrink-0 rounded-full {}", p.dot)
                            },
                            title: "{p.label}",
                        }
                    }
                }
            }
            div { class: "flex items-center gap-1.5 text-[0.7rem] text-muted-foreground",
                span { class: backend_chip_cls(&s.backend_id), "{s.backend_id}" }
                if let Some((nick, style)) = &subagent {
                    span { class: "rounded-full px-1.5", style: "{style}", "{nick}" }
                }
                span { "{when}" }
                if s.archived {
                    span { class: "italic", "archived" }
                }
            }
        }
    }
}

/// Backend chip styling — Hermes gets the primary accent.
fn backend_chip_cls(backend_id: &str) -> &'static str {
    if backend_id == "hermes" {
        "rounded-full bg-primary/15 px-1.5 text-primary"
    } else {
        "rounded-full bg-muted/60 px-1.5"
    }
}

/// Live event-stream health chip state.
#[derive(Clone, PartialEq)]
enum StreamState {
    Connecting,
    Live,
    Dead(String),
}

/// One open conversation. Keyed by session id — remounting on
/// selection change gives each session its own subscription
/// lifecycle.
#[component]
fn ChatPane(
    slug: String,
    session: Session,
    models: Vec<ModelInfo>,
    skills: Vec<SkillInfo>,
    inspector_open: bool,
    on_toggle_inspector: EventHandler<()>,
    on_activity: EventHandler<()>,
) -> Element {
    let session_id = session.id.clone();
    let mut messages = use_signal(Vec::<Message>::new);
    let mut streaming = use_signal(|| None::<(String, String)>);
    let mut reasoning = use_signal(String::new);
    // Live activity for the CURRENT turn; folded into `turns` on
    // completion (keyed by the concluding assistant message id).
    let mut live_lines = use_signal(Vec::<ActivityLine>::new);
    let mut turns = use_signal(HashMap::<String, TurnLog>::new);
    let expanded_folds = use_signal(std::collections::HashSet::<String>::new);
    let mut error = use_signal(String::new);
    let mut busy = use_signal(|| matches!(session.status, SessionStatus::Running));
    let mut composer = use_signal(String::new);
    // Queue-while-busy sends (CodexMonitor's queue intent).
    let mut queued = use_signal(Vec::<String>::new);
    // Pending structured question (numbered-shortcut cards).
    let mut pending_question = use_signal(|| None::<QuestionRequest>);
    let mut completion_sel = use_signal(|| 0usize);
    let mut completion_dismissed = use_signal(String::new);
    let mut model = use_signal(String::new);
    let mut responding = use_signal(String::new);
    let mut stream_state = use_signal(|| StreamState::Connecting);
    let mut tokens = use_signal(|| (session.usage.input_tokens, session.usage.output_tokens));
    let mut turn_started = use_signal(|| None::<chrono::DateTime<chrono::Utc>>);
    let mut elapsed = use_signal(|| 0i64);

    // 1s ticker driving the "Working… 0:42" row.
    use_future(move || async move {
        loop {
            architect::platform::sleep(std::time::Duration::from_secs(1)).await;
            if let Some(t0) = *turn_started.peek() {
                elapsed.set((chrono::Utc::now() - t0).num_seconds().max(0));
            }
        }
    });

    // The send path — used by the composer, queue drain, and
    // question cards.
    let send_slug = slug.clone();
    let send_sid = session_id.clone();
    let dispatch_text = use_callback(move |text: String| {
        messages.write().push(Message {
            id: format!("local-{}", chrono::Utc::now().timestamp_micros()),
            session_id: send_sid.clone(),
            role: Role::User,
            content: vec![ContentBlock::Text { text: text.clone() }],
            partial: false,
            errored: false,
            error_text: String::new(),
            reasoning: None,
            created_at: chrono::Utc::now(),
        });
        busy.set(true);
        error.set(String::new());
        let slug = send_slug.clone();
        let sid = send_sid.clone();
        let model_override = model.peek().trim().to_string();
        spawn(async move {
            match crate::feeds::dispatch_agent_turn(&slug, &sid, &text, &model_override).await {
                Ok(ack) => {
                    let with = if ack.effective_model.is_empty() {
                        format!("{} · default model", ack.effective_backend_id)
                    } else {
                        format!("{} · {}", ack.effective_backend_id, ack.effective_model)
                    };
                    responding.set(with);
                }
                Err(e) => {
                    busy.set(false);
                    error.set(format!("Dispatch failed: {e}"));
                }
            }
        });
    });

    // Transcript load + live event pump, once per mounted session.
    let stream_slug = slug.clone();
    let stream_sid = session_id.clone();
    use_future(move || {
        let slug = stream_slug.clone();
        let sid = stream_sid.clone();
        async move {
            match crate::feeds::fetch_agent_messages(&slug, &sid).await {
                Ok(mut msgs) => {
                    msgs.reverse();
                    messages.set(msgs);
                }
                Err(e) => error.set(format!("Couldn't load the transcript: {e}")),
            }

            let (tx, mut rx) = vox::channel::<AgentEvent>();
            let sub_slug = slug.clone();
            let sub_sid = sid.clone();
            spawn(async move {
                let outcome = crate::feeds::subscribe_agent_session(&sub_slug, &sub_sid, tx).await;
                let msg = match outcome {
                    Ok(()) => "event stream closed by the server".to_string(),
                    Err(e) => e,
                };
                stream_state.set(StreamState::Dead(msg));
            });
            stream_state.set(StreamState::Live);
            while let Ok(Some(ev)) = rx.recv().await {
                match ev.get().clone() {
                    AgentEvent::TurnStarted { at, .. } => {
                        busy.set(true);
                        error.set(String::new());
                        reasoning.set(String::new());
                        live_lines.set(Vec::new());
                        turn_started.set(Some(at));
                        elapsed.set(0);
                        on_activity.call(());
                    }
                    AgentEvent::MessageWritten { message } => {
                        if streaming
                            .peek()
                            .as_ref()
                            .is_some_and(|(id, _)| *id == message.id)
                        {
                            streaming.set(None);
                        }
                        let mut list = messages.write();
                        if let Some(existing) = list.iter_mut().find(|m| m.id == message.id) {
                            *existing = message;
                        } else if matches!(message.role, Role::User)
                            && list.last().is_some_and(|m| {
                                m.id.starts_with("local-") && text_of(m) == text_of(&message)
                            })
                        {
                            *list.last_mut().expect("non-empty") = message;
                        } else {
                            list.push(message);
                        }
                    }
                    AgentEvent::MessageDelta {
                        message_id,
                        content_delta,
                        ..
                    } => {
                        let mut cur = streaming.write();
                        match cur.as_mut() {
                            Some((id, text)) if *id == message_id => text.push_str(&content_delta),
                            _ => *cur = Some((message_id, content_delta)),
                        }
                    }
                    AgentEvent::ReasoningDelta { delta, .. } => {
                        reasoning.write().push_str(&delta);
                    }
                    AgentEvent::ToolStarted { tool_call } => {
                        push_line(
                            &mut live_lines.write(),
                            ActivityLine {
                                tone: ToolTone::Running,
                                text: if tool_call.title.is_empty() {
                                    tool_call.name.clone()
                                } else {
                                    tool_call.title.clone()
                                },
                                tool_id: tool_call.id.clone(),
                            },
                        );
                    }
                    AgentEvent::ToolFinished { tool_call } => {
                        let ok = !matches!(tool_call.status, agent_proto::tool::ToolStatus::Error);
                        settle_tool(
                            &mut live_lines.write(),
                            &tool_call.id,
                            if tool_call.title.is_empty() {
                                tool_call.name.clone()
                            } else {
                                tool_call.title.clone()
                            },
                            ok,
                            tool_call.duration_ms,
                        );
                    }
                    AgentEvent::ToolProgress { preview, .. } => {
                        push_line(
                            &mut live_lines.write(),
                            ActivityLine {
                                tone: ToolTone::Note,
                                text: preview,
                                tool_id: String::new(),
                            },
                        );
                    }
                    AgentEvent::Warning { kind, message, .. } => {
                        push_line(
                            &mut live_lines.write(),
                            ActivityLine {
                                tone: ToolTone::Note,
                                text: format!("{kind}: {message}"),
                                tool_id: String::new(),
                            },
                        );
                    }
                    AgentEvent::QuestionAsked { request } => {
                        pending_question.set(Some(request));
                    }
                    AgentEvent::QuestionResolved { .. } => pending_question.set(None),
                    AgentEvent::Metering {
                        input_tokens,
                        output_tokens,
                        ..
                    } => tokens.set((input_tokens, output_tokens)),
                    AgentEvent::TurnFinished { message_id, at, .. } => {
                        // Fold the live work log behind its assistant
                        // message (t3code's turn fold).
                        let duration = turn_started
                            .peek()
                            .as_ref()
                            .map(|t0| (at - *t0).num_seconds().max(0))
                            .unwrap_or(0);
                        let lines = std::mem::take(&mut *live_lines.write());
                        let r = std::mem::take(&mut *reasoning.write());
                        if !lines.is_empty() || !r.is_empty() {
                            turns.write().insert(
                                message_id,
                                TurnLog {
                                    lines,
                                    reasoning: r,
                                    duration_secs: duration,
                                },
                            );
                        }
                        busy.set(false);
                        streaming.set(None);
                        responding.set(String::new());
                        turn_started.set(None);
                        on_activity.call(());
                        // Drain the queue.
                        let next = {
                            let mut q = queued.write();
                            if q.is_empty() { None } else { Some(q.remove(0)) }
                        };
                        if let Some(text) = next {
                            dispatch_text(text);
                        }
                    }
                    AgentEvent::TurnErrored { kind, message, .. } => {
                        busy.set(false);
                        streaming.set(None);
                        responding.set(String::new());
                        turn_started.set(None);
                        error.set(format!("{kind}: {message}"));
                        on_activity.call(());
                    }
                    AgentEvent::TurnCancelled { .. } => {
                        busy.set(false);
                        streaming.set(None);
                        responding.set(String::new());
                        turn_started.set(None);
                        push_line(
                            &mut live_lines.write(),
                            ActivityLine {
                                tone: ToolTone::Note,
                                text: "cancelled".to_string(),
                                tool_id: String::new(),
                            },
                        );
                        on_activity.call(());
                    }
                    AgentEvent::Resync => {
                        if let Ok(mut msgs) = crate::feeds::fetch_agent_messages(&slug, &sid).await
                        {
                            msgs.reverse();
                            messages.set(msgs);
                        }
                    }
                    _ => {}
                }
            }
        }
    });

    // Send-or-queue (CodexMonitor: the button morphs; typing while
    // the agent runs queues the message for the next turn).
    let send = use_callback(move |_: ()| {
        let text = composer.peek().trim().to_string();
        if text.is_empty() {
            return;
        }
        composer.set(String::new());
        if *busy.peek() {
            queued.write().push(text);
        } else {
            dispatch_text(text);
        }
    });

    let stop_slug = slug.clone();
    let stop_sid = session_id.clone();
    let on_stop = move |_| {
        let slug = stop_slug.clone();
        let sid = stop_sid.clone();
        spawn(async move {
            let _ = crate::feeds::cancel_agent_turn(&slug, &sid).await;
        });
    };

    // Answer a question card: dispatch the chosen option's label as
    // the user turn. Provisional until backends grow a first-class
    // answer verb — conversational agents handle it naturally.
    let answer_question = use_callback(move |label: String| {
        pending_question.set(None);
        dispatch_text(label);
    });

    let title = if session.title.trim().is_empty() {
        "(untitled)".to_string()
    } else {
        session.title.clone()
    };
    let streaming_view = streaming.read().clone();
    let reasoning_text = reasoning.read().clone();
    let live_view = live_lines.read().clone();
    let (tok_in, tok_out) = tokens();
    let stream = stream_state.read().clone();
    let session_models: Vec<ModelInfo> = models
        .iter()
        .filter(|m| m.backend_id == session.backend_id)
        .cloned()
        .collect();
    // Context ring: window of the selected (or default) model.
    let context_window = {
        let chosen = model.read().trim().to_string();
        session_models
            .iter()
            .find(|m| {
                if chosen.is_empty() {
                    m.is_default
                } else {
                    m.id == chosen
                }
            })
            .map(|m| m.context_length)
            .unwrap_or(0)
    };
    let ring = context_free_percent(tok_in, context_window);
    let ring_pct_used = ring.map(|f| 100.0 - f);

    // Derived transcript rows (pure).
    let derived_rows = timeline::build_rows(&messages.read(), &turns.read());
    let msgs_snapshot = messages.read().clone();
    let turns_snapshot = turns.read().clone();

    // Composer autocomplete (ranked: exact > prefix > substring >
    // subsequence).
    let completion: Option<(usize, Vec<CompletionRow>)> = {
        let text = composer.read().clone();
        completion_trigger(&text).and_then(|(mode, query, start)| {
            if *completion_dismissed.read() == text {
                return None;
            }
            let rows: Vec<CompletionRow> = match mode {
                '/' if session.backend_id == "hermes" => {
                    let all: Vec<CompletionRow> = HERMES_COMMANDS
                        .iter()
                        .map(|(c, d)| CompletionRow {
                            insert: (*c).to_string(),
                            label: (*c).to_string(),
                            detail: (*d).to_string(),
                        })
                        .collect();
                    rank_by(&all, &query, |r| r.label[1..].to_string(), 10)
                }
                '$' => {
                    let all: Vec<CompletionRow> = skills
                        .iter()
                        .filter(|sk| sk.enabled)
                        .map(|sk| CompletionRow {
                            insert: format!("${}", sk.name),
                            label: sk.name.clone(),
                            detail: sk.description.clone(),
                        })
                        .collect();
                    rank_by(&all, &query, |r| r.label.clone(), 8)
                }
                _ => Vec::new(),
            };
            (!rows.is_empty()).then_some((start, rows))
        })
    };
    let completion_open = completion.is_some();
    let completion_rows = completion
        .as_ref()
        .map(|(_, r)| r.clone())
        .unwrap_or_default();
    let completion_start = completion.as_ref().map(|(s, _)| *s).unwrap_or(0);
    let sel = completion_sel().min(completion_rows.len().saturating_sub(1));

    let accept = use_callback(move |(start, insert): (usize, String)| {
        let mut text = composer.peek().clone();
        text.truncate(start);
        text.push_str(&insert);
        text.push(' ');
        composer.set(text);
        completion_sel.set(0);
    });

    use_effect(move || {
        let _ = messages.read().len();
        let _ = streaming.read().is_some();
        let _ = live_lines.read().len();
        let _ = dioxus::document::eval(
            "const el = document.getElementById('agent-transcript'); if (el) el.scrollTop = el.scrollHeight;",
        );
    });

    let question_view = pending_question.read().clone();
    let queued_view = queued.read().clone();

    rsx! {
        div { class: "flex min-w-0 flex-1 flex-col",
            // Header.
            div { class: "flex items-center justify-between gap-3 border-b border-border/60 px-4 py-2.5",
                div { class: "flex min-w-0 items-center gap-2",
                    span { class: "truncate text-sm font-semibold", "{title}" }
                    span { class: backend_chip_cls(&session.backend_id), "{session.backend_id}" }
                    match &stream {
                        StreamState::Connecting => rsx! {
                            span { class: "rounded-full bg-muted/60 px-1.5 text-[0.7rem] text-muted-foreground",
                                "connecting…"
                            }
                        },
                        StreamState::Live => rsx! {
                            span {
                                class: "rounded-full bg-emerald-500/15 px-1.5 text-[0.7rem] text-emerald-500",
                                title: "Live event stream connected",
                                "● live"
                            }
                        },
                        StreamState::Dead(why) => rsx! {
                            span {
                                class: "rounded-full bg-destructive/15 px-1.5 text-[0.7rem] text-destructive",
                                title: "{why}",
                                "○ stream down — reselect the chat to reconnect"
                            }
                        },
                    }
                }
                div { class: "flex shrink-0 items-center gap-2",
                    if !responding.read().is_empty() {
                        span { class: "text-[0.7rem] text-muted-foreground", "{responding}" }
                    }
                    if busy() {
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: on_stop,
                            "Stop"
                        }
                    }
                    button {
                        r#type: "button",
                        class: if inspector_open {
                            "rounded-md bg-accent p-1 text-foreground"
                        } else {
                            "rounded-md p-1 text-muted-foreground hover:text-foreground"
                        },
                        title: "Toggle inspector",
                        onclick: move |_| on_toggle_inspector.call(()),
                        Info { size: 14 }
                    }
                }
            }

            // Transcript timeline (derived rows + live tail).
            div {
                id: "agent-transcript",
                class: "flex min-h-0 flex-1 flex-col gap-3 overflow-y-auto px-4 py-4",
                for row in derived_rows.iter() {
                    match row {
                        Row::Message(i) => rsx! {
                            if let Some(m) = msgs_snapshot.get(*i) {
                                {message_view(m)}
                            }
                        },
                        Row::TurnFold { anchor, tool_count, duration_secs } => rsx! {
                            {turn_fold_view(anchor, *tool_count, *duration_secs, &turns_snapshot, expanded_folds)}
                        },
                    }
                }
                // Live turn tail: reasoning, activity, streaming, timer.
                if !reasoning_text.is_empty() {
                    div { class: "border-l-2 border-primary/40 pl-3",
                        details { class: "text-xs text-muted-foreground", open: busy(),
                            summary { class: "cursor-pointer select-none font-medium",
                                if busy() {
                                    span { class: "mr-1 inline-block h-1.5 w-1.5 rounded-full bg-primary align-middle agents-dot-pulse" }
                                }
                                "Thinking"
                            }
                            pre { class: "mt-1 whitespace-pre-wrap font-sans leading-relaxed", "{reasoning_text}" }
                        }
                    }
                }
                if !live_view.is_empty() {
                    div { class: "flex flex-col gap-1",
                        for (i , line) in live_view.iter().enumerate() {
                            {activity_line_view(i, line)}
                        }
                    }
                }
                if let Some((_, text)) = &streaming_view {
                    div { class: "max-w-none",
                        task_ui::Markdown { source: text.clone() }
                        span { class: "ml-0.5 inline-block h-4 w-2 bg-primary/70 agents-dot-pulse" }
                    }
                }
                if busy() {
                    div { class: "flex items-center gap-2 text-sm text-muted-foreground",
                        Spinner { size: SpinnerSize::Small }
                        span { class: "tabular-nums", "{fmt_elapsed(elapsed())} Working…" }
                    }
                }
                if !error.read().is_empty() {
                    div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                        "{error}"
                    }
                }
            }

            // Pending question card (numbered-shortcut options).
            if let Some(q) = &question_view {
                {question_card(q, answer_question)}
            }

            // Queued sends.
            if !queued_view.is_empty() {
                div { class: "flex flex-wrap items-center gap-1.5 border-t border-border/40 px-4 pt-2",
                    span { class: "text-[0.7rem] text-muted-foreground", "Queued:" }
                    for (i , q) in queued_view.iter().enumerate() {
                        span {
                            key: "{i}",
                            class: "flex max-w-64 items-center gap-1 rounded-full bg-muted/50 px-2 py-0.5 text-xs",
                            span { class: "truncate", "{q}" }
                            button {
                                r#type: "button",
                                class: "text-muted-foreground hover:text-foreground",
                                onclick: move |_| {
                                    queued.write().remove(i);
                                },
                                "×"
                            }
                        }
                    }
                }
            }

            // Composer + chip row.
            div { class: "relative border-t border-border/60 px-4 py-3",
                if completion_open {
                    div { class: "absolute bottom-full left-4 z-30 mb-1 max-h-64 w-[26rem] max-w-[85%] overflow-y-auto rounded-lg border border-border bg-popover p-1 shadow-lg",
                        for (i , row) in completion_rows.iter().enumerate() {
                            {
                                let insert = row.insert.clone();
                                rsx! {
                                    div {
                                        key: "{row.insert}",
                                        role: "button",
                                        class: if i == sel {
                                            "flex cursor-pointer items-baseline gap-2 rounded-md bg-accent px-2 py-1"
                                        } else {
                                            "flex cursor-pointer items-baseline gap-2 rounded-md px-2 py-1 hover:bg-accent/50"
                                        },
                                        onmousedown: move |e| {
                                            e.prevent_default();
                                            accept((completion_start, insert.clone()));
                                        },
                                        span { class: "shrink-0 font-mono text-xs font-semibold text-foreground", "{row.label}" }
                                        span { class: "truncate text-[0.72rem] text-muted-foreground", "{row.detail}" }
                                    }
                                }
                            }
                        }
                        div { class: "mt-0.5 border-t border-border/40 px-2 pt-1 text-[0.65rem] text-muted-foreground/70",
                            "↑↓ navigate · Tab/Enter accept · Esc dismiss"
                        }
                    }
                }
                div { class: "flex items-end gap-2",
                    textarea {
                        class: "max-h-40 min-h-[2.5rem] w-full flex-1 resize-y rounded-xl border border-border/70 bg-card/30 px-3 py-2 text-sm leading-relaxed text-foreground outline-none focus:border-primary/60",
                        placeholder: "Message the agent… (/ commands, $ skills, Enter to send)",
                        value: "{composer}",
                        oninput: move |e| {
                            composer.set(e.value());
                            completion_sel.set(0);
                        },
                        onkeydown: {
                            let rows = completion_rows.clone();
                            move |e| {
                                if completion_open && !rows.is_empty() {
                                    match e.key() {
                                        Key::ArrowDown => {
                                            e.prevent_default();
                                            completion_sel.set((sel + 1) % rows.len());
                                            return;
                                        }
                                        Key::ArrowUp => {
                                            e.prevent_default();
                                            completion_sel.set(sel.checked_sub(1).unwrap_or(rows.len() - 1));
                                            return;
                                        }
                                        Key::Tab => {
                                            e.prevent_default();
                                            accept((completion_start, rows[sel].insert.clone()));
                                            return;
                                        }
                                        Key::Enter if !e.modifiers().shift() => {
                                            e.prevent_default();
                                            accept((completion_start, rows[sel].insert.clone()));
                                            return;
                                        }
                                        Key::Escape => {
                                            e.prevent_default();
                                            completion_dismissed.set(composer.peek().clone());
                                            return;
                                        }
                                        _ => {}
                                    }
                                }
                                if e.key() == Key::Enter && !e.modifiers().shift() {
                                    e.prevent_default();
                                    send(());
                                }
                            }
                        },
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        disabled: composer.read().trim().is_empty(),
                        on_click: move |_| send(()),
                        if busy() { "Queue" } else { "Send" }
                    }
                }
                div { class: "mt-1.5 flex items-center gap-2",
                    select {
                        class: "w-52 rounded-md border border-border/60 bg-card/30 px-2 py-0.5 text-xs text-foreground outline-none focus:border-primary/60",
                        value: "{model}",
                        onchange: move |e| model.set(e.value()),
                        option { value: "", "default model" }
                        for m in session_models.iter() {
                            option {
                                key: "{m.id}",
                                value: "{m.id}",
                                if m.label.is_empty() {
                                    "{m.id}"
                                } else {
                                    "{m.label}"
                                }
                            }
                        }
                    }
                    // Context gauge: conic-gradient ring when the
                    // window is known, raw counter otherwise.
                    div { class: "ml-auto flex items-center gap-1.5 text-[0.7rem] tabular-nums text-muted-foreground",
                        if let (Some(free), Some(used_pct)) = (ring, ring_pct_used) {
                            span {
                                class: "inline-block h-5 w-5 rounded-full",
                                style: "{context_ring_style(free)}",
                                title: "{used_pct:.0}% of context used — {fmt_tokens(tok_in)} of {fmt_tokens(context_window)} tokens",
                            }
                        }
                        span {
                            title: "context (input) / generated (output) tokens",
                            if tok_in + tok_out > 0 {
                                "{fmt_tokens(tok_in)} ctx · {fmt_tokens(tok_out)} out"
                            } else {
                                "no usage yet"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A folded completed turn: one-line summary, expandable to the
/// retained activity log + reasoning.
fn turn_fold_view(
    anchor: &str,
    tool_count: usize,
    duration_secs: i64,
    turns: &HashMap<String, TurnLog>,
    mut expanded: Signal<std::collections::HashSet<String>>,
) -> Element {
    let is_open = expanded.read().contains(anchor);
    let key = anchor.to_string();
    let summary = fold_summary(tool_count, duration_secs);
    let log = turns.get(anchor).cloned().unwrap_or_default();

    rsx! {
        div { key: "fold-{anchor}", class: "flex flex-col gap-1",
            button {
                r#type: "button",
                class: "flex w-fit items-center gap-1.5 rounded-md px-1.5 py-0.5 text-[0.72rem] text-muted-foreground hover:bg-muted/40 hover:text-foreground",
                onclick: move |_| {
                    let mut set = expanded.write();
                    if !set.remove(&key) {
                        set.insert(key.clone());
                    }
                },
                span { class: if is_open { "inline-block rotate-90 transition-transform" } else { "inline-block transition-transform" }, "›" }
                span { "{summary}" }
            }
            if is_open {
                div { class: "flex flex-col gap-1 border-l border-border/40 pl-3",
                    if !log.reasoning.is_empty() {
                        details { class: "text-xs text-muted-foreground",
                            summary { class: "cursor-pointer select-none font-medium", "Thinking" }
                            pre { class: "mt-1 whitespace-pre-wrap font-sans leading-relaxed", "{log.reasoning}" }
                        }
                    }
                    for (i , line) in log.lines.iter().enumerate() {
                        {activity_line_view(i, line)}
                    }
                }
            }
        }
    }
}

/// One activity line: status glyph + mono text (t3code's
/// `SimpleWorkEntryRow` glyph state machine, compact form).
fn activity_line_view(i: usize, line: &ActivityLine) -> Element {
    let (glyph, glyph_cls) = match line.tone {
        ToolTone::Running => ("▸", "text-primary"),
        ToolTone::Ok => ("✓", "text-emerald-500"),
        ToolTone::Fail => ("✗", "text-destructive"),
        ToolTone::Note => ("·", "text-muted-foreground/70"),
    };
    rsx! {
        div {
            key: "{i}",
            class: "flex w-fit max-w-full items-baseline gap-1.5 rounded-md bg-muted/40 px-2 py-0.5 font-mono text-[0.72rem] text-muted-foreground",
            span { class: "{glyph_cls}", "{glyph}" }
            span { class: "truncate", title: "{line.text}", "{line.text}" }
        }
    }
}

/// A pending structured question: numbered option cards (t3code's
/// `ComposerPendingUserInputPanel`, compact form). Answering
/// dispatches the option label as the user's turn.
fn question_card(q: &QuestionRequest, answer: Callback<String>) -> Element {
    let Some(first) = q.questions.first() else {
        return rsx! {};
    };
    let total = q.questions.len();

    rsx! {
        div { class: "border-t border-border/60 bg-card/40 px-4 py-3",
            div { class: "mb-2 flex items-center gap-2",
                if !first.header.is_empty() {
                    span { class: "rounded-full bg-primary/15 px-2 py-0.5 text-[0.7rem] font-medium text-primary",
                        "{first.header}"
                    }
                }
                span { class: "text-sm font-medium", "{first.text}" }
                if total > 1 {
                    span { class: "ml-auto rounded-full bg-muted/60 px-2 py-0.5 text-[0.7rem] text-muted-foreground",
                        "1/{total}"
                    }
                }
            }
            div { class: "flex flex-col gap-1",
                for (i , opt) in first.options.iter().enumerate().take(9) {
                    {
                        let label = opt.label.clone();
                        rsx! {
                            button {
                                key: "{opt.label}",
                                r#type: "button",
                                class: "flex w-full items-baseline gap-2 rounded-lg border border-border/60 px-3 py-1.5 text-left hover:border-primary/60 hover:bg-primary/5",
                                onclick: move |_| answer(label.clone()),
                                kbd { class: "rounded border border-border/60 bg-muted/40 px-1 font-mono text-[0.7rem] text-muted-foreground",
                                    "{i + 1}"
                                }
                                span { class: "text-sm", "{opt.label}" }
                                if !opt.description.is_empty() {
                                    span { class: "truncate text-xs text-muted-foreground", "{opt.description}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Right panel: session meta + actions, skills, capabilities.
#[component]
fn Inspector(
    slug: String,
    session: Session,
    skills: Vec<SkillInfo>,
    capabilities: Vec<CapabilityFlag>,
    mutate: Callback<(String, String, SessionAction)>,
) -> Element {
    let mut title_draft = use_signal(|| session.title.clone());
    let mut confirm_delete = use_signal(|| false);
    let created = session.created_at.format("%b %-d, %-I:%M %p").to_string();
    let tokens = session.usage.input_tokens + session.usage.output_tokens;
    let sid = session.id.clone();
    let sslug = slug.clone();

    rsx! {
        div { class: "flex w-72 shrink-0 flex-col gap-4 overflow-y-auto border-l border-border/60 px-3 py-3",
            div { class: "flex flex-col gap-2",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.15em] text-muted-foreground",
                    "Session"
                }
                div { class: "flex items-center gap-1.5",
                    input {
                        class: "w-full rounded-md border border-border/60 bg-card/30 px-2 py-1 text-xs text-foreground outline-none focus:border-primary/60",
                        value: "{title_draft}",
                        placeholder: "Title",
                        oninput: move |e| title_draft.set(e.value()),
                    }
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: {
                            let (s, id) = (sslug.clone(), sid.clone());
                            move |_| {
                                mutate((s.clone(), id.clone(), SessionAction::Rename(title_draft.peek().clone())));
                            }
                        },
                        "Save"
                    }
                }
                div { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                    div { class: "flex justify-between",
                        span { "Backend" }
                        span { class: backend_chip_cls(&session.backend_id), "{session.backend_id}" }
                    }
                    div { class: "flex justify-between",
                        span { "Created" }
                        span { "{created}" }
                    }
                    div { class: "flex justify-between",
                        span { "Tokens" }
                        span { class: "tabular-nums", "{fmt_tokens(tokens)}" }
                    }
                    div { class: "flex justify-between",
                        span { "Status" }
                        span { "{status_text(session.status)}" }
                    }
                }
                div { class: "flex items-center gap-1.5",
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: {
                            let (s, id, v) = (sslug.clone(), sid.clone(), session.pinned);
                            move |_| mutate((s.clone(), id.clone(), SessionAction::Pin(!v)))
                        },
                        if session.pinned { "Unpin" } else { "Pin" }
                    }
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: {
                            let (s, id, v) = (sslug.clone(), sid.clone(), session.archived);
                            move |_| mutate((s.clone(), id.clone(), SessionAction::Archive(!v)))
                        },
                        if session.archived { "Unarchive" } else { "Archive" }
                    }
                    button {
                        r#type: "button",
                        class: "ml-auto flex items-center gap-1 rounded-md px-1.5 py-1 text-xs text-destructive hover:bg-destructive/10",
                        onclick: {
                            let (s, id) = (sslug.clone(), sid.clone());
                            move |_| {
                                if *confirm_delete.peek() {
                                    mutate((s.clone(), id.clone(), SessionAction::Delete));
                                } else {
                                    confirm_delete.set(true);
                                }
                            }
                        },
                        Trash2 { size: 12 }
                        if confirm_delete() { "Really delete?" } else { "Delete" }
                    }
                }
            }

            div { class: "flex flex-col gap-1.5",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.15em] text-muted-foreground",
                    "Skills ({skills.len()})"
                }
                if skills.is_empty() {
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "No skills reported — the agent learns them over time (`/learn`)."
                    }
                }
                for sk in skills.iter() {
                    div { key: "{sk.backend_id}/{sk.name}", class: "rounded-md border border-border/50 bg-card/30 px-2 py-1.5",
                        div { class: "flex items-center justify-between gap-2",
                            span { class: "truncate text-xs font-medium text-foreground", "{sk.name}" }
                            if !sk.enabled {
                                span { class: "text-[0.65rem] text-muted-foreground", "off" }
                            }
                        }
                        if !sk.description.is_empty() {
                            p { class: "mt-0.5 line-clamp-2 text-[0.7rem] leading-snug text-muted-foreground",
                                "{sk.description}"
                            }
                        }
                    }
                }
            }

            if !capabilities.is_empty() {
                div { class: "flex flex-col gap-1",
                    span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.15em] text-muted-foreground",
                        "Capabilities"
                    }
                    div { class: "flex flex-wrap gap-1",
                        for c in capabilities.iter() {
                            span {
                                key: "{c.backend_id}/{c.name}",
                                class: if c.enabled {
                                    "rounded-full bg-emerald-500/10 px-1.5 py-0.5 text-[0.65rem] text-emerald-500"
                                } else {
                                    "rounded-full bg-muted/40 px-1.5 py-0.5 text-[0.65rem] text-muted-foreground line-through"
                                },
                                "{c.name}"
                            }
                        }
                    }
                }
            }
        }
    }
}

fn status_text(status: SessionStatus) -> &'static str {
    match status {
        SessionStatus::Idle => "Idle",
        SessionStatus::Running => "Running",
        SessionStatus::AwaitingUser => "Awaiting user",
        SessionStatus::Cancelled => "Cancelled",
        SessionStatus::Errored => "Errored",
    }
}

/// Concatenated text content of a message.
fn text_of(m: &Message) -> String {
    m.content
        .iter()
        .filter_map(|b| match b {
            ContentBlock::Text { text } => Some(text.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Copy text to the clipboard via the browser API.
fn copy_text(text: &str) {
    let js = format!(
        "navigator.clipboard && navigator.clipboard.writeText({});",
        serde_json::to_string(text).unwrap_or_default()
    );
    let _ = dioxus::document::eval(&js);
}

/// One transcript entry. Assistant messages get a hover copy
/// button; user messages are right-aligned bubbles.
fn message_view(m: &Message) -> Element {
    let text = text_of(m);
    if m.errored {
        return rsx! {
            div { key: "{m.id}", class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                "{m.error_text}"
            }
        };
    }
    match m.role {
        Role::User => rsx! {
            div { key: "{m.id}", class: "flex justify-end",
                div { class: "max-w-[85%] whitespace-pre-wrap rounded-2xl rounded-br-sm bg-primary/15 px-3.5 py-2 text-sm leading-relaxed",
                    "{text}"
                }
            }
        },
        Role::Assistant => {
            let copy_source = text.clone();
            rsx! {
                div { key: "{m.id}", class: "group relative max-w-none",
                    task_ui::Markdown { source: text }
                    button {
                        r#type: "button",
                        class: "absolute -top-1 right-0 hidden rounded-md border border-border/60 bg-card/80 p-1 text-muted-foreground hover:text-foreground group-hover:block",
                        title: "Copy message",
                        onclick: move |_| copy_text(&copy_source),
                        Copy { size: 12 }
                    }
                }
            }
        }
        Role::System | Role::Tool => rsx! {
            div { key: "{m.id}", class: "text-xs italic text-muted-foreground", "{text}" }
        },
    }
}
