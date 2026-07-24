//! `/agents` — the agent command center.
//!
//! Three panes, CodexMonitor-style:
//!
//! - **Session rail** — searchable, pinned-first conversation list
//!   with hover actions (pin/archive), status dots, relative times,
//!   and a New-chat button with a backend picker.
//! - **Chat pane** — the transcript timeline: user bubbles,
//!   markdown assistant messages, dim reasoning blocks with a live
//!   accent bar, one-line mono activity pills for tool events, a
//!   ticking "Working…" row, and a composer with a model chip fed
//!   by live [`Discovery`] data plus a context-token gauge.
//! - **Inspector** — a collapsible right panel: session meta +
//!   rename/pin/archive/delete, the agent's skill library, and
//!   backend capability flags.
//!
//! Everything binds the agent-proto services (`Sessions`,
//! `TurnDispatch`, `Threads`, `Subscriptions`, `Discovery`) served
//! by the org's `AgentRouter` (Hermes gateway + Codex).

use agent_proto::event::AgentEvent;
use agent_proto::message::{ContentBlock, Message, Role};
use agent_proto::service::discovery::{CapabilityFlag, ModelInfo, SkillInfo};
use agent_proto::session::{Session, SessionStatus};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Archive, Bot, Info, Pin, Trash2};
use fts_ui::prelude::*;

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

    // The open conversation: (org slug, session).
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

    // Filter + order the rail: search, archived toggle, pinned first,
    // then newest activity.
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

    // Session mutations shared by rail hover-actions and inspector.
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
                // Keep the open pane coherent with the mutation.
                let deleted = matches!(
                    selected.peek().as_ref(),
                    Some((_, s)) if s.id == id
                );
                if deleted {
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

/// Rail/inspector session mutations.
#[derive(Clone, PartialEq)]
enum SessionAction {
    Pin(bool),
    Archive(bool),
    Rename(String),
    Delete,
}

/// One conversation row: status dot, title, backend chip, relative
/// time, hover actions (pin / archive).
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
    let status_dot = match s.status {
        SessionStatus::Running => Some("h-2 w-2 shrink-0 animate-pulse rounded-full bg-primary"),
        SessionStatus::Errored => Some("h-2 w-2 shrink-0 rounded-full bg-destructive"),
        SessionStatus::AwaitingUser => Some("h-2 w-2 shrink-0 rounded-full bg-amber-500"),
        _ => None,
    };

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
                    // Hover actions.
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
                    if let Some(dot) = status_dot {
                        span { class: "{dot}" }
                    }
                }
            }
            div { class: "flex items-center gap-1.5 text-[0.7rem] text-muted-foreground",
                span { class: backend_chip_cls(&s.backend_id), "{s.backend_id}" }
                span { "{when}" }
                if s.archived {
                    span { class: "italic", "archived" }
                }
            }
        }
    }
}

/// "44m" / "11h" / "4d" style relative timestamps for the rail.
fn relative_time(t: chrono::DateTime<chrono::Utc>) -> String {
    let secs = (chrono::Utc::now() - t).num_seconds().max(0);
    match secs {
        0..=59 => "now".to_string(),
        60..=3599 => format!("{}m", secs / 60),
        3600..=86_399 => format!("{}h", secs / 3600),
        _ => format!("{}d", secs / 86_400),
    }
}

/// Backend chip styling — Hermes gets the primary accent (it's the
/// conversational agent), everything else stays neutral.
fn backend_chip_cls(backend_id: &str) -> &'static str {
    if backend_id == "hermes" {
        "rounded-full bg-primary/15 px-1.5 text-primary"
    } else {
        "rounded-full bg-muted/60 px-1.5"
    }
}

/// Live event-stream health, surfaced as a chip in the chat
/// header — a dead stream is the difference between "the agent is
/// thinking" and "you will never hear back", so it must be visible.
#[derive(Clone, PartialEq)]
enum StreamState {
    Connecting,
    Live,
    Dead(String),
}

/// One open conversation: transcript + live stream + composer.
/// Keyed by session id — remounting on selection change gives each
/// session its own subscription lifecycle.
#[component]
fn ChatPane(
    slug: String,
    session: Session,
    models: Vec<ModelInfo>,
    inspector_open: bool,
    on_toggle_inspector: EventHandler<()>,
    on_activity: EventHandler<()>,
) -> Element {
    let session_id = session.id.clone();
    // Chronological transcript (server returns newest-first).
    let mut messages = use_signal(Vec::<Message>::new);
    // In-flight assistant message: (message id, text so far).
    let mut streaming = use_signal(|| None::<(String, String)>);
    let mut reasoning = use_signal(String::new);
    // One-line activity pills for the current turn (tool events,
    // backend warnings) — CodexMonitor's command-chip stream.
    let mut activity = use_signal(Vec::<String>::new);
    let mut error = use_signal(String::new);
    let mut busy = use_signal(|| matches!(session.status, SessionStatus::Running));
    let mut composer = use_signal(String::new);
    // Per-turn model override; empty = the backend's default. The
    // ack's effective model is echoed back into `responding`.
    let mut model = use_signal(String::new);
    let mut responding = use_signal(String::new);
    let mut stream_state = use_signal(|| StreamState::Connecting);
    // Cumulative token metering (live ticks); .0=input .1=output.
    let mut tokens = use_signal(|| (session.usage.input_tokens, session.usage.output_tokens));
    // Working-timer: seconds since TurnStarted while busy.
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
                        activity.set(Vec::new());
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
                        push_activity(&mut activity, format!("▸ {}", tool_call.title));
                    }
                    AgentEvent::ToolFinished { tool_call } => {
                        push_activity(&mut activity, format!("✓ {}", tool_call.title));
                    }
                    AgentEvent::ToolProgress { preview, .. } => {
                        push_activity(&mut activity, preview);
                    }
                    AgentEvent::Warning { kind, message, .. } => {
                        push_activity(&mut activity, format!("{kind}: {message}"));
                    }
                    AgentEvent::Metering {
                        input_tokens,
                        output_tokens,
                        ..
                    } => tokens.set((input_tokens, output_tokens)),
                    AgentEvent::TurnFinished { .. } => {
                        busy.set(false);
                        streaming.set(None);
                        responding.set(String::new());
                        turn_started.set(None);
                        on_activity.call(());
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
                        push_activity(&mut activity, "cancelled".to_string());
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

    let send_slug = slug.clone();
    let send_sid = session_id.clone();
    let send = use_callback(move |_: ()| {
        let text = composer.peek().trim().to_string();
        if text.is_empty() || *busy.peek() {
            return;
        }
        composer.set(String::new());
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

    let stop_slug = slug.clone();
    let stop_sid = session_id.clone();
    let on_stop = move |_| {
        let slug = stop_slug.clone();
        let sid = stop_sid.clone();
        spawn(async move {
            let _ = crate::feeds::cancel_agent_turn(&slug, &sid).await;
        });
    };

    let title = if session.title.trim().is_empty() {
        "(untitled)".to_string()
    } else {
        session.title.clone()
    };
    let streaming_view = streaming.read().clone();
    let reasoning_text = reasoning.read().clone();
    let activity_view = activity.read().clone();
    let (tok_in, tok_out) = tokens();
    let stream = stream_state.read().clone();
    // Only models for this session's backend in the chip.
    let session_models: Vec<ModelInfo> = models
        .iter()
        .filter(|m| m.backend_id == session.backend_id)
        .cloned()
        .collect();

    use_effect(move || {
        let _ = messages.read().len();
        let _ = streaming.read().is_some();
        let _ = activity.read().len();
        let _ = dioxus::document::eval(
            "const el = document.getElementById('agent-transcript'); if (el) el.scrollTop = el.scrollHeight;",
        );
    });

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

            // Transcript timeline.
            div {
                id: "agent-transcript",
                class: "flex min-h-0 flex-1 flex-col gap-3 overflow-y-auto px-4 py-4",
                for m in messages.read().iter() {
                    {message_view(m)}
                }
                if !reasoning_text.is_empty() {
                    div { class: "border-l-2 border-primary/40 pl-3",
                        details { class: "text-xs text-muted-foreground", open: busy(),
                            summary { class: "cursor-pointer select-none font-medium",
                                if busy() {
                                    span { class: "mr-1 inline-block h-1.5 w-1.5 animate-pulse rounded-full bg-primary align-middle" }
                                }
                                "Thinking"
                            }
                            pre { class: "mt-1 whitespace-pre-wrap font-sans leading-relaxed", "{reasoning_text}" }
                        }
                    }
                }
                if !activity_view.is_empty() {
                    div { class: "flex flex-col gap-1",
                        for (i , line) in activity_view.iter().enumerate() {
                            div {
                                key: "{i}",
                                class: "w-fit max-w-full truncate rounded-md bg-muted/40 px-2 py-0.5 font-mono text-[0.72rem] text-muted-foreground",
                                "{line}"
                            }
                        }
                    }
                }
                if let Some((_, text)) = &streaming_view {
                    div { class: "max-w-none",
                        task_ui::Markdown { source: text.clone() }
                        span { class: "ml-0.5 inline-block h-4 w-2 animate-pulse bg-primary/70" }
                    }
                }
                if busy() {
                    div { class: "flex items-center gap-2 text-sm text-muted-foreground",
                        Spinner { size: SpinnerSize::Small }
                        span { "{fmt_elapsed(elapsed())} Working…" }
                    }
                }
                if !error.read().is_empty() {
                    div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                        "{error}"
                    }
                }
            }

            // Composer + chip row.
            div { class: "border-t border-border/60 px-4 py-3",
                div { class: "flex items-end gap-2",
                    textarea {
                        class: "max-h-40 min-h-[2.5rem] w-full flex-1 resize-y rounded-xl border border-border/70 bg-card/30 px-3 py-2 text-sm leading-relaxed text-foreground outline-none focus:border-primary/60",
                        placeholder: "Message the agent… (Enter to send, Shift+Enter for newline)",
                        value: "{composer}",
                        oninput: move |e| composer.set(e.value()),
                        onkeydown: move |e| {
                            if e.key() == Key::Enter && !e.modifiers().shift() {
                                e.prevent_default();
                                send(());
                            }
                        },
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        disabled: busy() || composer.read().trim().is_empty(),
                        on_click: move |_| send(()),
                        "Send"
                    }
                }
                div { class: "mt-1.5 flex items-center gap-2",
                    // Model chip — live Discovery data for this backend.
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
                    // Context gauge (right-aligned): tokens in/out.
                    span { class: "ml-auto text-[0.7rem] tabular-nums text-muted-foreground",
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
            // ── Session ──
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
                    div { class: "flex justify-between", span { "Backend" } span { class: backend_chip_cls(&session.backend_id), "{session.backend_id}" } }
                    div { class: "flex justify-between", span { "Created" } span { "{created}" } }
                    div { class: "flex justify-between", span { "Tokens" } span { class: "tabular-nums", "{fmt_tokens(tokens)}" } }
                    div { class: "flex justify-between", span { "Status" } span { "{status_label(session.status)}" } }
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

            // ── Skills ──
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

            // ── Capabilities ──
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

/// Append an activity pill, deduping immediate repeats and capping
/// the per-turn list.
fn push_activity(activity: &mut Signal<Vec<String>>, line: String) {
    let mut list = activity.write();
    if list.last() == Some(&line) {
        return;
    }
    list.push(line);
    let overflow = list.len().saturating_sub(200);
    if overflow > 0 {
        list.drain(..overflow);
    }
}

fn fmt_elapsed(secs: i64) -> String {
    format!("{}:{:02}", secs / 60, secs % 60)
}

fn fmt_tokens(n: u64) -> String {
    if n >= 1000 {
        format!("{:.1}k", n as f64 / 1000.0)
    } else {
        n.to_string()
    }
}

fn status_label(status: SessionStatus) -> &'static str {
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

/// One transcript entry. User messages are right-aligned bubbles;
/// assistant messages render as markdown; system notes + errors get
/// muted/destructive styling.
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
        Role::Assistant => rsx! {
            div { key: "{m.id}", class: "max-w-none",
                task_ui::Markdown { source: text }
            }
        },
        Role::System | Role::Tool => rsx! {
            div { key: "{m.id}", class: "text-xs italic text-muted-foreground", "{text}" }
        },
    }
}
