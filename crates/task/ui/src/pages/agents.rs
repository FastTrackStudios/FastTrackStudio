//! `/agents` — chat with the org's agents.
//!
//! Master-detail: the left rail lists the conversations hosted by
//! each org's agent backends (Hermes gateway + in-process Codex),
//! fetched over the architect-generated `SessionsClient`; selecting
//! one (or starting a new chat) opens the CHAT PANE — a composer
//! that dispatches turns (`TurnDispatchClient`) and a live
//! transcript fed by the session's `AgentEvent` stream
//! (`SubscriptionsClient::subscribe_session`): message deltas render
//! as they arrive, reasoning streams into a collapsible block, tool
//! progress ticks the status line.
//!
//! The default backend is the server's choice — Hermes when a
//! gateway is configured (`TASK_HERMES_URL`), else Codex.

use agent_proto::event::AgentEvent;
use agent_proto::message::{ContentBlock, Message, Role};
use agent_proto::session::{Session, SessionStatus};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Bot;
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

    // The open conversation: (org slug, session).
    let mut selected = use_signal(|| None::<(String, Session)>);
    let mut create_error = use_signal(String::new);

    let active = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let on_new_chat = move |_| {
        let Some(slug) = active() else { return };
        spawn(async move {
            match crate::feeds::create_agent_session(&slug, "", "").await {
                Ok(session) => {
                    create_error.set(String::new());
                    selected.set(Some((slug, session)));
                    sessions.restart();
                }
                Err(e) => create_error.set(e),
            }
        });
    };

    let rows: Vec<(String, Session)> = match &*sessions.read_unchecked() {
        Some(Ok(rows)) => {
            let mut rows = rows.clone();
            rows.sort_by(|(_, a), (_, b)| {
                let ka = a.last_message_at.unwrap_or(a.created_at);
                let kb = b.last_message_at.unwrap_or(b.created_at);
                kb.cmp(&ka)
            });
            rows
        }
        _ => Vec::new(),
    };
    let fetch_err = match &*sessions.read_unchecked() {
        Some(Err(e)) => e.clone(),
        _ => String::new(),
    };
    let selected_id = selected.read().as_ref().map(|(_, s)| s.id.clone());

    rsx! {
        div { class: "flex h-full min-h-0 w-full",
            // ── Session rail ──
            div { class: "flex w-72 shrink-0 flex-col border-r border-border/60",
                div { class: "flex items-center justify-between gap-2 px-3 py-3",
                    div { class: "flex items-center gap-2",
                        Bot { size: 18 }
                        span { class: "text-sm font-semibold", "Agents" }
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        disabled: active().is_none(),
                        on_click: on_new_chat,
                        "New chat"
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
                div { class: "min-h-0 flex-1 overflow-y-auto px-1.5 pb-2",
                    if rows.is_empty() && fetch_err.is_empty() {
                        div { class: "flex flex-col items-center gap-2 px-4 py-10 text-center",
                            Bot { size: 24 }
                            Text { variant: TextVariant::Muted, class: "text-xs",
                                "No conversations yet — start one with New chat."
                            }
                        }
                    }
                    for (slug , s) in rows.iter() {
                        {
                            let is_sel = selected_id.as_deref() == Some(s.id.as_str());
                            let row_slug = slug.clone();
                            let row_session = s.clone();
                            let title = if s.title.trim().is_empty() {
                                "(untitled)".to_string()
                            } else {
                                s.title.clone()
                            };
                            let when = s
                                .last_message_at
                                .unwrap_or(s.created_at)
                                .format("%b %-d, %-I:%M %p")
                                .to_string();
                            let cls = if is_sel {
                                "flex w-full flex-col gap-0.5 rounded-md bg-accent px-2 py-1.5 text-left"
                            } else {
                                "flex w-full flex-col gap-0.5 rounded-md px-2 py-1.5 text-left hover:bg-accent/40"
                            };
                            rsx! {
                                button {
                                    key: "{slug}/{s.id}",
                                    r#type: "button",
                                    class: "{cls}",
                                    onclick: move |_| {
                                        selected.set(Some((row_slug.clone(), row_session.clone())));
                                    },
                                    div { class: "flex items-center justify-between gap-2",
                                        span { class: "truncate text-sm text-foreground", "{title}" }
                                        if matches!(s.status, SessionStatus::Running) {
                                            span { class: "h-2 w-2 shrink-0 animate-pulse rounded-full bg-primary" }
                                        }
                                    }
                                    div { class: "flex items-center gap-1.5 text-[0.7rem] text-muted-foreground",
                                        span { class: backend_chip_cls(&s.backend_id), "{s.backend_id}" }
                                        span { "{when}" }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // ── Chat pane ──
            if let Some((slug, session)) = selected.read().clone() {
                ChatPane {
                    key: "{session.id}",
                    slug,
                    session,
                    // Turn lifecycle changes refresh the rail (status
                    // pulses, timestamps, token counts).
                    on_activity: move |()| sessions.restart(),
                }
            } else {
                div { class: "flex flex-1 flex-col items-center justify-center gap-3 text-center",
                    Bot { size: 32 }
                    Heading { level: HeadingLevel::H3, "Chat with your agents" }
                    Text { variant: TextVariant::Muted, class: "max-w-sm text-sm",
                        "Pick a conversation, or start a new chat — Hermes answers when a gateway is configured, Codex otherwise."
                    }
                }
            }
        }
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
fn ChatPane(slug: String, session: Session, on_activity: EventHandler<()>) -> Element {
    let session_id = session.id.clone();
    // Chronological transcript (server returns newest-first).
    let mut messages = use_signal(Vec::<Message>::new);
    // In-flight assistant message: (message id, text so far).
    let mut streaming = use_signal(|| None::<(String, String)>);
    let mut reasoning = use_signal(String::new);
    let mut status = use_signal(String::new);
    let mut error = use_signal(String::new);
    let mut busy = use_signal(|| matches!(session.status, SessionStatus::Running));
    let mut composer = use_signal(String::new);
    // Per-turn model override; empty = the backend's default. The
    // ack's effective model is echoed back into `responding`.
    let mut model = use_signal(String::new);
    let mut responding = use_signal(String::new);
    let mut stream_state = use_signal(|| StreamState::Connecting);
    // Cumulative token metering for this session (live ticks).
    let mut tokens = use_signal(|| (session.usage.input_tokens, session.usage.output_tokens));

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
                // The call returns only when the stream ends — a clean
                // close or a transport failure both mean we're deaf.
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
                    AgentEvent::TurnStarted { .. } => {
                        busy.set(true);
                        status.set(String::new());
                        error.set(String::new());
                        reasoning.set(String::new());
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
                        if let Some(existing) =
                            list.iter_mut().find(|m| m.id == message.id)
                        {
                            *existing = message;
                        } else if matches!(message.role, Role::User)
                            && list.last().is_some_and(|m| {
                                m.id.starts_with("local-") && text_of(m) == text_of(&message)
                            })
                        {
                            // Replace the optimistic local echo.
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
                    AgentEvent::ToolProgress { preview, .. } => status.set(preview),
                    AgentEvent::Warning { kind, message, .. } => {
                        status.set(format!("{kind}: {message}"));
                    }
                    AgentEvent::Metering {
                        input_tokens,
                        output_tokens,
                        ..
                    } => tokens.set((input_tokens, output_tokens)),
                    AgentEvent::TurnFinished { .. } => {
                        busy.set(false);
                        status.set(String::new());
                        streaming.set(None);
                        responding.set(String::new());
                        on_activity.call(());
                    }
                    AgentEvent::TurnErrored { kind, message, .. } => {
                        busy.set(false);
                        status.set(String::new());
                        streaming.set(None);
                        responding.set(String::new());
                        error.set(format!("{kind}: {message}"));
                        on_activity.call(());
                    }
                    AgentEvent::TurnCancelled { .. } => {
                        busy.set(false);
                        status.set("Cancelled".to_string());
                        streaming.set(None);
                        responding.set(String::new());
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
        // Optimistic local echo; replaced when the backend's
        // MessageWritten lands (Hermes emits it; Codex doesn't, so
        // the echo simply stays).
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
                    // Echo what's actually answering — backend + model
                    // resolution happens server-side.
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
    let (tok_in, tok_out) = tokens();
    let stream = stream_state.read().clone();

    // Keep the transcript pinned to the newest content.
    use_effect(move || {
        let _ = messages.read().len();
        let _ = streaming.read().is_some();
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
                    if tok_in + tok_out > 0 {
                        span {
                            class: "text-[0.7rem] tabular-nums text-muted-foreground",
                            title: "input / output tokens",
                            "{tok_in}↑ {tok_out}↓"
                        }
                    }
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
                }
            }

            // Transcript.
            div {
                id: "agent-transcript",
                class: "flex min-h-0 flex-1 flex-col gap-3 overflow-y-auto px-4 py-4",
                for m in messages.read().iter() {
                    {message_view(m)}
                }
                if !reasoning_text.is_empty() {
                    details { class: "rounded-lg border border-border/50 bg-card/40 px-3 py-2 text-xs text-muted-foreground",
                        summary { class: "cursor-pointer select-none", "Thinking" }
                        pre { class: "mt-1 whitespace-pre-wrap font-sans", "{reasoning_text}" }
                    }
                }
                if let Some((_, text)) = &streaming_view {
                    div { class: "max-w-none",
                        task_ui::Markdown { source: text.clone() }
                        span { class: "ml-0.5 inline-block h-4 w-2 animate-pulse bg-primary/70" }
                    }
                }
                if busy() && streaming_view.is_none() {
                    div { class: "flex items-center gap-2 text-sm text-muted-foreground",
                        Spinner { size: SpinnerSize::Small }
                        if status.read().is_empty() {
                            span { "Thinking…" }
                        } else {
                            span { "{status}" }
                        }
                    }
                } else if !status.read().is_empty() {
                    div { class: "text-xs text-muted-foreground", "{status}" }
                }
                if !error.read().is_empty() {
                    div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                        "{error}"
                    }
                }
            }

            // Composer.
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
                // Model override — free-form with suggestions; empty
                // rides the backend's default. Resolution happens
                // server-side (gateway profile / provider routing),
                // so this is a hint, echoed back via the ack.
                div { class: "mt-1.5 flex items-center gap-2",
                    span { class: "text-[0.7rem] text-muted-foreground", "Model" }
                    input {
                        class: "w-48 rounded-md border border-border/60 bg-card/30 px-2 py-0.5 text-xs text-foreground outline-none focus:border-primary/60",
                        placeholder: "backend default",
                        list: "agent-model-suggestions",
                        value: "{model}",
                        oninput: move |e| model.set(e.value()),
                    }
                    datalist { id: "agent-model-suggestions",
                        option { value: "hermes" }
                        option { value: "gpt-5.5" }
                        option { value: "gpt-5.5-mini" }
                        option { value: "claude-opus-4-8" }
                        option { value: "deepseek-v4" }
                    }
                    if !model.read().trim().is_empty() {
                        button {
                            r#type: "button",
                            class: "text-[0.7rem] text-muted-foreground hover:text-foreground",
                            onclick: move |_| model.set(String::new()),
                            "reset"
                        }
                    }
                }
            }
        }
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
