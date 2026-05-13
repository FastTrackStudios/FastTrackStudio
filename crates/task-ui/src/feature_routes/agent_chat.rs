//! AI chat route — owns a `CrdtDoc` shared between `MessageRepoLoro`
//! and `AgentConversationRepoLoro`, syncs over WebSocket, and drives
//! `agent-ui::AgentChatPage`.
//!
//! v1 limitation: this route does NOT hit the server's
//! `POST /api/agent-chat/send` endpoint from wasm. Instead it
//! simulates the assistant stream locally — appending chunks to the
//! Loro doc through `MessageRepoLoro::update`. The CRDT is the wire,
//! so the result still surfaces on every peer. The server endpoint
//! exists for native clients (or a future wasm `fetch` migration).

use std::collections::HashMap;
use std::rc::Rc;

use agent_crdt::AgentConversationRepoLoro;
use agent_proto::{
    AgentConversation, AgentConversationCreate, AgentConversationRepo, AgentConversationUpdate,
    default_model_catalog,
};
use agent_ui::AgentChatPage;
use architect::Page;
use chat_crdt::MessageRepoLoro;
use chat_proto::{Message, MessageCreate, MessageRepo, MessageUpdate};
use chat_ui::{MessageAction, SendIntent};
use crdt::CrdtDoc;
use dioxus::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

const AVAILABLE_TOOLS: &[&str] = &["shell", "edit", "grep", "read", "write", "web_fetch"];

#[component]
pub fn AgentChatRoute() -> Element {
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::ephemeral()));
    let message_repo: Rc<MessageRepoLoro> = use_hook({
        let doc = doc.clone();
        move || Rc::new(MessageRepoLoro::new(&doc))
    });
    let conv_repo: Rc<AgentConversationRepoLoro> = use_hook({
        let doc = doc.clone();
        move || Rc::new(AgentConversationRepoLoro::new(&doc))
    });

    let mut conversations = use_signal::<Vec<AgentConversation>>(Vec::new);
    let mut messages = use_signal::<Vec<Message>>(Vec::new);
    let mut active = use_signal::<Option<Uuid>>(|| None);
    let streaming_message_id = use_signal::<Option<Uuid>>(|| None);

    // Cancellation flags keyed by assistant message id.
    let cancel_flags: Signal<HashMap<Uuid, Arc<AtomicBool>>> = use_signal(HashMap::new);

    // Refresh pump: list both entities into signals.
    let refresh_tx: mpsc::UnboundedSender<()> = use_hook({
        let conv_repo = conv_repo.clone();
        let message_repo = message_repo.clone();
        move || {
            let (tx, mut rx) = mpsc::unbounded::<()>();
            spawn_local(async move {
                while rx.next().await.is_some() {
                    if let Ok(list) = conv_repo
                        .list(
                            Page {
                                index: 0,
                                size: 200,
                            },
                            None,
                            None,
                        )
                        .await
                    {
                        conversations.set(list.items);
                    }
                    if let Ok(list) = message_repo
                        .list(
                            Page {
                                index: 0,
                                size: 500,
                            },
                            None,
                            None,
                        )
                        .await
                    {
                        let mut items = list.items;
                        items.sort_by(|a, b| a.created_at.cmp(&b.created_at));
                        messages.set(items);
                    }
                }
            });
            tx
        }
    });

    // WS sync session.
    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx = refresh_tx.clone();
        move || {
            let _ = tx.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx_for_cb = tx.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx_for_cb.unbounded_send(());
            }) {
                Ok(s) => Rc::new(Some(s)),
                Err(_) => Rc::new(None),
            }
        }
    });

    // Seed on first mount if empty.
    use_effect({
        let conv_repo = conv_repo.clone();
        let message_repo = message_repo.clone();
        let tx = refresh_tx.clone();
        move || {
            let convs = conversations.read().clone();
            let has_messages = !messages.read().is_empty();
            if !convs.is_empty() || has_messages {
                return;
            }
            let conv_repo = conv_repo.clone();
            let message_repo = message_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let model_id = default_model_catalog()
                    .first()
                    .map(|m| m.id.clone())
                    .unwrap_or_else(|| "mock-fast".into());
                let created = conv_repo
                    .create(AgentConversationCreate {
                        title: "Welcome".to_string(),
                        system_prompt: Some("You are a helpful AI assistant.".to_string()),
                        default_model: model_id.clone(),
                        temperature_milli: 700,
                        max_tokens: None,
                        tool_set: vec!["read".into(), "grep".into()],
                        agent_run_id: None,
                        project_id: None,
                        parent_conversation_id: None,
                        branch_from_message_id: None,
                        archived: false,
                    })
                    .await;
                if let Ok(conv) = created {
                    let cid = conv.id;
                    for (role, body, reasoning) in [
                        ("system", "You are a helpful AI assistant.", None),
                        ("user", "Welcome — say hi.", None),
                        (
                            "assistant",
                            "Hi! Ready when you are.",
                            Some("Greeting the user briefly and offering follow-up."),
                        ),
                        ("user", "What can you do?", None),
                    ] {
                        let _ = message_repo
                            .create(MessageCreate {
                                channel_id: None,
                                author: role.to_string(),
                                body: body.to_string(),
                                reply_to: None,
                                edited_at: None,
                                deleted: false,
                                mentions: Vec::new(),
                                attachment_ids: Vec::new(),
                                role: Some(role.to_string()),
                                model: if role == "assistant" {
                                    Some(model_id.clone())
                                } else {
                                    None
                                },
                                reasoning: reasoning.map(|s| s.to_string()),
                                tool_calls_json: None,
                                finish_reason: if role == "assistant" {
                                    Some("stop".to_string())
                                } else {
                                    None
                                },
                                tokens_input: None,
                                tokens_output: None,
                                cost_cents: None,
                                streaming: false,
                                agent_conversation_id: Some(cid),
                            })
                            .await;
                    }
                    active.set(Some(cid));
                    let _ = tx.unbounded_send(());
                }
            });
        }
    });

    let available_models = use_memo(|| default_model_catalog());
    let available_tools: Vec<String> = AVAILABLE_TOOLS.iter().map(|s| s.to_string()).collect();

    let on_select_conversation = move |id: Uuid| active.set(Some(id));

    let on_new_conversation = {
        let conv_repo = conv_repo.clone();
        let tx = refresh_tx.clone();
        move |_| {
            let conv_repo = conv_repo.clone();
            let tx = tx.clone();
            let default_model = default_model_catalog()
                .first()
                .map(|m| m.id.clone())
                .unwrap_or_else(|| "mock-fast".into());
            spawn_local(async move {
                let created = conv_repo
                    .create(AgentConversationCreate {
                        title: "New chat".to_string(),
                        system_prompt: None,
                        default_model,
                        temperature_milli: 700,
                        max_tokens: None,
                        tool_set: Vec::new(),
                        agent_run_id: None,
                        project_id: None,
                        parent_conversation_id: None,
                        branch_from_message_id: None,
                        archived: false,
                    })
                    .await;
                if let Ok(conv) = created {
                    active.set(Some(conv.id));
                }
                let _ = tx.unbounded_send(());
            });
        }
    };

    // Local simulation of the server-side ChatModel stream. FUTURE:
    // replace with `fetch("/api/agent-chat/send", ...)` once we add a
    // wasm HTTP layer.
    let on_send = {
        let message_repo = message_repo.clone();
        let tx = refresh_tx.clone();
        let active_for_send = active;
        let mut streaming_id = streaming_message_id;
        let mut flags = cancel_flags;
        move |intent: SendIntent| {
            let conv_id = match *active_for_send.read() {
                Some(id) => id,
                None => return,
            };
            let message_repo = message_repo.clone();
            let tx = tx.clone();
            let body = intent.body.clone();
            let model_id = intent.model.clone();
            spawn_local(async move {
                // 1. user message
                let _ = message_repo
                    .create(MessageCreate {
                        channel_id: None,
                        author: "user".to_string(),
                        body: body.clone(),
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
                        agent_conversation_id: Some(conv_id),
                    })
                    .await;

                // 2. assistant placeholder
                let assistant = match message_repo
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
                        model: Some(model_id.clone()),
                        reasoning: None,
                        tool_calls_json: None,
                        finish_reason: None,
                        tokens_input: None,
                        tokens_output: None,
                        cost_cents: None,
                        streaming: true,
                        agent_conversation_id: Some(conv_id),
                    })
                    .await
                {
                    Ok(m) => m,
                    Err(_) => return,
                };
                let assistant_id = assistant.id;
                streaming_id.set(Some(assistant_id));
                let cancel = Arc::new(AtomicBool::new(false));
                flags.write().insert(assistant_id, cancel.clone());

                let _ = tx.unbounded_send(());

                // 3. scripted stream — 8 chunks at 200ms intervals.
                let trimmed: String = body.chars().take(50).collect();
                let full = format!(
                    "Got it. Here's how I'd approach \"{trimmed}\":\n\n\
                     1. Map the surface area.\n\
                     2. Identify the smallest slice.\n\
                     3. Land it end-to-end.\n\
                     4. Loop back for edges."
                );
                let chars: Vec<char> = full.chars().collect();
                let chunk_size = chars.len().div_ceil(8).max(1);
                let chunks: Vec<String> = chars
                    .chunks(chunk_size)
                    .map(|c| c.iter().collect::<String>())
                    .collect();
                let mut acc = String::new();
                for piece in chunks {
                    if cancel.load(Ordering::Relaxed) {
                        break;
                    }
                    delay_ms(200).await;
                    acc.push_str(&piece);
                    let _ = message_repo
                        .update(
                            assistant_id,
                            MessageUpdate {
                                body: Some(acc.clone()),
                                ..Default::default()
                            },
                        )
                        .await;
                    let _ = tx.unbounded_send(());
                }
                let _ = message_repo
                    .update(
                        assistant_id,
                        MessageUpdate {
                            body: Some(acc),
                            streaming: Some(false),
                            finish_reason: Some(Some("stop".to_string())),
                            tokens_input: Some(Some(142)),
                            tokens_output: Some(Some(98)),
                            cost_cents: Some(Some(1)),
                            ..Default::default()
                        },
                    )
                    .await;
                flags.write().remove(&assistant_id);
                if streaming_id.read().as_ref() == Some(&assistant_id) {
                    streaming_id.set(None);
                }
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_stop = {
        let mut flags = cancel_flags;
        move |id: Uuid| {
            if let Some(f) = flags.read().get(&id) {
                f.store(true, Ordering::Relaxed);
            }
            flags.write().remove(&id);
        }
    };

    let on_update_conversation = {
        let conv_repo = conv_repo.clone();
        let tx = refresh_tx.clone();
        move |(id, patch): (Uuid, AgentConversationUpdate)| {
            let conv_repo = conv_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = conv_repo.update(id, patch).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_archive_conversation = {
        let conv_repo = conv_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let conv_repo = conv_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = conv_repo
                    .update(
                        id,
                        AgentConversationUpdate {
                            archived: Some(true),
                            ..Default::default()
                        },
                    )
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_delete_conversation = {
        let conv_repo = conv_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let conv_repo = conv_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = conv_repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_rename_conversation = {
        let conv_repo = conv_repo.clone();
        let tx = refresh_tx.clone();
        move |(id, title): (Uuid, String)| {
            let conv_repo = conv_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = conv_repo
                    .update(
                        id,
                        AgentConversationUpdate {
                            title: Some(title),
                            ..Default::default()
                        },
                    )
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_message_action = {
        // Re-run the last user message through the local sim on Regenerate;
        // every other action (Copy, etc.) is handled inline by the component.
        let messages_signal = messages;
        let mut do_send = on_send.clone();
        let active_for_action = active;
        move |(_id, action): (Uuid, MessageAction)| {
            if matches!(action, MessageAction::Regenerate) {
                let conv_id = match *active_for_action.read() {
                    Some(id) => id,
                    None => return,
                };
                let last_user = messages_signal
                    .read()
                    .iter()
                    .rev()
                    .find(|m| {
                        m.agent_conversation_id == Some(conv_id)
                            && m.role.as_deref() == Some("user")
                    })
                    .cloned();
                if let Some(m) = last_user {
                    do_send(SendIntent {
                        body: m.body,
                        model: "mock-fast".to_string(),
                        reasoning: false,
                        tool_set: Vec::new(),
                    });
                }
            }
        }
    };

    rsx! {
        AgentChatPage {
            conversations: conversations(),
            active_conversation: *active.read(),
            messages: messages(),
            available_models: available_models().clone(),
            available_tools,
            streaming_message_id: *streaming_message_id.read(),
            on_select_conversation,
            on_new_conversation,
            on_send,
            on_stop,
            on_update_conversation,
            on_message_action,
            on_archive_conversation,
            on_delete_conversation,
            on_rename_conversation,
        }
    }
}

async fn delay_ms(ms: u32) {
    use wasm_bindgen::JsCast;
    use wasm_bindgen::prelude::*;
    let promise = js_sys::Promise::new(&mut |resolve, _| {
        let window = web_sys::window().expect("window");
        let _ = window.set_timeout_with_callback_and_timeout_and_arguments_0(
            resolve.unchecked_ref::<js_sys::Function>(),
            ms as i32,
        );
    });
    let _ = wasm_bindgen_futures::JsFuture::from(promise).await;
}
