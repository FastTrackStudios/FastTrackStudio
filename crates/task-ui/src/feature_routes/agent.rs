//! AgentRun feature route. Holds a local `CrdtDoc` + `AgentRunRepoLoro`,
//! syncs over WebSocket, drives `agent-ui` dumb components.

use std::rc::Rc;

use std::collections::HashMap;

use agent_crdt::{AgentRunRepoLoro, CrdtDoc};
use agent_proto::{AgentRun, AgentRunCreate, AgentRunRepo, GitRepoConnection};
use agent_ui::AgentRunDashboard;
use agent_ui::hermes_kit::{
    AgentRunBoard, ConfiguredIntegration, GitRepoDraft, HermesConfigDraft, IntegrationConfigStatus,
    IntegrationSettings, WebhookEventLog, WebhookFilter, WebhookInboxRow,
};
use architect::Page;
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn AgentRunView() -> Element {
    let repo: Rc<AgentRunRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(AgentRunRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));

    let mut items = use_signal::<Vec<AgentRun>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = repo_for_loop
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
                    items.set(list.items);
                }
            }
        });
        tx
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    let on_submit = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |payload: AgentRunCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_delete = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            AgentRunDashboard {
                items: items(),
                status: status_msg(),
                on_create: on_submit,
                on_delete,
            }
        }
    }
}

// ── New views for /agents/runs, /settings/integrations, /settings/webhooks
//
// These mount over a shared CrdtDoc so any agent runs created from
// the Projects-live route show up here too. v1 mock data is seeded
// for settings + webhooks since the real persistence layer isn't
// wired yet — see the FUTURE notes per view.

#[component]
pub fn AgentRunBoardView() -> Element {
    let repo: Rc<AgentRunRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(AgentRunRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));
    let mut runs = use_signal::<Vec<AgentRun>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = repo_for_loop
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
                    runs.set(list.items);
                }
            }
        });
        tx
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    // FUTURE: cross-reference Project repo for task titles. The board
    // accepts an empty map gracefully so this stays usable today.
    let task_titles: HashMap<Uuid, String> = HashMap::new();

    rsx! {
        div { class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10",
            SectionHeader { label: "Agent runs" }
            Text { variant: TextVariant::Muted, "{status_msg}" }
            AgentRunBoard {
                runs: runs(),
                task_titles_by_id: task_titles,
                on_open: move |_id| {
                    // FUTURE: deep-link into the originating task's
                    // Agent tab. For v1 the card surfaces enough
                    // metadata to identify the run.
                },
                on_cancel: move |id| {
                    let mut next = runs.read().clone();
                    if let Some(r) = next.iter_mut().find(|r| r.id == id) {
                        r.status = "cancelled".into();
                        r.completed_at = Some(Utc::now());
                        r.updated_at = Utc::now();
                    }
                    runs.set(next);
                },
            }
        }
    }
}

#[component]
pub fn IntegrationSettingsView() -> Element {
    // Mock data — real config will flow through a vox settings endpoint.
    let configured = vec![
        ConfiguredIntegration {
            name: "hermes".into(),
            label: "Hermes".into(),
            status: IntegrationConfigStatus::NotConfigured,
            base_url: None,
            default_profile: None,
        },
        ConfiguredIntegration {
            name: "mock".into(),
            label: "Mock".into(),
            status: IntegrationConfigStatus::Configured,
            base_url: Some("memory://".into()),
            default_profile: Some("sim".into()),
        },
    ];
    let git_connections = use_signal::<Vec<GitRepoConnection>>(Vec::new);

    rsx! {
        div { class: "mx-auto flex max-w-4xl flex-col gap-4 p-6 lg:p-10",
            IntegrationSettings {
                configured,
                git_connections: git_connections(),
                on_save_hermes: move |draft: HermesConfigDraft| {
                    let _ = draft;
                    // FUTURE: persist via a vox config endpoint.
                },
                on_test_hermes: move |_| {
                    // FUTURE: probe `${base_url}/api/health` and surface the result.
                },
                on_add_repo: move |draft: GitRepoDraft| {
                    let _ = draft;
                    // FUTURE: POST to a vox endpoint that seals the secret
                    // and writes a `GitRepoConnection` row.
                },
                on_delete_repo: move |_id| {
                    // FUTURE: DELETE via vox.
                },
            }
        }
    }
}

#[component]
pub fn WebhookEventLogView() -> Element {
    // FUTURE: fetch from `/api/webhooks/inbox` once that route lands.
    let now = Utc::now();
    let initial: Vec<WebhookInboxRow> = vec![
        WebhookInboxRow {
            id: Uuid::new_v4(),
            integration: "github".into(),
            received_at: now - Duration::minutes(2),
            headers: {
                let mut h = HashMap::new();
                h.insert("x-github-event".into(), "pull_request".into());
                h.insert("x-hub-signature-256".into(), "sha256=…".into());
                h
            },
            body_preview: r#"{"action":"opened","pull_request":{"number":42, ...}}"#.into(),
            signature_ok: true,
            processed_at: Some(now - Duration::minutes(2)),
            error: None,
        },
        WebhookInboxRow {
            id: Uuid::new_v4(),
            integration: "hermes".into(),
            received_at: now - Duration::hours(1),
            headers: {
                let mut h = HashMap::new();
                h.insert("content-type".into(), "application/json".into());
                h
            },
            body_preview: r#"{"event":"run.completed","run_id":"abc",...}"#.into(),
            signature_ok: true,
            processed_at: Some(now - Duration::hours(1)),
            error: None,
        },
        WebhookInboxRow {
            id: Uuid::new_v4(),
            integration: "github".into(),
            received_at: now - Duration::hours(4),
            headers: HashMap::new(),
            body_preview: "(malformed body)".into(),
            signature_ok: false,
            processed_at: None,
            error: Some("signature mismatch".into()),
        },
    ];
    let events = use_signal::<Vec<WebhookInboxRow>>(|| initial);
    let mut filter = use_signal::<WebhookFilter>(WebhookFilter::default);

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            WebhookEventLog {
                events: events(),
                filter: filter(),
                on_filter_change: move |f| filter.set(f),
            }
        }
    }
}
