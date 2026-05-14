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
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{
    BlockCreate as KBlockCreate, BlockRepo, PageCreate as KPageCreate, PageRepo, VaultCreate,
    VaultRepo,
};
use knowledge_ui::canvas::agent_diagram::layout_agent_runs;
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
    let k_vault_repo: Rc<VaultRepoLoro> = use_hook(|| Rc::new(VaultRepoLoro::new(&doc)));
    let k_page_repo: Rc<PageRepoLoro> = use_hook(|| Rc::new(PageRepoLoro::new(&doc)));
    let k_block_repo: Rc<BlockRepoLoro> = use_hook(|| Rc::new(BlockRepoLoro::new(&doc)));
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

    let open_as_whiteboard = {
        let k_vault_repo = k_vault_repo.clone();
        let k_page_repo = k_page_repo.clone();
        let k_block_repo = k_block_repo.clone();
        move |_| {
            let runs_now = runs.read().clone();
            let k_vault_repo = k_vault_repo.clone();
            let k_page_repo = k_page_repo.clone();
            let k_block_repo = k_block_repo.clone();
            spawn_local(async move {
                // Find or create a vault.
                let vault_id = match k_vault_repo
                    .list(Page { index: 0, size: 50 }, None, None)
                    .await
                {
                    Ok(list) if !list.items.is_empty() => list.items[0].id,
                    _ => match k_vault_repo
                        .create(VaultCreate {
                            name: "Workspace".into(),
                            root_path: None,
                            use_markdown_links: false,
                            new_link_format: "shortest".into(),
                            attachment_folder_path: String::new(),
                            default_view_mode: "live-preview".into(),
                            config_json: "{}".into(),
                        })
                        .await
                    {
                        Ok(v) => v.id,
                        Err(_) => return,
                    },
                };
                let now = Utc::now();
                let basename = format!("Agent runs {}", now.format("%Y-%m-%d %H:%M"));
                let page = match k_page_repo
                    .create(KPageCreate {
                        vault_id,
                        folder_id: None,
                        path: format!("Daily/{basename}.canvas"),
                        basename: basename.clone(),
                        ext: "canvas".into(),
                        aliases: Vec::new(),
                        frontmatter_json: "{}".into(),
                        stat_ctime: now,
                        stat_mtime: now,
                        stat_size: 0,
                        is_journal: false,
                        journal_day: None,
                        shadow_for_kind: None,
                        shadow_for_id: None,
                    })
                    .await
                {
                    Ok(p) => p,
                    Err(_) => return,
                };
                let tasks_by_id: HashMap<Uuid, project_proto::Task> = HashMap::new();
                let blocks = layout_agent_runs(&runs_now, &tasks_by_id, vault_id, page.id);
                for b in blocks {
                    let _ = k_block_repo
                        .create(KBlockCreate {
                            vault_id: b.vault_id,
                            page_id: b.page_id,
                            parent_block_id: b.parent_block_id,
                            sort_key: b.sort_key,
                            kind: b.kind,
                            content: b.content,
                            heading_level: b.heading_level,
                            list_ordered: b.list_ordered,
                            list_task: b.list_task,
                            code_lang: b.code_lang,
                            callout_kind: b.callout_kind,
                            callout_foldable: b.callout_foldable,
                            properties_json: b.properties_json,
                            obsidian_block_id: b.obsidian_block_id,
                            collapsed: b.collapsed,
                            refs_json: b.refs_json,
                            canvas_node_json: b.canvas_node_json,
                        })
                        .await;
                }
                // FUTURE: deep-link nav to the new page (currently the
                // user navigates to /knowledge → Daily folder manually).
            });
        }
    };

    rsx! {
        div { class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10",
            div { class: "flex items-center justify-between",
                SectionHeader { label: "Agent runs" }
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    on_click: open_as_whiteboard,
                    "Open as whiteboard"
                }
            }
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

// ── P4 dashboard views ───────────────────────────────────────────────
//
// `/agent/dashboard` and `/agent/dashboard/:run_id` per issue #24 +
// plans/agent-p4-dashboard.goal.md. Reuses the same Loro-backed
// `AgentRunRepoLoro` so the dashboard sees the same runs as the
// existing chat / board surfaces. Detail view's logs/tool-calls/
// conversation panes use polling via repo `list()` calls — live
// streaming via `LiveUpdateBus` is a follow-up that needs the server-
// side bus reachable from wasm clients (out of scope for the goal's
// "polls repo if not wired" fallback).

use agent_proto::{AgentLogLine, ConversationMessage, ToolCall};
use agent_ui::dashboard::{AgentDashboard, RunDetailView};
use dioxus_router::hooks::use_navigator;

use crate::app::Route;

fn build_repo() -> (Rc<AgentRunRepoLoro>, Rc<CrdtDoc>) {
    let doc = Rc::new(CrdtDoc::ephemeral());
    let repo = Rc::new(AgentRunRepoLoro::new(&doc));
    (repo, doc)
}

#[component]
pub fn AgentDashboardView() -> Element {
    let (repo, doc) = use_hook(build_repo);
    let mut items = use_signal::<Vec<AgentRun>>(Vec::new);
    let nav = use_navigator();

    // Notifications watcher — diff AgentRun.status against the prev
    // snapshot on every items() change; push terminal/blocking
    // transitions through the shared NotificationsCtx (drives bell,
    // inbox, toast stack).
    let mut notif_ctx = use_context::<crate::notifications_ctx::NotificationsCtx>();
    let prev_statuses =
        use_signal::<std::collections::HashMap<Uuid, String>>(std::collections::HashMap::new);
    use_effect(move || {
        let current = items.read().clone();
        let (emit, next) =
            crate::notifications_ctx::diff_runs_for_notifications(&prev_statuses.peek(), &current);
        for n in emit {
            notif_ctx.push(n);
        }
        let mut p = prev_statuses.clone();
        p.set(next);
    });

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

    use_hook({
        let tx = refresh_tx.clone();
        move || {
            let _ = tx.unbounded_send(());
        }
    });

    let on_open_run = {
        let nav = nav.clone();
        move |run_id: Uuid| {
            nav.push(Route::AgentRunDetailRoute { run_id });
        }
    };

    let on_create = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        let nav = nav.clone();
        move |payload: AgentRunCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            let nav = nav.clone();
            spawn_local(async move {
                if let Ok(run) = repo.create(payload).await {
                    let _ = tx.unbounded_send(());
                    nav.push(Route::AgentRunDetailRoute { run_id: run.id });
                }
            });
        }
    };

    rsx! {
        AgentDashboard {
            runs: items(),
            on_open_run,
            on_create,
        }
    }
}

#[component]
pub fn AgentRunDetailRouteView(run_id: Uuid) -> Element {
    let (repo, _doc) = use_hook(build_repo);
    let mut run = use_signal::<Option<AgentRun>>(|| None);
    let logs = use_signal::<Vec<AgentLogLine>>(Vec::new);
    let tool_calls = use_signal::<Vec<ToolCall>>(Vec::new);
    let conversation = use_signal::<Vec<ConversationMessage>>(Vec::new);
    let nav = use_navigator();

    let load_run_id = run_id;
    use_hook({
        let repo = repo.clone();
        move || {
            spawn_local(async move {
                if let Ok(r) = repo.get(load_run_id).await {
                    run.set(Some(r));
                }
            });
        }
    });

    match run() {
        Some(r) => {
            let on_back = {
                let nav = nav.clone();
                move |_| {
                    nav.push(Route::AgentDashboardRoute {});
                }
            };
            let on_cancel = move |_id: Uuid| {
                // Hook for AgentService.cancel — server wiring is a
                // follow-up; the row's status flips locally via the
                // sync round-trip when a real backend is connected.
            };
            rsx! {
                RunDetailView {
                    run: r,
                    logs: logs(),
                    tool_calls: tool_calls(),
                    conversation: conversation(),
                    on_cancel,
                    on_back,
                }
            }
        }
        None => rsx! {
            div { class: "p-6",
                fts_ui::prelude::Text { variant: fts_ui::prelude::TextVariant::Muted, "Loading run {run_id}…" }
            }
        },
    }
}
