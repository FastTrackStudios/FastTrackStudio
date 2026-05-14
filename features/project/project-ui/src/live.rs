//! Live wrappers — the real-time, offline-first half of the project
//! feature.
//!
//! Architecture:
//!
//! ```text
//!   server CrdtDoc ─┐
//!                   │  subscribe_local_update → broadcast → Tx<UpdateBytes>
//!                   ▼
//!         WorkspaceSync vox session
//!                   │ (typed RPC over WebSocket)
//!                   ▼
//!   client CrdtDoc (this module's `local_doc`)
//!                   │
//!                   ├── *RepoLoro::list — synchronous reads (renders)
//!                   │
//!                   └── *RepoLoro::update — local commit
//!                          │
//!                          └── subscribe_local_update fires
//!                                 │
//!                                 └── mpsc → apply_update → server
//! ```
//!
//! On mount the `TasksByProjectLive` component:
//! 1. Builds an empty in-memory `CrdtDoc` (no persistence; ephemeral).
//! 2. Opens two `WorkspaceSyncClient` sessions over fresh
//!    WebSockets — one for the long-lived `subscribe` stream, one
//!    for short-lived `apply_update` calls.
//! 3. Wires `local_doc.subscribe_local_update(callback)` so every
//!    locally-committed change pushes its bytes into a futures
//!    mpsc that a background task drains by calling
//!    `apply_update`. Loro's `import` (used for remote bytes)
//!    does NOT trigger this callback — no echo loop.
//! 4. Bumps a `version` `Signal` after every imported chunk so the
//!    render `use_resource` re-runs and pulls fresh data from the
//!    local doc.
//!
//! Reads always hit the local doc — works offline once the
//! snapshot is in. Writes commit locally first (instant render)
//! and propagate to peers via the upload pipeline.

use std::collections::HashMap;
use std::sync::Arc;

use crdt::CrdtDoc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures::StreamExt;
use futures::channel::mpsc::unbounded;
use project_crdt::{ProjectRepoLoro, TaskRepoLoro};
use project_proto::architect::Page;
use project_proto::{
    Project, ProjectRepo, Task, TaskRepo, TaskUpdate, UpdateBytes, WorkspaceSyncClient,
};
use uuid::Uuid;

/// The full route entry. Spawns the sync loop and renders from the
/// local doc on every imported chunk.
#[component]
pub fn TasksByProjectLive(vox_url: String) -> Element {
    let local_doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let mut version: Signal<u64> = use_signal(|| 0u64);
    let mut last_error: Signal<Option<String>> = use_signal(|| None::<String>);

    // Spawn the sync loop once. Captures clones of the doc + signals;
    // when the WS dies the task ends and `last_error` reflects it.
    let url_for_hook = vox_url.clone();
    let doc_for_hook = local_doc.read().clone();
    use_hook(move || {
        let url = url_for_hook.clone();
        let doc = doc_for_hook.clone();
        spawn(async move {
            run_sync_loop(url, doc, version, last_error).await;
        });
    });

    // Render half — re-runs whenever `version` changes (every
    // imported chunk). Reads come from the LOCAL doc, so they're
    // synchronous + offline-safe.
    let snapshot = use_resource(move || {
        let _v = version.read();
        let doc = local_doc.read().clone();
        async move { build_snapshot(doc).await }
    });

    // Mutation handler — commits to the LOCAL doc; the
    // subscribe_local_update callback in run_sync_loop picks up
    // the bytes and ships them to the server.
    let on_toggle_done = use_callback(move |(task_id, done): (Uuid, bool)| {
        let doc = local_doc.read().clone();
        spawn(async move {
            let task_repo = TaskRepoLoro::new(&doc);
            if let Err(e) = task_repo
                .update(
                    task_id,
                    TaskUpdate {
                        done: Some(done),
                        ..Default::default()
                    },
                )
                .await
            {
                tracing::warn!(?e, %task_id, "local task update failed");
            }
        });
    });

    let version_label = format!("v{}", version.read());
    rsx! {
        div {
            id: "projects-route",
            class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Projects" }
                span { "data-testid": "version-badge",
                    StatusBadge {
                        variant: if last_error.read().is_some() { StatusBadgeVariant::Danger } else { StatusBadgeVariant::Success },
                        label: version_label,
                    }
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                div { class: "rounded-md border border-border bg-card p-3 text-sm",
                    "Sync: {err}"
                }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! { Text { variant: TextVariant::Muted, "Building local doc…" } },
                Some(Err(err)) => rsx! { Text { variant: TextVariant::Muted, "Decode failed: {err}" } },
                Some(Ok(snap)) => rsx! { TasksByProjectView {
                    snapshot: snap.clone(),
                    on_toggle_done: on_toggle_done,
                } },
            }
        }
    }
}

/// Render half — feed it a `Snapshot` and a done-toggle callback.
#[component]
pub fn TasksByProjectView(snapshot: Snapshot, on_toggle_done: Callback<(Uuid, bool)>) -> Element {
    if snapshot.ordered_projects.is_empty() {
        return rsx! {
            Text { variant: TextVariant::Muted, "No projects with tasks (waiting on snapshot…)." }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-6",
            for (project_id, name) in snapshot.ordered_projects.iter() {
                ProjectBlock {
                    key: "{project_id}",
                    project_id: *project_id,
                    name: name.clone(),
                    tasks: snapshot.tasks_by_project.get(project_id).cloned().unwrap_or_default(),
                    on_toggle_done,
                }
            }
        }
    }
}

#[component]
fn ProjectBlock(
    project_id: Uuid,
    name: String,
    tasks: Vec<Task>,
    on_toggle_done: Callback<(Uuid, bool)>,
) -> Element {
    rsx! {
        section { class: "rounded-md border border-border bg-card p-4",
            HStack { class: "items-baseline justify-between mb-3",
                Heading { level: HeadingLevel::H3, "{name}" }
                Text { variant: TextVariant::Muted, "{tasks.len()} task(s)" }
            }
            ul { class: "flex flex-col gap-1.5",
                for task in tasks.iter() {
                    TaskRow { key: "{task.id}", task: task.clone(), on_toggle_done }
                }
            }
        }
    }
}

#[component]
fn TaskRow(task: Task, on_toggle_done: Callback<(Uuid, bool)>) -> Element {
    let task_id = task.id;
    let current_done = task.done;
    let row_testid = format!("task-row-{task_id}");
    let checkbox_testid = format!("task-checkbox-{task_id}");
    rsx! {
        li {
            class: "flex items-center gap-3 text-sm",
            "data-testid": row_testid,
            "data-task-done": if current_done { "true" } else { "false" },
            input {
                r#type: "checkbox",
                "data-testid": checkbox_testid,
                checked: current_done,
                onchange: move |_| on_toggle_done.call((task_id, !current_done)),
            }
            span {
                class: if current_done { "text-muted-foreground line-through" } else { "text-foreground" },
                "{task.title}"
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Snapshot {
    pub project_names: HashMap<Uuid, String>,
    pub tasks_by_project: HashMap<Uuid, Vec<Task>>,
    pub ordered_projects: Vec<(Uuid, String)>,
}

async fn build_snapshot(doc: Arc<CrdtDoc>) -> Result<Snapshot, String> {
    let project_repo = ProjectRepoLoro::new(&doc);
    let task_repo = TaskRepoLoro::new(&doc);
    let big_page = Page {
        index: 0,
        size: 1000,
    };
    let project_page = project_repo
        .list(big_page.clone(), None, None)
        .await
        .map_err(|e| format!("project list: {e}"))?;
    let task_page = task_repo
        .list(big_page, None, None)
        .await
        .map_err(|e| format!("task list: {e}"))?;

    let mut project_names: HashMap<Uuid, String> = HashMap::new();
    for p in project_page.items.iter() {
        project_names.insert(p.id, p.name.clone());
    }
    let mut tasks_by_project: HashMap<Uuid, Vec<Task>> = HashMap::new();
    for t in task_page.items.into_iter() {
        tasks_by_project.entry(t.project_id).or_default().push(t);
    }
    let mut ordered_projects: Vec<(Uuid, String)> = project_page
        .items
        .into_iter()
        .filter(|p: &Project| tasks_by_project.contains_key(&p.id))
        .map(|p| (p.id, p.name))
        .collect();
    ordered_projects.sort_by(|a, b| a.1.cmp(&b.1));

    Ok(Snapshot {
        project_names,
        tasks_by_project,
        ordered_projects,
    })
}

async fn run_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    mut version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
) {
    // One vox session per direction — `subscribe` holds its session
    // for the lifetime of the stream, so `apply_update` needs its
    // own.
    let sub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "sync subscribe-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };
    let apply_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "sync apply-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };

    // Wire local commits → mpsc → apply_update. The Subscription
    // handle returned by `subscribe_local_update` must outlive the
    // doc; leak it (the doc itself outlives the page).
    let (upload_tx, mut upload_rx) = unbounded::<Vec<u8>>();
    let upload_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = upload_tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(upload_sub);

    // Background uploader: drains the mpsc, ships each chunk via
    // `apply_update`. Holds `apply_client` for the lifetime of the
    // page (component teardown drops it implicitly when the spawn
    // task is canceled).
    spawn(async move {
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_client.apply_update(UpdateBytes(bytes)).await {
                tracing::warn!(?e, "apply_update failed");
            }
        }
    });

    // Subscribe stream — pumps remote bytes into the local doc.
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    spawn(async move {
        if let Err(e) = sub_client.subscribe(tx).await {
            tracing::warn!(error = ?e, "WorkspaceSync::subscribe ended with error");
        }
    });

    loop {
        match rx.recv().await {
            Ok(Some(msg)) => {
                let bytes = &msg.get().0;
                if let Err(e) = doc.apply_remote(bytes) {
                    tracing::warn!(?e, "apply_remote failed");
                    last_error.set(Some(format!("apply_remote: {e}")));
                    continue;
                }
                version.with_mut(|v| *v += 1);
            }
            Ok(None) => {
                tracing::info!("sync stream closed by server");
                last_error.set(Some("stream closed by server".into()));
                return;
            }
            Err(e) => {
                tracing::warn!(?e, "rx.recv failed");
                last_error.set(Some(format!("recv: {e:?}")));
                return;
            }
        }
    }
}

/// Open a typed vox client over a fresh WebSocket. Wasm-only.
#[cfg(target_arch = "wasm32")]
async fn connect_client<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    use vox_core::{TransportMode, initiator_on};
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("vox establish: {e:?}"))
}

#[cfg(not(target_arch = "wasm32"))]
async fn connect_client<C>(_url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    Err("connect_client only implemented for wasm32".into())
}
