//! Phase 8 — `/federated-tasks` route. Iterates the
//! [`ServerRegistry`], opens a `WorkspaceSyncClient` per entry,
//! pulls each server's `workspace` snapshot, decodes tasks, and
//! shows a combined list tagged by server label.
//!
//! Phase 8 MVP: one-shot snapshot per server on mount. Live
//! sync per server lands when the federated view grows
//! mutations.

use std::collections::HashMap;
use std::sync::Arc;

use crdt::CrdtDoc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use project_crdt::TaskRepoLoro;
use project_proto::architect::Page;
use project_proto::{DocId, Task, TaskRepo, UpdateBytes, WorkspaceSyncClient};
use uuid::Uuid;

use crate::server_registry::{ServerEntry, ServerRegistry};

#[component]
pub fn FederatedTasksView() -> Element {
    let registry: ServerRegistry = use_context_provider(ServerRegistry::new);
    let entries = registry.list();

    let rows: Signal<Vec<FederatedRow>> = use_signal(Vec::new);
    let mut last_error: Signal<Option<String>> = use_signal(|| None);

    // On mount + whenever the entries list changes, kick off a
    // fan-out fetch. We re-key on the count + label hash so add /
    // remove of servers re-fires.
    let entries_for_fetch = entries.clone();
    use_effect(move || {
        let entries = entries_for_fetch.clone();
        let mut rows = rows;
        let mut last_error = last_error;
        spawn(async move {
            let mut all: Vec<FederatedRow> = Vec::new();
            for entry in entries.iter() {
                match fetch_tasks_for(entry).await {
                    Ok(items) => all.extend(items),
                    Err(e) => {
                        tracing::warn!(server = %entry.label, %e, "federated fetch failed");
                        last_error.set(Some(format!("{}: {e}", entry.label)));
                    }
                }
            }
            // Dedupe on (server_id, task_id) — within one server
            // task ids are unique, across servers they may collide
            // because the seed uses the same RNG. The (server_id,
            // task_id) tuple is what makes a row unique.
            let mut seen: HashMap<(Uuid, Uuid), bool> = HashMap::new();
            all.retain(|r| seen.insert((r.server_id, r.task_id), true).is_none());
            rows.set(all);
        });
    });

    let snapshot = rows.read().clone();
    rsx! {
        div {
            id: "federated-tasks-route",
            class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-baseline justify-between",
                Heading { level: HeadingLevel::H1, "Federated tasks" }
                Text { variant: TextVariant::Muted,
                    "across {entries.len()} server(s)"
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                div { "data-testid": "federated-error",
                    class: "rounded-md border border-border bg-card p-3 text-sm",
                    "{err}"
                }
            }
            if entries.is_empty() {
                Text { variant: TextVariant::Muted,
                    "No servers registered. Go to /servers to add one."
                }
            }
            ul {
                "data-testid": "federated-tasks-list",
                class: "rounded-md border border-border bg-card p-3 flex flex-col gap-1",
                for row in snapshot.iter() {
                    FederatedTaskRow { key: "{row.server_id}/{row.task_id}", row: row.clone() }
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
struct FederatedRow {
    server_id: Uuid,
    server_label: String,
    task_id: Uuid,
    title: String,
    done: bool,
}

#[component]
fn FederatedTaskRow(row: FederatedRow) -> Element {
    let testid = format!("federated-task-{}-{}", row.server_id, row.task_id);
    rsx! {
        li {
            "data-testid": testid,
            "data-server-id": "{row.server_id}",
            "data-task-id": "{row.task_id}",
            "data-task-done": if row.done { "true" } else { "false" },
            class: "text-sm flex items-center gap-2",
            span { class: "rounded bg-muted px-1.5 py-0.5 text-xs", "{row.server_label}" }
            span { class: if row.done { "line-through text-muted-foreground" } else { "" },
                "{row.title}"
            }
        }
    }
}

/// Open the server's `workspace` doc, drain the snapshot, decode
/// tasks, project them as `FederatedRow`s. Phase 8 MVP — one
/// snapshot per visit, no live subscribe.
async fn fetch_tasks_for(entry: &ServerEntry) -> Result<Vec<FederatedRow>, String> {
    let client: WorkspaceSyncClient = connect_workspace_sync(&entry.server_url).await?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let server_label = entry.label.clone();
    let server_id = entry.id;
    let doc_id = DocId::new("workspace");
    spawn(async move {
        let _ = client.subscribe(doc_id, tx).await;
    });
    let frame = rx
        .recv()
        .await
        .map_err(|e| format!("recv: {e:?}"))?
        .ok_or_else(|| "stream closed before snapshot".to_string())?;
    let doc = Arc::new(CrdtDoc::ephemeral());
    doc.apply_remote(&frame.get().0)
        .map_err(|e| format!("apply: {e}"))?;
    let tasks = TaskRepoLoro::new(&doc)
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| format!("task list: {e}"))?;
    Ok(tasks
        .items
        .into_iter()
        .map(|t: Task| FederatedRow {
            server_id,
            server_label: server_label.clone(),
            task_id: t.id,
            title: t.title,
            done: t.done,
        })
        .collect())
}

#[cfg(target_arch = "wasm32")]
async fn connect_workspace_sync(url: &str) -> Result<WorkspaceSyncClient, String> {
    use vox_core::{TransportMode, initiator_on};
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish()
        .await
        .map_err(|e| format!("vox establish: {e:?}"))
}

#[cfg(not(target_arch = "wasm32"))]
async fn connect_workspace_sync(_url: &str) -> Result<WorkspaceSyncClient, String> {
    Err("federated fetch is wasm-only in Phase 8".into())
}
