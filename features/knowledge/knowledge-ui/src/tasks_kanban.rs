//! Phase 6 demo — `TasksKanbanLive`.
//!
//! Subscribes to `vault/org` (same DocId the `KnowledgeLive`
//! component uses) and renders pages with `kind: task` as a
//! kanban grouped by their `status` frontmatter field.
//!
//! Moving a card flips the page's `status` frontmatter to the new
//! bucket label and commits locally. The commit propagates over
//! `WorkspaceSync::apply_update` just like every other Knowledge
//! mutation in Phase 5c.
//!
//! Three baseline columns are always rendered so a fresh page
//! defaults into "todo": `todo`, `in_progress`, `done`.

use std::collections::HashMap;
use std::sync::Arc;

use crdt::CrdtDoc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures::StreamExt;
use futures::channel::mpsc::unbounded;
use knowledge_crdt::{PageRepoLoro, VaultRepoLoro};
use knowledge_proto::bases::{
    BaseRow, CmpOp, ExecutedView, Expr, FilterNode, ParsedBase, SortDir, SortKey, ViewKind,
    ViewSpec, execute_view,
};
use knowledge_proto::lexorank;
use knowledge_proto::{Page, PageCreate, PageRepo, PageUpdate, VaultRepo};
use project_proto::architect::Page as PageWindow;
use project_proto::{UpdateBytes, WorkspaceSyncClient};
use uuid::Uuid;

const BASELINE_BUCKETS: &[&str] = &["todo", "in_progress", "done"];

/// Live wrapper for the kanban demo. Same prop shape as the other
/// Knowledge live components.
#[component]
pub fn TasksKanbanLive(vox_url: String) -> Element {
    let local_doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let mut version: Signal<u64> = use_signal(|| 0u64);
    let mut last_error: Signal<Option<String>> = use_signal(|| None::<String>);

    let url_for_hook = vox_url.clone();
    let doc_for_hook = local_doc.read().clone();
    use_hook(move || {
        let url = url_for_hook.clone();
        let doc = doc_for_hook.clone();
        spawn(async move {
            run_kanban_sync_loop(url, doc, version, last_error).await;
        });
    });

    // Build the executed view on every version bump.
    let snapshot = use_resource(move || {
        let _v = version.read();
        let doc = local_doc.read().clone();
        async move { build_view(doc).await }
    });

    let move_doc = local_doc.read().clone();
    let on_move = use_callback(move |(page_id, target): (Uuid, String)| {
        let doc = move_doc.clone();
        spawn(async move {
            let page_repo = PageRepoLoro::new(&doc);
            let page = match page_repo.get(page_id).await {
                Ok(p) => p,
                Err(e) => {
                    tracing::warn!(?e, %page_id, "kanban: page lookup failed");
                    return;
                }
            };
            // Re-encode frontmatter_json with the new status +
            // a fresh sort_order at the end of the target bucket.
            // Phase 6.5b: append-only (LexoRank::after of the
            // tail rank in the target bucket). Phase 6.6+ will
            // grow drop-position awareness.
            let mut fm: indexmap::IndexMap<String, serde_json::Value> =
                serde_json::from_str(&page.frontmatter_json).unwrap_or_default();
            fm.insert("status".into(), serde_json::Value::String(target.clone()));
            // Ensure kind:task is preserved (defensive).
            fm.entry("kind".into())
                .or_insert(serde_json::Value::String("task".into()));
            // Compute a sort_order that places this card after any
            // existing card in the target bucket.
            let tail_rank = match tail_rank_in_bucket(&doc, &target).await {
                Ok(opt) => opt,
                Err(e) => {
                    tracing::warn!(?e, "kanban: tail_rank lookup failed");
                    None
                }
            };
            let new_rank = match tail_rank {
                Some(r) => lexorank::after(&r),
                None => lexorank::first(),
            };
            fm.insert("sort_order".into(), serde_json::Value::String(new_rank));
            let new_json = serde_json::to_string(&fm).unwrap_or_else(|_| "{}".into());
            if let Err(e) = page_repo
                .update(
                    page_id,
                    PageUpdate {
                        frontmatter_json: Some(new_json),
                        ..Default::default()
                    },
                )
                .await
            {
                tracing::warn!(?e, %page_id, "kanban: page update failed");
            }
        });
    });

    let add_doc = local_doc.read().clone();
    let on_add_task = use_callback(move |basename: String| {
        let doc = add_doc.clone();
        spawn(async move {
            let vault_repo = VaultRepoLoro::new(&doc);
            let vaults = match vault_repo.list(big_page(), None, None).await {
                Ok(p) => p,
                Err(e) => {
                    tracing::warn!(?e, "kanban: vault list failed");
                    return;
                }
            };
            let Some(vault) = vaults.items.into_iter().next() else {
                tracing::warn!("kanban: no vault to attach the new task to");
                return;
            };
            let now = chrono::Utc::now();
            let fm = serde_json::json!({"kind": "task", "status": "todo"}).to_string();
            let page_repo = PageRepoLoro::new(&doc);
            if let Err(e) = page_repo
                .create(PageCreate {
                    vault_id: vault.id,
                    folder_id: None,
                    path: format!("{basename}.md"),
                    basename,
                    ext: "md".into(),
                    aliases: Vec::new(),
                    frontmatter_json: fm,
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
                tracing::warn!(?e, "kanban: page create failed");
            }
        });
    });

    let on_select = use_callback(move |_: Uuid| {});

    let version_label = format!("v{}", version.read());
    rsx! {
        div {
            id: "tasks-kanban-route",
            class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Tasks (kanban)" }
                span { "data-testid": "tasks-kanban-version-badge",
                    StatusBadge {
                        variant: if last_error.read().is_some() { StatusBadgeVariant::Danger } else { StatusBadgeVariant::Success },
                        label: version_label,
                    }
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                div { "data-testid": "tasks-kanban-sync-error",
                    class: "rounded-md border border-border bg-card p-3 text-sm",
                    "Sync: {err}"
                }
            }
            AddTaskRow { on_add_task }
            match &*snapshot.read_unchecked() {
                None => rsx! { Text { variant: TextVariant::Muted, "Building local doc…" } },
                Some(Err(err)) => rsx! { Text { variant: TextVariant::Muted, "Decode failed: {err}" } },
                Some(Ok(executed)) => rsx! {
                    super::views::KindKanban {
                        view: executed.clone(),
                        group_key: String::from("status"),
                        on_select,
                        on_move,
                    }
                },
            }
        }
    }
}

#[component]
fn AddTaskRow(on_add_task: Callback<String>) -> Element {
    let mut value = use_signal(String::new);
    let mut submit = move |_: ()| {
        let v = value.read().clone();
        if v.trim().is_empty() {
            return;
        }
        on_add_task.call(v.trim().to_string());
        value.set(String::new());
    };
    rsx! {
        HStack { class: "gap-2",
            input {
                "data-testid": "tasks-kanban-new-task-input",
                r#type: "text",
                class: "flex-1 rounded border border-border bg-background px-2 py-1 text-sm",
                value: "{value}",
                placeholder: "New task title",
                oninput: move |e| value.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { submit(()); }
                },
            }
            span { "data-testid": "tasks-kanban-add-button",
                Button {
                    on_click: move |_| submit(()),
                    "Add task"
                }
            }
        }
    }
}

fn big_page() -> PageWindow {
    PageWindow {
        index: 0,
        size: 1000,
    }
}

/// The Base + view used by the demo route. Inlined here so the
/// kanban is self-contained; production usage would parse this from
/// a `.base` page in the org vault.
fn tasks_kanban_base() -> ParsedBase {
    ParsedBase {
        global_filter: FilterNode::Cmp {
            left: Expr::NoteProp {
                name: "kind".into(),
            },
            op: CmpOp::Eq,
            right: Expr::Literal {
                value: serde_json::json!("task"),
            },
        },
        formulas: vec![],
        properties: vec![],
        views: vec![ViewSpec {
            kind: ViewKind::Board,
            name: "Tasks".into(),
            filter: None,
            order: vec!["status".into()],
            sort: vec![SortKey {
                property: "basename".into(),
                direction: SortDir::Asc,
            }],
            limit: None,
            group_by: Some("status".into()),
            extras: serde_json::Value::Null,
        }],
    }
}

async fn build_view(doc: Arc<CrdtDoc>) -> Result<ExecutedView, String> {
    let page_repo = PageRepoLoro::new(&doc);
    let pages = page_repo
        .list(big_page(), None, None)
        .await
        .map_err(|e| format!("page list: {e}"))?;

    let rows: Vec<BaseRow> = pages.items.into_iter().map(page_to_row).collect();

    let base = tasks_kanban_base();
    let view = &base.views[0];
    let mut executed = execute_view(&base, view, rows);

    // Pad with the baseline columns so empty buckets render too.
    let mut existing: HashMap<String, Vec<BaseRow>> = executed.groups.drain(..).collect();
    let mut ordered: Vec<(String, Vec<BaseRow>)> = Vec::new();
    for baseline in BASELINE_BUCKETS {
        let rows = existing.remove(*baseline).unwrap_or_default();
        ordered.push(((*baseline).to_string(), rows));
    }
    // Any extra buckets (custom statuses) tack onto the end.
    for (k, v) in existing {
        ordered.push((k, v));
    }
    Ok(ExecutedView { groups: ordered })
}

fn page_to_row(p: Page) -> BaseRow {
    BaseRow::from_parts(p.id, p.basename, &p.frontmatter_json)
}

/// Find the largest sort_order rank among task pages currently in
/// `bucket` (status = `bucket`). Returns None when the bucket is
/// empty — the caller then picks `lexorank::first()`. Phase 6.5b
/// MVP: append-only on drop.
async fn tail_rank_in_bucket(
    doc: &CrdtDoc,
    bucket: &str,
) -> Result<Option<String>, knowledge_proto::architect::RepoError> {
    let page_repo = PageRepoLoro::new(doc);
    let pages = page_repo.list(big_page(), None, None).await?;
    let mut max_rank: Option<String> = None;
    for p in pages.items {
        let fm: serde_json::Value =
            serde_json::from_str(&p.frontmatter_json).unwrap_or(serde_json::Value::Null);
        let status = fm.get("status").and_then(|v| v.as_str()).unwrap_or("");
        if status != bucket {
            continue;
        }
        let rank = fm
            .get("sort_order")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());
        if let Some(r) = rank {
            match &max_rank {
                Some(cur) if cur.as_str() >= r.as_str() => {}
                _ => max_rank = Some(r),
            }
        }
    }
    Ok(max_rank)
}

async fn run_kanban_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    mut version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
) {
    let sub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "kanban subscribe-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };
    let apply_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "kanban apply-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };

    let (upload_tx, mut upload_rx) = unbounded::<Vec<u8>>();
    let upload_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = upload_tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(upload_sub);

    let doc_id = project_proto::DocId::org_vault();
    let upload_doc_id = doc_id.clone();
    spawn(async move {
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_client
                .apply_update(upload_doc_id.clone(), UpdateBytes(bytes))
                .await
            {
                tracing::warn!(?e, "kanban apply_update failed");
            }
        }
    });

    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let sub_doc_id = doc_id.clone();
    spawn(async move {
        if let Err(e) = sub_client.subscribe(sub_doc_id, tx).await {
            tracing::warn!(error = ?e, "kanban WorkspaceSync::subscribe ended");
        }
    });

    loop {
        match rx.recv().await {
            Ok(Some(msg)) => {
                let bytes = &msg.get().0;
                if let Err(e) = doc.apply_remote(bytes) {
                    tracing::warn!(?e, "kanban apply_remote failed");
                    last_error.set(Some(format!("apply_remote: {e}")));
                    continue;
                }
                version.with_mut(|v| *v += 1);
            }
            Ok(None) => {
                last_error.set(Some("stream closed by server".into()));
                return;
            }
            Err(e) => {
                last_error.set(Some(format!("recv: {e:?}")));
                return;
            }
        }
    }
}

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
