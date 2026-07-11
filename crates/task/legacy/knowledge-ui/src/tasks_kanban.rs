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
use knowledge_crdt::{IndexedPageRepo, PageRepoLoro, VaultRepoLoro};
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
    let version: Signal<u64> = use_signal(|| 0u64);
    let last_error: Signal<Option<String>> = use_signal(|| None::<String>);

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
    let on_move = use_callback(move |drop: super::views::KanbanDrop| {
        let doc = move_doc.clone();
        spawn(async move {
            // Tier 2: route writes through IndexedPageRepo so the
            // page-prop index follows status flips.
            let page_repo = IndexedPageRepo::new(&doc);
            let page = match page_repo.get(drop.page_id).await {
                Ok(p) => p,
                Err(e) => {
                    tracing::warn!(?e, page_id = %drop.page_id, "kanban: page lookup failed");
                    return;
                }
            };
            let mut fm: indexmap::IndexMap<String, serde_json::Value> =
                serde_json::from_str(&page.frontmatter_json).unwrap_or_default();
            fm.insert(
                "status".into(),
                serde_json::Value::String(drop.target_bucket.clone()),
            );
            fm.entry("kind".into())
                .or_insert(serde_json::Value::String("task".into()));
            // Compute the new sort_order. Three cases:
            //  - `before` set: drop just before that card.
            //    `between(prev_rank, before_rank)`.
            //  - `before` unset: drop at the bucket's tail.
            //    `after(tail_rank)` or `first()`.
            // We exclude the dragged page itself from the
            // bucket scan so the rank space stays consistent.
            let new_rank = match compute_drop_rank(&doc, &drop).await {
                Ok(r) => r,
                Err(e) => {
                    tracing::warn!(
                        ?e,
                        "kanban: sort_order compute failed; falling back to tail"
                    );
                    lexorank::first()
                }
            };
            fm.insert("sort_order".into(), serde_json::Value::String(new_rank));
            let new_json = serde_json::to_string(&fm).unwrap_or_else(|_| "{}".into());
            if let Err(e) = page_repo
                .update(
                    drop.page_id,
                    PageUpdate {
                        frontmatter_json: Some(new_json),
                        ..Default::default()
                    },
                )
                .await
            {
                tracing::warn!(?e, page_id = %drop.page_id, "kanban: page update failed");
            }
        });
    });

    // Per-column inline add: creates a `kind: task` page already
    // in the target bucket so it lands directly under the input
    // without a status flip.
    let add_doc = local_doc.read().clone();
    let on_add_to_bucket = use_callback(move |(bucket, title): (String, String)| {
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
            let fm = serde_json::json!({
                "kind": "task",
                "title": title.clone(),
                "status": bucket,
            })
            .to_string();
            let page_repo = IndexedPageRepo::new(&doc);
            if let Err(e) = page_repo
                .create(PageCreate {
                    vault_id: vault.id,
                    folder_id: None,
                    path: format!("{title}.md"),
                    basename: title,
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
            class: "mx-auto flex max-w-7xl flex-col gap-4 p-4 sm:p-6 lg:p-10",
            div { class: "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-2",
                Heading { level: HeadingLevel::H1, "Tasks (kanban)" }
                span { "data-testid": "tasks-kanban-version-badge",
                    StatusBadge {
                        variant: if last_error.read().is_some() { StatusBadgeVariant::Danger } else { StatusBadgeVariant::Success },
                        label: version_label,
                    }
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                Alert {
                    variant: AlertVariant::Destructive,
                    AlertTitle { "Sync error" }
                    AlertDescription { "{err}" }
                }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! { Text { variant: TextVariant::Muted, "Building local doc…" } },
                Some(Err(err)) => rsx! { Text { variant: TextVariant::Muted, "Decode failed: {err}" } },
                Some(Ok(executed)) => rsx! {
                    super::views::KindKanban {
                        view: executed.clone(),
                        group_key: String::from("status"),
                        on_select,
                        on_move,
                        on_add: Some(on_add_to_bucket),
                    }
                },
            }
        }
    }
}

#[allow(dead_code)]
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
/// Compute the new `sort_order` rank for a kanban drop.
///
/// - `drop.before == None` → append. `lexorank::after(tail_rank)`
///   or `first()` if the bucket was empty.
/// - `drop.before == Some(target)` → insert just before
///   `target`. Find the card immediately above `target` in the
///   target bucket (excluding the dragged card itself) and call
///   `lexorank::between(prev_rank, target_rank)`. If `target`
///   is the first card, use `lexorank::before(target_rank)`.
async fn compute_drop_rank(
    doc: &CrdtDoc,
    drop: &super::views::KanbanDrop,
) -> Result<String, knowledge_proto::architect::RepoError> {
    let page_repo = PageRepoLoro::new(doc);
    let pages = page_repo.list(big_page(), None, None).await?;

    // Build the bucket's ordered list of (page_id, sort_order),
    // excluding the dragged card so its current rank doesn't
    // interfere with `between` math.
    let mut bucket: Vec<(Uuid, String)> = pages
        .items
        .into_iter()
        .filter(|p| p.id != drop.page_id)
        .filter_map(|p| {
            let fm: serde_json::Value = serde_json::from_str(&p.frontmatter_json).ok()?;
            let status = fm.get("status").and_then(|v| v.as_str()).unwrap_or("");
            if status != drop.target_bucket {
                return None;
            }
            let rank = fm
                .get("sort_order")
                .and_then(|v| v.as_str())
                .map(String::from)
                .unwrap_or_default();
            Some((p.id, rank))
        })
        .collect();
    bucket.sort_by(|a, b| a.1.cmp(&b.1));

    let Some(before_id) = drop.before else {
        // Append case.
        return Ok(match bucket.last() {
            Some((_, r)) if !r.is_empty() => lexorank::after(r),
            _ => lexorank::first(),
        });
    };

    let pos = bucket.iter().position(|(id, _)| *id == before_id);
    match pos {
        Some(0) => {
            // Insert before the first card.
            let head_rank = &bucket[0].1;
            if head_rank.is_empty() {
                Ok(lexorank::first())
            } else {
                Ok(lexorank::before(head_rank))
            }
        }
        Some(i) => {
            let prev = &bucket[i - 1].1;
            let target = &bucket[i].1;
            Ok(lexorank::between(prev, target).unwrap_or_else(|| lexorank::after(prev)))
        }
        None => {
            // before_id isn't in the target bucket — drop landed
            // on the column rather than a card. Append.
            Ok(match bucket.last() {
                Some((_, r)) if !r.is_empty() => lexorank::after(r),
                _ => lexorank::first(),
            })
        }
    }
}

async fn run_kanban_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    mut version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
) {
    if url.is_empty() {
        let _ = (doc, &mut version, &mut last_error);
        return;
    }
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

/// WebSocket-backed vox client. `vox-websocket::WsLink::connect`
/// implements both `tokio-tungstenite` on native and
/// `web_sys::WebSocket` on wasm, so the same code path works
/// for the desktop binary and the browser bundle.
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
