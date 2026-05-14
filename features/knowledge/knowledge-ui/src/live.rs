//! Live Knowledge wrapper — same shape as
//! `project-ui::TasksByProjectLive`, scoped to `DocId::org_vault()`
//! (`vault/org`).
//!
//! The route shows a list of pages on the left and the blocks of
//! the selected page on the right. Block content is editable; edits
//! commit locally and sync to peers.

use std::collections::HashMap;
use std::sync::Arc;

use crdt::CrdtDoc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures::StreamExt;
use futures::channel::mpsc::unbounded;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{
    Block, BlockCreate, BlockRepo, BlockUpdate, Page, PageCreate, PageRepo, Vault, VaultRepo,
};
use project_proto::architect::Page as PageWindow;
use project_proto::{UpdateBytes, WorkspaceSyncClient};
use uuid::Uuid;

/// Live route component. One prop — the vox URL. Subscribes to
/// `vault/org`.
#[component]
pub fn KnowledgeLive(vox_url: String) -> Element {
    let local_doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let mut version: Signal<u64> = use_signal(|| 0u64);
    let mut last_error: Signal<Option<String>> = use_signal(|| None::<String>);
    let mut selected_page: Signal<Option<Uuid>> = use_signal(|| None);

    let url_for_hook = vox_url.clone();
    let doc_for_hook = local_doc.read().clone();
    use_hook(move || {
        let url = url_for_hook.clone();
        let doc = doc_for_hook.clone();
        spawn(async move {
            run_sync_loop(url, doc, version, last_error).await;
        });
    });

    let snapshot = use_resource(move || {
        let _v = version.read();
        let doc = local_doc.read().clone();
        async move { build_snapshot(doc).await }
    });

    let on_select_page = use_callback(move |id: Uuid| {
        selected_page.set(Some(id));
    });

    // Commit a block content edit locally; the upload pipeline ships
    // it to the server.
    // Phase 6.5b: property edits round-trip the same way as block
    // edits — write to the local doc, the upload pipeline ships it
    // to peers.
    let prop_doc = local_doc.read().clone();
    let on_edit_property = use_callback(
        move |(page_id, key, value): (Uuid, String, serde_json::Value)| {
            let doc = prop_doc.clone();
            spawn(async move {
                let page_repo = PageRepoLoro::new(&doc);
                let Ok(page) = page_repo.get(page_id).await else {
                    return;
                };
                let mut fm: indexmap::IndexMap<String, serde_json::Value> =
                    serde_json::from_str(&page.frontmatter_json).unwrap_or_default();
                if matches!(value, serde_json::Value::Null) {
                    fm.shift_remove(&key);
                } else {
                    fm.insert(key, value);
                }
                let new_json = serde_json::to_string(&fm).unwrap_or_else(|_| "{}".into());
                let _ = page_repo
                    .update(
                        page_id,
                        knowledge_proto::PageUpdate {
                            frontmatter_json: Some(new_json),
                            ..Default::default()
                        },
                    )
                    .await;
            });
        },
    );

    let block_doc = local_doc.read().clone();
    let on_edit_block = use_callback(move |(block_id, content): (Uuid, String)| {
        let doc = block_doc.clone();
        spawn(async move {
            let block_repo = BlockRepoLoro::new(&doc);
            if let Err(e) = block_repo
                .update(
                    block_id,
                    BlockUpdate {
                        content: Some(content),
                        ..Default::default()
                    },
                )
                .await
            {
                tracing::warn!(?e, %block_id, "local block update failed");
            }
        });
    });

    // Append a new paragraph block to the currently-selected page.
    let new_block_doc = local_doc.read().clone();
    let on_add_block = use_callback(
        move |(page_id, vault_id, after_count): (Uuid, Uuid, usize)| {
            let doc = new_block_doc.clone();
            spawn(async move {
                let block_repo = BlockRepoLoro::new(&doc);
                // Simple lexicographic key — "a0", "a1", … is enough for
                // Phase 5c demos; FractionalIndex follows.
                let key = format!("a{after_count}");
                if let Err(e) = block_repo
                    .create(BlockCreate {
                        vault_id,
                        page_id,
                        parent_block_id: None,
                        sort_key: key,
                        kind: "paragraph".into(),
                        content: String::new(),
                        heading_level: None,
                        list_ordered: false,
                        list_task: None,
                        code_lang: None,
                        callout_kind: None,
                        callout_foldable: false,
                        properties_json: "{}".into(),
                        obsidian_block_id: None,
                        collapsed: false,
                        refs_json: "[]".into(),
                        canvas_node_json: None,
                    })
                    .await
                {
                    tracing::warn!(?e, "local block create failed");
                }
            });
        },
    );

    // Add a fresh page to the first vault.
    let new_page_doc = local_doc.read().clone();
    let on_add_page = use_callback(move |(vault_id, basename): (Uuid, String)| {
        let doc = new_page_doc.clone();
        spawn(async move {
            let page_repo = PageRepoLoro::new(&doc);
            let now = chrono::Utc::now();
            if let Err(e) = page_repo
                .create(PageCreate {
                    vault_id,
                    folder_id: None,
                    path: format!("{basename}.md"),
                    basename,
                    ext: "md".into(),
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
                tracing::warn!(?e, "local page create failed");
            }
        });
    });

    let version_label = format!("v{}", version.read());
    let selected = *selected_page.read();
    rsx! {
        div {
            id: "knowledge-route",
            class: "mx-auto flex max-w-6xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Knowledge" }
                span { "data-testid": "knowledge-version-badge",
                    StatusBadge {
                        variant: if last_error.read().is_some() { StatusBadgeVariant::Danger } else { StatusBadgeVariant::Success },
                        label: version_label,
                    }
                }
            }
            if let Some(err) = last_error.read().as_ref() {
                div {
                    "data-testid": "knowledge-sync-error",
                    class: "rounded-md border border-border bg-card p-3 text-sm",
                    "Sync: {err}"
                }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! { Text { variant: TextVariant::Muted, "Building local doc…" } },
                Some(Err(err)) => rsx! { Text { variant: TextVariant::Muted, "Decode failed: {err}" } },
                Some(Ok(snap)) => rsx! { KnowledgeView {
                    snapshot: snap.clone(),
                    selected_page: selected,
                    on_select_page,
                    on_edit_block,
                    on_add_block,
                    on_add_page,
                    on_edit_property,
                } },
            }
        }
    }
}

/// Render half — pure data + callbacks.
#[component]
pub fn KnowledgeView(
    snapshot: KnowledgeSnapshot,
    selected_page: Option<Uuid>,
    on_select_page: Callback<Uuid>,
    on_edit_block: Callback<(Uuid, String)>,
    on_add_block: Callback<(Uuid, Uuid, usize)>,
    on_add_page: Callback<(Uuid, String)>,
    on_edit_property: Callback<(Uuid, String, serde_json::Value)>,
) -> Element {
    if snapshot.vaults.is_empty() {
        return rsx! {
            div { "data-testid": "knowledge-empty",
                Text { variant: TextVariant::Muted, "Waiting on vault snapshot…" }
            }
        };
    }
    let vault = snapshot.vaults[0].clone();
    let pages = snapshot
        .pages_by_vault
        .get(&vault.id)
        .cloned()
        .unwrap_or_default();
    let active_page = selected_page.or_else(|| pages.first().map(|p| p.id));

    rsx! {
        div { class: "grid grid-cols-1 md:grid-cols-[260px_1fr] gap-4",
            // ─ Page list (left) ─
            section {
                "data-testid": "knowledge-page-list",
                class: "rounded-md border border-border bg-card p-3 flex flex-col gap-2",
                HStack { class: "items-baseline justify-between mb-1",
                    Heading { level: HeadingLevel::H3, "Pages" }
                    Text { variant: TextVariant::Muted, "{pages.len()}" }
                }
                ul { class: "flex flex-col gap-1",
                    for page in pages.iter() {
                        PageRow {
                            key: "{page.id}",
                            page: page.clone(),
                            is_active: Some(page.id) == active_page,
                            on_select: on_select_page,
                        }
                    }
                }
                AddPageRow {
                    vault_id: vault.id,
                    on_add_page,
                }
            }
            // ─ Page body (right) ─
            section { class: "rounded-md border border-border bg-card p-4 flex flex-col gap-3",
                PageBody {
                    snapshot: snapshot.clone(),
                    vault_id: vault.id,
                    active_page,
                    pages: pages.clone(),
                    on_edit_block,
                    on_add_block,
                    on_edit_property,
                }
            }
        }
    }
}

#[component]
fn PageBody(
    snapshot: KnowledgeSnapshot,
    vault_id: Uuid,
    active_page: Option<Uuid>,
    pages: Vec<Page>,
    on_edit_block: Callback<(Uuid, String)>,
    on_add_block: Callback<(Uuid, Uuid, usize)>,
    on_edit_property: Callback<(Uuid, String, serde_json::Value)>,
) -> Element {
    let Some(page_id) = active_page else {
        return rsx! { Text { variant: TextVariant::Muted, "Select a page from the list." } };
    };
    let blocks = snapshot
        .blocks_by_page
        .get(&page_id)
        .cloned()
        .unwrap_or_default();
    let page = pages.iter().find(|p| p.id == page_id).cloned();
    let basename = page
        .as_ref()
        .map(|p| p.basename.clone())
        .unwrap_or_default();

    // Phase 6.5b: render the properties pane for whichever `kind:`
    // the page declares. Schema lookup uses the built-in registry
    // (server has the same). If the page doesn't carry a `kind:`
    // we skip the pane entirely — untyped pages still render their
    // blocks below.
    let schema = page.as_ref().and_then(|p| {
        let fm: serde_json::Value =
            serde_json::from_str(&p.frontmatter_json).unwrap_or(serde_json::Value::Null);
        let kind = fm.get("kind").and_then(|v| v.as_str())?;
        knowledge_proto::property_schema::PropertySchemaRegistry::with_builtins().get(kind)
    });

    let on_prop_change = use_callback(move |(key, value): (String, serde_json::Value)| {
        on_edit_property.call((page_id, key, value));
    });

    rsx! {
        HStack { class: "items-baseline justify-between mb-2",
            Heading { level: HeadingLevel::H2, "{basename}" }
            Text { variant: TextVariant::Muted, "{blocks.len()} block(s)" }
        }
        if let (Some(schema), Some(page)) = (schema, page) {
            crate::properties_pane::PropertiesPane {
                schema,
                frontmatter_json: page.frontmatter_json.clone(),
                on_change: on_prop_change,
            }
        }
        div {
            "data-testid": "knowledge-block-list",
            class: "flex flex-col gap-1.5",
            for block in blocks.iter() {
                BlockRow { key: "{block.id}", block: block.clone(), on_edit_block }
            }
        }
        AddBlockButton {
            page_id,
            vault_id,
            after_count: blocks.len(),
            on_add_block,
        }
    }
}

#[component]
fn PageRow(page: Page, is_active: bool, on_select: Callback<Uuid>) -> Element {
    let page_id = page.id;
    let row_testid = format!("page-row-{page_id}");
    let cls = if is_active {
        "text-sm rounded px-2 py-1 cursor-pointer bg-accent text-accent-foreground"
    } else {
        "text-sm rounded px-2 py-1 cursor-pointer hover:bg-muted"
    };
    rsx! {
        li {
            "data-testid": row_testid,
            class: cls,
            onclick: move |_| on_select.call(page_id),
            "{page.basename}"
        }
    }
}

#[component]
fn AddPageRow(vault_id: Uuid, on_add_page: Callback<(Uuid, String)>) -> Element {
    let mut value = use_signal(String::new);
    let mut submit = move |_: ()| {
        let v = value.read().clone();
        if v.trim().is_empty() {
            return;
        }
        on_add_page.call((vault_id, v.trim().to_string()));
        value.set(String::new());
    };
    rsx! {
        HStack { class: "gap-2 mt-1",
            input {
                "data-testid": "knowledge-new-page-input",
                r#type: "text",
                class: "flex-1 rounded border border-border bg-background px-2 py-1 text-sm",
                value: "{value}",
                placeholder: "New page basename",
                oninput: move |e| value.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { submit(()); }
                },
            }
            span { "data-testid": "knowledge-add-page-button",
                Button {
                    on_click: move |_| submit(()),
                    "Add"
                }
            }
        }
    }
}

#[component]
fn BlockRow(block: Block, on_edit_block: Callback<(Uuid, String)>) -> Element {
    let id = block.id;
    let row_testid = format!("block-row-{id}");
    let area_testid = format!("block-textarea-{id}");
    rsx! {
        div {
            "data-testid": row_testid,
            class: "rounded border border-border bg-background p-2",
            textarea {
                "data-testid": area_testid,
                class: "w-full bg-transparent text-sm text-foreground outline-none resize-none",
                rows: 2,
                value: "{block.content}",
                oninput: move |e| on_edit_block.call((id, e.value())),
            }
        }
    }
}

#[component]
fn AddBlockButton(
    page_id: Uuid,
    vault_id: Uuid,
    after_count: usize,
    on_add_block: Callback<(Uuid, Uuid, usize)>,
) -> Element {
    rsx! {
        HStack {
            span { "data-testid": "knowledge-add-block-button",
                Button {
                    on_click: move |_| on_add_block.call((page_id, vault_id, after_count)),
                    "+ block"
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct KnowledgeSnapshot {
    pub vaults: Vec<Vault>,
    pub pages_by_vault: HashMap<Uuid, Vec<Page>>,
    pub blocks_by_page: HashMap<Uuid, Vec<Block>>,
}

fn big_page() -> PageWindow {
    PageWindow {
        index: 0,
        size: 1000,
    }
}

async fn build_snapshot(doc: Arc<CrdtDoc>) -> Result<KnowledgeSnapshot, String> {
    let vault_repo = VaultRepoLoro::new(&doc);
    let page_repo = PageRepoLoro::new(&doc);
    let block_repo = BlockRepoLoro::new(&doc);

    let vaults = vault_repo
        .list(big_page(), None, None)
        .await
        .map_err(|e| format!("vault list: {e}"))?;
    let pages = page_repo
        .list(big_page(), None, None)
        .await
        .map_err(|e| format!("page list: {e}"))?;
    let blocks = block_repo
        .list(big_page(), None, None)
        .await
        .map_err(|e| format!("block list: {e}"))?;

    let mut pages_by_vault: HashMap<Uuid, Vec<Page>> = HashMap::new();
    for p in pages.items.into_iter() {
        pages_by_vault.entry(p.vault_id).or_default().push(p);
    }
    // Sort each vault's pages by basename for stable rendering.
    for v in pages_by_vault.values_mut() {
        v.sort_by(|a, b| a.basename.cmp(&b.basename));
    }

    let mut blocks_by_page: HashMap<Uuid, Vec<Block>> = HashMap::new();
    for b in blocks.items.into_iter() {
        blocks_by_page.entry(b.page_id).or_default().push(b);
    }
    for v in blocks_by_page.values_mut() {
        v.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }

    Ok(KnowledgeSnapshot {
        vaults: vaults.items,
        pages_by_vault,
        blocks_by_page,
    })
}

async fn run_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    mut version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
) {
    let sub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "knowledge subscribe-client connect failed");
            last_error.set(Some(e));
            return;
        }
    };
    let apply_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "knowledge apply-client connect failed");
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

    // Phase 5c: org vault doc only. Per-project vault docs follow
    // when the project routes need them.
    let doc_id = project_proto::DocId::org_vault();
    let upload_doc_id = doc_id.clone();
    spawn(async move {
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_client
                .apply_update(upload_doc_id.clone(), UpdateBytes(bytes))
                .await
            {
                tracing::warn!(?e, "knowledge apply_update failed");
            }
        }
    });

    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let sub_doc_id = doc_id.clone();
    spawn(async move {
        if let Err(e) = sub_client.subscribe(sub_doc_id, tx).await {
            tracing::warn!(error = ?e, "knowledge WorkspaceSync::subscribe ended with error");
        }
    });

    loop {
        match rx.recv().await {
            Ok(Some(msg)) => {
                let bytes = &msg.get().0;
                if let Err(e) = doc.apply_remote(bytes) {
                    tracing::warn!(?e, "knowledge apply_remote failed");
                    last_error.set(Some(format!("apply_remote: {e}")));
                    continue;
                }
                version.with_mut(|v| *v += 1);
            }
            Ok(None) => {
                tracing::info!("knowledge sync stream closed by server");
                last_error.set(Some("stream closed by server".into()));
                return;
            }
            Err(e) => {
                tracing::warn!(?e, "knowledge rx.recv failed");
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
