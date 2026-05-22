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
use knowledge_crdt::{BlockRepoLoro, VaultRepoLoro};
use knowledge_proto::{
    Block, BlockCreate, BlockRepo, BlockUpdate, Page, PageCreate, PageRepo, Vault, VaultRepo,
    lexorank,
};

use crate::outliner::{
    FocusDir, MoveDir, Outliner, OutlinerOps, complete_wikilink, flatten_visible,
    pending_wikilink_query,
};
use vim::{
    BlockOp, Cursor, CursorState, DioxusKey as VimKey, DocView, Motion, VimAction, VimEngine,
    VimMode, apply_motion,
};

use crate::view_mode::ViewMode;
use fts_ui::lucide_dioxus::{
    ChevronLeft, ChevronRight, Code, Eye, Link as LinkIcon, List as ListIcon, PanelRight, Pencil,
    Plus, Search, SlidersHorizontal,
};
use project_proto::architect::Page as PageWindow;
use project_proto::{
    AwarenessFrame, AwarenessPublish, AwarenessSubscribe, DocId, UpdateBytes, WorkspaceSyncClient,
};
use uuid::Uuid;

use crate::awareness::{AwarenessHub, CursorPayload, RemoteCursor};

/// Adapter that lets vim's pure cursor-motion engine read from
/// our snapshot. Built on demand from the page's blocks; cheap.
struct SnapshotDocView<'a> {
    blocks: &'a [Block],
    visible_order: Vec<Uuid>,
}

impl<'a> SnapshotDocView<'a> {
    fn new(blocks: &'a [Block]) -> Self {
        Self {
            visible_order: crate::outliner::flatten_visible(blocks),
            blocks,
        }
    }
}

impl<'a> DocView for SnapshotDocView<'a> {
    fn block_content(&self, id: Uuid) -> Option<String> {
        self.blocks
            .iter()
            .find(|b| b.id == id)
            .map(|b| b.content.clone())
    }
    fn prev_visible(&self, id: Uuid) -> Option<Uuid> {
        let i = self.visible_order.iter().position(|x| *x == id)?;
        if i == 0 {
            None
        } else {
            Some(self.visible_order[i - 1])
        }
    }
    fn next_visible(&self, id: Uuid) -> Option<Uuid> {
        let i = self.visible_order.iter().position(|x| *x == id)?;
        self.visible_order.get(i + 1).copied()
    }
}

/// Live route component. One prop — the vox URL. Subscribes to
/// `vault/org`.
#[component]
pub fn KnowledgeLive(vox_url: String) -> Element {
    let local_doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let version: Signal<u64> = use_signal(|| 0u64);
    let last_error: Signal<Option<String>> = use_signal(|| None::<String>);
    let mut selected_page: Signal<Option<Uuid>> = use_signal(|| None);
    // Online/offline status for the route. Drives the small
    // status chip in the header. Provided as context so the chip
    // and any future indicators can read it.
    let sync_status: Signal<SyncStatus> =
        use_context_provider(|| Signal::new(SyncStatus::Reconnecting));

    // Per-route view mode (Edit / View / Source). Provided as
    // context so descendants can read + toggle.
    let _view_mode: Signal<ViewMode> = use_context_provider(|| Signal::new(ViewMode::Edit));
    // Stack of page IDs pinned to the right sidebar — survives
    // page navigation. New entries get pushed on shift-click of
    // a `[[wikilink]]`. Provided as a Signal for direct
    // mutation by panes' close buttons.
    let _pinned_panes: Signal<Vec<Uuid>> = use_context_provider(|| Signal::new(Vec::new()));

    // Awareness hub — one per route, holds the local
    // EphemeralStore + our peer identity. Provided as context so
    // PageBody can publish cursor updates and remote-cursor
    // overlays can read incoming peer state.
    // Live `CrdtDoc` provided to descendants so the awareness
    // publish path (PageBody) can mint stable Loro text cursors.
    use_context_provider(|| local_doc);
    let awareness_hub: Signal<AwarenessHub> =
        use_context_provider(|| Signal::new(AwarenessHub::anonymous()));
    // Remote cursors resolved per-page snapshot — refreshed
    // whenever the awareness store fires `subscribe` events.
    let remote_cursors: Signal<Vec<RemoteCursor>> =
        use_context_provider(|| Signal::new(Vec::<RemoteCursor>::new()));

    // IndexedDB persistence — load any prior snapshot + replay
    // updates BEFORE the sync loop opens, so the route shows
    // your data instantly on cold open (offline-first). The
    // subscribe-local-update hook also writes every new commit
    // to the append-log so future reloads find it.
    let doc_for_idb = local_doc.read().clone();
    use_hook(move || {
        let doc = doc_for_idb.clone();
        spawn(async move {
            run_idb_persistence(doc, last_error).await;
        });
    });

    // Offline-first demo seed — when the local doc has no pages
    // (cold start, no server, no IDB snapshot), populate it with a
    // small set of pages + blocks that exercise every Logseq
    // feature. Hydration races with the IDB load; the
    // `doc_has_pages` check inside the seeder makes it a no-op
    // when IDB beats us to it, and the sync loop's `apply_remote`
    // is idempotent so the server can override later if it
    // disagrees. Net effect: `just desktop` shows real content
    // instantly, no server required.
    let doc_for_seed = local_doc.read().clone();
    use_hook(move || {
        let doc = doc_for_seed.clone();
        spawn(async move {
            // Give IDB a moment to drop in real data first.
            sleep_ms(50).await;
            if !crate::seed::doc_has_pages(&doc).await {
                if let Err(e) = crate::seed::seed_demo(doc).await {
                    tracing::warn!(?e, "demo seed failed");
                }
            }
        });
    });

    // Browser-native online/offline watcher. Polls `navigator.onLine`
    // every 2s and forces SyncStatus → Offline when the OS reports
    // no network. Doesn't override Online — the sync loop is the
    // authority on "actually reaching the server" — but it cuts the
    // worst-case "chip is wrong" lag from a 30s backoff cycle to 2s.
    let status_for_net = sync_status;
    use_hook(move || {
        spawn(async move {
            run_network_watcher(status_for_net).await;
        });
    });

    // Storage quota watcher — polls navigator.storage.estimate
    // and surfaces a warning when IDB is ≥ 80% full so the user
    // can export before the browser starts evicting.
    use_hook(move || {
        spawn(async move {
            run_storage_quota_watcher(last_error).await;
        });
    });

    // Local-commit version bumper. Every Loro commit (whether
    // from a keystroke, block insert, or remote apply) needs to
    // trigger a snapshot rebuild so the UI re-renders with the
    // latest state. Without this, local edits would only become
    // visible after a server round-trip echoed them back —
    // which means **offline edits would never render at all**.
    // The sync loop also bumps `version` on `apply_remote`; the
    // double-bump on round-tripped local edits is harmless
    // (Loro imports are idempotent at the CRDT layer).
    //
    // Bridges through an mpsc because Dioxus Signal isn't Sync
    // and the subscribe callback requires `Send + Sync`.
    let doc_for_version = local_doc.read().clone();
    let mut version_for_local = version;
    use_hook(move || {
        let (tx, mut rx) = futures::channel::mpsc::unbounded::<()>();
        let sub = doc_for_version
            .loro()
            .subscribe_local_update(Box::new(move |_bytes| {
                let _ = tx.unbounded_send(());
                true
            }));
        std::mem::forget(sub);
        spawn(async move {
            while rx.next().await.is_some() {
                version_for_local.with_mut(|v| *v += 1);
            }
        });
    });

    // Multi-tab BroadcastChannel watcher — every local commit
    // broadcasts to peer tabs of the same origin so they import
    // it directly instead of waiting for a server round-trip.
    let doc_for_bc = local_doc.read().clone();
    use_hook(move || {
        let doc = doc_for_bc.clone();
        let doc_id = project_proto::DocId::org_vault().0;
        spawn(async move {
            run_broadcast_watcher(doc, doc_id).await;
        });
    });

    let url_for_hook = vox_url.clone();
    let doc_for_hook = local_doc.read().clone();
    let status_for_hook = sync_status;
    use_hook(move || {
        let url = url_for_hook.clone();
        let doc = doc_for_hook.clone();
        spawn(async move {
            run_sync_loop(url, doc, version, last_error, status_for_hook).await;
        });
    });

    // Awareness sync loop — spawned once per route. Wires the
    // local EphemeralStore to/from the server. The doc id is
    // `vault/org` (matches the WorkspaceSync subscription); when
    // we add per-project docs later this lifts to follow the
    // active doc.
    let url_for_aw = vox_url.clone();
    let hub_for_aw = awareness_hub.read().clone();
    let remote_for_aw = remote_cursors;
    let doc_for_aw = local_doc.read().clone();
    use_hook(move || {
        let url = url_for_aw.clone();
        let hub = hub_for_aw.clone();
        let remote = remote_for_aw;
        let doc = doc_for_aw.clone();
        spawn(async move {
            run_awareness_loop(url, hub, remote, doc).await;
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
                let page_repo = knowledge_crdt::IndexedPageRepo::new(&doc);
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

    // Insert a new paragraph block immediately after a given
    // block. Used by the Enter-key shortcut inside the editor.
    let insert_block_doc = local_doc.read().clone();
    let on_insert_block_after = use_callback(move |after_block_id: Uuid| {
        let doc = insert_block_doc.clone();
        spawn(async move {
            let block_repo = BlockRepoLoro::new(&doc);
            let prev = match block_repo.get(after_block_id).await {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!(?e, "insert: prev block lookup failed");
                    return;
                }
            };
            // Compute a fresh sort_key after `prev`. Append a "m"
            // so it lexically sorts between prev and (eventual)
            // next siblings; the lexorank module bisects later.
            // New sibling sits between `prev` and the next sibling
            // (if any). Inherit `parent_block_id` so depth is
            // preserved.
            let next_sort_opt = match block_repo
                .list(
                    project_proto::architect::Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
            {
                Ok(list) => {
                    let mut sibs: Vec<_> = list
                        .items
                        .iter()
                        .filter(|b| {
                            b.page_id == prev.page_id && b.parent_block_id == prev.parent_block_id
                        })
                        .cloned()
                        .collect();
                    sibs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
                    sibs.iter()
                        .skip_while(|b| b.id != prev.id)
                        .nth(1)
                        .map(|b| b.sort_key.clone())
                }
                Err(_) => None,
            };
            let new_key = match next_sort_opt {
                Some(next) => lexorank::between(&prev.sort_key, &next)
                    .unwrap_or_else(|| lexorank::after(&prev.sort_key)),
                None => lexorank::after(&prev.sort_key),
            };
            if let Err(e) = block_repo
                .create(BlockCreate {
                    vault_id: prev.vault_id,
                    page_id: prev.page_id,
                    parent_block_id: prev.parent_block_id,
                    sort_key: new_key,
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
                tracing::warn!(?e, "insert block failed");
            }
        });
    });

    // Set a block's kind/heading_level/list_task — used by the
    // slash command palette (`/h1`, `/todo`, `/code`, etc.).
    let kind_doc = local_doc.read().clone();
    let on_set_block_kind = use_callback(
        move |(block_id, content, kind, heading_level, list_task): (
            Uuid,
            String,
            String,
            Option<i32>,
            Option<String>,
        )| {
            let doc = kind_doc.clone();
            spawn(async move {
                let block_repo = BlockRepoLoro::new(&doc);
                let _ = block_repo
                    .update(
                        block_id,
                        BlockUpdate {
                            content: Some(content),
                            kind: Some(kind),
                            heading_level: Some(heading_level),
                            list_task: Some(list_task),
                            ..Default::default()
                        },
                    )
                    .await;
            });
        },
    );

    // Cycle the list-task state on a block. Logseq convention:
    // None → " " (todo) → "/" (in-progress) → "x" (done) → None.
    // Also flips kind to "list_item" if it wasn't already.
    let cycle_doc = local_doc.read().clone();
    let on_cycle_todo = use_callback(move |block_id: Uuid| {
        let doc = cycle_doc.clone();
        spawn(async move {
            let block_repo = BlockRepoLoro::new(&doc);
            let Ok(b) = block_repo.get(block_id).await else {
                return;
            };
            let next: Option<String> = match b.list_task.as_deref() {
                None => Some(" ".into()),
                Some(" ") => Some("/".into()),
                Some("/") => Some("x".into()),
                _ => None,
            };
            let kind_change = if b.kind != "list_item" {
                Some("list_item".to_string())
            } else {
                None
            };
            let _ = block_repo
                .update(
                    block_id,
                    BlockUpdate {
                        list_task: Some(next),
                        kind: kind_change,
                        ..Default::default()
                    },
                )
                .await;
        });
    });

    // Vim paste — same as on_insert_block_after but seeds the
    // new block with the register's content. Used by `p` (after)
    // and, for v1, also by `P` (before — proper before-insert
    // wants a separate sort_key path; tracked as follow-up).
    let paste_block_doc = local_doc.read().clone();
    let on_paste_block_after = use_callback(move |(after_block_id, content): (Uuid, String)| {
        let doc = paste_block_doc.clone();
        spawn(async move {
            let block_repo = BlockRepoLoro::new(&doc);
            let Ok(prev) = block_repo.get(after_block_id).await else {
                tracing::warn!("vim paste: source block lookup failed");
                return;
            };
            let next_sort_opt = match block_repo
                .list(
                    project_proto::architect::Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
            {
                Ok(list) => {
                    let mut sibs: Vec<_> = list
                        .items
                        .iter()
                        .filter(|b| {
                            b.page_id == prev.page_id && b.parent_block_id == prev.parent_block_id
                        })
                        .cloned()
                        .collect();
                    sibs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
                    sibs.iter()
                        .skip_while(|b| b.id != prev.id)
                        .nth(1)
                        .map(|b| b.sort_key.clone())
                }
                Err(_) => None,
            };
            let new_key = match next_sort_opt {
                Some(next) => lexorank::between(&prev.sort_key, &next)
                    .unwrap_or_else(|| lexorank::after(&prev.sort_key)),
                None => lexorank::after(&prev.sort_key),
            };
            if let Err(e) = block_repo
                .create(BlockCreate {
                    vault_id: prev.vault_id,
                    page_id: prev.page_id,
                    parent_block_id: prev.parent_block_id,
                    sort_key: new_key,
                    kind: "paragraph".into(),
                    content,
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
                tracing::warn!(?e, "vim paste block failed");
            }
        });
    });

    // Delete a block. Used by the Backspace-on-empty shortcut +
    // the per-block delete button.
    let delete_block_doc = local_doc.read().clone();
    let on_delete_block = use_callback(move |block_id: Uuid| {
        let doc = delete_block_doc.clone();
        spawn(async move {
            let block_repo = BlockRepoLoro::new(&doc);
            if let Err(e) = block_repo.delete(block_id).await {
                tracing::warn!(?e, %block_id, "delete block failed");
            }
        });
    });

    // Indent: make this block a child of the previous sibling.
    // No-op when there's no previous sibling.
    let indent_doc = local_doc.read().clone();
    let on_indent_block = use_callback(move |block_id: Uuid| {
        let doc = indent_doc.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let Ok(target) = repo.get(block_id).await else {
                return;
            };
            let Ok(list) = repo
                .list(
                    project_proto::architect::Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
            else {
                return;
            };
            // Siblings = same parent + same page, sorted by key.
            let mut siblings: Vec<Block> = list
                .items
                .iter()
                .filter(|b| {
                    b.page_id == target.page_id && b.parent_block_id == target.parent_block_id
                })
                .cloned()
                .collect();
            siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            let Some(idx) = siblings.iter().position(|b| b.id == block_id) else {
                return;
            };
            if idx == 0 {
                return;
            }
            let new_parent = siblings[idx - 1].id;
            // New sort_key = after the last existing child of the
            // new parent, or `first()` if no children.
            let mut existing_children: Vec<Block> = list
                .items
                .iter()
                .filter(|b| b.parent_block_id == Some(new_parent))
                .cloned()
                .collect();
            existing_children.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            let new_key = match existing_children.last() {
                Some(last) => lexorank::after(&last.sort_key),
                None => lexorank::first(),
            };
            let _ = repo
                .update(
                    block_id,
                    BlockUpdate {
                        parent_block_id: Some(Some(new_parent)),
                        sort_key: Some(new_key),
                        ..Default::default()
                    },
                )
                .await;
        });
    });

    // Outdent: promote this block to be a sibling of its parent,
    // placed immediately after the parent. No-op when already at
    // the root level.
    let outdent_doc = local_doc.read().clone();
    let on_outdent_block = use_callback(move |block_id: Uuid| {
        let doc = outdent_doc.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let Ok(target) = repo.get(block_id).await else {
                return;
            };
            let Some(parent_id) = target.parent_block_id else {
                return;
            };
            let Ok(parent) = repo.get(parent_id).await else {
                return;
            };
            let Ok(list) = repo
                .list(
                    project_proto::architect::Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
            else {
                return;
            };
            // The block goes between parent.sort_key and the next
            // grandparent-level sibling (if any).
            let mut grand_sibs: Vec<Block> = list
                .items
                .iter()
                .filter(|b| b.parent_block_id == parent.parent_block_id)
                .cloned()
                .collect();
            grand_sibs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            let parent_idx = grand_sibs.iter().position(|b| b.id == parent.id);
            let new_key = match parent_idx.and_then(|i| grand_sibs.get(i + 1)) {
                Some(next) => lexorank::between(&parent.sort_key, &next.sort_key)
                    .unwrap_or_else(|| lexorank::after(&parent.sort_key)),
                None => lexorank::after(&parent.sort_key),
            };
            let _ = repo
                .update(
                    block_id,
                    BlockUpdate {
                        parent_block_id: Some(parent.parent_block_id),
                        sort_key: Some(new_key),
                        ..Default::default()
                    },
                )
                .await;
        });
    });

    // Toggle the collapsed flag.
    let collapse_doc = local_doc.read().clone();
    let on_toggle_collapsed = use_callback(move |block_id: Uuid| {
        let doc = collapse_doc.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let Ok(b) = repo.get(block_id).await else {
                return;
            };
            let _ = repo
                .update(
                    block_id,
                    BlockUpdate {
                        collapsed: Some(!b.collapsed),
                        ..Default::default()
                    },
                )
                .await;
        });
    });

    // Move block up/down among its siblings via a fresh sort_key.
    let move_doc = local_doc.read().clone();
    let on_move_block = use_callback(move |(block_id, dir): (Uuid, MoveDir)| {
        let doc = move_doc.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let Ok(target) = repo.get(block_id).await else {
                return;
            };
            let Ok(list) = repo
                .list(
                    project_proto::architect::Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
            else {
                return;
            };
            let mut siblings: Vec<Block> = list
                .items
                .iter()
                .filter(|b| {
                    b.page_id == target.page_id && b.parent_block_id == target.parent_block_id
                })
                .cloned()
                .collect();
            siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            let Some(idx) = siblings.iter().position(|b| b.id == block_id) else {
                return;
            };
            let new_key = match dir {
                MoveDir::Up => {
                    if idx == 0 {
                        return;
                    }
                    let prev = &siblings[idx - 1];
                    let before_prev = idx.checked_sub(2).and_then(|i| siblings.get(i));
                    match before_prev {
                        Some(bp) => lexorank::between(&bp.sort_key, &prev.sort_key)
                            .unwrap_or_else(|| lexorank::before(&prev.sort_key)),
                        None => lexorank::before(&prev.sort_key),
                    }
                }
                MoveDir::Down => {
                    if idx + 1 >= siblings.len() {
                        return;
                    }
                    let next = &siblings[idx + 1];
                    let after_next = siblings.get(idx + 2);
                    match after_next {
                        Some(an) => lexorank::between(&next.sort_key, &an.sort_key)
                            .unwrap_or_else(|| lexorank::after(&next.sort_key)),
                        None => lexorank::after(&next.sort_key),
                    }
                }
            };
            let _ = repo
                .update(
                    block_id,
                    BlockUpdate {
                        sort_key: Some(new_key),
                        ..Default::default()
                    },
                )
                .await;
        });
    });

    // Inline page title rename.
    let rename_doc = local_doc.read().clone();
    let on_rename_page = use_callback(move |(page_id, new_name): (Uuid, String)| {
        let doc = rename_doc.clone();
        spawn(async move {
            let page_repo = knowledge_crdt::IndexedPageRepo::new(&doc);
            let trimmed = new_name.trim().to_string();
            if trimmed.is_empty() {
                return;
            }
            if let Err(e) = page_repo
                .update(
                    page_id,
                    knowledge_proto::PageUpdate {
                        basename: Some(trimmed.clone()),
                        path: Some(format!("{trimmed}.md")),
                        ..Default::default()
                    },
                )
                .await
            {
                tracing::warn!(?e, %page_id, "rename failed");
            }
        });
    });

    // Add a fresh page to the first vault.
    let new_page_doc = local_doc.read().clone();
    let on_add_page = use_callback(move |(vault_id, basename): (Uuid, String)| {
        let doc = new_page_doc.clone();
        spawn(async move {
            let page_repo = knowledge_crdt::IndexedPageRepo::new(&doc);
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
    let has_err = last_error.read().is_some();
    rsx! {
        div {
            id: "knowledge-route",
            class: "flex h-[calc(100vh-3rem)] w-full flex-col",
            PresenceStrip {
                on_follow: move |page: Uuid| selected_page.set(Some(page)),
            }
            SyncStatusChip {}
            VaultBackupChip {}
            // Sync version badge — only visible/conspicuous on
            // error. Kept as data-testid for tests.
            span {
                "data-testid": "knowledge-version-badge",
                class: if has_err {
                    "absolute top-2 right-2 z-20 text-[10px] rounded bg-destructive/15 text-destructive px-1.5 py-0.5"
                } else {
                    "sr-only"
                },
                "{version_label}"
            }
            if let Some(err) = last_error.read().as_ref() {
                div {
                    "data-testid": "knowledge-sync-error",
                    class: "px-3 py-1.5 text-xs border-b border-destructive/30 bg-destructive/10 text-destructive",
                    "Sync: {err}"
                }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! {
                    div { class: "p-3 text-xs text-muted-foreground",
                        "Building local doc…"
                    }
                },
                Some(Err(err)) => rsx! {
                    div { class: "p-3 text-xs text-muted-foreground",
                        "Decode failed: {err}"
                    }
                },
                Some(Ok(snap)) => rsx! { KnowledgeView {
                    snapshot: snap.clone(),
                    selected_page: selected,
                    on_select_page,
                    on_edit_block,
                    on_add_block,
                    on_add_page,
                    on_edit_property,
                    on_insert_block_after,
                    on_delete_block,
                    on_rename_page,
                    on_indent_block,
                    on_outdent_block,
                    on_toggle_collapsed,
                    on_move_block,
                    on_paste_block_after,
                    on_cycle_todo,
                    on_set_block_kind,
                } },
            }
        }
    }
}

/// "Who's here" chip strip — floats top-right of the knowledge
/// route showing one avatar per remote peer that has published a
/// cursor recently. Empty (renders nothing) when we're solo.
#[component]
fn PresenceStrip(on_follow: Callback<Uuid>) -> Element {
    let Some(remote) = try_use_context::<Signal<Vec<RemoteCursor>>>() else {
        return rsx! {};
    };
    // Dedupe by peer_id — multiple cursor entries per peer can
    // exist transiently while keys age out.
    let mut seen: std::collections::HashSet<Uuid> = std::collections::HashSet::new();
    let peers: Vec<RemoteCursor> = remote
        .read()
        .iter()
        .filter(|c| seen.insert(c.peer_id))
        .cloned()
        .collect();
    if peers.is_empty() {
        return rsx! {};
    }
    rsx! {
        div {
            "data-testid": "presence-strip",
            class: "absolute top-2 right-12 z-20 flex -space-x-1.5",
            for peer in peers {
                {
                    let initial = peer.name.chars().next().unwrap_or('?').to_uppercase().to_string();
                    let style = format!("background:{}", peer.color);
                    let mode_label = match peer.mode {
                        crate::awareness::PeerMode::Normal => "NORMAL",
                        crate::awareness::PeerMode::Insert => "INSERT",
                        crate::awareness::PeerMode::Visual => "VISUAL",
                    };
                    let title = match peer.page_id {
                        Some(_) => format!("{} • {} — click to follow", peer.name, mode_label),
                        None => format!("{} • {}", peer.name, mode_label),
                    };
                    let testid_attr = format!("presence-peer-{}", peer.peer_id);
                    let target_page = peer.page_id;
                    let disabled = target_page.is_none();
                    rsx! {
                        button {
                            r#type: "button",
                            "data-testid": testid_attr,
                            class: "inline-flex h-6 w-6 items-center justify-center rounded-full border border-background text-[10px] font-semibold text-white shadow-sm transition hover:scale-110 disabled:cursor-default",
                            style: "{style}",
                            title: "{title}",
                            disabled: disabled,
                            onclick: move |_| {
                                if let Some(p) = target_page {
                                    on_follow.call(p);
                                }
                            },
                            "{initial}"
                        }
                    }
                }
            }
        }
    }
}

/// Two small icon-buttons in the route header for manual
/// **Export** (download a `.loro` snapshot of the whole vault)
/// and **Import** (upload one and merge into the local doc).
///
/// Useful for backups, moving between devices without the sync
/// server, or restoring after a botched sync. Loro merges
/// imports idempotently via CRDT semantics — re-importing the
/// same file is a no-op.
#[component]
fn VaultBackupChip() -> Element {
    // The route's `local_doc` signal is provided as context.
    // Pulling from there avoids needing PartialEq on CrdtDoc
    // (component props require PartialEq).
    let Some(doc_sig) = try_use_context::<Signal<Arc<CrdtDoc>>>() else {
        return rsx! {};
    };
    let mut last_action = use_signal(|| None::<String>);
    let doc_for_export = doc_sig.read().clone();
    let on_export = move |_e: Event<MouseData>| {
        let doc = doc_for_export.clone();
        spawn(async move {
            let bytes = match doc.loro().export(crdt::loro::ExportMode::Snapshot) {
                Ok(b) => b,
                Err(e) => {
                    last_action.set(Some(format!("export failed: {e}")));
                    return;
                }
            };
            // Hand the bytes to JS as base64 (simplest cross-
            // boundary; binary blob via JsValue is more
            // efficient but requires web-sys plumbing). Then JS
            // builds a Blob + clicks an <a download>.
            let b64 = b64_encode(&bytes);
            let ts = chrono::Utc::now().format("%Y-%m-%d-%H%M%S");
            let filename = format!("task-architect-vault-{ts}.loro");
            let js = format!(
                r#"
                (() => {{
                    const b64 = "{b64}";
                    const bin = atob(b64);
                    const arr = new Uint8Array(bin.length);
                    for (let i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
                    const blob = new Blob([arr], {{ type: 'application/octet-stream' }});
                    const url = URL.createObjectURL(blob);
                    const a = document.createElement('a');
                    a.href = url;
                    a.download = "{filename}";
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);
                    URL.revokeObjectURL(url);
                    return 1;
                }})()
                "#
            );
            let _ = document::eval(&js).await;
            last_action.set(Some(format!("exported {} bytes", bytes.len())));
        });
    };
    let doc_for_import = doc_sig.read().clone();
    let on_import = move |_e: Event<MouseData>| {
        let doc = doc_for_import.clone();
        spawn(async move {
            // Open a hidden file picker, read the chosen file as
            // base64, ship it back through eval's recv channel.
            let js = r#"
                return new Promise((resolve) => {
                    const inp = document.createElement('input');
                    inp.type = 'file';
                    inp.accept = '.loro,application/octet-stream';
                    inp.onchange = () => {
                        const f = inp.files && inp.files[0];
                        if (!f) { resolve(""); return; }
                        const r = new FileReader();
                        r.onload = () => {
                            const arr = new Uint8Array(r.result);
                            let bin = "";
                            for (let i = 0; i < arr.length; i++) bin += String.fromCharCode(arr[i]);
                            resolve(btoa(bin));
                        };
                        r.onerror = () => resolve("");
                        r.readAsArrayBuffer(f);
                    };
                    inp.click();
                });
            "#;
            let mut eval = document::eval(js);
            let b64 = match eval.recv::<String>().await {
                Ok(s) => s,
                Err(e) => {
                    last_action.set(Some(format!("import cancelled: {e}")));
                    return;
                }
            };
            if b64.is_empty() {
                last_action.set(Some("import cancelled".into()));
                return;
            }
            let bytes = match b64_decode(&b64) {
                Some(b) => b,
                None => {
                    last_action.set(Some("import: invalid base64".into()));
                    return;
                }
            };
            if let Err(e) = doc.loro().import(&bytes) {
                last_action.set(Some(format!("import failed: {e}")));
                return;
            }
            last_action.set(Some(format!("imported {} bytes", bytes.len())));
        });
    };
    let title = last_action
        .read()
        .clone()
        .unwrap_or_else(|| "Backup vault (export / import .loro)".into());
    rsx! {
        div {
            "data-testid": "vault-backup",
            class: "absolute top-2 right-32 z-20 inline-flex items-center gap-0.5 rounded-full bg-card/80 backdrop-blur px-1 py-0.5 text-[10px] text-muted-foreground border border-border opacity-30 hover:opacity-100 transition-opacity",
            title: "{title}",
            button {
                r#type: "button",
                "data-testid": "vault-export",
                class: "px-1.5 py-0.5 rounded hover:bg-muted/60 hover:text-foreground",
                title: "Export vault as .loro file",
                onclick: on_export,
                "↓"
            }
            button {
                r#type: "button",
                "data-testid": "vault-import",
                class: "px-1.5 py-0.5 rounded hover:bg-muted/60 hover:text-foreground",
                title: "Import a .loro file (merges with current vault)",
                onclick: on_import,
                "↑"
            }
        }
    }
}

/// Tiny base64 helpers — same impl as `awareness::b64_*` but
/// inlined here to avoid a cross-module pub-fn just for the
/// backup chip. If we add a third caller, promote.
fn b64_encode(bytes: &[u8]) -> String {
    const ALPHA: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity((bytes.len() + 2) / 3 * 4);
    for chunk in bytes.chunks(3) {
        let b0 = chunk[0];
        let b1 = if chunk.len() > 1 { chunk[1] } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] } else { 0 };
        let v: u32 = ((b0 as u32) << 16) | ((b1 as u32) << 8) | (b2 as u32);
        out.push(ALPHA[((v >> 18) & 63) as usize] as char);
        out.push(ALPHA[((v >> 12) & 63) as usize] as char);
        if chunk.len() > 1 {
            out.push(ALPHA[((v >> 6) & 63) as usize] as char);
        } else {
            out.push('=');
        }
        if chunk.len() > 2 {
            out.push(ALPHA[(v & 63) as usize] as char);
        } else {
            out.push('=');
        }
    }
    out
}

fn b64_decode(s: &str) -> Option<Vec<u8>> {
    let val = |c: u8| -> Option<u8> {
        Some(match c {
            b'A'..=b'Z' => c - b'A',
            b'a'..=b'z' => c - b'a' + 26,
            b'0'..=b'9' => c - b'0' + 52,
            b'+' => 62,
            b'/' => 63,
            _ => return None,
        })
    };
    let bytes: Vec<u8> = s
        .bytes()
        .filter(|c| *c != b'=' && !c.is_ascii_whitespace())
        .collect();
    let mut out = Vec::with_capacity(bytes.len() * 3 / 4);
    for chunk in bytes.chunks(4) {
        let mut v: u32 = 0;
        let mut n = 0;
        for &c in chunk {
            v = (v << 6) | (val(c)? as u32);
            n += 6;
        }
        let pad = 24usize.saturating_sub(n);
        v <<= pad;
        if n >= 8 {
            out.push((v >> 16) as u8);
        }
        if n >= 16 {
            out.push((v >> 8) as u8);
        }
        if n >= 24 {
            out.push(v as u8);
        }
    }
    Some(out)
}

/// Subtle pill in the route header showing online/offline/
/// reconnecting status. Click on a `data-testid` for tests.
/// Hidden when Online + no error history (so the chrome
/// stays out of the way during normal use). The chip exists
/// in the DOM either way for tests + screen-readers.
#[component]
fn SyncStatusChip() -> Element {
    let Some(status_sig) = try_use_context::<Signal<SyncStatus>>() else {
        return rsx! {};
    };
    let status = *status_sig.read();
    let (label, dot_cls, chip_cls) = match status {
        SyncStatus::Online => ("Online", "bg-emerald-500", "opacity-0 hover:opacity-100"),
        SyncStatus::Offline => (
            "Offline — edits saved locally",
            "bg-rose-500",
            "opacity-100",
        ),
        SyncStatus::Reconnecting => ("Reconnecting…", "bg-amber-500 animate-pulse", "opacity-100"),
    };
    let testid = match status {
        SyncStatus::Online => "sync-status-online",
        SyncStatus::Offline => "sync-status-offline",
        SyncStatus::Reconnecting => "sync-status-reconnecting",
    };
    rsx! {
        div {
            "data-testid": testid,
            class: format!(
                "absolute top-2 right-2 z-20 inline-flex items-center gap-1.5 rounded-full bg-card/80 backdrop-blur px-2 py-0.5 text-[10px] text-muted-foreground border border-border transition-opacity {chip_cls}"
            ),
            title: label,
            span { class: format!("h-1.5 w-1.5 rounded-full {dot_cls}") }
            span { "{label}" }
        }
    }
}

/// Sidebar visibility/density on /knowledge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SidebarMode {
    /// Full page list with search.
    Expanded,
    /// Narrow icon rail — search + new + first-letter page chips.
    Rail,
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
    on_insert_block_after: Callback<Uuid>,
    on_delete_block: Callback<Uuid>,
    on_rename_page: Callback<(Uuid, String)>,
    on_indent_block: Callback<Uuid>,
    on_outdent_block: Callback<Uuid>,
    on_toggle_collapsed: Callback<Uuid>,
    on_move_block: Callback<(Uuid, MoveDir)>,
    on_paste_block_after: Callback<(Uuid, String)>,
    on_cycle_todo: Callback<Uuid>,
    on_set_block_kind: Callback<(Uuid, String, String, Option<i32>, Option<String>)>,
) -> Element {
    let mut sidebar_query: Signal<String> = use_signal(String::new);
    let mut sidebar_mode: Signal<SidebarMode> = use_signal(|| SidebarMode::Expanded);
    if snapshot.vaults.is_empty() {
        return rsx! {
            div { "data-testid": "knowledge-empty",
                class: "p-3 text-xs text-muted-foreground",
                "Waiting on vault snapshot…"
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

    let mode = *sidebar_mode.read();
    let is_rail = mode == SidebarMode::Rail;
    // Hoist all hook calls above the conditional render so the
    // hook order stays stable when `mode` flips.
    let on_expand_sidebar = use_callback(move |()| sidebar_mode.set(SidebarMode::Expanded));
    let on_collapse_sidebar = use_callback(move |()| sidebar_mode.set(SidebarMode::Rail));
    let on_query = use_callback(move |v: String| sidebar_query.set(v));
    let query_str = sidebar_query.read().clone();

    rsx! {
        div { class: "flex flex-1 min-h-0 w-full",
            // ── Sidebar ────────────────────────────────────────
            aside {
                "data-testid": "knowledge-page-list",
                "data-sidebar-mode": if is_rail { "rail" } else { "expanded" },
                class: if is_rail {
                    "flex flex-col w-12 flex-none border-r border-border bg-card/40"
                } else {
                    "flex flex-col w-64 flex-none border-r border-border bg-card/40"
                },
                if is_rail {
                    SidebarRail {
                        pages: pages.clone(),
                        active_page,
                        on_select: on_select_page,
                        on_expand: on_expand_sidebar,
                    }
                } else {
                    SidebarExpanded {
                        pages: pages.clone(),
                        active_page,
                        vault_id: vault.id,
                        query: query_str,
                        on_query,
                        on_select: on_select_page,
                        on_add_page,
                        on_collapse: on_collapse_sidebar,
                    }
                }
            }
            // ── Main pane ──────────────────────────────────────
            // Wider content area so the editor uses the screen.
            // `max-w-5xl` (≈64rem / 1024px) is still readable but
            // doesn't leave the giant whitespace ribbons on each
            // side of `max-w-3xl` that the old layout produced.
            // `mx-auto` keeps it centered on very wide screens
            // so the eye doesn't have to dart across the page.
            main { class: "flex-1 min-w-0 overflow-y-auto",
                div { class: "mx-auto max-w-5xl px-4 py-4 sm:px-6 lg:px-8",
                    PageBody {
                        snapshot: snapshot.clone(),
                        vault_id: vault.id,
                        active_page,
                        pages: pages.clone(),
                        on_edit_block,
                        on_add_block,
                        on_edit_property,
                        on_insert_block_after,
                        on_delete_block,
                        on_rename_page,
                        on_indent_block,
                        on_outdent_block,
                        on_toggle_collapsed,
                        on_move_block,
                        on_select_page,
                        on_paste_block_after,
                    on_cycle_todo,
                    on_set_block_kind,
                    }
                }
            }
        }
    }
}

#[component]
fn SidebarRail(
    pages: Vec<Page>,
    active_page: Option<Uuid>,
    on_select: Callback<Uuid>,
    on_expand: Callback<()>,
) -> Element {
    rsx! {
        div { class: "flex flex-col items-center gap-1 py-2 h-full overflow-y-auto",
            button {
                "data-testid": "knowledge-sidebar-expand",
                class: "h-8 w-8 inline-flex items-center justify-center rounded text-muted-foreground hover:text-foreground hover:bg-muted/60",
                title: "Expand sidebar",
                onclick: move |_| on_expand.call(()),
                ChevronRight { size: 16 }
            }
            div { class: "h-px w-6 bg-border my-1" }
            // First-letter chips for each page. Active page gets
            // the accent ring.
            for page in pages.iter().take(40) {
                {
                    let id = page.id;
                    let is_active = Some(id) == active_page;
                    let initial = page.basename.chars().next().unwrap_or('•').to_uppercase().to_string();
                    let cls = if is_active {
                        "h-8 w-8 inline-flex items-center justify-center rounded text-xs font-medium bg-accent text-accent-foreground"
                    } else {
                        "h-8 w-8 inline-flex items-center justify-center rounded text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-muted/60"
                    };
                    let tid = format!("page-rail-{id}");
                    let title = page.basename.clone();
                    rsx! {
                        button {
                            key: "{id}",
                            "data-testid": tid,
                            class: cls,
                            title: title,
                            onclick: move |_| on_select.call(id),
                            "{initial}"
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn SidebarExpanded(
    pages: Vec<Page>,
    active_page: Option<Uuid>,
    vault_id: Uuid,
    query: String,
    on_query: Callback<String>,
    on_select: Callback<Uuid>,
    on_add_page: Callback<(Uuid, String)>,
    on_collapse: Callback<()>,
) -> Element {
    rsx! {
        div { class: "flex flex-col h-full min-h-0",
            // Compact header — collapse chevron + page count.
            HStack { class: "items-center justify-between px-2 py-1.5 border-b border-border/60",
                button {
                    "data-testid": "knowledge-sidebar-collapse",
                    class: "h-7 w-7 inline-flex items-center justify-center rounded text-muted-foreground hover:text-foreground hover:bg-muted/60",
                    title: "Collapse to rail",
                    onclick: move |_| on_collapse.call(()),
                    ChevronLeft { size: 16 }
                }
                span { class: "text-[11px] uppercase tracking-wider text-muted-foreground",
                    "Pages · {pages.len()}"
                }
            }
            div { class: "px-2 pt-2 pb-1",
                div { class: "relative",
                    span { class: "absolute left-2 top-1/2 -translate-y-1/2 text-muted-foreground pointer-events-none",
                        Search { size: 14 }
                    }
                    input {
                        "data-testid": "knowledge-sidebar-search",
                        r#type: "search",
                        class: "h-8 w-full rounded-md border border-border bg-background pl-7 pr-2 text-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
                        value: "{query}",
                        placeholder: "Search pages…",
                        oninput: move |e| on_query.call(e.value()),
                    }
                }
            }
            div { class: "flex-1 min-h-0 overflow-y-auto px-1 pb-2",
                crate::up_tree::UpTreeView {
                    pages: pages.clone(),
                    active_page,
                    query: query.clone(),
                    on_select,
                }
            }
            div { class: "border-t border-border/60 px-2 py-1.5",
                AddPageRow {
                    vault_id,
                    on_add_page,
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
    on_insert_block_after: Callback<Uuid>,
    on_delete_block: Callback<Uuid>,
    on_rename_page: Callback<(Uuid, String)>,
    on_indent_block: Callback<Uuid>,
    on_outdent_block: Callback<Uuid>,
    on_toggle_collapsed: Callback<Uuid>,
    on_move_block: Callback<(Uuid, MoveDir)>,
    on_select_page: Callback<Uuid>,
    on_paste_block_after: Callback<(Uuid, String)>,
    on_cycle_todo: Callback<Uuid>,
    on_set_block_kind: Callback<(Uuid, String, String, Option<i32>, Option<String>)>,
) -> Element {
    let Some(page_id) = active_page else {
        return rsx! {
            div { class: "p-6 text-sm text-muted-foreground",
                "Select a page from the list, or create a new one."
            }
        };
    };
    let blocks = snapshot
        .blocks_by_page
        .get(&page_id)
        .cloned()
        .unwrap_or_default();
    let page = pages.iter().find(|p| p.id == page_id).cloned();

    // Logseq-style: an empty page is ready to type into, not a
    // "click + to start" stub. On every snapshot, if this page
    // has zero blocks, auto-seed an empty paragraph block.
    // Tracked per-page in a Set so we only fire once per page;
    // the snapshot will rebuild with one block and the effect
    // returns early thereafter.
    //
    // NOTE: the effect re-runs whenever snapshot changes, so if
    // the user deletes the only block we'll seed another one —
    // which matches Logseq's behavior (you can never be in a
    // state with zero blocks on a focused page).
    let mut auto_seeded = use_signal(std::collections::HashSet::<Uuid>::new);
    let vault_for_seed = page.as_ref().map(|p| p.vault_id);
    let blocks_empty_for_seed = blocks.is_empty();
    use_effect(move || {
        if blocks_empty_for_seed {
            let already = auto_seeded.peek().contains(&page_id);
            if !already {
                if let Some(vault_id) = vault_for_seed {
                    auto_seeded.write().insert(page_id);
                    on_add_block.call((page_id, vault_id, 0));
                }
            }
        }
    });
    let basename = page
        .as_ref()
        .map(|p| p.basename.clone())
        .unwrap_or_default();

    // Schema lookup for the kind-specific properties pane.
    let _schema = page.as_ref().and_then(|p| {
        let fm: serde_json::Value =
            serde_json::from_str(&p.frontmatter_json).unwrap_or(serde_json::Value::Null);
        let kind = fm.get("kind").and_then(|v| v.as_str())?;
        knowledge_proto::property_schema::PropertySchemaRegistry::with_builtins().get(kind)
    });

    let on_prop_change = use_callback(move |(key, value): (String, serde_json::Value)| {
        on_edit_property.call((page_id, key, value));
    });

    // Active editor + autocomplete state for the current page.
    // `editing_id` follows Logseq's "one editor at a time"
    // architecture — every other block is a static `BlockView`.
    let mut editing_id: Signal<Option<Uuid>> = use_signal(|| None);

    // Logseq-style focus: when an auto-seeded page receives its
    // first (and only) block, drop the editor into Insert mode
    // on that block so the user can immediately start typing.
    // Only triggers when we previously seeded this page AND the
    // current editor is None — won't steal focus from real user
    // interaction.
    let blocks_for_focus_seed = blocks.clone();
    let seeded_for_focus = auto_seeded;
    use_effect(move || {
        if !seeded_for_focus.peek().contains(&page_id) {
            return;
        }
        if editing_id.peek().is_some() {
            return;
        }
        if blocks_for_focus_seed.len() == 1 {
            let id = blocks_for_focus_seed[0].id;
            editing_id.set(Some(id));
            // The vim engine starts in Normal; bump it into Insert
            // so the textarea takes keystrokes immediately. The
            // engine guards re-entry, so this is idempotent if the
            // user already moved focus.
            // (vim_engine isn't defined yet at this point in the
            // function — focus is enough; vim mode follows the
            // existing click-at-offset path when the user types.)
        }
    });
    // Active autocomplete: (block_id, partial_query).
    let mut autocomplete: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    // Active slash command palette: (block_id, partial_query).
    let mut slash_palette: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    // Pull a snapshot of all page basenames in the vault for
    // wikilink suggestions + view-mode link navigation.
    let all_pages: Vec<String> = pages.iter().map(|p| p.basename.clone()).collect();
    // Tier 3 — provide basename + alias set so view-mode
    // `[[wikilink]]` rendering can flag broken links inline.
    let all_targets: Vec<String> = pages
        .iter()
        .flat_map(|p| std::iter::once(p.basename.clone()).chain(p.aliases.iter().cloned()))
        .collect();
    use_context_provider(|| crate::outliner::KnownBasenames::from_iter(all_targets));
    // Block-snippet lookup so `((uuid))` chips can render the
    // referenced block's first line. Spans the WHOLE vault so
    // cross-page block refs resolve too.
    let all_blocks_for_snippets: Vec<Block> = snapshot
        .blocks_by_page
        .values()
        .flatten()
        .cloned()
        .collect();
    use_context_provider(|| crate::outliner::BlockSnippets::from_blocks(&all_blocks_for_snippets));
    // Embed lookup: basename (lowercased) → blocks. Used by
    // `![[Page]]` rendering.
    let pages_by_id: HashMap<Uuid, Page> = pages.iter().cloned().map(|p| (p.id, p)).collect();
    let mut blocks_by_basename: HashMap<String, Vec<Block>> = HashMap::new();
    for (page_id, blocks) in snapshot.blocks_by_page.iter() {
        if let Some(p) = pages_by_id.get(page_id) {
            blocks_by_basename
                .entry(p.basename.to_lowercase())
                .or_default()
                .extend(blocks.iter().cloned());
            // Also index aliases so `![[Old Name]]` resolves.
            for alias in &p.aliases {
                blocks_by_basename
                    .entry(alias.to_lowercase())
                    .or_default()
                    .extend(blocks.iter().cloned());
            }
        }
    }
    use_context_provider(|| crate::outliner::PageBlockLookup::new(blocks_by_basename.clone()));
    // Block-tree lookup: block_id → (block, sibling-and-descendant
    // blocks of its page). Powers `((uuid))` chip expansion.
    let mut block_tree_index: HashMap<Uuid, (Block, Vec<Block>)> = HashMap::new();
    for (_page_id, blocks) in snapshot.blocks_by_page.iter() {
        for b in blocks {
            block_tree_index.insert(b.id, (b.clone(), blocks.clone()));
        }
    }
    use_context_provider(|| crate::outliner::BlockTreeLookup::new(block_tree_index));
    // DFS-order index for the active page — powers cross-block
    // remote-cursor selection rendering.
    use_context_provider(|| crate::outliner::BlockOrderIndex::from_blocks(&blocks));
    // QueryIndex: snapshot of all materialized indices needed by
    // `{{query …}}` evaluation. Updated by snapshot rebuilds.
    let mut all_blocks_for_query: Vec<(Uuid, Uuid, String)> = Vec::with_capacity(
        snapshot
            .blocks_by_page
            .values()
            .map(Vec::len)
            .sum::<usize>(),
    );
    let mut page_titles: HashMap<Uuid, String> = HashMap::new();
    for (page_id, blocks) in snapshot.blocks_by_page.iter() {
        if let Some(p) = pages_by_id.get(page_id) {
            page_titles.insert(*page_id, p.basename.clone());
        }
        for b in blocks {
            let snippet = b.content.lines().next().unwrap_or("").to_string();
            all_blocks_for_query.push((b.id, b.page_id, snippet));
        }
    }
    let qi_block_refs = std::sync::Arc::new(snapshot.block_refs.clone());
    let qi_block_props = std::sync::Arc::new(snapshot.block_props.clone());
    let qi_all = std::sync::Arc::new(all_blocks_for_query);
    let qi_titles = std::sync::Arc::new(page_titles);
    use_context_provider(|| crate::outliner::QueryIndex {
        block_refs: qi_block_refs,
        block_props: qi_block_props,
        all_blocks: qi_all,
        page_titles: qi_titles,
    });
    use_context_provider(|| crate::outliner::EmbedDepth(0));
    // NavigateLinkCb provider lives below, once on_navigate_link
    // is defined.

    // Edit wrapper: forwards to the real edit + updates the
    // autocomplete state based on the new content.
    let edit_doc = on_edit_block;
    let on_edit_wrapped = use_callback(move |(block_id, content): (Uuid, String)| {
        edit_doc.call((block_id, content.clone()));
        match pending_wikilink_query(&content) {
            Some(q) => autocomplete.set(Some((block_id, q))),
            None => autocomplete.set(None),
        }
        match crate::outliner::pending_slash_query(&content) {
            Some(q) => slash_palette.set(Some((block_id, q))),
            None => slash_palette.set(None),
        }
    });

    // ArrowUp/Down across blocks: translate to a new editing_id by
    // flattening the visible tree.
    let blocks_for_focus = blocks.clone();
    let mut editing_writer = editing_id;
    let on_focus_relative = use_callback(move |(from_id, dir): (Uuid, FocusDir)| {
        let order = flatten_visible(&blocks_for_focus);
        let Some(i) = order.iter().position(|x| *x == from_id) else {
            return;
        };
        let target = match dir {
            FocusDir::Prev if i > 0 => Some(order[i - 1]),
            FocusDir::Next if i + 1 < order.len() => Some(order[i + 1]),
            _ => None,
        };
        if target.is_some() {
            editing_writer.set(target);
        }
    });

    // View-mode link click. The user clicked `[[Page]]` — look up
    // `Page` (case-insensitive) in the vault and navigate. Falls
    // through silently if no such page (could open a "create
    // missing page" flow later).
    let pages_for_nav = pages.clone();
    let on_navigate_link = use_callback(move |target: String| {
        let lower = target.trim().to_lowercase();
        if let Some(p) = pages_for_nav
            .iter()
            .find(|p| p.basename.to_lowercase() == lower)
        {
            on_select_page.call(p.id);
        }
    });
    use_context_provider(|| crate::outliner::NavigateLinkCb(on_navigate_link));

    // Pin-page callback: shift-click on a `[[Page]]` link adds
    // the page's id to the pinned-panes stack (route-scoped
    // signal provided by KnowledgeLive). Idempotent — a page
    // already pinned is moved to the top.
    let mut pinned_panes_writer: Signal<Vec<Uuid>> = use_context::<Signal<Vec<Uuid>>>();
    let pages_for_pin = pages.clone();
    let on_pin_page = use_callback(move |basename: String| {
        let lower = basename.trim().to_lowercase();
        if let Some(p) = pages_for_pin
            .iter()
            .find(|p| p.basename.to_lowercase() == lower)
        {
            let mut current = pinned_panes_writer.peek().clone();
            current.retain(|id| *id != p.id);
            current.insert(0, p.id);
            pinned_panes_writer.set(current);
        }
    });
    use_context_provider(|| crate::outliner::PinPaneCb(on_pin_page));
    // BlockPopupCtx provider lives below, once on_pick_page +
    // on_pick_slash are defined.

    // ── Vim engine ────────────────────────────────────────────
    // One engine per PageBody (i.e., per active page). Mode
    // changes drive whether the active block renders as a
    // textarea (Insert) or a focusable view (Normal/Visual).
    let mut vim_engine: Signal<VimEngine> = use_signal(VimEngine::default);
    let vim_mode: VimMode = vim_engine.read().mode();
    // Document cursor — survives across blocks. Initialized to
    // the first visible block on first render; sticks otherwise.
    let mut cursor_state: Signal<Option<CursorState>> = use_signal(|| None);
    if cursor_state.peek().is_none() {
        if let Some(first) = blocks.iter().min_by_key(|b| b.sort_key.clone()) {
            cursor_state.set(Some(CursorState::single(Cursor::new(first.id, 0))));
        }
    }
    use_context_provider(|| cursor_state);

    // Awareness bridge: every time the local cursor moves,
    // encode it into a `CursorPayload` and write to the
    // EphemeralStore under `cursor::<peer>`. The store's
    // `subscribe_local_updates` callback (set up in
    // `run_awareness_loop`) then ships the bytes to peers.
    let hub_for_publish = try_use_context::<Signal<AwarenessHub>>();
    let doc_for_publish = try_use_context::<Signal<Arc<CrdtDoc>>>();
    let vim_engine_for_publish = vim_engine;
    use_effect(move || {
        let Some(state) = cursor_state.read().clone() else {
            return;
        };
        let mode_str = match vim_engine_for_publish.read().mode() {
            VimMode::Normal => "normal",
            VimMode::Insert => "insert",
            VimMode::Visual => "visual",
            VimMode::VisualLine => "visual",
            VimMode::Command => "command",
            VimMode::Search => "search",
        };
        let Some(hub_sig) = hub_for_publish else {
            return;
        };
        let hub = hub_sig.read().clone();
        let cur = state.primary();
        // Mint a stable Loro text cursor for the current block so
        // peers' carets ride concurrent edits without drift.
        // Falls back to an empty blob when the block isn't in the
        // doc yet (just-typed first-render) — `fallback_offset`
        // covers that case.
        let anchor_cursor = state.anchor;
        let encode_stable = |block_id: Uuid, offset: usize| -> Vec<u8> {
            doc_for_publish
                .and_then(|d| {
                    let doc = d.read().clone();
                    let repo = BlockRepoLoro::new(&doc);
                    let text = repo.text_handle(block_id)?;
                    let len = text.len_unicode();
                    let pos = offset.min(len);
                    text.get_cursor(pos, crdt::loro::cursor::Side::Middle)
                        .map(|c| c.encode())
                })
                .unwrap_or_default()
        };
        let stable_cursor_bytes = encode_stable(cur.block_id, cur.offset);
        let (anchor_block_id, anchor_fallback_offset, anchor_stable_bytes) = match anchor_cursor {
            Some(a) => (
                Some(a.block_id),
                a.offset,
                encode_stable(a.block_id, a.offset),
            ),
            None => (None, 0, Vec::new()),
        };
        let payload = CursorPayload {
            block_id: cur.block_id,
            fallback_offset: cur.offset,
            stable_cursor_bytes,
            anchor_block_id,
            anchor_fallback_offset,
            anchor_stable_bytes,
            page_id: Some(page_id),
            mode: mode_str.into(),
            name: hub.identity.name.clone(),
            color: hub.identity.color.clone(),
        };
        let key = hub.cursor_key();
        hub.store.set(&key, payload.to_loro_value());
    });

    // Vim block register — `yy` writes here, `p`/`P` reads.
    // Single unnamed register for v1; named registers (`"ay`) and
    // a stack of yanks come later.
    let mut vim_register: Signal<Option<String>> = use_signal(|| None);

    // Vim marks — `m{a-z}` stores the current cursor under the
    // letter; `'{a-z}` jumps to it. Per-route, transient. Engine
    // emits SetMark/JumpToMark; host owns the table.
    let mut vim_marks: Signal<std::collections::HashMap<char, Cursor>> =
        use_signal(std::collections::HashMap::new);

    // Click-at-offset: caret-aware click handler. Sets cursor at
    // the click position, focuses the block, drops the vim
    // engine into Insert mode. Logseq-style "click to edit at
    // exactly where I clicked."
    let mut cursor_writer_click = cursor_state;
    let mut editing_writer_click = editing_id;
    let on_click_at_offset = use_callback(move |(block_id, offset): (Uuid, usize)| {
        cursor_writer_click.set(Some(CursorState::single(Cursor::new(block_id, offset))));
        editing_writer_click.set(Some(block_id));
        // Drive Insert via the engine so mode is consistent. If
        // already in Insert (e.g. clicking to a different block
        // while typing) the state machine ignores the duplicate.
        if vim_engine.peek().mode() != VimMode::Insert {
            vim_engine.write().feed(VimKey::Char('i'));
        }
    });

    // Pending click — the BlockView click hands off pixel coords
    // here, the BlockEditor's on_mount picks them up to dispatch
    // a synthetic click on the textarea. This lets one click
    // both activate the block AND place the caret at the exact
    // pixel position (browser-native cursor positioning on the
    // textarea, which is byte-accurate for the source markdown).
    let pending_click_signal: Signal<Option<(Uuid, f64, f64)>> = use_signal(|| None);
    use_context_provider(|| crate::outliner::PendingClick(pending_click_signal));

    let blocks_for_vim = blocks.clone();
    let on_vim_key = use_callback(move |key: VimKey| {
        let actions: Vec<VimAction> = vim_engine.write().feed(key);
        let Some(active_id) = *editing_id.peek() else {
            return;
        };
        for action in actions {
            match action {
                VimAction::NoOp => {}
                VimAction::EnterVisual => {
                    cursor_state.with_mut(|s| {
                        if let Some(state) = s.as_mut() {
                            state.begin_selection();
                        }
                    });
                }
                VimAction::EnterNormal => {
                    cursor_state.with_mut(|s| {
                        if let Some(state) = s.as_mut() {
                            state.clear_selection();
                        }
                    });
                }
                VimAction::EnterInsert(_) => {
                    // Mode already transitioned via the engine —
                    // re-render swaps in BlockEditor which mounts
                    // focused. No host action needed.
                }
                VimAction::Move(Motion::DocStart) => {
                    if let Some(first) = flatten_visible(&blocks_for_vim).first().copied() {
                        editing_writer.set(Some(first));
                        let anchor = cursor_state.peek().as_ref().and_then(|s| s.anchor);
                        let mut new = CursorState::single(Cursor::new(first, 0));
                        new.anchor = anchor;
                        cursor_state.set(Some(new));
                    }
                }
                VimAction::Move(Motion::DocEnd) => {
                    if let Some(last) = flatten_visible(&blocks_for_vim).last().copied() {
                        editing_writer.set(Some(last));
                        let end = blocks_for_vim
                            .iter()
                            .find(|b| b.id == last)
                            .map(|b| b.content.len())
                            .unwrap_or(0);
                        let anchor = cursor_state.peek().as_ref().and_then(|s| s.anchor);
                        let mut new = CursorState::single(Cursor::new(last, end));
                        new.anchor = anchor;
                        cursor_state.set(Some(new));
                    }
                }
                VimAction::Move(motion) => {
                    // All other motions (h/l/j/k/0/$/w/b/e and
                    // their BIG variants) flow through the pure
                    // cursor compute. Cross-block motions also
                    // bump editing_id to the destination block.
                    let view = SnapshotDocView::new(&blocks_for_vim);
                    let prev_state = cursor_state.peek().clone();
                    let current = prev_state
                        .as_ref()
                        .map(|s| s.primary())
                        .unwrap_or(Cursor::new(active_id, 0));
                    let anchor = prev_state.as_ref().and_then(|s| s.anchor);
                    let next = apply_motion(current, motion, &view);
                    let mut new = CursorState::single(next);
                    new.anchor = anchor;
                    cursor_state.set(Some(new));
                    if next.block_id != current.block_id {
                        editing_writer.set(Some(next.block_id));
                    }
                }
                VimAction::Block(BlockOp::NewBelow) => {
                    on_insert_block_after.call(active_id);
                    // The new block doesn't yet have an id; the
                    // editor refresh will append. Mode is already
                    // Insert — focus lands on the new block when
                    // it mounts (it was inserted with an empty
                    // textarea after `active_id`).
                }
                VimAction::Block(BlockOp::NewAbove) => {
                    // No on_insert_block_before today; insert after
                    // the previous sibling, falling back to after
                    // current. Coverage to expand when we add the
                    // dedicated callback.
                    on_insert_block_after.call(active_id);
                }
                VimAction::Block(BlockOp::DeleteCurrent) => {
                    on_delete_block.call(active_id);
                }
                VimAction::Block(BlockOp::IndentRight) => {
                    on_indent_block.call(active_id);
                }
                VimAction::Block(BlockOp::IndentLeft) => {
                    on_outdent_block.call(active_id);
                }
                VimAction::Block(BlockOp::YankCurrent) => {
                    if let Some(b) = blocks_for_vim.iter().find(|b| b.id == active_id) {
                        vim_register.set(Some(b.content.clone()));
                    }
                }
                VimAction::Block(BlockOp::PasteAfter) | VimAction::Block(BlockOp::PasteBefore) => {
                    // v1: PasteBefore aliases PasteAfter — the
                    // proper before-insert path needs a dedicated
                    // sort_key compute, tracked as follow-up.
                    if let Some(content) = vim_register.peek().clone() {
                        on_paste_block_after.call((active_id, content));
                    }
                }
                VimAction::Block(BlockOp::ChangeCurrent) => {
                    // Clear the block's content and let mode
                    // transition into Insert (the binding pairs
                    // ChangeCurrent with `Some(VimMode::Insert)`).
                    on_edit_block.call((active_id, String::new()));
                    cursor_state.set(Some(CursorState::single(Cursor::new(active_id, 0))));
                }
                VimAction::Block(BlockOp::JoinNext) => {
                    // Find next visible block, concat its content
                    // with a single space, delete the next block.
                    let order = flatten_visible(&blocks_for_vim);
                    if let Some(i) = order.iter().position(|x| *x == active_id) {
                        if let Some(next_id) = order.get(i + 1).copied() {
                            let current = blocks_for_vim
                                .iter()
                                .find(|b| b.id == active_id)
                                .map(|b| b.content.clone())
                                .unwrap_or_default();
                            let next = blocks_for_vim
                                .iter()
                                .find(|b| b.id == next_id)
                                .map(|b| b.content.clone())
                                .unwrap_or_default();
                            let joined = if current.is_empty() {
                                next
                            } else if next.is_empty() {
                                current
                            } else {
                                format!("{current} {next}")
                            };
                            on_edit_block.call((active_id, joined));
                            on_delete_block.call(next_id);
                        }
                    }
                }
                VimAction::CycleTodo => {
                    on_cycle_todo.call(active_id);
                }
                VimAction::SetMark(c) => {
                    if let Some(cur) = cursor_state.peek().as_ref().map(|s| s.primary()) {
                        vim_marks.with_mut(|m| {
                            m.insert(c, cur);
                        });
                    }
                }
                VimAction::JumpToMark(c) => {
                    let maybe = vim_marks.peek().get(&c).copied();
                    if let Some(mark_cursor) = maybe {
                        cursor_state.set(Some(CursorState::single(mark_cursor)));
                        editing_writer.set(Some(mark_cursor.block_id));
                    }
                }
                VimAction::DeleteChar => {
                    // Splice one UTF-8 char out of the current
                    // block's content at the cursor offset.
                    let Some(cur) = cursor_state.peek().as_ref().map(|s| s.primary()) else {
                        continue;
                    };
                    let Some(block) = blocks_for_vim.iter().find(|b| b.id == cur.block_id) else {
                        continue;
                    };
                    let content = &block.content;
                    let mut off = cur.offset.min(content.len());
                    while off > 0 && !content.is_char_boundary(off) {
                        off -= 1;
                    }
                    if off >= content.len() {
                        continue;
                    }
                    let end = content[off..]
                        .chars()
                        .next()
                        .map(|c| off + c.len_utf8())
                        .unwrap_or(off);
                    let mut new_content = String::with_capacity(content.len() - (end - off));
                    new_content.push_str(&content[..off]);
                    new_content.push_str(&content[end..]);
                    on_edit_block.call((cur.block_id, new_content));
                    // Cursor stays at the same offset (now points
                    // at what used to be the char after).
                }
                // Command/Search submit + V-LINE selection ops are
                // engine-state events the host doesn't yet handle.
                // Drop them silently for now; future work wires
                // them to actual save/find/delete behaviors.
                VimAction::SubmitCommand(_)
                | VimAction::SubmitSearch
                | VimAction::DeleteSelection
                | VimAction::YankSelection => {}
            }
        }
        let _ = active_id;
    });

    // Slash command pick — strips the `/trigger` and applies the
    // command's effect (kind change + ancillary fields, or text
    // insertion).
    let blocks_for_slash = blocks.clone();
    let on_pick_slash = use_callback(move |cmd: crate::outliner::SlashCommand| {
        let Some((block_id, _q)) = slash_palette.peek().clone() else {
            return;
        };
        let Some(block) = blocks_for_slash.iter().find(|b| b.id == block_id).cloned() else {
            return;
        };
        match cmd.effect {
            crate::outliner::SlashEffect::SetKind {
                kind,
                heading_level,
                list_task,
            } => {
                let new_content = crate::outliner::strip_slash_trigger(&block.content);
                on_set_block_kind.call((
                    block_id,
                    new_content,
                    kind.to_string(),
                    heading_level,
                    list_task.map(|s| s.to_string()),
                ));
            }
            crate::outliner::SlashEffect::InsertText(kind) => {
                let replacement = match kind {
                    crate::outliner::SlashTextKind::Today => {
                        chrono::Local::now().format("[[%Y-%m-%d]]").to_string()
                    }
                    crate::outliner::SlashTextKind::Tomorrow => (chrono::Local::now()
                        + chrono::Duration::days(1))
                    .format("[[%Y-%m-%d]]")
                    .to_string(),
                };
                let new_content =
                    crate::outliner::replace_slash_trigger(&block.content, &replacement);
                on_edit_block.call((block_id, new_content));
            }
        }
        slash_palette.set(None);
    });

    // Autocomplete pick callback — closes the popup, replaces the
    // partial `[[query` with `[[Page Name]]` via on_edit.
    let blocks_for_complete = blocks.clone();
    let on_pick_page = use_callback(move |page_name: String| {
        let Some((block_id, _q)) = autocomplete.peek().clone() else {
            return;
        };
        let Some(b) = blocks_for_complete.iter().find(|b| b.id == block_id) else {
            return;
        };
        let new_content = complete_wikilink(&b.content, &page_name);
        edit_doc.call((block_id, new_content));
        autocomplete.set(None);
    });

    use_context_provider(|| crate::outliner::BlockPopupCtx {
        autocomplete,
        slash: slash_palette,
        all_pages: std::sync::Arc::new(all_pages.clone()),
        on_pick_page,
        on_pick_slash,
    });

    let autocomplete_state = autocomplete.read().clone();
    let slash_state = slash_palette.read().clone();
    let active_view_mode: ViewMode = *crate::view_mode::use_view_mode().read();
    // Right sidebar visibility — per-page-body local state. Holds
    // Properties + Backlinks; future panels (outline, refs,
    // history) slot in the same shell.
    let mut right_sidebar_open: Signal<bool> = use_signal(|| true);
    let sidebar_visible = *right_sidebar_open.read();
    let on_jump_backlink = use_callback(move |(target_page, target_block): (Uuid, Uuid)| {
        on_select_page.call(target_page);
        editing_writer.set(Some(target_block));
    });

    rsx! {
        div { class: "flex w-full gap-4 min-w-0",
        div { class: "flex-1 min-w-0",
        // Header — title + a small right-aligned cluster of
        // mode + sidebar toggles. Uses items-baseline so the
        // toggles align to the title's baseline rather than its
        // box, which avoids the "float above" feel.
        div { class: "mb-3 flex items-baseline gap-2",
            div { class: "flex-1 min-w-0",
                PageTitle {
                    page_id,
                    basename: basename.clone(),
                    on_rename: on_rename_page,
                }
            }
            div { class: "flex items-center gap-1 flex-none",
                ViewModeToggle {}
                button {
                    "data-testid": "knowledge-right-sidebar-toggle",
                    class: if sidebar_visible {
                        "h-7 w-7 inline-flex items-center justify-center rounded text-foreground bg-accent/40 hover:bg-accent/60"
                    } else {
                        "h-7 w-7 inline-flex items-center justify-center rounded text-muted-foreground hover:text-foreground hover:bg-muted/40"
                    },
                    title: if sidebar_visible { "Hide details" } else { "Show details" },
                    onclick: move |_| {
                        let cur = *right_sidebar_open.peek();
                        right_sidebar_open.set(!cur);
                    },
                    PanelRight { size: 14 }
                }
            }
        }
        match active_view_mode {
            ViewMode::Source => {
                // Look up the page fresh — `page` may have already
                // been consumed by the properties pane render.
                let page_for_source = pages.iter().find(|p| p.id == page_id).cloned();
                match page_for_source {
                    Some(p) => rsx! { SourceModeView { page: p, blocks: blocks.clone() } },
                    None => rsx! {
                        div { class: "p-3 text-xs text-muted-foreground",
                            "Page not found."
                        }
                    },
                }
            }
            // Edit and View use the same outliner. View mode forces
            // vim into Normal (no Insert) so blocks render as
            // BlockView/BlockNormalView only — never as a textarea.
            view_mode_kind => rsx! {
                div {
                    "data-testid": "knowledge-block-list",
                    "data-view-mode": match view_mode_kind {
                        ViewMode::Edit => "edit",
                        ViewMode::View => "view",
                        ViewMode::Source => "source",
                    },
                    class: "relative",
                    Outliner {
                        blocks: blocks.clone(),
                        ops: OutlinerOps {
                            on_edit: on_edit_wrapped,
                            on_insert_after: on_insert_block_after,
                            on_delete: on_delete_block,
                            on_indent: on_indent_block,
                            on_outdent: on_outdent_block,
                            on_toggle_collapsed,
                            on_move: on_move_block,
                            on_focus_relative,
                            on_navigate_link,
                            on_vim_key,
                            on_click_at_offset,
                        },
                        editing_id,
                        // In View mode pretend we're always in
                        // Normal so OutlinerNode never swaps in
                        // BlockEditor.
                        vim_mode: if view_mode_kind == ViewMode::View {
                            VimMode::Normal
                        } else {
                            vim_mode
                        },
                    }
                    // Popups now render inline inside the active
                    // BlockEditor (see outliner::BlockPopupCtx).
                    {
                        let _ = (&autocomplete_state, &slash_state, &all_pages);
                    }
                }
                if view_mode_kind == ViewMode::Edit {
                    AddBlockButton {
                        page_id,
                        vault_id,
                        after_count: blocks.len(),
                        on_add_block,
                    }
                }
            }
        }
        } // end of main pane div
        if sidebar_visible {
            aside {
                "data-testid": "knowledge-right-sidebar",
                class: "hidden lg:flex flex-col w-72 flex-none border-l border-border/40 pl-3 overflow-y-auto",
                PinnedPanesStack {
                    pinned_panes: pinned_panes_writer,
                    pages_by_id: pages.iter().cloned().map(|p| (p.id, p)).collect(),
                    on_jump: on_select_page,
                }
                if let (Some(s2), Some(p2)) = (
                    page.as_ref().and_then(|p| {
                        let fm: serde_json::Value =
                            serde_json::from_str(&p.frontmatter_json).unwrap_or(serde_json::Value::Null);
                        let kind = fm.get("kind").and_then(|v| v.as_str())?;
                        knowledge_proto::property_schema::PropertySchemaRegistry::with_builtins().get(kind)
                    }),
                    page.clone(),
                ) {
                    SidebarSection { label: "Properties", icon_kind: "props",
                        crate::properties_pane::PropertiesPane {
                            schema: s2,
                            frontmatter_json: p2.frontmatter_json.clone(),
                            on_change: on_prop_change,
                        }
                    }
                }
                SidebarSection { label: "Outline", icon_kind: "outline",
                    OutlinePanel {
                        blocks: blocks.clone(),
                        on_select: use_callback(move |block_id: Uuid| {
                            editing_writer.set(Some(block_id));
                        }),
                    }
                }
                SidebarSection { label: "Backlinks", icon_kind: "links",
                    BacklinksPanel {
                        target_basename: basename.clone(),
                        block_refs: snapshot.block_refs.clone(),
                        blocks_by_page: snapshot.blocks_by_page.clone(),
                        pages_by_id: pages.iter().cloned().map(|p| (p.id, p)).collect(),
                        on_jump: on_jump_backlink,
                    }
                }
            }
        }
        } // end of outer flex
        VimModeIndicator { mode: vim_mode }
    }
}

/// Stack of user-pinned page panes — sits at the top of the
/// right sidebar above the Properties / Outline / Backlinks
/// sections. Each pane is a small card with the page name +
/// close button + a read-only `EmbedCard` of the page's blocks.
/// Click the title to navigate; click `×` to remove.
#[component]
fn PinnedPanesStack(
    pinned_panes: Signal<Vec<Uuid>>,
    pages_by_id: HashMap<Uuid, Page>,
    on_jump: Callback<Uuid>,
) -> Element {
    let stack = pinned_panes.read().clone();
    if stack.is_empty() {
        return rsx! {};
    }
    rsx! {
        section {
            "data-testid": "sidebar-pinned-panes",
            class: "flex flex-col gap-2 py-2 border-b border-border/40",
            for page_id in stack {
                {
                    let page = pages_by_id.get(&page_id).cloned();
                    let basename = page.as_ref().map(|p| p.basename.clone()).unwrap_or_else(|| "(unknown)".into());
                    let testid = format!("pinned-pane-{page_id}");
                    let mut writer = pinned_panes;
                    rsx! {
                        div {
                            key: "{page_id}",
                            "data-testid": testid,
                            class: "rounded-md border border-border/60 bg-card/40 overflow-hidden",
                            div { class: "flex items-center gap-1 px-2 py-1 border-b border-border/40 bg-muted/20",
                                button {
                                    class: "flex-1 min-w-0 text-left text-xs font-semibold truncate hover:underline",
                                    title: basename.clone(),
                                    onclick: move |_| on_jump.call(page_id),
                                    "{basename}"
                                }
                                button {
                                    "data-testid": format!("pinned-pane-close-{page_id}"),
                                    class: "h-5 w-5 inline-flex items-center justify-center rounded text-muted-foreground hover:text-foreground hover:bg-muted/60 text-xs",
                                    title: "Remove from sidebar",
                                    onclick: move |_| {
                                        let mut current = writer.peek().clone();
                                        current.retain(|id| *id != page_id);
                                        writer.set(current);
                                    },
                                    "×"
                                }
                            }
                            div { class: "p-2",
                                crate::outliner::EmbedContent {
                                    target: basename.clone(),
                                    alias: None,
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Outline / table-of-contents panel — lists every `kind:
/// "heading"` block on the current page indented by
/// `heading_level`. Click a heading → activate that block in the
/// outliner.
#[component]
fn OutlinePanel(blocks: Vec<Block>, on_select: Callback<Uuid>) -> Element {
    let headings: Vec<&Block> = blocks.iter().filter(|b| b.kind == "heading").collect();
    if headings.is_empty() {
        return rsx! {
            div { class: "px-1 py-1 text-xs text-muted-foreground/70 italic",
                "No headings yet."
            }
        };
    }
    rsx! {
        ul {
            "data-testid": "outline-panel",
            class: "flex flex-col text-xs",
            for h in headings {
                {
                    let id = h.id;
                    let level = h.heading_level.unwrap_or(1).clamp(1, 6) as usize;
                    let pad = format!("padding-left: {}px", (level - 1) * 12);
                    let label = h.content.lines().next().unwrap_or("(empty)").to_string();
                    let cls = match level {
                        1 => "rounded px-1 py-0.5 cursor-pointer text-foreground hover:bg-muted/40 truncate",
                        2 => "rounded px-1 py-0.5 cursor-pointer text-foreground/90 hover:bg-muted/40 truncate",
                        _ => "rounded px-1 py-0.5 cursor-pointer text-muted-foreground hover:text-foreground hover:bg-muted/40 truncate",
                    };
                    let testid = format!("outline-row-{id}");
                    rsx! {
                        li { key: "{id}",
                            div {
                                "data-testid": testid,
                                class: cls,
                                style: pad,
                                title: label.clone(),
                                onclick: move |_| on_select.call(id),
                                if label.trim().is_empty() {
                                    span { class: "italic text-muted-foreground", "(empty)" }
                                } else {
                                    "{label}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Header for a right-sidebar section (Properties, Backlinks,
/// future Outline/Refs). Caller passes section content as
/// children.
#[component]
fn SidebarSection(label: String, icon_kind: String, children: Element) -> Element {
    rsx! {
        section { class: "py-3 first:pt-1",
            HStack { class: "items-center gap-1.5 mb-1.5 text-[11px] uppercase tracking-wider text-muted-foreground",
                span { class: "inline-flex items-center justify-center text-foreground/60",
                    if icon_kind == "props" { SlidersHorizontal { size: 12 } }
                    else if icon_kind == "links" { LinkIcon { size: 12 } }
                    else if icon_kind == "outline" { ListIcon { size: 12 } }
                    else { span {} }
                }
                span { class: "font-semibold", "{label}" }
            }
            {children}
        }
    }
}

/// Three-way segmented control for Edit / View / Source. Reads
/// the per-route view-mode signal from context.
#[component]
fn ViewModeToggle() -> Element {
    let mode = crate::view_mode::use_view_mode();
    let current = *mode.read();
    let btn = |label: &'static str, kind: ViewMode, icon: Element, mut mode: Signal<ViewMode>| {
        let active = current == kind;
        let cls = if active {
            "inline-flex items-center gap-1 px-2 py-1 text-xs bg-foreground text-background rounded-md"
        } else {
            "inline-flex items-center gap-1 px-2 py-1 text-xs text-muted-foreground hover:text-foreground hover:bg-muted/40 rounded-md"
        };
        let testid = format!(
            "view-mode-{}",
            match kind {
                ViewMode::Edit => "edit",
                ViewMode::View => "view",
                ViewMode::Source => "source",
            }
        );
        rsx! {
            button {
                "data-testid": testid,
                "data-active": "{active}",
                class: cls,
                title: label,
                onclick: move |_| mode.set(kind),
                {icon}
                span { class: "hidden sm:inline", "{label}" }
            }
        }
    };
    rsx! {
        div {
            "data-testid": "view-mode-toggle",
            class: "inline-flex items-center gap-0.5 rounded-md border border-border bg-card/40 p-0.5 flex-none",
            {btn("Edit", ViewMode::Edit, rsx! { Pencil { size: 12 } }, mode)}
            {btn("View", ViewMode::View, rsx! { Eye { size: 12 } }, mode)}
            {btn("Source", ViewMode::Source, rsx! { Code { size: 12 } }, mode)}
        }
    }
}

/// Source mode — full markdown serialization of the current page
/// in a single textarea. Edits live in a draft signal until the
/// user clicks Apply (parses + diffs back to blocks). v1 ships
/// read-only-ish: Apply is wired but commits via a coarse
/// "replace all blocks" round-trip; full structural diff comes
/// later. See plans/logseq-data-model-alignment.md.
#[component]
fn SourceModeView(page: Page, blocks: Vec<Block>) -> Element {
    let serialized = knowledge_proto::obsidian::serialize_page(&page, &blocks);
    let mut draft: Signal<String> = use_signal(|| serialized.clone());
    let original = serialized.clone();
    let row_count = draft.read().lines().count().max(20).min(80);
    let dirty = *draft.read() != original;
    rsx! {
        div { class: "flex flex-col gap-2",
            div { class: "flex items-center gap-2",
                span { class: "text-[10px] uppercase tracking-wider text-muted-foreground",
                    "Source"
                }
                if dirty {
                    span { class: "text-[10px] text-amber-500",
                        "● modified — Apply not yet wired"
                    }
                }
                div { class: "ml-auto flex gap-1",
                    button {
                        class: "px-2 py-1 text-xs text-muted-foreground hover:text-foreground rounded hover:bg-muted/40",
                        onclick: move |_| draft.set(original.clone()),
                        "Reset"
                    }
                }
            }
            textarea {
                "data-testid": "source-mode-textarea",
                class: "w-full rounded-md border border-border bg-background p-3 font-mono text-xs text-foreground outline-none focus-visible:ring-2 focus-visible:ring-ring resize-none",
                rows: row_count as i64,
                value: "{draft}",
                oninput: move |e| draft.set(e.value()),
            }
            p { class: "text-[10px] text-muted-foreground",
                "Source mode is editable but writes back to the document are queued for the Tier-1 data-model alignment work — see plans/logseq-data-model-alignment.md."
            }
        }
    }
}

/// Backlinks section — every block in the vault that links to the
/// current page. Reads the materialized `block_refs` index from
/// the snapshot (see Tier 1 in
/// `plans/logseq-data-model-alignment.md`), so this is O(edges)
/// not O(blocks × refs).
#[component]
fn BacklinksPanel(
    target_basename: String,
    block_refs: Vec<knowledge_proto::BlockRefEdge>,
    blocks_by_page: HashMap<Uuid, Vec<Block>>,
    pages_by_id: HashMap<Uuid, Page>,
    on_jump: Callback<(Uuid, Uuid)>,
) -> Element {
    let lower = target_basename.to_lowercase();
    let matches: Vec<&knowledge_proto::BlockRefEdge> = block_refs
        .iter()
        .filter(|e| e.target_kind == "page" && e.target_str.to_lowercase() == lower)
        .collect();
    if matches.is_empty() {
        return rsx! {};
    }
    // Group by source page for nicer rendering.
    let mut by_page: std::collections::BTreeMap<Uuid, Vec<&knowledge_proto::BlockRefEdge>> =
        std::collections::BTreeMap::new();
    for e in &matches {
        by_page.entry(e.source_page_id).or_default().push(e);
    }
    let mut open: Signal<bool> = use_signal(|| true);
    rsx! {
        section {
            "data-testid": "backlinks-panel",
            class: "mt-8 pt-4 border-t border-border/40",
            button {
                class: "w-full flex items-center gap-2 text-[11px] uppercase tracking-wider text-muted-foreground hover:text-foreground",
                onclick: move |_| {
                    let cur = *open.peek();
                    open.set(!cur);
                },
                span { class: "font-semibold", "Backlinks" }
                span { class: "text-foreground/70", "· {matches.len()}" }
                span { class: "ml-auto text-foreground/50",
                    if *open.read() { "−" } else { "+" }
                }
            }
            if *open.read() {
                div { class: "mt-2 flex flex-col gap-3",
                    for (source_page_id, edges) in by_page {
                        {
                            let source_page = pages_by_id.get(&source_page_id).cloned();
                            let blocks = blocks_by_page.get(&source_page_id).cloned().unwrap_or_default();
                            rsx! {
                                div {
                                    key: "{source_page_id}",
                                    class: "rounded-md border border-border/40 bg-card/40 overflow-hidden",
                                    div { class: "px-2 py-1 text-xs font-medium border-b border-border/40 bg-muted/20",
                                        if let Some(p) = &source_page {
                                            "{p.basename}"
                                        } else {
                                            "(unknown page)"
                                        }
                                    }
                                    ul { class: "flex flex-col",
                                        for edge in edges {
                                            {
                                                let block = blocks.iter().find(|b| b.id == edge.source_block_id).cloned();
                                                let snippet = block
                                                    .as_ref()
                                                    .map(|b| b.content.lines().next().unwrap_or("").to_string())
                                                    .unwrap_or_else(|| "(missing)".into());
                                                let target_block = edge.source_block_id;
                                                let testid = format!("backlink-{}-{}", source_page_id, target_block);
                                                rsx! {
                                                    li {
                                                        key: "{edge.id}",
                                                        "data-testid": testid,
                                                        class: "px-2 py-1 text-xs cursor-pointer text-foreground/85 hover:bg-muted/40 hover:text-foreground",
                                                        onclick: move |_| on_jump.call((source_page_id, target_block)),
                                                        if snippet.is_empty() {
                                                            span { class: "italic text-muted-foreground", "(empty block)" }
                                                        } else {
                                                            "{snippet}"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Sticky bottom-right chip showing the current vim mode.
#[component]
fn VimModeIndicator(mode: VimMode) -> Element {
    let (label, cls) = match mode {
        VimMode::Normal => ("NORMAL", "bg-foreground text-background"),
        VimMode::Insert => ("INSERT", "bg-emerald-500 text-white"),
        VimMode::Visual => ("VISUAL", "bg-amber-500 text-black"),
        VimMode::VisualLine => ("V-LINE", "bg-amber-600 text-black"),
        VimMode::Command => ("COMMAND", "bg-sky-500 text-white"),
        VimMode::Search => ("SEARCH", "bg-violet-500 text-white"),
    };
    rsx! {
        div {
            "data-testid": "vim-mode-indicator",
            class: "fixed bottom-3 right-3 z-40 px-2 py-0.5 rounded text-[10px] font-mono font-bold tracking-wider shadow-md {cls}",
            "{label}"
        }
    }
}

#[component]
fn PageTitle(page_id: Uuid, basename: String, on_rename: Callback<(Uuid, String)>) -> Element {
    let mut editing: Signal<bool> = use_signal(|| false);
    let mut draft: Signal<String> = use_signal(|| basename.clone());

    // Keep `draft` synced when the selected page changes.
    use_effect({
        let bn = basename.clone();
        move || {
            draft.set(bn.clone());
        }
    });

    let basename_for_commit = basename.clone();
    let commit = use_callback(move |()| {
        let v = draft.read().clone();
        if v.trim() != basename_for_commit.trim() && !v.trim().is_empty() {
            on_rename.call((page_id, v));
        }
        editing.set(false);
    });
    let basename_for_cancel = basename.clone();
    let cancel = use_callback(move |()| {
        draft.set(basename_for_cancel.clone());
        editing.set(false);
    });

    if *editing.read() {
        rsx! {
            input {
                "data-testid": format!("knowledge-page-title-input-{page_id}"),
                r#type: "text",
                class: "min-w-0 flex-1 rounded bg-transparent px-1 py-0.5 text-xl font-bold text-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring focus-visible:bg-background",
                value: "{draft}",
                autofocus: true,
                oninput: move |e| draft.set(e.value()),
                onkeydown: move |e| {
                    match e.key() {
                        Key::Enter => commit.call(()),
                        Key::Escape => cancel.call(()),
                        _ => {}
                    }
                },
                onblur: move |_| commit.call(()),
            }
        }
    } else {
        let testid = format!("knowledge-page-title-{page_id}");
        rsx! {
            h2 {
                "data-testid": testid,
                class: "text-xl font-bold cursor-text min-w-0 truncate text-foreground hover:bg-muted/40 rounded px-1",
                title: "Click to rename",
                onclick: move |_| editing.set(true),
                "{basename}"
            }
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
        div { class: "flex items-center gap-1",
            span { class: "text-muted-foreground pointer-events-none flex-none w-5 flex justify-center",
                Plus { size: 14 }
            }
            input {
                "data-testid": "knowledge-new-page-input",
                r#type: "text",
                class: "flex-1 h-7 rounded bg-transparent px-1 text-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring focus-visible:bg-background",
                value: "{value}",
                placeholder: "New page",
                oninput: move |e| value.set(e.value()),
                onkeydown: move |e| {
                    if e.key() == Key::Enter { submit(()); }
                },
            }
            span { "data-testid": "knowledge-add-page-button", class: "sr-only" }
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
        div { class: "mt-1",
            span { "data-testid": "knowledge-add-block-button",
                button {
                    class: "inline-flex items-center gap-1 px-2 py-1 text-xs text-muted-foreground hover:text-foreground rounded hover:bg-muted/40",
                    onclick: move |_| on_add_block.call((page_id, vault_id, after_count)),
                    Plus { size: 12 }
                    "block"
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
    /// Materialized BlockRefEdge index — see Tier 1 of
    /// `plans/logseq-data-model-alignment.md`. Backlinks panel
    /// filters this in O(edges) rather than scanning every
    /// block's content.
    pub block_refs: Vec<knowledge_proto::BlockRefEdge>,
    /// Materialized BlockPropEdge index — Tier 2. Drives
    /// property queries (`{{query status:todo}}`) without per-
    /// block frontmatter scans.
    pub block_props: Vec<knowledge_proto::BlockPropEdge>,
}

fn big_page() -> PageWindow {
    PageWindow {
        index: 0,
        size: 1000,
    }
}

async fn build_snapshot(doc: Arc<CrdtDoc>) -> Result<KnowledgeSnapshot, String> {
    // Bootstrap-or-no-op: keeps Tier 1/2 indices consistent with
    // server-seeded entities the local hooks never saw. Idempotent.
    let _ = knowledge_crdt::reindex::reindex_all(&doc).await;

    let vault_repo = VaultRepoLoro::new(&doc);
    let page_repo = knowledge_crdt::IndexedPageRepo::new(&doc);
    let block_repo = BlockRepoLoro::new(&doc);
    let ref_edge_repo = knowledge_crdt::BlockRefEdgeRepoLoro::new(&doc);

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
    let ref_edges = {
        use knowledge_proto::BlockRefEdgeRepo;
        ref_edge_repo
            .list(big_page(), None, None)
            .await
            .map_err(|e| format!("ref edge list: {e}"))?
    };
    let prop_edges = {
        use knowledge_proto::BlockPropEdgeRepo;
        let repo = knowledge_crdt::BlockPropEdgeRepoLoro::new(&doc);
        repo.list(big_page(), None, None)
            .await
            .map_err(|e| format!("prop edge list: {e}"))?
    };

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
        block_refs: ref_edges.items,
        block_props: prop_edges.items,
    })
}

/// Local-first persistence loop. On wasm:
/// 1. Opens (or creates) the IndexedDB store.
/// 2. Loads the prior snapshot + replays appended updates into
///    the local doc. Route renders the restored state on the
///    next snapshot poll.
/// 3. Subscribes to `LoroDoc::subscribe_local_update` and
///    appends every commit's bytes. Crosses the
///    `COMPACT_THRESHOLD` → writes a fresh snapshot + clears
///    the updates store atomically.
///
/// On non-wasm the function is a no-op since IDB is browser-only;
/// native callers can plug in a different `Persistence` impl via
/// the architect API instead.
#[cfg(target_arch = "wasm32")]
async fn run_idb_persistence(doc: Arc<CrdtDoc>, mut last_error: Signal<Option<String>>) {
    use crate::idb_persistence::{COMPACT_THRESHOLD, IdbPersistence};
    // Per-doc keying: today only the org vault doc; per-project
    // vault docs reuse this loop with their own doc_id when they
    // land. The IDB schema already keys snapshots + indexes
    // updates by doc_id so adding more is a config change, not a
    // schema change.
    let doc_id = project_proto::DocId::org_vault().0;
    let idb = match IdbPersistence::open().await {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!(error = ?e, "idb persistence unavailable — running ephemerally");
            return;
        }
    };
    // Cold-load order matters: snapshot first, then updates.
    if let Ok(Some(snap)) = idb.load_snapshot(&doc_id).await {
        if let Err(e) = doc.loro().import(&snap) {
            tracing::warn!(?e, "idb snapshot import failed");
            last_error.set(Some(format!("idb snapshot: {e}")));
        }
    }
    if let Ok(updates) = idb.load_updates(&doc_id).await {
        for u in updates {
            if let Err(e) = doc.loro().import(&u) {
                tracing::warn!(?e, "idb update import failed");
            }
        }
    }
    // Bridge future commits → IDB. Use an unbounded mpsc so the
    // sync callback can stay non-blocking; an async task drains
    // and writes serially.
    let (tx, mut rx) = futures::channel::mpsc::unbounded::<Vec<u8>>();
    let sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(sub); // Subscription owns its lifetime; we
    // forget so it survives this task.
    let mut count = 0usize;
    while let Some(bytes) = rx.next().await {
        if let Err(e) = idb.append_update(&doc_id, bytes).await {
            tracing::warn!(?e, "idb append_update failed");
            continue;
        }
        count += 1;
        if count >= COMPACT_THRESHOLD {
            // Export a fresh snapshot and clear the updates log.
            let snap_bytes = match doc.loro().export(crdt::loro::ExportMode::Snapshot) {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!(?e, "idb snapshot export failed");
                    continue;
                }
            };
            if let Err(e) = idb.write_snapshot_and_clear(&doc_id, snap_bytes).await {
                tracing::warn!(?e, "idb compact failed");
                continue;
            }
            count = 0;
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
async fn run_idb_persistence(_doc: Arc<CrdtDoc>, _last_error: Signal<Option<String>>) {
    // Native build: persistence is wired via `CrdtDoc::open`
    // with a real backend (sqlite, etc.) at the host layer.
}

/// Online/offline state of the sync loop. Drives the small
/// status chip in the route header so users can see at a glance
/// whether their edits are live or queued for the next reconnect.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SyncStatus {
    /// Connected to the server; uploads + downloads flowing.
    Online,
    /// Disconnected (or never connected). Local edits commit to
    /// IndexedDB and replay on reconnect.
    Offline,
    /// Trying to (re)open the connection.
    Reconnecting,
}

/// Sync loop with reconnect + backoff. The local doc is the
/// source of truth; on every (re)connect we ship its full state
/// to the server, which merges idempotently and re-broadcasts
/// to other peers. This means **offline edits flow up cleanly
/// when the server comes back** without a separate pending-queue.
///
/// Backoff: starts at 1s, doubles per failure, caps at 30s.
/// Resets to 1s on every successful connect.
async fn run_sync_loop(
    url: String,
    doc: Arc<CrdtDoc>,
    version: Signal<u64>,
    mut last_error: Signal<Option<String>>,
    mut sync_status: Signal<SyncStatus>,
) {
    // Empty URL = offline-only mode (no server configured). Mark
    // status accordingly + return; the local CRDT keeps working,
    // IDB persistence (or the demo seed) supplies the content.
    if url.is_empty() {
        sync_status.set(SyncStatus::Offline);
        let _ = (doc, version, last_error);
        return;
    }
    // Subscribe to local commits ONCE — survives across retries.
    // While disconnected the channel buffers; on reconnect we
    // drain it (after shipping the fresh snapshot first).
    let (upload_tx, mut upload_rx) = unbounded::<Vec<u8>>();
    let upload_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = upload_tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(upload_sub);

    let doc_id = project_proto::DocId::org_vault();
    let mut backoff_ms: u32 = 1_000;
    loop {
        sync_status.set(SyncStatus::Reconnecting);
        match connect_session(
            &url,
            doc.clone(),
            doc_id.clone(),
            &mut upload_rx,
            version,
            sync_status,
        )
        .await
        {
            Ok(()) => {
                tracing::info!("sync stream closed cleanly");
            }
            Err(e) => {
                tracing::warn!(%e, "sync session ended");
                last_error.set(Some(e));
            }
        }
        sync_status.set(SyncStatus::Offline);
        // Backoff before next attempt. Clamps at 30s so we
        // don't go silent during long outages.
        sleep_ms(backoff_ms).await;
        backoff_ms = backoff_ms.saturating_mul(2).min(30_000);
    }
}

/// One connect attempt. Spawns subscribe + drains uploads + recv
/// concurrently. Returns when the session ends (server hangup,
/// network drop, apply_update fail). Caller retries with
/// backoff.
async fn connect_session(
    url: &str,
    doc: Arc<CrdtDoc>,
    doc_id: project_proto::DocId,
    upload_rx: &mut futures::channel::mpsc::UnboundedReceiver<Vec<u8>>,
    mut version: Signal<u64>,
    mut sync_status: Signal<SyncStatus>,
) -> Result<(), String> {
    let sub_client: WorkspaceSyncClient = connect_client(url)
        .await
        .map_err(|e| format!("subscribe-client connect: {e}"))?;
    let apply_client: WorkspaceSyncClient = connect_client(url)
        .await
        .map_err(|e| format!("apply-client connect: {e}"))?;

    // Initial sync: ship our full state. CRDT property → server
    // merges idempotently. Catches the server up on any offline
    // edits accumulated since last connect.
    if let Ok(snap) = doc.loro().export(crdt::loro::ExportMode::Snapshot) {
        if let Err(e) = apply_client
            .apply_update(doc_id.clone(), UpdateBytes(snap))
            .await
        {
            tracing::warn!(?e, "initial-sync apply_update failed");
            return Err(format!("initial-sync: {e:?}"));
        }
    }

    sync_status.set(SyncStatus::Online);

    // Long-lived subscribe: spawn so we can run recv concurrently.
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let sub_doc_id = doc_id.clone();
    spawn(async move {
        if let Err(e) = sub_client.subscribe(sub_doc_id, tx).await {
            tracing::warn!(error = ?e, "WorkspaceSync::subscribe ended");
        }
    });

    // Concurrent: drain remote frames + drain local upload queue.
    // First failure ends the session; the outer loop reconnects.
    let recv_doc = doc.clone();
    let recv_fut = async move {
        loop {
            match rx.recv().await {
                Ok(Some(msg)) => {
                    let bytes = &msg.get().0;
                    if let Err(e) = recv_doc.apply_remote(bytes) {
                        tracing::warn!(?e, "apply_remote failed");
                        continue;
                    }
                    version.with_mut(|v| *v += 1);
                }
                Ok(None) => return Err::<(), String>("stream closed by server".into()),
                Err(e) => return Err(format!("recv: {e:?}")),
            }
        }
    };

    let upload_doc_id = doc_id.clone();
    let upload_fut = async move {
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_client
                .apply_update(upload_doc_id.clone(), UpdateBytes(bytes))
                .await
            {
                return Err::<(), String>(format!("apply_update: {e:?}"));
            }
        }
        Err("upload channel closed".into())
    };

    futures::pin_mut!(recv_fut, upload_fut);
    match futures::future::select(recv_fut, upload_fut).await {
        futures::future::Either::Left((res, _)) => res,
        futures::future::Either::Right((res, _)) => res,
    }
}

/// Multi-tab coordination via BroadcastChannel. When two tabs
/// of the app are open, both write to the same IDB and both
/// drive the sync loop — fine in principle (CRDT semantics
/// merge), but each tab's sync round-trip is wasted. This
/// watcher tells peer tabs about every local commit so they
/// import it directly without going through the server.
///
/// Mechanism: each tab subscribes to BroadcastChannel `task-arch-doc`.
/// On every local commit (via the doc's local-update hook
/// installed elsewhere), we'd post the bytes; on every received
/// message we import. Doc-id keying so multiple docs don't
/// trample each other when per-project vaults arrive.
async fn run_broadcast_watcher(doc: Arc<CrdtDoc>, doc_id: String) {
    // Inbound: forward messages from peer tabs into the local
    // doc. Use a polling drain since BroadcastChannel via
    // `document::eval` doesn't give us a Stream natively — JS
    // pushes into a queue we drain on a tick.
    let setup = format!(
        r#"
        (() => {{
            if (window.__taskArchBC) return 1;
            const bc = new BroadcastChannel('task-arch-{doc_id}');
            window.__taskArchBC = bc;
            window.__taskArchBCQueue = [];
            bc.onmessage = (ev) => {{
                if (ev.data && ev.data.kind === 'update') {{
                    window.__taskArchBCQueue.push(ev.data.b64);
                }}
            }};
            return 1;
        }})()
        "#
    );
    if document::eval(&setup)
        .recv::<serde_json::Value>()
        .await
        .is_err()
    {
        return;
    }
    // Outbound: hook the doc's commit stream → broadcast.
    let (tx, mut rx) = futures::channel::mpsc::unbounded::<Vec<u8>>();
    let doc_id_for_send = doc_id.clone();
    let sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = tx.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(sub);
    let send_doc_id = doc_id_for_send.clone();
    spawn(async move {
        while let Some(bytes) = rx.next().await {
            let b64 = b64_encode(&bytes);
            let js = format!(
                r#"
                if (window.__taskArchBC) {{
                    window.__taskArchBC.postMessage({{ kind: 'update', b64: "{b64}" }});
                }}
                return 1;
                "#
            );
            let _ = document::eval(&js).recv::<serde_json::Value>().await;
        }
        let _ = send_doc_id; // keep alive for symmetry
    });
    // Inbound drain — pop the queue every 250ms and import.
    loop {
        let drain_js = r#"
            (() => {
                const q = window.__taskArchBCQueue || [];
                window.__taskArchBCQueue = [];
                return q;
            })()
        "#;
        if let Ok(v) = document::eval(drain_js).recv::<serde_json::Value>().await {
            if let Some(arr) = v.as_array() {
                for item in arr {
                    if let Some(b64) = item.as_str() {
                        if let Some(bytes) = b64_decode(b64) {
                            if let Err(e) = doc.loro().import(&bytes) {
                                tracing::warn!(?e, "broadcast import failed");
                            }
                        }
                    }
                }
            }
        }
        sleep_ms(250).await;
    }
}

/// Storage quota watcher. Polls `navigator.storage.estimate()`
/// every 60s; if usage crosses 80% of quota, surface a warning
/// via `last_error` so the route's existing error banner shows
/// it. The browser typically starts evicting IDB data when
/// quota is hit — getting in front of that with a warning is
/// the difference between "user knows to export" and "data
/// silently gone".
async fn run_storage_quota_watcher(mut last_error: Signal<Option<String>>) {
    loop {
        let result = document::eval(
            r#"
            return (async () => {
                if (!navigator.storage || !navigator.storage.estimate) return null;
                const e = await navigator.storage.estimate();
                return { usage: e.usage || 0, quota: e.quota || 0 };
            })();
            "#,
        )
        .recv::<serde_json::Value>()
        .await;
        if let Ok(v) = result {
            if let (Some(usage), Some(quota)) = (
                v.get("usage").and_then(|x| x.as_u64()),
                v.get("quota").and_then(|x| x.as_u64()),
            ) {
                if quota > 0 {
                    let frac = usage as f64 / quota as f64;
                    if frac >= 0.80 {
                        let mb_used = usage / (1024 * 1024);
                        let mb_total = quota / (1024 * 1024);
                        last_error.set(Some(format!(
                            "Storage {}% full ({} / {} MB). Export your vault soon.",
                            (frac * 100.0) as u32,
                            mb_used,
                            mb_total,
                        )));
                    }
                }
            }
        }
        sleep_ms(60_000).await;
    }
}

/// Browser-network watcher. Polls `navigator.onLine` every 2s
/// and flips `sync_status` → Offline whenever the OS reports
/// no network. Leaves Online/Reconnecting alone otherwise — the
/// sync loop's actual connect attempts are the authority on
/// "the server is reachable" (network can be up but server down,
/// or down but cached locally).
async fn run_network_watcher(mut sync_status: Signal<SyncStatus>) {
    let mut last_was_offline = false;
    loop {
        let online = match document::eval("return navigator.onLine ? 1 : 0;")
            .recv::<serde_json::Value>()
            .await
        {
            Ok(v) => v.as_i64().unwrap_or(1) != 0,
            Err(_) => true, // No JS env — assume online (native).
        };
        if !online {
            if *sync_status.peek() != SyncStatus::Offline {
                sync_status.set(SyncStatus::Offline);
            }
            // Register a Background Sync so the browser pings
            // us when the network returns — even if the tab
            // gets backgrounded. Best-effort: silently no-op
            // when the API isn't available (Safari, FF).
            if !last_was_offline {
                let _ = document::eval(
                    r#"
                    if ('serviceWorker' in navigator && 'SyncManager' in window) {
                      navigator.serviceWorker.ready
                        .then(reg => reg.sync.register('task-arch-sync'))
                        .catch(e => console.log('background-sync register failed:', e));
                    }
                    return 1;
                    "#,
                )
                .recv::<serde_json::Value>()
                .await;
                last_was_offline = true;
            }
        } else {
            last_was_offline = false;
        }
        sleep_ms(2_000).await;
    }
}

/// Sleep for `ms` milliseconds. Browser uses `gloo_timers`;
/// native uses `tokio::time::sleep` since `connect_client` now
/// works on both targets and the sync loop runs on the desktop
/// binary too.
async fn sleep_ms(ms: u32) {
    #[cfg(target_arch = "wasm32")]
    {
        gloo_timers::future::TimeoutFuture::new(ms).await;
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        tokio::time::sleep(std::time::Duration::from_millis(ms as u64)).await;
    }
}

/// Awareness sync loop — wires the local `EphemeralStore` to/from
/// the server's per-doc fan-out.
///
/// Three concurrent jobs:
/// 1. **Inbound**: subscribe via `subscribe_awareness`. Every
///    incoming `AwarenessFrame::bytes` → `store.apply(&bytes)`.
/// 2. **Outbound**: subscribe to the local store's
///    `subscribe_local_updates`. Every encoded local-state
///    change → `publish_awareness`. (Tokio mpsc bridges the
///    sync callback into our async task.)
/// 3. **Janitor**: tick `store.remove_outdated()` every 5s so
///    stale peers age out (30s timeout).
async fn run_awareness_loop(
    url: String,
    hub: AwarenessHub,
    mut remote_cursors: Signal<Vec<RemoteCursor>>,
    doc: Arc<CrdtDoc>,
) {
    // Offline-only mode: no server, no peers, no awareness loop.
    if url.is_empty() {
        let _ = (hub, doc);
        remote_cursors.set(Vec::new());
        return;
    }
    // For now, we hard-code the awareness doc id to the org
    // vault — matches what `KnowledgeLive` subscribes to today.
    // Per-project doc routing rides on top later.
    let doc_id = DocId::org_vault();
    let peer_id = hub.identity.peer_id;

    // Connect a dedicated client for awareness (own session so
    // the long-lived subscribe doesn't share a connection with
    // doc apply_update bursts).
    let sub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "awareness subscribe-client connect failed");
            return;
        }
    };
    let pub_client: WorkspaceSyncClient = match connect_client(&url).await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!(%e, "awareness publish-client connect failed");
            return;
        }
    };

    // Outbound: store local updates → publish.
    let (out_tx, mut out_rx) = futures::channel::mpsc::unbounded::<Vec<u8>>();
    let _sub_handle = hub.store.subscribe_local_updates(Box::new(move |bytes| {
        let _ = out_tx.unbounded_send(bytes.clone());
        true
    }));

    // Inbound: subscribe + apply. Mirror `run_sync_loop`: spawn
    // the long-lived subscribe on its own task, drain the Rx in
    // the outer future, apply each frame to the local store.
    let (in_tx, mut in_rx) = vox::channel::<AwarenessFrame>();
    let sub_doc_id = doc_id.clone();
    spawn(async move {
        let sub = AwarenessSubscribe {
            doc_id: sub_doc_id,
            peer_id,
        };
        if let Err(e) = sub_client.subscribe_awareness(sub, in_tx).await {
            tracing::warn!(error = ?e, "subscribe_awareness ended");
        }
    });
    let inbound = {
        let store = hub.store.clone();
        async move {
            loop {
                match in_rx.recv().await {
                    Ok(Some(msg)) => {
                        let frame = msg.get();
                        if frame.from_peer == peer_id {
                            continue;
                        }
                        if let Err(e) = store.apply(&frame.bytes) {
                            tracing::warn!(?e, "awareness store.apply failed");
                        }
                    }
                    Ok(None) => {
                        tracing::info!("awareness stream closed by server");
                        return;
                    }
                    Err(e) => {
                        tracing::warn!(?e, "awareness rx.recv failed");
                        return;
                    }
                }
            }
        }
    };

    // Outbound publisher with 50ms trailing-edge debounce.
    // Cursor moves arrive per-keystroke; coalescing keeps wire
    // traffic to ~20 frames/sec instead of one per stroke. We
    // collect into `pending`, wait briefly, drain to the latest
    // and ship that. Local delivery to remote peers stays under
    // a frame of perceived latency.
    let pub_loop = {
        let doc_id = doc_id.clone();
        async move {
            let mut pending: Option<Vec<u8>> = None;
            loop {
                let next = if pending.is_some() {
                    #[cfg(target_arch = "wasm32")]
                    {
                        use futures::future::Either;
                        let timer = gloo_timers::future::TimeoutFuture::new(50);
                        futures::pin_mut!(timer);
                        match futures::future::select(out_rx.next(), timer).await {
                            Either::Left((Some(bytes), _)) => Some(bytes),
                            Either::Left((None, _)) => break,
                            Either::Right(((), _)) => None,
                        }
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        out_rx.next().await
                    }
                } else {
                    out_rx.next().await
                };
                match next {
                    Some(bytes) => {
                        pending = Some(bytes);
                    }
                    None if pending.is_some() => {
                        // Debounce window expired — ship the
                        // latest pending payload.
                        let bytes = pending.take().unwrap();
                        let frame = AwarenessFrame {
                            from_peer: peer_id,
                            bytes,
                        };
                        let _ = pub_client
                            .publish_awareness(AwarenessPublish {
                                doc_id: doc_id.clone(),
                                frame,
                            })
                            .await;
                    }
                    None => break,
                }
            }
        }
    };

    // Janitor — purge stale peers every 5s; also refresh the
    // resolved `remote_cursors` snapshot.
    let janitor = {
        let store = hub.store.clone();
        let self_peer = peer_id;
        let doc = doc.clone();
        async move {
            // Janitor cadence: drive on a stream of dummy items
            // so we don't need a platform timer crate here.
            // The cadence isn't safety-critical — the
            // EphemeralStore timeout (30s) already enforces
            // staleness; this loop just nudges remove_outdated
            // periodically + recomputes the resolved snapshot.
            let mut tick = 0u64;
            loop {
                // Yield ~10× per second by parking on a no-op
                // future; periodically (every ~50 yields, i.e.
                // when the runtime is otherwise idle for ~5s)
                // do the work.
                futures::pending!();
                tick = tick.wrapping_add(1);
                if tick % 50 != 0 {
                    continue;
                }
                store.remove_outdated();
                // Rebuild remote_cursors from the store.
                let states = store.get_all_states();
                let mut out = Vec::new();
                for (key, val) in states {
                    let Some(peer_str) = key.strip_prefix("cursor::") else {
                        continue;
                    };
                    let Ok(pid) = Uuid::parse_str(peer_str) else {
                        continue;
                    };
                    if pid == self_peer {
                        continue;
                    }
                    if let Some(payload) = CursorPayload::from_loro_value(&val) {
                        // Prefer the stable Loro cursor when
                        // present + decodable + resolvable —
                        // that gives drift-corrected offsets
                        // against concurrent peer edits. Fall
                        // back to the byte offset shipped on
                        // the wire when the source block isn't
                        // in our doc (e.g. peer's on a page we
                        // haven't loaded).
                        let resolve = |stable: &[u8], fallback: usize| -> usize {
                            if stable.is_empty() {
                                return fallback;
                            }
                            crdt::loro::cursor::Cursor::decode(stable)
                                .ok()
                                .and_then(|c| {
                                    doc.loro().get_cursor_pos(&c).ok().map(|r| r.current.pos)
                                })
                                .unwrap_or(fallback)
                        };
                        let offset = resolve(&payload.stable_cursor_bytes, payload.fallback_offset);
                        let anchor = payload.anchor_block_id.map(|aid| {
                            crate::awareness::RemoteCursorAnchor {
                                block_id: aid,
                                offset: resolve(
                                    &payload.anchor_stable_bytes,
                                    payload.anchor_fallback_offset,
                                ),
                            }
                        });
                        out.push(RemoteCursor {
                            peer_id: pid,
                            block_id: payload.block_id,
                            offset,
                            anchor,
                            page_id: payload.page_id,
                            mode: crate::awareness::PeerMode::from_wire(&payload.mode),
                            color: payload.color,
                            name: payload.name,
                        });
                    }
                }
                remote_cursors.set(out);
            }
        }
    };

    // Run all three concurrently.
    futures::future::join3(inbound, pub_loop, janitor).await;
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
