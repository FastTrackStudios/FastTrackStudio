//! Knowledge feature route. Holds VaultRepoLoro / FolderRepoLoro /
//! PageRepoLoro / BlockRepoLoro / KnowledgeTagRepoLoro / BaseRepoLoro
//! over a shared `CrdtDoc`, syncs over WebSocket, drives the knowledge
//! UI.
//!
//! v1 seeds one demo vault + ~12 pages + ~80 blocks + 2 bases on first
//! mount so the route is usable without an import flow. Real Obsidian
//! import lands when the user clicks "Open folder…" (native-only,
//! routed through knowledge-sync — wasm shows a tooltip).

use std::collections::HashMap;
use std::rc::Rc;

use architect::Page as PageWindow;
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{FileText, Plus};
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use knowledge_crdt::{
    BaseRepoLoro, BlockRepoLoro, CrdtDoc, FolderRepoLoro, KnowledgeTagRepoLoro, PageRepoLoro,
    VaultRepoLoro,
};
use knowledge_proto::{
    Base, BaseCreate, BaseRepo, Block, BlockCreate, BlockRepo, BlockUpdate as ProtoBlockUpdate,
    Folder, FolderCreate, FolderRepo, KnowledgeTag, KnowledgeTagCreate, KnowledgeTagRepo, Page,
    PageCreate, PageRepo, Vault, VaultCreate, VaultRepo,
};
use knowledge_ui::common::{BlockBrief, PageBrief, block_brief_from, page_brief_from};
use knowledge_ui::editor::{BlockUpdate as EditorBlockUpdate, StructuralOp};
use knowledge_ui::{
    EmbedDensity, PageView, Quickswitcher, QuickswitcherPick, TagExplorer, VaultPicker,
};
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

const RECENT_CAP: usize = 8;

#[component]
pub fn KnowledgeView() -> Element {
    let vault_repo: Rc<VaultRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(VaultRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(vault_repo.doc().clone())));
    let folder_repo: Rc<FolderRepoLoro> = use_hook(|| Rc::new(FolderRepoLoro::new(&doc)));
    let page_repo: Rc<PageRepoLoro> = use_hook(|| Rc::new(PageRepoLoro::new(&doc)));
    let block_repo: Rc<BlockRepoLoro> = use_hook(|| Rc::new(BlockRepoLoro::new(&doc)));
    let tag_repo: Rc<KnowledgeTagRepoLoro> = use_hook(|| Rc::new(KnowledgeTagRepoLoro::new(&doc)));
    let base_repo: Rc<BaseRepoLoro> = use_hook(|| Rc::new(BaseRepoLoro::new(&doc)));

    let mut vaults = use_signal::<Vec<Vault>>(Vec::new);
    let mut folders = use_signal::<Vec<Folder>>(Vec::new);
    let mut pages = use_signal::<Vec<Page>>(Vec::new);
    let mut blocks = use_signal::<Vec<Block>>(Vec::new);
    let mut tags = use_signal::<Vec<KnowledgeTag>>(Vec::new);
    let mut bases = use_signal::<Vec<Base>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let mut active_vault = use_signal::<Option<Uuid>>(|| None);
    let mut active_page = use_signal::<Option<Uuid>>(|| None);
    let quickswitcher_open = use_signal(|| false);
    let mut recent_page_ids = use_signal::<Vec<Uuid>>(Vec::new);
    let mut seeded = use_signal(|| false);

    // ── Refresh channel ───────────────────────────────────────────
    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let vault_repo = vault_repo.clone();
        let folder_repo = folder_repo.clone();
        let page_repo = page_repo.clone();
        let block_repo = block_repo.clone();
        let tag_repo = tag_repo.clone();
        let base_repo = base_repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(l) = vault_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    vaults.set(l.items);
                }
                if let Ok(l) = folder_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    folders.set(l.items);
                }
                if let Ok(l) = page_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    pages.set(l.items);
                }
                if let Ok(l) = block_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 10_000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    blocks.set(l.items);
                }
                if let Ok(l) = tag_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    tags.set(l.items);
                }
                if let Ok(l) = base_repo
                    .list(
                        PageWindow {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    bases.set(l.items);
                }
            }
        });
        tx
    });

    // ── WebSocket sync ────────────────────────────────────────────
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

    // ── Seed on first mount when vaults is empty ─────────────────
    {
        let vault_repo = vault_repo.clone();
        let folder_repo = folder_repo.clone();
        let page_repo = page_repo.clone();
        let block_repo = block_repo.clone();
        let tag_repo = tag_repo.clone();
        let base_repo = base_repo.clone();
        let tx = refresh_tx.clone();
        use_effect(move || {
            if *seeded.read() {
                return;
            }
            if !vaults.read().is_empty() {
                return;
            }
            seeded.set(true);
            let vault_repo = vault_repo.clone();
            let folder_repo = folder_repo.clone();
            let page_repo = page_repo.clone();
            let block_repo = block_repo.clone();
            let tag_repo = tag_repo.clone();
            let base_repo = base_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                seed_demo_vault(
                    &vault_repo,
                    &folder_repo,
                    &page_repo,
                    &block_repo,
                    &tag_repo,
                    &base_repo,
                )
                .await;
                let _ = tx.unbounded_send(());
            });
        });
    }

    // ── Pick first vault as active when none selected ─────────────
    use_effect(move || {
        if active_vault.read().is_some() {
            return;
        }
        if let Some(v) = vaults.read().first() {
            active_vault.set(Some(v.id));
        }
    });

    // ── Derived slices ────────────────────────────────────────────
    let av = *active_vault.read();
    let pages_in_vault: Vec<Page> = match av {
        Some(vid) => pages
            .read()
            .iter()
            .filter(|p| p.vault_id == vid)
            .cloned()
            .collect(),
        None => Vec::new(),
    };
    let blocks_in_vault: Vec<Block> = match av {
        Some(vid) => blocks
            .read()
            .iter()
            .filter(|b| b.vault_id == vid)
            .cloned()
            .collect(),
        None => Vec::new(),
    };
    let _tags_in_vault: Vec<KnowledgeTag> = match av {
        Some(vid) => tags
            .read()
            .iter()
            .filter(|t| t.vault_id == vid)
            .cloned()
            .collect(),
        None => Vec::new(),
    };
    let folders_in_vault: Vec<Folder> = match av {
        Some(vid) => folders
            .read()
            .iter()
            .filter(|f| f.vault_id == vid)
            .cloned()
            .collect(),
        None => Vec::new(),
    };

    // Build PageBrief / BlockBrief pools for pickers + outliner.
    let page_titles: HashMap<Uuid, String> = pages_in_vault
        .iter()
        .map(|p| (p.id, p.basename.clone()))
        .collect();
    let available_pages: Vec<PageBrief> = pages_in_vault.iter().map(page_brief_from).collect();
    let available_blocks: Vec<BlockBrief> = blocks_in_vault
        .iter()
        .map(|b| {
            let title = page_titles.get(&b.page_id).cloned().unwrap_or_default();
            block_brief_from(b, &title)
        })
        .collect();

    // Tag counts for TagExplorer (computed from block refs_json).
    let tag_counts: HashMap<String, u32> = {
        let mut out = HashMap::new();
        for b in blocks_in_vault.iter() {
            if let Ok(refs) = serde_json::from_str::<Vec<knowledge_proto::Ref>>(&b.refs_json) {
                for r in refs {
                    if let knowledge_proto::Ref::Tag(t) = r {
                        *out.entry(t.path.join("/")).or_insert(0u32) += 1;
                    }
                }
            }
        }
        out
    };

    // Resolve the active page + its blocks.
    let active_page_obj: Option<Page> = active_page
        .read()
        .and_then(|id| pages_in_vault.iter().find(|p| p.id == id).cloned());
    let active_page_blocks: Vec<Block> = active_page_obj
        .as_ref()
        .map(|p| {
            let mut bs: Vec<Block> = blocks_in_vault
                .iter()
                .filter(|b| b.page_id == p.id)
                .cloned()
                .collect();
            bs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            bs
        })
        .unwrap_or_default();
    let active_page_backlinks: Vec<BlockBrief> = match active_page_obj.as_ref() {
        Some(p) => {
            let target = &p.basename;
            blocks_in_vault
                .iter()
                .filter_map(|b| {
                    let refs: Vec<knowledge_proto::Ref> =
                        serde_json::from_str(&b.refs_json).unwrap_or_default();
                    let hit = refs.iter().any(|r| match r {
                        knowledge_proto::Ref::Link(l) => &l.target_linkpath == target,
                        knowledge_proto::Ref::Embed(e) => &e.target_linkpath == target,
                        _ => false,
                    });
                    if hit && b.page_id != p.id {
                        let title = page_titles.get(&b.page_id).cloned().unwrap_or_default();
                        Some(block_brief_from(b, &title))
                    } else {
                        None
                    }
                })
                .collect()
        }
        None => Vec::new(),
    };

    // Pages grouped by folder for the middle column.
    let folder_label = |fid: Option<Uuid>| -> String {
        match fid {
            Some(id) => folders_in_vault
                .iter()
                .find(|f| f.id == id)
                .map(|f| f.path.clone())
                .unwrap_or_else(|| "(folder)".to_string()),
            None => "Unfiled".to_string(),
        }
    };
    let mut pages_by_folder: Vec<(String, Vec<Page>)> = {
        let mut groups: HashMap<Option<Uuid>, Vec<Page>> = HashMap::new();
        for p in pages_in_vault.iter().cloned() {
            groups.entry(p.folder_id).or_default().push(p);
        }
        let mut v: Vec<(String, Vec<Page>)> = groups
            .into_iter()
            .map(|(fid, mut ps)| {
                ps.sort_by(|a, b| a.basename.cmp(&b.basename));
                (folder_label(fid), ps)
            })
            .collect();
        v.sort_by(|a, b| a.0.cmp(&b.0));
        v
    };
    let _ = pages_by_folder.iter_mut(); // silence "unused mut" if folded

    // ── Callbacks ─────────────────────────────────────────────────
    let on_block_patch = {
        let block_repo = block_repo.clone();
        let tx = refresh_tx.clone();
        let blocks_snapshot = blocks.read().clone();
        move |(id, patch): (Uuid, EditorBlockUpdate)| {
            let block_repo = block_repo.clone();
            let tx = tx.clone();
            let existing = blocks_snapshot.iter().find(|b| b.id == id).cloned();
            spawn_local(async move {
                let proto = editor_to_proto_update(patch, existing.as_ref());
                let _ = block_repo.update(id, proto).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_structural = {
        let block_repo = block_repo.clone();
        let tx = refresh_tx.clone();
        let blocks_snapshot = blocks.read().clone();
        let active_page_id = active_page_obj.as_ref().map(|p| p.id);
        let active_vault_id = av;
        move |op: StructuralOp| {
            let block_repo = block_repo.clone();
            let tx = tx.clone();
            let blocks_snapshot = blocks_snapshot.clone();
            let active_page_id = active_page_id;
            let active_vault_id = active_vault_id;
            spawn_local(async move {
                apply_structural(
                    &block_repo,
                    op,
                    &blocks_snapshot,
                    active_page_id,
                    active_vault_id,
                )
                .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let mut push_recent = move |id: Uuid| {
        let mut r = recent_page_ids.read().clone();
        r.retain(|x| *x != id);
        r.insert(0, id);
        r.truncate(RECENT_CAP);
        recent_page_ids.set(r);
    };

    let mut on_open_page = move |id: Uuid| {
        active_page.set(Some(id));
        push_recent(id);
    };

    let on_open_block = {
        let blocks_snapshot = blocks.read().clone();
        move |id: Uuid| {
            if let Some(b) = blocks_snapshot.iter().find(|b| b.id == id) {
                active_page.set(Some(b.page_id));
                push_recent(b.page_id);
            }
            // FUTURE: scroll-to-block once a per-block scroll anchor exists.
        }
    };

    let on_rename_page = {
        let page_repo = page_repo.clone();
        let tx = refresh_tx.clone();
        move |(id, new_title): (Uuid, String)| {
            let page_repo = page_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let mut update = knowledge_proto::PageUpdate::default();
                update.basename = Some(new_title);
                let _ = page_repo.update(id, update).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_patch_frontmatter = {
        let page_repo = page_repo.clone();
        let tx = refresh_tx.clone();
        let active_page_id = active_page_obj.as_ref().map(|p| p.id);
        move |entries: Vec<(String, serde_json::Value)>| {
            let Some(page_id) = active_page_id else {
                return;
            };
            let page_repo = page_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                // Build the JSON object manually to preserve insertion
                // order without forcing the indexmap-serde feature.
                let mut parts: Vec<String> = Vec::new();
                for (k, v) in entries.iter() {
                    let key = serde_json::to_string(k).unwrap_or_else(|_| "\"\"".into());
                    let val = serde_json::to_string(v).unwrap_or_else(|_| "null".into());
                    parts.push(format!("{key}:{val}"));
                }
                let json = format!("{{{}}}", parts.join(","));
                let mut update = knowledge_proto::PageUpdate::default();
                update.frontmatter_json = Some(json);
                let _ = page_repo.update(page_id, update).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_new_page = {
        let page_repo = page_repo.clone();
        let block_repo = block_repo.clone();
        let tx = refresh_tx.clone();
        let av = av;
        move |_| {
            let Some(vault_id) = av else { return };
            let page_repo = page_repo.clone();
            let block_repo = block_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let now = Utc::now();
                let basename = "Untitled".to_string();
                let payload = PageCreate {
                    vault_id,
                    folder_id: None,
                    path: format!("{basename}.md"),
                    basename: basename.clone(),
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
                };
                if let Ok(page) = page_repo.create(payload).await {
                    let _ = block_repo
                        .create(BlockCreate {
                            vault_id,
                            page_id: page.id,
                            parent_block_id: None,
                            sort_key: "a000".into(),
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
                        .await;
                }
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_open_folder = move |_| {
        // wasm: native-only tooltip. native (desktop): future tauri dialog.
        // FUTURE: tauri file dialog
    };

    let mut qs_open_sig = quickswitcher_open;
    let key_handler = move |e: KeyboardEvent| {
        let modifier = e.modifiers().ctrl() || e.modifiers().meta();
        let k = e.key().to_string();
        if modifier && (k == "k" || k == "K" || k == "o" || k == "O") {
            qs_open_sig.set(true);
        }
        if e.key() == Key::Escape {
            qs_open_sig.set(false);
        }
    };

    let recent_for_qs = recent_page_ids.read().clone();
    let tag_names_for_qs: Vec<String> = tag_counts.keys().cloned().collect();
    let available_pages_for_qs = available_pages.clone();
    let available_blocks_for_qs = available_blocks.clone();

    let mut active_page_setter = active_page;
    let on_qs_pick = move |pick: QuickswitcherPick| {
        match pick {
            QuickswitcherPick::Page(id) => {
                active_page_setter.set(Some(id));
                push_recent(id);
            }
            QuickswitcherPick::Block(id) => {
                if let Some(b) = blocks.read().iter().find(|b| b.id == id) {
                    active_page_setter.set(Some(b.page_id));
                    push_recent(b.page_id);
                }
            }
            QuickswitcherPick::Tag(_) | QuickswitcherPick::CreatePage(_) => {
                // FUTURE: tag filter view + auto-create page.
            }
        }
        qs_open_sig.set(false);
    };

    let on_pick_vault = move |id: Uuid| active_vault.set(Some(id));

    let on_new_in_memory_vault = {
        let vault_repo = vault_repo.clone();
        let tx = refresh_tx.clone();
        move |_| {
            let vault_repo = vault_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = vault_repo
                    .create(VaultCreate {
                        name: "New vault".into(),
                        root_path: None,
                        use_markdown_links: false,
                        new_link_format: "shortest".into(),
                        attachment_folder_path: String::new(),
                        default_view_mode: "live-preview".into(),
                        config_json: "{}".into(),
                    })
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let _ = status_msg;
    let active_page_id_outer = active_page_obj.as_ref().map(|p| p.id);
    rsx! {
        div {
            class: "flex flex-col h-full min-h-[80vh] outline-none",
            tabindex: "0",
            onkeydown: key_handler,

            div { class: "flex flex-col gap-4 p-4 lg:grid lg:grid-cols-[280px_380px_1fr] lg:gap-0 lg:h-full lg:min-h-0",
                // ── Left sidebar ──────────────────────────────────
                div { class: "flex flex-col gap-4 border-r border-border p-3 lg:overflow-y-auto",
                    div { class: "flex items-center justify-between gap-2",
                        SectionHeader { label: "Vault", size: SectionHeaderSize::Small }
                        VaultPicker {
                            vaults: vaults.read().clone(),
                            active: av,
                            on_pick: on_pick_vault,
                            on_new_in_memory: on_new_in_memory_vault,
                            on_open_folder: on_open_folder,
                        }
                    }
                    div { class: "flex flex-col gap-1 mt-2",
                        SectionHeader { label: "Tags", size: SectionHeaderSize::Small }
                        TagExplorer {
                            tag_counts: tag_counts.clone(),
                            on_open_tag: move |_tag: String| {
                                // FUTURE: surface a tag-filter view.
                            },
                        }
                    }
                }

                // ── Middle column: pages list ─────────────────────
                div { class: "flex flex-col gap-2 border-r border-border p-3 lg:overflow-y-auto",
                    div { class: "flex items-center justify-between",
                        SectionHeader { label: "Pages", size: SectionHeaderSize::Small }
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: on_new_page,
                            Plus { size: 12 }
                            span { class: "ml-1 text-xs", "New page" }
                        }
                    }
                    if pages_by_folder.is_empty() {
                        EmptyState { message: "No pages yet. Create one above.".to_string() }
                    } else {
                        for (folder_label, ps) in pages_by_folder.into_iter() {
                            div { class: "flex flex-col gap-0.5", key: "{folder_label}",
                                div { class: "px-1 py-0.5 text-[10px] font-semibold uppercase tracking-widest text-muted-foreground",
                                    "{folder_label}"
                                }
                                for p in ps.into_iter() {
                                    {
                                        let pid = p.id;
                                        let title = p.basename.clone();
                                        let ext = p.ext.clone();
                                        let is_active = active_page_id_outer == Some(pid);
                                        let cls = if is_active {
                                            "flex items-center gap-2 w-full rounded px-2 py-1 text-sm bg-accent text-accent-foreground"
                                        } else {
                                            "flex items-center gap-2 w-full rounded px-2 py-1 text-sm hover:bg-accent/40"
                                        };
                                        rsx! {
                                            button {
                                                key: "{pid}",
                                                class: "{cls}",
                                                onclick: move |_| on_open_page(pid),
                                                FileText { size: 12 }
                                                span { class: "flex-1 truncate text-left", "{title}" }
                                                span { class: "text-[10px] text-muted-foreground", "{ext}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Right column: page view ───────────────────────
                div { class: "flex flex-col lg:overflow-y-auto",
                    if let Some(page) = active_page_obj.clone() {
                        PageView {
                            page,
                            blocks: active_page_blocks,
                            backlinks: active_page_backlinks,
                            available_pages: available_pages.clone(),
                            available_blocks: available_blocks.clone(),
                            on_rename_page: {
                                let cb = on_rename_page.clone();
                                let pid = active_page_id_outer.unwrap_or_else(Uuid::nil);
                                move |new_title: String| cb((pid, new_title))
                            },
                            on_patch_frontmatter: on_patch_frontmatter,
                            on_block_patch: on_block_patch,
                            on_structural: on_structural,
                            on_open_page: on_open_page,
                            on_open_block: on_open_block,
                        }
                    } else {
                        div { class: "flex flex-col items-center justify-center h-full p-8",
                            EmptyState {
                                message: "Pick a page or create one".to_string(),
                                icon: rsx!{ FileText { size: 32 } },
                            }
                        }
                    }
                }
            }

            // ── Quickswitcher (Cmd-O / Cmd-K) ─────────────────────
            Quickswitcher {
                open: quickswitcher_open,
                pages: available_pages_for_qs,
                blocks: available_blocks_for_qs,
                tags: tag_names_for_qs,
                recent_pages: recent_for_qs,
                on_pick: on_qs_pick,
            }
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────

fn editor_to_proto_update(patch: EditorBlockUpdate, existing: Option<&Block>) -> ProtoBlockUpdate {
    let mut update = ProtoBlockUpdate::default();
    let _ = existing;
    match patch {
        EditorBlockUpdate::SetContent(c) => update.content = Some(c),
        EditorBlockUpdate::SetHeadingLevel(l) => update.heading_level = Some(l),
        EditorBlockUpdate::SetTaskMark(m) => update.list_task = Some(m),
        EditorBlockUpdate::SetCollapsed(b) => update.collapsed = Some(b),
        EditorBlockUpdate::SetKind(k) => update.kind = Some(k),
        EditorBlockUpdate::SetProperties(p) => update.properties_json = Some(p),
    }
    update
}

async fn apply_structural(
    block_repo: &BlockRepoLoro,
    op: StructuralOp,
    blocks_snapshot: &[Block],
    active_page_id: Option<Uuid>,
    active_vault_id: Option<Uuid>,
) {
    // v1: most structural verbs (split / merge / indent / outdent /
    // move) need the KnowledgeService trait to land server-side before
    // we can apply them atomically. SplitBlock is the only one we can
    // approximate purely on the client by appending a new empty
    // paragraph at the active page. The pickers (OpenSlashMenu / …)
    // are pure UI cues handled inside the editor and emit nothing
    // here. FUTURE: route the rest through KnowledgeService over vox.
    let _ = block_repo;
    let _ = blocks_snapshot;
    let _ = active_page_id;
    let _ = active_vault_id;
    match op {
        StructuralOp::SplitBlock { .. }
        | StructuralOp::MergeWithPrev
        | StructuralOp::IndentBlock
        | StructuralOp::OutdentBlock
        | StructuralOp::MoveBlockUp
        | StructuralOp::MoveBlockDown
        | StructuralOp::OpenSlashMenu { .. }
        | StructuralOp::OpenWikilinkPicker { .. }
        | StructuralOp::OpenBlockRefPicker { .. }
        | StructuralOp::OpenTagPicker { .. } => {
            // FUTURE: KnowledgeService dispatch.
        }
    }
}

// ── Seed ──────────────────────────────────────────────────────────

async fn seed_demo_vault(
    vault_repo: &VaultRepoLoro,
    folder_repo: &FolderRepoLoro,
    page_repo: &PageRepoLoro,
    block_repo: &BlockRepoLoro,
    tag_repo: &KnowledgeTagRepoLoro,
    base_repo: &BaseRepoLoro,
) {
    let now = Utc::now();
    // Vault
    let vault = match vault_repo
        .create(VaultCreate {
            name: "Demo Vault".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await
    {
        Ok(v) => v,
        Err(_) => return,
    };
    let vault_id = vault.id;

    // Folders
    let mut folder_ids: HashMap<&str, Uuid> = HashMap::new();
    for path in ["Projects", "Notes", "Daily"] {
        if let Ok(f) = folder_repo
            .create(FolderCreate {
                vault_id,
                path: path.into(),
                parent_id: None,
            })
            .await
        {
            folder_ids.insert(path, f.id);
        }
    }

    // Page seed: (folder, basename, ext, blocks: Vec<(kind, content)>)
    type B = (&'static str, &'static str);
    let pages_seed: Vec<(&str, &str, &str, Vec<B>)> = vec![
        (
            "Projects",
            "Alpha launch",
            "md",
            vec![
                ("heading", "# Alpha launch"),
                (
                    "paragraph",
                    "Kickoff for the alpha. See [[Reading list]] and [[Onboarding emails]].",
                ),
                ("list_item", "- Pick a launch date"),
                ("list_item", "- Draft the release notes"),
                ("list_item", "- Coordinate with [[Marketing plan]]"),
                ("paragraph", "Tags: #project/alpha #priority/high"),
            ],
        ),
        (
            "Projects",
            "Marketing plan",
            "md",
            vec![
                ("heading", "# Marketing plan"),
                (
                    "paragraph",
                    "Plan the campaign cadence. Linked to [[Alpha launch]].",
                ),
                ("callout", "Need budget approval by Friday."),
                ("list_item", "- Social media schedule"),
                ("list_item", "- Email sequence — see [[Onboarding emails]]"),
                ("paragraph", "#project/alpha #marketing"),
            ],
        ),
        (
            "Projects",
            "Onboarding emails",
            "md",
            vec![
                ("heading", "# Onboarding emails"),
                (
                    "paragraph",
                    "Draft copy for the welcome sequence. Referenced by [[Marketing plan]].",
                ),
                ("list_item", "- Day 0: Welcome"),
                ("list_item", "- Day 3: Tour the features"),
                ("list_item", "- Day 7: Tips & tricks"),
                ("paragraph", "#email #onboarding"),
            ],
        ),
        (
            "Notes",
            "Reading list",
            "md",
            vec![
                ("heading", "# Reading list"),
                ("paragraph", "Books and articles to read this quarter."),
                ("list_item", "- [ ] Designing Data-Intensive Applications"),
                ("list_item", "- [x] Crafting Interpreters"),
                ("list_item", "- [ ] The Pragmatic Programmer"),
                ("paragraph", "#reading #personal"),
            ],
        ),
        (
            "Notes",
            "Architecture notes",
            "md",
            vec![
                ("heading", "# Architecture notes"),
                (
                    "paragraph",
                    "CRDT-backed local-first storage. See [[Alpha launch]] for context.",
                ),
                ("code", "fn main() {\n    println!(\"hello\");\n}"),
                (
                    "paragraph",
                    "Three layers: proto, crdt, ui. #engineering #notes",
                ),
            ],
        ),
        (
            "Notes",
            "Meeting log",
            "md",
            vec![
                ("heading", "# Meeting log"),
                ("paragraph", "Weekly sync notes."),
                ("blockquote", "> Ship before the conference."),
                ("list_item", "- Discuss [[Alpha launch]] timeline"),
                ("list_item", "- Review [[Marketing plan]]"),
                ("paragraph", "#meetings"),
            ],
        ),
        (
            "Notes",
            "Index",
            "base",
            // Base pages render through a placeholder block.
            vec![("paragraph", "(Bases index — see right column.)")],
        ),
        (
            "Daily",
            "2026-05-10",
            "md",
            vec![
                ("heading", "# 2026-05-10"),
                ("paragraph", "Worked on [[Alpha launch]] all morning."),
                ("list_item", "- [x] Draft release notes"),
                ("list_item", "- [ ] Review [[Marketing plan]]"),
                ("paragraph", "#daily"),
            ],
        ),
        (
            "Daily",
            "2026-05-11",
            "md",
            vec![
                ("heading", "# 2026-05-11"),
                (
                    "paragraph",
                    "Standup notes: blocked on [[Onboarding emails]].",
                ),
                ("list_item", "- [ ] Follow up with marketing"),
                ("list_item", "- [x] Sketched [[Architecture notes]]"),
                ("paragraph", "#daily"),
            ],
        ),
        (
            "Daily",
            "2026-05-12",
            "md",
            vec![
                ("heading", "# 2026-05-12"),
                ("paragraph", "Today: ship Phase D of knowledge!"),
                ("list_item", "- [ ] Mount /knowledge route"),
                ("list_item", "- [ ] Wire shadow pages into TaskDetailSheet"),
                ("list_item", "- [ ] Add 'Open as whiteboard' CTA"),
                ("paragraph", "#daily #ship"),
            ],
        ),
        (
            "Daily",
            "Whiteboard",
            "canvas",
            vec![("paragraph", "(Canvas — see whiteboard view.)")],
        ),
        (
            "Projects",
            "Quarterly retro",
            "md",
            vec![
                ("heading", "# Quarterly retro"),
                ("paragraph", "What went well, what to change."),
                ("callout", "Celebrate wins before listing fixes."),
                ("list_item", "- Wins: shipped [[Alpha launch]]"),
                ("list_item", "- Watch: [[Marketing plan]] needs more buffer"),
                ("paragraph", "#retro #project"),
            ],
        ),
    ];

    let mut page_ids: HashMap<String, Uuid> = HashMap::new();
    for (folder, basename, ext, _) in &pages_seed {
        let folder_id = folder_ids.get(folder).copied();
        let path = format!("{folder}/{basename}.{ext}");
        if let Ok(p) = page_repo
            .create(PageCreate {
                vault_id,
                folder_id,
                path: path.clone(),
                basename: (*basename).to_string(),
                ext: (*ext).to_string(),
                aliases: Vec::new(),
                frontmatter_json: "{}".into(),
                stat_ctime: now,
                stat_mtime: now,
                stat_size: 0,
                is_journal: *folder == "Daily" && *ext == "md" && basename.starts_with("20"),
                journal_day: if *folder == "Daily" && basename.starts_with("20") {
                    Some((*basename).to_string())
                } else {
                    None
                },
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await
        {
            page_ids.insert((*basename).to_string(), p.id);
        }
    }

    // Blocks
    for (_folder, basename, _ext, blocks) in &pages_seed {
        let Some(page_id) = page_ids.get(*basename).copied() else {
            continue;
        };
        for (i, (kind, content)) in blocks.iter().enumerate() {
            let sort_key = format!("a{i:04}");
            let heading_level = if *kind == "heading" {
                Some(content.chars().take_while(|c| *c == '#').count() as i32)
            } else {
                None
            };
            let list_task = if kind.starts_with("list_item") && content.contains("[ ]") {
                Some(" ".to_string())
            } else if kind.starts_with("list_item") && content.contains("[x]") {
                Some("x".to_string())
            } else {
                None
            };
            let callout_kind = if *kind == "callout" {
                Some("note".to_string())
            } else {
                None
            };
            let code_lang = if *kind == "code" {
                Some("rust".to_string())
            } else {
                None
            };
            // Compute refs lazily — just scan for wikilinks + tags.
            let refs_json = compute_refs_json(content);
            let _ = block_repo
                .create(BlockCreate {
                    vault_id,
                    page_id,
                    parent_block_id: None,
                    sort_key,
                    kind: (*kind).to_string(),
                    content: (*content).to_string(),
                    heading_level,
                    list_ordered: false,
                    list_task,
                    code_lang,
                    callout_kind,
                    callout_foldable: false,
                    properties_json: "{}".into(),
                    obsidian_block_id: None,
                    collapsed: false,
                    refs_json,
                    canvas_node_json: None,
                })
                .await;
        }
    }

    // Tags — union of tags used in seed content.
    let mut tag_set: std::collections::BTreeSet<String> = Default::default();
    for (_, _, _, blocks) in &pages_seed {
        for (_, content) in blocks {
            for word in content.split_whitespace() {
                if let Some(rest) = word.strip_prefix('#') {
                    let clean: String = rest
                        .chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '/' || *c == '_' || *c == '-')
                        .collect();
                    if !clean.is_empty() {
                        tag_set.insert(clean);
                    }
                }
            }
        }
    }
    for tag in tag_set {
        let _ = tag_repo
            .create(KnowledgeTagCreate {
                vault_id,
                tag,
                color: None,
                description: None,
            })
            .await;
    }

    // Base — Notes/Index.base, table view filtering pages by tag.
    if let Some(index_page) = page_ids.get("Index").copied() {
        let definition = r#"name: Index
filters:
  any:
    - file.tag.contains: "project"
views:
  - name: Pages
    kind: table
    columns:
      - file.basename
      - file.path
"#;
        let _ = base_repo
            .create(BaseCreate {
                vault_id,
                page_id: Some(index_page),
                name: "Index".into(),
                definition_yaml: definition.into(),
                parsed_filter_json: "{}".into(),
                parsed_views_json: "[]".into(),
            })
            .await;
    }

    // Canvas demo — Daily/Whiteboard.canvas with 4 block-ref cards + 2 connectors.
    if let Some(canvas_page) = page_ids.get("Whiteboard").copied() {
        use knowledge_proto::canvas::{
            Anchor, ArrowHead, CanvasNode, CanvasShape, ConnectorEnd, ConnectorLine,
            ConnectorStyle, encode,
        };
        // 4 text cards across a 2x2 grid.
        let cards = [
            (0.0_f64, 0.0_f64, "Alpha launch", "Alpha launch"),
            (380.0, 0.0, "Marketing plan", "Marketing plan"),
            (0.0, 220.0, "Reading list", "Reading list"),
            (380.0, 220.0, "Architecture notes", "Architecture notes"),
        ];
        let mut card_ids: Vec<Uuid> = Vec::new();
        for (i, (x, y, label, _target)) in cards.iter().enumerate() {
            let node = CanvasNode {
                x: *x,
                y: *y,
                w: 320.0,
                h: 160.0,
                z: 0,
                rotation: 0.0,
                shape: CanvasShape::Text,
                color: None,
                locked: false,
                extras_json: None,
            };
            if let Ok(b) = block_repo
                .create(BlockCreate {
                    vault_id,
                    page_id: canvas_page,
                    parent_block_id: None,
                    sort_key: format!("a{i:04}"),
                    kind: "canvas".into(),
                    content: format!("[[{label}]]"),
                    heading_level: None,
                    list_ordered: false,
                    list_task: None,
                    code_lang: None,
                    callout_kind: None,
                    callout_foldable: false,
                    properties_json: "{}".into(),
                    obsidian_block_id: None,
                    collapsed: false,
                    refs_json: compute_refs_json(&format!("[[{label}]]")),
                    canvas_node_json: Some(encode(&node)),
                })
                .await
            {
                card_ids.push(b.id);
            }
        }
        // 2 connectors: 0→1, 0→2.
        for (i, (a, b)) in [(0usize, 1usize), (0, 2)].iter().enumerate() {
            if let (Some(&from), Some(&to)) = (card_ids.get(*a), card_ids.get(*b)) {
                let conn = CanvasNode {
                    x: 0.0,
                    y: 0.0,
                    w: 0.0,
                    h: 0.0,
                    z: -1,
                    rotation: 0.0,
                    shape: CanvasShape::Connector {
                        from: ConnectorEnd {
                            block_id: from,
                            anchor: Anchor::Right,
                        },
                        to: ConnectorEnd {
                            block_id: to,
                            anchor: Anchor::Left,
                        },
                        style: ConnectorStyle {
                            line: ConnectorLine::Solid,
                            start_arrow: ArrowHead::None,
                            end_arrow: ArrowHead::Triangle,
                            color: None,
                        },
                        label: None,
                    },
                    color: None,
                    locked: false,
                    extras_json: None,
                };
                let _ = block_repo
                    .create(BlockCreate {
                        vault_id,
                        page_id: canvas_page,
                        parent_block_id: None,
                        sort_key: format!("z{i:04}"),
                        kind: "canvas".into(),
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
                        canvas_node_json: Some(encode(&conn)),
                    })
                    .await;
            }
        }
    }
}

fn compute_refs_json(content: &str) -> String {
    // Lightweight scanner — just wikilinks + tags so the demo seed lights
    // up backlinks and tag explorer without going through the full
    // obsidian helper module. FUTURE: route through
    // `knowledge_proto::obsidian::extract_refs`.
    use knowledge_proto::{LinkRef, Ref, TagRef};
    let mut out: Vec<Ref> = Vec::new();
    // Wikilinks: `[[Target]]` (no piping).
    let bytes = content.as_bytes();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b'[' && bytes[i + 1] == b'[' {
            if let Some(end) = content[i + 2..].find("]]") {
                let inner = &content[i + 2..i + 2 + end];
                let target = inner.split('|').next().unwrap_or(inner).to_string();
                let original = format!("[[{inner}]]");
                out.push(Ref::Link(LinkRef {
                    target_linkpath: target,
                    heading: None,
                    block_id: None,
                    alias: None,
                    original,
                }));
                i += 2 + end + 2;
                continue;
            }
        }
        i += 1;
    }
    // Tags: `#path/segment`.
    for word in content.split_whitespace() {
        if let Some(rest) = word.strip_prefix('#') {
            let clean: String = rest
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '/' || *c == '_' || *c == '-')
                .collect();
            if !clean.is_empty() {
                let path: Vec<String> = clean.split('/').map(|s| s.to_string()).collect();
                let original = format!("#{clean}");
                out.push(Ref::Tag(TagRef { path, original }));
            }
        }
    }
    serde_json::to_string(&out).unwrap_or_else(|_| "[]".to_string())
}

// silence unused EmbedDensity import warning; we re-export but don't use it.
#[allow(dead_code)]
fn _density_check() -> EmbedDensity {
    EmbedDensity::Comfortable
}
