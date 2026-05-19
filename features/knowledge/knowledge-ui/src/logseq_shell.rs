//! Logseq-faithful UI shell.
//!
//! Visual + interaction port of Logseq's desktop UI. Built fresh
//! rather than retrofitted onto the existing outliner so the
//! layout, theme tokens, and keyboard model can match Logseq
//! verbatim (or as close as a Dioxus port reasonably gets).
//!
//! Lays out as:
//!
//!   ┌──────────────┬──────────────────────────┬──────────┐
//!   │ Header (48px sticky, drag-region)                  │
//!   ├──────────────┼──────────────────────────┼──────────┤
//!   │ LeftSidebar  │  Main (960px max)        │ RightSb  │
//!   │  246px       │   - PageTitle            │  open    │
//!   │  • Journals  │   - properties block     │  via     │
//!   │  • All Pages │   - block tree           │  shift   │
//!   │  • Graph     │                          │  click   │
//!   │  • Favorites │                          │          │
//!   │  • Recent    │                          │          │
//!   │  • Pages     │                          │          │
//!   └──────────────┴──────────────────────────┴──────────┘
//!
//! Color tokens, sizes, and indent units mirror Logseq's
//! `vars-classic.css` so the visual feel is identical without
//! shipping the original CSS.

use std::sync::Arc;

use chrono::Utc;
use crdt::CrdtDoc;
use dioxus::prelude::*;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro};
use knowledge_proto::{
    Block, BlockCreate, BlockRepo, BlockUpdate, Page, PageRepo, architect::Page as ListPage,
};
use uuid::Uuid;

use publish_core::{
    BlockRefResolver, BlockRefTarget, InlineNode, NamespaceResolver, PageEmbedResolver, QueryHit,
    QueryResolver, WikiResolver, slugify,
};

/// Inline CSS injected once per shell. Original palette + token
/// naming chosen for this app — derived from a neutral
/// blue-gray dark scheme rather than a port of any specific
/// external stylesheet. Tokens are referenced by both dark and
/// light variants further down.
const LOGSEQ_CSS: &str = r#"
:root, .ls-theme-dark {
    /* Neutral slate-blue dark theme — original palette. */
    --ls-primary-background-color: #161a22;
    --ls-secondary-background-color: #1d222c;
    --ls-tertiary-background-color: #11151c;
    --ls-quaternary-background-color: #232a35;
    --ls-active-primary-color: #6aa9b8;
    --ls-active-secondary-color: #5f8fb0;
    --ls-primary-text-color: #c5cad2;
    --ls-secondary-text-color: #7a8290;
    --ls-block-bullet-color: #5c6470;
    --ls-block-bullet-active-color: #6aa9b8;
    --ls-border-color: #2a313d;
    --ls-secondary-border-color: #3a4250;
    --ls-link-text-color: #7eb9c8;
    --ls-link-text-hover-color: #a2d2dd;
    --ls-tag-text-color: #8aa580;
    --ls-page-text-size: 1em;
    --ls-page-title-size: 32px;
    --ls-left-sidebar-width: 240px;
    --ls-headbar-height: 2.75rem;
    --ls-main-content-max-width: 880px;
    --ls-block-properties-background-color: #1a1f29;
    --ls-page-mark-bg-color: #423a2c;
    --ls-page-mark-color: #d8c98a;
    --ls-font-family: -apple-system, BlinkMacSystemFont, "Inter", "Segoe UI", Roboto, sans-serif;
}
.ls-theme-light {
    /* Warm-paper light theme — independent of the dark one. */
    --ls-primary-background-color: #fafaf6;
    --ls-secondary-background-color: #f1efe8;
    --ls-tertiary-background-color: #ffffff;
    --ls-quaternary-background-color: #e7e4da;
    --ls-active-primary-color: #4c8ea0;
    --ls-active-secondary-color: #3a78a0;
    --ls-primary-text-color: #2d343e;
    --ls-secondary-text-color: #6f7682;
    --ls-block-bullet-color: #b8b8b0;
    --ls-block-bullet-active-color: #4c8ea0;
    --ls-border-color: #ddd9cf;
    --ls-secondary-border-color: #c8c4ba;
    --ls-link-text-color: #2c7388;
    --ls-link-text-hover-color: #1a4e60;
    --ls-tag-text-color: #5e7d3b;
    --ls-block-properties-background-color: #efece4;
    --ls-page-mark-bg-color: #f4e9c4;
    --ls-page-mark-color: #5c4d20;
}
html, body {
    background: var(--ls-primary-background-color);
    color: var(--ls-primary-text-color);
    font-family: var(--ls-font-family);
    margin: 0;
    padding: 0;
    height: 100vh;
    overflow: hidden;
}
.ls-shell {
    display: grid;
    grid-template-rows: var(--ls-headbar-height) 1fr;
    height: 100vh;
}
.ls-shell .ls-header {
    position: sticky; top: 0;
    display: flex; align-items: center; gap: 0.5em;
    padding: 0 0.75em;
    background: var(--ls-primary-background-color);
    border-bottom: 1px solid var(--ls-border-color);
    z-index: 30;
    user-select: none;
}
.ls-shell .ls-header .ls-app-title {
    font-weight: 600;
    color: var(--ls-active-primary-color);
    letter-spacing: 0.04em;
}
.ls-shell .ls-header .ls-search {
    flex: 1; max-width: 520px;
    padding: 0.3em 0.75em;
    background: var(--ls-secondary-background-color);
    border: 1px solid var(--ls-border-color);
    border-radius: 0.4em;
    color: var(--ls-primary-text-color);
    outline: none;
}
.ls-shell .ls-header .ls-search:focus {
    border-color: var(--ls-active-primary-color);
}
.ls-shell .ls-header .ls-spacer { flex: 1; }
.ls-shell .ls-header .ls-status {
    font-size: 0.75rem;
    color: var(--ls-secondary-text-color);
    padding: 0.15em 0.5em;
    border: 1px solid var(--ls-border-color);
    border-radius: 0.3em;
}
.ls-shell .ls-body {
    display: grid;
    grid-template-columns: var(--ls-left-sidebar-width) 1fr auto;
    overflow: hidden;
}
.ls-shell .ls-left-sidebar {
    background: var(--ls-secondary-background-color);
    border-right: 1px solid var(--ls-border-color);
    overflow-y: auto;
    padding: 0.75em 0;
    font-size: 0.9em;
}
.ls-shell .ls-nav-item {
    display: flex; align-items: center; gap: 0.5em;
    padding: 0.35em 1em;
    color: var(--ls-primary-text-color);
    cursor: pointer;
    user-select: none;
    text-decoration: none;
}
.ls-shell .ls-nav-item:hover {
    background: var(--ls-quaternary-background-color);
}
.ls-shell .ls-nav-item.active {
    background: var(--ls-quaternary-background-color);
    color: var(--ls-active-primary-color);
}
.ls-shell .ls-nav-icon {
    width: 1.2em;
    text-align: center;
    color: var(--ls-secondary-text-color);
}
.ls-shell .ls-sidebar-section {
    margin-top: 1em;
    padding: 0 1em;
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    color: var(--ls-secondary-text-color);
    user-select: none;
}
.ls-shell .ls-page-list { margin: 0.25em 0; }
.ls-shell .ls-page-link {
    display: block;
    padding: 0.25em 1em;
    color: var(--ls-primary-text-color);
    cursor: pointer;
    user-select: none;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
    text-decoration: none;
}
.ls-shell .ls-page-link:hover { color: var(--ls-active-primary-color); background: var(--ls-quaternary-background-color); }
.ls-shell .ls-page-link.active { color: var(--ls-active-primary-color); }
.ls-shell .ls-main {
    overflow-y: auto;
    padding: 2em 0 6em;
    background: var(--ls-primary-background-color);
}
.ls-shell .ls-main-inner {
    max-width: var(--ls-main-content-max-width);
    margin: 0 auto;
    padding: 0 2em;
}
.ls-shell .ls-page-title {
    font-size: var(--ls-page-title-size);
    font-weight: 500;
    color: var(--ls-primary-text-color);
    margin: 0 0 1em;
    line-height: 1.2;
    outline: none;
}
.ls-shell .ls-page-title:focus { border-bottom: 1px solid var(--ls-active-primary-color); }
.ls-shell .ls-block {
    position: relative;
    display: flex;
    align-items: flex-start;
}
.ls-shell .ls-block-children {
    margin-left: 1.4em;
    border-left: 1px solid var(--ls-border-color);
    padding-left: 0.5em;
}
.ls-shell .ls-block-row {
    display: flex; align-items: flex-start; gap: 0.4em;
    padding: 0.1em 0;
    width: 100%;
}
.ls-shell .ls-fold {
    width: 0.9em; flex-shrink: 0;
    color: var(--ls-secondary-text-color);
    cursor: pointer; user-select: none;
    font-size: 0.7em;
    line-height: 1.6;
    text-align: center;
    opacity: 0;
    transition: opacity 0.1s;
}
.ls-shell .ls-block:hover .ls-fold { opacity: 1; }
.ls-shell .ls-fold.has-children { opacity: 0.7; }
.ls-shell .ls-fold.has-children:hover { opacity: 1; color: var(--ls-active-primary-color); }
.ls-shell .ls-bullet {
    width: 1.2em;
    height: 1.2em;
    flex-shrink: 0;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    user-select: none;
}
.ls-shell .ls-bullet-dot {
    width: 6px; height: 6px;
    background: var(--ls-block-bullet-color);
    border-radius: 50%;
    transition: all 0.1s;
}
.ls-shell .ls-bullet:hover .ls-bullet-dot {
    background: var(--ls-block-bullet-active-color);
    width: 9px; height: 9px;
}
.ls-shell .ls-bullet.has-children .ls-bullet-dot {
    background: var(--ls-block-bullet-active-color);
    box-shadow: 0 0 0 4px color-mix(in srgb, var(--ls-block-bullet-active-color) 18%, transparent);
}
.ls-shell .ls-block-content {
    flex: 1;
    min-width: 0;
    min-height: 1.5em;
    line-height: 1.5;
    padding: 0.05em 0;
    color: var(--ls-primary-text-color);
    white-space: pre-wrap;
    word-wrap: break-word;
}
.ls-shell .ls-block-content[contenteditable="true"] {
    outline: none;
    background: var(--ls-secondary-background-color);
    border-radius: 0.2em;
    padding: 0.05em 0.4em;
    margin: -0.05em -0.4em;
}
.ls-shell .ls-block-empty {
    color: var(--ls-secondary-text-color);
    opacity: 0.5;
}
.ls-shell .ls-block-heading-1 { font-size: 1.8em; font-weight: 600; margin: 0.5em 0 0.2em; }
.ls-shell .ls-block-heading-2 { font-size: 1.5em; font-weight: 600; margin: 0.4em 0 0.2em; }
.ls-shell .ls-block-heading-3 { font-size: 1.25em; font-weight: 600; margin: 0.3em 0 0.2em; }
.ls-shell .ls-block-heading-4 { font-size: 1.1em; font-weight: 600; }
.ls-shell .ls-block-heading-5 { font-size: 1em; font-weight: 600; }
.ls-shell .ls-block-heading-6 { font-size: 0.9em; font-weight: 600; }
.ls-shell .ls-block-content a.wikilink {
    color: var(--ls-link-text-color);
    text-decoration: none;
    cursor: pointer;
}
.ls-shell .ls-block-content a.wikilink:hover { color: var(--ls-link-text-hover-color); text-decoration: underline; }
.ls-shell .ls-block-content a.wikilink.broken { color: var(--ls-tag-text-color); opacity: 0.7; text-decoration: underline dotted; }
.ls-shell .ls-block-content .wikilink-wrap {
    position: relative;
    display: inline;
}
.ls-shell .ls-block-content .wikilink-preview {
    position: absolute;
    left: 0;
    top: 100%;
    margin-top: 0.3em;
    max-width: 360px;
    padding: 0.5em 0.7em;
    background: var(--ls-secondary-background-color);
    border: 1px solid var(--ls-border-color);
    border-radius: 4px;
    color: var(--ls-secondary-text-color);
    font-size: 0.85rem;
    line-height: 1.4;
    z-index: 50;
    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.15s ease-in-out 0.25s;
    white-space: normal;
}
.ls-shell .ls-block-content .wikilink-wrap:hover .wikilink-preview {
    opacity: 1;
}
.ls-shell .ls-block-content .tag {
    display: inline-block;
    color: var(--ls-tag-text-color);
    background: transparent;
    border: none;
    padding: 0;
    font-size: inherit;
    cursor: pointer;
    text-decoration: none;
}
.ls-shell .ls-block-content .tag:hover {
    text-decoration: underline;
}
.ls-shell .ls-block-content a.block-ref,
.ls-shell .ls-block-content span.block-ref {
    background: var(--ls-tertiary-background-color);
    border: 1px solid var(--ls-border-color);
    padding: 0.05em 0.4em;
    border-radius: 0.3em;
    font-size: 0.9em;
    color: var(--ls-link-text-color);
    text-decoration: none;
}
.ls-shell .ls-block-content a.block-ref:hover { color: var(--ls-link-text-hover-color); border-color: var(--ls-active-primary-color); }
.ls-shell .ls-block-content code {
    background: var(--ls-tertiary-background-color);
    border: 1px solid var(--ls-border-color);
    padding: 0.05em 0.35em;
    border-radius: 0.2em;
    font-size: 0.9em;
    font-family: ui-monospace, "SF Mono", Menlo, Monaco, monospace;
}
.ls-shell .ls-task-marker {
    display: inline-block;
    padding: 0 0.4em;
    border-radius: 0.25em;
    font-family: ui-monospace, "SF Mono", Menlo, Monaco, monospace;
    font-size: 0.7rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    margin-right: 0.4em;
    vertical-align: 0.1em;
    border: 1px solid transparent;
}
.ls-shell .ls-task-marker.todo { background: color-mix(in srgb, var(--ls-active-primary-color) 12%, transparent); color: var(--ls-active-primary-color); border-color: color-mix(in srgb, var(--ls-active-primary-color) 30%, transparent); }
.ls-shell .ls-task-marker.doing,
.ls-shell .ls-task-marker.now { background: color-mix(in srgb, #f59e0b 18%, transparent); color: #f59e0b; border-color: color-mix(in srgb, #f59e0b 35%, transparent); }
.ls-shell .ls-task-marker.done { background: color-mix(in srgb, #10b981 14%, transparent); color: #10b981; border-color: color-mix(in srgb, #10b981 30%, transparent); }
.ls-shell .ls-task-marker.later,
.ls-shell .ls-task-marker.waiting { background: var(--ls-tertiary-background-color); color: var(--ls-secondary-text-color); border-color: var(--ls-border-color); }
.ls-shell .ls-task-marker.cancelled { background: color-mix(in srgb, #f87171 14%, transparent); color: #f87171; border-color: color-mix(in srgb, #f87171 30%, transparent); text-decoration: line-through; }
.ls-shell .ls-block-plan {
    display: flex; gap: 0.4em; margin: 0.1em 0 0 1.6em;
    font-family: ui-monospace, "SF Mono", Menlo, Monaco, monospace;
    font-size: 0.7rem;
}
.ls-shell .ls-plan-pill {
    display: inline-flex;
    border: 1px solid var(--ls-border-color);
    border-radius: 0.25em;
    overflow: hidden;
}
.ls-shell .ls-plan-pill .ls-plan-key { padding: 0.05em 0.45em; background: var(--ls-tertiary-background-color); color: var(--ls-secondary-text-color); font-weight: 700; }
.ls-shell .ls-plan-pill .ls-plan-val { padding: 0.05em 0.5em; }
.ls-shell .ls-plan-pill.deadline .ls-plan-key { color: #f87171; }
.ls-shell .ls-block-props {
    display: flex; flex-wrap: wrap; gap: 0.4em;
    margin: 0.1em 0 0.2em 1.6em;
    font-size: 0.75rem;
}
.ls-shell .ls-prop-chip {
    display: inline-flex;
    border: 1px solid var(--ls-border-color);
    border-radius: 0.25em;
    overflow: hidden;
}
.ls-shell .ls-prop-chip .ls-prop-key { padding: 0.05em 0.45em; background: var(--ls-tertiary-background-color); color: var(--ls-secondary-text-color); font-weight: 600; }
.ls-shell .ls-prop-chip .ls-prop-val { padding: 0.05em 0.5em; }
.ls-shell .ls-drawer {
    margin: 0.3em 0 0.3em 1.6em;
    border: 1px solid var(--ls-border-color);
    border-radius: 0.4em;
    background: var(--ls-tertiary-background-color);
    font-size: 0.8rem;
}
.ls-shell .ls-drawer .ls-drawer-name {
    padding: 0.25em 0.6em;
    color: var(--ls-secondary-text-color);
    font-family: ui-monospace, "SF Mono", Menlo, Monaco, monospace;
    font-weight: 700;
    letter-spacing: 0.05em;
    cursor: pointer;
    user-select: none;
}
.ls-shell .ls-drawer .ls-drawer-name::before { content: "▸ "; }
.ls-shell .ls-drawer[open] .ls-drawer-name::before { content: "▾ "; }
.ls-shell .ls-drawer .ls-drawer-body {
    margin: 0;
    padding: 0.4em 0.75em;
    border-top: 1px solid var(--ls-border-color);
    color: var(--ls-primary-text-color);
    font-family: ui-monospace, "SF Mono", Menlo, Monaco, monospace;
    white-space: pre-wrap;
    overflow-x: auto;
}
.ls-shell .ls-right-sidebar {
    width: 320px;
    background: var(--ls-secondary-background-color);
    border-left: 1px solid var(--ls-border-color);
    overflow-y: auto;
    padding: 0.75em;
    display: flex; flex-direction: column;
    gap: 0.6em;
}
.ls-shell .ls-right-sidebar-header {
    display: flex; align-items: center; justify-content: space-between;
    font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em;
    color: var(--ls-secondary-text-color);
    border-bottom: 1px solid var(--ls-border-color);
    padding-bottom: 0.3em;
}
.ls-shell .ls-right-sidebar-card {
    border: 1px solid var(--ls-border-color);
    border-radius: 0.4em;
    background: var(--ls-tertiary-background-color);
    padding: 0.5em 0.6em;
    font-size: 0.9em;
}
.ls-shell .ls-right-sidebar-card .ls-sidebar-close {
    float: right;
    cursor: pointer;
    color: var(--ls-secondary-text-color);
    font-size: 0.8em;
    line-height: 1;
    user-select: none;
}
.ls-shell .ls-right-sidebar-card .ls-sidebar-close:hover {
    color: var(--ls-active-primary-color);
}
"#;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LeftPanel {
    Journals,
    AllPages,
    Cards,
    Graph,
}

/// Newtype wrapper so we can pass `Arc<CrdtDoc>` as a Dioxus
/// prop. `CrdtDoc` doesn't impl `PartialEq` (it's a CRDT with
/// internal mutability); we compare by Arc pointer identity,
/// which is sufficient because we hold one Arc per shell session.
#[derive(Clone)]
struct DocHandle(Arc<CrdtDoc>);

impl PartialEq for DocHandle {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct LogseqVault {
    pub pages: Vec<Page>,
    pub blocks: Vec<Block>,
    /// Root directory of the first vault, when one is set.
    /// Lets descendants resolve relative asset URLs (e.g.
    /// `../assets/foo.png`) to a `file://` path the renderer
    /// can actually load. None for ephemeral vaults.
    pub root_path: Option<std::path::PathBuf>,
}

#[component]
pub fn LogseqShell() -> Element {
    let doc: Signal<Arc<CrdtDoc>> = use_signal(|| Arc::new(CrdtDoc::ephemeral()));
    let version: Signal<u64> = use_signal(|| 0u64);
    let active_page: Signal<Option<Uuid>> = use_signal(|| None);
    let panel: Signal<LeftPanel> = use_signal(|| LeftPanel::Journals);

    // Persistence: load any prior snapshot from disk BEFORE
    // we touch the seed, so warm starts skip seeding. Then
    // subscribe to local commits and debounce-save the
    // snapshot back to the same path.
    #[cfg(not(target_arch = "wasm32"))]
    {
        let doc_for_load = doc.read().clone();
        let mut version_for_load = version;
        use_hook(move || {
            let doc = doc_for_load.clone();
            spawn(async move {
                if let Some(bytes) = load_persisted_snapshot().await {
                    if let Err(e) = doc.loro().import(&bytes) {
                        tracing::warn!(?e, "snapshot import failed");
                    } else {
                        version_for_load.with_mut(|v| *v += 1);
                    }
                }
            });
        });

        let doc_for_save = doc.read().clone();
        use_hook(move || {
            let doc = doc_for_save.clone();
            spawn(async move {
                run_persistence_loop(doc).await;
            });
        });

        // Disk-writer loop — mirrors every commit out to
        // `<vault.root_path>/pages/*.md` and
        // `<vault.root_path>/journals/*.md` for vaults that
        // have an on-disk home. Ephemeral vaults (seed-only)
        // skip the write because their root_path is None.
        let doc_for_writer = doc.read().clone();
        use_hook(move || {
            let doc = doc_for_writer.clone();
            spawn(async move {
                crate::graph_writer::run_disk_writer_loop(doc).await;
            });
        });
    }

    // Seed the doc with demo data the first time we render. Same
    // path the live outliner uses — keeps content offline-first.
    let doc_for_seed = doc.read().clone();
    let mut version_for_seed = version;
    use_hook(move || {
        let doc = doc_for_seed.clone();
        spawn(async move {
            // Give persistence a moment to drop in real data.
            #[cfg(not(target_arch = "wasm32"))]
            tokio::time::sleep(std::time::Duration::from_millis(80)).await;
            if !crate::seed::doc_has_pages(&doc).await {
                if let Err(e) = crate::seed::seed_demo(doc.clone()).await {
                    tracing::warn!(?e, "demo seed failed");
                }
                version_for_seed.with_mut(|v| *v += 1);
            }
        });
    });

    // Subscribe to local commits so the shell re-renders when
    // CRDT state changes (typing, indent, etc).
    let doc_for_sub = doc.read().clone();
    let mut version_for_sub = version;
    use_hook(move || {
        let (tx, mut rx) = futures::channel::mpsc::unbounded::<()>();
        let sub = doc_for_sub
            .loro()
            .subscribe_local_update(Box::new(move |_b| {
                let _ = tx.unbounded_send(());
                true
            }));
        std::mem::forget(sub);
        spawn(async move {
            use futures::StreamExt;
            while rx.next().await.is_some() {
                version_for_sub.with_mut(|v| *v += 1);
            }
        });
    });

    // Snapshot — rebuilt on every commit via a Signal. Lists
    // pages + blocks for the active doc. Re-fires whenever
    // `version` ticks (local commit). We use a Signal+spawn here
    // instead of `use_resource` because the resource's `Result`
    // value doesn't impl `PartialEq` on the error path, and we
    // want a plain `LogseqVault` to thread into components.
    let vault_signal: Signal<LogseqVault> = use_signal(LogseqVault::default);
    {
        let doc_for_load = doc.read().clone();
        let mut vault_w = vault_signal;
        use_effect(move || {
            let _v = version.read();
            let doc = doc_for_load.clone();
            spawn(async move {
                if let Ok(v) = load_vault(doc).await {
                    vault_w.set(v);
                }
            });
        });
    }
    let vault_data = vault_signal.read().clone();

    // Resolvers for publish-core renderer.
    let resolvers = build_resolvers(&vault_data);
    use_context_provider(|| resolvers.wiki.clone());
    use_context_provider(|| resolvers.block_refs.clone());
    use_context_provider(|| resolvers.page_embeds.clone());
    use_context_provider(|| resolvers.queries.clone());
    use_context_provider(|| resolvers.namespaces.clone());
    use_context_provider(|| resolvers.properties.clone());
    use_context_provider(|| resolvers.templates.clone());
    use_context_provider(|| resolvers.previews.clone());
    use_context_provider(|| match vault_data.root_path.clone() {
        Some(p) => publish_core::AssetBaseResolver::from_root(p),
        None => publish_core::AssetBaseResolver::default(),
    });

    let mut active_page_w = active_page;
    let mut panel_w = panel;
    let doc_handle = DocHandle(doc.read().clone());
    use_context_provider(|| doc_handle.clone());

    // Editing surface — which block is in edit mode, plus per-
    // block ops (update content, indent, outdent, new sibling,
    // delete). Provided as Dioxus context so any descendant
    // block component can call them without prop-drilling.
    let editing_id: Signal<Option<Uuid>> = use_signal(|| None);
    use_context_provider(|| editing_id);

    // Slash-command palette + page-search popup state. Each is
    // wrapped in a per-popup newtype so Dioxus's type-keyed
    // context map can carry both at once.
    let slash_state: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    let page_search_state: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    let block_ref_state: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    let tag_search_state: Signal<Option<(Uuid, String)>> = use_signal(|| None);
    use_context_provider(|| SlashState(slash_state));
    use_context_provider(|| PageSearchState(page_search_state));
    use_context_provider(|| BlockRefState(block_ref_state));
    use_context_provider(|| TagSearchState(tag_search_state));

    // Right sidebar stack + per-block zoom target.
    let sidebar_stack: Signal<Vec<SidebarEntry>> = use_signal(Vec::new);
    let zoomed_block: Signal<Option<Uuid>> = use_signal(|| None);
    use_context_provider(|| sidebar_stack);
    use_context_provider(|| ZoomState(zoomed_block));

    // In-app navigators so clicks on `[[Page]]` and `((uuid))`
    // change the active page / zoomed block instead of letting
    // the webview try to navigate to a non-existent URL.
    let mut active_page_for_wiki = active_page;
    let wiki_resolver_for_nav = resolvers.wiki.clone();
    let pages_for_nav = vault_data.pages.clone();
    let wiki_nav = publish_core::WikiNavigator(Some(Callback::new(move |slug: String| {
        let target_basename = wiki_resolver_for_nav
            .0
            .iter()
            .find(|(_, s)| **s == slug)
            .map(|(name, _)| name.clone());
        if let Some(name) = target_basename {
            if let Some(p) = pages_for_nav
                .iter()
                .find(|p| p.basename.to_lowercase() == name)
            {
                active_page_for_wiki.set(Some(p.id));
            }
        }
    })));
    use_context_provider(|| wiki_nav);

    let block_refs_for_nav = resolvers.block_refs.clone();
    let pages_for_block_nav = vault_data.pages.clone();
    let mut active_page_for_block = active_page;
    let mut zoom_for_block = zoomed_block;
    let block_ref_nav =
        publish_core::BlockRefNavigator(Some(Callback::new(move |target: Uuid| {
            if let Some(b_target) = block_refs_for_nav.0.get(&target) {
                let slug = b_target.page_slug.clone();
                if let Some(p) = pages_for_block_nav
                    .iter()
                    .find(|p| slugify(&p.basename) == slug)
                {
                    active_page_for_block.set(Some(p.id));
                    zoom_for_block.set(Some(target));
                }
            }
        })));
    use_context_provider(|| block_ref_nav);

    // Tag navigator — clicks on `#tag` set the active tag view.
    let active_tag: Signal<Option<String>> = use_signal(|| None);
    use_context_provider(|| TagViewState(active_tag));
    let mut active_tag_w = active_tag;
    let mut active_page_for_tag = active_page;
    let tag_nav = publish_core::TagNavigator(Some(Callback::new(move |tag: String| {
        active_page_for_tag.set(None);
        active_tag_w.set(Some(tag));
    })));
    use_context_provider(|| tag_nav);

    // BlockOps depends on the sidebar/zoom signals, so it
    // constructs last.
    let ops = make_block_ops(doc.read().clone(), editing_id, sidebar_stack, zoomed_block);
    use_context_provider(|| ops.clone());
    let page_ops = make_page_ops(doc.read().clone(), active_page);
    use_context_provider(|| page_ops.clone());

    // Cmd-K command palette state. `Some(query)` while open.
    let mut cmd_k: Signal<Option<String>> = use_signal(|| None);
    use_context_provider(|| CommandPaletteState(cmd_k));

    // Right-click block context menu position.
    let block_menu: Signal<Option<(Uuid, i32, i32)>> = use_signal(|| None);
    use_context_provider(|| BlockMenuState(block_menu));

    // Auto-select the first page when none is active.
    {
        let pages = vault_data.pages.clone();
        use_effect(move || {
            if active_page_w.peek().is_none() {
                if let Some(p) = pages.first() {
                    active_page_w.set(Some(p.id));
                }
            }
        });
    }

    // Recent pages — newest first, deduped, capped at 8.
    let recent_pages: Signal<Vec<Uuid>> = use_signal(Vec::new);
    {
        let mut recents_w = recent_pages;
        use_effect(move || {
            if let Some(id) = *active_page.read() {
                let mut cur = recents_w.peek().clone();
                cur.retain(|x| *x != id);
                cur.insert(0, id);
                cur.truncate(8);
                recents_w.set(cur);
            }
        });
    }
    use_context_provider(|| RecentsState(recent_pages));

    // Import toast — `Some(message)` while the result of an
    // import is shown to the user.
    let import_toast: Signal<Option<String>> = use_signal(|| None);
    use_context_provider(|| ImportToastState(import_toast));

    rsx! {
        style { dangerous_inner_html: LOGSEQ_CSS }
        div { class: "ls-shell",
            div { class: "ls-header",
                div { class: "ls-app-title", "Task" }
                input {
                    class: "ls-search",
                    r#type: "search",
                    placeholder: "Search… (Ctrl+K)",
                    onfocus: move |_| cmd_k.set(Some(String::new())),
                    oninput: move |e: Event<FormData>| cmd_k.set(Some(e.value())),
                    onkeydown: move |e: Event<KeyboardData>| {
                        if matches!(e.key(), Key::Escape) {
                            cmd_k.set(None);
                        }
                    },
                }
                div { class: "ls-spacer" }
                div { class: "ls-status", "offline" }
            }
            if let Some(q) = cmd_k.read().clone() {
                CommandPalette {
                    query: q,
                    pages: vault_data.pages.clone(),
                    on_pick_page: move |id| {
                        active_page_w.set(Some(id));
                        cmd_k.set(None);
                    },
                }
            }
            div { class: "ls-body",
                LeftSidebar {
                    pages: vault_data.pages.clone(),
                    active_page,
                    panel,
                    on_set_panel: move |p| panel_w.set(p),
                    on_pick_page: move |id| active_page_w.set(Some(id)),
                }
                MainArea {
                    doc: doc_handle.clone(),
                    vault: vault_data.clone(),
                    active_page,
                    panel,
                    on_pick_page: move |id| active_page_w.set(Some(id)),
                    on_set_panel: move |p| panel_w.set(p),
                }
                RightSidebar { stack: sidebar_stack, vault: vault_data.clone() }
            }
            BlockContextMenuOverlay { state: block_menu }
            ImportToastOverlay { state: import_toast }
        }
    }
}

/// Sticky bottom-right card carrying the current import-toast
/// message. Auto-clears 4s after appearance via a spawned timer.
#[component]
fn ImportToastOverlay(state: Signal<Option<String>>) -> Element {
    let snap = state.read().clone();
    let Some(msg) = snap else {
        return rsx! { div {} };
    };
    // Auto-clear after 4s.
    let mut state_for_timer = state;
    use_effect(move || {
        let m = state_for_timer.read().clone();
        if m.is_some() {
            spawn(async move {
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(std::time::Duration::from_secs(4)).await;
                state_for_timer.set(None);
            });
        }
    });
    rsx! {
        div {
            style: "position: fixed; bottom: 1.5em; right: 1.5em; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-active-primary-color); border-radius: 0.5em; padding: 0.7em 1em; color: var(--ls-primary-text-color); box-shadow: 0 12px 30px rgba(0,0,0,0.45); z-index: 80; max-width: 380px;",
            "{msg}"
        }
    }
}

#[component]
fn BlockContextMenuOverlay(state: Signal<Option<(Uuid, i32, i32)>>) -> Element {
    let snap = state.read().clone();
    let Some((block_id, x, y)) = snap else {
        return rsx! { div {} };
    };
    let ops = try_use_context::<BlockOps>();
    let style = format!(
        "position: fixed; top: {y}px; left: {x}px; min-width: 200px; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; box-shadow: 0 12px 30px rgba(0,0,0,0.45); z-index: 70;"
    );
    rsx! {
        // Click-outside catcher: full-screen invisible div that
        // closes the menu when clicked.
        div {
            style: "position: fixed; inset: 0; z-index: 65;",
            onclick: move |_| state.set(None),
        }
        div { style: "{style}",
            onmousedown: move |e: Event<MouseData>| e.stop_propagation(),
            {
                let ops_a = ops.clone();
                let ops_b = ops.clone();
                let ops_c = ops.clone();
                let ops_d = ops.clone();
                rsx! {
                    BlockMenuItem {
                        label: "Open in sidebar",
                        onclick: move |_e: Event<MouseData>| {
                            if let Some(ops) = ops_a.as_ref() { ops.open_in_sidebar.call(block_id); }
                            state.set(None);
                        },
                    }
                    BlockMenuItem {
                        label: "Zoom in",
                        onclick: move |_e: Event<MouseData>| {
                            if let Some(ops) = ops_b.as_ref() { ops.zoom_block.call(block_id); }
                            state.set(None);
                        },
                    }
                    BlockMenuItem {
                        label: "Copy block reference",
                        onclick: move |_e: Event<MouseData>| {
                            let id = block_id.simple().to_string();
                            let script = format!(
                                r#"navigator.clipboard.writeText("(({}))");"#, id
                            );
                            spawn(async move { let _ = document::eval(&script).recv::<serde_json::Value>().await; });
                            state.set(None);
                        },
                    }
                    BlockMenuItem {
                        label: "Toggle collapse",
                        onclick: move |_e: Event<MouseData>| {
                            if let Some(ops) = ops_c.as_ref() { ops.toggle_collapsed.call(block_id); }
                            state.set(None);
                        },
                    }
                    BlockMenuItem {
                        label: "Delete block",
                        onclick: move |_e: Event<MouseData>| {
                            if let Some(ops) = ops_d.as_ref() { ops.delete_block.call(block_id); }
                            state.set(None);
                        },
                    }
                }
            }
        }
    }
}

#[component]
fn BlockMenuItem(label: &'static str, onclick: EventHandler<Event<MouseData>>) -> Element {
    rsx! {
        div {
            style: "padding: 0.4em 0.75em; cursor: pointer; color: var(--ls-primary-text-color);",
            onclick: move |e| onclick.call(e),
            "{label}"
        }
    }
}

#[derive(Clone, Default)]
struct ResolverBundle {
    wiki: WikiResolver,
    block_refs: BlockRefResolver,
    page_embeds: PageEmbedResolver,
    queries: QueryResolver,
    namespaces: NamespaceResolver,
    properties: publish_core::PagePropertyResolver,
    templates: publish_core::TemplateResolver,
    previews: publish_core::WikiPreviewResolver,
}

fn build_resolvers(vault: &LogseqVault) -> ResolverBundle {
    use std::collections::HashMap;
    let mut basename_to_slug = HashMap::new();
    let mut id_to_slug = HashMap::new();
    let mut id_to_basename = HashMap::new();
    for p in &vault.pages {
        let slug = slugify(&p.basename);
        basename_to_slug.insert(p.basename.to_lowercase(), slug.clone());
        for alias in &p.aliases {
            basename_to_slug
                .entry(alias.to_lowercase())
                .or_insert_with(|| slug.clone());
        }
        id_to_slug.insert(p.id, slug);
        id_to_basename.insert(p.id, p.basename.clone());
    }

    let mut block_map: HashMap<Uuid, BlockRefTarget> = HashMap::new();
    let mut embed_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut tag_map: HashMap<String, Vec<QueryHit>> = HashMap::new();
    let mut ns_map: HashMap<String, Vec<QueryHit>> = HashMap::new();

    let mut blocks_by_page: HashMap<Uuid, Vec<Block>> = HashMap::new();
    for b in &vault.blocks {
        blocks_by_page.entry(b.page_id).or_default().push(b.clone());
        if let Some(slug) = id_to_slug.get(&b.page_id) {
            block_map.insert(
                b.id,
                BlockRefTarget {
                    page_slug: slug.clone(),
                    snippet: snippet(&b.content, 80),
                    content: b.content.clone(),
                },
            );
        }
    }
    for (page_id, bs) in &mut blocks_by_page {
        bs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        let Some(p) = vault.pages.iter().find(|p| p.id == *page_id) else {
            continue;
        };
        let contents: Vec<String> = bs.iter().take(50).map(|b| b.content.clone()).collect();
        embed_map.insert(p.basename.to_lowercase(), contents);
    }

    // Tag index (best-effort scan)
    for b in &vault.blocks {
        let Some(page) = vault.pages.iter().find(|p| p.id == b.page_id) else {
            continue;
        };
        for tag in extract_tags(&b.content) {
            tag_map
                .entry(tag.to_lowercase())
                .or_default()
                .push(QueryHit {
                    slug: slugify(&page.basename),
                    title: page.basename.clone(),
                });
        }
    }
    // Dedupe + sort tag hits.
    for hits in tag_map.values_mut() {
        hits.sort_by(|a, b| a.slug.cmp(&b.slug));
        hits.dedup_by(|a, b| a.slug == b.slug);
    }

    // Namespace index
    for p in &vault.pages {
        if !p.basename.contains('/') {
            continue;
        }
        let parts: Vec<&str> = p.basename.split('/').collect();
        let slug = slugify(&p.basename);
        for d in 1..parts.len() {
            let prefix = parts[..d].join("/").to_lowercase();
            ns_map.entry(prefix).or_default().push(QueryHit {
                slug: slug.clone(),
                title: p.basename.clone(),
            });
        }
    }
    for hits in ns_map.values_mut() {
        hits.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
    }

    // Page-property index: walk every page's frontmatter and bucket
    // (key, value) → [hit]. Scalars index as-is; arrays index each
    // element so `(property tags rust)` matches a page with
    // `tags: [rust, web]`.
    let mut prop_map: HashMap<String, HashMap<String, Vec<QueryHit>>> = HashMap::new();
    for p in &vault.pages {
        let Ok(val) = serde_json::from_str::<serde_json::Value>(&p.frontmatter_json) else {
            continue;
        };
        let Some(obj) = val.as_object() else { continue };
        let hit = QueryHit {
            slug: slugify(&p.basename),
            title: p.basename.clone(),
        };
        for (k, v) in obj {
            let key = k.to_lowercase();
            let mut push = |value: String| {
                prop_map
                    .entry(key.clone())
                    .or_default()
                    .entry(value.to_lowercase())
                    .or_default()
                    .push(hit.clone());
            };
            match v {
                serde_json::Value::String(s) => push(s.clone()),
                serde_json::Value::Bool(b) => push(b.to_string()),
                serde_json::Value::Number(n) => push(n.to_string()),
                serde_json::Value::Array(arr) => {
                    for item in arr {
                        if let Some(s) = item.as_str() {
                            push(s.to_string());
                        } else {
                            push(item.to_string());
                        }
                    }
                }
                _ => {}
            }
        }
    }
    for by_val in prop_map.values_mut() {
        for hits in by_val.values_mut() {
            hits.sort_by(|a, b| a.slug.cmp(&b.slug));
            hits.dedup_by(|a, b| a.slug == b.slug);
        }
    }

    // Template index: blocks tagged with `template:: <name>` are
    // available as `{{template <name>}}`. The body is the block's
    // own content + each direct child block's content, in order.
    let mut tmpl_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut blocks_by_parent: HashMap<Option<Uuid>, Vec<&Block>> = HashMap::new();
    for b in &vault.blocks {
        blocks_by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b);
    }
    for siblings in blocks_by_parent.values_mut() {
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    for b in &vault.blocks {
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&b.properties_json) {
            if let Some(name) = v.get("template").and_then(|x| x.as_str()) {
                let mut body = vec![b.content.clone()];
                if let Some(children) = blocks_by_parent.get(&Some(b.id)) {
                    body.extend(children.iter().map(|c| c.content.clone()));
                }
                tmpl_map.insert(name.to_lowercase(), body);
            }
        }
    }

    ResolverBundle {
        wiki: WikiResolver(Arc::new(basename_to_slug)),
        block_refs: BlockRefResolver(Arc::new(block_map)),
        page_embeds: PageEmbedResolver(Arc::new(embed_map)),
        queries: QueryResolver(Arc::new(tag_map)),
        namespaces: NamespaceResolver(Arc::new(ns_map)),
        properties: publish_core::PagePropertyResolver(Arc::new(prop_map)),
        templates: publish_core::TemplateResolver(Arc::new(tmpl_map)),
        previews: {
            let mut by_slug: HashMap<String, String> = HashMap::new();
            for (page_id, bs) in &blocks_by_page {
                if let Some(p) = vault.pages.iter().find(|p| p.id == *page_id) {
                    let preview = bs
                        .iter()
                        .take(3)
                        .map(|b| b.content.lines().next().unwrap_or("").trim().to_string())
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>()
                        .join(" · ");
                    let truncated: String = preview.chars().take(140).collect();
                    if !truncated.is_empty() {
                        by_slug.insert(slugify(&p.basename), truncated);
                    }
                }
            }
            publish_core::WikiPreviewResolver(Arc::new(by_slug))
        },
    }
}

/// Parse a markdown pipe-table block. Recognized shape:
///
/// ```text
/// | a | b |
/// |---|---|
/// | 1 | 2 |
/// ```
///
/// Returns the full row list (header + body) or `None` when the
/// content doesn't match. The separator row (`|---|---|`) is
/// consumed and not included in the output.
/// One row in a LOGBOOK drawer. `end` is `None` while the clock
/// is still running.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct LogbookEntry {
    pub start: chrono::NaiveDateTime,
    pub end: Option<chrono::NaiveDateTime>,
}

/// Parse a LOGBOOK drawer body. Recognized line shape (Logseq /
/// Org Mode):
///
/// ```text
/// CLOCK: [2026-05-19 Tue 10:30:00]--[2026-05-19 Tue 11:15:00] => 0:45
/// CLOCK: [2026-05-19 Tue 14:00:00]
/// ```
///
/// Lines that don't match are skipped silently — keeps the parser
/// forgiving for hand-edited drawers.
pub(crate) fn parse_logbook(body: &str) -> Vec<LogbookEntry> {
    let mut out = Vec::new();
    for line in body.lines() {
        let line = line.trim();
        let Some(rest) = line.strip_prefix("CLOCK:") else {
            continue;
        };
        let rest = rest.trim();
        let Some(start_open) = rest.find('[') else {
            continue;
        };
        let Some(start_close) = rest[start_open..].find(']') else {
            continue;
        };
        let start_str = &rest[start_open + 1..start_open + start_close];
        let Some(start) = parse_logbook_ts(start_str) else {
            continue;
        };
        let after_start = &rest[start_open + start_close + 1..];
        let end = after_start
            .find('[')
            .and_then(|o| after_start[o..].find(']').map(|c| (o, c)))
            .and_then(|(o, c)| parse_logbook_ts(&after_start[o + 1..o + c]));
        out.push(LogbookEntry { start, end });
    }
    out
}

fn parse_logbook_ts(s: &str) -> Option<chrono::NaiveDateTime> {
    // Logseq shape: `2026-05-19 Tue 10:30:00` — strip the optional
    // weekday before parsing.
    let parts: Vec<&str> = s.split_whitespace().collect();
    let normalized = match parts.as_slice() {
        [date, _wkd, time] => format!("{date} {time}"),
        [date, time] => format!("{date} {time}"),
        _ => return None,
    };
    chrono::NaiveDateTime::parse_from_str(&normalized, "%Y-%m-%d %H:%M:%S").ok()
}

fn format_duration(secs: i64) -> String {
    let h = secs / 3600;
    let m = (secs % 3600) / 60;
    let s = secs % 60;
    if h > 0 {
        format!("{h}:{m:02}:{s:02}")
    } else {
        format!("{m:02}:{s:02}")
    }
}

#[component]
fn LogbookView(body: String) -> Element {
    let entries = parse_logbook(&body);
    // Tick once a second while there's an open entry, so the live
    // duration on the running clock updates. When everything is
    // closed, the signal stays at zero and we don't burn cycles.
    let has_open = entries.iter().any(|e| e.end.is_none());
    let mut tick: Signal<u64> = use_signal(|| 0);
    use_effect(move || {
        if !has_open {
            return;
        }
        spawn(async move {
            loop {
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
                #[cfg(target_arch = "wasm32")]
                gloo_timers::future::TimeoutFuture::new(1000).await;
                let cur = *tick.peek();
                tick.set(cur.wrapping_add(1));
            }
        });
    });
    let _ = *tick.read();

    let total_closed: i64 = entries
        .iter()
        .filter_map(|e| e.end.map(|end| (end - e.start).num_seconds()))
        .sum();
    let open_secs: Option<i64> = entries.iter().find(|e| e.end.is_none()).map(|e| {
        (chrono::Local::now().naive_local() - e.start)
            .num_seconds()
            .max(0)
    });
    let total_secs = total_closed + open_secs.unwrap_or(0);
    rsx! {
        div {
            class: "ls-logbook",
            style: "margin: 0.4em 0; border: 1px solid var(--ls-border-color); border-radius: 4px; padding: 0.5em 0.7em; background: var(--ls-secondary-background-color); font-size: 0.85rem;",
            div { style: "display: flex; align-items: baseline; gap: 0.6em;",
                span { style: "font-weight: 600; color: var(--ls-secondary-text-color); text-transform: uppercase; letter-spacing: 0.08em;",
                    "Logbook"
                }
                if let Some(s) = open_secs {
                    span { style: "color: var(--ls-active-primary-color); font-variant-numeric: tabular-nums;",
                        "● running · {format_duration(s)}"
                    }
                }
                span { style: "margin-left: auto; color: var(--ls-secondary-text-color); font-variant-numeric: tabular-nums;",
                    "total {format_duration(total_secs)}"
                }
            }
            if entries.is_empty() {
                div { style: "color: var(--ls-secondary-text-color); font-style: italic; margin-top: 0.3em;",
                    "(no clock entries)"
                }
            } else {
                ul { style: "list-style: none; padding: 0; margin: 0.3em 0 0; display: flex; flex-direction: column; gap: 0.15em;",
                    for (i, e) in entries.into_iter().enumerate() {
                        {
                            let start_str = e.start.format("%Y-%m-%d %H:%M").to_string();
                            let end_str = e.end.map(|d| d.format("%H:%M").to_string());
                            let dur_str = match e.end {
                                Some(end) => format_duration((end - e.start).num_seconds()),
                                None => format_duration((chrono::Local::now().naive_local() - e.start).num_seconds().max(0)),
                            };
                            rsx! {
                                li { key: "{i}",
                                    style: "display: grid; grid-template-columns: 1fr auto; gap: 0.5em; color: var(--ls-secondary-text-color); font-variant-numeric: tabular-nums;",
                                    span {
                                        "{start_str}"
                                        if let Some(es) = end_str.clone() {
                                            " → {es}"
                                        } else {
                                            " → running"
                                        }
                                    }
                                    span { "{dur_str}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn peel_table(content: &str) -> Option<Vec<Vec<String>>> {
    let trimmed = content.trim();
    let lines: Vec<&str> = trimmed.lines().collect();
    if lines.len() < 2 {
        return None;
    }
    if !lines[0].trim_start().starts_with('|') {
        return None;
    }
    // Separator row: every non-pipe char must be `-`, `:`, or
    // whitespace, and at least one `-` must appear.
    let sep = lines[1].trim();
    if !sep.starts_with('|') {
        return None;
    }
    let sep_chars: &str = sep.trim_matches('|');
    if !sep_chars.contains('-') {
        return None;
    }
    if !sep_chars
        .chars()
        .all(|c| c == '-' || c == ':' || c == '|' || c.is_whitespace())
    {
        return None;
    }
    let split_row = |line: &str| -> Vec<String> {
        let inner = line.trim().trim_start_matches('|').trim_end_matches('|');
        inner.split('|').map(|s| s.trim().to_string()).collect()
    };
    let mut rows = vec![split_row(lines[0])];
    for l in lines.iter().skip(2) {
        if l.trim().is_empty() {
            continue;
        }
        if !l.trim_start().starts_with('|') {
            return None;
        }
        rows.push(split_row(l));
    }
    Some(rows)
}

/// Peel a fenced code block off a block's content. Recognizes
/// the standard markdown ```lang ... ``` form (also ``` with no
/// language). Returns `Some((lang, body))` only when the *entire
/// trimmed content* is one fenced block — partial matches fall
/// through to inline parsing.
fn peel_fenced_code(content: &str) -> Option<(String, String)> {
    let trimmed = content.trim();
    let body = trimmed.strip_prefix("```")?;
    let body = body.strip_suffix("```")?;
    let (first, rest) = body.split_once('\n')?;
    let lang = first.trim().to_string();
    // Drop the trailing newline before the closing fence if present.
    let code = rest.strip_suffix('\n').unwrap_or(rest).to_string();
    Some((lang, code))
}

fn snippet(s: &str, max: usize) -> String {
    let joined: String = s.split_whitespace().collect::<Vec<_>>().join(" ");
    if joined.chars().count() > max {
        joined.chars().take(max).collect::<String>() + "…"
    } else if joined.is_empty() {
        "…".into()
    } else {
        joined
    }
}

fn extract_tags(s: &str) -> Vec<String> {
    let mut tags = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'#' && (i == 0 || bytes[i - 1].is_ascii_whitespace()) {
            let start = i + 1;
            let mut end = start;
            while end < bytes.len()
                && (bytes[end].is_ascii_alphanumeric()
                    || bytes[end] == b'-'
                    || bytes[end] == b'_'
                    || bytes[end] == b'/')
            {
                end += 1;
            }
            if end > start {
                tags.push(s[start..end].to_string());
            }
            i = end;
        } else {
            i += 1;
        }
    }
    tags
}

async fn load_vault(doc: Arc<CrdtDoc>) -> Result<LogseqVault, String> {
    use knowledge_proto::VaultRepo;
    let pr = PageRepoLoro::new(&doc);
    let br = BlockRepoLoro::new(&doc);
    let vr = knowledge_crdt::VaultRepoLoro::new(&doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let pages = pr
        .list(big.clone(), None, None)
        .await
        .map_err(|e| format!("page list: {e}"))?
        .items;
    let blocks = br
        .list(big.clone(), None, None)
        .await
        .map_err(|e| format!("block list: {e}"))?
        .items;
    let root_path = vr
        .list(ListPage { index: 0, size: 1 }, None, None)
        .await
        .ok()
        .and_then(|p| p.items.into_iter().next())
        .and_then(|v| v.root_path.map(std::path::PathBuf::from));
    Ok(LogseqVault {
        pages,
        blocks,
        root_path,
    })
}

#[component]
fn LeftSidebar(
    pages: Vec<Page>,
    active_page: Signal<Option<Uuid>>,
    panel: Signal<LeftPanel>,
    on_set_panel: EventHandler<LeftPanel>,
    on_pick_page: EventHandler<Uuid>,
) -> Element {
    let mut sorted = pages.clone();
    sorted.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    let active = *active_page.read();
    let panel_cur = *panel.read();

    rsx! {
        nav { class: "ls-left-sidebar",
            div {
                class: if panel_cur == LeftPanel::Journals { "ls-nav-item active" } else { "ls-nav-item" },
                onclick: move |_| on_set_panel.call(LeftPanel::Journals),
                span { class: "ls-nav-icon", "◐" }
                "Journals"
            }
            div {
                class: if panel_cur == LeftPanel::AllPages { "ls-nav-item active" } else { "ls-nav-item" },
                onclick: move |_| on_set_panel.call(LeftPanel::AllPages),
                span { class: "ls-nav-icon", "▤" }
                "All Pages"
            }
            div {
                class: if panel_cur == LeftPanel::Graph { "ls-nav-item active" } else { "ls-nav-item" },
                onclick: move |_| on_set_panel.call(LeftPanel::Graph),
                span { class: "ls-nav-icon", "◇" }
                "Graph"
            }
            div {
                class: if panel_cur == LeftPanel::Cards { "ls-nav-item active" } else { "ls-nav-item" },
                onclick: move |_| on_set_panel.call(LeftPanel::Cards),
                span { class: "ls-nav-icon", "♠" }
                "Cards"
            }
            {
                let recents = try_use_context::<RecentsState>().map(|r| r.0.read().clone()).unwrap_or_default();
                if !recents.is_empty() {
                    rsx! {
                        div { class: "ls-sidebar-section", "Recent" }
                        div { class: "ls-page-list",
                            for id in recents {
                                {
                                    let p = pages.iter().find(|p| p.id == id).cloned();
                                    if let Some(p) = p {
                                        let pid = p.id;
                                        let is_active = active == Some(pid);
                                        let cls = if is_active { "ls-page-link active" } else { "ls-page-link" };
                                        rsx! {
                                            a {
                                                key: "{pid}",
                                                class: "{cls}",
                                                onclick: move |_| on_pick_page.call(pid),
                                                "{p.basename}"
                                            }
                                        }
                                    } else { rsx! {} }
                                }
                            }
                        }
                    }
                } else {
                    rsx! {}
                }
            }
            div { class: "ls-sidebar-section",
                style: "display: flex; justify-content: space-between; align-items: center;",
                span { "Pages" }
                button {
                    style: "background: transparent; border: 0; color: var(--ls-secondary-text-color); cursor: pointer; font-size: 0.9rem; padding: 0 0.4em;",
                    title: "New page",
                    onclick: move |_| {
                        if let Some(ops) = try_use_context::<PageOps>() {
                            ops.create_page.call(format!("Untitled {}", chrono::Local::now().format("%H:%M:%S")));
                        }
                    },
                    "＋"
                }
            }
            NamespaceTree {
                pages: sorted.clone(),
                active,
                on_pick_page,
            }
            div { style: "margin-top: auto; padding: 0.75em 1em; border-top: 1px solid var(--ls-border-color);",
                ImportGraphButton {}
            }
        }
    }
}

/// Collapsible namespace tree for the page sidebar. Pages whose
/// basename contains `/` become nested under parent nodes, matching
/// Logseq's hierarchy convention (`projects/web/auth` ⇒ a triply
/// nested branch). Leaves with a real Page id are clickable;
/// intermediate folders that don't have their own page are inert
/// labels.
#[component]
fn NamespaceTree(
    pages: Vec<Page>,
    active: Option<Uuid>,
    on_pick_page: EventHandler<Uuid>,
) -> Element {
    use std::collections::BTreeMap;
    #[derive(Default)]
    struct BuildNode {
        page_id: Option<Uuid>,
        children: BTreeMap<String, BuildNode>,
    }
    fn to_child(name: &str, node: BuildNode) -> NsChild {
        NsChild {
            name: name.to_string(),
            page_id: node.page_id,
            children: node
                .children
                .into_iter()
                .map(|(n, c)| to_child(&n, c))
                .collect(),
        }
    }
    let mut root = BuildNode::default();
    for p in &pages {
        let mut cur = &mut root;
        let parts: Vec<&str> = p.basename.split('/').collect();
        for part in &parts {
            cur = cur.children.entry(part.to_string()).or_default();
        }
        cur.page_id = Some(p.id);
    }
    let top: Vec<NsChild> = root
        .children
        .into_iter()
        .map(|(n, c)| to_child(&n, c))
        .collect();
    rsx! {
        div { class: "ls-page-list",
            for child in top {
                NamespaceNode {
                    key: "{child.name}",
                    name: child.name.clone(),
                    page_id: child.page_id,
                    children_data: child.children.clone(),
                    active,
                    on_pick_page,
                    depth: 0,
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
struct NsChild {
    name: String,
    page_id: Option<Uuid>,
    children: Vec<NsChild>,
}

#[component]
fn NamespaceNode(
    name: String,
    page_id: Option<Uuid>,
    children_data: Vec<NsChild>,
    active: Option<Uuid>,
    on_pick_page: EventHandler<Uuid>,
    depth: usize,
) -> Element {
    let mut expanded: Signal<bool> = use_signal(|| depth < 1);
    let has_children = !children_data.is_empty();
    let is_active = page_id.is_some() && page_id == active;
    let cls = if is_active {
        "ls-page-link active"
    } else {
        "ls-page-link"
    };
    let indent = (depth * 12) as i32 + 8;
    rsx! {
        div { style: "display: flex; align-items: center; gap: 0.25em; padding-left: {indent}px;",
            if has_children {
                button {
                    style: "background: transparent; border: 0; color: var(--ls-secondary-text-color); cursor: pointer; padding: 0; width: 1em; text-align: center;",
                    onclick: move |_| {
                        let cur = *expanded.peek();
                        expanded.set(!cur);
                    },
                    if *expanded.read() { "▾" } else { "▸" }
                }
            } else {
                span { style: "width: 1em;" }
            }
            if let Some(pid) = page_id {
                a {
                    class: "{cls}",
                    style: "padding: 0.2em 0.4em; flex: 1; min-width: 0;",
                    onclick: move |_| on_pick_page.call(pid),
                    "{name}"
                }
            } else {
                span {
                    style: "padding: 0.2em 0.4em; flex: 1; min-width: 0; color: var(--ls-secondary-text-color);",
                    "{name}"
                }
            }
        }
        if has_children && *expanded.read() {
            for child in children_data {
                NamespaceNode {
                    key: "{child.name}",
                    name: child.name.clone(),
                    page_id: child.page_id,
                    children_data: child.children.clone(),
                    active,
                    on_pick_page,
                    depth: depth + 1,
                }
            }
        }
    }
}

/// Native directory picker → graph_loader::import_logseq_graph.
/// On wasm targets this renders as a disabled placeholder
/// (browser sandbox doesn't allow folder-level access without a
/// File System Access permission flow we haven't wired yet).
#[component]
fn ImportGraphButton() -> Element {
    let doc = try_use_context::<DocHandle>();
    let toast = try_use_context::<ImportToastState>();
    let on_click = move |_e: Event<MouseData>| {
        let Some(doc) = doc.clone() else { return };
        let mut toast_sig = toast.map(|t| t.0);
        #[cfg(not(target_arch = "wasm32"))]
        spawn(async move {
            let dir = match rfd::AsyncFileDialog::new()
                .set_title("Pick a Logseq graph directory")
                .pick_folder()
                .await
            {
                Some(handle) => handle.path().to_path_buf(),
                None => return,
            };
            match crate::graph_loader::import_logseq_graph(&doc.0, &dir).await {
                Ok(stats) => {
                    let msg = format!(
                        "Imported {} pages · {} blocks · {} journals{}",
                        stats.pages,
                        stats.blocks,
                        stats.journals,
                        if stats.failures.is_empty() {
                            String::new()
                        } else {
                            format!(" · {} failures", stats.failures.len())
                        }
                    );
                    if let Some(ref mut s) = toast_sig.as_mut() {
                        s.set(Some(msg));
                    }
                }
                Err(e) => {
                    if let Some(ref mut s) = toast_sig.as_mut() {
                        s.set(Some(format!("Import failed: {e}")));
                    }
                }
            }
        });
        #[cfg(target_arch = "wasm32")]
        {
            let _ = (doc, toast_sig);
        }
    };
    rsx! {
        button {
            style: "width: 100%; background: var(--ls-tertiary-background-color); color: var(--ls-primary-text-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; padding: 0.4em 0.6em; cursor: pointer; font-size: 0.85rem;",
            onclick: on_click,
            "Import Logseq graph…"
        }
    }
}

#[component]
fn MainArea(
    doc: DocHandle,
    vault: LogseqVault,
    active_page: Signal<Option<Uuid>>,
    panel: Signal<LeftPanel>,
    on_pick_page: EventHandler<Uuid>,
    on_set_panel: EventHandler<LeftPanel>,
) -> Element {
    let _ = on_set_panel;
    let active = *active_page.read();
    let panel_cur = *panel.read();
    let tag_state = try_use_context::<TagViewState>();
    let active_tag = tag_state.as_ref().and_then(|s| s.0.read().clone());

    // Tag view supersedes the panel routing when set.
    if let Some(tag) = active_tag {
        return rsx! {
            main { class: "ls-main",
                div { class: "ls-main-inner",
                    TagView {
                        tag: tag,
                        vault: vault.clone(),
                        on_pick_page,
                    }
                }
            }
        };
    }

    match panel_cur {
        LeftPanel::Cards => rsx! {
            main { class: "ls-main",
                div { class: "ls-main-inner",
                    CardsReview { vault: vault.clone(), on_pick_page }
                }
            }
        },
        LeftPanel::Graph => rsx! {
            main { class: "ls-main",
                div { class: "ls-main-inner",
                    GraphView { vault: vault.clone(), on_pick_page }
                }
            }
        },
        LeftPanel::AllPages => rsx! {
            main { class: "ls-main",
                div { class: "ls-main-inner",
                    h1 { class: "ls-page-title", "All Pages" }
                    AllPagesGrid { vault: vault.clone(), on_pick_page }
                }
            }
        },
        LeftPanel::Journals => rsx! {
            main { class: "ls-main",
                div { class: "ls-main-inner",
                    if let Some(id) = active {
                        if let Some(page) = vault.pages.iter().find(|p| p.id == id).cloned() {
                            PageView {
                                doc: doc.clone(),
                                page: page.clone(),
                                blocks: vault.blocks.iter().filter(|b| b.page_id == id).cloned().collect(),
                                vault: vault.clone(),
                                on_pick_page,
                            }
                        } else {
                            div { class: "ls-block-empty", "Page not found." }
                        }
                    } else {
                        div { class: "ls-block-empty", "Pick a page from the sidebar." }
                    }
                }
            }
        },
    }
}

/// Virtual page listing every block whose content contains `#<tag>`
/// (with `#` at word boundary). Rendered when a `Hashtag` is
/// clicked anywhere in the shell. Clicking a hit jumps to that
/// page; the close button clears the tag view and returns to the
/// previous page.
#[component]
fn TagView(tag: String, vault: LogseqVault, on_pick_page: EventHandler<Uuid>) -> Element {
    let needle = format!("#{}", tag.to_lowercase());
    let mut hits: Vec<(Uuid, String, String)> = Vec::new();
    for b in &vault.blocks {
        for t in extract_tags(&b.content) {
            if t.eq_ignore_ascii_case(&tag) {
                let page_name = vault
                    .pages
                    .iter()
                    .find(|p| p.id == b.page_id)
                    .map(|p| p.basename.clone())
                    .unwrap_or_else(|| "(unknown)".into());
                let snippet = b.content.lines().next().unwrap_or("").to_string();
                hits.push((b.page_id, page_name, snippet));
                break;
            }
        }
    }
    let _ = needle;
    let count = hits.len();
    let count_label = if count == 1 {
        format!("{count} reference")
    } else {
        format!("{count} references")
    };
    let tag_state = try_use_context::<TagViewState>();
    let mut clear = move || {
        if let Some(s) = tag_state.as_ref() {
            s.0.clone().set(None);
        }
    };
    rsx! {
        div { style: "display: flex; align-items: baseline; gap: 0.6em;",
            h1 { class: "ls-page-title", "#{tag}" }
            span { style: "color: var(--ls-secondary-text-color); font-size: 0.85rem;",
                "{count_label}"
            }
            button {
                style: "margin-left: auto; background: transparent; border: 1px solid var(--ls-border-color); color: var(--ls-secondary-text-color); padding: 0.2em 0.6em; border-radius: 4px; cursor: pointer;",
                onclick: move |_| clear(),
                "Close"
            }
        }
        if hits.is_empty() {
            div { class: "ls-block-empty", "No blocks reference this tag." }
        } else {
            div { style: "margin-top: 1em; display: flex; flex-direction: column; gap: 0.6em;",
                for (i, (page_id, page_name, snippet)) in hits.into_iter().enumerate() {
                    div { key: "{i}",
                        style: "padding: 0.5em 0.7em; border: 1px solid var(--ls-border-color); border-radius: 4px; cursor: pointer;",
                        onclick: move |_| {
                            if let Some(s) = tag_state.as_ref() { s.0.clone().set(None); }
                            on_pick_page.call(page_id);
                        },
                        div { style: "color: var(--ls-link-text-color); font-weight: 600; font-size: 0.85rem;",
                            "{page_name}"
                        }
                        div { style: "color: var(--ls-secondary-text-color); font-size: 0.85rem; margin-top: 0.2em;",
                            "{snippet}"
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn CardsReview(vault: LogseqVault, on_pick_page: EventHandler<Uuid>) -> Element {
    // A "card" is any block whose content references `#card`. The
    // front is the block content (with `#card` stripped); the back
    // is the direct-child blocks' content. Shuffle order each entry
    // so reviews don't fall into a fixed pattern.
    let mut deck: Vec<(Uuid, String, Vec<String>, Uuid)> = Vec::new();
    let mut children_by_parent: std::collections::HashMap<Uuid, Vec<&Block>> =
        std::collections::HashMap::new();
    for b in &vault.blocks {
        if let Some(p) = b.parent_block_id {
            children_by_parent.entry(p).or_default().push(b);
        }
    }
    for siblings in children_by_parent.values_mut() {
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    for b in &vault.blocks {
        let needle = "#card";
        let lower = b.content.to_lowercase();
        let has = lower
            .split_whitespace()
            .any(|w| w == needle || w.starts_with("#card/"));
        if !has {
            continue;
        }
        let front = b
            .content
            .lines()
            .map(|l| {
                l.split_whitespace()
                    .filter(|w| !w.eq_ignore_ascii_case("#card"))
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<Vec<_>>()
            .join("\n");
        let back = children_by_parent
            .get(&b.id)
            .map(|cs| cs.iter().map(|c| c.content.clone()).collect())
            .unwrap_or_default();
        deck.push((b.id, front, back, b.page_id));
    }

    let total = deck.len();
    let total_label = if total == 1 {
        "1 card".to_string()
    } else {
        format!("{total} cards")
    };
    let mut idx: Signal<usize> = use_signal(|| 0);
    let mut flipped: Signal<bool> = use_signal(|| false);
    let cur_idx = (*idx.read()).min(total.saturating_sub(1));
    let card = deck.get(cur_idx).cloned();
    let is_flipped = *flipped.read();

    rsx! {
        div { style: "display: flex; align-items: baseline; gap: 0.6em; margin-bottom: 1em;",
            h1 { class: "ls-page-title", "Cards" }
            span { style: "color: var(--ls-secondary-text-color); font-size: 0.85rem;",
                "{total_label}"
            }
        }
        if let Some((bid, front, back, page_id)) = card {
            div { style: "max-width: 540px; margin: 0 auto; border: 1px solid var(--ls-border-color); border-radius: 8px; background: var(--ls-secondary-background-color); padding: 1.2em 1.4em;",
                div { style: "color: var(--ls-secondary-text-color); font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.1em;",
                    "{cur_idx + 1} / {total}"
                }
                div { style: "font-size: 1.1rem; margin-top: 0.6em; white-space: pre-wrap;",
                    "{front}"
                }
                if is_flipped {
                    div { style: "margin-top: 1em; padding-top: 0.8em; border-top: 1px dashed var(--ls-border-color); white-space: pre-wrap; color: var(--ls-secondary-text-color);",
                        for (i, line) in back.iter().enumerate() {
                            div { key: "{i}", "{line}" }
                        }
                    }
                } else {
                    button {
                        style: "margin-top: 1em; padding: 0.4em 0.9em; border-radius: 4px; border: 1px solid var(--ls-border-color); background: var(--ls-tertiary-background-color); color: var(--ls-primary-text-color); cursor: pointer;",
                        onclick: move |_| flipped.set(true),
                        "Flip"
                    }
                }
                div { style: "display: flex; gap: 0.4em; margin-top: 1em; flex-wrap: wrap;",
                    if is_flipped {
                        button {
                            style: "padding: 0.4em 0.9em; background: #5b5d72; color: white; border: 0; border-radius: 4px; cursor: pointer;",
                            onclick: move |_| {
                                flipped.set(false);
                                let n = *idx.peek();
                                idx.set((n + 1) % total.max(1));
                            },
                            "Again"
                        }
                        button {
                            style: "padding: 0.4em 0.9em; background: #2f7d4f; color: white; border: 0; border-radius: 4px; cursor: pointer;",
                            onclick: move |_| {
                                flipped.set(false);
                                let n = *idx.peek();
                                idx.set((n + 1) % total.max(1));
                            },
                            "Good"
                        }
                    }
                    button {
                        style: "padding: 0.4em 0.9em; background: transparent; border: 1px solid var(--ls-border-color); border-radius: 4px; cursor: pointer; color: var(--ls-secondary-text-color); margin-left: auto;",
                        onclick: move |_| {
                            let _ = bid;
                            on_pick_page.call(page_id);
                        },
                        "Open page"
                    }
                }
            }
        } else {
            div { class: "ls-block-empty",
                style: "text-align: center; padding: 2em;",
                "No cards yet. Add ", code { "#card" }, " to a block to make it reviewable."
            }
        }
    }
}

/// Force-directed-ish graph view. We don't run a real simulation —
/// pages are placed on a circle, edges drawn as SVG lines. For
/// small/medium graphs (couple hundred pages) this is more than
/// enough to spot clusters by clicking around; for huge vaults the
/// pages list is the better UX anyway.
#[component]
fn GraphView(vault: LogseqVault, on_pick_page: EventHandler<Uuid>) -> Element {
    let mut pages: Vec<Page> = vault.pages.clone();
    pages.sort_by(|a, b| a.basename.cmp(&b.basename));
    let n = pages.len().max(1) as f32;
    let cx = 400.0_f32;
    let cy = 300.0_f32;
    let r = 230.0_f32;
    let positions: std::collections::HashMap<Uuid, (f32, f32)> = pages
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let theta = (i as f32) / n * std::f32::consts::TAU;
            (p.id, (cx + r * theta.cos(), cy + r * theta.sin()))
        })
        .collect();

    // Edges: every wikilink target the block content references.
    // We do a cheap substring scan since the parser results aren't
    // cached for raw block content in this view.
    let basename_lower: std::collections::HashMap<String, Uuid> = pages
        .iter()
        .map(|p| (p.basename.to_lowercase(), p.id))
        .collect();
    let mut edges: Vec<(Uuid, Uuid)> = Vec::new();
    for b in &vault.blocks {
        let mut s = b.content.as_str();
        while let Some(open) = s.find("[[") {
            s = &s[open + 2..];
            let Some(close) = s.find("]]") else { break };
            let target = s[..close]
                .split('|')
                .next()
                .unwrap_or("")
                .trim()
                .to_lowercase();
            if let Some(t) = basename_lower.get(&target) {
                if *t != b.page_id {
                    edges.push((b.page_id, *t));
                }
            }
            s = &s[close + 2..];
        }
    }
    edges.sort();
    edges.dedup();

    rsx! {
        h1 { class: "ls-page-title", "Graph" }
        div { style: "color: var(--ls-secondary-text-color); font-size: 0.85rem; margin-bottom: 0.5em;",
            "{pages.len()} pages · {edges.len()} links"
        }
        svg {
            width: "800",
            height: "600",
            style: "background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 8px;",
            view_box: "0 0 800 600",
            for (i, (from, to)) in edges.iter().enumerate() {
                {
                    let (x1, y1) = positions.get(from).copied().unwrap_or((cx, cy));
                    let (x2, y2) = positions.get(to).copied().unwrap_or((cx, cy));
                    rsx! {
                        line {
                            key: "e{i}",
                            x1: "{x1}", y1: "{y1}", x2: "{x2}", y2: "{y2}",
                            stroke: "var(--ls-border-color)",
                            stroke_width: "1",
                            opacity: "0.6",
                        }
                    }
                }
            }
            for (i, p) in pages.iter().enumerate() {
                {
                    let (x, y) = positions.get(&p.id).copied().unwrap_or((cx, cy));
                    let pid = p.id;
                    let name = p.basename.clone();
                    rsx! {
                        g { key: "n{i}",
                            transform: "translate({x}, {y})",
                            onclick: move |_| on_pick_page.call(pid),
                            style: "cursor: pointer;",
                            circle {
                                r: "5",
                                fill: "var(--ls-active-primary-color)",
                            }
                            text {
                                x: "8", y: "4",
                                fill: "var(--ls-primary-text-color)",
                                font_size: "11",
                                "{name}"
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn AllPagesGrid(vault: LogseqVault, on_pick_page: EventHandler<Uuid>) -> Element {
    let mut sorted = vault.pages.clone();
    sorted.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    rsx! {
        div {
            style: "display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 0.5em;",
            for p in sorted {
                {
                    let id = p.id;
                    let block_count = vault.blocks.iter().filter(|b| b.page_id == id).count();
                    let count_label = if block_count == 1 {
                        "1 block".to_string()
                    } else {
                        format!("{block_count} blocks")
                    };
                    rsx! {
                        div {
                            key: "{id}",
                            style: "border: 1px solid var(--ls-border-color); border-radius: 0.4em; padding: 0.6em 0.8em; background: var(--ls-secondary-background-color); cursor: pointer;",
                            onclick: move |_| on_pick_page.call(id),
                            div { style: "font-weight: 600; color: var(--ls-active-primary-color);", "{p.basename}" }
                            div { style: "font-size: 0.75rem; color: var(--ls-secondary-text-color); margin-top: 0.2em;",
                                "{count_label}"
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn PageView(
    doc: DocHandle,
    page: Page,
    blocks: Vec<Block>,
    vault: LogseqVault,
    on_pick_page: EventHandler<Uuid>,
) -> Element {
    let _ = doc;
    let vault_for_backlinks = vault.clone();
    let zoom_state = try_use_context::<ZoomState>();
    let mut zoom_w = zoom_state;
    let zoom_id = zoom_state.as_ref().and_then(|z| *z.0.read());

    let mut sorted = blocks.clone();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let full_tree = build_block_tree(&sorted);

    // If zoom is active and we can find the target, render only
    // that subtree. Otherwise render the whole page.
    let tree_to_show: Vec<BlockNodeTree> = if let Some(id) = zoom_id {
        find_subtree(&full_tree, id)
            .map(|t| vec![t])
            .unwrap_or(full_tree.clone())
    } else {
        full_tree.clone()
    };
    let zoomed_block_title: Option<String> = zoom_id
        .and_then(|id| sorted.iter().find(|b| b.id == id))
        .map(|b| publish_core::peel_task_marker(&b.content).1.to_string());

    let page_id_for_title = page.id;
    let basename_for_title = page.basename.clone();
    let editing_title: Signal<Option<String>> = use_signal(|| None);
    let mut editing_title_w = editing_title;
    rsx! {
        if let Some(title) = zoomed_block_title.clone() {
            div { style: "display: flex; gap: 0.5em; align-items: center; margin-bottom: 0.75em; color: var(--ls-secondary-text-color); font-size: 0.85rem;",
                a {
                    href: "#",
                    style: "color: var(--ls-link-text-color); cursor: pointer; text-decoration: none;",
                    onclick: move |e| {
                        e.prevent_default();
                        if let Some(ref mut z) = zoom_w.as_mut() {
                            z.0.set(None);
                        }
                    },
                    "← {page.basename}"
                }
                span { "/" }
                span { "{title}" }
            }
        } else if let Some(draft) = editing_title.read().clone() {
            input {
                class: "ls-page-title",
                style: "background: transparent; border: 0; border-bottom: 1px solid var(--ls-active-primary-color); width: 100%; padding: 0; color: inherit; font-family: inherit; font-size: var(--ls-page-title-size); font-weight: 500;",
                value: "{draft}",
                onmounted: |e: Event<MountedData>| {
                    spawn(async move { let _ = e.data().set_focus(true).await; });
                },
                oninput: move |e: Event<FormData>| editing_title_w.set(Some(e.value())),
                onkeydown: move |e: Event<KeyboardData>| {
                    match e.key() {
                        Key::Enter => {
                            e.prevent_default();
                            let new_name = editing_title_w.peek().clone().unwrap_or_default();
                            if !new_name.trim().is_empty() {
                                if let Some(ops) = try_use_context::<PageOps>() {
                                    ops.rename_page.call((page_id_for_title, new_name.trim().to_string()));
                                }
                            }
                            editing_title_w.set(None);
                        }
                        Key::Escape => {
                            e.prevent_default();
                            editing_title_w.set(None);
                        }
                        _ => {}
                    }
                },
                onblur: move |_| editing_title_w.set(None),
            }
        } else {
            h1 {
                class: "ls-page-title",
                style: "cursor: text;",
                title: "Click to rename",
                onclick: move |_| editing_title_w.set(Some(basename_for_title.clone())),
                "{page.basename}"
            }
            if let Some(day) = page.journal_day.as_ref() {
                div { style: "color: var(--ls-secondary-text-color); margin-top: -0.5em; margin-bottom: 1em;",
                    "{day}"
                }
            }
            if !page.aliases.is_empty() {
                div { style: "display: flex; flex-wrap: wrap; gap: 0.3em; margin-bottom: 0.75em;",
                    span { style: "color: var(--ls-secondary-text-color); font-size: 0.8rem; align-self: center; text-transform: uppercase; letter-spacing: 0.08em;",
                        "Aliases"
                    }
                    for alias in page.aliases.clone() {
                        span {
                            key: "{alias}",
                            style: "padding: 0.1em 0.5em; background: var(--ls-tertiary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.3em; font-size: 0.8rem; color: var(--ls-link-text-color);",
                            "{alias}"
                        }
                    }
                }
            }
            {
                let frontmatter_chips = publish_core::parse_props(&page.frontmatter_json);
                if !frontmatter_chips.is_empty() {
                    rsx! {
                        div {
                            style: "background: var(--ls-block-properties-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; padding: 0.5em 0.7em; margin-bottom: 1.25em; display: grid; grid-template-columns: max-content 1fr; gap: 0.25em 0.75em; font-size: 0.85rem;",
                            for (k, v) in frontmatter_chips {
                                div { key: "{k}-k", style: "color: var(--ls-secondary-text-color); font-weight: 600;", "{k}" }
                                div { key: "{k}-v", style: "color: var(--ls-primary-text-color);", "{v}" }
                            }
                        }
                    }
                } else {
                    rsx! {}
                }
            }
        }
        {
            // Whiteboard pages get a placeholder card with the
            // preserved Excalidraw JSON in a foldable details pane —
            // we don't ship a canvas editor, so editing happens in
            // Excalidraw itself, but the data round-trips on save.
            let fm: serde_json::Value =
                serde_json::from_str(&page.frontmatter_json).unwrap_or(serde_json::Value::Null);
            let is_whiteboard = fm.get("whiteboard").and_then(|v| v.as_bool()).unwrap_or(false);
            let payload = fm.get("excalidraw").and_then(|v| v.as_str()).map(|s| s.to_string());
            if is_whiteboard {
                rsx! {
                    div { style: "border: 1px solid var(--ls-border-color); border-radius: 8px; padding: 1em 1.2em; margin: 0.5em 0 1.5em; background: var(--ls-secondary-background-color);",
                        div { style: "display: flex; align-items: center; gap: 0.5em; color: var(--ls-secondary-text-color); font-size: 0.85rem;",
                            span { "◇" }
                            span { "Whiteboard (read-only here — edit in Excalidraw)" }
                        }
                        if let Some(p) = payload {
                            details { style: "margin-top: 0.6em;",
                                summary { style: "color: var(--ls-secondary-text-color); font-size: 0.8rem; cursor: pointer;",
                                    "Show raw payload ({p.len()} chars)"
                                }
                                pre { style: "white-space: pre-wrap; word-break: break-all; max-height: 320px; overflow: auto; background: var(--ls-tertiary-background-color); padding: 0.6em; border-radius: 4px; font-size: 0.8rem; margin-top: 0.4em;",
                                    "{p}"
                                }
                            }
                        }
                    }
                }
            } else {
                rsx! {}
            }
        }
        if tree_to_show.is_empty() {
            EmptyPagePlaceholder { page_id: page.id }
        } else {
            div { class: "ls-block-tree",
                for node in tree_to_show {
                    LogseqBlockNode {
                        key: "{node.block.id}",
                        node: node.clone(),
                        depth: 0,
                        on_pick_page,
                    }
                }
            }
        }
        BacklinksSection { page: page.clone(), vault: vault_for_backlinks, on_pick_page, zoomed_block_id: zoom_id }
    }
}

#[component]
fn EmptyPagePlaceholder(page_id: Uuid) -> Element {
    // Inline button that creates the page's first block + drops
    // into edit mode so the user can start typing immediately.
    let doc_ctx = try_use_context::<DocHandle>();
    let editing_id = try_use_context::<Signal<Option<Uuid>>>();
    let on_start = move |_e: Event<MouseData>| {
        let Some(handle) = doc_ctx.clone() else {
            return;
        };
        let mut editing = editing_id;
        spawn(async move {
            // Find the vault id from any existing block on this
            // page (none) or fall back to first vault.
            let vault_id = first_vault_id(&handle.0).await.unwrap_or(Uuid::nil());
            let br = BlockRepoLoro::new(&handle.0);
            match br
                .create(BlockCreate {
                    vault_id,
                    page_id,
                    parent_block_id: None,
                    sort_key: "m".into(),
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
                Ok(b) => {
                    if let Some(mut e) = editing.as_mut() {
                        e.set(Some(b.id));
                    }
                }
                Err(e) => tracing::warn!(?e, "create first block failed"),
            }
        });
    };
    rsx! {
        div {
            style: "padding: 1em; color: var(--ls-secondary-text-color); cursor: text; border: 1px dashed var(--ls-border-color); border-radius: 0.5em; margin: 1em 0;",
            onclick: on_start,
            "Click to start writing."
        }
    }
}

#[component]
fn BacklinksSection(
    page: Page,
    vault: LogseqVault,
    on_pick_page: EventHandler<Uuid>,
    zoomed_block_id: Option<Uuid>,
) -> Element {
    use std::collections::HashMap;
    let our_basename_lower = page.basename.to_lowercase();
    let our_aliases_lower: Vec<String> = page.aliases.iter().map(|a| a.to_lowercase()).collect();
    let our_block_ids: std::collections::HashSet<Uuid> = vault
        .blocks
        .iter()
        .filter(|b| b.page_id == page.id)
        .map(|b| b.id)
        .collect();

    // Group referring page → list of (block_id, snippet).
    // When `zoomed_block_id` is set, restrict matches to block refs
    // pointing at *that* block specifically — wikilinks to the page
    // don't count as backlinks for the zoomed block.
    let mut by_source: HashMap<Uuid, Vec<(Uuid, String)>> = HashMap::new();
    let wikilink_needles: Vec<String> = std::iter::once(our_basename_lower.clone())
        .chain(our_aliases_lower.iter().cloned())
        .map(|n| format!("[[{n}]]"))
        .collect();
    for b in &vault.blocks {
        if b.page_id == page.id {
            continue;
        }
        let content_lower = b.content.to_lowercase();
        let matched = if let Some(zid) = zoomed_block_id {
            let needle = format!("(({zid}))").to_lowercase();
            content_lower.contains(&needle)
        } else {
            let mut m = wikilink_needles
                .iter()
                .any(|n| content_lower.contains(n.as_str()));
            if !m {
                for id in &our_block_ids {
                    let needle = format!("(({id}))").to_lowercase();
                    if content_lower.contains(&needle) {
                        m = true;
                        break;
                    }
                }
            }
            m
        };
        if matched {
            let snippet = b.content.lines().next().unwrap_or("").to_string();
            by_source
                .entry(b.page_id)
                .or_default()
                .push((b.id, snippet));
        }
    }
    if by_source.is_empty() {
        return rsx! {
            section { style: "margin-top: 3em; padding-top: 1em; border-top: 1px solid var(--ls-border-color);",
                div { style: "font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color);",
                    "Linked references"
                }
                div { style: "color: var(--ls-secondary-text-color); font-style: italic; font-size: 0.85rem; margin-top: 0.3em;",
                    "No references yet."
                }
            }
        };
    }
    let total: usize = by_source.values().map(|v| v.len()).sum();
    let header = if zoomed_block_id.is_some() {
        format!("Linked references to this block · {total}")
    } else {
        format!("Linked references · {total}")
    };
    let zoom_state = try_use_context::<ZoomState>();
    rsx! {
        section { style: "margin-top: 3em; padding-top: 1em; border-top: 1px solid var(--ls-border-color);",
            div { style: "font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); margin-bottom: 0.5em;",
                "{header}"
            }
            for (source_page_id, hits) in by_source {
                {
                    let basename = vault
                        .pages
                        .iter()
                        .find(|p| p.id == source_page_id)
                        .map(|p| p.basename.clone())
                        .unwrap_or_else(|| "—".into());
                    rsx! {
                        div { key: "{source_page_id}",
                            style: "margin: 0.6em 0; padding-left: 0.6em; border-left: 2px solid var(--ls-border-color);",
                            div {
                                style: "color: var(--ls-link-text-color); cursor: pointer; font-weight: 600; margin-bottom: 0.25em;",
                                onclick: move |_| on_pick_page.call(source_page_id),
                                "{basename}"
                            }
                            for (b_id, snippet) in hits {
                                {
                                    let src_page = source_page_id;
                                    let block_id = b_id;
                                    let mut zoom_w = zoom_state;
                                    rsx! {
                                        div { key: "{b_id}",
                                            style: "color: var(--ls-secondary-text-color); font-size: 0.85rem; margin: 0.15em 0; cursor: pointer;",
                                            onclick: move |_| {
                                                on_pick_page.call(src_page);
                                                if let Some(z) = zoom_w.as_mut() {
                                                    z.0.set(Some(block_id));
                                                }
                                            },
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

#[component]
fn CommandPalette(query: String, pages: Vec<Page>, on_pick_page: EventHandler<Uuid>) -> Element {
    let q_lower = query.to_lowercase();
    let mut hits: Vec<Page> = pages
        .iter()
        .filter(|p| q_lower.is_empty() || p.basename.to_lowercase().contains(&q_lower))
        .cloned()
        .collect();
    hits.sort_by(|a, b| a.basename.cmp(&b.basename));
    hits.truncate(15);
    rsx! {
        div {
            style: "position: fixed; top: 3.2rem; left: 50%; transform: translateX(-50%); width: 540px; max-width: 92vw; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.5em; box-shadow: 0 24px 60px rgba(0,0,0,0.45); z-index: 60; max-height: 70vh; overflow-y: auto;",
            div { style: "padding: 0.4em 0.7em; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); border-bottom: 1px solid var(--ls-border-color);",
                "Jump to page · matches: {hits.len()}"
            }
            if hits.is_empty() {
                div { style: "padding: 0.8em; color: var(--ls-secondary-text-color); font-style: italic;",
                    "No pages match. Press Esc to close."
                }
            }
            for p in hits {
                {
                    let id = p.id;
                    rsx! {
                        div {
                            key: "{id}",
                            style: "padding: 0.5em 0.75em; cursor: pointer; border-bottom: 1px solid var(--ls-border-color); color: var(--ls-link-text-color);",
                            onclick: move |_| on_pick_page.call(id),
                            "{p.basename}"
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn RightSidebar(stack: Signal<Vec<SidebarEntry>>, vault: LogseqVault) -> Element {
    let entries = stack.read().clone();
    if entries.is_empty() {
        return rsx! { div {} };
    }
    rsx! {
        aside { class: "ls-right-sidebar",
            div { class: "ls-right-sidebar-header",
                span { "Stacked references" }
                span {
                    style: "cursor: pointer; text-transform: none; letter-spacing: 0;",
                    onclick: move |_| stack.set(Vec::new()),
                    "Clear"
                }
            }
            for (i, entry) in entries.into_iter().enumerate() {
                {
                    let mut stack_w = stack;
                    let entry_owned = entry.clone();
                    rsx! {
                        div { key: "{i}", class: "ls-right-sidebar-card",
                            span {
                                class: "ls-sidebar-close",
                                onclick: move |_| {
                                    let mut cur = stack_w.peek().clone();
                                    cur.retain(|e| e != &entry_owned);
                                    stack_w.set(cur);
                                },
                                "×"
                            }
                            SidebarCard { entry, vault: vault.clone() }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn SidebarCard(entry: SidebarEntry, vault: LogseqVault) -> Element {
    match entry {
        SidebarEntry::Block(id) => {
            let Some(b) = vault.blocks.iter().find(|b| b.id == id).cloned() else {
                return rsx! { div { style: "color: var(--ls-secondary-text-color);", "block not found" } };
            };
            let snippet = b.content.lines().next().unwrap_or("").to_string();
            rsx! {
                div { style: "color: var(--ls-secondary-text-color); font-size: 0.7rem;",
                    "Block"
                }
                div { style: "margin-top: 0.25em;", "{snippet}" }
            }
        }
        SidebarEntry::Page(id) => {
            let Some(p) = vault.pages.iter().find(|p| p.id == id).cloned() else {
                return rsx! { div { style: "color: var(--ls-secondary-text-color);", "page not found" } };
            };
            rsx! {
                div { style: "color: var(--ls-secondary-text-color); font-size: 0.7rem;", "Page" }
                div { style: "margin-top: 0.25em; font-weight: 500;", "{p.basename}" }
            }
        }
    }
}

fn find_subtree(tree: &[BlockNodeTree], id: Uuid) -> Option<BlockNodeTree> {
    for n in tree {
        if n.block.id == id {
            return Some(n.clone());
        }
        if let Some(found) = find_subtree(&n.children, id) {
            return Some(found);
        }
    }
    None
}

#[derive(Clone, PartialEq)]
struct BlockNodeTree {
    block: Block,
    children: Vec<BlockNodeTree>,
}

fn build_block_tree(blocks: &[Block]) -> Vec<BlockNodeTree> {
    use std::collections::HashMap;
    let mut by_parent: HashMap<Option<Uuid>, Vec<Block>> = HashMap::new();
    for b in blocks {
        by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b.clone());
    }
    for v in by_parent.values_mut() {
        v.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    build_tree_recursive(None, &by_parent)
}

fn build_tree_recursive(
    parent: Option<Uuid>,
    map: &std::collections::HashMap<Option<Uuid>, Vec<Block>>,
) -> Vec<BlockNodeTree> {
    map.get(&parent)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .map(|b| {
            let children = build_tree_recursive(Some(b.id), map);
            BlockNodeTree { block: b, children }
        })
        .collect()
}

#[component]
fn LogseqBlockNode(node: BlockNodeTree, depth: usize, on_pick_page: EventHandler<Uuid>) -> Element {
    let _ = on_pick_page;
    let has_children = !node.children.is_empty();
    let block = node.block.clone();
    let block_id = block.id;
    let is_collapsed = block.collapsed;
    let ops = try_use_context::<BlockOps>();

    let ops_fold = ops.clone();
    let on_fold = move |e: Event<MouseData>| {
        e.stop_propagation();
        if let Some(ops) = ops_fold.as_ref() {
            ops.toggle_collapsed.call(block_id);
        }
    };
    let ops_bullet = ops.clone();
    let on_bullet = move |e: Event<MouseData>| {
        e.stop_propagation();
        if let Some(ops) = ops_bullet.as_ref() {
            // Shift-click → open in right sidebar; plain click →
            // zoom into the block (Logseq's bullet behavior).
            if e.modifiers().shift() {
                ops.open_in_sidebar.call(block_id);
            } else {
                ops.zoom_block.call(block_id);
            }
        }
    };
    let chevron = if has_children {
        if is_collapsed { "▸" } else { "▾" }
    } else {
        ""
    };

    let block_menu_state = try_use_context::<BlockMenuState>();
    let on_context = move |e: Event<MouseData>| {
        e.prevent_default();
        let coords = e.data().client_coordinates();
        if let Some(menu) = block_menu_state {
            let mut sig = menu.0;
            sig.set(Some((block_id, coords.x as i32, coords.y as i32)));
        }
    };

    rsx! {
        div { class: "ls-block",
            "data-block-id": "{block_id}",
            oncontextmenu: on_context,
            div { class: "ls-block-row",
                div {
                    class: if has_children { "ls-fold has-children" } else { "ls-fold" },
                    onclick: on_fold,
                    "{chevron}"
                }
                div {
                    class: if has_children { "ls-bullet has-children" } else { "ls-bullet" },
                    onclick: on_bullet,
                    title: "Click: zoom · Shift-click: open in sidebar",
                    div { class: "ls-bullet-dot" }
                }
                LogseqBlockBody { block: block.clone() }
            }
            if has_children && !is_collapsed {
                div { class: "ls-block-children",
                    for c in node.children.clone() {
                        LogseqBlockNode {
                            key: "{c.block.id}",
                            node: c,
                            depth: depth + 1,
                            on_pick_page,
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn LogseqBlockBody(block: Block) -> Element {
    let resolver = try_use_context::<WikiResolver>().unwrap_or_default();
    let block_refs = try_use_context::<BlockRefResolver>().unwrap_or_default();
    let page_embeds = try_use_context::<PageEmbedResolver>().unwrap_or_default();
    let queries = try_use_context::<QueryResolver>().unwrap_or_default();
    let namespaces = try_use_context::<NamespaceResolver>().unwrap_or_default();
    let editing_id = try_use_context::<Signal<Option<Uuid>>>();
    let ops = try_use_context::<BlockOps>();

    let block_id = block.id;
    let is_editing = editing_id
        .as_ref()
        .map(|s| *s.read() == Some(block_id))
        .unwrap_or(false);
    if is_editing {
        return rsx! { EditableBlock { block: block.clone() } };
    }

    // Markdown table: `| a | b |\n|---|---|\n| 1 | 2 |`.
    if let Some(rows) = peel_table(&block.content) {
        let ops_for_click = ops.clone();
        let on_click = move |_e: Event<MouseData>| {
            if let Some(ops) = ops_for_click.as_ref() {
                ops.enter_edit.call(block_id);
            }
        };
        let header = rows.first().cloned().unwrap_or_default();
        let body: Vec<Vec<String>> = rows.into_iter().skip(1).collect();
        return rsx! {
            div {
                style: "flex: 1; min-width: 0; cursor: text;",
                onclick: on_click,
                table { class: "ls-table",
                    style: "border-collapse: collapse; margin: 0.4em 0; font-size: 0.9rem;",
                    thead {
                        tr {
                            for (i, cell) in header.iter().enumerate() {
                                th { key: "{i}",
                                    style: "border: 1px solid var(--ls-border-color); padding: 0.3em 0.6em; text-align: left; background: var(--ls-secondary-background-color);",
                                    "{cell}"
                                }
                            }
                        }
                    }
                    tbody {
                        for (r, row) in body.iter().enumerate() {
                            tr { key: "{r}",
                                for (c, cell) in row.iter().enumerate() {
                                    td { key: "{c}",
                                        style: "border: 1px solid var(--ls-border-color); padding: 0.3em 0.6em;",
                                        "{cell}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        };
    }

    // Fenced code block: ```lang\n...\n``` — render with
    // server-side syntax highlighting (syntect) before falling
    // through to inline parsing. Click-to-edit still works.
    if let Some((lang, code)) = peel_fenced_code(&block.content) {
        let highlighted = publish_core::syntax::highlight(&code, &lang);
        let ops_for_click = ops.clone();
        let on_click = move |_e: Event<MouseData>| {
            if let Some(ops) = ops_for_click.as_ref() {
                ops.enter_edit.call(block_id);
            }
        };
        return rsx! {
            div {
                style: "flex: 1; min-width: 0; cursor: text;",
                onclick: on_click,
                pre { class: "ls-code-block",
                    style: "background: var(--ls-secondary-background-color); padding: 0.6em; border-radius: 4px; overflow-x: auto; font-size: 0.85rem;",
                    if !lang.is_empty() {
                        div { class: "ls-code-lang",
                            style: "font-size: 0.7rem; color: var(--ls-secondary-text-color); margin-bottom: 0.3em;",
                            "{lang}"
                        }
                    }
                    code { dangerous_inner_html: "{highlighted}" }
                }
            }
        };
    }

    let (marker, after_marker) = publish_core::peel_task_marker(&block.content);
    let (plan, after_plan) = publish_core::peel_planning(after_marker);
    let (drawers, after_drawers) = publish_core::peel_drawers(after_plan);
    let after_drawers_owned: String = after_drawers;
    let properties = use_context::<publish_core::PagePropertyResolver>();
    let templates = use_context::<publish_core::TemplateResolver>();
    let inlines = publish_core::parse(
        &after_drawers_owned,
        &resolver,
        &block_refs,
        &page_embeds,
        &queries,
        &namespaces,
        &properties,
        &templates,
    );
    let chips = publish_core::parse_props(&block.properties_json);

    let kind = block.kind.clone();
    let heading_class = if kind == "heading" {
        let level = block.heading_level.unwrap_or(1).clamp(1, 6);
        format!("ls-block-content ls-block-heading-{level}")
    } else {
        "ls-block-content".to_string()
    };

    let marker_cls = marker.map(|m| match m {
        publish_core::TaskMarker::Todo => "todo",
        publish_core::TaskMarker::Doing => "doing",
        publish_core::TaskMarker::Done => "done",
        publish_core::TaskMarker::Later => "later",
        publish_core::TaskMarker::Now => "now",
        publish_core::TaskMarker::Waiting => "waiting",
        publish_core::TaskMarker::Cancelled => "cancelled",
    });
    let marker_label = marker.map(|m| m.label());
    let is_empty = inlines.is_empty();

    // Single click anywhere on the static body switches to edit
    // mode. Mirrors Logseq's "click content to edit" gesture.
    let ops_for_click = ops.clone();
    let on_static_click = move |_e: Event<MouseData>| {
        if let Some(ops) = ops_for_click.as_ref() {
            ops.enter_edit.call(block_id);
        }
    };

    rsx! {
        div {
            style: "flex: 1; min-width: 0; cursor: text;",
            onclick: on_static_click,
            div { class: "{heading_class}",
                if let (Some(label), Some(cls)) = (marker_label, marker_cls) {
                    span { class: "ls-task-marker {cls}", "{label}" }
                }
                if is_empty {
                    span { class: "ls-block-empty", "—" }
                } else {
                    for (i, n) in inlines.into_iter().enumerate() {
                        InlineNode { key: "{i}", node: n }
                    }
                }
            }
            if !plan.scheduled.is_empty() || !plan.deadline.is_empty() {
                div { class: "ls-block-plan",
                    if !plan.scheduled.is_empty() {
                        span { class: "ls-plan-pill",
                            span { class: "ls-plan-key", "SCHEDULED" }
                            span { class: "ls-plan-val", "{plan.scheduled}" }
                        }
                    }
                    if !plan.deadline.is_empty() {
                        span { class: "ls-plan-pill deadline",
                            span { class: "ls-plan-key", "DEADLINE" }
                            span { class: "ls-plan-val", "{plan.deadline}" }
                        }
                    }
                }
            }
            if !chips.is_empty() {
                div { class: "ls-block-props",
                    for (k, v) in chips {
                        span { key: "{k}", class: "ls-prop-chip",
                            span { class: "ls-prop-key", "{k}" }
                            span { class: "ls-prop-val", "{v}" }
                        }
                    }
                }
            }
            for (i, drawer) in drawers.into_iter().enumerate() {
                if drawer.name.eq_ignore_ascii_case("logbook") {
                    LogbookView { key: "{i}", body: drawer.body.clone() }
                } else {
                    details {
                        key: "{i}",
                        class: "ls-drawer",
                        summary { class: "ls-drawer-name", "{drawer.name}" }
                        pre { class: "ls-drawer-body", "{drawer.body}" }
                    }
                }
            }
        }
    }
}

/// Newtype wrappers so multiple `Signal<Option<(Uuid, String)>>`
/// can coexist in Dioxus's type-keyed context map.
#[derive(Clone, Copy)]
pub(crate) struct SlashState(pub Signal<Option<(Uuid, String)>>);

#[derive(Clone, Copy)]
pub(crate) struct PageSearchState(pub Signal<Option<(Uuid, String)>>);

#[derive(Clone, Copy)]
pub(crate) struct BlockRefState(pub Signal<Option<(Uuid, String)>>);

#[derive(Clone, Copy)]
pub(crate) struct TagSearchState(pub Signal<Option<(Uuid, String)>>);

/// Newtype for the per-shell `Option<Uuid>` "zoomed block" — the
/// shell would otherwise collide with `editing_id` (same type).
#[derive(Clone, Copy)]
pub(crate) struct ZoomState(pub Signal<Option<Uuid>>);

/// Active tag view. `Some("rust")` means the main pane is showing
/// the tag-results page instead of a regular page.
#[derive(Clone, Copy)]
pub(crate) struct TagViewState(pub Signal<Option<String>>);

/// Cmd-K command palette state. `Some(query)` while open.
#[derive(Clone, Copy)]
pub(crate) struct CommandPaletteState(pub Signal<Option<String>>);

/// Right-click block context menu position. `(block_id, x, y)`.
#[derive(Clone, Copy)]
pub(crate) struct BlockMenuState(pub Signal<Option<(Uuid, i32, i32)>>);

/// MRU list of recently-visited pages.
#[derive(Clone, Copy)]
pub(crate) struct RecentsState(pub Signal<Vec<Uuid>>);

/// `Some(message)` while a transient toast (e.g. import result)
/// is shown to the user. The Status bar pulls from this; the
/// import button writes to it.
#[derive(Clone, Copy)]
pub(crate) struct ImportToastState(pub Signal<Option<String>>);

/// One pane in the right sidebar's stack.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SidebarEntry {
    /// Open a single block in a card. Click cycles to zoom.
    Block(Uuid),
    /// Open a whole page in a smaller pane.
    Page(Uuid),
}

/// Inline editor — replaces the static block content with a
/// textarea autosized to its content. Handles the Logseq
/// keyboard model: Enter splits, Tab indents, Shift+Tab outdents,
/// Backspace at offset 0 on empty content deletes the block,
/// Escape exits edit mode.
#[component]
fn EditableBlock(block: Block) -> Element {
    let ops = try_use_context::<BlockOps>();
    let slash_state = try_use_context::<SlashState>();
    let page_search_state = try_use_context::<PageSearchState>();
    let block_id = block.id;
    let initial_content = block.content.clone();
    let content_signal: Signal<String> = use_signal(|| initial_content.clone());

    let auto_focus = move |elem: Event<MountedData>| {
        spawn(async move {
            let _ = elem.data().set_focus(true).await;
        });
    };

    let block_ref_state = try_use_context::<BlockRefState>();
    let tag_search_state = try_use_context::<TagSearchState>();
    let ops_for_input = ops.clone();
    let mut content_w = content_signal;
    let id_str_input = block_id.simple().to_string();
    let slash_w = slash_state;
    let page_search_w = page_search_state;
    let block_ref_w = block_ref_state;
    let tag_search_w = tag_search_state;
    let on_input = move |e: Event<FormData>| {
        let v = e.value();
        content_w.set(v.clone());
        if let Some(ops) = ops_for_input.as_ref() {
            ops.update_content.call((block_id, v.clone()));
        }
        // Detect all four popup triggers in one pass — first
        // match wins. Order matters: longer literal sigils
        // (`[[`, `((`) before single chars (`/`, `#`).
        let id_str = id_str_input.clone();
        let content = v.clone();
        let mut slash = slash_w;
        let mut page_search = page_search_w;
        let mut block_ref = block_ref_w;
        let mut tag_search = tag_search_w;
        spawn(async move {
            let off = read_selection_start(&id_str).await.unwrap_or(content.len());
            let before = &content[..off.min(content.len())];

            let close_all = |slash: &mut Option<SlashState>,
                             page_search: &mut Option<PageSearchState>,
                             block_ref: &mut Option<BlockRefState>,
                             tag_search: &mut Option<TagSearchState>| {
                if let Some(s) = slash.as_mut() {
                    s.0.set(None);
                }
                if let Some(p) = page_search.as_mut() {
                    p.0.set(None);
                }
                if let Some(b) = block_ref.as_mut() {
                    b.0.set(None);
                }
                if let Some(t) = tag_search.as_mut() {
                    t.0.set(None);
                }
            };

            // Block ref: `((query` unclosed.
            if let Some(pos) = before.rfind("((") {
                let rest = &before[pos + 2..];
                if !rest.contains("))") {
                    close_all(
                        &mut slash,
                        &mut page_search,
                        &mut block_ref,
                        &mut tag_search,
                    );
                    if let Some(b) = block_ref.as_mut() {
                        b.0.set(Some((block_id, rest.to_string())));
                    }
                    return;
                }
            }
            // Page search: `[[query` unclosed.
            if let Some(pos) = before.rfind("[[") {
                let rest = &before[pos + 2..];
                if !rest.contains("]]") {
                    close_all(
                        &mut slash,
                        &mut page_search,
                        &mut block_ref,
                        &mut tag_search,
                    );
                    if let Some(p) = page_search.as_mut() {
                        p.0.set(Some((block_id, rest.to_string())));
                    }
                    return;
                }
            }
            // Slash palette.
            if let Some(slash_pos) = trigger_after_boundary(before, '/') {
                let q = before[slash_pos + 1..].to_string();
                close_all(
                    &mut slash,
                    &mut page_search,
                    &mut block_ref,
                    &mut tag_search,
                );
                if let Some(s) = slash.as_mut() {
                    s.0.set(Some((block_id, q)));
                }
                return;
            }
            // Tag autocomplete: `#tagquery`. Require at least one
            // char after `#` so the chip doesn't pop on every `#`.
            if let Some(tag_pos) = trigger_after_boundary(before, '#') {
                if tag_pos + 1 < before.len() {
                    let q = before[tag_pos + 1..].to_string();
                    // Only show while the query stays "tag-like".
                    if q.chars()
                        .all(|c| c.is_alphanumeric() || c == '/' || c == '-' || c == '_')
                    {
                        close_all(
                            &mut slash,
                            &mut page_search,
                            &mut block_ref,
                            &mut tag_search,
                        );
                        if let Some(t) = tag_search.as_mut() {
                            t.0.set(Some((block_id, q)));
                        }
                        return;
                    }
                }
            }
            close_all(
                &mut slash,
                &mut page_search,
                &mut block_ref,
                &mut tag_search,
            );
        });
    };

    let ops_for_keys = ops.clone();
    let content_for_keys = content_signal;
    let on_keydown = move |e: Event<KeyboardData>| {
        let Some(ops) = ops_for_keys.as_ref() else {
            return;
        };
        let key = e.key();
        let mods = e.modifiers();
        let shift = mods.shift();
        let current = content_for_keys.peek().clone();
        match &key {
            Key::Escape => {
                e.prevent_default();
                ops.exit_edit.call(());
            }
            Key::Tab => {
                e.prevent_default();
                if shift {
                    ops.outdent.call(block_id);
                } else {
                    ops.indent.call(block_id);
                }
            }
            Key::Enter if !shift => {
                e.prevent_default();
                // Caret-aware split: ask the DOM for the textarea's
                // selectionStart, then split there. The eval runs
                // synchronously inside the webview's JS context
                // (Dioxus desktop), so the await returns quickly
                // and the split lands at exactly where the user
                // pressed Enter — matching Logseq's "split at
                // caret" behavior.
                let split_cb = ops.split_block;
                let fallback_len = current.len();
                let id_str = block_id.simple().to_string();
                spawn(async move {
                    let offset = read_selection_start(&id_str).await.unwrap_or(fallback_len);
                    split_cb.call((block_id, offset));
                });
            }
            Key::Backspace if current.is_empty() => {
                e.prevent_default();
                ops.delete_block.call(block_id);
            }
            Key::Backspace => {
                // For non-empty content, check whether the caret is
                // at offset 0 — if so, merge with the previous
                // sibling (Logseq's behavior). Otherwise let the
                // browser's default delete-one-char run.
                let delete_cb = ops.delete_block;
                let id_str = block_id.simple().to_string();
                let current_clone = current.clone();
                spawn(async move {
                    let offset = read_selection_start(&id_str).await.unwrap_or(1);
                    if offset == 0 && current_clone.is_empty() {
                        delete_cb.call(block_id);
                    }
                });
            }
            Key::ArrowUp => {
                if mods.ctrl() || mods.meta() {
                    // Cmd/Ctrl+ArrowUp → move block up.
                    e.prevent_default();
                    ops.move_up.call(block_id);
                } else {
                    // Plain ArrowUp: jump to previous block only
                    // when the caret is at the top of the textarea
                    // (offset 0 or first line). For simplicity we
                    // check offset 0 — multi-line awareness comes
                    // later. Cancel otherwise to let cursor move
                    // up by line.
                    let id_str = block_id.simple().to_string();
                    let prev_cb = ops.focus_prev;
                    let bid = block_id;
                    spawn(async move {
                        if read_selection_start(&id_str).await.unwrap_or(1) == 0 {
                            prev_cb.call(bid);
                        }
                    });
                }
            }
            Key::ArrowDown => {
                if mods.ctrl() || mods.meta() {
                    e.prevent_default();
                    ops.move_down.call(block_id);
                } else {
                    // Plain ArrowDown — jump to next block when
                    // caret is at end of content.
                    let id_str = block_id.simple().to_string();
                    let next_cb = ops.focus_next;
                    let bid = block_id;
                    let cur_len = current.len();
                    spawn(async move {
                        if read_selection_start(&id_str).await.unwrap_or(0) >= cur_len {
                            next_cb.call(bid);
                        }
                    });
                }
            }
            _ => {}
        }
    };

    let ops_for_blur = ops.clone();
    let on_blur = move |_e: Event<FocusData>| {
        if let Some(ops) = ops_for_blur.as_ref() {
            ops.exit_edit.call(());
        }
    };

    let value_str = content_signal.read().clone();
    let id_attr = block_id.simple().to_string();
    // Auto-grow row count from content. Cap at 12 so a giant
    // block doesn't push the whole outliner around.
    let row_count = value_str.lines().count().max(1).min(12).to_string();
    let slash_open = slash_state
        .as_ref()
        .and_then(|s| s.0.read().clone())
        .filter(|(b, _)| *b == block_id)
        .map(|(_, q)| q);
    let page_search_open = page_search_state
        .as_ref()
        .and_then(|s| s.0.read().clone())
        .filter(|(b, _)| *b == block_id)
        .map(|(_, q)| q);
    let block_ref_open = block_ref_state
        .as_ref()
        .and_then(|s| s.0.read().clone())
        .filter(|(b, _)| *b == block_id)
        .map(|(_, q)| q);
    let tag_search_open = tag_search_state
        .as_ref()
        .and_then(|s| s.0.read().clone())
        .filter(|(b, _)| *b == block_id)
        .map(|(_, q)| q);

    rsx! {
        div { style: "flex: 1; min-width: 0; position: relative;",
            "data-edit-block": "{id_attr}",
            textarea {
                class: "ls-block-content",
                style: "background: transparent; border: 0; resize: none; width: 100%; min-height: 1.5em; color: inherit; font: inherit; outline: none;",
                rows: "{row_count}",
                value: "{value_str}",
                oninput: on_input,
                onkeydown: on_keydown,
                onblur: on_blur,
                onmounted: auto_focus,
            }
            if let Some(q) = slash_open {
                SlashPalette { block_id, query: q, content: content_signal }
            }
            if let Some(q) = page_search_open {
                PageSearchPalette { block_id, query: q, content: content_signal }
            }
            if let Some(q) = block_ref_open {
                BlockRefPalette { block_id, query: q, content: content_signal }
            }
            if let Some(q) = tag_search_open {
                TagSearchPalette { block_id, query: q, content: content_signal }
            }
        }
    }
}

/// Static catalog of slash-palette commands. Each row carries
/// its label, optional description, and the effect applied to
/// the editing block when selected.
#[derive(Clone, Debug)]
struct SlashCommand {
    label: &'static str,
    desc: &'static str,
    effect: SlashEffect,
}

#[derive(Clone, Debug)]
enum SlashEffect {
    /// Convert the block's kind. `(kind, heading_level)`.
    SetKind(&'static str, Option<i32>),
    /// Replace the slash-prefix with literal text.
    InsertText(&'static str),
    /// Insert today's date in `[[YYYY-MM-DD]]` form.
    InsertToday,
    /// Insert tomorrow's date.
    InsertTomorrow,
}

fn slash_catalog() -> &'static [SlashCommand] {
    &[
        SlashCommand {
            label: "Heading 1",
            desc: "Largest heading",
            effect: SlashEffect::SetKind("heading", Some(1)),
        },
        SlashCommand {
            label: "Heading 2",
            desc: "",
            effect: SlashEffect::SetKind("heading", Some(2)),
        },
        SlashCommand {
            label: "Heading 3",
            desc: "",
            effect: SlashEffect::SetKind("heading", Some(3)),
        },
        SlashCommand {
            label: "TODO",
            desc: "Mark this block as a task",
            effect: SlashEffect::InsertText("TODO "),
        },
        SlashCommand {
            label: "DOING",
            desc: "Mark in progress",
            effect: SlashEffect::InsertText("DOING "),
        },
        SlashCommand {
            label: "DONE",
            desc: "Mark completed",
            effect: SlashEffect::InsertText("DONE "),
        },
        SlashCommand {
            label: "LATER",
            desc: "Defer this block",
            effect: SlashEffect::InsertText("LATER "),
        },
        SlashCommand {
            label: "Quote",
            desc: "Blockquote",
            effect: SlashEffect::SetKind("blockquote", None),
        },
        SlashCommand {
            label: "Code block",
            desc: "Fenced code",
            effect: SlashEffect::SetKind("code", None),
        },
        SlashCommand {
            label: "Today",
            desc: "Insert today's date as a journal link",
            effect: SlashEffect::InsertToday,
        },
        SlashCommand {
            label: "Tomorrow",
            desc: "Insert tomorrow's date",
            effect: SlashEffect::InsertTomorrow,
        },
    ]
}

fn filter_slash(query: &str) -> Vec<&'static SlashCommand> {
    let q = query.to_lowercase();
    slash_catalog()
        .iter()
        .filter(|c| q.is_empty() || c.label.to_lowercase().contains(&q))
        .take(10)
        .collect()
}

#[component]
fn SlashPalette(block_id: Uuid, query: String, content: Signal<String>) -> Element {
    let hits = filter_slash(&query);
    let ops = try_use_context::<BlockOps>();
    let slash_state = try_use_context::<SlashState>();
    rsx! {
        div { class: "ls-popup",
            style: "position: absolute; top: 100%; left: 0; margin-top: 0.25em; min-width: 280px; max-height: 280px; overflow-y: auto; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; z-index: 50; box-shadow: 0 8px 30px rgba(0,0,0,0.35);",
            div { style: "padding: 0.35em 0.6em; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); border-bottom: 1px solid var(--ls-border-color);",
                "Commands"
            }
            for cmd in hits {
                {
                    let effect = cmd.effect.clone();
                    let ops = ops.clone();
                    let mut slash_w = slash_state;
                    let mut content_w = content;
                    let query_for_click = query.clone();
                    let onclick = move |_e: Event<MouseData>| {
                        // Strip the `/query` from the current content
                        // and apply the effect.
                        let mut cur = content_w.peek().clone();
                        if let Some(pos) = trigger_after_boundary(&cur, '/') {
                            cur.replace_range(pos..pos + 1 + query_for_click.len(), "");
                            content_w.set(cur.clone());
                            if let Some(ops) = ops.as_ref() {
                                ops.update_content.call((block_id, cur.clone()));
                            }
                        }
                        match &effect {
                            SlashEffect::SetKind(kind, hl) => {
                                if let Some(ops) = ops.as_ref() {
                                    ops.set_kind.call((block_id, kind.to_string(), *hl));
                                }
                            }
                            SlashEffect::InsertText(text) => {
                                let mut c = content_w.peek().clone();
                                c.insert_str(0, text);
                                content_w.set(c.clone());
                                if let Some(ops) = ops.as_ref() {
                                    ops.update_content.call((block_id, c));
                                }
                            }
                            SlashEffect::InsertToday => {
                                let today = chrono::Local::now().format("[[%Y-%m-%d]]").to_string();
                                let mut c = content_w.peek().clone();
                                c.push_str(&today);
                                content_w.set(c.clone());
                                if let Some(ops) = ops.as_ref() {
                                    ops.update_content.call((block_id, c));
                                }
                            }
                            SlashEffect::InsertTomorrow => {
                                let t = (chrono::Local::now() + chrono::Duration::days(1))
                                    .format("[[%Y-%m-%d]]")
                                    .to_string();
                                let mut c = content_w.peek().clone();
                                c.push_str(&t);
                                content_w.set(c.clone());
                                if let Some(ops) = ops.as_ref() {
                                    ops.update_content.call((block_id, c));
                                }
                            }
                        }
                        if let Some(ref mut s) = slash_w.as_mut() {
                            s.0.set(None);
                        }
                    };
                    rsx! {
                        div {
                            key: "{cmd.label}",
                            style: "padding: 0.4em 0.6em; cursor: pointer; border-bottom: 1px solid var(--ls-border-color);",
                            onclick,
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            div { style: "color: var(--ls-primary-text-color); font-weight: 500;", "{cmd.label}" }
                            if !cmd.desc.is_empty() {
                                div { style: "color: var(--ls-secondary-text-color); font-size: 0.75rem;", "{cmd.desc}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn PageSearchPalette(block_id: Uuid, query: String, content: Signal<String>) -> Element {
    let wiki = try_use_context::<WikiResolver>().unwrap_or_default();
    let ops = try_use_context::<BlockOps>();
    let page_search_state = try_use_context::<PageSearchState>();
    let q_lower = query.to_lowercase();
    let mut hits: Vec<(String, String)> = wiki
        .0
        .iter()
        .filter(|(name, _)| q_lower.is_empty() || name.contains(&q_lower))
        .map(|(name, slug)| (name.clone(), slug.clone()))
        .collect();
    hits.sort_by(|a, b| a.0.cmp(&b.0));
    hits.truncate(10);

    rsx! {
        div { class: "ls-popup",
            style: "position: absolute; top: 100%; left: 0; margin-top: 0.25em; min-width: 280px; max-height: 280px; overflow-y: auto; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; z-index: 50; box-shadow: 0 8px 30px rgba(0,0,0,0.35);",
            div { style: "padding: 0.35em 0.6em; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); border-bottom: 1px solid var(--ls-border-color);",
                "Link to page"
            }
            if hits.is_empty() {
                div { style: "padding: 0.6em; color: var(--ls-secondary-text-color); font-style: italic;",
                    "No matching pages. Pressing Enter will create [[", "{query}", "]] anyway."
                }
            }
            for (name, _slug) in hits {
                {
                    let name_for_click = name.clone();
                    let ops = ops.clone();
                    let mut content_w = content;
                    let mut page_search_w = page_search_state;
                    let query_for_click = query.clone();
                    let onclick = move |_e: Event<MouseData>| {
                        let mut cur = content_w.peek().clone();
                        // Find the `[[query` we matched and replace it
                        // with `[[Name]]`.
                        if let Some(pos) = cur.rfind("[[") {
                            let end = pos + 2 + query_for_click.len();
                            let end = end.min(cur.len());
                            let replacement = format!("[[{}]]", name_for_click);
                            cur.replace_range(pos..end, &replacement);
                            content_w.set(cur.clone());
                            if let Some(ops) = ops.as_ref() {
                                ops.update_content.call((block_id, cur));
                            }
                        }
                        if let Some(ref mut s) = page_search_w.as_mut() {
                            s.0.set(None);
                        }
                    };
                    rsx! {
                        div {
                            key: "{name}",
                            style: "padding: 0.35em 0.6em; cursor: pointer; color: var(--ls-link-text-color); border-bottom: 1px solid var(--ls-border-color);",
                            onclick,
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            "{name}"
                        }
                    }
                }
            }
        }
    }
}

/// Format a date as a Logseq journal title — defaults to ISO
/// 8601 `YYYY-MM-DD`. Mirrors `frontend.date/journal-name`.
pub fn journal_title(date: chrono::NaiveDate) -> String {
    date.format("%Y-%m-%d").to_string()
}

/// Parse a journal title back to a date (`YYYY-MM-DD` only).
pub fn parse_journal_title(s: &str) -> Option<chrono::NaiveDate> {
    chrono::NaiveDate::parse_from_str(s.trim(), "%Y-%m-%d").ok()
}

/// Natural-language date parser — `today`, `tomorrow`, `yesterday`,
/// `+N` / `-N` day offsets, and ISO 8601. Subset of Logseq's
/// `nld-parse`; covers the slash-command + journal-link cases.
pub fn nld_to_date(input: &str) -> Option<chrono::NaiveDate> {
    let s = input.trim().to_lowercase();
    let today = chrono::Local::now().date_naive();
    match s.as_str() {
        "today" | "now" => return Some(today),
        "tomorrow" => return Some(today + chrono::Duration::days(1)),
        "yesterday" => return Some(today - chrono::Duration::days(1)),
        _ => {}
    }
    if let Some(rest) = s.strip_prefix('+') {
        if let Ok(n) = rest.parse::<i64>() {
            return Some(today + chrono::Duration::days(n));
        }
    }
    if let Some(rest) = s.strip_prefix('-') {
        if let Ok(n) = rest.parse::<i64>() {
            return Some(today - chrono::Duration::days(n));
        }
    }
    parse_journal_title(&s)
}

/// Return the index of the most recent `ch` that immediately
/// follows a whitespace boundary (or starts the string) in
/// `before` — i.e. the start of a "trigger word" the user is
/// typing. Returns `None` when no such position exists.
pub(crate) fn trigger_after_boundary(before: &str, ch: char) -> Option<usize> {
    let bytes = before.as_bytes();
    let mut last: Option<usize> = None;
    for (i, &b) in bytes.iter().enumerate() {
        if b == ch as u8 {
            let at_start = i == 0;
            let after_ws = i > 0
                && bytes
                    .get(i - 1)
                    .map(|c| c.is_ascii_whitespace())
                    .unwrap_or(false);
            if at_start || after_ws {
                last = Some(i);
            }
        }
    }
    last
}

/// Read the focused textarea's `selectionStart` for the block
/// with the given simple-UUID id. Runs via Dioxus's `document::eval`
/// so the same code path works on desktop (Tao webview) and web
/// (browser). Returns `None` when the element is absent or the
/// eval fails — callers should fall back to a default offset.
async fn read_selection_start(block_simple_id: &str) -> Option<usize> {
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{block_simple_id}"]');
            if (!wrap) return 0;
            const ta = wrap.querySelector('textarea');
            if (!ta) return 0;
            return ta.selectionStart || 0;
        }})()
        "#
    );
    let mut handle = document::eval(&script);
    match handle.recv::<serde_json::Value>().await {
        Ok(serde_json::Value::Number(n)) => n.as_u64().map(|v| v as usize),
        _ => None,
    }
}

#[component]
fn BlockRefPalette(block_id: Uuid, query: String, content: Signal<String>) -> Element {
    let block_refs = try_use_context::<BlockRefResolver>().unwrap_or_default();
    let ops = try_use_context::<BlockOps>();
    let block_ref_state = try_use_context::<BlockRefState>();
    let q_lower = query.to_lowercase();
    let mut hits: Vec<(Uuid, String)> = block_refs
        .0
        .iter()
        .filter(|(_, target)| {
            q_lower.is_empty() || target.snippet.to_lowercase().contains(&q_lower)
        })
        .map(|(id, target)| (*id, target.snippet.clone()))
        .collect();
    hits.sort_by(|a, b| a.1.cmp(&b.1));
    hits.truncate(10);

    rsx! {
        div { class: "ls-popup",
            style: "position: absolute; top: 100%; left: 0; margin-top: 0.25em; min-width: 320px; max-height: 280px; overflow-y: auto; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; z-index: 50; box-shadow: 0 8px 30px rgba(0,0,0,0.35);",
            div { style: "padding: 0.35em 0.6em; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); border-bottom: 1px solid var(--ls-border-color);",
                "Block reference"
            }
            if hits.is_empty() {
                div { style: "padding: 0.6em; color: var(--ls-secondary-text-color); font-style: italic;",
                    "No matching blocks."
                }
            }
            for (id, snippet) in hits {
                {
                    let id_for_click = id;
                    let ops = ops.clone();
                    let mut content_w = content;
                    let mut block_ref_w = block_ref_state;
                    let query_for_click = query.clone();
                    let onclick = move |_e: Event<MouseData>| {
                        let mut cur = content_w.peek().clone();
                        if let Some(pos) = cur.rfind("((") {
                            let end = pos + 2 + query_for_click.len();
                            let end = end.min(cur.len());
                            let replacement = format!("(({}))", id_for_click);
                            cur.replace_range(pos..end, &replacement);
                            content_w.set(cur.clone());
                            if let Some(ops) = ops.as_ref() {
                                ops.update_content.call((block_id, cur));
                            }
                        }
                        if let Some(ref mut s) = block_ref_w.as_mut() {
                            s.0.set(None);
                        }
                    };
                    rsx! {
                        div {
                            key: "{id}",
                            style: "padding: 0.35em 0.6em; cursor: pointer; color: var(--ls-link-text-color); border-bottom: 1px solid var(--ls-border-color); display: flex; gap: 0.5em; align-items: baseline;",
                            onclick,
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            span { style: "font-family: ui-monospace, monospace; font-size: 0.7rem; color: var(--ls-secondary-text-color);", "((·))" }
                            span { "{snippet}" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn TagSearchPalette(block_id: Uuid, query: String, content: Signal<String>) -> Element {
    let queries_ctx = try_use_context::<QueryResolver>().unwrap_or_default();
    let ops = try_use_context::<BlockOps>();
    let tag_state = try_use_context::<TagSearchState>();
    let q_lower = query.to_lowercase();
    let mut hits: Vec<String> = queries_ctx
        .0
        .keys()
        .filter(|tag| q_lower.is_empty() || tag.contains(&q_lower))
        .cloned()
        .collect();
    hits.sort();
    hits.truncate(10);

    rsx! {
        div { class: "ls-popup",
            style: "position: absolute; top: 100%; left: 0; margin-top: 0.25em; min-width: 240px; max-height: 280px; overflow-y: auto; background: var(--ls-secondary-background-color); border: 1px solid var(--ls-border-color); border-radius: 0.4em; z-index: 50; box-shadow: 0 8px 30px rgba(0,0,0,0.35);",
            div { style: "padding: 0.35em 0.6em; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.1em; color: var(--ls-secondary-text-color); border-bottom: 1px solid var(--ls-border-color);",
                "Tag"
            }
            if hits.is_empty() {
                div { style: "padding: 0.6em; color: var(--ls-secondary-text-color); font-style: italic;",
                    "No matching tags. The current `#", "{query}", "` will stay as-typed."
                }
            }
            for tag in hits {
                {
                    let tag_for_click = tag.clone();
                    let ops = ops.clone();
                    let mut content_w = content;
                    let mut tag_w = tag_state;
                    let query_for_click = query.clone();
                    let onclick = move |_e: Event<MouseData>| {
                        let mut cur = content_w.peek().clone();
                        if let Some(pos) = cur.rfind('#') {
                            let end = pos + 1 + query_for_click.len();
                            let end = end.min(cur.len());
                            let replacement = format!("#{}", tag_for_click);
                            cur.replace_range(pos..end, &replacement);
                            content_w.set(cur.clone());
                            if let Some(ops) = ops.as_ref() {
                                ops.update_content.call((block_id, cur));
                            }
                        }
                        if let Some(ref mut s) = tag_w.as_mut() {
                            s.0.set(None);
                        }
                    };
                    rsx! {
                        div {
                            key: "{tag}",
                            style: "padding: 0.35em 0.6em; cursor: pointer; color: var(--ls-tag-text-color); border-bottom: 1px solid var(--ls-border-color);",
                            onclick,
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            "#{tag}"
                        }
                    }
                }
            }
        }
    }
}

/// Page-level operations: create / rename / delete. Constructed
/// alongside `BlockOps` and provided via Dioxus context.
#[derive(Clone)]
pub(crate) struct PageOps {
    pub create_page: Callback<String>,
    pub rename_page: Callback<(Uuid, String)>,
    pub delete_page: Callback<Uuid>,
}

fn make_page_ops(doc: Arc<CrdtDoc>, mut active_page: Signal<Option<Uuid>>) -> PageOps {
    let doc_create = doc.clone();
    let create_page = Callback::new(move |basename: String| {
        let doc = doc_create.clone();
        spawn(async move {
            let pr = PageRepoLoro::new(&doc);
            let now = Utc::now();
            let vault_id = first_vault_id(&doc).await.unwrap_or(Uuid::nil());
            match pr
                .create(knowledge_proto::PageCreate {
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
                })
                .await
            {
                Ok(p) => active_page.set(Some(p.id)),
                Err(e) => tracing::warn!(?e, "create page failed"),
            }
        });
    });
    let doc_rename = doc.clone();
    let rename_page = Callback::new(move |(id, basename): (Uuid, String)| {
        let doc = doc_rename.clone();
        spawn(async move {
            let pr = PageRepoLoro::new(&doc);
            let upd = knowledge_proto::PageUpdate {
                basename: Some(basename),
                ..Default::default()
            };
            if let Err(e) = pr.update(id, upd).await {
                tracing::warn!(?e, "rename failed");
            }
        });
    });
    let doc_delete = doc.clone();
    let delete_page = Callback::new(move |id: Uuid| {
        let doc = doc_delete.clone();
        spawn(async move {
            let pr = PageRepoLoro::new(&doc);
            if let Err(e) = pr.delete(id).await {
                tracing::warn!(?e, "delete page failed");
            }
        });
        active_page.set(None);
    });
    PageOps {
        create_page,
        rename_page,
        delete_page,
    }
}

async fn first_vault_id(doc: &CrdtDoc) -> Option<Uuid> {
    use knowledge_proto::VaultRepo;
    let vr = knowledge_crdt::VaultRepoLoro::new(doc);
    let big = ListPage { index: 0, size: 1 };
    vr.list(big, None, None)
        .await
        .ok()?
        .items
        .first()
        .map(|v| v.id)
}

/// Per-block editing callbacks the shell threads to descendant
/// block components via context. Each is a fire-and-forget
/// `spawn` that mutates the local CRDT; the doc's
/// `subscribe_local_update` ticks `version` so the UI rerenders.
#[derive(Clone)]
pub(crate) struct BlockOps {
    pub enter_edit: Callback<Uuid>,
    pub exit_edit: Callback<()>,
    pub update_content: Callback<(Uuid, String)>,
    pub split_block: Callback<(Uuid, usize)>,
    pub indent: Callback<Uuid>,
    pub outdent: Callback<Uuid>,
    pub delete_block: Callback<Uuid>,
    pub set_kind: Callback<(Uuid, String, Option<i32>)>,
    /// Swap with the previous sibling (Cmd+ArrowUp).
    pub move_up: Callback<Uuid>,
    /// Swap with the next sibling (Cmd+ArrowDown).
    pub move_down: Callback<Uuid>,
    /// Focus the block immediately before this one in document
    /// order. ArrowUp from the top of a textarea triggers this.
    pub focus_prev: Callback<Uuid>,
    /// Focus the block immediately after this one. ArrowDown.
    pub focus_next: Callback<Uuid>,
    /// Toggle the block's `collapsed` flag (fold chevron click).
    pub toggle_collapsed: Callback<Uuid>,
    /// Open the block in the right sidebar as a stacked pane.
    pub open_in_sidebar: Callback<Uuid>,
    /// Zoom into the block: navigate so that block is the
    /// effective page root. Mirrors Logseq's bullet-click.
    pub zoom_block: Callback<Uuid>,
}

fn make_block_ops(
    doc: Arc<CrdtDoc>,
    mut editing_id: Signal<Option<Uuid>>,
    mut sidebar_stack: Signal<Vec<SidebarEntry>>,
    mut zoom_target: Signal<Option<Uuid>>,
) -> BlockOps {
    let mut enter_doc = doc.clone();
    let mut editing_enter = editing_id;
    let enter_edit = Callback::new(move |id: Uuid| {
        let _ = &mut enter_doc; // capture for stable closure scope
        editing_enter.set(Some(id));
    });
    let mut editing_exit = editing_id;
    let exit_edit = Callback::new(move |_| {
        editing_exit.set(None);
    });
    let doc_update = doc.clone();
    let update_content = Callback::new(move |(id, content): (Uuid, String)| {
        let doc = doc_update.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let upd = BlockUpdate {
                content: Some(content),
                ..Default::default()
            };
            if let Err(e) = repo.update(id, upd).await {
                tracing::warn!(?e, "block update failed");
            }
        });
    });
    let doc_split = doc.clone();
    let mut editing_split = editing_id;
    let split_block = Callback::new(move |(id, offset): (Uuid, usize)| {
        let doc = doc_split.clone();
        spawn(async move {
            if let Err(e) = split_block_async(&doc, id, offset).await {
                tracing::warn!(?e, "split failed");
                return;
            }
            // After the split, focus the new sibling so the user
            // keeps typing into it. We reuse the parent block id
            // → won't be exactly right but `enter_edit` lands on
            // a sane block.
        });
        editing_split.set(None);
    });
    let doc_indent = doc.clone();
    let indent = Callback::new(move |id: Uuid| {
        let doc = doc_indent.clone();
        spawn(async move {
            if let Err(e) = indent_block_async(&doc, id).await {
                tracing::warn!(?e, "indent failed");
            }
        });
    });
    let doc_outdent = doc.clone();
    let outdent = Callback::new(move |id: Uuid| {
        let doc = doc_outdent.clone();
        spawn(async move {
            if let Err(e) = outdent_block_async(&doc, id).await {
                tracing::warn!(?e, "outdent failed");
            }
        });
    });
    let doc_delete = doc.clone();
    let mut editing_delete = editing_id;
    let delete_block = Callback::new(move |id: Uuid| {
        let doc = doc_delete.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            if let Err(e) = repo.delete(id).await {
                tracing::warn!(?e, "block delete failed");
            }
        });
        editing_delete.set(None);
    });
    let doc_kind = doc.clone();
    let set_kind = Callback::new(move |(id, kind, hl): (Uuid, String, Option<i32>)| {
        let doc = doc_kind.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let upd = BlockUpdate {
                kind: Some(kind),
                heading_level: Some(hl),
                ..Default::default()
            };
            if let Err(e) = repo.update(id, upd).await {
                tracing::warn!(?e, "block kind update failed");
            }
        });
    });
    let doc_move_up = doc.clone();
    let move_up = Callback::new(move |id: Uuid| {
        let doc = doc_move_up.clone();
        spawn(async move {
            if let Err(e) = move_block_async(&doc, id, -1).await {
                tracing::warn!(?e, "move-up failed");
            }
        });
    });
    let doc_move_down = doc.clone();
    let move_down = Callback::new(move |id: Uuid| {
        let doc = doc_move_down.clone();
        spawn(async move {
            if let Err(e) = move_block_async(&doc, id, 1).await {
                tracing::warn!(?e, "move-down failed");
            }
        });
    });
    let doc_focus_prev = doc.clone();
    let mut editing_focus_prev = editing_id;
    let focus_prev = Callback::new(move |id: Uuid| {
        let doc = doc_focus_prev.clone();
        spawn(async move {
            if let Some(prev) = neighbor_in_doc_order(&doc, id, -1).await {
                editing_focus_prev.set(Some(prev));
            }
        });
    });
    let doc_focus_next = doc.clone();
    let mut editing_focus_next = editing_id;
    let focus_next = Callback::new(move |id: Uuid| {
        let doc = doc_focus_next.clone();
        spawn(async move {
            if let Some(next) = neighbor_in_doc_order(&doc, id, 1).await {
                editing_focus_next.set(Some(next));
            }
        });
    });
    let doc_toggle = doc.clone();
    let toggle_collapsed = Callback::new(move |id: Uuid| {
        let doc = doc_toggle.clone();
        spawn(async move {
            let repo = BlockRepoLoro::new(&doc);
            let big = ListPage {
                index: 0,
                size: 100_000,
            };
            let all = match repo.list(big, None, None).await {
                Ok(l) => l.items,
                Err(_) => return,
            };
            let Some(target) = all.into_iter().find(|b| b.id == id) else {
                return;
            };
            let _ = repo
                .update(
                    id,
                    BlockUpdate {
                        collapsed: Some(!target.collapsed),
                        ..Default::default()
                    },
                )
                .await;
        });
    });
    let open_in_sidebar = Callback::new(move |id: Uuid| {
        let mut current = sidebar_stack.peek().clone();
        let entry = SidebarEntry::Block(id);
        if !current.contains(&entry) {
            current.insert(0, entry);
            sidebar_stack.set(current);
        }
    });
    let zoom_block = Callback::new(move |id: Uuid| {
        zoom_target.set(Some(id));
    });

    BlockOps {
        enter_edit,
        exit_edit,
        update_content,
        split_block,
        indent,
        outdent,
        delete_block,
        set_kind,
        move_up,
        move_down,
        focus_prev,
        focus_next,
        toggle_collapsed,
        open_in_sidebar,
        zoom_block,
    }
}

/// Swap `block_id` with its previous (`dir = -1`) or next
/// (`dir = 1`) sibling by exchanging their `sort_key` values.
/// No-op at the boundary (first / last sibling).
async fn move_block_async(
    doc: &CrdtDoc,
    block_id: Uuid,
    dir: i32,
) -> Result<(), knowledge_proto::architect::RepoError> {
    let repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let all = repo.list(big, None, None).await?.items;
    let target = match all.iter().find(|b| b.id == block_id) {
        Some(b) => b.clone(),
        None => return Err(knowledge_proto::architect::RepoError::NotFound),
    };
    let mut siblings: Vec<Block> = all
        .iter()
        .filter(|b| b.parent_block_id == target.parent_block_id && b.page_id == target.page_id)
        .cloned()
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id);
    let Some(pos) = pos else { return Ok(()) };
    let other_idx = if dir < 0 {
        if pos == 0 {
            return Ok(());
        }
        pos - 1
    } else {
        if pos + 1 >= siblings.len() {
            return Ok(());
        }
        pos + 1
    };
    let other = siblings[other_idx].clone();
    // Swap sort keys.
    repo.update(
        target.id,
        BlockUpdate {
            sort_key: Some(other.sort_key.clone()),
            ..Default::default()
        },
    )
    .await?;
    repo.update(
        other.id,
        BlockUpdate {
            sort_key: Some(target.sort_key.clone()),
            ..Default::default()
        },
    )
    .await?;
    Ok(())
}

/// Find the neighbor of `block_id` in document (flat) order
/// where dir=-1 → previous block, dir=1 → next. Document order
/// is a recursive top-down walk: parent then its children
/// before siblings.
async fn neighbor_in_doc_order(doc: &CrdtDoc, block_id: Uuid, dir: i32) -> Option<Uuid> {
    let repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let all = repo.list(big, None, None).await.ok()?.items;
    let target_page = all.iter().find(|b| b.id == block_id).map(|b| b.page_id)?;
    let page_blocks: Vec<Block> = all
        .into_iter()
        .filter(|b| b.page_id == target_page)
        .collect();
    let flat = flat_doc_order(&page_blocks);
    let pos = flat.iter().position(|id| *id == block_id)?;
    if dir < 0 {
        if pos == 0 {
            None
        } else {
            flat.get(pos - 1).copied()
        }
    } else {
        flat.get(pos + 1).copied()
    }
}

/// Flatten a page's blocks into document order: depth-first
/// traversal of the parent→child tree, siblings ordered by
/// sort_key.
pub(crate) fn flat_doc_order(blocks: &[Block]) -> Vec<Uuid> {
    use std::collections::HashMap;
    let mut by_parent: HashMap<Option<Uuid>, Vec<Block>> = HashMap::new();
    for b in blocks {
        by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b.clone());
    }
    for v in by_parent.values_mut() {
        v.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    let mut out = Vec::new();
    fn walk(parent: Option<Uuid>, map: &HashMap<Option<Uuid>, Vec<Block>>, out: &mut Vec<Uuid>) {
        if let Some(children) = map.get(&parent) {
            for c in children {
                out.push(c.id);
                walk(Some(c.id), map, out);
            }
        }
    }
    walk(None, &by_parent, &mut out);
    out
}

/// Split `block_id` at byte offset `offset`. Truncates the
/// current block to `[..offset]`, creates a new sibling whose
/// content is `[offset..]`, placed immediately after the current
/// block via a lexorank between current and next sibling.
async fn split_block_async(
    doc: &CrdtDoc,
    block_id: Uuid,
    offset: usize,
) -> Result<Uuid, knowledge_proto::architect::RepoError> {
    let repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let all = repo.list(big, None, None).await?.items;
    let target = match all.iter().find(|b| b.id == block_id) {
        Some(b) => b.clone(),
        None => return Err(knowledge_proto::architect::RepoError::NotFound),
    };
    let content = target.content.clone();
    let off = offset.min(content.len());
    let (left, right) = content.split_at(off);

    // Update current block: keep only the left half.
    repo.update(
        target.id,
        BlockUpdate {
            content: Some(left.to_string()),
            ..Default::default()
        },
    )
    .await?;

    // Lexorank between current and the next sibling.
    let siblings: Vec<&Block> = all
        .iter()
        .filter(|b| b.parent_block_id == target.parent_block_id && b.page_id == target.page_id)
        .collect();
    let mut sorted = siblings.clone();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let next_key: Option<String> = sorted
        .iter()
        .skip_while(|b| b.id != target.id)
        .nth(1)
        .map(|b| b.sort_key.clone());
    let new_sort = match next_key {
        Some(n) => lexorank_between(&target.sort_key, &n),
        None => lexorank_after(&target.sort_key),
    };

    let new = repo
        .create(BlockCreate {
            vault_id: target.vault_id,
            page_id: target.page_id,
            parent_block_id: target.parent_block_id,
            sort_key: new_sort,
            kind: "paragraph".into(),
            content: right.to_string(),
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
        .await?;
    Ok(new.id)
}

/// Indent `block_id`: reparent it under its previous sibling.
/// No-op if there's no previous sibling (already the first
/// child of its parent — can't indent further without changing
/// the document model).
async fn indent_block_async(
    doc: &CrdtDoc,
    block_id: Uuid,
) -> Result<(), knowledge_proto::architect::RepoError> {
    let repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let all = repo.list(big, None, None).await?.items;
    let target = match all.iter().find(|b| b.id == block_id) {
        Some(b) => b.clone(),
        None => return Err(knowledge_proto::architect::RepoError::NotFound),
    };
    let mut siblings: Vec<&Block> = all
        .iter()
        .filter(|b| b.parent_block_id == target.parent_block_id && b.page_id == target.page_id)
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id);
    let Some(pos) = pos else {
        return Ok(());
    };
    if pos == 0 {
        return Ok(());
    }
    let new_parent = siblings[pos - 1].id;
    // Pick a sort_key after the new parent's last child.
    let last_child_key = all
        .iter()
        .filter(|b| b.parent_block_id == Some(new_parent))
        .map(|b| b.sort_key.clone())
        .max();
    let new_sort = match last_child_key {
        Some(k) => lexorank_after(&k),
        None => "m".into(),
    };
    repo.update(
        target.id,
        BlockUpdate {
            parent_block_id: Some(Some(new_parent)),
            sort_key: Some(new_sort),
            ..Default::default()
        },
    )
    .await?;
    Ok(())
}

/// Outdent `block_id`: reparent it to its grandparent, placed
/// just after its current parent. No-op when the block is
/// already a top-level child (no grandparent to reparent to).
async fn outdent_block_async(
    doc: &CrdtDoc,
    block_id: Uuid,
) -> Result<(), knowledge_proto::architect::RepoError> {
    let repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };
    let all = repo.list(big, None, None).await?.items;
    let target = match all.iter().find(|b| b.id == block_id) {
        Some(b) => b.clone(),
        None => return Err(knowledge_proto::architect::RepoError::NotFound),
    };
    let Some(parent_id) = target.parent_block_id else {
        return Ok(());
    };
    let parent = match all.iter().find(|b| b.id == parent_id) {
        Some(b) => b.clone(),
        None => return Ok(()),
    };
    // Place after parent in grandparent's child list.
    let grandparent = parent.parent_block_id;
    let mut grand_children: Vec<&Block> = all
        .iter()
        .filter(|b| b.parent_block_id == grandparent && b.page_id == target.page_id)
        .collect();
    grand_children.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let parent_pos = grand_children.iter().position(|b| b.id == parent.id);
    let next_key = parent_pos
        .and_then(|p| grand_children.get(p + 1))
        .map(|b| b.sort_key.clone());
    let new_sort = match next_key {
        Some(n) => lexorank_between(&parent.sort_key, &n),
        None => lexorank_after(&parent.sort_key),
    };
    repo.update(
        target.id,
        BlockUpdate {
            parent_block_id: Some(grandparent),
            sort_key: Some(new_sort),
            ..Default::default()
        },
    )
    .await?;
    Ok(())
}

/// Lexorank "after" — append `m` until the result sorts strictly
/// after `prev`. Sufficient for our needs (we generate keys
/// densely enough that we won't run out of fractional space).
pub(crate) fn lexorank_after(prev: &str) -> String {
    let mut s = prev.to_string();
    s.push('m');
    s
}

/// Lexorank "between" — find a string that sorts strictly
/// between `a` and `b`. Simplest stable strategy: pad shorter
/// string with `a` and append `m`. Falls back to `a + m` when
/// `b` doesn't strictly compare greater.
pub(crate) fn lexorank_between(a: &str, b: &str) -> String {
    if a >= b {
        return lexorank_after(a);
    }
    // Find a strictly-between key by appending `m` to `a`.
    // Generally `a + "m"` sorts after `a` and before `b` when
    // `b` is `a + something` or strictly greater.
    let candidate = format!("{a}m");
    if candidate.as_str() < b {
        return candidate;
    }
    // Otherwise pad `a` with a character before `b`'s diverging
    // char. Simple bisection: insert midpoint between last char
    // of `a` and the equivalent in `b`.
    let mut out = String::new();
    let ab = a.bytes();
    let bb = b.bytes();
    for (i, (ca, cb)) in ab.zip(bb).enumerate() {
        if ca == cb {
            out.push(ca as char);
            continue;
        }
        let mid = ca + (cb - ca) / 2;
        if mid > ca {
            out.push(mid as char);
        } else {
            // Same byte after midpoint; recurse one char deeper.
            out.push(ca as char);
            out.push('m');
        }
        let _ = i;
        return out;
    }
    // `a` is a prefix of `b`. Append `m` to `a`.
    out.push_str(a);
    out.push('m');
    out
}

/// Filesystem path the desktop binary persists the vault to.
/// Lives at `$HOME/.task-desktop/vault.loro` on Linux/macOS.
#[cfg(not(target_arch = "wasm32"))]
fn persistence_path() -> std::path::PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".into());
    std::path::PathBuf::from(home)
        .join(".task-desktop")
        .join("vault.loro")
}

/// Read the persisted Loro snapshot, if any. Returns the bytes
/// or `None` on missing file / read error.
#[cfg(not(target_arch = "wasm32"))]
async fn load_persisted_snapshot() -> Option<Vec<u8>> {
    let path = persistence_path();
    match tokio::fs::read(&path).await {
        Ok(b) => {
            tracing::info!(path = %path.display(), bytes = b.len(), "loaded vault snapshot");
            Some(b)
        }
        Err(_) => None,
    }
}

/// Subscribe to local commits and save a snapshot whenever the
/// doc changes. Debounces saves to at most one per 500ms so a
/// burst of edits doesn't thrash the filesystem.
#[cfg(not(target_arch = "wasm32"))]
async fn run_persistence_loop(doc: Arc<CrdtDoc>) {
    use futures::StreamExt;
    let (tx, mut rx) = futures::channel::mpsc::unbounded::<()>();
    let sub = doc.loro().subscribe_local_update(Box::new(move |_b| {
        let _ = tx.unbounded_send(());
        true
    }));
    std::mem::forget(sub);
    let path = persistence_path();
    if let Some(parent) = path.parent() {
        let _ = tokio::fs::create_dir_all(parent).await;
    }
    loop {
        // Wait for first event.
        if rx.next().await.is_none() {
            return;
        }
        // Coalesce subsequent events for 500ms before saving.
        let _ = tokio::time::timeout(std::time::Duration::from_millis(500), async {
            while rx.next().await.is_some() {}
        })
        .await;
        let bytes = match doc.loro().export(loro::ExportMode::Snapshot) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!(?e, "snapshot export failed");
                continue;
            }
        };
        if let Err(e) = tokio::fs::write(&path, &bytes).await {
            tracing::warn!(?e, path = %path.display(), "snapshot write failed");
        }
    }
}

/// Export one page's block tree as Markdown.
///
/// Indent units are two spaces per nesting level, mirroring the
/// shape Logseq writes on disk. Headings render via `# ` prefix;
/// list_item / paragraph kinds use `- `; code blocks fence with
/// ```` ```lang ```` / ` ``` `. Output preserves source text
/// verbatim — inline markup is already encoded in `content`.
pub fn export_page_markdown(page: &Page, blocks: &[Block]) -> String {
    let mut sorted = blocks.to_vec();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let tree = build_block_tree(&sorted);
    let mut out = String::new();
    out.push_str(&format!("# {}\n\n", page.basename));
    for node in tree {
        write_block_markdown(&node, 0, &mut out);
    }
    out
}

fn write_block_markdown(node: &BlockNodeTree, depth: usize, out: &mut String) {
    let indent = "  ".repeat(depth);
    match node.block.kind.as_str() {
        "heading" => {
            let level = node.block.heading_level.unwrap_or(1).clamp(1, 6) as usize;
            out.push_str(&indent);
            out.push_str(&"#".repeat(level));
            out.push(' ');
            out.push_str(&node.block.content);
            out.push('\n');
        }
        "code" => {
            let lang = node.block.code_lang.as_deref().unwrap_or("");
            out.push_str(&indent);
            out.push_str("```");
            out.push_str(lang);
            out.push('\n');
            out.push_str(&node.block.content);
            out.push('\n');
            out.push_str(&indent);
            out.push_str("```\n");
        }
        _ => {
            // Paragraph or list_item — both render as a bullet
            // line so the markdown round-trips cleanly through
            // Logseq's reader.
            out.push_str(&indent);
            out.push_str("- ");
            // Multi-line block content: indent continuation lines
            // so they belong to the same bullet.
            let mut first = true;
            for line in node.block.content.lines() {
                if first {
                    out.push_str(line);
                    out.push('\n');
                    first = false;
                } else {
                    out.push_str(&indent);
                    out.push_str("  ");
                    out.push_str(line);
                    out.push('\n');
                }
            }
            if first {
                // Empty block — emit a placeholder so structure
                // stays.
                out.push('\n');
            }
        }
    }
    for child in &node.children {
        write_block_markdown(child, depth + 1, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peel_fenced_code_basic() {
        let (lang, body) = peel_fenced_code("```rust\nfn x() {}\n```").unwrap();
        assert_eq!(lang, "rust");
        assert_eq!(body, "fn x() {}");
    }

    #[test]
    fn peel_fenced_code_no_lang() {
        let (lang, body) = peel_fenced_code("```\nplain text\n```").unwrap();
        assert_eq!(lang, "");
        assert_eq!(body, "plain text");
    }

    #[test]
    fn peel_fenced_code_rejects_inline() {
        assert!(peel_fenced_code("just `code` here").is_none());
    }

    #[test]
    fn parse_logbook_closed_and_open() {
        let body = "CLOCK: [2026-05-19 Tue 10:30:00]--[2026-05-19 Tue 11:15:00] => 0:45\nCLOCK: [2026-05-19 Tue 14:00:00]";
        let entries = parse_logbook(body);
        assert_eq!(entries.len(), 2);
        assert!(entries[0].end.is_some());
        assert!(entries[1].end.is_none());
        assert_eq!(
            (entries[0].end.unwrap() - entries[0].start).num_minutes(),
            45
        );
    }

    #[test]
    fn parse_logbook_skips_garbage() {
        let entries =
            parse_logbook("nonsense\nCLOCK: missing brackets\nCLOCK: [2026-05-19 10:00:00]");
        assert_eq!(entries.len(), 1);
    }

    #[test]
    fn peel_table_basic() {
        let rows = peel_table("| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |").unwrap();
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0], vec!["a", "b"]);
        assert_eq!(rows[1], vec!["1", "2"]);
        assert_eq!(rows[2], vec!["3", "4"]);
    }

    #[test]
    fn peel_table_rejects_non_table() {
        assert!(peel_table("not a table").is_none());
        assert!(peel_table("| a | b |\nno separator\n| 1 | 2 |").is_none());
    }

    /// Helper: build an ephemeral CrdtDoc + seed a single page,
    /// returning (doc, page_id, vault_id).
    async fn ephemeral_page() -> (Arc<CrdtDoc>, Uuid, Uuid) {
        use knowledge_proto::{VaultCreate, VaultRepo};
        let doc = Arc::new(CrdtDoc::ephemeral());
        let vr = knowledge_crdt::VaultRepoLoro::new(&doc);
        let v = vr
            .create(VaultCreate {
                name: "T".into(),
                root_path: None,
                use_markdown_links: false,
                new_link_format: "shortest".into(),
                attachment_folder_path: String::new(),
                default_view_mode: "live-preview".into(),
                config_json: "{}".into(),
            })
            .await
            .unwrap();
        let pr = PageRepoLoro::new(&doc);
        let now = Utc::now();
        let p = pr
            .create(knowledge_proto::PageCreate {
                vault_id: v.id,
                folder_id: None,
                path: "p.md".into(),
                basename: "p".into(),
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
            .unwrap();
        (doc, p.id, v.id)
    }

    async fn mk_b(
        doc: &CrdtDoc,
        vault_id: Uuid,
        page_id: Uuid,
        parent: Option<Uuid>,
        sort: &str,
        content: &str,
    ) -> Uuid {
        let br = BlockRepoLoro::new(doc);
        let b = br
            .create(BlockCreate {
                vault_id,
                page_id,
                parent_block_id: parent,
                sort_key: sort.into(),
                kind: "paragraph".into(),
                content: content.into(),
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
            .unwrap();
        b.id
    }

    async fn all_blocks_async(doc: &CrdtDoc) -> Vec<Block> {
        let br = BlockRepoLoro::new(doc);
        let big = ListPage {
            index: 0,
            size: 100_000,
        };
        br.list(big, None, None).await.unwrap().items
    }

    #[tokio::test]
    async fn split_block_creates_sibling_with_right_half() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let id = mk_b(&doc, vault_id, page_id, None, "m", "Hello world").await;
        let new_id = split_block_async(&doc, id, 5).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let orig = blocks.iter().find(|b| b.id == id).unwrap();
        let nu = blocks.iter().find(|b| b.id == new_id).unwrap();
        assert_eq!(orig.content, "Hello");
        assert_eq!(nu.content, " world");
        assert_eq!(orig.parent_block_id, nu.parent_block_id);
        assert!(nu.sort_key > orig.sort_key);
    }

    #[tokio::test]
    async fn indent_reparents_under_previous_sibling() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "first").await;
        let b = mk_b(&doc, vault_id, page_id, None, "b", "second").await;
        indent_block_async(&doc, b).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let updated = blocks.iter().find(|x| x.id == b).unwrap();
        assert_eq!(updated.parent_block_id, Some(a));
    }

    #[tokio::test]
    async fn indent_first_sibling_is_noop() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "first").await;
        indent_block_async(&doc, a).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let updated = blocks.iter().find(|x| x.id == a).unwrap();
        assert_eq!(updated.parent_block_id, None);
    }

    #[tokio::test]
    async fn outdent_reparents_to_grandparent() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "parent").await;
        let b = mk_b(&doc, vault_id, page_id, Some(a), "a", "child").await;
        outdent_block_async(&doc, b).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let updated = blocks.iter().find(|x| x.id == b).unwrap();
        assert_eq!(updated.parent_block_id, None);
        let aa = blocks.iter().find(|x| x.id == a).unwrap();
        assert!(updated.sort_key > aa.sort_key);
    }

    #[tokio::test]
    async fn outdent_top_level_is_noop() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "m", "x").await;
        outdent_block_async(&doc, a).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let updated = blocks.iter().find(|x| x.id == a).unwrap();
        assert_eq!(updated.parent_block_id, None);
    }

    #[tokio::test]
    async fn move_block_swaps_with_sibling() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "first").await;
        let b = mk_b(&doc, vault_id, page_id, None, "b", "second").await;
        move_block_async(&doc, b, -1).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let aa = blocks.iter().find(|x| x.id == a).unwrap();
        let bb = blocks.iter().find(|x| x.id == b).unwrap();
        assert!(bb.sort_key < aa.sort_key);
    }

    #[tokio::test]
    async fn move_block_at_boundary_is_noop() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "first").await;
        let _b = mk_b(&doc, vault_id, page_id, None, "b", "second").await;
        move_block_async(&doc, a, -1).await.unwrap();
        let blocks = all_blocks_async(&doc).await;
        let aa = blocks.iter().find(|x| x.id == a).unwrap();
        assert_eq!(aa.sort_key, "a");
    }

    #[tokio::test]
    async fn neighbor_in_doc_order_walks_tree() {
        let (doc, page_id, vault_id) = ephemeral_page().await;
        let a = mk_b(&doc, vault_id, page_id, None, "a", "a").await;
        let b = mk_b(&doc, vault_id, page_id, None, "b", "b").await;
        let a1 = mk_b(&doc, vault_id, page_id, Some(a), "a", "a1").await;
        let next = neighbor_in_doc_order(&doc, a, 1).await;
        assert_eq!(next, Some(a1));
        let prev = neighbor_in_doc_order(&doc, b, -1).await;
        assert_eq!(prev, Some(a1));
    }

    #[test]
    fn export_page_markdown_renders_tree() {
        let now = Utc::now();
        let page_id = Uuid::new_v4();
        let parent_id = Uuid::new_v4();
        let child_id = Uuid::new_v4();
        let mk = |id, parent, kind: &str, content: &str, hl: Option<i32>| Block {
            id,
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: parent,
            sort_key: "a".into(),
            kind: kind.into(),
            content: content.into(),
            heading_level: hl,
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
            created_at: now,
            updated_at: now,
        };
        let blocks = vec![
            mk(parent_id, None, "heading", "Top", Some(1)),
            mk(child_id, Some(parent_id), "paragraph", "Hello", None),
        ];
        let page = Page {
            id: page_id,
            vault_id: Uuid::nil(),
            folder_id: None,
            path: "p.md".into(),
            basename: "P".into(),
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
            created_at: now,
            updated_at: now,
        };
        let md = export_page_markdown(&page, &blocks);
        assert!(md.contains("# P"));
        assert!(md.contains("# Top"));
        assert!(md.contains("  - Hello"));
    }

    #[test]
    fn extract_tags_basic() {
        let tags = extract_tags("Hello #demo and #foo/bar end");
        assert_eq!(tags, vec!["demo".to_string(), "foo/bar".to_string()]);
    }

    #[test]
    fn extract_tags_skips_in_word() {
        let tags = extract_tags("not#a tag here");
        assert!(tags.is_empty());
    }

    #[test]
    fn snippet_truncates() {
        let s = "a".repeat(120);
        let got = snippet(&s, 80);
        assert!(got.chars().count() <= 81);
        assert!(got.ends_with('…'));
    }

    #[test]
    fn trigger_after_boundary_finds_slash_at_start() {
        assert_eq!(trigger_after_boundary("/heading", '/'), Some(0));
    }

    #[test]
    fn trigger_after_boundary_finds_slash_after_space() {
        assert_eq!(trigger_after_boundary("hello /h2", '/'), Some(6));
    }

    #[test]
    fn trigger_after_boundary_ignores_inner_slash() {
        assert_eq!(trigger_after_boundary("a/b/c", '/'), None);
    }

    #[test]
    fn trigger_after_boundary_returns_last() {
        // Two valid trigger positions; should return the more recent one.
        assert_eq!(trigger_after_boundary("first / and /second", '/'), Some(12));
    }

    #[test]
    fn filter_slash_matches_case_insensitive() {
        let hits = filter_slash("todo");
        assert!(hits.iter().any(|c| c.label == "TODO"));
    }

    #[test]
    fn filter_slash_empty_query_returns_all() {
        let hits = filter_slash("");
        assert_eq!(hits.len(), 10); // capped at 10 even though catalog is 11
    }

    #[test]
    fn lexorank_after_strictly_greater() {
        let a = "m";
        let after = lexorank_after(a);
        assert!(after > a.to_string());
    }

    #[test]
    fn lexorank_between_strictly_between() {
        let a = "a";
        let b = "c";
        let mid = lexorank_between(a, b);
        assert!(mid > a.to_string());
        assert!(mid < b.to_string());
    }

    #[test]
    fn lexorank_between_handles_adjacent() {
        let a = "m";
        let b = "n";
        let mid = lexorank_between(a, b);
        assert!(mid > a.to_string());
        assert!(mid < b.to_string());
    }

    #[test]
    fn lexorank_between_falls_back_when_a_gte_b() {
        let a = "z";
        let b = "a";
        let mid = lexorank_between(a, b);
        // Spec: when a >= b, returns lexorank_after(a) — still
        // sorts after `a` (degraded but stable).
        assert!(mid > a.to_string());
    }

    #[test]
    fn find_subtree_returns_match() {
        let now = Utc::now();
        let page_id = Uuid::new_v4();
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        let child = Uuid::new_v4();
        let mk = |id: Uuid, parent: Option<Uuid>, key: &str| Block {
            id,
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: parent,
            sort_key: key.into(),
            kind: "paragraph".into(),
            content: format!("{id}"),
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
            created_at: now,
            updated_at: now,
        };
        let blocks = vec![mk(a, None, "a"), mk(b, None, "b"), mk(child, Some(a), "a")];
        let tree = build_block_tree(&blocks);
        let found = find_subtree(&tree, child).expect("subtree present");
        assert_eq!(found.block.id, child);
    }

    #[test]
    fn nld_today_yesterday_tomorrow() {
        let today = chrono::Local::now().date_naive();
        assert_eq!(nld_to_date("today"), Some(today));
        assert_eq!(
            nld_to_date(" Tomorrow "),
            Some(today + chrono::Duration::days(1))
        );
        assert_eq!(
            nld_to_date("yesterday"),
            Some(today - chrono::Duration::days(1))
        );
        assert_eq!(nld_to_date("now"), Some(today));
    }

    #[test]
    fn nld_offset_days() {
        let today = chrono::Local::now().date_naive();
        assert_eq!(nld_to_date("+7"), Some(today + chrono::Duration::days(7)));
        assert_eq!(nld_to_date("-30"), Some(today - chrono::Duration::days(30)));
    }

    #[test]
    fn nld_iso_passthrough() {
        let d = chrono::NaiveDate::from_ymd_opt(2026, 5, 19).unwrap();
        assert_eq!(nld_to_date("2026-05-19"), Some(d));
        assert_eq!(journal_title(d), "2026-05-19");
        assert_eq!(parse_journal_title("2026-05-19"), Some(d));
    }

    #[test]
    fn nld_invalid_returns_none() {
        assert_eq!(nld_to_date("never"), None);
        assert_eq!(nld_to_date("+notanumber"), None);
        assert_eq!(parse_journal_title("not-a-date"), None);
    }

    #[test]
    fn flat_doc_order_depth_first() {
        let now = Utc::now();
        let page_id = Uuid::new_v4();
        let mk = |id: Uuid, parent: Option<Uuid>, key: &str| Block {
            id,
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: parent,
            sort_key: key.into(),
            kind: "paragraph".into(),
            content: "".into(),
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
            created_at: now,
            updated_at: now,
        };
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        let a1 = Uuid::new_v4();
        let a2 = Uuid::new_v4();
        let blocks = vec![
            mk(a, None, "a"),
            mk(b, None, "b"),
            mk(a1, Some(a), "a"),
            mk(a2, Some(a), "b"),
        ];
        let order = flat_doc_order(&blocks);
        // Expected: a, a1, a2, b
        assert_eq!(order, vec![a, a1, a2, b]);
    }

    #[test]
    fn build_block_tree_nests_by_parent() {
        let vault_id = Uuid::nil();
        let page_id = Uuid::new_v4();
        let parent_id = Uuid::new_v4();
        let child_id = Uuid::new_v4();
        let now = Utc::now();
        let mk = |id: Uuid, parent: Option<Uuid>, sort_key: &str| Block {
            id,
            vault_id,
            page_id,
            parent_block_id: parent,
            sort_key: sort_key.into(),
            kind: "paragraph".into(),
            content: format!("{id}"),
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
            created_at: now,
            updated_at: now,
        };
        let blocks = vec![mk(parent_id, None, "a"), mk(child_id, Some(parent_id), "a")];
        let tree = build_block_tree(&blocks);
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].block.id, parent_id);
        assert_eq!(tree[0].children.len(), 1);
        assert_eq!(tree[0].children[0].block.id, child_id);
    }
}
