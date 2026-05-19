//! Logseq-style recursive outliner with view/edit-mode swap.
//!
//! Architecture mirrors Logseq:
//! - The whole page is a tree of blocks rendered as **static
//!   inline-markdown** (`BlockView`) by default.
//! - At any time at most ONE block is the "active editor" and
//!   renders a `<textarea>` instead. The active block is identified
//!   by [`Outliner::editing_id`] (a `Signal<Option<Uuid>>`).
//! - Click a view-mode block → that block becomes the active
//!   editor. Arrow / Enter navigation moves the active id to the
//!   prev/next visible block. Click on a `[[Page]]` link in view
//!   mode → navigate (does not enter edit mode).
//!
//! Inline parsing comes from [`crate::inline_md`].

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    ChevronRight, CircleAlert, CircleCheck, Copy, Info, Lightbulb, Quote, TriangleAlert,
};
use knowledge_proto::Block;
use uuid::Uuid;
use vim::{DioxusKey as VimKey, VimMode};

use crate::inline_md::{self, Inline};

/// Snapshot indices passed via context to `{{query}}` renderers
/// so they can resolve hits without a callback chain.
#[derive(Clone, Default, PartialEq)]
pub struct QueryIndex {
    pub block_refs: std::sync::Arc<Vec<knowledge_proto::BlockRefEdge>>,
    pub block_props: std::sync::Arc<Vec<knowledge_proto::BlockPropEdge>>,
    /// All visible blocks in the vault — for the `{{query}}`
    /// (no body) "list everything" form.
    pub all_blocks: std::sync::Arc<Vec<(Uuid, Uuid, String)>>, // (block_id, page_id, snippet)
    /// page_id → display basename, for rendering hit row labels.
    pub page_titles: std::sync::Arc<std::collections::HashMap<Uuid, String>>,
}

/// Bundle of state + callbacks the active `BlockEditor` needs
/// to render its inline popups (wikilink autocomplete + slash
/// command palette) anchored just below the textarea. Provided
/// by `PageBody` via context so popups follow the active
/// editor without prop-drilling through Outliner.
#[derive(Clone)]
pub struct BlockPopupCtx {
    pub autocomplete: Signal<Option<(Uuid, String)>>,
    pub slash: Signal<Option<(Uuid, String)>>,
    pub all_pages: std::sync::Arc<Vec<String>>,
    pub on_pick_page: Callback<String>,
    pub on_pick_slash: Callback<SlashCommand>,
}

/// Context-provided "pin this page in the right sidebar"
/// callback. Triggered by shift-click on a `[[Page]]` link.
#[derive(Clone, Copy)]
pub struct PinPaneCb(pub Callback<String>);

/// Context-provided navigate-link callback so deep components
/// (embeds, recursive inline renderers) can route a `[[Foo]]`
/// click to the host's page-select handler without prop
/// drilling. Set by the route component.
#[derive(Clone, Copy)]
pub struct NavigateLinkCb(pub Callback<String>);

/// Context-provided lookup from block id → (block, all of its
/// page's blocks for child resolution). Powers `((uuid))` block
/// expansion. Empty default → expansion just shows "block not
/// found".
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BlockTreeLookup(
    pub std::sync::Arc<std::collections::HashMap<Uuid, (Block, Vec<Block>)>>,
);

impl BlockTreeLookup {
    pub fn new(by_id: std::collections::HashMap<Uuid, (Block, Vec<Block>)>) -> Self {
        Self(std::sync::Arc::new(by_id))
    }
    pub fn get(&self, id: Uuid) -> Option<(Block, Vec<Block>)> {
        self.0.get(&id).cloned()
    }
}

/// DFS-visible-order index of the current page's blocks, plus a
/// snapshot of their raw text content. Powers cross-block
/// remote-cursor selection rendering: each block can answer
/// "where am I in the visible order?" and "what is the text of
/// peer X's anchor or head block?".
#[derive(Clone, Default, PartialEq)]
pub struct BlockOrderIndex {
    pub order: std::sync::Arc<Vec<Uuid>>,
    pub content: std::sync::Arc<std::collections::HashMap<Uuid, String>>,
}

impl BlockOrderIndex {
    pub fn from_blocks(blocks: &[Block]) -> Self {
        let order = flatten_visible(blocks);
        let content = blocks
            .iter()
            .map(|b| (b.id, b.content.clone()))
            .collect::<std::collections::HashMap<_, _>>();
        Self {
            order: std::sync::Arc::new(order),
            content: std::sync::Arc::new(content),
        }
    }
    pub fn position_of(&self, id: Uuid) -> Option<usize> {
        self.order.iter().position(|x| *x == id)
    }
    pub fn content_of(&self, id: Uuid) -> Option<&String> {
        self.content.get(&id)
    }
}

/// Context-provided lookup from page basename (lowercased) →
/// the page's blocks (already sorted by sort_key). Powers
/// `![[Page]]` embed rendering. Empty default = no embeds
/// resolve.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct PageBlockLookup(pub std::sync::Arc<std::collections::HashMap<String, Vec<Block>>>);

impl PageBlockLookup {
    pub fn new(by_basename: std::collections::HashMap<String, Vec<Block>>) -> Self {
        Self(std::sync::Arc::new(by_basename))
    }
    pub fn blocks_for(&self, basename: &str) -> Option<Vec<Block>> {
        self.0.get(&basename.to_lowercase()).cloned()
    }
}

/// Bound recursion depth for embed rendering (`![[Foo]]` →
/// shows Foo's blocks → some of which might themselves contain
/// embeds). Past this depth we render a "depth limit" stub
/// instead of recursing further.
const EMBED_MAX_DEPTH: usize = 2;

/// Context counter for the current embed-render depth. Components
/// that recurse into an embed bump this when re-rendering.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct EmbedDepth(pub usize);

/// Context-provided lookup from block id → first-line snippet,
/// used to render `((uuid))` block-ref chips with meaningful
/// content. Empty default → chips fall back to the short uuid.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BlockSnippets(pub std::sync::Arc<std::collections::HashMap<Uuid, String>>);

impl BlockSnippets {
    pub fn from_blocks(blocks: &[Block]) -> Self {
        let mut m = std::collections::HashMap::with_capacity(blocks.len());
        for b in blocks {
            m.insert(b.id, b.content.lines().next().unwrap_or("").to_string());
        }
        Self(std::sync::Arc::new(m))
    }
    pub fn snippet(&self, id: Uuid) -> Option<String> {
        self.0.get(&id).cloned()
    }
}

/// Context-provided set of known page basenames (lowercased) so
/// `[[wikilink]]` rendering can flag broken links inline. Set by
/// the route-level component; defaults to "everything resolves"
/// when missing.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct KnownBasenames(pub std::sync::Arc<std::collections::HashSet<String>>);

impl KnownBasenames {
    pub fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(std::sync::Arc::new(
            iter.into_iter().map(|s| s.to_lowercase()).collect(),
        ))
    }
    pub fn knows(&self, basename: &str) -> bool {
        self.0.is_empty() || self.0.contains(&basename.to_lowercase())
    }
}

/// Direction for [`OutlinerOps::on_move`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MoveDir {
    Up,
    Down,
}

/// Cross-block focus traversal direction for [`OutlinerOps::on_focus_relative`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FocusDir {
    Prev,
    Next,
}

/// Bundle of mutation + focus callbacks the outliner needs.
#[derive(Clone, Copy, PartialEq)]
pub struct OutlinerOps {
    pub on_edit: Callback<(Uuid, String)>,
    pub on_insert_after: Callback<Uuid>,
    pub on_delete: Callback<Uuid>,
    pub on_indent: Callback<Uuid>,
    pub on_outdent: Callback<Uuid>,
    pub on_toggle_collapsed: Callback<Uuid>,
    pub on_move: Callback<(Uuid, MoveDir)>,
    /// Move the active editor to the prev/next visible block.
    pub on_focus_relative: Callback<(Uuid, FocusDir)>,
    /// Click on a `[[Page]]` link in view mode — caller looks up
    /// the basename and navigates.
    pub on_navigate_link: Callback<String>,
    /// A vim key was captured by a block in Normal/Visual mode (or
    /// by the `<textarea>` in Insert mode for the few keys we
    /// intercept). Caller feeds it to the vim engine and applies
    /// the resulting actions.
    pub on_vim_key: Callback<VimKey>,
    /// User clicked on a block at a specific character offset
    /// (computed via `caretPositionFromPoint` in JS). Caller
    /// places the document cursor at that offset, focuses the
    /// block, and switches the vim engine to Insert mode.
    pub on_click_at_offset: Callback<(Uuid, usize)>,
}

/// One-click caret positioning. The `BlockView` click handler
/// stashes the click's pixel coords here; the `BlockEditor`'s
/// `on_mount` reads them and dispatches a synthetic click on
/// the textarea at those coords so the browser can place the
/// caret natively. This avoids the offset-against-rendered-DOM
/// problem (where `**bold**` shows as `bold` and any caret
/// offset computed from the rendered text doesn't match the
/// source-text offset the textarea wants).
///
/// Block ID is included so a stale pending click doesn't apply
/// to the wrong block if signals fire out of order.
#[derive(Clone, Copy, Default)]
pub struct PendingClick(pub dioxus::prelude::Signal<Option<(Uuid, f64, f64)>>);

/// Flatten the visible tree into render order so the caller can
/// compute prev/next. Skips children of collapsed parents.
pub fn flatten_visible(blocks: &[Block]) -> Vec<Uuid> {
    let mut by_parent: HashMap<Option<Uuid>, Vec<&Block>> = HashMap::new();
    for b in blocks {
        by_parent.entry(b.parent_block_id).or_default().push(b);
    }
    for siblings in by_parent.values_mut() {
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    let mut out = Vec::with_capacity(blocks.len());
    fn walk<'a>(
        parent: Option<Uuid>,
        by_parent: &HashMap<Option<Uuid>, Vec<&'a Block>>,
        out: &mut Vec<Uuid>,
    ) {
        if let Some(kids) = by_parent.get(&parent) {
            for k in kids {
                out.push(k.id);
                if !k.collapsed {
                    walk(Some(k.id), by_parent, out);
                }
            }
        }
    }
    walk(None, &by_parent, &mut out);
    out
}

#[component]
pub fn Outliner(
    blocks: Vec<Block>,
    ops: OutlinerOps,
    editing_id: Signal<Option<Uuid>>,
    vim_mode: VimMode,
) -> Element {
    let mut by_parent: HashMap<Option<Uuid>, Vec<Block>> = HashMap::new();
    for b in &blocks {
        by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b.clone());
    }
    for siblings in by_parent.values_mut() {
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }

    let roots = by_parent.get(&None).cloned().unwrap_or_default();
    if roots.is_empty() {
        // Logseq-style: an empty page seeds its first block via
        // an effect at the PageBody level (`auto_seeded`). The
        // first snapshot after seeding renders the real block,
        // so this branch is only visible for a frame or two and
        // shouldn't carry a placeholder. Empty container keeps
        // the layout stable.
        return rsx! {
            div { "data-testid": "outliner-empty" }
        };
    }
    rsx! {
        div {
            "data-testid": "outliner",
            class: "flex flex-col",
            for root in roots {
                OutlinerNode {
                    key: "{root.id}",
                    block: root.clone(),
                    children_by_parent: by_parent.clone(),
                    depth: 0,
                    ops,
                    editing_id,
                    vim_mode,
                }
            }
        }
    }
}

#[component]
fn OutlinerNode(
    block: Block,
    children_by_parent: HashMap<Option<Uuid>, Vec<Block>>,
    depth: usize,
    ops: OutlinerOps,
    editing_id: Signal<Option<Uuid>>,
    vim_mode: VimMode,
) -> Element {
    let id = block.id;
    let row_testid = format!("outliner-row-{id}");
    let chevron_testid = format!("outliner-fold-{id}");
    let kind = block.kind.clone();
    let collapsed = block.collapsed;
    let children = children_by_parent
        .get(&Some(id))
        .cloned()
        .unwrap_or_default();
    let has_children = !children.is_empty();
    let is_active = *editing_id.read() == Some(id);
    let in_insert = vim_mode == VimMode::Insert;
    let show_editor = is_active && in_insert;
    let show_normal = is_active && !in_insert;

    let on_enter_edit = use_callback(move |target_id: Uuid| {
        editing_id.set(Some(target_id));
    });

    rsx! {
        div {
            "data-testid": row_testid,
            "data-block-kind": "{kind}",
            "data-block-depth": "{depth}",
            "data-editing": "{is_active}",
            "data-vim-mode": match vim_mode { VimMode::Normal => "normal", VimMode::Insert => "insert", VimMode::Visual => "visual", VimMode::VisualLine => "visual-line", VimMode::Command => "command", VimMode::Search => "search" },
            // Highlight via a thin left-border accent — doesn't
            // shift content position the way bg fill + padding
            // would, so the view↔edit swap stays metric-stable.
            class: if is_active && !in_insert {
                "group/row relative flex items-start gap-1 pl-1.5 pr-0.5 border-l-2 border-accent"
            } else {
                "group/row relative flex items-start gap-1 pl-1.5 pr-0.5 border-l-2 border-transparent"
            },
            // Fold chevron — fixed slot so childless rows align.
            // Only renders the glyph when there ARE children.
            span {
                class: "h-5 w-4 flex-none inline-flex items-center justify-center text-muted-foreground/0 group-hover/row:text-muted-foreground/70 select-none transition-opacity",
                if has_children {
                    button {
                        "data-testid": chevron_testid,
                        class: "h-full w-full inline-flex items-center justify-center hover:text-foreground",
                        onclick: move |_| ops.on_toggle_collapsed.call(id),
                        style: if collapsed { "" } else { "transform: rotate(90deg)" },
                        ChevronRight { size: 10 }
                    }
                }
            }
            // Bullet — Logseq-style. Small grey dot at rest,
            // grows to a filled accent disc on row hover.
            span {
                class: "h-5 w-3 flex-none inline-flex items-center justify-center select-none text-[6px] text-muted-foreground/50 group-hover/row:text-foreground/80 group-hover/row:text-[8px] transition-all",
                "●"
            }
            if show_editor {
                BlockEditor {
                    block: block.clone(),
                    ops,
                    editing_id,
                }
            } else if show_normal {
                BlockNormalView {
                    block: block.clone(),
                    ops,
                    on_navigate_link: ops.on_navigate_link,
                }
            } else {
                BlockView {
                    block: block.clone(),
                    on_enter_edit,
                    on_navigate_link: ops.on_navigate_link,
                }
            }
        }
        // Children — Logseq's "bullet rail" indent guide. The
        // vertical line lives inside the column where the parent
        // bullet renders, so descending rows visually trace back
        // to it. Subtle by default, slightly stronger on hover.
        if has_children && !collapsed {
            div {
                "data-testid": format!("outliner-children-{id}"),
                class: "ml-3 pl-1.5 border-l border-border/30 hover:border-border/60 transition-colors",
                for child in children {
                    OutlinerNode {
                        key: "{child.id}",
                        block: child.clone(),
                        children_by_parent: children_by_parent.clone(),
                        depth: depth + 1,
                        ops,
                        editing_id,
                        vim_mode,
                    }
                }
            }
        }
    }
}

#[component]
fn BlockEditor(block: Block, ops: OutlinerOps, editing_id: Signal<Option<Uuid>>) -> Element {
    let id = block.id;
    let area_testid = format!("outliner-textarea-{id}");
    let _kind = block.kind.clone();
    let content = block.content.clone();
    let content_for_keys = content.clone();
    let is_single_line = !content_for_keys.contains('\n');

    let area_class = block_text_class(&block, BlockMode::Editor);
    // No `rows=` — we size via JS auto-grow + `field-sizing:content`
    // CSS. Setting `rows=N` pins a minimum height that conflicts
    // with the content-driven sizing, causing wrapped text to
    // overflow when N is too small.
    let _row_count = content.lines().count().max(1).min(40);

    // Read the document cursor so we can mirror the offset into
    // the textarea's selection on mount (Insert-mode entry).
    let cursor_signal_for_mount = try_use_context::<Signal<Option<vim::CursorState>>>();
    // Fallback to start-of-block (0), not end. When the cursor
    // state doesn't have this block (e.g. block was just
    // activated via click without an explicit document-cursor
    // update), we want the caret at the start so the user
    // starts typing at the beginning — not jumping ahead to
    // the end. The synthetic-position eval below will override
    // when the user clicked with a known pixel position.
    let initial_offset = cursor_signal_for_mount
        .and_then(|s| s.read().clone())
        .map(|s| s.primary())
        .filter(|c| c.block_id == id)
        .map(|c| c.offset)
        .unwrap_or(0);

    // On mount: seed the textarea's value from the data
    // attribute (uncontrolled — see note below), grab focus, set
    // selection to the document-cursor offset so Insert mode
    // picks up where Normal mode left off.
    //
    // Why uncontrolled: `value="{content}"` re-asserts the value
    // on every Dioxus render, and after each keystroke the CRDT
    // round-trip rebuilds the snapshot → re-renders → DOM
    // re-asserts value, which browsers respond to by snapping
    // the caret to the end. The data attribute approach hands
    // the initial content to the textarea once on mount and
    // leaves the textarea uncontrolled thereafter.
    let area_id_attr = format!("outliner-textarea-{id}");
    let area_id_for_mount = area_id_attr.clone();
    // The click handler in BlockNormalView stashes a pending
    // caret offset in `window.__taArchPendingCaret` keyed by
    // block id. We read + clear it here to position the caret
    // at the click point on the very first paint of the textarea.
    let block_id_for_mount = id.to_string();
    let on_mount = move |elem: Event<MountedData>| {
        let area_id = area_id_for_mount.clone();
        let bid = block_id_for_mount.clone();
        spawn(async move {
            let _ = elem.data().set_focus(true).await;
            // Combined script: seed value (idempotent), then if a
            // pending caret for THIS block exists and is fresh,
            // apply it; otherwise fall back to `initial_offset`.
            let script = format!(
                r#"(function() {{
                    let el = document.querySelector('[data-testid="{area_id}"]');
                    if (!el) return;
                    if (el.dataset.taArchInit !== '1') {{
                        el.dataset.taArchInit = '1';
                        el.value = el.dataset.initialContent || '';
                    }}
                    el.style.height = 'auto';
                    el.style.height = el.scrollHeight + 'px';
                    el.focus();
                    let pending = window.__taArchPendingCaret;
                    let n = {initial_offset};
                    if (pending && pending.blockId === "{bid}" &&
                        (Date.now() - pending.ts) < 1000 && pending.offset >= 0) {{
                        n = pending.offset;
                        window.__taArchPendingCaret = null;
                    }}
                    n = Math.max(0, Math.min(n, el.value.length));
                    el.setSelectionRange(n, n);
                }})();"#
            );
            let _ = dioxus::document::eval(&script).await;
        });
    };
    // Auto-grow script shared with `oninput` so each keystroke
    // re-sizes the textarea to its content. Cheaper to inline
    // a tiny eval per keystroke than wire a JS event listener
    // through Dioxus.
    let area_id_for_input = area_id_attr.clone();
    let autoresize = move || {
        let area_id = area_id_for_input.clone();
        spawn(async move {
            let script = format!(
                "(function(){{\
                    let el = document.querySelector('[data-testid=\"{area_id}\"]');\
                    if (!el) return;\
                    el.style.height = 'auto';\
                    el.style.height = el.scrollHeight + 'px';\
                }})();"
            );
            let _ = dioxus::document::eval(&script).await;
        });
    };

    // On Escape (Insert → Normal), read the textarea's current
    // selectionStart and write it to the document cursor so the
    // visible cursor lands where the user was typing.
    let cursor_writer_on_escape = try_use_context::<Signal<Option<vim::CursorState>>>();
    let area_id_for_escape = area_id_attr.clone();
    let id_for_escape = id;
    let sync_cursor_from_textarea = move || {
        let area_id = area_id_for_escape.clone();
        let mut writer = cursor_writer_on_escape;
        spawn(async move {
            let script = format!(
                "(function(){{\
                    let el = document.querySelector('[data-testid=\"{area_id}\"]');\
                    if (!el) return -1;\
                    return el.selectionStart;\
                }})();"
            );
            if let Ok(value) = dioxus::document::eval(&script).await {
                if let Some(off) = value.as_i64().filter(|n| *n >= 0) {
                    if let Some(state_handle) = writer.as_mut() {
                        let mut new_state = state_handle.peek().clone().unwrap_or_else(|| {
                            vim::CursorState::single(vim::Cursor::new(id_for_escape, 0))
                        });
                        new_state.set_primary(vim::Cursor::new(id_for_escape, off as usize));
                        state_handle.set(Some(new_state));
                    }
                }
            }
        });
    };

    // Pull popup state from context. The active textarea hosts
    // the popup(s) so they visually anchor to it instead of
    // floating at the top-right of the block list.
    let popups = try_use_context::<BlockPopupCtx>();
    let ac_state = popups
        .as_ref()
        .and_then(|p| p.autocomplete.read().clone())
        .filter(|(bid, _)| *bid == id);
    let slash_state = popups
        .as_ref()
        .and_then(|p| p.slash.read().clone())
        .filter(|(bid, _)| *bid == id);

    rsx! {
        // Wrap the textarea in a relative-positioned container so
        // the popups can be absolutely positioned just below it.
        div { class: "relative flex-1 min-w-0",
            textarea {
                "data-testid": area_testid,
                "data-initial-content": "{content}",
                class: area_class,
                rows: 1i64,
                // Uncontrolled — value seeded once via on_mount JS
                // (HTML attribute escaping handles arbitrary
                // characters in `content`). Avoiding the
                // `value=` reactive prop keeps Dioxus from
                // re-asserting the value each render, which would
                // snap the caret to the end on every keystroke.
                onmounted: on_mount,
                oninput: move |e| {
                    ops.on_edit.call((id, e.value()));
                    autoresize();
                },
                onkeydown: move |e| {
                    let mods = e.modifiers();
                    match e.key() {
                        Key::Tab => {
                            e.prevent_default();
                            if mods.shift() {
                                ops.on_outdent.call(id);
                            } else {
                                ops.on_indent.call(id);
                            }
                        }
                        Key::Enter => {
                            if !mods.shift() {
                                e.prevent_default();
                                ops.on_insert_after.call(id);
                            }
                        }
                        Key::Backspace => {
                            if content_for_keys.is_empty() {
                                e.prevent_default();
                                ops.on_delete.call(id);
                            }
                        }
                        Key::Escape => {
                            // Forward to the vim engine, which
                            // transitions Insert → Normal. The
                            // block stays the active editing
                            // block, just in a different mode
                            // (re-renders as BlockNormalView).
                            // Don't clear editing_id — Logseq-
                            // style "stay on the same row".
                            // First snapshot the textarea's
                            // current caret into the document
                            // cursor so the visible cursor
                            // appears where the user was typing.
                            e.prevent_default();
                            sync_cursor_from_textarea();
                            ops.on_vim_key.call(VimKey::Escape);
                            let _ = editing_id;
                        }
                        Key::ArrowUp => {
                            if mods.meta() || mods.ctrl() {
                                e.prevent_default();
                                ops.on_move.call((id, MoveDir::Up));
                            } else if is_single_line {
                                e.prevent_default();
                                ops.on_focus_relative.call((id, FocusDir::Prev));
                            }
                        }
                        Key::ArrowDown => {
                            if mods.meta() || mods.ctrl() {
                                e.prevent_default();
                                ops.on_move.call((id, MoveDir::Down));
                            } else if is_single_line {
                                e.prevent_default();
                                ops.on_focus_relative.call((id, FocusDir::Next));
                            }
                        }
                        _ => {}
                    }
                },
            }
            // Inline popups anchored to the active textarea. Only
            // one renders at a time — slash takes precedence
            // (more recent intent).
            if let Some(ctx) = popups.clone() {
                if let Some((_, q)) = slash_state {
                    InlineSlashPalette { query: q, on_pick: ctx.on_pick_slash }
                } else if let Some((_, q)) = ac_state {
                    InlineWikilinkPalette {
                        query: q,
                        pages: (*ctx.all_pages).clone(),
                        on_pick: ctx.on_pick_page,
                    }
                }
            }
        }
    }
}

/// Inline `[[wikilink]]` autocomplete — absolutely positioned
/// just below the textarea. Same content / behavior as the old
/// `WikilinkAutocomplete` in `live.rs`; lives here so it can
/// anchor relative to the active block.
#[component]
fn InlineWikilinkPalette(query: String, pages: Vec<String>, on_pick: Callback<String>) -> Element {
    let q_lower = query.to_lowercase();
    let matches: Vec<String> = pages
        .into_iter()
        .filter(|n| q_lower.is_empty() || n.to_lowercase().contains(&q_lower))
        .take(8)
        .collect();
    if matches.is_empty() {
        return rsx! {};
    }
    rsx! {
        div {
            "data-testid": "wikilink-autocomplete",
            class: "absolute left-0 top-full z-30 mt-1 w-64 rounded-md border border-border bg-popover shadow-md overflow-hidden",
            div { class: "px-2 py-1 text-[10px] uppercase tracking-wider text-muted-foreground border-b border-border/60",
                if query.is_empty() { "Link a page" } else { "Link · {query}" }
            }
            ul { class: "max-h-64 overflow-y-auto py-1",
                for name in matches {
                    {
                        let label = name.clone();
                        let tid = format!("wikilink-option-{label}");
                        rsx! {
                            li { key: "{label}",
                                button {
                                    "data-testid": tid,
                                    class: "w-full text-left px-2 py-1 text-sm hover:bg-accent hover:text-accent-foreground rounded-sm",
                                    onclick: move |_| on_pick.call(label.clone()),
                                    "{name}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Inline slash-command palette — same content as `live.rs`'s
/// `SlashCommandPalette`, anchored to the active block.
#[component]
fn InlineSlashPalette(query: String, on_pick: Callback<SlashCommand>) -> Element {
    let q_lower = query.to_lowercase();
    let matches: Vec<SlashCommand> = default_slash_commands()
        .into_iter()
        .filter(|c| {
            q_lower.is_empty()
                || c.trigger.contains(&q_lower)
                || c.label.to_lowercase().contains(&q_lower)
        })
        .take(10)
        .collect();
    if matches.is_empty() {
        return rsx! {};
    }
    rsx! {
        div {
            "data-testid": "slash-palette",
            class: "absolute left-0 top-full z-30 mt-1 w-72 rounded-md border border-border bg-popover shadow-md overflow-hidden",
            div { class: "px-2 py-1 text-[10px] uppercase tracking-wider text-muted-foreground border-b border-border/60",
                if query.is_empty() { "Insert" } else { "/{query}" }
            }
            ul { class: "max-h-72 overflow-y-auto py-1",
                for cmd in matches {
                    {
                        let label = cmd.label.to_string();
                        let trigger = cmd.trigger.to_string();
                        let cmd_clone = cmd.clone();
                        let tid = format!("slash-option-{trigger}");
                        rsx! {
                            li { key: "{trigger}",
                                button {
                                    "data-testid": tid,
                                    class: "w-full flex items-baseline gap-2 px-2 py-1 text-left text-sm hover:bg-accent hover:text-accent-foreground rounded-sm",
                                    onclick: move |_| on_pick.call(cmd_clone.clone()),
                                    span { class: "flex-1 truncate", "{label}" }
                                    span { class: "text-[10px] text-muted-foreground/70 font-mono",
                                        "/{trigger}"
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
fn BlockView(
    block: Block,
    on_enter_edit: Callback<Uuid>,
    on_navigate_link: Callback<String>,
) -> Element {
    let id = block.id;
    let kind = block.kind.clone();
    let content = block.content.clone();
    let block_id_attr = id.to_string();

    // Per-kind chrome. Shares the same vocab as the editor via
    // `block_text_class` so the view↔edit swap doesn't shift
    // layout (font-size, line-height, padding all match).
    let outer_class = block_text_class(&block, BlockMode::View);

    // Single-click → cursor at clicked position.
    //
    // Strategy (mirrors Logseq's `util/caret-range`): use the
    // browser's native selection model to extract the rendered
    // text length from the start of the block up to the click
    // point. The browser already handles font metrics (mono or
    // proportional), line wrapping, RTL, ligatures — none of
    // which we want to reimplement.
    //
    // Flow:
    // 1. `mousedown` lets the browser place its selection at
    //    the click point on the rendered DOM.
    // 2. Inside our click handler we read `window.getSelection`,
    //    clone the range, anchor start to the block element's
    //    beginning, and take `toString().length` — that's the
    //    rendered offset.
    // 3. Stash `(block_id, rendered_offset)` in `PendingClick`.
    // 4. `BlockEditor::on_mount` reads it, translates to source
    //    offset (identity for plain text; close enough for
    //    formatted text since rendered chars == source chars
    //    for emphasis runs in our editor today), and calls
    //    `setSelectionRange` on the textarea.
    let click_cb = try_use_context::<OutlinerOps>().map(|o| o.on_click_at_offset);
    let id_str = block_id_attr.clone();
    let content_for_click = content.clone();
    let click_handler = move |e: Event<MouseData>| {
        // Hit-test the click coords against the rendered DOM via
        // `caretRangeFromPoint`, stash the result in a JS global
        // (`window.__taArchPendingCaret`). BlockEditor's on_mount
        // reads it on the very next render. This is fire-and-forget
        // so it can't block edit-mode transition (which fires sync
        // below). The eval JS captures the DOM synchronously in
        // the same microtask, BEFORE Dioxus has re-rendered the
        // textarea, because it runs before the next paint.
        let coords = e.data.client_coordinates();
        let cx = coords.x;
        let cy = coords.y;
        let block_id_for_eval = id_str.clone();
        let source = content_for_click.clone();
        let source_json = serde_json::to_string(&source).unwrap_or_else(|_| "\"\"".into());
        let script = format!(
            r#"(function() {{
                let wrapper = document.querySelector('[data-block-id="{block_id_for_eval}"]');
                if (!wrapper) return;
                // Use the inner content-only span so leading chrome
                // (task glyphs, query results, etc.) doesn't inflate
                // the rendered offset.
                let block = wrapper.querySelector('[data-block-content]') || wrapper;
                let x = {cx}, y = {cy};
                let range = null;
                if (document.caretRangeFromPoint) {{
                    range = document.caretRangeFromPoint(x, y);
                }} else if (document.caretPositionFromPoint) {{
                    let pos = document.caretPositionFromPoint(x, y);
                    if (pos) {{
                        range = document.createRange();
                        range.setStart(pos.offsetNode, pos.offset);
                        range.collapse(true);
                    }}
                }}
                let rendered = -1;
                if (range && block.contains(range.endContainer)) {{
                    let pre = document.createRange();
                    pre.selectNodeContents(block);
                    try {{
                        pre.setEnd(range.endContainer, range.endOffset);
                        rendered = pre.toString().length;
                    }} catch (err) {{}}
                }} else if (block.textContent) {{
                    rendered = block.textContent.length;
                }}
                // Translate rendered → source offset by walking the
                // source string and skipping over inline markdown
                // syntax (`**`, `*`, `~~`, `==`, `` ` ``, wikilinks,
                // external links, images). Covers common cases.
                let src = {source_json};
                function r2s(src, target) {{
                    if (target < 0) return -1;
                    let i = 0, r = 0;
                    while (i < src.length && r < target) {{
                        let two = src.substr(i, 2);
                        if (two === '**' || two === '~~' || two === '==') {{ i += 2; continue; }}
                        let c = src[i];
                        if (c === '*' || c === '`') {{ i += 1; continue; }}
                        if (two === '[[') {{
                            let end = src.indexOf(']]', i + 2);
                            if (end > 0) {{
                                let inner = src.substring(i + 2, end);
                                let pipe = inner.indexOf('|');
                                let label = pipe >= 0 ? inner.substring(pipe + 1) : inner;
                                let labelStart = i + 2 + (pipe >= 0 ? pipe + 1 : 0);
                                if (r + label.length >= target) return labelStart + (target - r);
                                r += label.length; i = end + 2; continue;
                            }}
                        }}
                        let isImg = c === '!' && src[i+1] === '[';
                        if (c === '[' || isImg) {{
                            let labelStart = isImg ? i + 2 : i + 1;
                            let close = src.indexOf(']', labelStart);
                            if (close > 0 && src[close+1] === '(') {{
                                let urlEnd = src.indexOf(')', close + 2);
                                if (urlEnd > 0) {{
                                    let label = src.substring(labelStart, close);
                                    if (r + label.length >= target) return labelStart + (target - r);
                                    r += label.length; i = urlEnd + 1; continue;
                                }}
                            }}
                        }}
                        r += 1; i += 1;
                    }}
                    return i;
                }}
                let offset = r2s(src, rendered);
                window.__taArchPendingCaret = {{
                    blockId: "{block_id_for_eval}",
                    offset: offset,
                    ts: Date.now()
                }};
            }})();"#
        );
        let _ = dioxus::document::eval(&script);
        // Fire edit transition synchronously — the JS above is
        // queued but the click already happened, so by the time
        // Dioxus re-renders, the eval will have run and the
        // global is populated.
        if let Some(cb) = click_cb {
            cb.call((id, 0));
        } else {
            on_enter_edit.call(id);
        }
    };

    // Code blocks render raw — don't try to inline-parse. Adds
    // a header chip (language label + copy-to-clipboard) and a
    // monospace pre-formatted body. Block-level kind drives this
    // — the `code` inline span renders as a chip via `InlineNode`.
    if kind == "code" {
        let lang = block.code_lang.clone().unwrap_or_default();
        let lang_label = if lang.is_empty() {
            "plain".to_string()
        } else {
            lang.clone()
        };
        let copy_payload = content.clone();
        return rsx! {
            div {
                "data-block-id": "{id}",
                "data-code-lang": "{lang}",
                class: "group relative rounded-md border border-border bg-card/60 my-1 overflow-hidden",
                onclick: click_handler,
                div {
                    class: "flex items-center justify-between px-3 py-1 border-b border-border/60 bg-muted/30 text-[10px] uppercase tracking-wide text-muted-foreground",
                    span { "{lang_label}" }
                    button {
                        r#type: "button",
                        class: "opacity-0 group-hover:opacity-100 transition flex items-center gap-1 px-1.5 py-0.5 rounded hover:bg-muted/60 text-muted-foreground hover:text-foreground",
                        title: "Copy code",
                        onclick: move |e: Event<MouseData>| {
                            e.stop_propagation();
                            let payload = copy_payload.clone();
                            spawn(async move {
                                let _ = document::eval(&format!(
                                    "navigator.clipboard.writeText({})",
                                    serde_json::to_string(&payload).unwrap_or_else(|_| "''".into())
                                )).await;
                            });
                        },
                        Copy { class: "h-3 w-3" }
                        "copy"
                    }
                }
                pre {
                    class: "px-3 py-2 font-mono text-xs leading-relaxed whitespace-pre-wrap text-foreground/90",
                    if content.is_empty() {
                        span { class: "text-muted-foreground/50", "click to edit" }
                    } else {
                        code { "{content}" }
                    }
                }
            }
        };
    }

    let task_glyph = task_state_glyph(&block);
    let strike_through = matches!(block.list_task.as_deref(), Some("x"));
    let body_cls = if strike_through {
        format!("{outer_class} line-through opacity-70")
    } else {
        outer_class.to_string()
    };
    // Footnote definition block: `[^id]: body…`. Renders as
    // a tight gray row with an id anchor so the inline `[^id]`
    // ref can jump to it. Body re-parses through `parse_inline`.
    if let Some((fn_id, fn_body)) = parse_footnote_def(&content) {
        let anchor_id = format!("fn-{fn_id}");
        return rsx! {
            div {
                "data-block-id": "{id}",
                "data-testid": "footnote-def-{fn_id}",
                id: "{anchor_id}",
                class: format!("{body_cls} flex items-baseline gap-2 text-sm text-muted-foreground"),
                onclick: click_handler,
                span { class: "font-mono text-sky-500 select-none", "[{fn_id}]" }
                span { class: "flex-1",
                    for (j, inline) in inline_md::parse_inline(&fn_body).into_iter().enumerate() {
                        InlineNode { key: "{j}", inline, on_navigate_link }
                    }
                }
            }
        };
    }
    // GFM table: `| h1 | h2 |\n|---|---|\n| a | b |`. Rendered
    // as a real `<table>` with per-column alignment from the
    // divider row.
    if let Some(table) = parse_table(&content) {
        let on_nav = on_navigate_link;
        return rsx! {
            div {
                "data-block-id": "{id}",
                "data-testid": "table-block",
                class: "{body_cls} my-1 overflow-x-auto",
                onclick: click_handler,
                table { class: "min-w-full text-sm border-collapse",
                    thead {
                        tr { class: "border-b border-border",
                            for (ci, h) in table.headers.iter().cloned().enumerate() {
                                th {
                                    key: "h-{ci}",
                                    class: format!(
                                        "px-3 py-1.5 font-semibold {} text-muted-foreground",
                                        align_class(table.alignments.get(ci).copied().unwrap_or(TableAlign::Left))
                                    ),
                                    for (j, inline) in inline_md::parse_inline(&h).into_iter().enumerate() {
                                        InlineNode { key: "{j}", inline, on_navigate_link: on_nav }
                                    }
                                }
                            }
                        }
                    }
                    tbody {
                        for (ri, row) in table.rows.iter().cloned().enumerate() {
                            tr {
                                key: "r-{ri}",
                                class: "border-b border-border/40 hover:bg-muted/20",
                                for (ci, cell) in row.into_iter().enumerate() {
                                    td {
                                        key: "c-{ci}",
                                        class: format!(
                                            "px-3 py-1 align-top {}",
                                            align_class(table.alignments.get(ci).copied().unwrap_or(TableAlign::Left))
                                        ),
                                        for (j, inline) in inline_md::parse_inline(&cell).into_iter().enumerate() {
                                            InlineNode { key: "{j}", inline, on_navigate_link: on_nav }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        };
    }
    // Obsidian-style callout: `> [!type] Title\n> body…`. Rendered
    // as a colored card with the matching icon. Detected eagerly
    // here so we don't waste the inline parse below.
    if let Some(callout) = parse_callout(&content) {
        return rsx! {
            div {
                "data-block-id": "{id}",
                "data-testid": "callout-{callout.kind}",
                class: format!("{body_cls} {}", callout.classes()),
                onclick: click_handler,
                div { class: "flex items-center gap-2 mb-1 text-sm font-semibold",
                    {callout.icon()}
                    span { class: "uppercase tracking-wide", "{callout.label()}" }
                    if let Some(title) = callout.title.as_ref() {
                        span { class: "font-normal opacity-80", "{title}" }
                    }
                }
                if !callout.body.is_empty() {
                    div { class: "text-sm leading-relaxed",
                        for (i, line) in callout.body.lines().enumerate() {
                            div { key: "{i}",
                                for (j, inline) in inline_md::parse_inline(line).into_iter().enumerate() {
                                    InlineNode { key: "{j}", inline, on_navigate_link }
                                }
                            }
                        }
                    }
                }
            }
        };
    }
    let block_query = crate::query::parse_block_level_query(&content);
    // Logseq parity (shared with the static-site renderer): if
    // the block carries a leading TODO/DOING/etc keyword, peel it
    // off so the rest of the content renders cleanly with a pill
    // prefix. Same goes for `SCHEDULED:` / `DEADLINE:` lines that
    // belong below the body, not inline. `prop:: value` chips come
    // from the existing `properties_json` blob the CRDT layer
    // already maintains.
    let (task_marker, content_after_marker) = publish_core::peel_task_marker(&content);
    let (planning, content_after_planning) = publish_core::peel_planning(content_after_marker);
    let prop_chips: Vec<(String, String)> = publish_core::parse_props(&block.properties_json);
    let task_marker_label = task_marker.map(|m| m.label());
    let task_marker_cls = task_marker.map(|m| match m {
        publish_core::TaskMarker::Todo => "task-todo",
        publish_core::TaskMarker::Doing => "task-doing",
        publish_core::TaskMarker::Done => "task-done",
        publish_core::TaskMarker::Later => "task-later",
        publish_core::TaskMarker::Now => "task-now",
        publish_core::TaskMarker::Waiting => "task-waiting",
        publish_core::TaskMarker::Cancelled => "task-cancelled",
    });
    let inlines = inline_md::parse_inline(content_after_planning);
    if inlines.is_empty() {
        return rsx! {
            div {
                "data-block-id": "{id}",
                class: "{body_cls} text-muted-foreground/40",
                onclick: click_handler,
                if let Some(g) = task_glyph {
                    span { class: "mr-1 text-muted-foreground/80 select-none font-mono", "{g}" }
                }
                " "
            }
        };
    }
    rsx! {
        div {
            "data-block-id": "{id}",
            class: body_cls,
            onclick: click_handler,
            if let Some(g) = task_glyph {
                span { class: "mr-1 text-muted-foreground/80 select-none font-mono", "{g}" }
            }
            if let (Some(label), Some(cls)) = (task_marker_label, task_marker_cls) {
                span {
                    class: "mr-1.5 inline-block rounded px-1.5 py-0 text-[0.7rem] font-bold uppercase tracking-wider border align-baseline {cls}",
                    "{label}"
                }
            }
            span {
                "data-block-content": "true",
                for (i, inline) in inlines.into_iter().enumerate() {
                    InlineNode { key: "{i}", inline, on_navigate_link }
                }
            }
            if !planning.scheduled.is_empty() || !planning.deadline.is_empty() {
                div { class: "mt-0.5 ml-6 flex flex-wrap gap-1 text-[0.7rem] font-mono",
                    if !planning.scheduled.is_empty() {
                        span { class: "inline-flex rounded border border-border overflow-hidden",
                            span { class: "px-1.5 bg-muted/40 text-muted-foreground font-bold", "SCHEDULED" }
                            span { class: "px-1.5 tabular-nums", "{planning.scheduled}" }
                        }
                    }
                    if !planning.deadline.is_empty() {
                        span { class: "inline-flex rounded border border-border overflow-hidden",
                            span { class: "px-1.5 bg-muted/40 text-destructive font-bold", "DEADLINE" }
                            span { class: "px-1.5 tabular-nums", "{planning.deadline}" }
                        }
                    }
                }
            }
            if !prop_chips.is_empty() {
                div { class: "mt-0.5 ml-6 flex flex-wrap gap-1 text-[0.7rem]",
                    for (k, v) in prop_chips {
                        span { key: "{k}", class: "inline-flex rounded border border-border overflow-hidden",
                            span { class: "px-1.5 bg-muted/40 text-muted-foreground font-semibold", "{k}" }
                            span { class: "px-1.5", "{v}" }
                        }
                    }
                }
            }
            if let Some(q) = block_query {
                QueryResultsCard { query: q }
            }
        }
    }
}

#[component]
pub fn InlineNode(inline: Inline, on_navigate_link: Callback<String>) -> Element {
    match inline {
        Inline::Text(s) => rsx! { "{s}" },
        Inline::Link { target, alias } => {
            let display = alias.clone().unwrap_or_else(|| target.clone());
            let target_for_click = target.clone();
            let target_for_pin = target.clone();
            let known = try_use_context::<KnownBasenames>().unwrap_or_default();
            let pin_cb = try_use_context::<PinPaneCb>();
            let resolves = known.knows(&target);
            let cls = if resolves {
                "text-primary underline decoration-primary/30 hover:decoration-primary cursor-pointer"
            } else {
                "text-destructive/90 underline decoration-destructive/40 decoration-dotted hover:decoration-destructive cursor-pointer"
            };
            let title = if resolves {
                target.clone()
            } else {
                format!("{target} (broken — page doesn't exist)")
            };
            rsx! {
                a {
                    class: cls,
                    "data-broken": "{!resolves}",
                    title: title,
                    onclick: move |e: Event<MouseData>| {
                        e.stop_propagation();
                        // Shift-click → pin the page as a sidebar
                        // pane instead of navigating.
                        if e.modifiers().shift() {
                            if let Some(cb) = pin_cb {
                                cb.0.call(target_for_pin.clone());
                                return;
                            }
                        }
                        on_navigate_link.call(target_for_click.clone());
                    },
                    "{display}"
                }
            }
        }
        Inline::Embed { target, alias } => {
            rsx! { EmbedCard { target: target.clone(), alias: alias.clone() } }
        }
        Inline::BlockRef { target_block_id } => {
            rsx! { BlockRefChip { target_block_id } }
        }
        Inline::Tag(name) => {
            let label = format!("#{name}");
            rsx! {
                span { class: "text-violet-500 hover:text-violet-400 cursor-pointer",
                    "{label}"
                }
            }
        }
        Inline::Code(c) => rsx! {
            code { class: "rounded bg-muted/50 px-1 py-0.5 font-mono text-[0.85em]",
                "{c}"
            }
        },
        Inline::Bold(children) => rsx! {
            strong { class: "font-bold",
                for (i, child) in children.into_iter().enumerate() {
                    InlineNode { key: "{i}", inline: child, on_navigate_link }
                }
            }
        },
        Inline::Italic(children) => rsx! {
            em {
                for (i, child) in children.into_iter().enumerate() {
                    InlineNode { key: "{i}", inline: child, on_navigate_link }
                }
            }
        },
        Inline::Strikethrough(children) => rsx! {
            s { class: "text-muted-foreground",
                for (i, child) in children.into_iter().enumerate() {
                    InlineNode { key: "{i}", inline: child, on_navigate_link }
                }
            }
        },
        Inline::Highlight(children) => rsx! {
            mark { class: "rounded-sm bg-yellow-400/30 px-0.5 text-foreground",
                for (i, child) in children.into_iter().enumerate() {
                    InlineNode { key: "{i}", inline: child, on_navigate_link }
                }
            }
        },
        Inline::ExternalLink { label, url } => {
            let display = if label.is_empty() {
                url.clone()
            } else {
                label.clone()
            };
            rsx! {
                a {
                    class: "text-sky-500 underline decoration-sky-500/40 hover:decoration-sky-500 cursor-pointer",
                    href: "{url}",
                    target: "_blank",
                    rel: "noopener noreferrer",
                    title: "{url}",
                    onclick: move |e: Event<MouseData>| e.stop_propagation(),
                    "{display}"
                }
            }
        }
        Inline::Image { alt, url } => rsx! {
            img {
                class: "inline-block max-w-full rounded border border-border",
                src: "{url}",
                alt: "{alt}",
                title: "{alt}",
                loading: "lazy",
                onclick: move |e: Event<MouseData>| e.stop_propagation(),
            }
        },
        Inline::FootnoteRef(id) => {
            let href = format!("#fn-{id}");
            rsx! {
                sup {
                    class: "text-sky-500 hover:text-sky-400",
                    a {
                        href: "{href}",
                        "data-footnote-ref": "{id}",
                        onclick: move |e: Event<MouseData>| e.stop_propagation(),
                        "[{id}]"
                    }
                }
            }
        }
    }
}

/// Active block in Normal/Visual mode. Renders the same parsed
/// markdown as `BlockView` but is itself focusable (`tabindex=0`)
/// and captures keydown events to feed the vim engine.
#[component]
fn BlockNormalView(block: Block, ops: OutlinerOps, on_navigate_link: Callback<String>) -> Element {
    let id = block.id;
    let area_testid = format!("outliner-normal-{id}");

    // Mount-time focus so the user starts in vim Normal mode on
    // the active block with no extra click.
    let on_mount = move |elem: Event<MountedData>| {
        spawn(async move {
            let _ = elem.data().set_focus(true).await;
        });
    };

    // The visible content uses the same renderer as BlockView so
    // mode swaps don't shift layout.
    let block_id = id;
    let ops_for_click = ops;
    let on_click = move |_e: Event<MouseData>| {
        // Re-entering Insert on the active block. Use offset 0
        // — the user's intent here is "I want to edit this
        // block", not "place cursor at a precise position"
        // (use BlockView clicks for that). Logseq's same
        // gesture lands you at start-of-block; we match it.
        ops_for_click.on_click_at_offset.call((block_id, 0));
    };
    let ops_for_keys = ops;
    rsx! {
        div {
            "data-testid": area_testid,
            tabindex: "0",
            class: "flex-1 outline-none focus-visible:ring-1 focus-visible:ring-ring/40 rounded-sm cursor-text",
            onmounted: on_mount,
            onclick: on_click,
            onkeydown: move |e| {
                // Tab + Shift-Tab indent/outdent in Normal mode
                // too — Logseq parity. Otherwise the vim engine
                // (which doesn't bind Tab) would swallow them
                // silently.
                if matches!(e.key(), Key::Tab) {
                    e.prevent_default();
                    if e.modifiers().shift() {
                        ops_for_keys.on_outdent.call(id);
                    } else {
                        ops_for_keys.on_indent.call(id);
                    }
                    return;
                }
                let key = match e.key() {
                    Key::Escape => Some(VimKey::Escape),
                    Key::Enter => Some(VimKey::Enter),
                    Key::Tab => Some(VimKey::Tab),
                    Key::Backspace => Some(VimKey::Backspace),
                    Key::ArrowLeft => Some(VimKey::ArrowLeft),
                    Key::ArrowRight => Some(VimKey::ArrowRight),
                    Key::ArrowUp => Some(VimKey::ArrowUp),
                    Key::ArrowDown => Some(VimKey::ArrowDown),
                    Key::Character(s) if s.chars().count() == 1 => {
                        s.chars().next().map(VimKey::Char)
                    }
                    _ => None,
                };
                if let Some(k) = key {
                    e.prevent_default();
                    ops_for_keys.on_vim_key.call(k);
                }
                let _ = id;
            },
            // Active block in Normal mode renders as plain text
            // with a visible cursor block — formatting hides
            // when the editor is "live" on this block, just
            // like Logseq. When the cursor isn't on this block
            // (shouldn't happen in BlockNormalView but defensive),
            // fall back to the rich render below.
            CursorRow { block: block.clone() }
            // `on_navigate_link` stays in scope so non-cursor
            // navigation paths still typecheck; not used here.
            { let _ = on_navigate_link; }
        }
    }
}

/// Visible cursor row — used by `BlockNormalView` for the
/// active block in Normal/Visual mode. Splits the block content
/// at the cursor offset and inserts a faux-cursor element so
/// the user can see exactly where the document cursor is. Plain
/// text only (no inline markdown render) — formatting only
/// shows when the block is *not* the active editor, which
/// matches Logseq's "edit hides formatting" idiom.
#[component]
fn CursorRow(block: Block) -> Element {
    let cursor_signal = try_use_context::<Signal<Option<vim::CursorState>>>();
    let cursor = cursor_signal
        .as_ref()
        .and_then(|s| s.read().clone())
        .map(|s| s.primary());
    let class = block_text_class(&block, BlockMode::View);
    let task_glyph = task_state_glyph(&block);
    let content = block.content.clone();
    let active_in_block = cursor.map(|c| c.block_id == block.id).unwrap_or(false);
    let offset = if active_in_block {
        let off = cursor.unwrap().offset;
        // Snap to a char boundary so the split is safe.
        let mut o = off.min(content.len());
        while o > 0 && !content.is_char_boundary(o) {
            o -= 1;
        }
        o
    } else {
        content.len()
    };
    // Vim-style block cursor: the character AT `offset` gets
    // inverted background. At end-of-content (no char to cover)
    // we render an empty block-width cursor so the position is
    // still visible.
    let (before, rest) = content.split_at(offset);
    let (cursor_char, after) = if active_in_block {
        match rest.chars().next() {
            Some(c) => {
                let len = c.len_utf8();
                (Some(rest[..len].to_string()), &rest[len..])
            }
            None => (None, rest),
        }
    } else {
        (None, rest)
    };
    let cursor_char_for_render = cursor_char.clone();
    let row_class = format!("relative {class}");
    rsx! {
        div {
            class: "{row_class}",
            "data-cursor-row": "{block.id}",
            if let Some(g) = task_glyph {
                span { class: "mr-1 text-muted-foreground/80 select-none font-mono", "{g}" }
            }
            // Plain text to preserve byte-accurate offsets.
            // Whitespace + newlines render via whitespace-pre-wrap
            // baked into the per-kind text class.
            span { "{before}" }
            if active_in_block {
                if let Some(ch) = cursor_char_for_render.clone() {
                    // Block cursor sits ON the char.
                    span {
                        "data-testid": format!("doc-cursor-{}", block.id),
                        class: "bg-foreground text-background rounded-[1px]",
                        // Newline/CR under cursor: render a space
                        // so the inverted block stays visible at
                        // line ends inside multi-line blocks.
                        if ch == "\n" || ch == "\r" {
                            " "
                            "{ch}"
                        } else {
                            "{ch}"
                        }
                    }
                } else {
                    // End-of-content cursor — empty inverted block
                    // sized to a monospace cell.
                    span {
                        "data-testid": format!("doc-cursor-{}", block.id),
                        class: "bg-foreground text-background rounded-[1px] inline-block w-[1ch]",
                        " "
                    }
                }
            }
            span { "{after}" }
        }
        // Remote peer cursors anchored to this block. Each
        // colored bar sits absolute-positioned at the right
        // column for that peer's offset. Monospace font means
        // `col * ch` is a stable horizontal advance.
        RemoteCursorOverlay { block_id: block.id, content: block.content.clone() }
    }
}

/// Overlay for peer cursors landing inside this block.
/// Renders one colored vertical bar + a tiny name chip per
/// remote peer whose `block_id == this`. Pulls the resolved
/// remote-cursor list from context (populated by the
/// awareness sync loop).
#[component]
fn RemoteCursorOverlay(block_id: Uuid, content: String) -> Element {
    let remote = try_use_context::<Signal<Vec<crate::awareness::RemoteCursor>>>();
    let order_index = try_use_context::<BlockOrderIndex>().unwrap_or_default();
    let this_pos = order_index.position_of(block_id);
    // Include a peer cursor if it touches this block: head here,
    // anchor here, or this block sits between them in DFS order.
    let list: Vec<crate::awareness::RemoteCursor> = remote
        .as_ref()
        .map(|s| s.read().clone())
        .unwrap_or_default()
        .into_iter()
        .filter(|c| {
            if c.block_id == block_id {
                return true;
            }
            let Some(a) = c.anchor.as_ref() else {
                return false;
            };
            if a.block_id == block_id {
                return true;
            }
            let (Some(tp), Some(ap), Some(hp)) = (
                this_pos,
                order_index.position_of(a.block_id),
                order_index.position_of(c.block_id),
            ) else {
                return false;
            };
            let (lo, hi) = if ap <= hp { (ap, hp) } else { (hp, ap) };
            tp > lo && tp < hi
        })
        .collect();
    if list.is_empty() {
        return rsx! {};
    }
    let line_col_in = |c: &str, offset: usize| -> (usize, usize) {
        let mut off = offset.min(c.len());
        while off > 0 && !c.is_char_boundary(off) {
            off -= 1;
        }
        let head = &c[..off];
        let line = head.bytes().filter(|b| *b == b'\n').count();
        let col = head.rfind('\n').map(|i| off - i - 1).unwrap_or(off);
        (line, col)
    };
    let content_for_line_col = content.clone();
    let line_col_of =
        move |offset: usize| -> (usize, usize) { line_col_in(&content_for_line_col, offset) };
    let line_lengths: Vec<usize> = content.split('\n').map(|l| l.chars().count()).collect();
    let total_lines = line_lengths.len();
    rsx! {
        for rc in list {
            {
                let (line, col) = line_col_of(rc.offset);
                let chip_bg = rc.color.clone();
                let testid = format!("remote-cursor-{}", rc.peer_id);
                let name = rc.name.clone();
                let head_style = format!(
                    "left: {}ch; top: {:.2}em; background: {};",
                    col,
                    line as f32 * 1.4,
                    rc.color
                );
                // Build per-line rects. We compute the slice of
                // the selection that falls inside *this* block:
                // - same block as both endpoints: between them
                // - this is the start block (anchor or head
                //   depending on DFS order): from start col → EOB
                // - this is the end block: SOB → end col
                // - this is between in DFS order: full block
                // - else: no rects.
                let rects_for_range =
                    |start_line: usize, start_col: usize, end_line: usize, end_col: usize| -> Vec<(String, String)> {
                        let mut rects = Vec::new();
                        for l in start_line..=end_line {
                            let s_col = if l == start_line { start_col } else { 0 };
                            let e_col = if l == end_line {
                                end_col
                            } else {
                                line_lengths.get(l).copied().unwrap_or(0)
                            };
                            let width = e_col.saturating_sub(s_col);
                            let style = if width == 0 && l != end_line {
                                format!(
                                    "left: {}ch; top: {:.2}em; width: 0.4ch; height: 1.4em; background: {};",
                                    s_col,
                                    l as f32 * 1.4,
                                    rc.color
                                )
                            } else if width == 0 {
                                continue;
                            } else {
                                format!(
                                    "left: {}ch; top: {:.2}em; width: {}ch; height: 1.4em; background: {};",
                                    s_col,
                                    l as f32 * 1.4,
                                    width,
                                    rc.color
                                )
                            };
                            rects.push((format!("sel-{}-{}", rc.peer_id, l), style));
                        }
                        rects
                    };
                let end_of_block_line = total_lines.saturating_sub(1);
                let end_of_block_col = line_lengths.last().copied().unwrap_or(0);
                let selection_rects: Vec<(String, String)> = match rc.anchor.as_ref() {
                    Some(a) if a.block_id == block_id => {
                        // Anchor and head share this block — render
                        // between them (single-block selection).
                        let (al, ac) = line_col_of(a.offset);
                        let ((s_line, s_col), (e_line, e_col)) = if (al, ac) <= (line, col) {
                            ((al, ac), (line, col))
                        } else {
                            ((line, col), (al, ac))
                        };
                        rects_for_range(s_line, s_col, e_line, e_col)
                    }
                    Some(a) => {
                        // Cross-block selection. Decide where we
                        // sit (anchor, head, or middle) using DFS
                        // positions, then render the appropriate
                        // slice within this block.
                        let anchor_pos = order_index.position_of(a.block_id);
                        let head_pos = order_index.position_of(rc.block_id);
                        match (this_pos, anchor_pos, head_pos) {
                            (Some(tp), Some(ap), Some(hp)) => {
                                let (start_block, start_off, end_block, end_off) = if ap <= hp {
                                    (a.block_id, a.offset, rc.block_id, rc.offset)
                                } else {
                                    (rc.block_id, rc.offset, a.block_id, a.offset)
                                };
                                let start_pos = order_index.position_of(start_block).unwrap_or(0);
                                let end_pos = order_index.position_of(end_block).unwrap_or(0);
                                if tp == start_pos && tp == end_pos {
                                    let (s_line, s_col) = line_col_of(start_off);
                                    let (e_line, e_col) = line_col_of(end_off);
                                    rects_for_range(s_line, s_col, e_line, e_col)
                                } else if tp == start_pos {
                                    let (s_line, s_col) = line_col_of(start_off);
                                    rects_for_range(s_line, s_col, end_of_block_line, end_of_block_col)
                                } else if tp == end_pos {
                                    // End block — need to resolve
                                    // end_off in THIS block's text.
                                    // Fall back to the local
                                    // `content` since end_block ==
                                    // block_id here.
                                    let (e_line, e_col) = line_col_of(end_off);
                                    rects_for_range(0, 0, e_line, e_col)
                                } else if tp > start_pos && tp < end_pos {
                                    rects_for_range(0, 0, end_of_block_line, end_of_block_col)
                                } else {
                                    Vec::new()
                                }
                            }
                            _ => Vec::new(),
                        }
                    }
                    None => Vec::new(),
                };
                let is_head_block = rc.block_id == block_id;
                // Mode-aware caret glyph: block cursor for
                // Normal/Visual (vim "on the char"), thin caret
                // for Insert ("between chars"). Width changes via
                // class; left/top/background reuse `head_style`.
                let head_class = match rc.mode {
                    crate::awareness::PeerMode::Insert => {
                        "pointer-events-none absolute w-[2px] h-[1.1em] z-20"
                    }
                    _ => "pointer-events-none absolute w-[1ch] h-[1.1em] z-20 opacity-60",
                };
                rsx! {
                    for (key, style) in selection_rects {
                        span {
                            key: "{key}",
                            class: "pointer-events-none absolute z-10 opacity-25 rounded-sm",
                            style: "{style}",
                        }
                    }
                    if is_head_block {
                        span {
                            "data-testid": testid,
                            class: head_class,
                            style: head_style,
                            // Tiny name chip floating above the bar.
                            span {
                                class: "absolute -top-[1em] left-0 px-1 rounded-[2px] text-[9px] font-mono leading-tight whitespace-nowrap text-white",
                                style: format!("background: {chip_bg};"),
                                "{name}"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// `{{query …}}` result card. Resolves the query against the
/// snapshot indices via the `QueryIndex` context and renders
/// each hit as a clickable row showing the source page + the
/// block snippet.
#[component]
fn QueryResultsCard(query: crate::query::Query) -> Element {
    let idx = try_use_context::<QueryIndex>().unwrap_or_default();
    let nav = try_use_context::<NavigateLinkCb>()
        .map(|c| c.0)
        .unwrap_or_else(|| use_callback(|_: String| {}));

    let all_iter = idx.all_blocks.iter().map(|(b, p, _)| (*b, *p));
    let hits = crate::query::evaluate(&query, &idx.block_refs, &idx.block_props, all_iter);
    let hit_count = hits.len();
    let snippet_for = |bid: Uuid| -> String {
        idx.all_blocks
            .iter()
            .find(|(id, _, _)| *id == bid)
            .map(|(_, _, s)| s.clone())
            .unwrap_or_else(|| "(missing)".into())
    };
    let label = match &query {
        crate::query::Query::All => "Everything".to_string(),
        crate::query::Query::Tag(t) => format!("#{t}"),
        crate::query::Query::Link(p) => format!("[[{p}]]"),
        crate::query::Query::Property { key, value } => format!("{key}: {value}"),
    };

    rsx! {
        div {
            "data-testid": "query-results",
            class: "mt-2 rounded-md border border-border/60 bg-card/40 overflow-hidden",
            div { class: "flex items-baseline gap-2 px-2 py-1 text-[11px] uppercase tracking-wider text-muted-foreground border-b border-border/40 bg-muted/20",
                span { class: "font-semibold", "Query" }
                span { class: "text-foreground/80 normal-case", "{label}" }
                span { class: "ml-auto text-foreground/70", "· {hit_count}" }
            }
            if hits.is_empty() {
                div { class: "px-2 py-2 text-xs italic text-muted-foreground", "No matches." }
            } else {
                ul { class: "max-h-72 overflow-y-auto",
                    for hit in hits.iter().take(50) {
                        {
                            let bid = hit.block_id;
                            let pid = hit.page_id;
                            let title = idx.page_titles.get(&pid).cloned().unwrap_or_else(|| "(unknown)".into());
                            let title_for_click = title.clone();
                            let snippet = snippet_for(bid);
                            let testid = format!("query-hit-{bid}");
                            rsx! {
                                li { key: "{bid}",
                                    "data-testid": testid,
                                    class: "px-2 py-1 text-xs cursor-pointer text-foreground/85 hover:bg-muted/40 hover:text-foreground",
                                    onclick: move |_| nav.call(title_for_click.clone()),
                                    div { class: "text-[10px] uppercase tracking-wider text-muted-foreground/70", "{title}" }
                                    div { class: "truncate",
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

/// `((uuid))` block-reference chip with click-to-expand. Click
/// the chip → expands to show the referenced block + its
/// descendants inline (read-only). Click again → collapses
/// back to the chip.
#[component]
fn BlockRefChip(target_block_id: Uuid) -> Element {
    let mut expanded: Signal<bool> = use_signal(|| false);
    let snippets = try_use_context::<BlockSnippets>().unwrap_or_default();
    let tree = try_use_context::<BlockTreeLookup>().unwrap_or_default();
    let depth = try_use_context::<EmbedDepth>().unwrap_or_default().0;

    let snippet = snippets.snippet(target_block_id);
    let resolved = snippet.is_some();
    let label = snippet
        .clone()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| {
            let s = target_block_id.to_string();
            format!("(({}))", &s[..s.len().min(8)])
        });
    let cls = if resolved {
        "inline-flex items-center gap-1 rounded bg-muted/40 px-1.5 py-0.5 text-[0.85em] text-foreground/85 hover:bg-muted/70 cursor-pointer"
    } else {
        "inline-flex items-center gap-1 rounded bg-destructive/10 px-1.5 py-0.5 text-[0.85em] text-destructive/90 cursor-pointer"
    };
    let title = if resolved {
        format!("(({target_block_id})) — click to expand")
    } else {
        format!("(({target_block_id})) — block not found")
    };
    let is_expanded = *expanded.read();
    // If expanded AND we can resolve the tree AND we haven't
    // hit depth cap, render the full subtree below the chip.
    let expansion = if is_expanded && resolved && depth < EMBED_MAX_DEPTH {
        tree.get(target_block_id).map(|(root, all_blocks)| {
            let mut by_parent: std::collections::HashMap<Option<Uuid>, Vec<Block>> =
                std::collections::HashMap::new();
            for b in &all_blocks {
                by_parent
                    .entry(b.parent_block_id)
                    .or_default()
                    .push(b.clone());
            }
            for siblings in by_parent.values_mut() {
                siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
            }
            (root, by_parent)
        })
    } else {
        None
    };
    rsx! {
        span {
            class: "inline-block align-top",
            "data-block-ref": "{target_block_id}",
            "data-expanded": "{is_expanded}",
            span {
                class: cls,
                title: title,
                onclick: move |e: Event<MouseData>| {
                    e.stop_propagation();
                    if resolved {
                        let cur = *expanded.peek();
                        expanded.set(!cur);
                    }
                },
                span { class: "text-muted-foreground/60",
                    if is_expanded { "▾ " } else { "▸ " }
                }
                span { class: "truncate max-w-[16rem]", "{label}" }
            }
            if let Some((root, by_parent)) = expansion {
                {
                    use_context_provider(|| EmbedDepth(depth + 1));
                    rsx! {
                        div { class: "mt-1 ml-3 rounded-md border border-border/60 bg-card/40 p-2 text-sm",
                            EmbedBlockRow {
                                block: root,
                                children_by_parent: by_parent,
                            }
                        }
                    }
                }
            }
        }
    }
}

/// `![[Page]]` embed render. Resolves the target via the
/// [`PageBlockLookup`] context and shows a bordered card with
/// the page title at the top + a read-only render of the page's
/// blocks underneath. Bounded by [`EmbedDepth`] so an embed
/// chain (`![[A]]` → A contains `![[B]]` → B contains `![[A]]`)
/// terminates instead of looping.
#[component]
fn EmbedCard(target: String, alias: Option<String>) -> Element {
    let display = alias.unwrap_or_else(|| target.clone());
    let testid = format!("embed-{display}");
    rsx! {
        span {
            "data-testid": testid,
            // Logseq-style: a left-rule + tight padding, no
            // heavy bordered card. Reads as a quoted insert
            // rather than a separate widget.
            class: "block my-1 pl-2 border-l-2 border-border/60",
            EmbedContent { target, alias: Some(display) }
        }
    }
}

/// Pure block-tree renderer for an embedded page — no header,
/// no border, no card chrome. Used by `EmbedCard` (inline
/// `![[Page]]`) and by the right-sidebar pinned-pane stack
/// (which provides its own header/card so it doesn't get
/// nested chrome).
#[component]
pub fn EmbedContent(target: String, alias: Option<String>) -> Element {
    let depth = try_use_context::<EmbedDepth>().unwrap_or_default().0;
    let lookup = try_use_context::<PageBlockLookup>().unwrap_or_default();
    let display = alias.unwrap_or_else(|| target.clone());
    let resolved = lookup.blocks_for(&target);

    if depth >= EMBED_MAX_DEPTH {
        return rsx! {
            span {
                class: "text-[11px] italic text-amber-700 dark:text-amber-400",
                "↵ ![[{display}]] (embed depth limit)"
            }
        };
    }
    let Some(blocks) = resolved else {
        return rsx! {
            span {
                class: "text-[11px] italic text-destructive",
                "![[{display}]] — page not found"
            }
        };
    };
    use_context_provider(|| EmbedDepth(depth + 1));
    let mut by_parent: std::collections::HashMap<Option<Uuid>, Vec<Block>> =
        std::collections::HashMap::new();
    for b in &blocks {
        by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b.clone());
    }
    for siblings in by_parent.values_mut() {
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    let roots = by_parent.get(&None).cloned().unwrap_or_default();
    rsx! {
        if roots.is_empty() {
            span { class: "text-muted-foreground italic text-xs", "(empty)" }
        } else {
            for root in roots {
                EmbedBlockRow {
                    key: "{root.id}",
                    block: root.clone(),
                    children_by_parent: by_parent.clone(),
                }
            }
        }
    }
}

/// Read-only block row for embeds — renders the block's parsed
/// inline content + recursively renders children. No editor, no
/// chevron, no event handlers.
#[component]
fn EmbedBlockRow(
    block: Block,
    children_by_parent: std::collections::HashMap<Option<Uuid>, Vec<Block>>,
) -> Element {
    let id = block.id;
    let kind = block.kind.clone();
    let content = block.content.clone();
    let children = children_by_parent
        .get(&Some(id))
        .cloned()
        .unwrap_or_default();
    let task_glyph = task_state_glyph(&block);
    let inlines = inline_md::parse_inline(&content);
    let row_cls = match kind.as_str() {
        "heading" => match block.heading_level.unwrap_or(1).clamp(1, 6) {
            1 => "text-lg font-bold text-foreground py-0.5",
            2 => "text-base font-semibold text-foreground py-0.5",
            _ => "text-sm font-medium text-foreground py-0.5",
        },
        "code" => "bg-muted/40 rounded px-1.5 py-0.5 font-mono text-xs my-0.5 whitespace-pre",
        "blockquote" | "callout" => "italic text-foreground/85 py-0.5",
        _ => "py-0.5 leading-snug",
    };
    let nav = try_use_context::<NavigateLinkCb>()
        .map(|c| c.0)
        .unwrap_or_else(|| use_callback(|_: String| {}));
    rsx! {
        div { class: "flex items-start gap-1.5",
            span { class: "h-4 w-2 flex-none inline-flex items-center justify-center text-muted-foreground/70 select-none text-[10px]",
                "•"
            }
            div { class: "flex-1 min-w-0",
                div { class: row_cls,
                    if let Some(g) = task_glyph {
                        span { class: "mr-1 text-muted-foreground/80 select-none font-mono", "{g}" }
                    }
                    if kind == "code" {
                        "{content}"
                    } else if inlines.is_empty() {
                        span { class: "text-muted-foreground/40", " " }
                    } else {
                        for (i, inline) in inlines.into_iter().enumerate() {
                            InlineNode { key: "{i}", inline, on_navigate_link: nav }
                        }
                    }
                }
                if !children.is_empty() {
                    div { class: "ml-2 pl-2 border-l border-border/30",
                        for child in children {
                            EmbedBlockRow {
                                key: "{child.id}",
                                block: child.clone(),
                                children_by_parent: children_by_parent.clone(),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Whether we're rendering a block as the static view or as
/// the active textarea. Drives a small handful of class
/// differences (resize affordance, placeholder color) without
/// changing the metrics that view↔edit swap depends on.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BlockMode {
    View,
    Editor,
}

/// Single source of truth for per-kind text styling shared
/// between `BlockView` and `BlockEditor`. Font-size, font-weight,
/// line-height, padding, font-family — all matched exactly so
/// the view↔edit transition is metric-stable. Only the editor
/// adds `outline-none resize-none` (textarea-specific).
fn block_text_class(block: &Block, mode: BlockMode) -> &'static str {
    let editor = mode == BlockMode::Editor;
    // Editor variants:
    // - `whitespace-pre-wrap`: long lines wrap instead of
    //   horizontal-scrolling (default textarea behavior is to
    //   wrap; this is explicit + survives any future polyfills).
    // - `overflow-hidden`: hides the scrollbar during the
    //   sub-frame window where JS auto-grow hasn't matched
    //   scrollHeight yet. The JS sizes via `.style.height` on
    //   every input + on mount.
    // - `[field-sizing:content]`: native auto-grow on browsers
    //   that support it (Chrome 123+, fallback to JS otherwise).
    match block.kind.as_str() {
        "heading" => match block.heading_level.unwrap_or(1).clamp(1, 6) {
            1 => {
                if editor {
                    "block w-full bg-transparent font-mono text-2xl font-bold text-foreground leading-tight outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 p-0 m-0"
                } else {
                    "flex-1 font-mono text-2xl font-bold text-foreground leading-tight cursor-text min-h-[1.5rem] p-0 m-0"
                }
            }
            2 => {
                if editor {
                    "block w-full bg-transparent font-mono text-xl font-semibold text-foreground leading-tight outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 p-0 m-0"
                } else {
                    "flex-1 font-mono text-xl font-semibold text-foreground leading-tight cursor-text min-h-[1.5rem] p-0 m-0"
                }
            }
            _ => {
                if editor {
                    "block w-full bg-transparent font-mono text-lg font-medium text-foreground leading-tight outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 p-0 m-0"
                } else {
                    "flex-1 font-mono text-lg font-medium text-foreground leading-tight cursor-text min-h-[1.5rem] p-0 m-0"
                }
            }
        },
        "code" => {
            if editor {
                "block w-full bg-muted/30 rounded font-mono text-xs text-foreground leading-snug outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 px-2 py-1"
            } else {
                "flex-1 bg-muted/30 rounded font-mono text-xs text-foreground leading-snug whitespace-pre cursor-text min-h-[1.25rem] px-2 py-1"
            }
        }
        "blockquote" | "callout" => {
            if editor {
                "block w-full bg-transparent font-mono text-sm italic text-foreground/90 leading-snug outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 p-0 m-0"
            } else {
                "flex-1 font-mono text-sm italic text-foreground/90 leading-snug cursor-text min-h-[1.25rem] p-0 m-0"
            }
        }
        _ => {
            if editor {
                "block w-full bg-transparent font-mono text-sm text-foreground leading-snug outline-none resize-none whitespace-pre-wrap [field-sizing:content] placeholder:text-muted-foreground/60 p-0 m-0"
            } else {
                "flex-1 font-mono text-sm text-foreground leading-snug cursor-text min-h-[1.25rem] whitespace-pre-wrap p-0 m-0"
            }
        }
    }
}

/// Task-state checkbox glyph for a list_item block. Returns
/// `None` for non-task blocks.
fn task_state_glyph(b: &Block) -> Option<&'static str> {
    match b.list_task.as_deref() {
        Some(" ") => Some("[ ]"),
        Some("/") => Some("[/]"),
        Some("x") => Some("[x]"),
        _ => None,
    }
}

// ── Slash commands ─────────────────────────────────────────────────

/// One command in the slash palette.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SlashCommand {
    /// Trigger text following the `/`, e.g. `"h1"`, `"todo"`,
    /// `"today"`. Lowercase.
    pub trigger: &'static str,
    /// Human label shown in the palette.
    pub label: &'static str,
    /// What the command does when picked.
    pub effect: SlashEffect,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SlashEffect {
    /// Change the active block's kind (and optional heading/task
    /// metadata). Strips the `/trigger` text from the content.
    SetKind {
        kind: &'static str,
        heading_level: Option<i32>,
        list_task: Option<&'static str>,
    },
    /// Replace the `/trigger` text with the given literal in the
    /// block content. Used for `/today`, `/tomorrow`, etc.
    InsertText(SlashTextKind),
}

/// Tagged "compute this string at apply time" — keeps the
/// command table free of host-environment lookups.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SlashTextKind {
    /// Today as `YYYY-MM-DD`.
    Today,
    /// Tomorrow as `YYYY-MM-DD`.
    Tomorrow,
}

pub fn default_slash_commands() -> Vec<SlashCommand> {
    use SlashEffect::*;
    vec![
        SlashCommand {
            trigger: "h1",
            label: "Heading 1",
            effect: SetKind {
                kind: "heading",
                heading_level: Some(1),
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "h2",
            label: "Heading 2",
            effect: SetKind {
                kind: "heading",
                heading_level: Some(2),
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "h3",
            label: "Heading 3",
            effect: SetKind {
                kind: "heading",
                heading_level: Some(3),
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "todo",
            label: "TODO list-item",
            effect: SetKind {
                kind: "list_item",
                heading_level: None,
                list_task: Some(" "),
            },
        },
        SlashCommand {
            trigger: "doing",
            label: "DOING list-item",
            effect: SetKind {
                kind: "list_item",
                heading_level: None,
                list_task: Some("/"),
            },
        },
        SlashCommand {
            trigger: "done",
            label: "DONE list-item",
            effect: SetKind {
                kind: "list_item",
                heading_level: None,
                list_task: Some("x"),
            },
        },
        SlashCommand {
            trigger: "list",
            label: "Bulleted list",
            effect: SetKind {
                kind: "list_item",
                heading_level: None,
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "code",
            label: "Code block",
            effect: SetKind {
                kind: "code",
                heading_level: None,
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "quote",
            label: "Quote",
            effect: SetKind {
                kind: "blockquote",
                heading_level: None,
                list_task: None,
            },
        },
        SlashCommand {
            trigger: "today",
            label: "Today's date",
            effect: InsertText(SlashTextKind::Today),
        },
        SlashCommand {
            trigger: "tomorrow",
            label: "Tomorrow's date",
            effect: InsertText(SlashTextKind::Tomorrow),
        },
    ]
}

/// Detect a pending `/query` trigger at the end of the content.
/// `query` is the text after the trailing `/` with no
/// whitespace, no newline. `None` if no trigger is active.
pub fn pending_slash_query(content: &str) -> Option<String> {
    let last_slash = content.rfind('/')?;
    let after = &content[last_slash + 1..];
    if after.contains(char::is_whitespace) || after.is_empty() {
        // Empty trigger (just `/` typed) is allowed — show the
        // full menu. Other special cases (URLs etc.) check for a
        // preceding non-whitespace boundary.
        if !after.is_empty() {
            return None;
        }
    }
    // Reject `://` (URL-ish) — the slash must be at start of
    // the content or preceded by whitespace.
    let preceded_ok = last_slash == 0
        || content[..last_slash]
            .chars()
            .last()
            .map(|c| c.is_whitespace())
            .unwrap_or(false);
    if !preceded_ok {
        return None;
    }
    Some(after.to_lowercase())
}

/// Strip the trailing `/query` from content and return the
/// remainder. Used when applying a SetKind command.
pub fn strip_slash_trigger(content: &str) -> String {
    let Some(last_slash) = content.rfind('/') else {
        return content.to_string();
    };
    content[..last_slash].trim_end().to_string()
}

/// Replace the trailing `/query` with `replacement`. Used for
/// InsertText commands.
pub fn replace_slash_trigger(content: &str, replacement: &str) -> String {
    let Some(last_slash) = content.rfind('/') else {
        return format!("{content}{replacement}");
    };
    let before = &content[..last_slash];
    format!("{before}{replacement}")
}

/// Scan `content` for an unclosed trailing `[[query` autocomplete
/// trigger. Returns the partial query (the text after the last
/// `[[` with no intervening `]]` or newline). `None` if the user
/// isn't currently typing a wikilink.
pub fn pending_wikilink_query(content: &str) -> Option<String> {
    let last_open = content.rfind("[[")?;
    let after = &content[last_open + 2..];
    if after.contains("]]") || after.contains('\n') {
        return None;
    }
    Some(after.to_string())
}

/// Replace the unclosed `[[query` with `[[Page Name]]`. If there's
/// no unclosed trigger, returns the content unchanged.
pub fn complete_wikilink(content: &str, page_name: &str) -> String {
    let Some(last_open) = content.rfind("[[") else {
        return content.to_string();
    };
    let prefix = &content[..last_open + 2];
    format!("{prefix}{page_name}]]")
}

/// `[^id]: body` block-level footnote definition. Returns
/// `(id, body)`. Body keeps the original content trimmed; `parse_inline`
/// runs over it at render time so emphasis/links nest. Continuation
/// lines (subsequent non-empty lines) are joined with newlines.
pub(crate) fn parse_footnote_def(content: &str) -> Option<(String, String)> {
    let trimmed = content.trim_start();
    let after_caret = trimmed.strip_prefix("[^")?;
    let id_end = after_caret.find(']')?;
    let id = after_caret[..id_end].trim();
    if id.is_empty() || id.contains(char::is_whitespace) {
        return None;
    }
    let rest = after_caret.get(id_end + 1..)?;
    let body_start = rest.strip_prefix(':')?;
    Some((id.to_string(), body_start.trim_start().to_string()))
}

/// Parsed GFM table. Headers + per-column alignments + body
/// rows. Cells keep their raw text so the renderer can re-run
/// `parse_inline` and nest emphasis/links inside.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Table {
    pub headers: Vec<String>,
    pub alignments: Vec<TableAlign>,
    pub rows: Vec<Vec<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TableAlign {
    Left,
    Center,
    Right,
}

pub(crate) fn align_class(a: TableAlign) -> &'static str {
    match a {
        TableAlign::Left => "text-left",
        TableAlign::Center => "text-center",
        TableAlign::Right => "text-right",
    }
}

/// Split a pipe-row into cell strings. Strips a leading and
/// trailing `|` if present and trims each cell.
fn split_row(line: &str) -> Vec<String> {
    let trimmed = line.trim();
    let inner = trimmed.strip_prefix('|').unwrap_or(trimmed);
    let inner = inner.strip_suffix('|').unwrap_or(inner);
    inner.split('|').map(|s| s.trim().to_string()).collect()
}

/// Parse a GFM table from block content. Returns `None` if the
/// content doesn't match: needs ≥2 lines, second line must be
/// the alignment row (each cell `:?-+:?`), header + body cells
/// padded to header width.
pub(crate) fn parse_table(content: &str) -> Option<Table> {
    let mut lines = content.lines();
    let header_line = lines.next()?;
    let divider_line = lines.next()?;
    if !header_line.contains('|') {
        return None;
    }
    let headers = split_row(header_line);
    if headers.is_empty() {
        return None;
    }
    let divider_cells = split_row(divider_line);
    if divider_cells.len() != headers.len() {
        return None;
    }
    let mut alignments = Vec::with_capacity(divider_cells.len());
    for cell in &divider_cells {
        let s = cell.trim();
        if s.is_empty() || !s.chars().all(|c| c == '-' || c == ':') {
            return None;
        }
        let starts = s.starts_with(':');
        let ends = s.ends_with(':');
        alignments.push(match (starts, ends) {
            (true, true) => TableAlign::Center,
            (false, true) => TableAlign::Right,
            _ => TableAlign::Left,
        });
    }
    let mut rows = Vec::new();
    for line in lines {
        if !line.contains('|') {
            continue;
        }
        let mut cells = split_row(line);
        cells.resize(headers.len(), String::new());
        rows.push(cells);
    }
    Some(Table {
        headers,
        alignments,
        rows,
    })
}

/// Parsed Obsidian-flavored callout. First line must be
/// `> [!type] optional title`, subsequent lines start with `> `.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Callout {
    pub kind: String,
    pub title: Option<String>,
    pub body: String,
}

impl Callout {
    pub fn label(&self) -> &str {
        match self.kind.as_str() {
            "note" | "info" => "NOTE",
            "tip" | "hint" => "TIP",
            "success" | "check" | "done" => "SUCCESS",
            "question" | "help" | "faq" => "QUESTION",
            "warning" | "caution" | "attention" => "WARNING",
            "failure" | "fail" | "missing" => "FAILURE",
            "danger" | "error" => "DANGER",
            "bug" => "BUG",
            "example" => "EXAMPLE",
            "quote" | "cite" => "QUOTE",
            _ => "NOTE",
        }
    }
    pub fn classes(&self) -> &str {
        match self.kind.as_str() {
            "tip" | "hint" | "success" | "check" | "done" => {
                "border-l-4 border-emerald-500 bg-emerald-500/10 pl-3 py-2 rounded-r-md text-emerald-200/90"
            }
            "warning" | "caution" | "attention" => {
                "border-l-4 border-amber-500 bg-amber-500/10 pl-3 py-2 rounded-r-md text-amber-200/90"
            }
            "danger" | "error" | "failure" | "fail" | "bug" => {
                "border-l-4 border-rose-500 bg-rose-500/10 pl-3 py-2 rounded-r-md text-rose-200/90"
            }
            "question" | "help" | "faq" => {
                "border-l-4 border-violet-500 bg-violet-500/10 pl-3 py-2 rounded-r-md text-violet-200/90"
            }
            "quote" | "cite" => {
                "border-l-4 border-muted-foreground/40 bg-muted/30 pl-3 py-2 rounded-r-md italic text-muted-foreground"
            }
            _ => "border-l-4 border-sky-500 bg-sky-500/10 pl-3 py-2 rounded-r-md text-sky-200/90",
        }
    }
    pub fn icon(&self) -> Element {
        let cls = "h-4 w-4";
        match self.kind.as_str() {
            "tip" | "hint" => rsx! { Lightbulb { class: cls } },
            "success" | "check" | "done" => rsx! { CircleCheck { class: cls } },
            "warning" | "caution" | "attention" => rsx! { TriangleAlert { class: cls } },
            "danger" | "error" | "failure" | "fail" | "bug" => {
                rsx! { CircleAlert { class: cls } }
            }
            "question" | "help" | "faq" => rsx! { CircleAlert { class: cls } },
            "quote" | "cite" => rsx! { Quote { class: cls } },
            _ => rsx! { Info { class: cls } },
        }
    }
}

/// Parse `> [!type] Title\n> body…`. Lines after the first must
/// be prefixed with `> ` (or be exactly `>`). Returns `None`
/// when the content isn't a callout — caller falls back to
/// inline-md rendering.
pub(crate) fn parse_callout(content: &str) -> Option<Callout> {
    let mut lines = content.lines();
    let first = lines.next()?;
    let rest = first.strip_prefix(">")?.trim_start();
    let after_bracket = rest.strip_prefix("[!")?;
    let close = after_bracket.find(']')?;
    let kind = after_bracket[..close].trim().to_lowercase();
    if kind.is_empty() {
        return None;
    }
    let title_raw = after_bracket[close + 1..].trim();
    let title = if title_raw.is_empty() {
        None
    } else {
        Some(title_raw.to_string())
    };
    let mut body_lines = Vec::new();
    for line in lines {
        let stripped = line
            .strip_prefix("> ")
            .or_else(|| line.strip_prefix(">"))
            .unwrap_or(line);
        body_lines.push(stripped);
    }
    let body = body_lines.join("\n");
    Some(Callout { kind, title, body })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slash_at_block_start_triggers() {
        assert_eq!(pending_slash_query("/h1"), Some("h1".into()));
    }

    #[test]
    fn slash_after_word_does_not_trigger() {
        // `path/to/file` → not a slash command.
        assert_eq!(pending_slash_query("path/to/file"), None);
    }

    #[test]
    fn slash_after_whitespace_triggers() {
        assert_eq!(pending_slash_query("hello /code"), Some("code".into()));
    }

    #[test]
    fn bare_slash_returns_empty_query() {
        assert_eq!(pending_slash_query("/"), Some(String::new()));
        assert_eq!(pending_slash_query("hello /"), Some(String::new()));
    }

    #[test]
    fn closed_slash_no_trigger() {
        assert_eq!(pending_slash_query("/h1 done"), None);
    }

    #[test]
    fn strip_trigger_removes_slash() {
        assert_eq!(strip_slash_trigger("hello /h1"), "hello".to_string());
        assert_eq!(strip_slash_trigger("/h1"), "".to_string());
    }

    #[test]
    fn replace_trigger_inserts() {
        assert_eq!(
            replace_slash_trigger("today is /today", "[[2026-05-14]]"),
            "today is [[2026-05-14]]".to_string()
        );
    }

    #[test]
    fn callout_parses_note_with_title_and_body() {
        let s = "> [!note] Heads up\n> first\n> second";
        let c = parse_callout(s).expect("callout");
        assert_eq!(c.kind, "note");
        assert_eq!(c.title.as_deref(), Some("Heads up"));
        assert_eq!(c.body, "first\nsecond");
    }

    #[test]
    fn callout_no_match_on_plain_quote() {
        assert!(parse_callout("> just a quote").is_none());
    }

    #[test]
    fn table_parses_with_alignments() {
        let s = "| a | b | c |\n|:--|:--:|--:|\n| 1 | 2 | 3 |\n| 4 | 5 | 6 |";
        let t = parse_table(s).expect("table");
        assert_eq!(t.headers, vec!["a", "b", "c"]);
        assert_eq!(
            t.alignments,
            vec![TableAlign::Left, TableAlign::Center, TableAlign::Right]
        );
        assert_eq!(t.rows.len(), 2);
        assert_eq!(t.rows[0], vec!["1", "2", "3"]);
    }

    #[test]
    fn table_rejects_no_divider() {
        // Header pipes but no divider row → not a table.
        assert!(parse_table("| a | b |\n| 1 | 2 |").is_none());
    }

    #[test]
    fn footnote_def_parses() {
        let (id, body) = parse_footnote_def("[^1]: actually it's fine").expect("footnote def");
        assert_eq!(id, "1");
        assert_eq!(body, "actually it's fine");
    }

    #[test]
    fn footnote_def_rejects_plain_link() {
        assert!(parse_footnote_def("[hello](world)").is_none());
        assert!(parse_footnote_def("[^1] no colon").is_none());
    }

    #[test]
    fn callout_kind_is_lowercased() {
        let c = parse_callout("> [!WARNING] Title").expect("callout");
        assert_eq!(c.kind, "warning");
    }

    #[test]
    fn default_commands_include_h1_todo_today() {
        let cmds = default_slash_commands();
        let triggers: Vec<&str> = cmds.iter().map(|c| c.trigger).collect();
        assert!(triggers.contains(&"h1"));
        assert!(triggers.contains(&"todo"));
        assert!(triggers.contains(&"today"));
        assert!(triggers.contains(&"code"));
    }
}
