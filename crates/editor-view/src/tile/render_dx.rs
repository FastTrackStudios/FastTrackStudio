//! Dioxus render pass for the tile tree. The CM6 equivalent
//! is `DocView.sync` (`view/src/docview.ts:80+`) which
//! imperatively patches the DOM. We don't patch — Dioxus does
//! that for us. Our job is to emit the right rsx for the
//! current tile tree and let the reconciler diff against the
//! live DOM.
//!
//! Each emitted DOM element carries `data-tile-id="N"` so the
//! JS bridge can walk from `Selection.anchorNode` up to find
//! the owning tile and look up its doc position via the
//! arena. That's CM6's `dom.cmTile` expando (`tile.ts:11-12`)
//! in attribute form — required because our JS lives across
//! the wasm boundary and can't share heap with Rust.

use dioxus::prelude::*;

use crate::tile::arena::{Arena, TileId};
use crate::tile::mark::mark_spec_of;
use crate::tile::text::text_of;
use crate::tile::{TileBody, TileKind};

/// Render a tile and its descendants. The root call passes the
/// DocTile; recursion walks into composites.
pub fn render_tile(arena: &Arena, tile: TileId) -> Element {
    let t = arena.get(tile);
    let tid = tile.0;
    match t.kind {
        TileKind::Doc => rsx! {
            // The doc tile itself is rendered by the editor
            // component's outer <div contenteditable>. We just
            // emit children here.
            for &child in &t.children {
                {render_tile(arena, child)}
            }
        },
        TileKind::Text => {
            // Leaf: a bare text node. Wrapping in a span just
            // for `data-tile-id` would break the "matches DOM"
            // trick that keeps the cursor stable during typing
            // — Dioxus's text-node reconciler is a no-op for
            // unchanged text. Instead, we emit a zero-width
            // sentinel right after the text that JS uses to
            // associate the previous text node with this tile.
            //
            // Wait — actually the parent (often a MarkTile or
            // the DocTile div) carries data-tile-id, and the
            // text node is its direct child. The JS bridge can
            // walk up from a text node to its parent element
            // and read the parent's data-tile-id. For text
            // tiles whose parent is the Doc, the doc itself
            // gets data-tile-id. For mark-wrapped text, the
            // MarkTile span carries it. Both cases: walk up
            // one level.
            //
            // What's missing in v1: when the same parent has
            // multiple text children (two adjacent TextTiles
            // under one MarkTile), JS can't tell which one a
            // given anchor sits in. We address that by always
            // inserting a comment-or-empty span between
            // adjacent text-only siblings — but in practice
            // buildtile merges them into a single TextTile
            // (see Phase 6's same-mark merging). The "two
            // adjacent text tiles under doc" case happens
            // around mark/hidden/widget boundaries, where the
            // intermediate tile gives JS its bearings.
            let text = text_of(t);
            rsx! { "{text}" }
        }
        TileKind::Mark => {
            let spec = mark_spec_of(t).clone();
            let tag = spec.tag;
            let class = spec.class;
            // Dioxus's `tag!` macro is per-element, so we
            // expand the few we care about. v1 only emits
            // `<span>` from buildtile; once we add tag
            // variation we'll generalize.
            if tag == "span" {
                rsx! {
                    span {
                        class: "{class}",
                        "data-tile-id": "{tid}",
                        for &child in &t.children {
                            {render_tile(arena, child)}
                        }
                    }
                }
            } else {
                // Future: pluggable tag rendering.
                rsx! {
                    span {
                        class: "{class}",
                        "data-tile-id": "{tid}",
                        for &child in &t.children {
                            {render_tile(arena, child)}
                        }
                    }
                }
            }
        }
        TileKind::Widget | TileKind::WidgetBuffer => {
            let html = match &t.body {
                TileBody::Widget { html } => html.clone(),
                _ => String::new(),
            };
            if html.is_empty() {
                // Hidden replacement: emit nothing. The tile
                // still occupies its doc-range, and the JS
                // bridge can't see DOM here — that's fine,
                // selection inside a hidden range is impossible
                // by construction (the bytes aren't rendered).
                rsx! {}
            } else {
                rsx! {
                    span {
                        class: "editor-widget",
                        contenteditable: "false",
                        "data-tile-id": "{tid}",
                        dangerous_inner_html: "{html}"
                    }
                }
            }
        }
        // Block layer not in Phase 5 — emit children flat for
        // now. Phase 8 makes these honest LineTile/BlockTile
        // wrappers.
        TileKind::Line | TileKind::BlockWrapper => rsx! {
            for &child in &t.children {
                {render_tile(arena, child)}
            }
        },
    }
}
