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
/// `DocTile`; recursion walks into composites.
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
            // Wrap in a span carrying `data-tile-pos` so the
            // JS bridge can translate `Selection.anchorNode`
            // (the text node inside) directly to a doc offset
            // without walking the rest of the document. This
            // is CM6's `dom.cmTile` expando
            // (`tile.ts:11-12`) in data-attribute form.
            //
            // Wrapping in a span doesn't disturb the cursor on
            // typing: the *text node* is what mutates when the
            // user types, and Dioxus's reconciler is a no-op
            // when its rendered text matches what the DOM
            // already has. The span's `data-tile-pos`
            // attribute stays the same unless this tile's
            // position changes (which only happens when an
            // earlier tile changes size — not on local typing).
            let text = text_of(t);
            let pos = crate::tile::pos::pos_at_start(arena, tile);
            rsx! {
                span {
                    "data-tile-id": "{tid}",
                    "data-tile-pos": "{pos}",
                    "{text}"
                }
            }
        }
        TileKind::Mark => {
            let spec = mark_spec_of(t).clone();
            let class = spec.class;
            let pos = crate::tile::pos::pos_at_start(arena, tile);
            // v1 only emits `<span>` from buildtile; once tag
            // variation lands we'll match on `spec.tag`.
            //
            // Empty MarkTiles still render as a span — they
            // act as a stable insertion point for the cursor
            // and keep the DOM shape consistent when the mark
            // later fills with content. We don't add any
            // placeholder inside (no <br>, no ZWSP): the
            // writeback's `placeEdge` second pass detects
            // empty tiles and sets the range to `(span, 0)`,
            // which lets the browser place the caret inside
            // the otherwise-empty span. (CSS `:empty:before {
            // content: "\200B" }` could be added later if some
            // browsers need a visual anchor.)
            rsx! {
                span {
                    class: "{class}",
                    "data-tile-id": "{tid}",
                    "data-tile-pos": "{pos}",
                    for &child in &t.children {
                        {render_tile(arena, child)}
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
                let pos = crate::tile::pos::pos_at_start(arena, tile);
                rsx! {
                    span {
                        class: "editor-widget",
                        contenteditable: "false",
                        "data-tile-id": "{tid}",
                        "data-tile-pos": "{pos}",
                        dangerous_inner_html: "{html}"
                    }
                }
            }
        }
        TileKind::Line => {
            let pos = crate::tile::pos::pos_at_start(arena, tile);
            // Empty lines render with a `<br>` so the contenteditable
            // browser has somewhere to anchor the cursor. Without
            // it, `<div class="cm-line">` is zero-height and
            // Selection lands at the div root (offset 0) which
            // walks past our data-tile-pos. `<br>` doesn't add
            // visible text (matches our LineTile semantics where
            // the `\n` belongs to BreakAfter, not the line's
            // content).
            if t.children.is_empty() {
                rsx! {
                    div {
                        class: "cm-line",
                        "data-tile-id": "{tid}",
                        "data-tile-pos": "{pos}",
                        br {}
                    }
                }
            } else {
                rsx! {
                    div {
                        class: "cm-line",
                        "data-tile-id": "{tid}",
                        "data-tile-pos": "{pos}",
                        for &child in &t.children {
                            {render_tile(arena, child)}
                        }
                    }
                }
            }
        }
        TileKind::BlockWrapper => {
            // Same shape as Line for now; v1 has no block
            // decorations carrying their own DOM.
            let pos = crate::tile::pos::pos_at_start(arena, tile);
            rsx! {
                div {
                    "data-tile-id": "{tid}",
                    "data-tile-pos": "{pos}",
                    for &child in &t.children {
                        {render_tile(arena, child)}
                    }
                }
            }
        }
    }
}
