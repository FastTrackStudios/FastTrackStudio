//! `buildtile` — build a tile tree from a `Doc` + decoration
//! set. This is the Rust port of CM6's
//! `view/src/buildtile.ts:39+` (the `TileBuilder` class) +
//! the main `TileUpdate.build` entry point.
//!
//! ## Phase 5 scope: flat under Doc
//!
//! Per the port roadmap (`docs/tile-tree-port.md`), this first
//! pass produces a **flat** tree:
//!
//! ```text
//! DocTile
//! ├── TextTile  "he"
//! ├── MarkTile (md-bold)
//! │   └── TextTile  "llo"
//! ├── WidgetTile (hidden, replaces "**")
//! ├── TextTile  " world"
//! └── ...
//! ```
//!
//! No `LineTile` between `DocTile` and inline children yet —
//! that lands in Phase 8 once we have block-level decorations
//! to motivate it. Newlines are still respected: TextTiles
//! never span a `\n`.
//!
//! ## What's NOT here yet, intentionally
//!
//! - **Tile cache / reuse.** CM6's `TileCache` recycles DOM
//!   nodes from the previous build. We rebuild from scratch
//!   each render; Dioxus's reconciler handles DOM reuse.
//! - **Composition handling.** IME composition crosses many
//!   builds and CM6 has explicit "carry forward composition"
//!   logic (`addComposition`). Comes in Phase 10.
//! - **Block wrappers + line decorations.** No block-level
//!   decoration set today.
//! - **Mark stacking algorithm.** CM6's `ensureMarks` opens
//!   and closes mark wrappers as we walk; ours is simpler
//!   because we rebuild the mark stack at every boundary.
//! - **`openStart`/`openEnd` for wrapper carry-over** — only
//!   needed when reusing across builds. We rebuild.
//!
//! Each gap above ports cleanly later without re-architecting.

use editor_state::{DecoratedRange, DecorationKind};

use crate::tile::arena::{Arena, TileId};
use crate::tile::flag::TileFlagSet;
use crate::tile::mark::{MarkSpec, new_mark_tile};
use crate::tile::text::new_text_tile;
use crate::tile::widget::{new_widget_buffer_tile, new_widget_tile};
use crate::tile::{Tile, TileBody, TileKind};

/// Build the tile tree for `text` + `decorations`. Returns the
/// fresh arena and the root `DocTile`'s id.
///
/// `decorations` must be sorted by `from` ascending. Within
/// ties: Replace decorations are applied before Mark
/// decorations at the same start offset (so a Replace doesn't
/// get accidentally wrapped in a Mark).
pub fn build_tiles(text: &str, decorations: &[DecoratedRange]) -> (Arena, TileId) {
    let mut arena = Arena::new();
    let doc_id = arena.insert(Tile {
        parent: None,
        children: Vec::new(),
        length: text.len(),
        kind: TileKind::Doc,
        body: TileBody::Empty,
        flags: TileFlagSet::empty(),
    });

    let mut builder = TileBuilder::new(&mut arena, doc_id);
    builder.run(text, decorations);
    (arena, doc_id)
}

/// Mutable state during a build pass. Ports CM6's
/// `TileBuilder` (`buildtile.ts:39-240`), trimmed to the v1
/// surface.
struct TileBuilder<'a> {
    arena: &'a mut Arena,
    root: TileId,
    /// Doc byte offset where the next emitted tile starts.
    pos: usize,
}

impl<'a> TileBuilder<'a> {
    fn new(arena: &'a mut Arena, root: TileId) -> Self {
        Self {
            arena,
            root,
            pos: 0,
        }
    }

    /// Main loop. Walks `text` + `decorations` together and
    /// emits tiles. Single pass, O(n + d log d) once decorations
    /// are sorted.
    fn run(&mut self, text: &str, decorations: &[DecoratedRange]) {
        // Collect "events" — decoration boundaries — sorted by
        // position. For each event we know what changes at that
        // offset (a mark starts, ends, a replace covers a range,
        // a widget appears at a point).
        //
        // We walk from one event to the next, emitting plain
        // text in between under the active mark stack.
        let mut events = collect_events(decorations);
        events.sort_by_key(|e| e.at);

        // Decorations active right now. We rebuild the mark
        // stack each segment from this set; cheap because the
        // set is tiny (one or two marks in practice).
        let mut active_marks: Vec<MarkSpec> = Vec::new();
        let mut active_replace_until: Option<usize> = None;

        let mut event_idx = 0;
        while self.pos < text.len() || event_idx < events.len() {
            // Apply all events whose position == self.pos. A
            // Replace event opens a hidden range until its
            // `to`; a Mark event opens a mark wrapper for its
            // span.
            while event_idx < events.len() && events[event_idx].at == self.pos {
                let ev = &events[event_idx];
                event_idx += 1;
                match ev.kind {
                    EventKind::MarkStart(ref spec) => active_marks.push(spec.clone()),
                    EventKind::MarkEnd(ref spec) => {
                        if let Some(p) = active_marks.iter().rposition(|m| m == spec) {
                            active_marks.remove(p);
                        }
                    }
                    EventKind::ReplaceStart { to } => {
                        active_replace_until = Some(to);
                    }
                    EventKind::ReplaceEnd => {
                        active_replace_until = None;
                    }
                    EventKind::Widget { length, ref html } => {
                        self.emit_widget(html, length, &active_marks);
                    }
                }
            }

            // What's the next event position? Cap any emitted
            // segment so it ends at the next event (and we
            // re-enter the inner loop above).
            let next_event_pos = events
                .get(event_idx)
                .map(|e| e.at)
                .unwrap_or(text.len());

            if let Some(replace_until) = active_replace_until {
                // We're inside a Replace decoration: emit one
                // hidden widget covering this run, advance pos
                // to the replace's end (or the next event,
                // whichever is smaller — overlapping replaces
                // we resolve as "first wins").
                let until = replace_until.min(text.len());
                if until > self.pos {
                    self.emit_hidden(until - self.pos);
                }
                // Once we've reached replace_until, close it
                // out so the outer loop picks up the
                // ReplaceEnd event.
                if self.pos >= replace_until {
                    active_replace_until = None;
                }
                continue;
            }

            // Plain text run from self.pos to next_event_pos,
            // split at `\n` boundaries (lines are still a single
            // text node in v1 — Phase 8 will introduce LineTile).
            let end = next_event_pos.min(text.len());
            if end > self.pos {
                self.emit_text_run(&text[self.pos..end], &active_marks);
            } else if event_idx >= events.len() {
                // No events left and we're at end of text — done.
                break;
            }
        }
    }

    /// Emit text spanning `slice` under the current mark stack.
    /// Splits at `\n` so a single text tile never crosses a
    /// line break (consistent with CM6's `LineView`-per-line
    /// model, even though we don't have LineView yet).
    fn emit_text_run(&mut self, slice: &str, marks: &[MarkSpec]) {
        let mut local = 0;
        for (i, ch) in slice.char_indices() {
            if ch == '\n' {
                if i > local {
                    self.emit_text(&slice[local..i], marks);
                }
                // Newline char itself becomes part of nothing —
                // it lives in the doc but isn't a tile. We
                // still advance pos so positions stay right.
                self.pos += 1;
                local = i + 1;
            }
        }
        if local < slice.len() {
            self.emit_text(&slice[local..], marks);
        }
    }

    /// Emit one TextTile under the current mark stack. Wraps
    /// the text in `MarkTile`s outermost-first to match the
    /// CSS class layering you'd expect (`<span class="bold">
    /// <span class="italic">…</span></span>`).
    fn emit_text(&mut self, text: &str, marks: &[MarkSpec]) {
        let text_tile = self.insert_under_marks(marks, |arena| {
            arena.insert(new_text_tile(text))
        });
        let len = text.len();
        // Bubble length up the parent chain.
        self.bump_lengths_up(text_tile, len);
        self.pos += len;
    }

    /// Emit a widget under the current mark stack.
    fn emit_widget(&mut self, html: &str, length: usize, marks: &[MarkSpec]) {
        let widget_tile = self.insert_under_marks(marks, |arena| {
            arena.insert(new_widget_tile(html, length, TileFlagSet::empty()))
        });
        self.bump_lengths_up(widget_tile, length);
        self.pos += length;
    }

    /// Emit a hidden replacement. Length covers the replaced
    /// doc bytes; no DOM is produced (renderer skips Widget
    /// tiles with empty html and no widget content).
    fn emit_hidden(&mut self, length: usize) {
        let mut flags = TileFlagSet::empty();
        // Mark with both IncStart + IncEnd so adjacent typing
        // doesn't accidentally extend into the hidden range.
        flags.insert(crate::tile::flag::TileFlag::IncStart);
        flags.insert(crate::tile::flag::TileFlag::IncEnd);
        let hidden = self.arena.insert(new_widget_tile("", length, flags));
        self.append_to(self.root, hidden);
        self.bump_lengths_up(hidden, length);
        self.pos += length;
    }

    /// Walk the mark stack from outermost to innermost,
    /// finding (or creating) a MarkTile of each spec under the
    /// previous one. Returns the inner-most parent into which
    /// the caller should insert their text/widget tile.
    fn insert_under_marks(
        &mut self,
        marks: &[MarkSpec],
        emit_leaf: impl FnOnce(&mut Arena) -> TileId,
    ) -> TileId {
        let mut parent = self.root;
        for spec in marks {
            // Try to reuse the parent's last child if it's the
            // same mark (CM6's `ensureMarks` open-stack logic
            // boils down to this lookback).
            let reuse = self
                .arena
                .get(parent)
                .children
                .last()
                .copied()
                .filter(|&id| match &self.arena.get(id).body {
                    TileBody::Mark { spec: existing } => existing == spec,
                    _ => false,
                });
            let next_parent = if let Some(id) = reuse {
                id
            } else {
                let mark_id = self.arena.insert(new_mark_tile(spec.clone()));
                self.append_to(parent, mark_id);
                mark_id
            };
            parent = next_parent;
        }
        let leaf = emit_leaf(self.arena);
        self.append_to(parent, leaf);
        leaf
    }

    /// Append `child` as a child of `parent`, wiring parent
    /// pointer.
    fn append_to(&mut self, parent: TileId, child: TileId) {
        self.arena.get_mut(child).parent = Some(parent);
        self.arena.get_mut(parent).children.push(child);
    }

    /// Walk up from `tile` adding `length` to each ancestor's
    /// length. Stops at the root (DocTile) because we set its
    /// length once at the top of `build_tiles` from the full
    /// `text.len()` — bumping it again would double-count.
    /// (Intermediate composites like MarkTile still need this
    /// to sum their children correctly.)
    fn bump_lengths_up(&mut self, tile: TileId, length: usize) {
        let mut cur = self.arena.get(tile).parent;
        while let Some(p) = cur {
            if p == self.root {
                break;
            }
            self.arena.get_mut(p).length += length;
            cur = self.arena.get(p).parent;
        }
    }
}

/// One thing that happens at a doc offset. The builder walks
/// these in order; everything in between events is plain text
/// under the current mark stack.
#[derive(Debug, Clone)]
struct Event {
    at: usize,
    kind: EventKind,
}

#[derive(Debug, Clone)]
enum EventKind {
    MarkStart(MarkSpec),
    MarkEnd(MarkSpec),
    ReplaceStart { to: usize },
    ReplaceEnd,
    Widget { length: usize, html: String },
}

/// Turn a decoration set into start/end events.
fn collect_events(decorations: &[DecoratedRange]) -> Vec<Event> {
    let mut out = Vec::new();
    for d in decorations {
        match &d.kind {
            DecorationKind::Mark { class } => {
                let spec = MarkSpec::span_class(class);
                out.push(Event {
                    at: d.from,
                    kind: EventKind::MarkStart(spec.clone()),
                });
                out.push(Event {
                    at: d.to,
                    kind: EventKind::MarkEnd(spec),
                });
            }
            DecorationKind::Replace => {
                out.push(Event {
                    at: d.from,
                    kind: EventKind::ReplaceStart { to: d.to },
                });
                out.push(Event {
                    at: d.to,
                    kind: EventKind::ReplaceEnd,
                });
            }
            DecorationKind::Widget { html } => {
                out.push(Event {
                    at: d.from,
                    kind: EventKind::Widget {
                        length: d.to - d.from,
                        html: html.clone(),
                    },
                });
            }
            DecorationKind::Line { .. } => {
                // Line decorations not handled in Phase 6.
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use editor_state::Decoration;

    fn children_kinds(arena: &Arena, parent: TileId) -> Vec<TileKind> {
        arena
            .get(parent)
            .children
            .iter()
            .map(|&c| arena.get(c).kind.clone())
            .collect()
    }

    #[test]
    fn empty_text_no_decorations_makes_lone_doc() {
        let (arena, doc) = build_tiles("", &[]);
        assert_eq!(arena.get(doc).kind, TileKind::Doc);
        assert!(arena.get(doc).children.is_empty());
        assert_eq!(arena.get(doc).length, 0);
    }

    #[test]
    fn plain_text_one_text_tile() {
        let (arena, doc) = build_tiles("hello", &[]);
        assert_eq!(children_kinds(&arena, doc), vec![TileKind::Text]);
        let text = arena.get(doc).children[0];
        assert_eq!(arena.get(text).length, 5);
    }

    #[test]
    fn one_mark_wraps_text() {
        let decs = vec![Decoration::mark(2..4, "bold")];
        let (arena, doc) = build_tiles("hello", &decs);
        // Expected: Text(0..2), Mark(2..4){Text}, Text(4..5)
        let kids = children_kinds(&arena, doc);
        assert_eq!(kids, vec![TileKind::Text, TileKind::Mark, TileKind::Text]);
        let mark = arena.get(doc).children[1];
        assert_eq!(arena.get(mark).length, 2);
        // The mark wraps a single text tile.
        assert_eq!(arena.get(mark).children.len(), 1);
    }

    #[test]
    fn replace_emits_hidden_widget() {
        let decs = vec![Decoration::replace(0..2)];
        let (arena, doc) = build_tiles("**bold**", &decs);
        let kids = children_kinds(&arena, doc);
        // First child should be a Widget (hidden) for the replace.
        assert!(matches!(kids[0], TileKind::Widget));
        let hidden = arena.get(doc).children[0];
        assert_eq!(arena.get(hidden).length, 2);
    }

    #[test]
    fn doc_length_sums_children() {
        let decs = vec![Decoration::mark(2..4, "bold")];
        let (arena, doc) = build_tiles("hello", &decs);
        assert_eq!(arena.get(doc).length, 5);
    }

    #[test]
    fn newline_does_not_split_text_tile_in_phase5() {
        // Phase 5 has no LineTile; the only effect of a `\n`
        // in v1 is that it ends one text tile and starts
        // another (preserving positions). Verify both tiles
        // get created and pos accounting is correct.
        let (arena, doc) = build_tiles("ab\ncd", &[]);
        let kids = children_kinds(&arena, doc);
        assert_eq!(kids, vec![TileKind::Text, TileKind::Text]);
        let first = arena.get(doc).children[0];
        let second = arena.get(doc).children[1];
        assert_eq!(arena.get(first).length, 2);
        assert_eq!(arena.get(second).length, 2);
        assert_eq!(arena.get(doc).length, 5); // 2 + 1 (\n) + 2
    }

    #[test]
    fn nested_marks_share_parent_when_same_class() {
        // Two adjacent bold ranges produce ONE MarkTile with
        // both texts as children (CM6's same-mark merging).
        let decs = vec![
            Decoration::mark(0..2, "bold"),
            Decoration::mark(2..4, "bold"),
        ];
        let (arena, doc) = build_tiles("abcd", &decs);
        // Only one Mark child under Doc; covers full range.
        let kids = children_kinds(&arena, doc);
        assert_eq!(kids, vec![TileKind::Mark]);
        let mark = arena.get(doc).children[0];
        assert_eq!(arena.get(mark).length, 4);
        // Two text children inside the same mark.
        assert_eq!(arena.get(mark).children.len(), 2);
    }
}
