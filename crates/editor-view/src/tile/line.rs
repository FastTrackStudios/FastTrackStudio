//! `LineTile` constructor. A composite tile representing one
//! logical line of the document. Renders as
//! `<div class="cm-line">`.
//!
//! CM6 reference: `tile.ts:286-355` (the `LineTile` class). We
//! port the constructor + the default `cm-line` class; the
//! `resolveInline` / `coordsIn` / `domIn` methods from CM6 are
//! satisfied by our generic [`crate::tile::dom_pos`] walker
//! since our v1 LineTile carries no per-line attrs beyond the
//! base class. Line decorations from extensions would extend
//! [`TileBody`] with `Line { attrs }` later.

use crate::tile::flag::TileFlagSet;
use crate::tile::{Tile, TileBody, TileKind};

/// Build a new [`Tile`] of [`TileKind::Line`]. Empty until
/// inline children are appended.
///
/// Mirrors `LineTile.start` (`tile.ts:293-297`).
pub fn new_line_tile() -> Tile {
    Tile {
        parent: None,
        children: Vec::new(),
        length: 0,
        kind: TileKind::Line,
        body: TileBody::Empty,
        flags: TileFlagSet::empty(),
    }
}
