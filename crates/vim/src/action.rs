//! The vocabulary of actions the host editor knows how to apply.
//!
//! Kept deliberately small for v1. The vim engine is the *source*
//! of these actions; the host (knowledge-ui's outliner today,
//! sidebar nav tomorrow) is the *sink* that interprets them
//! against its own data model.
//!
//! This is intentionally NOT `editor_types::Action` — that crate's
//! action enum is large and assumes a rope-buffer model. We'll
//! migrate to it incrementally as our editor grows to need its
//! richer vocabulary; for now, a small purpose-built enum keeps
//! the wire-up easy to follow.

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum InsertEntry {
    /// `i` — insert before cursor.
    BeforeCursor,
    /// `a` — insert after cursor.
    AfterCursor,
    /// `I` — insert at first non-blank of line.
    LineStart,
    /// `A` — insert at end of line.
    LineEnd,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Motion {
    CharLeft,  // h
    CharRight, // l
    LineDown,  // j
    LineUp,    // k
    /// `w` — start of next word (alphanumeric run boundary).
    WordForward,
    /// `b` — start of previous word.
    WordBackward,
    /// `e` — end of current/next word.
    WordEnd,
    /// `W` — start of next WORD (whitespace-delimited).
    BigWordForward,
    /// `B` — start of previous WORD.
    BigWordBackward,
    /// `E` — end of current/next WORD.
    BigWordEnd,
    LineStart, // 0
    LineEnd,   // $
    DocStart,  // gg
    DocEnd,    // G
    /// `f{ch}` / `F{ch}` / `t{ch}` / `T{ch}` — find char on the
    /// current line. `direction: 1` is forward, `-1` is backward.
    /// `till: true` stops one char before the target (`t`/`T`),
    /// `false` lands on it (`f`/`F`).
    FindChar {
        ch: char,
        direction: i8,
        till: bool,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum BlockOp {
    /// `o` — new block below current, enter insert.
    NewBelow,
    /// `O` — new block above current, enter insert.
    NewAbove,
    /// `dd` — delete current block.
    DeleteCurrent,
    /// `>>` — indent current block.
    IndentRight,
    /// `<<` — outdent current block.
    IndentLeft,
    /// `yy` — yank (copy) the current block into the engine's
    /// register. Host doesn't have to do anything — the engine
    /// captures content from the host's last block-edit emission
    /// or via a paired YankCurrent action that the host fulfills.
    YankCurrent,
    /// `p` — paste the register's contents as a new block after
    /// the current block.
    PasteAfter,
    /// `P` — paste the register's contents as a new block before
    /// the current block.
    PasteBefore,
    /// `cc` — change current block: clear content and enter
    /// insert. Mirrors vim's "change line" but block-scoped.
    ChangeCurrent,
    /// `J` — join the next block's content into the current
    /// block (with a single space separator) and delete the next.
    /// Mirrors vim's `J` which joins the next line.
    JoinNext,
}

/// What the engine emits in response to a key sequence. The host
/// editor translates each variant to the appropriate effect on
/// its content + selection state.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum VimAction {
    /// Engine produced no observable action (mode-only transition,
    /// pending state, etc.). Hosts can ignore.
    NoOp,

    /// Cursor / selection movement.
    Move(Motion),

    /// Switch to insert mode at the given anchor.
    EnterInsert(InsertEntry),

    /// Switch to visual mode (character-wise).
    EnterVisual,

    /// Return to normal mode (also clears any pending op).
    EnterNormal,

    /// Delete a single character at the cursor (`x`).
    DeleteChar,

    /// Block-scope operation.
    Block(BlockOp),

    /// Cycle the current list-item block's task state through
    /// (none) → todo (`[ ]`) → in-progress (`[/]`) → done
    /// (`[x]`) → (none). Bound to Space in Normal mode (Logseq
    /// convention: focus + space toggles).
    CycleTodo,

    /// `m{a-z}` — set a named mark at the current cursor position.
    /// Host owns the `char → cursor` lookup table; engine just
    /// announces the operator.
    SetMark(char),

    /// `'{a-z}` — jump to a previously-set mark. Host looks up
    /// the cursor stored under the char and applies it.
    JumpToMark(char),
}

impl Default for VimAction {
    fn default() -> Self {
        Self::NoOp
    }
}
