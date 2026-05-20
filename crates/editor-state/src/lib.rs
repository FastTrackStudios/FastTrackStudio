//! Pure-logic core for the editor: document, transactions,
//! selections, decorations. No DOM, no Dioxus.
//!
//! Mirrors the `@codemirror/state` package in spirit — see the
//! [design notes](../../../docs/design.md) for the concept ↔
//! Rust-type mapping. Reference source at
//! `~/Development/research/codemirror/state/src/`.

pub mod change;
pub mod decoration;
pub mod doc;
pub mod selection;
pub mod state;
pub mod transaction;

pub use change::{Change, Changes};
pub use decoration::{Decoration, DecorationKind, DecorationSet, DecoratedRange};
pub use doc::Doc;
pub use selection::{Range, Selection};
pub use state::EditorState;
pub use transaction::{Transaction, TransactionSpec};
