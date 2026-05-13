//! v1.5a live-preview outliner editor.
//!
//! Span-level live preview: per-span Raw/Rendered swap based on caret
//! position; block-level constructs (code, callout, table, html) flip
//! block-wide on focus. Inspired by Obsidian Live Preview and Logseq's
//! WYSIWYG outliner.
//!
//! Layout:
//! - `inline_parser`  — `InlineSpan` + tokenizer (pure)
//! - `decoration`     — Rendered/Raw decision (pure)
//! - `caret`          — DOM caret tracking helpers (wasm-only)
//! - `input`          — pure beforeinput/keydown handlers
//! - `inline_spans`   — per-kind rendered inline components
//! - `block`          — `BlockEditor` contenteditable wrapper
//! - `pickers`        — `[[`, `((`, `/`, `#` autocomplete popovers
//! - `outliner`       — `OutlinerEditor` composing multiple blocks

pub mod block;
pub mod caret;
pub mod decoration;
pub mod inline_parser;
pub mod inline_spans;
pub mod input;
pub mod outliner;
pub mod pickers;

pub use block::{BlockEditor, RefClick, RefHover};
pub use caret::CaretPos;
pub use decoration::{SpanDecoration, decorate};
pub use inline_parser::{InlineSpan, SpanKind, parse_inline};
pub use input::{
    BeforeInputEvent, InputCommand, KeyEvent, StructuralOp, handle_beforeinput, handle_keydown,
};
pub use outliner::{BlockUpdate, OutlinerEditor};
pub use pickers::{
    BlockBrief, BlockRefPicker, PageBrief, SlashCommand, SlashCommandMenu, WikilinkPicker,
    default_slash_commands,
};
