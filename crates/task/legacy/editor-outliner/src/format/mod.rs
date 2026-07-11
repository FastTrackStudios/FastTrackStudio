//! Markdown parse + serialize. Mirrors Logseq's
//! `frontend.format.mldoc` / `frontend.format.block`.

pub mod inline;
mod markdown;

pub use inline::{Inline, InlineNode, Inlines, parse as parse_inline};
pub use markdown::{parse_page, serialize_page};
