//! Markdown parse + serialize. Mirrors Logseq's
//! `frontend.format.mldoc` / `frontend.format.block`.

mod markdown;

pub use markdown::{parse_page, serialize_page};
