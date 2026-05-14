//! Loro-backed source-of-truth for the knowledge feature.
//!
//! Six entities, each a small per-file module. Five (Vault, Folder,
//! Page, KnowledgeTag, Base) are emitted by the `entity_crdt!`
//! macro; [`block`] is hand-written because `Block.content` is a
//! `LoroText` child container the macro doesn't currently support.
//!
//! Roots are `knowledge_*` prefixed to keep them out of the way of
//! any other feature's root names.

pub mod base;
pub mod block;
pub mod folder;
pub mod knowledge_tag;
pub mod page;
pub mod vault;

pub use base::*;
pub use block::*;
pub use folder::*;
pub use knowledge_tag::*;
pub use page::*;
pub use vault::*;

pub use crdt::codec::TextOp;
pub use crdt::{CrdtDoc, LoroRepo};
