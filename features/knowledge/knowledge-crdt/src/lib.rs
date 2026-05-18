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
pub mod block_prop_edge;
pub mod block_ref_edge;
pub mod folder;
pub mod indexed_repos;
pub mod knowledge_tag;
pub mod page;
pub mod page_prop_edge;
pub mod reindex;
pub mod vault;

pub use base::*;
pub use block::*;
pub use block_prop_edge::*;
pub use block_ref_edge::*;
pub use folder::*;
pub use indexed_repos::IndexedPageRepo;
pub use knowledge_tag::*;
pub use page::*;
pub use page_prop_edge::*;
pub use vault::*;

pub use crdt::codec::TextOp;
pub use crdt::{CrdtDoc, LoroRepo};
