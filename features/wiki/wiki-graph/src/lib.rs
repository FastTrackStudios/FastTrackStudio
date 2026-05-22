//! `wiki-graph` — pure-computation graph layer for the
//! wiki feature.
//!
//! Walks `<vault>/Wiki/`, parses frontmatter + wikilinks,
//! and builds nodes + 4-signal-weighted edges.
//!
//! ## Signals (matches llm_wiki defaults)
//!
//! | Signal          | Weight | Condition                            |
//! |-----------------|--------|--------------------------------------|
//! | Direct link     | ×3.0   | `[[wikilink]]` from A → B            |
//! | Source overlap  | ×4.0   | A & B share an entry in `sources:`   |
//! | Adamic-Adar     | ×1.5   | Σ 1/log(deg(n)) over shared neighbors |
//! | Type affinity   | ×1.0   | Same `type:` frontmatter             |
//!
//! ## Gap kinds today
//!
//! - **Orphan** — node with degree ≤ 1.
//! - **Missing page** — wikilink target with no matching
//!   page on disk (the link references a concept that
//!   doesn't exist as a `.md` file).
//!
//! Louvain clustering + sparse-cluster + bridge-node gaps
//! land in a follow-up.
//!
//! ## Output
//!
//! Built around the wire types in `wiki_proto::graph`
//! (`WikiGraph`, `GraphNode`, `GraphEdge`,
//! `RelevanceScore`, `KnowledgeGap`). Backends mounting
//! `wiki_proto::WikiService` plug this in for the
//! `build_graph` / `gaps` methods.

mod build;
mod gaps;
mod louvain;
mod parse;
mod scan;
mod scoring;

pub use build::{build_clusters, build_graph};
pub use gaps::find_gaps;
pub use scan::ScanError;
