//! Top-level entry point — `build_graph(vault_root, opts)`.

use std::path::Path;

use wiki_proto::graph::{GraphOpts, RelevanceWeights, WikiGraph};

use crate::scan::{ScanError, scan_wiki};
use crate::scoring::{Indices, score_graph};

/// Walk `<vault_root>/Wiki/`, parse all pages, and return
/// the typed [`WikiGraph`] under the given options.
pub fn build_graph(vault_root: &Path, opts: GraphOpts) -> Result<WikiGraph, ScanError> {
    let pages = scan_wiki(vault_root)?;
    let idx = Indices::build(&pages);
    let weights = opts.weights.unwrap_or_else(RelevanceWeights::default);
    let mut graph = score_graph(&pages, &idx, &weights, &opts.query, &opts.node_type);
    // Honor `limit` last so the cap applies to the
    // already-filtered set.
    if opts.limit > 0 && graph.nodes.len() > opts.limit as usize {
        // Sort by link_count desc, then id for stability.
        graph
            .nodes
            .sort_by(|a, b| b.link_count.cmp(&a.link_count).then(a.id.cmp(&b.id)));
        graph.nodes.truncate(opts.limit as usize);
        let kept: std::collections::HashSet<String> =
            graph.nodes.iter().map(|n| n.id.clone()).collect();
        graph
            .edges
            .retain(|e| kept.contains(&e.source) && kept.contains(&e.target));
    }
    Ok(graph)
}
