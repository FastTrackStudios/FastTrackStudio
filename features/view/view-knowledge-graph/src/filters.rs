//! Visibility filtering — port of `graph-filters.ts` + `graph-visibility.ts`.

use std::collections::HashSet;

use crate::model::{GraphEdge, GraphNode};

/// Which nodes the user has chosen to hide.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphFilterState {
    /// Hide nodes whose `kind` is in this set.
    pub hidden_kinds: HashSet<String>,
    /// Hide nodes with these explicit ids.
    pub hidden_node_ids: HashSet<String>,
    /// Hide structural pages (index/overview/log/schema/purpose).
    pub hide_structural: bool,
    /// Hide nodes with no links (`link_count == 0`).
    pub hide_isolated: bool,
    /// Hide nodes with `link_count` above this cap.
    pub max_links: Option<u32>,
}

impl Default for GraphFilterState {
    /// Matches `DEFAULT_GRAPH_FILTERS`: structural hidden, rest shown.
    fn default() -> Self {
        Self {
            hidden_kinds: HashSet::new(),
            hidden_node_ids: HashSet::new(),
            hide_structural: true,
            hide_isolated: false,
            max_links: None,
        }
    }
}

impl GraphFilterState {
    /// True when any filter would remove something.
    pub fn is_active(&self) -> bool {
        self.hide_structural
            || self.hide_isolated
            || !self.hidden_kinds.is_empty()
            || !self.hidden_node_ids.is_empty()
            || self.max_links.is_some()
    }
}

/// Result of applying filters: the surviving slice + which ids were cut.
#[derive(Debug, Clone, PartialEq)]
pub struct FilteredGraph {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
    pub hidden_node_ids: HashSet<String>,
}

/// Ids treated as structural regardless of path.
const STRUCTURAL_IDS: &[&str] = &["index", "overview", "log", "schema", "purpose"];

/// True for index/overview/log/schema/purpose pages — the connective
/// scaffolding that links to everything and clutters the map.
pub fn is_structural(node: &GraphNode) -> bool {
    let id = node.id.to_ascii_lowercase();
    if STRUCTURAL_IDS.contains(&id.as_str()) {
        return true;
    }
    if node.kind == "overview" {
        return true;
    }
    let p = node.path.replace('\\', "/").to_ascii_lowercase();
    p.ends_with("/wiki/index.md")
        || p.ends_with("/wiki/overview.md")
        || p.ends_with("/wiki/log.md")
        || p.ends_with("/purpose.md")
        || p.ends_with("/schema.md")
}

/// Apply `filters` to a graph, dropping edges whose endpoints vanish.
pub fn apply_filters(
    nodes: &[GraphNode],
    edges: &[GraphEdge],
    filters: &GraphFilterState,
) -> FilteredGraph {
    let mut hidden: HashSet<String> = HashSet::new();
    for node in nodes {
        if filters.hidden_node_ids.contains(&node.id)
            || filters.hidden_kinds.contains(&node.kind)
            || (filters.hide_structural && is_structural(node))
            || (filters.hide_isolated && node.link_count == 0)
            || filters.max_links.is_some_and(|m| node.link_count > m)
        {
            hidden.insert(node.id.clone());
        }
    }

    let visible_nodes: Vec<GraphNode> =
        nodes.iter().filter(|n| !hidden.contains(&n.id)).cloned().collect();
    let visible_ids: HashSet<&str> = visible_nodes.iter().map(|n| n.id.as_str()).collect();
    let visible_edges: Vec<GraphEdge> = edges
        .iter()
        .filter(|e| visible_ids.contains(e.source.as_str()) && visible_ids.contains(e.target.as_str()))
        .cloned()
        .collect();

    FilteredGraph {
        nodes: visible_nodes,
        edges: visible_edges,
        hidden_node_ids: hidden,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(id: &str, kind: &str, links: u32) -> GraphNode {
        GraphNode {
            id: id.to_string(),
            label: id.to_string(),
            kind: kind.to_string(),
            path: format!("/wiki/{id}.md"),
            link_count: links,
            community: 0,
        }
    }

    #[test]
    fn default_hides_structural_only() {
        let nodes = vec![node("index", "overview", 9), node("acme", "entity", 2)];
        let edges = vec![];
        let f = apply_filters(&nodes, &edges, &GraphFilterState::default());
        assert_eq!(f.nodes.len(), 1);
        assert_eq!(f.nodes[0].id, "acme");
    }

    #[test]
    fn isolated_filter() {
        let nodes = vec![node("a", "entity", 0), node("b", "entity", 3)];
        let mut filt = GraphFilterState::default();
        filt.hide_isolated = true;
        let f = apply_filters(&nodes, &[], &filt);
        assert_eq!(f.nodes.len(), 1);
        assert_eq!(f.nodes[0].id, "b");
    }
}
