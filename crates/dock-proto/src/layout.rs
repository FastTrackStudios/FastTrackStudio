//! Dock layout — HashMap-backed flat storage with tree conversion.
//!
//! Stores nodes in a `HashMap<NodeId, FlatNode>` for O(1) lookups and
//! mutations. Provides tree conversion for serialization and construction.

use std::collections::HashMap;

use facet::Facet;

use crate::id::{NodeId, TileId};
use crate::panel::PanelId;
use crate::tab_group::TabGroup;
use crate::tree::{DockNode, SplitDirection};

/// Internal flat node representation stored in the HashMap.
#[derive(Debug, Clone, Facet)]
#[repr(C)]
pub enum FlatNode {
    Split {
        direction: SplitDirection,
        ratio: f64,
        first: NodeId,
        second: NodeId,
    },
    Tile {
        tile_id: TileId,
        tabs: TabGroup,
    },
}

/// The main layout data structure.
///
/// Stores nodes in a HashMap for O(1) lookups and mutations.
/// The root field points to the top-level node.
#[derive(Debug, Clone, Facet)]
pub struct DockLayout {
    nodes: HashMap<NodeId, FlatNode>,
    root: Option<NodeId>,
}

impl DockLayout {
    /// Create an empty layout.
    pub fn empty() -> Self {
        Self {
            nodes: HashMap::new(),
            root: None,
        }
    }

    /// Create a layout with a single panel.
    pub fn single(panel: PanelId) -> Self {
        let mut layout = Self::empty();
        let node_id = NodeId::new();
        layout.nodes.insert(
            node_id,
            FlatNode::Tile {
                tile_id: TileId::new(),
                tabs: TabGroup::single(panel),
            },
        );
        layout.root = Some(node_id);
        layout
    }

    /// Build from a tree representation (converts tree -> flat HashMap).
    pub fn from_tree(tree: DockNode) -> Self {
        let mut layout = Self::empty();
        let root_id = layout.insert_tree_node(tree);
        layout.root = Some(root_id);
        layout
    }

    /// Convert to a tree representation (for serialization or display).
    pub fn to_tree(&self) -> Option<DockNode> {
        self.root.map(|root_id| self.build_tree_node(root_id))
    }

    /// Get the root node ID.
    pub fn root(&self) -> Option<NodeId> {
        self.root
    }

    /// Get a node by ID.
    pub fn get_node(&self, id: NodeId) -> Option<&FlatNode> {
        self.nodes.get(&id)
    }

    /// Get a mutable node by ID.
    pub fn get_node_mut(&mut self, id: NodeId) -> Option<&mut FlatNode> {
        self.nodes.get_mut(&id)
    }

    /// Update a split ratio (clamped to 5.0–95.0).
    pub fn update_split_ratio(&mut self, node_id: NodeId, new_ratio: f64) {
        if let Some(FlatNode::Split { ratio, .. }) = self.nodes.get_mut(&node_id) {
            *ratio = new_ratio.clamp(5.0, 95.0);
        }
    }

    /// Split an existing tile, creating a new split node.
    ///
    /// The original tile becomes `first`, a new tile with `new_panel` becomes `second`.
    /// The split node reuses the original tile's NodeId so parent references stay valid.
    pub fn split_tile(
        &mut self,
        tile_node_id: NodeId,
        direction: SplitDirection,
        new_panel: PanelId,
        ratio: f64,
    ) -> Option<NodeId> {
        // Verify the target is a tile
        let existing_tile = self.nodes.remove(&tile_node_id)?;
        if !matches!(existing_tile, FlatNode::Tile { .. }) {
            self.nodes.insert(tile_node_id, existing_tile);
            return None;
        }

        // Re-insert the existing tile under a new ID
        let existing_child_id = NodeId::new();
        self.nodes.insert(existing_child_id, existing_tile);

        // Create the new tile
        let new_child_id = NodeId::new();
        self.nodes.insert(
            new_child_id,
            FlatNode::Tile {
                tile_id: TileId::new(),
                tabs: TabGroup::single(new_panel),
            },
        );

        // Create the split node, reusing the original node ID
        self.nodes.insert(
            tile_node_id,
            FlatNode::Split {
                direction,
                ratio: ratio.clamp(5.0, 95.0),
                first: existing_child_id,
                second: new_child_id,
            },
        );

        Some(new_child_id)
    }

    /// Close a tile and promote its sibling to take the parent split's place.
    pub fn close_tile(&mut self, tile_node_id: NodeId) -> bool {
        // Find parent of this node
        let parent_info = self.find_parent(tile_node_id);
        let Some((parent_id, is_first_child)) = parent_info else {
            // It's the root tile — clear layout
            if self.root == Some(tile_node_id) {
                self.nodes.remove(&tile_node_id);
                self.root = None;
                return true;
            }
            return false;
        };

        // Get the sibling
        let sibling_id =
            if let Some(FlatNode::Split { first, second, .. }) = self.nodes.get(&parent_id) {
                if is_first_child {
                    *second
                } else {
                    *first
                }
            } else {
                return false;
            };

        // Remove the closing tile and the parent split
        self.nodes.remove(&tile_node_id);
        let sibling = self.nodes.remove(&sibling_id);

        if let Some(sibling_node) = sibling {
            // Replace parent with sibling (reuse parent_id so grandparent refs stay valid)
            self.nodes.insert(parent_id, sibling_node);
        }

        true
    }

    /// Get all tile IDs and their panels.
    pub fn all_tiles(&self) -> Vec<(TileId, &TabGroup)> {
        self.nodes
            .values()
            .filter_map(|node| {
                if let FlatNode::Tile { tile_id, tabs } = node {
                    Some((*tile_id, tabs))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Check if a panel exists anywhere in the layout.
    pub fn contains_panel(&self, panel: PanelId) -> bool {
        self.nodes.values().any(|node| {
            if let FlatNode::Tile { tabs, .. } = node {
                tabs.panels.contains(&panel)
            } else {
                false
            }
        })
    }

    /// Check if a panel is the *active* tab in its tile (i.e. actually visible).
    /// Returns false if the panel is in a tabbed container but on a background tab.
    pub fn panel_is_visible(&self, panel: PanelId) -> bool {
        self.nodes.values().any(|node| {
            if let FlatNode::Tile { tabs, .. } = node {
                tabs.active_panel() == Some(panel)
            } else {
                false
            }
        })
    }

    /// Get the total number of nodes in the layout.
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Find the NodeId of the node containing a specific TileId.
    pub fn find_node_for_tile(&self, tile_id: TileId) -> Option<NodeId> {
        self.nodes.iter().find_map(|(node_id, node)| {
            if let FlatNode::Tile { tile_id: tid, .. } = node {
                if *tid == tile_id {
                    return Some(*node_id);
                }
            }
            None
        })
    }

    /// Find the NodeId of the first tile containing a specific panel.
    pub fn find_node_for_panel(&self, panel: PanelId) -> Option<NodeId> {
        self.nodes.iter().find_map(|(node_id, node)| {
            if let FlatNode::Tile { tabs, .. } = node {
                if tabs.panels.contains(&panel) {
                    return Some(*node_id);
                }
            }
            None
        })
    }

    /// Move a panel from one tile to another, creating a split at the drop zone.
    ///
    /// This handles the full drag-and-drop operation:
    /// 1. Remove the panel from its source tile
    /// 2. If the source tile is now empty, close it
    /// 3. Create a new split at the target, placing the panel according to the drop zone
    ///
    /// If `drop_zone` is `Center`, the panel is added as a tab instead of splitting.
    pub fn move_panel(
        &mut self,
        panel: PanelId,
        target_node_id: NodeId,
        drop_zone: crate::drop_zone::DropZone,
    ) -> bool {
        use crate::drop_zone::DropZone;

        // Find the source tile containing this panel
        let source_node_id = match self.find_node_for_panel(panel) {
            Some(id) => id,
            None => return false,
        };

        // Don't drop on self for edge zones (splitting yourself makes no sense)
        if source_node_id == target_node_id && !matches!(drop_zone, DropZone::Center) {
            return false;
        }

        // Center drop: add as tab to the target tile.
        // Must add to target BEFORE closing the empty source, because
        // close_tile can invalidate NodeIds when it promotes siblings.
        if matches!(drop_zone, DropZone::Center) {
            if source_node_id == target_node_id {
                return true; // already there
            }

            // Add to target first (while it's still valid)
            if let Some(FlatNode::Tile { tabs, .. }) = self.nodes.get_mut(&target_node_id) {
                tabs.add_panel(panel);
                let last = tabs.len() - 1;
                tabs.set_active(last);
            } else {
                return false;
            }

            // Now remove from source
            let source_empty =
                if let Some(FlatNode::Tile { tabs, .. }) = self.nodes.get_mut(&source_node_id) {
                    tabs.remove_panel(panel);
                    tabs.is_empty()
                } else {
                    false
                };

            if source_empty {
                self.close_tile(source_node_id);
            }

            return true;
        }

        // Edge drop: remove panel from source, then split the target.
        let source_was_single =
            if let Some(FlatNode::Tile { tabs, .. }) = self.nodes.get_mut(&source_node_id) {
                let was_single = tabs.len() == 1;
                if !tabs.remove_panel(panel) {
                    return false;
                }
                was_single
            } else {
                return false;
            };

        if source_was_single {
            self.close_tile(source_node_id);
        }

        let direction = drop_zone.split_direction();
        let dragged_is_first = drop_zone.dragged_is_first();

        // The target might have been invalidated by closing the source.
        if self.nodes.contains_key(&target_node_id) {
            // Create the new tile for the dragged panel
            let new_child_id = NodeId::new();
            self.nodes.insert(
                new_child_id,
                FlatNode::Tile {
                    tile_id: TileId::new(),
                    tabs: TabGroup::single(panel),
                },
            );

            // Remove existing target and re-insert under new ID
            let existing = match self.nodes.remove(&target_node_id) {
                Some(n) => n,
                None => return false,
            };
            let existing_child_id = NodeId::new();
            self.nodes.insert(existing_child_id, existing);

            // Create split, reusing target_node_id
            let (first, second) = if dragged_is_first {
                (new_child_id, existing_child_id)
            } else {
                (existing_child_id, new_child_id)
            };

            self.nodes.insert(
                target_node_id,
                FlatNode::Split {
                    direction,
                    ratio: 50.0,
                    first,
                    second,
                },
            );

            true
        } else {
            // Target was invalidated (it was the sibling that got promoted).
            // Fallback: just add the panel to whatever is at root.
            if let Some(root_id) = self.root {
                self.split_tile(root_id, direction, panel, 50.0);
                true
            } else {
                // Layout is empty, create a single tile
                *self = DockLayout::single(panel);
                true
            }
        }
    }

    // --- Private helpers ---

    fn insert_tree_node(&mut self, tree: DockNode) -> NodeId {
        let node_id = NodeId::new();
        match tree {
            DockNode::Split {
                direction,
                ratio,
                first,
                second,
            } => {
                let first_id = self.insert_tree_node(*first);
                let second_id = self.insert_tree_node(*second);
                self.nodes.insert(
                    node_id,
                    FlatNode::Split {
                        direction,
                        ratio,
                        first: first_id,
                        second: second_id,
                    },
                );
            }
            DockNode::Tile { id, tabs } => {
                self.nodes
                    .insert(node_id, FlatNode::Tile { tile_id: id, tabs });
            }
        }
        node_id
    }

    fn build_tree_node(&self, node_id: NodeId) -> DockNode {
        match self.nodes.get(&node_id) {
            Some(FlatNode::Split {
                direction,
                ratio,
                first,
                second,
            }) => DockNode::Split {
                direction: *direction,
                ratio: *ratio,
                first: Box::new(self.build_tree_node(*first)),
                second: Box::new(self.build_tree_node(*second)),
            },
            Some(FlatNode::Tile { tile_id, tabs }) => DockNode::Tile {
                id: *tile_id,
                tabs: tabs.clone(),
            },
            None => DockNode::tile(PanelId::Performance), // fallback
        }
    }

    fn find_parent(&self, child_id: NodeId) -> Option<(NodeId, bool)> {
        for (node_id, node) in &self.nodes {
            if let FlatNode::Split { first, second, .. } = node {
                if *first == child_id {
                    return Some((*node_id, true));
                }
                if *second == child_id {
                    return Some((*node_id, false));
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_panel_layout() {
        let layout = DockLayout::single(PanelId::Performance);
        assert!(layout.root().is_some());
        assert_eq!(layout.node_count(), 1);
        assert!(layout.contains_panel(PanelId::Performance));
        assert!(!layout.contains_panel(PanelId::ChartEditor));
    }

    #[test]
    fn from_tree_roundtrip() {
        let tree = DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            30.0,
        );
        let layout = DockLayout::from_tree(tree);
        assert_eq!(layout.node_count(), 3); // 1 split + 2 tiles

        let restored = layout.to_tree().unwrap();
        match restored {
            DockNode::Split {
                direction, ratio, ..
            } => {
                assert_eq!(direction, SplitDirection::Horizontal);
                assert!((ratio - 30.0).abs() < f64::EPSILON);
            }
            _ => panic!("expected Split"),
        }
    }

    #[test]
    fn split_tile_creates_correct_structure() {
        let mut layout = DockLayout::single(PanelId::Performance);
        let root_id = layout.root().unwrap();

        let new_id = layout.split_tile(
            root_id,
            SplitDirection::Horizontal,
            PanelId::ChartEditor,
            50.0,
        );
        assert!(new_id.is_some());

        // Root should now be a split
        assert!(matches!(
            layout.get_node(root_id),
            Some(FlatNode::Split { .. })
        ));

        // Should have 3 nodes: 1 split + 2 tiles
        assert_eq!(layout.node_count(), 3);
        assert!(layout.contains_panel(PanelId::Performance));
        assert!(layout.contains_panel(PanelId::ChartEditor));
    }

    #[test]
    fn close_tile_promotes_sibling() {
        let mut layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            30.0,
        ));
        let root_id = layout.root().unwrap();

        // Find the Navigator tile's node ID
        let nav_node_id = match layout.get_node(root_id) {
            Some(FlatNode::Split { first, .. }) => *first,
            _ => panic!("expected split"),
        };

        assert!(layout.close_tile(nav_node_id));

        // Layout should now have just one tile (Performance promoted to root's place)
        assert_eq!(layout.node_count(), 1);
        assert!(layout.contains_panel(PanelId::Performance));
        assert!(!layout.contains_panel(PanelId::Navigator));
    }

    #[test]
    fn close_root_tile_clears_layout() {
        let mut layout = DockLayout::single(PanelId::Performance);
        let root_id = layout.root().unwrap();

        assert!(layout.close_tile(root_id));
        assert!(layout.root().is_none());
        assert_eq!(layout.node_count(), 0);
    }

    #[test]
    fn update_split_ratio_clamps() {
        let mut layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            50.0,
        ));
        let root_id = layout.root().unwrap();

        layout.update_split_ratio(root_id, 150.0);
        if let Some(FlatNode::Split { ratio, .. }) = layout.get_node(root_id) {
            assert!((ratio - 95.0).abs() < f64::EPSILON);
        }

        layout.update_split_ratio(root_id, -10.0);
        if let Some(FlatNode::Split { ratio, .. }) = layout.get_node(root_id) {
            assert!((ratio - 5.0).abs() < f64::EPSILON);
        }
    }

    #[test]
    fn facet_json_roundtrip() {
        let layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::vertical(
                DockNode::tile(PanelId::Performance),
                DockNode::tile(PanelId::Transport),
                80.0,
            ),
            25.0,
        ));

        let json = facet_json::to_string(&layout).unwrap();
        let restored: DockLayout = facet_json::from_str(&json).unwrap();

        assert_eq!(restored.node_count(), layout.node_count());
        assert!(restored.contains_panel(PanelId::Navigator));
        assert!(restored.contains_panel(PanelId::Performance));
        assert!(restored.contains_panel(PanelId::Transport));
    }

    #[test]
    fn all_tiles_returns_only_leaves() {
        let layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            30.0,
        ));
        let tiles = layout.all_tiles();
        assert_eq!(tiles.len(), 2);
    }

    #[test]
    fn find_node_for_panel() {
        let layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            30.0,
        ));
        assert!(layout.find_node_for_panel(PanelId::Navigator).is_some());
        assert!(layout.find_node_for_panel(PanelId::Performance).is_some());
        assert!(layout.find_node_for_panel(PanelId::ChartEditor).is_none());
    }

    #[test]
    fn move_panel_to_edge_creates_split() {
        use crate::drop_zone::DropZone;

        let mut layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            50.0,
        ));
        let target = layout.find_node_for_panel(PanelId::Performance).unwrap();

        assert!(layout.move_panel(PanelId::Navigator, target, DropZone::Right));

        // Navigator should still exist but in a new position
        assert!(layout.contains_panel(PanelId::Navigator));
        assert!(layout.contains_panel(PanelId::Performance));
    }

    #[test]
    fn move_panel_to_center_adds_tab() {
        use crate::drop_zone::DropZone;

        let mut layout = DockLayout::from_tree(DockNode::horizontal(
            DockNode::tile(PanelId::Navigator),
            DockNode::tile(PanelId::Performance),
            50.0,
        ));
        let target = layout.find_node_for_panel(PanelId::Performance).unwrap();

        assert!(layout.move_panel(PanelId::Navigator, target, DropZone::Center));

        // Both panels should be in the same tile now
        assert!(layout.contains_panel(PanelId::Navigator));
        assert!(layout.contains_panel(PanelId::Performance));
        // Only 1 tile left (the navigator's empty tile was closed)
        let tiles = layout.all_tiles();
        assert_eq!(tiles.len(), 1);
        assert_eq!(tiles[0].1.len(), 2);
    }

    #[test]
    fn default_presets_are_valid() {
        let presets = crate::defaults::default_presets();
        assert!(!presets.presets.is_empty());
        for preset in &presets.presets {
            assert!(!preset.name.is_empty());
            let layout = &preset.layout;
            assert!(layout.root().is_some());
            assert!(layout.node_count() > 0);
            // All presets should survive a tree roundtrip
            let tree = layout.to_tree().unwrap();
            let restored = DockLayout::from_tree(tree);
            assert_eq!(restored.node_count(), layout.node_count());
        }
    }
}
