//! Selection model for the canvas.
//!
//! A flat set of block ids. The `bounds` helper computes the union
//! AABB of all selected nodes so the toolbar / resize-handle layer
//! can draw a single bounding box without recomputing per-frame.

use crate::canvas::hit::PlacedNode;
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Selection {
    pub block_ids: HashSet<Uuid>,
}

impl Selection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.block_ids.is_empty()
    }

    pub fn len(&self) -> usize {
        self.block_ids.len()
    }

    pub fn contains(&self, id: Uuid) -> bool {
        self.block_ids.contains(&id)
    }

    pub fn add(&mut self, id: Uuid) {
        self.block_ids.insert(id);
    }

    pub fn toggle(&mut self, id: Uuid) {
        if !self.block_ids.insert(id) {
            self.block_ids.remove(&id);
        }
    }

    pub fn clear(&mut self) {
        self.block_ids.clear();
    }

    pub fn replace_with(&mut self, ids: impl IntoIterator<Item = Uuid>) {
        self.block_ids.clear();
        self.block_ids.extend(ids);
    }

    /// Union AABB (world coords) of all selected nodes. `None` if
    /// the selection is empty or no selected ids are present in
    /// `nodes`.
    pub fn bounds(&self, nodes: &[PlacedNode]) -> Option<(f64, f64, f64, f64)> {
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut max_y = f64::NEG_INFINITY;
        let mut any = false;
        for n in nodes {
            if !self.block_ids.contains(&n.block_id) {
                continue;
            }
            any = true;
            min_x = min_x.min(n.node.x);
            min_y = min_y.min(n.node.y);
            max_x = max_x.max(n.node.x + n.node.w);
            max_y = max_y.max(n.node.y + n.node.h);
        }
        if !any {
            return None;
        }
        Some((min_x, min_y, max_x - min_x, max_y - min_y))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use knowledge_proto::canvas::{CanvasNode, CanvasShape};

    fn node(id: u128, x: f64, y: f64, w: f64, h: f64) -> PlacedNode {
        PlacedNode {
            block_id: Uuid::from_u128(id),
            node: CanvasNode {
                x,
                y,
                w,
                h,
                z: 0,
                rotation: 0.0,
                shape: CanvasShape::Rect,
                color: None,
                locked: false,
                extras_json: None,
            },
        }
    }

    #[test]
    fn add_toggle_clear() {
        let mut s = Selection::new();
        let a = Uuid::from_u128(1);
        s.add(a);
        assert!(s.contains(a));
        s.toggle(a); // removes
        assert!(!s.contains(a));
        s.toggle(a); // re-adds
        assert!(s.contains(a));
        s.clear();
        assert!(s.is_empty());
    }

    #[test]
    fn replace_with_overwrites() {
        let mut s = Selection::new();
        s.add(Uuid::from_u128(1));
        s.add(Uuid::from_u128(2));
        s.replace_with([Uuid::from_u128(9)]);
        assert_eq!(s.len(), 1);
        assert!(s.contains(Uuid::from_u128(9)));
    }

    #[test]
    fn bounds_union_of_selected() {
        let nodes = vec![
            node(1, 0.0, 0.0, 100.0, 50.0),
            node(2, 200.0, 100.0, 50.0, 50.0),
            node(3, -50.0, -10.0, 20.0, 20.0),
        ];
        let mut s = Selection::new();
        s.add(Uuid::from_u128(1));
        s.add(Uuid::from_u128(2));
        let b = s.bounds(&nodes).unwrap();
        // min_x = 0, min_y = 0, max_x = 250, max_y = 150
        assert_eq!(b, (0.0, 0.0, 250.0, 150.0));
    }

    #[test]
    fn bounds_none_when_empty() {
        let s = Selection::new();
        assert!(s.bounds(&[]).is_none());
    }
}
