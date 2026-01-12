//! Scene graph for efficient rendering and hit testing.
//!
//! The scene graph provides a hierarchical structure of graphical objects
//! that can be efficiently rendered and queried for hit testing.

use kurbo::{Point, Rect};

/// Unique identifier for a graphical object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GraphicalObjectId(pub u64);

/// Position and shape information for a graphical object.
#[derive(Debug, Clone)]
pub struct PositionAndShape {
    /// Position relative to parent
    pub relative_position: Point,
    /// Bounding box in local coordinates
    pub bounding_box: Rect,
    /// Parent object ID
    pub parent: Option<GraphicalObjectId>,
}

impl Default for PositionAndShape {
    fn default() -> Self {
        Self {
            relative_position: Point::ZERO,
            bounding_box: Rect::ZERO,
            parent: None,
        }
    }
}

/// The scene graph containing all graphical objects.
#[derive(Debug, Default)]
pub struct SceneGraph {
    next_id: u64,
    // TODO: Add object storage and spatial indexing
}

impl SceneGraph {
    /// Create a new empty scene graph.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new object ID.
    pub fn alloc_id(&mut self) -> GraphicalObjectId {
        let id = GraphicalObjectId(self.next_id);
        self.next_id += 1;
        id
    }

    /// Perform hit testing at a point.
    #[must_use]
    pub fn hit_test(&self, _point: Point) -> Option<GraphicalObjectId> {
        // TODO: Implement hit testing
        None
    }
}
