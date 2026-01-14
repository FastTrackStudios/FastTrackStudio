//! Scene graph node for hierarchical rendering.
//!
//! The scene graph represents the visual structure of a music score,
//! with nodes for pages, systems, measures, and individual elements.
//! Each node can have paint commands and child nodes.

use kurbo::{Affine, Point, Rect, Shape};
use std::collections::HashMap;

use super::id::SemanticId;
use super::paint::PaintCommand;

/// A node in the scene graph.
///
/// Scene nodes form a tree structure where each node can have:
/// - A semantic ID linking to the source music element
/// - A local transform (position, rotation, scale)
/// - Paint commands for rendering
/// - Child nodes
///
/// # Example
///
/// ```ignore
/// // Create a measure group with child chord
/// let mut measure = SceneNode::group(SemanticId::measure(1));
/// measure.add_child(
///     SceneNode::leaf(
///         SemanticId::chord(1),
///         vec![PaintCommand::glyph('\u{E0A4}', Point::ZERO, 1.0, Color::BLACK)],
///     )
///     .with_position(Point::new(50.0, 100.0))
/// );
/// ```
#[derive(Debug, Clone)]
pub struct SceneNode {
    /// Semantic identifier linking to source music element.
    /// None for anonymous grouping nodes.
    pub id: Option<SemanticId>,

    /// Local transform applied before rendering this node and children.
    pub transform: Affine,

    /// Bounding box in local coordinates.
    /// Computed from paint commands and children.
    pub bounds: Rect,

    /// Paint commands for this node (rendered before children).
    pub commands: Vec<PaintCommand>,

    /// Child nodes (rendered after this node's commands).
    pub children: Vec<SceneNode>,

    /// Whether this node is visible.
    /// Invisible nodes and their children are skipped during rendering.
    pub visible: bool,

    /// User-defined metadata (for extensibility).
    /// Can store additional attributes for SVG export.
    pub metadata: HashMap<String, String>,
}

impl Default for SceneNode {
    fn default() -> Self {
        Self {
            id: None,
            transform: Affine::IDENTITY,
            bounds: Rect::ZERO,
            commands: Vec::new(),
            children: Vec::new(),
            visible: true,
            metadata: HashMap::new(),
        }
    }
}

impl SceneNode {
    /// Create a new empty scene node.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a group node (no paint commands, just children).
    #[must_use]
    pub fn group(id: SemanticId) -> Self {
        Self {
            id: Some(id),
            ..Default::default()
        }
    }

    /// Create an anonymous group node (no semantic ID).
    #[must_use]
    pub fn anonymous_group() -> Self {
        Self::default()
    }

    /// Create a leaf node with paint commands.
    #[must_use]
    pub fn leaf(id: SemanticId, commands: Vec<PaintCommand>) -> Self {
        let bounds = compute_commands_bounds(&commands);
        Self {
            id: Some(id),
            commands,
            bounds,
            ..Default::default()
        }
    }

    /// Create an anonymous leaf node (no semantic ID).
    #[must_use]
    pub fn anonymous_leaf(commands: Vec<PaintCommand>) -> Self {
        let bounds = compute_commands_bounds(&commands);
        Self {
            commands,
            bounds,
            ..Default::default()
        }
    }

    /// Set the semantic ID.
    #[must_use]
    pub fn with_id(mut self, id: SemanticId) -> Self {
        self.id = Some(id);
        self
    }

    /// Set the local transform.
    #[must_use]
    pub fn with_transform(mut self, transform: Affine) -> Self {
        self.transform = transform;
        self
    }

    /// Set position (translation only).
    #[must_use]
    pub fn with_position(mut self, position: Point) -> Self {
        self.transform = Affine::translate((position.x, position.y));
        self
    }

    /// Set visibility.
    #[must_use]
    pub fn with_visible(mut self, visible: bool) -> Self {
        self.visible = visible;
        self
    }

    /// Set bounding box explicitly.
    #[must_use]
    pub fn with_bounds(mut self, bounds: Rect) -> Self {
        self.bounds = bounds;
        self
    }

    /// Add metadata.
    #[must_use]
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Add a child node.
    pub fn add_child(&mut self, child: SceneNode) {
        self.children.push(child);
    }

    /// Add multiple children.
    pub fn add_children(&mut self, children: impl IntoIterator<Item = SceneNode>) {
        self.children.extend(children);
    }

    /// Add a paint command.
    pub fn add_command(&mut self, command: PaintCommand) {
        self.commands.push(command);
    }

    /// Add multiple paint commands.
    pub fn add_commands(&mut self, commands: impl IntoIterator<Item = PaintCommand>) {
        self.commands.extend(commands);
    }

    /// Compute the bounds of this node in local coordinates.
    /// Includes the bounds of all children transformed by their local transforms.
    #[must_use]
    pub fn compute_bounds(&self) -> Rect {
        let mut bounds = self.bounds;

        for child in &self.children {
            if !child.visible {
                continue;
            }

            let child_bounds = child.compute_bounds();
            if child_bounds.is_zero_area() {
                continue;
            }

            // Transform child bounds by child's transform
            let transformed = child.transform.transform_rect_bbox(child_bounds);
            bounds = bounds.union(transformed);
        }

        bounds
    }

    /// Compute world-space bounds by applying all ancestor transforms.
    #[must_use]
    pub fn world_bounds(&self, parent_transform: Affine) -> Rect {
        let world_transform = parent_transform * self.transform;
        let local_bounds = self.compute_bounds();
        world_transform.transform_rect_bbox(local_bounds)
    }

    /// Get all nodes matching a predicate (depth-first).
    pub fn find_all(&self, predicate: impl Fn(&SceneNode) -> bool) -> Vec<&SceneNode> {
        let mut results = Vec::new();
        self.find_all_recursive(&predicate, &mut results);
        results
    }

    fn find_all_recursive<'a>(
        &'a self,
        predicate: &impl Fn(&SceneNode) -> bool,
        results: &mut Vec<&'a SceneNode>,
    ) {
        if predicate(self) {
            results.push(self);
        }
        for child in &self.children {
            child.find_all_recursive(predicate, results);
        }
    }

    /// Find the first node matching a predicate (depth-first).
    #[must_use]
    pub fn find_first(&self, predicate: impl Fn(&SceneNode) -> bool) -> Option<&SceneNode> {
        self.find_first_recursive(&predicate)
    }

    fn find_first_recursive(
        &self,
        predicate: &impl Fn(&SceneNode) -> bool,
    ) -> Option<&SceneNode> {
        if predicate(self) {
            return Some(self);
        }
        for child in &self.children {
            if let Some(found) = child.find_first_recursive(predicate) {
                return Some(found);
            }
        }
        None
    }

    /// Find a node by semantic ID.
    #[must_use]
    pub fn find_by_id(&self, id: &SemanticId) -> Option<&SceneNode> {
        self.find_first(|node| node.id.as_ref() == Some(id))
    }

    /// Count total number of nodes in the subtree.
    #[must_use]
    pub fn node_count(&self) -> usize {
        1 + self.children.iter().map(SceneNode::node_count).sum::<usize>()
    }

    /// Count total number of paint commands in the subtree.
    #[must_use]
    pub fn command_count(&self) -> usize {
        self.commands.len()
            + self
                .children
                .iter()
                .map(SceneNode::command_count)
                .sum::<usize>()
    }

    /// Check if this node or any descendant has a semantic ID.
    #[must_use]
    pub fn has_semantic_ids(&self) -> bool {
        self.id.is_some() || self.children.iter().any(SceneNode::has_semantic_ids)
    }
}

/// Compute bounding box from paint commands.
fn compute_commands_bounds(commands: &[PaintCommand]) -> Rect {
    let mut bounds = Rect::ZERO;

    for cmd in commands {
        let cmd_bounds = match cmd {
            PaintCommand::Fill { path, .. } | PaintCommand::Stroke { path, .. } => {
                path.bounding_box()
            }
            PaintCommand::Glyph { position, size, .. } => {
                // Approximate glyph bounds (actual bounds come from font metrics)
                Rect::new(
                    position.x,
                    position.y - size,
                    position.x + size,
                    position.y + size * 0.25,
                )
            }
            PaintCommand::Text {
                position,
                font_size,
                text,
                ..
            } => {
                // Approximate text bounds (actual bounds come from font metrics)
                let width = text.len() as f64 * font_size * 0.5;
                Rect::new(
                    position.x,
                    position.y - font_size,
                    position.x + width,
                    position.y + font_size * 0.25,
                )
            }
            PaintCommand::Line { start, end, width, .. } => {
                let half = width / 2.0;
                Rect::new(
                    start.x.min(end.x) - half,
                    start.y.min(end.y) - half,
                    start.x.max(end.x) + half,
                    start.y.max(end.y) + half,
                )
            }
            PaintCommand::Rect { rect, stroke_width, .. } => {
                let half = stroke_width / 2.0;
                rect.inflate(half, half)
            }
            PaintCommand::Circle {
                center,
                radius,
                stroke_width,
                ..
            } => {
                let r = radius + stroke_width / 2.0;
                Rect::new(center.x - r, center.y - r, center.x + r, center.y + r)
            }
            PaintCommand::Ellipse {
                center,
                radius_x,
                radius_y,
                stroke_width,
                ..
            } => {
                let half = stroke_width / 2.0;
                Rect::new(
                    center.x - radius_x - half,
                    center.y - radius_y - half,
                    center.x + radius_x + half,
                    center.y + radius_y + half,
                )
            }
        };

        if bounds.is_zero_area() {
            bounds = cmd_bounds;
        } else {
            bounds = bounds.union(cmd_bounds);
        }
    }

    bounds
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scene::id::ElementType;
    use peniko::Color;

    #[test]
    fn test_new_node() {
        let node = SceneNode::new();
        assert!(node.id.is_none());
        assert_eq!(node.transform, Affine::IDENTITY);
        assert!(node.commands.is_empty());
        assert!(node.children.is_empty());
        assert!(node.visible);
    }

    #[test]
    fn test_group_node() {
        let node = SceneNode::group(SemanticId::measure(1));
        assert!(node.id.is_some());
        assert_eq!(node.id.as_ref().unwrap().element_type, ElementType::Measure);
    }

    #[test]
    fn test_leaf_node() {
        let commands = vec![PaintCommand::line(
            Point::new(0.0, 0.0),
            Point::new(100.0, 0.0),
            Color::BLACK,
            1.0,
        )];
        let node = SceneNode::leaf(SemanticId::chord(1), commands);
        assert_eq!(node.commands.len(), 1);
        assert!(!node.bounds.is_zero_area());
    }

    #[test]
    fn test_with_position() {
        let node = SceneNode::new().with_position(Point::new(50.0, 100.0));
        let expected = Affine::translate((50.0, 100.0));
        assert_eq!(node.transform, expected);
    }

    #[test]
    fn test_add_child() {
        let mut parent = SceneNode::group(SemanticId::measure(1));
        let child = SceneNode::group(SemanticId::chord(1));
        parent.add_child(child);
        assert_eq!(parent.children.len(), 1);
    }

    #[test]
    fn test_node_count() {
        let mut root = SceneNode::new();
        let mut child1 = SceneNode::new();
        child1.add_child(SceneNode::new());
        child1.add_child(SceneNode::new());
        root.add_child(child1);
        root.add_child(SceneNode::new());

        assert_eq!(root.node_count(), 5); // root + child1 + 2 grandchildren + child2
    }

    #[test]
    fn test_find_by_id() {
        let mut root = SceneNode::group(SemanticId::page(1));
        let mut measure = SceneNode::group(SemanticId::measure(1));
        measure.add_child(SceneNode::group(SemanticId::chord(42)));
        root.add_child(measure);

        let found = root.find_by_id(&SemanticId::chord(42));
        assert!(found.is_some());
        assert_eq!(
            found.unwrap().id.as_ref().unwrap().element_type,
            ElementType::Chord
        );
    }

    #[test]
    fn test_compute_bounds() {
        let mut root = SceneNode::new();

        let child1 = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
            Rect::new(0.0, 0.0, 50.0, 50.0),
            Color::BLACK,
        )]);

        let child2 = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
            Rect::new(0.0, 0.0, 30.0, 30.0),
            Color::BLACK,
        )])
        .with_position(Point::new(100.0, 100.0));

        root.add_child(child1);
        root.add_child(child2);

        let bounds = root.compute_bounds();
        // Should encompass both children
        assert!(bounds.x0 <= 0.0);
        assert!(bounds.y0 <= 0.0);
        assert!(bounds.x1 >= 130.0); // 100 + 30
        assert!(bounds.y1 >= 130.0); // 100 + 30
    }

    #[test]
    fn test_invisible_node_excluded_from_bounds() {
        let mut root = SceneNode::new();

        let visible = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
            Rect::new(0.0, 0.0, 50.0, 50.0),
            Color::BLACK,
        )]);

        let invisible = SceneNode::anonymous_leaf(vec![PaintCommand::filled_rect(
            Rect::new(1000.0, 1000.0, 2000.0, 2000.0),
            Color::BLACK,
        )])
        .with_visible(false);

        root.add_child(visible);
        root.add_child(invisible);

        let bounds = root.compute_bounds();
        // Should not include the invisible child
        assert!(bounds.x1 <= 100.0);
        assert!(bounds.y1 <= 100.0);
    }
}
