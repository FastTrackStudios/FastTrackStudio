//! Shape system for collision detection.
//!
//! This module provides the `Shape` type for representing element boundaries
//! in music notation layout, based on MuseScore's Shape class.

use std::borrow::Cow;

use kurbo::{Point, Rect};

use crate::model::ElementId;

/// Element in a shape (rectangle + optional element reference).
#[derive(Debug, Clone, PartialEq)]
pub struct ShapeElement {
    /// Bounding rectangle (in points)
    pub rect: Rect,
    /// Optional reference to the element owning this shape
    pub element: Option<ElementId>,
    /// Whether to ignore this shape for layout calculations
    pub ignore_for_layout: bool,
}

impl ShapeElement {
    /// Create a new shape element.
    #[must_use]
    pub fn new(rect: Rect) -> Self {
        Self {
            rect,
            element: None,
            ignore_for_layout: false,
        }
    }

    /// Create a shape element with an element reference.
    #[must_use]
    pub fn with_element(rect: Rect, element: ElementId) -> Self {
        Self {
            rect,
            element: Some(element),
            ignore_for_layout: false,
        }
    }
}

/// Shape for collision detection.
///
/// Represents the boundary of a music notation element using rectangles.
/// Based on MuseScore's Shape class, which uses horizontal slices for
/// efficient collision detection.
///
/// # Variants
///
/// - `Fixed`: Single bounding box (most common case - optimized)
/// - `Composite`: Multiple rectangles for complex shapes
///
/// # Example
///
/// ```ignore
/// // Simple rectangular shape
/// let shape = Shape::from_rect(Rect::new(0.0, 0.0, 10.0, 5.0));
///
/// // Complex shape with multiple rectangles
/// let elements = vec![
///     ShapeElement::new(Rect::new(0.0, 0.0, 5.0, 10.0)),
///     ShapeElement::new(Rect::new(10.0, 2.0, 15.0, 8.0)),
/// ];
/// let shape = Shape::from_elements(elements);
///
/// // Collision detection
/// let distance = shape1.min_horizontal_distance(&shape2, 1.0);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Shape {
    /// Single bounding box (fast path for most elements)
    Fixed {
        /// Bounding rectangle
        bbox: Rect,
        /// Optional element reference
        element: Option<ElementId>,
    },

    /// Multiple rectangles (for complex shapes)
    Composite {
        /// Shape elements (uses Cow for zero-copy when possible)
        elements: Cow<'static, [ShapeElement]>,
        /// Cached bounding box (computed lazily)
        bbox_cache: Option<Rect>,
    },
}

impl Shape {
    /// Create an empty shape.
    #[must_use]
    pub fn empty() -> Self {
        Self::Fixed {
            bbox: Rect::ZERO,
            element: None,
        }
    }

    /// Create a shape from a single rectangle.
    #[must_use]
    pub fn from_rect(rect: Rect) -> Self {
        Self::Fixed {
            bbox: rect,
            element: None,
        }
    }

    /// Create a shape from a rectangle with an element reference.
    #[must_use]
    pub fn from_rect_with_element(rect: Rect, element: ElementId) -> Self {
        Self::Fixed {
            bbox: rect,
            element: Some(element),
        }
    }

    /// Create a shape from multiple elements.
    #[must_use]
    pub fn from_elements(elements: Vec<ShapeElement>) -> Self {
        if elements.is_empty() {
            return Self::empty();
        }

        if elements.len() == 1 {
            // Optimize single-element case
            return Self::Fixed {
                bbox: elements[0].rect,
                element: elements[0].element,
            };
        }

        Self::Composite {
            elements: Cow::Owned(elements),
            bbox_cache: None,
        }
    }

    /// Get the bounding box of this shape.
    #[must_use]
    pub fn bbox(&self) -> Rect {
        match self {
            Self::Fixed { bbox, .. } => *bbox,
            Self::Composite {
                elements,
                bbox_cache,
            } => {
                if let Some(bbox) = bbox_cache {
                    *bbox
                } else {
                    compute_bbox_from_elements(elements)
                }
            }
        }
    }

    /// Translate shape by an offset.
    #[must_use]
    pub fn translate(&self, offset: Point) -> Self {
        match self {
            Self::Fixed { bbox, element } => Self::Fixed {
                bbox: bbox.with_origin(Point::new(bbox.x0 + offset.x, bbox.y0 + offset.y)),
                element: *element,
            },
            Self::Composite { elements, .. } => {
                let translated: Vec<_> = elements
                    .iter()
                    .map(|e| ShapeElement {
                        rect: e.rect.with_origin(Point::new(
                            e.rect.x0 + offset.x,
                            e.rect.y0 + offset.y,
                        )),
                        element: e.element,
                        ignore_for_layout: e.ignore_for_layout,
                    })
                    .collect();
                Self::from_elements(translated)
            }
        }
    }

    /// Get the right-most edge of this shape.
    #[must_use]
    pub fn right(&self) -> f64 {
        self.bbox().x1
    }

    /// Get the left-most edge of this shape.
    #[must_use]
    pub fn left(&self) -> f64 {
        self.bbox().x0
    }

    /// Get the top edge of this shape.
    #[must_use]
    pub fn top(&self) -> f64 {
        self.bbox().y0
    }

    /// Get the bottom edge of this shape.
    #[must_use]
    pub fn bottom(&self) -> f64 {
        self.bbox().y1
    }

    /// Calculate minimum horizontal distance to avoid collision with another shape.
    ///
    /// Returns the minimum X offset needed for `self` to not overlap with `other`.
    /// Positive values mean shapes don't overlap; negative values mean they do.
    ///
    /// # Arguments
    ///
    /// * `other` - The other shape to check against
    /// * `min_spacing` - Minimum spacing margin to add (in points)
    #[must_use]
    pub fn min_horizontal_distance(&self, other: &Self, min_spacing: f64) -> f64 {
        // Simple implementation for Phase 0: use bounding box distance
        // Phase 1 will implement full horizontal slice algorithm
        let self_bbox = self.bbox();
        let other_bbox = other.bbox();

        // Check if Y ranges overlap
        if self_bbox.y0 > other_bbox.y1 || self_bbox.y1 < other_bbox.y0 {
            // No vertical overlap, no collision possible
            return 0.0;
        }

        // Calculate horizontal distance
        let distance = other_bbox.x0 - self_bbox.x1;

        // Add minimum spacing margin
        if distance < 0.0 {
            // Shapes overlap
            -distance + min_spacing
        } else {
            // Shapes don't overlap, but might be too close
            f64::max(0.0, min_spacing - distance)
        }
    }

    /// Check if this shape is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Fixed { bbox, .. } => bbox.width() == 0.0 && bbox.height() == 0.0,
            Self::Composite { elements, .. } => elements.is_empty(),
        }
    }

    /// Add a rectangle to this shape.
    pub fn add_rect(&mut self, rect: Rect) {
        match self {
            Self::Fixed { bbox, element } => {
                // Convert to composite if adding another rect
                if bbox.width() > 0.0 || bbox.height() > 0.0 {
                    let elements = vec![
                        ShapeElement {
                            rect: *bbox,
                            element: *element,
                            ignore_for_layout: false,
                        },
                        ShapeElement::new(rect),
                    ];
                    *self = Self::from_elements(elements);
                } else {
                    // First rect, just replace
                    *bbox = rect;
                }
            }
            Self::Composite { elements, .. } => {
                // Add to existing composite
                let mut new_elements = elements.clone().into_owned();
                new_elements.push(ShapeElement::new(rect));
                *self = Self::from_elements(new_elements);
            }
        }
    }
}

/// Compute bounding box from a list of shape elements.
fn compute_bbox_from_elements(elements: &[ShapeElement]) -> Rect {
    if elements.is_empty() {
        return Rect::ZERO;
    }

    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    for elem in elements {
        if elem.ignore_for_layout {
            continue;
        }

        min_x = min_x.min(elem.rect.x0);
        min_y = min_y.min(elem.rect.y0);
        max_x = max_x.max(elem.rect.x1);
        max_y = max_y.max(elem.rect.y1);
    }

    if min_x.is_infinite() {
        Rect::ZERO
    } else {
        Rect::new(min_x, min_y, max_x, max_y)
    }
}

impl Default for Shape {
    fn default() -> Self {
        Self::empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shape_from_rect() {
        let rect = Rect::new(10.0, 20.0, 50.0, 40.0);
        let shape = Shape::from_rect(rect);

        assert_eq!(shape.bbox(), rect);
        assert_eq!(shape.left(), 10.0);
        assert_eq!(shape.right(), 50.0);
        assert_eq!(shape.top(), 20.0);
        assert_eq!(shape.bottom(), 40.0);
    }

    #[test]
    fn test_shape_translate() {
        let rect = Rect::new(10.0, 20.0, 50.0, 40.0);
        let shape = Shape::from_rect(rect);

        let offset = Point::new(5.0, 3.0);
        let translated = shape.translate(offset);

        let bbox = translated.bbox();
        assert_eq!(bbox.x0, 15.0);
        assert_eq!(bbox.y0, 23.0);
        assert_eq!(bbox.x1, 55.0);
        assert_eq!(bbox.y1, 43.0);
    }

    #[test]
    fn test_shape_collision_no_overlap() {
        let shape1 = Shape::from_rect(Rect::new(0.0, 0.0, 10.0, 10.0));
        let shape2 = Shape::from_rect(Rect::new(15.0, 0.0, 25.0, 10.0));

        let distance = shape1.min_horizontal_distance(&shape2, 0.5);
        // Shapes are 5 units apart, min_spacing is 0.5, so no adjustment needed
        assert_eq!(distance, 0.0);
    }

    #[test]
    fn test_shape_collision_overlap() {
        let shape1 = Shape::from_rect(Rect::new(0.0, 0.0, 10.0, 10.0));
        let shape2 = Shape::from_rect(Rect::new(8.0, 2.0, 18.0, 8.0));

        let distance = shape1.min_horizontal_distance(&shape2, 0.5);
        // Shapes overlap by 2 units, need to move 2 + 0.5 = 2.5 units
        assert_eq!(distance, 2.5);
    }

    #[test]
    fn test_shape_collision_no_vertical_overlap() {
        let shape1 = Shape::from_rect(Rect::new(0.0, 0.0, 10.0, 5.0));
        let shape2 = Shape::from_rect(Rect::new(5.0, 10.0, 15.0, 15.0));

        let distance = shape1.min_horizontal_distance(&shape2, 0.5);
        // No vertical overlap, no collision
        assert_eq!(distance, 0.0);
    }

    #[test]
    fn test_shape_composite() {
        let elements = vec![
            ShapeElement::new(Rect::new(0.0, 0.0, 5.0, 10.0)),
            ShapeElement::new(Rect::new(10.0, 2.0, 15.0, 8.0)),
        ];
        let shape = Shape::from_elements(elements);

        let bbox = shape.bbox();
        assert_eq!(bbox.x0, 0.0);
        assert_eq!(bbox.y0, 0.0);
        assert_eq!(bbox.x1, 15.0);
        assert_eq!(bbox.y1, 10.0);
    }

    #[test]
    fn test_shape_add_rect() {
        let mut shape = Shape::from_rect(Rect::new(0.0, 0.0, 10.0, 10.0));
        shape.add_rect(Rect::new(15.0, 5.0, 20.0, 15.0));

        let bbox = shape.bbox();
        assert_eq!(bbox.x0, 0.0);
        assert_eq!(bbox.y0, 0.0);
        assert_eq!(bbox.x1, 20.0);
        assert_eq!(bbox.y1, 15.0);
    }

    #[test]
    fn test_shape_empty() {
        let shape = Shape::empty();
        assert!(shape.is_empty());
        assert_eq!(shape.bbox(), Rect::ZERO);
    }
}
