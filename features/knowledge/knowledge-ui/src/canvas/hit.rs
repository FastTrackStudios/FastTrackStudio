//! Hit-testing + connector routing.
//!
//! Hit-test respects per-node rotation by transforming the click
//! point into node-local coords. Connectors are routed as 3- to
//! 5-point orthogonal polylines (HV / VH / HVH / VHV) so the SVG
//! renderer can paint them as straight L-shapes with one bend, or a
//! Manhattan path with two bends when the anchors face the same
//! direction.

use crate::canvas::coords;
use knowledge_proto::canvas::{Anchor, CanvasNode, CanvasShape};
use uuid::Uuid;

/// A `CanvasNode` paired with its owning Block id — the renderer's
/// flattened drawable. We carry the id here so picking can return a
/// stable handle without re-borrowing the source `Block`.
#[derive(Clone, Debug, PartialEq)]
pub struct PlacedNode {
    pub block_id: Uuid,
    pub node: CanvasNode,
}

/// Topmost node at a world point. Iterates back-to-front in z-order
/// (highest `z` wins; ties broken by later-appearing). Locked nodes
/// remain hit-testable — the selection model is what excludes them.
pub fn pick(nodes: &[PlacedNode], world: (f64, f64)) -> Option<&PlacedNode> {
    // We expect callers to pre-sort by z ascending; iterate in
    // reverse so the visually-on-top node wins.
    nodes.iter().rev().find(|n| contains(&n.node, world))
}

/// Nodes whose AABB (axis-aligned bounding box, ignoring rotation —
/// good enough for marquee select) intersects `rect`.
pub fn pick_rect<'a>(nodes: &'a [PlacedNode], rect: (f64, f64, f64, f64)) -> Vec<&'a PlacedNode> {
    nodes
        .iter()
        .filter(|n| aabb_intersects(&n.node, rect))
        .collect()
}

/// Does a node's rotated rect contain a world point? For axis-
/// aligned nodes this is a plain rect-contains; rotated nodes
/// transform the point into the node's local frame first.
pub fn contains(node: &CanvasNode, world: (f64, f64)) -> bool {
    if node.w <= 0.0 || node.h <= 0.0 {
        // Zero-size shapes (Connectors carry w=h=0). They have their
        // own hit logic in `connector_hit`.
        return false;
    }
    let local = to_local(node, world);
    let inside = local.0 >= 0.0 && local.0 <= node.w && local.1 >= 0.0 && local.1 <= node.h;
    if !inside {
        return false;
    }
    // Refine for non-rectangular shapes.
    match node.shape {
        CanvasShape::Ellipse => {
            let cx = node.w / 2.0;
            let cy = node.h / 2.0;
            let dx = (local.0 - cx) / cx;
            let dy = (local.1 - cy) / cy;
            dx * dx + dy * dy <= 1.0
        }
        CanvasShape::Diamond => {
            let cx = node.w / 2.0;
            let cy = node.h / 2.0;
            ((local.0 - cx).abs() / cx) + ((local.1 - cy).abs() / cy) <= 1.0
        }
        _ => true,
    }
}

/// Transform `world` into the node's local frame (origin = node
/// top-left, axes counter-rotated by `node.rotation`).
fn to_local(node: &CanvasNode, world: (f64, f64)) -> (f64, f64) {
    let cx = node.x + node.w / 2.0;
    let cy = node.y + node.h / 2.0;
    let dx = world.0 - cx;
    let dy = world.1 - cy;
    if node.rotation == 0.0 {
        return (dx + node.w / 2.0, dy + node.h / 2.0);
    }
    let rad = -(node.rotation as f64).to_radians();
    let cos = rad.cos();
    let sin = rad.sin();
    let lx = dx * cos - dy * sin + node.w / 2.0;
    let ly = dx * sin + dy * cos + node.h / 2.0;
    (lx, ly)
}

fn aabb_intersects(node: &CanvasNode, rect: (f64, f64, f64, f64)) -> bool {
    let (rx, ry, rw, rh) = rect;
    !(node.x + node.w < rx || node.x > rx + rw || node.y + node.h < ry || node.y > ry + rh)
}

/// Distance from a point to a connector polyline, in world units.
/// Used for hit-testing connector lines with a tolerance band.
pub fn distance_to_polyline(points: &[(f64, f64)], p: (f64, f64)) -> f64 {
    if points.len() < 2 {
        return f64::INFINITY;
    }
    let mut best = f64::INFINITY;
    for w in points.windows(2) {
        let d = point_to_segment(w[0], w[1], p);
        if d < best {
            best = d;
        }
    }
    best
}

/// Hit-test a connector polyline with a 6-screen-pixel tolerance
/// band — the band is converted to world units by the caller (divide
/// by viewport zoom).
pub fn connector_hit(points: &[(f64, f64)], p: (f64, f64), tolerance_world: f64) -> bool {
    distance_to_polyline(points, p) <= tolerance_world
}

fn point_to_segment(a: (f64, f64), b: (f64, f64), p: (f64, f64)) -> f64 {
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    let len2 = dx * dx + dy * dy;
    if len2 == 0.0 {
        let ex = p.0 - a.0;
        let ey = p.1 - a.1;
        return (ex * ex + ey * ey).sqrt();
    }
    let t = (((p.0 - a.0) * dx + (p.1 - a.1) * dy) / len2).clamp(0.0, 1.0);
    let qx = a.0 + t * dx;
    let qy = a.1 + t * dy;
    ((p.0 - qx).powi(2) + (p.1 - qy).powi(2)).sqrt()
}

// ── Anchor + connector routing ──────────────────────────────────────

/// Where on a node's bounding rect does this anchor live (world)?
pub fn anchor_point(node: &CanvasNode, anchor: Anchor) -> (f64, f64) {
    let l = node.x;
    let r = node.x + node.w;
    let t = node.y;
    let b = node.y + node.h;
    let cx = l + node.w / 2.0;
    let cy = t + node.h / 2.0;
    match anchor {
        Anchor::Top => (cx, t),
        Anchor::Right => (r, cy),
        Anchor::Bottom => (cx, b),
        Anchor::Left => (l, cy),
        Anchor::Center | Anchor::Auto => (cx, cy),
    }
}

/// Resolve `Anchor::Auto` to whichever side faces the target most
/// directly. Other anchors pass through unchanged.
pub fn resolve_anchor(from: &CanvasNode, to: &CanvasNode, anchor: Anchor) -> Anchor {
    if anchor != Anchor::Auto {
        return anchor;
    }
    let from_c = (from.x + from.w / 2.0, from.y + from.h / 2.0);
    let to_c = (to.x + to.w / 2.0, to.y + to.h / 2.0);
    let dx = to_c.0 - from_c.0;
    let dy = to_c.1 - from_c.1;
    if dx.abs() >= dy.abs() {
        if dx >= 0.0 {
            Anchor::Right
        } else {
            Anchor::Left
        }
    } else if dy >= 0.0 {
        Anchor::Bottom
    } else {
        Anchor::Top
    }
}

/// Orthogonal connector path between two node anchors.
///
/// Strategy: project a short "stub" outward from each anchor (so the
/// line never visually enters its own node), then connect the stub
/// endpoints with a 1- or 2-bend Manhattan polyline. Result: 4-6
/// points for the typical case, exactly 4 (3 segments) when one
/// turn is enough.
pub fn connector_path(
    from_node: &PlacedNode,
    from_anchor: Anchor,
    to_node: &PlacedNode,
    to_anchor: Anchor,
) -> Vec<(f64, f64)> {
    const STUB: f64 = 24.0;

    let from_a = resolve_anchor(&from_node.node, &to_node.node, from_anchor);
    let to_a = resolve_anchor(&to_node.node, &from_node.node, to_anchor);

    let p_from = anchor_point(&from_node.node, from_a);
    let p_to = anchor_point(&to_node.node, to_a);

    let s_from = stub(p_from, from_a, STUB);
    let s_to = stub(p_to, to_a, STUB);

    let mut out = Vec::with_capacity(6);
    out.push(p_from);
    out.push(s_from);

    // Decide bend orientation based on which anchor "faces" what
    // axis. Horizontal-facing anchors → bend uses dy first.
    let from_h = matches!(from_a, Anchor::Left | Anchor::Right);
    let to_h = matches!(to_a, Anchor::Left | Anchor::Right);

    match (from_h, to_h) {
        (true, true) => {
            let mx = (s_from.0 + s_to.0) / 2.0;
            out.push((mx, s_from.1));
            out.push((mx, s_to.1));
        }
        (false, false) => {
            let my = (s_from.1 + s_to.1) / 2.0;
            out.push((s_from.0, my));
            out.push((s_to.0, my));
        }
        (true, false) => {
            out.push((s_to.0, s_from.1));
        }
        (false, true) => {
            out.push((s_from.0, s_to.1));
        }
    }

    out.push(s_to);
    out.push(p_to);
    out
}

fn stub(p: (f64, f64), anchor: Anchor, len: f64) -> (f64, f64) {
    match anchor {
        Anchor::Top => (p.0, p.1 - len),
        Anchor::Right => (p.0 + len, p.1),
        Anchor::Bottom => (p.0, p.1 + len),
        Anchor::Left => (p.0 - len, p.1),
        Anchor::Center | Anchor::Auto => p,
    }
}

/// Tolerance band in world units for connector hit-testing, given
/// the current viewport zoom. 6 screen px is the design budget.
pub fn connector_tolerance_world(zoom: f64) -> f64 {
    6.0 / zoom.max(coords::MIN_ZOOM)
}

#[cfg(test)]
mod tests {
    use super::*;
    use knowledge_proto::canvas::CanvasShape;

    fn rect_node(id: Uuid, x: f64, y: f64, w: f64, h: f64, z: i32) -> PlacedNode {
        PlacedNode {
            block_id: id,
            node: CanvasNode {
                x,
                y,
                w,
                h,
                z,
                rotation: 0.0,
                shape: CanvasShape::Rect,
                color: None,
                locked: false,
                extras_json: None,
            },
        }
    }

    #[test]
    fn pick_returns_topmost_by_z_order() {
        let a = rect_node(Uuid::nil(), 0.0, 0.0, 100.0, 100.0, 0);
        let b = rect_node(Uuid::from_u128(2), 0.0, 0.0, 100.0, 100.0, 5);
        let nodes = vec![a, b.clone()];
        let hit = pick(&nodes, (50.0, 50.0)).unwrap();
        // Both contain the point — z=5 wins.
        assert_eq!(hit.block_id, b.block_id);
    }

    #[test]
    fn pick_misses_outside_bounds() {
        let a = rect_node(Uuid::nil(), 0.0, 0.0, 100.0, 100.0, 0);
        assert!(pick(&[a], (200.0, 200.0)).is_none());
    }

    #[test]
    fn pick_rect_intersection_aabb() {
        let a = rect_node(Uuid::from_u128(1), 0.0, 0.0, 50.0, 50.0, 0);
        let b = rect_node(Uuid::from_u128(2), 100.0, 100.0, 50.0, 50.0, 0);
        let c = rect_node(Uuid::from_u128(3), 200.0, 200.0, 50.0, 50.0, 0);
        let nodes = vec![a, b, c];
        let got = pick_rect(&nodes, (40.0, 40.0, 80.0, 80.0));
        // a's AABB (0..50) intersects (40..120) → hit. b's AABB
        // (100..150) intersects (40..120) → hit. c is far.
        assert_eq!(got.len(), 2);
    }

    #[test]
    fn ellipse_corner_outside_ellipse() {
        let mut n = rect_node(Uuid::nil(), 0.0, 0.0, 100.0, 100.0, 0);
        n.node.shape = CanvasShape::Ellipse;
        // Corner (1,1) is inside the bounding box but outside the inscribed ellipse.
        assert!(!contains(&n.node, (1.0, 1.0)));
        // Center is inside.
        assert!(contains(&n.node, (50.0, 50.0)));
    }

    #[test]
    fn rotation_transforms_into_local() {
        let mut n = rect_node(Uuid::nil(), 0.0, 0.0, 100.0, 20.0, 0);
        n.node.rotation = 90.0;
        // After a 90° CW rotation around center (50,10), the point
        // that used to be at (50, 50) in world is outside the rotated
        // rect. The point that used to be at the rotated long edge
        // should be inside.
        // The rotated rect's bounding box in world is roughly 20x100
        // centered at (50,10) → spans x:40..60, y:-40..60.
        assert!(contains(&n.node, (50.0, 10.0))); // center
        assert!(contains(&n.node, (50.0, 50.0))); // along rotated long axis
        assert!(!contains(&n.node, (90.0, 50.0))); // outside
    }

    #[test]
    fn connector_path_right_to_left_is_orthogonal() {
        let a = rect_node(Uuid::from_u128(1), 0.0, 0.0, 100.0, 100.0, 0);
        let b = rect_node(Uuid::from_u128(2), 300.0, 0.0, 100.0, 100.0, 0);
        let path = connector_path(&a, Anchor::Right, &b, Anchor::Left);
        // Right→Left → 6 points (anchor, stub, mid, mid, stub, anchor).
        assert_eq!(path.len(), 6);
        // First point is exactly a's right anchor.
        assert_eq!(path[0], (100.0, 50.0));
        // Last point is b's left anchor.
        assert_eq!(path[5], (300.0, 50.0));
        // Every segment is axis-aligned.
        for w in path.windows(2) {
            assert!(
                w[0].0 == w[1].0 || w[0].1 == w[1].1,
                "non-orthogonal segment"
            );
        }
    }

    #[test]
    fn connector_path_right_to_top_is_one_bend() {
        let a = rect_node(Uuid::from_u128(1), 0.0, 0.0, 100.0, 100.0, 0);
        let b = rect_node(Uuid::from_u128(2), 300.0, 200.0, 100.0, 100.0, 0);
        let path = connector_path(&a, Anchor::Right, &b, Anchor::Top);
        // Horizontal-then-vertical → 5 points.
        assert_eq!(path.len(), 5);
        for w in path.windows(2) {
            assert!(w[0].0 == w[1].0 || w[0].1 == w[1].1);
        }
    }

    #[test]
    fn connector_auto_anchor_picks_axis_of_greatest_distance() {
        let a = rect_node(Uuid::from_u128(1), 0.0, 0.0, 100.0, 100.0, 0);
        let b = rect_node(Uuid::from_u128(2), 500.0, 30.0, 100.0, 100.0, 0);
        // a center=(50,50), b center=(550,80) → dx=500, dy=30 → Right.
        let path = connector_path(&a, Anchor::Auto, &b, Anchor::Auto);
        assert_eq!(path[0], (100.0, 50.0));
        assert_eq!(*path.last().unwrap(), (500.0, 80.0));
    }

    #[test]
    fn polyline_distance_is_zero_on_segment() {
        let pts = vec![(0.0, 0.0), (100.0, 0.0), (100.0, 100.0)];
        assert!(distance_to_polyline(&pts, (50.0, 0.0)).abs() < 1e-9);
        assert!(distance_to_polyline(&pts, (100.0, 50.0)).abs() < 1e-9);
        // 10 units away
        assert!((distance_to_polyline(&pts, (50.0, 10.0)) - 10.0).abs() < 1e-9);
    }
}
