//! Pure coordinate-math for the canvas viewport.
//!
//! "World" coords are the unit the data lives in
//! (`CanvasNode.x`/`y`/`w`/`h`). "Screen" coords are CSS pixels
//! inside the canvas container. The viewport maps between the two
//! via a pan + zoom — exactly the parameters fed to the inner
//! `transform: translate(...) scale(...)` div.
//!
//! Zoom-at-pivot is the key operation: when the user ctrl-wheels at
//! a screen point, we want that world point to stay under the
//! cursor. `zoom_at` computes the new pan that satisfies the
//! invariant.

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport {
    pub pan_x: f64,
    pub pan_y: f64,
    pub zoom: f64,
}

impl Default for Viewport {
    fn default() -> Self {
        Self {
            pan_x: 0.0,
            pan_y: 0.0,
            zoom: 1.0,
        }
    }
}

/// Lower bound on zoom — keeps the panel usable when zoomed way
/// out. Picked to match common whiteboard tools (Figma ≈ 0.02, Miro
/// ≈ 0.1); we stay closer to Miro.
pub const MIN_ZOOM: f64 = 0.1;
/// Upper bound — anything beyond ~8× and individual pixels are huge.
pub const MAX_ZOOM: f64 = 8.0;

/// Map a world point through the viewport into screen-space pixels.
pub fn world_to_screen(v: &Viewport, world: (f64, f64)) -> (f64, f64) {
    (world.0 * v.zoom + v.pan_x, world.1 * v.zoom + v.pan_y)
}

/// Inverse of `world_to_screen`. Useful for translating a mouse
/// event's offset coords back to a world point for hit-testing.
pub fn screen_to_world(v: &Viewport, screen: (f64, f64)) -> (f64, f64) {
    ((screen.0 - v.pan_x) / v.zoom, (screen.1 - v.pan_y) / v.zoom)
}

/// Zoom by `factor` (e.g. `1.1` to zoom in 10%) about a screen-space
/// pivot point. Pan is adjusted so the world point currently under
/// the pivot stays under the pivot after the zoom.
pub fn zoom_at(v: Viewport, screen_pivot: (f64, f64), factor: f64) -> Viewport {
    let new_zoom = (v.zoom * factor).clamp(MIN_ZOOM, MAX_ZOOM);
    // Invariant we want preserved:
    //   world_to_screen(after, world) == screen_pivot
    // where world = screen_to_world(before, screen_pivot).
    let world = screen_to_world(&v, screen_pivot);
    let pan_x = screen_pivot.0 - world.0 * new_zoom;
    let pan_y = screen_pivot.1 - world.1 * new_zoom;
    Viewport {
        pan_x,
        pan_y,
        zoom: new_zoom,
    }
}

/// Pick a viewport that fits `content_bounds` (world rect: x, y, w,
/// h) into `viewport_size` (screen w, h) with `padding` screen-pixels
/// of margin on all sides. Centers the content.
pub fn fit_to_bounds(
    _v: Viewport,
    viewport_size: (f64, f64),
    content_bounds: (f64, f64, f64, f64),
    padding: f64,
) -> Viewport {
    let (vw, vh) = viewport_size;
    let (cx, cy, cw, ch) = content_bounds;
    if cw <= 0.0 || ch <= 0.0 || vw <= 0.0 || vh <= 0.0 {
        return Viewport::default();
    }
    let avail_w = (vw - 2.0 * padding).max(1.0);
    let avail_h = (vh - 2.0 * padding).max(1.0);
    let zoom = (avail_w / cw).min(avail_h / ch).clamp(MIN_ZOOM, MAX_ZOOM);
    // Center the content rect in the screen.
    let world_cx = cx + cw / 2.0;
    let world_cy = cy + ch / 2.0;
    let pan_x = vw / 2.0 - world_cx * zoom;
    let pan_y = vh / 2.0 - world_cy * zoom;
    Viewport { pan_x, pan_y, zoom }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn approx(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-9
    }

    #[test]
    fn world_screen_round_trip_identity_viewport() {
        let v = Viewport::default();
        let p = (123.0, -45.5);
        let s = world_to_screen(&v, p);
        let w = screen_to_world(&v, s);
        assert!(approx(w.0, p.0));
        assert!(approx(w.1, p.1));
    }

    #[test]
    fn world_screen_round_trip_with_pan_zoom() {
        let v = Viewport {
            pan_x: 20.0,
            pan_y: -10.0,
            zoom: 1.75,
        };
        for p in &[(0.0, 0.0), (100.0, 200.0), (-50.0, 50.0)] {
            let s = world_to_screen(&v, *p);
            let w = screen_to_world(&v, s);
            assert!(approx(w.0, p.0), "x: {} != {}", w.0, p.0);
            assert!(approx(w.1, p.1), "y: {} != {}", w.1, p.1);
        }
    }

    #[test]
    fn zoom_at_preserves_world_under_pivot() {
        let v = Viewport {
            pan_x: 30.0,
            pan_y: 60.0,
            zoom: 1.0,
        };
        let pivot = (400.0, 300.0);
        let world_before = screen_to_world(&v, pivot);
        let v2 = zoom_at(v, pivot, 1.5);
        let world_after = screen_to_world(&v2, pivot);
        assert!(approx(world_before.0, world_after.0));
        assert!(approx(world_before.1, world_after.1));
        assert!(approx(v2.zoom, 1.5));
    }

    #[test]
    fn zoom_at_clamped_min_max() {
        let v = Viewport::default();
        // huge zoom factor — should clamp at MAX_ZOOM
        let v_in = zoom_at(v, (0.0, 0.0), 1000.0);
        assert!(approx(v_in.zoom, MAX_ZOOM));
        let v_out = zoom_at(v, (0.0, 0.0), 0.0001);
        assert!(approx(v_out.zoom, MIN_ZOOM));
    }

    #[test]
    fn fit_to_bounds_centers_content() {
        let bounds = (100.0, 100.0, 200.0, 100.0);
        let v = fit_to_bounds(Viewport::default(), (800.0, 600.0), bounds, 20.0);
        // The world center of the bounds should map to the screen center.
        let world_center = (bounds.0 + bounds.2 / 2.0, bounds.1 + bounds.3 / 2.0);
        let s = world_to_screen(&v, world_center);
        assert!(approx(s.0, 400.0), "x: {}", s.0);
        assert!(approx(s.1, 300.0), "y: {}", s.1);
    }

    #[test]
    fn fit_to_bounds_zoom_respects_padding() {
        // 1000x1000 viewport, 100x100 content, 50px padding → max
        // avail = 900px; 900/100 = 9 → clamped to MAX_ZOOM (8).
        let v = fit_to_bounds(
            Viewport::default(),
            (1000.0, 1000.0),
            (0.0, 0.0, 100.0, 100.0),
            50.0,
        );
        assert!(approx(v.zoom, MAX_ZOOM));
    }

    #[test]
    fn fit_to_bounds_degenerate_returns_default() {
        let v = fit_to_bounds(
            Viewport::default(),
            (800.0, 600.0),
            (0.0, 0.0, 0.0, 0.0),
            20.0,
        );
        assert_eq!(v, Viewport::default());
    }
}
