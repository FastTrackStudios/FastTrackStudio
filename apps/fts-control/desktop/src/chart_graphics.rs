//! Chart graphics context using anyrender for hybrid WGPU/Dioxus rendering.
//!
//! This module provides GPU-accelerated chart rendering using anyrender's
//! VelloWindowRenderer, designed to work alongside the Dioxus WebView UI.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────┐
//! │  Main Window (WGPU Surface - background)        │
//! │  ┌───────────────────────────────────────────┐  │
//! │  │  Vello Chart Rendering via anyrender      │  │
//! │  │  (renders behind transparent areas)       │  │
//! │  └───────────────────────────────────────────┘  │
//! │                                                  │
//! │  ┌───────────────────────────────────────────┐  │
//! │  │  Dioxus WebView (transparent overlay)     │  │
//! │  │  - UI controls float on top               │  │
//! │  │  - Transparent areas show WGPU content    │  │
//! │  └───────────────────────────────────────────┘  │
//! └─────────────────────────────────────────────────┘
//! ```

use std::sync::Arc;

use anyrender::{PaintScene, WindowRenderer};
use anyrender_vello::{VelloRendererOptions, VelloWindowRenderer};
use dioxus::desktop::tao::window::Window;
use kurbo::{Affine, Circle, Point, Rect, Stroke};
use peniko::{Color, Fill};

/// Chart graphics context wrapping anyrender's VelloWindowRenderer.
///
/// This provides a simple API for rendering charts to the window surface,
/// with the Dioxus WebView overlaid on top as a transparent child window.
pub struct ChartGraphics {
    renderer: VelloWindowRenderer,
    width: u32,
    height: u32,
}

impl ChartGraphics {
    /// Create a new ChartGraphics context for the given window.
    ///
    /// This initializes the Vello GPU renderer via anyrender.
    /// The window should have transparency enabled for the hybrid overlay to work.
    pub fn new(window: Arc<Window>, width: u32, height: u32) -> Self {
        let mut renderer = VelloWindowRenderer::with_options(VelloRendererOptions {
            // Transparent base color so WGPU content shows through
            // where Dioxus UI is transparent
            base_color: Color::TRANSPARENT,
            ..Default::default()
        });

        // Resume the renderer with the window
        renderer.resume(window, width, height);

        Self {
            renderer,
            width,
            height,
        }
    }

    /// Resize the rendering surface.
    pub fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
        self.renderer.set_size(width, height);
    }

    /// Check if the renderer is active.
    pub fn is_active(&self) -> bool {
        self.renderer.is_active()
    }

    /// Render using a drawing function.
    ///
    /// The draw function receives the anyrender scene painter to draw to.
    pub fn render<F>(&mut self, draw_fn: F)
    where
        F: FnOnce(&mut <VelloWindowRenderer as WindowRenderer>::ScenePainter<'_>),
    {
        self.renderer.render(draw_fn);
    }

    /// Render a test pattern to verify the setup is working.
    ///
    /// Draws colored rectangles in the center of the screen.
    pub fn render_test(&mut self) {
        let width = self.width as f64;
        let height = self.height as f64;

        self.renderer.render(|scene| {
            // Draw a red rectangle in the center as a test pattern
            let rect = Rect::new(width * 0.25, height * 0.25, width * 0.75, height * 0.75);

            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgba8(255, 0, 0, 200),
                None,
                &rect,
            );

            // Draw a smaller blue rectangle
            let inner_rect = Rect::new(width * 0.35, height * 0.35, width * 0.65, height * 0.65);

            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgba8(0, 100, 255, 200),
                None,
                &inner_rect,
            );

            // Draw a circle to show curves work
            let circle = Circle::new(Point::new(width * 0.5, height * 0.5), 50.0);

            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgba8(0, 255, 100, 200),
                None,
                &circle,
            );
        });
    }

    /// Get the current surface dimensions.
    pub fn size(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// Render a pre-built Vello scene from the engraver pipeline.
    ///
    /// This bridges the engraver's chart output (a `vello::Scene`) to the
    /// WGPU surface. The scene is appended at the specified bounds with
    /// DPI-scaled transform.
    ///
    /// # Arguments
    /// * `chart_scene` - Pre-rendered chart scene from `ChartLayoutManager::render_to_scene`
    /// * `x`, `y` - Physical pixel position of the chart area
    /// * `width`, `height` - Physical pixel dimensions of the chart area
    pub fn render_chart_scene(
        &mut self,
        chart_scene: &vello::Scene,
        x: f64,
        y: f64,
        width: f64,
        height: f64,
    ) {
        tracing::info!(
            "render_chart_scene: surface={}x{} chart_pos=({:.0},{:.0} {:.0}x{:.0})",
            self.width,
            self.height,
            x,
            y,
            width,
            height
        );

        self.renderer.render(|painter| {
            let scene = painter.scene_mut();

            // DEBUG: Red filled border (4px wide bands) at surface coordinates
            let b = 4.0;
            // Top edge
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgb8(255, 0, 0),
                None,
                &Rect::new(x, y, x + width, y + b),
            );
            // Bottom edge
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgb8(255, 0, 0),
                None,
                &Rect::new(x, y + height - b, x + width, y + height),
            );
            // Left edge
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgb8(255, 0, 0),
                None,
                &Rect::new(x, y, x + b, y + height),
            );
            // Right edge
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgb8(255, 0, 0),
                None,
                &Rect::new(x + width - b, y, x + width, y + height),
            );

            // Append the engraver's complete scene
            scene.append(chart_scene, Some(Affine::translate((x, y))));
        });
    }

    /// Render a bounding box outline to visualize the chart area.
    ///
    /// This draws a colored rectangle outline at the specified bounds,
    /// useful for debugging the transparent area alignment.
    pub fn render_bounds(&mut self, x: f64, y: f64, width: f64, height: f64) {
        self.renderer.render(|scene| {
            // Create the bounding rect
            let rect = Rect::new(x, y, x + width, y + height);

            // Draw a semi-transparent fill to show the area
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgba8(100, 150, 255, 30), // Very light blue fill
                None,
                &rect,
            );

            // Draw a bright outline stroke
            let stroke = Stroke::new(3.0);
            scene.stroke(
                &stroke,
                Affine::IDENTITY,
                Color::from_rgba8(0, 200, 255, 255), // Cyan outline
                None,
                &rect,
            );

            // Draw corner markers for better visibility
            let corner_size = 20.0;
            let corners = [
                // Top-left
                Rect::new(x, y, x + corner_size, y + corner_size),
                // Top-right
                Rect::new(x + width - corner_size, y, x + width, y + corner_size),
                // Bottom-left
                Rect::new(x, y + height - corner_size, x + corner_size, y + height),
                // Bottom-right
                Rect::new(
                    x + width - corner_size,
                    y + height - corner_size,
                    x + width,
                    y + height,
                ),
            ];

            for corner in corners {
                scene.fill(
                    Fill::NonZero,
                    Affine::IDENTITY,
                    Color::from_rgba8(255, 100, 0, 200), // Orange corners
                    None,
                    &corner,
                );
            }

            // Draw a small circle in the center
            let center = Circle::new(Point::new(x + width / 2.0, y + height / 2.0), 15.0);
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                Color::from_rgba8(0, 255, 100, 200), // Green center marker
                None,
                &center,
            );
        });
    }
}

impl Drop for ChartGraphics {
    fn drop(&mut self) {
        self.renderer.suspend();
    }
}
