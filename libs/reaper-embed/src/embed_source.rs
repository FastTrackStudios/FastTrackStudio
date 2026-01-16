//! EmbedSource trait for renderable content
//!
//! This trait defines the interface for content that can be embedded in REAPER windows.
//! Implementations render to a Vello scene and handle window events.

use baseview::{Event, EventStatus};
use vello::Scene;

/// Trait for renderable content that can be embedded in REAPER windows.
///
/// Implementations of this trait provide the rendering and event handling logic
/// for embedded windows. The window handler calls these methods each frame.
///
/// # Example
///
/// ```ignore
/// struct MySource {
///     color: Color,
/// }
///
/// impl EmbedSource for MySource {
///     fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
///         scene.fill(
///             Fill::NonZero,
///             Affine::IDENTITY,
///             self.color,
///             None,
///             &Rect::new(0.0, 0.0, width as f64, height as f64),
///         );
///     }
/// }
/// ```
pub trait EmbedSource: Send + 'static {
    /// Build the Vello scene for this frame.
    ///
    /// Called on each frame tick by the window handler. The scene is reset
    /// before this method is called, so implementations should build the
    /// complete scene each frame.
    ///
    /// # Arguments
    ///
    /// * `scene` - The Vello scene to render into
    /// * `width` - Current window width in pixels
    /// * `height` - Current window height in pixels
    fn render(&mut self, scene: &mut Scene, width: u32, height: u32);

    /// Handle window events (mouse, keyboard, resize).
    ///
    /// Return `EventStatus::Captured` if the event was handled and should not
    /// be passed to other handlers. Return `EventStatus::Ignored` to allow
    /// the event to propagate.
    ///
    /// # Arguments
    ///
    /// * `event` - The baseview event to handle
    fn on_event(&mut self, event: &Event) -> EventStatus {
        let _ = event;
        EventStatus::Ignored
    }

    /// Called when the window gains or loses focus.
    ///
    /// # Arguments
    ///
    /// * `focused` - `true` if the window gained focus, `false` if it lost focus
    fn on_focus(&mut self, focused: bool) {
        let _ = focused;
    }

    /// Called when the window is about to close.
    ///
    /// Use this to perform cleanup before the window is destroyed.
    fn on_close(&mut self) {}
}
