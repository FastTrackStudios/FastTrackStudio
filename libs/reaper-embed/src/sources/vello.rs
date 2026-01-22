//! Vello-based embed source
//!
//! Provides a simple way to create an `EmbedSource` from a scene-building closure.

use baseview::{Event, EventStatus};
use vello::Scene;

use crate::embed_source::EmbedSource;

/// A simple embed source that uses a closure to build the Vello scene.
///
/// # Example
///
/// ```ignore
/// use reaper_embed::VelloEmbedSource;
/// use vello::peniko::{Color, Fill};
/// use vello::kurbo::Rect;
///
/// let source = VelloEmbedSource::new(|scene, width, height| {
///     scene.fill(
///         Fill::NonZero,
///         vello::kurbo::Affine::IDENTITY,
///         Color::BLUE,
///         None,
///         &Rect::new(10.0, 10.0, width as f64 - 10.0, height as f64 - 10.0),
///     );
/// });
/// ```
pub struct VelloEmbedSource<F>
where
    F: FnMut(&mut Scene, u32, u32) + Send + 'static,
{
    builder: F,
}

impl<F> VelloEmbedSource<F>
where
    F: FnMut(&mut Scene, u32, u32) + Send + 'static,
{
    /// Create a new Vello embed source with the given scene builder.
    ///
    /// The builder closure is called on each frame with:
    /// - `scene`: The Vello scene to render into
    /// - `width`: Current window width in pixels
    /// - `height`: Current window height in pixels
    pub fn new(builder: F) -> Self {
        Self { builder }
    }
}

impl<F> EmbedSource for VelloEmbedSource<F>
where
    F: FnMut(&mut Scene, u32, u32) + Send + 'static,
{
    fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
        (self.builder)(scene, width, height);
    }

    fn on_event(&mut self, _event: &Event) -> EventStatus {
        EventStatus::Ignored
    }
}

/// A Vello embed source with event handling support.
///
/// This variant allows handling events in addition to rendering.
///
/// # Example
///
/// ```ignore
/// use reaper_embed::VelloEmbedSourceWithEvents;
///
/// let source = VelloEmbedSourceWithEvents::new(
///     |scene, width, height| {
///         // Render scene
///     },
///     |event| {
///         // Handle events
///         EventStatus::Ignored
///     },
/// );
/// ```
pub struct VelloEmbedSourceWithEvents<R, E>
where
    R: FnMut(&mut Scene, u32, u32) + Send + 'static,
    E: FnMut(&Event) -> EventStatus + Send + 'static,
{
    render_fn: R,
    event_fn: E,
}

impl<R, E> VelloEmbedSourceWithEvents<R, E>
where
    R: FnMut(&mut Scene, u32, u32) + Send + 'static,
    E: FnMut(&Event) -> EventStatus + Send + 'static,
{
    /// Create a new Vello embed source with render and event handlers.
    pub fn new(render_fn: R, event_fn: E) -> Self {
        Self {
            render_fn,
            event_fn,
        }
    }
}

impl<R, E> EmbedSource for VelloEmbedSourceWithEvents<R, E>
where
    R: FnMut(&mut Scene, u32, u32) + Send + 'static,
    E: FnMut(&Event) -> EventStatus + Send + 'static,
{
    fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
        (self.render_fn)(scene, width, height);
    }

    fn on_event(&mut self, event: &Event) -> EventStatus {
        (self.event_fn)(event)
    }
}
