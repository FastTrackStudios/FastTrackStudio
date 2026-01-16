//! Dioxus-based embed source
//!
//! Provides an `EmbedSource` that renders Dioxus components using Blitz DOM.
//!
//! This module requires the `dioxus` feature to be enabled.

use baseview::{Event, EventStatus};
use vello::Scene;

use crate::embed_source::EmbedSource;

/// An embed source that renders a Dioxus component.
///
/// Uses Blitz's DOM implementation for layout and rendering.
///
/// # Example
///
/// ```ignore
/// use dioxus::prelude::*;
/// use reaper_embed::DioxusEmbedSource;
///
/// fn app() -> Element {
///     rsx! {
///         div { class: "container",
///             h1 { "Hello from REAPER!" }
///         }
///     }
/// }
///
/// let source = DioxusEmbedSource::new(app);
/// ```
pub struct DioxusEmbedSource {
    // TODO: Integrate with dioxus-native when ready
    // This requires understanding the Blitz DOM API
    _placeholder: (),
}

impl DioxusEmbedSource {
    /// Create a new Dioxus embed source with the given app component.
    ///
    /// # Arguments
    ///
    /// * `app` - The Dioxus component function to render
    pub fn new<F>(_app: F) -> Self
    where
        F: Fn() -> dioxus::prelude::Element + 'static,
    {
        // TODO: Initialize Blitz DOM with the app
        Self { _placeholder: () }
    }
}

impl EmbedSource for DioxusEmbedSource {
    fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
        // TODO: Update layout and paint to scene
        let _ = (scene, width, height);
    }

    fn on_event(&mut self, event: &Event) -> EventStatus {
        // TODO: Convert baseview event to Dioxus event and dispatch
        let _ = event;
        EventStatus::Ignored
    }

    fn on_focus(&mut self, focused: bool) {
        // TODO: Handle focus changes
        let _ = focused;
    }

    fn on_close(&mut self) {
        // TODO: Cleanup
    }
}
