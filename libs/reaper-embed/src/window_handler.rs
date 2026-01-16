//! Window handler implementation for BaseView
//!
//! Implements baseview's `WindowHandler` trait to manage the render loop
//! and event routing for embedded REAPER windows.

use baseview::{Event, EventStatus, Window, WindowEvent, WindowHandler};
use vello::Scene;

use crate::embed_source::EmbedSource;
use crate::gpu::{GpuError, GpuState};

/// Window handler that wraps an `EmbedSource` for rendering with Vello.
///
/// This handler:
/// - Manages WGPU device, surface, and Vello renderer
/// - Routes events to the embedded source
/// - Handles window lifecycle (resize, focus, close)
pub struct ReaperWindowHandler<S: EmbedSource> {
    source: S,
    gpu: Option<GpuState>,
    scene: Scene,
    width: u32,
    height: u32,
    gpu_init_error: Option<GpuError>,
}

impl<S: EmbedSource> ReaperWindowHandler<S> {
    /// Create a new window handler with the given source.
    ///
    /// GPU initialization is deferred until the first frame to allow
    /// proper window handle setup.
    ///
    /// # Arguments
    ///
    /// * `source` - The content source to render
    /// * `width` - Initial window width
    /// * `height` - Initial window height
    pub fn new(source: S, width: u32, height: u32) -> Self {
        Self {
            source,
            gpu: None,
            scene: Scene::new(),
            width,
            height,
            gpu_init_error: None,
        }
    }

    /// Initialize GPU state from the window.
    ///
    /// This must be called from within `on_frame` or `on_event` to ensure
    /// the window handle is valid.
    fn init_gpu(&mut self, window: &Window) {
        if self.gpu.is_some() || self.gpu_init_error.is_some() {
            return;
        }

        match GpuState::new(window, self.width, self.height) {
            Ok(gpu) => {
                log::info!(
                    "GPU initialized: {}x{}, format {:?}",
                    self.width,
                    self.height,
                    gpu.format()
                );
                self.gpu = Some(gpu);
            }
            Err(e) => {
                log::error!("Failed to initialize GPU: {}", e);
                self.gpu_init_error = Some(e);
            }
        }
    }
}

impl<S: EmbedSource> WindowHandler for ReaperWindowHandler<S> {
    fn on_frame(&mut self, window: &mut Window) {
        // Initialize GPU on first frame
        self.init_gpu(window);

        let Some(gpu) = &mut self.gpu else {
            return;
        };

        // Build scene from source
        self.scene.reset();
        self.source.render(&mut self.scene, self.width, self.height);

        // Render to surface
        if let Err(e) = gpu.render(&self.scene) {
            log::error!("Render error: {}", e);
        }
    }

    fn on_event(&mut self, window: &mut Window, event: Event) -> EventStatus {
        // Handle window events
        match &event {
            Event::Window(WindowEvent::Resized(info)) => {
                let new_width = info.physical_size().width as u32;
                let new_height = info.physical_size().height as u32;

                if new_width != self.width || new_height != self.height {
                    self.width = new_width;
                    self.height = new_height;

                    if let Some(gpu) = &mut self.gpu {
                        gpu.resize(new_width, new_height);
                    }
                }
            }
            Event::Window(WindowEvent::Focused) => {
                self.source.on_focus(true);
            }
            Event::Window(WindowEvent::Unfocused) => {
                self.source.on_focus(false);
            }
            Event::Window(WindowEvent::WillClose) => {
                self.source.on_close();
            }
            _ => {}
        }

        // Forward to source
        self.source.on_event(&event)
    }
}
