//! Chart Window - Docked window displaying parsed chart from Keyflow
//!
//! Displays a parsed chart in a docked REAPER window using engraver's
//! ChartLayoutEngine and VelloSceneRenderer for proper music notation rendering.

use std::cell::RefCell;
use std::ptr::NonNull;
use std::sync::{Arc, Mutex};

use reaper_embed::{Event, EventStatus, MouseButton, MouseEvent, ScrollDelta};
use engraver::fonts::SMuFLFont;
use engraver::layout::chart::{ChartLayoutEngine, ChartLayoutResult, LayoutMode};
use engraver::renderer::scene_renderer::{SceneRenderBuilder, VelloSceneRenderer};
use engraver::style::MStyle;
use keyflow::Chart;
use raw_window_handle::{
    AppKitDisplayHandle, AppKitWindowHandle, DisplayHandle, HandleError, HasDisplayHandle,
    HasWindowHandle, RawDisplayHandle, RawWindowHandle, WindowHandle,
};
use reaper_high::Reaper;
use reaper_low::raw::HWND;
use tracing::{info, warn};
use vello::kurbo::{Affine, Point, Rect};
use vello::peniko::{Color, Fill};
use vello::Scene;

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use reaper_embed::{DockedWindow, ReaperWindow, VelloEmbedSourceWithEvents, WindowConfig};

// Embedded fonts from charts package
static BRAVURA_FONT: &[u8] = include_bytes!(
    "../../../../packages/charts/resources/fonts/musescore/fonts/bravura/Bravura.otf"
);
static BRAVURA_METADATA: &[u8] = include_bytes!(
    "../../../../packages/charts/resources/fonts/musescore/fonts/bravura/bravura_metadata.json"
);
static TEXT_FONT: &[u8] = include_bytes!(
    "../../../../packages/charts/resources/fonts/musescore/fonts/FreeSans.ttf"
);
static MUSEJAZZ_TEXT_FONT: &[u8] = include_bytes!(
    "../../../../packages/charts/resources/fonts/musescore/fonts/musejazz/MuseJazzText.otf"
);

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

/// Default chart text for testing (Thriller demo)
const DEFAULT_CHART_TEXT: &str = r#"Thriller - Dirty Loops, Cory Wong
Transcribed By Cody Wright
120bpm 4/4 #C
V2

Count 2
Hits 2
Intro 4 "Groove"
VS 16
CH 8
INST 4
VS
CH
BR 10
VS
CH +2
CH
Interlude 8
Interlude "Horn"
Interlude "Winds"
Interlude "Trumpets"
Outro 8
Outro
Hits 4
"#;

/// Wrapper for REAPER's main window that implements HasWindowHandle
struct ReaperMainWindow {
    hwnd: HWND,
}

impl ReaperMainWindow {
    fn new() -> Option<Self> {
        let reaper = Reaper::get();
        let hwnd = reaper.main_window().as_ptr();
        if hwnd.is_null() {
            None
        } else {
            Some(Self { hwnd })
        }
    }
}

// Implement HasWindowHandle for ReaperMainWindow (macOS)
#[cfg(target_os = "macos")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        let ns_view = self.hwnd as *mut std::ffi::c_void;
        let handle = AppKitWindowHandle::new(
            NonNull::new(ns_view).ok_or(HandleError::Unavailable)?,
        );
        let raw = RawWindowHandle::AppKit(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "macos")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        let raw = RawDisplayHandle::AppKit(AppKitDisplayHandle::new());
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::Win32WindowHandle;
        let handle = Win32WindowHandle::new(
            std::num::NonZeroIsize::new(self.hwnd as isize)
                .ok_or(HandleError::Unavailable)?,
        );
        let raw = RawWindowHandle::Win32(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::WindowsDisplayHandle;
        let raw = RawDisplayHandle::Windows(WindowsDisplayHandle::new());
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::XlibWindowHandle;
        let handle = XlibWindowHandle::new(self.hwnd as std::ffi::c_ulong);
        let raw = RawWindowHandle::Xlib(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::XlibDisplayHandle;
        let raw = RawDisplayHandle::Xlib(XlibDisplayHandle::new(None, 0));
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

/// Chart renderer state
struct ChartRendererState {
    /// SMuFL font for music notation
    smufl_font: SMuFLFont<'static>,
    /// Text font data (FreeSans)
    text_font_data: Arc<Vec<u8>>,
    /// MuseJazz font data for chord symbols
    musejazz_font_data: Arc<Vec<u8>>,
    /// Layout engine
    layout_engine: ChartLayoutEngine,
    /// Current chart
    chart: Option<Chart>,
    /// Layout result (cached)
    layout_result: Option<ChartLayoutResult>,
    /// Last rendered viewport size (for detecting resize)
    last_viewport: (u32, u32),
    /// View transform for pan/zoom
    transform: Affine,
    /// Whether mouse button is held (for drag-to-pan)
    mouse_down: bool,
    /// Last known mouse position
    prior_position: Option<Point>,
    /// Whether to auto-fit on first render
    needs_initial_fit: bool,
}

impl ChartRendererState {
    fn new(chart: Option<Chart>) -> Self {
        // Load SMuFL font
        let smufl_font = SMuFLFont::from_reader(BRAVURA_FONT, std::io::Cursor::new(BRAVURA_METADATA))
            .expect("Failed to load Bravura font");

        // Create font data arcs
        let text_font_data = Arc::new(TEXT_FONT.to_vec());
        let musejazz_font_data = Arc::new(MUSEJAZZ_TEXT_FONT.to_vec());

        // Create layout engine with static style
        let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
        let layout_engine = ChartLayoutEngine::new(
            style,
            text_font_data.clone(),
            musejazz_font_data.clone(),
        );

        // Layout chart if present
        let layout_result = chart.as_ref().map(|c| {
            layout_engine.layout_chart(c, &LayoutMode::default())
        });

        // Initial transform: translate to show content, then apply DPI scale
        let initial_transform = Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);

        Self {
            smufl_font,
            text_font_data,
            musejazz_font_data,
            layout_engine,
            chart,
            layout_result,
            last_viewport: (0, 0),
            transform: initial_transform,
            mouse_down: false,
            prior_position: None,
            needs_initial_fit: true,
        }
    }

    /// Handle baseview events for zoom/pan
    fn handle_event(&mut self, event: &Event) -> EventStatus {
        const BASE: f64 = 1.05; // 5% per scroll increment
        const PIXELS_PER_LINE: f64 = 20.0;

        match event {
            Event::Mouse(MouseEvent::WheelScrolled { delta, .. }) => {
                // Zoom about cursor position
                if let Some(cursor) = self.prior_position {
                    let delta_y = match delta {
                        ScrollDelta::Lines { y, .. } => *y as f64 * PIXELS_PER_LINE,
                        ScrollDelta::Pixels { y, .. } => *y as f64,
                    };
                    // Negate delta: scroll up (negative delta) = zoom in
                    let exponent = -delta_y / PIXELS_PER_LINE;
                    self.transform = self.transform.then_scale_about(BASE.powf(exponent), cursor);
                }
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::CursorMoved { position, .. }) => {
                let pos = Point::new(position.x, position.y);
                // Handle drag-to-pan when mouse is held down
                if self.mouse_down {
                    if let Some(prior) = self.prior_position {
                        let delta = pos - prior;
                        self.transform = self.transform.then_translate(delta);
                    }
                }
                self.prior_position = Some(pos);
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::ButtonPressed { button: MouseButton::Left, .. }) => {
                self.mouse_down = true;
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::ButtonReleased { button: MouseButton::Left, .. }) => {
                self.mouse_down = false;
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::CursorLeft) => {
                self.mouse_down = false;
                self.prior_position = None;
                EventStatus::Ignored
            }
            _ => EventStatus::Ignored,
        }
    }

    /// Calculate transform to fit page in viewport while maintaining aspect ratio
    fn calculate_fit_transform(&self, viewport_width: f64, viewport_height: f64) -> Affine {
        if let Some(ref layout) = self.layout_result {
            if !layout.pages.is_empty() {
                let page = &layout.pages[0];
                // Calculate scale to fit page in viewport (with margins)
                let margin = 20.0;
                let available_width = viewport_width - margin * 2.0;
                let available_height = viewport_height - margin * 2.0;

                let scale_x = available_width / page.width;
                let scale_y = available_height / page.height;
                // Use smaller scale to maintain aspect ratio
                let scale = scale_x.min(scale_y);

                // Center the page in the viewport
                let scaled_width = page.width * scale;
                let scaled_height = page.height * scale;
                let offset_x = (viewport_width - scaled_width) / 2.0;
                let offset_y = (viewport_height - scaled_height) / 2.0;

                return Affine::translate((offset_x, offset_y)) * Affine::scale(scale);
            }
        }
        // Fallback to default transform
        Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE)
    }

    fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
        // Update viewport tracking
        self.last_viewport = (width, height);

        // Auto-fit on first render
        if self.needs_initial_fit && self.layout_result.is_some() {
            self.transform = self.calculate_fit_transform(width as f64, height as f64);
            self.needs_initial_fit = false;
        }

        // Dark gray canvas background (page view style)
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(64, 64, 64),
            None,
            &Rect::new(0.0, 0.0, width as f64, height as f64),
        );

        if let Some(ref layout) = self.layout_result {
            // Create scene renderer with fonts
            let mut renderer = SceneRenderBuilder::new()
                .spatium(5.0)
                .build()
                .with_font(&self.smufl_font)
                .with_text_font_arc(self.text_font_data.clone())
                .with_named_font_arc("MuseJazzText", self.musejazz_font_data.clone())
                .with_named_font_arc("MuseJazz", self.musejazz_font_data.clone())
                .with_named_font_arc("section-note", self.text_font_data.clone())
                .with_named_font_arc("title-bold", self.text_font_data.clone())
                .with_named_font_arc("part-name-bold", self.text_font_data.clone());

            // Apply user's view transform to scene
            let mut transformed_scene = layout.scene.clone();
            transformed_scene.transform = self.transform;

            // Render scene nodes to Vello scene
            renderer.render(scene, &transformed_scene);
        }
    }
}

// Type alias for the docked window
type ChartDockedWindow = DockedWindow;
type ChartFloatingWindow = ReaperWindow;

thread_local! {
    static CHART_WINDOW: RefCell<Option<ChartDockedWindow>> = const { RefCell::new(None) };
    static FLOATING_CHART_WINDOW: RefCell<Option<ChartFloatingWindow>> = const { RefCell::new(None) };
}

/// Open the chart window
fn open_chart_window() {
    info!("Opening chart window with Thriller demo...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    // Parse the demo chart
    let chart = match Chart::parse(DEFAULT_CHART_TEXT) {
        Ok(c) => {
            let title = c.metadata.title.as_deref().unwrap_or("Untitled");
            info!(
                "Parsed chart: {} - {} sections, {} total measures",
                title,
                c.sections.len(),
                c.sections.iter().map(|s| s.measures().len()).sum::<usize>()
            );
            Some(c)
        }
        Err(e) => {
            warn!("Failed to parse chart: {}", e);
            None
        }
    };

    // Create renderer state (shared between render and event handlers)
    let state = Arc::new(Mutex::new(ChartRendererState::new(chart)));
    let render_state = state.clone();
    let event_state = state;

    // Create the render function
    let render_fn = move |scene: &mut Scene, w: u32, h: u32| {
        if let Ok(mut renderer) = render_state.lock() {
            renderer.render(scene, w, h);
        }
    };

    // Create the event handler function
    let event_fn = move |event: &Event| -> EventStatus {
        if let Ok(mut renderer) = event_state.lock() {
            renderer.handle_event(event)
        } else {
            EventStatus::Ignored
        }
    };

    let source = VelloEmbedSourceWithEvents::new(render_fn, event_fn);

    // Open as docked window
    let window = DockedWindow::open(
        low_reaper,
        &parent,
        source,
        "fts_chart_window",
        "FTS Chart",
        (400, 600),
        0, // Dock to left initially
    );

    CHART_WINDOW.with(|cell| {
        if let Some(mut old) = cell.borrow_mut().take() {
            old.close(low_reaper);
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Chart window opened");
}

/// Close the chart window
fn close_chart_window() {
    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    CHART_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close(low_reaper);
            info!("Chart window closed");
        }
    });
}

/// Open the chart window as floating (not docked)
fn open_floating_chart_window() {
    info!("Opening floating chart window with Thriller demo...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    // Parse the demo chart
    let chart = match Chart::parse(DEFAULT_CHART_TEXT) {
        Ok(c) => {
            let title = c.metadata.title.as_deref().unwrap_or("Untitled");
            info!(
                "Parsed chart: {} - {} sections, {} total measures",
                title,
                c.sections.len(),
                c.sections.iter().map(|s| s.measures().len()).sum::<usize>()
            );
            Some(c)
        }
        Err(e) => {
            warn!("Failed to parse chart: {}", e);
            None
        }
    };

    // Create renderer state (shared between render and event handlers)
    let state = Arc::new(Mutex::new(ChartRendererState::new(chart)));
    let render_state = state.clone();
    let event_state = state;

    // Create the render function
    let render_fn = move |scene: &mut Scene, w: u32, h: u32| {
        if let Ok(mut renderer) = render_state.lock() {
            renderer.render(scene, w, h);
        }
    };

    // Create the event handler function
    let event_fn = move |event: &Event| -> EventStatus {
        if let Ok(mut renderer) = event_state.lock() {
            renderer.handle_event(event)
        } else {
            EventStatus::Ignored
        }
    };

    let source = VelloEmbedSourceWithEvents::new(render_fn, event_fn);

    // Open as floating window
    let window = ReaperWindow::open(
        &parent,
        source,
        WindowConfig {
            title: "FTS Chart (Floating)".into(),
            size: (400, 600),
            ..Default::default()
        },
    );

    FLOATING_CHART_WINDOW.with(|cell| {
        if let Some(mut old) = cell.borrow_mut().take() {
            old.close();
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Floating chart window opened");
}

/// Close the floating chart window
fn close_floating_chart_window() {
    FLOATING_CHART_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close();
            info!("Floating chart window closed");
        }
    });
}

/// Check if the floating chart window is open
pub fn is_floating_window_open() -> bool {
    FLOATING_CHART_WINDOW.with(|cell| cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false))
}

/// Toggle the floating chart window
fn toggle_floating_chart_window() {
    if is_floating_window_open() {
        close_floating_chart_window();
    } else {
        open_floating_chart_window();
    }
}

/// Toggle the chart window
fn toggle_chart_window() {
    CHART_WINDOW.with(|cell| {
        let has_window = cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false);
        if has_window {
            close_chart_window();
        } else {
            open_chart_window();
        }
    });
}

/// Check if the chart window is open
pub fn is_window_open() -> bool {
    CHART_WINDOW.with(|cell| cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false))
}

/// Get action definitions for the chart window
pub fn actions() -> Vec<ActionDef> {
    vec![
        // Docked window actions
        ActionDef {
            command_id: "FTS_CHART_WINDOW_TOGGLE",
            display_name: "Toggle Chart Window (Docked)".to_string(),
            handler: toggle_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            toggle_state: Some(is_window_open),
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_OPEN",
            display_name: "Open Chart Window (Docked)".to_string(),
            handler: open_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_CLOSE",
            display_name: "Close Chart Window".to_string(),
            handler: close_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Floating window actions
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_TOGGLE",
            display_name: "Toggle Chart Window (Floating)".to_string(),
            handler: toggle_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            toggle_state: Some(is_floating_window_open),
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_OPEN",
            display_name: "Open Chart Window (Floating)".to_string(),
            handler: open_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_CLOSE",
            display_name: "Close Chart Window (Floating)".to_string(),
            handler: close_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}
