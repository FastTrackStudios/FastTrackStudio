//! Test module for reaper-embed windows
//!
//! Provides test actions for floating, docked, and transparent overlay windows.

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use raw_window_handle::{
    AppKitDisplayHandle, AppKitWindowHandle, DisplayHandle, HandleError, HasDisplayHandle,
    HasWindowHandle, RawDisplayHandle, RawWindowHandle, WindowHandle,
};
use reaper_embed::{DockedWindow, ReaperWindow, TransparentWindow, VelloEmbedSource, WindowConfig};
use reaper_high::Reaper;
use reaper_low::raw::HWND;
use std::cell::RefCell;
use std::ptr::NonNull;
use std::time::Instant;
use tracing::{info, warn};
use vello::Scene;
use vello::kurbo::{Affine, Circle, Point, Rect, RoundedRect, Vec2};
use vello::peniko::Color;
use vello::peniko::Fill;

// Type alias for the transparent window with our source type
type TestTransparentWindow =
    TransparentWindow<VelloEmbedSource<Box<dyn FnMut(&mut Scene, u32, u32) + Send>>>;

// Thread-local storage for test windows (REAPER extension runs on main thread only)
thread_local! {
    static FLOATING_WINDOW: RefCell<Option<ReaperWindow>> = const { RefCell::new(None) };
    static DOCKED_WINDOW: RefCell<Option<DockedWindow>> = const { RefCell::new(None) };
    static TRANSPARENT_WINDOW: RefCell<Option<TestTransparentWindow>> = const { RefCell::new(None) };
}

/// Wrapper around REAPER's main HWND that implements HasWindowHandle
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
        // On macOS, REAPER's HWND is actually an NSView*
        let ns_view = self.hwnd as *mut std::ffi::c_void;
        let handle =
            AppKitWindowHandle::new(NonNull::new(ns_view).ok_or(HandleError::Unavailable)?);
        let raw = RawWindowHandle::AppKit(handle);
        // SAFETY: The window handle is valid for the lifetime of self
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "macos")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        let raw = RawDisplayHandle::AppKit(AppKitDisplayHandle::new());
        // SAFETY: The display handle is valid
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

// Windows implementation
#[cfg(target_os = "windows")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::Win32WindowHandle;
        let handle = Win32WindowHandle::new(
            std::num::NonZeroIsize::new(self.hwnd as isize).ok_or(HandleError::Unavailable)?,
        );
        let raw = RawWindowHandle::Win32(handle);
        // SAFETY: The window handle is valid for the lifetime of self
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::WindowsDisplayHandle;
        let raw = RawDisplayHandle::Windows(WindowsDisplayHandle::new());
        // SAFETY: The display handle is valid
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

// Linux implementation
#[cfg(target_os = "linux")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::XlibWindowHandle;
        let handle = XlibWindowHandle::new(self.hwnd as std::ffi::c_ulong);
        let raw = RawWindowHandle::Xlib(handle);
        // SAFETY: The window handle is valid for the lifetime of self
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::XlibDisplayHandle;
        let raw = RawDisplayHandle::Xlib(XlibDisplayHandle::new(None, 0));
        // SAFETY: The display handle is valid
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

/// Create a test scene with animated content
fn create_test_scene(scene: &mut Scene, width: u32, height: u32, elapsed_secs: f64) {
    let w = width as f64;
    let h = height as f64;

    // Background
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(30, 30, 40, 255),
        None,
        &Rect::new(0.0, 0.0, w, h),
    );

    // Animated rotating rectangle
    let center_x = w / 2.0;
    let center_y = h / 2.0;
    let rotation = elapsed_secs * 0.5; // Radians per second

    let transform = Affine::translate(Vec2::new(center_x, center_y))
        .then_rotate(rotation)
        .then_translate(Vec2::new(-50.0, -30.0));

    scene.fill(
        Fill::NonZero,
        transform,
        Color::from_rgba8(100, 150, 255, 200),
        None,
        &RoundedRect::new(0.0, 0.0, 100.0, 60.0, 8.0),
    );

    // Animated bouncing circle
    let bounce_y = center_y + (elapsed_secs * 2.0).sin() * 50.0;
    let bounce_x = center_x + (elapsed_secs * 1.5).cos() * 80.0;

    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(255, 100, 100, 220),
        None,
        &Circle::new(Point::new(bounce_x, bounce_y), 25.0),
    );

    // Pulsating circle
    let pulse_radius = 15.0 + (elapsed_secs * 3.0).sin().abs() * 10.0;
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(100, 255, 150, 200),
        None,
        &Circle::new(Point::new(center_x, center_y), pulse_radius),
    );

    // Draw some static decorative elements
    for i in 0..5 {
        let x = 20.0 + i as f64 * 40.0;
        let y = h - 40.0;
        let hue = (i as f64 * 0.2 + elapsed_secs * 0.1) % 1.0;

        // Simple HSV to RGB conversion for hue
        let r = ((hue * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
        let g = (2.0 - (hue * 6.0 - 2.0).abs()).clamp(0.0, 1.0);
        let b = (2.0 - (hue * 6.0 - 4.0).abs()).clamp(0.0, 1.0);

        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgba8((r * 255.0) as u8, (g * 255.0) as u8, (b * 255.0) as u8, 255),
            None,
            &Circle::new(Point::new(x, y), 12.0),
        );
    }

    // Title text (simple rect placeholder - real text requires font loading)
    // We'll draw a "title bar" to show the window type
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(60, 60, 80, 255),
        None,
        &Rect::new(0.0, 0.0, w, 30.0),
    );

    // Time indicator (a moving bar)
    let time_bar_x = (elapsed_secs * 50.0) % w;
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(255, 255, 100, 150),
        None,
        &Rect::new(time_bar_x, 32.0, time_bar_x + 4.0, 38.0),
    );
}

/// Handler for opening a floating test window
fn open_floating_window_handler() {
    info!("Opening floating test window...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    let start_time = Instant::now();

    let source = VelloEmbedSource::new(move |scene, width, height| {
        let elapsed = start_time.elapsed().as_secs_f64();
        create_test_scene(scene, width, height, elapsed);
    });

    let config = WindowConfig {
        title: "FTS Floating Test".into(),
        size: (400, 300),
        ..Default::default()
    };

    let window = ReaperWindow::open(&parent, source, config);

    FLOATING_WINDOW.with(|cell| {
        // Close existing window if any
        if let Some(mut old_window) = cell.borrow_mut().take() {
            old_window.close();
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Floating test window opened");
}

/// Handler for opening a docked test window
fn open_docked_window_handler() {
    info!("Opening docked test window...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    let start_time = Instant::now();

    let source = VelloEmbedSource::new(move |scene, width, height| {
        let elapsed = start_time.elapsed().as_secs_f64();
        create_test_scene(scene, width, height, elapsed);
    });

    let window = DockedWindow::open(
        low_reaper,
        &parent,
        source,
        "fts_embed_test_docked",
        "FTS Docked Test",
        (350, 250),
        0, // Dock to left initially
    );

    DOCKED_WINDOW.with(|cell| {
        // Close existing window if any
        if let Some(mut old_window) = cell.borrow_mut().take() {
            old_window.close(low_reaper);
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Docked test window opened");
}

/// Handler for opening a transparent interactive window
fn open_overlay_window_handler() {
    info!("Opening transparent interactive window...");

    let start_time = Instant::now();

    // Create a boxed closure for the scene builder
    let builder: Box<dyn FnMut(&mut Scene, u32, u32) + Send> =
        Box::new(move |scene, width, height| {
            let elapsed = start_time.elapsed().as_secs_f64();
            create_overlay_scene(scene, width, height, elapsed);
        });

    let source = VelloEmbedSource::new(builder);

    // Open at a visible location - positioned 100 pixels from top-left
    match TransparentWindow::open(100, 100, 500, 400, source) {
        Ok(mut window) => {
            window.show();

            // Initial render
            if let Err(e) = window.render() {
                warn!("Initial render failed: {}", e);
            }

            TRANSPARENT_WINDOW.with(|cell| {
                // Close existing window if any
                if let Some(mut old_window) = cell.borrow_mut().take() {
                    old_window.close();
                }
                *cell.borrow_mut() = Some(window);
            });

            info!("Transparent interactive window opened - drag to move!");
        }
        Err(e) => {
            warn!("Failed to open transparent window: {}", e);
        }
    }
}

/// Create a transparent overlay scene
fn create_overlay_scene(scene: &mut Scene, width: u32, height: u32, elapsed_secs: f64) {
    let w = width as f64;
    let h = height as f64;

    // Draw some semi-transparent indicators at corners
    let corner_size = 30.0;
    let alpha = ((elapsed_secs * 2.0).sin() * 0.3 + 0.7).clamp(0.4, 1.0);
    let alpha_u8 = (alpha * 180.0) as u8;

    // Top-left corner indicator
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(100, 200, 255, alpha_u8),
        None,
        &RoundedRect::new(10.0, 10.0, 10.0 + corner_size, 10.0 + corner_size, 4.0),
    );

    // Top-right corner indicator
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(255, 100, 200, alpha_u8),
        None,
        &RoundedRect::new(
            w - 10.0 - corner_size,
            10.0,
            w - 10.0,
            10.0 + corner_size,
            4.0,
        ),
    );

    // Bottom indicators that might overlay track names
    for i in 0..5 {
        let x = 50.0 + i as f64 * 100.0;
        if x + 80.0 > w {
            break;
        }

        // Track name placeholder bar
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgba8(40, 40, 60, 200),
            None,
            &RoundedRect::new(x, h - 50.0, x + 80.0, h - 30.0, 3.0),
        );
    }

    // Center crosshair (playhead indicator style)
    let center_x = w / 2.0;
    let center_y = h / 2.0;

    // Horizontal line
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(255, 255, 255, 60),
        None,
        &Rect::new(
            center_x - 30.0,
            center_y - 1.0,
            center_x + 30.0,
            center_y + 1.0,
        ),
    );

    // Vertical line
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        Color::from_rgba8(255, 255, 255, 60),
        None,
        &Rect::new(
            center_x - 1.0,
            center_y - 30.0,
            center_x + 1.0,
            center_y + 30.0,
        ),
    );
}

/// Handler for closing all test windows
fn close_all_test_windows_handler() {
    info!("Closing all test windows...");

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    FLOATING_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close();
            info!("Closed floating window");
        }
    });

    DOCKED_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close(low_reaper);
            info!("Closed docked window");
        }
    });

    TRANSPARENT_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close();
            info!("Closed transparent window");
        }
    });
}

/// Handler for toggling dock state of docked window
fn toggle_dock_handler() {
    info!("Toggling dock state...");

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    DOCKED_WINDOW.with(|cell| {
        if let Some(ref mut window) = *cell.borrow_mut() {
            window.toggle_dock(low_reaper);
            info!("Dock state toggled");
        } else {
            warn!("No docked window to toggle");
        }
    });
}

/// Get action definitions for embed test
pub fn actions() -> Vec<ActionDef> {
    vec![
        ActionDef {
            command_id: "FTS_EMBED_TEST_FLOATING",
            display_name: "Open Floating Test Window".to_string(),
            handler: open_floating_window_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_EMBED_TEST_DOCKED",
            display_name: "Open Docked Test Window".to_string(),
            handler: open_docked_window_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_EMBED_TEST_TRANSPARENT",
            display_name: "Open Transparent Test Window".to_string(),
            handler: open_overlay_window_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_EMBED_TEST_TOGGLE_DOCK",
            display_name: "Toggle Dock State".to_string(),
            handler: toggle_dock_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_EMBED_TEST_CLOSE_ALL",
            display_name: "Close All Test Windows".to_string(),
            handler: close_all_test_windows_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}
