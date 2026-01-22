//! Transparent interactive window implementation
//!
//! A borderless transparent window that receives mouse events (not click-through).
//! Uses native platform windows with GPU rendering via Vello.

#![allow(unused_imports)]

use raw_window_handle::{
    DisplayHandle, HandleError, HasDisplayHandle, HasWindowHandle, RawDisplayHandle,
    RawWindowHandle, WindowHandle,
};
use std::ptr::NonNull;
use vello::Scene;

use crate::embed_source::EmbedSource;
use crate::gpu::{GpuError, GpuState};
use crate::platform::WindowRect;
use crate::reaper::hwnd::RawHwnd;

/// A transparent interactive window with GPU rendering.
///
/// Unlike `OverlayWindow`, this window receives mouse events and can be interacted with.
/// The background is fully transparent, allowing content underneath to show through.
///
/// # Platform Support
///
/// - **macOS**: Uses NSWindow with transparent background
/// - **Windows**: Uses WS_EX_LAYERED (TODO)
/// - **Linux**: Uses X11 ARGB visual (TODO)
pub struct TransparentWindow<S: EmbedSource> {
    /// The content source for rendering
    source: S,
    /// The native window handle
    hwnd: Option<RawHwnd>,
    /// Window handle wrapper for WGPU
    handle_wrapper: Option<NativeHandleWrapper>,
    /// GPU state for Vello rendering
    gpu: Option<GpuState>,
    /// Current window bounds
    bounds: WindowRect,
    /// Whether the window is visible
    visible: bool,
}

/// Wrapper around native window handles implementing HasWindowHandle
struct NativeHandleWrapper {
    #[cfg(target_os = "macos")]
    ns_view: *mut std::ffi::c_void,
    #[cfg(target_os = "windows")]
    hwnd: isize,
    #[cfg(target_os = "linux")]
    window: u64,
}

// SAFETY: We ensure the window is only used on the main thread
unsafe impl Send for NativeHandleWrapper {}
unsafe impl Sync for NativeHandleWrapper {}

#[cfg(target_os = "macos")]
impl HasWindowHandle for NativeHandleWrapper {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::AppKitWindowHandle;
        let handle =
            AppKitWindowHandle::new(NonNull::new(self.ns_view).ok_or(HandleError::Unavailable)?);
        let raw = RawWindowHandle::AppKit(handle);
        // SAFETY: The window handle is valid for the lifetime of the wrapper
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "macos")]
impl HasDisplayHandle for NativeHandleWrapper {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::AppKitDisplayHandle;
        let raw = RawDisplayHandle::AppKit(AppKitDisplayHandle::new());
        // SAFETY: AppKit display handle is always valid
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasWindowHandle for NativeHandleWrapper {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::Win32WindowHandle;
        let handle = Win32WindowHandle::new(
            std::num::NonZeroIsize::new(self.hwnd).ok_or(HandleError::Unavailable)?,
        );
        let raw = RawWindowHandle::Win32(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasDisplayHandle for NativeHandleWrapper {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::WindowsDisplayHandle;
        let raw = RawDisplayHandle::Windows(WindowsDisplayHandle::new());
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasWindowHandle for NativeHandleWrapper {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::XlibWindowHandle;
        let handle = XlibWindowHandle::new(self.window);
        let raw = RawWindowHandle::Xlib(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasDisplayHandle for NativeHandleWrapper {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::XlibDisplayHandle;
        // TODO: Get actual display pointer
        let raw = RawDisplayHandle::Xlib(XlibDisplayHandle::new(None, 0));
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

impl<S: EmbedSource> TransparentWindow<S> {
    /// Open a new transparent interactive window.
    ///
    /// # Arguments
    ///
    /// * `x` - X position in screen coordinates
    /// * `y` - Y position in screen coordinates
    /// * `width` - Window width
    /// * `height` - Window height
    /// * `source` - The content source to render
    ///
    /// # Returns
    ///
    /// A `TransparentWindow` handle, or an error if creation failed.
    pub fn open(x: i32, y: i32, width: u32, height: u32, source: S) -> Result<Self, GpuError> {
        let bounds = WindowRect::new(x, y, width, height);

        // Create native transparent window
        let hwnd = create_native_transparent_window(x, y, width, height);

        let hwnd = hwnd.ok_or(GpuError::InvalidWindowHandle)?;

        // Create handle wrapper for GPU
        #[cfg(target_os = "macos")]
        let handle_wrapper = NativeHandleWrapper {
            ns_view: hwnd as *mut std::ffi::c_void,
        };

        #[cfg(target_os = "windows")]
        let handle_wrapper = NativeHandleWrapper {
            hwnd: hwnd as isize,
        };

        #[cfg(target_os = "linux")]
        let handle_wrapper = NativeHandleWrapper {
            window: hwnd as u64,
        };

        // Initialize GPU state
        let gpu = GpuState::new(&handle_wrapper, width, height)?;

        Ok(Self {
            source,
            hwnd: Some(hwnd),
            handle_wrapper: Some(handle_wrapper),
            gpu: Some(gpu),
            bounds,
            visible: false,
        })
    }

    /// Show the window.
    pub fn show(&mut self) {
        if let Some(hwnd) = self.hwnd {
            show_native_window(hwnd, true);
            self.visible = true;
        }
    }

    /// Hide the window.
    pub fn hide(&mut self) {
        if let Some(hwnd) = self.hwnd {
            show_native_window(hwnd, false);
            self.visible = false;
        }
    }

    /// Check if the window is visible.
    pub fn is_visible(&self) -> bool {
        self.visible
    }

    /// Toggle window visibility.
    pub fn toggle(&mut self) {
        if self.visible {
            self.hide();
        } else {
            self.show();
        }
    }

    /// Set the window position and size.
    pub fn set_frame(&mut self, x: i32, y: i32, width: u32, height: u32) {
        self.bounds = WindowRect::new(x, y, width, height);

        if let Some(hwnd) = self.hwnd {
            set_native_window_frame(hwnd, x, y, width, height);
        }

        // Resize GPU surface
        if let Some(ref mut gpu) = self.gpu {
            gpu.resize(width, height);
        }
    }

    /// Render the window content.
    ///
    /// Call this periodically (e.g., on a timer) to update the display.
    pub fn render(&mut self) -> Result<(), GpuError> {
        if !self.visible {
            return Ok(());
        }

        let gpu = self.gpu.as_mut().ok_or(GpuError::InvalidWindowHandle)?;

        // Build the scene
        let mut scene = Scene::new();
        self.source
            .render(&mut scene, self.bounds.width, self.bounds.height);

        // Render
        gpu.render(&scene)
    }

    /// Get the current bounds.
    pub fn bounds(&self) -> &WindowRect {
        &self.bounds
    }

    /// Set whether the window should ignore mouse events (click-through).
    ///
    /// When `true`, all mouse events pass through to windows underneath.
    /// Use this for overlay windows that should not intercept user input.
    pub fn set_click_through(&mut self, click_through: bool) {
        if let Some(hwnd) = self.hwnd {
            set_native_click_through(hwnd, click_through);
        }
    }

    /// Close the window.
    pub fn close(&mut self) {
        if let Some(hwnd) = self.hwnd.take() {
            close_native_window(hwnd);
        }
        self.handle_wrapper = None;
        self.gpu = None;
        self.visible = false;
    }
}

impl<S: EmbedSource> Drop for TransparentWindow<S> {
    fn drop(&mut self) {
        self.close();
    }
}

// Platform-specific native window functions

#[cfg(target_os = "macos")]
fn create_native_transparent_window(x: i32, y: i32, width: u32, height: u32) -> Option<RawHwnd> {
    use cocoa::appkit::{NSBackingStoreType, NSWindow, NSWindowStyleMask};
    use cocoa::base::{NO, YES, id, nil};
    use cocoa::foundation::{NSPoint, NSRect, NSSize};
    use objc::{class, msg_send, sel, sel_impl};

    unsafe {
        // Get main screen height for coordinate conversion
        let screens: id = msg_send![class!(NSScreen), screens];
        let screen_height = if screens.is_null() {
            1080.0
        } else {
            let count: usize = msg_send![screens, count];
            if count == 0 {
                1080.0
            } else {
                let main_screen: id = msg_send![screens, objectAtIndex: 0usize];
                let frame: NSRect = msg_send![main_screen, frame];
                frame.size.height
            }
        };

        // Convert from top-left to bottom-left coordinates
        let flipped_y = screen_height - (y as f64) - (height as f64);

        let frame = NSRect::new(
            NSPoint::new(x as f64, flipped_y),
            NSSize::new(width as f64, height as f64),
        );

        // Borderless window
        let style = NSWindowStyleMask::NSBorderlessWindowMask;

        let window: id = msg_send![class!(NSWindow), alloc];
        let window: id = msg_send![
            window,
            initWithContentRect:frame
            styleMask:style
            backing:NSBackingStoreType::NSBackingStoreBuffered
            defer:NO
        ];

        if window.is_null() {
            log::error!("Failed to create NSWindow");
            return None;
        }

        // Configure for transparency
        let _: () = msg_send![window, setOpaque: NO];
        let clear_color: id = msg_send![class!(NSColor), clearColor];
        let _: () = msg_send![window, setBackgroundColor: clear_color];
        let _: () = msg_send![window, setHasShadow: NO];

        // IMPORTANT: Do NOT set ignoresMouseEvents - we want interaction
        // let _: () = msg_send![window, setIgnoresMouseEvents: NO]; // This is default

        // Allow the window to be moved by dragging anywhere
        let _: () = msg_send![window, setMovableByWindowBackground: YES];

        // Make it float above other windows
        let _: () = msg_send![window, setLevel: 3i64]; // NSFloatingWindowLevel

        // Get the content view
        let content_view: id = msg_send![window, contentView];

        // Enable layer-backing for the view (required for transparent WGPU rendering)
        let _: () = msg_send![content_view, setWantsLayer: YES];

        // Get the layer and configure it for transparency
        let layer: id = msg_send![content_view, layer];
        if !layer.is_null() {
            let _: () = msg_send![layer, setOpaque: NO];
            // Set clear background color for the layer
            let cg_clear: id = msg_send![class!(NSColor), clearColor];
            let cg_color: id = msg_send![cg_clear, CGColor];
            let _: () = msg_send![layer, setBackgroundColor: cg_color];
        }

        log::debug!(
            "Created transparent interactive window at ({}, {}) size {}x{}",
            x,
            y,
            width,
            height
        );

        Some(content_view as RawHwnd)
    }
}

#[cfg(target_os = "windows")]
fn create_native_transparent_window(x: i32, y: i32, width: u32, height: u32) -> Option<RawHwnd> {
    // TODO: Implement Windows transparent window
    // Would use CreateWindowEx with WS_EX_LAYERED
    log::warn!("Windows transparent window not yet implemented");
    None
}

#[cfg(target_os = "linux")]
fn create_native_transparent_window(x: i32, y: i32, width: u32, height: u32) -> Option<RawHwnd> {
    // TODO: Implement Linux transparent window
    // Would use X11 with ARGB visual
    log::warn!("Linux transparent window not yet implemented");
    None
}

#[cfg(target_os = "macos")]
fn show_native_window(ns_view: RawHwnd, show: bool) {
    use cocoa::base::{id, nil};
    use objc::{class, msg_send, sel, sel_impl};

    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = msg_send![view, window];
        if window.is_null() {
            return;
        }

        if show {
            let _: () = msg_send![window, orderFront: nil];
        } else {
            let _: () = msg_send![window, orderOut: nil];
        }
    }
}

#[cfg(target_os = "windows")]
fn show_native_window(_hwnd: RawHwnd, _show: bool) {
    // TODO: Implement
}

#[cfg(target_os = "linux")]
fn show_native_window(_hwnd: RawHwnd, _show: bool) {
    // TODO: Implement
}

#[cfg(target_os = "macos")]
fn set_native_window_frame(ns_view: RawHwnd, x: i32, y: i32, width: u32, height: u32) {
    use cocoa::base::{YES, id, nil};
    use cocoa::foundation::{NSPoint, NSRect, NSSize};
    use objc::{class, msg_send, sel, sel_impl};

    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = msg_send![view, window];
        if window.is_null() {
            return;
        }

        // Get screen height for coordinate conversion
        let screens: id = msg_send![class!(NSScreen), screens];
        let screen_height = if screens.is_null() {
            1080.0
        } else {
            let count: usize = msg_send![screens, count];
            if count == 0 {
                1080.0
            } else {
                let main_screen: id = msg_send![screens, objectAtIndex: 0usize];
                let frame: NSRect = msg_send![main_screen, frame];
                frame.size.height
            }
        };

        let flipped_y = screen_height - (y as f64) - (height as f64);
        let frame = NSRect::new(
            NSPoint::new(x as f64, flipped_y),
            NSSize::new(width as f64, height as f64),
        );

        let _: () = msg_send![window, setFrame:frame display:YES];
    }
}

#[cfg(target_os = "windows")]
fn set_native_window_frame(_hwnd: RawHwnd, _x: i32, _y: i32, _width: u32, _height: u32) {
    // TODO: Implement
}

#[cfg(target_os = "linux")]
fn set_native_window_frame(_hwnd: RawHwnd, _x: i32, _y: i32, _width: u32, _height: u32) {
    // TODO: Implement
}

#[cfg(target_os = "macos")]
fn close_native_window(ns_view: RawHwnd) {
    use cocoa::base::{id, nil};
    use objc::{class, msg_send, sel, sel_impl};

    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = msg_send![view, window];
        if !window.is_null() {
            let _: () = msg_send![window, close];
        }
    }
}

#[cfg(target_os = "macos")]
fn set_native_click_through(ns_view: RawHwnd, click_through: bool) {
    use cocoa::base::{NO, YES, id};
    use objc::{msg_send, sel, sel_impl};

    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = msg_send![view, window];
        if window.is_null() {
            return;
        }

        let value = if click_through { YES } else { NO };
        let _: () = msg_send![window, setIgnoresMouseEvents: value];
        log::debug!("Set window click-through: {}", click_through);
    }
}

#[cfg(target_os = "windows")]
fn close_native_window(_hwnd: RawHwnd) {
    // TODO: Implement
}

#[cfg(target_os = "windows")]
fn set_native_click_through(_hwnd: RawHwnd, _click_through: bool) {
    // TODO: Implement using WS_EX_TRANSPARENT
}

#[cfg(target_os = "linux")]
fn close_native_window(_hwnd: RawHwnd) {
    // TODO: Implement
}

#[cfg(target_os = "linux")]
fn set_native_click_through(_hwnd: RawHwnd, _click_through: bool) {
    // TODO: Implement using X11 input passthrough
}
