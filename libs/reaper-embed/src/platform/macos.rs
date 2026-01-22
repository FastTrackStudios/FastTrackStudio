//! macOS-specific window utilities
//!
//! Uses Cocoa APIs for overlay windows, window finding, and rect queries.

#![allow(unused_imports)]

use cocoa::appkit::{
    NSBackingStoreType, NSView, NSWindow, NSWindowCollectionBehavior, NSWindowStyleMask,
};
use cocoa::base::{NO, YES, id, nil};
use cocoa::foundation::{NSPoint, NSRect, NSSize, NSString};
use objc::runtime::{BOOL, Object, Sel};
use objc::{class, msg_send, sel, sel_impl};

use super::{OverlayConfig, WindowRect};
use crate::reaper::hwnd::RawHwnd;

/// Configure a window for overlay mode.
///
/// # Arguments
///
/// * `ns_view` - The NSView pointer (RawHwnd on macOS)
/// * `config` - Overlay configuration options
pub fn configure_overlay(ns_view: RawHwnd, config: &OverlayConfig) {
    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            log::warn!("configure_overlay: null NSView");
            return;
        }

        // Get the NSWindow from the NSView
        let window: id = msg_send![view, window];
        if window.is_null() {
            log::warn!("configure_overlay: NSView has no window");
            return;
        }

        // Transparency
        if config.transparent {
            let _: () = msg_send![window, setOpaque: NO];
            let clear_color: id = msg_send![class!(NSColor), clearColor];
            let _: () = msg_send![window, setBackgroundColor: clear_color];
        }

        // Click-through (ignore mouse events)
        if config.click_through {
            let _: () = msg_send![window, setIgnoresMouseEvents: YES];
        }

        // No decorations (borderless)
        if config.no_decorations {
            let _: () = msg_send![window, setStyleMask: NSWindowStyleMask::NSBorderlessWindowMask];
        }

        // No shadow
        if config.no_shadow {
            let _: () = msg_send![window, setHasShadow: NO];
        }

        // Topmost (floating window level)
        if config.topmost {
            // NSFloatingWindowLevel = 3
            let _: () = msg_send![window, setLevel: 3i64];
        }

        log::debug!(
            "Configured overlay window: transparent={}, click_through={}",
            config.transparent,
            config.click_through
        );
    }
}

/// Find a child NSView by tag (ID).
///
/// This mimics SWELL's GetDlgItem behavior on macOS.
///
/// # Arguments
///
/// * `parent` - The parent NSView or NSWindow pointer
/// * `child_id` - The tag value to search for
///
/// # Returns
///
/// The child NSView pointer if found, or `None`.
pub fn find_child_by_id(parent: RawHwnd, child_id: u32) -> Option<RawHwnd> {
    unsafe {
        let parent_obj: id = parent as id;
        if parent_obj.is_null() {
            return None;
        }

        // Get the content view if this is an NSWindow
        let view: id = if msg_send![parent_obj, isKindOfClass: class!(NSWindow)] {
            msg_send![parent_obj, contentView]
        } else if msg_send![parent_obj, isKindOfClass: class!(NSView)] {
            parent_obj
        } else {
            return None;
        };

        if view.is_null() {
            return None;
        }

        // Search subviews for matching tag
        find_view_by_tag(view, child_id as i64).map(|v| v as RawHwnd)
    }
}

/// Recursively search for a view with the given tag.
unsafe fn find_view_by_tag(view: id, tag: i64) -> Option<id> {
    if view.is_null() {
        return None;
    }

    // Check this view's tag
    let responds: BOOL = msg_send![view, respondsToSelector: sel!(tag)];
    if responds != NO {
        let view_tag: i64 = msg_send![view, tag];
        if view_tag == tag {
            return Some(view);
        }
    }

    // Search subviews
    let subviews: id = msg_send![view, subviews];
    if subviews.is_null() {
        return None;
    }

    let count: usize = msg_send![subviews, count];
    for i in 0..count {
        let subview: id = msg_send![subviews, objectAtIndex: i];

        // Check direct child
        let responds: BOOL = msg_send![subview, respondsToSelector: sel!(tag)];
        if responds != NO {
            let subview_tag: i64 = msg_send![subview, tag];
            if subview_tag == tag {
                return Some(subview);
            }
        }

        // Check if it's a scroll view (SWELL wraps some controls in scroll views)
        let is_scroll_view: BOOL = msg_send![subview, isKindOfClass: class!(NSScrollView)];
        if is_scroll_view != NO {
            let doc_view: id = msg_send![subview, documentView];
            if !doc_view.is_null() {
                let responds: BOOL = msg_send![doc_view, respondsToSelector: sel!(tag)];
                if responds != NO {
                    let doc_tag: i64 = msg_send![doc_view, tag];
                    if doc_tag == tag {
                        return Some(doc_view);
                    }
                }
            }
        }

        // Recurse into subviews
        // SAFETY: We're within an unsafe fn and subview is a valid id
        if let Some(found) = unsafe { find_view_by_tag(subview, tag) } {
            return Some(found);
        }
    }

    None
}

/// Get the screen-coordinate bounds of an NSView or NSWindow.
///
/// # Arguments
///
/// * `ns_view` - The NSView or NSWindow pointer
///
/// # Returns
///
/// The window rectangle in screen coordinates, or `None` if failed.
pub fn get_window_rect(ns_view: RawHwnd) -> Option<WindowRect> {
    unsafe {
        let obj: id = ns_view as id;
        if obj.is_null() {
            return None;
        }

        // Get frame based on type
        let frame: NSRect = if msg_send![obj, isKindOfClass: class!(NSWindow)] {
            msg_send![obj, frame]
        } else if msg_send![obj, isKindOfClass: class!(NSView)] {
            // For views, we need to convert to screen coordinates
            let view: id = obj;
            let window: id = msg_send![view, window];
            if window.is_null() {
                return None;
            }

            // Get bounds in window coordinates
            let bounds: NSRect = msg_send![view, bounds];

            // Convert to window coordinates
            let window_rect: NSRect = msg_send![view, convertRect:bounds toView:nil];

            // Convert to screen coordinates
            msg_send![window, convertRectToScreen: window_rect]
        } else {
            return None;
        };

        // macOS uses bottom-left origin, convert to top-left
        let screen_height = get_main_screen_height();
        let y = screen_height - (frame.origin.y + frame.size.height);

        Some(WindowRect {
            x: frame.origin.x as i32,
            y: y as i32,
            width: frame.size.width as u32,
            height: frame.size.height as u32,
        })
    }
}

/// Get the main screen height for coordinate conversion.
unsafe fn get_main_screen_height() -> f64 {
    let screens: id = msg_send![class!(NSScreen), screens];
    if screens.is_null() {
        return 1080.0; // fallback
    }
    let count: usize = msg_send![screens, count];
    if count == 0 {
        return 1080.0;
    }
    let main_screen: id = msg_send![screens, objectAtIndex: 0usize];
    let frame: NSRect = msg_send![main_screen, frame];
    frame.size.height
}

/// Create a new borderless, transparent overlay window.
///
/// # Arguments
///
/// * `x` - X position in screen coordinates
/// * `y` - Y position in screen coordinates (top-left origin)
/// * `width` - Window width
/// * `height` - Window height
///
/// # Returns
///
/// The NSWindow pointer, or `None` if creation failed.
pub fn create_overlay_window(x: i32, y: i32, width: u32, height: u32) -> Option<RawHwnd> {
    unsafe {
        // Convert from top-left to bottom-left coordinates
        let screen_height = get_main_screen_height();
        let flipped_y = screen_height - (y as f64) - (height as f64);

        let frame = NSRect::new(
            NSPoint::new(x as f64, flipped_y),
            NSSize::new(width as f64, height as f64),
        );

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

        // Configure for overlay
        let _: () = msg_send![window, setOpaque: NO];
        let clear_color: id = msg_send![class!(NSColor), clearColor];
        let _: () = msg_send![window, setBackgroundColor: clear_color];
        let _: () = msg_send![window, setHasShadow: NO];
        let _: () = msg_send![window, setIgnoresMouseEvents: YES];

        // Allow the window to be visible on all spaces
        let _: () = msg_send![window, setCollectionBehavior:
            NSWindowCollectionBehavior::NSWindowCollectionBehaviorCanJoinAllSpaces];

        // Get the content view to return
        let content_view: id = msg_send![window, contentView];

        log::debug!(
            "Created overlay window at ({}, {}) size {}x{}",
            x,
            y,
            width,
            height
        );

        Some(content_view as RawHwnd)
    }
}

/// Show or hide a window.
pub fn show_window(ns_view: RawHwnd, show: bool) {
    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = if msg_send![view, isKindOfClass: class!(NSWindow)] {
            view
        } else {
            msg_send![view, window]
        };

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

/// Set the position and size of a window.
pub fn set_window_frame(ns_view: RawHwnd, x: i32, y: i32, width: u32, height: u32) {
    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = if msg_send![view, isKindOfClass: class!(NSWindow)] {
            view
        } else {
            msg_send![view, window]
        };

        if window.is_null() {
            return;
        }

        // Convert from top-left to bottom-left coordinates
        let screen_height = get_main_screen_height();
        let flipped_y = screen_height - (y as f64) - (height as f64);

        let frame = NSRect::new(
            NSPoint::new(x as f64, flipped_y),
            NSSize::new(width as f64, height as f64),
        );

        let _: () = msg_send![window, setFrame:frame display:YES];
    }
}

/// Close and release a window.
pub fn close_window(ns_view: RawHwnd) {
    unsafe {
        let view: id = ns_view as id;
        if view.is_null() {
            return;
        }

        let window: id = if msg_send![view, isKindOfClass: class!(NSWindow)] {
            view
        } else {
            msg_send![view, window]
        };

        if !window.is_null() {
            let _: () = msg_send![window, close];
        }
    }
}
