//! Armed Click Action System
//!
//! Implements custom "armed" behavior without using REAPER's native ArmCommand.
//! REAPER's native arming consumes the next click entirely, which doesn't work
//! for our workflow pattern where we want to intercept clicks and run our action.
//!
//! Instead, we intercept WM_LBUTTONDOWN in the wheel hook and check if the
//! active workflow has an armed click action that matches the current mouse context.

use reaper_high::Reaper;
use std::sync::atomic::{AtomicBool, Ordering};
use tracing::debug;

// === Debug Mouse Context Toggle ===

static DEBUG_MOUSE_CONTEXT: AtomicBool = AtomicBool::new(false);

/// Toggle debug mouse context logging
pub fn toggle_debug_mouse_context() -> bool {
    let new_state = !DEBUG_MOUSE_CONTEXT.load(Ordering::Relaxed);
    DEBUG_MOUSE_CONTEXT.store(new_state, Ordering::Relaxed);
    new_state
}

/// Check if debug mouse context logging is enabled
pub fn is_debug_mouse_context_enabled() -> bool {
    DEBUG_MOUSE_CONTEXT.load(Ordering::Relaxed)
}

/// Defines what contexts an armed click action responds to
#[derive(Debug, Clone)]
pub struct ArmedClickAction {
    /// Action command ID to execute on click
    pub action: String,
    /// Mouse contexts that trigger this action
    pub target_contexts: Vec<ArmedContext>,
    /// Whether to pass the click through to REAPER after executing (default: false)
    pub pass_through: bool,
}

impl ArmedClickAction {
    pub fn new(action: impl Into<String>) -> Self {
        Self {
            action: action.into(),
            target_contexts: Vec::new(),
            pass_through: false,
        }
    }

    /// Add a target context
    pub fn with_context(mut self, context: ArmedContext) -> Self {
        self.target_contexts.push(context);
        self
    }

    /// Add multiple target contexts
    pub fn with_contexts(mut self, contexts: impl IntoIterator<Item = ArmedContext>) -> Self {
        self.target_contexts.extend(contexts);
        self
    }

    /// Set whether to pass the click through after executing
    pub fn with_pass_through(mut self, pass_through: bool) -> Self {
        self.pass_through = pass_through;
        self
    }

    /// Check if the given mouse position matches any of our target contexts
    pub fn matches_position(&self, mouse_x: i32, mouse_y: i32) -> bool {
        for context in &self.target_contexts {
            if context.matches_position(mouse_x, mouse_y) {
                return true;
            }
        }

        false
    }

    /// Execute the armed action
    pub fn execute(&self) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        // First, try using the action registry's stored command IDs
        if let Some(cmd_id) = crate::infrastructure::action_registry::get_command_id(&self.action) {
            debug!(action = %self.action, cmd_id = cmd_id.get(), "Executing armed click action via registry");
            unsafe {
                medium.low().Main_OnCommand(cmd_id.get() as i32, 0);
            }
            return;
        }

        // Try as named command
        if let Some(cmd_id) = medium.named_command_lookup(self.action.as_str()) {
            debug!(action = %self.action, cmd_id = cmd_id.get(), "Executing armed click action via named lookup");
            unsafe {
                medium.low().Main_OnCommand(cmd_id.get() as i32, 0);
            }
            return;
        }

        // Try parsing as numeric
        if let Ok(cmd_id) = self.action.parse::<i32>() {
            debug!(action = %self.action, cmd_id = cmd_id, "Executing armed click action as numeric");
            unsafe {
                medium.low().Main_OnCommand(cmd_id, 0);
            }
            return;
        }

        tracing::warn!(action = %self.action, "Could not resolve armed action command ID");
    }
}

/// Context where armed click action can be triggered
#[derive(Debug, Clone)]
pub enum ArmedContext {
    /// Any click in arrange view
    Arrange,
    /// Click on a media item (anywhere on the item)
    Item,
    /// Click on item edge (for trimming)
    ItemEdge,
    /// Click on item lower half
    ItemLower,
    /// Click in empty track area (no item)
    Track,
    /// Click on ruler
    Ruler,
    /// Click on envelope
    Envelope,
}

impl ArmedContext {
    /// Check if the given mouse position matches this context
    fn matches_position(&self, mouse_x: i32, mouse_y: i32) -> bool {
        match self {
            ArmedContext::Arrange => {
                // Check if mouse is in arrange view
                is_in_arrange_view(mouse_x, mouse_y)
            }
            ArmedContext::Item => {
                // Check if there's an item at this position
                is_over_item(mouse_x, mouse_y)
            }
            ArmedContext::ItemEdge => {
                // TODO: Detect item edge specifically
                // For now, check if over item (could be refined with position relative to item bounds)
                is_over_item(mouse_x, mouse_y)
            }
            ArmedContext::ItemLower => {
                // TODO: Detect lower half of item
                // For now, check if over item
                is_over_item(mouse_x, mouse_y)
            }
            ArmedContext::Track => {
                // In arrange but NOT over an item
                is_in_arrange_view(mouse_x, mouse_y) && !is_over_item(mouse_x, mouse_y)
            }
            ArmedContext::Ruler => {
                // TODO: Implement ruler detection
                false
            }
            ArmedContext::Envelope => {
                // TODO: Implement envelope detection
                false
            }
        }
    }
}

/// Check if the mouse is in the arrange view
fn is_in_arrange_view(mouse_x: i32, mouse_y: i32) -> bool {
    use crate::input::reaper_windows;
    use reaper_low::Swell;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Get arrange window using the existing helper
    if let Some(arrange_hwnd) = reaper_windows::get_arrange_wnd(&medium) {
        // Get arrange window rect
        let mut rect = reaper_low::raw::RECT {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
        };

        let swell = Swell::get();
        unsafe {
            swell.GetWindowRect(arrange_hwnd, &mut rect);
        }

        // Check if mouse is within arrange bounds
        mouse_x >= rect.left
            && mouse_x < rect.right
            && mouse_y >= rect.top
            && mouse_y < rect.bottom
    } else {
        false
    }
}

/// Check if the mouse is over a media item
fn is_over_item(mouse_x: i32, mouse_y: i32) -> bool {
    get_item_at_point(mouse_x, mouse_y).is_some()
}

/// Get the item at a screen position (if any)
pub fn get_item_at_point(mouse_x: i32, mouse_y: i32) -> Option<*mut reaper_low::raw::MediaItem> {
    use std::ptr;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    let mut take_out: *mut reaper_low::raw::MediaItem_Take = ptr::null_mut();

    let item = unsafe {
        medium.low().GetItemFromPoint(
            mouse_x,
            mouse_y,
            true, // allow_locked
            &mut take_out,
        )
    };

    if item.is_null() {
        None
    } else {
        Some(item)
    }
}

/// Detect the likely mouse modifier context based on mouse position
/// Returns (context_name, details) for debugging
pub fn detect_mouse_modifier_context(mouse_x: i32, mouse_y: i32) -> (String, String) {
    use crate::input::reaper_windows;
    use reaper_low::Swell;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Check if over an item
    let mut take_out: *mut reaper_low::raw::MediaItem_Take = std::ptr::null_mut();
    let item = unsafe {
        medium.low().GetItemFromPoint(mouse_x, mouse_y, true, &mut take_out)
    };

    if !item.is_null() {
        // We're over an item - try to determine which part
        // Get item bounds to determine if we're on edge, lower half, etc.
        unsafe {
            let track = medium.low().GetMediaItem_Track(item);
            let item_pos = medium.low().GetMediaItemInfo_Value(item, c"D_POSITION".as_ptr());
            let item_len = medium.low().GetMediaItemInfo_Value(item, c"D_LENGTH".as_ptr());
            let item_top = medium.low().GetMediaItemInfo_Value(item, c"F_FREEMODE_Y".as_ptr());
            let item_height = medium.low().GetMediaItemInfo_Value(item, c"F_FREEMODE_H".as_ptr());

            // Convert time to screen X
            let arrange_start = medium.low().GetSet_ArrangeView2(
                std::ptr::null_mut(),
                false,
                0,
                0,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            );

            // Get arrange window for coordinate conversion
            if let Some(arrange_hwnd) = reaper_windows::get_arrange_wnd(&medium) {
                let mut arrange_rect = reaper_low::raw::RECT {
                    left: 0, top: 0, right: 0, bottom: 0
                };
                Swell::get().GetWindowRect(arrange_hwnd, &mut arrange_rect);

                // Convert mouse to client coordinates
                let mut pt = reaper_low::raw::POINT { x: mouse_x, y: mouse_y };
                Swell::get().ScreenToClient(arrange_hwnd, &mut pt);

                // Calculate relative position within the item
                // This is approximate - REAPER's actual detection is more complex
                let item_screen_left = medium.low().SnapToGrid(std::ptr::null_mut(), item_pos);

                // Estimate edge zones (roughly 5-10 pixels from edge)
                let edge_threshold = 8;

                // For now, report what we can detect
                let track_num = if !track.is_null() {
                    medium.low().GetMediaTrackInfo_Value(track, c"IP_TRACKNUMBER".as_ptr()) as i32
                } else {
                    0
                };

                let details = format!(
                    "Track {}, Pos: {:.2}s, Len: {:.2}s, Mouse client: ({}, {})",
                    track_num, item_pos, item_len, pt.x, pt.y
                );

                // Try to detect edge vs body vs lower half
                // This is a rough approximation
                return ("MM_CTX_ITEM (or edge/lower)".to_string(), details);
            }
        }

        return ("MM_CTX_ITEM".to_string(), "Over item (bounds unknown)".to_string());
    }

    // Check if in arrange view
    if is_in_arrange_view(mouse_x, mouse_y) {
        // Not over item, but in arrange - likely track or empty area
        return ("MM_CTX_TRACK (or empty)".to_string(), "In arrange, no item".to_string());
    }

    // Check for ruler
    if let Some(ruler_hwnd) = reaper_windows::get_ruler_wnd(&medium) {
        let mut rect = reaper_low::raw::RECT { left: 0, top: 0, right: 0, bottom: 0 };
        unsafe {
            Swell::get().GetWindowRect(ruler_hwnd, &mut rect);
        }
        if mouse_x >= rect.left && mouse_x < rect.right
            && mouse_y >= rect.top && mouse_y < rect.bottom
        {
            return ("MM_CTX_RULER".to_string(), "On timeline ruler".to_string());
        }
    }

    ("Unknown".to_string(), format!("Screen pos: ({}, {})", mouse_x, mouse_y))
}

/// Helper to create common armed click configurations
impl ArmedClickAction {
    /// Create an armed action that triggers on any item click
    pub fn on_item(action: impl Into<String>) -> Self {
        Self::new(action).with_context(ArmedContext::Item)
    }

    /// Create an armed action that triggers anywhere in arrange view
    pub fn in_arrange(action: impl Into<String>) -> Self {
        Self::new(action).with_context(ArmedContext::Arrange)
    }

    /// Create an armed action that triggers on item lower half (slip edit zone)
    pub fn on_item_lower(action: impl Into<String>) -> Self {
        Self::new(action).with_context(ArmedContext::ItemLower)
    }
}
