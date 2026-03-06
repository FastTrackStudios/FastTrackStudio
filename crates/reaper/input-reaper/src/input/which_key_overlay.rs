//! Which-Key transparent overlay window.
//!
//! Shows available key continuations in a single-column popup anchored to the
//! bottom-right of REAPER's arrange view — similar to which-key.nvim.
//! Uses `reaper_embed::TransparentWindow` for GPU-accelerated Vello rendering.
//!
//! The overlay uses deferred display: when a partial match occurs, the data is
//! stored but the window isn't opened immediately. The timer callback (`refresh`)
//! checks if enough time has elapsed since the last keypress — only then does
//! the window appear. If the user completes the sequence quickly, the overlay
//! never flashes on screen.

use reaper_embed::{TransparentWindow, VelloTextRenderer};
use reaper_high::Reaper;
use reaper_low::Swell;
use reaper_low::raw::RECT;
use std::cell::RefCell;
use std::time::Instant;
use tracing::{debug, warn};
use vello::Scene;
use vello::kurbo::{Affine, RoundedRect};
use vello::peniko::{Color, Fill};

use crate::input::reaper_windows::get_arrange_wnd;

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const FONT_SIZE: f32 = 13.0;
const HEADER_FONT_SIZE: f32 = 12.0;
const LINE_HEIGHT: f64 = 24.0;
const PADDING_X: f64 = 16.0;
const PADDING_Y: f64 = 12.0;
const CORNER_RADIUS: f64 = 10.0;
const MARGIN: f64 = 16.0;
const SEPARATOR_HEIGHT: f64 = 1.0;
const HEADER_BOTTOM_PAD: f64 = 8.0;

/// Minimum popup width so small trees don't look cramped.
const MIN_POPUP_WIDTH: f64 = 200.0;
/// Extra horizontal padding beyond measured text.
const WIDTH_PADDING: f64 = 40.0;

/// Delay before the overlay window actually appears (ms).
/// If the sequence completes within this window, the popup never shows.
const SHOW_DELAY_MS: u64 = 300;

// ---------------------------------------------------------------------------
// Color palette — monochrome black & white
// ---------------------------------------------------------------------------

/// Background: pure black with full opacity
const BG_COLOR: Color = Color::from_rgba8(5, 5, 5, 252);
/// Subtle border tint
const BORDER_COLOR: Color = Color::from_rgba8(55, 55, 55, 200);
/// Header label: medium grey
const HEADER_COLOR: Color = Color::from_rgba8(140, 140, 140, 255);
/// Separator line
const SEPARATOR_COLOR: Color = Color::from_rgba8(55, 55, 55, 180);
/// Key character: bright white
const KEY_COLOR: Color = Color::from_rgba8(240, 240, 240, 255);
/// Arrow / separator between key and label
const ARROW_COLOR: Color = Color::from_rgba8(80, 80, 80, 255);
/// Leaf label: light grey
const LABEL_COLOR: Color = Color::from_rgba8(180, 180, 180, 255);
/// Branch label: white (to distinguish from leaves)
const BRANCH_COLOR: Color = Color::from_rgba8(220, 220, 220, 255);
/// Branch "+" indicator: medium grey
const BRANCH_INDICATOR_COLOR: Color = Color::from_rgba8(120, 120, 120, 200);

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

struct OverlayEntry {
    key: String,
    label: String,
    is_branch: bool,
}

struct PendingOverlay {
    entries: Vec<OverlayEntry>,
    sequence: String,
    header_label: String,
    requested_at: Instant,
}

struct LiveOverlay {
    window: TransparentWindow,
    text_renderer: VelloTextRenderer,
    entries: Vec<OverlayEntry>,
    sequence: String,
    header_label: String,
}

enum OverlayState {
    Pending(PendingOverlay),
    Live(LiveOverlay),
}

// ---------------------------------------------------------------------------
// Thread-local state
// ---------------------------------------------------------------------------

thread_local! {
    static OVERLAY: RefCell<Option<OverlayState>> = const { RefCell::new(None) };
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Request the which-key overlay with the given continuations.
///
/// If the overlay is already live, it updates immediately.
/// Otherwise stores pending data — the window opens after SHOW_DELAY_MS.
pub fn show(sequence: &str, continuations: &[(String, String, bool)]) {
    let header_label = resolve_header_label(sequence);

    let entries: Vec<OverlayEntry> = continuations
        .iter()
        .map(|(k, l, is_branch)| OverlayEntry {
            key: k.clone(),
            label: l.clone(),
            is_branch: *is_branch,
        })
        .collect();

    OVERLAY.with(|cell| {
        let mut borrow = cell.borrow_mut();

        match borrow.as_mut() {
            Some(OverlayState::Live(live)) => {
                live.entries = entries;
                live.sequence = sequence.to_string();
                live.header_label = header_label;
                reposition_and_render(live);
            }
            Some(OverlayState::Pending(pending)) => {
                pending.entries = entries;
                pending.sequence = sequence.to_string();
                pending.header_label = header_label;
            }
            None => {
                *borrow = Some(OverlayState::Pending(PendingOverlay {
                    entries,
                    sequence: sequence.to_string(),
                    header_label,
                    requested_at: Instant::now(),
                }));
            }
        }
    });
}

/// Show all registered which-key prefix trees (cheat sheet).
/// Opens immediately (no delay) since this is an explicit user action.
pub fn show_all_prefixes() {
    let proc = crate::input::processor::get_processor().read().unwrap();
    let roots = proc.all_root_prefixes();
    if roots.is_empty() {
        return;
    }

    let entries: Vec<OverlayEntry> = roots
        .iter()
        .map(|(k, l, is_branch)| OverlayEntry {
            key: k.clone(),
            label: l.clone(),
            is_branch: *is_branch,
        })
        .collect();

    OVERLAY.with(|cell| {
        let mut borrow = cell.borrow_mut();

        // Close any existing overlay first
        if let Some(OverlayState::Live(mut live)) = borrow.take() {
            live.window.close();
        }

        // Open immediately — build a temporary PendingOverlay just for the open helper
        let pending = PendingOverlay {
            entries,
            sequence: String::new(),
            header_label: "Which-Key Bindings".to_string(),
            requested_at: Instant::now(),
        };

        if let Some(live) = open_overlay_window(&pending) {
            *borrow = Some(OverlayState::Live(live));
        }
    });
}

/// Hide and close the overlay (or cancel a pending show).
pub fn hide() {
    OVERLAY.with(|cell| {
        let prev = cell.borrow_mut().take();
        if let Some(OverlayState::Live(mut live)) = prev {
            live.window.close();
            debug!("Which-key overlay closed");
        }
    });
}

/// Refresh the overlay. Called from the timer callback (~30fps).
///
/// Promotes pending → live after SHOW_DELAY_MS. Repositions live overlays.
pub fn refresh() {
    OVERLAY.with(|cell| {
        let mut borrow = cell.borrow_mut();

        let should_promote = matches!(
            borrow.as_ref(),
            Some(OverlayState::Pending(p)) if p.requested_at.elapsed().as_millis() as u64 >= SHOW_DELAY_MS
        );

        if should_promote {
            if let Some(OverlayState::Pending(pending)) = borrow.take() {
                if let Some(live) = open_overlay_window(&pending) {
                    *borrow = Some(OverlayState::Live(live));
                }
            }
        } else if let Some(OverlayState::Live(live)) = borrow.as_mut() {
            reposition_and_render(live);
        }
    });
}

/// Check if the overlay window is currently open.
pub fn is_visible() -> bool {
    OVERLAY.with(|cell| matches!(*cell.borrow(), Some(OverlayState::Live(_))))
}

// ---------------------------------------------------------------------------
// Window creation
// ---------------------------------------------------------------------------

fn open_overlay_window(pending: &PendingOverlay) -> Option<LiveOverlay> {
    let (x, y, width, height) = compute_popup_geometry(&pending.entries);

    match TransparentWindow::open(x, y, width, height) {
        Ok(mut window) => {
            window.show();
            let text_renderer = VelloTextRenderer::new();
            let mut live = LiveOverlay {
                window,
                text_renderer,
                entries: pending
                    .entries
                    .iter()
                    .map(|e| OverlayEntry {
                        key: e.key.clone(),
                        label: e.label.clone(),
                        is_branch: e.is_branch,
                    })
                    .collect(),
                sequence: pending.sequence.clone(),
                header_label: pending.header_label.clone(),
            };
            render_overlay(&mut live);
            debug!("Which-key overlay opened");
            Some(live)
        }
        Err(e) => {
            warn!("Failed to open which-key overlay: {}", e);
            None
        }
    }
}

// ---------------------------------------------------------------------------
// Layout helpers
// ---------------------------------------------------------------------------

fn resolve_header_label(sequence: &str) -> String {
    if sequence.is_empty() {
        return String::new();
    }
    let first_key = &sequence[..1];
    // Look up the label from the processor's which-key trees
    let proc = crate::input::processor::get_processor().read().unwrap();
    proc.current_trees()
        .iter()
        .find(|(prefix, _, _)| prefix == first_key)
        .map(|(_, label, _)| label.clone())
        .unwrap_or_else(|| sequence.to_string())
}

fn get_arrange_bounds() -> Option<(i32, i32, u32, u32)> {
    use reaper_low::raw::POINT;

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let arrange_hwnd = get_arrange_wnd(medium_reaper)?;
    let swell = Swell::get();

    let mut client_rect = RECT {
        left: 0,
        right: 0,
        top: 0,
        bottom: 0,
    };
    unsafe {
        swell.GetClientRect(arrange_hwnd, &mut client_rect);
    }

    let mut top_left = POINT {
        x: client_rect.left,
        y: client_rect.top,
    };
    unsafe {
        swell.ClientToScreen(arrange_hwnd, &mut top_left);
    }

    let width = (client_rect.right - client_rect.left).unsigned_abs();
    let height = (client_rect.bottom - client_rect.top).unsigned_abs();

    let width = width.saturating_sub(16);
    let height = height.saturating_sub(16);

    #[cfg(target_os = "macos")]
    let screen_y = {
        let screen_height = reaper_embed::main_screen_height();
        (screen_height as i32) - top_left.y
    };

    #[cfg(not(target_os = "macos"))]
    let screen_y = top_left.y;

    Some((top_left.x, screen_y, width, height))
}

/// Compute popup geometry — single column, dynamic size, bottom-right of arrange.
fn compute_popup_geometry(entries: &[OverlayEntry]) -> (i32, i32, u32, u32) {
    let text_renderer = VelloTextRenderer::new();

    // Measure the widest entry to size the popup
    let mut max_entry_width = 0.0f64;
    for entry in entries {
        let prefix = if entry.is_branch { "+" } else { "" };
        // Approximate: "key › +label"
        let text = format!("{}  {}{}", entry.key, prefix, entry.label);
        let w = text_renderer.measure_text(&text, FONT_SIZE);
        max_entry_width = max_entry_width.max(w);
    }

    let popup_width =
        (max_entry_width + PADDING_X * 2.0 + WIDTH_PADDING).max(MIN_POPUP_WIDTH) as u32;

    // Height: header + separator + one row per entry
    let header_height = HEADER_FONT_SIZE as f64 + HEADER_BOTTOM_PAD + SEPARATOR_HEIGHT;
    let content_height = LINE_HEIGHT * entries.len() as f64;
    let popup_height = (PADDING_Y + header_height + content_height + PADDING_Y) as u32;

    // Bottom-right of arrange view
    if let Some((ax, ay, aw, ah)) = get_arrange_bounds() {
        let x = ax + (aw as i32) - (popup_width as i32) - (MARGIN as i32);
        let y = ay + (ah as i32) - (popup_height as i32) - (MARGIN as i32);
        (x, y, popup_width, popup_height.max(48))
    } else {
        (400, 400, popup_width, popup_height.max(48))
    }
}

// ---------------------------------------------------------------------------
// Positioning + rendering
// ---------------------------------------------------------------------------

fn reposition_and_render(overlay: &mut LiveOverlay) {
    let (x, y, width, height) = compute_popup_geometry(&overlay.entries);

    let current = overlay.window.bounds();
    if current.x != x || current.y != y || current.width != width || current.height != height {
        overlay.window.set_frame(x, y, width, height);
    }

    render_overlay(overlay);
}

fn render_overlay(overlay: &mut LiveOverlay) {
    let bounds = overlay.window.bounds();
    let w = bounds.width as f64;
    let h = bounds.height as f64;

    let mut scene = Scene::new();

    // -- Border (slightly larger rounded rect behind the main bg) --
    let border_rect = RoundedRect::new(0.0, 0.0, w, h, CORNER_RADIUS);
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        BORDER_COLOR,
        None,
        &border_rect,
    );

    let inset = 1.0;
    let bg_rect = RoundedRect::new(inset, inset, w - inset, h - inset, CORNER_RADIUS - 0.5);
    scene.fill(Fill::NonZero, Affine::IDENTITY, BG_COLOR, None, &bg_rect);

    // -- Header --
    let header_text = if overlay.header_label.is_empty() {
        overlay.sequence.clone()
    } else {
        format!("{}  {}", overlay.header_label, overlay.sequence)
    };

    let header_y = PADDING_Y + HEADER_FONT_SIZE as f64 * 0.85;
    // Center the header text
    let header_width = overlay
        .text_renderer
        .measure_text(&header_text, HEADER_FONT_SIZE);
    let header_x = (w - header_width) / 2.0;
    overlay.text_renderer.draw_text(
        &mut scene,
        &header_text,
        header_x.max(PADDING_X),
        header_y,
        HEADER_FONT_SIZE,
        HEADER_COLOR,
    );

    // -- Separator line --
    let sep_y = PADDING_Y + HEADER_FONT_SIZE as f64 + HEADER_BOTTOM_PAD;
    let sep_rect =
        vello::kurbo::Rect::new(PADDING_X, sep_y, w - PADDING_X, sep_y + SEPARATOR_HEIGHT);
    scene.fill(
        Fill::NonZero,
        Affine::IDENTITY,
        SEPARATOR_COLOR,
        None,
        &sep_rect,
    );

    // -- Entries (single column) --
    let content_top = sep_y + SEPARATOR_HEIGHT + 4.0;

    for (i, entry) in overlay.entries.iter().enumerate() {
        let x_base = PADDING_X;
        let y_base = content_top + LINE_HEIGHT * i as f64 + FONT_SIZE as f64 * 0.85;

        // Key: bright cyan, no brackets — just the character
        overlay
            .text_renderer
            .draw_text(&mut scene, &entry.key, x_base, y_base, FONT_SIZE, KEY_COLOR);

        // Arrow separator
        let key_width = overlay.text_renderer.measure_text(&entry.key, FONT_SIZE);
        let arrow_x = x_base + key_width + 5.0;
        overlay.text_renderer.draw_text(
            &mut scene,
            "\u{203A}", // › single right-pointing angle quotation mark
            arrow_x,
            y_base,
            FONT_SIZE,
            ARROW_COLOR,
        );

        // Label
        let arrow_width = overlay.text_renderer.measure_text("\u{203A}", FONT_SIZE);
        let label_x = arrow_x + arrow_width + 5.0;

        if entry.is_branch {
            // Branch indicator "+" in muted purple, then label in bright purple
            overlay.text_renderer.draw_text(
                &mut scene,
                "+",
                label_x,
                y_base,
                FONT_SIZE,
                BRANCH_INDICATOR_COLOR,
            );
            let plus_w = overlay.text_renderer.measure_text("+", FONT_SIZE);
            overlay.text_renderer.draw_text(
                &mut scene,
                &entry.label,
                label_x + plus_w,
                y_base,
                FONT_SIZE,
                BRANCH_COLOR,
            );
        } else {
            overlay.text_renderer.draw_text(
                &mut scene,
                &entry.label,
                label_x,
                y_base,
                FONT_SIZE,
                LABEL_COLOR,
            );
        }
    }

    if let Err(e) = overlay.window.render(&scene) {
        debug!("Which-key overlay render error: {}", e);
    }
}
