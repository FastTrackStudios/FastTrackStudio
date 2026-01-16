//! Track Name Overlay - displays track names over the arrange view
//!
//! Inspired by TK_Trackname_in_Arrange by TouristKiller.
//! Uses a transparent window with Vello rendering positioned over REAPER's arrange view.

use reaper_high::Reaper;
use reaper_low::raw::RECT;
use reaper_low::Swell;
use reaper_medium::TrackAttributeKey;
use std::cell::RefCell;
use std::sync::Arc;
use tracing::{debug, info, warn};
use skrifa::raw::FileRef;
use skrifa::{instance::LocationRef, FontRef, MetadataProvider};
use vello::kurbo::{Affine, Rect, RoundedRect};
use vello::peniko::{Blob, Brush, Color, Fill, FontData};
use vello::{Glyph, Scene};

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use crate::input::reaper_windows::get_arrange_wnd;
use reaper_embed::{TransparentWindow, VelloEmbedSource};

/// Embedded Arial font data (macOS system font path)
/// For production, consider bundling fonts or using system font lookup
#[cfg(target_os = "macos")]
const ARIAL_FONT_DATA: &[u8] = include_bytes!("/System/Library/Fonts/Supplemental/Arial.ttf");

#[cfg(target_os = "windows")]
const ARIAL_FONT_DATA: &[u8] = include_bytes!("C:/Windows/Fonts/arial.ttf");

#[cfg(target_os = "linux")]
const ARIAL_FONT_DATA: &[u8] = include_bytes!("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf");

/// Get arrange view window bounds in screen coordinates (top-left origin)
fn get_arrange_bounds() -> Option<(i32, i32, u32, u32)> {
    use reaper_low::raw::POINT;

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    let arrange_hwnd = get_arrange_wnd(medium_reaper)?;
    let swell = Swell::get();

    // Get client rect (size of the arrange view)
    let mut client_rect = RECT {
        left: 0,
        right: 0,
        top: 0,
        bottom: 0,
    };

    unsafe {
        swell.GetClientRect(arrange_hwnd, &mut client_rect);
    }

    // Convert top-left corner of client area to screen coordinates
    let mut top_left = POINT {
        x: client_rect.left,
        y: client_rect.top,
    };

    unsafe {
        swell.ClientToScreen(arrange_hwnd, &mut top_left);
    }

    let width = (client_rect.right - client_rect.left).unsigned_abs();
    let height = (client_rect.bottom - client_rect.top).unsigned_abs();

    // Subtract scrollbar area
    const SCROLLBAR_SIZE: u32 = 16;
    let width = width.saturating_sub(SCROLLBAR_SIZE);
    let height = height.saturating_sub(SCROLLBAR_SIZE);

    // On macOS, ClientToScreen returns coordinates in macOS native coordinate system
    // (bottom-left origin, Y increases upward). We need to convert to top-left origin
    // for TransparentWindow::open which expects top-left coordinates.
    #[cfg(target_os = "macos")]
    let screen_y = {
        // Get main screen height for coordinate conversion
        let screen_height = unsafe {
            use objc::{class, msg_send, sel, sel_impl};
            use cocoa::base::id;
            use cocoa::foundation::NSRect;

            let screens: id = msg_send![class!(NSScreen), screens];
            if screens.is_null() {
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
            }
        };

        // Convert from bottom-left to top-left:
        // In macOS coords, top_left.y is distance from bottom of screen to top of window
        // In top-left coords, we want distance from top of screen to top of window
        let y_top_left = (screen_height as i32) - top_left.y;
        y_top_left
    };

    #[cfg(not(target_os = "macos"))]
    let screen_y = top_left.y;

    Some((top_left.x, screen_y, width, height))
}

/// Settings for the trackname overlay
#[derive(Clone)]
pub struct TracknameSettings {
    /// Horizontal offset from left edge
    pub horizontal_offset: f64,
    /// Vertical offset within track
    pub vertical_offset: f64,
    /// Text opacity (0.0 - 1.0)
    pub text_opacity: f32,
    /// Show label background
    pub show_label_bg: bool,
    /// Label background opacity
    pub label_bg_opacity: f32,
    /// Font size
    pub font_size: f32,
    /// Show parent/folder tracks
    pub show_parent_tracks: bool,
    /// Show child tracks
    pub show_child_tracks: bool,
    /// Use track color for label
    pub use_track_color: bool,
}

impl Default for TracknameSettings {
    fn default() -> Self {
        Self {
            horizontal_offset: 10.0,
            vertical_offset: 2.0,
            text_opacity: 1.0,
            show_label_bg: true,
            label_bg_opacity: 0.7,
            font_size: 13.0,
            show_parent_tracks: true,
            show_child_tracks: true,
            use_track_color: true,
        }
    }
}

/// Information about a visible track
struct TrackInfo {
    name: String,
    y: f64,
    height: f64,
    color: Option<u32>,
    is_folder: bool,
    depth: i32,
}

/// Get visible tracks and their positions
fn get_visible_tracks(scroll_y: f64, view_height: f64, scale: f64) -> Vec<TrackInfo> {
    let reaper = Reaper::get();
    let project = reaper.current_project();
    let track_count = project.track_count();

    let mut tracks = Vec::new();
    let view_bottom = scroll_y + view_height;

    for i in 0..track_count {
        let Some(track) = project.track_by_index(i) else {
            continue;
        };

        let Ok(medium_track) = track.raw() else {
            continue;
        };

        // Get track Y position and height
        // SAFETY: We have a valid track pointer from the current project
        let track_y = unsafe {
            reaper
                .medium_reaper()
                .get_media_track_info_value(medium_track, TrackAttributeKey::TcpY)
        } / scale;
        let track_height = unsafe {
            reaper
                .medium_reaper()
                .get_media_track_info_value(medium_track, TrackAttributeKey::TcpH)
        } / scale;

        let track_bottom = track_y + track_height;

        // Skip tracks outside visible area
        if track_bottom < scroll_y || track_y > view_bottom {
            continue;
        }

        // Skip tracks that are too small
        if track_height < 10.0 {
            continue;
        }

        // Get track name
        let name = track.name().map(|n| n.to_string()).unwrap_or_default();

        // Get track color (REAPER returns BGR format)
        let color = {
            let raw_color = unsafe {
                reaper.medium_reaper().low().GetTrackColor(medium_track.as_ptr())
            };
            if raw_color != 0 {
                Some(raw_color as u32)
            } else {
                None
            }
        };

        // Check if folder
        // SAFETY: We have a valid track pointer
        let folder_depth = unsafe {
            reaper
                .medium_reaper()
                .get_media_track_info_value(medium_track, TrackAttributeKey::FolderDepth)
        } as i32;

        tracks.push(TrackInfo {
            name,
            y: track_y - scroll_y,
            height: track_height,
            color,
            is_folder: folder_depth > 0,
            depth: folder_depth,
        });
    }

    tracks
}

/// Convert REAPER color (BGR) to Vello Color
fn reaper_color_to_vello(reaper_color: u32, alpha: f32) -> Color {
    let r = (reaper_color & 0xFF) as u8;
    let g = ((reaper_color >> 8) & 0xFF) as u8;
    let b = ((reaper_color >> 16) & 0xFF) as u8;
    Color::from_rgba8(r, g, b, (alpha * 255.0) as u8)
}

/// Text renderer using Vello's draw_glyphs API
struct VelloTextRenderer {
    font_data: FontData,
}

impl VelloTextRenderer {
    fn new() -> Self {
        let font_data = FontData::new(Blob::new(Arc::new(ARIAL_FONT_DATA.to_vec())), 0);
        Self { font_data }
    }

    /// Convert FontData to FontRef for metrics calculations
    fn font_ref(&self) -> Option<FontRef<'_>> {
        let file_ref = FileRef::new(self.font_data.data.as_ref()).ok()?;
        match file_ref {
            FileRef::Font(font) => Some(font),
            FileRef::Collection(collection) => collection.get(self.font_data.index).ok(),
        }
    }

    /// Measure text width using actual font metrics
    fn measure_text(&self, text: &str, font_size: f32) -> f64 {
        if let Some(font_ref) = self.font_ref() {
            let size = skrifa::instance::Size::new(font_size);
            let charmap = font_ref.charmap();
            let glyph_metrics = font_ref.glyph_metrics(size, LocationRef::default());

            let mut width = 0.0f32;
            for ch in text.chars() {
                let gid = charmap.map(ch).unwrap_or_default();
                let advance = glyph_metrics.advance_width(gid).unwrap_or_default();
                width += advance;
            }
            width as f64
        } else {
            // Fallback approximation
            text.len() as f64 * font_size as f64 * 0.6
        }
    }

    /// Draw text at the specified position
    fn draw_text(
        &self,
        scene: &mut Scene,
        text: &str,
        x: f64,
        y: f64,
        font_size: f32,
        color: Color,
    ) {
        if let Some(font_ref) = self.font_ref() {
            let size = skrifa::instance::Size::new(font_size);
            let charmap = font_ref.charmap();
            let glyph_metrics = font_ref.glyph_metrics(size, LocationRef::default());

            // Build glyph list with positions
            let mut glyphs = Vec::new();
            let mut pen_x = 0.0f32;

            for ch in text.chars() {
                if ch == '\n' {
                    continue;
                }
                let gid = charmap.map(ch).unwrap_or_default();
                let advance = glyph_metrics.advance_width(gid).unwrap_or_default();

                glyphs.push(Glyph {
                    id: gid.to_u32(),
                    x: pen_x,
                    y: 0.0,
                });

                pen_x += advance;
            }

            let text_transform = Affine::translate((x, y));

            scene
                .draw_glyphs(&self.font_data)
                .font_size(font_size)
                .transform(text_transform)
                .brush(color)
                .draw(Fill::NonZero, glyphs.into_iter());
        } else {
            // Fallback: draw placeholder rectangles
            let char_width = font_size * 0.55;
            let char_height = font_size * 0.8;
            let char_spacing = font_size * 0.1;
            let mut current_x = x;

            for ch in text.chars() {
                if ch == ' ' {
                    current_x += char_width as f64 * 0.5;
                    continue;
                }

                let char_rect = Rect::new(
                    current_x,
                    y - char_height as f64,
                    current_x + char_width as f64 * 0.8,
                    y,
                );
                scene.fill(Fill::NonZero, Affine::IDENTITY, color, None, &char_rect);
                current_x += (char_width + char_spacing) as f64;
            }
        }
    }
}

/// Render track names onto a Vello scene
fn render_tracknames(
    scene: &mut Scene,
    width: u32,
    height: u32,
    settings: &TracknameSettings,
    text_renderer: &VelloTextRenderer,
) {
    // Get DPI scale
    let scale = 1.0; // TODO: Get actual DPI scale from REAPER

    // Get scroll position - for now use 0, would need JS_Window API
    let scroll_y = 0.0;
    let view_height = height as f64;

    // Get visible tracks
    let tracks = get_visible_tracks(scroll_y, view_height, scale);

    for track in &tracks {
        if track.name.is_empty() {
            continue;
        }

        // Calculate label position
        let x = settings.horizontal_offset;
        let y = track.y + settings.vertical_offset;

        // Skip if outside view
        if y < 0.0 || y > height as f64 {
            continue;
        }

        // Determine colors
        let (text_color, bg_color) = if settings.use_track_color {
            if let Some(color) = track.color {
                let _base = reaper_color_to_vello(color, 1.0);
                let bg = reaper_color_to_vello(color, settings.label_bg_opacity);
                (Color::WHITE, bg)
            } else {
                (
                    Color::from_rgba8(220, 220, 220, (settings.text_opacity * 255.0) as u8),
                    Color::from_rgba8(40, 40, 50, (settings.label_bg_opacity * 255.0) as u8),
                )
            }
        } else {
            (
                Color::from_rgba8(220, 220, 220, (settings.text_opacity * 255.0) as u8),
                Color::from_rgba8(40, 40, 50, (settings.label_bg_opacity * 255.0) as u8),
            )
        };

        // Measure text width using actual font metrics
        let text_width = text_renderer.measure_text(&track.name, settings.font_size);
        let padding = 6.0;
        let label_height = settings.font_size as f64 + padding * 2.0;

        // Center vertically in track
        let label_y = y + (track.height - label_height) / 2.0;

        // Draw label background
        if settings.show_label_bg {
            let label_rect = RoundedRect::new(
                x - padding,
                label_y,
                x + text_width + padding,
                label_y + label_height,
                3.0,
            );
            scene.fill(Fill::NonZero, Affine::IDENTITY, bg_color, None, &label_rect);
        }

        // Draw text using Vello
        text_renderer.draw_text(
            scene,
            &track.name,
            x,
            label_y + padding + settings.font_size as f64 * 0.8,
            settings.font_size,
            text_color,
        );
    }
}

// Type alias for the window
type TracknameWindow = TransparentWindow<VelloEmbedSource<Box<dyn FnMut(&mut Scene, u32, u32) + Send>>>;

thread_local! {
    static TRACKNAME_WINDOW: RefCell<Option<TracknameWindow>> = const { RefCell::new(None) };
    static SETTINGS: RefCell<TracknameSettings> = RefCell::new(TracknameSettings::default());
}

/// Open the trackname overlay
fn open_trackname_overlay() {
    info!("Opening trackname overlay...");

    // Get arrange view bounds
    let (x, y, width, height) = match get_arrange_bounds() {
        Some(bounds) => bounds,
        None => {
            warn!("Could not get arrange view bounds, using fallback position");
            (300, 200, 800u32, 500u32)
        }
    };

    let settings = SETTINGS.with(|s| s.borrow().clone());
    let text_renderer = VelloTextRenderer::new();

    let builder: Box<dyn FnMut(&mut Scene, u32, u32) + Send> = Box::new(move |scene, w, h| {
        render_tracknames(scene, w, h, &settings, &text_renderer);
    });

    let source = VelloEmbedSource::new(builder);

    match TransparentWindow::open(x, y, width, height, source) {
        Ok(mut window) => {
            window.show();

            if let Err(e) = window.render() {
                warn!("Initial render failed: {}", e);
            }

            TRACKNAME_WINDOW.with(|cell| {
                if let Some(mut old) = cell.borrow_mut().take() {
                    old.close();
                }
                *cell.borrow_mut() = Some(window);
            });

            info!("Trackname overlay opened");
        }
        Err(e) => {
            warn!("Failed to open trackname overlay: {}", e);
        }
    }
}

/// Close the trackname overlay
fn close_trackname_overlay() {
    TRACKNAME_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close();
            info!("Trackname overlay closed");
        }
    });
}

/// Toggle the trackname overlay
fn toggle_trackname_overlay() {
    TRACKNAME_WINDOW.with(|cell| {
        let has_window = cell.borrow().is_some();
        if has_window {
            close_trackname_overlay();
        } else {
            open_trackname_overlay();
        }
    });
}

/// Refresh the overlay (call from timer)
/// Also syncs window position with arrange view if it has moved/resized
pub fn refresh_overlay() {
    TRACKNAME_WINDOW.with(|cell| {
        if let Some(ref mut window) = *cell.borrow_mut() {
            // Sync bounds with arrange view
            if let Some((x, y, width, height)) = get_arrange_bounds() {
                let current = window.bounds();
                // Only update if changed
                if current.x != x || current.y != y || current.width != width || current.height != height {
                    window.set_frame(x, y, width, height);
                }
            }

            // Render
            if let Err(e) = window.render() {
                debug!("Overlay render error: {}", e);
            }
        }
    });
}

/// Check if the overlay is currently open
pub fn is_overlay_open() -> bool {
    TRACKNAME_WINDOW.with(|cell| cell.borrow().is_some())
}

/// Get action definitions
pub fn actions() -> Vec<ActionDef> {
    vec![
        ActionDef {
            command_id: "FTS_TEST_WINDOW_TRACKNAME_TOGGLE",
            display_name: "Toggle Trackname Overlay".to_string(),
            handler: toggle_trackname_overlay,
            appears_in_menu: true,
            section: ActionSection::Main,
            toggle_state: Some(is_overlay_open),
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_TEST_WINDOW_TRACKNAME_OPEN",
            display_name: "Open Trackname Overlay".to_string(),
            handler: open_trackname_overlay,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_TEST_WINDOW_TRACKNAME_CLOSE",
            display_name: "Close Trackname Overlay".to_string(),
            handler: close_trackname_overlay,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}
