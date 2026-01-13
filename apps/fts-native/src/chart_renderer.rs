//! Chart Renderer - WGPU CustomPaintSource for chart visualization
//!
//! Renders a keyflow Chart using WGPU with sheet music style layout,
//! including SMuFL music notation symbols.
//!
//! Uses shared rendering primitives from engraver for 1:1 rendering
//! consistency with the music_symbols example.

use anyrender_vello::{CustomPaintCtx, CustomPaintSource, TextureHandle};
use engraver::fonts::{get_glyph_id, tessellate_glyph_to_ndc, Glyph, GlyphVertex, SMuFLMetadata};
use engraver::model::{
    compute_page_layout, compute_system_layout, ComputedHeaderLayout, HeaderFrameConfig,
    HeaderStyles, HeaderTextAlign, LineBreakPolicy, PageStyle, ScoreHeader,
};
use engraver::style::{MStyle, Sid};
use engraver::renderer::{
    create_blit_pipeline, create_blit_texture_bind_group_layout, create_camera_bind_group_layout,
    create_fullscreen_quad, create_line, create_main_pipeline, create_rect, create_sdf_pipeline,
    create_sdf_rounded_rect, BlitVertex, CameraUniform, SdfRectVertex, Vertex,
};
use engraver::ui::{format_rehearsal_label, CapsuleLabelConfig, CapsuleLabelMode, ComputedCapsuleLabel};
use glyphon::{
    Attrs, Buffer as TextBuffer, Cache as TextCache, Color as TextColor, Family, FontSystem,
    Metrics, Resolution, Shaping, SwashCache, TextArea, TextAtlas, TextBounds, TextRenderer,
    Viewport, Weight,
};
use keyflow::Chart;
use skrifa::FontRef;
use std::collections::VecDeque;
use std::io::Cursor;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::time::Instant;
use wgpu::util::DeviceExt;
use wgpu_context::DeviceHandle;

// ============================================================================
// Embedded Font Resources
// ============================================================================

/// Leland font embedded at compile time (works with dx serve and cargo run)
static LELAND_FONT_DATA: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../libs/reference/sheet-music/musescore/fonts/leland/Leland.otf"
));

/// Leland Text font for chord symbols (has music notation characters)
static LELAND_TEXT_FONT_DATA: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../libs/reference/sheet-music/musescore/fonts/leland/LelandText.otf"
));

/// Leland metadata embedded at compile time
static LELAND_METADATA_JSON: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../libs/reference/sheet-music/fonts/leland/leland_metadata.json"
));

// ============================================================================
// Page Layout Constants
// ============================================================================

/// Screen DPI for rendering (used to convert points to pixels)
const SCREEN_DPI: f32 = 96.0;

/// Points to pixels conversion factor at screen DPI
const PT_TO_PX: f32 = SCREEN_DPI / 72.0;

// ============================================================================
// MuseScore-Style Chord Symbol Rendering System
// ============================================================================

/// Parsed components of a chord symbol for rendering
/// Based on MuseScore's ParsedChord system
#[derive(Debug, Clone)]
struct ChordSymbolComponents {
    /// Root note (e.g., "C", "F#", "Bb")
    root: String,
    /// Root accidental if any (separate for special rendering)
    root_accidental: Option<ChordAccidental>,
    /// Quality modifier (e.g., "m", "dim", "aug", "")
    quality: String,
    /// Main extension (e.g., "7", "9", "11", "13", "6", "")
    extension: String,
    /// Additional alterations (e.g., "b5", "#9", "add9")
    alterations: Vec<String>,
    /// Bass note if slash chord (e.g., "E" in "C/E")
    bass: Option<String>,
    /// Bass accidental if any
    bass_accidental: Option<ChordAccidental>,
}

/// Accidental types for special symbol rendering
#[derive(Debug, Clone, Copy, PartialEq)]
enum ChordAccidental {
    Sharp,
    Flat,
    Natural,
}

/// Render action for chord symbol layout
/// Based on MuseScore's RenderAction enum
#[derive(Debug, Clone)]
enum ChordRenderAction {
    /// Set current text with font size multiplier
    Text { text: String, scale: f32 },
    /// Move position (in units relative to font size)
    Move { dx: f32, dy: f32 },
    /// Move up for superscript (based on cap-height)
    SuperScript,
    /// Move back to baseline
    BaseLine,
    /// Render a special symbol (triangle, circle, etc.)
    Symbol { symbol: ChordSymbol },
}

/// Special chord symbols that need glyph rendering
#[derive(Debug, Clone, Copy)]
enum ChordSymbol {
    MajorTriangle,    // Δ
    Diminished,       // °
    HalfDiminished,   // ø
    Augmented,        // +
    Sharp,            // ♯
    Flat,             // ♭
    Natural,          // ♮
}

impl ChordSymbol {
    fn to_char(&self) -> char {
        match self {
            Self::MajorTriangle => 'Δ',
            Self::Diminished => '°',
            Self::HalfDiminished => 'ø',
            Self::Augmented => '+',
            Self::Sharp => '♯',
            Self::Flat => '♭',
            Self::Natural => '♮',
        }
    }
}

impl ChordSymbolComponents {
    /// Parse a chord symbol string into components
    fn parse(symbol: &str) -> Self {
        let mut root = String::new();
        let mut root_accidental = None;
        let mut quality = String::new();
        let mut extension = String::new();
        let mut alterations = Vec::new();
        let mut bass = None;
        let mut bass_accidental = None;

        // Check for slash chord first
        let (main_part, bass_part) = if let Some(slash_idx) = symbol.rfind('/') {
            let bass_str = &symbol[slash_idx + 1..];
            if !bass_str.is_empty() && bass_str.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false) {
                // Parse bass note and accidental
                let mut bass_chars = bass_str.chars();
                let bass_note = bass_chars.next().unwrap().to_string();
                let remaining: String = bass_chars.collect();

                if remaining.starts_with('#') {
                    bass_accidental = Some(ChordAccidental::Sharp);
                    bass = Some(format!("{}{}", bass_note, &remaining[1..]));
                } else if remaining.starts_with('b') && remaining.len() > 1 && remaining.chars().nth(1).map(|c| !c.is_ascii_digit()).unwrap_or(true) {
                    bass_accidental = Some(ChordAccidental::Flat);
                    bass = Some(format!("{}{}", bass_note, &remaining[1..]));
                } else {
                    bass = Some(format!("{}{}", bass_note, remaining));
                }

                (&symbol[..slash_idx], bass.as_deref())
            } else {
                (symbol, None)
            }
        } else {
            (symbol, None)
        };

        // Parse root note (first letter + optional accidental)
        let chars: Vec<char> = main_part.chars().collect();
        if chars.is_empty() {
            return Self {
                root: String::new(),
                root_accidental: None,
                quality: String::new(),
                extension: String::new(),
                alterations: Vec::new(),
                bass: bass.map(|s| s.to_string()),
                bass_accidental,
            };
        }

        // Root is first uppercase letter
        root.push(chars[0]);
        let mut pos = 1;

        // Check for root accidental
        if pos < chars.len() {
            if chars[pos] == '#' {
                root_accidental = Some(ChordAccidental::Sharp);
                pos += 1;
            } else if chars[pos] == 'b' && (pos + 1 >= chars.len() || !chars[pos + 1].is_ascii_digit()) {
                root_accidental = Some(ChordAccidental::Flat);
                pos += 1;
            }
        }

        // Remaining string after root
        let remainder: String = chars[pos..].iter().collect();

        // Parse quality and extension
        // Order matters: check longer patterns first
        let quality_patterns = [
            ("maj7", "", "Δ7"),
            ("maj9", "", "Δ9"),
            ("maj11", "", "Δ11"),
            ("maj13", "", "Δ13"),
            ("m7b5", "ø", "7"),
            ("m7♭5", "ø", "7"),
            ("dim7", "°", "7"),
            ("dim", "°", ""),
            ("aug7", "+", "7"),
            ("aug", "+", ""),
            ("m7", "m", "7"),
            ("m9", "m", "9"),
            ("m11", "m", "11"),
            ("m6", "m", "6"),
            ("mi7", "m", "7"),
            ("mi", "m", ""),
            ("min7", "m", "7"),
            ("min", "m", ""),
            ("7", "", "7"),
            ("9", "", "9"),
            ("11", "", "11"),
            ("13", "", "13"),
            ("6", "", "6"),
            ("sus4", "sus", "4"),
            ("sus2", "sus", "2"),
            ("sus", "sus", ""),
            ("add9", "", "add9"),
            ("add2", "", "add2"),
            ("m", "m", ""),
            ("M7", "", "Δ7"),
            ("M9", "", "Δ9"),
        ];

        let mut found = false;
        for (pattern, qual, ext) in quality_patterns {
            if remainder.starts_with(pattern) {
                quality = qual.to_string();
                extension = ext.to_string();

                // Get remaining alterations
                let after = &remainder[pattern.len()..];
                if !after.is_empty() {
                    // Parse alterations like b5, #9, etc.
                    let mut alt_str = after.to_string();

                    // Split on common alteration patterns
                    while !alt_str.is_empty() {
                        let mut found_alt = false;
                        for alt_pattern in ["b5", "♭5", "#5", "♯5", "b9", "♭9", "#9", "♯9", "#11", "♯11", "b13", "♭13", "add9", "add2", "no3", "no5"] {
                            if alt_str.starts_with(alt_pattern) {
                                alterations.push(alt_pattern.to_string());
                                alt_str = alt_str[alt_pattern.len()..].to_string();
                                found_alt = true;
                                break;
                            }
                        }
                        if !found_alt {
                            // Just add remaining as single alteration
                            if !alt_str.is_empty() {
                                alterations.push(alt_str.clone());
                            }
                            break;
                        }
                    }
                }
                found = true;
                break;
            }
        }

        // If no pattern matched, treat remainder as extension/alterations
        if !found && !remainder.is_empty() {
            extension = remainder;
        }

        Self {
            root,
            root_accidental,
            quality,
            extension,
            alterations,
            bass: bass.map(|s| s.to_string()),
            bass_accidental,
        }
    }

    /// Generate render actions for this chord symbol
    fn to_render_actions(&self) -> Vec<ChordRenderAction> {
        let mut actions = Vec::new();

        // Root note (full size)
        if !self.root.is_empty() {
            actions.push(ChordRenderAction::Text {
                text: self.root.clone(),
                scale: 1.0,
            });
        }

        // Root accidental (slightly smaller, but at baseline)
        if let Some(acc) = &self.root_accidental {
            let symbol = match acc {
                ChordAccidental::Sharp => ChordSymbol::Sharp,
                ChordAccidental::Flat => ChordSymbol::Flat,
                ChordAccidental::Natural => ChordSymbol::Natural,
            };
            actions.push(ChordRenderAction::Symbol { symbol });
        }

        // Quality (varies by type)
        if !self.quality.is_empty() {
            // Check for special symbols
            match self.quality.as_str() {
                "Δ" => actions.push(ChordRenderAction::Symbol { symbol: ChordSymbol::MajorTriangle }),
                "°" => {
                    actions.push(ChordRenderAction::SuperScript);
                    actions.push(ChordRenderAction::Symbol { symbol: ChordSymbol::Diminished });
                    actions.push(ChordRenderAction::BaseLine);
                }
                "ø" => {
                    actions.push(ChordRenderAction::SuperScript);
                    actions.push(ChordRenderAction::Symbol { symbol: ChordSymbol::HalfDiminished });
                    actions.push(ChordRenderAction::BaseLine);
                }
                "+" => actions.push(ChordRenderAction::Symbol { symbol: ChordSymbol::Augmented }),
                _ => {
                    // Regular text quality (like "m", "sus")
                    actions.push(ChordRenderAction::Text {
                        text: self.quality.clone(),
                        scale: 1.0,
                    });
                }
            }
        }

        // Extension (superscript, smaller)
        if !self.extension.is_empty() {
            // Check if extension starts with special symbol
            if self.extension.starts_with('Δ') {
                actions.push(ChordRenderAction::Symbol { symbol: ChordSymbol::MajorTriangle });
                if self.extension.len() > 1 {
                    actions.push(ChordRenderAction::SuperScript);
                    actions.push(ChordRenderAction::Text {
                        text: self.extension[2..].to_string(), // Skip Δ (2 bytes in UTF-8... actually 2 for Δ)
                        scale: 0.75,
                    });
                    actions.push(ChordRenderAction::BaseLine);
                }
            } else if self.extension.starts_with("add") {
                // "add" stays at baseline
                actions.push(ChordRenderAction::Text {
                    text: "add".to_string(),
                    scale: 0.75,
                });
                actions.push(ChordRenderAction::SuperScript);
                actions.push(ChordRenderAction::Text {
                    text: self.extension[3..].to_string(),
                    scale: 0.75,
                });
                actions.push(ChordRenderAction::BaseLine);
            } else {
                // Regular extension number - superscript
                actions.push(ChordRenderAction::SuperScript);
                actions.push(ChordRenderAction::Text {
                    text: self.extension.clone(),
                    scale: 0.75,
                });
                actions.push(ChordRenderAction::BaseLine);
            }
        }

        // Alterations (smaller, in parentheses or superscript)
        for alt in &self.alterations {
            actions.push(ChordRenderAction::SuperScript);

            // Convert alterations to symbols
            let formatted = alt
                .replace("b5", "♭5")
                .replace("b9", "♭9")
                .replace("b13", "♭13")
                .replace("#5", "♯5")
                .replace("#9", "♯9")
                .replace("#11", "♯11");

            actions.push(ChordRenderAction::Text {
                text: formatted,
                scale: 0.65,
            });
            actions.push(ChordRenderAction::BaseLine);
        }

        // Bass note (slash chord)
        if let Some(ref bass) = self.bass {
            actions.push(ChordRenderAction::Text {
                text: "/".to_string(),
                scale: 1.0,
            });
            actions.push(ChordRenderAction::Text {
                text: bass.clone(),
                scale: 1.0,
            });

            if let Some(acc) = &self.bass_accidental {
                let symbol = match acc {
                    ChordAccidental::Sharp => ChordSymbol::Sharp,
                    ChordAccidental::Flat => ChordSymbol::Flat,
                    ChordAccidental::Natural => ChordSymbol::Natural,
                };
                actions.push(ChordRenderAction::Symbol { symbol });
            }
        }

        actions
    }
}

// ============================================================================
// MuseScore-Accurate Chord Symbol Rendering
// Based on harmonylayout.cpp render system
// ============================================================================

/// Font metrics estimation for chord rendering
/// Uses MuseScore's exact values from textbase.h and chords_std.xml
#[derive(Debug, Clone, Copy)]
struct ChordFontMetrics {
    /// Font size in pixels
    font_size: f32,
    /// x-height: height of lowercase 'x' (~0.52 of font size for most fonts)
    x_height: f32,
    /// cap-height: height of capital letters (~0.72 of font size)
    cap_height: f32,
    /// ascent: distance from baseline to top
    ascent: f32,
    /// descent: distance from baseline to bottom (positive value)
    descent: f32,
    /// MuseScore "super" entity: 0.36 cap-height for chord superscripts
    super_offset: f32,
}

impl ChordFontMetrics {
    fn new(font_size: f32) -> Self {
        // MuseScore values from textbase.h and chords_std.xml
        let cap_height = font_size * 0.72;
        Self {
            font_size,
            x_height: font_size * 0.52,
            cap_height,
            ascent: font_size * 0.85,
            descent: font_size * 0.20,
            // MuseScore "super" entity from chords_std.xml: 0.36 cap-height
            super_offset: cap_height * 0.36,
        }
    }

    fn scaled(&self, scale: f32) -> Self {
        Self::new(self.font_size * scale)
    }
}

/// Kerning pairs for chord symbols
/// Values from MuseScore chords_std.xml and harmonylayout.cpp
/// Units are cap-height fractions (multiply by cap_height to get pixels)
fn get_kerning(prev_char: char, next_char: char, is_jazz: bool) -> f32 {
    match (prev_char, next_char) {
        // MuseScore renderRoot: ":n :a m:0.036:0" - spacing after accidental
        ('A'..='G', '♯' | '♭' | '♮' | '#' | 'b') => 0.036,

        // Standard kerning for special symbols
        ('A'..='G', '°') => -0.3,        // Root to diminished (tuck in)
        ('A'..='G', 'ø') => -0.25,       // Root to half-diminished
        ('♯' | '♭', '°' | 'ø') => -0.2,  // Accidental to dim symbols
        ('Δ', '°') => -0.3,              // Triangle to diminished
        ('Δ', 'ø') => -0.25,             // Triangle to half-diminished

        // MuseScore renderBass: "m:-0.014:0 / m:0.014:0 :n :a"
        (_, '/') => -0.014,              // Before slash
        ('/', _) => 0.014,               // After slash

        // Jazz preset kerning (tighter overall)
        (_, _) if is_jazz => {
            match (prev_char, next_char) {
                ('♭' | '♯' | '♮', _) => -0.1,  // Tighter after accidentals
                (_, '7' | '9' | '6') => -0.05, // Tighter before numbers
                _ => 0.0,
            }
        }

        _ => 0.0,
    }
}

/// Chord rendering style constants from MuseScore styledef.cpp
#[derive(Debug, Clone)]
struct ChordRenderStyle {
    /// Scale factor for extensions (e.g., "7", "9")
    /// MuseScore default: 1.0 (configurable via chordExtensionMag)
    pub extension_mag: f32,
    /// Scale factor for modifiers (e.g., "b5", "#11")
    /// MuseScore default: 1.0 (configurable via chordModifierMag)
    pub modifier_mag: f32,
    /// Scale factor for stacked/superscript modifiers
    /// MuseScore default: 0.75 (chordStackedModifierMag)
    pub stacked_modifier_mag: f32,
    /// Scale factor for bass note
    /// MuseScore default: 1.0 (chordBassNoteScale)
    pub bass_note_scale: f32,
    /// Whether to use jazz-style kerning (tighter)
    pub jazz_style: bool,
}

impl Default for ChordRenderStyle {
    fn default() -> Self {
        Self {
            extension_mag: 1.0,
            modifier_mag: 1.0,
            stacked_modifier_mag: 0.75,  // MuseScore default
            bass_note_scale: 1.0,
            jazz_style: false,
        }
    }
}

/// Render context for chord symbol layout (like MuseScore's HarmonyRenderCtx)
#[derive(Debug, Clone)]
struct ChordRenderContext {
    /// Current X position in pixels
    x: f32,
    /// Current Y position in pixels (positive = down)
    y: f32,
    /// Current scale factor
    scale: f32,
    /// Position stack for PUSH/POP
    stack: Vec<(f32, f32)>,
    /// Base font metrics
    base_metrics: ChordFontMetrics,
    /// Current font metrics (scaled)
    metrics: ChordFontMetrics,
}

impl ChordRenderContext {
    fn new(base_font_size: f32) -> Self {
        let metrics = ChordFontMetrics::new(base_font_size);
        Self {
            x: 0.0,
            y: 0.0,
            scale: 1.0,
            stack: Vec::new(),
            base_metrics: metrics,
            metrics,
        }
    }

    fn push(&mut self) {
        self.stack.push((self.x, self.y));
    }

    fn pop(&mut self) {
        if let Some((x, y)) = self.stack.pop() {
            self.x = x;
            self.y = y;
        }
    }

    fn pop_x(&mut self) {
        if let Some((x, _)) = self.stack.pop() {
            self.x = x;
        }
    }

    fn set_scale(&mut self, new_scale: f32) {
        self.scale = new_scale;
        self.metrics = self.base_metrics.scaled(new_scale);
    }

    fn multiply_scale(&mut self, factor: f32) {
        self.set_scale(self.scale * factor);
    }

    /// Move by x-height (MuseScore MOVEXHEIGHT action)
    fn move_x_height(&mut self, up: bool, scaled: bool) {
        let x_height = if scaled { self.metrics.x_height } else { self.base_metrics.x_height };
        self.y += if up { -x_height } else { x_height };
    }

    /// Move to superscript position using MuseScore's "super" entity (0.36 cap-height)
    /// This is the standard chord symbol superscript positioning from chords_std.xml
    fn move_super(&mut self, up: bool, scaled: bool) {
        let super_offset = if scaled { self.metrics.super_offset } else { self.base_metrics.super_offset };
        self.y += if up { -super_offset } else { super_offset };
    }

    /// Move by cap-height units (MuseScore MOVE action)
    fn move_cap_height(&mut self, dx: f32, dy: f32, scaled: bool) {
        let cap = if scaled { self.metrics.cap_height } else { self.base_metrics.cap_height };
        self.x += dx * cap;
        self.y += dy * cap;
    }

    /// Advance X by text width
    fn advance_x(&mut self, width: f32) {
        self.x += width;
    }

    /// Apply kerning between characters
    fn apply_kerning(&mut self, prev: char, next: char, jazz: bool) {
        let kern = get_kerning(prev, next, jazz);
        if kern != 0.0 {
            self.x += kern * self.metrics.cap_height * self.scale;
        }
    }
}

/// A rendered chord segment with precise positioning
#[derive(Debug, Clone)]
struct ChordRenderSegment {
    /// Text to render
    pub text: String,
    /// X offset from chord origin
    pub x: f32,
    /// Y offset from chord origin (negative = up)
    pub y: f32,
    /// Font scale factor
    pub scale: f32,
    /// Font size in pixels
    pub font_size: f32,
}

/// Chord symbol renderer with MuseScore-accurate positioning
struct ChordSymbolRenderer {
    base_font_size: f32,
    style: ChordRenderStyle,
}

impl ChordSymbolRenderer {
    fn new(base_font_size: f32) -> Self {
        Self {
            base_font_size,
            style: ChordRenderStyle::default(),
        }
    }

    fn with_style(base_font_size: f32, style: ChordRenderStyle) -> Self {
        Self {
            base_font_size,
            style,
        }
    }

    /// Render a chord symbol to positioned segments
    /// This is the main rendering function that produces precise positioning
    fn render(&self, symbol: &str) -> Vec<ChordRenderSegment> {
        let components = ChordSymbolComponents::parse(symbol);
        let mut ctx = ChordRenderContext::new(self.base_font_size);
        let mut segments = Vec::new();

        let mut prev_char: Option<char> = None;

        // 1. Render root note (full size)
        if !components.root.is_empty() {
            let text = components.root.clone();
            segments.push(ChordRenderSegment {
                text: text.clone(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale,
                font_size: ctx.metrics.font_size,
            });
            // Estimate width and advance
            let width = self.estimate_text_width(&text, ctx.metrics.font_size);
            ctx.advance_x(width);
            prev_char = text.chars().last();
        }

        // 2. Render root accidental
        if let Some(acc) = &components.root_accidental {
            let text = match acc {
                ChordAccidental::Sharp => "♯".to_string(),
                ChordAccidental::Flat => "♭".to_string(),
                ChordAccidental::Natural => "♮".to_string(),
            };

            // Apply kerning from root to accidental
            if let Some(pc) = prev_char {
                ctx.apply_kerning(pc, text.chars().next().unwrap_or(' '), self.style.jazz_style);
            }

            segments.push(ChordRenderSegment {
                text: text.clone(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale,
                font_size: ctx.metrics.font_size,
            });
            let width = self.estimate_text_width(&text, ctx.metrics.font_size);
            ctx.advance_x(width);
            prev_char = text.chars().last();
        }

        // 3. Render quality (m, dim symbol, etc.)
        if !components.quality.is_empty() {
            let text = match components.quality.as_str() {
                "°" | "ø" => {
                    // Diminished/half-dim symbols are superscript (MuseScore "super" positioning)
                    ctx.move_super(true, false);
                    components.quality.clone()
                }
                _ => components.quality.clone(),
            };

            // Apply kerning
            if let Some(pc) = prev_char {
                ctx.apply_kerning(pc, text.chars().next().unwrap_or(' '), self.style.jazz_style);
            }

            segments.push(ChordRenderSegment {
                text: text.clone(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale,
                font_size: ctx.metrics.font_size,
            });
            let width = self.estimate_text_width(&text, ctx.metrics.font_size);
            ctx.advance_x(width);

            // Return to baseline if we moved up
            if components.quality == "°" || components.quality == "ø" {
                ctx.move_super(false, false);
            }
            prev_char = text.chars().last();
        }

        // 4. Render extension (7, 9, 11, 13) - superscript with stacked modifier scale
        if !components.extension.is_empty() {
            // Check for triangle symbol at start
            let (triangle_prefix, remaining) = if components.extension.starts_with('Δ') {
                (Some("Δ"), &components.extension[2..]) // Δ is 2 bytes
            } else {
                (None, components.extension.as_str())
            };

            // Render triangle at baseline
            if let Some(tri) = triangle_prefix {
                if let Some(pc) = prev_char {
                    ctx.apply_kerning(pc, 'Δ', self.style.jazz_style);
                }
                segments.push(ChordRenderSegment {
                    text: tri.to_string(),
                    x: ctx.x,
                    y: ctx.y,
                    scale: ctx.scale,
                    font_size: ctx.metrics.font_size,
                });
                let width = self.estimate_text_width(tri, ctx.metrics.font_size);
                ctx.advance_x(width);
                prev_char = Some('Δ');
            }

            // Render extension number as superscript using MuseScore's "super" positioning
            if !remaining.is_empty() {
                ctx.push();
                ctx.multiply_scale(self.style.stacked_modifier_mag);
                ctx.move_super(true, false); // Move up by BASE super_offset (0.36 cap-height)

                segments.push(ChordRenderSegment {
                    text: remaining.to_string(),
                    x: ctx.x,
                    y: ctx.y,
                    scale: ctx.scale,
                    font_size: ctx.metrics.font_size,
                });
                let width = self.estimate_text_width(remaining, ctx.metrics.font_size);
                ctx.advance_x(width);

                ctx.pop();
                // After pop, advance X to account for rendered text
                ctx.advance_x(width);
                prev_char = remaining.chars().last();
            }
        }

        // 5. Render alterations (b5, #9, etc.) - superscript using MuseScore "super" positioning
        for alt in &components.alterations {
            let formatted = alt
                .replace("b5", "♭5")
                .replace("b9", "♭9")
                .replace("b13", "♭13")
                .replace("#5", "♯5")
                .replace("#9", "♯9")
                .replace("#11", "♯11");

            ctx.push();
            ctx.multiply_scale(self.style.stacked_modifier_mag * 0.9); // Slightly smaller for alterations
            ctx.move_super(true, false); // Use MuseScore's "super" positioning

            segments.push(ChordRenderSegment {
                text: formatted.clone(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale,
                font_size: ctx.metrics.font_size,
            });
            let width = self.estimate_text_width(&formatted, ctx.metrics.font_size);

            ctx.pop();
            ctx.advance_x(width);
            prev_char = formatted.chars().last();
        }

        // 6. Render bass note (slash chord)
        // MuseScore renderBass: "m:-0.014:0 / m:0.014:0 :n :a"
        if let Some(ref bass) = components.bass {
            // MuseScore: small negative horizontal adjustment before slash
            ctx.move_cap_height(-0.014, 0.0, true);

            // Render slash
            segments.push(ChordRenderSegment {
                text: "/".to_string(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale,
                font_size: ctx.metrics.font_size,
            });
            let slash_width = self.estimate_text_width("/", ctx.metrics.font_size);
            ctx.advance_x(slash_width);

            // MuseScore: small positive adjustment after slash
            ctx.move_cap_height(0.014, 0.0, true);

            // Render bass note
            segments.push(ChordRenderSegment {
                text: bass.clone(),
                x: ctx.x,
                y: ctx.y,
                scale: ctx.scale * self.style.bass_note_scale,
                font_size: ctx.metrics.font_size * self.style.bass_note_scale,
            });
        }

        segments
    }

    /// Estimate text width based on character count and font size
    /// Tuned for Leland Text font - values measured from actual font metrics
    fn estimate_text_width(&self, text: &str, font_size: f32) -> f32 {
        let mut width = 0.0;
        for ch in text.chars() {
            // Width ratios tuned for Leland Text music font
            // Specific characters first, then ranges
            let char_width = match ch {
                // Root notes (uppercase)
                'A' | 'G' | 'D' => 0.52,
                'B' | 'E' | 'F' | 'C' => 0.48,
                // Specific lowercase before range
                'm' | 'w' => 0.52,   // Wide lowercase
                'i' | 'l' => 0.25,   // Narrow lowercase
                // Ranges
                'a'..='z' => 0.38,   // Other lowercase
                'A'..='Z' => 0.50,   // Other uppercase
                // Numbers - specific first
                '1' => 0.28,         // Narrow digit
                '7' => 0.42,         // Seven
                '0'..='9' => 0.42,   // Other numbers
                // Special symbols
                'Δ' => 0.52,         // Triangle
                '°' => 0.32,         // Diminished circle - quite small
                'ø' => 0.42,         // Half-diminished
                '♯' | '#' => 0.38,   // Sharp
                '♭' => 0.32,         // Flat (narrower)
                '♮' => 0.32,         // Natural
                '+' => 0.42,         // Augmented
                '/' => 0.22,         // Slash - very narrow
                _ => 0.38,           // Default
            };
            width += char_width * font_size;
        }
        width
    }

    /// Format chord symbol to simple string (for backwards compatibility)
    fn format(&self, symbol: &str) -> String {
        let components = ChordSymbolComponents::parse(symbol);
        let mut result = String::new();

        result.push_str(&components.root);
        if let Some(acc) = &components.root_accidental {
            result.push(match acc {
                ChordAccidental::Sharp => '♯',
                ChordAccidental::Flat => '♭',
                ChordAccidental::Natural => '♮',
            });
        }
        result.push_str(&components.quality);
        result.push_str(&components.extension);
        for alt in &components.alterations {
            result.push_str(&alt
                .replace("b5", "♭5")
                .replace("b9", "♭9")
                .replace("#9", "♯9")
                .replace("#11", "♯11"));
        }
        if let Some(bass) = &components.bass {
            result.push('/');
            result.push_str(bass);
        }

        result
    }

    /// Get segments for legacy rendering (backwards compatible)
    fn get_segments(&self, symbol: &str) -> Vec<ChordSegment> {
        let render_segments = self.render(symbol);

        render_segments.into_iter().map(|seg| ChordSegment {
            text: seg.text,
            scale: seg.scale,
            is_superscript: seg.y < 0.0, // If Y is negative, it's superscript
        }).collect()
    }
}

/// A segment of a chord symbol for rendering (legacy format)
#[derive(Debug, Clone)]
struct ChordSegment {
    text: String,
    scale: f32,
    is_superscript: bool,
}

/// Format chord symbol for display using the new MuseScore-style system
fn format_chord_symbol(symbol: &str) -> String {
    let renderer = ChordSymbolRenderer::new(12.0);
    renderer.format(symbol)
}

// ============================================================================
// MuseScore-Style Chord Symbol Positioning System
// ============================================================================

/// Horizontal alignment for chord symbols (matches MuseScore's AlignH)
#[derive(Debug, Clone, Copy, Default)]
enum ChordHorizontalAlign {
    Left,
    #[default]
    Center,
    Right,
}

impl ChordHorizontalAlign {
    /// Get the multiplier for positioning relative to notehead width
    /// LEFT = 0.0, CENTER = 0.5, RIGHT = 1.0
    fn position_multiplier(&self) -> f32 {
        match self {
            Self::Left => 0.0,
            Self::Center => 0.5,
            Self::Right => 1.0,
        }
    }
}

/// Vertical alignment for chord symbols (matches MuseScore's AlignV)
#[derive(Debug, Clone, Copy, Default)]
enum ChordVerticalAlign {
    /// Align to top of bounding box
    #[default]
    Top,
    /// Align to vertical center
    VCenter,
    /// Align to text baseline
    Baseline,
    /// Align to bottom of bounding box
    Bottom,
}

/// Bounding box for a rendered chord symbol
/// Used for collision detection and alignment
#[derive(Debug, Clone, Default)]
struct ChordBoundingBox {
    /// Left edge relative to chord position
    pub left: f32,
    /// Right edge relative to chord position
    pub right: f32,
    /// Top edge relative to chord position (negative = above)
    pub top: f32,
    /// Bottom edge relative to chord position
    pub bottom: f32,
}

impl ChordBoundingBox {
    fn width(&self) -> f32 {
        self.right - self.left
    }

    fn height(&self) -> f32 {
        self.bottom - self.top
    }

    /// Get the optical center Y for vertical alignment
    /// Based on MuseScore's yOpticalCenter calculation
    fn optical_center_y(&self, align: ChordVerticalAlign, baseline: f32, spatium: f32) -> f32 {
        match align {
            ChordVerticalAlign::Top => 0.5 * self.height(),
            ChordVerticalAlign::VCenter => 0.0,
            ChordVerticalAlign::Bottom => -0.5 * self.height(),
            // MuseScore uses 0.46 * spatium for baseline offset
            ChordVerticalAlign::Baseline => -0.46 * spatium,
        }
    }

    /// Unite this bounding box with another (expand to contain both)
    fn unite(&mut self, other: &Self) {
        self.left = self.left.min(other.left);
        self.right = self.right.max(other.right);
        self.top = self.top.min(other.top);
        self.bottom = self.bottom.max(other.bottom);
    }
}

/// Style settings for chord symbol positioning
/// Based on MuseScore's style constants from styledef.cpp
#[derive(Debug, Clone)]
struct HarmonyStyle {
    /// Offset from attachment point (x, y) in spatiums
    /// MuseScore default: (0.0, -2.5) for above staff
    pub pos_above: (f32, f32),
    /// Offset for below staff placement
    /// MuseScore default: (0.0, 3.5)
    pub pos_below: (f32, f32),
    /// Minimum distance from staff in spatiums
    /// MuseScore default: 0.5
    pub min_distance: f32,
    /// Distance between multiple chord symbols in spatiums
    /// MuseScore default: 0.5
    pub harmony_distance: f32,
    /// Horizontal alignment
    /// MuseScore default: CENTER
    pub horizontal_align: ChordHorizontalAlign,
    /// Vertical alignment
    /// MuseScore default: TOP
    pub vertical_align: ChordVerticalAlign,
    /// Whether to vertically align multiple chord symbols across the system
    /// MuseScore default: true
    pub vertically_align_system: bool,
    /// Minimum horizontal clearance for collision detection (spatiums)
    /// MuseScore: skylineMinHorizontalClearance
    pub min_horizontal_clearance: f32,
}

impl Default for HarmonyStyle {
    fn default() -> Self {
        Self {
            pos_above: (0.0, -2.5),      // MuseScore chordSymbolAPosAbove
            pos_below: (0.0, 3.5),       // MuseScore chordSymbolAPosBelow
            min_distance: 0.5,           // MuseScore minHarmonyDistance
            harmony_distance: 0.5,       // MuseScore harmonyHarmonyDistance
            horizontal_align: ChordHorizontalAlign::Center,
            vertical_align: ChordVerticalAlign::Top,
            vertically_align_system: true,
            min_horizontal_clearance: 0.25,
        }
    }
}

impl HarmonyStyle {
    /// Create HarmonyStyle from MStyle, pulling values from the style system
    #[must_use]
    pub fn from_mstyle(style: &MStyle) -> Self {
        Self {
            // HarmonyPosAbove is a Y offset in spatiums (negative = above staff)
            pos_above: (0.0, style.spatium(Sid::HarmonyPosAbove)),
            pos_below: (0.0, style.spatium(Sid::HarmonyPosBelow)),
            min_distance: style.spatium(Sid::MinHarmonyDistance),
            harmony_distance: style.spatium(Sid::MinHarmonyDistance), // Using same value
            horizontal_align: ChordHorizontalAlign::Center,
            vertical_align: ChordVerticalAlign::Top,
            vertically_align_system: true,
            min_horizontal_clearance: 0.25,
        }
    }
}

/// Calculated position for a chord symbol
#[derive(Debug, Clone)]
struct ChordPosition {
    /// X position in pixels (absolute)
    pub x: f32,
    /// Y position in pixels (absolute)
    pub y: f32,
    /// Beat position within measure (0.0 to beats_per_measure)
    pub beat: f32,
    /// Bounding box for collision detection
    pub bbox: ChordBoundingBox,
    /// Measure index within the system
    pub measure_in_system: usize,
}

/// A chord position with system-level context for alignment
#[derive(Debug, Clone)]
struct SystemChordPosition {
    /// The chord position
    pub pos: ChordPosition,
    /// Index in the original positions array
    pub original_index: usize,
    /// System index this chord belongs to
    pub system_index: usize,
}

/// Align all chord positions within a system to the outermost Y
/// Based on MuseScore's AlignmentLayout::alignItemsForSystem
fn align_chords_in_system(positions: &mut [ChordPosition], staff_space: f32, style: &HarmonyStyle) {
    if positions.is_empty() || !style.vertically_align_system {
        return;
    }

    // Find the outermost (minimum) Y position for above-staff chords
    // MuseScore: for above placement, use minimum Y (furthest from staff)
    let outermost_y = positions
        .iter()
        .map(|p| p.y + p.bbox.optical_center_y(style.vertical_align, 0.0, staff_space))
        .fold(f32::MAX, f32::min);

    // Move all chords to align with the outermost position
    for pos in positions.iter_mut() {
        let current_optical_center = pos.y + pos.bbox.optical_center_y(style.vertical_align, 0.0, staff_space);
        let delta = outermost_y - current_optical_center;
        pos.y += delta;
    }
}

/// Simple collision detection - push overlapping chords up
/// Based on MuseScore's skyline collision detection (simplified)
fn resolve_chord_collisions(positions: &mut [ChordPosition], staff_space: f32, style: &HarmonyStyle) {
    if positions.len() < 2 {
        return;
    }

    let min_clearance = style.min_horizontal_clearance * staff_space;
    let min_distance = style.harmony_distance * staff_space;

    // Sort by X position for collision detection
    // Note: positions are already roughly sorted by beat
    for i in 1..positions.len() {
        let prev = &positions[i - 1];
        let curr = &positions[i];

        // Check if bounding boxes overlap horizontally (with clearance)
        let prev_right = prev.x + prev.bbox.right;
        let curr_left = curr.x + curr.bbox.left;

        if prev_right + min_clearance > curr_left {
            // Horizontal overlap detected - check if we need to adjust
            // For simplicity, we could push the current chord to the right
            // But MuseScore typically pushes up instead for chord symbols

            // Check vertical overlap
            let prev_bottom = prev.y + prev.bbox.bottom;
            let curr_top = curr.y + curr.bbox.top;

            if prev_bottom > curr_top - min_distance {
                // Collision! Push current chord up
                // (MuseScore uses more sophisticated skyline-based approach)
                // For now, just ensure minimum vertical distance
                let overlap = prev_bottom - curr_top + min_distance;
                if overlap > 0.0 {
                    positions[i].y -= overlap;
                }
            }
        }
    }
}

/// Estimate bounding box for a chord symbol based on text
/// This is a simplified estimation - real measurement would use font metrics
fn estimate_chord_bbox(symbol: &str, font_size: f32, staff_space: f32) -> ChordBoundingBox {
    // Estimate width based on character count and average character width
    // Different characters have different widths:
    // - Root notes (A-G): ~0.6 em
    // - Accidentals (#, b): ~0.4 em
    // - Numbers (7, 9, 11): ~0.5 em
    // - Quality (m, dim): ~0.4 em
    let char_count = symbol.chars().count();
    let avg_char_width = font_size * 0.55; // Average character width
    let estimated_width = char_count as f32 * avg_char_width;

    // Height is approximately 1.2 * font_size for line height
    let estimated_height = font_size * 1.2;

    ChordBoundingBox {
        left: 0.0,
        right: estimated_width,
        top: -estimated_height * 0.8, // Most of the text is above baseline
        bottom: estimated_height * 0.2, // Small amount below baseline (descenders)
    }
}

/// Calculate chord symbol positions within a measure
/// Following MuseScore's positioning algorithm from harmonylayout.cpp
fn calculate_chord_positions(
    chords: &[&keyflow::chart::ChordInstance],
    measure_x: f32,
    measure_width: f32,
    measure_in_system: usize,
    staff_y: f32,
    staff_space: f32,
    font_size: f32,
    time_signature: (u8, u8),
    style: &HarmonyStyle,
    music_style: &MStyle,
) -> Vec<ChordPosition> {
    let beats_per_measure = time_signature.0 as f32;

    // Get MuseScore spacing values from style system (in spatiums):
    // - barNoteDistance: space from barline to first note (default: 1.5 sp)
    // - noteBarDistance: space from last note to barline (default: 1.0 sp)
    let bar_note_distance = staff_space * music_style.spatium(Sid::BarNoteDistance);
    let note_bar_distance = staff_space * music_style.spatium(Sid::NoteBarDistance);

    // Calculate the usable width for chord/note placement
    let usable_width = measure_width - bar_note_distance - note_bar_distance;

    // Notehead width approximation (used for alignment offset)
    // MuseScore uses symWidth(noteheadBlack), we approximate
    let notehead_width = staff_space * 1.18; // Standard notehead is ~1.18 spatiums wide

    // Calculate Y position: staff_y + offset in spatiums
    // pos_above.y is negative (-2.5), so this moves UP from staff top
    let chord_y = staff_y + (style.pos_above.1 * staff_space);

    let mut positions = Vec::with_capacity(chords.len());

    for chord in chords {
        // Get beat position within this measure
        // The chord's position.total_duration contains the absolute position
        // We need the beat within this specific measure
        let beat = chord.position.beats() as f32
            + (chord.position.subdivisions() as f32 / 1000.0);

        // Calculate X position based on beat
        // beat 0 = first beat position (after barNoteDistance)
        // beat (beats_per_measure) = position just before noteBarDistance
        let beat_fraction = beat / beats_per_measure;
        let beat_x = measure_x + bar_note_distance + (beat_fraction * usable_width);

        // Apply horizontal alignment offset
        // CENTER alignment shifts by half notehead width
        let align_offset = notehead_width * style.horizontal_align.position_multiplier();
        let final_x = beat_x + style.pos_above.0 * staff_space - align_offset;

        // Estimate bounding box for this chord
        let bbox = estimate_chord_bbox(&chord.full_symbol, font_size, staff_space);

        positions.push(ChordPosition {
            x: final_x,
            y: chord_y,
            beat,
            bbox,
            measure_in_system,
        });
    }

    positions
}

/// Calculate and align all chord positions for a system
/// This applies MuseScore's multi-pass positioning:
/// 1. Calculate individual positions per measure
/// 2. Detect and resolve collisions
/// 3. Align all chords to outermost Y position
fn calculate_system_chord_positions(
    measures_chords: &[Vec<&keyflow::chart::ChordInstance>],
    measures_time_sigs: &[(u8, u8)],
    content_left: f32,
    measure_width: f32,
    first_measure_offset: f32, // Space for clef, key sig, time sig in first measure
    staff_y: f32,
    staff_space: f32,
    font_size: f32,
    style: &HarmonyStyle,
    music_style: &MStyle,
) -> Vec<ChordPosition> {
    let mut all_positions = Vec::new();

    // Phase 1: Calculate individual positions for each measure
    for (m, chords) in measures_chords.iter().enumerate() {
        if chords.is_empty() {
            continue;
        }

        let measure_x = content_left + (m as f32) * measure_width;
        let time_sig = measures_time_sigs.get(m).copied().unwrap_or((4, 4));

        // First measure needs extra offset for clef/key/time sig
        let measure_offset = if m == 0 { first_measure_offset } else { 0.0 };

        let positions = calculate_chord_positions(
            chords,
            measure_x + measure_offset,
            measure_width - measure_offset, // Reduce usable width accordingly
            m,
            staff_y,
            staff_space,
            font_size,
            time_sig,
            style,
            music_style,
        );

        all_positions.extend(positions);
    }

    // Phase 2: Resolve collisions between adjacent chords
    resolve_chord_collisions(&mut all_positions, staff_space, style);

    // Phase 3: Align all chords to the outermost Y position
    align_chords_in_system(&mut all_positions, staff_space, style);

    all_positions
}

// ============================================================================
// Messages and Paint Source
// ============================================================================

/// Messages to update the chart display
pub enum ChartMessage {
    UpdateChart(Option<Chart>),
    /// Zoom at cursor position (delta is scroll amount, positive = zoom in)
    Zoom {
        delta: f32,
        cursor_x: f32,
        cursor_y: f32,
    },
    /// Pan by delta pixels
    Pan { dx: f32, dy: f32 },
    /// Update cursor position for hover-based zoom
    CursorMove { x: f32, y: f32 },
    /// Reset view to default
    ResetView,
}

/// View state for zoom and pan
#[derive(Debug, Clone)]
pub struct ViewState {
    pub zoom: f32,
    pub pan_x: f32,
    pub pan_y: f32,
    pub cursor_x: f32,
    pub cursor_y: f32,
}

impl Default for ViewState {
    fn default() -> Self {
        Self {
            zoom: 1.0,
            pan_x: 0.0,
            pan_y: 0.0,
            cursor_x: 0.0,
            cursor_y: 0.0,
        }
    }
}

impl ViewState {
    pub fn reset(&mut self) {
        self.zoom = 1.0;
        self.pan_x = 0.0;
        self.pan_y = 0.0;
    }

    /// Zoom centered on cursor position
    pub fn zoom_at(&mut self, cursor_x: f32, cursor_y: f32, delta: f32, width: f32, height: f32) {
        let old_zoom = self.zoom;

        // Calculate zoom factor
        let zoom_factor = 1.0 + delta * 0.1;
        self.zoom = (self.zoom * zoom_factor).clamp(0.1, 4.0);

        // Convert cursor to normalized coordinates (-1 to 1)
        let cursor_ndc_x = (cursor_x / width) * 2.0 - 1.0;
        let cursor_ndc_y = 1.0 - (cursor_y / height) * 2.0;

        // Adjust pan to keep cursor position stable
        let zoom_ratio = self.zoom / old_zoom;
        self.pan_x = cursor_ndc_x - (cursor_ndc_x - self.pan_x) * zoom_ratio;
        self.pan_y = cursor_ndc_y - (cursor_ndc_y - self.pan_y) * zoom_ratio;
    }

    /// Pan via scroll wheel (in pixels)
    pub fn scroll_pan(&mut self, dx: f32, dy: f32, width: f32, height: f32) {
        self.pan_x += (dx / width * 2.0) / self.zoom;
        self.pan_y -= (dy / height * 2.0) / self.zoom;
    }
}

/// WGPU Paint source for chart rendering
pub struct ChartPaintSource {
    sender: Sender<ChartMessage>,
    receiver: Receiver<ChartMessage>,
    state: ChartRendererState,
    current_chart: Option<Chart>,
    page_style: PageStyle,
    /// MuseScore-compatible style properties
    music_style: MStyle,
    view_state: ViewState,
    canvas_size: (u32, u32),
    /// Debug layout mode - shows margins, spacing, and layout guides
    debug_layout: bool,
    /// FPS tracking (using VecDeque for O(1) push/pop instead of Vec::remove(0) which is O(n))
    last_frame_time: Instant,
    frame_times: VecDeque<f32>,
    current_fps: f32,
    /// Cache invalidation: version number that increments when chart/style changes
    cache_version: u64,
    /// Last rendered cache version
    last_rendered_version: u64,
}

enum ChartRendererState {
    Active(Box<ActiveChartRenderer>),
    Suspended,
}

impl ChartPaintSource {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            sender,
            receiver,
            state: ChartRendererState::Suspended,
            current_chart: None,
            // Use lead_sheet style for LilyPond-based defaults
            page_style: PageStyle::lead_sheet(),
            // Use MuseScore-compatible lead sheet style
            music_style: MStyle::lead_sheet(),
            view_state: ViewState::default(),
            // Enable debug layout to visualize spacing parameters
            debug_layout: true,
            canvas_size: (800, 600),
            // FPS tracking (VecDeque for O(1) operations)
            last_frame_time: Instant::now(),
            frame_times: VecDeque::with_capacity(64),
            current_fps: 0.0,
            // Cache invalidation
            cache_version: 0,
            last_rendered_version: 0,
        }
    }

    /// Update FPS counter using O(1) VecDeque operations
    fn update_fps(&mut self) {
        let now = Instant::now();
        let frame_time = now.duration_since(self.last_frame_time).as_secs_f32();
        self.last_frame_time = now;

        // Keep last 60 frame times for smoothing (O(1) push_back + pop_front)
        self.frame_times.push_back(frame_time);
        if self.frame_times.len() > 60 {
            self.frame_times.pop_front();
        }

        // Calculate average FPS
        if !self.frame_times.is_empty() {
            let avg_frame_time: f32 = self.frame_times.iter().sum::<f32>() / self.frame_times.len() as f32;
            self.current_fps = 1.0 / avg_frame_time;
        }
    }

    pub fn sender(&self) -> Sender<ChartMessage> {
        self.sender.clone()
    }

    fn process_messages(&mut self) {
        let (width, height) = self.canvas_size;
        while let Ok(msg) = self.receiver.try_recv() {
            match msg {
                ChartMessage::UpdateChart(chart) => {
                    // Chart already has rhythm slashes generated during parsing
                    self.current_chart = chart;
                    // Invalidate cache when chart changes
                    self.cache_version = self.cache_version.wrapping_add(1);
                }
                ChartMessage::Zoom {
                    delta,
                    cursor_x,
                    cursor_y,
                } => {
                    self.view_state
                        .zoom_at(cursor_x, cursor_y, delta, width as f32, height as f32);
                }
                ChartMessage::Pan { dx, dy } => {
                    self.view_state
                        .scroll_pan(dx, dy, width as f32, height as f32);
                }
                ChartMessage::CursorMove { x, y } => {
                    self.view_state.cursor_x = x;
                    self.view_state.cursor_y = y;
                }
                ChartMessage::ResetView => {
                    self.view_state.reset();
                }
            }
        }
    }
}

impl Default for ChartPaintSource {
    fn default() -> Self {
        Self::new()
    }
}

impl CustomPaintSource for ChartPaintSource {
    fn resume(&mut self, device_handle: &DeviceHandle) {
        let active_state = ActiveChartRenderer::new(device_handle);
        self.state = ChartRendererState::Active(Box::new(active_state));
    }

    fn suspend(&mut self) {
        self.state = ChartRendererState::Suspended;
    }

    fn render(
        &mut self,
        ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        _scale: f64,
    ) -> Option<TextureHandle> {
        // Update canvas size for message processing
        self.canvas_size = (width, height);
        self.process_messages();

        // Update FPS counter
        self.update_fps();

        if width == 0 || height == 0 {
            return None;
        }

        let ChartRendererState::Active(state) = &mut self.state else {
            return None;
        };

        state.render(
            ctx,
            width,
            height,
            self.current_chart.as_ref(),
            &self.page_style,
            &self.music_style,
            &self.view_state,
            self.debug_layout,
            self.current_fps,
            self.cache_version,
        )
    }
}

// ============================================================================
// Loaded Font
// ============================================================================

/// Loaded SMuFL font with metadata
struct LoadedFont {
    font_data: Vec<u8>,
    metadata: Option<SMuFLMetadata>,
}

impl LoadedFont {
    fn load() -> Option<Self> {
        // Use embedded font data (works with both cargo run and dx serve)
        let font_data = LELAND_FONT_DATA.to_vec();

        // Parse embedded metadata JSON
        let metadata = SMuFLMetadata::from_reader(Cursor::new(LELAND_METADATA_JSON)).ok();

        Some(Self {
            font_data,
            metadata,
        })
    }

    fn font_ref(&self) -> Option<FontRef<'_>> {
        FontRef::new(&self.font_data).ok()
    }

    /// Get glyph width in pixels
    fn glyph_width(&self, glyph: Glyph, staff_space: f32) -> f32 {
        self.metadata
            .as_ref()
            .and_then(|m| m.bounding_boxes.get(glyph))
            .map(|bb| (f64::from(bb.ne.x()) - f64::from(bb.sw.x())) as f32 * staff_space)
            .unwrap_or(staff_space)
    }

    /// Get the stem-up anchor point for a glyph
    fn stem_up_se(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.as_ref()?.anchors.get(glyph)?;
        let coord = anchors.stem_up_se?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }

    /// Get the stem-down anchor point for a glyph
    fn stem_down_nw(&self, glyph: Glyph, staff_space: f32) -> Option<(f32, f32)> {
        let anchors = self.metadata.as_ref()?.anchors.get(glyph)?;
        let coord = anchors.stem_down_nw?;
        Some((
            f64::from(coord.x()) as f32 * staff_space,
            f64::from(coord.y()) as f32 * staff_space,
        ))
    }
}

/// Helper to convert GlyphVertex to local Vertex type
fn glyph_vertices_to_vertices(glyph_vertices: Vec<GlyphVertex>) -> Vec<Vertex> {
    glyph_vertices
        .into_iter()
        .map(|gv| Vertex {
            position: gv.position,
            color: gv.color,
        })
        .collect()
}

/// Convert keyflow Chart metadata to ScoreHeader
fn chart_to_score_header(chart: &Chart) -> ScoreHeader {
    let metadata = &chart.metadata;

    // Use artist as composer if composer is not set (common in lead sheets)
    let composer = metadata
        .composer
        .clone()
        .or_else(|| metadata.artist.clone());

    // Use chart subtitle or default to "Transcribed by Cody Wright"
    let subtitle = metadata
        .subtitle
        .clone()
        .or_else(|| Some("Transcribed by Cody Wright".to_string()));

    ScoreHeader {
        title: metadata.title.clone(),
        subtitle,
        composer,
        // Default part name to "Master\nRhythm" for lead sheets (with line break)
        part_name: Some("Master\nRhythm".to_string()),
        version: None, // User can add version field later
        lyricist: metadata.lyricist.clone(),
        arranger: metadata.arranger.clone(),
        copyright: metadata.copyright.clone(),
    }
}

// ============================================================================
// Active Renderer
// ============================================================================

struct TextureAndHandle {
    texture: wgpu::Texture,
    handle: TextureHandle,
}

/// Cached scene geometry to avoid rebuilding every frame
struct CachedScene {
    /// Vertex count for draw call (avoids storing full vertex data)
    vertex_count: u32,
    /// SDF vertex count for draw call
    sdf_vertex_count: u32,
    /// Text buffers for rendering
    text_info: Vec<(TextBuffer, f32, f32, f32, TextColor, bool)>,
    /// Cache version when this scene was built
    version: u64,
    /// Canvas size when scene was built
    canvas_size: (u32, u32),
}

/// Cached view state to detect when zoom/pan changes
#[derive(Clone, Copy, PartialEq)]
struct CachedViewState {
    zoom: f32,
    pan_x: f32,
    pan_y: f32,
}

struct ActiveChartRenderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    pipeline: wgpu::RenderPipeline,
    sdf_pipeline: wgpu::RenderPipeline,
    camera_bind_group_layout: wgpu::BindGroupLayout,
    displayed_texture: Option<TextureAndHandle>,
    next_texture: Option<TextureAndHandle>,
    // Text rendering
    font_system: FontSystem,
    swash_cache: SwashCache,
    text_cache: TextCache,
    text_atlas: TextAtlas,
    text_renderer: TextRenderer,
    viewport: Viewport,
    // SMuFL font
    loaded_font: Option<LoadedFont>,
    // Cached scene for performance - only rebuild when chart changes
    cached_scene: Option<CachedScene>,
    // Cached GPU buffers (with COPY_DST for efficient updates)
    cached_vertex_buffer: Option<wgpu::Buffer>,
    cached_sdf_buffer: Option<wgpu::Buffer>,
    cached_camera_buffer: Option<wgpu::Buffer>,
    cached_camera_bind_group: Option<wgpu::BindGroup>,
    // Retained rendering resources - render chart once, blit with transform on zoom
    blit_pipeline: wgpu::RenderPipeline,
    blit_texture_bind_group_layout: wgpu::BindGroupLayout,
    blit_vertex_buffer: wgpu::Buffer,
    blit_sampler: wgpu::Sampler,
    // Scene texture for retained rendering (rendered at identity transform)
    scene_texture: Option<wgpu::Texture>,
    scene_texture_bind_group: Option<wgpu::BindGroup>,
    // Track when scene was last rendered
    scene_render_version: u64,
}

impl ActiveChartRenderer {
    fn new(device_handle: &DeviceHandle) -> Self {
        let device = &device_handle.device;
        let queue = &device_handle.queue;
        // Use the same format as the surface - this is crucial for correct rendering
        let format = wgpu::TextureFormat::Rgba8Unorm;

        // Create camera bind group layout using shared function
        let camera_bind_group_layout = create_camera_bind_group_layout(device);

        // Create main pipeline using shared function
        let pipeline = create_main_pipeline(device, format, &camera_bind_group_layout);

        // Create SDF pipeline using shared function
        let sdf_pipeline = create_sdf_pipeline(device, format, &camera_bind_group_layout);

        // Create blit pipeline for retained rendering
        let blit_texture_bind_group_layout = create_blit_texture_bind_group_layout(device);
        let blit_pipeline = create_blit_pipeline(
            device,
            format,
            &camera_bind_group_layout,
            &blit_texture_bind_group_layout,
        );

        // Create fullscreen quad vertex buffer for blitting
        let blit_vertices = create_fullscreen_quad();
        let blit_vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Blit Vertex Buffer"),
            contents: bytemuck::cast_slice(&blit_vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });

        // Create sampler for scene texture
        let blit_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Blit Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // Initialize text rendering with custom fonts
        let mut font_system = FontSystem::new();

        // Load LelandText font for chord symbols (has music notation characters)
        font_system.db_mut().load_font_data(LELAND_TEXT_FONT_DATA.to_vec());
        log::info!("Loaded LelandText font for chord symbols");

        let swash_cache = SwashCache::new();
        let text_cache = TextCache::new(device);
        let text_atlas = TextAtlas::new(device, queue, &text_cache, format);
        let mut text_atlas = text_atlas;
        let text_renderer =
            TextRenderer::new(&mut text_atlas, device, wgpu::MultisampleState::default(), None);
        let viewport = Viewport::new(device, &text_cache);

        // Load SMuFL font (embedded at compile time)
        let loaded_font = LoadedFont::load();
        if loaded_font.is_some() {
            log::info!("Loaded embedded Leland font successfully");
        } else {
            log::warn!("Failed to parse embedded Leland font");
        }

        Self {
            device: device.clone(),
            queue: queue.clone(),
            pipeline,
            sdf_pipeline,
            camera_bind_group_layout,
            displayed_texture: None,
            next_texture: None,
            font_system,
            swash_cache,
            text_cache,
            text_atlas,
            text_renderer,
            viewport,
            loaded_font,
            // Cache fields for performance
            cached_scene: None,
            cached_vertex_buffer: None,
            cached_sdf_buffer: None,
            cached_camera_buffer: None,
            cached_camera_bind_group: None,
            // Retained rendering resources
            blit_pipeline,
            blit_texture_bind_group_layout,
            blit_vertex_buffer,
            blit_sampler,
            scene_texture: None,
            scene_texture_bind_group: None,
            scene_render_version: 0,
        }
    }

    fn render(
        &mut self,
        mut ctx: CustomPaintCtx<'_>,
        width: u32,
        height: u32,
        chart: Option<&Chart>,
        page_style: &PageStyle,
        music_style: &MStyle,
        view_state: &ViewState,
        debug_layout: bool,
        current_fps: f32,
        cache_version: u64,
    ) -> Option<TextureHandle> {
        // Handle output texture management
        if self
            .next_texture
            .as_ref()
            .is_some_and(|tex| tex.texture.width() != width || tex.texture.height() != height)
        {
            let handle = self.next_texture.take().unwrap().handle;
            ctx.unregister_texture(handle);
            // Invalidate geometry cache on resize
            self.cached_scene = None;
        }

        if self.next_texture.is_none() {
            let texture = create_texture(&self.device, width, height);
            let handle = ctx.register_texture(texture.clone());
            self.next_texture = Some(TextureAndHandle { texture, handle });
        }

        // Check if we need to rebuild geometry (content changed or size changed)
        let needs_rebuild = self.cached_scene.as_ref().map_or(true, |cache| {
            cache.version != cache_version || cache.canvas_size != (width, height)
        });

        // Get output texture handle (clone to avoid borrow issues)
        let next_texture_handle = self.next_texture.as_ref().unwrap().handle.clone();

        // Build or reuse cached geometry
        if needs_rebuild {
            let (vertices, sdf_vertices, text_info) =
                self.build_scene(chart, width, height, page_style, music_style, debug_layout);

            let vertex_count = vertices.len() as u32;
            let sdf_vertex_count = sdf_vertices.len() as u32;

            // Create GPU buffers
            self.cached_vertex_buffer = if !vertices.is_empty() {
                Some(self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("Chart Vertex Buffer"),
                    contents: bytemuck::cast_slice(&vertices),
                    usage: wgpu::BufferUsages::VERTEX,
                }))
            } else {
                None
            };

            self.cached_sdf_buffer = if !sdf_vertices.is_empty() {
                Some(self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("SDF Vertex Buffer"),
                    contents: bytemuck::cast_slice(&sdf_vertices),
                    usage: wgpu::BufferUsages::VERTEX,
                }))
            } else {
                None
            };

            // Update cache
            self.cached_scene = Some(CachedScene {
                vertex_count,
                sdf_vertex_count,
                text_info,
                version: cache_version,
                canvas_size: (width, height),
            });
        }

        // Get cached counts
        let (vertex_count, sdf_vertex_count) = self
            .cached_scene
            .as_ref()
            .map(|c| (c.vertex_count, c.sdf_vertex_count))
            .unwrap_or((0, 0));

        // Update viewport
        self.viewport
            .update(&self.queue, Resolution { width, height });

        // Create or update camera uniform buffer (with zoom/pan)
        let camera_uniform = CameraUniform::with_resolution(
            view_state.zoom,
            view_state.pan_x,
            view_state.pan_y,
            width as f32,
            height as f32,
        );

        if self.cached_camera_buffer.is_none() {
            let buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Camera Uniform Buffer"),
                contents: bytemuck::cast_slice(&[camera_uniform]),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            });
            let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Camera Bind Group"),
                layout: &self.camera_bind_group_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: buffer.as_entire_binding(),
                }],
            });
            self.cached_camera_buffer = Some(buffer);
            self.cached_camera_bind_group = Some(bind_group);
        } else {
            self.queue.write_buffer(
                self.cached_camera_buffer.as_ref().unwrap(),
                0,
                bytemuck::cast_slice(&[camera_uniform]),
            );
        }

        // Helper to transform pixel coordinates by camera (zoom + pan)
        let transform_by_camera = |px: f32, py: f32| -> (f32, f32) {
            let w = width as f32;
            let h = height as f32;
            let ndc_x = (px / w) * 2.0 - 1.0;
            let ndc_y = 1.0 - (py / h) * 2.0;
            let transformed_x = ndc_x * view_state.zoom + view_state.pan_x;
            let transformed_y = ndc_y * view_state.zoom + view_state.pan_y;
            let screen_x = (transformed_x + 1.0) / 2.0 * w;
            let screen_y = (1.0 - transformed_y) / 2.0 * h;
            (screen_x, screen_y)
        };

        // Create FPS counter text buffer (screen-space, not transformed)
        let fps_text = format!("{:.0} FPS", current_fps);
        let mut fps_buffer = TextBuffer::new(&mut self.font_system, Metrics::new(28.0, 32.0));
        fps_buffer.set_size(&mut self.font_system, Some(150.0), Some(40.0));
        fps_buffer.set_text(
            &mut self.font_system,
            &fps_text,
            &Attrs::new().family(Family::Monospace).weight(Weight::BOLD),
            Shaping::Advanced,
        );
        fps_buffer.shape_until_scroll(&mut self.font_system, false);

        // Build text areas: world-space text (transformed) + FPS counter (screen-space)
        let mut text_areas: Vec<TextArea> = self
            .cached_scene
            .as_ref()
            .map(|c| &c.text_info)
            .map(|info| info.iter())
            .into_iter()
            .flatten()
            .map(|(buffer, x, y, text_scale, color, _screen_space)| {
                // Transform world-space text positions by camera
                let (tx, ty) = transform_by_camera(*x, *y);
                TextArea {
                    buffer,
                    left: tx,
                    top: ty,
                    scale: view_state.zoom * text_scale,
                    bounds: TextBounds {
                        left: 0,
                        top: 0,
                        right: width as i32,
                        bottom: height as i32,
                    },
                    default_color: *color,
                    custom_glyphs: &[],
                }
            })
            .collect();

        // Add FPS counter (screen-space, fixed position)
        text_areas.push(TextArea {
            buffer: &fps_buffer,
            left: width as f32 - 160.0,
            top: 10.0,
            scale: 1.0,
            bounds: TextBounds {
                left: 0,
                top: 0,
                right: width as i32,
                bottom: height as i32,
            },
            default_color: TextColor::rgba(0, 255, 100, 255),
            custom_glyphs: &[],
        });

        // Prepare all text
        let _ = self.text_renderer.prepare(
            &self.device,
            &self.queue,
            &mut self.font_system,
            &mut self.text_atlas,
            &self.viewport,
            text_areas,
            &mut self.swash_cache,
        );

        // Get references for rendering
        let output_texture = &self.next_texture.as_ref().unwrap().texture;
        let camera_bind_group = self.cached_camera_bind_group.as_ref().unwrap();
        let output_view = output_texture.create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Chart Render Encoder"),
            });

        // Single render pass - direct vector rendering with camera transform
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Chart Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &output_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.2,
                            g: 0.2,
                            b: 0.22,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            // Draw main geometry with camera transform (crisp at any zoom)
            if let Some(buffer) = &self.cached_vertex_buffer {
                render_pass.set_pipeline(&self.pipeline);
                render_pass.set_bind_group(0, camera_bind_group, &[]);
                render_pass.set_vertex_buffer(0, buffer.slice(..));
                render_pass.draw(0..vertex_count, 0..1);
            }

            // Draw SDF shapes with camera transform (pixel-perfect at any zoom)
            if let Some(buffer) = &self.cached_sdf_buffer {
                render_pass.set_pipeline(&self.sdf_pipeline);
                render_pass.set_bind_group(0, camera_bind_group, &[]);
                render_pass.set_vertex_buffer(0, buffer.slice(..));
                render_pass.draw(0..sdf_vertex_count, 0..1);
            }

            // Draw text (already transformed to screen coordinates)
            let _ = self
                .text_renderer
                .render(&self.text_atlas, &self.viewport, &mut render_pass);
        }

        self.queue.submit(std::iter::once(encoder.finish()));

        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
        Some(next_texture_handle)
    }

    /// Get the time signature for a specific measure index in the chart.
    /// Walks through sections and measures to find the time signature.
    fn get_measure_time_sig(&self, chart: &Chart, measure_idx: usize) -> (u8, u8) {
        let mut measures_counted = 0;
        for section in &chart.sections {
            for measure in &section.measures {
                if measures_counted == measure_idx {
                    return measure.time_signature;
                }
                measures_counted += 1;
            }
        }
        // Default to 4/4 if measure not found
        (4, 4)
    }

    fn build_scene(
        &mut self,
        chart: Option<&Chart>,
        width: u32,
        height: u32,
        page_style: &PageStyle,
        music_style: &MStyle,
        debug_layout: bool,
    ) -> (
        Vec<Vertex>,
        Vec<SdfRectVertex>,
        Vec<(TextBuffer, f32, f32, f32, TextColor, bool)>, // (buffer, x, y, scale, color, screen_space)
    ) {
        let w = width as f32;
        let h = height as f32;

        let mut vertices = Vec::new();
        let mut sdf_vertices = Vec::new();
        let mut text_buffers = Vec::new();

        // Colors (same as music_symbols example)
        let black = [0.0, 0.0, 0.0, 1.0];
        let paper_white = [1.0, 1.0, 1.0, 1.0];
        let paper_shadow = [0.15, 0.15, 0.17, 1.0];
        let rehearsal_red = [1.0, 0.0, 0.0, 1.0];

        // Text colors for glyphon
        let text_black = TextColor::rgba(0, 0, 0, 255);
        let text_red = TextColor::rgba(255, 0, 0, 255);
        let text_gray = TextColor::rgba(128, 128, 128, 255);

        // Debug layout colors (semi-transparent for overlay)
        let debug_margin_color = [0.0, 0.5, 1.0, 0.3];      // Blue - margins
        let debug_header_color = [1.0, 0.5, 0.0, 0.3];      // Orange - header area
        let debug_content_color = [0.0, 1.0, 0.0, 0.15];    // Green - content area
        let debug_spacing_color = [1.0, 0.0, 1.0, 0.5];     // Magenta - system spacing
        let debug_line_color = [1.0, 0.0, 0.0, 0.8];        // Red - guide lines

        // Calculate page dimensions from PageStyle (points -> pixels)
        let (page_width_pt, page_height_pt) = page_style.paper_size.dimensions_pt();
        let page_width = page_width_pt * PT_TO_PX;
        let page_height = page_height_pt * PT_TO_PX;

        // Get margins (points -> pixels)
        let margin_left_px = page_style.margins.left * PT_TO_PX;
        let margin_top_px = page_style.margins.top * PT_TO_PX;
        let margin_right_px = page_style.margins.right * PT_TO_PX;
        let margin_bottom_px = page_style.margins.bottom * PT_TO_PX;

        // Staff measurements from PageStyle (points -> pixels)
        let staff_space = page_style.staff.staff_space * PT_TO_PX;
        let staff_height = page_style.staff.staff_height * PT_TO_PX; // Height of 5 staff lines (4 spaces)
        let system_height = page_style.system_height() * PT_TO_PX; // Staff + extra room for dynamics/lyrics
        let system_spacing = page_style.system_spacing.system_to_system * PT_TO_PX;
        // Section spacing is available but not used currently to keep consistent spacing
        let _section_extra_spacing = page_style.system_spacing.section_spacing * PT_TO_PX;
        let top_padding = page_style.system_spacing.top_padding * PT_TO_PX;
        let font_size = staff_space * 4.0; // Font size for SMuFL glyphs

        // Footer height from PageStyle
        let footer_height = page_style.footer_height * PT_TO_PX;

        // Content dimensions
        let content_width = page_width - margin_left_px - margin_right_px;
        let content_height = page_height - margin_top_px - margin_bottom_px;

        let Some(chart) = chart else {
            // Draw single empty page
            let page_x = (w - page_width) / 2.0;
            let page_y = 20.0;
            let shadow_offset = 4.0;
            vertices.extend(create_rect(
                page_x + shadow_offset, page_y + shadow_offset,
                page_width, page_height, paper_shadow, w, h,
            ));
            vertices.extend(create_rect(
                page_x, page_y, page_width, page_height, paper_white, w, h,
            ));
            return (vertices, sdf_vertices, text_buffers);
        };

        // Extract section information from chart
        let total_measures: usize = chart.sections.iter().map(|s| s.measures.len()).sum();
        let mut section_starts = Vec::new();
        let mut section_labels: Vec<String> = Vec::new();
        let mut current_measure = 0;

        for section in &chart.sections {
            section_starts.push(current_measure);
            // Use format_rehearsal_label for proper formatting (INTRO, VS 1, CH 1, etc.)
            let label = format_rehearsal_label(
                &section.section.section_type.full_name(),
                &section.section.section_type.abbreviation(),
                section.section.number,
            );
            section_labels.push(label);
            current_measure += section.measures.len();
        }

        // Compute system layout (4 measures per line, breaking at sections)
        let system_layout = compute_system_layout(
            total_measures,
            &section_starts,
            &LineBreakPolicy::four_per_line(),
        );

        // Create score header from chart metadata (needed for layout calculation)
        let score_header = chart_to_score_header(chart);
        let header_config = HeaderFrameConfig::default();
        let header_layout = ComputedHeaderLayout::compute(&score_header, &header_config);

        // Calculate header height for first page
        // IMPORTANT: header_layout.frame_height is in PIXELS (from glyphon font sizes)
        // but PageLayoutConfig expects POINTS (consistent with PageStyle)
        // So we must convert pixels -> points
        let first_page_header_height_pt = if score_header.has_content() {
            header_layout.frame_height / PT_TO_PX  // Convert pixels to points
        } else {
            0.0
        };

        // Compute page layout using PageStyle config with actual header height
        // All values in PageLayoutConfig are in POINTS
        let mut page_config = page_style.to_layout_config();
        page_config.first_page_header_height = first_page_header_height_pt;
        let page_layout = compute_page_layout(system_layout, &page_config);

        // Spacing between pages
        let page_gap = 40.0;
        let initial_page_y = 20.0;

        // Render each page
        for (page_idx, page_info) in page_layout.pages.iter().enumerate() {
            // Calculate page position (centered horizontally, stacked vertically)
            let page_x = (w - page_width) / 2.0;
            let page_y = initial_page_y + (page_idx as f32) * (page_height + page_gap);

            // Draw paper shadow
            let shadow_offset = 4.0;
            vertices.extend(create_rect(
                page_x + shadow_offset,
                page_y + shadow_offset,
                page_width,
                page_height,
                paper_shadow,
                w,
                h,
            ));

            // Draw paper background
            vertices.extend(create_rect(
                page_x, page_y, page_width, page_height, paper_white, w, h,
            ));

            // Content area for this page
            let content_left = page_x + margin_left_px;
            let content_top = page_y + margin_top_px;
            let content_right = page_x + page_width - margin_right_px;
            // Content extends to margin boundary - footer is rendered IN the margin area
            let content_bottom = page_y + page_height - margin_bottom_px;

            // For section label positioning in left margin
            let margin_padding_h = 2.0;
            let margin_capsule_width = margin_left_px - (margin_padding_h * 2.0);
            let margin_capsule_x = page_x + margin_padding_h;

            // Starting position for systems on this page
            let mut current_y = content_top;

            // Log page bounds for debugging
            log::debug!(
                "Page {}: content bounds y=[{:.1}, {:.1}], height={:.1}",
                page_idx + 1,
                content_top,
                content_bottom,
                content_bottom - content_top
            );

            // Render header on first page only
            if page_idx == 0 && score_header.has_content() {
                // Render header text elements
                let header_left = content_left;
                let header_right = content_right;
                let header_center = (header_left + header_right) / 2.0;

                // Part Name (top left) - supports multi-line with \n
                if let Some(ref part_name) = score_header.part_name {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::PART_NAME.font_size, HeaderStyles::PART_NAME.font_size * HeaderStyles::PART_NAME.line_height),
                    );
                    // Increased height to 50.0 to support multi-line part names like "Master\nRhythm"
                    buffer.set_size(&mut self.font_system, Some(content_width / 3.0), Some(50.0));
                    buffer.set_text(
                        &mut self.font_system,
                        part_name,
                        &Attrs::new().family(Family::SansSerif),
                        Shaping::Advanced,
                    );
                    buffer.shape_until_scroll(&mut self.font_system, false);
                    text_buffers.push((buffer, header_left, current_y + header_layout.top_row_y, 1.0, text_black, false));
                }

                // Composer (top right)
                if let Some(ref composer) = score_header.composer {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::COMPOSER.font_size, HeaderStyles::COMPOSER.font_size * HeaderStyles::COMPOSER.line_height),
                    );
                    buffer.set_size(&mut self.font_system, Some(content_width / 3.0), Some(30.0));
                    buffer.set_text(
                        &mut self.font_system,
                        composer,
                        &Attrs::new().family(Family::SansSerif),
                        Shaping::Advanced,
                    );
                    buffer.shape_until_scroll(&mut self.font_system, false);

                    // Measure text width for right alignment
                    let text_width: f32 = buffer.layout_runs().map(|run| run.line_w).next().unwrap_or(0.0);
                    let composer_x = header_right - text_width;
                    text_buffers.push((buffer, composer_x, current_y + header_layout.top_row_y, 1.0, text_black, false));
                }

                // Lyricist (second row left)
                if let Some(ref lyricist) = score_header.lyricist {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::LYRICIST.font_size, HeaderStyles::LYRICIST.font_size * HeaderStyles::LYRICIST.line_height),
                    );
                    buffer.set_size(&mut self.font_system, Some(content_width / 3.0), Some(30.0));
                    buffer.set_text(
                        &mut self.font_system,
                        lyricist,
                        &Attrs::new().family(Family::SansSerif),
                        Shaping::Advanced,
                    );
                    buffer.shape_until_scroll(&mut self.font_system, false);
                    text_buffers.push((buffer, header_left, current_y + header_layout.second_row_y, 1.0, text_black, false));
                }

                // Version (second row right)
                if let Some(ref version) = score_header.version {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::VERSION.font_size, HeaderStyles::VERSION.font_size * HeaderStyles::VERSION.line_height),
                    );
                    buffer.set_size(&mut self.font_system, Some(content_width / 3.0), Some(30.0));
                    let attrs = if HeaderStyles::VERSION.italic {
                        Attrs::new().family(Family::SansSerif).style(glyphon::Style::Italic)
                    } else {
                        Attrs::new().family(Family::SansSerif)
                    };
                    buffer.set_text(&mut self.font_system, version, &attrs, Shaping::Advanced);
                    buffer.shape_until_scroll(&mut self.font_system, false);

                    let text_width: f32 = buffer.layout_runs().map(|run| run.line_w).next().unwrap_or(0.0);
                    let version_x = header_right - text_width;
                    text_buffers.push((buffer, version_x, current_y + header_layout.second_row_y, 1.0, text_black, false));
                }

                // Title (centered, largest)
                if let Some(ref title) = score_header.title {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::TITLE.font_size, HeaderStyles::TITLE.font_size * HeaderStyles::TITLE.line_height),
                    );
                    buffer.set_size(&mut self.font_system, Some(content_width), Some(50.0));
                    let attrs = if HeaderStyles::TITLE.bold {
                        Attrs::new().family(Family::SansSerif).weight(Weight::BOLD)
                    } else {
                        Attrs::new().family(Family::SansSerif)
                    };
                    buffer.set_text(&mut self.font_system, title, &attrs, Shaping::Advanced);
                    buffer.shape_until_scroll(&mut self.font_system, false);

                    // Center the title
                    let text_width: f32 = buffer.layout_runs().map(|run| run.line_w).next().unwrap_or(0.0);
                    let title_x = header_center - text_width / 2.0;
                    text_buffers.push((buffer, title_x, current_y + header_layout.title_y, 1.0, text_black, false));
                }

                // Subtitle (centered, below title)
                if let Some(ref subtitle) = score_header.subtitle {
                    let mut buffer = TextBuffer::new(
                        &mut self.font_system,
                        Metrics::new(HeaderStyles::SUBTITLE.font_size, HeaderStyles::SUBTITLE.font_size * HeaderStyles::SUBTITLE.line_height),
                    );
                    buffer.set_size(&mut self.font_system, Some(content_width), Some(30.0));
                    buffer.set_text(
                        &mut self.font_system,
                        subtitle,
                        &Attrs::new().family(Family::SansSerif),
                        Shaping::Advanced,
                    );
                    buffer.shape_until_scroll(&mut self.font_system, false);

                    // Center the subtitle
                    let text_width: f32 = buffer.layout_runs().map(|run| run.line_w).next().unwrap_or(0.0);
                    let subtitle_x = header_center - text_width / 2.0;
                    text_buffers.push((buffer, subtitle_x, current_y + header_layout.subtitle_y, 1.0, text_black, false));
                }

                // Move current_y past the header
                current_y += header_layout.frame_height;
            }

            // Add top padding for systems
            current_y += top_padding;

            // Get systems for this page
            let first_sys = page_info.first_system_index;
            let last_sys = first_sys + page_info.system_count;

            // Track if this is the first system on this page (for extra section spacing)
            let mut first_system_on_page = true;

            for sys_idx in first_sys..last_sys {
                let sys_info = &page_layout.system_layout.systems[sys_idx];

                let staff_y = current_y;

                // Bounds check: ensure this system fits within the page content area
                let system_bottom = staff_y + system_height;
                if system_bottom > content_bottom {
                    log::warn!(
                        "System {} would exceed page {} bounds: y={:.1}, bottom={:.1}, content_bottom={:.1}",
                        sys_idx, page_idx + 1, staff_y, system_bottom, content_bottom
                    );
                    // Skip rendering this system - it shouldn't be on this page
                    // (This indicates a bug in page layout calculation)
                    continue;
                }

                // Draw 5 staff lines
                for i in 0..5 {
                    let y = staff_y + (i as f32) * staff_space;
                    vertices.extend(create_line(content_left, y, content_right, y, 1.0, black, w, h));
                }

                // Draw barlines (from top staff line to bottom staff line)
                let measures_in_system = sys_info.measure_count;
                let measure_width = content_width / measures_in_system.max(1) as f32;

                for m in 0..=measures_in_system {
                    let bar_x = content_left + (m as f32) * measure_width;
                    let thickness = if m == 0 || m == measures_in_system {
                        2.0
                    } else {
                        1.0
                    };
                    vertices.extend(create_line(
                        bar_x,
                        staff_y,
                        bar_x,
                        staff_y + staff_height, // Use staff_height, not system_height
                        thickness,
                        black,
                        w,
                        h,
                    ));
                }

                // Calculate offset for first measure to account for clef/key/time signature
                // Clef glyph at staff_space * 0.5, extends ~4 staff spaces wide
                // Time sig at staff_space * 4.5, extends ~2 staff spaces wide
                // Music content should start after time sig: ~7.5 staff spaces on first system
                let first_measure_offset = if sys_idx == 0 {
                    staff_space * 8.0 // Clef (~4.5) + time signature (~3.5) on first system
                } else {
                    staff_space * 5.5 // Just clef on subsequent systems
                };

                // Collect all rhythm slashes and time signatures for this system (for SMuFL rendering)
                let mut all_measure_slashes: Vec<Vec<&keyflow::chart::RhythmSlash>> = Vec::new();
                let mut all_time_sigs: Vec<(u8, u8)> = Vec::new();

                {
                    let start_measure = sys_info.start_measure;
                    for m in 0..measures_in_system {
                        let global_measure_idx = start_measure + m;
                        let mut measure_slashes: Vec<&keyflow::chart::RhythmSlash> = Vec::new();
                        let mut measure_time_sig = (4u8, 4u8);
                        let mut measures_counted = 0;

                        'outer_slash: for section in &chart.sections {
                            for measure in &section.measures {
                                if measures_counted == global_measure_idx {
                                    for slash in &measure.rhythm_slashes {
                                        measure_slashes.push(slash);
                                    }
                                    measure_time_sig = measure.time_signature;
                                    break 'outer_slash;
                                }
                                measures_counted += 1;
                            }
                        }

                        all_measure_slashes.push(measure_slashes);
                        all_time_sigs.push(measure_time_sig);
                    }
                }

                // Render chord symbols above the staff using MuseScore-style positioning
                // Uses system-level positioning with collision detection and vertical alignment
                {
                    let start_measure = sys_info.start_measure;
                    let base_chord_font_size = staff_space * 2.0; // ~10pt at default staff_space
                    // Create HarmonyStyle from MStyle to use style system values
                    let harmony_style = HarmonyStyle::from_mstyle(music_style);

                    // Phase 1: Collect all chords for this system
                    let mut all_measure_chords: Vec<Vec<&keyflow::chart::ChordInstance>> = Vec::new();
                    let mut chord_symbols: Vec<String> = Vec::new(); // For rendering

                    for m in 0..measures_in_system {
                        let global_measure_idx = start_measure + m;
                        let mut measure_chords: Vec<&keyflow::chart::ChordInstance> = Vec::new();
                        let mut measures_counted = 0;

                        'outer: for section in &chart.sections {
                            for measure in &section.measures {
                                if measures_counted == global_measure_idx {
                                    for chord in &measure.chords {
                                        measure_chords.push(chord);
                                        chord_symbols.push(chord.full_symbol.clone());
                                    }
                                    break 'outer;
                                }
                                measures_counted += 1;
                            }
                        }

                        all_measure_chords.push(measure_chords);
                    }

                    // Phase 2: Calculate system-level positions with collision detection and alignment
                    let positions = calculate_system_chord_positions(
                        &all_measure_chords,
                        &all_time_sigs,
                        content_left,
                        measure_width,
                        first_measure_offset,
                        staff_y,
                        staff_space,
                        base_chord_font_size,
                        &harmony_style,
                        music_style,
                    );

                    // Phase 3: Render all chords at their calculated positions
                    let mut pos_idx = 0;
                    for measure_chords in &all_measure_chords {
                        for chord in measure_chords {
                            if pos_idx >= positions.len() {
                                break;
                            }
                            let pos = &positions[pos_idx];
                            pos_idx += 1;

                            // Render chord with measured widths (not estimated)
                            // This gives accurate positioning based on actual font metrics
                            let components = ChordSymbolComponents::parse(&chord.full_symbol);

                            // Calculate MuseScore's super offset for superscripts (0.36 * cap-height)
                            let cap_height = base_chord_font_size * 0.72;
                            let super_offset = cap_height * 0.36;
                            let superscript_scale = 0.75; // MuseScore stacked_modifier_mag

                            // Helper to create buffer and measure actual width
                            let mut measure_text = |text: &str, font_size: f32| -> (TextBuffer, f32) {
                                let mut buffer = TextBuffer::new(
                                    &mut self.font_system,
                                    Metrics::new(font_size, font_size * 1.2),
                                );
                                buffer.set_size(&mut self.font_system, Some(200.0), Some(font_size * 2.0));
                                buffer.set_text(
                                    &mut self.font_system,
                                    text,
                                    &Attrs::new()
                                        .family(Family::Name("Leland Text"))
                                        .weight(Weight::NORMAL),
                                    Shaping::Advanced,
                                );
                                buffer.shape_until_scroll(&mut self.font_system, false);
                                let width = buffer.layout_runs()
                                    .map(|run| run.line_w)
                                    .next()
                                    .unwrap_or(font_size * 0.5);
                                (buffer, width)
                            };

                            let mut current_x = pos.x;
                            let baseline_y = pos.y;

                            // 1. Root note (full size, baseline)
                            if !components.root.is_empty() {
                                let (buffer, width) = measure_text(&components.root, base_chord_font_size);
                                text_buffers.push((buffer, current_x, baseline_y, 1.0, text_black, false));
                                current_x += width;
                            }

                            // 2. Root accidental (full size, baseline)
                            if let Some(acc) = &components.root_accidental {
                                let acc_text = match acc {
                                    ChordAccidental::Sharp => "♯",
                                    ChordAccidental::Flat => "♭",
                                    ChordAccidental::Natural => "♮",
                                };
                                let (buffer, width) = measure_text(acc_text, base_chord_font_size);
                                text_buffers.push((buffer, current_x, baseline_y, 1.0, text_black, false));
                                current_x += width;
                            }

                            // 3. Quality (m, dim, etc.) - full size unless it's ° or ø
                            if !components.quality.is_empty() {
                                let is_superscript = components.quality == "°" || components.quality == "ø";
                                let (font_size, y_offset) = if is_superscript {
                                    (base_chord_font_size * superscript_scale, baseline_y - super_offset)
                                } else {
                                    (base_chord_font_size, baseline_y)
                                };
                                let (buffer, width) = measure_text(&components.quality, font_size);
                                text_buffers.push((buffer, current_x, y_offset, 1.0, text_black, false));
                                current_x += width;
                            }

                            // 4. Extension (Δ7, 7, 9, etc.)
                            if !components.extension.is_empty() {
                                // Check for triangle at start
                                let (triangle, number) = if components.extension.starts_with('Δ') {
                                    (Some("Δ"), &components.extension[2..]) // Δ is 2 bytes UTF-8
                                } else {
                                    (None, components.extension.as_str())
                                };

                                // Triangle at baseline
                                if let Some(tri) = triangle {
                                    let (buffer, width) = measure_text(tri, base_chord_font_size);
                                    text_buffers.push((buffer, current_x, baseline_y, 1.0, text_black, false));
                                    current_x += width;
                                }

                                // Number as superscript
                                if !number.is_empty() {
                                    let sup_font_size = base_chord_font_size * superscript_scale;
                                    let (buffer, width) = measure_text(number, sup_font_size);
                                    text_buffers.push((buffer, current_x, baseline_y - super_offset, 1.0, text_black, false));
                                    current_x += width;
                                }
                            }

                            // 5. Alterations (b5, #9, etc.) - superscript
                            for alt in &components.alterations {
                                let formatted = alt
                                    .replace("b5", "♭5")
                                    .replace("b9", "♭9")
                                    .replace("b13", "♭13")
                                    .replace("#5", "♯5")
                                    .replace("#9", "♯9")
                                    .replace("#11", "♯11");
                                let sup_font_size = base_chord_font_size * superscript_scale * 0.9;
                                let (buffer, width) = measure_text(&formatted, sup_font_size);
                                text_buffers.push((buffer, current_x, baseline_y - super_offset, 1.0, text_black, false));
                                current_x += width;
                            }

                            // 6. Bass note (slash chord)
                            if let Some(bass) = &components.bass {
                                // Slash
                                let (buffer, width) = measure_text("/", base_chord_font_size);
                                text_buffers.push((buffer, current_x, baseline_y, 1.0, text_black, false));
                                current_x += width;
                                // Bass note
                                let (buffer, _width) = measure_text(bass, base_chord_font_size);
                                text_buffers.push((buffer, current_x, baseline_y, 1.0, text_black, false));
                            }
                        }
                    }
                }

                // Draw SMuFL symbols if font is loaded
                if let Some(ref loaded_font) = self.loaded_font {
                    if let Some(font_ref) = loaded_font.font_ref() {
                        // G Clef at start of every system
                        if let Some(gid) = get_glyph_id(&font_ref, Glyph::GClef) {
                            let clef_x = content_left + staff_space * 0.5;
                            let clef_y = staff_y + staff_space * 3.0; // G line
                            let clef_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                                &font_ref, gid, font_size, clef_x, clef_y, black, w, h,
                            ));
                            vertices.extend(clef_verts);
                        }

                        // Time signature only on first system of entire piece
                        if sys_idx == 0 {
                            if let Some(gid) = get_glyph_id(&font_ref, Glyph::TimeSigCommon) {
                                let ts_x = content_left + staff_space * 4.5;
                                let ts_y = staff_y + staff_space * 2.0;
                                let ts_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                                    &font_ref, gid, font_size, ts_x, ts_y, black, w, h,
                                ));
                                vertices.extend(ts_verts);
                            }
                        }

                        // Render rhythm slashes for empty beats
                        // Try NoteheadSlashHorizontalEnds first, fallback to RepeatBarSlash
                        let slash_gid = get_glyph_id(&font_ref, Glyph::NoteheadSlashHorizontalEnds)
                            .or_else(|| get_glyph_id(&font_ref, Glyph::RepeatBarSlash));
                        if let Some(slash_gid) = slash_gid {
                            // Use same spacing as chord symbols (from MStyle)
                            let bar_note_distance = staff_space * music_style.spatium(Sid::BarNoteDistance);
                            let note_bar_distance = staff_space * music_style.spatium(Sid::NoteBarDistance);

                            // Position slashes like noteheads: left edge at beat position
                            // MuseScore positions noteheads with left edge at segment x
                            // Chord symbols are then aligned relative to the notehead
                            // For CENTER alignment, chords shift left by notehead_width * 0.5
                            // We apply the same shift to slashes so they align with chords
                            let notehead_width = staff_space * 1.18;
                            let slash_offset = notehead_width * 0.5;

                            for (m, measure_slashes) in all_measure_slashes.iter().enumerate() {
                                let time_sig = all_time_sigs[m];
                                let beats_in_measure = time_sig.0 as f32;
                                let measure_x = content_left + (m as f32) * measure_width;

                                // For first measure, account for clef/time sig
                                let clef_offset = if m == 0 { first_measure_offset } else { 0.0 };

                                // Match chord positioning: start after bar_note_distance
                                let adjusted_measure_x = measure_x + clef_offset;
                                let usable_width = measure_width - clef_offset - bar_note_distance - note_bar_distance;

                                for slash in measure_slashes {
                                    let beat = slash.beat as f32;
                                    // Position like chords: beat_fraction * usable_width
                                    let beat_fraction = beat / beats_in_measure;
                                    let beat_x = adjusted_measure_x + bar_note_distance + (beat_fraction * usable_width);
                                    // Apply same offset as chord symbols for alignment
                                    let slash_x = beat_x - slash_offset;
                                    // Center line of staff (B4 position)
                                    let slash_y = staff_y + staff_space * 2.0;

                                    let slash_verts = glyph_vertices_to_vertices(tessellate_glyph_to_ndc(
                                        &font_ref, slash_gid, font_size, slash_x, slash_y, black, w, h,
                                    ));
                                    vertices.extend(slash_verts);
                                }
                            }
                        }
                    }
                }

                // Add section label if this is a section start
                if sys_info.is_section_start {
                    if let Some(section_idx) = section_starts
                        .iter()
                        .position(|&s| s == sys_info.start_measure)
                    {
                        let section_label = &section_labels[section_idx];

                        // Create text buffer first to measure text width
                        let mut buffer =
                            TextBuffer::new(&mut self.font_system, Metrics::new(14.0, 18.0));
                        buffer.set_size(
                            &mut self.font_system,
                            Some(500.0),
                            Some(50.0),
                        );
                        buffer.set_text(
                            &mut self.font_system,
                            section_label,
                            &Attrs::new().family(Family::SansSerif).weight(Weight::BOLD),
                            Shaping::Advanced,
                        );
                        buffer.shape_until_scroll(&mut self.font_system, false);

                        let measured_text_width: f32 = buffer
                            .layout_runs()
                            .map(|run| run.line_w)
                            .next()
                            .unwrap_or(50.0);

                        let label_config = CapsuleLabelConfig {
                            mode: CapsuleLabelMode::FixedWidth {
                                width: margin_capsule_width,
                                height: staff_height - 4.0, // Match staff height
                                internal_padding_h: 1.0,
                                internal_padding_v: 1.0,
                            },
                            font_size: 14.0,
                            line_height: 18.0,
                        };

                        let computed = ComputedCapsuleLabel::compute(
                            section_label,
                            margin_capsule_x,
                            staff_y + 2.0, // Slight offset to center on staff
                            measured_text_width,
                            &label_config,
                        );

                        sdf_vertices.extend(create_sdf_rounded_rect(
                            computed.capsule_x,
                            computed.capsule_y,
                            computed.capsule_width,
                            computed.capsule_height,
                            computed.corner_radius,
                            1.5,
                            rehearsal_red,
                            w,
                            h,
                        ));

                        text_buffers.push((buffer, computed.text_x, computed.text_y, computed.text_scale, text_red, false));
                    }
                }

                // Move to next system (staff_height + spacing between systems)
                current_y += staff_height + system_spacing;
                first_system_on_page = false;
            }

            // Draw page number at top right of page
            let page_num_text = format!("{}", page_idx + 1);
            let mut page_num_buffer =
                TextBuffer::new(&mut self.font_system, Metrics::new(12.0, 14.0));
            page_num_buffer.set_size(&mut self.font_system, Some(50.0), Some(20.0));
            page_num_buffer.set_text(
                &mut self.font_system,
                &page_num_text,
                &Attrs::new().family(Family::SansSerif),
                Shaping::Advanced,
            );
            page_num_buffer.shape_until_scroll(&mut self.font_system, false);

            // Position page number at top right
            let page_num_x = content_right - 20.0;
            let page_num_y = page_y + margin_top_px / 2.0;
            text_buffers.push((page_num_buffer, page_num_x, page_num_y, 1.0, text_black, false));

            // Draw footer "Created With FastTrackStudio" at fixed position at bottom of page
            let footer_text = "Created With FastTrackStudio";
            let mut footer_buffer =
                TextBuffer::new(&mut self.font_system, Metrics::new(10.0, 12.0));
            footer_buffer.set_size(&mut self.font_system, Some(250.0), Some(20.0));
            footer_buffer.set_text(
                &mut self.font_system,
                footer_text,
                &Attrs::new().family(Family::SansSerif),
                Shaping::Advanced,
            );
            footer_buffer.shape_until_scroll(&mut self.font_system, false);

            // Position footer at fixed location from page bottom (in bottom margin area)
            let footer_y = page_y + page_height - margin_bottom_px + 5.0;
            let footer_width: f32 = footer_buffer.layout_runs().map(|run| run.line_w).next().unwrap_or(180.0);
            let footer_x = page_x + (page_width - footer_width) / 2.0;
            text_buffers.push((footer_buffer, footer_x, footer_y, 1.0, text_gray, false));

            // ================================================================
            // DEBUG LAYOUT VISUALIZATION (LilyPond annotate-spacing style)
            // ================================================================
            if debug_layout {
                // Calculate positions
                let header_height_px = if page_idx == 0 && score_header.has_content() {
                    header_layout.frame_height
                } else {
                    0.0
                };
                let first_system_y = content_top + header_height_px + top_padding;
                let systems_count = page_info.system_count;

                // Calculate where last system ends
                let last_system_bottom = if systems_count > 0 {
                    first_system_y + staff_height + (systems_count - 1) as f32 * (staff_height + system_spacing)
                } else {
                    first_system_y
                };

                // ---- CONTENT AREA BOUNDARY ----
                let border_color = [0.0, 0.6, 0.0, 0.6];
                vertices.extend(create_line(content_left, content_top, content_right, content_top, 1.0, border_color, w, h));
                vertices.extend(create_line(content_left, content_bottom, content_right, content_bottom, 1.0, border_color, w, h));
                vertices.extend(create_line(content_left, content_top, content_left, content_bottom, 1.0, border_color, w, h));
                vertices.extend(create_line(content_right, content_top, content_right, content_bottom, 1.0, border_color, w, h));

                // ---- STAFF HEIGHT & SPACING ARROWS (on each system) ----
                let arrow_x = content_left + staff_space * 6.0; // After clef area
                let blue = [0.0, 0.0, 0.8, 0.8];
                let magenta = [0.8, 0.0, 0.5, 0.8];

                let mut sys_y = first_system_y;
                for i in 0..systems_count {
                    let staff_top = sys_y;
                    let staff_bot = sys_y + staff_height;

                    // Staff height arrow
                    vertices.extend(create_line(arrow_x, staff_top, arrow_x, staff_bot, 1.0, blue, w, h));
                    vertices.extend(create_line(arrow_x, staff_top, arrow_x - 3.0, staff_top + 4.0, 1.0, blue, w, h));
                    vertices.extend(create_line(arrow_x, staff_top, arrow_x + 3.0, staff_top + 4.0, 1.0, blue, w, h));
                    vertices.extend(create_line(arrow_x, staff_bot, arrow_x - 3.0, staff_bot - 4.0, 1.0, blue, w, h));
                    vertices.extend(create_line(arrow_x, staff_bot, arrow_x + 3.0, staff_bot - 4.0, 1.0, blue, w, h));

                    // Staff label (compact, just the number)
                    let mut buf = TextBuffer::new(&mut self.font_system, Metrics::new(7.0, 8.0));
                    buf.set_size(&mut self.font_system, Some(30.0), Some(9.0));
                    buf.set_text(&mut self.font_system, &format!("{:.0}", page_style.staff.staff_height),
                        &Attrs::new().family(Family::Monospace), Shaping::Advanced);
                    buf.shape_until_scroll(&mut self.font_system, false);
                    text_buffers.push((buf, arrow_x + 4.0, (staff_top + staff_bot) / 2.0 - 3.0, 1.0, text_gray, false));

                    // System spacing arrow (between this and next system)
                    if i < systems_count - 1 {
                        let spacing_arrow_x = arrow_x + staff_space * 2.5;
                        let spacing_top = staff_bot;
                        let spacing_bot = staff_bot + system_spacing;

                        vertices.extend(create_line(spacing_arrow_x, spacing_top, spacing_arrow_x, spacing_bot, 1.0, magenta, w, h));
                        vertices.extend(create_line(spacing_arrow_x, spacing_top, spacing_arrow_x - 3.0, spacing_top + 4.0, 1.0, magenta, w, h));
                        vertices.extend(create_line(spacing_arrow_x, spacing_top, spacing_arrow_x + 3.0, spacing_top + 4.0, 1.0, magenta, w, h));
                        vertices.extend(create_line(spacing_arrow_x, spacing_bot, spacing_arrow_x - 3.0, spacing_bot - 4.0, 1.0, magenta, w, h));
                        vertices.extend(create_line(spacing_arrow_x, spacing_bot, spacing_arrow_x + 3.0, spacing_bot - 4.0, 1.0, magenta, w, h));

                        // Spacing label
                        let mut buf = TextBuffer::new(&mut self.font_system, Metrics::new(7.0, 8.0));
                        buf.set_size(&mut self.font_system, Some(30.0), Some(9.0));
                        buf.set_text(&mut self.font_system, &format!("{:.0}", page_style.system_spacing.system_to_system),
                            &Attrs::new().family(Family::Monospace), Shaping::Advanced);
                        buf.shape_until_scroll(&mut self.font_system, false);
                        text_buffers.push((buf, spacing_arrow_x + 4.0, (spacing_top + spacing_bot) / 2.0 - 3.0, 1.0, text_gray, false));
                    }

                    // Move to next system
                    sys_y = staff_bot + system_spacing;
                }

                // ---- SPACE LEFT INDICATOR ----
                if last_system_bottom < content_bottom - 5.0 {
                    let space_left_px = content_bottom - last_system_bottom;
                    let space_left_pt = space_left_px / PT_TO_PX;
                    let arrow_x = content_right - 20.0;
                    let red = [0.8, 0.0, 0.0, 0.8];

                    vertices.extend(create_line(arrow_x, last_system_bottom, arrow_x, content_bottom, 1.5, red, w, h));
                    vertices.extend(create_line(arrow_x, last_system_bottom, arrow_x - 4.0, last_system_bottom + 6.0, 1.5, red, w, h));
                    vertices.extend(create_line(arrow_x, last_system_bottom, arrow_x + 4.0, last_system_bottom + 6.0, 1.5, red, w, h));
                    vertices.extend(create_line(arrow_x, content_bottom, arrow_x - 4.0, content_bottom - 6.0, 1.5, red, w, h));
                    vertices.extend(create_line(arrow_x, content_bottom, arrow_x + 4.0, content_bottom - 6.0, 1.5, red, w, h));

                    let mut buf = TextBuffer::new(&mut self.font_system, Metrics::new(10.0, 12.0));
                    buf.set_size(&mut self.font_system, Some(60.0), Some(14.0));
                    buf.set_text(&mut self.font_system, &format!("{:.0}pt", space_left_pt),
                        &Attrs::new().family(Family::Monospace), Shaping::Advanced);
                    buf.shape_until_scroll(&mut self.font_system, false);
                    text_buffers.push((buf, arrow_x - 45.0, (last_system_bottom + content_bottom) / 2.0 - 5.0, 1.0, text_gray, false));
                }

                // ---- SUMMARY ----
                let space_left_pt = if last_system_bottom < content_bottom {
                    (content_bottom - last_system_bottom) / PT_TO_PX
                } else { 0.0 };

                let summary = format!("p{} | {}sys | {:.0}pt free",
                    page_idx + 1, page_info.system_count, space_left_pt);
                let mut buf = TextBuffer::new(&mut self.font_system, Metrics::new(9.0, 11.0));
                buf.set_size(&mut self.font_system, Some(200.0), Some(12.0));
                buf.set_text(&mut self.font_system, &summary,
                    &Attrs::new().family(Family::Monospace), Shaping::Advanced);
                buf.shape_until_scroll(&mut self.font_system, false);
                text_buffers.push((buf, content_left, content_bottom + 3.0, 1.0, text_gray, false));
            }
        }

        // NOTE: FPS counter is rendered separately in render() since it changes every frame
        // and shouldn't be part of the cached scene

        (vertices, sdf_vertices, text_buffers)
    }
}

// ============================================================================
// Texture Creation
// ============================================================================

fn create_texture(device: &wgpu::Device, width: u32, height: u32) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Chart Texture"),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8Unorm,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT
            | wgpu::TextureUsages::TEXTURE_BINDING
            | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    })
}
