//! Unified color palette library for audio production applications.
//!
//! This crate provides:
//! - A unified [`Color`] type with conversions between formats
//! - A comprehensive Tailwind-style [`palette`] with 22 color families
//! - Semantic color mappings for [`instruments`] and song [`sections`]
//!
//! # Quick Start
//!
//! ```rust
//! use color_palette::{Color, palette, instruments, sections};
//!
//! // Create colors in various ways
//! let blue = Color::hex(0x3B82F6);
//! let red = Color::rgb(239, 68, 68);
//! let green = Color::from_hex_str("#22C55E").unwrap();
//!
//! // Use the Tailwind palette
//! let sky_blue = palette::sky::S500;
//! let dark_purple = palette::purple::S800;
//!
//! // Get semantic colors for instruments
//! let kick_color = instruments::drums::KICK;
//! let vocal_color = instruments::color_for_group("Vocals");
//!
//! // Get semantic colors for song sections
//! let verse_colors = sections::section_colors(sections::SectionType::Verse);
//! println!("Verse bright: {}", verse_colors.bright_css());
//! ```
//!
//! # Color Formats
//!
//! The [`Color`] type can be converted to/from:
//! - Hex integers: `0xRRGGBB`
//! - Hex strings: `"#rrggbb"` or `"#rgb"`
//! - RGB tuples: `(r, g, b)`
//! - CSS strings: `"rgb(r, g, b)"` or `"rgba(r, g, b, a)"`
//! - REAPER native format (platform-specific)
//!
//! # Palette Organization
//!
//! The palette is organized into color families, each with 11 shades:
//! - **50**: Lightest
//! - **100-400**: Light shades
//! - **500**: Base/default shade
//! - **600-900**: Dark shades
//! - **950**: Darkest
//!
//! Color families include: red, orange, amber, yellow, lime, green, emerald,
//! teal, cyan, sky, blue, indigo, violet, purple, fuchsia, pink, rose,
//! slate, gray, zinc, neutral, stone.

mod color;
pub mod palette;
pub mod semantic;

pub use color::{Color, ColorParseError};
pub use semantic::instruments;
pub use semantic::sections;

// Re-export commonly used items at crate root
pub use semantic::instruments::{color_for_group, color_for_instrument, color_for_path};
pub use semantic::sections::{section_colors, SectionColors, SectionType};
