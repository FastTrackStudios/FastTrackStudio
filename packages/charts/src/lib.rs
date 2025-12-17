// Charts package - UI components and LilyPond integration
//
// This package provides UI components and LilyPond document generation.
// All musical parsing and representation is handled by the keyflow package.

pub mod parsers;
pub mod lilypond_builder;
pub mod url_codec;
pub mod keyflow_to_lilypond;

// Re-export charts-specific functionality
pub use parsers::text_cues;
pub use keyflow_to_lilypond::{chart_to_lilypond, render_chart_to_pdf};
