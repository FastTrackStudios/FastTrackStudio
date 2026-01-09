// Charts package - UI components and LilyPond integration
//
// This package provides UI components and LilyPond document generation.
// All musical parsing and representation is handled by the keyflow package.

pub mod keyflow_to_lilypond;
pub mod lilypond_builder;
pub mod parsers;
pub mod url_codec;

// Re-export charts-specific functionality
pub use keyflow_to_lilypond::{chart_to_lilypond, render_chart_to_pdf};
pub use parsers::text_cues;
