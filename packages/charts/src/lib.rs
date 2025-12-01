// Charts package - UI components and LilyPond integration
//
// This package provides UI components and LilyPond document generation.
// All musical parsing and representation is handled by the keyflow package.

pub mod parsers;
pub mod lilypond_builder;
pub mod url_codec;

// Re-export charts-specific functionality
pub use parsers::text_cues;
