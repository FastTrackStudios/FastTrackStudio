//! Keyflow - Musical Chart Parser
//!
//! A trait-based system for parsing and manipulating musical charts.
//!
//! ## Features
//!
//! - `highlighting`: Syntax highlighting using the parser's AST spans
//! - `engraver`: GPU-accelerated music notation rendering (requires wgpu, vello)
//! - `engraver-example`: Example dependencies for running engraver examples
//! - `midi-import`: MIDI file import support

// region:    --- Modules

mod error;
pub mod ast;
pub mod chart;
pub mod chord;
pub mod core;
pub mod key;
pub mod metadata;
pub mod parsing;
pub mod patterns;
pub mod primitives;
pub mod sections;
pub mod time;

// Syntax highlighting module - parser-integrated highlighting
#[cfg(feature = "highlighting")]
pub mod highlighting;

// Engraver module - GPU-accelerated music notation rendering
#[cfg(feature = "engraver")]
pub mod engraver;

pub use error::{Error, Result};

// endregion: --- Modules

// region:    --- Re-exports

// -- Chart
pub use chart::{
    Chart, ChartIndex, ChartPosition, ChartSection, ChordInstance, DynamicMarking, ElementId,
    KeyChange, Measure, NavigationType, SemanticRole, SourceLink, TempoChange,
};

// -- Chord
pub use chord::{
    Alteration, Chord, ChordDegree, ChordFamily, ChordParseError, ChordParseErrors, ChordQuality,
    ChordRhythm, DetailLevel, ExtensionQuality, Extensions, LilySyntax, PushPullAmount,
    RootParseResult, SuspendedType, UpperStructure, parse_root,
};

// -- Key
pub use key::{Key, ScaleMode, ScaleType};

// -- Metadata
pub use metadata::SongMetadata;

// -- Parsing
pub use parsing::{Lexer, ParseError, TextSpan, Token, TokenType};

// -- Primitives
pub use primitives::{
    Interval, MusicalNote, MusicalNoteToken, Note, RomanCase, RomanNumeralToken, RootFormat,
    RootNotation, ScaleDegreeToken,
};

// -- Sections
pub use sections::{Section, SectionNumberer, SectionType};

// -- Time
pub use time::{
    AbsolutePosition, Duration, DurationTrait, MusicalDuration, MusicalPosition, PPQDuration,
    PPQPosition, Position, Tempo, TimeDuration, TimePosition, TimeSignature,
};

// endregion: --- Re-exports
