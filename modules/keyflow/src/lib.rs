//! Keyflow - Musical Chart Parser
//!
//! A trait-based system for parsing and manipulating musical charts

// Core modules - new trait-based architecture
pub mod time;
pub mod primitives;
pub mod key;
pub mod chord;
pub mod parsing;
pub mod metadata;
pub mod sections;
pub mod chart;

// Re-exports for convenience
pub use time::{Duration, MusicalDuration, Position, AbsolutePosition, TimeSignature, Tempo};
pub use primitives::{
    Note, MusicalNote, MusicalNoteToken, 
    RomanNumeralToken, ScaleDegreeToken,
    RootNotation, RootFormat, RomanCase, Interval
};
pub use key::{Key, ScaleType, ScaleMode};
pub use chord::{
    Chord, ChordQuality, SuspendedType, ChordFamily, ChordDegree, 
    Alteration, Extensions, ExtensionQuality,
    ChordRhythm, LilySyntax, PushPullAmount, 
    parse_root, RootParseResult,
    ChordParseError, ChordParseErrors
};
pub use parsing::{Lexer, Token, TokenType, ParseError};
pub use metadata::SongMetadata;
pub use sections::{Section, SectionType, SectionNumberer};
pub use chart::{Chart, ChordInstance, Measure, ChartSection, KeyChange};

