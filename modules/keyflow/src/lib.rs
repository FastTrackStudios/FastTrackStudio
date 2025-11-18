//! Keyflow - Musical Chart Parser
//!
//! A trait-based system for parsing and manipulating musical charts

// Core modules - new trait-based architecture
pub mod chart;
pub mod chord;
pub mod key;
pub mod metadata;
pub mod parsing;
pub mod primitives;
pub mod sections;
pub mod time;

// Re-exports for convenience
pub use chart::{Chart, ChartSection, ChordInstance, KeyChange, Measure};
pub use chord::{
    parse_root, Alteration, Chord, ChordDegree, ChordFamily, ChordParseError, ChordParseErrors,
    ChordQuality, ChordRhythm, ExtensionQuality, Extensions, LilySyntax, PushPullAmount,
    RootParseResult, SuspendedType,
};
pub use key::{Key, ScaleMode, ScaleType};
pub use metadata::SongMetadata;
pub use parsing::{Lexer, ParseError, Token, TokenType};
pub use primitives::{
    Interval, MusicalNote, MusicalNoteToken, Note, RomanCase, RomanNumeralToken, RootFormat,
    RootNotation, ScaleDegreeToken,
};
pub use sections::{Section, SectionNumberer, SectionType};
pub use time::{AbsolutePosition, Duration, MusicalDuration, Position, Tempo, TimeSignature};
