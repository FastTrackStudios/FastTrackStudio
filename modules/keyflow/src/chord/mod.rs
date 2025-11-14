//! Chord system
//!
//! Defines chords with quality, family, extensions, alterations, and parsing

pub mod quality;
pub mod family;
pub mod degree;
pub mod alteration;
pub mod extensions;
pub mod root;
pub mod duration;
pub mod chord;
pub mod error;
pub mod semitone_sequence;

pub use quality::{ChordQuality, SuspendedType};
pub use family::ChordFamily;
pub use degree::ChordDegree;
pub use alteration::Alteration;
pub use extensions::{Extensions, ExtensionQuality};
pub use root::{parse_root, RootParseResult};
pub use duration::{ChordRhythm, LilySyntax, PushPullAmount};
pub use chord::Chord;
pub use error::{ChordParseError, ChordParseErrors};
pub use semitone_sequence::{from_semitones, quality_from_semitones, SemitoneSequenceError};
