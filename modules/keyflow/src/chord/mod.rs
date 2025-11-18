//! Chord system
//!
//! Defines chords with quality, family, extensions, alterations, and parsing

pub mod alteration;
pub mod chord;
pub mod degree;
pub mod duration;
pub mod error;
pub mod extensions;
pub mod family;
pub mod quality;
pub mod root;
pub mod semitone_sequence;

pub use alteration::Alteration;
pub use chord::Chord;
pub use degree::ChordDegree;
pub use duration::{ChordRhythm, LilySyntax, PushPullAmount};
pub use error::{ChordParseError, ChordParseErrors};
pub use extensions::{ExtensionQuality, Extensions};
pub use family::ChordFamily;
pub use quality::{ChordQuality, SuspendedType};
pub use root::{parse_root, RootParseResult};
pub use semitone_sequence::{from_semitones, quality_from_semitones, SemitoneSequenceError};
