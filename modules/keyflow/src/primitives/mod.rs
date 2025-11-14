//! Musical Primitives
//!
//! Core musical building blocks: notes, intervals, root notations, and tokens

pub mod accidental;
pub mod note;
pub mod roman_numeral;
pub mod scale_degree;
pub mod root_notation;
pub mod interval;
pub mod tokens;

pub use accidental::{WithAccidental, Accidental, AccidentalType};
pub use note::{Note, MusicalNote, MusicalNoteToken};
pub use roman_numeral::RomanNumeralToken;
pub use scale_degree::ScaleDegreeToken;
pub use root_notation::{RootNotation, RootFormat, RomanCase};
pub use interval::Interval;
pub use tokens::MusicalToken;

