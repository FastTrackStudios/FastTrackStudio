//! Scale expressions for parsing

use crate::primitives::note::Note;

/// Scale expression types
#[derive(Debug, Clone)]
pub enum ScaleExpression {
    /// Root note
    Root(Note),
    /// Scale type (Major, Minor, Dorian, etc.)
    ScaleType(String),
    /// Custom semitone sequence
    CustomSemitones(Vec<u8>),
}

impl ScaleExpression {
    pub fn root(note: Note) -> Self {
        ScaleExpression::Root(note)
    }

    pub fn scale_type(scale_type: String) -> Self {
        ScaleExpression::ScaleType(scale_type)
    }

    pub fn custom_semitones(semitones: Vec<u8>) -> Self {
        ScaleExpression::CustomSemitones(semitones)
    }
}
