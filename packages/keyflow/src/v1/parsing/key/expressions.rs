//! Key-specific expressions for parsing

use crate::primitives::note::Note;
use crate::key::keys::{ScaleType, ScaleMode, MelodicMinorMode, HarmonicMinorMode};

#[derive(Debug, Clone, PartialEq)]
pub enum KeyExpression {
    /// Root note
    Root(Note),
    /// Scale type (Major, Minor, etc.)
    ScaleType(ScaleType),
    /// Scale mode (Ionian, Dorian, etc.)
    ScaleMode(ScaleMode),
    /// Melodic minor mode
    MelodicMinorMode(MelodicMinorMode),
    /// Harmonic minor mode  
    HarmonicMinorMode(HarmonicMinorMode),
    /// Custom semitone sequence
    CustomScale(Vec<u8>),
    /// Key signature modifiers
    Modifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyAST {
    pub root: Option<Note>,
    pub scale_type: Option<ScaleType>,
    pub scale_mode: Option<ScaleMode>,
    pub melodic_minor_mode: Option<MelodicMinorMode>,
    pub harmonic_minor_mode: Option<HarmonicMinorMode>,
    pub custom_scale: Option<Vec<u8>>,
    pub expressions: Vec<KeyExpression>,
    pub errors: Vec<crate::parsing::common::ParserError>,
}

impl KeyAST {
    pub fn new() -> Self {
        Self {
            root: None,
            scale_type: None,
            scale_mode: None,
            melodic_minor_mode: None,
            harmonic_minor_mode: None,
            custom_scale: None,
            expressions: Vec::new(),
            errors: Vec::new(),
        }
    }
    
    pub fn add_expression(&mut self, expr: KeyExpression) {
        self.expressions.push(expr);
    }
    
    pub fn add_error(&mut self, error: crate::parsing::common::ParserError) {
        self.errors.push(error);
    }
    
    pub fn is_valid(&self) -> bool {
        self.root.is_some() && (
            self.scale_type.is_some() || 
            self.scale_mode.is_some() || 
            self.melodic_minor_mode.is_some() || 
            self.harmonic_minor_mode.is_some() || 
            self.custom_scale.is_some()
        ) && self.errors.is_empty()
    }
}
