//! Scale AST for parsing scale definitions

use crate::primitives::note::Note;
use super::expressions::ScaleExpression;
use super::parser_error::ScaleParserError;
use crate::parsing::common::{ParserError, ParserErrors};

/// Scale AST node
#[derive(Debug, Clone)]
pub struct ScaleAST {
    pub root: Option<Note>,
    pub scale_type: Option<String>,
    pub custom_semitones: Option<Vec<u8>>,
    pub expressions: Vec<ScaleExpression>,
}

impl ScaleAST {
    pub fn new() -> Self {
        ScaleAST {
            root: None,
            scale_type: None,
            custom_semitones: None,
            expressions: Vec::new(),
        }
    }

    pub fn set_root(&mut self, root: Note) {
        self.root = Some(root);
    }

    pub fn set_scale_type(&mut self, scale_type: String) {
        self.scale_type = Some(scale_type);
    }

    pub fn set_custom_semitones(&mut self, semitones: Vec<u8>) {
        self.custom_semitones = Some(semitones);
    }

    pub fn add_expression(&mut self, expr: ScaleExpression) {
        self.expressions.push(expr);
    }

    pub fn is_valid(&self) -> bool {
        self.root.is_some() && (self.scale_type.is_some() || self.custom_semitones.is_some())
    }

    pub fn build_scale(&mut self) -> Result<crate::key::keys::Key, ParserErrors> {
        // For now, return a placeholder
        // TODO: Implement actual scale building logic
        if let Some(root) = self.root {
            Ok(crate::key::keys::Key::new(root, crate::key::keys::ScaleType::Diatonic, crate::key::keys::ScaleMode::Ionian))
        } else {
            Err(ParserErrors::new(vec![ParserError::Scale(ScaleParserError::MissingScaleDefinition)]))
        }
    }
}
