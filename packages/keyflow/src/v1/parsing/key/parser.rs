//! Key signature parser implementation

use crate::parsing::common::{ParserError, ParserErrors, Token, TokenType};
use crate::parsing::common::parser::MusicalParser;
use crate::primitives::note::{Note, NoteLiteral, Modifier};
use crate::key::keys::Key;
use super::expressions::KeyAST;
use super::lexer::KeyLexer;

pub struct KeyParser {
    lexer: KeyLexer,
    ast: KeyAST,
    errors: Vec<ParserError>,
}

impl KeyParser {
    pub fn new() -> Self {
        Self {
            lexer: KeyLexer::new(),
            ast: KeyAST::new(),
            errors: Vec::new(),
        }
    }
    
    /// Parse a key signature string like "C Major", "A Minor", "D Dorian", "C [0,2,4,5,7,9,11]"
    pub fn parse(&mut self, input: &str) -> Result<Key, ParserErrors> {
        let tokens = self.lexer.scan_tokens(input);
        let mut token_iter = tokens.iter().peekable();
        
        // Parse root note
        self.parse_root(&mut token_iter);
        
        // Parse scale information
        self.parse_scale(&mut token_iter);
        
        // Check for errors
        if !self.errors.is_empty() || !self.ast.errors.is_empty() {
            let mut all_errors = self.errors.clone();
            all_errors.extend(self.ast.errors.clone());
            return Err(ParserErrors::new(all_errors));
        }
        
        // Build the key
        let result = self.ast.build_key();
        
        // Cleanup
        self.cleanup();
        
        result
    }
    
    fn parse_root(&mut self, tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) {
        if let Some(token) = tokens.next() {
            match &token.token_type {
                TokenType::Note(note_str) => {
                    if let Some(note) = self.parse_note_string(note_str) {
                        self.ast.root = Some(note);
                    } else {
                        self.add_error(ParserError::Common(crate::parsing::common::CommonParserError::InvalidNoteName(note_str.clone())));
                    }
                },
                _ => {
                    self.add_error(ParserError::Common(crate::parsing::common::CommonParserError::MissingRootNote));
                }
            }
        } else {
            self.add_error(ParserError::Common(crate::parsing::common::CommonParserError::MissingRootNote));
        }
    }
    
    fn parse_scale(&mut self, tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) {
        // Check if we have a custom scale (starts with [)
        if let Some(token) = tokens.peek() {
            if matches!(token.token_type, TokenType::LBracket) {
                self.parse_custom_scale(tokens);
                return;
            }
        }
        
        // Parse scale name
        let mut scale_name_parts = Vec::new();
        while let Some(token) = tokens.next() {
            match &token.token_type {
                TokenType::Eof => break,
                TokenType::ScaleName(name) => {
                    scale_name_parts.push(name.clone());
                },
                _ => {
                    // Skip other tokens like spaces, etc.
                }
            }
        }
        
        if scale_name_parts.is_empty() {
            self.add_error(ParserError::Key(crate::parsing::key::KeyParserError::MissingScaleType));
            return;
        }
        
        let scale_string = scale_name_parts.join(" ");
        self.ast.parse_scale_name(&scale_string);
    }
    
    fn parse_custom_scale(&mut self, tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) {
        // Expect opening bracket
        if let Some(token) = tokens.next() {
            if !matches!(token.token_type, TokenType::LBracket) {
                self.add_error(ParserError::Key(crate::parsing::key::KeyParserError::InvalidCustomScale("Expected [".to_string())));
                return;
            }
        }
        
        let mut semitones = Vec::new();
        
        // Parse semitone values
        while let Some(token) = tokens.next() {
            match &token.token_type {
                TokenType::RBracket => break,
                TokenType::Extension(num_str) => {
                    if let Ok(num) = num_str.parse::<u8>() {
                        semitones.push(num);
                    } else {
                        self.add_error(ParserError::Key(crate::parsing::key::KeyParserError::InvalidCustomScale(format!("Invalid number: {}", num_str))));
                        return;
                    }
                },
                TokenType::Comma => {
                    // Skip commas
                },
                _ => {
                    self.add_error(ParserError::Key(crate::parsing::key::KeyParserError::InvalidCustomScale(format!("Unexpected token: {:?}", token.token_type))));
                    return;
                }
            }
        }
        
        if semitones.is_empty() {
            self.add_error(ParserError::Key(crate::parsing::key::KeyParserError::EmptyCustomScale));
        } else {
            self.ast.custom_scale = Some(semitones);
        }
    }
    
    fn parse_note_string(&self, note_str: &str) -> Option<Note> {
        if note_str.is_empty() {
            return None;
        }
        
        let mut chars = note_str.chars();
        let first_char = chars.next()?;
        
        // Parse note literal
        let literal = match first_char.to_ascii_uppercase() {
            'C' => NoteLiteral::C,
            'D' => NoteLiteral::D,
            'E' => NoteLiteral::E,
            'F' => NoteLiteral::F,
            'G' => NoteLiteral::G,
            'A' => NoteLiteral::A,
            'B' => NoteLiteral::B,
            _ => return None,
        };
        
        // Parse modifier
        let modifier = if let Some(next_char) = chars.next() {
            match next_char {
                '#' | '♯' => Some(Modifier::Sharp),
                'b' | '♭' => Some(Modifier::Flat),
                _ => None,
            }
        } else {
            None
        };
        
        Some(Note::new(literal, modifier))
    }
    
    fn add_error(&mut self, error: ParserError) {
        self.errors.push(error);
    }
    
    fn cleanup(&mut self) {
        self.errors.clear();
        self.ast = KeyAST::new();
    }
}

impl MusicalParser<Key> for KeyParser {
    fn parse(&mut self, input: &str) -> Result<Key, ParserErrors> {
        self.parse(input)
    }
    
    fn errors(&self) -> &[ParserError] {
        &self.errors
    }
    
    fn clear_errors(&mut self) {
        self.errors.clear();
    }
}
