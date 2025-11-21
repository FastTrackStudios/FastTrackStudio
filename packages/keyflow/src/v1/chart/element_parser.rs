// Temporary file showing how chart parser should orchestrate mini-parsers
// This demonstrates the architecture without breaking existing code

use crate::chord::chord::ChordData;
use crate::key::keys::Key;
use crate::parsing::common::{Lexer, TokenStream, TokenType};
use crate::parsing::chord::ChordMiniParser;
use crate::parsing::key::KeySignatureMiniParser;
use crate::parsing::{MiniParser, ParseResult};

/// Represents different types of musical elements that can appear in a chart
#[derive(Debug, Clone)]
pub enum ChartElement {
    Chord(ChordData),
    KeyChange(Key),
    Rest(String),   // Duration notation like "r4", "r2"
    Space(String),  // Duration notation like "s2", "s1"
}

/// Orchestrator for parsing chart line elements
pub struct ChartElementParser {
    common_lexer: Lexer,
    chord_parser: ChordMiniParser,
    key_parser: KeySignatureMiniParser,
}

impl ChartElementParser {
    pub fn new() -> Self {
        ChartElementParser {
            common_lexer: Lexer::new(),
            chord_parser: ChordMiniParser::new(),
            key_parser: KeySignatureMiniParser::new(),
        }
    }

    /// Parse a line from the chart and return a list of elements
    pub fn parse_line(&mut self, line: &str) -> Result<Vec<ChartElement>, String> {
        // Tokenize the line with the common lexer
        let tokens = self.common_lexer.scan_tokens(line);
        let mut stream = TokenStream::new(tokens);
        let mut elements = Vec::new();

        while !stream.is_empty() {
            // Skip whitespace
            stream.skip_whitespace();
            
            if stream.is_empty() {
                break;
            }

            // Try to parse the next element
            let element = self.parse_next_element(&mut stream)?;
            elements.push(element);
        }

        Ok(elements)
    }

    /// Parse the next element from the token stream
    fn parse_next_element(&mut self, stream: &mut TokenStream) -> Result<ChartElement, String> {
        // Check for key signature context
        if self.key_parser.can_parse(stream) {
            return self.parse_key_signature(stream);
        }

        // Check for rest/space notation
        if Self::looks_like_rest_or_space(stream) {
            return self.parse_rest_or_space(stream);
        }

        // Try chord parsing
        self.parse_chord(stream)
    }

    /// Parse a key signature
    fn parse_key_signature(&mut self, stream: &mut TokenStream) -> Result<ChartElement, String> {
        match self.key_parser.parse(stream) {
            ParseResult::Success(key, _) => Ok(ChartElement::KeyChange(key)),
            ParseResult::Error(e) => Err(format!("Key signature parse error: {}", e)),
            ParseResult::Escape(_) => Err("Unexpected escape from key parser".to_string()),
        }
    }

    /// Parse a chord
    fn parse_chord(&mut self, stream: &mut TokenStream) -> Result<ChartElement, String> {
        match self.chord_parser.parse(stream) {
            ParseResult::Success(chord, _) => Ok(ChartElement::Chord(chord)),
            ParseResult::Escape(tokens) => {
                // Chord parser hit `//` or similar escape sequence
                // This means we need to handle duration syntax
                // For now, return an error - the full integration will handle this
                Err(format!("Duration syntax not yet integrated (escaped with {} tokens)", tokens.len()))
            }
            ParseResult::Error(e) => Err(format!("Chord parse error: {}", e)),
        }
    }

    /// Check if token stream looks like rest or space notation (r4, s2, etc.)
    fn looks_like_rest_or_space(stream: &TokenStream) -> bool {
        match (stream.peek(), stream.peek_n(1)) {
            (Some(first), Some(second)) => {
                // Pattern: letter 'r' or 's' followed by a digit or number
                matches!(first.token_type, TokenType::Letter('r') | TokenType::Letter('s'))
                    && (matches!(second.token_type, TokenType::Digit(_)) 
                        || matches!(second.token_type, TokenType::Number(_)))
            }
            _ => false,
        }
    }

    /// Parse rest or space notation
    fn parse_rest_or_space(&mut self, stream: &mut TokenStream) -> Result<ChartElement, String> {
        let type_token = stream.consume().ok_or("Expected rest/space type")?;
        let is_rest = matches!(type_token.token_type, TokenType::Letter('r'));

        // Collect the duration part (digits)
        let mut duration = String::new();
        match stream.peek() {
            Some(token) => match &token.token_type {
                TokenType::Digit(d) => {
                    duration.push(*d);
                    stream.consume();
                }
                TokenType::Number(num) => {
                    duration.push_str(num);
                    stream.consume();
                }
                _ => return Err("Expected duration after rest/space type".to_string()),
            },
            None => return Err("Expected duration after rest/space type".to_string()),
        }

        let full_notation = format!("{}{}", if is_rest { "r" } else { "s" }, duration);
        
        if is_rest {
            Ok(ChartElement::Rest(full_notation))
        } else {
            Ok(ChartElement::Space(full_notation))
        }
    }
}

impl Default for ChartElementParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_chord() {
        let mut parser = ChartElementParser::new();
        let elements = parser.parse_line("C").unwrap();
        assert_eq!(elements.len(), 1);
        assert!(matches!(elements[0], ChartElement::Chord(_)));
    }

    #[test]
    fn test_parse_multiple_chords() {
        let mut parser = ChartElementParser::new();
        let elements = parser.parse_line("C F G C").unwrap();
        assert_eq!(elements.len(), 4);
    }

    #[test]
    fn test_parse_key_signature() {
        let mut parser = ChartElementParser::new();
        let elements = parser.parse_line("#G").unwrap();
        assert_eq!(elements.len(), 1);
        assert!(matches!(elements[0], ChartElement::KeyChange(_)));
    }

    #[test]
    fn test_parse_rest() {
        let mut parser = ChartElementParser::new();
        let elements = parser.parse_line("r4").unwrap();
        assert_eq!(elements.len(), 1);
        assert!(matches!(elements[0], ChartElement::Rest(_)));
    }

    #[test]
    fn test_parse_mixed() {
        let mut parser = ChartElementParser::new();
        let elements = parser.parse_line("C r4 G").unwrap();
        assert_eq!(elements.len(), 3);
        assert!(matches!(elements[0], ChartElement::Chord(_)));
        assert!(matches!(elements[1], ChartElement::Rest(_)));
        assert!(matches!(elements[2], ChartElement::Chord(_)));
    }
}

