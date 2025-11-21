use crate::key::keys::Key;
use crate::parsing::{ParseResult, MiniParser};
use crate::parsing::common::{TokenStream, TokenType};

/// Context-aware key signature mini-parser
///
/// Parses key signatures like:
/// - `#G` - G major (sharp prefix)
/// - `#C#` - C# major
/// - `bEb` - Eb major (flat prefix)
/// - `#Amin` - A minor
///
/// The parser only activates when the orchestrator detects key signature context
/// (e.g., at the start of a metadata line or after chord content).
pub struct KeySignatureMiniParser;

impl KeySignatureMiniParser {
    pub fn new() -> Self {
        KeySignatureMiniParser
    }

    /// Check if the token stream starts with a key signature pattern
    fn looks_like_key_signature(stream: &TokenStream) -> bool {
        // Pattern: # or b, followed by a letter
        match (stream.peek(), stream.peek_n(1)) {
            (Some(first), Some(second)) => {
                matches!(first.token_type, TokenType::Sharp | TokenType::Flat)
                    && matches!(second.token_type, TokenType::Letter(_))
            }
            _ => false,
        }
    }

    /// Parse the key note (e.g., "C#", "Eb")
    fn parse_key_note(&self, stream: &mut TokenStream) -> Result<String, String> {
        let mut key_note = String::new();

        // First token should be # or b (key indicator)
        match stream.peek() {
            Some(token) if matches!(token.token_type, TokenType::Sharp | TokenType::Flat) => {
                if matches!(token.token_type, TokenType::Sharp) {
                    key_note.push('#');
                } else {
                    key_note.push('b');
                }
                stream.consume();
            }
            _ => return Err("Expected # or b at start of key signature".to_string()),
        }

        // Next token should be a note letter (A-G)
        match stream.peek() {
            Some(token) => match token.token_type {
                TokenType::Letter(c) if Self::is_note_letter(c) => {
                    key_note.push(c.to_uppercase().next().unwrap());
                    stream.consume();
                }
                _ => return Err("Expected note letter in key signature".to_string()),
            },
            None => return Err("Unexpected end after key indicator".to_string()),
        }

        // Check for additional sharp/flat modifier on the note
        match stream.peek() {
            Some(token) if matches!(token.token_type, TokenType::Sharp) => {
                key_note.push('#');
                stream.consume();
            }
            Some(token) if matches!(token.token_type, TokenType::Flat) => {
                key_note.push('b');
                stream.consume();
            }
            Some(token) if matches!(token.token_type, TokenType::Letter('b')) => {
                // Could be flat modifier
                key_note.push('b');
                stream.consume();
            }
            _ => {}
        }

        Ok(key_note)
    }

    /// Parse optional mode (min, minor, maj, major)
    fn parse_mode(&self, stream: &mut TokenStream) -> Option<String> {
        let checkpoint = stream.checkpoint();
        let mut mode = String::new();

        // Collect letters that might be a mode
        while let Some(token) = stream.peek() {
            match &token.token_type {
                TokenType::Letter(c) => {
                    mode.push(*c);
                    stream.consume();
                }
                _ => break,
            }
        }

        // Check if it's a valid mode
        let mode_lower = mode.to_lowercase();
        if mode_lower == "min" || mode_lower == "minor" || mode_lower == "m" {
            Some("minor".to_string())
        } else if mode_lower == "maj" || mode_lower == "major" || mode_lower == "m" {
            Some("major".to_string())
        } else if !mode.is_empty() {
            // Not a recognized mode, restore
            stream.restore(checkpoint);
            None
        } else {
            None
        }
    }

    fn is_note_letter(c: char) -> bool {
        matches!(c.to_uppercase().next().unwrap(), 'A'..='G')
    }
}

impl Default for KeySignatureMiniParser {
    fn default() -> Self {
        Self::new()
    }
}

impl MiniParser<Key> for KeySignatureMiniParser {
    fn can_parse(&self, stream: &TokenStream) -> bool {
        Self::looks_like_key_signature(stream)
    }

    fn parse(&mut self, stream: &mut TokenStream) -> ParseResult<Key> {
        let start_pos = stream.checkpoint();

        // Parse the key note
        let key_note = match self.parse_key_note(stream) {
            Ok(note) => note,
            Err(e) => return ParseResult::Error(e),
        };

        // Parse optional mode
        let _mode = self.parse_mode(stream);

        // For now, create a Key from the string
        // The existing Key::from_string should handle this
        let key_str = key_note;
        match Key::from_string(&key_str) {
            Some(key) => {
                let tokens_consumed = stream.position() - start_pos;
                ParseResult::Success(key, tokens_consumed)
            }
            None => ParseResult::Error(format!("Invalid key signature: {}", key_str)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::common::Lexer;

    fn parse_key(input: &str) -> ParseResult<Key> {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens(input);
        let mut stream = TokenStream::new(tokens);
        let mut parser = KeySignatureMiniParser::new();
        parser.parse(&mut stream)
    }

    #[test]
    fn test_sharp_key() {
        let result = parse_key("#G");
        assert!(result.is_success());
    }

    #[test]
    fn test_sharp_sharp_key() {
        let result = parse_key("#C#");
        assert!(result.is_success());
    }

    #[test]
    fn test_flat_key() {
        let result = parse_key("bEb");
        assert!(result.is_success());
    }

    #[test]
    fn test_can_parse() {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens("#G");
        let stream = TokenStream::new(tokens);
        let parser = KeySignatureMiniParser::new();
        assert!(parser.can_parse(&stream));
    }

    #[test]
    fn test_cannot_parse_chord() {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens("Cmaj7");
        let stream = TokenStream::new(tokens);
        let parser = KeySignatureMiniParser::new();
        assert!(!parser.can_parse(&stream));
    }
}

