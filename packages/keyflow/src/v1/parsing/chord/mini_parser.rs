use crate::v1::chord::chord::ChordData;
use crate::parsing::{ParseResult, MiniParser};
use crate::parsing::common::{Token, TokenStream, TokenType};
use crate::primitives::note::Note;

/// Context-aware chord mini-parser that interprets basic tokens
/// based on their position within a chord symbol.
///
/// Examples of handling:
/// - `C#maj7` -> root C#, quality maj7
/// - `Cm7b5` -> root C, quality m7b5
/// - `4maj7` -> scale degree 4, quality maj7
/// - `C//` -> escapes on `//` for duration parsing
/// - `C/B` -> root C, bass B
pub struct ChordMiniParser {
    /// Whether to allow scale degrees (1-7) as roots
    allow_scale_degrees: bool,
}

impl ChordMiniParser {
    pub fn new() -> Self {
        ChordMiniParser {
            allow_scale_degrees: true,
        }
    }

    /// Parse a chord root (note name or scale degree)
    fn parse_root(&self, stream: &mut TokenStream) -> Result<String, String> {
        stream.skip_whitespace();
        
        let checkpoint = stream.checkpoint();
        
        // Try to parse note name (A-G) with optional sharp/flat
        if let Some(token) = stream.peek() {
            match &token.token_type {
                TokenType::Letter(c) if Self::is_note_letter(*c) => {
                    let note_char = *c;
                    stream.consume();
                    
                    // Check for sharp or flat modifier
                    let mut root = note_char.to_uppercase().to_string();
                    if let Some(modifier_token) = stream.peek() {
                        match &modifier_token.token_type {
                            TokenType::Sharp => {
                                root.push('#');
                                stream.consume();
                            }
                            TokenType::Flat => {
                                root.push('b');
                                stream.consume();
                            }
                            TokenType::Letter('b') if note_char.to_uppercase().next() != Some('B') => {
                                // 'b' as flat modifier (not note B)
                                root.push('b');
                                stream.consume();
                            }
                            _ => {}
                        }
                    }
                    return Ok(root);
                }
                TokenType::Number(num_str) if self.allow_scale_degrees => {
                    // Scale degrees 1-7
                    if let Ok(degree) = num_str.parse::<u8>() {
                        if (1..=7).contains(&degree) {
                            let num = num_str.clone();
                            stream.consume();
                            return Ok(num);
                        }
                    }
                }
                TokenType::Digit(d) if self.allow_scale_degrees => {
                    // Single digit scale degree
                    let degree = d.to_digit(10).unwrap() as u8;
                    if (1..=7).contains(&degree) {
                        let digit_str = d.to_string();
                        stream.consume();
                        return Ok(digit_str);
                    }
                }
                _ => {}
            }
        }
        
        stream.restore(checkpoint);
        Err("Expected chord root (note name or scale degree)".to_string())
    }

    /// Parse chord quality (maj7, m7, dim, aug, sus4, etc.)
    fn parse_quality(&self, stream: &mut TokenStream) -> Result<String, String> {
        let mut quality = String::new();
        
        // Consume quality tokens until we hit something that doesn't belong
        loop {
            match stream.peek() {
                Some(token) => match &token.token_type {
                    // Quality components
                    TokenType::Letter(c) => {
                        quality.push(*c);
                        stream.consume();
                    }
                    TokenType::Digit(d) => {
                        quality.push(*d);
                        stream.consume();
                    }
                    TokenType::Number(num) => {
                        quality.push_str(num);
                        stream.consume();
                    }
                    TokenType::Sharp => {
                        quality.push('#');
                        stream.consume();
                    }
                    TokenType::Flat => {
                        quality.push('b');
                        stream.consume();
                    }
                    TokenType::Plus => {
                        quality.push('+');
                        stream.consume();
                    }
                    TokenType::Minus => {
                        quality.push('-');
                        stream.consume();
                    }
                    TokenType::Triangle => {
                        quality.push('△');
                        stream.consume();
                    }
                    TokenType::Circle => {
                        quality.push('°');
                        stream.consume();
                    }
                    TokenType::LParen => {
                        quality.push('(');
                        stream.consume();
                    }
                    TokenType::RParen => {
                        quality.push(')');
                        stream.consume();
                    }
                    // Stop on these tokens
                    TokenType::Slash | TokenType::Whitespace | TokenType::Comma | TokenType::Eof => {
                        break;
                    }
                    _ => break,
                }
                None => break,
            }
        }
        
        Ok(quality)
    }

    /// Parse bass note after `/`
    fn parse_bass(&self, stream: &mut TokenStream) -> Result<Option<String>, String> {
        // Check for slash
        if let Some(token) = stream.peek() {
            if matches!(token.token_type, TokenType::Slash) {
                stream.consume();
                
                // Check for double slash (escape sequence)
                if let Some(next_token) = stream.peek() {
                    if matches!(next_token.token_type, TokenType::Slash) {
                        // This is `//` - escape back to orchestrator
                        return Err("escape://".to_string());
                    }
                }
                
                // Parse bass note
                let bass = self.parse_root(stream)?;
                return Ok(Some(bass));
            }
        }
        
        Ok(None)
    }

    fn is_note_letter(c: char) -> bool {
        matches!(c.to_uppercase().next().unwrap(), 'A'..='G')
    }
}

impl Default for ChordMiniParser {
    fn default() -> Self {
        Self::new()
    }
}

impl MiniParser<ChordData> for ChordMiniParser {
    fn can_parse(&self, stream: &TokenStream) -> bool {
        // Can parse if we see a letter (A-G) or scale degree (1-7)
        match stream.peek() {
            Some(token) => match &token.token_type {
                TokenType::Letter(c) => Self::is_note_letter(*c),
                TokenType::Number(num) => {
                    if let Ok(degree) = num.parse::<u8>() {
                        (1..=7).contains(&degree)
                    } else {
                        false
                    }
                }
                TokenType::Digit(d) => {
                    let degree = d.to_digit(10).unwrap() as u8;
                    (1..=7).contains(&degree)
                }
                _ => false,
            },
            None => false,
        }
    }

    fn parse(&mut self, stream: &mut TokenStream) -> ParseResult<ChordData> {
        let start_pos = stream.checkpoint();
        
        // Parse root
        let root_str = match self.parse_root(stream) {
            Ok(r) => r,
            Err(e) => return ParseResult::Error(e),
        };
        
        // Parse quality
        let _quality = match self.parse_quality(stream) {
            Ok(q) => q,
            Err(e) => return ParseResult::Error(e),
        };
        
        // Parse bass note if present
        let bass = match self.parse_bass(stream) {
            Ok(b) => b,
            Err(e) if e == "escape://" => {
                // Restore to before the slash and escape
                stream.restore(start_pos);
                // Consume until we hit the slashes
                while let Some(token) = stream.peek() {
                    if matches!(token.token_type, TokenType::Slash) {
                        break;
                    }
                    stream.consume();
                }
                // Collect the escape tokens
                let mut escape_tokens = vec![];
                while let Some(token) = stream.peek() {
                    if matches!(token.token_type, TokenType::Whitespace | TokenType::Eof) {
                        break;
                    }
                    escape_tokens.push(stream.consume().unwrap());
                }
                return ParseResult::Escape(escape_tokens);
            }
            Err(e) => return ParseResult::Error(e),
        };
        
        // For now, create a placeholder ChordData
        // TODO: Actually parse the quality string and create proper intervals
        let root_note = if root_str.chars().next().unwrap().is_ascii_digit() {
            // Scale degree - for now use C as placeholder
            Note::c()
        } else {
            // Parse as note
            match Note::from_string(&root_str) {
                Some(note) => note,
                None => return ParseResult::Error(format!("Invalid note: {}", root_str)),
            }
        };
        
        let bass_note = bass.and_then(|b| Note::from_string(&b));
        
        let tokens_consumed = stream.position() - start_pos;
        
        ParseResult::Success(
            ChordData {
                root: root_note,
                intervals: vec![], // TODO: parse quality into intervals
                bass: bass_note,
            },
            tokens_consumed,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::common::Lexer;

    fn parse_chord(input: &str) -> ParseResult<ChordData> {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens(input);
        let mut stream = TokenStream::new(tokens);
        let mut parser = ChordMiniParser::new();
        parser.parse(&mut stream)
    }

    #[test]
    fn test_simple_chord() {
        let result = parse_chord("C");
        assert!(result.is_success());
    }

    #[test]
    fn test_sharp_chord() {
        let result = parse_chord("C#");
        assert!(result.is_success());
    }

    #[test]
    fn test_chord_with_quality() {
        let result = parse_chord("Cmaj7");
        assert!(result.is_success());
    }

    #[test]
    fn test_scale_degree() {
        let result = parse_chord("4maj7");
        assert!(result.is_success());
    }

    #[test]
    fn test_slash_chord() {
        let result = parse_chord("C/B");
        assert!(result.is_success());
    }

    #[test]
    fn test_double_slash_escape() {
        let result = parse_chord("C//");
        assert!(result.is_escape());
    }
}

