//! Common lexer that emits basic tokens
//!
//! This lexer is context-free and doesn't interpret meaning.
//! Mini-parsers receive these tokens and interpret them contextually.

use super::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

/// Basic lexer that tokenizes input without context
pub struct Lexer {
    input: String,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input }
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = self.input.chars().peekable();
        let mut pos = 0;

        while chars.peek().is_some() {
            let token = self.scan_token(&mut chars, &mut pos);
            tokens.push(token);
        }

        // Add EOF token
        tokens.push(Token::new(TokenType::Eof, pos, 0));
        tokens
    }

    fn scan_token(&self, chars: &mut Peekable<Chars>, pos: &mut usize) -> Token {
        let start_pos = *pos;
        let ch = chars.next().unwrap();
        *pos += ch.len_utf8();

        match ch {
            // Symbols
            '#' | '♯' => Token::new(TokenType::Sharp, start_pos, 1),
            '♭' => Token::new(TokenType::Flat, start_pos, 1),
            '/' => Token::new(TokenType::Slash, start_pos, 1),
            '+' => Token::new(TokenType::Plus, start_pos, 1),
            '-' => Token::new(TokenType::Minus, start_pos, 1),
            '_' => Token::new(TokenType::Underscore, start_pos, 1),
            '\'' => Token::new(TokenType::Apostrophe, start_pos, 1),
            '.' => Token::new(TokenType::Dot, start_pos, 1),
            '~' => Token::new(TokenType::Tilde, start_pos, 1),
            '*' => Token::new(TokenType::Asterisk, start_pos, 1),
            '△' | '^' => Token::new(TokenType::Triangle, start_pos, 1),
            '°' => Token::new(TokenType::Circle, start_pos, 1),
            'ø' => Token::new(TokenType::HalfDiminished, start_pos, 1),
            '(' => Token::new(TokenType::LParen, start_pos, 1),
            ')' => Token::new(TokenType::RParen, start_pos, 1),
            ',' => Token::new(TokenType::Comma, start_pos, 1),
            '@' => Token::new(TokenType::At, start_pos, 1),
            ';' => Token::new(TokenType::Semicolon, start_pos, 1),
            '>' => Token::new(TokenType::GreaterThan, start_pos, 1),

            // Whitespace
            ' ' | '\t' => Token::new(TokenType::Space, start_pos, 1),

            // Numbers - collect consecutive digits
            c if c.is_ascii_digit() => {
                let mut num = String::from(c);
                while let Some(&next) = chars.peek() {
                    if next.is_ascii_digit() {
                        num.push(chars.next().unwrap());
                        *pos += 1;
                    } else {
                        break;
                    }
                }
                let len = num.len();
                Token::new(TokenType::Number(num), start_pos, len)
            }

            // Letters - single character
            // Note: 'b' could be note B or flat - context determines meaning
            c if c.is_ascii_alphabetic() => Token::new(TokenType::Letter(c), start_pos, 1),

            // Unknown character
            _ => Token::new(TokenType::Illegal, start_pos, 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let mut lexer = Lexer::new("C#maj7".to_string());
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 7); // C, #, m, a, j, 7, EOF
        assert!(matches!(tokens[0].token_type, TokenType::Letter('C')));
        assert!(matches!(tokens[1].token_type, TokenType::Sharp));
    }

    #[test]
    fn test_number_grouping() {
        let mut lexer = Lexer::new("Cmaj13".to_string());
        let tokens = lexer.tokenize();

        // Should group "13" as a single number token
        assert!(matches!(tokens[4].token_type, TokenType::Number(ref n) if n == "13"));
    }

    #[test]
    fn test_slash_notation() {
        let mut lexer = Lexer::new("C/E".to_string());
        let tokens = lexer.tokenize();

        assert!(matches!(tokens[0].token_type, TokenType::Letter('C')));
        assert!(matches!(tokens[1].token_type, TokenType::Slash));
        assert!(matches!(tokens[2].token_type, TokenType::Letter('E')));
    }
}
