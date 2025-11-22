use super::token::{Token, TokenType};
use std::{iter::Peekable, str::Chars};

/// Simplified lexer that emits basic tokens without contextual interpretation.
/// Context-dependent meaning is determined by mini-parsers that receive these tokens.
pub struct Lexer {
    tokens: Vec<Token>,
    current: usize,
    input_len: usize,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            input_len: 0,
            tokens: Vec::new(),
            current: 0,
        }
    }

    /// Tokenize input string into basic tokens
    pub fn scan_tokens(&mut self, source: &str) -> Vec<Token> {
        self.input_len = source.len();
        let mut iter = source.chars().peekable();
        
        while !self.is_at_end() {
            self.scan_token(&mut iter);
        }
        
        self.add_token(TokenType::Eof, self.current, 0);
        let res = self.tokens.clone();
        self.tokens.clear();
        self.current = 0;
        res
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input_len
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>) {
        let c = match self.advance(chars) {
            Some(c) => c,
            None => return,
        };

        let pos = self.current;

        match c {
            // Special symbols
            '#' | '♯' => self.add_token(TokenType::Sharp, pos, 1),
            '♭' => self.add_token(TokenType::Flat, pos, 1),
            '/' => self.add_token(TokenType::Slash, pos, 1),
            '+' => self.add_token(TokenType::Plus, pos, 1),
            '-' => self.add_token(TokenType::Minus, pos, 1),
            '\'' => self.add_token(TokenType::Apostrophe, pos, 1),
            '!' => self.add_token(TokenType::Exclamation, pos, 1),
            '_' => self.add_token(TokenType::Underscore, pos, 1),
            '△' | '^' => self.add_token(TokenType::Triangle, pos, 1),
            '°' | 'ø' => self.add_token(TokenType::Circle, pos, 1),
            '(' => self.add_token(TokenType::LParen, pos, 1),
            ')' => self.add_token(TokenType::RParen, pos, 1),
            ',' => self.add_token(TokenType::Comma, pos, 1),
            '[' => self.add_token(TokenType::LBracket, pos, 1),
            ']' => self.add_token(TokenType::RBracket, pos, 1),
            
            // Whitespace (track position but emit as token for parsers to handle)
            ' ' | '\t' => self.add_token(TokenType::Whitespace, pos, 1),
            
            // Numbers - collect consecutive digits for convenience
            c if c.is_ascii_digit() => {
                let mut literal = String::from(c);
                while let Some(&next_char) = chars.peek() {
                    if next_char.is_ascii_digit() {
                        literal.push(self.advance(chars).unwrap());
                    } else {
                        break;
                    }
                }
                let len = literal.len();
                self.add_token(TokenType::Number(literal), pos, len);
            }
            
            // Letters - emit as single character tokens
            // Note: 'b' could be note B or flat modifier - context determines meaning
            c if c.is_ascii_alphabetic() => {
                self.add_token(TokenType::Letter(c), pos, 1);
            }
            
            // Unrecognized character
            _ => self.add_token(TokenType::Illegal, pos, 1),
        }
    }

    fn add_token(&mut self, token_type: TokenType, pos: usize, len: usize) {
        self.tokens.push(Token::new(token_type, pos, len));
    }

    fn advance(&mut self, chars: &mut Peekable<Chars>) -> Option<char> {
        self.current += 1;
        chars.next()
    }
}

impl Default for Lexer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens("C#maj7");
        
        assert_eq!(tokens.len(), 7); // C, #, m, a, j, 7, EOF
        assert!(matches!(tokens[0].token_type, TokenType::Letter('C')));
        assert!(matches!(tokens[1].token_type, TokenType::Sharp));
        assert!(matches!(tokens[2].token_type, TokenType::Letter('m')));
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens("13");
        
        assert_eq!(tokens.len(), 2); // 13, EOF
        assert!(matches!(tokens[0].token_type, TokenType::Number(_)));
    }

    #[test]
    fn test_slash_notation() {
        let mut lexer = Lexer::new();
        let tokens = lexer.scan_tokens("C//");
        
        assert_eq!(tokens.len(), 4); // C, /, /, EOF
        assert!(matches!(tokens[0].token_type, TokenType::Letter('C')));
        assert!(matches!(tokens[1].token_type, TokenType::Slash));
        assert!(matches!(tokens[2].token_type, TokenType::Slash));
    }
}
