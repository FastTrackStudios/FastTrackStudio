use super::token::{Token, TokenType};

/// A stream of tokens that provides peek/consume operations for parsing.
/// This allows parsers to look ahead without consuming tokens.
#[derive(Debug, Clone)]
pub struct TokenStream {
    tokens: Vec<Token>,
    position: usize,
}

impl TokenStream {
    /// Create a new token stream from a vector of tokens
    pub fn new(tokens: Vec<Token>) -> Self {
        TokenStream { tokens, position: 0 }
    }

    /// Peek at the current token without consuming it
    pub fn peek(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    /// Peek at a token n positions ahead without consuming
    pub fn peek_n(&self, n: usize) -> Option<&Token> {
        let pos = self.position + n;
        if pos < self.tokens.len() {
            Some(&self.tokens[pos])
        } else {
            None
        }
    }

    /// Consume and return the current token
    pub fn consume(&mut self) -> Option<Token> {
        if self.position < self.tokens.len() {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Consume tokens while the predicate returns true
    pub fn consume_while<F>(&mut self, predicate: F) -> Vec<Token>
    where
        F: Fn(&Token) -> bool,
    {
        let mut consumed = Vec::new();
        while let Some(token) = self.peek() {
            if predicate(token) {
                consumed.push(self.consume().unwrap());
            } else {
                break;
            }
        }
        consumed
    }

    /// Consume tokens until the predicate returns true (not including the matching token)
    pub fn consume_until<F>(&mut self, predicate: F) -> Vec<Token>
    where
        F: Fn(&Token) -> bool,
    {
        let mut consumed = Vec::new();
        while let Some(token) = self.peek() {
            if predicate(token) {
                break;
            }
            consumed.push(self.consume().unwrap());
        }
        consumed
    }

    /// Skip whitespace tokens
    pub fn skip_whitespace(&mut self) {
        while let Some(token) = self.peek() {
            if matches!(token.token_type, TokenType::Whitespace) {
                self.consume();
            } else {
                break;
            }
        }
    }

    /// Check if we're at the end of the stream (or only have EOF left)
    pub fn is_empty(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => matches!(token.token_type, TokenType::Eof),
        }
    }

    /// Get the current position in the stream
    pub fn position(&self) -> usize {
        self.position
    }

    /// Create a checkpoint that can be restored later (for speculative parsing)
    pub fn checkpoint(&self) -> usize {
        self.position
    }

    /// Restore the stream to a previous checkpoint
    pub fn restore(&mut self, checkpoint: usize) {
        self.position = checkpoint;
    }

    /// Get the remaining tokens (useful for debugging or error messages)
    pub fn remaining(&self) -> &[Token] {
        &self.tokens[self.position..]
    }

    /// Peek at the token type without borrowing the whole token
    pub fn peek_type(&self) -> Option<&TokenType> {
        self.peek().map(|t| &t.token_type)
    }

    /// Check if the next token matches a specific type
    pub fn matches(&self, token_type: &TokenType) -> bool {
        match self.peek() {
            Some(token) => std::mem::discriminant(&token.token_type) == std::mem::discriminant(token_type),
            None => false,
        }
    }

    /// Try to consume a specific token type, returning it if successful
    pub fn expect(&mut self, expected: TokenType) -> Result<Token, String> {
        match self.peek() {
            Some(token) if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) => {
                Ok(self.consume().unwrap())
            }
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token.token_type)),
            None => Err(format!("Expected {:?}, found EOF", expected)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_token(token_type: TokenType) -> Token {
        Token::new(token_type, 0, 1)
    }

    #[test]
    fn test_peek_and_consume() {
        let tokens = vec![
            make_token(TokenType::Letter('C')),
            make_token(TokenType::Sharp),
            make_token(TokenType::Eof),
        ];
        let mut stream = TokenStream::new(tokens);

        assert!(matches!(stream.peek().unwrap().token_type, TokenType::Letter('C')));
        assert!(matches!(stream.consume().unwrap().token_type, TokenType::Letter('C')));
        assert!(matches!(stream.peek().unwrap().token_type, TokenType::Sharp));
    }

    #[test]
    fn test_peek_n() {
        let tokens = vec![
            make_token(TokenType::Letter('C')),
            make_token(TokenType::Sharp),
            make_token(TokenType::Letter('m')),
        ];
        let stream = TokenStream::new(tokens);

        assert!(matches!(stream.peek_n(0).unwrap().token_type, TokenType::Letter('C')));
        assert!(matches!(stream.peek_n(1).unwrap().token_type, TokenType::Sharp));
        assert!(matches!(stream.peek_n(2).unwrap().token_type, TokenType::Letter('m')));
        assert!(stream.peek_n(3).is_none());
    }

    #[test]
    fn test_checkpoint_restore() {
        let tokens = vec![
            make_token(TokenType::Letter('C')),
            make_token(TokenType::Sharp),
            make_token(TokenType::Eof),
        ];
        let mut stream = TokenStream::new(tokens);

        let checkpoint = stream.checkpoint();
        stream.consume();
        stream.consume();
        assert!(matches!(stream.peek().unwrap().token_type, TokenType::Eof));

        stream.restore(checkpoint);
        assert!(matches!(stream.peek().unwrap().token_type, TokenType::Letter('C')));
    }

    #[test]
    fn test_skip_whitespace() {
        let tokens = vec![
            make_token(TokenType::Whitespace),
            make_token(TokenType::Whitespace),
            make_token(TokenType::Letter('C')),
        ];
        let mut stream = TokenStream::new(tokens);

        stream.skip_whitespace();
        assert!(matches!(stream.peek().unwrap().token_type, TokenType::Letter('C')));
    }
}

