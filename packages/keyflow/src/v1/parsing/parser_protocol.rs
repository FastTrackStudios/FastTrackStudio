use super::common::{Token, TokenStream};

/// Result type for mini-parser operations
#[derive(Debug)]
pub enum ParseResult<T> {
    /// Successfully parsed the object, consuming the specified number of tokens
    Success(T, usize),
    
    /// Parser encountered a pattern it cannot handle - returns tokens to orchestrator
    /// This is used for escape sequences like `//` in chord parsing
    Escape(Vec<Token>),
    
    /// Parse error with description
    Error(String),
}

/// Trait for specialized mini-parsers that interpret tokens contextually
pub trait MiniParser<T> {
    /// Check if this parser can handle the current token stream
    /// This is a quick lookahead check without consuming tokens
    fn can_parse(&self, stream: &TokenStream) -> bool;
    
    /// Parse the token stream and return a result
    /// The parser should consume tokens from the stream as it parses
    fn parse(&mut self, stream: &mut TokenStream) -> ParseResult<T>;
}

/// Helper function to convert ParseResult to Result
impl<T> ParseResult<T> {
    pub fn into_result(self) -> Result<T, String> {
        match self {
            ParseResult::Success(value, _) => Ok(value),
            ParseResult::Escape(tokens) => Err(format!("Unexpected escape with {} tokens", tokens.len())),
            ParseResult::Error(msg) => Err(msg),
        }
    }
    
    pub fn is_success(&self) -> bool {
        matches!(self, ParseResult::Success(_, _))
    }
    
    pub fn is_escape(&self) -> bool {
        matches!(self, ParseResult::Escape(_))
    }
    
    pub fn is_error(&self) -> bool {
        matches!(self, ParseResult::Error(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_result_conversion() {
        let success: ParseResult<i32> = ParseResult::Success(42, 3);
        assert!(success.is_success());
        assert_eq!(success.into_result().unwrap(), 42);

        let error: ParseResult<i32> = ParseResult::Error("test error".to_string());
        assert!(error.is_error());
        assert!(error.into_result().is_err());
    }
}

