use thiserror::Error;

/// Generic parse error for smart templates
#[derive(Debug, Error)]
pub enum TemplateParseError {
    #[error("Not a {0} track")]
    NotMatch(String),
    #[error("Matches negative pattern: {0}")]
    NegativeMatch(String),
    #[error("Parse error: {0}")]
    Other(String),
}

/// Generic match error for smart templates
#[derive(Debug, Error)]
pub enum TemplateMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
