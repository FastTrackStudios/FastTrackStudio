use thiserror::Error;

#[derive(Error, Debug)]
pub enum MonarchyError {
    #[error("Parse error: {0}")]
    Parse(String),

    #[error("No matching group found for: {0}")]
    NoMatch(String),

    #[error("Invalid metadata field: {0}")]
    InvalidField(String),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("TOML error: {0}")]
    Toml(#[from] toml::de::Error),
}

pub type Result<T> = std::result::Result<T, MonarchyError>;
