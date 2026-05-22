use facet::Facet;
use thiserror::Error;

#[derive(Debug, Clone, Error, Facet)]
#[repr(u8)]
pub enum StoreError {
    #[error("not found: {namespace}/{key}")]
    NotFound { namespace: String, key: String },
    /// Backend IO error. Free-form message because the underlying
    /// driver is implementation-defined.
    #[error("backend: {message}")]
    Backend { message: String },
    #[error("invalid {field}: {reason}")]
    Invalid { field: String, reason: String },
}
