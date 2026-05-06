//! Shared conversion helpers for service_impl modules.
//!
//! These bridge between `crudcrate`-generated `*Api` / `*ApiList` /
//! `*ApiUpdate` types and the underlying domain models via JSON
//! round-trips. The pattern is the same one used by the four
//! services in [`crate::service_impl::business`].

use serde::{Serialize, de::DeserializeOwned};

use crate::service::VaultError;

/// JSON-roundtrip a borrowed value of one shape into another.
///
/// Used to turn a domain model into the matching `*ApiUpdate` so a
/// `crudcrate` repo can apply it.
pub(crate) fn convert_ref<T, U>(value: &T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}

/// JSON-roundtrip an owned value of one shape into another.
///
/// Used to turn `*ApiList` / `*Api` rows from a repo back into the
/// caller's domain model.
pub(crate) fn convert_model<T, U>(value: T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}

/// Build a uniform `provider not configured` error so call-sites are
/// easy to grep for.
#[allow(dead_code)]
pub(crate) fn provider_not_configured(operation: &str) -> VaultError {
    VaultError::IoError(format!("provider not configured: {operation}"))
}
