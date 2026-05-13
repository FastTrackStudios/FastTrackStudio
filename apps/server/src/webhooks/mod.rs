//! Inbound webhook receivers.
//!
//! One module per integration. Every module exposes a `router()` that
//! mounts under a stable prefix in `crate::lib::router`:
//!
//! - `/webhooks/github/{repo_path}`  → [`github::router`]
//! - `/webhooks/hermes/event`        → [`hermes::router`]
//!
//! Verification approach: each handler reads the raw `Bytes` body
//! *before* serde so HMAC computes over exactly the bytes the sender
//! signed. Signatures are compared in constant time with
//! `subtle::ConstantTimeEq`. Failed-auth attempts are still inserted
//! into the in-memory [`crate::webhook_inbox::WebhookInbox`] so
//! they're visible while debugging.

pub mod github;
pub mod hermes;

use axum::http::HeaderMap;

/// Render an axum `HeaderMap` as a flat JSON object string for the
/// audit log. Sensitive headers (signatures, cookies, auth) are
/// preserved verbatim — the inbox is server-only debug data.
pub(crate) fn headers_to_json_string(headers: &HeaderMap) -> String {
    let map: serde_json::Map<String, serde_json::Value> = headers
        .iter()
        .map(|(k, v)| {
            (
                k.as_str().to_string(),
                serde_json::Value::String(v.to_str().unwrap_or("<non-ascii>").to_string()),
            )
        })
        .collect();
    serde_json::Value::Object(map).to_string()
}
