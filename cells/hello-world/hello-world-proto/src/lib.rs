//! Hello World Protocol Definitions
//!
//! This crate defines the shared types and service interfaces for the Hello World protocol.
//!
//! r[define hello-world.protocol]
//! The Hello World Protocol provides a simple greeting service for testing Roam RPC.

#![deny(unsafe_code)]

use facet::Facet;
use roam::service;

/// r[define greeting.request]
/// Request for a greeting message.
#[derive(Debug, Clone, Facet)]
pub struct GreetingRequest {
    /// r[define greeting.request.name]
    /// Optional name to personalize the greeting.
    pub name: Option<String>,
}

impl GreetingRequest {
    /// r[define greeting.request.default]
    /// Create a default request with no name.
    pub fn default() -> Self {
        Self { name: None }
    }

    /// r[define greeting.request.with-name]
    /// Create a request with a specific name.
    pub fn with_name(name: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
        }
    }
}

/// r[define greeting.response]
/// Response containing the greeting message.
#[derive(Debug, Clone, Facet)]
pub struct GreetingResponse {
    /// r[define greeting.response.message]
    /// The greeting message.
    pub message: String,
}

/// r[spec greeter]
/// Greeter service for simple greetings.
///
/// This service provides a basic greeting functionality for demonstration purposes.
#[service]
pub trait Greeter {
    /// r[spec greeting.say-hello]
    /// Return a greeting message.
    ///
    /// r[impl greeting.say-hello]
    /// r[impl greeting.say-hello.name]
    /// r[impl greeting.say-hello.default]
    async fn say_hello(&self, request: GreetingRequest) -> GreetingResponse;
}

/// r[spec greeter.simple]
/// Simple greeter trait without request/response types.
pub trait SimpleGreeter {
    /// r[spec greeting.say-hello-simple]
    /// Return a simple greeting.
    async fn say_hello(&self, name: Option<String>) -> String;
}