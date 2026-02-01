# Hello World Protocol Specification

This document defines the Hello World Protocol - a simple demonstration protocol for the Roam RPC framework.

## Overview

The Hello World Protocol provides a basic service for testing and demonstrating the Roam RPC capabilities. It serves as a minimal example of how to define and implement a protocol.

## Service

The protocol defines a single service:

- **Greeter** - Simple greeting service

## Requirements

All requirements in this specification use the `r[]` prefix for traceability.

## Implementation

The Hello World Protocol is implemented by:
- `hello-world` - Standalone implementation

## Requirements

### Greeting

r[greeting.say-hello]
The greeter MUST return a greeting message when `say_hello()` is called.

r[greeting.say-hello.name]
The greeting MAY include a name parameter to personalize the message.

r[greeting.say-hello.default]
If no name is provided, the greeting SHOULD default to "World".

## Type Definitions

```rust
/// Greeting request
pub struct GreetingRequest {
    pub name: Option<String>,
}

/// Greeting response
pub struct GreetingResponse {
    pub message: String,
}
```

## Service Interface

```rust
#[roam::service]
pub trait Greeter {
    async fn say_hello(&self, request: GreetingRequest) -> GreetingResponse;
}
```

## Version

Current version: 0.1.0