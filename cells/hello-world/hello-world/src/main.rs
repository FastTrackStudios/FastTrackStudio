//! Hello World Implementation
//!
//! This is a simple implementation of the Hello World Protocol.
//!
//! r[impl hello-world.protocol]
//! This crate implements the Hello World Protocol for demonstration purposes.

#![deny(unsafe_code)]

use hello_world_proto::{GreetingRequest, GreetingResponse, Greeter};
use tracing::info;

/// r[impl greeter.service]
/// Simple greeter implementation.
#[derive(Clone)]
pub struct SimpleGreeter;

impl SimpleGreeter {
    pub fn new() -> Self {
        Self
    }
}

impl Greeter for SimpleGreeter {
    /// r[impl greeting.say-hello]
    /// r[impl greeting.say-hello.name]
    /// r[impl greeting.say-hello.default]
    #[tracing::instrument(skip(self, _cx, request))]
    async fn say_hello(&self, _cx: &roam::Context, request: GreetingRequest) -> GreetingResponse {
        let name = request.name.as_deref().unwrap_or("World");
        let message = format!("Hello, {}!", name);
        
        info!("Sending greeting: {}", message);
        
        GreetingResponse { message }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    info!("Hello World Cell starting...");

    let _greeter = SimpleGreeter::new();
    
    info!("Greeter service initialized");
    
    info!("Hello World Cell initialized");
    info!("Waiting for connections...");
    
    tokio::signal::ctrl_c().await?;
    info!("Shutting down...");
    
    Ok(())
}