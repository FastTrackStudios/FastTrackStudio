//! Network RPC Client for Unified Service Architecture
//!
//! This client demonstrates how to consume services deployed via network RPC.
//! The same service definitions work across:
//! - Direct function calls (monolith)
//! - Network RPC (microservices) <- THIS FILE
//! - Desktop IPC (Tauri apps)
//!
//! The client uses the generated DemoServiceClient to make type-safe RPC calls
//! over the network while maintaining the same API as direct service calls.

use clap::Parser;
use service::{DemoServiceClient, GreetingRequest, init_tracing};
use std::{net::SocketAddr, time::Duration};
use tarpc::{client, context, tokio_serde::formats::Json};
use tokio::time::sleep;
use tracing::Instrument;

#[derive(Parser)]
#[command(name = "tarpc-unified-client")]
#[command(about = "Network RPC client demonstrating unified service architecture")]
struct Flags {
    /// Sets the server address to connect to.
    #[clap(long, default_value = "[::1]:50051")]
    server_addr: SocketAddr,

    /// Sets the name to say hello to.
    #[clap(long, default_value = "NetworkClient")]
    name: String,

    /// Number of requests to make
    #[clap(long, default_value = "3")]
    requests: u32,

    /// Enable verbose logging
    #[clap(long, short)]
    verbose: bool,

    /// Request timeout in milliseconds
    #[clap(long, default_value = "5000")]
    timeout_ms: u64,

    /// Test different languages for greeting
    #[clap(long)]
    test_languages: bool,
}

/// Demonstrates basic hello calls over network RPC
async fn test_hello_calls(
    client: &DemoServiceClient,
    name: &str,
    count: u32,
) -> anyhow::Result<()> {
    println!("\n📞 Network RPC Hello Calls:");
    println!("============================");

    for i in 1..=count {
        let request_name = format!("{}-{}", name, i);

        let response = client
            .hello(context::current(), request_name.clone())
            .instrument(tracing::info_span!("hello_call", name = request_name.as_str(), attempt = i))
            .await
            .map_err(|e| anyhow::anyhow!("Hello call failed: {}", e))?;

        println!("  ✅ {}", response);

        // Small delay between requests to show network behavior
        if i < count {
            sleep(Duration::from_millis(200)).await;
        }
    }

    Ok(())
}

/// Demonstrates structured greeting calls with different languages
async fn test_greeting_calls(
    client: &DemoServiceClient,
    base_name: &str,
    test_languages: bool,
) -> anyhow::Result<()> {
    println!("\n🌍 Network RPC Multi-language Greetings:");
    println!("========================================");

    let test_cases = if test_languages {
        vec![
            ("Alice", Some("english".to_string())),
            ("María", Some("spanish".to_string())),
            ("Pierre", Some("french".to_string())),
            ("Hans", Some("german".to_string())),
            ("田中", Some("japanese".to_string())),
        ]
    } else {
        vec![
            (base_name, Some("english".to_string())),
            (&format!("{}-ES", base_name), Some("spanish".to_string())),
            (&format!("{}-FR", base_name), Some("french".to_string())),
        ]
    };

    for (name, language) in test_cases {
        let request = GreetingRequest {
            name: name.to_string(),
            language,
        };

        let response = client
            .greeting(context::current(), request.clone())
            .instrument(tracing::info_span!(
                "greeting_call",
                name = request.name.as_str(),
                language = ?request.language
            ))
            .await
            .map_err(|e| anyhow::anyhow!("Greeting call failed: {}", e))?;

        println!(
            "  ✅ {} (lang: {}, timestamp: {})",
            response.message,
            response.language_used,
            response.timestamp
        );

        sleep(Duration::from_millis(150)).await;
    }

    Ok(())
}

/// Demonstrates counter operations over network RPC
async fn test_counter_operations(
    client: &DemoServiceClient,
    operations: u32,
) -> anyhow::Result<()> {
    println!("\n🔢 Network RPC Counter Operations:");
    println!("==================================");

    // Get initial state
    let initial_state = client
        .get_counter(context::current())
        .instrument(tracing::info_span!("get_initial_counter"))
        .await
        .map_err(|e| anyhow::anyhow!("Get counter failed: {}", e))?;

    println!("  📊 Initial counter: {} (updated: {})", initial_state.value, initial_state.last_updated);

    // Perform increment
