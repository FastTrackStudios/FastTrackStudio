//! Simple HTTP Transport Test
//!
//! This example creates a transport HTTP server and makes test requests
//! to verify that POST endpoints are working correctly.

use transport::{
    core::{Transport, Tempo},
    infra::create_transport_http_router,
};
use std::sync::Arc;
use tokio::sync::Mutex;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Transport HTTP Test");
    println!("======================");

    // Create transport state
    let transport = Transport::new();
    let transport_state = Arc::new(Mutex::new(transport));

    // Create HTTP router
    let app = axum::Router::new()
        .nest("/transport", create_transport_http_router::<Transport>())
        .with_state(transport_state.clone());

    println!("🚀 Starting test server on http://localhost:3003");

    // Start server in background
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3003").await?;
    let server = axum::serve(listener, app);

    // Spawn server as background task
    tokio::spawn(async move {
        if let Err(e) = server.await {
            eprintln!("Server error: {}", e);
        }
    });

    // Wait a moment for server to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    println!("\n🧪 Testing HTTP endpoints...");

    // Create HTTP client
    let client = reqwest::Client::new();

    // Test 1: GET status (should work)
    println!("\n1. Testing GET /transport/status");
    match client.get("http://localhost:3003/transport/status").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 2: GET is_playing (should work)
    println!("\n2. Testing GET /transport/is_playing");
    match client.get("http://localhost:3003/transport/is_playing").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 3: POST play (this should work but might be failing)
    println!("\n3. Testing POST /transport/play");
    match client.post("http://localhost:3003/transport/play").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            } else {
                let body = response.text().await.unwrap_or_else(|_| "No body".to_string());
                println!("   ❌ Error body: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 4: POST pause
    println!("\n4. Testing POST /transport/pause");
    match client.post("http://localhost:3003/transport/pause").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            } else {
                let body = response.text().await.unwrap_or_else(|_| "No body".to_string());
                println!("   ❌ Error body: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 5: POST set_tempo with JSON body
    println!("\n5. Testing POST /transport/set_tempo");
    let tempo_payload = serde_json::json!({"bpm": 140.0});
    match client
        .post("http://localhost:3003/transport/set_tempo")
        .header("Content-Type", "application/json")
        .json(&tempo_payload)
        .send()
        .await
    {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            } else {
                let body = response.text().await.unwrap_or_else(|_| "No body".to_string());
                println!("   ❌ Error body: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 6: Verify state changed by checking tempo
    println!("\n6. Testing GET /transport/tempo (should show 140 BPM)");
    match client.get("http://localhost:3003/transport/tempo").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    // Test 7: Check if playing status changed
    println!("\n7. Testing GET /transport/is_playing (should be true after play)");
    match client.get("http://localhost:3003/transport/is_playing").send().await {
        Ok(response) => {
            println!("   ✅ Status: {}", response.status());
            if response.status().is_success() {
                let body = response.text().await?;
                println!("   📄 Response: {}", body);
            }
        },
        Err(e) => println!("   ❌ Error: {}", e),
    }

    println!("\n🏁 Test complete!");
    println!("\n💡 If POST requests are failing with 405 Method Not Allowed:");
    println!("   - Check that you're using POST for state-changing operations");
    println!("   - Verify the exact URL path (should include /transport prefix)");
    println!("   - Make sure Content-Type is set for JSON payloads");

    Ok(())
}
