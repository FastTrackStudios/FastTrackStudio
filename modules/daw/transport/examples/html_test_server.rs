//! HTML Test Server for Transport HTTP API
//!
//! This example creates a web server that serves both the transport API
//! and a simple HTML interface with buttons to test POST requests.

use transport::{
    core::Transport,
    infra::create_transport_http_router,
};
use axum::{
    response::Html,
    routing::get,
    Router,
};
use std::sync::Arc;
use tokio::sync::Mutex;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🌐 Transport HTML Test Server");
    println!("============================");

    // Create transport state
    let transport = Transport::new();
    let transport_state = Arc::new(Mutex::new(transport));

    // Create the main app with both API and web interface
    let app = Router::new()
        // Serve the HTML test page
        .route("/", get(serve_test_page))
        // Mount the transport API
        .nest("/transport", create_transport_http_router::<Transport>())
        .with_state(transport_state.clone());

    println!("🚀 Starting server on http://localhost:3004");
    println!();
    println!("📱 Open your browser and go to:");
    println!("   http://localhost:3004");
    println!();
    println!("🎮 The web page has buttons to test all POST endpoints!");
    println!("Press Ctrl+C to shutdown");

    // Start server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3004").await?;
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    println!("🛑 Server shutdown complete");
    Ok(())
}

async fn serve_test_page() -> Html<String> {
    Html(HTML_TEST_PAGE.to_string())
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("Failed to install CTRL+C signal handler");
    println!("\n🛑 Shutdown signal received, shutting down gracefully...");
}

const HTML_TEST_PAGE: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Transport API Test</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 { color: #333; text-align: center; }
        h2 { color: #666; border-bottom: 2px solid #eee; padding-bottom: 10px; }
        .button-group {
            display: flex;
            gap: 10px;
            flex-wrap: wrap;
            margin: 20px 0;
        }
        button {
            padding: 12px 20px;
            border: none;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            transition: all 0.2s;
        }
        .play-btn { background: #4CAF50; color: white; }
        .pause-btn { background: #FF9800; color: white; }
        .stop-btn { background: #f44336; color: white; }
        .record-btn { background: #e91e63; color: white; }
        .config-btn { background: #2196F3; color: white; }
        .query-btn { background: #9C27B0; color: white; }
        button:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
        button:active { transform: translateY(0); }
        #output {
            background: #2d3748;
            color: #e2e8f0;
            padding: 20px;
            border-radius: 6px;
            font-family: 'Monaco', 'Menlo', monospace;
            font-size: 12px;
            max-height: 400px;
            overflow-y: auto;
            margin-top: 20px;
            white-space: pre-wrap;
        }
        .input-group {
            display: flex;
            gap: 10px;
            align-items: center;
            margin: 10px 0;
        }
        input[type="number"] {
            padding: 8px;
            border: 1px solid #ddd;
            border-radius: 4px;
            width: 80px;
        }
        .clear-btn {
            background: #666;
            color: white;
            padding: 8px 12px;
            font-size: 12px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎵 Transport API Test Interface</h1>

        <h2>🎮 Playback Controls</h2>
        <div class="button-group">
            <button class="play-btn" onclick="makeRequest('POST', '/transport/play')">▶️ Play</button>
            <button class="pause-btn" onclick="makeRequest('POST', '/transport/pause')">⏸️ Pause</button>
            <button class="stop-btn" onclick="makeRequest('POST', '/transport/stop')">⏹️ Stop</button>
            <button class="play-btn" onclick="makeRequest('POST', '/transport/play_pause')">⏯️ Play/Pause Toggle</button>
            <button class="play-btn" onclick="makeRequest('POST', '/transport/play_stop')">⏭️ Play/Stop Toggle</button>
        </div>

        <h2>🔴 Recording Controls</h2>
        <div class="button-group">
            <button class="record-btn" onclick="makeRequest('POST', '/transport/start_recording')">🔴 Start Recording</button>
            <button class="record-btn" onclick="makeRequest('POST', '/transport/stop_recording')">⭕ Stop Recording</button>
            <button class="record-btn" onclick="makeRequest('POST', '/transport/toggle_recording')">🔄 Toggle Recording</button>
        </div>

        <h2>⚙️ Configuration</h2>
        <div class="input-group">
            <label>Tempo (BPM):</label>
            <input type="number" id="tempoInput" value="140" min="1" max="300">
            <button class="config-btn" onclick="setTempo()">🎼 Set Tempo</button>
        </div>
        <div class="input-group">
            <label>Position (seconds):</label>
            <input type="number" id="positionInput" value="30" min="0" step="0.1">
            <button class="config-btn" onclick="setPosition()">📍 Set Position</button>
        </div>
        <div class="input-group">
            <label>Time Signature:</label>
            <input type="number" id="numeratorInput" value="4" min="1" max="16" style="width: 40px">
            <span>/</span>
            <input type="number" id="denominatorInput" value="4" min="1" max="16" style="width: 40px">
            <button class="config-btn" onclick="setTimeSignature()">🎵 Set Time Signature</button>
        </div>

        <h2>📊 Query Status</h2>
        <div class="button-group">
            <button class="query-btn" onclick="makeRequest('GET', '/transport/status')">📋 Full Status</button>
            <button class="query-btn" onclick="makeRequest('GET', '/transport/is_playing')">🎮 Is Playing?</button>
            <button class="query-btn" onclick="makeRequest('GET', '/transport/is_recording')">🔴 Is Recording?</button>
            <button class="query-btn" onclick="makeRequest('GET', '/transport/tempo')">🎼 Get Tempo</button>
            <button class="query-btn" onclick="makeRequest('GET', '/transport/position')">📍 Get Position</button>
            <button class="query-btn" onclick="makeRequest('GET', '/transport/is_ready')">✅ Is Ready?</button>
        </div>

        <div style="display: flex; justify-content: space-between; align-items: center;">
            <h2>📺 API Response Log</h2>
            <button class="clear-btn" onclick="clearOutput()">🗑️ Clear Log</button>
        </div>
        <div id="output">Ready to test transport API endpoints!
Click any button above to make HTTP requests...</div>
    </div>

    <script>
        async function makeRequest(method, url, body = null) {
            const timestamp = new Date().toLocaleTimeString();
            const output = document.getElementById('output');

            // Log the request
            output.textContent += `\n[${timestamp}] ${method} ${url}`;
            if (body) {
                output.textContent += `\nBody: ${JSON.stringify(body, null, 2)}`;
            }
            output.textContent += '\n';

            try {
                const options = {
                    method: method,
                    headers: {}
                };

                if (body) {
                    options.headers['Content-Type'] = 'application/json';
                    options.body = JSON.stringify(body);
                }

                const response = await fetch(url, options);

                // Log response status
                output.textContent += `Response: ${response.status} ${response.statusText}\n`;

                // Log response body
                const responseText = await response.text();
                if (responseText) {
                    try {
                        const json = JSON.parse(responseText);
                        output.textContent += `${JSON.stringify(json, null, 2)}\n`;
                    } catch (e) {
                        output.textContent += `${responseText}\n`;
                    }
                }

                output.textContent += '\n' + '='.repeat(50) + '\n';

            } catch (error) {
                output.textContent += `Error: ${error.message}\n`;
                output.textContent += '\n' + '='.repeat(50) + '\n';
            }

            // Scroll to bottom
            output.scrollTop = output.scrollHeight;
        }

        function setTempo() {
            const bpm = parseFloat(document.getElementById('tempoInput').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                makeRequest('POST', '/transport/set_tempo', { bpm: bpm });
            } else {
                alert('Please enter a valid tempo between 1 and 300 BPM');
            }
        }

        function setPosition() {
            const seconds = parseFloat(document.getElementById('positionInput').value);
            if (seconds >= 0) {
                makeRequest('POST', '/transport/set_position', { seconds: seconds });
            } else {
                alert('Please enter a valid position (0 or greater)');
            }
        }

        function setTimeSignature() {
            const numerator = parseInt(document.getElementById('numeratorInput').value);
            const denominator = parseInt(document.getElementById('denominatorInput').value);
            if (numerator > 0 && denominator > 0) {
                makeRequest('POST', '/transport/set_time_signature', {
                    numerator: numerator,
                    denominator: denominator
                });
            } else {
                alert('Please enter valid time signature values');
            }
        }

        function clearOutput() {
            document.getElementById('output').textContent = 'Ready to test transport API endpoints!\nClick any button above to make HTTP requests...';
        }

        // Auto-refresh status every 5 seconds
        setInterval(() => {
            makeRequest('GET', '/transport/status');
        }, 5000);
    </script>
</body>
</html>"#;
