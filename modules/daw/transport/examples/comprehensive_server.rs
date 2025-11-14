//! Comprehensive Multi-Protocol Transport Server
//!
//! This example demonstrates a complete transport server implementation that integrates:
//! - HTTP REST API with comprehensive endpoints
//! - WebSocket server for real-time bidirectional communication
//! - OSC server for professional audio software integration
//! - Interactive web interface for testing all protocols
//!
//! ## Features
//!
//! ### HTTP REST API (Port 3000)
//! - Complete CRUD operations for transport control
//! - JSON request/response format
//! - CORS enabled for web integration
//! - Health check endpoint
//!
//! ### WebSocket Server (Port 3001)
//! - Real-time status updates
//! - Bidirectional command/response
//! - Connection status monitoring
//! - JSON message format
//!
//! ### OSC Server (UDP Port 9000)
//! - Professional audio software compatibility
//! - All transport operations via OSC addresses
//! - Optimized routing with matchit
//! - Response confirmations
//!
//! ### Web Interface (Port 3002)
//! - Interactive buttons for all operations
//! - Real-time WebSocket status display
//! - Protocol comparison testing
//! - Live response logging
//!
//! ## Usage
//!
//! ```bash
//! cargo run --example comprehensive_server
//! ```
//!
//! Then access:
//! - Web Interface: http://localhost:3002
//! - HTTP API: http://localhost:3000
//! - WebSocket: ws://localhost:3001/ws
//! - OSC: UDP port 9000
//!
//! ## Example Commands
//!
//! ### HTTP
//! ```bash
//! curl -X POST http://localhost:3000/play
//! curl -X POST http://localhost:3000/set_tempo -H "Content-Type: application/json" -d '{"bpm": 140.0}'
//! curl http://localhost:3000/status
//! ```
//!
//! ### OSC
//! ```bash
//! oscsend localhost 9000 /transport/play
//! oscsend localhost 9000 /transport/tempo f 140.0
//! oscsend localhost 9000 /transport/status
//! ```
//!
//! ### WebSocket (JavaScript)
//! ```javascript
//! const ws = new WebSocket('ws://localhost:3001/ws');
//! ws.send(JSON.stringify({command: 'play'}));
//! ```

use axum::{Router, response::Html, routing::get};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use transport::{
    MockTransportService, TransportActions,
    infra::{
        http::create_transport_http_router,
        osc::create_transport_osc_server,
        websocket::{WebSocketHandler, create_transport_ws_router},
    },
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("🎵 Comprehensive Multi-Protocol Transport Server");
    println!("===============================================");

    // Create shared transport service
    println!("📦 Initializing transport service...");
    let transport_service = MockTransportService::new();
    let transport = Arc::new(Mutex::new(transport_service));

    // Display initial transport state
    print_initial_status(&transport).await;

    println!("🚀 Starting all protocol servers...");
    println!();

    // Clone transport for each protocol server
    let transport_http = Arc::clone(&transport);
    let transport_ws = Arc::clone(&transport);
    let transport_osc = Arc::clone(&transport);
    let transport_web = Arc::clone(&transport);

    // Start HTTP REST API server (Port 3000)
    let http_handle = tokio::spawn(async move {
        start_http_server(transport_http).await;
    });

    // Start WebSocket server (Port 3001)
    let ws_handle = tokio::spawn(async move {
        start_websocket_server(transport_ws).await;
    });

    // Start OSC server (UDP Port 9000)
    let osc_handle = tokio::spawn(async move {
        start_osc_server(transport_osc).await;
    });

    // Start Web Interface server (Port 3002)
    let web_handle = tokio::spawn(async move {
        start_web_interface_server(transport_web).await;
    });

    // Allow servers to initialize
    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

    print_server_info();

    // Wait for any server to stop (they run indefinitely)
    tokio::select! {
        _ = http_handle => println!("🌐 HTTP server stopped"),
        _ = ws_handle => println!("🔌 WebSocket server stopped"),
        _ = osc_handle => println!("📡 OSC server stopped"),
        _ = web_handle => println!("🖥️  Web interface stopped"),
    }

    println!("✅ All servers stopped gracefully");
    Ok(())
}

async fn print_initial_status(transport: &Arc<Mutex<MockTransportService>>) {
    let t = transport.lock().await;
    match t.get_transport() {
        Ok(state) => {
            println!("🎼 Initial Transport State:");
            println!("   ▶️  Play State: {:?}", state.play_state);
            println!("   🎵 Tempo: {:.1} BPM", state.tempo.bpm);
            println!(
                "   🎶 Time Signature: {}/{}",
                state.time_signature.numerator, state.time_signature.denominator
            );
            println!(
                "   📍 Position: {:.2}s",
                state.playhead_position.time.to_seconds()
            );
            println!("   🔴 Record Mode: {:?}", state.record_mode);
        }
        Err(e) => println!("❌ Error getting initial state: {}", e),
    }
    println!();
}

async fn start_http_server(transport: Arc<Mutex<MockTransportService>>) {
    println!("🌐 Starting HTTP REST API server...");

    let http_router = create_transport_http_router();

    let app = http_router
        .route("/health", get(|| async { "HTTP API OK" }))
        .route("/info", get(|| async {
            "FastTrackStudio Transport HTTP API v1.0\nSupports: play, pause, stop, tempo, position, recording"
        }))
        .layer(
            ServiceBuilder::new()
                .layer(CorsLayer::permissive())
        )
        .with_state(transport);

    match tokio::net::TcpListener::bind("0.0.0.0:3000").await {
        Ok(listener) => {
            println!("✅ HTTP REST API running on http://localhost:3000");
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ HTTP server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind HTTP server: {}", e),
    }
}

async fn start_websocket_server(transport: Arc<Mutex<MockTransportService>>) {
    println!("🔌 Starting WebSocket server...");

    let ws_handler = WebSocketHandler::new(transport);
    let ws_router = create_transport_ws_router();

    let app = ws_router
        .route("/health", get(|| async { "WebSocket Server OK" }))
        .route(
            "/info",
            get(|| async {
                "FastTrackStudio Transport WebSocket API v1.0\nConnect to /ws for real-time updates"
            }),
        )
        .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
        .with_state(ws_handler);

    match tokio::net::TcpListener::bind("0.0.0.0:3001").await {
        Ok(listener) => {
            println!("✅ WebSocket server running on ws://localhost:3001/ws");
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ WebSocket server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind WebSocket server: {}", e),
    }
}

async fn start_osc_server(transport: Arc<Mutex<MockTransportService>>) {
    println!("📡 Starting OSC server...");

    match create_transport_osc_server(transport, "0.0.0.0:9000").await {
        Ok(()) => println!("✅ OSC server running on UDP port 9000"),
        Err(e) => eprintln!("❌ OSC server error: {}", e),
    }
}

async fn start_web_interface_server(transport: Arc<Mutex<MockTransportService>>) {
    println!("🖥️  Starting web interface server...");

    let app = Router::new()
        .route("/", get(serve_main_page))
        .route("/health", get(|| async { "Web Interface OK" }))
        .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
        .with_state(transport);

    match tokio::net::TcpListener::bind("0.0.0.0:3002").await {
        Ok(listener) => {
            println!("✅ Web interface running on http://localhost:3002");
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ Web interface server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind web interface server: {}", e),
    }
}

fn print_server_info() {
    println!("🎯 All servers are running!");
    println!();
    println!("📋 Protocol Endpoints:");
    println!("┌─────────────────────────────────────────────────────────────┐");
    println!("│ 🌐 HTTP REST API (Port 3000)                              │");
    println!("│   http://localhost:3000/play                               │");
    println!("│   http://localhost:3000/pause                              │");
    println!("│   http://localhost:3000/stop                               │");
    println!("│   http://localhost:3000/set_tempo                          │");
    println!("│   http://localhost:3000/status                             │");
    println!("│   http://localhost:3000/health                             │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ 🔌 WebSocket Real-time (Port 3001)                        │");
    println!("│   ws://localhost:3001/ws                                   │");
    println!("│   http://localhost:3001/health                             │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ 📡 OSC Protocol (UDP Port 9000)                           │");
    println!("│   /transport/play                                          │");
    println!("│   /transport/pause                                         │");
    println!("│   /transport/stop                                          │");
    println!("│   /transport/tempo [float bpm]                             │");
    println!("│   /transport/status                                        │");
    println!("├─────────────────────────────────────────────────────────────┤");
    println!("│ 🖥️  Web Interface (Port 3002)                              │");
    println!("│   http://localhost:3002                                    │");
    println!("│   Interactive testing for all protocols                   │");
    println!("└─────────────────────────────────────────────────────────────┘");
    println!();
    println!("💡 Quick Start Commands:");
    println!();
    println!("   HTTP:");
    println!("   curl -X POST http://localhost:3000/play");
    println!("   curl -X POST http://localhost:3000/set_tempo \\");
    println!("        -H 'Content-Type: application/json' \\");
    println!("        -d '{{\"bpm\": 140.0}}'");
    println!();
    println!("   OSC:");
    println!("   oscsend localhost 9000 /transport/play");
    println!("   oscsend localhost 9000 /transport/tempo f 140.0");
    println!();
    println!("   WebSocket (Node.js):");
    println!("   const ws = new WebSocket('ws://localhost:3001/ws');");
    println!("   ws.send(JSON.stringify({{command: 'play'}}));");
    println!();
    println!("🌟 Open http://localhost:3002 for interactive testing!");
    println!("🔧 Press Ctrl+C to stop all servers");
    println!("===============================================");
}

async fn serve_main_page() -> Html<String> {
    Html(WEB_INTERFACE_HTML.to_string())
}

const WEB_INTERFACE_HTML: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FastTrackStudio Transport - Multi-Protocol Test Interface</title>
    <style>
        * { box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0;
            padding: 20px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            color: #333;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: rgba(255, 255, 255, 0.95);
            border-radius: 20px;
            padding: 30px;
            box-shadow: 0 20px 40px rgba(0,0,0,0.1);
            backdrop-filter: blur(10px);
        }
        h1 {
            text-align: center;
            color: #2c3e50;
            margin-bottom: 10px;
            font-size: 2.5em;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
        }
        .subtitle {
            text-align: center;
            color: #7f8c8d;
            margin-bottom: 30px;
            font-size: 1.1em;
        }
        .protocols-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .protocol-section {
            background: white;
            border-radius: 15px;
            padding: 20px;
            box-shadow: 0 10px 20px rgba(0,0,0,0.1);
            border-left: 5px solid #3498db;
        }
        .protocol-section.http { border-left-color: #e74c3c; }
        .protocol-section.websocket { border-left-color: #2ecc71; }
        .protocol-section.osc { border-left-color: #f39c12; }
        .protocol-section h2 {
            margin-top: 0;
            color: #2c3e50;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .status-indicator {
            display: inline-block;
            width: 12px;
            height: 12px;
            border-radius: 50%;
            background: #95a5a6;
            animation: pulse 2s infinite;
        }
        .status-indicator.connected { background: #2ecc71; }
        .status-indicator.error { background: #e74c3c; }
        @keyframes pulse {
            0% { opacity: 1; }
            50% { opacity: 0.5; }
            100% { opacity: 1; }
        }
        .button-group {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            margin: 15px 0;
        }
        button {
            padding: 10px 16px;
            border: none;
            border-radius: 8px;
            cursor: pointer;
            font-size: 13px;
            font-weight: 600;
            transition: all 0.3s ease;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .play-btn { background: linear-gradient(135deg, #2ecc71, #27ae60); color: white; }
        .pause-btn { background: linear-gradient(135deg, #f39c12, #e67e22); color: white; }
        .stop-btn { background: linear-gradient(135deg, #e74c3c, #c0392b); color: white; }
        .config-btn { background: linear-gradient(135deg, #3498db, #2980b9); color: white; }
        .query-btn { background: linear-gradient(135deg, #9b59b6, #8e44ad); color: white; }
        button:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
        button:active { transform: translateY(0); }
        button:disabled {
            opacity: 0.5;
            cursor: not-allowed;
            transform: none !important;
        }
        .input-group {
            display: flex;
            gap: 10px;
            align-items: center;
            margin: 10px 0;
            flex-wrap: wrap;
        }
        input[type="number"] {
            padding: 8px 12px;
            border: 2px solid #bdc3c7;
            border-radius: 6px;
            width: 80px;
            font-size: 14px;
        }
        input[type="number"]:focus {
            outline: none;
            border-color: #3498db;
            box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.2);
        }
        .status-panel {
            background: #2c3e50;
            color: #ecf0f1;
            border-radius: 10px;
            padding: 20px;
            margin-top: 20px;
            font-family: 'Monaco', 'Menlo', monospace;
            font-size: 12px;
            max-height: 300px;
            overflow-y: auto;
        }
        .current-status {
            background: #34495e;
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 15px;
        }
        .log-output {
            background: #1e2832;
            border-radius: 8px;
            padding: 15px;
            white-space: pre-wrap;
            max-height: 200px;
            overflow-y: auto;
        }
        .clear-btn {
            background: #95a5a6;
            color: white;
            padding: 6px 12px;
            font-size: 11px;
            border-radius: 4px;
        }
        .websocket-status {
            font-size: 12px;
            margin-top: 10px;
            padding: 8px;
            border-radius: 4px;
            background: #ecf0f1;
        }
        .endpoint-info {
            background: #f8f9fa;
            padding: 10px;
            border-radius: 6px;
            font-size: 11px;
            color: #6c757d;
            margin-bottom: 15px;
        }
        .protocol-tabs {
            display: flex;
            gap: 10px;
            margin-bottom: 20px;
            background: #ecf0f1;
            padding: 5px;
            border-radius: 10px;
        }
        .tab-btn {
            flex: 1;
            padding: 10px;
            border: none;
            border-radius: 6px;
            background: transparent;
            cursor: pointer;
            transition: all 0.3s ease;
            font-weight: 600;
        }
        .tab-btn.active {
            background: white;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .tab-content {
            display: none;
        }
        .tab-content.active {
            display: block;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎵 FastTrackStudio Transport</h1>
        <p class="subtitle">Multi-Protocol Test Interface - HTTP • WebSocket • OSC</p>

        <!-- Protocol Status Grid -->
        <div class="protocols-grid">
            <!-- HTTP Section -->
            <div class="protocol-section http">
                <h2>🌐 HTTP REST API <span class="status-indicator" id="httpStatus"></span></h2>
                <div class="endpoint-info">Port 3000 • http://localhost:3000</div>

                <div class="button-group">
                    <button class="play-btn" onclick="httpRequest('POST', '/play')">▶️ Play</button>
                    <button class="pause-btn" onclick="httpRequest('POST', '/pause')">⏸️ Pause</button>
                    <button class="stop-btn" onclick="httpRequest('POST', '/stop')">⏹️ Stop</button>
                </div>

                <div class="input-group">
                    <label>Tempo:</label>
                    <input type="number" id="httpTempo" value="140" min="1" max="300">
                    <button class="config-btn" onclick="setHttpTempo()">Set</button>
                </div>

                <div class="button-group">
                    <button class="query-btn" onclick="httpRequest('GET', '/status')">📊 Status</button>
                    <button class="query-btn" onclick="httpRequest('GET', '/health')">❤️ Health</button>
                </div>
            </div>

            <!-- WebSocket Section -->
            <div class="protocol-section websocket">
                <h2>🔌 WebSocket <span class="status-indicator" id="wsStatus"></span></h2>
                <div class="endpoint-info">Port 3001 • ws://localhost:3001/ws</div>

                <div class="button-group">
                    <button class="play-btn" onclick="wsCommand('play')" id="wsPlayBtn">▶️ Play</button>
                    <button class="pause-btn" onclick="wsCommand('pause')" id="wsPauseBtn">⏸️ Pause</button>
                    <button class="stop-btn" onclick="wsCommand('stop')" id="wsStopBtn">⏹️ Stop</button>
                </div>

                <div class="input-group">
                    <label>Tempo:</label>
                    <input type="number" id="wsTempo" value="120" min="1" max="300">
                    <button class="config-btn" onclick="setWsTempo()" id="wsTempoBtn">Set</button>
                </div>

                <div class="button-group">
                    <button class="query-btn" onclick="wsCommand('status')" id="wsStatusBtn">📊 Status</button>
                    <button class="config-btn" onclick="connectWebSocket()" id="wsConnectBtn">🔗 Connect</button>
                </div>

                <div class="websocket-status" id="wsConnectionStatus">
                    Disconnected - Click Connect to start
                </div>
            </div>

            <!-- OSC Section -->
            <div class="protocol-section osc">
                <h2>📡 OSC Protocol <span class="status-indicator" id="oscStatus"></span></h2>
                <div class="endpoint-info">UDP Port 9000 • Requires OSC client (oscsend)</div>

                <div style="background: #fff3cd; padding: 10px; border-radius: 6px; margin-bottom: 15px; font-size: 12px;">
                    <strong>OSC Commands (Terminal):</strong><br>
                    <code>oscsend localhost 9000 /transport/play</code><br>
                    <code>oscsend localhost 9000 /transport/tempo f 140.0</code><br>
                    <code>oscsend localhost 9000 /transport/status</code>
                </div>

                <div style="background: #d1ecf1; padding: 10px; border-radius: 6px; font-size: 12px;">
                    <strong>Available Addresses:</strong><br>
                    • /transport/play, /transport/pause, /transport/stop<br>
                    • /transport/tempo [float bpm]<br>
                    • /transport/position [float seconds]<br>
                    • /transport/status<br>
                    • /transport/is_playing, /transport/is_recording
                </div>
            </div>
        </div>

        <!-- Real-time Status Panel -->
        <div class="status-panel">
            <div class="current-status">
                <strong>🎼 Current Transport Status</strong>
                <div id="currentStatus">Initializing...</div>
            </div>

            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;">
                <strong>📋 Protocol Response Log</strong>
                <button class="clear-btn" onclick="clearLog()">🗑️ Clear</button>
            </div>
            <div class="log-output" id="logOutput">Ready to test multi-protocol transport server!
Try the buttons above to see responses from different protocols...</div>
        </div>
    </div>

    <script>
        let websocket = null;
        let wsReconnectTimer = null;

        // Initialize the interface
        document.addEventListener('DOMContentLoaded', function() {
            updateConnectionStatus();
            setInterval(checkHttpHealth, 5000);
            setInterval(updateCurrentStatus, 3000);
        });

        // HTTP Functions
        async function httpRequest(method, path, body = null) {
            const timestamp = new Date().toLocaleTimeString();
            const url = `http://localhost:3000${path}`;

            logMessage(`[HTTP ${timestamp}] ${method} ${path}`);

            try {
                const options = { method, headers: {} };

                if (body) {
                    options.headers['Content-Type'] = 'application/json';
                    options.body = JSON.stringify(body);
                    logMessage(`Request body: ${JSON.stringify(body)}`);
                }

                const response = await fetch(url, options);
                const statusIndicator = document.getElementById('httpStatus');

                if (response.ok) {
                    statusIndicator.className = 'status-indicator connected';
                    const responseText = await response.text();
                    logMessage(`✅ HTTP ${response.status}: ${responseText}`);

                    // Update current status if this was a status request
                    if (path === '/status' && responseText) {
                        try {
                            const statusData = JSON.parse(responseText);
                            updateStatusDisplay(statusData);
                        } catch (e) {
                            // Not JSON, that's ok
                        }
                    }
                } else {
                    statusIndicator.className = 'status-indicator error';
                    logMessage(`❌ HTTP ${response.status}: ${response.statusText}`);
                }

            } catch (error) {
                document.getElementById('httpStatus').className = 'status-indicator error';
                logMessage(`❌ HTTP Error: ${error.message}`);
            }

            logMessage('─'.repeat(50));
        }

        function setHttpTempo() {
            const bpm = parseFloat(document.getElementById('httpTempo').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                httpRequest('POST', '/set_tempo', { bpm: bpm });
            } else {
                logMessage('❌ Invalid tempo value');
            }
        }

        // WebSocket Functions
        function connectWebSocket() {
            if (websocket && websocket.readyState === WebSocket.OPEN) {
                websocket.close();
                return;
            }

            logMessage('[WebSocket] Connecting to ws://localhost:3001/ws...');

            websocket = new WebSocket('ws://localhost:3001/ws');

            websocket.onopen = function() {
                logMessage('✅ WebSocket connected');
                updateWSButtonStates(false);
                updateWSConnectionStatus('Connected - Real-time updates active');
                document.getElementById('wsStatus').className = 'status-indicator connected';
                document.getElementById('wsConnectBtn').textContent = '🔌 Disconnect';
            };

            websocket.onmessage = function(event) {
                const timestamp = new Date().toLocaleTimeString();
                logMessage(`[WebSocket ${timestamp}] Received: ${event.data}`);

                try {
                    const data = JSON.parse(event.data);
                    if (data.status) {
                        updateStatusDisplay(data.status);
                    }
                } catch (e) {
                    // Not JSON status update, just log it
                }
            };

            websocket.onclose = function() {
                logMessage('🔌 WebSocket disconnected');
                updateWSButtonStates(true);
                updateWSConnectionStatus('Disconnected');
                document.getElementById('wsStatus').className = 'status-indicator error';
                document.getElementById('wsConnectBtn').textContent = '🔗 Connect';

                // Auto-reconnect after 3 seconds
                if (wsReconnectTimer) clearTimeout(wsReconnectTimer);
                wsReconnectTimer = setTimeout(connectWebSocket, 3000);
            };

            websocket.onerror = function(error) {
                logMessage('❌ WebSocket error: ' + error);
                document.getElementById('wsStatus').className = 'status-indicator error';
            };
        }

        function wsCommand(command) {
            if (!websocket || websocket.readyState !== WebSocket.OPEN) {
                logMessage('❌ WebSocket not connected');
                return;
            }

            const message = { command: command };
            websocket.send(JSON.stringify(message));
            logMessage(`[WebSocket] Sent: ${JSON.stringify(message)}`);
        }

        function setWsTempo() {
            const bpm = parseFloat(document.getElementById('wsTempo').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                const message = { command: 'set_tempo', bpm: bpm };
                if (websocket && websocket.readyState === WebSocket.OPEN) {
                    websocket.send(JSON.stringify(message));
                    logMessage(`[WebSocket] Sent: ${JSON.stringify(message)}`);
                } else {
                    logMessage('❌ WebSocket not connected');
                }
            } else {
                logMessage('❌ Invalid tempo value');
            }

            // Utility Functions
            function updateWSButtonStates(disabled) {
                const buttons = ['wsPlayBtn', 'wsPauseBtn', 'wsStopBtn', 'wsTempoBtn', 'wsStatusBtn'];
                buttons.forEach(id => {
                    const btn = document.getElementById(id);
                    if (btn) btn.disabled = disabled;
                });
            }

            function updateWSConnectionStatus(message) {
                const status = document.getElementById('wsConnectionStatus');
                if (status) status.textContent = message;
            }

            function updateConnectionStatus() {
                // Set initial status indicators
                document.getElementById('httpStatus').className = 'status-indicator';
                document.getElementById('wsStatus').className = 'status-indicator error';
                document.getElementById('oscStatus').className = 'status-indicator'; // OSC is always "available"
            }

            async function checkHttpHealth() {
                try {
                    const response = await fetch('http://localhost:3000/health');
                    const indicator = document.getElementById('httpStatus');
                    if (response.ok) {
                        indicator.className = 'status-indicator connected';
                    } else {
                        indicator.className = 'status-indicator error';
                    }
                } catch (error) {
                    document.getElementById('httpStatus').className = 'status-indicator error';
                }
            }

            async function updateCurrentStatus() {
                try {
                    const response = await fetch('http://localhost:3000/status');
                    if (response.ok) {
                        const status = await response.json();
                        updateStatusDisplay(status);
                    }
                } catch (error) {
                    // Silently fail - don't spam the log
                }
            }

            function updateStatusDisplay(status) {
                const statusDiv = document.getElementById('currentStatus');
                if (status && statusDiv) {
                    statusDiv.innerHTML = `
                        <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-top: 10px;">
                            <div>▶️ State: <strong>${status.play_state || 'Unknown'}</strong></div>
                            <div>🎵 Tempo: <strong>${status.tempo?.bpm || 'Unknown'} BPM</strong></div>
                            <div>🎶 Time: <strong>${status.time_signature?.numerator || '?'}/${status.time_signature?.denominator || '?'}</strong></div>
                            <div>📍 Position: <strong>${status.playhead_position?.time?.seconds?.toFixed(2) || '0.00'}s</strong></div>
                            <div>🔴 Record: <strong>${status.record_mode || 'Unknown'}</strong></div>
                            <div>📊 Updated: <strong>${new Date().toLocaleTimeString()}</strong></div>
                        </div>
                    `;
                }
            }

            function logMessage(message) {
                const log = document.getElementById('logOutput');
                if (log) {
                    log.textContent += message + '\n';
                    log.scrollTop = log.scrollHeight;
                }
            }

            function clearLog() {
                const log = document.getElementById('logOutput');
                if (log) {
                    log.textContent = 'Log cleared. Ready for new protocol responses...\n';
                }
            }

            // Auto-connect WebSocket on page load
            setTimeout(connectWebSocket, 1000);

            // OSC status - always show as available since it's UDP
            document.getElementById('oscStatus').className = 'status-indicator connected';

        </script>
    </body>
    </html>"#;
