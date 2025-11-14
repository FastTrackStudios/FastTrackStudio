//! FastTrackStudio Multi-Protocol Transport Server
//!
//! This is the main FastTrackStudio server application that provides transport
//! control through multiple protocols:
//!
//! - HTTP REST API for web integration and general API access
//! - WebSocket for real-time bidirectional communication
//! - OSC (Open Sound Control) for professional audio software integration
//! - Web interface for interactive testing and monitoring
//!
//! ## Architecture
//!
//! ```
//! ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
//! │    HTTP     │  │  WebSocket  │  │    OSC      │  │ Web Interface│
//! │   :8000     │  │   :8001     │  │   :9000     │  │    :8080     │
//! └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘
//!        │                │                │                │
//!        └────────────────┼────────────────┼────────────────┘
//!                         │                │
//!               ┌─────────▼────────────────▼────────┐
//!               │      FastTrackStudio             │
//!               │    Transport Service             │
//!               │   (Shared State & Logic)         │
//!               └──────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! Start the server:
//! ```bash
//! cargo run -p server
//! ```
//!
//! Access points:
//! - Web Interface: http://localhost:8080
//! - HTTP API: http://localhost:8000
//! - WebSocket: ws://localhost:8001/ws
//! - OSC: UDP port 9000

use axum::{response::Html, routing::get, Router};
use std::sync::Arc;
use tokio::sync::Mutex;
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use transport::{
    core::Tempo,
    infra::{
        http::create_transport_http_router,
        osc::create_transport_osc_server,
        websocket::{create_transport_ws_router, WebSocketHandler},
    },
    MockTransportService, TransportActions,
};

/// FastTrackStudio server configuration
#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub http_port: u16,
    pub websocket_port: u16,
    pub osc_port: u16,
    pub web_interface_port: u16,
    pub host: String,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            http_port: 8000,
            websocket_port: 8001,
            osc_port: 9000,
            web_interface_port: 8080,
            host: "0.0.0.0".to_string(),
        }
    }
}

/// Main server application
pub struct FastTrackStudioServer {
    config: ServerConfig,
    transport_service: Arc<Mutex<MockTransportService>>,
}

impl FastTrackStudioServer {
    /// Create a new FastTrackStudio server with default configuration
    pub fn new() -> Self {
        let transport_service = MockTransportService::new();
        Self {
            config: ServerConfig::default(),
            transport_service: Arc::new(Mutex::new(transport_service)),
        }
    }

    /// Create a new FastTrackStudio server with custom configuration
    pub fn with_config(config: ServerConfig) -> Self {
        let transport_service = MockTransportService::new();
        Self {
            config,
            transport_service: Arc::new(Mutex::new(transport_service)),
        }
    }

    /// Start all protocol servers
    pub async fn start(&self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("🎵 FastTrackStudio Multi-Protocol Server");
        println!("========================================");

        // Display initial transport state
        self.print_initial_status().await;

        println!("🚀 Starting all protocol servers...");
        println!();

        // Clone transport for each protocol server
        let http_transport = Arc::clone(&self.transport_service);
        let websocket_transport = Arc::clone(&self.transport_service);
        let osc_transport = Arc::clone(&self.transport_service);
        let web_transport = Arc::clone(&self.transport_service);

        // Start HTTP REST API server
        let http_task = {
            let config = self.config.clone();
            tokio::spawn(async move {
                start_http_server(http_transport, &config).await;
            })
        };

        // Start WebSocket server
        let websocket_task = {
            let config = self.config.clone();
            tokio::spawn(async move {
                start_websocket_server(websocket_transport, &config).await;
            })
        };

        // Start OSC server
        let osc_task = {
            let config = self.config.clone();
            tokio::spawn(async move {
                start_osc_server(osc_transport, &config).await;
                // Keep the task alive even if OSC server fails
                loop {
                    tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
                }
            })
        };

        // Start Web Interface server
        let web_task = {
            let config = self.config.clone();
            tokio::spawn(async move {
                start_web_interface_server(web_transport, &config).await;
            })
        };

        // Allow servers to initialize
        tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

        self.print_server_info();

        // Start demo routine
        self.start_demo_routine().await;

        // Wait for any server to stop or shutdown signal
        tokio::select! {
            _ = http_task => println!("🌐 HTTP server stopped"),
            _ = websocket_task => println!("🔌 WebSocket server stopped"),
            _ = osc_task => println!("📡 OSC server stopped"),
            _ = web_task => println!("🖥️  Web interface stopped"),
            _ = tokio::signal::ctrl_c() => {
                println!("\n🛑 Shutdown signal received");
            }
        }

        println!("✅ FastTrackStudio server stopped gracefully");
        Ok(())
    }

    /// Print initial transport status
    async fn print_initial_status(&self) {
        let transport = self.transport_service.lock().await;
        match transport.get_transport() {
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

    /// Print server information and endpoints
    fn print_server_info(&self) {
        let config = &self.config;
        println!("🎯 FastTrackStudio is running on all protocols!");
        println!();
        println!("📋 Available Endpoints:");
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│ 🖥️  Web Interface (Interactive Testing)                    │");
        println!(
            "│   http://{}:{}                                   │",
            if config.host == "0.0.0.0" {
                "localhost"
            } else {
                &config.host
            },
            config.web_interface_port
        );
        println!("├─────────────────────────────────────────────────────────────┤");
        println!("│ 🌐 HTTP REST API                                          │");
        println!(
            "│   http://{}:{}/*                              │",
            if config.host == "0.0.0.0" {
                "localhost"
            } else {
                &config.host
            },
            config.http_port
        );
        println!("│   GET  /status, /health, /tempo, /position                 │");
        println!("│   POST /play, /pause, /stop, /set_tempo                    │");
        println!("├─────────────────────────────────────────────────────────────┤");
        println!("│ 🔌 WebSocket Real-time                                    │");
        println!(
            "│   ws://{}:{}/ws                               │",
            if config.host == "0.0.0.0" {
                "localhost"
            } else {
                &config.host
            },
            config.websocket_port
        );
        println!("│   Real-time bidirectional transport control               │");
        println!("├─────────────────────────────────────────────────────────────┤");
        println!("│ 📡 OSC Protocol (Professional Audio Integration)          │");
        println!(
            "│   UDP port {}                                             │",
            config.osc_port
        );
        println!("│   /transport/play, /transport/pause, /transport/stop       │");
        println!("│   /transport/tempo [float], /transport/status              │");
        println!("└─────────────────────────────────────────────────────────────┘");
        println!();
        println!("💡 Quick Test Commands:");
        println!();
        println!("   HTTP API:");
        println!("   curl -X POST http://localhost:{}/play", config.http_port);
        println!("   curl http://localhost:{}/status", config.http_port);
        println!();
        println!("   OSC Commands:");
        println!("   oscsend localhost {} /transport/play", config.osc_port);
        println!(
            "   oscsend localhost {} /transport/tempo f 140.0",
            config.osc_port
        );
        println!();
        println!(
            "🌟 Open http://localhost:{} for interactive testing!",
            config.web_interface_port
        );
        println!("🔧 Press Ctrl+C to shutdown gracefully");
        println!("========================================");
    }

    /// Start demonstration routine showing cross-protocol integration
    async fn start_demo_routine(&self) {
        let demo_transport = Arc::clone(&self.transport_service);
        tokio::spawn(async move {
            // Wait for servers to be ready
            tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

            println!("\n🎬 Starting integration demonstration...");
            println!("   This shows how all protocols share the same transport state");

            // Demo: Change tempo through shared service
            {
                let mut transport = demo_transport.lock().await;
                println!("📝 [Demo] Setting tempo to 125 BPM via shared service...");
                if let Ok(msg) = transport.set_tempo(Tempo::new(125.0)) {
                    println!("   ✅ {}", msg);
                }
            }

            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

            // Demo: Start playback through shared service
            {
                let mut transport = demo_transport.lock().await;
                println!("📝 [Demo] Starting playback via shared service...");
                if let Ok(msg) = transport.play() {
                    println!("   ✅ {}", msg);
                }
            }

            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

            // Show current state
            {
                let transport = demo_transport.lock().await;
                if let Ok(state) = transport.get_transport() {
                    println!("🎼 [Demo] Current state after changes:");
                    println!("   ▶️  Play State: {:?}", state.play_state);
                    println!("   🎵 Tempo: {:.1} BPM", state.tempo.bpm);
                    println!(
                        "   📍 Position: {:.2}s",
                        state.playhead_position.time.to_seconds()
                    );
                }
            }

            println!();
            println!("💡 [Demo] All protocols now reflect these changes!");
            println!("   Test with HTTP, WebSocket, or OSC to see shared state.");
            println!("   Server will continue running until Ctrl+C is pressed.");
            println!();
        });
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // You can customize the configuration here
    let config = ServerConfig {
        http_port: 8000,
        websocket_port: 8001,
        osc_port: 9000,
        web_interface_port: 8080,
        host: "0.0.0.0".to_string(),
    };

    let server = FastTrackStudioServer::with_config(config);
    server.start().await
}

/// Start HTTP REST API server
async fn start_http_server(transport: Arc<Mutex<MockTransportService>>, config: &ServerConfig) {
    println!("🌐 Starting HTTP REST API server...");

    let router = create_transport_http_router();
    let app = router
        .route("/health", get(|| async { "FastTrackStudio HTTP API OK" }))
        .route("/info", get(|| async {
            "FastTrackStudio Transport HTTP API v1.0\nSupports: play, pause, stop, tempo, position, recording"
        }))
        .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
        .with_state(transport);

    let addr = format!("{}:{}", config.host, config.http_port);
    match tokio::net::TcpListener::bind(&addr).await {
        Ok(listener) => {
            println!(
                "✅ HTTP REST API listening on http://localhost:{}",
                config.http_port
            );
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ HTTP server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind HTTP server on {}: {}", addr, e),
    }
}

/// Start WebSocket server
async fn start_websocket_server(
    transport: Arc<Mutex<MockTransportService>>,
    config: &ServerConfig,
) {
    println!("🔌 Starting WebSocket server...");

    let ws_handler = WebSocketHandler::new(transport);
    let ws_router = create_transport_ws_router();

    let app = ws_router
        .route("/health", get(|| async { "FastTrackStudio WebSocket OK" }))
        .route(
            "/info",
            get(|| async {
                "FastTrackStudio WebSocket API v1.0\nConnect to /ws for real-time transport control"
            }),
        )
        .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
        .with_state(ws_handler);

    let addr = format!("{}:{}", config.host, config.websocket_port);
    match tokio::net::TcpListener::bind(&addr).await {
        Ok(listener) => {
            println!(
                "✅ WebSocket server listening on ws://localhost:{}/ws",
                config.websocket_port
            );
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ WebSocket server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind WebSocket server on {}: {}", addr, e),
    }
}

/// Start OSC server
async fn start_osc_server(transport: Arc<Mutex<MockTransportService>>, config: &ServerConfig) {
    println!("📡 Starting OSC server...");

    let addr = format!("{}:{}", config.host, config.osc_port);
    match create_transport_osc_server(transport, &addr).await {
        Ok(()) => println!("✅ OSC server listening on UDP port {}", config.osc_port),
        Err(e) => {
            eprintln!("❌ OSC server error: {}", e);
            println!("   OSC server will be disabled, other protocols remain active");
        }
    }
}

/// Start web interface server
async fn start_web_interface_server(
    transport: Arc<Mutex<MockTransportService>>,
    config: &ServerConfig,
) {
    println!("🖥️  Starting web interface server...");

    let app = Router::new()
        .route("/", get(serve_web_interface))
        .route(
            "/health",
            get(|| async { "FastTrackStudio Web Interface OK" }),
        )
        .layer(ServiceBuilder::new().layer(CorsLayer::permissive()))
        .with_state(transport);

    let addr = format!("{}:{}", config.host, config.web_interface_port);
    match tokio::net::TcpListener::bind(&addr).await {
        Ok(listener) => {
            println!(
                "✅ Web interface listening on http://localhost:{}",
                config.web_interface_port
            );
            if let Err(e) = axum::serve(listener, app).await {
                eprintln!("❌ Web interface server error: {}", e);
            }
        }
        Err(e) => eprintln!("❌ Failed to bind web interface server on {}: {}", addr, e),
    }
}

/// Serve the main web interface
async fn serve_web_interface() -> Html<String> {
    Html(WEB_INTERFACE_HTML.to_string())
}

/// HTML for the web interface
const WEB_INTERFACE_HTML: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FastTrackStudio - Transport Control</title>
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
            max-width: 1000px;
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
        .status-panel {
            background: #2c3e50;
            color: #ecf0f1;
            border-radius: 10px;
            padding: 20px;
            margin-bottom: 20px;
            font-family: 'Monaco', 'Menlo', monospace;
            font-size: 14px;
        }
        .protocol-section {
            background: white;
            border-radius: 15px;
            padding: 20px;
            margin-bottom: 20px;
            box-shadow: 0 10px 20px rgba(0,0,0,0.1);
            border-left: 5px solid #3498db;
        }
        .protocol-section h2 {
            margin-top: 0;
            color: #2c3e50;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .button-group {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            margin: 15px 0;
        }
        button {
            padding: 12px 20px;
            border: none;
            border-radius: 8px;
            cursor: pointer;
            font-size: 14px;
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
        .log-area {
            background: #1e2832;
            color: #ecf0f1;
            padding: 15px;
            border-radius: 8px;
            font-family: 'Monaco', 'Menlo', monospace;
            font-size: 12px;
            max-height: 200px;
            overflow-y: auto;
            white-space: pre-wrap;
        }
        .endpoint-info {
            background: #f8f9fa;
            padding: 10px;
            border-radius: 6px;
            font-size: 11px;
            color: #6c757d;
            margin-bottom: 15px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎵 FastTrackStudio</h1>
        <p class="subtitle">Multi-Protocol Transport Control Interface</p>

        <div class="status-panel">
            <strong>🎼 Transport Status</strong>
            <div id="currentStatus">Loading...</div>
        </div>

        <div class="protocol-section">
            <h2>🌐 HTTP REST API (Port 8000)</h2>
            <div class="endpoint-info">Base URL: http://localhost:8000</div>

            <div class="button-group">
                <button class="play-btn" onclick="httpRequest('POST', '/play')">▶️ Play</button>
                <button class="pause-btn" onclick="httpRequest('POST', '/pause')">⏸️ Pause</button>
                <button class="stop-btn" onclick="httpRequest('POST', '/stop')">⏹️ Stop</button>
                <button class="query-btn" onclick="httpRequest('GET', '/status')">📊 Status</button>
            </div>

            <div class="input-group">
                <label>Tempo (BPM):</label>
                <input type="number" id="httpTempo" value="140" min="1" max="300">
                <button class="config-btn" onclick="setHttpTempo()">Set Tempo</button>
            </div>
        </div>

        <div class="protocol-section">
            <h2>🔌 WebSocket (Port 8001)</h2>
            <div class="endpoint-info">WebSocket URL: ws://localhost:8001/ws</div>

            <div class="button-group">
                <button class="config-btn" onclick="connectWebSocket()" id="wsConnectBtn">🔗 Connect</button>
                <button class="play-btn" onclick="wsCommand('play')" disabled id="wsPlayBtn">▶️ Play</button>
                <button class="pause-btn" onclick="wsCommand('pause')" disabled id="wsPauseBtn">⏸️ Pause</button>
                <button class="stop-btn" onclick="wsCommand('stop')" disabled id="wsStopBtn">⏹️ Stop</button>
            </div>

            <div id="wsStatus">Disconnected</div>
        </div>

        <div class="protocol-section">
            <h2>📡 OSC Protocol (Port 9000)</h2>
            <div class="endpoint-info">
                <strong>Commands (using oscsend):</strong><br>
                <code>oscsend localhost 9000 /transport/play</code><br>
                <code>oscsend localhost 9000 /transport/tempo f 140.0</code><br>
                <code>oscsend localhost 9000 /transport/status</code>
            </div>
        </div>

        <div class="protocol-section">
            <h2>📋 Response Log</h2>
            <button onclick="clearLog()" style="float: right; padding: 5px 10px; font-size: 12px;">Clear</button>
            <div class="log-area" id="logArea">Ready for transport commands...</div>
        </div>
    </div>

    <script>
        let websocket = null;

        // Initialize
        document.addEventListener('DOMContentLoaded', function() {
            updateStatus();
            setInterval(updateStatus, 5000);
        });

        // HTTP Functions
        async function httpRequest(method, path, body = null) {
            const timestamp = new Date().toLocaleTimeString();
            const url = `http://localhost:8000${path}`;

            logMessage(`[${timestamp}] HTTP ${method} ${path}`);

            try {
                const options = { method, headers: {} };

                if (body) {
                    options.headers['Content-Type'] = 'application/json';
                    options.body = JSON.stringify(body);
                }

                const response = await fetch(url, options);
                const responseText = await response.text();

                if (response.ok) {
                    logMessage(`✅ ${response.status}: ${responseText}`);
                    if (path === '/status') {
                        try {
                            const status = JSON.parse(responseText);
                            updateStatusDisplay(status);
                        } catch (e) {}
                    }
                } else {
                    logMessage(`❌ ${response.status}: ${responseText}`);
                }

            } catch (error) {
                logMessage(`❌ Error: ${error.message}`);
            }
        }

        function setHttpTempo() {
            const bpm = parseFloat(document.getElementById('httpTempo').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                httpRequest('POST', '/set_tempo', { bpm: bpm });
            }
        }

        // WebSocket Functions
        function connectWebSocket() {
            if (websocket && websocket.readyState === WebSocket.OPEN) {
                websocket.close();
                return;
            }

            websocket = new WebSocket('ws://localhost:8001/ws');

            websocket.onopen = function() {
                logMessage('[WebSocket] Connected');
                updateWSButtons(false);
                document.getElementById('wsStatus').textContent = 'Connected';
                document.getElementById('wsConnectBtn').textContent = '🔌 Disconnect';
            };

            websocket.onmessage = function(event) {
                logMessage(`[WebSocket] Received: ${event.data}`);
                try {
                    const data = JSON.parse(event.data);
                    if (data.status) {
                        updateStatusDisplay(data.status);
                    }
                } catch (e) {}
            };

            websocket.onclose = function() {
                logMessage('[WebSocket] Disconnected');
                updateWSButtons(true);
                document.getElementById('wsStatus').textContent = 'Disconnected';
                document.getElementById('wsConnectBtn').textContent = '🔗 Connect';
            };

            websocket.onerror = function(error) {
                logMessage(`[WebSocket] Error: ${error}`);
            };
        }

        function wsCommand(command) {
            if (websocket && websocket.readyState === WebSocket.OPEN) {
                const message = { command: command };
                websocket.send(JSON.stringify(message));
                logMessage(`[WebSocket] Sent: ${JSON.stringify(message)}`);
            }
        }

        function updateWSButtons(disabled) {
            ['wsPlayBtn', 'wsPauseBtn', 'wsStopBtn'].forEach(id => {
                document.getElementById(id).disabled = disabled;
            });
        }

        // Utility Functions
        async function updateStatus() {
            try {
                const response = await fetch('http://localhost:8000/status');
                if (response.ok) {
                    const status = await response.json();
                    updateStatusDisplay(status);
                }
            } catch (e) {
                // Silently fail
            }
        }

        function updateStatusDisplay(status) {
            const statusDiv = document.getElementById('currentStatus');
            if (status) {
                statusDiv.innerHTML = `
                    <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 10px;">
                        <div>▶️ State: <strong>${status.play_state || 'Unknown'}</strong></div>
                        <div>🎵 Tempo: <strong>${status.tempo?.bpm || 'Unknown'} BPM</strong></div>
                        <div>🎶 Time: <strong>${status.time_signature?.numerator || '?'}/${status.time_signature?.denominator || '?'}</strong></div>
                        <div>📍 Position: <strong>${status.playhead_position?.time?.seconds?.toFixed(2) || '0.00'}s</strong></div>
                    </div>
                `;
            }
        }

        function logMessage(message) {
            const log = document.getElementById('logArea');
            const timestamp = new Date().toLocaleTimeString();
            log.textContent += `[${timestamp}] ${message}\n`;
            log.scrollTop = log.scrollHeight;
        }

        function clearLog() {
            document.getElementById('logArea').textContent = 'Log cleared...\n';
        }
    </script>
</body>
</html>"#;
