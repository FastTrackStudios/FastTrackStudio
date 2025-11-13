//! WebSocket Enhanced Transport Test Server
//!
//! This example creates a web server that serves both the transport HTTP API,
//! WebSocket real-time API, and a comprehensive HTML interface with buttons
//! to test both REST and WebSocket endpoints.

use transport::{
    core::Transport,
    infra::{create_transport_http_router, create_transport_ws_router, WebSocketHandler},
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
    println!("🌐 Transport WebSocket + HTTP Test Server");
    println!("==========================================");

    // Create transport state
    let transport = Transport::new();
    let transport_state = Arc::new(Mutex::new(transport));

    // Create WebSocket handler
    let ws_handler = WebSocketHandler::new(transport_state.clone());

    // Create the main app with HTTP API, WebSocket API, and web interface
    let app = Router::new()
        // Serve the HTML test page
        .route("/", get(serve_test_page))
        // Mount the HTTP transport API
        .nest("/api/transport", create_transport_http_router::<Transport>())
        .with_state(transport_state.clone())
        // Mount the WebSocket transport API
        .merge(
            create_transport_ws_router::<Transport>()
                .with_state(ws_handler)
        );

    println!("🚀 Starting server on http://localhost:3005");
    println!();
    println!("📱 Open your browser and go to:");
    println!("   http://localhost:3005");
    println!();
    println!("🎮 Features:");
    println!("   • HTTP REST API buttons");
    println!("   • Real-time WebSocket connection");
    println!("   • Live status updates");
    println!("   • Dual protocol testing");
    println!();
    println!("Press Ctrl+C to shutdown");

    // Start server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3005").await?;
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<std::net::SocketAddr>(),
    )
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
    <title>Transport API Test (HTTP + WebSocket)</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            max-width: 1200px;
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
        .protocol-section {
            border: 2px solid #eee;
            border-radius: 8px;
            padding: 20px;
            margin: 20px 0;
        }
        .http-section { border-color: #2196F3; }
        .websocket-section { border-color: #4CAF50; }
        h1 { color: #333; text-align: center; }
        h2 { color: #666; border-bottom: 2px solid #eee; padding-bottom: 10px; }
        h3 { color: #444; margin-top: 0; }
        .status-display {
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 4px;
            padding: 15px;
            margin: 15px 0;
            font-family: 'Monaco', monospace;
            font-size: 14px;
        }
        .connection-status {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 12px;
            font-size: 12px;
            font-weight: bold;
            margin-left: 10px;
        }
        .connected { background: #d4edda; color: #155724; }
        .disconnected { background: #f8d7da; color: #721c24; }
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
        .ws-btn { background: #4CAF50; color: white; }
        button:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
        button:active { transform: translateY(0); }
        button:disabled {
            background: #ccc;
            cursor: not-allowed;
            transform: none;
            box-shadow: none;
        }
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
        .live-status {
            background: #e8f5e8;
            border: 2px solid #4CAF50;
            padding: 15px;
            border-radius: 8px;
            margin: 15px 0;
        }
        .status-item {
            display: inline-block;
            margin: 5px 10px 5px 0;
            padding: 4px 8px;
            background: white;
            border-radius: 4px;
            font-size: 13px;
        }
        .split-container {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
        }
        @media (max-width: 800px) {
            .split-container {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎵 Transport API Test Interface</h1>
        <p style="text-align: center; color: #666;">Test both HTTP REST API and WebSocket real-time API</p>

        <!-- Live Status Display -->
        <div class="live-status">
            <h3>📊 Live Transport Status
                <span id="wsStatus" class="connection-status disconnected">WebSocket: Disconnected</span>
            </h3>
            <div id="liveStatus">
                <span class="status-item">🎮 Playing: <span id="livePlaying">false</span></span>
                <span class="status-item">🔴 Recording: <span id="liveRecording">false</span></span>
                <span class="status-item">🎼 Tempo: <span id="liveTempo">120</span> BPM</span>
                <span class="status-item">📍 Position: <span id="livePosition">0.0</span>s</span>
                <span class="status-item">🎵 Time Sig: <span id="liveTimeSig">4/4</span></span>
                <span class="status-item">📝 Record Mode: <span id="liveRecordMode">normal</span></span>
            </div>
        </div>

        <div class="split-container">
            <!-- HTTP REST API Section -->
            <div class="protocol-section http-section">
                <h3>🌐 HTTP REST API</h3>

                <h4>🎮 Playback Controls</h4>
                <div class="button-group">
                    <button class="play-btn" onclick="makeHttpRequest('POST', '/api/transport/play')">▶️ Play</button>
                    <button class="pause-btn" onclick="makeHttpRequest('POST', '/api/transport/pause')">⏸️ Pause</button>
                    <button class="stop-btn" onclick="makeHttpRequest('POST', '/api/transport/stop')">⏹️ Stop</button>
                </div>

                <h4>🔴 Recording Controls</h4>
                <div class="button-group">
                    <button class="record-btn" onclick="makeHttpRequest('POST', '/api/transport/start_recording')">🔴 Start Rec</button>
                    <button class="record-btn" onclick="makeHttpRequest('POST', '/api/transport/stop_recording')">⭕ Stop Rec</button>
                </div>

                <h4>⚙️ Configuration</h4>
                <div class="input-group">
                    <label>Tempo:</label>
                    <input type="number" id="httpTempoInput" value="140" min="1" max="300">
                    <button class="config-btn" onclick="setHttpTempo()">🎼 Set</button>
                </div>

                <h4>📊 Queries</h4>
                <div class="button-group">
                    <button class="query-btn" onclick="makeHttpRequest('GET', '/api/transport/status')">📋 Status</button>
                    <button class="query-btn" onclick="makeHttpRequest('GET', '/api/transport/is_playing')">🎮 Playing?</button>
                </div>
            </div>

            <!-- WebSocket Real-time API Section -->
            <div class="protocol-section websocket-section">
                <h3>⚡ WebSocket Real-time API</h3>

                <div class="button-group">
                    <button id="wsConnect" class="ws-btn" onclick="connectWebSocket()">🔌 Connect</button>
                    <button id="wsDisconnect" class="ws-btn" onclick="disconnectWebSocket()" disabled>🔌 Disconnect</button>
                </div>

                <h4>🎮 WebSocket Playback Controls</h4>
                <div class="button-group">
                    <button class="play-btn" onclick="sendWsCommand('Play')" id="wsPlay" disabled>▶️ WS Play</button>
                    <button class="pause-btn" onclick="sendWsCommand('Pause')" id="wsPause" disabled>⏸️ WS Pause</button>
                    <button class="stop-btn" onclick="sendWsCommand('Stop')" id="wsStop" disabled>⏹️ WS Stop</button>
                </div>

                <h4>🔴 WebSocket Recording</h4>
                <div class="button-group">
                    <button class="record-btn" onclick="sendWsCommand('StartRecording')" id="wsStartRec" disabled>🔴 WS Start</button>
                    <button class="record-btn" onclick="sendWsCommand('StopRecording')" id="wsStopRec" disabled>⭕ WS Stop</button>
                </div>

                <h4>⚙️ WebSocket Config</h4>
                <div class="input-group">
                    <label>Tempo:</label>
                    <input type="number" id="wsTempoInput" value="128" min="1" max="300">
                    <button class="config-btn" onclick="setWsTempo()" id="wsSetTempo" disabled>🎼 WS Set</button>
                </div>

                <h4>📊 WebSocket Queries</h4>
                <div class="button-group">
                    <button class="query-btn" onclick="sendWsCommand('GetStatus')" id="wsGetStatus" disabled>📋 WS Status</button>
                    <button class="query-btn" onclick="sendWsCommand('IsPlaying')" id="wsIsPlaying" disabled>🎮 WS Playing?</button>
                </div>
            </div>
        </div>

        <div style="display: flex; justify-content: space-between; align-items: center;">
            <h2>📺 API Response Log</h2>
            <button class="clear-btn" onclick="clearOutput()">🗑️ Clear Log</button>
        </div>
        <div id="output">Ready to test transport API endpoints!
HTTP and WebSocket protocols available...</div>
    </div>

    <script>
        let websocket = null;
        let wsConnected = false;

        // HTTP Request Function
        async function makeHttpRequest(method, url, body = null) {
            const timestamp = new Date().toLocaleTimeString();
            const output = document.getElementById('output');

            output.textContent += `\n[${timestamp}] HTTP ${method} ${url}`;
            if (body) {
                output.textContent += `\nBody: ${JSON.stringify(body, null, 2)}`;
            }

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
                output.textContent += `\nResponse: ${response.status} ${response.statusText}`;

                const responseText = await response.text();
                if (responseText) {
                    try {
                        const json = JSON.parse(responseText);
                        output.textContent += `\n${JSON.stringify(json, null, 2)}`;
                    } catch (e) {
                        output.textContent += `\n${responseText}`;
                    }
                }

                output.textContent += '\n' + '='.repeat(50) + '\n';

            } catch (error) {
                output.textContent += `\nError: ${error.message}\n`;
                output.textContent += '\n' + '='.repeat(50) + '\n';
            }

            output.scrollTop = output.scrollHeight;
        }

        // WebSocket Functions
        function connectWebSocket() {
            const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            const wsUrl = `${protocol}//${window.location.host}/ws`;

            const output = document.getElementById('output');
            const timestamp = new Date().toLocaleTimeString();

            output.textContent += `\n[${timestamp}] Connecting to WebSocket: ${wsUrl}\n`;

            websocket = new WebSocket(wsUrl);

            websocket.onopen = function(event) {
                wsConnected = true;
                updateWsStatus(true);
                const timestamp = new Date().toLocaleTimeString();
                output.textContent += `[${timestamp}] WebSocket connected!\n`;
                output.textContent += '='.repeat(50) + '\n';
                output.scrollTop = output.scrollHeight;
            };

            websocket.onmessage = function(event) {
                const timestamp = new Date().toLocaleTimeString();
                try {
                    const message = JSON.parse(event.data);
                    output.textContent += `\n[${timestamp}] WebSocket message received:`;
                    output.textContent += `\n${JSON.stringify(message, null, 2)}`;

                    // Update live status if it's a status update
                    if (message.type === 'StatusUpdate') {
                        updateLiveStatus(message.data);
                    }

                    output.textContent += '\n' + '='.repeat(50) + '\n';
                } catch (e) {
                    output.textContent += `\n[${timestamp}] WebSocket raw message: ${event.data}\n`;
                    output.textContent += '='.repeat(50) + '\n';
                }
                output.scrollTop = output.scrollHeight;
            };

            websocket.onclose = function(event) {
                wsConnected = false;
                updateWsStatus(false);
                const timestamp = new Date().toLocaleTimeString();
                output.textContent += `\n[${timestamp}] WebSocket disconnected\n`;
                output.textContent += '='.repeat(50) + '\n';
                output.scrollTop = output.scrollHeight;
            };

            websocket.onerror = function(error) {
                const timestamp = new Date().toLocaleTimeString();
                output.textContent += `\n[${timestamp}] WebSocket error: ${error}\n`;
                output.textContent += '='.repeat(50) + '\n';
                output.scrollTop = output.scrollHeight;
            };
        }

        function disconnectWebSocket() {
            if (websocket) {
                websocket.close();
                websocket = null;
                wsConnected = false;
                updateWsStatus(false);
            }
        }

        function sendWsCommand(action, data = {}) {
            if (!websocket || !wsConnected) {
                alert('WebSocket not connected! Click "Connect" first.');
                return;
            }

            const message = {
                type: 'Command',
                data: {
                    action: action,
                    ...data
                }
            };

            const timestamp = new Date().toLocaleTimeString();
            const output = document.getElementById('output');

            output.textContent += `\n[${timestamp}] Sending WebSocket command:`;
            output.textContent += `\n${JSON.stringify(message, null, 2)}\n`;

            websocket.send(JSON.stringify(message));
            output.scrollTop = output.scrollHeight;
        }

        function updateWsStatus(connected) {
            const statusEl = document.getElementById('wsStatus');
            const connectBtn = document.getElementById('wsConnect');
            const disconnectBtn = document.getElementById('wsDisconnect');

            // Update status display
            if (connected) {
                statusEl.textContent = 'WebSocket: Connected';
                statusEl.className = 'connection-status connected';
            } else {
                statusEl.textContent = 'WebSocket: Disconnected';
                statusEl.className = 'connection-status disconnected';
            }

            // Update button states
            connectBtn.disabled = connected;
            disconnectBtn.disabled = !connected;

            // Update WebSocket control buttons
            const wsButtons = ['wsPlay', 'wsPause', 'wsStop', 'wsStartRec', 'wsStopRec', 'wsSetTempo', 'wsGetStatus', 'wsIsPlaying'];
            wsButtons.forEach(id => {
                document.getElementById(id).disabled = !connected;
            });
        }

        function updateLiveStatus(status) {
            document.getElementById('livePlaying').textContent = status.is_playing;
            document.getElementById('liveRecording').textContent = status.is_recording;
            document.getElementById('liveTempo').textContent = status.tempo_bpm;
            document.getElementById('livePosition').textContent = status.position_seconds.toFixed(1);
            document.getElementById('liveTimeSig').textContent = `${status.time_signature.numerator}/${status.time_signature.denominator}`;
            document.getElementById('liveRecordMode').textContent = status.record_mode;
        }

        // Configuration Functions
        function setHttpTempo() {
            const bpm = parseFloat(document.getElementById('httpTempoInput').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                makeHttpRequest('POST', '/api/transport/set_tempo', { bpm: bpm });
            } else {
                alert('Please enter a valid tempo between 1 and 300 BPM');
            }
        }

        function setWsTempo() {
            const bpm = parseFloat(document.getElementById('wsTempoInput').value);
            if (bpm && bpm > 0 && bpm <= 300) {
                sendWsCommand('SetTempo', { bpm: bpm });
            } else {
                alert('Please enter a valid tempo between 1 and 300 BPM');
            }
        }

        function clearOutput() {
            document.getElementById('output').textContent = 'Ready to test transport API endpoints!\nHTTP and WebSocket protocols available...';
        }

        // Initialize the page
        window.addEventListener('load', function() {
            updateWsStatus(false);

            // Auto-connect WebSocket after a short delay
            setTimeout(() => {
                connectWebSocket();
            }, 1000);
        });

        // Cleanup on page unload
        window.addEventListener('beforeunload', function() {
            if (websocket) {
                websocket.close();
            }
        });
    </script>
</body>
</html>"#;
