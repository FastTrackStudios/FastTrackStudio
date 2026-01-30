//! WebSocket client for ROAM HTTP bridge
//!
//! Implements the `roam-bridge.v1` WebSocket protocol for communicating with
//! the HTTP gateway extension running in REAPER.
//!
//! Protocol:
//! - Connect to `/api/@ws` with subprotocol `roam-bridge.v1`
//! - Send JSON requests: `{"type": "request", "id": N, "service": "...", "method": "...", "args": [...]}`
//! - Receive responses: `{"type": "response", "id": N, "result": ...}` or `{"type": "error", "id": N, "error": "..."}`
//! - Receive stream data: `{"type": "data", "channel": N, "value": ...}`

use dioxus::prelude::*;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use tracing::{debug, error, info, warn};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CloseEvent, ErrorEvent, MessageEvent, WebSocket};

/// Connection status for the WebSocket
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionStatus {
    Disconnected,
    Connecting,
    Connected,
    Error,
}

/// Request message sent to the ROAM bridge
#[derive(Debug, Serialize)]
pub struct Request {
    #[serde(rename = "type")]
    pub msg_type: &'static str,
    pub id: u64,
    pub service: String,
    pub method: String,
    pub args: Vec<serde_json::Value>,
}

/// Server message received from the ROAM bridge
#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
pub enum ServerMessage {
    #[serde(rename = "response")]
    Response {
        id: u64,
        result: serde_json::Value,
    },
    #[serde(rename = "error")]
    Error {
        id: u64,
        error: String,
    },
    #[serde(rename = "data")]
    Data {
        channel: u64,
        value: serde_json::Value,
    },
    #[serde(rename = "reset")]
    Reset {
        channel: u64,
    },
}

/// Callback type for handling responses
type ResponseCallback = Box<dyn FnOnce(Result<serde_json::Value, String>)>;

/// Callback type for handling stream data
type DataCallback = Rc<dyn Fn(serde_json::Value)>;

/// Internal state for the WebSocket connection
struct WsState {
    socket: Option<WebSocket>,
    pending_requests: HashMap<u64, ResponseCallback>,
    data_handlers: HashMap<u64, DataCallback>,
}

/// ROAM WebSocket client for making RPC calls and subscribing to data streams
#[derive(Clone)]
pub struct RoamWebSocket {
    state: Rc<RefCell<WsState>>,
    next_id: Rc<AtomicU64>,
    status: Signal<ConnectionStatus>,
    last_error: Signal<Option<String>>,
}

impl RoamWebSocket {
    /// Create a new WebSocket client (not yet connected)
    pub fn new() -> Self {
        Self {
            state: Rc::new(RefCell::new(WsState {
                socket: None,
                pending_requests: HashMap::new(),
                data_handlers: HashMap::new(),
            })),
            next_id: Rc::new(AtomicU64::new(1)),
            status: Signal::new(ConnectionStatus::Disconnected),
            last_error: Signal::new(None),
        }
    }

    /// Get the current connection status
    pub fn status(&self) -> ConnectionStatus {
        *self.status.read()
    }

    /// Get the last error message, if any
    pub fn last_error(&self) -> Option<String> {
        self.last_error.read().clone()
    }

    /// Check if connected
    pub fn is_connected(&self) -> bool {
        self.status() == ConnectionStatus::Connected
    }

    /// Connect to the ROAM bridge WebSocket
    pub fn connect(&self, url: &str) {
        // Don't reconnect if already connected or connecting
        if matches!(
            self.status(),
            ConnectionStatus::Connected | ConnectionStatus::Connecting
        ) {
            return;
        }

        info!("Connecting to ROAM bridge: {}", url);

        // Get mutable access to signals
        let mut status = self.status;
        let mut last_error = self.last_error;

        status.set(ConnectionStatus::Connecting);
        last_error.set(None);

        // Create WebSocket with subprotocol
        let ws = match WebSocket::new_with_str(url, "roam-bridge.v1") {
            Ok(ws) => ws,
            Err(e) => {
                error!("Failed to create WebSocket: {:?}", e);
                status.set(ConnectionStatus::Error);
                last_error.set(Some(format!("Failed to create WebSocket: {:?}", e)));
                return;
            }
        };

        // Set binary type for potential future binary messages
        ws.set_binary_type(web_sys::BinaryType::Arraybuffer);

        // Clone references for closures
        let state_clone = self.state.clone();

        // onopen handler
        let mut status_open = status;
        let onopen = Closure::wrap(Box::new(move |_: JsValue| {
            info!("WebSocket connected");
            status_open.set(ConnectionStatus::Connected);
        }) as Box<dyn FnMut(JsValue)>);
        ws.set_onopen(Some(onopen.as_ref().unchecked_ref()));
        onopen.forget();

        // onclose handler
        let mut status_close = status;
        let onclose = Closure::wrap(Box::new(move |e: CloseEvent| {
            info!("WebSocket closed: code={}, reason={}", e.code(), e.reason());
            status_close.set(ConnectionStatus::Disconnected);
        }) as Box<dyn FnMut(CloseEvent)>);
        ws.set_onclose(Some(onclose.as_ref().unchecked_ref()));
        onclose.forget();

        // onerror handler
        let mut status_error = status;
        let mut error_set = last_error;
        let onerror = Closure::wrap(Box::new(move |e: ErrorEvent| {
            let msg = e.message();
            error!("WebSocket error: {}", msg);
            status_error.set(ConnectionStatus::Error);
            error_set.set(Some(msg));
        }) as Box<dyn FnMut(ErrorEvent)>);
        ws.set_onerror(Some(onerror.as_ref().unchecked_ref()));
        onerror.forget();

        // onmessage handler
        let state_msg = state_clone.clone();
        let onmessage = Closure::wrap(Box::new(move |e: MessageEvent| {
            if let Some(text) = e.data().as_string() {
                debug!("Received: {}", text);
                match serde_json::from_str::<ServerMessage>(&text) {
                    Ok(msg) => {
                        let mut state = state_msg.borrow_mut();
                        match msg {
                            ServerMessage::Response { id, result } => {
                                if let Some(callback) = state.pending_requests.remove(&id) {
                                    callback(Ok(result));
                                }
                            }
                            ServerMessage::Error { id, error } => {
                                if let Some(callback) = state.pending_requests.remove(&id) {
                                    callback(Err(error));
                                }
                            }
                            ServerMessage::Data { channel, value } => {
                                if let Some(handler) = state.data_handlers.get(&channel) {
                                    handler(value);
                                }
                            }
                            ServerMessage::Reset { channel } => {
                                warn!("Channel {} reset", channel);
                                state.data_handlers.remove(&channel);
                            }
                        }
                    }
                    Err(e) => {
                        warn!("Failed to parse message: {} - {}", e, text);
                    }
                }
            }
        }) as Box<dyn FnMut(MessageEvent)>);
        ws.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
        onmessage.forget();

        // Store the socket
        self.state.borrow_mut().socket = Some(ws);
    }

    /// Disconnect from the WebSocket
    pub fn disconnect(&self) {
        if let Some(ws) = self.state.borrow_mut().socket.take() {
            let _ = ws.close();
        }
        let mut status = self.status;
        status.set(ConnectionStatus::Disconnected);
    }

    /// Make an RPC call to a service method
    pub fn call<F>(&self, service: &str, method: &str, args: Vec<serde_json::Value>, callback: F)
    where
        F: FnOnce(Result<serde_json::Value, String>) + 'static,
    {
        let state = self.state.borrow();
        let Some(ws) = &state.socket else {
            callback(Err("Not connected".to_string()));
            return;
        };

        if ws.ready_state() != WebSocket::OPEN {
            callback(Err("WebSocket not open".to_string()));
            return;
        }

        let id = self.next_id.fetch_add(1, Ordering::SeqCst);
        let request = Request {
            msg_type: "request",
            id,
            service: service.to_string(),
            method: method.to_string(),
            args,
        };

        let json = match serde_json::to_string(&request) {
            Ok(j) => j,
            Err(e) => {
                callback(Err(format!("Failed to serialize request: {}", e)));
                return;
            }
        };

        debug!("Sending: {}", json);
        drop(state); // Release borrow before mutating

        // Store the callback
        self.state
            .borrow_mut()
            .pending_requests
            .insert(id, Box::new(callback));

        // Send the request
        let state = self.state.borrow();
        if let Some(ws) = &state.socket {
            if let Err(e) = ws.send_with_str(&json) {
                drop(state);
                if let Some(callback) = self.state.borrow_mut().pending_requests.remove(&id) {
                    callback(Err(format!("Failed to send: {:?}", e)));
                }
            }
        }
    }

    /// Fire-and-forget RPC call (no callback)
    pub fn call_ignore(&self, service: &str, method: &str, args: Vec<serde_json::Value>) {
        self.call(service, method, args, |result| {
            if let Err(e) = result {
                warn!("RPC call failed: {}", e);
            }
        });
    }

    /// Subscribe to a data channel
    pub fn subscribe<F>(&self, channel: u64, handler: F)
    where
        F: Fn(serde_json::Value) + 'static,
    {
        self.state
            .borrow_mut()
            .data_handlers
            .insert(channel, Rc::new(handler));
    }

    /// Unsubscribe from a data channel
    pub fn unsubscribe(&self, channel: u64) {
        self.state.borrow_mut().data_handlers.remove(&channel);
    }
}

impl Default for RoamWebSocket {
    fn default() -> Self {
        Self::new()
    }
}

/// Hook to use the ROAM WebSocket client
///
/// Automatically connects once on mount. Does not auto-reconnect to avoid loops.
pub fn use_roam_websocket() -> RoamWebSocket {
    // Get the WebSocket from context
    let ws = use_context::<RoamWebSocket>();

    // Track if we've already attempted connection (prevents reconnection loops)
    let has_connected = use_hook(|| Rc::new(RefCell::new(false)));

    // Clone for use in the effect
    let ws_effect = ws.clone();
    let has_connected_effect = has_connected.clone();

    // Auto-connect once on mount
    use_effect(move || {
        // Only connect once per component lifetime
        if *has_connected_effect.borrow() {
            return;
        }

        if !ws_effect.is_connected() && ws_effect.status() != ConnectionStatus::Connecting {
            *has_connected_effect.borrow_mut() = true;

            // Build WebSocket URL from current location
            let window = web_sys::window().expect("no window");
            let location = window.location();
            let protocol = location.protocol().unwrap_or_else(|_| "http:".to_string());
            let host = location.host().unwrap_or_else(|_| "localhost:9250".to_string());

            let ws_protocol = if protocol == "https:" { "wss:" } else { "ws:" };
            let url = format!("{}//{}/api/@ws", ws_protocol, host);

            info!("Auto-connecting to ROAM bridge: {}", url);
            ws_effect.connect(&url);
        }
    });

    ws
}

/// Provide the ROAM WebSocket context to child components
#[component]
pub fn RoamProvider(children: Element) -> Element {
    let ws = use_hook(RoamWebSocket::new);

    // Provide the WebSocket to the context
    use_context_provider(|| ws.clone());

    children
}

/// Status indicator component showing connection state
#[component]
pub fn ConnectionStatusIndicator() -> Element {
    let ws = use_roam_websocket();

    let (color, text) = match ws.status() {
        ConnectionStatus::Disconnected => ("bg-gray-500", "Disconnected"),
        ConnectionStatus::Connecting => ("bg-yellow-500 animate-pulse", "Connecting..."),
        ConnectionStatus::Connected => ("bg-green-500", "Connected"),
        ConnectionStatus::Error => ("bg-red-500", "Error"),
    };

    rsx! {
        div { class: "flex items-center gap-2 text-sm",
            div { class: "w-2 h-2 rounded-full {color}" }
            span { class: "text-muted-foreground", "{text}" }
            if let Some(error) = ws.last_error() {
                span { class: "text-red-500 text-xs", "({error})" }
            }
        }
    }
}
