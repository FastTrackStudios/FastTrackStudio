//! Session UI Components
//!
//! Dioxus components for rendering setlist and performance views.
//! Uses a signal-driven architecture with smart components that subscribe to granular
//! domain state.
//!
//! # Architecture
//!
//! This crate provides UI components that work with `session-proto` types and clients:
//!
//! - **Components**: Reusable UI primitives (progress bars, transport controls, etc.)
//! - **Layouts**: Complete view layouts (PerformanceLayout, etc.)
//! - **Signals**: Global state management via Dioxus signals
//! - **Session**: Singleton access to service clients (similar to `Daw::get()`)
//!
//! # Setup
//!
//! Initialize the `Session` singleton during app startup:
//!
//! ```rust,ignore
//! use session_ui::Session;
//! use session_proto::SetlistServiceClient;
//!
//! // During app startup
//! let client = SetlistServiceClient::new(connection);
//! Session::init(client).expect("Failed to init session");
//! ```
//!
//! # Usage
//!
//! Components call service methods directly via `Session::get()`:
//!
//! ```rust,ignore
//! use session_ui::{Session, PerformanceLayout};
//!
//! // In UI event handlers
//! spawn(async move {
//!     Session::get().setlist().play().await;
//! });
//!
//! // Render the performance view
//! rsx! {
//!     PerformanceLayout {}
//! }
//! ```

pub mod components;
pub mod layouts;
pub mod signals;

// Re-export key types for convenience
pub use layouts::top_bar::{ConnectionState, TopBar};
pub use layouts::PerformanceLayout;
pub use signals::{
    LatencyAction, LatencyInfo, LatencyMeasurement, LatencyTracker, Session, TransportState,
    ACTIVE_INDICES, AUDIO_LATENCY_SECONDS, LATENCY_INFO, LATENCY_TRACKER, PLAYBACK_STATE,
    SETLIST_STRUCTURE, SONG_TRANSPORT,
};
