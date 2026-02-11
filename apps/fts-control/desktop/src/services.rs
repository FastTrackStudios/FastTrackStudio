//! Local Session Services
//!
//! Instantiates session services in-process rather than via separate cell processes.
//! This provides the same functionality as the session cell but without the SHM
//! complexity.
//!
//! # Architecture
//!
//! ```text
//!                      ┌──────────────────┐
//!                      │  LocalServices   │
//!                      │                  │
//!                      │ ┌──────────────┐ │
//!                      │ │SetlistService│ │
//!                      │ └──────────────┘ │
//!                      │ ┌──────────────┐ │
//!                      │ │ SongService  │ │
//!                      │ └──────────────┘ │
//!                      └────────┬─────────┘
//!                               │
//!                               ▼
//!                      ┌──────────────────┐
//!                      │ RoutedDispatcher │
//!                      │ (routes by       │
//!                      │  method ID)      │
//!                      └──────────────────┘
//! ```

use roam::session::{ConnectionHandle, HandshakeConfig, ServiceDispatcher};
use roam_session::RoutedDispatcher;
use roam_stream::LengthPrefixedFramed;
use session::{SetlistServiceImpl, SongServiceImpl};
use session_proto::{SetlistServiceClient, SetlistServiceDispatcher, SongServiceDispatcher};
use tracing::debug;

/// Local session services running in-process.
///
/// Unlike the cell-based approach where services run in separate processes
/// communicating via SHM, these services run directly in the desktop app process.
#[allow(dead_code)]
pub struct LocalServices {
    /// Setlist management service
    setlist: SetlistServiceImpl,
    /// Song management service
    song: SongServiceImpl,
}

#[allow(dead_code)]
impl LocalServices {
    /// Create new local services.
    pub fn new() -> Self {
        Self {
            setlist: SetlistServiceImpl::new(),
            song: SongServiceImpl::new(),
        }
    }

    /// Get a reference to the setlist service implementation.
    pub fn setlist(&self) -> &SetlistServiceImpl {
        &self.setlist
    }

    /// Get a reference to the song service implementation.
    pub fn song(&self) -> &SongServiceImpl {
        &self.song
    }

    /// Create a combined dispatcher for all local services.
    ///
    /// The dispatcher routes RPC calls to the appropriate service based on
    /// method ID. This can be passed to the WebSocket gateway to handle
    /// incoming browser connections.
    pub fn create_dispatcher(&self) -> impl ServiceDispatcher + Clone {
        let setlist_dispatcher = SetlistServiceDispatcher::new(self.setlist.clone());
        let song_dispatcher = SongServiceDispatcher::new(self.song.clone());

        RoutedDispatcher::new(setlist_dispatcher, song_dispatcher)
    }

    /// Create an in-process loopback connection to the local services.
    ///
    /// This creates a `ConnectionHandle` that routes RPC calls directly to
    /// the local service implementations via an in-memory duplex pipe.
    /// Used to initialize `Session::init()` for UI components.
    pub async fn create_loopback_connection(&self) -> eyre::Result<ConnectionHandle> {
        // Create an in-memory bidirectional pipe (larger buffer for high-frequency updates)
        let (client_stream, server_stream) = tokio::io::duplex(256 * 1024);

        // Create the dispatcher for the server side
        let dispatcher = self.create_dispatcher();

        // Wrap both sides in length-prefixed framing
        let client_framed = LengthPrefixedFramed::new(client_stream);
        let server_framed = LengthPrefixedFramed::new(server_stream);

        // Use larger credit for high-frequency streaming (60Hz transport updates)
        // Default is 64KB which can exhaust quickly
        let config = HandshakeConfig {
            max_payload_size: 1024 * 1024,            // 1 MiB (default)
            initial_channel_credit: 16 * 1024 * 1024, // 16 MiB - effectively infinite for local
            max_concurrent_requests: 64,
        };

        // Start the server side (accepts connection and handles requests)
        let server_config = config.clone();
        tokio::spawn(async move {
            match roam_session::accept_framed(server_framed, server_config, dispatcher).await {
                Ok((_handle, _incoming, driver)) => {
                    // Run the driver to process requests
                    if let Err(e) = driver.run().await {
                        tracing::warn!("Loopback server driver error: {}", e);
                    }
                }
                Err(e) => {
                    tracing::error!("Loopback server accept failed: {}", e);
                }
            }
        });

        // Connect from the client side
        let (handle, _incoming, driver) =
            roam_session::initiate_framed(client_framed, config, roam_session::NoDispatcher)
                .await?;

        // Spawn the client driver
        tokio::spawn(async move {
            if let Err(e) = driver.run().await {
                tracing::warn!("Loopback client driver error: {}", e);
            }
        });

        debug!("Created loopback connection to local services");
        Ok(handle)
    }

    /// Create a SetlistServiceClient connected to the local services.
    ///
    /// This is the main entry point for UI code to access session services.
    pub async fn create_setlist_client(&self) -> eyre::Result<SetlistServiceClient> {
        let handle = self.create_loopback_connection().await?;
        Ok(SetlistServiceClient::new(handle))
    }
}

impl Default for LocalServices {
    fn default() -> Self {
        Self::new()
    }
}
