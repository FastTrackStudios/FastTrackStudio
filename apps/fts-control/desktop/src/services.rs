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
//!                      │  RoutedHandler   │
//!                      │ (routes by       │
//!                      │  method ID)      │
//!                      └──────────────────┘
//! ```

use crate::routed_handler::RoutedHandler;
use daw::sync::LocalCaller;
use roam::ErasedCaller;
use session::{SetlistServiceImpl, SongServiceImpl};
use session_proto::{
    SetlistServiceClient, SetlistServiceDispatcher, SongServiceDispatcher,
    setlist_service_service_descriptor, song_service_service_descriptor,
};

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

    /// Create a combined handler for all local services.
    ///
    /// The handler routes RPC calls to the appropriate service based on
    /// method ID. This can be passed to the WebSocket gateway to handle
    /// incoming browser connections.
    pub fn create_handler(&self) -> RoutedHandler {
        let setlist_dispatcher = SetlistServiceDispatcher::new(self.setlist.clone());
        let song_dispatcher = SongServiceDispatcher::new(self.song.clone());

        RoutedHandler::new()
            .with(setlist_service_service_descriptor(), setlist_dispatcher)
            .with(song_service_service_descriptor(), song_dispatcher)
    }

    /// Create an in-process loopback connection to the local services.
    ///
    /// Uses `LocalCaller` for the memory channel setup. Returns both the
    /// `ErasedCaller` and the `LocalCaller` (which must be kept alive).
    pub async fn create_loopback_connection(&self) -> eyre::Result<(ErasedCaller, LocalCaller)> {
        let handler = self.create_handler();
        let local = LocalCaller::new(handler).await?;
        Ok((local.erased_caller(), local))
    }

    /// Create a SetlistServiceClient connected to the local services.
    ///
    /// This is the main entry point for UI code to access session services.
    /// Returns the client and the `LocalCaller` keep-alive handle.
    pub async fn create_setlist_client(&self) -> eyre::Result<(SetlistServiceClient, LocalCaller)> {
        let (caller, local) = self.create_loopback_connection().await?;
        Ok((SetlistServiceClient::new(caller), local))
    }
}

impl Default for LocalServices {
    fn default() -> Self {
        Self::new()
    }
}
