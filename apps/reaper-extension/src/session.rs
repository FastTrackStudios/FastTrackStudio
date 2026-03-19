//! Session integration — setlist services for RPC exposure.
//!
//! Imports the `session` crate and runs it in-process within the REAPER extension.
//! The session services (`SetlistServiceImpl`, `SongServiceImpl`) talk to REAPER
//! via an in-memory loopback connection to the DAW dispatcher.
//!
//! Action registration and handlers have moved to session-extension (SHM guest).

use daw::sync::LocalCaller;
use daw::Daw;
use session::{
    SetlistServiceDispatcher, SetlistServiceImpl, SongServiceDispatcher, SongServiceImpl,
};
use std::sync::OnceLock;
use tracing::info;

use crate::routed_handler::RoutedHandler;

/// Global session manager singleton.
static SESSION: OnceLock<SessionManager> = OnceLock::new();

/// Manages the session lifecycle within the REAPER extension.
pub struct SessionManager {
    setlist_service: SetlistServiceImpl,
    song_service: SongServiceImpl,
    /// The combined DAW + session handler for the Unix socket server.
    daw_handler: RoutedHandler,
    /// Keeps the in-process loopback connection alive.
    _local_caller: LocalCaller,
}

impl SessionManager {
    /// Get the global session manager (panics if not initialized).
    pub fn get() -> &'static SessionManager {
        SESSION.get().expect("SessionManager not initialized")
    }

    /// Try to get the global session manager.
    pub fn try_get() -> Option<&'static SessionManager> {
        SESSION.get()
    }

    /// Access the setlist service for dispatcher creation.
    pub fn setlist_service(&self) -> &SetlistServiceImpl {
        &self.setlist_service
    }

    /// Access the song service for dispatcher creation.
    pub fn song_service(&self) -> &SongServiceImpl {
        &self.song_service
    }

    /// Create a combined DAW + session handler for the Unix socket.
    pub fn create_handler(&self) -> RoutedHandler {
        use session::{setlist_service_service_descriptor, song_service_service_descriptor};

        let setlist_dispatcher = SetlistServiceDispatcher::new(self.setlist_service.clone());
        let song_dispatcher = SongServiceDispatcher::new(self.song_service.clone());

        self.daw_handler
            .clone()
            .with(setlist_service_service_descriptor(), setlist_dispatcher)
            .with(song_service_service_descriptor(), song_dispatcher)
    }
}

/// Initialize the session subsystem.
///
/// Creates an in-process loopback connection to the DAW services, initializes
/// the global `Daw` singleton, and creates the session services.
///
/// Must be called from within a tokio runtime context (e.g., `block_on`).
/// Must be called AFTER the DAW dispatcher is registered (so the loopback
/// connection has services to talk to).
pub async fn init(daw_handler: RoutedHandler) -> eyre::Result<()> {
    info!("Initializing session subsystem...");

    // Create in-process loopback connection to DAW services
    let local_caller = LocalCaller::new(daw_handler.clone()).await?;

    // Initialize the global Daw singleton so the session crate can call
    // daw-control methods that resolve locally via the loopback
    Daw::init(local_caller.erased_caller())?;
    info!("Daw singleton initialized via LocalCaller (in-process)");

    // Create session services
    let setlist_service = SetlistServiceImpl::new();
    let song_service = SongServiceImpl::new();

    SESSION
        .set(SessionManager {
            setlist_service,
            song_service,
            daw_handler,
            _local_caller: local_caller,
        })
        .map_err(|_| eyre::eyre!("SessionManager already initialized"))?;

    info!("Session subsystem initialized");
    Ok(())
}

// Session action registration and handlers moved to session-extension
// (registered via ActionRegistryService RPC, handled via subscribe_actions stream).
