//! Session integration — setlist management, navigation, auto-advance.
//!
//! Imports the `session` crate and runs it in-process within the REAPER extension.
//! The session services (`SetlistServiceImpl`, `SongServiceImpl`) talk to REAPER
//! via an in-memory loopback connection to the DAW dispatcher, so all RPC calls
//! resolve locally without leaving the process.
//!
//! # Architecture
//!
//! ```text
//!  REAPER Actions ──► SessionManager ──► SetlistServiceImpl
//!                                              │
//!                                              ▼
//!                                     Daw (loopback conn)
//!                                              │
//!                                              ▼
//!                                     DAW Dispatcher (in-process)
//!                                              │
//!                                              ▼
//!                                     daw-reaper (REAPER API)
//! ```

use actions_proto::ActionResult;
use daw_control::Daw;
use roam::session::{ConnectionHandle, HandshakeConfig, ServiceDispatcher};
use roam_session::RoutedDispatcher;
use roam_stream::LengthPrefixedFramed;
use session::{SetlistServiceImpl, SongServiceImpl};
use session_proto::{SetlistServiceDispatcher, SongServiceDispatcher};
use std::sync::OnceLock;
use tracing::{debug, info, warn};

/// Global session manager singleton.
static SESSION: OnceLock<SessionManager> = OnceLock::new();

/// Manages the session lifecycle within the REAPER extension.
pub struct SessionManager {
    setlist_service: SetlistServiceImpl,
    song_service: SongServiceImpl,
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

    /// Create a combined session dispatcher for the Unix socket chain.
    pub fn create_dispatcher(&self) -> impl ServiceDispatcher + Clone {
        let setlist_dispatcher = SetlistServiceDispatcher::new(self.setlist_service.clone());
        let song_dispatcher = SongServiceDispatcher::new(self.song_service.clone());
        RoutedDispatcher::new(setlist_dispatcher, song_dispatcher)
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
pub async fn init<D>(daw_dispatcher: D) -> eyre::Result<()>
where
    D: ServiceDispatcher + Clone + Send + Sync + 'static,
{
    info!("Initializing session subsystem...");

    // Create in-process loopback connection to DAW services
    let handle = create_loopback_connection(daw_dispatcher).await?;

    // Initialize the global Daw singleton so the session crate can call
    // daw-control methods that resolve locally via the loopback
    Daw::init(handle)?;
    info!("Daw singleton initialized via in-process loopback");

    // Create session services
    let setlist_service = SetlistServiceImpl::new();
    let song_service = SongServiceImpl::new();

    SESSION
        .set(SessionManager {
            setlist_service,
            song_service,
        })
        .map_err(|_| eyre::eyre!("SessionManager already initialized"))?;

    info!("Session subsystem initialized");
    Ok(())
}

/// Create an in-process loopback connection to the DAW dispatcher.
///
/// Uses `tokio::io::duplex` for a zero-copy in-memory bidirectional pipe,
/// wrapped in `LengthPrefixedFramed` for roam message framing.
async fn create_loopback_connection<D>(dispatcher: D) -> eyre::Result<ConnectionHandle>
where
    D: ServiceDispatcher + Clone + Send + Sync + 'static,
{
    // In-memory bidirectional pipe (256 KB buffer for high-frequency updates)
    let (client_stream, server_stream) = tokio::io::duplex(256 * 1024);

    let client_framed = LengthPrefixedFramed::new(client_stream);
    let server_framed = LengthPrefixedFramed::new(server_stream);

    // Large credit for 60Hz transport streaming
    let config = HandshakeConfig {
        max_payload_size: 1024 * 1024,            // 1 MiB
        initial_channel_credit: 16 * 1024 * 1024, // 16 MiB
        max_concurrent_requests: 64,
        ..Default::default()
    };

    // Server side: accepts and dispatches DAW service requests
    let server_config = config.clone();
    tokio::spawn(async move {
        match roam_session::accept_framed(server_framed, server_config, dispatcher).await {
            Ok((_handle, _incoming, driver)) => {
                if let Err(e) = driver.run().await {
                    warn!("DAW loopback server driver ended: {}", e);
                }
            }
            Err(e) => {
                warn!("DAW loopback server accept failed: {}", e);
            }
        }
    });

    // Client side: we get the ConnectionHandle for Daw::init()
    let (handle, _incoming, driver) =
        roam_session::initiate_framed(client_framed, config, roam_session::NoDispatcher).await?;

    tokio::spawn(async move {
        if let Err(e) = driver.run().await {
            warn!("DAW loopback client driver ended: {}", e);
        }
    });

    debug!("Created in-process loopback connection to DAW services");
    Ok(handle)
}

// =============================================================================
// Action Handlers
// =============================================================================
//
// These handlers are called from REAPER actions (main thread, synchronous).
// They use the tokio runtime to call async session service methods.

/// Create a dummy roam Context for direct (non-RPC) service calls.
fn dummy_context() -> roam::session::Context {
    roam::session::Context::new(
        roam_wire::ConnectionId::ROOT,
        roam_wire::RequestId::new(0),
        roam_wire::MethodId::new(0),
        roam_wire::Metadata::default(),
        Vec::new(),
    )
}

/// Handle: Build Setlist from open projects
pub fn handle_build_setlist() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };

    // The SetlistService::build_from_open_projects is async.
    // Dispatch to the tokio runtime via a spawned task.
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-build-setlist", async move {
        let cx = dummy_context();
        session_proto::SetlistService::build_from_open_projects(&setlist_svc, &cx).await;
        info!("Setlist built from open projects");
    });

    ActionResult::success_with_message("Building setlist...")
}

/// Handle: Next Song
pub fn handle_next_song() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-next-song", async move {
        let cx = dummy_context();
        session_proto::SetlistService::next_song(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Previous Song
pub fn handle_previous_song() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-prev-song", async move {
        let cx = dummy_context();
        session_proto::SetlistService::previous_song(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Next Section
pub fn handle_next_section() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-next-section", async move {
        let cx = dummy_context();
        session_proto::SetlistService::next_section(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Previous Section
pub fn handle_previous_section() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-prev-section", async move {
        let cx = dummy_context();
        session_proto::SetlistService::previous_section(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Toggle Playback
pub fn handle_toggle_playback() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-toggle-playback", async move {
        let cx = dummy_context();
        session_proto::SetlistService::toggle_playback(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Toggle Song Loop
pub fn handle_toggle_song_loop() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-toggle-song-loop", async move {
        let cx = dummy_context();
        session_proto::SetlistService::toggle_song_loop(&setlist_svc, &cx).await;
    });
    ActionResult::success()
}

/// Handle: Log Session Status
pub fn handle_log_status() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    peeps::spawn_tracked!("session-log-status", async move {
        let cx = dummy_context();
        let setlist = session_proto::SetlistService::get_setlist(&setlist_svc, &cx).await;
        match setlist {
            Some(sl) => info!(
                "Session status: {} songs in setlist '{}'",
                sl.songs.len(),
                sl.name
            ),
            None => info!("Session status: no setlist loaded"),
        }
    });
    ActionResult::success()
}

// =============================================================================
// REAPER Action Definitions
// =============================================================================

// Session actions for the REAPER action list.
// Uses `define_actions!` macro to generate action IDs and wire them
// to the handler functions above.
actions_proto::define_actions! {
    pub session_reaper_actions {
        prefix: "fts.session",
        BUILD_SETLIST = "build_setlist" {
            name: "Build Setlist",
            description: "Build setlist from all open REAPER project tabs",
            category: Session,
            group: "Session",
            implementation: supported(super::handle_build_setlist),
        }
        NEXT_SONG = "next_song" {
            name: "Next Song",
            description: "Navigate to the next song in the setlist",
            category: Session,
            group: "Session/Navigate",
            implementation: supported(super::handle_next_song),
        }
        PREVIOUS_SONG = "previous_song" {
            name: "Previous Song",
            description: "Navigate to the previous song in the setlist",
            category: Session,
            group: "Session/Navigate",
            implementation: supported(super::handle_previous_song),
        }
        NEXT_SECTION = "next_section" {
            name: "Next Section",
            description: "Navigate to the next section in the current song",
            category: Session,
            group: "Session/Navigate",
            implementation: supported(super::handle_next_section),
        }
        PREVIOUS_SECTION = "previous_section" {
            name: "Previous Section",
            description: "Navigate to the previous section in the current song",
            category: Session,
            group: "Session/Navigate",
            implementation: supported(super::handle_previous_section),
        }
        TOGGLE_PLAYBACK = "toggle_playback" {
            name: "Toggle Playback (Session)",
            description: "Toggle play/pause via session service",
            category: Transport,
            group: "Session/Transport",
            implementation: supported(super::handle_toggle_playback),
        }
        TOGGLE_SONG_LOOP = "toggle_song_loop" {
            name: "Toggle Song Loop",
            description: "Toggle looping for the current song",
            category: Transport,
            group: "Session/Transport",
            implementation: supported(super::handle_toggle_song_loop),
        }
        LOG_STATUS = "log_status" {
            name: "Log Session Status",
            description: "Log current session state to console",
            category: Dev,
            group: "Session/Dev",
            implementation: supported(super::handle_log_status),
        }
    }
}

/// Get all session action registrations for `builtin_local_actions()`.
pub fn session_action_registrations() -> Vec<actions_proto::LocalActionRegistration> {
    session_reaper_actions::definitions_with_handlers()
}
