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
//!                                     DAW Handler (in-process)
//!                                              │
//!                                              ▼
//!                                     daw-reaper (REAPER API)
//! ```

use actions_proto::ActionResult;
use daw_control::Daw;
use daw_control_sync::LocalCaller;
use session::{SetlistServiceImpl, SongServiceImpl};
use session_proto::{SetlistServiceDispatcher, SongServiceDispatcher};
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
        use session_proto::{
            setlist_service_service_descriptor, song_service_service_descriptor,
        };

        let setlist_dispatcher = SetlistServiceDispatcher::new(self.setlist_service.clone());
        let song_dispatcher = SongServiceDispatcher::new(self.song_service.clone());

        self.daw_handler
            .clone()
            .with(
                setlist_service_service_descriptor(),
                setlist_dispatcher,
            )
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

// =============================================================================
// Action Handlers
// =============================================================================
//
// These handlers are called from REAPER actions (main thread, synchronous).
// They use the tokio runtime to call async session service methods.

/// Handle: Build Setlist from open projects
pub fn handle_build_setlist() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };

    // The SetlistService::build_from_open_projects is async.
    // Dispatch to the tokio runtime via a spawned task.
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::build_from_open_projects(&setlist_svc).await;
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
    moire::task::spawn(async move {
        session_proto::SetlistService::next_song(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Previous Song
pub fn handle_previous_song() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::previous_song(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Next Section
pub fn handle_next_section() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::next_section(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Previous Section
pub fn handle_previous_section() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::previous_section(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Toggle Playback
pub fn handle_toggle_playback() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::toggle_playback(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Toggle Song Loop
pub fn handle_toggle_song_loop() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        session_proto::SetlistService::toggle_song_loop(&setlist_svc).await;
    });
    ActionResult::success()
}

/// Handle: Log Session Status
pub fn handle_log_status() -> ActionResult {
    let Some(session) = SessionManager::try_get() else {
        return ActionResult::failure("Session not initialized");
    };
    let setlist_svc = session.setlist_service().clone();
    moire::task::spawn(async move {
        let setlist = session_proto::SetlistService::get_setlist(&setlist_svc).await;
        match setlist {
            Ok(sl) => info!(
                "Session status: {} songs in setlist '{}'",
                sl.songs.len(),
                sl.name
            ),
            Err(_) => info!("Session status: no setlist loaded"),
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
