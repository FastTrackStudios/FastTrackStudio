//! REAPER Extension - FastTrackStudio2 Plugin
//!
//! This is a REAPER extension that:
//! 1. Registers in-process DAW dispatchers (ReaperTransport, ReaperProject, etc.)
//! 2. Exposes DAW services via Unix socket (`/tmp/fts-daw-{pid}.sock`) for roam RPC
//! 3. Uses ActionsRegistry to register REAPER actions
//! 4. Builds menus in the Extensions menu
//!
//! The extension provides DAW services that can be called by external applications
//! (like the fts-control desktop app) via roam over a Unix socket.

mod action_registry;
mod auto_color;
mod dock_icon;
mod global;
mod keyflow_actions;
mod local_actions;
mod menu;
mod routed_handler;
mod session;
mod signal_actions;
pub(crate) mod signal_bridge;
mod setlist_nav;
mod signal_save;
mod sync_bridge;
mod toolbar_manager;
mod visibility;

use fragile::Fragile;
use reaper_high::{MainTaskMiddleware, Reaper as HighReaper};
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::cell::RefCell;
use std::error::Error;
use std::ffi::CString;
use std::sync::OnceLock;
use tracing::{debug, info, warn};

use global::Global;
use routed_handler::RoutedHandler;

// ============================================================================
// RT-Aware Global Allocator (Helgobox pattern)
// ============================================================================

/// Global allocator that offloads deallocations from REAPER's real-time audio
/// thread to a background "daw-deallocator" thread.
///
/// Inspired by Helgobox (https://github.com/helgoboss/helgobox).
#[cfg(feature = "rt-allocator")]
#[global_allocator]
static ALLOCATOR: daw_allocator::FtsAllocator = daw_allocator::FtsAllocator::new();
#[cfg(not(feature = "rt-allocator"))]
static ALLOCATOR: daw_allocator::FtsAllocator = daw_allocator::FtsAllocator::new();
use std::path::PathBuf;
use tokio::net::UnixListener;

// ============================================================================
// Eager Plugin Loading (Helgobox pattern)
// ============================================================================

/// Loaded plugin libraries — kept alive for the process lifetime.
static LOADED_PLUGINS: OnceLock<Vec<libloading::Library>> = OnceLock::new();

/// Eagerly load FTS CLAP plugins and call `ReaperPluginEntry` on them.
///
/// This follows the Helgobox pattern: the extension loads the plugin .dylib
/// and calls its `ReaperPluginEntry`, giving the plugin its own initialized
/// `PluginContext`. Each .dylib has separate Rust statics, so the plugin
/// gets its own `reaper-high::Reaper`, `TaskSupport`, and `daw-reaper`.
fn eagerly_load_fts_plugins(context: PluginContext) {
    let mut loaded = Vec::new();

    // Reconstruct the raw plugin info from our PluginContext
    let ext_context = match context.type_specific() {
        reaper_low::TypeSpecificPluginContext::Extension(ext) => ext,
        _ => {
            warn!("Cannot eagerly load plugins: not an extension context");
            return;
        }
    };
    let mut raw_info = ext_context.to_raw();
    let h_instance = context.h_instance();

    // Search paths for fts-macros.clap bundle
    let resource_path = HighReaper::get().resource_path().to_path_buf();
    let home = std::env::var("HOME").unwrap_or_default();
    let candidates: Vec<PathBuf> = vec![
        // REAPER resource path (portable install)
        resource_path.join("UserPlugins/FX/fts-macros.clap/Contents/MacOS/fts-macros").into(),
        // Alternate: underscore variant (cdylib naming)
        resource_path.join("UserPlugins/FX/fts-macros.clap/Contents/MacOS/fts_macros").into(),
        // Standard system CLAP directory (macOS)
        PathBuf::from(&home)
            .join("Library/Audio/Plug-Ins/CLAP/fts-macros.clap/Contents/MacOS/fts-macros"),
    ];

    for path in &candidates {
        if !path.exists() {
            debug!("FTS plugin not found at: {}", path.display());
            continue;
        }

        info!("Eagerly loading fts-macros from: {}", path.display());
        match unsafe { libloading::Library::new(path) } {
            Ok(lib) => {
                // Look up ReaperPluginEntry symbol
                type EntryFn = unsafe extern "C" fn(
                    reaper_low::raw::HINSTANCE,
                    *mut reaper_low::raw::reaper_plugin_info_t,
                ) -> std::os::raw::c_int;

                match unsafe { lib.get::<EntryFn>(b"ReaperPluginEntry\0") } {
                    Ok(entry_fn) => {
                        let result =
                            unsafe { entry_fn(h_instance, &mut raw_info as *mut _) };
                        if result != 0 {
                            info!("FTS Macros: ReaperPluginEntry succeeded (result={})", result);
                            loaded.push(lib);
                        } else {
                            warn!("FTS Macros: ReaperPluginEntry returned 0 (init failed)");
                        }
                    }
                    Err(e) => {
                        warn!(
                            "FTS Macros: ReaperPluginEntry symbol not found: {}",
                            e
                        );
                    }
                }
                break; // Found the plugin, stop searching
            }
            Err(e) => {
                warn!("Failed to load fts-macros from {}: {}", path.display(), e);
            }
        }
    }

    let _ = LOADED_PLUGINS.set(loaded);
}

// Service dispatchers for method ID routing
use daw::service::{
    AudioEngineServiceDispatcher, ExtStateServiceDispatcher, FxServiceDispatcher,
    HealthServiceDispatcher, ItemServiceDispatcher, LiveMidiServiceDispatcher,
    MarkerServiceDispatcher, MidiAnalysisServiceDispatcher, MidiServiceDispatcher,
    ProjectServiceDispatcher, RegionServiceDispatcher, RoutingServiceDispatcher,
    TakeServiceDispatcher, TempoMapServiceDispatcher, TrackServiceDispatcher,
    TransportServiceDispatcher,
};

// ============================================================================
// Application State
// ============================================================================

/// Global application state
struct App {
    session: RefCell<ReaperSession>,
    #[allow(dead_code)]
    tokio_runtime: tokio::runtime::Runtime,
    /// Main task middleware for processing TaskSupport queued tasks
    task_middleware: RefCell<MainTaskMiddleware>,
}

// Manual Debug impl since ReaperSession and Runtime don't implement Debug
impl std::fmt::Debug for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("App").finish_non_exhaustive()
    }
}

impl App {
    fn new(session: ReaperSession) -> Result<Self, Box<dyn Error>> {
        // Create a tokio runtime for async operations
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()?;

        // Initialize Global (creates TaskSupport channels)
        Global::init();

        // Create task middleware for processing queued tasks
        let task_middleware = Global::get().create_task_middleware();

        Ok(Self {
            session: RefCell::new(session),
            tokio_runtime,
            task_middleware: RefCell::new(task_middleware),
        })
    }

    /// Process pending main thread tasks (called from timer callback)
    fn process_tasks(&self) {
        self.task_middleware.borrow_mut().run();
    }

    /// Initialize the extension (register actions, menus)
    fn initialize(&mut self) -> Result<(), Box<dyn Error>> {
        info!("Initializing FastTrackStudio2 extension...");

        // Initialize the in-process actions registry
        action_registry::init_registry();
        self.tokio_runtime.block_on(async {
            register_actions().await;

            // Register the DAW dispatcher for in-process REAPER API handling
            // and initialize the session subsystem with an in-process loopback
            register_daw_dispatcher().await;
        });

        // Register the menu hook
        menu::register_extension_menu()?;

        info!("FastTrackStudio2 extension initialized");
        Ok(())
    }
}

/// Register the DAW dispatcher for in-process REAPER API handling.
///
/// This creates dispatchers for all DAW services that use actual REAPER APIs,
/// and registers them with the Host. When guest cells make DAW service calls,
/// they're handled locally in-process.
///
/// Services registered:
/// - TransportService: Play/pause/stop, position, tempo, looping
/// - ProjectService: Project management
/// - MarkerService: Markers for song boundaries
/// - RegionService: Section regions
/// - TempoMapService: Tempo and time signature changes
///
/// Uses TaskSupport from reaper-high to dispatch REAPER API calls to the main thread.
async fn register_daw_dispatcher() {
    info!("Registering DAW dispatcher for in-process REAPER API handling...");

    // Set TaskSupport for daw-reaper to use
    // This allows all REAPER implementations to dispatch calls to the main thread
    daw::reaper::set_task_support(Global::task_support());

    // Initialize transport broadcaster for low-latency state streaming
    // The timer callback will call poll_and_broadcast() to push state updates
    daw::reaper::init_transport_broadcaster();
    info!("Transport broadcaster initialized for low-latency streaming");

    // Initialize FX event broadcaster for reactive FX chain observation
    // The timer callback will call poll_and_broadcast_fx() to push FX events
    daw::reaper::init_fx_broadcaster();
    info!("FX event broadcaster initialized for reactive observation");

    // Initialize track, item, routing, and tempo map broadcasters
    // The timer callback will call poll_and_broadcast_*() to push state updates
    daw::reaper::init_track_broadcaster();
    daw::reaper::init_item_broadcaster();
    daw::reaper::init_routing_broadcaster();
    daw::reaper::init_tempo_map_broadcaster();
    info!("Track, item, routing, and tempo map broadcasters initialized");

    // Create REAPER implementations (they use TaskSupport for main thread dispatch)
    let transport = daw::reaper::ReaperTransport::new();
    let project = daw::reaper::ReaperProject::new();
    let marker = daw::reaper::ReaperMarker::new();
    let region = daw::reaper::ReaperRegion::new();
    let tempo_map = daw::reaper::ReaperTempoMap::new();
    let audio_engine = daw::reaper::ReaperAudioEngine::new();
    let midi = daw::reaper::ReaperMidi::new();
    let midi_analysis = daw::reaper::ReaperMidiAnalysis::new();
    let fx = daw::reaper::ReaperFx::new();
    let track = daw::reaper::ReaperTrack::new();
    let routing = daw::reaper::ReaperRouting::new();
    let live_midi = daw::reaper::ReaperLiveMidi::new();
    let ext_state = daw::reaper::ReaperExtState::new();
    let item = daw::reaper::ReaperItem::new();
    let take = daw::reaper::ReaperTake::new();
    let health = daw::reaper::ReaperHealth::new();

    // Import service descriptor functions for method_id routing.
    // Each `*_service_descriptor()` is generated by the #[roam::service] macro
    // and re-exported via `daw::service::*`.
    use daw::service::{
        audio_engine_service_service_descriptor, ext_state_service_service_descriptor,
        fx_service_service_descriptor, health_service_service_descriptor,
        item_service_service_descriptor, live_midi_service_service_descriptor,
        marker_service_service_descriptor, midi_analysis_service_service_descriptor,
        midi_service_service_descriptor, project_service_service_descriptor,
        region_service_service_descriptor, routing_service_service_descriptor,
        take_service_service_descriptor, tempo_map_service_service_descriptor,
        track_service_service_descriptor, transport_service_service_descriptor,
    };

    // Create dispatchers and compose them via RoutedHandler (method_id routing)
    let daw_handler = RoutedHandler::new()
        .with(transport_service_service_descriptor(), TransportServiceDispatcher::new(transport))
        .with(project_service_service_descriptor(), ProjectServiceDispatcher::new(project))
        .with(marker_service_service_descriptor(), MarkerServiceDispatcher::new(marker))
        .with(region_service_service_descriptor(), RegionServiceDispatcher::new(region))
        .with(tempo_map_service_service_descriptor(), TempoMapServiceDispatcher::new(tempo_map))
        .with(audio_engine_service_service_descriptor(), AudioEngineServiceDispatcher::new(audio_engine))
        .with(midi_service_service_descriptor(), MidiServiceDispatcher::new(midi))
        .with(midi_analysis_service_service_descriptor(), MidiAnalysisServiceDispatcher::new(midi_analysis))
        .with(fx_service_service_descriptor(), FxServiceDispatcher::new(fx))
        .with(track_service_service_descriptor(), TrackServiceDispatcher::new(track))
        .with(routing_service_service_descriptor(), RoutingServiceDispatcher::new(routing))
        .with(live_midi_service_service_descriptor(), LiveMidiServiceDispatcher::new(live_midi))
        .with(ext_state_service_service_descriptor(), ExtStateServiceDispatcher::new(ext_state))
        .with(health_service_service_descriptor(), HealthServiceDispatcher::new(health))
        .with(item_service_service_descriptor(), ItemServiceDispatcher::new(item))
        .with(take_service_service_descriptor(), TakeServiceDispatcher::new(take));

    // Initialize the session subsystem with an in-process loopback to the DAW handler.
    // This sets up Daw::init() so the session crate can call daw-control methods locally,
    // and creates SetlistServiceImpl + SongServiceImpl.
    match session::init(daw_handler).await {
        Ok(()) => {
            info!("Session subsystem initialized with in-process DAW loopback");
        }
        Err(e) => {
            warn!("Failed to initialize session subsystem: {}", e);
        }
    }

    // Initialize the signal subsystem (rig/profile/preset management).
    // Must happen before the socket server starts so signal services are available.
    signal_bridge::init().await;

    // Start the Unix socket server with DAW + Session + Signal services.
    if let Some(session_mgr) = session::SessionManager::try_get() {
        let mut handler = session_mgr.create_handler();

        // Add signal services if the controller is ready
        if let Some(ctrl) = signal_bridge::controller() {
            handler = signal_bridge::add_signal_services(handler, ctrl);
            info!("Signal services added to Unix socket handler");
        }

        start_unix_socket_server(handler.clone());
        info!("Unix socket serves DAW + Session + Signal services");

        // Start the TCP server + mDNS advertisement for network sync.
        start_network_server(handler);
    } else {
        // Session init failed — can't serve without handler (it was moved)
        warn!("Unix socket not started (session init failed)");
    }

    // Wire signal appliers (ReaperPatchApplier + RigSceneManager) and set
    // the initial ActiveContext. This requires both signal_bridge::init() and
    // Daw::init() to have completed (the DAW loopback provides Project handles).
    if std::env::var("FTS_DAW_ROLE").as_deref() == Ok("signal") {
        signal_bridge::wire_appliers().await;
    }

    // Write FTS_DAW_ROLE env var to ExtState so fts-control can classify this instance.
    // Uses persist=false — the role only exists while REAPER is running.
    if let Ok(role) = std::env::var("FTS_DAW_ROLE") {
        let section = CString::new("FTS").expect("valid CString");
        let key = CString::new("role").expect("valid CString");
        let value = CString::new(role.clone()).expect("valid CString");
        let low = reaper_high::Reaper::get().medium_reaper().low();
        unsafe {
            low.SetExtState(section.as_ptr(), key.as_ptr(), value.as_ptr(), false);
        }
        info!("FTS_DAW_ROLE='{}' written to ExtState FTS/role", role);

        // Write FTS_RIG_TYPE env var to ExtState so fts-control can identify this
        // instance's rig type (guitar, vocals, drums, etc.).
        if let Ok(rig_type) = std::env::var("FTS_RIG_TYPE") {
            let rt_key = CString::new("rig_type").expect("valid CString");
            let rt_value = CString::new(rig_type.clone()).expect("valid CString");
            unsafe {
                low.SetExtState(section.as_ptr(), rt_key.as_ptr(), rt_value.as_ptr(), false);
            }
            info!("FTS_RIG_TYPE='{}' written to ExtState FTS/rig_type", rig_type);
        }

        // Set a distinct dock icon and color theme per role.
        dock_icon::set_dock_icon_for_role(&role);
        dock_icon::set_theme_for_role(&role);
    }

    // Write sync group to ExtState. Instances with the same sync_group auto-pair
    // when sync is enabled. Default: from FTS_SYNC_GROUP env, or "default".
    // Can be changed at runtime via ExtState (FTS_SYNC/group).
    {
        let low = reaper_high::Reaper::get().medium_reaper().low();
        let section = CString::new("FTS_SYNC").expect("valid CString");
        let group_key = CString::new("group").expect("valid CString");
        let group_value = std::env::var("FTS_SYNC_GROUP").unwrap_or_else(|_| "default".into());
        let group_cstr = CString::new(group_value.clone()).expect("valid CString");
        unsafe {
            low.SetExtState(section.as_ptr(), group_key.as_ptr(), group_cstr.as_ptr(), true);
        }
        info!("Sync group '{}' written to ExtState FTS_SYNC/group", group_value);
    }

    // Initialize the sync bridge (Ableton Link engine).
    // The timer callback will call sync_bridge::tick() to drive transport sync.
    sync_bridge::init();

    // Start discovering other FTS instances on the network via mDNS.
    // Auto-connect to peers in the same sync group.
    {
        let our_sync_group = std::env::var("FTS_SYNC_GROUP").unwrap_or_else(|_| "default".into());
        let our_pid = std::process::id().to_string();

        moire::task::spawn(async move {
            let mut rx = roam_discover::discover("fts-daw").await;
            while let Some(event) = rx.recv().await {
                match event {
                    roam_discover::PeerEvent::Found(peer) => {
                        let role = peer.get_meta("role").unwrap_or("?");
                        let group = peer.get_meta("sync_group").unwrap_or("?");
                        let peer_pid = peer.get_meta("pid").unwrap_or("");
                        let peer_setlist = peer.get_meta("setlist_id").unwrap_or("");

                        // Skip ourselves
                        if peer_pid == our_pid {
                            continue;
                        }

                        info!(
                            "Network peer found: {} (role={}, group={}, setlist={}, {}:{})",
                            peer.instance_name, role, group,
                            if peer_setlist.is_empty() { "none" } else { peer_setlist },
                            peer.host, peer.port
                        );

                        // Auto-connect if same sync group
                        if group == our_sync_group {
                            info!(
                                "Peer {} is in our sync group '{}' — connecting...",
                                peer.instance_name, group
                            );
                            if let Some(addr) = peer.addr() {
                                let instance_name = peer.instance_name.clone();
                                moire::task::spawn(async move {
                                    match tokio::net::TcpStream::connect(addr).await {
                                        Ok(stream) => {
                                            info!("Connected to peer {} at {}", instance_name, addr);
                                            let link = roam_stream::StreamLink::tcp(stream);
                                            match roam::initiator_conduit(roam::BareConduit::new(link))
                                                .max_concurrent_requests(64)
                                                .establish::<roam::DriverCaller>(())
                                                .await
                                            {
                                                Ok((caller, _session)) => {
                                                    info!("Roam session established with {}", instance_name);

                                                    // Create a Daw handle pointing at the remote peer
                                                    let remote_daw = daw::Daw::new(roam::ErasedCaller::new(caller));

                                                    // Start a sync engine for this peer
                                                    let sync_session = sync_proto::SyncSession {
                                                        session_id: format!("network-{}", instance_name),
                                                        peer_id: format!("local-{}", std::process::id()),
                                                        display_name: instance_name.clone(),
                                                    };
                                                    let engine = sync::Engine::new(
                                                        remote_daw,
                                                        sync_session,
                                                        sync_proto::SyncConfig::transport_only(),
                                                    );
                                                    match engine.start().await {
                                                        Ok(()) => {
                                                            info!("Sync engine started for peer {}", instance_name);
                                                            // Keep alive — engine runs via spawned subscription tasks
                                                            std::future::pending::<()>().await;
                                                        }
                                                        Err(e) => {
                                                            warn!("Sync engine start failed for {}: {e}", instance_name);
                                                        }
                                                    }
                                                }
                                                Err(e) => {
                                                    warn!("Roam handshake failed with {}: {:?}", instance_name, e);
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            warn!("TCP connect to {} ({}) failed: {}", instance_name, addr, e);
                                        }
                                    }
                                });
                            }
                        }
                    }
                    roam_discover::PeerEvent::Lost(name) => {
                        debug!("Network peer lost: {}", name);
                    }
                }
            }
        });
    }

    info!(
        "DAW dispatcher registered (Transport, Project, Marker, Region, TempoMap, Midi, MidiAnalysis, AudioEngine, Fx, Track, Routing, LiveMidi, ExtState, Session, Signal, Sync)"
    );
}

// ============================================================================
// Unix Socket Server
// ============================================================================

/// Directory for DAW sockets (discoverable by fts-control).
const SOCKET_DIR: &str = "/tmp";

/// Prefix for PID-based socket names. fts-control globs `fts-daw-*.sock` to
/// discover all running REAPER instances.
const SOCKET_PREFIX: &str = "fts-daw-";

/// Build the socket path for this REAPER instance.
///
/// Default: `/tmp/fts-daw-{pid}.sock` — each REAPER process gets a unique
/// socket so fts-control can discover and connect to all of them.
///
/// Override with `FTS_SOCKET` env var for testing or custom setups.
fn socket_path() -> PathBuf {
    std::env::var("FTS_SOCKET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            let pid = std::process::id();
            PathBuf::from(format!("{}/{}{}.sock", SOCKET_DIR, SOCKET_PREFIX, pid))
        })
}

/// Start the Unix socket server exposing DAW services to external clients.
///
/// Spawns a background tokio task that accepts connections on `/tmp/fts-daw-{pid}.sock`.
/// Each connection runs a roam session with the full DAW handler, allowing
/// fts-control desktop (or any roam client) to call all DAW + Session services.
fn start_unix_socket_server(handler: RoutedHandler) {
    let path = socket_path();

    // Remove stale socket from a previous run
    let _ = std::fs::remove_file(&path);

    let listener = match UnixListener::bind(&path) {
        Ok(l) => l,
        Err(e) => {
            warn!("Failed to bind Unix socket at {}: {}", path.display(), e);
            return;
        }
    };

    info!("Unix socket server listening on {}", path.display());

    let handler = std::sync::Arc::new(handler);

    moire::task::spawn(async move {
        loop {
            match listener.accept().await {
                Ok((stream, _addr)) => {
                    info!("fts-control client connected via Unix socket");
                    let handler = handler.clone();
                    moire::task::spawn(async move {
                        let link = roam_stream::StreamLink::unix(stream);
                        match roam::acceptor(roam::BareConduit::new(link))
                            .establish::<roam::DriverCaller>(handler.as_ref().clone())
                            .await
                        {
                            Ok((_caller, _session_handle)) => {
                                // Session message processing runs in background tasks
                                // spawned by establish(). We must keep `_caller` alive —
                                // dropping it closes the root connection in roam v7,
                                // which cancels any pending RPCs from the client.
                                debug!("Unix socket session established");
                                std::future::pending::<()>().await;
                            }
                            Err(e) => {
                                warn!("Unix socket handshake failed: {:?}", e);
                            }
                        }
                    });
                }
                Err(e) => {
                    warn!("Unix socket accept error: {}", e);
                }
            }
        }
    });
}

/// Start a TCP server for network-accessible roam RPC and advertise via mDNS.
///
/// This enables cross-machine DAW sync: other FTS instances on the local network
/// discover this one via mDNS (`_fts-daw._tcp.local.`) and connect over TCP.
fn start_network_server(handler: RoutedHandler) {
    use tokio::net::TcpListener;

    let handler = std::sync::Arc::new(handler);

    moire::task::spawn(async move {
        // Bind to any available port
        let listener = match TcpListener::bind("0.0.0.0:0").await {
            Ok(l) => l,
            Err(e) => {
                warn!("Failed to bind TCP server: {}", e);
                return;
            }
        };

        let port = match listener.local_addr() {
            Ok(addr) => addr.port(),
            Err(e) => {
                warn!("Failed to get TCP server address: {}", e);
                return;
            }
        };

        info!("TCP server listening on port {}", port);

        // Advertise via mDNS with role + sync group + setlist_id metadata
        let role = std::env::var("FTS_DAW_ROLE").unwrap_or_else(|_| "unknown".into());
        let sync_group = std::env::var("FTS_SYNC_GROUP").unwrap_or_else(|_| "default".into());
        let pid = std::process::id();

        // Read setlist_id from ExtState (persisted from last setlist generation)
        let setlist_id = {
            let low = reaper_high::Reaper::get().medium_reaper().low();
            let section = CString::new("FTS_SYNC").unwrap();
            let key = CString::new("setlist_id").unwrap();
            unsafe {
                let ptr = low.GetExtState(section.as_ptr(), key.as_ptr());
                if ptr.is_null() {
                    String::new()
                } else {
                    std::ffi::CStr::from_ptr(ptr).to_string_lossy().to_string()
                }
            }
        };

        let instance_name = format!("FTS-{}-{}", role.to_uppercase(), pid);
        let mut metadata = vec![
            ("role".into(), role),
            ("sync_group".into(), sync_group),
            ("pid".into(), pid.to_string()),
            ("version".into(), env!("CARGO_PKG_VERSION").into()),
        ];
        if !setlist_id.is_empty() {
            metadata.push(("setlist_id".into(), setlist_id));
        }

        let _mdns_guard = match roam_discover::advertise(roam_discover::ServiceInfo {
            service_type: "fts-daw",
            instance_name,
            port,
            metadata,
        }) {
            Ok(guard) => {
                info!("mDNS: advertised as _fts-daw._tcp on port {}", port);
                guard
            }
            Err(e) => {
                warn!("mDNS advertisement failed: {}", e);
                // Continue without mDNS — TCP server still works for direct connections
                return;
            }
        };

        // Accept TCP connections (same pattern as Unix socket)
        loop {
            match listener.accept().await {
                Ok((stream, addr)) => {
                    info!("Network peer connected from {}", addr);
                    let handler = handler.clone();
                    moire::task::spawn(async move {
                        let link = roam_stream::StreamLink::tcp(stream);
                        match roam::acceptor(roam::BareConduit::new(link))
                            .establish::<roam::DriverCaller>(handler.as_ref().clone())
                            .await
                        {
                            Ok((_caller, _session_handle)) => {
                                debug!("TCP session established with {}", addr);
                                std::future::pending::<()>().await;
                            }
                            Err(e) => {
                                warn!("TCP handshake failed with {}: {:?}", addr, e);
                            }
                        }
                    });
                }
                Err(e) => {
                    warn!("TCP accept error: {}", e);
                }
            }
        }
    });
}

async fn register_actions() {
    local_actions::register_toggle_states();

    if let Err(error) =
        action_registry::register_actions(vec![action_registry::ActionRegistrationSource::Local(
            local_actions::builtin_local_actions(),
        )])
        .await
    {
        warn!(%error, "Failed to register actions");
        return;
    }

    // Add quick input validation buttons to the default FTS floating toolbar.
    let add_toggle = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_TOGGLE_INPUT_RUNTIME", "Toggle Input")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_toggle {
        warn!(%error, "Failed to queue Toggle Input toolbar button");
    }

    let add_log = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_LOG_INPUT_RUNTIME_STATE", "Log Input")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_log {
        warn!(%error, "Failed to queue Log Input toolbar button");
    }

    let add_intercept = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_TOGGLE_INPUT_INTERCEPT", "Intercept")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_intercept {
        warn!(%error, "Failed to queue Intercept toolbar button");
    }

    let add_menu = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_INPUT_MENU", "Input Menu")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32))
            .double_wide(),
        "__input_runtime__",
    );
    if let Err(error) = add_menu {
        warn!(%error, "Failed to queue Input Menu toolbar button");
    }

    let add_reset_mouse = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new(
            "FTS_INPUT_RESET_MOUSE_MODIFIERS",
            "Reset Mouse Modifiers",
        )
        .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_reset_mouse {
        warn!(%error, "Failed to queue Reset Mouse Modifiers toolbar button");
    }
}

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// Timer callback for periodic updates (runs on main thread)
extern "C" fn timer_callback() {
    if let Some(app_fragile) = get_app() {
        let app = app_fragile.get();

        // Periodic profiling — accumulate per-section times, log every ~5s
        use std::sync::atomic::{AtomicU64, Ordering};
        static TICK_COUNT: AtomicU64 = AtomicU64::new(0);
        static TOTAL_US: AtomicU64 = AtomicU64::new(0);
        static TASKS_US: AtomicU64 = AtomicU64::new(0);
        static ALLOC_US: AtomicU64 = AtomicU64::new(0);
        static INPUT_US: AtomicU64 = AtomicU64::new(0);
        static TRANSPORT_US: AtomicU64 = AtomicU64::new(0);
        static FX_US: AtomicU64 = AtomicU64::new(0);
        static MAX_TICK_US: AtomicU64 = AtomicU64::new(0);

        let timer_start = std::time::Instant::now();

        macro_rules! timed {
            ($accum:expr, $body:expr) => {{
                let s = std::time::Instant::now();
                $body;
                let us = s.elapsed().as_micros() as u64;
                $accum.fetch_add(us, Ordering::Relaxed);
            }};
        }

        timed!(TASKS_US, app.process_tasks());
        timed!(ALLOC_US, {
            if let Some(runtime) = daw_allocator::FtsRuntime::try_get() {
                runtime.process_main_thread_tasks();
            }
        });
        timed!(INPUT_US, {
            #[cfg(feature = "input-hook")]
            {
                input_reaper::check_and_hook_windows();
                input_reaper::check_which_key_timeout();
                input_reaper::refresh_which_key_overlay();
            }
        });
        timed!(TRANSPORT_US, daw::reaper::poll_and_broadcast());
        timed!(FX_US, daw::reaper::poll_and_broadcast_fx());

        toolbar_manager::process_deferred_ops();

        let total_us = timer_start.elapsed().as_micros() as u64;
        TOTAL_US.fetch_add(total_us, Ordering::Relaxed);
        MAX_TICK_US.fetch_max(total_us, Ordering::Relaxed);

        let n = TICK_COUNT.fetch_add(1, Ordering::Relaxed) + 1;
        if n % 150 == 0 {
            // Log every ~5 seconds (at 30Hz)
            let avg = |acc: &AtomicU64| acc.swap(0, Ordering::Relaxed) / 150;
            let max = MAX_TICK_US.swap(0, Ordering::Relaxed);
            info!(
                "Timer profile (150 ticks): avg={:.1}ms max={:.1}ms | tasks={:.1}ms alloc={:.1}ms input={:.1}ms transport={:.1}ms fx={:.1}ms",
                avg(&TOTAL_US) as f64 / 1000.0,
                max as f64 / 1000.0,
                avg(&TASKS_US) as f64 / 1000.0,
                avg(&ALLOC_US) as f64 / 1000.0,
                avg(&INPUT_US) as f64 / 1000.0,
                avg(&TRANSPORT_US) as f64 / 1000.0,
                avg(&FX_US) as f64 / 1000.0,
            );
        }
    }
}

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing — write to /tmp/fts-reaper.log
    let log_file =
        std::fs::File::create("/tmp/fts-reaper.log").expect("Failed to create /tmp/fts-reaper.log");
    tracing_subscriber::fmt()
        .with_writer(std::sync::Mutex::new(log_file))
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::DEBUG.into()),
        )
        .init();

    info!("FastTrackStudio REAPER Extension starting...");

    // Make Swell available globally (required for swell-ui menu operations)
    let _ = Swell::make_available_globally(Swell::load(context));

    // Initialize REAPER high-level API
    match HighReaper::load(context).setup() {
        Ok(_) => {
            info!("REAPER high-level API initialized");
        }
        Err(_) => {
            debug!("REAPER high-level API already initialized");
        }
    }

    // Initialize the RT-aware allocator runtime.
    // Uses REAPER's IsInRealTimeAudio() to detect audio threads — deallocations
    // on those threads are offloaded to the "daw-deallocator" background thread.
    #[cfg(feature = "rt-allocator")]
    {
        let low = HighReaper::get().medium_reaper().low();
        if let Some(is_in_rt_audio) = low.pointers().IsInRealTimeAudio {
            struct ReaperRtDetector(unsafe extern "C" fn() -> i32);
            impl daw_allocator::RtDetector for ReaperRtDetector {
                fn is_rt_thread(&self) -> bool {
                    unsafe { (self.0)() != 0 }
                }
            }
            // Safety: function pointer is valid for the lifetime of the REAPER process.
            unsafe impl Send for ReaperRtDetector {}
            unsafe impl Sync for ReaperRtDetector {}

            daw_allocator::FtsRuntime::init(
                &ALLOCATOR,
                daw_allocator::FtsRuntimeConfig {
                    dealloc_channel_capacity: 10_000,
                    rt_detector: Box::new(ReaperRtDetector(is_in_rt_audio)),
                },
            );
            info!("DAW allocator initialized (RT-safe deallocation active)");
        } else {
            warn!("IsInRealTimeAudio not available — RT deallocation offloading disabled");
        }
    }
    #[cfg(not(feature = "rt-allocator"))]
    info!("RT allocator DISABLED (diagnostic mode)");

    // Create a medium-level API session
    let session = ReaperSession::load(context);

    // Create the App (initializes Global/TaskSupport)
    let mut app = App::new(session)?;

    // Initialize the extension (mut needed to store SHM temp dir)
    app.initialize()?;

    // Register REAPER input bridge on top of the shared input core.
    #[cfg(feature = "input-hook")]
    if let Err(error) =
        input_reaper::register_with_default_keymap(input_reaper::InputRuntimeConfig::default())
    {
        warn!(%error, "Failed to register input-reaper runtime");
    }
    #[cfg(not(feature = "input-hook"))]
    info!("Input hooks DISABLED (diagnostic mode)");

    // Eagerly load FTS CLAP plugins (Helgobox pattern)
    // Must happen after initialize() so TaskSupport is ready
    #[cfg(feature = "eager-plugins")]
    eagerly_load_fts_plugins(context);
    #[cfg(not(feature = "eager-plugins"))]
    info!("Eager plugin loading DISABLED (diagnostic mode)");

    // Store app globally
    APP_INSTANCE
        .set(Fragile::new(app))
        .expect("App already initialized");

    // Register timer callback for periodic updates
    let app = APP_INSTANCE.get().expect("App should be initialized").get();
    let mut session = app.session.borrow_mut();
    session.plugin_register_add_timer(timer_callback)?;

    // Register hidden control surface for event-driven auto-color.
    // REAPER calls our CSurf callbacks when tracks change (name, FX, routing, etc.)
    // instead of us polling on a timer.
    #[cfg(feature = "auto-color")]
    session.plugin_register_add_csurf_inst(Box::new(auto_color::AutoColorSurface))?;
    #[cfg(not(feature = "auto-color"))]
    info!("Auto-color surface DISABLED (diagnostic mode)");
    drop(session);

    info!("FastTrackStudio REAPER Extension initialized successfully");
    Ok(())
}
