//! Sync domain SHM guest process.
//!
//! Connects to REAPER via daw-bridge SHM and manages sync state:
//! Ableton Link, tempo mapping, time signature changes, ruler lanes,
//! sync markers, and **multi-instance transport sync via mDNS + TCP**.
//!
//! Registers sync-domain actions with REAPER's action system and runs
//! the Link engine tick loop using daw RPC services.
//!
//! Placed in `UserPlugins/fts-extensions/` and hot-reloaded by daw-bridge.

mod link_bridge;

use daw::Daw;
use daw_extension_runtime::{ActionDef, GuestOptions};
use eyre::Result;
use sync::actions::sync_actions;
use sync::network::{MeshConfig, PeerMesh};
use sync::{Engine, SyncConfig, SyncSession};
use tracing::{debug, info};

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    rt.block_on(run())
}

async fn run() -> Result<()> {
    let pid = std::process::id();
    info!("[sync:{pid}] Sync extension starting");

    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "sync",
        ..Default::default()
    })
    .await?;

    info!("[sync:{pid}] Connected to REAPER via SHM");

    // Signal that we're alive — tests read this to verify the extension connected
    daw.ext_state()
        .set("FTS_SYNC_EXT", "status", "ready", false)
        .await?;
    daw.ext_state()
        .set("FTS_SYNC_EXT", "pid", &pid.to_string(), false)
        .await?;
    info!("[sync:{pid}] Health beacon written");

    // Register sync-domain actions with REAPER.
    // Action definitions live in sync-proto — single source of truth.
    // Action registration is best-effort — the extension can still sync transport
    // even if the bridge doesn't support action_registry (e.g. version mismatch).
    let action_defs: Vec<ActionDef> = sync_actions::definitions()
        .into_iter()
        .map(|def| ActionDef {
            command_name: Box::leak(def.id.to_command_id().into_boxed_str()),
            description: Box::leak(def.description.clone().into_boxed_str()),
        })
        .collect();

    let mut action_rx = None;
    match daw_extension_runtime::register_actions(&daw, &action_defs).await {
        Ok(reg) => {
            action_rx = Some(reg.rx);
            info!("[sync:{pid}] Action registration complete ({} registered, {} failed)",
                reg.registered, reg.failed);
        }
        Err(e) => {
            tracing::warn!("[sync:{pid}] Action registration failed (non-fatal): {e}");
        }
    }

    // ── Sync Engine ─────────────────────────────────────────────────────
    let session = SyncSession {
        peer_id: format!("reaper-{pid}"),
        session_id: "default".to_string(),
        display_name: format!("REAPER ({pid})"),
    };
    let config = SyncConfig::transport_only();
    let engine = Engine::new(daw.clone(), session.clone(), config);
    engine.start().await.map_err(|e| eyre::eyre!("{e}"))?;
    info!("[sync:{pid}] Sync engine started");

    // ── TCP Peer Mesh ───────────────────────────────────────────────────
    let mesh_config = MeshConfig {
        peer_id: session.peer_id.clone(),
        session_id: session.session_id.clone(),
    };
    let (mesh, mut incoming_rx) = PeerMesh::bind(mesh_config, engine.subscribe()).await?;
    let mesh_port = mesh.local_port();
    info!("[sync:{pid}] PeerMesh listening on port {mesh_port}");

    // Write mesh port + peer_id to ExtState for test orchestration
    daw.ext_state()
        .set("FTS_SYNC_EXT", "mesh_port", &mesh_port.to_string(), false)
        .await?;
    daw.ext_state()
        .set("FTS_SYNC_EXT", "peer_id", &session.peer_id, false)
        .await?;

    // ── mDNS Advertisement ──────────────────────────────────────────────
    let _advert_guard = roam_discover::advertise(roam_discover::ServiceInfo {
        service_type: "fts-sync",
        instance_name: format!("fts-sync-{pid}"),
        port: mesh_port,
        metadata: vec![
            ("peer_id".into(), session.peer_id.clone()),
            ("session_id".into(), session.session_id.clone()),
            ("pid".into(), pid.to_string()),
        ],
    })?;
    info!("[sync:{pid}] mDNS service advertised as fts-sync");

    // ── mDNS Discovery ──────────────────────────────────────────────────
    let mut discover_rx = roam_discover::discover("fts-sync").await;
    let local_peer_id = session.peer_id.clone();

    // ── Run all loops concurrently ──────────────────────────────────────
    tokio::select! {
        result = link_bridge::run_link_engine(&daw) => result,
        _ = handle_actions(&daw, action_rx) => Ok(()),
        _ = handle_incoming_events(&engine, &mut incoming_rx) => Ok(()),
        _ = handle_discovery(&mesh, &mut discover_rx, &local_peer_id) => Ok(()),
        _ = poll_connect_peers(&daw, &mesh, &session.peer_id) => Ok(()),
        _ = report_peer_count(&daw, &mesh) => Ok(()),
    }
}

/// Listen for action triggers from REAPER and dispatch them.
async fn handle_actions(
    daw: &Daw,
    mut rx: Option<roam::Rx<daw::service::ActionEvent>>,
) {
    let pid = std::process::id();
    let Some(ref mut rx) = rx else {
        // No action subscription — just park forever
        std::future::pending::<()>().await;
        return;
    };
    while let Ok(Some(event)) = rx.recv().await {
        match &*event {
            daw::service::ActionEvent::Triggered { command_name } => {
                info!("[sync:{pid}] Action triggered: {command_name}");

                // Write the last triggered action to ExtState so tests can verify.
                if let Err(e) = daw
                    .ext_state()
                    .set("FTS_SYNC_EXT", "last_action", command_name, false)
                    .await
                {
                    debug!("[sync:{pid}] Failed to write last_action: {e}");
                }

                // TODO: Dispatch to actual sync handlers (toggle_link, etc.)
            }
        }
    }
    info!("[sync:{pid}] Action event stream ended");
}

/// Apply incoming remote SyncEvents to the local DAW.
async fn handle_incoming_events(
    engine: &Engine,
    rx: &mut tokio::sync::mpsc::Receiver<sync::SyncEvent>,
) {
    let pid = std::process::id();
    while let Some(event) = rx.recv().await {
        debug!(
            "[sync:{pid}] Received remote event from {} (seq={})",
            event.origin_peer, event.sequence
        );
        engine.apply_remote(&event).await;
    }
    info!("[sync:{pid}] Incoming event stream ended");
}

/// Handle mDNS discovery events — connect to newly found peers, remove lost ones.
async fn handle_discovery(
    mesh: &PeerMesh,
    rx: &mut tokio::sync::mpsc::Receiver<roam_discover::PeerEvent>,
    local_peer_id: &str,
) {
    let pid = std::process::id();
    while let Some(event) = rx.recv().await {
        match event {
            roam_discover::PeerEvent::Found(peer) => {
                // Skip self-discovery
                if let Some(remote_peer_id) = peer.get_meta("peer_id") {
                    if remote_peer_id == local_peer_id {
                        debug!("[sync:{pid}] Ignoring self-discovery");
                        continue;
                    }
                    if let Some(addr) = peer.addr() {
                        info!(
                            "[sync:{pid}] Discovered peer {remote_peer_id} at {addr}"
                        );
                        mesh.connect_peer(addr, remote_peer_id.to_string()).await;
                    }
                }
            }
            roam_discover::PeerEvent::Lost(name) => {
                info!("[sync:{pid}] Peer lost: {name}");
                // Extract peer_id from the lost name if possible.
                // The name is the mDNS fullname — we stored peer_id in metadata,
                // but Lost only gives us the fullname. Best-effort removal.
                mesh.remove_peer(&name).await;
            }
        }
    }
    info!("[sync:{pid}] Discovery stream ended");
}

/// Poll ExtState for `connect_peers` key and connect directly.
///
/// Tests (or other tools) can write `connect_peers = "peer_id@host:port,..."` to ExtState
/// to trigger direct TCP connections, bypassing mDNS. This is essential for same-machine
/// testing where multicast doesn't work on loopback.
async fn poll_connect_peers(daw: &Daw, mesh: &PeerMesh, local_peer_id: &str) {
    let pid = std::process::id();
    let mut interval = tokio::time::interval(std::time::Duration::from_secs(1));
    let mut last_value = String::new();
    loop {
        interval.tick().await;
        let val = match daw.ext_state().get("FTS_SYNC_EXT", "connect_peers").await {
            Ok(Some(v)) if !v.is_empty() => v,
            _ => continue,
        };
        if val == last_value {
            continue;
        }
        last_value = val.clone();

        // Format: "peer_id@host:port,peer_id@host:port,..."
        for entry in val.split(',') {
            let entry = entry.trim();
            if entry.is_empty() {
                continue;
            }
            if let Some((peer_id, addr_str)) = entry.split_once('@') {
                if peer_id == local_peer_id {
                    continue; // skip self
                }
                match addr_str.parse() {
                    Ok(addr) => {
                        info!("[sync:{pid}] Direct-connecting to peer {peer_id} at {addr}");
                        mesh.connect_peer(addr, peer_id.to_string()).await;
                    }
                    Err(e) => {
                        debug!("[sync:{pid}] Bad peer address '{addr_str}': {e}");
                    }
                }
            }
        }
    }
}

/// Periodically write the current peer count to ExtState for test observability.
async fn report_peer_count(daw: &Daw, mesh: &PeerMesh) {
    let pid = std::process::id();
    let mut interval = tokio::time::interval(std::time::Duration::from_millis(500));
    loop {
        interval.tick().await;
        let count = mesh.peer_count().await;
        if let Err(e) = daw
            .ext_state()
            .set("FTS_SYNC_EXT", "peer_count", &count.to_string(), false)
            .await
        {
            debug!("[sync:{pid}] Failed to write peer_count: {e}");
        }
    }
}
