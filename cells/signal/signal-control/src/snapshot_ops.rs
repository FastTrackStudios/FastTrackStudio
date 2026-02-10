//! Snapshot Operations — capture/recall with SQLite persistence.
//!
//! Bridges `daw_bridge` (capture/apply from DAW FX chain) with
//! `signal_storage::snapshot_service` (persist to SQLite).

use daw_control::FxChain;
use eyre::Result;
use signal_storage::DatabaseConnection;
use uuid::Uuid;

use crate::daw_bridge::{
    apply_parameter_snapshot, apply_state_chunks, capture_parameter_snapshot, capture_state_chunks,
    DawParameterSnapshot, DawStateChunkSnapshot,
};
use signal_storage::snapshot_service;

// ─────────────────────────────────────────────────────────────────────────────
// Parameter snapshots (Snapshooter-style)
// ─────────────────────────────────────────────────────────────────────────────

/// Capture a parameter snapshot from the live DAW chain and save to SQLite.
///
/// Returns the snapshot's UUID for later recall.
pub async fn capture_and_save_snapshot(
    chain: &FxChain,
    db: &DatabaseConnection,
    track_guid: &str,
    name: &str,
) -> Result<Uuid> {
    let snapshot = capture_parameter_snapshot(chain, name).await?;
    let json_str = facet_json::to_string(&snapshot).map_err(|e| eyre::eyre!("{e}"))?;
    let json: serde_json::Value = serde_json::from_str(&json_str)?;
    let id = snapshot_service::save_parameter_snapshot(db, track_guid, name, json).await?;
    Ok(id)
}

/// Load a parameter snapshot from SQLite and apply it to the DAW chain.
///
/// Uses diff-based application — only changed parameters are written.
pub async fn recall_snapshot(chain: &FxChain, db: &DatabaseConnection, id: Uuid) -> Result<()> {
    let model = snapshot_service::load_parameter_snapshot(db, id)
        .await?
        .ok_or_else(|| eyre::eyre!("parameter snapshot {id} not found"))?;

    let json_str = serde_json::to_string(&model.data)?;
    let snapshot: DawParameterSnapshot =
        facet_json::from_str(&json_str).map_err(|e| eyre::eyre!("{e}"))?;
    apply_parameter_snapshot(chain, &snapshot).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// State chunk snapshots (Track Snapshot-style presets)
// ─────────────────────────────────────────────────────────────────────────────

/// Capture a state chunk snapshot from the live DAW chain and save to SQLite.
///
/// Returns the preset's UUID for later recall.
pub async fn capture_and_save_preset(
    chain: &FxChain,
    db: &DatabaseConnection,
    track_guid: &str,
    name: &str,
) -> Result<Uuid> {
    let snapshot = capture_state_chunks(chain, name).await?;
    let json_str = facet_json::to_string(&snapshot).map_err(|e| eyre::eyre!("{e}"))?;
    let json: serde_json::Value = serde_json::from_str(&json_str)?;
    let id = snapshot_service::save_state_chunk_snapshot(db, track_guid, name, json).await?;
    Ok(id)
}

/// Load a state chunk snapshot from SQLite and apply it to the DAW chain.
///
/// Restores full binary plugin state for each FX matched by GUID.
pub async fn recall_preset(chain: &FxChain, db: &DatabaseConnection, id: Uuid) -> Result<()> {
    let model = snapshot_service::load_state_chunk_snapshot(db, id)
        .await?
        .ok_or_else(|| eyre::eyre!("state chunk snapshot {id} not found"))?;

    let json_str = serde_json::to_string(&model.data)?;
    let snapshot: DawStateChunkSnapshot =
        facet_json::from_str(&json_str).map_err(|e| eyre::eyre!("{e}"))?;
    apply_state_chunks(chain, &snapshot).await?;
    Ok(())
}
