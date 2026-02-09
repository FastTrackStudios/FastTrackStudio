//! DAW Bridge — capture and apply snapshots via real DAW FX chains.
//!
//! This module bridges signal-proto snapshot types with daw-control FX APIs,
//! providing Snapshooter-style parameter capture/recall and Track Snapshot-style
//! full state chunk capture/restore.
//!
//! # Two Snapshot Strategies
//!
//! 1. **Parameter snapshots** (`DawParameterSnapshot`): Captures normalized parameter
//!    values (0.0-1.0) for every FX parameter. Lightweight, supports diff-based
//!    recall (only apply changed values) and morphing (linear interpolation).
//!    Inspired by tilr's Snapshooter.
//!
//! 2. **State chunk snapshots** (`DawStateChunkSnapshot`): Captures the full binary
//!    plugin state (base64-encoded). Heavier but captures internal state not
//!    exposed as parameters (e.g., wavetable positions, sample mappings).
//!    Inspired by Daniel Lumertz's Track Snapshot.
//!
//! 3. **Scene snapshots** (`DawSceneSnapshot`): Combines both for complete scene
//!    switching — parameter values for fast recall + state chunks for full fidelity.

use daw_control::FxChain;
use daw_proto::{FxParameter, FxStateChunk};
use eyre::Result;
use std::collections::HashMap;

// ─────────────────────────────────────────────────────────────────────────────
// DawParameterSnapshot — Snapshooter-style parameter capture
// ─────────────────────────────────────────────────────────────────────────────

/// A captured FX parameter value (index + normalized value).
#[derive(Debug, Clone)]
pub struct DawParamValue {
    /// Parameter index within the FX
    pub param_index: u32,
    /// Parameter name (for display/debugging)
    pub param_name: String,
    /// Normalized value [0.0, 1.0]
    pub value: f64,
}

/// Captured state of a single FX plugin's parameters.
#[derive(Debug, Clone)]
pub struct DawFxParameterState {
    /// FX GUID (stable identifier across sessions)
    pub fx_guid: String,
    /// FX chain index (may change if chain is reordered)
    pub fx_index: u32,
    /// Plugin name (for display/debugging)
    pub plugin_name: String,
    /// Whether the FX is enabled (not bypassed)
    pub enabled: bool,
    /// All captured parameter values
    pub parameters: Vec<DawParamValue>,
}

/// Full parameter snapshot of an FX chain.
///
/// Captures all FX and their parameter values at a point in time.
/// Suitable for diff-based recall — only changed parameters are written.
#[derive(Debug, Clone)]
pub struct DawParameterSnapshot {
    /// Human-readable label
    pub name: String,
    /// Per-FX parameter states, in chain order
    pub fx_states: Vec<DawFxParameterState>,
}

/// A parameter that changed between two snapshots.
#[derive(Debug, Clone)]
pub struct DawParamChange {
    /// FX GUID
    pub fx_guid: String,
    /// Parameter index
    pub param_index: u32,
    /// Parameter name
    pub param_name: String,
    /// Previous value
    pub from_value: f64,
    /// New value
    pub to_value: f64,
}

impl DawParamChange {
    /// Absolute magnitude of the change.
    pub fn magnitude(&self) -> f64 {
        (self.to_value - self.from_value).abs()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// DawStateChunkSnapshot — Track Snapshot-style full state capture
// ─────────────────────────────────────────────────────────────────────────────

/// Full state chunk snapshot of an FX chain.
///
/// Captures the complete binary state of every plugin in a chain.
/// Heavier than parameter snapshots but captures ALL internal state.
#[derive(Debug, Clone)]
pub struct DawStateChunkSnapshot {
    /// Human-readable label
    pub name: String,
    /// Per-FX state chunks, in chain order
    pub chunks: Vec<FxStateChunk>,
}

// ─────────────────────────────────────────────────────────────────────────────
// DawSceneSnapshot — Combined snapshot for complete scene switching
// ─────────────────────────────────────────────────────────────────────────────

/// Combined parameter + state chunk snapshot for a complete scene.
///
/// Contains both parameter values (for fast diff-based recall and morphing)
/// and state chunks (for full fidelity restore when parameters alone aren't
/// enough).
#[derive(Debug, Clone)]
pub struct DawSceneSnapshot {
    /// Human-readable label
    pub name: String,
    /// Parameter values for all FX (for morphing/diff)
    pub parameters: DawParameterSnapshot,
    /// Full binary state for all FX (for complete restore)
    pub state_chunks: DawStateChunkSnapshot,
}

// ─────────────────────────────────────────────────────────────────────────────
// Capture Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Capture a parameter snapshot of all FX in a chain.
///
/// Reads every parameter's normalized value from the DAW. This is the
/// Snapshooter approach — lightweight, supports diff and morphing.
pub async fn capture_parameter_snapshot(
    chain: &FxChain,
    name: impl Into<String>,
) -> Result<DawParameterSnapshot> {
    let all_fx = chain.all().await?;
    let mut fx_states = Vec::with_capacity(all_fx.len());

    for fx_info in &all_fx {
        let handle = chain.by_guid(&fx_info.guid).await?;
        let Some(handle) = handle else { continue };

        let params: Vec<FxParameter> = handle.parameters().await?;
        let param_values: Vec<DawParamValue> = params
            .iter()
            .map(|p| DawParamValue {
                param_index: p.index,
                param_name: p.name.clone(),
                value: p.value,
            })
            .collect();

        fx_states.push(DawFxParameterState {
            fx_guid: fx_info.guid.clone(),
            fx_index: fx_info.index,
            plugin_name: fx_info.plugin_name.clone(),
            enabled: fx_info.enabled,
            parameters: param_values,
        });
    }

    Ok(DawParameterSnapshot {
        name: name.into(),
        fx_states,
    })
}

/// Capture state chunks for all FX in a chain.
///
/// This is the Track Snapshot approach — captures full binary plugin state.
pub async fn capture_state_chunks(
    chain: &FxChain,
    name: impl Into<String>,
) -> Result<DawStateChunkSnapshot> {
    let chunks = chain.state().await?;
    Ok(DawStateChunkSnapshot {
        name: name.into(),
        chunks,
    })
}

/// Capture a combined scene snapshot (parameters + state chunks).
pub async fn capture_scene_snapshot(
    chain: &FxChain,
    name: impl Into<String>,
) -> Result<DawSceneSnapshot> {
    let name = name.into();
    let parameters = capture_parameter_snapshot(chain, &name).await?;
    let state_chunks = capture_state_chunks(chain, &name).await?;
    Ok(DawSceneSnapshot {
        name,
        parameters,
        state_chunks,
    })
}

// ─────────────────────────────────────────────────────────────────────────────
// Apply Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Apply a parameter snapshot, writing only changed values.
///
/// Reads current parameter state, diffs against the snapshot, and only
/// writes parameters that actually changed. This minimizes DAW API calls
/// and avoids unnecessary plugin processing.
pub async fn apply_parameter_snapshot(
    chain: &FxChain,
    snapshot: &DawParameterSnapshot,
) -> Result<Vec<DawParamChange>> {
    let mut changes = Vec::new();

    for fx_state in &snapshot.fx_states {
        let Some(handle) = chain.by_guid(&fx_state.fx_guid).await? else {
            tracing::debug!(
                "FX {} ({}) not found in chain, skipping",
                fx_state.fx_guid,
                fx_state.plugin_name
            );
            continue;
        };

        // Read current parameters for diff
        let current_params = handle.parameters().await?;
        let current_map: HashMap<u32, f64> =
            current_params.iter().map(|p| (p.index, p.value)).collect();

        // Apply only changed values
        for param in &fx_state.parameters {
            let current_value = current_map.get(&param.param_index).copied().unwrap_or(0.0);
            let delta = (current_value - param.value).abs();

            if delta > 0.0001 {
                handle.param(param.param_index).set(param.value).await?;
                changes.push(DawParamChange {
                    fx_guid: fx_state.fx_guid.clone(),
                    param_index: param.param_index,
                    param_name: param.param_name.clone(),
                    from_value: current_value,
                    to_value: param.value,
                });
            }
        }

        // Apply enable/bypass state
        let current_info = handle.info().await?;
        if current_info.enabled != fx_state.enabled {
            if fx_state.enabled {
                handle.enable().await?;
            } else {
                handle.disable().await?;
            }
        }
    }

    Ok(changes)
}

/// Apply state chunks to restore full plugin state.
///
/// Matches FX by GUID and restores their binary state. FX not found
/// in the current chain are skipped gracefully.
pub async fn apply_state_chunks(chain: &FxChain, snapshot: &DawStateChunkSnapshot) -> Result<()> {
    chain.restore_state(snapshot.chunks.clone()).await
}

/// Apply a combined scene snapshot.
///
/// Applies state chunks first (full plugin state), then parameter values
/// on top (in case the state chunk didn't capture everything or we want
/// to ensure specific parameter positions).
pub async fn apply_scene_snapshot(chain: &FxChain, snapshot: &DawSceneSnapshot) -> Result<()> {
    // Apply state chunks first for full fidelity
    apply_state_chunks(chain, &snapshot.state_chunks).await?;
    // Then apply parameter values on top for precision
    apply_parameter_snapshot(chain, &snapshot.parameters).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Diff Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Compute the diff between two parameter snapshots.
///
/// Returns a list of parameter changes, suitable for selective recall
/// or morph interpolation.
pub fn diff_parameter_snapshots(
    from: &DawParameterSnapshot,
    to: &DawParameterSnapshot,
) -> Vec<DawParamChange> {
    let mut changes = Vec::new();

    // Build lookup: fx_guid -> { param_index -> value }
    let from_map: HashMap<&str, HashMap<u32, &DawParamValue>> = from
        .fx_states
        .iter()
        .map(|s| {
            let params: HashMap<u32, &DawParamValue> =
                s.parameters.iter().map(|p| (p.param_index, p)).collect();
            (s.fx_guid.as_str(), params)
        })
        .collect();

    for to_fx in &to.fx_states {
        let Some(from_params) = from_map.get(to_fx.fx_guid.as_str()) else {
            // FX exists in target but not in source — all params are "new"
            for p in &to_fx.parameters {
                changes.push(DawParamChange {
                    fx_guid: to_fx.fx_guid.clone(),
                    param_index: p.param_index,
                    param_name: p.param_name.clone(),
                    from_value: 0.0,
                    to_value: p.value,
                });
            }
            continue;
        };

        for to_param in &to_fx.parameters {
            let from_value = from_params
                .get(&to_param.param_index)
                .map(|p| p.value)
                .unwrap_or(0.0);
            let delta = (from_value - to_param.value).abs();

            if delta > 0.0001 {
                changes.push(DawParamChange {
                    fx_guid: to_fx.fx_guid.clone(),
                    param_index: to_param.param_index,
                    param_name: to_param.param_name.clone(),
                    from_value,
                    to_value: to_param.value,
                });
            }
        }
    }

    changes
}
