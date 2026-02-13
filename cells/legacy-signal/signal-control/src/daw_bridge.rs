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
use facet::Facet;

use std::collections::HashMap;

// ─────────────────────────────────────────────────────────────────────────────
// DawParameterSnapshot — Snapshooter-style parameter capture
// ─────────────────────────────────────────────────────────────────────────────

/// A captured FX parameter value (index + normalized value).
#[derive(Debug, Clone, Facet)]
pub struct DawParamValue {
    /// Parameter index within the FX
    pub param_index: u32,
    /// Parameter name (for display/debugging)
    pub param_name: String,
    /// Normalized value [0.0, 1.0]
    pub value: f64,
    /// Formatted display value from the plugin (e.g., "-12.5 dB", "250 Hz")
    pub formatted: String,
    /// Whether this is a toggle/boolean parameter
    pub is_toggle: bool,
    /// Number of discrete steps (None = continuous slider, Some(n) = dropdown with n choices)
    pub step_count: Option<u32>,
    /// Labels for each discrete step: (normalized_value, display_label)
    pub step_labels: Vec<(f64, String)>,
}

/// Captured state of a single FX plugin's parameters.
#[derive(Debug, Clone, Facet)]
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
#[derive(Debug, Clone, Facet)]
pub struct DawParameterSnapshot {
    /// Human-readable label
    pub name: String,
    /// Per-FX parameter states, in chain order
    pub fx_states: Vec<DawFxParameterState>,
}

/// A parameter that changed between two snapshots.
#[derive(Debug, Clone, Facet)]
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
#[derive(Debug, Clone, Facet)]
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
#[derive(Debug, Clone, Facet)]
pub struct DawSceneSnapshot {
    /// Human-readable label
    pub name: String,
    /// Parameter values for all FX (for morphing/diff)
    pub parameters: DawParameterSnapshot,
    /// Full binary state for all FX (for complete restore)
    pub state_chunks: DawStateChunkSnapshot,
}

// ─────────────────────────────────────────────────────────────────────────────
// DawFullPreset — Preset bundled with associated snapshots
// ─────────────────────────────────────────────────────────────────────────────

/// A preset that bundles state chunks with associated parameter snapshots.
///
/// When saving a preset, we also capture any filled snapshot slots so they
/// can be restored on a different track. GUIDs are remapped during cross-track
/// recall so morph/diff continues to work.
#[derive(Debug, Clone, Facet)]
pub struct DawFullPreset {
    /// Human-readable label
    pub name: String,
    /// Full binary state for all FX
    pub state_chunks: DawStateChunkSnapshot,
    /// Bundled snapshot slots (parameter snapshots captured on the source track)
    pub snapshot_slots: Vec<DawFullPresetSlot>,
}

/// A snapshot slot bundled inside a full preset.
#[derive(Debug, Clone, Facet)]
pub struct DawFullPresetSlot {
    /// Slot index in the snapshot panel
    pub slot_index: usize,
    /// Snapshot name
    pub name: String,
    /// Parameter snapshot data
    pub snapshot: DawParameterSnapshot,
}

// ─────────────────────────────────────────────────────────────────────────────
// DawModulePreset — Container-level preset (FX loaded inside a named container)
// ─────────────────────────────────────────────────────────────────────────────

/// A module preset stored as raw RPP chunk text.
///
/// Contains the complete `<CONTAINER Container NAME ...>...</CONTAINER>` RPP
/// block that can be spliced directly into any track's FXCHAIN section.
/// REAPER handles all plugin instantiation and state restoration atomically —
/// no manual FX creation or container manipulation needed.
#[derive(Debug, Clone, Facet)]
pub struct DawModulePreset {
    /// Module/container name (e.g., "TIME", "DRIVE", "AMP")
    pub container_name: String,
    /// Raw RPP chunk text for the container block (the full `<CONTAINER ...>...\n>` text)
    pub chunk_text: String,
    /// Bundled parameter snapshots (for morph/diff after loading)
    pub snapshot_slots: Vec<DawFullPresetSlot>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Apply result types
// ─────────────────────────────────────────────────────────────────────────────

/// Result of applying a parameter snapshot, including GUID remap info for
/// cross-track loading.
pub struct ApplySnapshotResult {
    /// Parameters that were actually changed
    pub changes: Vec<DawParamChange>,
    /// GUID remap: source_guid → target_guid (non-empty when cross-track)
    pub guid_remap: HashMap<String, String>,
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
                formatted: p.formatted.clone(),
                is_toggle: p.is_toggle,
                step_count: p.step_count,
                step_labels: p.step_labels.clone(),
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

/// Apply a parameter snapshot with cross-track support.
///
/// Reads current parameter state, diffs against the snapshot, and only
/// writes parameters that actually changed. When an FX GUID isn't found
/// in the target chain, falls back to plugin name matching and records
/// the GUID remap for the caller.
pub async fn apply_parameter_snapshot(
    chain: &FxChain,
    snapshot: &DawParameterSnapshot,
) -> Result<ApplySnapshotResult> {
    let mut changes = Vec::new();
    let mut guid_remap: HashMap<String, String> = HashMap::new();

    // Pre-fetch target chain FX for name-based fallback
    let target_fx = chain.all().await?;
    // Build name → list of (guid, index) for positional matching of duplicates
    let mut name_to_fx: HashMap<String, Vec<(String, u32)>> = HashMap::new();
    for fx in &target_fx {
        name_to_fx
            .entry(fx.plugin_name.clone())
            .or_default()
            .push((fx.guid.clone(), fx.index));
    }
    let mut name_consumed: HashMap<String, usize> = HashMap::new();

    // Also build a GUID set for fast membership check
    let target_guids: std::collections::HashSet<&str> =
        target_fx.iter().map(|f| f.guid.as_str()).collect();

    for fx_state in &snapshot.fx_states {
        // Try GUID match first
        let handle = if target_guids.contains(fx_state.fx_guid.as_str()) {
            chain.by_guid(&fx_state.fx_guid).await?
        } else {
            // GUID not in target chain — try name-based fallback
            let consumed = name_consumed
                .entry(fx_state.plugin_name.clone())
                .or_insert(0);
            if let Some(candidates) = name_to_fx.get(&fx_state.plugin_name) {
                if let Some((target_guid, _idx)) = candidates.get(*consumed) {
                    tracing::debug!(
                        "Cross-track name match: '{}' {} → {}",
                        fx_state.plugin_name,
                        fx_state.fx_guid,
                        target_guid
                    );
                    guid_remap.insert(fx_state.fx_guid.clone(), target_guid.clone());
                    *consumed += 1;
                    chain.by_guid(target_guid).await?
                } else {
                    tracing::warn!(
                        "FX '{}' has no more unmatched instances on target",
                        fx_state.plugin_name
                    );
                    None
                }
            } else {
                tracing::warn!(
                    "FX '{}' (GUID {}) not found by name on target",
                    fx_state.plugin_name,
                    fx_state.fx_guid
                );
                None
            }
        };

        let Some(handle) = handle else { continue };

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

    Ok(ApplySnapshotResult {
        changes,
        guid_remap,
    })
}

/// Apply state chunks to restore full plugin state.
///
/// Matches FX by GUID and restores their binary state. FX not found
/// in the current chain are skipped gracefully.
pub async fn apply_state_chunks(chain: &FxChain, snapshot: &DawStateChunkSnapshot) -> Result<()> {
    chain.restore_state(snapshot.chunks.clone()).await
}

/// Apply state chunks with cross-track support.
///
/// When GUIDs don't match (different track), falls back to matching by
/// plugin name. If no match is found at all, **adds the FX** to the chain
/// first, then applies the state chunk. Returns a GUID remap table so the
/// caller can update cached snapshots.
pub async fn apply_state_chunks_cross_track(
    chain: &FxChain,
    snapshot: &DawStateChunkSnapshot,
) -> Result<HashMap<String, String>> {
    let mut guid_remap: HashMap<String, String> = HashMap::new();

    // Get target chain FX info for name-based matching
    let target_fx = chain.all().await?;
    let target_guids: std::collections::HashSet<String> =
        target_fx.iter().map(|f| f.guid.clone()).collect();

    // Build plugin_name → list of target GUIDs for positional matching
    // NOTE: Use plugin_name (original name), not name (user-renameable display name)
    let mut name_to_target: HashMap<String, Vec<String>> = HashMap::new();
    for fx in &target_fx {
        name_to_target
            .entry(fx.plugin_name.clone())
            .or_default()
            .push(fx.guid.clone());
    }
    let mut name_consumed: HashMap<String, usize> = HashMap::new();

    // Phase 1: Match existing FX by GUID or name, collect unmatched chunks
    let mut remapped_chunks = Vec::with_capacity(snapshot.chunks.len());
    let mut unmatched_chunks: Vec<&FxStateChunk> = Vec::new();

    for chunk in &snapshot.chunks {
        if target_guids.contains(&chunk.fx_guid) {
            // GUID match — use as-is
            remapped_chunks.push(chunk.clone());
        } else {
            // Fallback: match by plugin name
            let consumed = name_consumed.entry(chunk.plugin_name.clone()).or_insert(0);
            if let Some(target_guids_for_name) = name_to_target.get(&chunk.plugin_name) {
                if let Some(target_guid) = target_guids_for_name.get(*consumed) {
                    tracing::debug!(
                        "Cross-track chunk match: '{}' {} → {}",
                        chunk.plugin_name,
                        chunk.fx_guid,
                        target_guid
                    );
                    guid_remap.insert(chunk.fx_guid.clone(), target_guid.clone());
                    let mut remapped = chunk.clone();
                    remapped.fx_guid = target_guid.clone();
                    remapped_chunks.push(remapped);
                    *consumed += 1;
                } else {
                    unmatched_chunks.push(chunk);
                }
            } else {
                unmatched_chunks.push(chunk);
            }
        }
    }

    // Phase 2: Apply matched chunks to existing FX
    if !remapped_chunks.is_empty() {
        tracing::debug!("Restoring {} matched state chunks", remapped_chunks.len());
        chain.restore_state(remapped_chunks).await?;
    }

    // Phase 3: Add missing FX to the chain, then apply their state chunks
    for chunk in &unmatched_chunks {
        tracing::debug!(
            "Adding missing FX '{}' to chain (source GUID {})",
            chunk.plugin_name,
            chunk.fx_guid
        );
        match chain.add(&chunk.plugin_name).await {
            Ok(handle) => {
                let new_guid = handle.guid().to_string();
                tracing::debug!(
                    "Added '{}' → new GUID {}, applying state chunk",
                    chunk.plugin_name,
                    new_guid
                );
                guid_remap.insert(chunk.fx_guid.clone(), new_guid.clone());

                // Apply state chunk to the newly added FX
                let mut remapped = (*chunk).clone();
                remapped.fx_guid = new_guid;
                // Restore just this one chunk
                chain.restore_state(vec![remapped]).await?;
            }
            Err(e) => {
                tracing::warn!("Failed to add FX '{}': {e}", chunk.plugin_name);
            }
        }
    }

    Ok(guid_remap)
}

/// Apply a module preset by inserting its raw RPP chunk into the FX chain.
///
/// This is a single atomic operation — REAPER handles all plugin instantiation,
/// container creation, and state restoration from the RPP chunk text.
pub async fn apply_module_preset(chain: &FxChain, preset: &DawModulePreset) -> Result<()> {
    tracing::debug!(
        "Applying module preset '{}' ({} bytes chunk)",
        preset.container_name,
        preset.chunk_text.len()
    );
    chain.insert_chunk(&preset.chunk_text).await
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

/// Remap GUIDs in a parameter snapshot using a remap table.
///
/// Used after cross-track loading to update cached snapshots so that
/// morph/diff operations reference the new track's FX GUIDs.
pub fn remap_snapshot_guids(
    snapshot: &mut DawParameterSnapshot,
    guid_remap: &HashMap<String, String>,
) {
    for fx_state in &mut snapshot.fx_states {
        if let Some(new_guid) = guid_remap.get(&fx_state.fx_guid) {
            fx_state.fx_guid = new_guid.clone();
        }
    }
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
