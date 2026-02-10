//! Live DAW Snapshot Panel — captures and recalls real FX parameter snapshots
//! and state chunk presets from the currently selected REAPER track.
//!
//! Features:
//! - Follows REAPER's track selection (polling every 2s)
//! - Captures parameter snapshots and state chunk presets via `daw_bridge`
//! - Morph diff view: shows only parameters that differ between A/B snapshots
//! - Debounced morph slider applies interpolated values to DAW in real-time

use crate::components::daw_preset_panel::{
    DawPresetEntry, DawPresetPanel, DAW_ACTIVE_PRESET, DAW_PRESETS,
};
use crate::components::daw_snapshot_panel::{
    DawSnapshotPanel, DAW_SNAPSHOT_SLOTS, MORPH_EASING, MORPH_POSITION, MORPH_SLOT_A, MORPH_SLOT_B,
};
use crate::prelude::*;
use daw_control::FxTree;
use signal_control::daw_bridge::{
    self, DawFullPreset, DawFullPresetSlot, DawModulePreset, DawParameterSnapshot,
    DawStateChunkSnapshot,
};
use signal_control::morph_engine::EasingCurve;
use std::collections::HashMap;
use tracing::{debug, info, warn};
use uuid::Uuid;

// ─── Global state ───────────────────────────────────────────────────

/// Status log — shows last action taken.
static PANEL_STATUS: GlobalSignal<String> = Signal::global(|| "Waiting for DAW...".to_string());

/// In-memory cache: snapshot UUID → serialized DawParameterSnapshot JSON.
static SNAPSHOT_CACHE: GlobalSignal<HashMap<Uuid, String>> = Signal::global(HashMap::new);

/// In-memory cache: preset UUID → serialized DawStateChunkSnapshot JSON.
static PRESET_CACHE: GlobalSignal<HashMap<Uuid, String>> = Signal::global(HashMap::new);

/// Currently tracked REAPER track GUID.
static TRACKED_TRACK_GUID: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Currently tracked REAPER track name.
static TRACKED_TRACK_NAME: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Whether the DAW connection is live.
static DAW_CONNECTED: GlobalSignal<bool> = Signal::global(|| false);

/// FX count on current track (for display).
static TRACKED_FX_COUNT: GlobalSignal<usize> = Signal::global(|| 0);

/// FX plugin names on current track (for dialog display).
static TRACKED_FX_NAMES: GlobalSignal<Vec<String>> = Signal::global(Vec::new);

/// Full FX info list on current track (for the FX list sidebar).
static TRACKED_FX_LIST: GlobalSignal<Vec<daw_control::Fx>> = Signal::global(Vec::new);

/// FX tree for current track (for tree preview).
static TRACKED_FX_TREE: GlobalSignal<FxTree> = Signal::global(FxTree::new);

/// Selected FX index in the list sidebar.
static SELECTED_FX_INDICES: GlobalSignal<Vec<usize>> = Signal::global(Vec::new);

// ─── Morph diff state ───────────────────────────────────────────────

/// A single parameter difference between morph snapshots A and B.
#[derive(Clone, Debug)]
struct MorphDiffEntry {
    fx_guid: String,
    fx_name: String,
    param_index: u32,
    param_name: String,
    value_a: f64,
    value_b: f64,
}

/// Computed diff entries between morph A and B.
static MORPH_DIFF: GlobalSignal<Vec<MorphDiffEntry>> = Signal::global(Vec::new);

/// Cached deserialized snapshot for morph A.
static MORPH_SNAP_A: GlobalSignal<Option<DawParameterSnapshot>> = Signal::global(|| None);

/// Cached deserialized snapshot for morph B.
static MORPH_SNAP_B: GlobalSignal<Option<DawParameterSnapshot>> = Signal::global(|| None);

/// Name of morph A snapshot.
static MORPH_NAME_A: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Name of morph B snapshot.
static MORPH_NAME_B: GlobalSignal<Option<String>> = Signal::global(|| None);

/// Live current values for diff params: keyed by (fx_guid, param_index).
/// Updated periodically when morph diff is active.
static LIVE_PARAM_VALUES: GlobalSignal<HashMap<(String, u32), f64>> = Signal::global(HashMap::new);

// ─── Helper ─────────────────────────────────────────────────────────

/// Resolve the current track's FxChain handle.
async fn get_tracked_fx_chain() -> Option<daw_control::FxChain> {
    let daw = daw_control::Daw::try_get()?;
    let guid = TRACKED_TRACK_GUID.read().clone()?;
    let project = daw.current_project().await.ok()?;
    let track = project.tracks().by_guid(&guid).await.ok()??;
    Some(track.fx_chain())
}

/// Recompute the morph diff from current MORPH_SLOT_A/B → SNAPSHOT_CACHE → MORPH_DIFF.
fn recompute_morph_diff() {
    let slot_a = *MORPH_SLOT_A.read();
    let slot_b = *MORPH_SLOT_B.read();

    // Resolve slot A → snapshot UUID → cached JSON → deserialized
    let snap_a = slot_a.and_then(|(page, idx)| {
        let slots = DAW_SNAPSHOT_SLOTS.read();
        let slot = slots.pages.get(page)?.get(idx)?;
        let id = slot.snapshot_id?;
        let cache = SNAPSHOT_CACHE.read();
        let json = cache.get(&id)?;
        facet_json::from_str::<DawParameterSnapshot>(json).ok()
    });

    let snap_b = slot_b.and_then(|(page, idx)| {
        let slots = DAW_SNAPSHOT_SLOTS.read();
        let slot = slots.pages.get(page)?.get(idx)?;
        let id = slot.snapshot_id?;
        let cache = SNAPSHOT_CACHE.read();
        let json = cache.get(&id)?;
        facet_json::from_str::<DawParameterSnapshot>(json).ok()
    });

    // Store names for display
    *MORPH_NAME_A.write() = snap_a.as_ref().map(|s| s.name.clone());
    *MORPH_NAME_B.write() = snap_b.as_ref().map(|s| s.name.clone());

    // Compute diff if both exist
    if let (Some(ref a), Some(ref b)) = (&snap_a, &snap_b) {
        let raw_diffs = daw_bridge::diff_parameter_snapshots(a, b);

        // Build FX GUID → name lookup from snapshot B (target)
        let fx_names: HashMap<&str, &str> = b
            .fx_states
            .iter()
            .map(|s| (s.fx_guid.as_str(), s.plugin_name.as_str()))
            .collect();

        // Also check snapshot A for FX names not in B
        let fx_names_a: HashMap<&str, &str> = a
            .fx_states
            .iter()
            .map(|s| (s.fx_guid.as_str(), s.plugin_name.as_str()))
            .collect();

        let entries: Vec<MorphDiffEntry> = raw_diffs
            .iter()
            .map(|d| {
                let fx_name = fx_names
                    .get(d.fx_guid.as_str())
                    .or_else(|| fx_names_a.get(d.fx_guid.as_str()))
                    .unwrap_or(&"Unknown FX")
                    .to_string();
                MorphDiffEntry {
                    fx_guid: d.fx_guid.clone(),
                    fx_name,
                    param_index: d.param_index,
                    param_name: d.param_name.clone(),
                    value_a: d.from_value,
                    value_b: d.to_value,
                }
            })
            .collect();

        *MORPH_DIFF.write() = entries;
    } else {
        MORPH_DIFF.write().clear();
    }

    *MORPH_SNAP_A.write() = snap_a;
    *MORPH_SNAP_B.write() = snap_b;
}

// ─── Component ──────────────────────────────────────────────────────

#[component]
pub fn SnapshotTestHarness() -> Element {
    // ── DAW polling loop ─────────────────────────────────────────
    use_future(move || async move {
        loop {
            if daw_control::Daw::try_get().is_some() {
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }

        *DAW_CONNECTED.write() = true;
        *PANEL_STATUS.write() = "Connected — select a track in REAPER".to_string();

        let daw = daw_control::Daw::get();

        loop {
            if let Ok(project) = daw.current_project().await {
                let sel_tracks = project.tracks().selected().await.unwrap_or_default();
                let sel_track = sel_tracks.into_iter().next();

                if let Some(th) = sel_track {
                    let guid = th.guid().to_string();
                    let name = th.info().await.map(|t| t.name).unwrap_or_default();

                    let prev_guid = TRACKED_TRACK_GUID.read().clone();
                    let track_changed = prev_guid.as_deref() != Some(&guid);

                    *TRACKED_TRACK_GUID.write() = Some(guid.clone());
                    *TRACKED_TRACK_NAME.write() = Some(name);

                    if track_changed {
                        SELECTED_FX_INDICES.write().clear();
                    }

                    // Always refresh FX list and tree (not just on track change)
                    if let Ok(Some(track_handle)) = project.tracks().by_guid(&guid).await {
                        let chain = track_handle.fx_chain();
                        if let Ok(fx_list) = chain.all().await {
                            *TRACKED_FX_COUNT.write() = fx_list.len();
                            *TRACKED_FX_NAMES.write() =
                                fx_list.iter().map(|fx| fx.plugin_name.clone()).collect();
                            *TRACKED_FX_LIST.write() = fx_list.clone();
                            if track_changed {
                                *PANEL_STATUS.write() =
                                    format!("Tracking: {} FX on chain", fx_list.len());
                            }
                        }
                        if let Ok(tree) = chain.tree().await {
                            *TRACKED_FX_TREE.write() = tree;
                        }
                    }
                } else if TRACKED_TRACK_GUID.read().is_some() {
                    *TRACKED_TRACK_GUID.write() = None;
                    *TRACKED_TRACK_NAME.write() = None;
                    *TRACKED_FX_COUNT.write() = 0;
                    TRACKED_FX_NAMES.write().clear();
                    TRACKED_FX_LIST.write().clear();
                    *TRACKED_FX_TREE.write() = FxTree::new();
                    SELECTED_FX_INDICES.write().clear();
                    *PANEL_STATUS.write() = "No track selected in REAPER".to_string();
                }
            }

            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
        }
    });

    // ── Debounced morph application loop ─────────────────────────
    use_future(move || async move {
        let mut last_pos = -1.0f64;
        loop {
            tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

            let pos = *MORPH_POSITION.peek();
            if (pos - last_pos).abs() < 0.001 {
                continue;
            }
            last_pos = pos;

            // Need both snapshots loaded
            let has_both = MORPH_SNAP_A.peek().is_some() && MORPH_SNAP_B.peek().is_some();
            if !has_both {
                continue;
            }

            let easing = *MORPH_EASING.peek();
            let eased_t = easing.apply(pos);
            let diffs = MORPH_DIFF.peek().clone();

            if diffs.is_empty() {
                continue;
            }

            let Some(chain) = get_tracked_fx_chain().await else {
                warn!("Morph: could not get tracked FX chain");
                continue;
            };

            let mut applied = 0usize;
            let mut not_found = 0usize;
            let mut errors = 0usize;
            for diff in &diffs {
                let interpolated = diff.value_a + eased_t * (diff.value_b - diff.value_a);
                match chain.by_guid(&diff.fx_guid).await {
                    Ok(Some(handle)) => {
                        match handle.param(diff.param_index).set(interpolated).await {
                            Ok(()) => {
                                applied += 1;
                            }
                            Err(e) => {
                                warn!(
                                    "Morph: set param failed for {} param {} = {}: {}",
                                    diff.fx_name, diff.param_name, interpolated, e
                                );
                                errors += 1;
                            }
                        }
                    }
                    Ok(None) => {
                        debug!(
                            "Morph: FX not found by GUID '{}' ({})",
                            diff.fx_guid, diff.fx_name
                        );
                        not_found += 1;
                    }
                    Err(e) => {
                        warn!("Morph: by_guid error for '{}': {}", diff.fx_guid, e);
                        errors += 1;
                    }
                }
            }

            if not_found > 0 || errors > 0 {
                info!(
                    "Morph {:.0}%: applied={}, not_found={}, errors={}, total={}",
                    pos * 100.0,
                    applied,
                    not_found,
                    errors,
                    diffs.len()
                );
            }

            *PANEL_STATUS.write() = if not_found > 0 {
                format!(
                    "Morph {:.0}% — {}/{} params ({} FX not found)",
                    pos * 100.0,
                    applied,
                    diffs.len(),
                    not_found
                )
            } else if errors > 0 {
                format!(
                    "Morph {:.0}% — {}/{} params ({} errors)",
                    pos * 100.0,
                    applied,
                    diffs.len(),
                    errors
                )
            } else {
                format!(
                    "Morph {:.0}% ({}) — {}/{} params",
                    pos * 100.0,
                    easing.label(),
                    applied,
                    diffs.len()
                )
            };
        }
    });

    // ── Live parameter value polling ─────────────────────────────
    // Read actual DAW parameter values every 500ms for diff display
    use_future(move || async move {
        loop {
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

            let diffs = MORPH_DIFF.peek().clone();
            if diffs.is_empty() {
                continue;
            }

            let Some(chain) = get_tracked_fx_chain().await else {
                continue;
            };

            let mut live_values = HashMap::new();
            for diff in &diffs {
                if let Ok(Some(handle)) = chain.by_guid(&diff.fx_guid).await {
                    if let Ok(val) = handle.param(diff.param_index).get().await {
                        live_values.insert((diff.fx_guid.clone(), diff.param_index), val);
                    }
                }
            }

            if !live_values.is_empty() {
                *LIVE_PARAM_VALUES.write() = live_values;
            }
        }
    });

    // ── Reactive diff computation ────────────────────────────────
    // Recompute when morph A/B slot assignments change
    let _slot_a = *MORPH_SLOT_A.read();
    let _slot_b = *MORPH_SLOT_B.read();
    // Also react to snapshot cache changes (new captures)
    let _cache_len = SNAPSHOT_CACHE.read().len();
    use_effect(move || {
        recompute_morph_diff();
    });

    // ── Module preset dialog state ──────────────────────────────
    let mut show_module_save_dialog = use_signal(|| false);
    let mut module_save_name = use_signal(String::new);

    // ── Read reactive state ──────────────────────────────────────
    let connected = *DAW_CONNECTED.read();
    let track_name = TRACKED_TRACK_NAME.read().clone();
    let track_available = TRACKED_TRACK_GUID.read().is_some();
    let fx_count = *TRACKED_FX_COUNT.read();
    let fx_names = TRACKED_FX_NAMES.read().clone();
    let status = PANEL_STATUS.read().clone();

    // Count filled snapshot slots for the module save dialog
    let filled_snapshot_count = {
        let slots = DAW_SNAPSHOT_SLOTS.read();
        slots
            .pages
            .iter()
            .flat_map(|page| page.iter())
            .filter(|s| s.is_filled())
            .count()
    };

    let morph_diffs = MORPH_DIFF.read().clone();
    let morph_pos = *MORPH_POSITION.read();
    let morph_easing = *MORPH_EASING.read();
    let morph_name_a = MORPH_NAME_A.read().clone();
    let morph_name_b = MORPH_NAME_B.read().clone();
    let has_morph = morph_name_a.is_some() && morph_name_b.is_some();
    let eased_t = morph_easing.apply(morph_pos);
    let live_values = LIVE_PARAM_VALUES.read().clone();

    // Group diffs by FX name for display
    let grouped_diffs: Vec<(String, Vec<&MorphDiffEntry>)> = {
        let mut groups: Vec<(String, Vec<&MorphDiffEntry>)> = Vec::new();
        for entry in morph_diffs.iter() {
            if let Some(group) = groups.iter_mut().find(|(name, _)| *name == entry.fx_name) {
                group.1.push(entry);
            } else {
                groups.push((entry.fx_name.clone(), vec![entry]));
            }
        }
        groups
    };

    // ── Snapshot callbacks ───────────────────────────────────────

    let snap_on_save = Callback::new(move |(slot_index, name): (usize, String)| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available for capture".to_string();
                return;
            };

            let display_name = if name.starts_with("Slot") {
                format!("Snap {}", slot_index + 1)
            } else {
                name
            };

            match daw_bridge::capture_parameter_snapshot(&chain, &display_name).await {
                Ok(snapshot) => {
                    let fx_count = snapshot.fx_states.len();
                    let param_count: usize =
                        snapshot.fx_states.iter().map(|s| s.parameters.len()).sum();
                    info!(
                        "Captured snapshot '{}': {} FX, {} params",
                        display_name, fx_count, param_count
                    );
                    for fx in &snapshot.fx_states {
                        info!(
                            "  Captured FX '{}' guid='{}' idx={} params={}",
                            fx.plugin_name,
                            fx.fx_guid,
                            fx.fx_index,
                            fx.parameters.len()
                        );
                    }
                    let id = Uuid::new_v4();

                    if let Ok(json) = facet_json::to_string(&snapshot) {
                        SNAPSHOT_CACHE.write().insert(id, json);
                    }

                    DAW_SNAPSHOT_SLOTS
                        .write()
                        .save_to_slot(slot_index, id, display_name.clone());

                    *PANEL_STATUS.write() = format!(
                        "Captured '{}' → slot {} ({} FX, {} params)",
                        display_name,
                        slot_index + 1,
                        fx_count,
                        param_count
                    );
                }
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Capture failed: {e}");
                }
            }
        });
    });

    let snap_on_recall = Callback::new(move |id: Uuid| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available for recall".to_string();
                return;
            };

            let json = {
                let cache = SNAPSHOT_CACHE.read();
                cache.get(&id).cloned()
            };

            let Some(json) = json else {
                *PANEL_STATUS.write() = format!("Snapshot {} not in cache", &id.to_string()[..8]);
                return;
            };

            match facet_json::from_str::<DawParameterSnapshot>(&json) {
                Ok(snapshot) => {
                    info!(
                        "Recalling snapshot '{}' with {} FX states",
                        snapshot.name,
                        snapshot.fx_states.len()
                    );
                    for fx in &snapshot.fx_states {
                        info!(
                            "  FX '{}' guid='{}' idx={} params={}",
                            fx.plugin_name,
                            fx.fx_guid,
                            fx.fx_index,
                            fx.parameters.len()
                        );
                    }
                    match daw_bridge::apply_parameter_snapshot(&chain, &snapshot).await {
                        Ok(result) => {
                            info!("Recall applied {} changes", result.changes.len());
                            for c in &result.changes {
                                debug!(
                                    "  Changed: {} param {} '{}' {:.4} -> {:.4}",
                                    c.fx_guid,
                                    c.param_index,
                                    c.param_name,
                                    c.from_value,
                                    c.to_value
                                );
                            }
                            // If GUIDs were remapped (cross-track), update cached snapshot
                            if !result.guid_remap.is_empty() {
                                info!(
                                    "Cross-track recall: {} GUIDs remapped",
                                    result.guid_remap.len()
                                );
                                let mut remapped = snapshot.clone();
                                daw_bridge::remap_snapshot_guids(&mut remapped, &result.guid_remap);
                                if let Ok(updated_json) = facet_json::to_string(&remapped) {
                                    SNAPSHOT_CACHE.write().insert(id, updated_json);
                                }
                            }
                            let status = if !result.guid_remap.is_empty() {
                                format!(
                                    "Recalled '{}' — {} params changed ({} cross-track remapped)",
                                    snapshot.name,
                                    result.changes.len(),
                                    result.guid_remap.len()
                                )
                            } else {
                                format!(
                                    "Recalled '{}' — {} params changed",
                                    snapshot.name,
                                    result.changes.len()
                                )
                            };
                            *PANEL_STATUS.write() = status;
                        }
                        Err(e) => {
                            warn!("Recall failed: {e}");
                            *PANEL_STATUS.write() = format!("Recall failed: {e}");
                        }
                    }
                }
                Err(e) => {
                    warn!("Snapshot deserialize error: {e}");
                    *PANEL_STATUS.write() = format!("Deserialize error: {e}");
                }
            }
        });
    });

    // on_morph just updates the position signal — the debounced loop handles DAW writes
    let snap_on_morph = Callback::new(move |(pos, _easing): (f64, EasingCurve)| {
        *MORPH_POSITION.write() = pos;
    });

    let snap_on_delete = Callback::new(move |(slot_index, id): (usize, Uuid)| {
        SNAPSHOT_CACHE.write().remove(&id);
        DAW_SNAPSHOT_SLOTS.write().clear_slot(slot_index);
        *PANEL_STATUS.write() = format!("Deleted snapshot from slot {}", slot_index + 1);
    });

    // ── Preset callbacks ─────────────────────────────────────────

    let preset_on_save = Callback::new(move |name: String| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available for preset capture".to_string();
                return;
            };

            match daw_bridge::capture_state_chunks(&chain, &name).await {
                Ok(state_chunks) => {
                    let chunk_count = state_chunks.chunks.len();
                    let id = Uuid::new_v4();

                    // Bundle current snapshot slots into the preset
                    let snapshot_slots = {
                        let slots_state = DAW_SNAPSHOT_SLOTS.read();
                        let cache = SNAPSHOT_CACHE.read();
                        let mut bundled = Vec::new();
                        for (page_idx, page) in slots_state.pages.iter().enumerate() {
                            for (slot_idx, slot) in page.iter().enumerate() {
                                if let (Some(snap_id), Some(slot_name)) =
                                    (slot.snapshot_id, slot.name.as_ref())
                                {
                                    if let Some(json) = cache.get(&snap_id) {
                                        if let Ok(snapshot) =
                                            facet_json::from_str::<DawParameterSnapshot>(json)
                                        {
                                            bundled.push(DawFullPresetSlot {
                                                slot_index: page_idx * slots_state.slots_per_page
                                                    + slot_idx,
                                                name: slot_name.clone(),
                                                snapshot,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        bundled
                    };

                    let snap_count = snapshot_slots.len();
                    let full_preset = DawFullPreset {
                        name: name.clone(),
                        state_chunks,
                        snapshot_slots,
                    };

                    if let Ok(json) = facet_json::to_string(&full_preset) {
                        PRESET_CACHE.write().insert(id, json);
                    }

                    DAW_PRESETS.write().push(DawPresetEntry {
                        id,
                        name: name.clone(),
                        created_at: "Just now".to_string(),
                        fx_count: chunk_count,
                        has_snapshots: snap_count > 0,
                        is_module: false,
                    });

                    *PANEL_STATUS.write() = format!(
                        "Saved preset '{}' ({} FX, {} snapshots)",
                        name, chunk_count, snap_count
                    );
                }
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Preset capture failed: {e}");
                }
            }
        });
    });

    let preset_on_recall = Callback::new(move |id: Uuid| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available for preset recall".to_string();
                return;
            };

            let json = {
                let cache = PRESET_CACHE.read();
                cache.get(&id).cloned()
            };

            let Some(json) = json else {
                *PANEL_STATUS.write() = format!("Preset {} not in cache", &id.to_string()[..8]);
                return;
            };

            // Try full preset first (with bundled snapshots), fall back to legacy state-only
            if let Ok(full_preset) = facet_json::from_str::<DawFullPreset>(&json) {
                // Apply state chunks with cross-track name-based matching
                match daw_bridge::apply_state_chunks_cross_track(&chain, &full_preset.state_chunks)
                    .await
                {
                    Ok(guid_remap) => {
                        let chunk_count = full_preset.state_chunks.chunks.len();
                        let remap_count = guid_remap.len();

                        // Restore bundled snapshots with GUID remapping
                        let snap_count = full_preset.snapshot_slots.len();
                        if snap_count > 0 {
                            let mut slots_state = DAW_SNAPSHOT_SLOTS.write();
                            let mut cache = SNAPSHOT_CACHE.write();
                            let spp = slots_state.slots_per_page;

                            for bundled_slot in &full_preset.snapshot_slots {
                                let mut snapshot = bundled_slot.snapshot.clone();
                                // Remap GUIDs for cross-track loading
                                if !guid_remap.is_empty() {
                                    daw_bridge::remap_snapshot_guids(&mut snapshot, &guid_remap);
                                }
                                let snap_id = Uuid::new_v4();
                                if let Ok(snap_json) = facet_json::to_string(&snapshot) {
                                    cache.insert(snap_id, snap_json);
                                }
                                // Convert absolute slot_index → (page, slot_within_page)
                                let page_idx = bundled_slot.slot_index / spp;
                                let slot_in_page = bundled_slot.slot_index % spp;

                                // Ensure enough pages exist
                                while slots_state.pages.len() <= page_idx {
                                    use crate::components::daw_snapshot_panel::DawSnapshotSlot;
                                    let new_page: Vec<DawSnapshotSlot> =
                                        (0..spp).map(DawSnapshotSlot::empty).collect();
                                    slots_state.pages.push(new_page);
                                }
                                // Write directly into the target page's slot
                                if let Some(slot) = slots_state
                                    .pages
                                    .get_mut(page_idx)
                                    .and_then(|p| p.get_mut(slot_in_page))
                                {
                                    slot.snapshot_id = Some(snap_id);
                                    slot.name = Some(bundled_slot.name.clone());
                                }
                            }
                        }

                        *DAW_ACTIVE_PRESET.write() = Some(id);
                        let status = if remap_count > 0 {
                            format!(
                                "Recalled '{}' ({} FX, {} cross-track remapped, {} snapshots)",
                                full_preset.name, chunk_count, remap_count, snap_count
                            )
                        } else {
                            format!(
                                "Recalled '{}' ({} FX, {} snapshots)",
                                full_preset.name, chunk_count, snap_count
                            )
                        };
                        *PANEL_STATUS.write() = status;
                    }
                    Err(e) => {
                        *PANEL_STATUS.write() = format!("Preset recall failed: {e}");
                    }
                }
            } else if let Ok(legacy) = facet_json::from_str::<DawStateChunkSnapshot>(&json) {
                // Legacy preset without bundled snapshots
                match daw_bridge::apply_state_chunks_cross_track(&chain, &legacy).await {
                    Ok(guid_remap) => {
                        let remap_count = guid_remap.len();
                        *DAW_ACTIVE_PRESET.write() = Some(id);
                        let status = if remap_count > 0 {
                            format!(
                                "Recalled '{}' ({} FX, {} cross-track remapped)",
                                legacy.name,
                                legacy.chunks.len(),
                                remap_count
                            )
                        } else {
                            format!(
                                "Recalled '{}' ({} chunks restored)",
                                legacy.name,
                                legacy.chunks.len()
                            )
                        };
                        *PANEL_STATUS.write() = status;
                    }
                    Err(e) => {
                        *PANEL_STATUS.write() = format!("Preset recall failed: {e}");
                    }
                }
            } else {
                *PANEL_STATUS.write() = "Deserialize error: unrecognized preset format".to_string();
            }
        });
    });

    let preset_on_rename = Callback::new(move |(id, new_name): (Uuid, String)| {
        let mut presets = DAW_PRESETS.write();
        if let Some(p) = presets.iter_mut().find(|p| p.id == id) {
            p.name = new_name.clone();
        }
        *PANEL_STATUS.write() = format!("Renamed preset → '{new_name}'");
    });

    let preset_on_delete = Callback::new(move |id: Uuid| {
        DAW_PRESETS.write().retain(|p| p.id != id);
        PRESET_CACHE.write().remove(&id);
        if *DAW_ACTIVE_PRESET.read() == Some(id) {
            *DAW_ACTIVE_PRESET.write() = None;
        }
        *PANEL_STATUS.write() = "Deleted preset".to_string();
    });

    // ── Action bar buttons ───────────────────────────────────────

    let capture_snapshot = move |_| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available".to_string();
                return;
            };

            let name = format!("Snap {}", chrono_stub());

            match daw_bridge::capture_parameter_snapshot(&chain, &name).await {
                Ok(snapshot) => {
                    let fx_count = snapshot.fx_states.len();
                    let id = Uuid::new_v4();

                    if let Ok(json) = facet_json::to_string(&snapshot) {
                        SNAPSHOT_CACHE.write().insert(id, json);
                    }

                    let mut state = DAW_SNAPSHOT_SLOTS.write();
                    let empty_idx = state.current_slots().iter().position(|s| !s.is_filled());
                    if let Some(idx) = empty_idx {
                        state.save_to_slot(idx, id, name.clone());
                        *PANEL_STATUS.write() =
                            format!("Captured '{}' → slot {} ({} FX)", name, idx + 1, fx_count);
                    } else {
                        *PANEL_STATUS.write() = "All slots full — go to next page".to_string();
                    }
                }
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Capture failed: {e}");
                }
            }
        });
    };

    let save_preset = move |_| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available".to_string();
                return;
            };

            let name = format!("Preset {}", chrono_stub());

            match daw_bridge::capture_state_chunks(&chain, &name).await {
                Ok(state_chunks) => {
                    let chunk_count = state_chunks.chunks.len();
                    let id = Uuid::new_v4();

                    // Bundle snapshots (same logic as preset_on_save)
                    let snapshot_slots = {
                        let slots_state = DAW_SNAPSHOT_SLOTS.read();
                        let cache = SNAPSHOT_CACHE.read();
                        let mut bundled = Vec::new();
                        for (page_idx, page) in slots_state.pages.iter().enumerate() {
                            for (slot_idx, slot) in page.iter().enumerate() {
                                if let (Some(snap_id), Some(slot_name)) =
                                    (slot.snapshot_id, slot.name.as_ref())
                                {
                                    if let Some(json) = cache.get(&snap_id) {
                                        if let Ok(snapshot) =
                                            facet_json::from_str::<DawParameterSnapshot>(json)
                                        {
                                            bundled.push(DawFullPresetSlot {
                                                slot_index: page_idx * slots_state.slots_per_page
                                                    + slot_idx,
                                                name: slot_name.clone(),
                                                snapshot,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        bundled
                    };
                    let snap_count = snapshot_slots.len();
                    let full_preset = DawFullPreset {
                        name: name.clone(),
                        state_chunks,
                        snapshot_slots,
                    };

                    if let Ok(json) = facet_json::to_string(&full_preset) {
                        PRESET_CACHE.write().insert(id, json);
                    }

                    DAW_PRESETS.write().push(DawPresetEntry {
                        id,
                        name: name.clone(),
                        created_at: "Just now".to_string(),
                        fx_count: chunk_count,
                        has_snapshots: snap_count > 0,
                        is_module: false,
                    });

                    *PANEL_STATUS.write() = format!(
                        "Saved preset '{}' ({} FX, {} snapshots)",
                        name, chunk_count, snap_count
                    );
                }
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Preset save failed: {e}");
                }
            }
        });
    };

    // Diagnostic: directly test setting a param on the first FX
    let test_set_param = move |_| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "Test: No track chain available".to_string();
                return;
            };

            // Get all FX on the chain
            match chain.all().await {
                Ok(fx_list) => {
                    if fx_list.is_empty() {
                        *PANEL_STATUS.write() = "Test: No FX on track".to_string();
                        return;
                    }

                    let first_fx = &fx_list[0];
                    info!(
                        "Test: First FX = '{}' guid='{}' idx={}",
                        first_fx.plugin_name, first_fx.guid, first_fx.index
                    );

                    // Try to get the FX by GUID
                    match chain.by_guid(&first_fx.guid).await {
                        Ok(Some(handle)) => {
                            info!("Test: Found FX by GUID '{}'", first_fx.guid);

                            // Read current param 0
                            match handle.param(0).get().await {
                                Ok(current_val) => {
                                    // Toggle: if > 0.5, set to 0.1, else set to 0.9
                                    let new_val = if current_val > 0.5 { 0.1 } else { 0.9 };
                                    info!(
                                        "Test: param 0 current={:.4}, setting to {:.4}",
                                        current_val, new_val
                                    );

                                    match handle.param(0).set(new_val).await {
                                        Ok(()) => {
                                            // Read back to confirm
                                            match handle.param(0).get().await {
                                                Ok(readback) => {
                                                    *PANEL_STATUS.write() = format!(
                                                        "Test SET: {} param0 {:.3} → {:.3} (readback: {:.3})",
                                                        first_fx.plugin_name,
                                                        current_val,
                                                        new_val,
                                                        readback
                                                    );
                                                    info!(
                                                        "Test: readback={:.4} (expected {:.4})",
                                                        readback, new_val
                                                    );
                                                }
                                                Err(e) => {
                                                    *PANEL_STATUS.write() = format!(
                                                        "Test: set OK but readback failed: {e}"
                                                    );
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            warn!("Test: set param failed: {e}");
                                            *PANEL_STATUS.write() =
                                                format!("Test: set failed: {e}");
                                        }
                                    }
                                }
                                Err(e) => {
                                    warn!("Test: get param failed: {e}");
                                    *PANEL_STATUS.write() =
                                        format!("Test: get param 0 failed: {e}");
                                }
                            }
                        }
                        Ok(None) => {
                            warn!("Test: FX not found by GUID '{}'", first_fx.guid);
                            *PANEL_STATUS.write() = format!(
                                "Test: FX '{}' not found by GUID '{}'",
                                first_fx.plugin_name, first_fx.guid
                            );
                        }
                        Err(e) => {
                            warn!("Test: by_guid error: {e}");
                            *PANEL_STATUS.write() = format!("Test: by_guid error: {e}");
                        }
                    }
                }
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Test: chain.all() failed: {e}");
                }
            }
        });
    };

    // Diagnostic: directly load the first preset with verbose logging
    let load_preset_debug = move |_| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "Load Preset: No track chain available".to_string();
                return;
            };

            // Find the first available preset
            let (preset_id, preset_json) = {
                let presets = DAW_PRESETS.read();
                let cache = PRESET_CACHE.read();
                if let Some(entry) = presets.first() {
                    if let Some(json) = cache.get(&entry.id) {
                        (entry.id, json.clone())
                    } else {
                        *PANEL_STATUS.write() =
                            format!("Load Preset: '{}' not in cache", entry.name);
                        return;
                    }
                } else {
                    *PANEL_STATUS.write() = "Load Preset: No presets saved yet".to_string();
                    return;
                }
            };

            // Log the target chain state before applying
            match chain.all().await {
                Ok(fx_list) => {
                    info!("Load Preset: target chain has {} FX:", fx_list.len());
                    for fx in &fx_list {
                        info!(
                            "  Target FX: '{}' (plugin='{}') guid='{}' idx={}",
                            fx.name, fx.plugin_name, fx.guid, fx.index
                        );
                    }
                }
                Err(e) => {
                    warn!("Load Preset: failed to list target FX: {e}");
                }
            }

            // Try full preset first
            if let Ok(full_preset) = facet_json::from_str::<DawFullPreset>(&preset_json) {
                info!(
                    "Load Preset: '{}' has {} chunks, {} bundled snapshots",
                    full_preset.name,
                    full_preset.state_chunks.chunks.len(),
                    full_preset.snapshot_slots.len()
                );
                for chunk in &full_preset.state_chunks.chunks {
                    info!(
                        "  Chunk: plugin='{}' guid='{}' idx={} chunk_len={}",
                        chunk.plugin_name,
                        chunk.fx_guid,
                        chunk.fx_index,
                        chunk.encoded_chunk.len()
                    );
                }

                match daw_bridge::apply_state_chunks_cross_track(&chain, &full_preset.state_chunks)
                    .await
                {
                    Ok(guid_remap) => {
                        info!("Load Preset: GUID remap ({} entries):", guid_remap.len());
                        for (old, new) in &guid_remap {
                            info!("  {} → {}", old, new);
                        }

                        // Restore bundled snapshots
                        let snap_count = full_preset.snapshot_slots.len();
                        if snap_count > 0 {
                            let mut slots_state = DAW_SNAPSHOT_SLOTS.write();
                            let mut cache = SNAPSHOT_CACHE.write();
                            let spp = slots_state.slots_per_page;

                            for bundled_slot in &full_preset.snapshot_slots {
                                let mut snapshot = bundled_slot.snapshot.clone();
                                if !guid_remap.is_empty() {
                                    daw_bridge::remap_snapshot_guids(&mut snapshot, &guid_remap);
                                }
                                let snap_id = Uuid::new_v4();
                                if let Ok(snap_json) = facet_json::to_string(&snapshot) {
                                    cache.insert(snap_id, snap_json);
                                }
                                let page_idx = bundled_slot.slot_index / spp;
                                let slot_in_page = bundled_slot.slot_index % spp;

                                while slots_state.pages.len() <= page_idx {
                                    use crate::components::daw_snapshot_panel::DawSnapshotSlot;
                                    let new_page: Vec<DawSnapshotSlot> =
                                        (0..spp).map(DawSnapshotSlot::empty).collect();
                                    slots_state.pages.push(new_page);
                                }
                                if let Some(slot) = slots_state
                                    .pages
                                    .get_mut(page_idx)
                                    .and_then(|p| p.get_mut(slot_in_page))
                                {
                                    slot.snapshot_id = Some(snap_id);
                                    slot.name = Some(bundled_slot.name.clone());
                                }
                                info!(
                                    "  Restored snapshot '{}' → slot {}",
                                    bundled_slot.name, bundled_slot.slot_index
                                );
                            }
                        }

                        // Log post-apply chain state
                        if let Ok(fx_list) = chain.all().await {
                            info!("Load Preset: chain after apply has {} FX:", fx_list.len());
                            for fx in &fx_list {
                                info!(
                                    "  After: '{}' (plugin='{}') guid='{}' idx={}",
                                    fx.name, fx.plugin_name, fx.guid, fx.index
                                );
                            }
                        }

                        *DAW_ACTIVE_PRESET.write() = Some(preset_id);
                        *PANEL_STATUS.write() = format!(
                            "Loaded '{}' ({} FX, {} remapped, {} snaps)",
                            full_preset.name,
                            full_preset.state_chunks.chunks.len(),
                            guid_remap.len(),
                            snap_count
                        );
                    }
                    Err(e) => {
                        warn!("Load Preset: apply failed: {e}");
                        *PANEL_STATUS.write() = format!("Load Preset failed: {e}");
                    }
                }
            } else if let Ok(legacy) = facet_json::from_str::<DawStateChunkSnapshot>(&preset_json) {
                info!(
                    "Load Preset: legacy format '{}' with {} chunks",
                    legacy.name,
                    legacy.chunks.len()
                );
                match daw_bridge::apply_state_chunks_cross_track(&chain, &legacy).await {
                    Ok(guid_remap) => {
                        *DAW_ACTIVE_PRESET.write() = Some(preset_id);
                        *PANEL_STATUS.write() = format!(
                            "Loaded legacy '{}' ({} FX, {} remapped)",
                            legacy.name,
                            legacy.chunks.len(),
                            guid_remap.len()
                        );
                    }
                    Err(e) => {
                        *PANEL_STATUS.write() = format!("Load Preset failed: {e}");
                    }
                }
            } else {
                *PANEL_STATUS.write() = "Load Preset: unrecognized format".to_string();
            }
        });
    };

    // Save module preset: captures the raw RPP container chunk from the current chain.
    //
    // Flow:
    // 1. Get the raw FXCHAIN chunk text
    // 2. Look for an existing <CONTAINER block matching the given name
    // 3. If not found, enclose all FX in a container first, then re-read
    // 4. Store the container block as chunk_text in DawModulePreset
    let save_module_preset = move |container_name: String| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "No track available for module capture".to_string();
                return;
            };

            let upper_name = container_name.trim().to_uppercase();
            if upper_name.is_empty() {
                *PANEL_STATUS.write() = "Module name cannot be empty".to_string();
                return;
            }

            // Step 1: Check current FX tree for a container with this name.
            // RPP container tags are always `<CONTAINER Container` (generic) —
            // the display name is stored via `renamed_name` config param, NOT
            // in the opening tag text. So we use the FX tree API to identify
            // containers by name, then extract the block by position.
            let tree = match chain.tree().await {
                Ok(t) => t,
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Failed to read FX tree: {e}");
                    return;
                }
            };

            if tree.nodes.is_empty() {
                *PANEL_STATUS.write() = "No FX on track to save as module".to_string();
                return;
            }

            let has_matching_container = tree
                .nodes
                .iter()
                .any(|n| n.is_container() && n.display_name().to_uppercase() == upper_name);

            if !has_matching_container {
                // Enclose all top-level FX in a new container with this name
                info!(
                    "No container '{}' found — enclosing {} FX in container",
                    upper_name,
                    tree.nodes.len()
                );
                let node_ids: Vec<_> = tree.nodes.iter().map(|n| n.id.clone()).collect();
                match chain.enclose_in_container(&node_ids, &upper_name).await {
                    Ok(_container_id) => {
                        info!(
                            "Enclosed {} FX in container '{}'",
                            node_ids.len(),
                            upper_name
                        );
                    }
                    Err(e) => {
                        *PANEL_STATUS.write() = format!("Failed to create container: {e}");
                        return;
                    }
                }
            }

            // Step 2: Read the FXCHAIN chunk text and extract the container block.
            // We search for any `<CONTAINER Container` tag since the display name
            // isn't part of the RPP tag syntax.
            let fxchain_text = match chain.fx_chain_chunk_text().await {
                Ok(text) => text,
                Err(e) => {
                    *PANEL_STATUS.write() = format!("Module capture failed: {e}");
                    return;
                }
            };

            info!(
                "FXCHAIN chunk ({} bytes), first 500 chars:\n{}",
                fxchain_text.len(),
                &fxchain_text[..fxchain_text.len().min(500)]
            );

            let final_chunk_text =
                match extract_container_block(&fxchain_text, "<CONTAINER Container") {
                    Some(block) => {
                        info!("Extracted container block ({} bytes)", block.len());
                        block
                    }
                    None => {
                        *PANEL_STATUS.write() =
                            "Failed to find <CONTAINER block in FXCHAIN chunk".to_string();
                        return;
                    }
                };

            let id = Uuid::new_v4();

            // Bundle current snapshot slots
            let snapshot_slots = {
                let slots_state = DAW_SNAPSHOT_SLOTS.read();
                let cache = SNAPSHOT_CACHE.read();
                let mut bundled = Vec::new();
                for (page_idx, page) in slots_state.pages.iter().enumerate() {
                    for (slot_idx, slot) in page.iter().enumerate() {
                        if let (Some(snap_id), Some(slot_name)) =
                            (slot.snapshot_id, slot.name.as_ref())
                        {
                            if let Some(json) = cache.get(&snap_id) {
                                if let Ok(snapshot) =
                                    facet_json::from_str::<DawParameterSnapshot>(json)
                                {
                                    bundled.push(DawFullPresetSlot {
                                        slot_index: page_idx * slots_state.slots_per_page
                                            + slot_idx,
                                        name: slot_name.clone(),
                                        snapshot,
                                    });
                                }
                            }
                        }
                    }
                }
                bundled
            };

            let snap_count = snapshot_slots.len();
            let chunk_len = final_chunk_text.len();
            let module_preset = DawModulePreset {
                container_name: upper_name.clone(),
                chunk_text: final_chunk_text,
                snapshot_slots,
            };

            if let Ok(json) = facet_json::to_string(&module_preset) {
                PRESET_CACHE.write().insert(id, json);
            }

            DAW_PRESETS.write().push(DawPresetEntry {
                id,
                name: format!("{} Module", upper_name),
                created_at: "Just now".to_string(),
                fx_count: 0, // chunk-based, not individual FX count
                has_snapshots: snap_count > 0,
                is_module: true,
            });

            info!(
                "Saved module preset '{}' ({} bytes chunk, {} snapshots)",
                upper_name, chunk_len, snap_count
            );
            *PANEL_STATUS.write() = format!(
                "Saved module '{}' ({} bytes, {} snapshots)",
                upper_name, chunk_len, snap_count
            );
        });
    };

    // Diagnostic: load the first module preset with verbose logging
    let load_module_preset_debug = move |_| {
        spawn(async move {
            let Some(chain) = get_tracked_fx_chain().await else {
                *PANEL_STATUS.write() = "Load Module: No track chain available".to_string();
                return;
            };

            // Find the first module preset
            let (preset_id, preset_json) = {
                let presets = DAW_PRESETS.read();
                let cache = PRESET_CACHE.read();
                if let Some(entry) = presets.iter().find(|p| p.is_module) {
                    if let Some(json) = cache.get(&entry.id) {
                        (entry.id, json.clone())
                    } else {
                        *PANEL_STATUS.write() =
                            format!("Load Module: '{}' not in cache", entry.name);
                        return;
                    }
                } else {
                    *PANEL_STATUS.write() = "Load Module: No module presets saved yet".to_string();
                    return;
                }
            };

            // Log target chain state
            match chain.all().await {
                Ok(fx_list) => {
                    info!("Load Module: target chain has {} FX:", fx_list.len());
                    for fx in &fx_list {
                        info!(
                            "  Target FX: '{}' (plugin='{}') guid='{}' idx={}",
                            fx.name, fx.plugin_name, fx.guid, fx.index
                        );
                    }
                }
                Err(e) => {
                    warn!("Load Module: failed to list target FX: {e}");
                }
            }

            match facet_json::from_str::<DawModulePreset>(&preset_json) {
                Ok(module_preset) => {
                    info!(
                        "Load Module: '{}' chunk={} bytes, {} bundled snapshots",
                        module_preset.container_name,
                        module_preset.chunk_text.len(),
                        module_preset.snapshot_slots.len()
                    );

                    match daw_bridge::apply_module_preset(&chain, &module_preset).await {
                        Ok(()) => {
                            // Restore bundled snapshots (note: GUIDs from the chunk
                            // will be new, so snapshot GUID remapping is a future concern)
                            let snap_count = module_preset.snapshot_slots.len();
                            if snap_count > 0 {
                                let mut slots_state = DAW_SNAPSHOT_SLOTS.write();
                                let mut cache = SNAPSHOT_CACHE.write();
                                let spp = slots_state.slots_per_page;

                                for bundled_slot in &module_preset.snapshot_slots {
                                    let snapshot = bundled_slot.snapshot.clone();
                                    let snap_id = Uuid::new_v4();
                                    if let Ok(snap_json) = facet_json::to_string(&snapshot) {
                                        cache.insert(snap_id, snap_json);
                                    }
                                    let page_idx = bundled_slot.slot_index / spp;
                                    let slot_in_page = bundled_slot.slot_index % spp;

                                    while slots_state.pages.len() <= page_idx {
                                        use crate::components::daw_snapshot_panel::DawSnapshotSlot;
                                        let new_page: Vec<DawSnapshotSlot> =
                                            (0..spp).map(DawSnapshotSlot::empty).collect();
                                        slots_state.pages.push(new_page);
                                    }
                                    if let Some(slot) = slots_state
                                        .pages
                                        .get_mut(page_idx)
                                        .and_then(|p| p.get_mut(slot_in_page))
                                    {
                                        slot.snapshot_id = Some(snap_id);
                                        slot.name = Some(bundled_slot.name.clone());
                                    }
                                    info!(
                                        "  Restored snapshot '{}' → slot {}",
                                        bundled_slot.name, bundled_slot.slot_index
                                    );
                                }
                            }

                            // Log post-apply chain state
                            if let Ok(fx_list) = chain.all().await {
                                info!("Load Module: chain after apply has {} FX:", fx_list.len());
                                for fx in &fx_list {
                                    info!(
                                        "  After: '{}' (plugin='{}') guid='{}' idx={}",
                                        fx.name, fx.plugin_name, fx.guid, fx.index
                                    );
                                }
                            }

                            *DAW_ACTIVE_PRESET.write() = Some(preset_id);
                            *PANEL_STATUS.write() = format!(
                                "Loaded module '{}' ({} bytes chunk, {} snaps)",
                                module_preset.container_name,
                                module_preset.chunk_text.len(),
                                snap_count
                            );
                        }
                        Err(e) => {
                            warn!("Load Module: apply failed: {e}");
                            *PANEL_STATUS.write() = format!("Load Module failed: {e}");
                        }
                    }
                }
                Err(e) => {
                    warn!("Load Module: deserialize failed: {e}");
                    *PANEL_STATUS.write() = format!("Load Module: deserialize error: {e}");
                }
            }
        });
    };

    let clear_all = move |_| {
        *DAW_SNAPSHOT_SLOTS.write() = Default::default();
        DAW_PRESETS.write().clear();
        *DAW_ACTIVE_PRESET.write() = None;
        SNAPSHOT_CACHE.write().clear();
        PRESET_CACHE.write().clear();
        *MORPH_POSITION.write() = 0.0;
        *MORPH_EASING.write() = EasingCurve::Linear;
        *MORPH_SNAP_A.write() = None;
        *MORPH_SNAP_B.write() = None;
        *MORPH_NAME_A.write() = None;
        *MORPH_NAME_B.write() = None;
        MORPH_DIFF.write().clear();
        LIVE_PARAM_VALUES.write().clear();
        *PANEL_STATUS.write() = "Cleared all snapshots and presets".to_string();
    };

    // ── Render ───────────────────────────────────────────────────

    let btn_disabled = !connected || !track_available;
    let btn_class_snapshot = if btn_disabled {
        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
    } else {
        "px-2 py-1 rounded text-[10px] font-medium bg-blue-900/40 text-blue-300 border border-blue-800/40 hover:bg-blue-800/50 transition-colors"
    };
    let btn_class_preset = if btn_disabled {
        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
    } else {
        "px-2 py-1 rounded text-[10px] font-medium bg-green-900/40 text-green-300 border border-green-800/40 hover:bg-green-800/50 transition-colors"
    };

    let diff_count = morph_diffs.len();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // ── Top bar ───────────────────────────────────────────
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/50 flex-shrink-0",
                if let Some(ref name) = track_name {
                    {
                        rsx! {
                            span { class: "text-[11px] font-medium text-zinc-100",
                                "Track: {name}"
                            }
                            span { class: "text-[10px] text-zinc-500 ml-1",
                                "({fx_count} FX)"
                            }
                        }
                    }
                } else if connected {
                    span { class: "text-[11px] text-zinc-400 italic", "No track selected in REAPER" }
                } else {
                    span { class: "text-[11px] text-zinc-500 italic", "Waiting for DAW connection..." }
                }

                div { class: "w-px h-4 bg-zinc-700 mx-1" }

                button {
                    class: btn_class_snapshot,
                    disabled: btn_disabled,
                    onclick: capture_snapshot,
                    "Capture Snapshot"
                }
                button {
                    class: btn_class_preset,
                    disabled: btn_disabled,
                    onclick: save_preset,
                    "Save Preset"
                }

                div { class: "w-px h-4 bg-zinc-700 mx-1" }

                button {
                    class: if btn_disabled {
                        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
                    } else {
                        "px-2 py-1 rounded text-[10px] font-medium bg-yellow-900/40 text-yellow-300 border border-yellow-800/40 hover:bg-yellow-800/50 transition-colors"
                    },
                    disabled: btn_disabled,
                    onclick: test_set_param,
                    "Test Set Param"
                }

                button {
                    class: if btn_disabled {
                        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
                    } else {
                        "px-2 py-1 rounded text-[10px] font-medium bg-purple-900/40 text-purple-300 border border-purple-800/40 hover:bg-purple-800/50 transition-colors"
                    },
                    disabled: btn_disabled,
                    onclick: load_preset_debug,
                    "Load Preset"
                }

                div { class: "w-px h-4 bg-zinc-700 mx-1" }

                button {
                    class: if btn_disabled {
                        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
                    } else {
                        "px-2 py-1 rounded text-[10px] font-medium bg-teal-900/40 text-teal-300 border border-teal-800/40 hover:bg-teal-800/50 transition-colors"
                    },
                    disabled: btn_disabled,
                    onclick: move |_| {
                        *module_save_name.write() = String::new();
                        *show_module_save_dialog.write() = true;
                    },
                    "Save Module"
                }
                button {
                    class: if btn_disabled {
                        "px-2 py-1 rounded text-[10px] font-medium bg-zinc-800 text-zinc-500 border border-zinc-700 cursor-not-allowed"
                    } else {
                        "px-2 py-1 rounded text-[10px] font-medium bg-teal-900/40 text-teal-300 border border-teal-800/40 hover:bg-teal-800/50 transition-colors"
                    },
                    disabled: btn_disabled,
                    onclick: load_module_preset_debug,
                    "Load Module"
                }

                button {
                    class: "px-2 py-1 rounded text-[10px] font-medium text-red-400 \
                            border border-red-800/40 hover:bg-red-900/30 transition-colors",
                    onclick: clear_all,
                    "Clear All"
                }

                div { class: "flex-1" }
                div { class: "flex items-center gap-1.5",
                    span { class: "text-[10px] text-zinc-500", "Status:" }
                    span { class: "text-[10px] text-zinc-300 font-mono max-w-[400px] truncate",
                        "{status}"
                    }
                }
            }

            // ── Module save dialog (inline) ──────────────────────
            if show_module_save_dialog() {
                div { class: "border-b border-border bg-zinc-900/60 px-3 py-2 flex-shrink-0",
                    div { class: "flex items-center gap-2 mb-2",
                        span { class: "text-[10px] font-semibold text-teal-400 uppercase tracking-wider",
                            "Save Module Preset"
                        }
                    }
                    // Name input
                    div { class: "flex items-center gap-2 mb-2",
                        span { class: "text-[10px] text-zinc-400 w-16", "Name:" }
                        input {
                            class: "flex-1 text-xs bg-zinc-800 text-zinc-200 rounded px-2 py-1.5 \
                                    border border-zinc-700 outline-none focus:border-teal-500 uppercase",
                            placeholder: "e.g. TIME, DRIVE, MODULATION...",
                            autofocus: true,
                            value: "{module_save_name}",
                            oninput: move |evt| { *module_save_name.write() = evt.value(); },
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    let name = module_save_name().trim().to_string();
                                    if !name.is_empty() {
                                        save_module_preset(name);
                                        *show_module_save_dialog.write() = false;
                                    }
                                } else if evt.key() == Key::Escape {
                                    *show_module_save_dialog.write() = false;
                                }
                            },
                        }
                        button {
                            class: "px-2 py-1.5 rounded text-[10px] font-medium bg-teal-900/40 \
                                    text-teal-400 border border-teal-800/40 hover:bg-teal-800/50 \
                                    transition-colors disabled:opacity-30",
                            disabled: module_save_name().trim().is_empty(),
                            onclick: move |_| {
                                let name = module_save_name().trim().to_string();
                                if !name.is_empty() {
                                    save_module_preset(name);
                                    *show_module_save_dialog.write() = false;
                                }
                            },
                            "Save"
                        }
                        button {
                            class: "px-2 py-1.5 rounded text-[10px] font-medium text-zinc-400 \
                                    hover:text-zinc-200 transition-colors",
                            onclick: move |_| { *show_module_save_dialog.write() = false; },
                            "Cancel"
                        }
                    }
                    // FX list preview
                    div { class: "flex gap-4",
                        div { class: "flex-1",
                            span { class: "text-[9px] text-zinc-500 uppercase tracking-wider block mb-1",
                                "FX to capture ({fx_count}):"
                            }
                            if fx_names.is_empty() {
                                span { class: "text-[10px] text-zinc-600 italic", "No FX on track" }
                            } else {
                                div { class: "flex flex-wrap gap-1",
                                    for name in fx_names.iter() {
                                        span {
                                            class: "text-[9px] px-1.5 py-0.5 rounded bg-zinc-800 text-zinc-300 border border-zinc-700",
                                            "{name}"
                                        }
                                    }
                                }
                            }
                        }
                        div {
                            span { class: "text-[9px] text-zinc-500 uppercase tracking-wider block mb-1",
                                "Snapshots:"
                            }
                            if filled_snapshot_count > 0 {
                                span { class: "text-[10px] text-blue-400",
                                    "{filled_snapshot_count} will be bundled"
                                }
                            } else {
                                span { class: "text-[10px] text-zinc-600 italic", "None" }
                            }
                        }
                    }
                }
            }

            // ── Main body ─────────────────────────────────────────
            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ── Left: FX Chain Tree (reuses daw-ui component) ──
                div { class: "w-80 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/30",
                    // The FxChainTree component handles its own polling, track
                    // following, container names, routing badges, collapse state,
                    // and context menus (enclose, explode, rename, delete).
                    daw_ui::FxChainTree {}
                }

                // ── Center: Snapshot slots ────────────────────────
                div { class: "flex-1 min-h-0 min-w-0 border-r border-border overflow-hidden",
                    DawSnapshotPanel {
                        on_save: snap_on_save,
                        on_recall: snap_on_recall,
                        on_morph: snap_on_morph,
                        on_delete: snap_on_delete,
                    }
                }

                // ── Right: Presets ────────────────────────────────
                div { class: "flex-1 min-h-0 min-w-0 overflow-hidden",
                    DawPresetPanel {
                        on_save: preset_on_save,
                        on_recall: preset_on_recall,
                        on_rename: preset_on_rename,
                        on_delete: preset_on_delete,
                    }
                }
            }
        }
    }
}

/// Extract a `<CONTAINER ...>...</CONTAINER>` block from RPP chunk text.
///
/// Finds the first occurrence of `tag_prefix` (e.g., `"<CONTAINER Container"`)
/// and extracts the complete block including all nested tags. RPP uses `<TAG`
/// to open blocks and a standalone `>` at line-start to close them, so we track
/// nesting depth by counting `<` at line-starts vs `>` at line-starts.
fn extract_container_block(chunk_text: &str, tag_prefix: &str) -> Option<String> {
    // Find the start of the tag
    let start_idx = chunk_text.find(tag_prefix)?;
    let region = &chunk_text[start_idx..];

    // RPP nesting: each line starting with `<` opens a block, each line that
    // is just `>` (possibly with whitespace) closes a block. We can't just
    // count raw `<` and `>` characters because `>` appears in base64 data
    // and attribute values. Instead, track line-by-line.
    let mut depth = 0i32;
    let mut end_byte = 0;
    let mut found_end = false;

    for line in region.lines() {
        end_byte += line.len() + 1; // +1 for newline

        let trimmed = line.trim();
        if trimmed.starts_with('<') {
            depth += 1;
        }
        if trimmed == ">" {
            depth -= 1;
            if depth == 0 {
                found_end = true;
                break;
            }
        }
    }

    if found_end {
        // end_byte might overshoot the actual region length if the last line
        // has no trailing newline
        let result = &region[..end_byte.min(region.len())];
        Some(result.trim_end().to_string())
    } else {
        None
    }
}

/// Simple timestamp stub for auto-naming.
fn chrono_stub() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    format!("{}", secs % 10000)
}
