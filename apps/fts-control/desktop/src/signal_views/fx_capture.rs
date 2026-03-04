//! FX Chain Scan → Block Preset Capture
//!
//! Dedicated "Capture" tab that bridges the REAPER FX chain (daw-control) with
//! the signal domain model (Block / Preset / Snapshot).
//!
//! **Reactive**: Track list polls every 2s (no track subscription API exists yet).
//! FX chain subscribes to `fx_chain.subscribe_events()` for real-time updates
//! when plugins are added, removed, enabled, or reordered.
//!
//! **Deduplication**: When an FX is selected, the library is searched for an
//! existing preset with a matching `source:` tag (the raw REAPER plugin name).
//! If found, the existing preset is shown instead of a new-capture form.

use dioxus::prelude::*;
use daw_control::FxParameter;
use signal::block::BlockType;
use signal_proto::easing::EasingCurve;
use signal_proto::macro_bank::{GroupSelector, MacroBank, MacroGroup, MacroKnob};
use signal_proto::{MacroBinding, ResponseCurve};
use signal_proto::metadata::Metadata;
use signal_proto::param_curation::ParamCuration;
use signal_ui::components::{Dialog, DialogClose, DialogFooter, DialogHeader, DialogTitle};
use signal_ui::views::MiniKnob;
use tracing::warn;

use crate::daw_registry::{self, register_block_fx};

// ============================================================================
// Types
// ============================================================================

/// A track discovered in the REAPER project.
#[derive(Clone, Debug, PartialEq)]
struct TrackInfo {
    guid: String,
    name: String,
    index: u32,
    fx_count: u32,
}

/// A plugin discovered on a REAPER FX chain.
#[derive(Clone, Debug, PartialEq)]
struct ScannedFx {
    guid: String,
    name: String,
    plugin_name: String,
    chain_index: u32,
    enabled: bool,
    parameter_count: u32,
    preset_name: Option<String>,
}

/// Flat item for the block library list.
#[derive(Clone, Debug, PartialEq)]
struct LibraryItem {
    preset_id: String,
    name: String,
    block_type: BlockType,
    snapshot_count: usize,
    source_tag: Option<String>,
    /// True if this preset is a seed/template (no `source:` tag).
    /// Templates haven't been captured from a real REAPER plugin yet.
    is_template: bool,
    /// Vendor extracted from `vendor:` tag, e.g. `"Neural DSP"`.
    vendor: Option<String>,
    /// Plugin format from `plugin:` tag, e.g. `"vst3"`.
    format: Option<String>,
    /// All raw tags for search matching.
    tags: Vec<String>,
    /// Default snapshot id for loading.
    default_snapshot_id: String,
    /// All snapshot (id, name) pairs for the snapshot picker.
    snapshots: Vec<(String, String)>,
}

impl LibraryItem {
    /// Extract the raw REAPER plugin name from the `source:` tag.
    /// Returns `None` for template blocks (no source tag).
    fn raw_plugin_name(&self) -> Option<&str> {
        self.source_tag.as_deref()?.strip_prefix("source:")
    }
}

// ============================================================================
// Async helpers
// ============================================================================

/// Fetch all tracks from the Signal DAW's current project.
async fn fetch_tracks() -> Option<Vec<TrackInfo>> {
    let daw = daw_registry::signal_daw()?;
    let project = daw.current_project().await.ok()?;
    let tracks = project.tracks().all().await.ok()?;

    let mut infos = Vec::with_capacity(tracks.len());
    for (i, t) in tracks.iter().enumerate() {
        let fx_count = if let Ok(Some(handle)) = project.tracks().by_guid(&t.guid).await {
            handle.fx_chain().count().await.unwrap_or(0)
        } else {
            0
        };
        infos.push(TrackInfo {
            guid: t.guid.clone(),
            name: if t.name.is_empty() {
                format!("Track {}", i + 1)
            } else {
                t.name.clone()
            },
            index: i as u32,
            fx_count,
        });
    }
    Some(infos)
}

/// Fetch the FX chain for a specific track.
async fn fetch_track_fx(track_guid: &str) -> Option<Vec<ScannedFx>> {
    let daw = daw_registry::signal_daw()?;
    let project = daw.current_project().await.ok()?;
    let track = project.tracks().by_guid(track_guid).await.ok()??;
    let fxs = track.fx_chain().all().await.ok()?;

    Some(
        fxs.into_iter()
            .map(|fx| ScannedFx {
                guid: fx.guid,
                name: fx.name,
                plugin_name: fx.plugin_name,
                chain_index: fx.index,
                enabled: fx.enabled,
                parameter_count: fx.parameter_count,
                preset_name: fx.preset_name,
            })
            .collect(),
    )
}

/// Capture a single FX's parameters as a Block + optional binary state.
async fn capture_fx_as_block(
    track_guid: &str,
    fx_guid: &str,
) -> eyre::Result<(signal::Block, Option<Vec<u8>>)> {
    let daw = daw_registry::signal_daw().ok_or_else(|| eyre::eyre!("no signal DAW connected"))?;
    let project = daw.current_project().await?;
    let track = project
        .tracks()
        .by_guid(track_guid)
        .await?
        .ok_or_else(|| eyre::eyre!("track not found: {track_guid}"))?;
    let fx = track
        .fx_chain()
        .by_guid(fx_guid)
        .await?
        .ok_or_else(|| eyre::eyre!("FX not found: {fx_guid}"))?;

    let params = fx.parameters().await?;
    let block_params: Vec<signal::BlockParameter> = params
        .iter()
        .map(|p| {
            let id = p.name.to_lowercase().replace([' ', '/', '\\'], "-");
            signal::BlockParameter::new(id, p.name.clone(), p.value as f32)
        })
        .collect();
    let block = signal::Block::from_parameters(block_params);
    let state_data = fx.state_chunk().await.ok().flatten();

    Ok((block, state_data))
}

/// Push a Block's parameters to a REAPER FX plugin.
async fn apply_block_to_fx(
    fx_guid: &str,
    block: &signal::Block,
    state_data: Option<&[u8]>,
) -> eyre::Result<()> {
    let daw = daw_registry::signal_daw().ok_or_else(|| eyre::eyre!("no signal DAW connected"))?;
    let project = daw.current_project().await?;

    let tracks = project.tracks().all().await?;
    let mut fx_handle = None;
    for t in &tracks {
        if let Ok(Some(handle)) = project.tracks().by_guid(&t.guid).await {
            if let Ok(Some(fh)) = handle.fx_chain().by_guid(fx_guid).await {
                fx_handle = Some(fh);
                break;
            }
        }
    }
    let fx = fx_handle.ok_or_else(|| eyre::eyre!("FX not found: {fx_guid}"))?;

    if let Some(data) = state_data {
        fx.set_state_chunk(data.to_vec()).await?;
    } else {
        for param in block.parameters() {
            fx.param_by_name(param.name())
                .set(param.value().get() as f64)
                .await?;
        }
    }

    Ok(())
}

/// Insert a plugin on a track and apply a block preset's parameters to it.
/// Returns the new FX's GUID on success.
async fn add_block_to_track(
    track_guid: &str,
    raw_plugin_name: &str,
    block: &signal::Block,
    state_data: Option<&[u8]>,
) -> eyre::Result<String> {
    let daw = daw_registry::signal_daw().ok_or_else(|| eyre::eyre!("no signal DAW connected"))?;
    let project = daw.current_project().await?;
    let track = project
        .tracks()
        .by_guid(track_guid)
        .await?
        .ok_or_else(|| eyre::eyre!("track not found: {track_guid}"))?;
    let fx = track.fx_chain().add(raw_plugin_name).await?;

    if let Some(data) = state_data {
        fx.set_state_chunk(data.to_vec()).await?;
    } else {
        for param in block.parameters() {
            fx.param_by_name(param.name())
                .set(param.value().get() as f64)
                .await?;
        }
    }

    Ok(fx.guid().to_string())
}

/// Fetch all parameters for a specific FX on a track.
async fn fetch_fx_params(track_guid: &str, fx_guid: &str) -> Option<Vec<FxParameter>> {
    let daw = daw_registry::signal_daw()?;
    let project = daw.current_project().await.ok()?;
    let track = project.tracks().by_guid(track_guid).await.ok()??;
    let fx = track.fx_chain().by_guid(fx_guid).await.ok()??;
    fx.parameters().await.ok()
}

/// Set a single parameter on a REAPER FX by index.
async fn set_fx_param(track_guid: &str, fx_guid: &str, index: u32, value: f64) -> eyre::Result<()> {
    let daw = daw_registry::signal_daw().ok_or_else(|| eyre::eyre!("no signal DAW connected"))?;
    let project = daw.current_project().await?;
    let track = project
        .tracks()
        .by_guid(track_guid)
        .await?
        .ok_or_else(|| eyre::eyre!("track not found"))?;
    let fx = track
        .fx_chain()
        .by_guid(fx_guid)
        .await?
        .ok_or_else(|| eyre::eyre!("FX not found"))?;
    fx.param(index).set(value).await?;
    Ok(())
}

/// Find the REAPER FX parameter index for a block parameter slug ID.
///
/// Block parameters use slugified IDs (e.g. `"plugin-gain"` from `"Plugin Gain"`).
/// This matches them back to REAPER's runtime FX parameters by applying the same
/// slug transformation to FxParameter names.
fn find_fx_index(fx_params: &[FxParameter], block_param_id: &str) -> Option<u32> {
    fx_params
        .iter()
        .find(|p| p.name.to_lowercase().replace([' ', '/', '\\'], "-") == block_param_id)
        .map(|p| p.index)
}

/// Capture live REAPER state as a new snapshot on an existing preset.
#[allow(dead_code)] // Phase 3 — wired to "Capture Live Snapshot" action
pub async fn capture_live_snapshot(
    fx_guid: &str,
    existing_block: &signal::Block,
) -> eyre::Result<(signal::Block, Option<Vec<u8>>)> {
    let daw = daw_registry::signal_daw().ok_or_else(|| eyre::eyre!("no signal DAW connected"))?;
    let project = daw.current_project().await?;

    let tracks = project.tracks().all().await?;
    let mut fx_handle = None;
    for t in &tracks {
        if let Ok(Some(handle)) = project.tracks().by_guid(&t.guid).await {
            if let Ok(Some(fh)) = handle.fx_chain().by_guid(fx_guid).await {
                fx_handle = Some(fh);
                break;
            }
        }
    }
    let fx = fx_handle.ok_or_else(|| eyre::eyre!("FX not found: {fx_guid}"))?;

    let params = fx.parameters().await?;
    let live_params: Vec<signal::LiveParam> = params
        .into_iter()
        .map(|p| signal::LiveParam {
            index: p.index,
            name: p.name,
            value: p.value,
        })
        .collect();

    let updated_block = signal::live_params_into_block(existing_block.clone(), &live_params);
    let state_data = fx.state_chunk().await.ok().flatten();

    Ok((updated_block, state_data))
}

/// Fetch all block presets across every BlockType as flat library items.
async fn fetch_library(signal: &signal::Signal) -> Vec<LibraryItem> {
    let mut items = Vec::new();
    for &bt in signal::block::ALL_BLOCK_TYPES {
        if let Ok(presets) = signal.block_presets().list(bt).await {
            for p in presets {
                let all_tags: Vec<String> = p.metadata().tags.as_slice().to_vec();
                let source_tag = all_tags.iter().find(|t| t.starts_with("source:")).cloned();
                let vendor = all_tags.iter().find_map(|t| {
                    t.strip_prefix("vendor:").map(|v| v.to_string())
                });
                let format = all_tags.iter().find_map(|t| {
                    t.strip_prefix("plugin:").map(|v| v.to_string())
                });
                let is_template = source_tag.is_none();
                let default_snapshot_id = p.default_snapshot().id().to_string();
                let snapshots: Vec<(String, String)> = p
                    .snapshots()
                    .iter()
                    .map(|s| (s.id().to_string(), s.name().to_string()))
                    .collect();
                items.push(LibraryItem {
                    preset_id: p.id().to_string(),
                    name: p.name().to_string(),
                    block_type: bt,
                    snapshot_count: p.snapshots().len(),
                    source_tag,
                    is_template,
                    vendor,
                    format,
                    tags: all_tags,
                    default_snapshot_id,
                    snapshots,
                });
            }
        }
    }
    items
}

// ============================================================================
// Tag helpers
// ============================================================================

/// Parsed components from a REAPER plugin name.
///
/// REAPER reports names like `"VST3: BigSky (Strymon)"`, `"AU: Pro-Q 3 (FabFilter)"`,
/// `"VST3: Archetype Cory Wong (Neural DSP)"`, `"JS: utility/volume"`.
struct ParsedPluginName {
    /// Clean product name, e.g. `"BigSky"`, `"Pro-Q 3"`, `"Archetype Cory Wong"`
    clean_name: String,
    /// Vendor extracted from trailing parentheses, e.g. `"Strymon"`, `"Neural DSP"`
    vendor: Option<String>,
    /// Plugin format tag, e.g. `"plugin:vst3"`
    format_tag: Option<&'static str>,
}

/// Parse a REAPER plugin name into clean name, vendor, and format.
fn parse_plugin_name(raw: &str) -> ParsedPluginName {
    let lower = raw.to_lowercase();

    // 1. Detect and strip format prefix
    let (format_tag, after_prefix) = if lower.starts_with("vst3:") || lower.starts_with("vst3i:") {
        (Some("plugin:vst3"), raw.split_once(':').map(|(_, r)| r.trim()).unwrap_or(raw))
    } else if lower.starts_with("vst:") || lower.starts_with("vsti:") {
        (Some("plugin:vst2"), raw.split_once(':').map(|(_, r)| r.trim()).unwrap_or(raw))
    } else if lower.starts_with("au:") || lower.starts_with("aui:") {
        (Some("plugin:au"), raw.split_once(':').map(|(_, r)| r.trim()).unwrap_or(raw))
    } else if lower.starts_with("clap:") {
        (Some("plugin:clap"), raw.split_once(':').map(|(_, r)| r.trim()).unwrap_or(raw))
    } else if lower.starts_with("js:") {
        (Some("plugin:jsfx"), raw.split_once(':').map(|(_, r)| r.trim()).unwrap_or(raw))
    } else {
        (None, raw)
    };

    // 2. Extract vendor from trailing parentheses: "BigSky (Strymon)" → ("BigSky", "Strymon")
    let (clean_name, vendor) = if let Some(paren_start) = after_prefix.rfind('(') {
        let before = after_prefix[..paren_start].trim();
        let inside = after_prefix[paren_start + 1..]
            .trim_end_matches(')')
            .trim();
        if !inside.is_empty() && !before.is_empty() {
            (before.to_string(), Some(inside.to_string()))
        } else {
            (after_prefix.trim().to_string(), None)
        }
    } else {
        (after_prefix.trim().to_string(), None)
    };

    ParsedPluginName {
        clean_name,
        vendor,
        format_tag,
    }
}

/// Build the source tag from a raw REAPER plugin name.
/// Used both at capture time and for dedup matching.
fn source_tag(raw_plugin_name: &str) -> String {
    format!("source:{raw_plugin_name}")
}

/// Build metadata tags from the capture form state.
fn build_capture_metadata(
    raw_plugin_name: &str,
    parsed: &ParsedPluginName,
    is_paid: bool,
    requires_activation: bool,
    mac: bool,
    windows: bool,
    linux: bool,
    linux_yabridge: bool,
) -> Metadata {
    let mut meta = Metadata::new();

    // Source identity — the raw REAPER plugin name for dedup matching
    meta = meta.with_tag(source_tag(raw_plugin_name));

    // Pricing
    meta = meta.with_tag(if is_paid { "workflow:paid" } else { "workflow:free" });

    // Activation requirement
    meta = meta.with_tag(if requires_activation {
        "workflow:licensed"
    } else {
        "workflow:portable"
    });

    // Plugin format (auto-inferred from name prefix)
    if let Some(format_tag) = parsed.format_tag {
        meta = meta.with_tag(format_tag);
    }

    // Vendor (extracted from trailing parentheses)
    if let Some(ref vendor) = parsed.vendor {
        meta = meta.with_tag(format!("vendor:{vendor}"));
    }

    // Platform compatibility
    if mac {
        meta = meta.with_tag("custom:mac");
    }
    if windows {
        meta = meta.with_tag("custom:windows");
    }
    if linux {
        meta = meta.with_tag("custom:linux");
    }
    if linux_yabridge {
        meta = meta.with_tag("custom:linux-yabridge");
    }

    meta
}

// ============================================================================
// SignalCaptureTab — reactive Dioxus component
// ============================================================================

/// Generation counter for the FX subscription task. Bumped when the selected
/// track changes so the previous subscription loop exits.
static FX_SUB_GEN: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

// ============================================================================
// Macro Knob Editor Dialog
// ============================================================================

/// Unified macro editor dialog — one dialog for ALL macros with group support.
///
/// Shows a knob row at the top (shared + active-group knobs), a selected-knob
/// detail panel below with assignment cards (min/max/curve), and group selector
/// bar when macro groups are configured.
#[component]
fn MacroEditorDialog(
    block: signal::Block,
    fx_params: Signal<Vec<FxParameter>>,
    track_guid: String,
    fx_guid: String,
    /// Which knob to pre-select when the dialog opens (e.g. from pencil button).
    #[props(default)]
    initial_knob_id: Option<String>,
    on_save: EventHandler<signal::Block>,
    on_close: EventHandler<()>,
) -> Element {
    // Local clone for in-dialog editing — only persisted on "Done"
    let mut local_block = use_signal(|| block.clone());
    let mut selected_knob_id: Signal<Option<String>> = use_signal(|| initial_knob_id.clone());
    let mut show_add_picker = use_signal(|| false);

    // Derive bank data from local block
    let bank_data = use_memo(move || {
        local_block()
            .macro_bank
            .clone()
            .unwrap_or_default()
    });

    let bank = bank_data();
    let has_groups = bank.has_groups();
    let selector_knob_id = bank.group_selector.as_ref().map(|s| s.knob_id.clone());
    let active_group = bank.active_group();
    let active_group_id = active_group.map(|g| g.id.clone());
    let active_group_color = active_group.map(|g| g.color.clone());

    // Visible knobs: shared + active group's knobs
    let visible_knobs: Vec<MacroKnob> = bank.visible_knobs().into_iter().cloned().collect();
    let shared_count = bank.knobs.len();

    // Selected knob data — search across shared + all groups
    let selected_knob = selected_knob_id().and_then(|id| bank.get_knob(&id).cloned());

    // Unbound parameters for "Add Assignment" picker
    let unbound_params: Vec<(String, String)> = {
        let bound_ids: Vec<String> = selected_knob.as_ref()
            .map(|k| k.bindings.iter().map(|b| b.target.param_id.clone()).collect())
            .unwrap_or_default();
        local_block()
            .parameters()
            .iter()
            .filter(|p| !bound_ids.contains(&p.id().to_string()))
            .map(|p| (p.id().to_string(), p.name().to_string()))
            .collect()
    };

    // Determine which group a knob belongs to (None = shared)
    let knob_group_id = |knob_id: &str| -> Option<String> {
        for group in &bank.groups {
            if group.knobs.iter().any(|k| k.id == knob_id) {
                return Some(group.id.clone());
            }
        }
        None
    };

    rsx! {
        Dialog {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "!max-w-none !w-[90vw] !h-[90vh] overflow-y-auto",

            DialogHeader {
                DialogTitle { "Macro Editor" }
                DialogClose { on_click: move |_| on_close.call(()) }
            }

            // ── Group Selector Bar (only when groups are configured) ──
            if has_groups {
                div { class: "flex items-center gap-3 px-2 py-2 bg-zinc-900/40 border-b border-zinc-800/50 rounded-t",
                    span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Group:" }
                    div { class: "flex gap-1.5",
                        for group in bank.groups.iter() {
                            {
                                let gid = group.id.clone();
                                let is_active = active_group_id.as_deref() == Some(&gid);
                                let color = group.color.clone();
                                let sel_val = group.selector_value;
                                let sel_kid = selector_knob_id.clone();
                                rsx! {
                                    button {
                                        key: "{gid}",
                                        class: if is_active {
                                            "px-2.5 py-1 text-[10px] rounded font-medium border transition-colors"
                                        } else {
                                            "px-2.5 py-1 text-[10px] rounded text-zinc-500 hover:text-zinc-300 border border-zinc-700 hover:bg-zinc-800/50 transition-colors"
                                        },
                                        style: if is_active { "background: {color}22; border-color: {color}; color: {color};" } else { "" },
                                        onclick: move |_| {
                                            if let Some(ref kid) = sel_kid {
                                                let mut blk = local_block();
                                                if let Some(ref mut bk) = blk.macro_bank {
                                                    if let Some(k) = bk.get_mut(kid) {
                                                        k.set_value(sel_val);
                                                    }
                                                }
                                                local_block.set(blk);
                                            }
                                        },
                                        "{group.label}"
                                    }
                                }
                            }
                        }
                    }
                    div { class: "flex-1" }
                    // Color pills showing all group colors
                    div { class: "flex gap-1",
                        for group in bank.groups.iter() {
                            div {
                                key: "pill-{group.id}",
                                class: "w-2.5 h-2.5 rounded-full",
                                style: "background: {group.color};",
                            }
                        }
                    }
                }
            }

            // ── Knob Row ──
            div { class: "px-2 py-3",
                div { class: "flex items-end gap-3 flex-wrap",
                    // Shared knobs
                    for (i, knob) in visible_knobs.iter().enumerate() {
                        {
                            let kid = knob.id.clone();
                            let knob_label = knob.label.clone();
                            let value = knob.value;
                            let is_selected = selected_knob_id() == Some(kid.clone());
                            let is_group_knob = i >= shared_count;
                            let accent = if is_group_knob {
                                active_group_color.clone().unwrap_or_else(|| "#3B82F6".to_string())
                            } else {
                                knob.color.clone().unwrap_or_else(|| "#3B82F6".to_string())
                            };
                            let binding_count = knob.bindings.len();
                            rsx! {
                                // Separator between shared and group knobs
                                if i == shared_count && shared_count > 0 {
                                    div { class: "w-px h-16 bg-zinc-700 mx-1" }
                                }
                                div {
                                    key: "{kid}",
                                    class: if is_selected {
                                        "flex flex-col items-center gap-1 p-2 rounded-lg bg-zinc-800/60 border border-zinc-600 cursor-pointer"
                                    } else {
                                        "flex flex-col items-center gap-1 p-2 rounded-lg hover:bg-zinc-800/40 cursor-pointer"
                                    },
                                    onclick: {
                                        let kid = kid.clone();
                                        move |_| {
                                            if selected_knob_id() == Some(kid.clone()) {
                                                selected_knob_id.set(None);
                                            } else {
                                                selected_knob_id.set(Some(kid.clone()));
                                            }
                                        }
                                    },
                                    MiniKnob {
                                        value,
                                        on_change: {
                                            let kid = kid.clone();
                                            let tg = track_guid.clone();
                                            let fg = fx_guid.clone();
                                            move |new_val: f32| {
                                                let mut blk = local_block();
                                                let params = fx_params();
                                                let binding_outputs: Vec<(String, f32, Option<u32>)> = {
                                                    if let Some(ref mut bk) = blk.macro_bank {
                                                        if let Some(k) = bk.get_knob_mut(&kid) {
                                                            k.set_value(new_val);
                                                            k.bindings.iter().map(|binding| {
                                                                let out_val = k.compute_binding_value(binding);
                                                                let fx_idx = find_fx_index(&params, &binding.target.param_id);
                                                                (binding.target.param_id.clone(), out_val, fx_idx)
                                                            }).collect()
                                                        } else { vec![] }
                                                    } else { vec![] }
                                                };
                                                for (param_id, out_val, fx_idx) in &binding_outputs {
                                                    if let Some(idx) = blk.parameters().iter().position(|p| p.id() == param_id) {
                                                        blk.set_parameter_value(idx, *out_val);
                                                    }
                                                    if let Some(idx) = fx_idx {
                                                        let tg = tg.clone();
                                                        let fg = fg.clone();
                                                        let idx = *idx;
                                                        let out_val = *out_val;
                                                        spawn(async move {
                                                            let _ = set_fx_param(&tg, &fg, idx, out_val as f64).await;
                                                        });
                                                    }
                                                }
                                                local_block.set(blk);
                                            }
                                        },
                                    }
                                    span {
                                        class: "text-[10px] font-medium text-center truncate w-14",
                                        style: "color: {accent};",
                                        "{knob_label}"
                                    }
                                    {
                                        let suffix = if binding_count != 1 { "s" } else { "" };
                                        rsx! {
                                            span { class: "text-[9px] text-zinc-600",
                                                "{binding_count} binding{suffix}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Add knob button
                    if visible_knobs.len() < MacroBank::MAX_KNOBS {
                        div {
                            class: "flex flex-col items-center justify-center gap-1 p-2 w-[72px] h-[88px] \
                                    rounded-lg border border-zinc-700 border-dashed \
                                    hover:bg-zinc-800/40 hover:border-zinc-600 \
                                    cursor-pointer transition-colors",
                            onclick: move |_| {
                                let mut blk = local_block();
                                let bk = blk.macro_bank.get_or_insert_with(MacroBank::new);
                                let idx = bk.knobs.len() + bk.groups.iter().map(|g| g.knobs.len()).sum::<usize>() + 1;
                                let knob = MacroKnob::new(format!("m{idx}"), format!("Macro {idx}"));
                                // Add to active group if groups exist, otherwise to shared
                                if let Some(group) = bk.active_group_for(bk.selector_value()).map(|g| g.id.clone()) {
                                    if let Some(g) = bk.groups.iter_mut().find(|g| g.id == group) {
                                        g.knobs.push(knob);
                                    }
                                } else {
                                    bk.add(knob);
                                }
                                local_block.set(blk);
                            },
                            span { class: "text-zinc-600 text-lg", "+" }
                            span { class: "text-[9px] text-zinc-600", "Add" }
                        }
                    }
                }
            }

            // ── Selected Knob Panel ──
            if let Some(ref knob) = selected_knob {
                div { class: "border-t border-zinc-800/50 px-2 pt-3 pb-2 flex flex-col gap-3",
                    // Header + label input
                    div { class: "flex items-center gap-2",
                        span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                            "Edit Macro:"
                        }
                        input {
                            class: "flex-1 bg-zinc-800 border border-zinc-700 rounded px-2 py-1 text-sm text-zinc-200 focus:outline-none focus:border-blue-500",
                            r#type: "text",
                            value: "{knob.label}",
                            oninput: {
                                let kid = knob.id.clone();
                                move |evt: FormEvent| {
                                    let mut blk = local_block();
                                    if let Some(ref mut bk) = blk.macro_bank {
                                        if let Some(k) = bk.get_knob_mut(&kid) {
                                            k.label = evt.value().to_string();
                                        }
                                    }
                                    local_block.set(blk);
                                }
                            },
                        }
                    }

                    // ── Assignments header ──
                    div { class: "flex items-center gap-2",
                        div { class: "h-px flex-1 bg-zinc-700" }
                        span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Assignments" }
                        div { class: "h-px flex-1 bg-zinc-700" }
                    }

                    // ── Assignment cards ──
                    div { class: "flex flex-col gap-2 max-h-60 overflow-y-auto pr-1",
                        if knob.bindings.is_empty() {
                            div { class: "text-xs text-zinc-600 italic text-center py-4",
                                "No assignments — click \"+ Add Assignment\" below"
                            }
                        }
                        for (pos, binding) in knob.bindings.iter().enumerate() {
                            {
                                let param_name = local_block()
                                    .parameters()
                                    .iter()
                                    .find(|p| p.id() == binding.target.param_id)
                                    .map(|p| p.name().to_string())
                                    .unwrap_or_else(|| binding.target.param_id.clone());
                                let output_val = knob.compute_binding_value(binding);
                                let output_pct = (output_val * 100.0) as i32;
                                let bar_width = format!("{}%", output_pct.max(0).min(100));
                                let current_curve = match binding.curve {
                                    ResponseCurve::Easing(ec) => ec,
                                    _ => EasingCurve::Linear,
                                };
                                let kid = knob.id.clone();
                                rsx! {
                                    div {
                                        key: "{binding.target.param_id}",
                                        class: "bg-zinc-800/60 border border-zinc-700/50 rounded-lg p-2.5",

                                        // Card header: param name + remove button
                                        div { class: "flex items-center justify-between mb-2",
                                            span { class: "text-xs font-medium text-zinc-300", "{param_name}" }
                                            button {
                                                class: "text-zinc-600 hover:text-red-400 text-sm leading-none transition-colors",
                                                onclick: {
                                                    let kid = kid.clone();
                                                    move |_| {
                                                        let mut blk = local_block();
                                                        if let Some(ref mut bk) = blk.macro_bank {
                                                            if let Some(k) = bk.get_knob_mut(&kid) {
                                                                if pos < k.bindings.len() {
                                                                    k.bindings.remove(pos);
                                                                }
                                                            }
                                                        }
                                                        local_block.set(blk);
                                                    }
                                                },
                                                "\u{00D7}" // ×
                                            }
                                        }

                                        // Min slider
                                        div { class: "flex items-center gap-2 mb-1.5",
                                            span { class: "text-[10px] text-zinc-500 w-7", "Min:" }
                                            input {
                                                class: "flex-1 h-1 accent-blue-500",
                                                r#type: "range",
                                                min: "0",
                                                max: "1",
                                                step: "0.01",
                                                value: "{binding.min}",
                                                oninput: {
                                                    let kid = kid.clone();
                                                    move |evt: FormEvent| {
                                                        if let Ok(v) = evt.value().parse::<f32>() {
                                                            let mut blk = local_block();
                                                            if let Some(ref mut bk) = blk.macro_bank {
                                                                if let Some(k) = bk.get_knob_mut(&kid) {
                                                                    if let Some(b) = k.bindings.get_mut(pos) {
                                                                        b.min = v;
                                                                    }
                                                                }
                                                            }
                                                            local_block.set(blk);
                                                        }
                                                    }
                                                },
                                            }
                                            span { class: "text-[10px] font-mono text-zinc-400 w-8 text-right",
                                                "{binding.min:.2}"
                                            }
                                        }

                                        // Max slider
                                        div { class: "flex items-center gap-2 mb-1.5",
                                            span { class: "text-[10px] text-zinc-500 w-7", "Max:" }
                                            input {
                                                class: "flex-1 h-1 accent-blue-500",
                                                r#type: "range",
                                                min: "0",
                                                max: "1",
                                                step: "0.01",
                                                value: "{binding.max}",
                                                oninput: {
                                                    let kid = kid.clone();
                                                    move |evt: FormEvent| {
                                                        if let Ok(v) = evt.value().parse::<f32>() {
                                                            let mut blk = local_block();
                                                            if let Some(ref mut bk) = blk.macro_bank {
                                                                if let Some(k) = bk.get_knob_mut(&kid) {
                                                                    if let Some(b) = k.bindings.get_mut(pos) {
                                                                        b.max = v;
                                                                    }
                                                                }
                                                            }
                                                            local_block.set(blk);
                                                        }
                                                    }
                                                },
                                            }
                                            span { class: "text-[10px] font-mono text-zinc-400 w-8 text-right",
                                                "{binding.max:.2}"
                                            }
                                        }

                                        // Curve dropdown + output bar
                                        div { class: "flex items-center gap-2",
                                            span { class: "text-[10px] text-zinc-500 w-7", "Curve:" }
                                            select {
                                                class: "bg-zinc-700 border border-zinc-600 rounded px-1.5 py-0.5 text-[10px] text-zinc-300 focus:outline-none focus:border-blue-500",
                                                value: "{current_curve.display_name()}",
                                                onchange: {
                                                    let kid = kid.clone();
                                                    move |evt: FormEvent| {
                                                        let selected = evt.value();
                                                        let curve = EasingCurve::ALL.iter()
                                                            .find(|c| c.display_name() == selected.as_str())
                                                            .copied()
                                                            .unwrap_or(EasingCurve::Linear);
                                                        let mut blk = local_block();
                                                        if let Some(ref mut bk) = blk.macro_bank {
                                                            if let Some(k) = bk.get_knob_mut(&kid) {
                                                                if let Some(b) = k.bindings.get_mut(pos) {
                                                                    b.curve = ResponseCurve::Easing(curve);
                                                                }
                                                            }
                                                        }
                                                        local_block.set(blk);
                                                    }
                                                },
                                                for curve in EasingCurve::ALL.iter() {
                                                    option {
                                                        value: "{curve.display_name()}",
                                                        selected: *curve == current_curve,
                                                        "{curve.display_name()}"
                                                    }
                                                }
                                            }
                                            div { class: "flex-1" }
                                            // Output bar
                                            span { class: "text-[10px] text-zinc-500", "Output:" }
                                            div { class: "w-16 h-2.5 bg-zinc-900 rounded-full overflow-hidden border border-zinc-700/50",
                                                div {
                                                    class: "h-full bg-blue-500/70 rounded-full transition-all duration-75",
                                                    style: "width: {bar_width}",
                                                }
                                            }
                                            span { class: "text-[10px] font-mono text-zinc-400 w-7 text-right",
                                                "{output_pct}%"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ── Add Assignment button ──
                    div { class: "relative",
                        button {
                            class: "text-xs px-3 py-1.5 rounded bg-zinc-800 border border-zinc-700 text-blue-400 hover:text-blue-300 hover:bg-zinc-700/50 transition-colors",
                            disabled: unbound_params.is_empty(),
                            onclick: move |_| show_add_picker.toggle(),
                            "+ Add Assignment"
                        }
                        if show_add_picker() && !unbound_params.is_empty() {
                            div { class: "absolute bottom-full left-0 mb-1 bg-zinc-800 border border-zinc-700 rounded-lg shadow-xl max-h-40 overflow-y-auto min-w-48 z-10",
                                for (pid, pname) in unbound_params.iter() {
                                    {
                                        let pid = pid.clone();
                                        let kid = knob.id.clone();
                                        rsx! {
                                            button {
                                                key: "{pid}",
                                                class: "w-full text-left px-3 py-1.5 text-xs text-zinc-300 hover:bg-zinc-700/60 transition-colors",
                                                onclick: move |_| {
                                                    let mut blk = local_block();
                                                    if let Some(ref mut bk) = blk.macro_bank {
                                                        if let Some(k) = bk.get_knob_mut(&kid) {
                                                            k.bindings.push(MacroBinding::from_ids("self", pid.clone(), 0.0, 1.0));
                                                        }
                                                    }
                                                    local_block.set(blk);
                                                    show_add_picker.set(false);
                                                },
                                                "{pname}"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ── Location selector (move between groups) ──
                    if has_groups {
                        {
                            let kid = knob.id.clone();
                            let current_group = knob_group_id(&kid);
                            rsx! {
                                div { class: "flex items-center gap-2 pt-2 border-t border-zinc-800/50",
                                    span { class: "text-[10px] text-zinc-500", "Location:" }
                                    select {
                                        class: "bg-zinc-700 border border-zinc-600 rounded px-1.5 py-0.5 text-[10px] text-zinc-300 focus:outline-none focus:border-blue-500",
                                        value: current_group.as_deref().unwrap_or("shared"),
                                        onchange: {
                                            let kid = kid.clone();
                                            move |evt: FormEvent| {
                                                let target = evt.value();
                                                let mut blk = local_block();
                                                if let Some(ref mut bk) = blk.macro_bank {
                                                    // Remove knob from its current location
                                                    let removed = {
                                                        if let Some(pos) = bk.knobs.iter().position(|k| k.id == kid) {
                                                            Some(bk.knobs.remove(pos))
                                                        } else {
                                                            let mut found = None;
                                                            for group in &mut bk.groups {
                                                                if let Some(pos) = group.knobs.iter().position(|k| k.id == kid) {
                                                                    found = Some(group.knobs.remove(pos));
                                                                    break;
                                                                }
                                                            }
                                                            found
                                                        }
                                                    };
                                                    // Add to new location
                                                    if let Some(knob) = removed {
                                                        if target == "shared" {
                                                            bk.knobs.push(knob);
                                                        } else if let Some(group) = bk.groups.iter_mut().find(|g| g.id == target.as_str()) {
                                                            group.knobs.push(knob);
                                                        }
                                                    }
                                                }
                                                local_block.set(blk);
                                            }
                                        },
                                        option { value: "shared", "Shared (always visible)" }
                                        for group in bank.groups.iter() {
                                            option {
                                                value: "{group.id}",
                                                "{group.label} group"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // ── Footer: Done button ──
            DialogFooter {
                button {
                    class: "px-4 py-2 rounded bg-blue-600 hover:bg-blue-500 text-white text-sm font-medium transition-colors",
                    onclick: move |_| {
                        on_save.call(local_block());
                    },
                    "Done"
                }
            }
        }
    }
}

/// Capture tab: three-panel layout for scanning REAPER tracks and capturing
/// FX parameters as block presets.
///
/// **Reactive behavior**:
/// - Track list polls every 2s (no DAW track subscription API exists yet)
/// - FX list subscribes to `fx_chain.subscribe_events()` for real-time updates
/// - Block library fetched on mount and refreshed after each capture
/// - Dedup: selects an existing preset if one matches the FX's source tag
#[component]
pub(crate) fn SignalCaptureTab() -> Element {
    let signal = signal_ui::use_signal_service();

    // ── State ──
    let mut tracks = use_signal(Vec::<TrackInfo>::new);
    let mut has_daw = use_signal(|| daw_registry::signal_daw().is_some());
    let mut selected_track_guid = use_signal(|| None::<String>);

    let mut fx_list = use_signal(Vec::<ScannedFx>::new);
    let mut selected_fx = use_signal(|| None::<ScannedFx>);

    let mut preset_name = use_signal(String::new);
    let mut selected_block_type = use_signal(|| BlockType::Amp);
    let mut capturing = use_signal(|| false);
    let mut error_msg = use_signal(|| None::<String>);

    // ── Tag state ──
    let mut is_paid = use_signal(|| false);
    let mut requires_activation = use_signal(|| false);
    let mut platform_mac = use_signal(|| true);
    let mut platform_windows = use_signal(|| true);
    let mut platform_linux = use_signal(|| false);
    let mut platform_linux_yabridge = use_signal(|| false);

    // ── Live FX parameters ──
    let mut fx_params = use_signal(Vec::<FxParameter>::new);
    let mut params_expanded = use_signal(|| false);
    let mut param_search = use_signal(String::new);
    // Sub-tab toggle: "gui" or "params" for curated GUI vs raw plugin parameters
    let mut capture_param_view = use_signal(|| "params".to_string());

    // ── Matched block from library preset (macro_bank + param_curation) ──
    let mut matched_block = use_signal(|| None::<signal::Block>);
    // Per-FX-instance block cache: keyed by FX GUID so each instance keeps
    // its own macro/curation state independently of the shared preset.
    let mut instance_block_cache = use_signal(std::collections::HashMap::<String, signal::Block>::new);
    // Editing state for macros and curation
    let mut selected_macro_id = use_signal(|| None::<String>);
    let mut editing_macros = use_signal(|| false);
    let mut editing_macros_initial_knob = use_signal(|| None::<String>);
    let mut curation_edit_mode = use_signal(|| false);

    // ── Apply-from-library state ──
    let mut selected_library_item = use_signal(|| None::<LibraryItem>);
    // Index into the selected library item's `snapshots` vec. 0 = default.
    let mut selected_snapshot_idx = use_signal(|| 0usize);
    let mut applying = use_signal(|| false);

    // ── Block library ──
    let mut library = use_signal(Vec::<LibraryItem>::new);
    // Bumped after each capture to trigger a library refresh.
    let mut library_gen = use_signal(|| 0u64);
    // Filter: None = show all, Some(bt) = show only that type
    let mut library_filter_bt = use_signal(|| None::<BlockType>);
    // Search query for the library sidebar
    let mut library_search = use_signal(String::new);

    // Fetch library on mount + whenever library_gen changes
    {
        let signal = signal.clone();
        use_effect(move || {
            let _gen = library_gen(); // subscribe to changes
            let signal = signal.clone();
            spawn(async move {
                let items = fetch_library(&signal).await;
                library.set(items);
            });
        });
    }

    // Filtered + sorted library view
    let filtered_library = use_memo(move || {
        let filter = library_filter_bt();
        let query = library_search().to_lowercase();
        let query = query.trim();
        let mut items: Vec<LibraryItem> = library()
            .iter()
            .filter(|item| filter.map_or(true, |bt| item.block_type == bt))
            .filter(|item| {
                if query.is_empty() {
                    return true;
                }
                // Search across name, vendor, format, block type display name, and all tags
                let name_lower = item.name.to_lowercase();
                if name_lower.contains(query) {
                    return true;
                }
                if let Some(ref v) = item.vendor {
                    if v.to_lowercase().contains(query) {
                        return true;
                    }
                }
                if let Some(ref f) = item.format {
                    if f.to_lowercase().contains(query) {
                        return true;
                    }
                }
                if item.block_type.display_name().to_lowercase().contains(query) {
                    return true;
                }
                // Search raw tags (e.g., "free", "paid", "mac", "licensed")
                item.tags.iter().any(|t| t.to_lowercase().contains(query))
            })
            .cloned()
            .collect();
        // Sort: captured blocks first, then templates; alphabetical within each group
        items.sort_by(|a, b| {
            a.is_template
                .cmp(&b.is_template)
                .then_with(|| a.name.cmp(&b.name))
        });
        items
    });

    // Collect which BlockTypes have presets (for the filter pills)
    let available_types = use_memo(move || {
        let mut types: Vec<BlockType> = Vec::new();
        for item in library().iter() {
            if !types.contains(&item.block_type) {
                types.push(item.block_type);
            }
        }
        types.sort_by_key(|bt| bt.display_name());
        types
    });

    // ── Track list: poll every 2s ──
    let _track_poll = use_future(move || async move {
        loop {
            let connected = daw_registry::signal_daw().is_some();
            has_daw.set(connected);

            if connected {
                if let Some(found) = fetch_tracks().await {
                    tracks.set(found);
                }
            } else {
                tracks.set(Vec::new());
            }

            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }
    });

    // ── FX chain: subscribe to events for the selected track ──
    {
        use_effect(move || {
            let track_guid = selected_track_guid();
            let Some(guid) = track_guid else {
                fx_list.set(Vec::new());
                return;
            };

            let gen = FX_SUB_GEN.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;

            spawn(async move {
                if let Some(fxs) = fetch_track_fx(&guid).await {
                    fx_list.set(fxs);
                }

                let Some(daw) = daw_registry::signal_daw() else { return };
                let Ok(project) = daw.current_project().await else { return };
                let Ok(Some(track)) = project.tracks().by_guid(&guid).await else { return };
                let Ok(mut rx) = track.fx_chain().subscribe_events().await else { return };

                loop {
                    if FX_SUB_GEN.load(std::sync::atomic::Ordering::Relaxed) != gen {
                        break;
                    }
                    match tokio::time::timeout(
                        std::time::Duration::from_millis(500),
                        rx.recv(),
                    )
                    .await
                    {
                        Ok(Ok(Some(_event))) => {
                            if let Some(fxs) = fetch_track_fx(&guid).await {
                                fx_list.set(fxs);
                            }
                        }
                        Ok(Ok(None)) => break,
                        Ok(Err(_)) => break,
                        Err(_) => continue,
                    }
                }
            });
        });
    }

    // ── Dedup: find existing preset matching the selected FX ──
    let existing_match = use_memo(move || {
        let fx = selected_fx.read();
        let Some(ref fx) = *fx else { return None };
        let tag = source_tag(&fx.plugin_name);
        library().iter().find(|item| {
            item.source_tag.as_deref() == Some(tag.as_str())
        }).cloned()
    });

    // Load Block from per-instance cache (keyed by FX GUID) or fall back to
    // the library preset when existing_match / selected FX changes.
    {
        let signal = signal.clone();
        use_effect(move || {
            let em = existing_match();
            let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone());
            let signal = signal.clone();
            spawn(async move {
                // If we have a cached block for this FX instance, use it
                if let Some(ref guid) = fx_guid {
                    if let Some(cached) = instance_block_cache.read().get(guid).cloned() {
                        matched_block.set(Some(cached));
                        return;
                    }
                }
                // Otherwise load from the library preset
                if let Some(item) = em {
                    if let Ok(presets) = signal.block_presets().list(item.block_type).await {
                        if let Some(preset) = presets
                            .iter()
                            .find(|p| p.id().to_string() == item.preset_id)
                        {
                            let block = preset.default_snapshot().block();
                            // Cache for this instance
                            if let Some(ref guid) = fx_guid {
                                instance_block_cache.write().insert(guid.clone(), block.clone());
                            }
                            matched_block.set(Some(block));
                            return;
                        }
                    }
                }
                matched_block.set(None);
            });
        });
    }

    // ── Track selection handler ──
    let on_select_track = move |guid: String| {
        selected_fx.set(None);
        error_msg.set(None);
        fx_list.set(Vec::new());
        fx_params.set(Vec::new());
        params_expanded.set(false);
        param_search.set(String::new());
        selected_track_guid.set(Some(guid));
    };

    // ── FX selection handler ──
    let on_select_fx = move |fx: ScannedFx| {
        let parsed = parse_plugin_name(&fx.plugin_name);
        preset_name.set(parsed.clean_name);
        error_msg.set(None);
        is_paid.set(false);
        requires_activation.set(false);
        platform_mac.set(true);
        platform_windows.set(true);
        platform_linux.set(false);
        platform_linux_yabridge.set(false);
        params_expanded.set(false);
        param_search.set(String::new());
        // Fetch live parameters from REAPER
        let track_guid = selected_track_guid().unwrap_or_default();
        let fx_guid = fx.guid.clone();
        spawn(async move {
            if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                fx_params.set(params);
            } else {
                fx_params.set(Vec::new());
            }
        });
        selected_fx.set(Some(fx));
    };

    // ── Library item selection handler ──
    let on_select_library_item = move |item: LibraryItem| {
        selected_snapshot_idx.set(0);
        error_msg.set(None);
        selected_library_item.set(Some(item));
    };

    // ── Capture handler ──
    let capture_signal = signal.clone();
    let on_capture = move |_| {
        let track_guid = selected_track_guid().unwrap_or_default();
        let Some(ref fx) = *selected_fx.read() else { return };
        let fx_guid = fx.guid.clone();
        let raw_plugin_name = fx.plugin_name.clone();
        let parsed = parse_plugin_name(&raw_plugin_name);
        let name = preset_name().trim().to_string();
        let bt = selected_block_type();
        let signal = capture_signal.clone();

        if name.is_empty() || track_guid.is_empty() {
            return;
        }

        let paid = is_paid();
        let activation = requires_activation();
        let mac = platform_mac();
        let win = platform_windows();
        let lin = platform_linux();
        let lin_yb = platform_linux_yabridge();

        capturing.set(true);
        error_msg.set(None);
        let capture_name = name.clone();
        spawn(async move {
            match capture_fx_as_block(&track_guid, &fx_guid).await {
                Ok((block, _state_data)) => {
                    let metadata = build_capture_metadata(
                        &raw_plugin_name, &parsed, paid, activation, mac, win, lin, lin_yb,
                    );

                    let preset = signal_proto::Preset::with_default_snapshot(
                        signal_proto::PresetId::new(),
                        capture_name.clone(),
                        bt,
                        signal_proto::Snapshot::new(
                            signal_proto::SnapshotId::new(),
                            "Default",
                            block,
                        ),
                    )
                    .with_metadata(metadata);

                    match signal.block_presets().save(preset.clone()).await {
                        Ok(saved) => {
                            register_block_fx(&saved.id().to_string(), &fx_guid);
                            // Refresh library and deselect FX
                            library_gen += 1;
                            selected_fx.set(None);
                            preset_name.set(String::new());
                        }
                        Err(e) => {
                            warn!("Failed to save captured preset: {e:?}");
                            error_msg.set(Some(format!("Save failed: {e}")));
                        }
                    }
                }
                Err(e) => {
                    warn!("FX capture failed: {e:?}");
                    error_msg.set(Some(format!("Capture failed: {e}")));
                }
            }
            capturing.set(false);
        });
    };

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Top bar ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0",
                span { class: "text-xs font-medium text-zinc-300", "FX Capture" }
                div { class: "flex-1" }
                span {
                    class: if has_daw() {
                        "text-[10px] px-1.5 py-0.5 rounded bg-emerald-900/50 text-emerald-400"
                    } else {
                        "text-[10px] px-1.5 py-0.5 rounded bg-red-900/50 text-red-400"
                    },
                    if has_daw() { "DAW Connected" } else { "No DAW" }
                }
            }

            // ── Main body: [Tracks | FX Chain | Capture form | Library sidebar] ──
            div { class: "flex flex-1 min-h-0 overflow-hidden",

                // ═══ Panel 1: Track list ═══
                if has_daw() {
                    div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                        div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                            h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                "Tracks"
                            }
                        }
                        div { class: "flex-1 overflow-y-auto",
                            if tracks().is_empty() {
                                div { class: "px-3 py-4",
                                    p { class: "text-xs text-zinc-600 animate-pulse", "Waiting for tracks..." }
                                }
                            } else {
                                for track in tracks().iter().cloned() {
                                    {
                                        let is_sel = selected_track_guid()
                                            .as_deref() == Some(track.guid.as_str());
                                        let guid = track.guid.clone();
                                        let mut handler = on_select_track.clone();
                                        rsx! {
                                            button {
                                                key: "{track.guid}",
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                                } else {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                                },
                                                onclick: move |_| handler(guid.clone()),
                                                div { class: "flex items-center gap-2",
                                                    span { class: "text-[10px] text-zinc-600 w-5 text-right flex-shrink-0",
                                                        "{track.index + 1}"
                                                    }
                                                    span { class: "text-sm text-zinc-200 truncate flex-1",
                                                        "{track.name}"
                                                    }
                                                    if track.fx_count > 0 {
                                                        span { class: "text-[10px] px-1 rounded bg-zinc-800 text-zinc-400 flex-shrink-0",
                                                            "{track.fx_count} FX"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ═══ Panel 2: FX chain (live-subscribed) ═══
                    div { class: "w-72 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/20",
                        div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center gap-2",
                            h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                "FX Chain"
                            }
                            {
                                let sel_guid = selected_track_guid();
                                let track_name = sel_guid.as_ref().and_then(|g| {
                                    tracks().iter().find(|t| t.guid == *g).map(|t| t.name.clone())
                                });
                                rsx! {
                                    if let Some(name) = track_name {
                                        span { class: "text-[10px] text-zinc-600 ml-auto truncate",
                                            "{name}"
                                        }
                                    }
                                }
                            }
                        }
                        div { class: "flex-1 overflow-y-auto",
                            if selected_track_guid().is_none() {
                                div { class: "flex items-center justify-center h-full",
                                    p { class: "text-xs text-zinc-600", "Select a track" }
                                }
                            } else if fx_list().is_empty() {
                                div { class: "px-3 py-4",
                                    p { class: "text-xs text-zinc-600 animate-pulse",
                                        "Loading FX..."
                                    }
                                }
                            } else {
                                for fx in fx_list().iter().cloned() {
                                    {
                                        let is_sel = selected_fx
                                            .read()
                                            .as_ref()
                                            .map(|f| &f.guid) == Some(&fx.guid);
                                        let tag = source_tag(&fx.plugin_name);
                                        let has_preset = library().iter().any(|item| {
                                            item.source_tag.as_deref() == Some(tag.as_str())
                                        });
                                        let fx_click = fx.clone();
                                        let mut handler = on_select_fx.clone();
                                        rsx! {
                                            button {
                                                key: "{fx.guid}",
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2.5 border-b border-zinc-800/50 bg-blue-900/30 border-l-2 border-l-blue-500"
                                                } else {
                                                    "w-full text-left px-3 py-2.5 border-b border-zinc-800/50 hover:bg-zinc-800/60 border-l-2 border-l-transparent"
                                                },
                                                onclick: move |_| handler(fx_click.clone()),
                                                div { class: "flex items-center gap-2",
                                                    span { class: "text-[10px] text-zinc-600 w-4 text-right flex-shrink-0",
                                                        "{fx.chain_index + 1}"
                                                    }
                                                    span {
                                                        class: if fx.enabled {
                                                            "text-[9px] px-1 rounded bg-emerald-800/60 text-emerald-300 flex-shrink-0"
                                                        } else {
                                                            "text-[9px] px-1 rounded bg-zinc-700 text-zinc-500 flex-shrink-0"
                                                        },
                                                        if fx.enabled { "ON" } else { "OFF" }
                                                    }
                                                    span { class: "text-sm text-zinc-200 truncate flex-1",
                                                        "{fx.name}"
                                                    }
                                                    if has_preset {
                                                        span { class: "text-[9px] px-1 rounded bg-emerald-900/50 text-emerald-400 flex-shrink-0",
                                                            "captured"
                                                        }
                                                    }
                                                }
                                                div { class: "mt-0.5 flex items-center gap-2 pl-6",
                                                    span { class: "text-[10px] text-zinc-500 truncate",
                                                        "{fx.plugin_name}"
                                                    }
                                                    span { class: "text-[10px] text-zinc-600 flex-shrink-0",
                                                        "{fx.parameter_count}p"
                                                    }
                                                }
                                                if let Some(ref pn) = fx.preset_name {
                                                    div { class: "mt-0.5 pl-6 text-[10px] text-zinc-600 truncate",
                                                        "Preset: {pn}"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ═══ Panel 3: Capture form / Apply form / empty state ═══
                //
                // State machine:
                // | Library sel? | FX sel? | Track sel? | Shows                       |
                // |--------------|---------|------------|-----------------------------|
                // | No           | No      | —          | Placeholder                 |
                // | No           | Yes+dup | —          | Already Captured card        |
                // | No           | Yes     | —          | Capture form                |
                // | Yes          | No      | No         | "Select a track" message    |
                // | Yes          | No      | Yes        | "Add to FX Chain" form      |
                // | Yes          | Yes     | Yes        | "Apply to [FX]" form        |
                div { class: "flex-1 min-w-0 flex flex-col overflow-y-auto bg-zinc-950/10",
                    {
                        let lib_item = selected_library_item.read().clone();
                        let fx_sel = selected_fx.read().clone();
                        let track_sel = selected_track_guid().is_some();
                        let existing = existing_match.read().clone();

                        match (lib_item, fx_sel, track_sel) {
                            // ── Library item selected + FX selected → "Apply to [FX]" ──
                            (Some(ref li), Some(ref fx), true) => {
                                let li = li.clone();
                                let fx_name = fx.name.clone();
                                let fx_guid = fx.guid.clone();
                                let bt = li.block_type;
                                let preset_id = li.preset_id.clone();
                                let snapshots = li.snapshots.clone();
                                let snap_count = li.snapshot_count;
                                let is_tmpl = li.is_template;
                                let signal = signal.clone();
                                rsx! {
                                    div { class: "p-4 space-y-4",
                                        // Info card
                                        div { class: "px-3 py-2.5 rounded border border-blue-800/50 bg-blue-900/20",
                                            div { class: "flex items-center gap-2",
                                                span { class: "text-[9px] px-1.5 py-0.5 rounded bg-blue-800/60 text-blue-300",
                                                    "Apply to FX"
                                                }
                                            }
                                            div { class: "text-sm text-zinc-200 font-medium mt-2",
                                                "{li.name}"
                                            }
                                            div { class: "text-[10px] text-zinc-500 mt-0.5",
                                                "Target: {fx_name}"
                                            }
                                            {
                                                let bt_name = bt.display_name();
                                                rsx! {
                                                    div { class: "text-[10px] text-zinc-600 mt-1",
                                                        "{bt_name} · {snap_count} snapshot(s)"
                                                    }
                                                }
                                            }
                                        }

                                        // Snapshot picker (if multiple snapshots)
                                        if snapshots.len() > 1 {
                                            div { class: "space-y-1",
                                                label { class: "text-[10px] text-zinc-500 uppercase tracking-wider",
                                                    "Snapshot"
                                                }
                                                div { class: "flex flex-wrap gap-1",
                                                    for (idx, (_sid, sname)) in snapshots.iter().cloned().enumerate() {
                                                        {
                                                            let is_active = selected_snapshot_idx() == idx;
                                                            rsx! {
                                                                button {
                                                                    key: "{_sid}",
                                                                    class: if is_active {
                                                                        "px-2 py-0.5 text-[10px] rounded bg-blue-700/80 text-blue-200 border border-blue-600"
                                                                    } else {
                                                                        "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200 border border-zinc-700 hover:border-zinc-500"
                                                                    },
                                                                    onclick: move |_| selected_snapshot_idx.set(idx),
                                                                    "{sname}"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // Error message
                                        if let Some(ref msg) = *error_msg.read() {
                                            div { class: "text-sm text-red-400 px-3 py-2 rounded bg-red-900/20 border border-red-800/30",
                                                "{msg}"
                                            }
                                        }

                                        // Apply button
                                        {
                                            let preset_id = preset_id.clone();
                                            let fx_guid = fx_guid.clone();
                                            let signal = signal.clone();
                                            rsx! {
                                                button {
                                                    class: "w-full px-4 py-2.5 text-sm font-medium rounded bg-blue-600 hover:bg-blue-500 text-white transition-colors disabled:opacity-40 disabled:cursor-not-allowed",
                                                    disabled: applying(),
                                                    onclick: move |_| {
                                                        let preset_id = preset_id.clone();
                                                        let fx_guid = fx_guid.clone();
                                                        let signal = signal.clone();
                                                        let snap_idx = selected_snapshot_idx();
                                                        applying.set(true);
                                                        error_msg.set(None);
                                                        spawn(async move {
                                                            let result: eyre::Result<()> = async {
                                                                let presets = signal.block_presets().list(bt).await?;
                                                                let preset = presets.iter()
                                                                    .find(|p| p.id().to_string() == preset_id)
                                                                    .ok_or_else(|| eyre::eyre!("preset not found"))?;
                                                                let snapshot = preset.snapshots()
                                                                    .get(snap_idx)
                                                                    .cloned()
                                                                    .unwrap_or_else(|| preset.default_snapshot());
                                                                let block = snapshot.block();
                                                                let state = snapshot.state_data().map(|d| d.to_vec());
                                                                apply_block_to_fx(
                                                                    &fx_guid,
                                                                    &block,
                                                                    state.as_deref(),
                                                                ).await?;
                                                                register_block_fx(&preset_id, &fx_guid);
                                                                Ok(())
                                                            }.await;
                                                            if let Err(e) = result {
                                                                warn!("Apply to FX failed: {e:?}");
                                                                error_msg.set(Some(format!("Apply failed: {e}")));
                                                            }
                                                            applying.set(false);
                                                        });
                                                    },
                                                    if applying() {
                                                        "Applying..."
                                                    } else if is_tmpl {
                                                        "Apply Parameters to FX"
                                                    } else {
                                                        "Apply to FX"
                                                    }
                                                }
                                            }
                                        }

                                        // Deselect buttons
                                        div { class: "flex gap-2",
                                            button {
                                                class: "px-3 py-1.5 text-xs text-zinc-400 hover:text-zinc-200 border border-zinc-700 rounded hover:bg-zinc-800 transition-colors",
                                                onclick: move |_| selected_library_item.set(None),
                                                "Deselect Block"
                                            }
                                            button {
                                                class: "px-3 py-1.5 text-xs text-zinc-400 hover:text-zinc-200 border border-zinc-700 rounded hover:bg-zinc-800 transition-colors",
                                                onclick: move |_| selected_fx.set(None),
                                                "Deselect FX"
                                            }
                                        }
                                    }
                                }
                            }

                            // ── Library item selected, no FX, track selected → "Add to FX Chain" ──
                            (Some(ref li), None, true) => {
                                let li = li.clone();
                                let is_tmpl = li.is_template;
                                let raw_plugin = li.raw_plugin_name().map(|s| s.to_string());
                                let bt = li.block_type;
                                let preset_id = li.preset_id.clone();
                                let snapshots = li.snapshots.clone();
                                let snap_count = li.snapshot_count;
                                let signal = signal.clone();
                                let color = bt.color();
                                rsx! {
                                    div { class: "p-4 space-y-4",
                                        // Info card
                                        div { class: "px-3 py-2.5 rounded border border-blue-800/50 bg-blue-900/20",
                                            div { class: "flex items-center gap-2",
                                                span {
                                                    class: "text-[9px] px-1 py-0.5 rounded flex-shrink-0",
                                                    style: format!(
                                                        "background-color: {}; color: {}; border: 1px solid {}",
                                                        color.bg, color.fg, color.border,
                                                    ),
                                                    "{bt.display_name()}"
                                                }
                                                span { class: "text-[9px] px-1.5 py-0.5 rounded bg-blue-800/60 text-blue-300",
                                                    "Add to Track"
                                                }
                                            }
                                            div { class: "text-sm text-zinc-200 font-medium mt-2",
                                                "{li.name}"
                                            }
                                            if let Some(ref v) = li.vendor {
                                                div { class: "text-[10px] text-cyan-500/70 mt-0.5",
                                                    "{v}"
                                                }
                                            }
                                            div { class: "text-[10px] text-zinc-600 mt-1",
                                                "{snap_count} snapshot(s)"
                                            }
                                            if let Some(ref src) = raw_plugin {
                                                div { class: "text-[10px] text-zinc-500 mt-0.5",
                                                    "Plugin: {src}"
                                                }
                                            }
                                        }

                                        // Template warning
                                        if is_tmpl {
                                            div { class: "px-3 py-2 rounded border border-amber-800/40 bg-amber-900/20 text-xs text-amber-300",
                                                "Template blocks cannot be added to a track — capture this plugin from REAPER first. "
                                                "However, you can select an existing FX on the chain and apply parameters to it."
                                            }
                                        }

                                        // Snapshot picker (if multiple snapshots and not a template)
                                        if !is_tmpl && snapshots.len() > 1 {
                                            div { class: "space-y-1",
                                                label { class: "text-[10px] text-zinc-500 uppercase tracking-wider",
                                                    "Snapshot"
                                                }
                                                div { class: "flex flex-wrap gap-1",
                                                    for (idx, (_sid, sname)) in snapshots.iter().cloned().enumerate() {
                                                        {
                                                            let is_active = selected_snapshot_idx() == idx;
                                                            rsx! {
                                                                button {
                                                                    key: "{_sid}",
                                                                    class: if is_active {
                                                                        "px-2 py-0.5 text-[10px] rounded bg-blue-700/80 text-blue-200 border border-blue-600"
                                                                    } else {
                                                                        "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200 border border-zinc-700 hover:border-zinc-500"
                                                                    },
                                                                    onclick: move |_| selected_snapshot_idx.set(idx),
                                                                    "{sname}"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // Error message
                                        if let Some(ref msg) = *error_msg.read() {
                                            div { class: "text-sm text-red-400 px-3 py-2 rounded bg-red-900/20 border border-red-800/30",
                                                "{msg}"
                                            }
                                        }

                                        // "Add to FX Chain" button (only for non-template blocks)
                                        if !is_tmpl {
                                            {
                                                let raw_plugin = raw_plugin.clone().unwrap_or_default();
                                                let preset_id = preset_id.clone();
                                                let signal = signal.clone();
                                                rsx! {
                                                    button {
                                                        class: "w-full px-4 py-2.5 text-sm font-medium rounded bg-blue-600 hover:bg-blue-500 text-white transition-colors disabled:opacity-40 disabled:cursor-not-allowed",
                                                        disabled: applying(),
                                                        onclick: move |_| {
                                                            let track_guid = selected_track_guid().unwrap_or_default();
                                                            let raw_plugin = raw_plugin.clone();
                                                            let preset_id = preset_id.clone();
                                                            let signal = signal.clone();
                                                            let snap_idx = selected_snapshot_idx();
                                                            applying.set(true);
                                                            error_msg.set(None);
                                                            spawn(async move {
                                                                let result: eyre::Result<()> = async {
                                                                    let presets = signal.block_presets().list(bt).await?;
                                                                    let preset = presets.iter()
                                                                        .find(|p| p.id().to_string() == preset_id)
                                                                        .ok_or_else(|| eyre::eyre!("preset not found"))?;
                                                                    let snapshot = preset.snapshots()
                                                                        .get(snap_idx)
                                                                        .cloned()
                                                                        .unwrap_or_else(|| preset.default_snapshot());
                                                                    let block = snapshot.block();
                                                                    let state = snapshot.state_data().map(|d| d.to_vec());
                                                                    let new_guid = add_block_to_track(
                                                                        &track_guid,
                                                                        &raw_plugin,
                                                                        &block,
                                                                        state.as_deref(),
                                                                    ).await?;
                                                                    register_block_fx(&preset_id, &new_guid);
                                                                    Ok(())
                                                                }.await;
                                                                if let Err(e) = result {
                                                                    warn!("Add to track failed: {e:?}");
                                                                    error_msg.set(Some(format!("Add failed: {e}")));
                                                                } else {
                                                                    // Refresh library gen to pick up any changes
                                                                    library_gen += 1;
                                                                    selected_library_item.set(None);
                                                                }
                                                                applying.set(false);
                                                            });
                                                        },
                                                        if applying() { "Adding..." } else { "Add to FX Chain" }
                                                    }
                                                }
                                            }
                                        }

                                        // Deselect
                                        button {
                                            class: "px-3 py-1.5 text-xs text-zinc-400 hover:text-zinc-200 border border-zinc-700 rounded hover:bg-zinc-800 transition-colors",
                                            onclick: move |_| selected_library_item.set(None),
                                            "Deselect Block"
                                        }
                                    }
                                }
                            }

                            // ── Library item selected, no FX, no track → "Select a track" ──
                            (Some(ref li), None, false) => {
                                rsx! {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center space-y-2",
                                            div { class: "text-sm text-zinc-200 font-medium",
                                                "{li.name}"
                                            }
                                            p { class: "text-xs text-zinc-500",
                                                "Select a track to apply this block preset."
                                            }
                                            button {
                                                class: "px-3 py-1.5 text-xs text-zinc-400 hover:text-zinc-200 border border-zinc-700 rounded hover:bg-zinc-800 transition-colors",
                                                onclick: move |_| selected_library_item.set(None),
                                                "Deselect"
                                            }
                                        }
                                    }
                                }
                            }

                            // ── No library item, FX selected + existing match → "Already Captured" ──
                            (None, Some(ref fx), _) if existing.is_some() => {
                                let matched = existing.as_ref().unwrap();
                                rsx! {
                                    div { class: "p-4 space-y-4",
                                        div { class: "px-3 py-2.5 rounded border border-emerald-800/50 bg-emerald-900/20",
                                            div { class: "flex items-center gap-2",
                                                span { class: "text-[9px] px-1.5 py-0.5 rounded bg-emerald-800/60 text-emerald-300",
                                                    "Already Captured"
                                                }
                                            }
                                            div { class: "text-sm text-zinc-200 font-medium mt-2",
                                                "{matched.name}"
                                            }
                                            div { class: "text-[10px] text-zinc-500 mt-0.5",
                                                "{fx.plugin_name}"
                                            }
                                            {
                                                let bt_name = matched.block_type.display_name();
                                                let snap_count = matched.snapshot_count;
                                                rsx! {
                                                    div { class: "text-[10px] text-zinc-600 mt-1",
                                                        "{bt_name} · {snap_count} snapshot(s)"
                                                    }
                                                }
                                            }
                                        }

                                        p { class: "text-xs text-zinc-400",
                                            "This plugin already has a block preset in the library. "
                                            "Use the Editor tab to manage its snapshots, or capture a new snapshot from the live REAPER state."
                                        }

                                        button {
                                            class: "px-3 py-1.5 text-xs text-zinc-400 hover:text-zinc-200 border border-zinc-700 rounded hover:bg-zinc-800 transition-colors",
                                            onclick: move |_| selected_fx.set(None),
                                            "Deselect"
                                        }
                                    }
                                }
                            }

                            // ── No library item, FX selected, no match → Capture form ──
                            (None, Some(ref fx), _) => {
                                rsx! {
                                    div { class: "p-4 space-y-4",
                                        // FX identity card
                                        div { class: "px-3 py-2.5 rounded border border-zinc-800 bg-zinc-800/30",
                                            div { class: "text-sm text-zinc-200 font-medium", "{fx.name}" }
                                            div { class: "text-[10px] text-zinc-500 mt-0.5", "{fx.plugin_name}" }
                                            div { class: "text-[10px] text-zinc-600 mt-0.5",
                                                "{fx.parameter_count} parameters"
                                                if let Some(ref pn) = fx.preset_name {
                                                    " · preset: {pn}"
                                                }
                                            }
                                        }

                                        // Preset name
                                        div { class: "space-y-1",
                                            label { class: "text-[10px] text-zinc-500 uppercase tracking-wider", "Preset Name" }
                                            input {
                                                class: "w-full px-3 py-1.5 text-sm bg-zinc-800 border border-zinc-700 rounded text-zinc-200 placeholder-zinc-600 focus:border-blue-500 focus:outline-none",
                                                r#type: "text",
                                                value: "{preset_name}",
                                                placeholder: "e.g. Clean Amp, Crunchy Drive...",
                                                oninput: move |e| preset_name.set(e.value()),
                                            }
                                        }

                                        // Block type picker
                                        div { class: "space-y-1",
                                            label { class: "text-[10px] text-zinc-500 uppercase tracking-wider", "Block Type" }
                                            div { class: "flex flex-wrap gap-1",
                                                for &bt in signal::block::ALL_BLOCK_TYPES {
                                                    {
                                                        let is_selected = selected_block_type() == bt;
                                                        let color = bt.color();
                                                        rsx! {
                                                            button {
                                                                key: "{bt.as_str()}",
                                                                class: "px-2 py-0.5 text-[10px] rounded border transition-colors",
                                                                style: if is_selected {
                                                                    format!(
                                                                        "background-color: {}; color: {}; border-color: {}",
                                                                        color.bg, color.fg, color.border,
                                                                    )
                                                                } else {
                                                                    format!(
                                                                        "border-color: {}40; color: {}80",
                                                                        color.border, color.fg,
                                                                    )
                                                                },
                                                                onclick: move |_| selected_block_type.set(bt),
                                                                "{bt.display_name()}"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // ── Plugin info tags ──
                                        div { class: "space-y-2",
                                            label { class: "text-[10px] text-zinc-500 uppercase tracking-wider", "Plugin Info" }
                                            {
                                                let parsed = parse_plugin_name(&fx.plugin_name);
                                                rsx! {
                                                    if let Some(tag) = parsed.format_tag {
                                                        div { class: "flex items-center gap-2",
                                                            span { class: "text-[10px] text-zinc-500", "Format" }
                                                            span { class: "text-[10px] px-1.5 py-0.5 rounded bg-violet-900/40 text-violet-300", "{tag}" }
                                                        }
                                                    }
                                                    if let Some(ref vendor) = parsed.vendor {
                                                        div { class: "flex items-center gap-2",
                                                            span { class: "text-[10px] text-zinc-500", "Vendor" }
                                                            span { class: "text-[10px] px-1.5 py-0.5 rounded bg-cyan-900/40 text-cyan-300", "{vendor}" }
                                                        }
                                                    }
                                                }
                                            }

                                            // Free / Paid toggle
                                            div { class: "flex items-center gap-2",
                                                span { class: "text-[10px] text-zinc-500 w-16", "Pricing" }
                                                div { class: "flex gap-0.5 bg-zinc-800/80 rounded p-0.5",
                                                    button {
                                                        class: if !is_paid() {
                                                            "px-2 py-0.5 text-[10px] rounded bg-emerald-700/80 text-emerald-200"
                                                        } else {
                                                            "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200"
                                                        },
                                                        onclick: move |_| is_paid.set(false),
                                                        "Free"
                                                    }
                                                    button {
                                                        class: if is_paid() {
                                                            "px-2 py-0.5 text-[10px] rounded bg-amber-700/80 text-amber-200"
                                                        } else {
                                                            "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200"
                                                        },
                                                        onclick: move |_| is_paid.set(true),
                                                        "Paid"
                                                    }
                                                }
                                            }

                                            // Licensed / Portable toggle
                                            div { class: "flex items-center gap-2",
                                                span { class: "text-[10px] text-zinc-500 w-16", "Access" }
                                                div { class: "flex gap-0.5 bg-zinc-800/80 rounded p-0.5",
                                                    button {
                                                        class: if !requires_activation() {
                                                            "px-2 py-0.5 text-[10px] rounded bg-emerald-700/80 text-emerald-200"
                                                        } else {
                                                            "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200"
                                                        },
                                                        onclick: move |_| requires_activation.set(false),
                                                        "Portable"
                                                    }
                                                    button {
                                                        class: if requires_activation() {
                                                            "px-2 py-0.5 text-[10px] rounded bg-amber-700/80 text-amber-200"
                                                        } else {
                                                            "px-2 py-0.5 text-[10px] rounded text-zinc-400 hover:text-zinc-200"
                                                        },
                                                        onclick: move |_| requires_activation.set(true),
                                                        "Licensed"
                                                    }
                                                }
                                            }
                                        }

                                        // ── Platform compatibility ──
                                        div { class: "space-y-1.5",
                                            label { class: "text-[10px] text-zinc-500 uppercase tracking-wider", "Platform Compatibility" }
                                            div { class: "flex flex-wrap gap-1",
                                                {
                                                    let active = platform_mac();
                                                    rsx! {
                                                        button {
                                                            class: if active {
                                                                "px-2 py-0.5 text-[10px] rounded border border-blue-600/60 bg-blue-900/40 text-blue-300"
                                                            } else {
                                                                "px-2 py-0.5 text-[10px] rounded border border-zinc-700 text-zinc-500 hover:text-zinc-300 hover:border-zinc-500"
                                                            },
                                                            onclick: move |_| platform_mac.set(!active),
                                                            "Mac"
                                                        }
                                                    }
                                                }
                                                {
                                                    let active = platform_windows();
                                                    rsx! {
                                                        button {
                                                            class: if active {
                                                                "px-2 py-0.5 text-[10px] rounded border border-blue-600/60 bg-blue-900/40 text-blue-300"
                                                            } else {
                                                                "px-2 py-0.5 text-[10px] rounded border border-zinc-700 text-zinc-500 hover:text-zinc-300 hover:border-zinc-500"
                                                            },
                                                            onclick: move |_| platform_windows.set(!active),
                                                            "Windows"
                                                        }
                                                    }
                                                }
                                                {
                                                    let active = platform_linux();
                                                    rsx! {
                                                        button {
                                                            class: if active {
                                                                "px-2 py-0.5 text-[10px] rounded border border-blue-600/60 bg-blue-900/40 text-blue-300"
                                                            } else {
                                                                "px-2 py-0.5 text-[10px] rounded border border-zinc-700 text-zinc-500 hover:text-zinc-300 hover:border-zinc-500"
                                                            },
                                                            onclick: move |_| platform_linux.set(!active),
                                                            "Linux"
                                                        }
                                                    }
                                                }
                                                {
                                                    let active = platform_linux_yabridge();
                                                    rsx! {
                                                        button {
                                                            class: if active {
                                                                "px-2 py-0.5 text-[10px] rounded border border-blue-600/60 bg-blue-900/40 text-blue-300"
                                                            } else {
                                                                "px-2 py-0.5 text-[10px] rounded border border-zinc-700 text-zinc-500 hover:text-zinc-300 hover:border-zinc-500"
                                                            },
                                                            onclick: move |_| platform_linux_yabridge.set(!active),
                                                            "Linux-Yabridge"
                                                        }
                                                    }
                                                }
                                            }
                                        }

                                        // Error message
                                        if let Some(ref msg) = *error_msg.read() {
                                            div { class: "text-sm text-red-400 px-3 py-2 rounded bg-red-900/20 border border-red-800/30",
                                                "{msg}"
                                            }
                                        }

                                        // Capture button
                                        button {
                                            class: "w-full px-4 py-2.5 text-sm font-medium rounded bg-emerald-600 hover:bg-emerald-500 text-white transition-colors disabled:opacity-40 disabled:cursor-not-allowed",
                                            disabled: preset_name().trim().is_empty() || capturing(),
                                            onclick: on_capture,
                                            if capturing() { "Capturing..." } else { "Capture & Save" }
                                        }
                                    }
                                }
                            }

                            // ── Nothing selected ──
                            (None, None, _) => {
                                rsx! {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center space-y-1",
                                            p { class: "text-xs text-zinc-500",
                                                if has_daw() {
                                                    "Select an FX plugin to capture, or a block to apply"
                                                } else {
                                                    "Connect a DAW to start capturing"
                                                }
                                            }
                                            p { class: "text-[10px] text-zinc-600",
                                                "Choose a track, then click a plugin to capture its parameters as a block preset."
                                            }
                                        }
                                    }
                                }
                            }

                            // ── Library selected + FX selected but no track (shouldn't happen since FX requires track) ──
                            _ => {
                                rsx! {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-xs text-zinc-500", "Select a track to continue" }
                                    }
                                }
                            }
                        }
                    }

                    // ── Bottom Controls: Macros + GUI/Plugin Parameters ──
                    if selected_fx.read().is_some() && !fx_params().is_empty() {
                        div { class: "border-t border-border flex flex-col",

                            // ── Macro Controls (from matched library preset) ──
                            {
                                let mb = matched_block();
                                let macro_bank = mb.as_ref().and_then(|b| b.macro_bank.clone());
                                let knobs = macro_bank.as_ref().map(|b| b.knobs.clone()).unwrap_or_default();
                                let has_match = existing_match().is_some();
                                let can_add = has_match && knobs.len() < MacroBank::MAX_KNOBS;
                                rsx! {
                                    div { class: "px-3 py-2 bg-zinc-900/20 border-b border-zinc-800/50",
                                        div { class: "flex items-center gap-2 mb-1",
                                            span { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                                "Macros"
                                            }
                                            div { class: "flex-1" }
                                            if has_match && !knobs.is_empty() {
                                                button {
                                                    class: "text-[10px] px-1.5 py-0.5 rounded text-zinc-400 hover:text-zinc-200 hover:bg-zinc-700/50 transition-colors",
                                                    onclick: move |_| {
                                                        editing_macros_initial_knob.set(None);
                                                        editing_macros.set(true);
                                                    },
                                                    "Edit"
                                                }
                                            }
                                            if can_add {
                                                {
                                                    rsx! {
                                                        button {
                                                            class: "text-[10px] px-1.5 py-0.5 rounded text-blue-400 hover:text-blue-300 hover:bg-blue-900/30 transition-colors",
                                                            onclick: move |_| {
                                                                spawn(async move {
                                                                    if let Some(ref _item) = existing_match() {
                                                                        if let Some(mut blk) = matched_block() {
                                                                            let bank = blk.macro_bank.get_or_insert_with(MacroBank::new);
                                                                            let idx = bank.knobs.len() + 1;
                                                                            let knob = MacroKnob::new(
                                                                                format!("m{idx}"),
                                                                                format!("Macro {idx}"),
                                                                            );
                                                                            bank.add(knob);
                                                                            // Save to per-instance cache (not the shared preset)
                                                                            if let Some(guid) = selected_fx.read().as_ref().map(|f| f.guid.clone()) {
                                                                                instance_block_cache.write().insert(guid, blk.clone());
                                                                            }
                                                                            matched_block.set(Some(blk));
                                                                            library_gen += 1;
                                                                        }
                                                                    }
                                                                });
                                                            },
                                                            "+ Add"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        if knobs.is_empty() {
                                            if has_match {
                                                div { class: "text-[10px] text-zinc-600 italic", "No macros — click + Add to create one" }
                                            } else {
                                                div { class: "text-[10px] text-zinc-600 italic", "No macros configured" }
                                            }
                                        } else {
                                            div { class: "flex flex-wrap gap-3 py-1",
                                                for knob in knobs.iter().cloned() {
                                                    {
                                                        let knob_id = knob.id.clone();
                                                        let knob_label = knob.label.clone();
                                                        let knob_value = knob.value;
                                                        let is_selected = selected_macro_id() == Some(knob_id.clone());
                                                        rsx! {
                                                            div {
                                                                key: "{knob_id}",
                                                                class: if is_selected {
                                                                    "flex flex-col items-center gap-1 p-1 rounded border border-blue-500/40 bg-blue-900/20"
                                                                } else {
                                                                    "flex flex-col items-center gap-1 p-1 rounded border border-transparent hover:border-zinc-700"
                                                                },
                                                                onclick: {
                                                                    let kid = knob_id.clone();
                                                                    move |_| {
                                                                        if selected_macro_id() == Some(kid.clone()) {
                                                                            selected_macro_id.set(None);
                                                                        } else {
                                                                            selected_macro_id.set(Some(kid.clone()));
                                                                        }
                                                                    }
                                                                },
                                                                MiniKnob {
                                                                    value: knob_value,
                                                                    on_change: {
                                                                        let kid = knob_id.clone();
                                                                        move |new_val: f32| {
                                                                            if let Some(mut blk) = matched_block() {
                                                                                // Collect binding outputs from macro knob, then release bank borrow
                                                                                let binding_outputs: Vec<(String, f32, Option<u32>)> = {
                                                                                    let params = fx_params();
                                                                                    if let Some(ref mut bank) = blk.macro_bank {
                                                                                        if let Some(k) = bank.get_mut(&kid) {
                                                                                            k.set_value(new_val);
                                                                                            k.bindings.iter().map(|binding| {
                                                                                                let out_val = k.compute_binding_value(binding);
                                                                                                let fx_idx = find_fx_index(&params, &binding.target.param_id);
                                                                                                (binding.target.param_id.clone(), out_val, fx_idx)
                                                                                            }).collect()
                                                                                        } else { vec![] }
                                                                                    } else { vec![] }
                                                                                };
                                                                                // Sync curated GUI knobs + drive REAPER params
                                                                                for (param_id, out_val, fx_idx) in &binding_outputs {
                                                                                    if let Some(idx) = blk.parameters().iter().position(|p| p.id() == param_id) {
                                                                                        blk.set_parameter_value(idx, *out_val);
                                                                                    }
                                                                                    if let Some(idx) = fx_idx {
                                                                                        let track_guid = selected_track_guid().unwrap_or_default();
                                                                                        let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                        let idx = *idx;
                                                                                        let out_val = *out_val;
                                                                                        spawn(async move {
                                                                                            let _ = set_fx_param(&track_guid, &fx_guid, idx, out_val as f64).await;
                                                                                        });
                                                                                    }
                                                                                }
                                                                                matched_block.set(Some(blk.clone()));
                                                                                // Save to per-instance cache (not the shared preset)
                                                                                if let Some(guid) = selected_fx.read().as_ref().map(|f| f.guid.clone()) {
                                                                                    instance_block_cache.write().insert(guid, blk.clone());
                                                                                }
                                                                                // Re-fetch REAPER params so raw list stays in sync
                                                                                spawn(async move {
                                                                                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                                                                                    let track_guid = selected_track_guid().unwrap_or_default();
                                                                                    let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                    if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                                                                                        fx_params.set(params);
                                                                                    }
                                                                                });
                                                                            }
                                                                        }
                                                                    },
                                                                }
                                                                div { class: "flex items-center gap-0.5",
                                                                    span { class: "text-[10px] text-zinc-400 text-center truncate w-12",
                                                                        "{knob_label}"
                                                                    }
                                                                    button {
                                                                        class: "text-[9px] text-zinc-600 hover:text-blue-400 transition-colors leading-none",
                                                                        title: "Edit macro",
                                                                        onclick: {
                                                                            let kid = knob_id.clone();
                                                                            move |evt: MouseEvent| {
                                                                                evt.stop_propagation();
                                                                                editing_macros_initial_knob.set(Some(kid.clone()));
                                                                                editing_macros.set(true);
                                                                            }
                                                                        },
                                                                        "\u{270E}" // ✎ pencil
                                                                    }
                                                                }
                                                                span { class: "text-[10px] font-mono text-zinc-300 text-center",
                                                                    "{(knob_value * 100.0) as i32}%"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            // Unified macro editor dialog
                                            if editing_macros() {
                                                MacroEditorDialog {
                                                    block: matched_block().unwrap(),
                                                    fx_params: fx_params,
                                                    track_guid: selected_track_guid().unwrap_or_default(),
                                                    fx_guid: selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default(),
                                                    initial_knob_id: editing_macros_initial_knob(),
                                                    on_save: move |updated_blk: signal::Block| {
                                                        matched_block.set(Some(updated_blk.clone()));
                                                        // Save to per-instance cache (not the shared preset)
                                                        if let Some(guid) = selected_fx.read().as_ref().map(|f| f.guid.clone()) {
                                                            instance_block_cache.write().insert(guid, updated_blk);
                                                        }
                                                        editing_macros.set(false);
                                                        editing_macros_initial_knob.set(None);
                                                        library_gen += 1;
                                                    },
                                                    on_close: move |_| {
                                                        editing_macros.set(false);
                                                        editing_macros_initial_knob.set(None);
                                                    },
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            // ── Sub-tab toggle: GUI vs Plugin Parameters ──
                            div { class: "px-3 py-1.5 bg-zinc-900/30 border-b border-zinc-800/50 flex items-center gap-1",
                                button {
                                    class: if capture_param_view() == "gui" {
                                        "px-2.5 py-1 text-[10px] rounded font-medium bg-blue-600/20 text-blue-300 border border-blue-500/30"
                                    } else {
                                        "px-2.5 py-1 text-[10px] rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/50"
                                    },
                                    onclick: move |_| capture_param_view.set("gui".to_string()),
                                    "GUI"
                                }
                                button {
                                    class: if capture_param_view() == "params" {
                                        "px-2.5 py-1 text-[10px] rounded font-medium bg-blue-600/20 text-blue-300 border border-blue-500/30"
                                    } else {
                                        "px-2.5 py-1 text-[10px] rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/50"
                                    },
                                    onclick: move |_| capture_param_view.set("params".to_string()),
                                    "Plugin Parameters"
                                }
                                div { class: "flex-1" }
                                // Refresh button
                                button {
                                    class: "text-[10px] px-1.5 py-0.5 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 transition-colors",
                                    onclick: move |_| {
                                        let track_guid = selected_track_guid().unwrap_or_default();
                                        let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                        spawn(async move {
                                            if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                                                fx_params.set(params);
                                            }
                                        });
                                    },
                                    "↻"
                                }
                            }

                            // ── Content area ──
                            if capture_param_view() == "gui" {
                                // Curated GUI view
                                {
                                    let mb = matched_block();
                                    let curation = mb.as_ref().and_then(|b| b.param_curation.clone());
                                    let block_params = mb.as_ref().map(|b| b.parameters().to_vec()).unwrap_or_default();
                                    let has_match = existing_match().is_some();
                                    let editing = curation_edit_mode();
                                    rsx! {
                                        div { class: "p-3",
                                            if !has_match {
                                                // No matched preset — can't curate
                                                div { class: "flex flex-col items-center justify-center h-24 text-center",
                                                    div { class: "text-zinc-600 text-xs", "No curated parameters" }
                                                    div { class: "text-zinc-700 text-[10px] mt-1",
                                                        "Capture a preset and add parameters to the curation list to build a custom GUI."
                                                    }
                                                }
                                            } else if editing {
                                                // ── Edit mode: checkboxes for all block params ──
                                                div { class: "space-y-1",
                                                    div { class: "flex items-center gap-2 mb-2",
                                                        span { class: "text-[10px] text-zinc-400", "Toggle parameters to feature in the GUI:" }
                                                        div { class: "flex-1" }
                                                        button {
                                                            class: "text-[10px] px-2 py-0.5 rounded bg-zinc-700 text-zinc-300 hover:bg-zinc-600 transition-colors",
                                                            onclick: move |_| curation_edit_mode.set(false),
                                                            "Done"
                                                        }
                                                    }
                                                    div { class: "flex flex-wrap gap-1 max-h-48 overflow-y-auto",
                                                        for bp in block_params.iter() {
                                                            {
                                                                let pid = bp.id().to_string();
                                                                let pname = bp.name().to_string();
                                                                let is_featured = curation.as_ref().map_or(false, |c| c.is_featured(&pid));
                                                                rsx! {
                                                                    button {
                                                                        key: "{pid}",
                                                                        class: if is_featured {
                                                                            "px-2 py-1 text-[10px] rounded bg-emerald-700/60 text-emerald-200 border border-emerald-600/40"
                                                                        } else {
                                                                            "px-2 py-1 text-[10px] rounded text-zinc-500 hover:text-zinc-300 border border-zinc-700/40 hover:bg-zinc-700/30"
                                                                        },
                                                                        onclick: {
                                                                            let pid = pid.clone();
                                                                            move |_| {
                                                                                let pid = pid.clone();
                                                                                spawn(async move {
                                                                                    if let Some(mut blk) = matched_block() {
                                                                                        let cur = blk.param_curation.get_or_insert_with(ParamCuration::default);
                                                                                        if let Some(pos) = cur.featured_param_ids.iter().position(|id| id == &pid) {
                                                                                            cur.featured_param_ids.remove(pos);
                                                                                        } else {
                                                                                            cur.featured_param_ids.push(pid.clone());
                                                                                        }
                                                                                        // Save to per-instance cache (not the shared preset)
                                                                                        if let Some(guid) = selected_fx.read().as_ref().map(|f| f.guid.clone()) {
                                                                                            instance_block_cache.write().insert(guid, blk.clone());
                                                                                        }
                                                                                        matched_block.set(Some(blk));
                                                                                        library_gen += 1;
                                                                                    }
                                                                                });
                                                                            }
                                                                        },
                                                                        if is_featured { "☑ " } else { "☐ " }
                                                                        "{pname}"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                // ── Normal mode: show curated knobs ──
                                                {
                                                    let featured_ids = curation.as_ref()
                                                        .map(|c| c.featured_param_ids.clone())
                                                        .unwrap_or_default();
                                                    let featured_params: Vec<_> = featured_ids.iter()
                                                        .filter_map(|fid| block_params.iter().find(|bp| bp.id() == fid))
                                                        .cloned()
                                                        .collect();
                                                    rsx! {
                                                        if featured_params.is_empty() {
                                                            div { class: "flex flex-col items-center justify-center h-24 text-center",
                                                                div { class: "text-zinc-600 text-xs", "No parameters curated yet" }
                                                                div { class: "text-zinc-700 text-[10px] mt-1",
                                                                    "Click Edit to select which parameters appear here."
                                                                }
                                                            }
                                                        } else {
                                                            div { class: "grid grid-cols-4 gap-3",
                                                                for bp in featured_params.iter() {
                                                                    {
                                                                        let pid = bp.id().to_string();
                                                                        let pname = bp.name().to_string();
                                                                        let pval = bp.value().get();
                                                                        rsx! {
                                                                            div {
                                                                                key: "{pid}",
                                                                                class: "flex flex-col items-center gap-1",
                                                                                MiniKnob {
                                                                                    value: pval,
                                                                                    on_change: {
                                                                                        let pid = pid.clone();
                                                                                        move |new_val: f32| {
                                                                                            // Update REAPER FX param
                                                                                            let params = fx_params();
                                                                                            if let Some(idx) = find_fx_index(&params, &pid) {
                                                                                                let track_guid = selected_track_guid().unwrap_or_default();
                                                                                                let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                                spawn(async move {
                                                                                                    let _ = set_fx_param(&track_guid, &fx_guid, idx, new_val as f64).await;
                                                                                                });
                                                                                            }
                                                                                            // Update local block state
                                                                                            if let Some(mut blk) = matched_block() {
                                                                                                if let Some(bp) = blk.parameters().iter().position(|p| p.id() == pid) {
                                                                                                    blk.set_parameter_value(bp, new_val);
                                                                                                    matched_block.set(Some(blk));
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    },
                                                                                }
                                                                                span { class: "text-[10px] text-zinc-400 text-center truncate w-14",
                                                                                    "{pname}"
                                                                                }
                                                                                span { class: "text-[10px] font-mono text-zinc-300 text-center",
                                                                                    "{(pval * 100.0) as i32}%"
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                        // Edit button
                                                        div { class: "flex justify-end mt-2",
                                                            button {
                                                                class: "text-[10px] px-2 py-0.5 rounded text-zinc-400 hover:text-zinc-200 border border-zinc-700 hover:bg-zinc-800 transition-colors",
                                                                onclick: move |_| curation_edit_mode.set(true),
                                                                "Edit ✎"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Plugin Parameters — raw REAPER FX parameter list
                                div {
                                    // Header with expand toggle + search + count
                                    div { class: "px-3 py-1.5 flex items-center gap-2",
                                        button {
                                            class: "text-[10px] text-zinc-400 hover:text-zinc-200 transition-colors",
                                            onclick: move |_| params_expanded.set(!params_expanded()),
                                            if params_expanded() { "▼" } else { "▶" }
                                        }
                                        span { class: "text-[10px] text-zinc-600",
                                            {
                                                let total = fx_params().len();
                                                let query = param_search().to_lowercase();
                                                let shown = if query.trim().is_empty() {
                                                    total
                                                } else {
                                                    fx_params().iter().filter(|p| p.name.to_lowercase().contains(query.trim())).count()
                                                };
                                                format!("{shown}/{total}")
                                            }
                                        }
                                        div { class: "flex-1" }
                                        if params_expanded() {
                                            input {
                                                class: "w-32 px-2 py-0.5 text-[10px] bg-zinc-800/80 border border-zinc-700/60 rounded text-zinc-200 placeholder-zinc-600 focus:border-blue-500/60 focus:outline-none",
                                                r#type: "text",
                                                value: "{param_search}",
                                                placeholder: "Filter params...",
                                                oninput: move |e| param_search.set(e.value()),
                                            }
                                        }
                                    }

                                    // Parameter list (collapsible)
                                    if params_expanded() {
                                        div { class: "max-h-96 overflow-y-auto",
                                            {
                                                let query = param_search().to_lowercase();
                                                let query = query.trim().to_string();
                                                rsx! {
                                                    for param in fx_params().iter().filter(|p| {
                                                        query.is_empty() || p.name.to_lowercase().contains(&query)
                                                    }).cloned() {
                                                        {
                                                            let pct = (param.value * 100.0).round();
                                                            let param_idx = param.index;
                                                            rsx! {
                                                                div {
                                                                    key: "{param.index}",
                                                                    class: "flex items-center gap-2 px-3 py-1 border-b border-zinc-800/30 hover:bg-zinc-800/20 group",

                                                                    // Index
                                                                    span { class: "text-[9px] text-zinc-600 w-5 text-right flex-shrink-0",
                                                                        "{param.index}"
                                                                    }

                                                                    // Name
                                                                    span { class: "text-[10px] text-zinc-300 w-32 truncate flex-shrink-0",
                                                                        title: "{param.name}",
                                                                        "{param.name}"
                                                                    }

                                                                    // Slider track with value fill
                                                                    if param.is_toggle {
                                                                        // Toggle button
                                                                        button {
                                                                            class: if param.value > 0.5 {
                                                                                "px-2 py-0.5 text-[9px] rounded bg-emerald-700/80 text-emerald-200 flex-shrink-0"
                                                                            } else {
                                                                                "px-2 py-0.5 text-[9px] rounded bg-zinc-700 text-zinc-400 flex-shrink-0"
                                                                            },
                                                                            onclick: move |_| {
                                                                                let new_val = if param.value > 0.5 { 0.0 } else { 1.0 };
                                                                                let track_guid = selected_track_guid().unwrap_or_default();
                                                                                let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                spawn(async move {
                                                                                    if let Err(e) = set_fx_param(&track_guid, &fx_guid, param_idx, new_val).await {
                                                                                        warn!("Failed to set param: {e:?}");
                                                                                    }
                                                                                    // Refresh params to get updated formatted value
                                                                                    if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                                                                                        fx_params.set(params);
                                                                                    }
                                                                                });
                                                                            },
                                                                            if param.value > 0.5 { "ON" } else { "OFF" }
                                                                        }
                                                                    } else if param.step_count.is_some() && !param.step_labels.is_empty() {
                                                                        // Discrete parameter — dropdown-style pills
                                                                        div { class: "flex-1 flex flex-wrap gap-0.5 min-w-0",
                                                                            for (norm_val, label) in param.step_labels.iter().cloned() {
                                                                                {
                                                                                    // Find the closest step label to the current value
                                                                                    let is_active = param.step_labels.iter()
                                                                                        .min_by(|a, b| {
                                                                                            (a.0 - param.value).abs().partial_cmp(&(b.0 - param.value).abs()).unwrap_or(std::cmp::Ordering::Equal)
                                                                                        })
                                                                                        .map(|closest| closest.0 == norm_val)
                                                                                        .unwrap_or(false);
                                                                                    rsx! {
                                                                                        button {
                                                                                            class: if is_active {
                                                                                                "px-1 py-0 text-[8px] rounded bg-blue-700/70 text-blue-200"
                                                                                            } else {
                                                                                                "px-1 py-0 text-[8px] rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50"
                                                                                            },
                                                                                            onclick: move |_| {
                                                                                                let track_guid = selected_track_guid().unwrap_or_default();
                                                                                                let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                                spawn(async move {
                                                                                                    if let Err(e) = set_fx_param(&track_guid, &fx_guid, param_idx, norm_val).await {
                                                                                                        warn!("Failed to set param: {e:?}");
                                                                                                    }
                                                                                                    if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                                                                                                        fx_params.set(params);
                                                                                                    }
                                                                                                });
                                                                                            },
                                                                                            "{label}"
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    } else {
                                                                        // Continuous parameter — slider
                                                                        div { class: "flex-1 relative h-4 rounded bg-zinc-800 overflow-hidden min-w-0",
                                                                            // Fill bar
                                                                            div {
                                                                                class: "absolute inset-y-0 left-0 bg-blue-600/40 rounded-l pointer-events-none",
                                                                                style: "width: {pct}%",
                                                                            }
                                                                            // Invisible native range input
                                                                            input {
                                                                                class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer",
                                                                                r#type: "range",
                                                                                min: "0",
                                                                                max: "1000",
                                                                                value: "{(param.value * 1000.0).round() as i64}",
                                                                                // oninput: send to REAPER + update local state (fires during drag)
                                                                                oninput: move |evt: FormEvent| {
                                                                                    if let Ok(v) = evt.value().parse::<f64>() {
                                                                                        let normalized = v / 1000.0;
                                                                                        // Optimistic local update for responsive UI
                                                                                        let mut params = fx_params();
                                                                                        if let Some(p) = params.iter_mut().find(|p| p.index == param_idx) {
                                                                                            p.value = normalized;
                                                                                        }
                                                                                        fx_params.set(params);
                                                                                        // Send to REAPER for real-time audio feedback
                                                                                        let track_guid = selected_track_guid().unwrap_or_default();
                                                                                        let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                        spawn(async move {
                                                                                            if let Err(e) = set_fx_param(&track_guid, &fx_guid, param_idx, normalized).await {
                                                                                                warn!("Failed to set param: {e:?}");
                                                                                            }
                                                                                        });
                                                                                    }
                                                                                },
                                                                                // onchange: refresh all params on mouse release for accurate formatted values
                                                                                onchange: move |_| {
                                                                                    let track_guid = selected_track_guid().unwrap_or_default();
                                                                                    let fx_guid = selected_fx.read().as_ref().map(|f| f.guid.clone()).unwrap_or_default();
                                                                                    spawn(async move {
                                                                                        if let Some(params) = fetch_fx_params(&track_guid, &fx_guid).await {
                                                                                            fx_params.set(params);
                                                                                        }
                                                                                    });
                                                                                },
                                                                            }
                                                                        }
                                                                    }

                                                                    // Formatted value
                                                                    span { class: "text-[9px] text-zinc-500 w-20 text-right flex-shrink-0 truncate",
                                                                        title: "{param.formatted}",
                                                                        "{param.formatted}"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // ═══ Panel 4: Block Library sidebar (always visible) ═══
                div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/30",
                    // Header + counts
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center gap-2",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                            "Block Library"
                        }
                        {
                            let shown = filtered_library().len();
                            let total = library().len();
                            rsx! {
                                span { class: "text-[10px] text-zinc-600 ml-auto",
                                    "{shown}/{total}"
                                }
                            }
                        }
                    }

                    // Search bar
                    div { class: "px-2 py-1.5 border-b border-border flex-shrink-0",
                        input {
                            class: "w-full px-2 py-1 text-[11px] bg-zinc-800/80 border border-zinc-700/60 rounded text-zinc-200 placeholder-zinc-600 focus:border-blue-500/60 focus:outline-none",
                            r#type: "text",
                            value: "{library_search}",
                            placeholder: "Search blocks, vendors, tags...",
                            oninput: move |e| library_search.set(e.value()),
                        }
                    }

                    // BlockType filter pills
                    div { class: "px-2 py-1.5 border-b border-border flex-shrink-0 flex flex-wrap gap-1 max-h-20 overflow-y-auto",
                        // "All" pill
                        {
                            let is_all = library_filter_bt().is_none();
                            rsx! {
                                button {
                                    class: if is_all {
                                        "px-2 py-0.5 text-[10px] rounded bg-zinc-600 text-zinc-200"
                                    } else {
                                        "px-2 py-0.5 text-[10px] rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800"
                                    },
                                    onclick: move |_| library_filter_bt.set(None),
                                    "All"
                                }
                            }
                        }
                        // One pill per BlockType that has presets
                        for bt in available_types().iter().copied() {
                            {
                                let is_active = library_filter_bt() == Some(bt);
                                let color = bt.color();
                                rsx! {
                                    button {
                                        key: "{bt.as_str()}",
                                        class: "px-2 py-0.5 text-[10px] rounded border transition-colors",
                                        style: if is_active {
                                            format!(
                                                "background-color: {}; color: {}; border-color: {}",
                                                color.bg, color.fg, color.border,
                                            )
                                        } else {
                                            format!(
                                                "border-color: {}30; color: {}60",
                                                color.border, color.fg,
                                            )
                                        },
                                        onclick: move |_| library_filter_bt.set(Some(bt)),
                                        "{bt.display_name()}"
                                    }
                                }
                            }
                        }
                    }

                    // Library items
                    if filtered_library().is_empty() {
                        div { class: "flex items-center justify-center flex-1",
                            p { class: "text-[10px] text-zinc-600",
                                if library_search().is_empty() && library_filter_bt().is_none() {
                                    "No block presets yet"
                                } else {
                                    "No blocks match filter"
                                }
                            }
                        }
                    } else {
                        div { class: "flex-1 overflow-y-auto",
                            for item in filtered_library().iter().cloned() {
                                {
                                    let color = item.block_type.color();
                                    let bt_name = item.block_type.display_name();
                                    let snap_label = if item.snapshot_count == 1 {
                                        "1 snap".to_string()
                                    } else {
                                        format!("{} snaps", item.snapshot_count)
                                    };
                                    let is_sel = selected_library_item
                                        .read()
                                        .as_ref()
                                        .map(|li| &li.preset_id) == Some(&item.preset_id);
                                    let item_click = item.clone();
                                    let mut handler = on_select_library_item.clone();
                                    rsx! {
                                        button {
                                            key: "{item.preset_id}",
                                            class: if is_sel {
                                                "w-full text-left px-3 py-1.5 border-b border-blue-700/40 bg-blue-900/30 border-l-2 border-l-blue-500"
                                            } else if item.is_template {
                                                "w-full text-left px-3 py-1.5 border-b border-zinc-800/30 opacity-40 border-l-2 border-l-transparent hover:opacity-70"
                                            } else {
                                                "w-full text-left px-3 py-1.5 border-b border-zinc-800/50 hover:bg-zinc-800/30 border-l-2 border-l-transparent"
                                            },
                                            onclick: move |_| handler(item_click.clone()),
                                            // Row 1: type badge + name + snap count
                                            div { class: "flex items-center gap-1.5",
                                                span {
                                                    class: "text-[9px] px-1 py-0.5 rounded flex-shrink-0",
                                                    style: format!(
                                                        "background-color: {}; color: {}; border: 1px solid {}",
                                                        color.bg, color.fg, color.border,
                                                    ),
                                                    "{bt_name}"
                                                }
                                                span {
                                                    class: if item.is_template {
                                                        "text-xs text-zinc-500 truncate flex-1"
                                                    } else {
                                                        "text-xs text-zinc-200 truncate flex-1"
                                                    },
                                                    "{item.name}"
                                                }
                                                span { class: "text-[10px] text-zinc-600 flex-shrink-0",
                                                    "{snap_label}"
                                                }
                                            }
                                            // Row 2: vendor + format + template label
                                            {
                                                let has_detail = item.vendor.is_some()
                                                    || item.format.is_some()
                                                    || item.is_template;
                                                rsx! {
                                                    if has_detail {
                                                        div { class: "flex items-center gap-1.5 pl-7 mt-0.5",
                                                            if let Some(ref vendor) = item.vendor {
                                                                span { class: "text-[9px] text-cyan-500/70 truncate",
                                                                    "{vendor}"
                                                                }
                                                            }
                                                            if let Some(ref fmt) = item.format {
                                                                span { class: "text-[9px] px-1 rounded bg-zinc-800/60 text-zinc-500",
                                                                    "{fmt}"
                                                                }
                                                            }
                                                            if item.is_template {
                                                                span { class: "text-[9px] text-zinc-600 italic",
                                                                    "template"
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
