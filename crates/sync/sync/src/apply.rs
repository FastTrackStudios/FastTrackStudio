//! Applies remote sync events to the local DAW via daw-control mutations.
//!
//! Each [`SyncDomain`] variant is matched and translated into the appropriate
//! daw-control API call. The suppression set is updated before each mutation
//! so the next poll cycle knows to skip the resulting change.

use daw::Daw;
use daw::service::{
    FxChainContext, FxEvent, ItemEvent, MarkerEvent, ProjectContext, RegionEvent, RoutingEvent,
    TakeEvent, TempoMapEvent, TrackEvent, TrackRef, Transport,
    routing::RouteType,
};
use sync_proto::{SyncDomain, SyncEvent};
use tracing::{debug, info, warn};

use crate::drift::{DriftAction, DriftCorrector};
use crate::suppression::{SuppressionKey, SuppressionSet};

/// Apply a remote sync domain event to the local DAW.
///
/// Wraps mutations in a REAPER undo block so Ctrl+Z undoes the entire sync
/// operation as a single step (not individual API calls).
///
/// The caller is responsible for inserting suppression keys into the suppression set.
pub async fn apply_remote_event(
    daw: &Daw,
    project_guid: &str,
    domain: &SyncDomain,
    suppression: &mut SuppressionSet,
    drift_corrector: &mut DriftCorrector,
    link_active: bool,
    event_created_at_ms: u64,
) {
    // Remote project GUIDs are process-local (e.g., REAPER pointer-based).
    // If the exact GUID isn't found locally, fall back to the current project.
    let (resolved_guid, ctx) = if daw.project(project_guid).await.is_ok() {
        (project_guid.to_string(), ProjectContext::Project(project_guid.to_string()))
    } else {
        match daw.current_project().await {
            Ok(p) => match p.info().await {
                Ok(info) => (info.guid.clone(), ProjectContext::Project(info.guid)),
                Err(_) => (project_guid.to_string(), ProjectContext::Current),
            },
            Err(_) => (project_guid.to_string(), ProjectContext::Current),
        }
    };
    let project_guid = &resolved_guid;

    // Wrap in an undo block (best-effort — if project isn't found, skip)
    let label = format!("FTS Sync: {:?}", domain_label(domain));
    if let Ok(project) = daw.project(project_guid).await {
        let _ = project.begin_undo_block(&label).await;
    }

    match domain {
        SyncDomain::Transport(transport) => {
            apply_transport(daw, &ctx, project_guid, transport, suppression, drift_corrector, link_active, event_created_at_ms).await;
        }
        SyncDomain::Track(event) => {
            apply_track(daw, &ctx, event, suppression).await;
        }
        SyncDomain::Fx(event) => {
            apply_fx(daw, &ctx, event, suppression).await;
        }
        SyncDomain::Item(event) => {
            apply_item(daw, &ctx, event, suppression).await;
        }
        SyncDomain::Take(event) => {
            apply_take(daw, &ctx, event, suppression).await;
        }
        SyncDomain::Routing(event) => {
            apply_routing(daw, &ctx, event, suppression).await;
        }
        SyncDomain::TempoMap(event) => {
            apply_tempo_map(daw, &ctx, project_guid, event, suppression).await;
        }
        SyncDomain::Marker(event) => {
            apply_marker(daw, &ctx, project_guid, event, suppression).await;
        }
        SyncDomain::Region(event) => {
            apply_region(daw, &ctx, project_guid, event, suppression).await;
        }
        SyncDomain::Project(_event) => {
            // Project events (open/close) are informational — we don't
            // automatically open/close projects on remote peers.
            debug!("Received project event from remote peer (informational only)");
        }
    }

    // Close the undo block
    if let Ok(project) = daw.project(project_guid).await {
        let _ = project.end_undo_block(&label).await;
    }
}

/// Short label for undo block display.
fn domain_label(domain: &SyncDomain) -> &'static str {
    match domain {
        SyncDomain::Transport(_) => "Transport",
        SyncDomain::Track(_) => "Track",
        SyncDomain::Fx(_) => "FX",
        SyncDomain::Item(_) => "Item",
        SyncDomain::Take(_) => "Take",
        SyncDomain::Routing(_) => "Routing",
        SyncDomain::TempoMap(_) => "Tempo",
        SyncDomain::Marker(_) => "Marker",
        SyncDomain::Region(_) => "Region",
        SyncDomain::Project(_) => "Project",
    }
}

// ── Transport ────────────────────────────────────────────────────────────────

async fn apply_transport(
    daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    transport: &Transport,
    suppression: &mut SuppressionSet,
    drift_corrector: &mut DriftCorrector,
    link_active: bool,
    event_created_at_ms: u64,
) {
    let project = match daw.project(project_guid).await {
        Ok(p) => p,
        Err(e) => {
            warn!("Cannot apply transport: project {project_guid} not found: {e}");
            return;
        }
    };
    let t = project.transport();

    use daw::service::PlayState;

    // If both sides are stopped, sync edit cursor position changes without
    // triggering a full state transition (no suppression, no play state change).
    if transport.play_state == PlayState::Stopped {
        if let Ok(local) = t.get_state().await {
            if local.play_state == PlayState::Stopped {
                if let Some(ref edit_pos) = transport.edit_position.time {
                    let local_edit = local.edit_position.time
                        .as_ref()
                        .map(|t| t.as_seconds())
                        .unwrap_or(0.0);
                    let remote_edit = edit_pos.as_seconds();
                    if (remote_edit - local_edit).abs() > 0.001 {
                        debug!("Syncing edit cursor: {local_edit:.3}s → {remote_edit:.3}s");
                        let _ = t.set_position(remote_edit).await;
                    }
                }
                // Also sync tempo if changed
                if (transport.tempo.bpm - local.tempo.bpm).abs() > 0.001 {
                    let _ = t.set_tempo(transport.tempo.bpm).await;
                }
                return;
            }
        }
    }

    // Check if this is a drift-correction heartbeat (both sides playing).
    // When Link is active, skip drift correction entirely — Link handles
    // tempo and phase sync via its own playrate nudging mechanism.
    if transport.play_state == PlayState::Playing {
        if let Ok(local) = t.get_state().await {
            if local.play_state == PlayState::Playing {
                if link_active {
                    // Link is handling continuous sync — skip heartbeat drift correction
                    return;
                }

                // Both playing — drift correct via DriftCorrector (fallback when Link is off)
                //
                // Compensate for the time gap between reading the two positions:
                // - Master position was read at `event_created_at_ms`
                // - Local position was just read (via get_state above)
                // Both positions are stale, but the local position is more recent.
                // Adding the time gap to the master position aligns both to the
                // same effective timestamp.
                let local_read_at = SyncEvent::now_ms();
                if let (Some(ref remote_pos), Some(ref local_pos)) = (
                    transport.playhead_position.time.as_ref(),
                    local.playhead_position.time.as_ref(),
                ) {
                    let latency_compensation = if event_created_at_ms > 0 {
                        let elapsed_ms = local_read_at.saturating_sub(event_created_at_ms);
                        (elapsed_ms as f64) / 1000.0
                    } else {
                        0.0
                    };
                    let estimated_remote = remote_pos.as_seconds() + latency_compensation;
                    let drift = estimated_remote - local_pos.as_seconds();

                    match drift_corrector.correct(drift, local.playrate) {
                        DriftAction::SetRate { new_playrate } => {
                            debug!(
                                "Drift correction: drift={drift:.4}s, playrate → {new_playrate:.4}"
                            );
                            let _ = t.set_playrate(new_playrate).await;
                        }
                        DriftAction::Reset => {
                            debug!("Drift within tolerance, resetting playrate");
                            let _ = t.set_playrate(1.0).await;
                        }
                        DriftAction::HardSeek => {
                            info!(
                                "Hard seek to estimated position {estimated_remote:.3}s (drift={drift:.3}s)"
                            );
                            let _ = t.set_position(estimated_remote).await;
                            let _ = t.set_playrate(1.0).await;
                        }
                        DriftAction::None => {}
                    }
                }
                // Also sync tempo if it drifted
                if (transport.tempo.bpm - local.tempo.bpm).abs() > 0.001 {
                    info!(
                        "Syncing tempo: remote={:.1} local={:.1}",
                        transport.tempo.bpm, local.tempo.bpm
                    );
                    let _ = t.set_tempo(transport.tempo.bpm).await;
                }
                return;
            }
        }
    }

    // Full transport state transition (play/stop/pause) — suppress echo so
    // our subscription forwarder doesn't re-broadcast this change.
    suppression.suppress(SuppressionKey::transport(project_guid));

    // Reset drift corrector on transport state changes.
    drift_corrector.reset();

    // Apply position first, then play state, so the playhead is at the
    // right position when playback starts.
    let pos_secs = transport.playhead_position.time.as_ref().map(|t| t.as_seconds());
    if let Some(pos) = pos_secs {
        info!(
            "Transport state transition: setting position to {pos:.3}s before {:?}",
            transport.play_state
        );
        if let Err(e) = t.set_position(pos).await {
            warn!("Failed to set transport position: {e}");
        }
    } else {
        info!(
            "Transport state transition: no position in event, applying {:?}",
            transport.play_state
        );
    }

    if let Err(e) = t.set_tempo(transport.tempo.bpm).await {
        warn!("Failed to set tempo: {e}");
    }

    if let Err(e) = t.set_loop(transport.looping).await {
        warn!("Failed to set loop state: {e}");
    }

    // Reset playrate on transport state transitions
    let _ = t.set_playrate(1.0).await;

    // Play state is applied last to avoid race conditions
    match transport.play_state {
        PlayState::Playing => {
            info!("Applying remote play command (pos={pos_secs:?})");
            match tokio::time::timeout(
                std::time::Duration::from_secs(5),
                t.play(),
            ).await {
                Ok(Ok(_)) => info!("Play command completed successfully"),
                Ok(Err(e)) => warn!("Play command returned error: {e}"),
                Err(_) => warn!("Play command TIMED OUT after 5s"),
            }
        }
        PlayState::Paused => {
            info!("Applying remote pause command");
            let _ = t.pause().await;
        }
        PlayState::Stopped => {
            info!("Applying remote stop command");
            let _ = t.stop().await;
        }
        _ => {}
    }
}

// ── Track ────────────────────────────────────────────────────────────────────

async fn apply_track(
    daw: &Daw,
    ctx: &ProjectContext,
    event: &TrackEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        TrackEvent::VolumeChanged { guid, volume } => {
            suppression.suppress(SuppressionKey::track(guid, "volume"));
            apply_track_mutation(daw, ctx, guid, |handle| {
                let volume = *volume;
                Box::pin(async move { handle.set_volume(volume).await })
            })
            .await;
        }
        TrackEvent::PanChanged { guid, pan } => {
            suppression.suppress(SuppressionKey::track(guid, "pan"));
            apply_track_mutation(daw, ctx, guid, |handle| {
                let pan = *pan;
                Box::pin(async move { handle.set_pan(pan).await })
            })
            .await;
        }
        TrackEvent::MuteChanged { guid, muted } => {
            suppression.suppress(SuppressionKey::track(guid, "muted"));
            let muted = *muted;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if muted {
                        handle.mute().await
                    } else {
                        handle.unmute().await
                    }
                })
            })
            .await;
        }
        TrackEvent::SoloChanged { guid, soloed } => {
            suppression.suppress(SuppressionKey::track(guid, "soloed"));
            let soloed = *soloed;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if soloed {
                        handle.solo().await
                    } else {
                        handle.unsolo().await
                    }
                })
            })
            .await;
        }
        TrackEvent::ArmChanged { guid, armed } => {
            suppression.suppress(SuppressionKey::track(guid, "armed"));
            let armed = *armed;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if armed {
                        handle.arm().await
                    } else {
                        handle.disarm().await
                    }
                })
            })
            .await;
        }
        TrackEvent::Renamed { guid, name } => {
            suppression.suppress(SuppressionKey::track(guid, "name"));
            let name = name.clone();
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move { handle.rename(&name).await })
            })
            .await;
        }
        TrackEvent::ColorChanged { guid, color } => {
            suppression.suppress(SuppressionKey::track(guid, "color"));
            let color = color.unwrap_or(0);
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move { handle.set_color(color).await })
            })
            .await;
        }
        TrackEvent::SelectionChanged { guid, selected } => {
            suppression.suppress(SuppressionKey::track(guid, "selected"));
            let selected = *selected;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if selected {
                        handle.select().await
                    } else {
                        handle.deselect().await
                    }
                })
            })
            .await;
        }
        TrackEvent::TcpVisibilityChanged { guid, visible } => {
            suppression.suppress(SuppressionKey::track(guid, "tcp_visible"));
            let visible = *visible;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move { handle.set_visible_in_tcp(visible).await })
            })
            .await;
        }
        TrackEvent::MixerVisibilityChanged { guid, visible } => {
            suppression.suppress(SuppressionKey::track(guid, "mixer_visible"));
            let visible = *visible;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move { handle.set_visible_in_mixer(visible).await })
            })
            .await;
        }
        TrackEvent::Added(track) => {
            suppression.suppress(SuppressionKey::track(&track.guid, "added"));
            let project = match resolve_project(daw, ctx).await {
                Some(p) => p,
                None => return,
            };
            let tracks = project.tracks();
            // Add at the same index if possible
            if let Err(e) = tracks.add(&track.name, Some(track.index)).await {
                warn!("Failed to add track '{}': {e}", track.name);
            }
        }
        TrackEvent::Removed(guid) => {
            suppression.suppress(SuppressionKey::track(guid, "removed"));
            let project = match resolve_project(daw, ctx).await {
                Some(p) => p,
                None => return,
            };
            let tracks = project.tracks();
            if let Err(e) = tracks.remove(TrackRef::Guid(guid.clone())).await {
                warn!("Failed to remove track {guid}: {e}");
            }
        }
        TrackEvent::Moved {
            guid, new_index, ..
        } => {
            suppression.suppress(SuppressionKey::track(guid, "moved"));
            // Track reordering requires a dedicated API (not yet available in daw-control)
            debug!("Track {guid} moved to index {new_index} (reordering not yet wired)");
        }
    }
}

/// Helper to resolve a project and track, then apply a mutation.
async fn apply_track_mutation<F>(daw: &Daw, ctx: &ProjectContext, guid: &str, mutation: F)
where
    F: FnOnce(
        daw::TrackHandle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = daw::Result<()>> + Send>>,
{
    let project = match resolve_project(daw, ctx).await {
        Some(p) => p,
        None => return,
    };
    let handle = match project.tracks().by_guid(guid).await {
        Ok(Some(h)) => h,
        Ok(None) => {
            debug!("Track {guid} not found, skipping mutation");
            return;
        }
        Err(e) => {
            warn!("Failed to resolve track {guid}: {e}");
            return;
        }
    };
    if let Err(e) = mutation(handle).await {
        warn!("Track mutation failed for {guid}: {e}");
    }
}

// ── FX ───────────────────────────────────────────────────────────────────────

async fn apply_fx(
    daw: &Daw,
    ctx: &ProjectContext,
    event: &FxEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        FxEvent::ParameterChanged {
            context,
            fx_guid,
            param_index,
            value,
        } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(
                &context_key,
                fx_guid,
                *param_index,
            ));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Ok(Some(fx)) = chain.by_guid(fx_guid).await {
                    if let Err(e) = fx.param(*param_index).set(*value).await {
                        warn!("Failed to set FX param {param_index} on {fx_guid}: {e}");
                    }
                } else {
                    debug!("FX {fx_guid} not found in chain");
                }
            }
        }
        FxEvent::Added { context, fx } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, &fx.guid, 0));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                match chain.add(&fx.name).await {
                    Ok(handle) => {
                        if !fx.enabled {
                            let _ = handle.disable().await;
                        }
                    }
                    Err(e) => warn!("Failed to add FX '{}': {e}", fx.name),
                }
            }
        }
        FxEvent::Removed { context, fx_guid } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, 0));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Ok(Some(fx)) = chain.by_guid(fx_guid).await {
                    if let Err(e) = fx.remove().await {
                        warn!("Failed to remove FX {fx_guid}: {e}");
                    }
                }
            }
        }
        FxEvent::EnabledChanged {
            context,
            fx_guid,
            enabled,
        } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, 0));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Ok(Some(fx)) = chain.by_guid(fx_guid).await {
                    let result = if *enabled {
                        fx.enable().await
                    } else {
                        fx.disable().await
                    };
                    if let Err(e) = result {
                        warn!("Failed to set FX enabled={enabled} on {fx_guid}: {e}");
                    }
                }
            }
        }
        FxEvent::Moved {
            context,
            fx_guid,
            new_index,
            ..
        } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, 0));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Ok(Some(fx)) = chain.by_guid(fx_guid).await {
                    if let Err(e) = fx.move_to(*new_index).await {
                        warn!("Failed to move FX {fx_guid} to index {new_index}: {e}");
                    }
                }
            }
        }
        FxEvent::PresetChanged {
            context,
            fx_guid,
            preset_name,
        } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, 0));
            debug!(
                "FX preset changed: {fx_guid} → {:?} (preset sync requires index)",
                preset_name
            );
        }
        FxEvent::WindowChanged {
            context,
            fx_guid,
            open,
        } => {
            let context_key = format!("{context:?}");
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, 0));
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Ok(Some(fx)) = chain.by_guid(fx_guid).await {
                    let result = if *open {
                        fx.open_ui().await
                    } else {
                        fx.close_ui().await
                    };
                    if let Err(e) = result {
                        warn!("Failed to set FX window open={open} on {fx_guid}: {e}");
                    }
                }
            }
        }
        FxEvent::ContainerCreated {
            context,
            name,
            ..
        } => {
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                let count = chain.count().await.unwrap_or(0);
                if let Err(e) = chain.create_container(name, count).await {
                    warn!("Failed to create FX container '{name}': {e}");
                }
            }
        }
        FxEvent::ContainerRemoved {
            context,
            container_id,
        } => {
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Err(e) = chain.explode_container(container_id).await {
                    warn!("Failed to remove FX container: {e}");
                }
            }
        }
        FxEvent::RoutingModeChanged {
            context,
            container_id,
            mode,
        } => {
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Err(e) = chain.set_routing_mode(container_id, mode.clone()).await {
                    warn!("Failed to set container routing mode: {e}");
                }
            }
        }
        FxEvent::MovedToContainer {
            context,
            node_id,
            dest_container,
            ..
        } => {
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Err(e) = chain.move_to_container(node_id, dest_container, 0).await {
                    warn!("Failed to move FX to container: {e}");
                }
            }
        }
        FxEvent::ContainerRenamed {
            context,
            container_id,
            name,
        } => {
            if let Some(chain) = resolve_fx_chain(daw, ctx, context).await {
                if let Err(e) = chain.rename_container(container_id, name).await {
                    warn!("Failed to rename FX container: {e}");
                }
            }
        }
        FxEvent::TreeStructureChanged { context } => {
            debug!("FX tree structure changed for {context:?} (no automatic sync for bulk changes)");
        }
    }
}

/// Resolve an FxChainContext to an FxChain handle.
async fn resolve_fx_chain(
    daw: &Daw,
    ctx: &ProjectContext,
    fx_ctx: &FxChainContext,
) -> Option<daw::FxChain> {
    let project = resolve_project(daw, ctx).await?;
    let track_guid = match fx_ctx {
        FxChainContext::Track(guid) | FxChainContext::Input(guid) => guid,
        FxChainContext::Monitoring => {
            debug!("Monitoring FX chain sync not yet supported");
            return None;
        }
    };
    match project.tracks().by_guid(track_guid).await {
        Ok(Some(track)) => {
            let chain = match fx_ctx {
                FxChainContext::Track(_) => track.fx_chain(),
                FxChainContext::Input(_) => track.input_fx_chain(),
                _ => unreachable!(),
            };
            Some(chain)
        }
        Ok(None) => {
            debug!("Track {track_guid} not found for FX chain resolution");
            None
        }
        Err(e) => {
            warn!("Failed to resolve track {track_guid} for FX chain: {e}");
            None
        }
    }
}

// ── Item ─────────────────────────────────────────────────────────────────────

async fn apply_item(
    daw: &Daw,
    ctx: &ProjectContext,
    event: &ItemEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        ItemEvent::PositionChanged {
            item_guid,
            new_position,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "position"));
            apply_item_mutation(daw, ctx, item_guid, |handle| {
                let pos = *new_position;
                Box::pin(async move {
                    handle
                        .set_position(daw::service::primitives::PositionInSeconds::from_seconds(
                            pos,
                        ))
                        .await
                })
            })
            .await;
        }
        ItemEvent::LengthChanged {
            item_guid,
            new_length,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "length"));
            apply_item_mutation(daw, ctx, item_guid, |handle| {
                let len = *new_length;
                Box::pin(async move {
                    handle
                        .set_length(daw::service::primitives::Duration::from_seconds(len))
                        .await
                })
            })
            .await;
        }
        ItemEvent::MuteChanged {
            item_guid, muted, ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "muted"));
            let muted = *muted;
            apply_item_mutation(daw, ctx, item_guid, move |handle| {
                Box::pin(async move {
                    if muted {
                        handle.mute().await
                    } else {
                        handle.unmute().await
                    }
                })
            })
            .await;
        }
        ItemEvent::VolumeChanged {
            item_guid, volume, ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "volume"));
            let volume = *volume;
            apply_item_mutation(daw, ctx, item_guid, move |handle| {
                Box::pin(async move { handle.set_volume(volume).await })
            })
            .await;
        }
        ItemEvent::Created {
            track_guid, item, ..
        } => {
            suppression.suppress(SuppressionKey::item(&item.guid, "created"));
            if let Some(project) = resolve_project(daw, ctx).await {
                if let Ok(Some(track)) = project.tracks().by_guid(track_guid).await {
                    let pos = daw::service::primitives::PositionInSeconds::from_seconds(
                        item.position.as_seconds(),
                    );
                    let len =
                        daw::service::primitives::Duration::from_seconds(item.length.as_seconds());
                    if let Err(e) = track.items().add(pos, len).await {
                        warn!("Failed to add item on track {track_guid}: {e}");
                    }
                }
            }
        }
        ItemEvent::Deleted { item_guid, .. } => {
            suppression.suppress(SuppressionKey::item(item_guid, "deleted"));
            apply_item_mutation(daw, ctx, item_guid, |handle| {
                Box::pin(async move { handle.delete().await })
            })
            .await;
        }
        ItemEvent::MovedToTrack {
            item_guid,
            new_track_guid,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "moved_to_track"));
            // move_to_track exists in the service layer but not exposed through ItemHandle.
            // Log for now — this is a rare operation.
            debug!(
                "Item {item_guid} moved to track {new_track_guid} (cross-track move not yet wired)"
            );
        }
        ItemEvent::SelectionChanged {
            item_guid,
            selected,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "selected"));
            let selected = *selected;
            apply_item_mutation(daw, ctx, item_guid, move |handle| {
                Box::pin(async move {
                    if selected {
                        handle.select().await
                    } else {
                        handle.deselect().await
                    }
                })
            })
            .await;
        }
        ItemEvent::ActiveTakeChanged {
            item_guid,
            new_take_index,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "active_take"));
            let idx = *new_take_index;
            if let Some(project) = resolve_project(daw, ctx).await {
                if let Ok(Some(item)) = project.items().by_guid(item_guid).await {
                    if let Ok(Some(take)) = item.takes().by_index(idx).await {
                        if let Err(e) = take.make_active().await {
                            warn!("Failed to set active take {idx} on item {item_guid}: {e}");
                        }
                    }
                }
            }
        }
    }
}

/// Helper to resolve a project and item by GUID, then apply a mutation.
async fn apply_item_mutation<F>(daw: &Daw, ctx: &ProjectContext, item_guid: &str, mutation: F)
where
    F: FnOnce(
        daw::ItemHandle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = daw::Result<()>> + Send>>,
{
    let project = match resolve_project(daw, ctx).await {
        Some(p) => p,
        None => return,
    };
    let handle = match project.items().by_guid(item_guid).await {
        Ok(Some(h)) => h,
        Ok(None) => {
            debug!("Item {item_guid} not found, skipping mutation");
            return;
        }
        Err(e) => {
            warn!("Failed to resolve item {item_guid}: {e}");
            return;
        }
    };
    if let Err(e) = mutation(handle).await {
        warn!("Item mutation failed for {item_guid}: {e}");
    }
}

// ── Take ─────────────────────────────────────────────────────────────────────

async fn apply_take(
    daw: &Daw,
    ctx: &ProjectContext,
    event: &TakeEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        TakeEvent::NameChanged {
            item_guid,
            take_guid,
            name,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_name"));
            apply_take_mutation(daw, ctx, item_guid, take_guid, |handle| {
                let name = name.clone();
                Box::pin(async move { handle.set_name(&name).await })
            })
            .await;
        }
        TakeEvent::PitchChanged {
            item_guid,
            take_guid,
            pitch,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_pitch"));
            let pitch = *pitch;
            apply_take_mutation(daw, ctx, item_guid, take_guid, move |handle| {
                Box::pin(async move { handle.set_pitch(pitch).await })
            })
            .await;
        }
        TakeEvent::PlayRateChanged {
            item_guid,
            take_guid,
            play_rate,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_play_rate"));
            let rate = *play_rate;
            apply_take_mutation(daw, ctx, item_guid, take_guid, move |handle| {
                Box::pin(async move { handle.set_play_rate(rate).await })
            })
            .await;
        }
        TakeEvent::VolumeChanged {
            item_guid,
            take_guid,
            volume,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_volume"));
            let volume = *volume;
            apply_take_mutation(daw, ctx, item_guid, take_guid, move |handle| {
                Box::pin(async move { handle.set_volume(volume).await })
            })
            .await;
        }
        TakeEvent::SourceChanged {
            item_guid,
            take_guid,
            source_path,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_source"));
            if let Some(path) = source_path {
                let path = path.clone();
                apply_take_mutation(daw, ctx, item_guid, take_guid, move |handle| {
                    Box::pin(async move { handle.set_source_file(&path).await })
                })
                .await;
            }
        }
        TakeEvent::Created {
            item_guid, ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_created"));
            if let Some(project) = resolve_project(daw, ctx).await {
                if let Ok(Some(item)) = project.items().by_guid(item_guid).await {
                    if let Err(e) = item.takes().add().await {
                        warn!("Failed to add take to item {item_guid}: {e}");
                    }
                }
            }
        }
        TakeEvent::Deleted {
            item_guid,
            take_guid,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "take_deleted"));
            apply_take_mutation(daw, ctx, item_guid, take_guid, |handle| {
                Box::pin(async move { handle.delete().await })
            })
            .await;
        }
    }
}

/// Helper to resolve a project, item, and take by GUIDs, then apply a mutation.
async fn apply_take_mutation<F>(
    daw: &Daw,
    ctx: &ProjectContext,
    item_guid: &str,
    take_guid: &str,
    mutation: F,
) where
    F: FnOnce(
        daw::TakeHandle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = daw::Result<()>> + Send>>,
{
    let project = match resolve_project(daw, ctx).await {
        Some(p) => p,
        None => return,
    };
    let item = match project.items().by_guid(item_guid).await {
        Ok(Some(i)) => i,
        Ok(None) => {
            debug!("Item {item_guid} not found for take mutation");
            return;
        }
        Err(e) => {
            warn!("Failed to resolve item {item_guid}: {e}");
            return;
        }
    };
    // Find the take by GUID — iterate all takes and match by index
    let takes = match item.takes().all().await {
        Ok(t) => t,
        Err(e) => {
            warn!("Failed to get takes for item {item_guid}: {e}");
            return;
        }
    };
    let take_index = takes
        .iter()
        .enumerate()
        .find(|(_, t)| t.guid == take_guid)
        .map(|(i, _)| i as u32);
    let Some(idx) = take_index else {
        debug!("Take {take_guid} not found in item {item_guid}");
        return;
    };
    let take_handle = match item.takes().by_index(idx).await {
        Ok(Some(h)) => h,
        _ => {
            debug!("Take at index {idx} not found for item {item_guid}");
            return;
        }
    };
    if let Err(e) = mutation(take_handle).await {
        warn!("Take mutation failed for {take_guid} on item {item_guid}: {e}");
    }
}

// ── Routing ──────────────────────────────────────────────────────────────────

async fn apply_routing(
    daw: &Daw,
    ctx: &ProjectContext,
    event: &RoutingEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        RoutingEvent::VolumeChanged {
            source_track_guid,
            route_type,
            route_index,
            volume,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("volume:{route_index}"),
            ));
            apply_route_mutation(daw, ctx, source_track_guid, *route_type, *route_index, |handle| {
                let volume = *volume;
                Box::pin(async move { handle.set_volume(volume).await })
            })
            .await;
        }
        RoutingEvent::PanChanged {
            source_track_guid,
            route_type,
            route_index,
            pan,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("pan:{route_index}"),
            ));
            apply_route_mutation(daw, ctx, source_track_guid, *route_type, *route_index, |handle| {
                let pan = *pan;
                Box::pin(async move { handle.set_pan(pan).await })
            })
            .await;
        }
        RoutingEvent::MuteChanged {
            source_track_guid,
            route_type,
            route_index,
            muted,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("mute:{route_index}"),
            ));
            let muted = *muted;
            apply_route_mutation(daw, ctx, source_track_guid, *route_type, *route_index, move |handle| {
                Box::pin(async move {
                    if muted {
                        handle.mute().await
                    } else {
                        handle.unmute().await
                    }
                })
            })
            .await;
        }
        RoutingEvent::RouteCreated {
            source_track_guid,
            route,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("created:{}", route.index),
            ));
            if let Some(project) = resolve_project(daw, ctx).await {
                if let Ok(Some(track)) = project.tracks().by_guid(source_track_guid).await {
                    match route.route_type {
                        RouteType::Send => {
                            if let Some(ref dest_guid) = route.dest_track_guid {
                                if let Err(e) = track.sends().add_to(dest_guid).await {
                                    warn!("Failed to create send from {source_track_guid} to {dest_guid}: {e}");
                                }
                            }
                        }
                        RouteType::HardwareOutput => {
                            if let Some(hw_idx) = route.hw_output_index {
                                if let Err(e) = track.hardware_outputs().add(hw_idx).await {
                                    warn!("Failed to add hardware output {hw_idx}: {e}");
                                }
                            }
                        }
                        RouteType::Receive => {
                            // Receives are the inverse of sends — creating a receive on track A
                            // from track B is the same as creating a send on track B to track A.
                            debug!("Receive creation not directly supported — handled via send creation on source");
                        }
                    }
                }
            }
        }
        RoutingEvent::RouteDeleted {
            source_track_guid,
            route_type,
            route_index,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("deleted:{route_index}"),
            ));
            apply_route_mutation(daw, ctx, source_track_guid, *route_type, *route_index, |handle| {
                Box::pin(async move { handle.remove().await })
            })
            .await;
        }
        RoutingEvent::ParentSendChanged {
            track_guid,
            enabled,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(track_guid, "parent_send"));
            if let Some(project) = resolve_project(daw, ctx).await {
                if let Ok(Some(track)) = project.tracks().by_guid(track_guid).await {
                    if let Err(e) = track.set_parent_send(*enabled).await {
                        warn!("Failed to set parent send on {track_guid}: {e}");
                    }
                }
            }
        }
    }
}

/// Helper to resolve a route handle and apply a mutation.
async fn apply_route_mutation<F>(
    daw: &Daw,
    ctx: &ProjectContext,
    track_guid: &str,
    route_type: RouteType,
    route_index: u32,
    mutation: F,
) where
    F: FnOnce(
        daw::RouteHandle,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = daw::Result<()>> + Send>>,
{
    let project = match resolve_project(daw, ctx).await {
        Some(p) => p,
        None => return,
    };
    let track = match project.tracks().by_guid(track_guid).await {
        Ok(Some(t)) => t,
        Ok(None) => {
            debug!("Track {track_guid} not found for route mutation");
            return;
        }
        Err(e) => {
            warn!("Failed to resolve track {track_guid} for route: {e}");
            return;
        }
    };
    let route_handle = match route_type {
        RouteType::Send => track.sends().by_index(route_index).await,
        RouteType::Receive => track.receives().by_index(route_index).await,
        RouteType::HardwareOutput => track.hardware_outputs().by_index(route_index).await,
    };
    match route_handle {
        Ok(Some(handle)) => {
            if let Err(e) = mutation(handle).await {
                warn!("Route mutation failed on {track_guid}[{route_index}]: {e}");
            }
        }
        Ok(None) => {
            debug!("Route {route_type:?}[{route_index}] not found on track {track_guid}");
        }
        Err(e) => {
            warn!("Failed to resolve route {route_type:?}[{route_index}] on {track_guid}: {e}");
        }
    }
}

// ── Tempo Map ────────────────────────────────────────────────────────────────

async fn apply_tempo_map(
    daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &TempoMapEvent,
    suppression: &mut SuppressionSet,
) {
    suppression.suppress(SuppressionKey::tempo_map(project_guid));

    let project = match daw.project(project_guid).await {
        Ok(p) => p,
        Err(_) => return,
    };
    let tempo_map = project.tempo_map();

    match event {
        TempoMapEvent::PointAdded(point) => {
            let pos_secs = point
                .position
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);
            match tempo_map.add_point(pos_secs, point.bpm).await {
                Ok(idx) => {
                    if let Some(ref ts) = point.time_signature {
                        let _ = tempo_map
                            .set_time_signature_at(idx, ts.numerator as i32, ts.denominator as i32)
                            .await;
                    }
                }
                Err(e) => warn!("Failed to add tempo point at {pos_secs:.1}s: {e}"),
            }
        }
        TempoMapEvent::PointRemoved(index) => {
            if let Err(e) = tempo_map.remove_point(*index).await {
                warn!("Failed to remove tempo point at index {index}: {e}");
            }
        }
        TempoMapEvent::PointChanged(point) => {
            // Match by position — find the closest existing point
            let pos_secs = point
                .position
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);
            if let Ok(points) = tempo_map.points().await {
                let closest = points
                    .iter()
                    .enumerate()
                    .min_by(|(_, a), (_, b)| {
                        let a_pos = a.position.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0);
                        let b_pos = b.position.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0);
                        (a_pos - pos_secs)
                            .abs()
                            .partial_cmp(&(b_pos - pos_secs).abs())
                            .unwrap_or(std::cmp::Ordering::Equal)
                    })
                    .map(|(i, _)| i as u32);
                if let Some(idx) = closest {
                    let _ = tempo_map.set_tempo_at(idx, point.bpm).await;
                    if let Some(ref ts) = point.time_signature {
                        let _ = tempo_map
                            .set_time_signature_at(idx, ts.numerator as i32, ts.denominator as i32)
                            .await;
                    }
                }
            }
        }
        TempoMapEvent::MapChanged(points) => {
            // Full map replacement — remove all existing points, then add new ones.
            // Keep point 0 (default tempo) and update it, then add the rest.
            if let Ok(existing) = tempo_map.points().await {
                // Remove from end to avoid index shifting
                for i in (1..existing.len()).rev() {
                    let _ = tempo_map.remove_point(i as u32).await;
                }
            }
            // Update point 0 if it exists in the new map
            if let Some(first) = points.first() {
                let _ = tempo_map.set_default_tempo(first.bpm).await;
                if let Some(ref ts) = first.time_signature {
                    let _ = tempo_map
                        .set_default_time_signature(ts.numerator as i32, ts.denominator as i32)
                        .await;
                }
            }
            // Add remaining points
            for point in points.iter().skip(1) {
                let pos_secs = point
                    .position
                    .time
                    .as_ref()
                    .map(|t| t.as_seconds())
                    .unwrap_or(0.0);
                if let Ok(idx) = tempo_map.add_point(pos_secs, point.bpm).await {
                    if let Some(ref ts) = point.time_signature {
                        let _ = tempo_map
                            .set_time_signature_at(idx, ts.numerator as i32, ts.denominator as i32)
                            .await;
                    }
                }
            }
        }
    }
}

// ── Marker ───────────────────────────────────────────────────────────────────

async fn apply_marker(
    daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &MarkerEvent,
    suppression: &mut SuppressionSet,
) {
    let project = match daw.project(project_guid).await {
        Ok(p) => p,
        Err(_) => return,
    };
    let markers = project.markers();

    match event {
        MarkerEvent::Added(marker) => {
            if let Some(id) = marker.id {
                suppression.suppress(SuppressionKey::marker(project_guid, id));
            }
            let pos = marker
                .position
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0);
            if let Err(e) = markers.add(pos, &marker.name).await {
                warn!("Failed to add marker '{}': {e}", marker.name);
            }
        }
        MarkerEvent::Changed(marker) => {
            if let Some(id) = marker.id {
                suppression.suppress(SuppressionKey::marker(project_guid, id));
                let pos = marker
                    .position
                    .time
                    .as_ref()
                    .map(|t| t.as_seconds())
                    .unwrap_or(0.0);
                let _ = markers.move_to(id, pos).await;
                let _ = markers.rename(id, &marker.name).await;
                if let Some(color) = marker.color {
                    let _ = markers.set_color(id, color).await;
                }
            }
        }
        MarkerEvent::Removed(id) => {
            suppression.suppress(SuppressionKey::marker(project_guid, *id));
            let _ = markers.remove(*id).await;
        }
        MarkerEvent::MarkersChanged(new_markers) => {
            // Bulk marker replacement — remove all existing, then add new ones
            if let Ok(existing) = markers.all().await {
                for m in existing.iter().rev() {
                    if let Some(id) = m.id {
                        suppression.suppress(SuppressionKey::marker(project_guid, id));
                        let _ = markers.remove(id).await;
                    }
                }
            }
            for marker in new_markers {
                let pos = marker
                    .position
                    .time
                    .as_ref()
                    .map(|t| t.as_seconds())
                    .unwrap_or(0.0);
                let _ = markers.add(pos, &marker.name).await;
            }
        }
    }
}

// ── Region ───────────────────────────────────────────────────────────────────

async fn apply_region(
    daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &RegionEvent,
    suppression: &mut SuppressionSet,
) {
    let project = match daw.project(project_guid).await {
        Ok(p) => p,
        Err(_) => return,
    };
    let regions = project.regions();

    match event {
        RegionEvent::Added(region) => {
            if let Some(id) = region.id {
                suppression.suppress(SuppressionKey::region(project_guid, id));
            }
            let start = region.time_range.start_seconds();
            let end = region.time_range.end_seconds();
            if let Err(e) = regions.add(start, end, &region.name).await {
                warn!("Failed to add region '{}': {e}", region.name);
            }
        }
        RegionEvent::Changed(region) => {
            if let Some(id) = region.id {
                suppression.suppress(SuppressionKey::region(project_guid, id));
                let start = region.time_range.start_seconds();
                let end = region.time_range.end_seconds();
                let _ = regions.set_bounds(id, start, end).await;
                let _ = regions.rename(id, &region.name).await;
                if let Some(color) = region.color {
                    let _ = regions.set_color(id, color).await;
                }
            }
        }
        RegionEvent::Removed(id) => {
            suppression.suppress(SuppressionKey::region(project_guid, *id));
            let _ = regions.remove(*id).await;
        }
        RegionEvent::RegionsChanged(new_regions) => {
            // Bulk region replacement — remove all existing, then add new ones
            if let Ok(existing) = regions.all().await {
                for r in existing.iter().rev() {
                    if let Some(id) = r.id {
                        suppression.suppress(SuppressionKey::region(project_guid, id));
                        let _ = regions.remove(id).await;
                    }
                }
            }
            for region in new_regions {
                let start = region.time_range.start_seconds();
                let end = region.time_range.end_seconds();
                let _ = regions.add(start, end, &region.name).await;
            }
        }
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

async fn resolve_project(daw: &Daw, ctx: &ProjectContext) -> Option<daw::Project> {
    match ctx {
        ProjectContext::Project(guid) => match daw.project(guid.as_str()).await {
            Ok(p) => Some(p),
            Err(e) => {
                debug!("Project {guid} not found locally: {e}");
                None
            }
        },
        ProjectContext::Current => match daw.current_project().await {
            Ok(p) => Some(p),
            Err(e) => {
                warn!("Failed to get current project: {e}");
                None
            }
        },
    }
}
