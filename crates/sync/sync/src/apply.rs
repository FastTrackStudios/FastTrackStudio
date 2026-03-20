//! Applies remote sync events to the local DAW via daw-control mutations.
//!
//! Each [`SyncDomain`] variant is matched and translated into the appropriate
//! daw-control API call. The suppression set is updated before each mutation
//! so the next poll cycle knows to skip the resulting change.

use daw::Daw;
use daw::service::{
    FxEvent, ItemEvent, MarkerEvent, ProjectContext, RegionEvent, RoutingEvent, TakeEvent,
    TempoMapEvent, TrackEvent, TrackRef, Transport,
};
use session::offset_map::SetlistOffsetMap;
use sync_proto::SyncDomain;
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
            apply_transport(daw, &ctx, project_guid, transport, suppression, drift_corrector, link_active).await;
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
                if let (Some(ref remote_pos), Some(ref local_pos)) = (
                    transport.playhead_position.time.as_ref(),
                    local.playhead_position.time.as_ref(),
                ) {
                    let drift = remote_pos.as_seconds() - local_pos.as_seconds();

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
                                "Hard seek to master position {:.3}s (drift={drift:.3}s)",
                                remote_pos.as_seconds()
                            );
                            let _ = t.set_position(remote_pos.as_seconds()).await;
                            let _ = t.set_playrate(1.0).await;
                        }
                        DriftAction::None => {}
                    }
                }
                // Also sync tempo if it drifted
                if (transport.tempo.bpm - local.tempo.bpm).abs() > 0.001 {
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
            let _ = t.play().await;
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
            // Track reordering is complex — log for now, implement later
            debug!("Track {guid} moved to index {new_index} (reordering not yet applied)");
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
    _daw: &Daw,
    _ctx: &ProjectContext,
    event: &FxEvent,
    suppression: &mut SuppressionSet,
) {
    // FX events are complex — parameter changes, enable/disable, add/remove, presets.
    // Initial implementation handles the most common case: parameter changes.
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
            debug!(
                "FX param change: context={context:?} fx={fx_guid} param={param_index} value={value} (apply TBD)"
            );
            // TODO: Resolve fx chain context → fx handle → set_parameter
            // This requires matching FxChainContext to get track GUID + chain type,
            // then resolving through daw-control's FxChain API.
        }
        _ => {
            debug!("FX event not yet handled for sync apply: {event:?}");
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
        _ => {
            debug!("Item event not yet handled for sync apply: {event:?}");
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
    _daw: &Daw,
    _ctx: &ProjectContext,
    event: &TakeEvent,
    _suppression: &mut SuppressionSet,
) {
    debug!("Take event not yet handled for sync apply: {event:?}");
}

// ── Routing ──────────────────────────────────────────────────────────────────

async fn apply_routing(
    _daw: &Daw,
    _ctx: &ProjectContext,
    event: &RoutingEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        RoutingEvent::VolumeChanged {
            source_track_guid,
            route_index,
            volume,
            ..
        } => {
            suppression.suppress(SuppressionKey::routing(
                source_track_guid,
                &format!("volume:{route_index}"),
            ));
            debug!(
                "Routing volume change on {source_track_guid}[{route_index}] → {volume} (apply TBD)"
            );
        }
        _ => {
            debug!("Routing event not yet handled for sync apply: {event:?}");
        }
    }
}

// ── Tempo Map ────────────────────────────────────────────────────────────────

async fn apply_tempo_map(
    _daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &TempoMapEvent,
    suppression: &mut SuppressionSet,
) {
    suppression.suppress(SuppressionKey::tempo_map(project_guid));

    match event {
        TempoMapEvent::PointChanged(point) => {
            // PointChanged carries the full TempoPoint but not an index.
            // We'd need to match by position to find the right index to update.
            // For now, log it — full tempo map sync will use MapChanged.
            debug!(
                "Tempo point changed at {:?} → {:.1} BPM (positional matching TBD)",
                point.position, point.bpm
            );
        }
        TempoMapEvent::MapChanged(_points) => {
            // Full map replacement — complex, defer
            debug!("Tempo map full replacement not yet implemented");
        }
        _ => {
            debug!("Tempo map event not yet handled: {event:?}");
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
        _ => {}
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
        _ => {}
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
