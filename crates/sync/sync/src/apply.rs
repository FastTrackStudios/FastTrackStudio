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
use sync_proto::SyncDomain;
use tracing::{debug, warn};

use crate::suppression::{SuppressionKey, SuppressionSet};

/// Apply a remote sync domain event to the local DAW.
///
/// Returns suppression keys that should be recorded to prevent echo.
/// The caller is responsible for inserting these into the suppression set.
pub async fn apply_remote_event(
    daw: &Daw,
    project_guid: &str,
    domain: &SyncDomain,
    suppression: &mut SuppressionSet,
) {
    let ctx = ProjectContext::Project(project_guid.to_string());

    match domain {
        SyncDomain::Transport(transport) => {
            apply_transport(daw, &ctx, project_guid, transport, suppression).await;
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
}

// ── Transport ────────────────────────────────────────────────────────────────

async fn apply_transport(
    daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    transport: &Transport,
    suppression: &mut SuppressionSet,
) {
    suppression.suppress(SuppressionKey::transport(project_guid));

    let project = match daw.project(project_guid).await {
        Ok(p) => p,
        Err(e) => {
            warn!("Cannot apply transport: project {project_guid} not found: {e}");
            return;
        }
    };
    let t = project.transport();

    // Apply play state
    // Note: we apply position first, then play state, so the playhead
    // is at the right position when playback starts
    if let Some(ref pos) = transport.playhead_position.time {
        if let Err(e) = t.set_position(pos.as_seconds()).await {
            warn!("Failed to set transport position: {e}");
        }
    }

    if let Err(e) = t.set_tempo(transport.tempo.bpm).await {
        warn!("Failed to set tempo: {e}");
    }

    if let Err(e) = t.set_loop(transport.looping).await {
        warn!("Failed to set loop state: {e}");
    }

    // Play state is applied last to avoid race conditions
    use daw::service::PlayState;
    match transport.play_state {
        PlayState::Playing => {
            let _ = t.play().await;
        }
        PlayState::Paused => {
            let _ = t.pause().await;
        }
        PlayState::Stopped => {
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
                    if muted { handle.mute().await } else { handle.unmute().await }
                })
            })
            .await;
        }
        TrackEvent::SoloChanged { guid, soloed } => {
            suppression.suppress(SuppressionKey::track(guid, "soloed"));
            let soloed = *soloed;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if soloed { handle.solo().await } else { handle.unsolo().await }
                })
            })
            .await;
        }
        TrackEvent::ArmChanged { guid, armed } => {
            suppression.suppress(SuppressionKey::track(guid, "armed"));
            let armed = *armed;
            apply_track_mutation(daw, ctx, guid, move |handle| {
                Box::pin(async move {
                    if armed { handle.arm().await } else { handle.disarm().await }
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
                    if selected { handle.select().await } else { handle.deselect().await }
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
        TrackEvent::Moved { guid, new_index, .. } => {
            // Track reordering is complex — log for now, implement later
            debug!("Track {guid} moved to index {new_index} (reordering not yet applied)");
        }
    }
}

/// Helper to resolve a project and track, then apply a mutation.
async fn apply_track_mutation<F>(
    daw: &Daw,
    ctx: &ProjectContext,
    guid: &str,
    mutation: F,
) where
    F: FnOnce(daw::TrackHandle) -> std::pin::Pin<Box<dyn std::future::Future<Output = daw::Result<()>> + Send>>,
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
            suppression.suppress(SuppressionKey::fx_param(&context_key, fx_guid, *param_index));
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
    _daw: &Daw,
    _ctx: &ProjectContext,
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
            debug!("Item {item_guid} position → {new_position} (apply TBD)");
            // TODO: daw.project().items().by_guid().set_position()
        }
        ItemEvent::LengthChanged {
            item_guid,
            new_length,
            ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "length"));
            debug!("Item {item_guid} length → {new_length} (apply TBD)");
        }
        ItemEvent::MuteChanged {
            item_guid, muted, ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "muted"));
            debug!("Item {item_guid} mute → {muted} (apply TBD)");
        }
        ItemEvent::VolumeChanged {
            item_guid, volume, ..
        } => {
            suppression.suppress(SuppressionKey::item(item_guid, "volume"));
            debug!("Item {item_guid} volume → {volume} (apply TBD)");
        }
        _ => {
            debug!("Item event not yet handled for sync apply: {event:?}");
        }
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
            debug!("Routing volume change on {source_track_guid}[{route_index}] → {volume} (apply TBD)");
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
            debug!("Tempo point changed at {:?} → {:.1} BPM (positional matching TBD)", point.position, point.bpm);
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
    _daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &MarkerEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        MarkerEvent::Changed(marker) => {
            if let Some(id) = marker.id {
                suppression.suppress(SuppressionKey::marker(project_guid, id));
            }
            debug!("Marker changed: {:?} (apply TBD)", marker.name);
        }
        MarkerEvent::Added(marker) => {
            if let Some(id) = marker.id {
                suppression.suppress(SuppressionKey::marker(project_guid, id));
            }
            debug!("Marker added: {:?} (apply TBD)", marker.name);
        }
        MarkerEvent::Removed(id) => {
            suppression.suppress(SuppressionKey::marker(project_guid, *id));
            debug!("Marker removed: {id} (apply TBD)");
        }
        _ => {
            debug!("Marker event not yet handled: {event:?}");
        }
    }
}

// ── Region ───────────────────────────────────────────────────────────────────

async fn apply_region(
    _daw: &Daw,
    _ctx: &ProjectContext,
    project_guid: &str,
    event: &RegionEvent,
    suppression: &mut SuppressionSet,
) {
    match event {
        RegionEvent::Changed(region) => {
            if let Some(id) = region.id {
                suppression.suppress(SuppressionKey::region(project_guid, id));
            }
            debug!("Region changed: {:?} (apply TBD)", region.name);
        }
        RegionEvent::Added(region) => {
            if let Some(id) = region.id {
                suppression.suppress(SuppressionKey::region(project_guid, id));
            }
            debug!("Region added: {:?} (apply TBD)", region.name);
        }
        RegionEvent::Removed(id) => {
            suppression.suppress(SuppressionKey::region(project_guid, *id));
            debug!("Region removed: {id} (apply TBD)");
        }
        _ => {
            debug!("Region event not yet handled: {event:?}");
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
