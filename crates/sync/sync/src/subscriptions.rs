//! Local DAW subscription management.
//!
//! Spawns tasks that subscribe to each DAW domain stream for a given project,
//! wrap incoming events as [`SyncEvent`]s, and broadcast them through the engine.
//! Also manages project-level subscriptions to detect new/closed projects.

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use daw::service::{
    ItemEvent, MarkerEvent, ProjectEvent, RegionEvent, TakeEvent, TempoMapEvent, TrackEvent,
};
use daw::{Daw, Project, TransportState};
use sync_proto::{SyncConfig, SyncDomain, SyncEvent};
use tokio::sync::broadcast;
use tracing::{debug, info, warn};

/// Handle to a set of subscriptions for a single project.
///
/// Dropping this cancels all subscription tasks via the cancellation token.
pub struct ProjectSubscriptions {
    project_guid: String,
    cancel: tokio_util::sync::CancellationToken,
}

impl ProjectSubscriptions {
    /// Cancel all subscription tasks for this project.
    pub fn cancel(&self) {
        info!("Cancelling subscriptions for project {}", self.project_guid);
        self.cancel.cancel();
    }

    pub fn project_guid(&self) -> &str {
        &self.project_guid
    }
}

impl Drop for ProjectSubscriptions {
    fn drop(&mut self) {
        self.cancel.cancel();
    }
}

/// Context shared by all forwarder tasks.
struct ForwarderCtx {
    project_guid: String,
    peer_id: String,
    sequence: Arc<AtomicU64>,
    event_tx: broadcast::Sender<SyncEvent>,
    cancel: tokio_util::sync::CancellationToken,
}

impl ForwarderCtx {
    fn next_sequence(&self) -> u64 {
        self.sequence.fetch_add(1, Ordering::Relaxed)
    }

    fn send(&self, domain: SyncDomain) {
        let event = SyncEvent {
            origin_peer: self.peer_id.clone(),
            sequence: self.next_sequence(),
            project_guid: self.project_guid.clone(),
            domain,
        };
        let _ = self.event_tx.send(event);
    }
}

/// Subscribe to all enabled domain streams for a project and forward as SyncEvents.
///
/// Returns a handle that cancels all subscriptions when dropped.
pub async fn subscribe_project(
    _daw: &Daw,
    project: &Project,
    project_guid: String,
    peer_id: String,
    sequence: Arc<AtomicU64>,
    event_tx: broadcast::Sender<SyncEvent>,
    config: &SyncConfig,
) -> Result<ProjectSubscriptions, daw::Error> {
    let cancel = tokio_util::sync::CancellationToken::new();
    let short_guid = &project_guid[..8.min(project_guid.len())];

    let ctx = Arc::new(ForwarderCtx {
        project_guid: project_guid.clone(),
        peer_id,
        sequence,
        event_tx,
        cancel: cancel.clone(),
    });

    if config.transport {
        let mut rx = project.transport().subscribe_state().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.transport.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(state)) => {
                                let transport: TransportState = (*state).clone();
                                ctx.send(SyncDomain::Transport(transport));
                            }
                            Ok(None) => { debug!("transport stream ended"); break; }
                            Err(e) => { warn!("transport recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    if config.tracks {
        let mut rx = project.tracks().subscribe().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.tracks.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: TrackEvent = (*event).clone();
                                ctx.send(SyncDomain::Track(event));
                            }
                            Ok(None) => { debug!("track stream ended"); break; }
                            Err(e) => { warn!("track recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    if config.items {
        let mut rx = project.items().subscribe().await?;
        let ctx_items = ctx.clone();
        let name = format!("sync.sub.items.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx_items.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: ItemEvent = (*event).clone();
                                ctx_items.send(SyncDomain::Item(event));
                            }
                            Ok(None) => { debug!("item stream ended"); break; }
                            Err(e) => { warn!("item recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);

        let mut rx = project.items().subscribe_takes().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.takes.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: TakeEvent = (*event).clone();
                                ctx.send(SyncDomain::Take(event));
                            }
                            Ok(None) => { debug!("take stream ended"); break; }
                            Err(e) => { warn!("take recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    if config.tempo_map {
        let mut rx = project.tempo_map().subscribe().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.tempo.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: TempoMapEvent = (*event).clone();
                                ctx.send(SyncDomain::TempoMap(event));
                            }
                            Ok(None) => { debug!("tempo map stream ended"); break; }
                            Err(e) => { warn!("tempo map recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    if config.markers {
        let mut rx = project.markers().subscribe().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.markers.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: MarkerEvent = (*event).clone();
                                ctx.send(SyncDomain::Marker(event));
                            }
                            Ok(None) => { debug!("marker stream ended"); break; }
                            Err(e) => { warn!("marker recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    if config.regions {
        let mut rx = project.regions().subscribe().await?;
        let ctx = ctx.clone();
        let name = format!("sync.sub.regions.{short_guid}");
        moire::task::spawn(async move {
            loop {
                tokio::select! {
                    _ = ctx.cancel.cancelled() => break,
                    result = rx.recv() => {
                        match result {
                            Ok(Some(event)) => {
                                let event: RegionEvent = (*event).clone();
                                ctx.send(SyncDomain::Region(event));
                            }
                            Ok(None) => { debug!("region stream ended"); break; }
                            Err(e) => { warn!("region recv error: {e}"); break; }
                        }
                    }
                }
            }
        })
        .named(&name);
    }

    // Note: FX subscriptions are per-chain (per track), not per-project.
    // They'll be set up when we receive TrackEvent::Added or during initial sync.

    info!("Subscribed to all enabled domains for project {short_guid}");

    Ok(ProjectSubscriptions {
        project_guid,
        cancel,
    })
}

/// Subscribe to project lifecycle events and auto-subscribe new projects.
///
/// Returns a cancellation token that stops the project watcher when dropped.
pub fn watch_projects(
    daw: Daw,
    peer_id: String,
    sequence: Arc<AtomicU64>,
    event_tx: broadcast::Sender<SyncEvent>,
    config: SyncConfig,
    project_subs: Arc<moire::sync::Mutex<Vec<ProjectSubscriptions>>>,
) -> tokio_util::sync::CancellationToken {
    let cancel = tokio_util::sync::CancellationToken::new();
    let cancel_child = cancel.child_token();

    moire::task::spawn(async move {
        let mut rx = match daw.subscribe_projects().await {
            Ok(rx) => rx,
            Err(e) => {
                warn!("Failed to subscribe to project events: {e}");
                return;
            }
        };

        loop {
            tokio::select! {
                _ = cancel_child.cancelled() => {
                    debug!("Project watcher cancelled");
                    break;
                }
                result = rx.recv() => {
                    match result {
                        Ok(Some(event_ref)) => {
                            let event: ProjectEvent = (*event_ref).clone();

                            match &event {
                                ProjectEvent::Opened(info) => {
                                    info!("Project opened: {} — subscribing", info.name);
                                    match daw.project(&info.guid).await {
                                        Ok(project) => {
                                            match subscribe_project(
                                                &daw,
                                                &project,
                                                info.guid.clone(),
                                                peer_id.clone(),
                                                sequence.clone(),
                                                event_tx.clone(),
                                                &config,
                                            ).await {
                                                Ok(sub) => {
                                                    project_subs.lock().await.push(sub);
                                                }
                                                Err(e) => {
                                                    warn!("Failed to subscribe to project {}: {e}", info.guid);
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            warn!("Failed to get project handle for {}: {e}", info.guid);
                                        }
                                    }
                                }
                                ProjectEvent::Closed(guid) => {
                                    info!("Project closed: {guid} — removing subscriptions");
                                    let mut subs = project_subs.lock().await;
                                    subs.retain(|s| s.project_guid() != guid.as_str());
                                }
                                _ => {}
                            }

                            // Also forward project events through sync
                            let sync_event = SyncEvent {
                                origin_peer: peer_id.clone(),
                                sequence: sequence.fetch_add(1, Ordering::Relaxed),
                                project_guid: String::new(), // project events are global
                                domain: SyncDomain::Project(event),
                            };
                            let _ = event_tx.send(sync_event);
                        }
                        Ok(None) => {
                            debug!("Project event stream ended");
                            break;
                        }
                        Err(e) => {
                            warn!("Project event recv error: {e}");
                            break;
                        }
                    }
                }
            }
        }
    })
    .named("sync.project_watcher");

    cancel
}
