//! Sync engine — orchestrates local event collection, remote forwarding, and event application.
//!
//! The engine subscribes to all local DAW change streams, wraps them in [`SyncEvent`]
//! envelopes, and forwards them to connected peers. It also receives remote events
//! and applies them via the [`apply`] module, with echo suppression.

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use daw::Daw;
use sync_proto::{SyncConfig, SyncDomain, SyncEvent, SyncPeer, SyncSession, SyncStatus};
use tokio::sync::broadcast;
use tracing::{debug, info, warn};

use crate::apply;
use crate::subscriptions::{self, ProjectSubscriptions};
use crate::suppression::SuppressionSet;

/// The sync engine. Owns the session state, event routing, and suppression logic.
pub struct Engine {
    /// The local DAW connection used for subscribing and mutating.
    daw: Daw,
    /// This peer's identity within the sync session.
    session: SyncSession,
    /// Current configuration (what domains to sync).
    config: SyncConfig,
    /// Monotonically increasing sequence number for outgoing events.
    sequence: Arc<AtomicU64>,
    /// Broadcast channel for distributing sync events to subscribers.
    event_tx: broadcast::Sender<SyncEvent>,
    /// Echo suppression set.
    suppression: Arc<moire::sync::Mutex<SuppressionSet>>,
    /// Current sync status.
    status: moire::sync::Mutex<SyncStatus>,
    /// Connected peers.
    peers: moire::sync::Mutex<Vec<SyncPeer>>,
    /// Active per-project subscriptions.
    project_subs: Arc<moire::sync::Mutex<Vec<ProjectSubscriptions>>>,
    /// Cancellation token for the project watcher task.
    project_watcher: moire::sync::Mutex<Option<tokio_util::sync::CancellationToken>>,
}

impl Engine {
    /// Create a new sync engine.
    ///
    /// The engine is created in `Disconnected` state. Call [`start`] to
    /// subscribe to all DAW streams and begin syncing.
    pub fn new(daw: Daw, session: SyncSession, config: SyncConfig) -> Self {
        let (event_tx, _) = broadcast::channel(4096);

        Self {
            daw,
            session,
            config,
            sequence: Arc::new(AtomicU64::new(0)),
            event_tx,
            suppression: Arc::new(moire::sync::Mutex::new(
                "sync.suppression",
                SuppressionSet::new(),
            )),
            status: moire::sync::Mutex::new("sync.status", SyncStatus::Disconnected),
            peers: moire::sync::Mutex::new("sync.peers", Vec::new()),
            project_subs: Arc::new(moire::sync::Mutex::new("sync.project_subs", Vec::new())),
            project_watcher: moire::sync::Mutex::new("sync.project_watcher", None),
        }
    }

    /// Start the sync engine — subscribe to all open projects and watch for new ones.
    ///
    /// This subscribes to every enabled domain stream for each currently open project,
    /// and starts a project watcher that auto-subscribes new projects as they open.
    pub async fn start(&self) -> Result<(), daw::Error> {
        *self.status.lock().await = SyncStatus::Connecting;

        // Subscribe to all currently open projects
        let projects = self.daw.projects().await?;
        for project in &projects {
            let info = project.info().await?;
            match subscriptions::subscribe_project(
                &self.daw,
                project,
                info.guid.clone(),
                self.session.peer_id.clone(),
                self.sequence.clone(),
                self.event_tx.clone(),
                &self.config,
            )
            .await
            {
                Ok(sub) => {
                    self.project_subs.lock().await.push(sub);
                }
                Err(e) => {
                    warn!("Failed to subscribe to project {}: {e}", info.guid);
                }
            }
        }

        // Start project watcher for auto-subscribing new projects
        let watcher_cancel = subscriptions::watch_projects(
            self.daw.clone(),
            self.session.peer_id.clone(),
            self.sequence.clone(),
            self.event_tx.clone(),
            self.config.clone(),
            self.project_subs.clone(),
        );
        *self.project_watcher.lock().await = Some(watcher_cancel);

        // Start the suppression GC ticker
        let suppression = Arc::clone(&self.suppression);
        moire::task::spawn(async move {
            loop {
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
                suppression.lock().await.gc();
            }
        })
        .named("sync.suppression_gc");

        *self.status.lock().await = SyncStatus::Connected;
        info!(
            "Sync engine started — subscribed to {} project(s)",
            projects.len()
        );

        Ok(())
    }

    /// Stop the sync engine — cancel all subscriptions.
    pub async fn stop(&self) {
        // Cancel project watcher
        if let Some(cancel) = self.project_watcher.lock().await.take() {
            cancel.cancel();
        }

        // Cancel all project subscriptions
        let mut subs = self.project_subs.lock().await;
        for sub in subs.drain(..) {
            sub.cancel();
        }

        *self.status.lock().await = SyncStatus::Disconnected;
        info!("Sync engine stopped");
    }

    /// Get the current sync status.
    pub async fn status(&self) -> SyncStatus {
        self.status.lock().await.clone()
    }

    /// Get the list of connected peers.
    pub async fn peers(&self) -> Vec<SyncPeer> {
        self.peers.lock().await.clone()
    }

    /// Get the local peer ID.
    pub fn peer_id(&self) -> &str {
        &self.session.peer_id
    }

    /// Update the sync configuration.
    pub async fn update_config(&self, config: SyncConfig) {
        info!("Sync config updated");
        let _ = config; // TODO: restart subscriptions with new config
    }

    /// Get the next sequence number for an outgoing event.
    fn next_sequence(&self) -> u64 {
        self.sequence.fetch_add(1, Ordering::Relaxed)
    }

    /// Wrap a domain event as a SyncEvent with this peer's identity.
    pub fn wrap_event(&self, project_guid: String, domain: SyncDomain) -> SyncEvent {
        SyncEvent {
            origin_peer: self.session.peer_id.clone(),
            sequence: self.next_sequence(),
            project_guid,
            domain,
        }
    }

    /// Broadcast a locally-detected change to all subscribers (remote peers).
    ///
    /// Returns `false` if the event was suppressed (echo from a remote apply).
    pub async fn broadcast_local(&self, event: SyncEvent) -> bool {
        // Check if this event matches a recent suppression entry
        let suppressed = {
            let suppression = self.suppression.lock().await;
            is_event_suppressed(&suppression, &event)
        };

        if suppressed {
            debug!(
                "Suppressed echo for {:?} (origin: {})",
                std::mem::discriminant(&event.domain),
                event.origin_peer
            );
            return false;
        }

        // Not suppressed — broadcast to subscribers
        match self.event_tx.send(event) {
            Ok(n) => {
                debug!("Broadcast sync event to {n} subscribers");
                true
            }
            Err(_) => {
                // No active subscribers — that's fine, events are fire-and-forget
                true
            }
        }
    }

    /// Apply a remote sync event to the local DAW.
    ///
    /// Skips events from self (origin_peer == local peer_id).
    pub async fn apply_remote(&self, event: &SyncEvent) {
        // Skip our own events
        if event.origin_peer == self.session.peer_id {
            return;
        }

        // Check if this domain is enabled in our config
        if !is_domain_enabled(&self.config, &event.domain) {
            debug!("Skipping event for disabled domain");
            return;
        }

        // Apply with suppression
        let mut suppression = self.suppression.lock().await;
        apply::apply_remote_event(
            &self.daw,
            &event.project_guid,
            &event.domain,
            &mut suppression,
        )
        .await;
    }

    /// Subscribe to outgoing sync events (for forwarding to remote peers).
    pub fn subscribe(&self) -> broadcast::Receiver<SyncEvent> {
        self.event_tx.subscribe()
    }

    /// Get the DAW connection (for full-state snapshot requests).
    pub fn daw(&self) -> &Daw {
        &self.daw
    }
}

/// Check if a domain is enabled in the sync config.
fn is_domain_enabled(config: &SyncConfig, domain: &SyncDomain) -> bool {
    match domain {
        SyncDomain::Transport(_) => config.transport,
        SyncDomain::Track(_) => config.tracks,
        SyncDomain::Fx(_) => config.fx,
        SyncDomain::Item(_) => config.items,
        SyncDomain::Take(_) => config.items, // Takes follow items config
        SyncDomain::Routing(_) => config.routing,
        SyncDomain::TempoMap(_) => config.tempo_map,
        SyncDomain::Marker(_) => config.markers,
        SyncDomain::Region(_) => config.regions,
        SyncDomain::Project(_) => true, // Project events always enabled
    }
}

/// Check if a sync event should be suppressed based on the suppression set.
fn is_event_suppressed(suppression: &SuppressionSet, event: &SyncEvent) -> bool {
    use crate::suppression::SuppressionKey;
    use daw::service::{FxEvent, ItemEvent, TrackEvent};

    match &event.domain {
        SyncDomain::Transport(_) => {
            suppression.is_suppressed(&SuppressionKey::transport(&event.project_guid))
        }
        SyncDomain::Track(te) => {
            let guid = match te {
                TrackEvent::VolumeChanged { guid, .. } => guid,
                TrackEvent::PanChanged { guid, .. } => guid,
                TrackEvent::MuteChanged { guid, .. } => guid,
                TrackEvent::SoloChanged { guid, .. } => guid,
                TrackEvent::ArmChanged { guid, .. } => guid,
                TrackEvent::Renamed { guid, .. } => guid,
                TrackEvent::ColorChanged { guid, .. } => guid,
                TrackEvent::SelectionChanged { guid, .. } => guid,
                TrackEvent::TcpVisibilityChanged { guid, .. } => guid,
                TrackEvent::MixerVisibilityChanged { guid, .. } => guid,
                TrackEvent::Added(track) => &track.guid,
                TrackEvent::Removed(guid) => guid,
                TrackEvent::Moved { guid, .. } => guid,
            };
            let field = match te {
                TrackEvent::VolumeChanged { .. } => "volume",
                TrackEvent::PanChanged { .. } => "pan",
                TrackEvent::MuteChanged { .. } => "muted",
                TrackEvent::SoloChanged { .. } => "soloed",
                TrackEvent::ArmChanged { .. } => "armed",
                TrackEvent::Renamed { .. } => "name",
                TrackEvent::ColorChanged { .. } => "color",
                TrackEvent::SelectionChanged { .. } => "selected",
                TrackEvent::TcpVisibilityChanged { .. } => "tcp_visible",
                TrackEvent::MixerVisibilityChanged { .. } => "mixer_visible",
                TrackEvent::Added(_) => "added",
                TrackEvent::Removed(_) => "removed",
                TrackEvent::Moved { .. } => "moved",
            };
            suppression.is_suppressed(&SuppressionKey::track(guid, field))
        }
        SyncDomain::Fx(fe) => {
            if let FxEvent::ParameterChanged {
                context,
                fx_guid,
                param_index,
                ..
            } = fe
            {
                let context_key = format!("{context:?}");
                suppression.is_suppressed(&SuppressionKey::fx_param(
                    &context_key,
                    fx_guid,
                    *param_index,
                ))
            } else {
                false
            }
        }
        SyncDomain::Item(ie) => {
            let (guid, field) = match ie {
                ItemEvent::PositionChanged { item_guid, .. } => (item_guid.as_str(), "position"),
                ItemEvent::LengthChanged { item_guid, .. } => (item_guid.as_str(), "length"),
                ItemEvent::MuteChanged { item_guid, .. } => (item_guid.as_str(), "muted"),
                ItemEvent::VolumeChanged { item_guid, .. } => (item_guid.as_str(), "volume"),
                _ => return false,
            };
            suppression.is_suppressed(&SuppressionKey::item(guid, field))
        }
        SyncDomain::TempoMap(_) => {
            suppression.is_suppressed(&SuppressionKey::tempo_map(&event.project_guid))
        }
        _ => false,
    }
}
