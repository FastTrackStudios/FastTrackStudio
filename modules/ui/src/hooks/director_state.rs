//! Hook for syncing director service state to global signals
//!
//! This hook subscribes to the director service via ROAM and populates the global
//! signals that UI components read from:
//! - `DIRECTOR_INFO` - Director information
//! - `DIRECTOR_RIG_VIEWS` - All rig views with resolved roles
//! - `DIRECTOR_SENDS_ENGINE` - Shared sends engine info
//! - etc.

use crate::context::director::use_director_service;
use dioxus::prelude::*;
use fts::rig::{
    DirectorEvent, DIRECTOR_CONNECTED, DIRECTOR_INFO, DIRECTOR_RIG_VIEWS, DIRECTOR_ROLES,
    DIRECTOR_SENDS_ENGINE,
};

/// Hook that subscribes to director service events and updates global signals
///
/// This hook:
/// 1. Fetches the initial director state from the service
/// 2. Subscribes to director events via ROAM channels
/// 3. Updates global signals when the service notifies of changes
///
/// Call this hook in a component that wraps your director UI.
pub fn use_director_subscription() {
    let ctx = use_director_service();

    // Initialize and subscribe on mount
    use_effect(move || {
        let client = ctx.client.clone();

        spawn(async move {
            tracing::info!("use_director_subscription: starting subscription");

            // Mark as connected
            *DIRECTOR_CONNECTED.write() = true;

            // Fetch initial director info
            if let Some(info) = client.get_director().await {
                tracing::info!(
                    "use_director_subscription: loaded director '{}' with {} rigs",
                    info.name,
                    info.rig_count
                );
                *DIRECTOR_INFO.write() = Some(info);
            }

            // Fetch rig views
            let rig_views = client.get_rig_views().await;
            tracing::info!(
                "use_director_subscription: {} rig views loaded",
                rig_views.len()
            );
            *DIRECTOR_RIG_VIEWS.write() = rig_views;

            // Fetch sends engine
            if let Some(sends) = client.get_sends_engine().await {
                tracing::debug!(
                    "use_director_subscription: sends engine '{}' loaded",
                    sends.name
                );
                *DIRECTOR_SENDS_ENGINE.write() = Some(sends);
            }

            // Fetch roles
            let roles = client.get_roles().await;
            tracing::debug!("use_director_subscription: {} roles loaded", roles.len());
            *DIRECTOR_ROLES.write() = roles;

            // Create a channel for receiving director events
            let (tx, mut rx) = roam::channel::<DirectorEvent>();

            // Subscribe to director events
            client.subscribe(tx).await;
            tracing::debug!("use_director_subscription: subscribed to director events");

            // Listen for updates and sync to global signals
            while let Ok(Some(event)) = rx.recv().await {
                handle_director_event(&client, event).await;
            }
            tracing::debug!("use_director_subscription: subscription loop ended");

            *DIRECTOR_CONNECTED.write() = false;
        });
    });
}

/// Handle a director event and update global signals
async fn handle_director_event<S: fts::rig::DirectorService + Send + Sync + 'static>(
    client: &fts::rig::LocalDirectorClient<S>,
    event: DirectorEvent,
) {
    match event {
        DirectorEvent::DirectorLoaded { info } => {
            tracing::info!("DirectorEvent: director loaded '{}'", info.name);
            *DIRECTOR_INFO.write() = Some(info);
        }
        DirectorEvent::RigChanged { rig_id } => {
            tracing::debug!("DirectorEvent: rig {:?} changed", rig_id);
            // Refetch rig views
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::RoleAssignmentChanged { rig_id, role_id } => {
            tracing::debug!(
                "DirectorEvent: role assignment changed for rig {:?} -> role {:?}",
                rig_id,
                role_id
            );
            // Refetch rig views (roles are resolved in views)
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::ModuleChanged { rig_id, module_type } => {
            tracing::debug!(
                "DirectorEvent: engine {:?} changed for rig {:?}",
                module_type,
                rig_id
            );
            // Refetch rig views
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::MacroChanged {
            rig_id,
            macro_id,
            value,
        } => {
            tracing::trace!(
                "DirectorEvent: macro {:?} changed to {} for rig {:?}",
                macro_id,
                value,
                rig_id
            );
            // Refetch rig views
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::SendLevelChanged {
            rig_id,
            sends_block_id,
            level,
        } => {
            tracing::debug!(
                "DirectorEvent: send level to {:?} changed to {} for rig {:?}",
                sends_block_id,
                level,
                rig_id
            );
            // Refetch rig views
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::InputReassigned { rig_id, input_index } => {
            tracing::info!(
                "DirectorEvent: input reassigned to {} for rig {:?}",
                input_index,
                rig_id
            );
            // Refetch rig views
            let views = client.get_rig_views().await;
            *DIRECTOR_RIG_VIEWS.write() = views;
        }
        DirectorEvent::SendsModuleChanged => {
            tracing::debug!("DirectorEvent: sends engine changed");
            // Refetch sends engine
            if let Some(sends) = client.get_sends_engine().await {
                *DIRECTOR_SENDS_ENGINE.write() = Some(sends);
            }
        }
    }
}
