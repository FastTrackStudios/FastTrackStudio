//! Track command functions for desktop app
//!
//! Provides async functions to send track commands (mute, solo) to REAPER via IRPC.

use daw::tracks::reactive::irpc::TrackApi;
use daw::tracks::api::solo::SoloMode;
use crate::tracks_connection::TRACKS_API;
use tracing::{info, warn};
use tokio::sync::oneshot;
use anyhow::Result;

/// Set track mute
#[cfg(not(target_arch = "wasm32"))]
pub async fn set_track_mute(project_id: String, track_index: usize, muted: bool) -> Result<(), String> {
    info!("[Track Commands] set_track_mute called: project={}, track={}, muted={}", project_id, track_index, muted);
    let (tx, rx) = oneshot::channel();
    let project_id_clone = project_id.clone();
    
    tokio::spawn(async move {
        let result = if let Some(storage) = TRACKS_API.get() {
            let mut guard = storage.lock().await;
            if let Some(api) = guard.as_ref() {
                match api.set_track_mute(project_id_clone, track_index, muted).await {
                    Ok(Ok(())) => {
                        info!("[Track Commands] ✅ Successfully set track mute");
                        Ok(())
                    }
                    Ok(Err(e)) => {
                        warn!("[Track Commands] ❌ Failed to execute set_track_mute: {}", e);
                        Err(e)
                    }
                    Err(e) => {
                        warn!("[Track Commands] ❌ RPC error sending set_track_mute: {}", e);
                        Err(format!("RPC error: {}", e))
                    }
                }
            } else {
                Err("Track API not available".to_string())
            }
        } else {
            Err("API storage not initialized".to_string())
        };
        
        let _ = tx.send(result);
    });
    
    rx.await.map_err(|e| format!("Command task failed: {}", e))?
}

/// Set track solo
#[cfg(not(target_arch = "wasm32"))]
pub async fn set_track_solo(project_id: String, track_index: usize, solo_mode: SoloMode) -> Result<(), String> {
    info!("[Track Commands] set_track_solo called: project={}, track={}, solo_mode={:?}", project_id, track_index, solo_mode);
    let (tx, rx) = oneshot::channel();
    let project_id_clone = project_id.clone();
    
    tokio::spawn(async move {
        let result = if let Some(storage) = TRACKS_API.get() {
            let mut guard = storage.lock().await;
            if let Some(api) = guard.as_ref() {
                match api.set_track_solo(project_id_clone, track_index, solo_mode).await {
                    Ok(Ok(())) => {
                        info!("[Track Commands] ✅ Successfully set track solo");
                        Ok(())
                    }
                    Ok(Err(e)) => {
                        warn!("[Track Commands] ❌ Failed to execute set_track_solo: {}", e);
                        Err(e)
                    }
                    Err(e) => {
                        warn!("[Track Commands] ❌ RPC error sending set_track_solo: {}", e);
                        Err(format!("RPC error: {}", e))
                    }
                }
            } else {
                Err("Track API not available".to_string())
            }
        } else {
            Err("API storage not initialized".to_string())
        };
        
        let _ = tx.send(result);
    });
    
    rx.await.map_err(|e| format!("Command task failed: {}", e))?
}

