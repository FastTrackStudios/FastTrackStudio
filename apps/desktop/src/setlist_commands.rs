//! Setlist Command Sender
//!
//! Provides functions to send commands to REAPER via the setlist API.
//! Uses the same storage as setlist_connection to access the API.

use crate::setlist_connection::SETLIST_API;
use fts::setlist::{NavigationCommand, TransportCommand};
use tracing::{info, warn};

/// Send a transport command to REAPER
#[cfg(not(target_arch = "wasm32"))]
pub async fn send_transport_command(command: TransportCommand) -> Result<(), String> {
    info!(
        "[Setlist Commands] send_transport_command called: {:?}",
        command
    );
    if let Some(storage) = SETLIST_API.get() {
        info!("[Setlist Commands] API storage found, acquiring lock...");
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            info!("[Setlist Commands] API found, sending transport command...");
            match api.transport_command(command).await {
                Ok(Ok(())) => {
                    info!("[Setlist Commands] ✅ Successfully sent transport command");
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!(
                        "[Setlist Commands] ❌ Failed to execute transport command: {}",
                        e
                    );
                    Err(e)
                }
                Err(e) => {
                    warn!(
                        "[Setlist Commands] ❌ RPC error sending transport command: {}",
                        e
                    );
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            warn!("[Setlist Commands] ⚠️ Setlist API not available (None in storage)");
            Err("Setlist API not available".to_string())
        }
    } else {
        warn!("[Setlist Commands] ⚠️ API storage not initialized");
        Err("API storage not initialized".to_string())
    }
}

/// Send a navigation command to REAPER
#[cfg(not(target_arch = "wasm32"))]
pub async fn send_navigation_command(command: NavigationCommand) -> Result<(), String> {
    info!(
        "[Setlist Commands] send_navigation_command called: {:?}",
        command
    );
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            match api.navigation_command(command).await {
                Ok(Ok(())) => {
                    info!("[Setlist Commands] ✅ Successfully sent navigation command");
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!(
                        "[Setlist Commands] ❌ Failed to execute navigation command: {}",
                        e
                    );
                    Err(e)
                }
                Err(e) => {
                    warn!(
                        "[Setlist Commands] ❌ RPC error sending navigation command: {}",
                        e
                    );
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            warn!("[Setlist Commands] ⚠️ Setlist API not available (None in storage)");
            Err("Setlist API not available".to_string())
        }
    } else {
        warn!("[Setlist Commands] ⚠️ API storage not initialized");
        Err("API storage not initialized".to_string())
    }
}

/// Seek to a specific section
#[cfg(not(target_arch = "wasm32"))]
pub async fn seek_to_section(song_index: usize, section_index: usize) -> Result<(), String> {
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            match api.seek_to_section(song_index, section_index).await {
                Ok(Ok(())) => {
                    info!(
                        "Successfully sought to song {} section {}",
                        song_index, section_index
                    );
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!("Failed to seek to section: {}", e);
                    Err(e)
                }
                Err(e) => {
                    warn!("RPC error seeking to section: {}", e);
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            Err("Setlist API not available".to_string())
        }
    } else {
        Err("API storage not initialized".to_string())
    }
}

/// Seek to a specific song (switches to that song's tab)
#[cfg(not(target_arch = "wasm32"))]
pub async fn seek_to_song(song_index: usize) -> Result<(), String> {
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            match api.seek_to_song(song_index).await {
                Ok(Ok(())) => {
                    info!("Successfully sought to song {}", song_index);
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!("Failed to seek to song: {}", e);
                    Err(e)
                }
                Err(e) => {
                    warn!("RPC error seeking to song: {}", e);
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            Err("Setlist API not available".to_string())
        }
    } else {
        Err("API storage not initialized".to_string())
    }
}

/// Seek to a specific musical position
#[cfg(not(target_arch = "wasm32"))]
pub async fn seek_to_musical_position(
    song_index: usize,
    musical_position: daw::primitives::MusicalPosition,
) -> Result<(), String> {
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            let musical_pos_for_log = musical_position.clone();
            match api
                .seek_to_musical_position(song_index, musical_position)
                .await
            {
                Ok(Ok(())) => {
                    info!(
                        "Successfully sought to song {} musical position {}",
                        song_index, musical_pos_for_log
                    );
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!("Failed to seek to musical position: {}", e);
                    Err(e)
                }
                Err(e) => {
                    warn!("API error seeking to musical position: {}", e);
                    Err(format!("API error: {}", e))
                }
            }
        } else {
            Err("API not available".to_string())
        }
    } else {
        Err("API storage not initialized".to_string())
    }
}

#[cfg(target_arch = "wasm32")]
pub async fn seek_to_musical_position(
    _song_index: usize,
    _musical_position: daw::primitives::MusicalPosition,
) -> Result<(), String> {
    Ok(())
}

/// Seek to a specific time position
#[cfg(not(target_arch = "wasm32"))]
pub async fn seek_to_time(song_index: usize, time_seconds: f64) -> Result<(), String> {
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            match api.seek_to_time(song_index, time_seconds).await {
                Ok(Ok(())) => {
                    info!(
                        "Successfully sought to song {} time {}",
                        song_index, time_seconds
                    );
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!("Failed to seek to time: {}", e);
                    Err(e)
                }
                Err(e) => {
                    warn!("RPC error seeking to time: {}", e);
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            Err("Setlist API not available".to_string())
        }
    } else {
        Err("API storage not initialized".to_string())
    }
}

/// Toggle loop state
#[cfg(not(target_arch = "wasm32"))]
pub async fn toggle_loop() -> Result<(), String> {
    if let Some(storage) = SETLIST_API.get() {
        let mut guard = storage.lock().await;
        if let Some(api) = guard.as_ref() {
            match api.toggle_loop().await {
                Ok(Ok(())) => {
                    info!("Successfully toggled loop");
                    Ok(())
                }
                Ok(Err(e)) => {
                    warn!("Failed to toggle loop: {}", e);
                    Err(e)
                }
                Err(e) => {
                    warn!("RPC error toggling loop: {}", e);
                    Err(format!("RPC error: {}", e))
                }
            }
        } else {
            Err("Setlist API not available".to_string())
        }
    } else {
        Err("API storage not initialized".to_string())
    }
}
