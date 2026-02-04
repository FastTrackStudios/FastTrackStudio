//! Hot Reload Watcher - Monitors cell binaries for changes
//!
//! Uses the `notify` crate to watch the Extensions/FTS2 directory for:
//! - New cell binaries being added
//! - Existing cell binaries being modified
//! - Cell binaries being removed
//!
//! When changes are detected, callbacks are triggered to reload or load cells.

use anyhow::{Context, Result};
use crossbeam_channel::{bounded, Receiver};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread;
use tracing::{debug, error, info, warn};

/// Watch events for different file operations
#[derive(Debug, Clone)]
pub enum WatchEvent {
    /// A new file was created
    FileCreated(PathBuf),
    /// An existing file was modified
    FileModified(PathBuf),
    /// A file was removed
    FileRemoved(PathBuf),
}

/// Callback function type for watch events
type EventCallback = Box<dyn Fn(WatchEvent) + Send + 'static>;

/// File watcher for hot-reload functionality
pub struct HotReloadWatcher {
    /// The notify watcher instance
    _watcher: RecommendedWatcher,
    /// Receiver for watch events (processed in background thread)
    event_rx: Arc<Mutex<Receiver<WatchEvent>>>,
    /// Callback invoked for all watch events
    callback: Arc<Mutex<Option<EventCallback>>>,
}

impl std::fmt::Debug for HotReloadWatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HotReloadWatcher")
            .field("has_callback", &self.callback.lock().unwrap().is_some())
            .finish()
    }
}

impl HotReloadWatcher {
    /// Create a new hot-reload watcher
    pub fn new() -> Result<Self> {
        // Create channel for watch events with bounded capacity
        let (event_tx, event_rx) = bounded(100);
        let event_rx = Arc::new(Mutex::new(event_rx));

        // Create the notify watcher
        let tx_clone = event_tx.clone();
        let watcher = notify::recommended_watcher(move |res: notify::Result<Event>| {
            match res {
                Ok(event) => {
                    // Filter for relevant events (create, modify, remove)
                    match event.kind {
                        EventKind::Create(_) => {
                            for path in &event.paths {
                                if path.is_file() {
                                    debug!("notify: File created: {}", path.display());
                                    let _ = tx_clone.send(WatchEvent::FileCreated(path.clone()));
                                }
                            }
                        }
                        EventKind::Modify(_) => {
                            for path in &event.paths {
                                if path.is_file() {
                                    debug!("notify: File modified: {}", path.display());
                                    let _ = tx_clone.send(WatchEvent::FileModified(path.clone()));
                                }
                            }
                        }
                        EventKind::Remove(_) => {
                            for path in &event.paths {
                                debug!("notify: File removed: {}", path.display());
                                let _ = tx_clone.send(WatchEvent::FileRemoved(path.clone()));
                            }
                        }
                        _ => {}
                    }
                }
                Err(e) => error!("Watch error: {:#}", e),
            }
        })
        .context("Failed to create file watcher")?;

        Ok(Self {
            _watcher: watcher,
            event_rx,
            callback: Arc::new(Mutex::new(None)),
        })
    }

    /// Watch a directory for changes and invoke callback on events
    pub fn watch_directory<F>(&mut self, path: &Path, callback: F) -> Result<()>
    where
        F: Fn(WatchEvent) + Send + 'static,
    {
        info!("Watching directory for changes: {}", path.display());

        // Store the callback
        *self.callback.lock().unwrap() = Some(Box::new(callback));

        // Watch the directory recursively
        self._watcher
            .watch(path, RecursiveMode::Recursive)
            .context("Failed to watch directory")?;

        // Start background thread to process events
        let event_rx = self.event_rx.clone();
        let callback = self.callback.clone();

        thread::Builder::new()
            .name("cell-watcher".to_string())
            .spawn(move || {
                info!("Hot-reload watcher thread started");

                loop {
                    // Wait for events
                    let rx = event_rx.lock().unwrap();
                    match rx.recv() {
                        Ok(event) => {
                            drop(rx); // Release lock before calling callback

                            // Call the callback
                            if let Some(ref cb) = *callback.lock().unwrap() {
                                cb(event);
                            }
                        }
                        Err(_) => {
                            // Channel closed, exit thread
                            warn!("Watch event channel closed, stopping watcher thread");
                            break;
                        }
                    }
                }

                info!("Hot-reload watcher thread stopped");
            })
            .context("Failed to spawn watcher thread")?;

        Ok(())
    }
}
