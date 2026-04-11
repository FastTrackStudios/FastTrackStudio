// r[impl sync.file-watch]
//! File watching for the vault directory.
//!
//! Uses the `notify` crate for cross-platform FSEvents / inotify support.
//! Changes are debounced: the first event starts a 500ms window; any further
//! events within that window are coalesced. The counter is incremented once
//! per coalesced burst.

use std::path::Path;
use std::time::Duration;

use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::watch;

/// Holds the underlying notify watcher alive. Dropping this stops file watching.
pub struct WatchHandle(#[allow(dead_code)] RecommendedWatcher);

const DEBOUNCE: Duration = Duration::from_millis(500);

// r[impl sync.file-watch]
/// Start watching `root` recursively. Increments `counter` once per debounced
/// change burst. Returns a `WatchHandle` that stops watching when dropped.
pub fn start_watch(
    root: &Path,
    counter: watch::Sender<u64>,
) -> notify::Result<WatchHandle> {
    let (tx, rx) = std::sync::mpsc::channel();

    let mut watcher = RecommendedWatcher::new(
        move |res: notify::Result<notify::Event>| {
            if res.is_ok() {
                let _ = tx.send(());
            }
        },
        Config::default(),
    )?;

    watcher.watch(root, RecursiveMode::Recursive)?;

    // Debounce thread: coalesce rapid events into a single notification.
    std::thread::spawn(move || {
        loop {
            match rx.recv() {
                Ok(()) => {
                    // Drain additional events within the debounce window.
                    while rx.recv_timeout(DEBOUNCE).is_ok() {}
                    counter.send_modify(|v| *v += 1);
                }
                Err(_) => break, // sender dropped → watcher was dropped
            }
        }
    });

    Ok(WatchHandle(watcher))
}
