//! Build Statistics
//!
//! Tracks statistics for setlist building (batched logging).

use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

/// Batched logging stats for setlist building
struct BuildStats {
    sections_created: usize,
    songs_built: usize,
    songs_reused: usize,
    last_log_time: Instant,
}

impl Default for BuildStats {
    fn default() -> Self {
        Self {
            sections_created: 0,
            songs_built: 0,
            songs_reused: 0,
            last_log_time: Instant::now(),
        }
    }
}

static BUILD_STATS: OnceLock<Mutex<BuildStats>> = OnceLock::new();

/// Record a section creation (batched logging)
pub fn record_section_created() {
    let stats = BUILD_STATS.get_or_init(|| Mutex::new(BuildStats::default()));
    if let Ok(mut s) = stats.lock() {
        s.sections_created += 1;
    }
}

/// Record a song being built (batched logging)
pub fn record_song_built() {
    let stats = BUILD_STATS.get_or_init(|| Mutex::new(BuildStats::default()));
    if let Ok(mut s) = stats.lock() {
        s.songs_built += 1;
    }
}

/// Record a song being reused from cache (batched logging)
pub fn record_song_reused() {
    let stats = BUILD_STATS.get_or_init(|| Mutex::new(BuildStats::default()));
    if let Ok(mut s) = stats.lock() {
        s.songs_reused += 1;
    }
}

/// Flush batched stats if enough time has passed (every 5 seconds)
pub fn flush_build_stats_if_needed() {
    let stats = BUILD_STATS.get_or_init(|| Mutex::new(BuildStats::default()));
    if let Ok(mut s) = stats.lock() {
        let now = Instant::now();
        if s.last_log_time.elapsed() >= Duration::from_secs(5) {
            // Stats logging removed - too verbose
            s.sections_created = 0;
            s.songs_built = 0;
            s.songs_reused = 0;
            s.last_log_time = now;
        }
    }
}
