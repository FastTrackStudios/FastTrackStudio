//! Comprehensive Change Detection
//!
//! Uses ChangeDetectionMiddleware to listen for ALL changes (tracks, play state, tempo, bookmarks, etc.)
//! instead of polling. This is much more efficient than polling every 100ms.

use reaper_high::{
    ChangeDetectionMiddleware, ChangeEvent, ControlSurfaceEvent, ControlSurfaceMiddleware,
};
use std::sync::{Arc, Mutex, OnceLock};
use tracing::{debug, info, trace};
use crate::setlist_stream;

/// Control surface middleware that processes ALL change events
#[derive(Debug)]
pub struct ComprehensiveChangeDetectionMiddleware {
    change_detection: ChangeDetectionMiddleware,
}

impl ComprehensiveChangeDetectionMiddleware {
    pub fn new() -> Self {
        let mut middleware = ChangeDetectionMiddleware::new();
        
        // Initialize the change detection middleware by calling reset
        // This captures the initial project/track/transport state
        middleware.reset(|event| {
            trace!(event = ?event, "Initial change detection reset event");
            handle_change_event(event);
        });
        
        Self {
            change_detection: middleware,
        }
    }
}

impl ControlSurfaceMiddleware for ComprehensiveChangeDetectionMiddleware {
    fn run(&mut self) {
        // Process any pending change events (polling-based detection for things like visibility)
        // run() takes &mut self and a closure that receives ChangeEvent
        self.change_detection.run(&mut |event| {
            trace!(event = ?event, "Change detection run event");
            handle_change_event(event);
        });
    }

    fn handle_event(&self, event: ControlSurfaceEvent) -> bool {
        // Process the control surface event through change detection middleware
        // This converts ControlSurfaceEvent into ChangeEvent
        self.change_detection.process(&event, |change_event| {
            trace!(
                control_event = ?event,
                change_event = ?change_event,
                "Processing change event"
            );
            handle_change_event(change_event);
        })
    }
}

/// Handle a change event and update relevant caches
fn handle_change_event(event: ChangeEvent) {
    use ChangeEvent::*;
    
    match event {
        // Track structure changes - mark setlist dirty (tracks are read fresh each time now)
        TrackAdded(_) | TrackRemoved(_) | TracksReordered(_) | ProjectSwitched(_) => {
            debug!(event = ?event, "Track structure changed");
            // Tracks are read fresh each poll, so no need to mark track cache dirty
            // But we can mark setlist dirty if we want immediate updates (optional)
        }
        
        // Track property changes - tracks are read fresh each poll, so no action needed
        TrackNameChanged(_)
        | TrackVolumeChanged(_)
        | TrackPanChanged(_)
        | TrackMuteChanged(_)
        | TrackSoloChanged(_)
        | TrackSelectedChanged(_)
        | TrackArmChanged(_)
        | TrackVisibilityChanged(_)
        | TrackAutomationModeChanged(_)
        | TrackInputChanged(_)
        | TrackInputMonitoringChanged(_) => {
            // Tracks are read fresh each poll via chunks, no caching needed
            trace!(event = ?event, "Track property changed (will be picked up on next poll)");
        }
        
        // Route changes - tracks are read fresh each poll
        TrackRouteVolumeChanged(_)
        | TrackRoutePanChanged(_)
        | ReceiveCountChanged(_)
        | TrackSendCountChanged(_)
        | HardwareOutputSendCountChanged(_) => {
            trace!(event = ?event, "Track route changed (will be picked up on next poll)");
        }
        
        // FX changes - tracks are read fresh each poll
        FxAdded(_) | FxRemoved(_) | FxEnabledChanged(_) | FxReordered(_) => {
            trace!(event = ?event, "FX changed (will be picked up on next poll)");
        }
        
        // Transport/Playback events - setlist is polled continuously, so these are informational
        PlayStateChanged(_) => {
            debug!(event = ?event, "Play state changed (will be picked up on next poll)");
            // Setlist is polled continuously, so changes will be detected automatically
        }
        
        // Tempo/Time signature events - setlist is polled continuously
        MasterTempoChanged(_) | MasterPlayRateChanged(_) => {
            debug!(event = ?event, "Tempo/play rate changed (will be picked up on next poll)");
        }
        
        // Repeat state - setlist is polled continuously
        RepeatStateChanged(_) => {
            trace!(event = ?event, "Repeat state changed (will be picked up on next poll)");
        }
        
        // Bookmarks/Markers - setlist is polled continuously
        BookmarksChanged(_) => {
            debug!(event = ?event, "Bookmarks/markers changed (will be picked up on next poll)");
        }
        
        // Project changes - setlist is polled continuously
        ProjectClosed(_) => {
            debug!(event = ?event, "Project closed (will be picked up on next poll)");
        }
        
        // Global automation override - might affect setlist behavior
        GlobalAutomationOverrideChanged(_) => {
            trace!(event = ?event, "Global automation override changed");
            // Doesn't directly affect setlist structure, but might affect playback
        }
        
        // FX UI events - don't affect structure
        FxOpened(_) | FxClosed(_) | FxFocused(_) | FxParameterValueChanged(_) | FxPresetChanged(_) => {
            // These are UI-only events, don't affect structure
            trace!(event = ?event, "FX UI event, ignoring");
        }
    }
}

// Tracks are now read fresh each poll using optimized chunk-based API
// No caching needed - chunks are efficient enough

/// Global instance of the change detection middleware (for run() calls from timer)
static CHANGE_DETECTION_MIDDLEWARE: OnceLock<Arc<Mutex<ComprehensiveChangeDetectionMiddleware>>> = OnceLock::new();

/// Initialize the change detection middleware (returns Arc for timer callback access)
pub fn init_change_detection() -> Arc<Mutex<ComprehensiveChangeDetectionMiddleware>> {
    CHANGE_DETECTION_MIDDLEWARE.get_or_init(|| {
        info!("Initializing comprehensive change detection middleware");
        Arc::new(Mutex::new(ComprehensiveChangeDetectionMiddleware::new()))
    }).clone()
}

/// Get the change detection middleware instance (for timer callback)
pub fn get_change_detection() -> Arc<Mutex<ComprehensiveChangeDetectionMiddleware>> {
    init_change_detection()
}

