//! Reactive Polling Service
//!
//! Polls values that can't be detected reactively (cursor positions, transport data)
//! and emits reactive events only when values actually change.
//! This limits traffic by only sending updates when something changes, not every timer tick.

use reaper_high::Reaper;
use rxrust::prelude::*;
use std::cell::RefCell;
use tracing::{debug, trace};
use daw::transport::Transport;

// Re-export EventStreamSubject from rxrust for convenience
pub use rxrust::prelude::LocalSubject;
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Extended reactive streams for polled values
#[derive(Clone, Default, Debug)]
pub struct ReactivePollingStreams {
    /// Edit cursor position in seconds (only emits when position changes)
    pub edit_cursor_position_changed: EventStreamSubject<f64>,
    /// Play cursor position in seconds (only emits when position changes)
    pub play_cursor_position_changed: EventStreamSubject<f64>,
    /// Transport playhead position in seconds (only emits when position changes)
    pub transport_position_changed: EventStreamSubject<f64>,
    /// Full transport state changed (only emits when any transport property changes)
    pub transport_changed: EventStreamSubject<Transport>,
    /// Active song index changed (only emits when index changes)
    pub active_song_index_changed: EventStreamSubject<Option<usize>>,
    /// Active section index changed (only emits when index changes)
    pub active_section_index_changed: EventStreamSubject<Option<usize>>,
    /// Active slide index changed (only emits when index changes)
    pub active_slide_index_changed: EventStreamSubject<Option<usize>>,
}

impl ReactivePollingStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            edit_cursor_position_changed: default(),
            play_cursor_position_changed: default(),
            transport_position_changed: default(),
            transport_changed: default(),
            active_song_index_changed: default(),
            active_section_index_changed: default(),
            active_slide_index_changed: default(),
        }
    }

    pub fn edit_cursor_position_changed(&self) -> LocalSubject<'static, f64, ()> {
        self.edit_cursor_position_changed.borrow().clone()
    }

    pub fn play_cursor_position_changed(&self) -> LocalSubject<'static, f64, ()> {
        self.play_cursor_position_changed.borrow().clone()
    }

    pub fn transport_position_changed(&self) -> LocalSubject<'static, f64, ()> {
        self.transport_position_changed.borrow().clone()
    }

    pub fn active_song_index_changed(&self) -> LocalSubject<'static, Option<usize>, ()> {
        self.active_song_index_changed.borrow().clone()
    }

    pub fn active_section_index_changed(&self) -> LocalSubject<'static, Option<usize>, ()> {
        self.active_section_index_changed.borrow().clone()
    }

    pub fn active_slide_index_changed(&self) -> LocalSubject<'static, Option<usize>, ()> {
        self.active_slide_index_changed.borrow().clone()
    }

    pub fn transport_changed(&self) -> LocalSubject<'static, Transport, ()> {
        self.transport_changed.borrow().clone()
    }
}

/// Polling state - tracks last known values to detect changes
#[derive(Debug, Default)]
struct PollingState {
    last_edit_cursor: Option<f64>,
    last_play_cursor: Option<f64>,
    last_transport_position: Option<f64>,
    last_transport: Option<Transport>,
    last_active_song_index: Option<usize>,
    last_active_section_index: Option<usize>,
    last_active_slide_index: Option<usize>,
}

/// Reactive polling service that polls values and emits events only on change
#[derive(Debug)]
pub struct ReactivePollingService {
    streams: ReactivePollingStreams,
    state: RefCell<PollingState>,
    /// Guard to prevent nested next() calls (which would cause RefCell borrow panics)
    /// This is set to true while emitting events, and checked before emitting
    emitting: RefCell<bool>,
}

impl ReactivePollingService {
    pub fn new() -> Self {
        Self {
            streams: ReactivePollingStreams::new(),
            state: RefCell::new(PollingState::default()),
            emitting: RefCell::new(false),
        }
    }

    /// Get the reactive streams
    pub fn streams(&self) -> &ReactivePollingStreams {
        &self.streams
    }

    /// Poll all values and emit events only when they change
    /// This should be called from the timer callback (main thread)
    pub fn poll(&self) {
        let reaper = Reaper::get();
        let project = reaper.current_project();

        // Check all changes first, then emit events to avoid nested borrows
        let (edit_changed, play_changed, transport_pos_changed, transport_changed, edit_pos, play_pos, transport_pos, transport) = {
            let mut state = self.state.borrow_mut();
            
            // Poll edit cursor position
            let edit_changed = if let Ok(edit_pos) = project.edit_cursor_position() {
                let edit_pos_seconds = edit_pos.get();
                let changed = state.last_edit_cursor.map(|p| (p - edit_pos_seconds).abs() > 0.001).unwrap_or(true);
                if changed {
                    state.last_edit_cursor = Some(edit_pos_seconds);
                }
                (changed, Some(edit_pos_seconds))
            } else {
                (false, None)
            };

            // Poll play cursor position (latency-compensated)
            let play_pos_seconds = project.play_position_latency_compensated().get();
            let play_changed = state.last_play_cursor.map(|p| (p - play_pos_seconds).abs() > 0.001).unwrap_or(true);
            if play_changed {
                state.last_play_cursor = Some(play_pos_seconds);
            }

            // Poll transport position (same as play position when playing, edit position when stopped)
            let transport_pos = if project.is_playing() {
                play_pos_seconds
            } else {
                project.edit_cursor_position()
                    .map(|p| p.get())
                    .unwrap_or(play_pos_seconds)
            };
            let transport_pos_changed = state.last_transport_position.map(|p| (p - transport_pos).abs() > 0.001).unwrap_or(true);
            if transport_pos_changed {
                state.last_transport_position = Some(transport_pos);
            }

            // Poll full transport state
            // Project is Copy, so we can use it directly
            let transport = {
                let transport_adapter = crate::implementation::transport::ReaperTransport::new(project);
                transport_adapter.read_transport().ok()
            };
            let transport_changed = if let Some(ref new_transport) = transport {
                state.last_transport.as_ref().map(|old| old != new_transport).unwrap_or(true)
            } else {
                false
            };
            if transport_changed {
                state.last_transport = transport.clone();
            }

            (edit_changed.0, play_changed, transport_pos_changed, transport_changed, edit_changed.1, play_pos_seconds, transport_pos, transport)
        };
        
        // Now emit events (state borrow is dropped)
        // Check if we're already emitting to avoid nested next() calls
        if *self.emitting.borrow() {
            // Already emitting - skip to avoid nested RefCell borrow panic
            // This can happen if a subscriber triggers another poll/update
            trace!("Skipping poll event emission - already emitting");
            return;
        }
        
        *self.emitting.borrow_mut() = true;
        
        // Emit all events
        if edit_changed {
            if let Some(pos) = edit_pos {
                self.streams.edit_cursor_position_changed.borrow_mut().next(pos);
                trace!(position = pos, "Edit cursor position changed");
            }
        }
        
        if play_changed {
            self.streams.play_cursor_position_changed.borrow_mut().next(play_pos);
            trace!(position = play_pos, "Play cursor position changed");
        }
        
        if transport_pos_changed {
            self.streams.transport_position_changed.borrow_mut().next(transport_pos);
            trace!(position = transport_pos, "Transport position changed");
        }
        
        if transport_changed {
            if let Some(transport) = transport {
                self.streams.transport_changed.borrow_mut().next(transport);
                trace!("Transport state changed");
            }
        }
        
        *self.emitting.borrow_mut() = false;
    }

    /// Update active indices (called from setlist service when it detects changes)
    pub fn update_active_indices(
        &self,
        song_index: Option<usize>,
        section_index: Option<usize>,
        slide_index: Option<usize>,
    ) {
        // Check all changes first, then emit events to avoid nested borrows
        let (song_changed, section_changed, slide_changed) = {
            let mut state = self.state.borrow_mut();
            let song_changed = state.last_active_song_index != song_index;
            let section_changed = state.last_active_section_index != section_index;
            let slide_changed = state.last_active_slide_index != slide_index;
            
            // Update state values
            if song_changed {
                state.last_active_song_index = song_index;
            }
            if section_changed {
                state.last_active_section_index = section_index;
            }
            if slide_changed {
                state.last_active_slide_index = slide_index;
            }
            
            (song_changed, section_changed, slide_changed)
        };
        
        // Now emit events (state borrow is dropped)
        // Check if we're already emitting to avoid nested next() calls
        if *self.emitting.borrow() {
            // Already emitting - skip to avoid nested RefCell borrow panic
            // This can happen if a subscriber triggers another poll/update
            trace!("Skipping active indices event emission - already emitting");
            return;
        }
        
        *self.emitting.borrow_mut() = true;
        
        // Emit all events
        if song_changed {
            self.streams.active_song_index_changed.borrow_mut().next(song_index);
            debug!(song_index = ?song_index, "Active song index changed");
        }
        
        if section_changed {
            self.streams.active_section_index_changed.borrow_mut().next(section_index);
            debug!(section_index = ?section_index, "Active section index changed");
        }
        
        if slide_changed {
            self.streams.active_slide_index_changed.borrow_mut().next(slide_index);
            debug!(slide_index = ?slide_index, "Active slide index changed");
        }
        
        *self.emitting.borrow_mut() = false;
    }
}


