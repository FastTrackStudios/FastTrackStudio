//! Composed reactive streams for DAW domain types
//!
//! This module provides convenience types for composing reactive streams
//! from transport and tracks modules. Each domain defines its own reactive
//! behavior, and these types provide a convenient way to access them together.

use crate::transport::reactive::TransportStreams;
use crate::tracks::reactive::TrackStreams;
use uuid::Uuid;
use rxrust::prelude::*;
use std::cell::RefCell;

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Reactive streams for project state
#[derive(Clone, Default, Debug)]
pub struct ProjectStreams {
    /// Project opened/closed
    pub project_changed: EventStreamSubject<Option<Uuid>>, // project_id or None if closed
}

impl ProjectStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            project_changed: default(),
        }
    }
}

/// All DAW reactive streams composed together
/// 
/// This is a convenience type for accessing transport, tracks, and project streams together.
/// Individual domains define their own reactive behavior in their respective modules.
#[derive(Clone, Default, Debug)]
pub struct DawStreams {
    pub transport: TransportStreams,
    pub tracks: TrackStreams,
    pub project: ProjectStreams,
}

impl DawStreams {
    pub fn new() -> Self {
        Self {
            transport: TransportStreams::new(),
            tracks: TrackStreams::new(),
            project: ProjectStreams::new(),
        }
    }
}

