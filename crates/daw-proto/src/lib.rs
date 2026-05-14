//! DAW Protocol Definitions
//!
//! This crate defines the shared types and service interfaces for DAW cells.

#![deny(unsafe_code)]

pub mod action_registry;
pub mod actions;
pub mod audio_accessor;
pub mod audio_engine;
pub mod automation;
pub mod batch;
pub mod capability;
pub mod dawfile_service;
pub mod dock_host;
#[cfg(feature = "test-utils")]
pub mod dock_host_mock;
pub mod error;
pub mod ext_state;
pub mod fx;
pub mod health;
pub mod input;
pub mod item;
pub mod live_midi;
pub mod marker;
pub mod markers_regions;
pub mod midi;
pub mod peak;
pub mod plugin_loader;
pub mod position_conversion;
pub mod primitives;
pub mod project;
pub mod region;
pub mod resource;
pub mod routing;
pub mod screenset;
pub mod sync;
pub mod take;
pub mod tempo_map;
pub mod toolbar;
pub mod track;
pub mod transport;
pub mod ui;
pub mod undo;
pub mod window_geometry;

pub use action_registry::*;
pub use actions::*;
pub use audio_accessor::*;
pub use audio_engine::*;
pub use automation::*;
pub use batch::*;
pub use capability::*;
pub use dawfile_service::*;
pub use dock_host::*;
pub use error::*;
pub use ext_state::*;
pub use fx::*;
pub use health::*;
pub use input::*;
pub use item::*;
pub use live_midi::*;
// Explicit re-exports rather than glob: the architect-emitted
// `serve` / `descriptor` / `Dispatcher` aliases in `marker::*` and
// `track::*` collide when glob-imported at the crate root. Callers
// reach those via the fully qualified paths `daw_proto::marker::*`.
#[cfg(feature = "vox")]
pub use marker::MarkersClient;
pub use marker::{Marker, MarkerError, MarkerEvent, Markers, MarkersRpc};
pub use markers_regions::*;
pub use midi::*;
pub use peak::*;
pub use plugin_loader::*;
pub use position_conversion::*;
pub use primitives::*;
pub use project::*;
// Explicit re-exports (see marker / track for the rationale).
#[cfg(feature = "vox")]
pub use region::RegionsClient;
pub use region::{AddRegionInLaneRequest, Region, RegionError, RegionEvent, Regions, RegionsRpc};
pub use resource::*;
pub use routing::*;
pub use screenset::*;
// Explicit re-exports (see marker / track / region for rationale —
// architect-emitted `serve` / `descriptor` / `Dispatcher` aliases
// can't be glob-imported at the crate root.)
#[cfg(feature = "vox")]
pub use take::TakesClient;
pub use take::{Takes, TakesRpc};
#[cfg(feature = "vox")]
pub use tempo_map::TempoMapClient;
pub use tempo_map::{TempoMap, TempoMapError, TempoMapEvent, TempoMapRpc, TempoPoint};
pub use toolbar::*;
// Explicit re-exports (see marker::* note above for the rationale).
#[cfg(feature = "vox")]
pub use track::TracksClient;
pub use track::{
    AddChildren, FolderDepthChange, InputMonitoringMode, RecordInput, Track, TrackError,
    TrackEvent, TrackExtStateRequest, TrackGroup, TrackHierarchy, TrackHierarchyBuilder, TrackNode,
    TrackRef, TrackStructureBuilder, Tracks, TracksRpc, assert_tracks_equal, display_tracklist,
    format_tracklist,
};
pub use transport::*;
pub use ui::*;
pub use undo::*;
pub use window_geometry::*;
