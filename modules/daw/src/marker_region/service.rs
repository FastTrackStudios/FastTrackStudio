//! MarkerRegionService - Roam service for marker and region operations
//!
//! This service provides RPC access to timeline markers and regions.
//! Commands are executed via the service implementation (Mock or REAPER).
//!
//! Uses the full domain types (Marker, Region) with Facet for serialization.

use crate::marker_region::core::{Marker, Region};
use facet::Facet;
use roam::Tx;

// region:    --- Commands

/// Commands that can be executed on markers and regions
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum MarkerRegionCommand {
    /// Add a marker at the specified position
    AddMarker { position: f64, name: String },
    /// Add a marker with color
    AddMarkerWithColor {
        position: f64,
        name: String,
        color: u32,
    },
    /// Remove a marker by ID
    RemoveMarker { id: u32 },
    /// Update a marker's position
    MoveMarker { id: u32, position: f64 },
    /// Update a marker's name
    RenameMarker { id: u32, name: String },
    /// Add a region
    AddRegion { start: f64, end: f64, name: String },
    /// Add a region with color
    AddRegionWithColor {
        start: f64,
        end: f64,
        name: String,
        color: u32,
    },
    /// Remove a region by ID
    RemoveRegion { id: u32 },
    /// Update a region's bounds
    MoveRegion { id: u32, start: f64, end: f64 },
    /// Update a region's name
    RenameRegion { id: u32, name: String },
    /// Go to the next marker from current position
    GoToNextMarker,
    /// Go to the previous marker from current position
    GoToPreviousMarker,
    /// Go to a specific marker by ID
    GoToMarker { id: u32 },
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the marker/region service
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum MarkerRegionEvent {
    /// A marker was added
    MarkerAdded(Marker),
    /// A marker was removed (by ID)
    MarkerRemoved(u32),
    /// A marker's properties changed
    MarkerChanged(Marker),
    /// All markers changed (bulk update)
    MarkersChanged(Vec<Marker>),
    /// A region was added
    RegionAdded(Region),
    /// A region was removed (by ID)
    RegionRemoved(u32),
    /// A region's properties changed
    RegionChanged(Region),
    /// All regions changed (bulk update)
    RegionsChanged(Vec<Region>),
}

// endregion: --- Events

// region:    --- Service Trait

/// MarkerRegionService provides RPC access to timeline markers and regions.
///
/// This trait defines the contract for marker/region management services.
/// Implementations can be for REAPER, mock testing, or other DAWs.
#[roam::service]
pub trait MarkerRegionService {
    /// Get all markers in the current project
    async fn get_markers(&self) -> Vec<Marker>;

    /// Get a specific marker by ID
    async fn get_marker(&self, id: u32) -> Option<Marker>;

    /// Get markers within a time range
    async fn get_markers_in_range(&self, start: f64, end: f64) -> Vec<Marker>;

    /// Get the next marker after the given position
    async fn get_next_marker(&self, after: f64) -> Option<Marker>;

    /// Get the previous marker before the given position
    async fn get_previous_marker(&self, before: f64) -> Option<Marker>;

    /// Get all regions in the current project
    async fn get_regions(&self) -> Vec<Region>;

    /// Get a specific region by ID
    async fn get_region(&self, id: u32) -> Option<Region>;

    /// Get regions that intersect with a time range
    async fn get_regions_in_range(&self, start: f64, end: f64) -> Vec<Region>;

    /// Get the region containing the given position (if any)
    async fn get_region_at(&self, position: f64) -> Option<Region>;

    /// Get the number of markers
    async fn marker_count(&self) -> usize;

    /// Get the number of regions
    async fn region_count(&self) -> usize;

    /// Execute a marker/region command
    async fn execute(&self, cmd: MarkerRegionCommand);

    /// Subscribe to marker/region events
    async fn subscribe(&self, events: Tx<MarkerRegionEvent>);
}

// endregion: --- Service Trait

// Re-export domain types for convenience
pub use crate::marker_region::core::{Marker as MarkerInfo, Region as RegionInfo};
