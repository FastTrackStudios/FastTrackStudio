//! Marker Region Module
//!
//! This module provides comprehensive marker and region functionality for DAW applications.
//! It follows a clean architecture with separation between core domain logic,
//! application implementations, and infrastructure adapters.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    INFRASTRUCTURE                           │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
//! │  │    HTTP     │  │   WebSocket │  │    OSC (Future)     │ │
//! │  │  (REST API) │  │ (Real-time) │  │  (Real-time)        │ │
//! │  └─────────────┘  └─────────────┘  └─────────────────────┘ │
//! └─────────────────────────┬───────────────────────────────────┘
//!                           │ Adapters
//! ┌─────────────────────────▼───────────────────────────────────┐
//! │                 APPLICATION LAYER                           │
//! │  ┌─────────────────────────────────────────────────────────┐ │
//! │  │  MockService, ReaperSource, RppSource                  │ │
//! │  │  TempoTimeEnvelope (Tempo calculations)                │ │
//! │  └─────────────────────────────────────────────────────────┘ │
//! └─────────────────────────▼───────────────────────────────────┘
//! │                    CORE DOMAIN                              │
//! │  ┌─────────────────────────────────────────────────────────┐ │
//! │  │  MarkerRegionSource trait (Port)                       │ │
//! │  │  Marker, Region, NavigationPoint (Types)               │ │
//! │  │  MarkerRegionError (Error Handling)                    │ │
//! │  └─────────────────────────────────────────────────────────┘ │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Features
//!
//! ### Core Domain Types
//! - **Marker**: Timeline points with position, name, and metadata
//! - **Region**: Timeline spans with start/end positions, name, and metadata
//! - **NavigationPoint**: Combined marker and region boundary navigation
//! - **MarkerRegionSource**: Trait for accessing markers and regions from any source
//!
//! ### Application Layer
//! - **MockMarkerRegionService**: In-memory implementation for testing
//! - **ReaperMarkerRegionSource**: REAPER DAW integration via reaper-rs
//! - **RppMarkerRegionSource**: RPP file parsing integration
//! - **TempoTimeEnvelope**: Advanced tempo/time signature calculations
//!
//! ### Infrastructure Layer
//! - **HTTP REST API**: Complete CRUD operations via Axum
//! - **WebSocket**: Real-time updates and subscriptions
//! - **OSC Support**: (Future) Open Sound Control integration
//!
//! ## Usage Examples
//!
//! ### Basic Domain Usage
//! ```rust
//! use marker_region::core::{Marker, Region};
//! use primitives::Position;
//!
//! // Create a marker
//! let position = Position::from_seconds(30.0);
//! let marker = Marker::new(position, "Chorus Start".to_string());
//!
//! // Create a region
//! let region = Region::from_seconds(60.0, 120.0, "Verse Section".to_string())?;
//! assert_eq!(region.duration_seconds(), 60.0);
//! ```
//!
//! ### Using Application Services
//! ```rust
//! use marker_region::application::{MockMarkerRegionService, MarkerRegionSource};
//!
//! let service = MockMarkerRegionService::with_sample_data();
//! let markers = service.get_markers()?;
//! let regions = service.get_regions()?;
//!
//! // Get navigation points sorted by time
//! let nav_points = service.get_navigation_points()?;
//! for point in nav_points {
//!     println!("{}", point.display_string());
//! }
//! ```
//!
//! ### REAPER Integration
//! ```rust
//! use marker_region::application::ReaperMarkerRegionSource;
//!
//! let reaper_source = ReaperMarkerRegionSource::new();
//! let markers = reaper_source.get_markers()?;  // Gets markers from current REAPER project
//! let regions = reaper_source.get_regions()?;   // Gets regions from current REAPER project
//! ```
//!
//! ### RPP File Integration
//! ```rust
//! use marker_region::application::RppMarkerRegionSource;
//!
//! let rpp_source = RppMarkerRegionSource::from_file("project.rpp")?;
//! let summary = rpp_source.get_summary()?;
//! println!("Found {} markers and {} regions", summary.marker_count, summary.region_count);
//! ```
//!
//! ### HTTP REST API
//! ```rust
//! use marker_region::infra::create_marker_region_http_router;
//! use marker_region::application::MockMarkerRegionService;
//! use axum::Router;
//! use std::sync::Arc;
//! use tokio::sync::Mutex;
//!
//! let service = Arc::new(Mutex::new(MockMarkerRegionService::with_sample_data()));
//! let app = Router::new()
//!     .nest("/api", create_marker_region_http_router::<MockMarkerRegionService>())
//!     .with_state(service);
//!
//! // Provides endpoints like:
//! // GET  /api/markers           - Get all markers
//! // GET  /api/regions           - Get all regions
//! // GET  /api/navigation        - Get all navigation points
//! // GET  /api/markers/range?start=10&end=60 - Get markers in time range
//! ```
//!
//! ### WebSocket Real-time Updates
//! ```rust
//! use marker_region::infra::{MarkerRegionWebSocketHandler, websocket_handler};
//! use marker_region::application::ReaperMarkerRegionSource;
//! use axum::routing::get;
//! use std::sync::Arc;
//! use tokio::sync::Mutex;
//!
//! let service = Arc::new(Mutex::new(ReaperMarkerRegionSource::new()));
//! let ws_handler = Arc::new(MarkerRegionWebSocketHandler::new(service));
//!
//! // Start background update loop
//! let ws_handler_clone = Arc::clone(&ws_handler);
//! tokio::spawn(async move {
//!     ws_handler_clone.start_update_loop().await;
//! });
//!
//! let app = Router::new()
//!     .route("/ws/marker-region", get(websocket_handler))
//!     .with_state(ws_handler);
//! ```
//!
//! ### Complete Infrastructure Setup
//! ```rust
//! use marker_region::infra::create_complete_infrastructure;
//! use marker_region::application::MockMarkerRegionService;
//!
//! let service = MockMarkerRegionService::with_sample_data();
//! let (router, ws_handler, background_task) = create_complete_infrastructure(service);
//!
//! // This provides both HTTP REST API and WebSocket real-time updates
//! // with automatic background synchronization
//! ```
//!
//! ## Navigation and Progress Bar Integration
//!
//! The marker-region system is designed to support rich progress bar functionality:
//!
//! ```rust
//! use marker_region::core::MarkerRegionSource;
//! use marker_region::application::ReaperMarkerRegionSource;
//!
//! let source = ReaperMarkerRegionSource::new();
//!
//! // Get all navigation points for progress bar tick marks
//! let nav_points = source.get_navigation_points()?;
//!
//! // Find next/previous markers for navigation
//! let current_position = 45.0; // seconds
//! let next_marker = source.get_next_marker(current_position)?;
//! let prev_marker = source.get_previous_marker(current_position)?;
//!
//! // Get regions for colored sections on progress bar
//! let regions = source.get_regions()?;
//! for region in regions {
//!     println!("Section '{}': {:.1}s - {:.1}s ({} color: {:?})",
//!              region.name,
//!              region.start_seconds(),
//!              region.end_seconds(),
//!              region.rgb_color());
//! }
//! ```
//!
//! ## Error Handling
//!
//! All operations use the `MarkerRegionError` type for consistent error handling:
//!
//! ```rust
//! use marker_region::core::{Region, MarkerRegionError};
//!
//! match Region::from_seconds(100.0, 50.0, "Invalid".to_string()) {
//!     Ok(region) => println!("Created region: {}", region),
//!     Err(MarkerRegionError::InvalidRegion { start, end }) => {
//!         println!("Invalid region: start {} must be before end {}", start, end);
//!     }
//!     Err(e) => println!("Other error: {}", e),
//! }
//! ```
//!
//! ## Feature Flags
//!
//! - `rpp` - Enable RPP file parsing support (requires rpp-parser crate)
//! - `http` - Enable HTTP REST API infrastructure (enabled by default)
//! - `websocket` - Enable WebSocket real-time updates (enabled by default)
//!
//! ## Integration with FastTrackStudio
//!
//! This module integrates with the broader FastTrackStudio ecosystem:
//!
//! - Uses `primitives` crate for `Position`, `TimeRange`, and `TimeSignature` types
//! - Exports TypeScript bindings via `ts-rs` for frontend integration
//! - Designed to work with the `transport` module for playback coordination
//! - Provides data for the `SongProgressBar` component in the desktop app

// Core domain module
pub mod core;

// Application layer module
pub mod application;

// Infrastructure adapters module (to be implemented later)
// pub mod infra;

// Re-export core types for convenience
pub use core::{
    Marker, Region, MarkerRegionError, MarkerSource, RegionSource,
    MarkerRegionSource, NavigationPoint, SourceSummary
};

// Re-export application types for convenience
pub use application::{
    MockMarkerRegionService, ReaperMarkerRegionSource, RppMarkerRegionSource,
    TempoTimeEnvelope, TempoTimePoint, MarkerRegionSourceBuilder, SourceType,
    mock_source, mock_source_with_data, reaper_source
};

// Re-export infrastructure adapters for convenience (to be implemented later)
// pub use infra::{
//     // HTTP
//     create_marker_region_http_router, ApiResponse, MarkerRequest, RegionRequest,
//     RangeQuery, PositionQuery,
//
//     // WebSocket
//     MarkerRegionWebSocketHandler, MarkerRegionMessage, SubscriptionFilters,
//     ClientConnection, websocket_handler,
//
//     // Complete setups
//     create_complete_infrastructure, create_http_only_infrastructure,
//     create_websocket_only_infrastructure, InfrastructureBuilder,
// };

// ============================================================================
// CONVENIENCE TYPES AND FUNCTIONS
// ============================================================================

/// Convenient type alias for the most common marker/region source type
pub type DefaultMarkerRegionSource = MockMarkerRegionService;

/// Create a default marker/region source with sample data for quick testing
pub fn default_source() -> DefaultMarkerRegionSource {
    MockMarkerRegionService::with_sample_data()
}

/// Create a marker/region source from the current REAPER project
pub fn reaper_current_project() -> ReaperMarkerRegionSource {
    ReaperMarkerRegionSource::new()
}

/// Create a marker/region source from an RPP file
#[cfg(feature = "rpp")]
pub fn from_rpp_file<P: AsRef<std::path::Path>>(
    path: P
) -> Result<RppMarkerRegionSource, MarkerRegionError> {
    RppMarkerRegionSource::from_file(path).map_err(|e| e.into())
}

/// Quick validation helper for marker/region data
pub fn validate_marker_region_data<T: MarkerRegionSource>(
    source: &T
) -> Result<Vec<String>, MarkerRegionError> {
    source.validate_all()
}

/// Helper to get a summary string for any marker/region source
pub fn get_source_info<T: MarkerRegionSource>(
    source: &T
) -> Result<String, MarkerRegionError> {
    let summary = source.get_summary()?;
    Ok(format!(
        "{}: {} markers, {} regions, {:.1}s duration {}",
        summary.source_name,
        summary.marker_count,
        summary.region_count,
        summary.total_duration,
        if summary.is_writable { "(writable)" } else { "(read-only)" }
    ))
}

// ============================================================================
// VERSION AND METADATA
// ============================================================================

/// Current version of the marker-region module
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Module name for logging and debugging
pub const MODULE_NAME: &str = "marker-region";

/// Get runtime information about this module
pub fn module_info() -> std::collections::HashMap<String, String> {
    let mut info = std::collections::HashMap::new();
    info.insert("name".to_string(), MODULE_NAME.to_string());
    info.insert("version".to_string(), VERSION.to_string());

    #[cfg(feature = "rpp")]
    info.insert("rpp_support".to_string(), "enabled".to_string());
    #[cfg(not(feature = "rpp"))]
    info.insert("rpp_support".to_string(), "disabled".to_string());

    info.insert("reaper_support".to_string(), "stub".to_string());

    info
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_source() {
        let source = default_source();
        assert!(source.marker_count() > 0);
        assert!(source.region_count() > 0);
    }

    #[test]
    fn test_convenience_functions() {
        let source = default_source();
        let info = get_source_info(&source).unwrap();
        assert!(info.contains("markers"));
        assert!(info.contains("regions"));

        let _validation_errors = validate_marker_region_data(&source).unwrap();
        // Sample data might have overlapping regions, so errors are expected
    }

    #[test]
    fn test_module_info() {
        let info = module_info();
        assert_eq!(info.get("name"), Some(&MODULE_NAME.to_string()));
        assert_eq!(info.get("version"), Some(&VERSION.to_string()));
        assert!(info.contains_key("rpp_support"));
    }

    #[test]
    fn test_core_domain_integration() {
        // Test that core types work as expected
        let marker = Marker::from_seconds(30.0, "Test".to_string());
        assert_eq!(marker.position_seconds(), 30.0);
        assert_eq!(marker.name, "Test");

        let region = Region::from_seconds(10.0, 40.0, "Test Region".to_string()).unwrap();
        assert!(region.contains_position(25.0));
        assert!(!region.contains_position(5.0));
    }

    #[test]
    fn test_application_layer_integration() {
        let service = MockMarkerRegionService::new();
        let marker = Marker::from_seconds(60.0, "Added Marker".to_string());

        let id = service.add_marker(marker).unwrap();
        assert_eq!(service.marker_count(), 1);

        let markers = service.get_markers().unwrap();
        assert_eq!(markers[0].id, Some(id));
    }

    #[test]
    fn test_navigation_points() {
        let service = MockMarkerRegionService::with_sample_data();
        let nav_points = service.get_navigation_points().unwrap();

        // Should have markers + region boundaries
        assert!(!nav_points.is_empty());

        // Should be sorted by time
        for i in 1..nav_points.len() {
            assert!(nav_points[i-1].position_seconds() <= nav_points[i].position_seconds());
        }
    }
}
