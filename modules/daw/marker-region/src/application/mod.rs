//! Marker Region Application Layer
//!
//! This module contains concrete implementations of the MarkerRegionSource trait
//! for different types of applications and data sources. It represents the application
//! layer from the clean architecture pattern.
//!
//! Each implementation maps domain requests to actual data source functionality:
//! - MockMarkerRegionService: Testing and development
//! - ReaperMarkerRegionSource: REAPER DAW API integration
//! - RppMarkerRegionSource: RPP file parsing integration
//!
//! The application layer is where domain logic meets real-world data sources.
//! These implementations are completely isolated from protocol concerns and can
//! work with any infrastructure adapter in the system.

pub mod mock;
pub mod reaper;
pub mod rpp;
pub mod tempo;

// Re-export implementations
pub use mock::MockMarkerRegionService;
pub use reaper::ReaperMarkerRegionSource;
pub use rpp::RppMarkerRegionSource;
pub use tempo::{TempoTimeEnvelope, TempoTimePoint};

// Re-export for convenience
pub use crate::core::{MarkerRegionSource, MarkerSource, RegionSource};

/// Trait alias for marker/region implementations that can be used across the system
pub trait ApplicationMarkerRegionSource: MarkerRegionSource + Send + Sync + 'static {}

// Implement the trait alias for all concrete types
impl ApplicationMarkerRegionSource for MockMarkerRegionService {}
impl ApplicationMarkerRegionSource for ReaperMarkerRegionSource {}
impl ApplicationMarkerRegionSource for RppMarkerRegionSource {}

// ============================================================================
// BUILDER PATTERNS FOR COMMON SETUPS
// ============================================================================

/// Builder for creating marker/region source implementations with common configurations
pub struct MarkerRegionSourceBuilder {
    source_type: SourceType,
    config: SourceConfig,
}

/// Types of marker/region source implementations available
#[derive(Debug, Clone)]
pub enum SourceType {
    Mock,
    MockWithSampleData,
    MockWithComplexData,
    Reaper,
    RppFile { path: String },
}

/// Configuration options for marker/region source implementations
#[derive(Debug, Clone, Default)]
pub struct SourceConfig {
    pub writable: Option<bool>,
    pub validate_on_load: bool,
    pub auto_sort: bool,
}

impl MarkerRegionSourceBuilder {
    /// Create a new marker/region source builder
    pub fn new() -> Self {
        Self {
            source_type: SourceType::Mock,
            config: SourceConfig::default(),
        }
    }

    /// Set the source type
    pub fn source_type(mut self, source_type: SourceType) -> Self {
        self.source_type = source_type;
        self
    }

    /// Set whether the source should be writable
    pub fn writable(mut self, writable: bool) -> Self {
        self.config.writable = Some(writable);
        self
    }

    /// Enable validation on load
    pub fn validate_on_load(mut self, enabled: bool) -> Self {
        self.config.validate_on_load = enabled;
        self
    }

    /// Enable auto-sorting of markers and regions
    pub fn auto_sort(mut self, enabled: bool) -> Self {
        self.config.auto_sort = enabled;
        self
    }

    /// Build a mock marker/region source
    pub fn build_mock(self) -> MockMarkerRegionService {
        let service = match self.source_type {
            SourceType::Mock => MockMarkerRegionService::new(),
            SourceType::MockWithSampleData => MockMarkerRegionService::with_sample_data(),
            SourceType::MockWithComplexData => MockMarkerRegionService::with_complex_data(),
            _ => MockMarkerRegionService::new(),
        };

        if let Some(writable) = self.config.writable {
            service.set_writable(writable);
        }

        service
    }

    /// Build a REAPER marker/region source
    pub fn build_reaper(self) -> ReaperMarkerRegionSource {
        ReaperMarkerRegionSource::new()
    }

    /// Build an RPP file marker/region source
    pub fn build_rpp(
        self,
        path: &str,
    ) -> Result<RppMarkerRegionSource, crate::core::MarkerRegionError> {
        RppMarkerRegionSource::from_file(path).map_err(|e| e.into())
    }
}

impl Default for MarkerRegionSourceBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

/// Create a mock marker/region source for testing
pub fn mock_source() -> MockMarkerRegionService {
    MockMarkerRegionService::new()
}

/// Create a mock marker/region source with sample data
pub fn mock_source_with_data() -> MockMarkerRegionService {
    MockMarkerRegionService::with_sample_data()
}

/// Create a REAPER marker/region source
pub fn reaper_source() -> ReaperMarkerRegionSource {
    ReaperMarkerRegionSource::new()
}

/// Create an RPP file marker/region source
pub fn rpp_source_from_file(
    path: &str,
) -> Result<RppMarkerRegionSource, crate::core::MarkerRegionError> {
    RppMarkerRegionSource::from_file(path).map_err(|e| e.into())
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::MarkerRegionSource;

    #[test]
    fn test_builder_mock() {
        let source = MarkerRegionSourceBuilder::new()
            .source_type(SourceType::Mock)
            .writable(true)
            .build_mock();

        assert!(MarkerSource::is_writable(&source));
        assert_eq!(source.marker_count(), 0);
        assert_eq!(source.region_count(), 0);
    }

    #[test]
    fn test_builder_sample_data() {
        let source = MarkerRegionSourceBuilder::new()
            .source_type(SourceType::MockWithSampleData)
            .build_mock();

        assert!(source.marker_count() > 0);
        assert!(source.region_count() > 0);
    }

    #[test]
    fn test_convenience_functions() {
        let _mock = mock_source();
        let _sample = mock_source_with_data();
        let _reaper = reaper_source();

        // Test that they implement the trait
        fn test_trait<T: MarkerRegionSource>(_source: T) {}
        test_trait(mock_source());
        test_trait(reaper_source());
    }

    #[test]
    fn test_application_trait_alias() {
        fn test_application_trait<T: ApplicationMarkerRegionSource>(_source: T) {}

        test_application_trait(mock_source());
        test_application_trait(reaper_source());
    }
}
