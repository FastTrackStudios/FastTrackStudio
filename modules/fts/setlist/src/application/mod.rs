//! Application layer for setlist functionality
//!
//! This module provides concrete implementations of the setlist domain traits,
//! including services for different data sources and application orchestration.

mod app;
mod mock;

pub use app::*;
pub use mock::*;

use crate::core::{SetlistSource, Setlist, SetlistError, SourceInfo};
use std::collections::HashMap;

/// Application-level trait that extends SetlistSource with additional functionality
pub trait ApplicationSetlistSource: SetlistSource {
    /// Get detailed source information
    fn get_detailed_info(&self) -> SourceInfo {
        let mut info = self.get_source_info();
        info.metadata.insert("layer".to_string(), "application".to_string());
        info
    }

    /// Refresh/reload data from the source
    fn refresh(&mut self) -> Result<(), SetlistError> {
        // Default implementation does nothing
        Ok(())
    }

    /// Check if the source supports real-time updates
    fn supports_real_time_updates(&self) -> bool {
        false
    }

    /// Enable/disable real-time updates if supported
    fn set_real_time_updates(&mut self, _enabled: bool) -> Result<(), SetlistError> {
        if self.supports_real_time_updates() {
            Ok(())
        } else {
            Err(SetlistError::config_error("Real-time updates not supported"))
        }
    }
}

/// Source type enumeration for different setlist sources
#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    App,
    Mock,
    Reaper,
    Rpp,
    Custom(String),
}

impl std::fmt::Display for SourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceType::App => write!(f, "FastTrackStudio App"),
            SourceType::Mock => write!(f, "Mock"),
            SourceType::Reaper => write!(f, "REAPER"),
            SourceType::Rpp => write!(f, "RPP File"),
            SourceType::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// Convenience functions for creating setlist sources

/// Create an app setlist source
pub fn app_source() -> AppSetlistSource {
    AppSetlistSource::new()
}

/// Create an app setlist source with default setlist
pub fn app_source_with_default() -> AppSetlistSource {
    AppSetlistSource::with_default_setlist()
}

/// Create an app setlist source with sample data
pub fn app_source_with_sample_data() -> AppSetlistSource {
    AppSetlistSource::with_sample_data()
}

/// Create a mock setlist source
pub fn mock_source() -> MockSetlistService {
    MockSetlistService::new()
}

/// Create a mock setlist source with sample data
pub fn mock_source_with_data() -> MockSetlistService {
    MockSetlistService::with_sample_data()
}

/// Create a REAPER setlist source
#[cfg(feature = "reaper")]
pub fn reaper_source() -> ReaperSetlistSource {
    ReaperSetlistSource::new()
}

/// Create an RPP file setlist source
#[cfg(feature = "rpp")]
pub fn rpp_source<P: AsRef<std::path::Path>>(
    path: P,
) -> Result<RppSetlistSource, SetlistError> {
    RppSetlistSource::from_file(path)
}

/// Validate setlist data from any source
pub fn validate_setlist_data<T: SetlistSource>(
    source: &T,
) -> Result<Vec<String>, SetlistError> {
    let setlist = source.build_setlist()?;

    match setlist.validate() {
        Ok(()) => Ok(vec![]),
        Err(e) => {
            if e.is_recoverable() {
                Ok(vec![e.to_string()])
            } else {
                Err(e)
            }
        }
    }
}

/// Get a summary of any setlist source
pub fn get_setlist_source_summary<T: SetlistSource>(
    source: &T,
) -> Result<String, SetlistError> {
    let setlist = source.build_setlist()?;
    let summary = setlist.summary();

    Ok(format!(
        "{}: {} songs, {:.1}s total duration, {} section types",
        source.source_name(),
        summary.song_count,
        summary.total_duration,
        summary.section_types.len()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_source_creation() {
        let source = app_source();
        assert_eq!(source.source_name(), "FastTrackStudio App");
        assert!(source.is_available());
    }

    #[test]
    fn test_mock_source_creation() {
        let source = mock_source();
        assert_eq!(source.source_name(), "Mock");
        assert!(source.is_available());
    }

    #[test]
    fn test_mock_source_with_data() {
        let source = mock_source_with_data();
        let setlist = source.build_setlist().unwrap();
        assert!(setlist.song_count() > 0);
    }

    #[test]
    fn test_source_type_display() {
        assert_eq!(SourceType::App.to_string(), "FastTrackStudio App");
        assert_eq!(SourceType::Mock.to_string(), "Mock");
        assert_eq!(SourceType::Reaper.to_string(), "REAPER");
        assert_eq!(SourceType::Rpp.to_string(), "RPP File");
        assert_eq!(SourceType::Custom("Test".to_string()).to_string(), "Test");
    }

    #[test]
    fn test_validation_functions() {
        let source = mock_source_with_data();
        let validation_result = validate_setlist_data(&source).unwrap();
        // Sample data should be valid
        assert!(validation_result.is_empty());

        let summary = get_setlist_source_summary(&source).unwrap();
        assert!(summary.contains("Mock"));
        assert!(summary.contains("songs"));
    }
}
