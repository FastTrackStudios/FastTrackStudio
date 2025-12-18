//! Config loader trait
//!
//! Shared trait for loading configurations from various sources.

use crate::smart_template::config::template_config::InheritanceMode;

/// Trait for loading configurations
///
/// Implementations of this trait can load configuration data from various
/// sources (files, strings, databases, etc.) and support inheritance modes.
pub trait ConfigLoader: Send + Sync {
    /// The configuration type this loader produces
    type Config;
    
    /// Error type for loading operations
    type Error: std::error::Error + Send + Sync;
    
    /// Load configuration from a source (file path, URL, etc.)
    fn load(&self, source: &str) -> Result<Self::Config, Self::Error>;
    
    /// Load configuration with inheritance (defaults + overrides)
    ///
    /// This method loads a default configuration and a user override configuration,
    /// then merges them according to the specified inheritance mode.
    fn load_with_inheritance(
        &self,
        defaults: &str,
        overrides: &str,
        mode: InheritanceMode,
    ) -> Result<Self::Config, Self::Error>;
    
    /// Get the name/identifier of this loader
    fn name(&self) -> &str;
}
