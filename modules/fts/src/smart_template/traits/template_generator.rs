//! Template generator trait
//!
//! Defines the interface for generating templates from configurations.

use crate::smart_template::template::TemplateConfig;

/// A template represents a track structure
#[derive(Debug, Clone)]
pub struct Template {
    /// Template name
    pub name: String,
    
    /// Track names in this template (hierarchical structure)
    pub tracks: Vec<TrackInfo>,
}

/// Information about a track in a template
#[derive(Debug, Clone)]
pub struct TrackInfo {
    /// Track name
    pub name: String,
    
    /// Track type (e.g., "BUS", "SUM", "AUX")
    pub track_type: Option<String>,
    
    /// Parent track name (for folder hierarchy)
    pub parent: Option<String>,
}

/// Trait for generating templates from configurations
///
/// Implementations of this trait create template structures from configuration
/// data, which can then be used for matching and track creation.
pub trait TemplateGenerator: Send + Sync {
    /// The configuration type this generator works with
    type Config;
    
    /// Error type for generation operations
    type Error: std::error::Error + Send + Sync;
    
    /// Generate a template from a configuration
    fn generate(&self, config: &Self::Config) -> Result<Template, Self::Error>;
    
    /// Get the name/identifier of this generator
    fn name(&self) -> &str;
}
