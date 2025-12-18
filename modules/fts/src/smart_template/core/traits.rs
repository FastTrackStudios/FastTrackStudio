//! Core traits
//!
//! Traits for template generation and building.

use crate::smart_template::core::template::Template;

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

/// Track identifier type (platform-specific)
pub type TrackId = String;

/// Trait for building tracks from templates
///
/// Implementations of this trait can create tracks in a DAW from template
/// configurations, including copying properties from template tracks.
pub trait TemplateBuilder: Send + Sync {
    /// Error type for building operations
    type Error: std::error::Error + Send + Sync;
    
    /// Create a track from a template track
    ///
    /// This creates a new track with the given name, optionally as a child of
    /// a parent track, and copies properties from the template track.
    fn create_track_from_template(
        &self,
        template_track_id: &str,
        track_name: &str,
        parent_track_id: Option<&str>,
    ) -> Result<TrackId, Self::Error>;
    
    /// Copy track properties from one track to another
    ///
    /// This copies FX, sends, receives, and other properties from the source
    /// track to the destination track.
    fn copy_track_properties(
        &self,
        source_track_id: &str,
        dest_track_id: &str,
    ) -> Result<(), Self::Error>;
    
    /// Get the name/identifier of this builder
    fn name(&self) -> &str;
}
