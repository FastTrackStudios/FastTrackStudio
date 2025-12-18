//! Core traits
//!
//! Traits for parsing, matching, templates, and group configurations.

use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template_config::InheritanceMode;
use crate::smart_template::features::naming::track_name::TrackNameLike;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

/// Trait for parsing track names into structured data
pub trait Parser: Send + Sync {
    /// The output type (must implement TrackNameLike)
    type Output: TrackNameLike;
    
    /// Error type for parsing operations
    type Error: std::error::Error + Send + Sync;
    
    /// Parse a track name string into structured data
    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error>;
}

/// Trait for matching parsed track names to template tracks
pub trait Matcher: Send + Sync {
    /// The track name type (must implement TrackNameLike)
    type TrackName: TrackNameLike;
    
    /// Error type for matching operations
    type Error: std::error::Error + Send + Sync;
    
    /// Find the best match for a parsed track name in the template
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult>;
    
    /// Find an existing match or create a new track name for the parsed track
    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error>;
}

/// Trait for objects that can provide a track structure template
pub trait TemplateSource: Send + Sync {
    /// Generate the full template structure with all possible tracks and levels
    fn full_template(&self) -> Template;

    /// Generate the default template structure (optimized for a clean starting point)
    fn default_template(&self) -> Template {
        self.full_template()
    }

    /// Generate the minimal template structure (only the essential tracks)
    fn minimal_template(&self) -> Template {
        self.default_template()
    }

    /// Generate the template structure (legacy compatibility - defaults to default_template)
    fn template(&self) -> Template {
        self.default_template()
    }
}

/// Trait for providing group identity and configuration
pub trait Group: Send + Sync {
    /// Get the name of this group
    fn group_name(&self) -> &str;
    
    /// Get the configuration for this group
    fn group_config(&self) -> GroupConfig;
    
    /// Get the default track list for this group (typically derived from template)
    fn default_tracklist(&self) -> Vec<Track>;
}

/// Extension trait for Group to provide static-like helpers
pub trait GroupExt: Group + Default {
    /// Get the default track list without needing an existing instance
    fn get_default_tracklist() -> Vec<Track> {
        Self::default().default_tracklist()
    }

    /// Get the full template without needing an existing instance
    fn get_full_template() -> Template 
    where 
        Self: TemplateSource 
    {
        Self::default().full_template()
    }

    /// Get the default template without needing an existing instance
    fn get_default_template() -> Template 
    where 
        Self: TemplateSource 
    {
        Self::default().default_template()
    }

    /// Get the minimal template without needing an existing instance
    fn get_minimal_template() -> Template 
    where 
        Self: TemplateSource 
    {
        Self::default().minimal_template()
    }

    /// Get the template without needing an existing instance (legacy compatibility)
    fn get_template() -> Template 
    where 
        Self: TemplateSource 
    {
        Self::default().template()
    }
}

impl<T: Group + Default> GroupExt for T {}

/// Track identifier type (platform-specific)
pub type TrackId = String;

/// Trait for building tracks from templates in a DAW
pub trait DawTemplateBuilder: Send + Sync {
    /// Error type for building operations
    type Error: std::error::Error + Send + Sync;
    
    /// Create a track from a template track
    fn create_track_from_template(
        &self,
        template_track_id: &str,
        track_name: &str,
        parent_track_id: Option<&str>,
    ) -> Result<TrackId, Self::Error>;
    
    /// Copy track properties from one track to another
    fn copy_track_properties(
        &self,
        source_track_id: &str,
        dest_track_id: &str,
    ) -> Result<(), Self::Error>;
    
    /// Get the name/identifier of this builder
    fn name(&self) -> &str;
}

/// Trait for loading configurations
pub trait ConfigLoader: Send + Sync {
    /// The configuration type this loader produces
    type Config;
    
    /// Error type for loading operations
    type Error: std::error::Error + Send + Sync;
    
    /// Load configuration from a source
    fn load(&self, source: &str) -> Result<Self::Config, Self::Error>;
    
    /// Load configuration with inheritance
    fn load_with_inheritance(
        &self,
        defaults: &str,
        overrides: &str,
        mode: InheritanceMode,
    ) -> Result<Self::Config, Self::Error>;
    
    /// Get the name/identifier of this loader
    fn name(&self) -> &str;
}
