//! Track version domain model
//! 
//! Versions represent variations of a track type.
//! For example, a "Guitar Modeler Stereo" track type might have versions:
//! - "With FX" - Full processing chain
//! - "No FX" - Clean modeler output
//! - "DI Only" - Just the DI signal
//! - "Amp Only" - Just the amp sim, no effects

/// Unique identifier for a version
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionId(pub String);

impl VersionId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for VersionId {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

/// Configuration for a track version
#[derive(Debug, Clone)]
pub struct VersionConfig {
    /// Whether to include FX in this version
    pub include_fx: bool,
    
    /// Whether to include sends
    pub include_sends: bool,
    
    /// Whether to include receives
    pub include_receives: bool,
    
    /// Whether to include track routing
    pub include_routing: bool,
    
    /// Custom configuration flags
    pub custom_flags: Vec<String>,
}

impl Default for VersionConfig {
    fn default() -> Self {
        Self {
            include_fx: true,
            include_sends: true,
            include_receives: true,
            include_routing: true,
            custom_flags: Vec::new(),
        }
    }
}

/// Represents a version of a track type
#[derive(Debug, Clone)]
pub struct TrackVersion {
    /// Unique identifier
    pub id: VersionId,
    
    /// Human-readable name
    pub name: String,
    
    /// Version configuration
    pub config: VersionConfig,
    
    /// Description/notes
    pub description: Option<String>,
    
    /// Whether this is the default version for the track type
    pub is_default: bool,
}

impl TrackVersion {
    pub fn new(id: impl Into<VersionId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            config: VersionConfig::default(),
            description: None,
            is_default: false,
        }
    }
    
    pub fn with_config(mut self, config: VersionConfig) -> Self {
        self.config = config;
        self
    }
    
    pub fn as_default(mut self) -> Self {
        self.is_default = true;
        self
    }
    
    pub fn without_fx(mut self) -> Self {
        self.config.include_fx = false;
        self
    }
    
    pub fn without_sends(mut self) -> Self {
        self.config.include_sends = false;
        self
    }
    
    pub fn without_receives(mut self) -> Self {
        self.config.include_receives = false;
        self
    }
}

/// Example: Guitar track versions
pub mod examples {
    use super::*;
    
    pub fn guitar_with_fx() -> TrackVersion {
        TrackVersion::new("gtr-fx", "With FX")
            .as_default()
    }
    
    pub fn guitar_no_fx() -> TrackVersion {
        TrackVersion::new("gtr-no-fx", "No FX")
            .without_fx()
    }
    
    pub fn guitar_di_only() -> TrackVersion {
        TrackVersion::new("gtr-di-only", "DI Only")
            .without_fx()
            .without_sends()
    }
    
    pub fn guitar_amp_only() -> TrackVersion {
        TrackVersion::new("gtr-amp-only", "Amp Only")
            .with_config(VersionConfig {
                include_fx: false,
                include_sends: true,
                include_receives: true,
                include_routing: true,
                custom_flags: vec!["amp_sim_only".to_string()],
            })
    }
}

