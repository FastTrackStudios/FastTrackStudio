use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::models::organization::OrganizationMode;
use crate::smart_template::utils::track_helpers::{create_track, TrackExt};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::track_ext::TrackItemPropertiesExt;
use daw::tracks::Track;
use daw::tracks::api::folder::FolderDepthChange;

/// A builder for creating Template structures declaratively
pub struct TemplateBuilder {
    name: String,
    tracks: Vec<Track>,
    parent_stack: Vec<String>,
    property_stack: Vec<ItemProperties>,
    mode: OrganizationMode,
}

impl TemplateBuilder {
    /// Create a new TemplateBuilder with the given template name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            tracks: Vec::new(),
            parent_stack: Vec::new(),
            property_stack: vec![ItemProperties::new()],
            mode: OrganizationMode::ByPerformer,
        }
    }

    /// Set the organization mode for this builder
    pub fn with_mode(mut self, mode: OrganizationMode) -> Self {
        self.mode = mode;
        self
    }

    /// Get current properties from the stack
    fn current_props(&self) -> &ItemProperties {
        self.property_stack.last().unwrap()
    }

    /// Update track with current properties
    fn apply_properties(&self, track: &mut Track) {
        let mut props = self.current_props().clone();
        props.original_name = Some(track.name.0.clone());
        let _ = track.set_item_properties(&props);
    }

    /// Add a BUS track and set it as the current parent for subsequent tracks
    pub fn bus(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        let mut track = create_track(&name_str, Some("BUS"), parent.as_deref(), &[]);
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        self.apply_properties(&mut track);
        self.tracks.push(track);
        self.parent_stack.push(name_str);
        
        // Push a copy of properties for the new level
        let next_props = self.current_props().clone();
        self.property_stack.push(next_props);
        
        self
    }

    /// Add a SUM track and set it as the current parent for subsequent tracks
    pub fn sum(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        let mut track = create_track(&name_str, Some("SUM"), parent.as_deref(), &[]);
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        self.apply_properties(&mut track);
        self.tracks.push(track);
        self.parent_stack.push(name_str);
        
        // Push a copy of properties for the new level
        let next_props = self.current_props().clone();
        self.property_stack.push(next_props);
        
        self
    }

    /// Add a regular track under the current parent
    pub fn track(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        let mut track = create_track(&name_str, None, parent.as_deref(), &[]);
        track.is_folder = false;
        track.folder_depth_change = FolderDepthChange::Normal;
        self.apply_properties(&mut track);
        self.tracks.push(track);
        self
    }

    /// Add all tracks from another template under the current parent
    pub fn add_template(mut self, template: Template) -> Self {
        let current_parent = self.parent_stack.last().cloned();
        for mut track in template.tracks {
            // If the track doesn't have a parent, give it the current builder's parent
            if track.parent_name().is_none() {
                if let Some(ref p) = current_parent {
                    track.set_parent_name(p);
                }
            }
            self.tracks.push(track);
        }
        self
    }

    /// Add a complex instrument entry (Arrangement -> Layers -> Sources)
    /// 
    /// If layers is empty, it creates: Arrangement -> Sources
    /// If layers is not empty, it creates: Arrangement -> Layer -> Sources
    /// 
    /// The actual hierarchy produced depends on the builder's OrganizationMode.
    pub fn entry(mut self, arrangement: &str, layers: &[&str], sources: &[&str]) -> Self {
        match self.mode {
            OrganizationMode::ByPerformer => {
                // Current level is Performer (or root).
                // We add Arrangement -> [Layers] -> Sources
                if layers.is_empty() {
                    // Single entry: Arrangement (Folder/Track) -> Sources (Children)
                    self = self.arrangement(arrangement);
                    for source in sources {
                        self = self.source(*source);
                    }
                    self.end()
                } else {
                    // Layered entry: Arrangement (Folder) -> Layer (Folder/Track) -> Sources (Children)
                    self = self.arrangement(arrangement);
                    for layer in layers {
                        self = self.layer(*layer);
                        for source in sources {
                            self = self.source(*source);
                        }
                        self = self.end();
                    }
                    self.end()
                }
            }
            OrganizationMode::ByArrangement => {
                // Current level is root.
                // We want: Arrangement (Performer) -> [Layers] -> Sources
                
                let performer = self.current_props().performer.clone();
                let display_name = match performer {
                    Some(p) => format!("{} ({})", arrangement, p),
                    None => arrangement.to_string(),
                };
                
                if layers.is_empty() {
                    self = self.arrangement(display_name);
                    for source in sources {
                        self = self.source(*source);
                    }
                    self.end()
                } else {
                    self = self.arrangement(display_name);
                    for layer in layers {
                        self = self.layer(*layer);
                        for source in sources {
                            self = self.source(*source);
                        }
                        self = self.end();
                    }
                    self.end()
                }
            }
        }
    }

    /// Set visibility modes for the last added track
    pub fn modes(mut self, modes: &[GroupMode]) -> Self {
        if let Some(track) = self.tracks.last_mut() {
            track.set_modes(modes);
        }
        self
    }
    
    /// Set the track type for the last added track
    pub fn with_type(mut self, track_type: &str) -> Self {
        if let Some(track) = self.tracks.last_mut() {
            track.set_track_type(track_type);
        }
        self
    }

    /// Go up one level in the hierarchy
    pub fn end(mut self) -> Self {
        if self.parent_stack.pop().is_some() {
            // When a folder ends, the LAST track added at this level (or deeper) 
            // must close the level.
            if let Some(last_track) = self.tracks.last_mut() {
                let current = last_track.folder_depth_change.to_reaper_value();
                last_track.folder_depth_change = FolderDepthChange::from_reaper_value(current - 1);
            }
            
            // Pop the properties for this level
            self.property_stack.pop();
        }
        self
    }

    /// Build the Template
    pub fn build(mut self) -> Template {
        // Automatically close any remaining open folders
        while !self.parent_stack.is_empty() {
            self = self.end();
        }
        
        Template {
            name: self.name.into(),
            tracks: self.tracks,
        }
    }

    // --- Property Context Helpers (No track creation) ---

    /// Set the performer context for subsequent tracks
    pub fn with_performer(mut self, name: impl Into<String>) -> Self {
        if let Some(props) = self.property_stack.last_mut() {
            props.performer = Some(name.into());
        }
        self
    }

    /// Set the arrangement context for subsequent tracks
    pub fn with_arrangement(mut self, name: impl Into<String>) -> Self {
        if let Some(props) = self.property_stack.last_mut() {
            props.arrangement = Some(name.into());
        }
        self
    }

    // --- Semantic Aliases with Property Tracking ---

    /// Alias for bus() to represent a Performer folder
    pub fn performer(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        // Update properties for the new level BEFORE calling bus()
        if let Some(props) = self.property_stack.last_mut() {
            props.performer = Some(name_str.clone());
        }
        self.bus(name_str)
    }

    /// Alias for bus() to represent an Arrangement folder/track
    pub fn arrangement(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        // Update properties for the new level BEFORE calling bus()
        if let Some(props) = self.property_stack.last_mut() {
            props.arrangement = Some(name_str.clone());
        }
        self.bus(name_str)
    }

    /// Alias for bus() to represent a Layer (e.g. Main, DBL) folder/track
    pub fn layer(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        // Update properties for the new level BEFORE calling bus()
        if let Some(props) = self.property_stack.last_mut() {
            props.layers = Some(name_str.clone());
        }
        self.bus(name_str)
    }

    /// Alias for track() to represent a Source (e.g. DI, NO-FX) track
    pub fn source(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        // Update properties for this track
        if let Some(props) = self.property_stack.last_mut() {
            let mut multi_mic = props.multi_mic.clone().unwrap_or_default();
            multi_mic.push(name_str.clone());
            props.multi_mic = Some(multi_mic);
        }
        self.track(name_str)
    }
}
