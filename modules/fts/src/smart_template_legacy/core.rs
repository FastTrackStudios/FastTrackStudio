//! Smart Template Core
//!
//! Combines naming conventions and track templates to provide intelligent
//! track organization, matching, and visibility management.

use std::collections::HashMap;
use super::naming_convention::{Group, SimpleParser, TrackName};
use super::track_template::{Template, TrackList, Track, TrackMatcher};

/// A smart template that combines naming conventions with track templates
/// and integrates with the visibility manager for track selection.
#[derive(Debug, Clone)]
pub struct SmartTemplate {
    /// The naming convention group this template is based on
    group: Group,
    
    /// The track template structure
    template: Template,
    
    /// Parser for matching track names
    parser: SimpleParser,
    
    /// Matcher for finding tracks in the template
    matcher: TrackMatcher,
    
    /// Additional metadata
    metadata: HashMap<String, String>,
}

impl SmartTemplate {
    /// Create a new smart template from a naming convention group
    pub fn from_group(group: Group) -> Self {
        // Create a template from the group's track structure
        let template = Self::create_template_from_group(&group);
        
        // Create parser with all groups (we'll need to get all groups from somewhere)
        // For now, just use the single group
        let groups = vec![group.clone()];
        let parser = SimpleParser::new(groups);
        
        // Create matcher from the template
        let matcher = template.create_matcher();
        
        Self {
            group,
            template,
            parser,
            matcher,
            metadata: HashMap::new(),
        }
    }
    
    /// Create a template from a group definition
    fn create_template_from_group(group: &Group) -> Template {
        let mut track_list = TrackList::new();
        
        // Create parent track if specified
        if let Some(parent_track_name) = &group.parent_track {
            let mut parent_track = Track::with_type(parent_track_name.clone(), "BUS");
            track_list.add_track(parent_track);
        }
        
        // Create tracks for child groups recursively
        for child_group in &group.children {
            Self::add_group_tracks_to_list(&mut track_list, child_group, &group.parent_track);
        }
        
        Template::from_track_list(group.name.clone(), track_list)
    }
    
    /// Recursively add tracks from a group hierarchy to a track list
    fn add_group_tracks_to_list(
        track_list: &mut TrackList,
        group: &Group,
        parent_name: &Option<String>,
    ) {
        // Create track for this group
        let track_name = if let Some(parent) = parent_name {
            format!("{} {}", parent, group.name)
        } else {
            group.name.clone()
        };
        
        let mut track = Track::new(&track_name);
        
        // Set parent if we have one
        if let Some(parent) = parent_name {
            track.set_parent(parent);
        }
        
        // Set track type if parent_track suggests it's a BUS
        if group.parent_track.is_some() {
            track.track_type = Some("BUS".to_string());
        }
        
        track_list.add_track(track.clone());
        
        // Recursively add child groups
        for child_group in &group.children {
            Self::add_group_tracks_to_list(
                track_list,
                child_group,
                &Some(track_name.clone()),
            );
        }
    }
    
    /// Get the group this template is based on
    pub fn group(&self) -> &Group {
        &self.group
    }
    
    /// Get the template
    pub fn template(&self) -> &Template {
        &self.template
    }
    
    /// Get a mutable reference to the template
    pub fn template_mut(&mut self) -> &mut Template {
        &mut self.template
    }
    
    /// Get the matcher
    pub fn matcher(&self) -> &TrackMatcher {
        &self.matcher
    }
    
    /// Get a mutable reference to the matcher
    pub fn matcher_mut(&mut self) -> &mut TrackMatcher {
        &mut self.matcher
    }
    
    /// Parse a track name and find the best match in the template
    pub fn match_track_name(&self, track_name: &str) -> Option<super::track_template::MatchResult> {
        let parsed = self.parser.parse(track_name);
        self.matcher.find_best_match(&parsed)
    }
    
    /// Find or create a track for a given track name
    pub fn find_or_create_track(&mut self, track_name: &str) -> (Track, bool) {
        let parsed = self.parser.parse(track_name);
        self.matcher.find_or_create_track(&parsed, Some(track_name))
    }
    
    /// Get all tracks that match a given naming convention pattern
    pub fn find_tracks_by_pattern(&self, pattern: &str) -> Vec<&Track> {
        let parsed = self.parser.parse(pattern);
        // This would need to be implemented in TrackMatcher
        // For now, return empty
        vec![]
    }
    
    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
    
    /// Get metadata
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
}

/// Visibility group for track selection and visibility management
/// This represents a group in the visibility manager system
#[derive(Debug, Clone)]
pub struct VisibilityGroup {
    /// Group name
    pub name: String,
    
    /// Parent track GUID (for REAPER integration)
    pub parent_track_guid: Option<String>,
    
    /// Parent track name (for matching)
    pub parent_track_name: Option<String>,
    
    /// Additional track GUIDs in scope (beyond parent + children)
    pub additional_track_guids: Vec<String>,
    
    /// Whether this group is currently active
    pub active: bool,
    
    /// Whether this is a global group
    pub is_global: bool,
    
    /// Group color (for UI display)
    pub color: Option<u32>,
    
    /// Group icon (for UI display)
    pub icon: Option<String>,
    
    /// Selected TCP snapshot name
    pub selected_tcp: Option<String>,
    
    /// Selected MCP snapshot name
    pub selected_mcp: Option<String>,
}

impl VisibilityGroup {
    /// Create a new visibility group
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            parent_track_guid: None,
            parent_track_name: None,
            additional_track_guids: Vec::new(),
            active: false,
            is_global: false,
            color: Some(0x4444FFFF), // Default blue
            icon: None,
            selected_tcp: None,
            selected_mcp: None,
        }
    }
    
    /// Set the parent track
    pub fn set_parent_track(&mut self, guid: impl Into<String>, name: impl Into<String>) {
        self.parent_track_guid = Some(guid.into());
        self.parent_track_name = Some(name.into());
    }
    
    /// Add an additional track to the group scope
    pub fn add_track_to_scope(&mut self, guid: impl Into<String>) {
        self.additional_track_guids.push(guid.into());
    }
    
    /// Set the group as active
    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }
    
    /// Set the group color
    pub fn set_color(&mut self, color: u32) {
        self.color = Some(color);
    }
    
    /// Set the group icon
    pub fn set_icon(&mut self, icon: impl Into<String>) {
        self.icon = Some(icon.into());
    }
}

/// Smart template manager that combines templates with visibility groups
#[derive(Debug)]
pub struct SmartTemplateManager {
    /// Smart templates indexed by group name
    templates: HashMap<String, SmartTemplate>,
    
    /// Visibility groups
    visibility_groups: Vec<VisibilityGroup>,
    
    /// Parser for matching
    parser: SimpleParser,
}

impl SmartTemplateManager {
    /// Create a new smart template manager
    pub fn new() -> Self {
        #[cfg(feature = "default-groups")]
        let groups = super::naming_convention::create_default_groups();
        #[cfg(not(feature = "default-groups"))]
        let groups = Vec::new();
        
        let parser = SimpleParser::new(groups);
        
        Self {
            templates: HashMap::new(),
            visibility_groups: Vec::new(),
            parser,
        }
    }
    
    /// Create a smart template from a naming convention group
    pub fn create_template_from_group(&mut self, group: Group) -> &SmartTemplate {
        let template = SmartTemplate::from_group(group.clone());
        let name = group.name.clone();
        self.templates.insert(name.clone(), template);
        self.templates.get(&name).unwrap()
    }
    
    /// Create a visibility group from a smart template
    pub fn create_visibility_group(&mut self, template: &SmartTemplate) -> &mut VisibilityGroup {
        let mut vg = VisibilityGroup::new(template.group().name.clone());
        
        // Set parent track name from group
        if let Some(parent_track) = &template.group().parent_track {
            vg.parent_track_name = Some(parent_track.clone());
        }
        
        self.visibility_groups.push(vg);
        let index = self.visibility_groups.len() - 1;
        &mut self.visibility_groups[index]
    }
    
    /// Get a smart template by group name
    pub fn get_template(&self, group_name: &str) -> Option<&SmartTemplate> {
        self.templates.get(group_name)
    }
    
    /// Get a mutable smart template by group name
    pub fn get_template_mut(&mut self, group_name: &str) -> Option<&mut SmartTemplate> {
        self.templates.get_mut(group_name)
    }
    
    /// Get all visibility groups
    pub fn visibility_groups(&self) -> &[VisibilityGroup] {
        &self.visibility_groups
    }
    
    /// Get a mutable visibility group by name
    pub fn get_visibility_group_mut(&mut self, name: &str) -> Option<&mut VisibilityGroup> {
        self.visibility_groups.iter_mut().find(|g| g.name == name)
    }
    
    /// Match a track name against all templates and return the best match
    pub fn match_track(&self, track_name: &str) -> Option<(String, super::track_template::MatchResult)> {
        let parsed = self.parser.parse(track_name);
        
        let mut best_match: Option<(String, super::track_template::MatchResult)> = None;
        let mut best_score = 0;
        
        for (group_name, template) in &self.templates {
            if let Some(result) = template.matcher().find_best_match(&parsed) {
                if result.score > best_score {
                    best_score = result.score;
                    best_match = Some((group_name.clone(), result));
                }
            }
        }
        
        best_match
    }
}

impl Default for SmartTemplateManager {
    fn default() -> Self {
        Self::new()
    }
}
