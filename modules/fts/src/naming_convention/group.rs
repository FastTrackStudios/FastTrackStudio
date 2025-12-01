//! Group structure for FTS naming convention
//! 
//! Defines the hierarchical organization of groups in the naming convention.
//! Groups can be nested recursively to create hierarchies of any depth.
//! 
//! Example hierarchy:
//! - Band Section
//!   - Drums
//!     - Kick
//!     - Snare
//!     - Hi-Hat
//!   - Guitars
//!     - Electric
//!     - Acoustic

use crate::component_patterns::{ComponentPatternProvider, ComponentPatterns};
use crate::component::ComponentType;
use crate::multi_mic_descriptor::MultiMicDescriptor;

/// Builder for multi-mic descriptors in a fluent API
pub struct MultiMicBuilder<'a> {
    group: &'a mut FullGroup,
    descriptor: MultiMicDescriptor,
}

impl<'a> MultiMicBuilder<'a> {
    fn new(group: &'a mut FullGroup, name: impl Into<String>) -> Self {
        Self {
            group,
            descriptor: MultiMicDescriptor::new(name),
        }
    }
    
    /// Add patterns to this multi-mic descriptor
    pub fn patterns(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.descriptor.patterns.extend(patterns.into_iter().map(|p| p.into()));
        self
    }
    
    /// Add negative patterns to this multi-mic descriptor
    pub fn negative_patterns(mut self, patterns: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.descriptor.negative_patterns.extend(patterns.into_iter().map(|p| p.into()));
        self
    }
}

impl<'a> Drop for MultiMicBuilder<'a> {
    fn drop(&mut self) {
        self.group.multi_mic_descriptors.push(self.descriptor.clone());
    }
}

/// Defines the behavior type for groups
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GroupType {
    /// Standard group with no special parsing behavior
    Static,
    /// Supports numbered instances (Tom 1, Tom 2, etc.) - numbers parsed as increments
    Increment,
    /// Creates nested track structures dynamically
    DynamicHierarchy,
}

impl GroupType {
    pub fn as_str(&self) -> &'static str {
        match self {
            GroupType::Static => "Static",
            GroupType::Increment => "Increment", 
            GroupType::DynamicHierarchy => "Dynamic-Hierarchy",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "Static" => Some(GroupType::Static),
            "Increment" => Some(GroupType::Increment),
            "Dynamic-Hierarchy" => Some(GroupType::DynamicHierarchy),
            _ => None,
        }
    }
}

impl Default for GroupType {
    fn default() -> Self {
        GroupType::Static
    }
}

/// Core functionality for track groups
pub trait Group {
    fn name(&self) -> &str;
    fn prefix(&self) -> &str;
    fn patterns(&self) -> &[String];
    fn negative_patterns(&self) -> &[String];
    fn priority(&self) -> i32;
    fn parent_track(&self) -> Option<&str>;
    fn group_type(&self) -> Option<&GroupType>;
}

/// Full group definition with all configuration
/// 
/// Groups can be nested recursively to create hierarchies of any depth.
/// Each group can have child groups, which can themselves have children, etc.
/// 
/// Component patterns are stored using a trait-based system, allowing patterns
/// to be attached to any component type (multi-mic, arrangement, section, etc.).
#[derive(Debug, Clone)]
pub struct FullGroup {
    pub name: String,
    pub prefix: String,
    pub patterns: Vec<String>,
    pub negative_patterns: Vec<String>,
    pub priority: i32,
    pub parent_track: Option<String>,
    
    // Component patterns (trait-based, flexible)
    pub component_patterns: ComponentPatterns,
    
    // Multi-mic descriptors (each with their own patterns)
    pub multi_mic_descriptors: Vec<MultiMicDescriptor>,
    
    // Nested child groups (recursive structure)
    pub children: Vec<FullGroup>,
    
    // Group type for behavior control
    pub group_type: Option<GroupType>,
}

impl Group for FullGroup {
    fn name(&self) -> &str {
        &self.name
    }

    fn prefix(&self) -> &str {
        &self.prefix
    }

    fn patterns(&self) -> &[String] {
        &self.patterns
    }

    fn negative_patterns(&self) -> &[String] {
        &self.negative_patterns
    }

    fn priority(&self) -> i32 {
        self.priority
    }

    fn parent_track(&self) -> Option<&str> {
        self.parent_track.as_deref()
    }

    fn group_type(&self) -> Option<&GroupType> {
        self.group_type.as_ref()
    }
}

impl FullGroup {
    /// Create a new empty group
    pub fn new(name: impl Into<String>, prefix: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            prefix: prefix.into(),
            patterns: Vec::new(),
            negative_patterns: Vec::new(),
            priority: 0,
            parent_track: None,
            component_patterns: ComponentPatterns::new(),
            multi_mic_descriptors: Vec::new(),
            children: Vec::new(),
            group_type: None,
        }
    }
    
    /// Start building a multi-mic descriptor (fluent API)
    /// 
    /// Example:
    /// ```rust
    /// group.multi_mic("In")
    ///     .patterns(["Inside", "Internal"])
    ///     .negative_patterns(["Input"]);
    /// ```
    pub fn multi_mic(&mut self, name: impl Into<String>) -> MultiMicBuilder<'_> {
        MultiMicBuilder::new(self, name)
    }
    
    /// Add a multi-mic descriptor (mutating, for backward compatibility)
    pub fn add_multi_mic_descriptor(&mut self, descriptor: MultiMicDescriptor) {
        self.multi_mic_descriptors.push(descriptor);
    }
    
    /// Find a multi-mic descriptor by name
    pub fn find_multi_mic_descriptor(&self, name: &str) -> Option<&MultiMicDescriptor> {
        self.multi_mic_descriptors.iter().find(|d| d.name.eq_ignore_ascii_case(name))
    }
    
    /// Find a multi-mic descriptor by name (mutable)
    pub fn find_multi_mic_descriptor_mut(&mut self, name: &str) -> Option<&mut MultiMicDescriptor> {
        self.multi_mic_descriptors.iter_mut().find(|d| d.name.eq_ignore_ascii_case(name))
    }
    
    /// Get all multi-mic descriptor names
    pub fn multi_mic_descriptor_names(&self) -> Vec<String> {
        self.multi_mic_descriptors.iter().map(|d| d.name.clone()).collect()
    }
    
    // Backward compatibility getters for component patterns
    
    /// Get arrangement patterns (backward compatibility)
    pub fn arrangement_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Arrangement)
    }
    
    /// Get section patterns (backward compatibility)
    pub fn section_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Section)
    }
    
    /// Get layers patterns (backward compatibility)
    pub fn layers_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Layers)
    }
    
    /// Get multi-mic patterns (backward compatibility)
    /// Returns the descriptor names (e.g., ["In", "Out", "Trig", "Sub", "Ambient"])
    pub fn multi_mic_patterns(&self) -> Vec<String> {
        if !self.multi_mic_descriptors.is_empty() {
            self.multi_mic_descriptor_names()
        } else {
            // Fallback to component patterns for backward compatibility
            self.component_patterns.get_patterns(ComponentType::MultiMic)
        }
    }
    
    /// Get track type patterns (backward compatibility)
    pub fn track_type_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::TrackType)
    }
    
    /// Get rec tag patterns (backward compatibility)
    pub fn rec_tag_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::RecTag)
    }
    
    /// Get performer patterns (backward compatibility)
    pub fn performer_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Performer)
    }
    
    /// Get playlist patterns (backward compatibility)
    pub fn playlist_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Playlist)
    }
    
    /// Get effect patterns (backward compatibility)
    pub fn effect_patterns(&self) -> Vec<String> {
        self.component_patterns.get_patterns(ComponentType::Effect)
    }
    
    // Setters for component patterns
    
    /// Set arrangement patterns
    pub fn set_arrangement_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Arrangement, patterns);
    }
    
    /// Set section patterns
    pub fn set_section_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Section, patterns);
    }
    
    /// Set layers patterns
    pub fn set_layers_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Layers, patterns);
    }
    
    /// Set multi-mic patterns
    pub fn set_multi_mic_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::MultiMic, patterns);
    }
    
    /// Set track type patterns
    pub fn set_track_type_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::TrackType, patterns);
    }
    
    /// Set rec tag patterns
    pub fn set_rec_tag_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::RecTag, patterns);
    }
    
    /// Set performer patterns
    pub fn set_performer_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Performer, patterns);
    }
    
    /// Set playlist patterns
    pub fn set_playlist_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Playlist, patterns);
    }
    
    /// Set effect patterns
    pub fn set_effect_patterns(&mut self, patterns: Vec<String>) {
        self.component_patterns.set_patterns(ComponentType::Effect, patterns);
    }
    
    /// Get patterns for any component type
    pub fn get_component_patterns(&self, component_type: ComponentType) -> Vec<String> {
        self.component_patterns.get_patterns(component_type)
    }
    
    /// Set patterns for any component type
    pub fn set_component_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>) {
        self.component_patterns.set_patterns(component_type, patterns);
    }
    
    /// Get negative patterns for any component type
    pub fn get_component_negative_patterns(&self, component_type: ComponentType) -> Vec<String> {
        self.component_patterns.get_negative_patterns(component_type)
    }
    
    /// Set negative patterns for any component type
    pub fn set_component_negative_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>) {
        self.component_patterns.set_negative_patterns(component_type, patterns);
    }
    
    /// Add a child group
    pub fn add_child(&mut self, child: FullGroup) {
        self.children.push(child);
    }
    
    /// Find a child group by name
    pub fn find_child(&self, name: &str) -> Option<&FullGroup> {
        self.children.iter().find(|g| g.name == name)
    }
    
    /// Find a child group by name (mutable)
    pub fn find_child_mut(&mut self, name: &str) -> Option<&mut FullGroup> {
        self.children.iter_mut().find(|g| g.name == name)
    }
    
    /// Find a child group by prefix
    pub fn find_child_by_prefix(&self, prefix: &str) -> Option<&FullGroup> {
        self.children.iter().find(|g| g.prefix == prefix)
    }
    
    /// Find a child group by prefix (mutable)
    pub fn find_child_by_prefix_mut(&mut self, prefix: &str) -> Option<&mut FullGroup> {
        self.children.iter_mut().find(|g| g.prefix == prefix)
    }
    
    /// Get all descendant groups (children, grandchildren, etc.) recursively
    pub fn all_descendants(&self) -> Vec<&FullGroup> {
        let mut result = Vec::new();
        for child in &self.children {
            result.push(child);
            result.extend(child.all_descendants());
        }
        result
    }
    
    /// Get arrangement patterns for this group, considering child group context
    pub fn get_arrangement_patterns(&self, child_group: Option<&FullGroup>) -> Vec<String> {
        if let Some(child) = child_group {
            let child_patterns = child.get_component_patterns(ComponentType::Arrangement);
            if !child_patterns.is_empty() {
                return child_patterns;
            }
        }
        self.get_component_patterns(ComponentType::Arrangement)
    }
    
    /// Get section patterns for this group, with optional child group override
    pub fn get_section_patterns(&self, child_group: Option<&FullGroup>) -> Vec<String> {
        if let Some(child) = child_group {
            let child_patterns = child.get_component_patterns(ComponentType::Section);
            if !child_patterns.is_empty() {
                return child_patterns;
            }
        }
        self.get_component_patterns(ComponentType::Section)
    }
    
    /// Get multi-mic patterns for this group, with optional child group override
    pub fn get_multi_mic_patterns(&self, child_group: Option<&FullGroup>) -> Vec<String> {
        if let Some(child) = child_group {
            let child_patterns = child.get_component_patterns(ComponentType::MultiMic);
            if !child_patterns.is_empty() {
                return child_patterns;
            }
        }
        self.get_component_patterns(ComponentType::MultiMic)
    }
    
    /// Check if this group has any children
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
    
    /// Get the depth of this group in the hierarchy (0 = root, 1 = first level, etc.)
    pub fn depth(&self) -> usize {
        if self.children.is_empty() {
            0
        } else {
            1 + self.children.iter().map(|c| c.depth()).max().unwrap_or(0)
        }
    }
    
    /// Get the total number of descendant groups (including all nested levels)
    pub fn descendant_count(&self) -> usize {
        self.children.len() + self.children.iter().map(|c| c.descendant_count()).sum::<usize>()
    }
}
