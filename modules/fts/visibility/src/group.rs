//! Sorting group domain model
//! 
//! Groups represent collections of tracks organized for visibility management.
//! Groups can be nested to create hierarchies of any depth (e.g., Drums → Kick, Orchestra → Winds).

use super::track_scope::TrackScope;
use super::view_mode::ViewMode;
use naming_convention::group::FullGroup;

/// Trait for sorting groups
/// Types implementing this trait can be used as groups and converted to the concrete `SortingGroupData` struct
pub trait SortingGroup {
    /// Get the group configuration
    fn config() -> super::group_config::GroupConfig;
    
    /// Build the concrete group from this definition
    fn build() -> SortingGroupData {
        let config = Self::config();
        let mut group = SortingGroupData::new(config.id, config.name);
        group.prefix = config.prefix.to_string();
        
        // Add children
        for child_name in config.children {
            group.add_child_by_name(child_name);
        }
        
        group
    }
    
    /// Get the group ID
    fn id() -> &'static str {
        Self::config().id
    }
    
    /// Get the group name
    fn name() -> &'static str {
        Self::config().name
    }
}

/// Helper trait to convert to SortingGroupData
/// This avoids blanket impl conflicts
pub trait ToSortingGroupData {
    fn to_group_data(self) -> SortingGroupData;
}

/// Types implementing SortingGroup can be converted
impl<G> ToSortingGroupData for G
where
    G: SortingGroup,
{
    fn to_group_data(self) -> SortingGroupData {
        G::build()
    }
}

/// SortingGroupData can be converted to itself
impl ToSortingGroupData for SortingGroupData {
    fn to_group_data(self) -> SortingGroupData {
        self
    }
}

/// Concrete sorting group data structure
/// Groups can be nested to create hierarchies of any depth
#[derive(Debug, Clone)]
pub struct SortingGroupData {
    /// Unique identifier
    pub id: String,
    
    /// Human-readable name
    pub name: String,
    
    /// Prefix from naming convention (e.g., "GTR", "D", "Bass")
    /// Used for matching tracks to this group
    pub prefix: String,
    
    /// Reference to the naming convention group this is based on (optional)
    pub naming_group: Option<String>,
    
    /// Track scope definition
    pub scope: TrackScope,
    
    /// Whether this is a global group (affects all tracks)
    pub is_global: bool,
    
    /// Whether the group is currently active (tracks visible)
    pub active: bool,
    
    /// View mode for this group (can override global)
    pub view_mode: Option<ViewMode>,
    
    /// Visual identification
    pub color: Option<u32>, // ARGB format
    pub icon: Option<String>,
    
    /// Selected snapshot IDs (one for TCP, one for MCP)
    pub selected_tcp_snapshot: Option<String>,
    pub selected_mcp_snapshot: Option<String>,
    
    /// Child groups (nested hierarchy support)
    /// Example: Drums group contains Kick, Snare, etc. as children
    pub children: Vec<SortingGroupData>,
}

impl SortingGroupData {
    /// Create a new sorting group
    pub fn new(id: impl Into<String>, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let id_val = id.into();
        
        Self {
            id: id_val,
            name: name_str.clone(),
            prefix: String::new(),
            naming_group: None,
            scope: TrackScope::new(),
            is_global: false,
            active: false,
            view_mode: None,
            color: None,
            icon: None,
            selected_tcp_snapshot: None,
            selected_mcp_snapshot: None,
            children: Vec::new(),
        }
    }
    
    /// Create a new sorting group with just a name (ID auto-generated from name)
    pub fn with_name(name: impl Into<String>) -> Self {
        let name_str = name.into();
        let id = name_str.to_uppercase().replace(" ", "_");
        Self::new(id, name_str)
    }
    
    /// Create from a naming convention group
    pub fn from_naming_group(naming_group: &FullGroup) -> Self {
        Self {
            id: naming_group.name.clone(),
            name: naming_group.name.clone(),
            prefix: naming_group.prefix.clone(),
            naming_group: Some(naming_group.name.clone()),
            scope: TrackScope::new(),
            is_global: false,
            active: false,
            view_mode: None,
            color: None,
            icon: None,
            selected_tcp_snapshot: None,
            selected_mcp_snapshot: None,
            children: Vec::new(),
        }
    }
    
    /// Set the parent track for this group
    pub fn set_parent_track(&mut self, track_id: impl Into<super::track_scope::TrackIdentifier>) {
        self.scope.parent_track = Some(track_id.into());
    }
    
    /// Add an additional track to the scope
    pub fn add_track(&mut self, track_id: impl Into<super::track_scope::TrackIdentifier>) {
        self.scope.add_track(track_id.into());
    }
    
    /// Add a child group (supports nested hierarchies)
    /// 
    /// Accepts anything that can be converted to SortingGroupData:
    /// - Types implementing `SortingGroup`: `add_child(Drums)` 
    /// - Concrete `SortingGroupData` instances: `add_child(my_group)`
    /// 
    /// The child's ID will be automatically generated from parent ID + child name if not set.
    pub fn add_child<G>(&mut self, group: G) -> &mut SortingGroupData
    where
        G: ToSortingGroupData,
    {
        let mut child = group.to_group_data();
        
        // Auto-generate child ID from parent ID + child name if not already set
        if child.id.is_empty() || child.id == child.name {
            child.id = format!("{}_{}", self.id, child.name.to_uppercase().replace(" ", "_"));
        }
        
        // Inherit prefix from parent if not set
        if child.prefix.is_empty() {
            child.prefix = self.prefix.clone();
        }
        
        self.children.push(child);
        self.children.last_mut().unwrap()
    }
    
    /// Create and add a child group with just a name (ID auto-generated)
    pub fn add_child_by_name(&mut self, name: impl Into<String>) -> &mut SortingGroupData {
        let name_str = name.into();
        let child_id = format!("{}_{}", self.id, name_str.to_uppercase().replace(" ", "_"));
        let mut child = SortingGroupData::new(child_id, name_str);
        child.prefix = self.prefix.clone(); // Inherit prefix from parent
        self.children.push(child);
        // Return mutable reference to the last child (the one we just added)
        self.children.last_mut().unwrap()
    }
    
    /// Check if this group has children
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
    
    /// Get all descendant groups (children and their children, recursively)
    pub fn all_descendants(&self) -> Vec<&SortingGroupData> {
        let mut descendants = Vec::new();
        for child in &self.children {
            descendants.push(child);
            descendants.extend(child.all_descendants());
        }
        descendants
    }
    
    /// Find a child group by ID (searches recursively)
    pub fn find_child(&self, id: &str) -> Option<&SortingGroupData> {
        for child in &self.children {
            if child.id == id {
                return Some(child);
            }
            if let Some(found) = child.find_child(id) {
                return Some(found);
            }
        }
        None
    }
    
    /// Find a child group by ID (mutable, searches recursively)
    pub fn find_child_mut(&mut self, id: &str) -> Option<&mut SortingGroupData> {
        for child in &mut self.children {
            if child.id == id {
                return Some(child);
            }
            if let Some(found) = child.find_child_mut(id) {
                return Some(found);
            }
        }
        None
    }
}
