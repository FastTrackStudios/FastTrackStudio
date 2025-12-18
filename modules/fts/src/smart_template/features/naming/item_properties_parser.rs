//! Parser for ItemProperties
//!
//! Parses track names like "D Kick In .02" into ItemProperties structs
//! that can be stored in Track.ext_state.
//!
//! Uses the two-phase fold pattern:
//! 1. Group Matching Phase: Fold over groups to find the best match
//! 2. Component Parsing Phase: Fold over components in ComponentOrder

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::components::{
    ComponentFoldParser,
    GroupFolder,
    DefaultGroupFolder,
};
use crate::smart_template::features::naming::default_groups::create_default_groups;
use crate::smart_template::core::models::group_config::GroupConfig;

/// Parser that converts track names into ItemProperties
/// 
/// Uses the fold pattern with two phases:
/// 1. Group matching (finds best matching group)
/// 2. Component parsing (extracts components in order)
pub struct ItemPropertiesParser {
    groups: Vec<GroupConfig>,
    group_folder: DefaultGroupFolder,
    component_parser: ComponentFoldParser,
}

impl ItemPropertiesParser {
    /// Create a new parser with default groups
    /// 
    /// This uses the default groups from the naming implementations.
    /// If you need custom groups, use `with_groups()` instead.
    pub fn new() -> Self {
        let groups = create_default_groups();
        Self {
            groups: groups.clone(),
            group_folder: DefaultGroupFolder::new(),
            component_parser: ComponentFoldParser::new(),
        }
    }
    
    /// Create a parser with custom groups
    pub fn with_groups(groups: Vec<GroupConfig>) -> Self {
        Self {
            groups: groups.clone(),
            group_folder: DefaultGroupFolder::new(),
            component_parser: ComponentFoldParser::new(),
        }
    }
    
    /// Parse a track name string into ItemProperties
    /// 
    /// Example: "D Kick In .02" -> ItemProperties with:
    /// - group_prefix: Some("D")
    /// - sub_type: Some(vec!["Kick"])
    /// - multi_mic: Some(vec!["In"])
    /// - playlist: Some(".02")
    /// 
    /// # Flow
    /// 
    /// 1. Phase 1: Fold over groups to find best match
    /// 2. Phase 2: Fold over components in ComponentOrder
    /// 3. Return complete ItemProperties
    /// 
    /// If no group matches, the NOT_SORTED group will be used (if present).
    pub fn parse_item_properties(&self, name: &str) -> ItemProperties {
        // Phase 1: Group Matching (Fold over groups)
        let mut matched_group = self.group_folder.fold_groups(name, &self.groups);
        
        // If no group matched and we have a NOT_SORTED group, use it
        if matched_group.is_none() {
            matched_group = self.groups.iter()
                .find(|g| g.name == "NOT_SORTED")
                .map(|g| g);
        }
        
        // Phase 2: Component Parsing (Fold over components)
        // ComponentFoldParser will extract group_prefix and sub_type from matched_group
        self.component_parser.parse_item_properties(name, matched_group)
    }
}

impl Default for ItemPropertiesParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Global function to parse FTS item properties from a track name
/// 
/// By default, uses the default groups configuration (all groups together).
/// You can optionally provide custom groups for testing or special cases.
/// 
/// # Examples
/// 
/// ```rust
/// use fts::smart_template::naming::parse_fts_item_properties;
/// 
/// // Use default config (all groups)
/// let props = parse_fts_item_properties("D Kick In .02", None);
/// 
/// // Use custom groups
/// let custom_groups = vec![/* ... */];
/// let props = parse_fts_item_properties("D Kick In .02", Some(custom_groups));
/// ```
pub fn parse_fts_item_properties(name: &str, groups: Option<Vec<GroupConfig>>) -> ItemProperties {
    let parser = if let Some(custom_groups) = groups {
        ItemPropertiesParser::with_groups(custom_groups)
    } else {
        ItemPropertiesParser::new()
    };
    
    parser.parse_item_properties(name)
}
