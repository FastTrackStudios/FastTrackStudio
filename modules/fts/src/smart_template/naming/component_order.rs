//! Component ordering for FTS naming convention
//! 
//! Defines the order in which components appear in track names.

use crate::smart_template::naming::item_properties::ComponentType;

/// Defines the standard order of components in an FTS track name
#[derive(Debug, Clone)]
pub struct ComponentOrder {
    pub order: Vec<ComponentOrderType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComponentOrderType {
    RecTag,
    GroupPrefix,
    SubType,
    Performer,
    Arrangement,
    Section,
    Layers,
    MultiMic,
    Effect,
    Increment,
    Channel,
    Playlist,
    TrackType,
}

impl ComponentOrderType {
    /// Convert to ComponentType
    pub fn to_component_type(&self) -> ComponentType {
        match self {
            ComponentOrderType::RecTag => ComponentType::RecTag,
            ComponentOrderType::GroupPrefix => ComponentType::GroupPrefix,
            ComponentOrderType::SubType => ComponentType::SubType,
            ComponentOrderType::Performer => ComponentType::Performer,
            ComponentOrderType::Arrangement => ComponentType::Arrangement,
            ComponentOrderType::Section => ComponentType::Section,
            ComponentOrderType::Layers => ComponentType::Layers,
            ComponentOrderType::MultiMic => ComponentType::MultiMic,
            ComponentOrderType::Effect => ComponentType::Effect,
            ComponentOrderType::Increment => ComponentType::Increment,
            ComponentOrderType::Channel => ComponentType::Channel,
            ComponentOrderType::Playlist => ComponentType::Playlist,
            ComponentOrderType::TrackType => ComponentType::TrackType,
        }
    }
    
    /// Get a string representation of the component order type
    pub fn as_str(&self) -> &'static str {
        match self {
            ComponentOrderType::RecTag => "RecTag",
            ComponentOrderType::GroupPrefix => "GroupPrefix",
            ComponentOrderType::SubType => "SubType",
            ComponentOrderType::Performer => "Performer",
            ComponentOrderType::Arrangement => "Arrangement",
            ComponentOrderType::Section => "Section",
            ComponentOrderType::Layers => "Layers",
            ComponentOrderType::MultiMic => "MultiMic",
            ComponentOrderType::Effect => "Effect",
            ComponentOrderType::Increment => "Increment",
            ComponentOrderType::Channel => "Channel",
            ComponentOrderType::Playlist => "Playlist",
            ComponentOrderType::TrackType => "TrackType",
        }
    }
}

impl From<ComponentOrderType> for ComponentType {
    fn from(order_type: ComponentOrderType) -> Self {
        order_type.to_component_type()
    }
}

impl ComponentOrder {
    /// Create the default component order (standard FTS convention)
    pub fn default() -> Self {
        Self {
            order: vec![
                ComponentOrderType::RecTag,
                ComponentOrderType::GroupPrefix,
                ComponentOrderType::SubType,
                ComponentOrderType::Performer,
                ComponentOrderType::Arrangement,
                ComponentOrderType::Section,
                ComponentOrderType::Layers,
                ComponentOrderType::MultiMic,
                ComponentOrderType::Effect,
                ComponentOrderType::TrackType,
                ComponentOrderType::Increment,
                ComponentOrderType::Channel,
                ComponentOrderType::Playlist,
            ],
        }
    }
    
    /// Get the position of a component type in the order
    pub fn position_of(&self, component: &ComponentOrderType) -> Option<usize> {
        self.order.iter().position(|c| c == component)
    }
    
    /// Check if one component should come before another
    pub fn comes_before(&self, first: &ComponentOrderType, second: &ComponentOrderType) -> bool {
        match (self.position_of(first), self.position_of(second)) {
            (Some(pos1), Some(pos2)) => pos1 < pos2,
            _ => false,
        }
    }
}
