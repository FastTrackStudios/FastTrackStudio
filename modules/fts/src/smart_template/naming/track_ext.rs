//! Track extension methods for ItemProperties
//!
//! Provides methods to work with ItemProperties on Track objects.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use daw::tracks::Track;
use serde_json;

/// Extension trait for Track to work with ItemProperties
pub trait TrackItemPropertiesExt {
    /// Get ItemProperties from ext_state (parsed from JSON)
    /// 
    /// This deserializes the ItemProperties from the ext_state field.
    /// Returns None if ext_state is empty or if deserialization fails.
    fn get_item_properties(&self) -> Option<ItemProperties>;
    
    /// Set ItemProperties in ext_state (serialized as JSON)
    /// 
    /// This serializes the ItemProperties to JSON and stores it in ext_state.
    fn set_item_properties(&mut self, properties: &ItemProperties) -> Result<(), serde_json::Error>;
    
    /// Parse track name and store as ItemProperties in ext_state
    /// 
    /// This is a convenience method that parses the track name and stores
    /// the result in ext_state.
    fn parse_and_store_item_properties(&mut self) -> Result<(), serde_json::Error>;
}

impl TrackItemPropertiesExt for Track {
    fn get_item_properties(&self) -> Option<ItemProperties> {
        self.ext_state.as_ref().and_then(|s| {
            serde_json::from_str(s).ok()
        })
    }
    
    fn set_item_properties(&mut self, properties: &ItemProperties) -> Result<(), serde_json::Error> {
        let json = serde_json::to_string(properties)?;
        self.ext_state = Some(json);
        Ok(())
    }
    
    fn parse_and_store_item_properties(&mut self) -> Result<(), serde_json::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(&self.name);
        self.set_item_properties(&props)
    }
}
