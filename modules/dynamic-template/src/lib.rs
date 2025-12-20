use daw::tracks::item::Item;
use daw::tracks::track::Track;
use monarchy::test_utils::StructureAssertions;
use monarchy::*;
use serde::{Deserialize, Serialize};

mod groups;
mod item_metadata;

pub use groups::Drums;
pub use item_metadata::ItemMetadata;

/// Type alias for our standard Config with ItemMetadata
pub type DynamicTemplateConfig = Config<ItemMetadata>;

/// Dynamic template system for organizing DAW items
pub struct DynamicTemplate;

impl DynamicTemplate {
    pub fn new() -> Self {
        Self
    }
}

impl Default for DynamicTemplateConfig {
    fn default() -> Self {
        Config::builder()
            .group(Drums)
            .fallback(FallbackStrategy::CreateMisc)
            .build()
    }
}

// Make Item work with Monarchy by implementing Into<String>
impl Into<String> for Item {
    fn into(self) -> String {
        self.properties
            .original_name
            .unwrap_or_else(|| self.id.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kick_and_drumkit() {
        // Just simple string inputs - the system figures out what they are
        let inputs = vec!["kick_in.wav", "kick_out.wav", "snare.wav"];
        // Use the default config
        let config = DynamicTemplateConfig::default();
        let result = monarchy_sort(inputs, config).unwrap();

        // Print the structure
        result.print_tree();

        // Verify
        result
            .assert()
            .has_total_items(3)
            .has_groups(1)
            .group("Drums")
            .has_groups(1)
            .group("Kick")
            .contains_exactly(&["kick_in.wav", "kick_out.wav"])
            .done();
    }
}
