use daw::tracks::item::Item;
use monarchy::*;

mod groups;
mod item_metadata;

pub use groups::{
    Bass, Choir, Drums, Effects, Guitars, Keys, Orchestra, Percussion, Synths, Vocals,
};
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

/// Creates a default configuration for the dynamic template system
pub fn default_config() -> DynamicTemplateConfig {
    Config::builder()
        .group(Drums)
        .group(Percussion)
        .group(Bass)
        .group(Guitars)
        .group(Keys)
        .group(Synths)
        .group(Vocals)
        .group(Choir)
        .group(Orchestra)
        .group(Effects)
        .fallback(FallbackStrategy::CreateMisc)
        .build()
}

// Helper trait to convert Item to String for Monarchy
pub trait ItemToString {
    fn to_monarchy_string(self) -> String;
}

impl ItemToString for Item {
    fn to_monarchy_string(self) -> String {
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
        let config = default_config();
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

    #[test]
    fn test_bass_types() {
        // Test different bass types
        let inputs = vec![
            "bass_guitar_di.wav",
            "synth_bass_sub.wav",
            "upright_pizz.wav",
            "electric_bass_amp.wav",
            "808_bass.wav",
            "acoustic_bass.wav",
        ];

        let config = default_config();
        let result = monarchy_sort(inputs, config).unwrap();

        // Print the structure
        result.print_tree();

        // Verify bass categorization
        result
            .assert()
            .has_total_items(6)
            .has_groups(1)
            .group("Bass")
            .has_groups(3)
            .group("Bass Guitar")
            .contains_exactly(&["bass_guitar_di.wav", "electric_bass_amp.wav"])
            .done()
            .group("Bass")
            .group("Synth Bass")
            .contains_exactly(&["synth_bass_sub.wav", "808_bass.wav"])
            .done()
            .group("Bass")
            .group("Upright Bass")
            .contains_exactly(&["upright_pizz.wav", "acoustic_bass.wav"])
            .done();
    }
}
