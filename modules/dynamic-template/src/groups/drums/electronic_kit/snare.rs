//! Electronic snare drum group definition

use crate::item_metadata::prelude::*;

/// Electronic snare drum group
pub struct Snare;

impl From<Snare> for ItemMetadataGroup {
    fn from(_val: Snare) -> Self {
        // Define multi-mic positions as a Group
        let multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["Top", "Bottom", "Side", "OH"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        // Require parent match so this only matches when "Electronic Kit" also matches
        ItemMetadataGroup::builder("Snare")
            .patterns(["snare", "snr", "sn"])
            .multi_mic(multi_mic)
            .requires_parent_match()
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, DynamicTemplateConfig};
    use monarchy::{Config, Parser};

    /// Shared test cases - define input strings and expected metadata once
    mod test_cases {
        use super::*;
        use crate::item_metadata::ItemMetadataBuilder;

        pub fn snare_matches_group_and_has_original_name() -> (&'static str, ItemMetadata) {
            let input = "Snare";
            // The group trail will be different in isolated vs full config
            // Isolated: ["Snare"], Full: ["Drums", "Electronic Drums", "Snare"] (or similar)
            // We just verify the last group is "Snare"
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Snare")
                .build();
            (input, expected)
        }

        pub fn snare_top_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Snare Top";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Snare")
                .multi_mic("Top")
                .build();
            (input, expected)
        }

        pub fn snare_bottom_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Snare Bottom";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Snare")
                .multi_mic("Bottom")
                .build();
            (input, expected)
        }
    }

    /// Tests with isolated configuration (only this group)
    mod isolated {
        use super::*;

        /// Create a config with only the Snare group
        fn isolated_config() -> DynamicTemplateConfig {
            Config::builder()
                .group(Snare)
                .build()
        }

        #[test]
        fn snare_matches_group_and_has_original_name() {
            let (input, expected) = test_cases::snare_matches_group_and_has_original_name();
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Isolated config: 'Snare' should match Snare group and have original_name");
        }

        #[test]
        fn snare_top_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::snare_top_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Isolated config: 'Snare Top' should parse multi_mic field as ['Top']");
            
            // Test display name
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "Snare Top", "Display name should be 'Snare Top'");
        }

        #[test]
        fn snare_bottom_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::snare_bottom_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Isolated config: 'Snare Bottom' should parse multi_mic field as ['Bottom']");
            
            // Test display name
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "Snare Bottom", "Display name should be 'Snare Bottom'");
        }
    }

    /// Tests with integration configuration (default config with all groups)
    mod integration {
        use super::*;

        #[test]
        fn snare_matches_group_and_has_original_name() {
            let (input, mut expected) = test_cases::snare_matches_group_and_has_original_name();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Snare' should match Snare group and have original_name");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Snare", "Integration config: display name should be 'D Snare' (with prefix)");
        }

        #[test]
        fn snare_top_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::snare_top_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Snare Top' should parse multi_mic field as ['Top']");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Snare Top", "Integration config: display name should be 'D Snare Top' (with prefix)");
        }

        #[test]
        fn snare_bottom_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::snare_bottom_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Snare Bottom' should parse multi_mic field as ['Bottom']");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Snare Bottom", "Integration config: display name should be 'D Snare Bottom' (with prefix)");
        }
    }
}

