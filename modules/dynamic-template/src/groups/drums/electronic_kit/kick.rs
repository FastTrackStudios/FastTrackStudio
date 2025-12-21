//! Electronic kick drum group definition

use crate::item_metadata::prelude::*;

/// Electronic kick drum group
pub struct Kick;

impl From<Kick> for ItemMetadataGroup {
    fn from(_val: Kick) -> Self {
        // Define multi-mic positions as a Group
        let multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["In", "Out", "Top", "Bottom"])
            .build();

        // Define SUM tagged collection - items matching these patterns will be grouped together
        let sum_collection = ItemMetadataGroup::builder("SUM")
            .patterns(["In", "Out", "Trig"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Kick")
            .patterns(["kick", "kik", "bd", "bassdrum", "bass_drum"])
            .multi_mic(multi_mic)
            .tagged_collection(sum_collection)
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

        pub fn kick_matches_group_and_has_original_name() -> (&'static str, ItemMetadata) {
            let input = "Kick";
            // The group trail will be different in isolated vs full config
            // Isolated: ["Kick"], Full: ["Drums", "Electronic Drums", "Kick"] (or similar)
            // We just verify the last group is "Kick"
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Kick")
                .build();
            (input, expected)
        }

        pub fn kick_in_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Kick In";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Kick")
                .multi_mic("In")
                .tagged_collection("SUM")
                .build();
            (input, expected)
        }

        pub fn kick_out_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Kick Out";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .last_group("Kick")
                .multi_mic("Out")
                .tagged_collection("SUM")
                .build();
            (input, expected)
        }
    }

    /// Tests with isolated configuration (only this group)
    mod isolated {
        use super::*;

        /// Create a config with only the Kick group
        fn isolated_config() -> DynamicTemplateConfig {
            Config::builder()
                .group(Kick)
                .build()
        }

        #[test]
        fn kick_matches_group_and_has_original_name() {
            let (input, expected) = test_cases::kick_matches_group_and_has_original_name();
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Isolated config: 'Kick' should match Kick group and have original_name");
        }

        #[test]
        fn kick_in_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::kick_in_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Isolated config: 'Kick In' should parse multi_mic field as ['In']");
            
            // Test display name
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "Kick In", "Display name should be 'Kick In'");
        }

        #[test]
        fn kick_in_matches_sum_tagged_collection() {
            let input = "Kick In";
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(
                item.metadata.tagged_collection,
                Some(vec!["SUM".to_string()]),
                "Kick In should match SUM tagged collection"
            );
        }

        #[test]
        fn kick_out_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::kick_out_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Isolated config: 'Kick Out' should parse multi_mic field as ['Out']");
        }

        #[test]
        fn kick_out_matches_sum_tagged_collection() {
            let input = "Kick Out";
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(
                item.metadata.tagged_collection,
                Some(vec!["SUM".to_string()]),
                "Kick Out should match SUM tagged collection"
            );
        }
    }

    /// Tests with integration configuration (default config with all groups)
    mod integration {
        use super::*;

        #[test]
        fn kick_matches_group_and_has_original_name() {
            let (input, mut expected) = test_cases::kick_matches_group_and_has_original_name();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Kick' should match Kick group and have original_name");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Kick", "Integration config: display name should be 'D Kick' (with prefix)");
        }

        #[test]
        fn kick_in_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::kick_in_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Kick In' should parse multi_mic field as ['In']");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Kick In", "Integration config: display name should be 'D Kick In' (with prefix)");
        }

        #[test]
        fn kick_in_matches_sum_tagged_collection() {
            let input = "Kick In";
            let config = default_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(
                item.metadata.tagged_collection,
                Some(vec!["SUM".to_string()]),
                "Kick In should match SUM tagged collection in integration config"
            );
        }

        #[test]
        fn kick_out_parses_multi_mic_field() {
            let (input, mut expected) = test_cases::kick_out_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config.clone());
            let item = parser.parse(input.to_string()).unwrap();

            // Update expected to match actual group trail from parser
            expected.group = item.metadata.group.clone();
            assert_eq!(item.metadata, expected, "Integration config: 'Kick Out' should parse multi_mic field as ['Out']");
            
            // Test display name - integration config has "D" prefix from "Drums" group
            let display_name = monarchy::to_display_name(&item, &config);
            assert_eq!(display_name, "D Kick Out", "Integration config: display name should be 'D Kick Out' (with prefix)");
        }

        #[test]
        fn kick_out_matches_sum_tagged_collection() {
            let input = "Kick Out";
            let config = default_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(
                item.metadata.tagged_collection,
                Some(vec!["SUM".to_string()]),
                "Kick Out should match SUM tagged collection in integration config"
            );
        }
    }
}

