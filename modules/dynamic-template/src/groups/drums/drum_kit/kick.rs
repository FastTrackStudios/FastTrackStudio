//! Kick drum group definition

use crate::item_metadata::prelude::*;

/// Kick drum group
pub struct Kick;

impl From<Kick> for ItemMetadataGroup {
    fn from(_val: Kick) -> Self {
        // Define multi-mic positions as a Group
        let multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["In", "Out", "Top", "Bottom"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Kick")
            .patterns(["kick", "kik", "bd", "bassdrum", "bass_drum"])
            .multi_mic(multi_mic)
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

        pub fn kick_in_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Kick In";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .group("Kick")
                .multi_mic("In")
                .build();
            (input, expected)
        }

        pub fn kick_out_parses_multi_mic() -> (&'static str, ItemMetadata) {
            let input = "Kick Out";
            let expected = ItemMetadataBuilder::new()
                .original_name(input)
                .group("Kick")
                .multi_mic("Out")
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
        fn kick_in_parses_multi_mic_field() {
            let (input, expected) = test_cases::kick_in_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Isolated config: 'Kick In' should parse multi_mic field as ['In']");
        }

        #[test]
        fn kick_out_parses_multi_mic_field() {
            let (input, expected) = test_cases::kick_out_parses_multi_mic();
            let config = isolated_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Isolated config: 'Kick Out' should parse multi_mic field as ['Out']");
        }
    }

    /// Tests with integration configuration (default config with all groups)
    mod integration {
        use super::*;

        #[test]
        fn kick_in_parses_multi_mic_field() {
            let (input, expected) = test_cases::kick_in_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Integration config: 'Kick In' should parse multi_mic field as ['In']");
        }

        #[test]
        fn kick_out_parses_multi_mic_field() {
            let (input, expected) = test_cases::kick_out_parses_multi_mic();
            let config = default_config();
            let parser = Parser::new(config);
            let item = parser.parse(input.to_string()).unwrap();

            assert_eq!(item.metadata, expected, "Integration config: 'Kick Out' should parse multi_mic field as ['Out']");
        }
    }
}
