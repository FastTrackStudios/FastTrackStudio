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
    use crate::{default_config, DynamicTemplateConfig, OrganizeIntoTracks};
    use daw::tracks::TrackStructureBuilder;
    use monarchy::Config;

    /// Shared test cases - define expected structures once
    mod test_cases {
        use super::*;
        use daw::tracks::TrackStructureBuilder;

        pub fn single_item_creates_track_at_deepest_group_level() -> (Vec<&'static str>, daw::tracks::TrackStructureBuilder) {
            let items = vec!["Kick In"];
            let expected = TrackStructureBuilder::new()
                .track("Kick", "Kick In");
            (items, expected)
        }

        pub fn multiple_items_of_same_subgroup_create_folder_with_subtracks() -> (Vec<&'static str>, daw::tracks::TrackStructureBuilder) {
            let items = vec!["Kick In", "Kick Out"];
            let expected = TrackStructureBuilder::new()
                .folder("Kick")
                    .track("In", "Kick In")
                    .track("Out", "Kick Out")
                .end();
            (items, expected)
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
        fn single_item_creates_track_at_deepest_group_level() {
            let (items, expected_builder) = test_cases::single_item_creates_track_at_deepest_group_level();
            let config = isolated_config();
            let tracks = items.organize_into_tracks(&config, None).unwrap();
            let expected = expected_builder.build();

            assert_eq!(tracks, expected, "Isolated config: single item should create Kick track");
        }

        #[test]
        fn multiple_items_of_same_subgroup_create_folder_with_subtracks() {
            let (items, expected_builder) = test_cases::multiple_items_of_same_subgroup_create_folder_with_subtracks();
            let config = isolated_config();
            let tracks = items.organize_into_tracks(&config, None).unwrap();
            let expected = expected_builder.build();

            assert_eq!(tracks, expected, "Isolated config: multiple items should create Kick folder with subtracks");
        }
    }

    /// Tests with integration configuration (default config with all groups)
    mod integration {
        use super::*;

        #[test]
        fn single_item_creates_track_at_deepest_group_level() {
            let (items, expected_builder) = test_cases::single_item_creates_track_at_deepest_group_level();
            let config = default_config();
            let tracks = items.organize_into_tracks(&config, None).unwrap();
            let expected = expected_builder.build();

            assert_eq!(tracks, expected, "Integration config: single item should create Kick track");
        }

        #[test]
        fn multiple_items_of_same_subgroup_create_folder_with_subtracks() {
            let (items, expected_builder) = test_cases::multiple_items_of_same_subgroup_create_folder_with_subtracks();
            let config = default_config();
            let tracks = items.organize_into_tracks(&config, None).unwrap();
            let expected = expected_builder.build();

            assert_eq!(tracks, expected, "Integration config: multiple items should create Kick folder with subtracks");
        }
    }
}
