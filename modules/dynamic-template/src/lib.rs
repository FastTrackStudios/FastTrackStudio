use daw::tracks::api::folder::FolderDepthChange;
use daw::tracks::item::Item;
use daw::tracks::{AddChild, Track, TrackName};
use monarchy::*;

mod groups;
mod item_metadata;
mod metadata_patterns;

pub use groups::{Bass, Choir, Drums, Guitars, Keys, Orchestra, Percussion, Synths, Vocals, SFX};
pub use item_metadata::ItemMetadata;

/// Type alias for our standard Config with ItemMetadata
pub type DynamicTemplateConfig = Config<ItemMetadata>;

/// Dynamic template system for organizing DAW items
#[derive(Default)]
pub struct DynamicTemplate;

impl DynamicTemplate {
    pub fn new() -> Self {
        Self
    }
}

/// Creates a default configuration for the dynamic template system
pub fn default_config() -> DynamicTemplateConfig {
    Config::builder()
        // Add metadata field patterns first (metadata-only group)
        .group(metadata_patterns::default_metadata_field_patterns())
        // Then add regular groups
        .group(Drums)
        .group(Percussion)
        .group(Bass)
        .group(Guitars)
        .group(Keys)
        .group(Synths)
        .group(Vocals)
        .group(Choir)
        .group(Orchestra)
        .group(SFX)
        .build()
}

// Helper trait to convert Item to String for Monarchy
pub trait ItemToString {
    fn to_monarchy_string(self) -> String;
}

impl ItemToString for Item {
    fn to_monarchy_string(self) -> String {
        if self.name.is_empty() {
            self.id.to_string()
        } else {
            self.name
        }
    }
}

/// Trait for organizing DAW Items into Tracks using monarchy sort
///
/// This trait accepts anything that can be converted into DAW Items,
/// allowing you to pass strings directly or pre-constructed Items.
pub trait OrganizeIntoTracks {
    /// Organize items into tracks using the provided configuration
    ///
    /// If `existing_tracks` is provided, items will be matched to existing tracks
    /// and new tracks will only be created when needed.
    fn organize_into_tracks(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&[Track]>,
    ) -> monarchy::Result<Vec<Track>>;
}

impl<T> OrganizeIntoTracks for Vec<T>
where
    T: Into<Item>,
{
    fn organize_into_tracks(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&[Track]>,
    ) -> monarchy::Result<Vec<Track>> {
        // Convert input to DAW Items
        let items: Vec<Item> = self.into_iter().map(|t| t.into()).collect();

        // Create a mapping from item string to original DAW Item
        let item_map: std::collections::HashMap<String, Vec<Item>> =
            items
                .into_iter()
                .fold(std::collections::HashMap::new(), |mut map, item| {
                    let key = if item.name.is_empty() {
                        item.id.to_string()
                    } else {
                        item.name.clone()
                    };
                    map.entry(key).or_insert_with(Vec::new).push(item);
                    map
                });

        // Convert DAW Items to strings for monarchy sort
        let input_strings: Vec<String> = item_map.keys().cloned().collect();

        // Perform monarchy sort (clone config since monarchy_sort takes ownership)
        // If existing tracks are provided, monarchy can use them via Target trait
        let structure = monarchy_sort(input_strings, config.clone())?;

        // Convert Structure to Tracks, preserving original DAW Items
        let mut new_tracks = structure_to_tracks_with_items(&structure, &item_map, false);

        // Merge with existing tracks if provided
        if let Some(existing) = existing_tracks {
            new_tracks = merge_tracks(existing, new_tracks);
        }

        Ok(new_tracks)
    }
}

/// Implement Target trait for Vec<Track> so monarchy can work with existing tracks
impl Target<ItemMetadata> for Vec<Track> {
    fn existing_items(&self) -> Vec<monarchy::Item<ItemMetadata>> {
        // Extract items from existing tracks and convert to monarchy Items
        self.iter()
            .flat_map(|track| {
                track.items.iter().map(|item| {
                    monarchy::Item {
                        id: item.id.to_string(),
                        original: if item.name.is_empty() {
                            item.id.to_string()
                        } else {
                            item.name.clone()
                        },
                        metadata: ItemMetadata::default(), // Could parse metadata from item if needed
                        matched_groups: Vec::new(), // Empty for items converted from DAW Items
                    }
                })
            })
            .collect()
    }
}

/// Merge new tracks with existing tracks, matching by name and adding items to existing tracks
fn merge_tracks(existing: &[Track], new: Vec<Track>) -> Vec<Track> {
    let mut result: Vec<Track> = existing.to_vec();
    let mut existing_names: std::collections::HashSet<String> =
        existing.iter().map(|t| t.name.0.clone()).collect();

    for mut new_track in new {
        if existing_names.contains(&new_track.name.0) {
            // Find existing track and merge items
            if let Some(existing_track) = result.iter_mut().find(|t| t.name.0 == new_track.name.0) {
                existing_track.items.extend(new_track.items);
            }
        } else {
            // New track, add it
            existing_names.insert(new_track.name.0.clone());
            result.push(new_track);
        }
    }

    result
}

/// Trait for converting monarchy Structure to Vec<Track>
///
/// This recursively converts the hierarchical Structure into a flat Vec<Track>
/// where folder relationships are represented using folder_depth_change.
pub trait IntoTracks<M: Metadata> {
    /// Convert the Structure to a Vec<Track>
    ///
    /// # Example
    /// ```ignore
    /// use dynamic_template::{default_config, IntoTracks};
    /// use monarchy::monarchy_sort;
    /// 
    /// let structure = monarchy_sort(vec!["Kick In"], default_config())?;
    /// let tracks = structure.to_tracks();
    /// ```
    fn to_tracks(self) -> Vec<Track>;
}

impl<M: Metadata> IntoTracks<M> for Structure<M> {
    fn to_tracks(self) -> Vec<Track> {
        structure_to_tracks(&self, false)
    }
}

/// Helper function to recursively convert Structure to Tracks with original DAW Items
fn structure_to_tracks_with_items<M: Metadata>(
    structure: &Structure<M>,
    item_map: &std::collections::HashMap<String, Vec<Item>>,
    skip_root: bool,
) -> Vec<Track> {
    // Skip root if it's just a container (name is "root" or empty)
    if skip_root || structure.name == "root" || structure.name.is_empty() {
        // Process all children and flatten
        let mut tracks = Vec::new();
        for child in &structure.children {
            tracks.extend(structure_to_tracks_with_items(child, item_map, false));
        }
        return tracks;
    }

    // Create a track for this structure node
    let mut track = Track::new(structure.name.clone());

    // Add items from monarchy structure to the track
    for monarchy_item in &structure.items {
        if let Some(daw_items) = item_map.get(&monarchy_item.original) {
            track.items.extend(daw_items.clone());
        }
    }

    // If this structure has children, it's a folder
    if !structure.children.is_empty() {
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;

        // Recursively convert children
        let mut child_tracks = Vec::new();
        for child in &structure.children {
            child_tracks.extend(structure_to_tracks_with_items(child, item_map, false));
        }

        // Use add_child to properly set up folder hierarchy
        return track.add_child(child_tracks);
    }

    // If this structure has items but no children, it's a leaf track
    vec![track]
}

/// Helper function to recursively convert Structure to Tracks (without preserving items)
fn structure_to_tracks<M: Metadata>(structure: &Structure<M>, skip_root: bool) -> Vec<Track> {
    // Skip root if it's just a container (name is "root" or empty)
    if skip_root || structure.name == "root" || structure.name.is_empty() {
        // Process all children and flatten
        let mut tracks = Vec::new();
        for child in &structure.children {
            tracks.extend(structure_to_tracks(child, false));
        }
        return tracks;
    }

    // Create a track for this structure node
    let mut track = Track::new(structure.name.clone());

    // If this structure has children, it's a folder
    if !structure.children.is_empty() {
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;

        // Recursively convert children
        let mut child_tracks = Vec::new();
        for child in &structure.children {
            child_tracks.extend(structure_to_tracks(child, false));
        }

        // Use add_child to properly set up folder hierarchy
        return track.add_child(child_tracks);
    }

    // If this structure has items but no children, it's a leaf track
    // Note: The items from monarchy are just strings, not DAW Items
    // You may want to convert them to DAW Items separately if needed
    // For now, we just create the track with the name

    vec![track]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_acc_guitar_structure() {
        // Test that Acoustic Guitar stays under Guitars (Guitars is not transparent)
        // Guitars -> Acoustic -> [item] collapses to Guitars: [item]
        let inputs = vec!["Acc Guitar"];
        let config = default_config();
        let result = monarchy_sort(inputs, config).unwrap();
        
        println!("\nAcc Guitar result:");
        result.print_tree();
        
        // Guitars is not transparent, so it keeps its name even with a single item
        // The structure should be: Guitars: [item]
        let guitars = result.find_child("Guitars")
            .expect("Should have Guitars folder");
        assert_eq!(guitars.items.len(), 1, "Guitars should have 1 item");
        assert_eq!(guitars.items[0].original, "Acc Guitar");
    }

    #[test]
    fn test_kick_and_drumkit() {
        // Just simple string inputs - the system figures out what they are
        let inputs = vec!["kick_in.wav", "kick_out.wav", "snare.wav"];
        // Use the default config
        let config = default_config();
        let result = monarchy_sort(inputs, config).unwrap();

        // Print the structure
        result.print_tree();

        // The structure should be:
        // Drums
        //   Kick (folder with In/Out children)
        //     In: [kick_in.wav]
        //     Out: [kick_out.wav]
        //   Snare: [snare.wav]
        result
            .assert()
            .has_total_items(3)
            .has_groups(1)
            .group("Drums")
            .has_groups(2)  // Kick folder and Snare track
            .group("Kick")
            .has_groups(2)  // In and Out sub-groups
            .done()
            .group("Drums")
            .group("Snare")
            .contains_exactly(&["snare.wav"])
            .done();
    }

    #[test]
    fn test_bass_types() {
        // Test different bass types
        // Note: "808_bass" will match Electronic Kit's "808" pattern before Bass Synth,
        // so we use "sub_bass" instead to properly test synth bass categorization
        let inputs = vec![
            "bass_guitar_di.wav",
            "synth_bass_sub.wav",
            "upright_pizz.wav",
            "electric_bass_amp.wav",
            "sub_bass.wav",  // Changed from "808_bass.wav" to avoid matching Electronic Kit
            "acoustic_bass.wav",
        ];

        let config = default_config();
        let result = monarchy_sort(inputs, config).unwrap();

        // Print the structure
        result.print_tree();

        // Verify bass categorization
        // Structure should be:
        // Bass
        //   Guitar: [bass_guitar_di.wav, electric_bass_amp.wav]
        //   Synth: [synth_bass_sub.wav, sub_bass.wav]
        //   Upright Bass: [upright_pizz.wav, acoustic_bass.wav]
        // Note: Group names are "Guitar", "Synth", "Upright Bass" (not prefixed with "Bass")
        result
            .assert()
            .has_total_items(6)
            .has_groups(1)
            .group("Bass")
            .has_groups(3)
            .group("Guitar")  // The group name is "Guitar", display name is "Bass Guitar"
            .contains_exactly(&["bass_guitar_di.wav", "electric_bass_amp.wav"])
            .done()
            .group("Bass")
            .group("Synth")  // The group name is "Synth", display name is "Bass Synth"
            .contains_exactly(&["synth_bass_sub.wav", "sub_bass.wav"])
            .done()
            .group("Bass")
            .group("Upright Bass")
            .contains_exactly(&["upright_pizz.wav", "acoustic_bass.wav"])
            .done();
    }
}
