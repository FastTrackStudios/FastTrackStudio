//! Metadata Module
//!
//! Song metadata parsing and representation

pub mod metadata;
pub mod parser;

pub use metadata::SongMetadata;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metadata_integration() {
        // Test that parser and metadata work together
        let (title, artist) = SongMetadata::parse_title_artist("Integration Test - Test Artist");

        let mut metadata = SongMetadata::new();
        metadata.title = title;
        metadata.artist = artist;

        assert_eq!(metadata.title, Some("Integration Test".to_string()));
        assert_eq!(metadata.artist, Some("Test Artist".to_string()));
    }
}
