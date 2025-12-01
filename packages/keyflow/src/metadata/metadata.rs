//! Song Metadata
//!
//! Represents song information like title, artist, composer, etc.

use serde::{Deserialize, Serialize};

/// Complete song metadata
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct SongMetadata {
    pub title: Option<String>,
    pub subtitle: Option<String>,
    pub artist: Option<String>,
    pub composer: Option<String>,
    pub writer: Option<String>,
    pub arranger: Option<String>,
    pub lyricist: Option<String>,
    pub copyright: Option<String>,
    pub year: Option<u16>,
    pub tempo: Option<u32>,
}

impl SongMetadata {
    pub fn new() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_metadata() {
        let metadata = SongMetadata::new();
        assert_eq!(metadata.title, None);
        assert_eq!(metadata.artist, None);
        assert_eq!(metadata.tempo, None);
    }

    #[test]
    fn test_default_metadata() {
        let metadata = SongMetadata::default();
        assert!(metadata.title.is_none());
        assert!(metadata.composer.is_none());
        assert!(metadata.copyright.is_none());
    }

    #[test]
    fn test_metadata_fields() {
        let mut metadata = SongMetadata::new();
        metadata.title = Some("Test Song".to_string());
        metadata.artist = Some("Test Artist".to_string());
        metadata.tempo = Some(120);
        metadata.year = Some(2024);

        assert_eq!(metadata.title, Some("Test Song".to_string()));
        assert_eq!(metadata.artist, Some("Test Artist".to_string()));
        assert_eq!(metadata.tempo, Some(120));
        assert_eq!(metadata.year, Some(2024));
    }

    #[test]
    fn test_metadata_clone() {
        let mut metadata1 = SongMetadata::new();
        metadata1.title = Some("Original".to_string());

        let metadata2 = metadata1.clone();
        assert_eq!(metadata1, metadata2);
    }
}
