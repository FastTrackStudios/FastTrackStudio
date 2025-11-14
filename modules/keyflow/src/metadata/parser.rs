//! Metadata Parser
//!
//! Parses song metadata from chart input

use super::metadata::SongMetadata;

impl SongMetadata {
    /// Parse a title/artist line
    /// 
    /// Format: "Song Title - Artist Name"
    /// or just: "Song Title"
    pub fn parse_title_artist(input: &str) -> (Option<String>, Option<String>) {
        let input = input.trim();
        
        if input.is_empty() {
            return (None, None);
        }
        
        // Check for " - " separator
        if let Some(dash_pos) = input.find(" - ") {
            let title = input[..dash_pos].trim().to_string();
            let artist = input[dash_pos + 3..].trim().to_string();
            
            (
                if title.is_empty() { None } else { Some(title) },
                if artist.is_empty() { None } else { Some(artist) },
            )
        } else {
            // No artist, just title
            (Some(input.to_string()), None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_title_only() {
        let (title, artist) = SongMetadata::parse_title_artist("Amazing Grace");
        assert_eq!(title, Some("Amazing Grace".to_string()));
        assert_eq!(artist, None);
    }

    #[test]
    fn test_parse_title_and_artist() {
        let (title, artist) = SongMetadata::parse_title_artist("Reckless Love - Cory Asbury");
        assert_eq!(title, Some("Reckless Love".to_string()));
        assert_eq!(artist, Some("Cory Asbury".to_string()));
    }

    #[test]
    fn test_parse_empty_string() {
        let (title, artist) = SongMetadata::parse_title_artist("");
        assert_eq!(title, None);
        assert_eq!(artist, None);
    }

    #[test]
    fn test_parse_with_extra_spaces() {
        let (title, artist) = SongMetadata::parse_title_artist("  Song Title  -  Artist Name  ");
        assert_eq!(title, Some("Song Title".to_string()));
        assert_eq!(artist, Some("Artist Name".to_string()));
    }

    #[test]
    fn test_parse_multiple_dashes() {
        let (title, artist) = SongMetadata::parse_title_artist("Title - With - Dashes - Artist");
        // Should only split on first " - "
        assert_eq!(title, Some("Title".to_string()));
        assert_eq!(artist, Some("With - Dashes - Artist".to_string()));
    }

    #[test]
    fn test_parse_title_with_dash_no_spaces() {
        let (title, artist) = SongMetadata::parse_title_artist("Title-WithDash");
        // No " - " separator, so entire string is title
        assert_eq!(title, Some("Title-WithDash".to_string()));
        assert_eq!(artist, None);
    }
}

