// Song Metadata Parser
//
// Parses song information like:
// - "Reckless Love - Cory Asbury"
// - "Amazing Grace"
//
// Handles title, artist, composer, writer, and other song metadata


/// Complete song metadata
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
}

impl SongMetadata {
    pub fn new() -> Self {
        Self {
            title: None,
            subtitle: None,
            artist: None,
            composer: None,
            writer: None,
            arranger: None,
            lyricist: None,
            copyright: None,
            year: None,
        }
    }
    
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
    
    /// Parse a composer/writer line
    /// 
    /// Format: "Composer Name - Writer Name"
    /// or just: "Composer Name"
    pub fn parse_composer_writer(input: &str) -> (Option<String>, Option<String>) {
        let input = input.trim();
        
        if input.is_empty() {
            return (None, None);
        }
        
        // Check for " - " separator
        if let Some(dash_pos) = input.find(" - ") {
            let composer = input[..dash_pos].trim().to_string();
            let writer = input[dash_pos + 3..].trim().to_string();
            
            (
                if composer.is_empty() { None } else { Some(composer) },
                if writer.is_empty() { None } else { Some(writer) },
            )
        } else {
            // No writer, just composer
            (Some(input.to_string()), None)
        }
    }
    
    /// Parse complete song metadata from lines
    /// 
    /// Expected format:
    /// Line 1: "Song Title - Artist Name"
    /// Line 2: "Composer Name - Writer Name" (optional)
    /// Line 3: "Copyright Year" (optional)
    pub fn parse_from_lines(lines: &[&str]) -> Result<Self, String> {
        let mut metadata = Self::new();
        
        if lines.is_empty() {
            return Ok(metadata);
        }
        
        // First line: title and artist
        if !lines[0].trim().is_empty() {
            let (title, artist) = Self::parse_title_artist(lines[0]);
            metadata.title = title;
            metadata.artist = artist;
        }
        
        // Second line (if present): composer and writer
        if lines.len() > 1 && !lines[1].trim().is_empty() {
            let (composer, writer) = Self::parse_composer_writer(lines[1]);
            metadata.composer = composer;
            metadata.writer = writer;
        }
        
        // Third line (if present): copyright year
        if lines.len() > 2 && !lines[2].trim().is_empty() {
            if let Ok(year) = lines[2].trim().parse::<u16>() {
                metadata.year = Some(year);
            }
        }
        
        Ok(metadata)
    }
    
}

impl Default for SongMetadata {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_title_artist() {
        let (title, artist) = SongMetadata::parse_title_artist("Reckless Love - Cory Asbury");
        assert_eq!(title, Some("Reckless Love".to_string()));
        assert_eq!(artist, Some("Cory Asbury".to_string()));
    }
    
    #[test]
    fn test_parse_title_only() {
        let (title, artist) = SongMetadata::parse_title_artist("Amazing Grace");
        assert_eq!(title, Some("Amazing Grace".to_string()));
        assert_eq!(artist, None);
    }
    
    #[test]
    fn test_parse_composer_writer() {
        let (composer, writer) = SongMetadata::parse_composer_writer("John Williams - Steven Spielberg");
        assert_eq!(composer, Some("John Williams".to_string()));
        assert_eq!(writer, Some("Steven Spielberg".to_string()));
    }
    
    #[test]
    fn test_parse_composer_only() {
        let (composer, writer) = SongMetadata::parse_composer_writer("Ludwig van Beethoven");
        assert_eq!(composer, Some("Ludwig van Beethoven".to_string()));
        assert_eq!(writer, None);
    }
    
    #[test]
    fn test_parse_from_lines() {
        let lines = vec![
            "Reckless Love - Cory Asbury",
            "Cory Asbury - John Mark McMillan",
            "2018"
        ];
        
        let metadata = SongMetadata::parse_from_lines(&lines).unwrap();
        
        assert_eq!(metadata.title, Some("Reckless Love".to_string()));
        assert_eq!(metadata.artist, Some("Cory Asbury".to_string()));
        assert_eq!(metadata.composer, Some("Cory Asbury".to_string()));
        assert_eq!(metadata.writer, Some("John Mark McMillan".to_string()));
        assert_eq!(metadata.year, Some(2018));
    }
    
    #[test]
    fn test_parse_from_lines_minimal() {
        let lines = vec!["Just A Title"];
        
        let metadata = SongMetadata::parse_from_lines(&lines).unwrap();
        
        assert_eq!(metadata.title, Some("Just A Title".to_string()));
        assert_eq!(metadata.artist, None);
        assert_eq!(metadata.composer, None);
        assert_eq!(metadata.writer, None);
        assert_eq!(metadata.year, None);
    }
}
