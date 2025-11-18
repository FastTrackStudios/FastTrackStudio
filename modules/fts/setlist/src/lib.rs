pub mod application;
pub mod core;

pub use core::{
    Section, SectionType, Setlist, SetlistEntry, SetlistError, SetlistOrder, SetlistSummary, Song,
    SongSummary,
};

pub fn default_setlist() -> Result<Setlist, SetlistError> {
    Setlist::default_app_setlist()
}

pub fn sample_setlist() -> Result<Setlist, SetlistError> {
    Setlist::sample_concert_setlist()
}

pub fn load_setlist_from_path<P: AsRef<std::path::Path>>(
    path: P,
) -> Result<Setlist, SetlistError> {
    Setlist::load_from_path(path)
}

pub fn save_setlist_to_path<P: AsRef<std::path::Path>>(
    setlist: &Setlist,
    path: P,
) -> Result<(), SetlistError> {
    setlist.save_to_path(path)
}

pub fn validate_setlist(setlist: &Setlist) -> Result<(), SetlistError> {
    setlist.validate()
}

pub fn parse_section_type(input: &str) -> Result<SectionType, SetlistError> {
    SectionType::from_str(input)
}

pub fn create_setlist(name: &str) -> Result<Setlist, SetlistError> {
    Setlist::new(name.to_string())
}

pub fn create_song(name: &str) -> Result<Song, SetlistError> {
    Song::new(name.to_string())
}

pub fn create_section(
    section_type: SectionType,
    start_seconds: f64,
    end_seconds: f64,
    name: &str,
) -> Result<Section, SetlistError> {
    Section::from_seconds(
        section_type,
        start_seconds,
        end_seconds,
        name.to_string(),
        None,
    )
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const MODULE_NAME: &str = "setlist";

pub fn module_info() -> std::collections::HashMap<String, String> {
    let mut info = std::collections::HashMap::new();
    info.insert("name".to_string(), MODULE_NAME.to_string());
    info.insert("version".to_string(), VERSION.to_string());

    #[cfg(feature = "reaper")]
    info.insert("reaper_support".to_string(), "enabled".to_string());
    #[cfg(not(feature = "reaper"))]
    info.insert("reaper_support".to_string(), "disabled".to_string());

    #[cfg(feature = "rpp")]
    info.insert("rpp_support".to_string(), "enabled".to_string());
    #[cfg(not(feature = "rpp"))]
    info.insert("rpp_support".to_string(), "disabled".to_string());

    info.insert("app_support".to_string(), "enabled".to_string());

    info
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_setlist_helper() {
        let setlist = default_setlist().unwrap();
        assert_eq!(setlist.name, "New Setlist");
        assert!(setlist.get_metadata("created_with").is_some());
    }

    #[test]
    fn test_sample_setlist_helper() {
        let setlist = sample_setlist().unwrap();
        assert!(setlist.song_count() > 0);
        assert_eq!(setlist.name, "Sample Concert Setlist");
    }

    #[test]
    fn test_convenience_functions() {
        let mut setlist = create_setlist("Convenience").unwrap();
        let song = create_song("Test").unwrap();
        setlist.add_song(song).unwrap();
        assert!(validate_setlist(&setlist).is_ok());
    }

    #[test]
    fn test_section_type_parsing() {
        assert!(parse_section_type("verse").is_ok());
        assert!(parse_section_type("chorus").is_ok());
        assert!(parse_section_type("invalid").is_err());
    }

    #[test]
    fn test_creation_helpers() {
        let setlist = create_setlist("Test Setlist").unwrap();
        assert_eq!(setlist.name, "Test Setlist");

        let song = create_song("Test Song").unwrap();
        assert_eq!(song.name, "Test Song");

        let section = create_section(SectionType::Verse, 0.0, 30.0, "Test Section").unwrap();
        assert_eq!(section.name, "Test Section");
        assert_eq!(section.duration(), 30.0);
    }

    #[test]
    fn test_module_info() {
        let info = module_info();
        assert_eq!(info.get("name"), Some(&MODULE_NAME.to_string()));
        assert_eq!(info.get("version"), Some(&VERSION.to_string()));
        assert!(info.contains_key("reaper_support"));
        assert!(info.contains_key("rpp_support"));
        assert_eq!(info.get("app_support"), Some(&"enabled".to_string()));
    }

    #[test]
    fn test_core_domain_integration() {
        let setlist = Setlist::new("Test".to_string()).unwrap();
        assert_eq!(setlist.name, "Test");
        assert_eq!(setlist.song_count(), 0);

        let song = Song::new("Test Song".to_string()).unwrap();
        assert_eq!(song.name, "Test Song");
        assert_eq!(song.sections.len(), 0);

        let section =
            Section::from_seconds(SectionType::Verse, 10.0, 40.0, "Verse".to_string(), None)
                .unwrap();
        assert_eq!(section.duration(), 30.0);
        assert!(section.contains_position(25.0));
    }

    #[test]
    fn test_application_layer_integration() {
        let setlist = sample_setlist().unwrap();
        assert!(setlist.song_count() > 0);
        assert!(setlist.get_metadata("venue").is_some());
    }

    #[test]
    fn test_error_handling() {
        let result =
            Section::from_seconds(SectionType::Verse, 40.0, 10.0, "Invalid".to_string(), None);
        assert!(result.is_err());

        let result = Song::new("".to_string());
        assert!(result.is_err());

        let result = Setlist::new("".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_primitives_integration() {
        use primitives::Position;

        let section = Section::new(
            SectionType::Chorus,
            Position::from_seconds(30.0),
            Position::from_seconds(60.0),
            "Chorus".to_string(),
            None,
        )
        .unwrap();

        assert_eq!(section.start_seconds(), 30.0);
        assert_eq!(section.end_seconds(), 60.0);
        assert_eq!(section.duration(), 30.0);
    }
}
