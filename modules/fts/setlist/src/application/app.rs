//! App setlist source implementation
//!
//! This module provides the default setlist source implementation for the standalone
//! FastTrackStudio desktop application when not connected to REAPER extension.
//! It manages setlists, songs, and sections with in-memory storage and persistence.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::path::{Path, PathBuf};
use serde::{Deserialize, Serialize};
use primitives::Position;

use crate::core::{
    Setlist, SetlistSource, SetlistError, Song, Section, SectionType, SourceInfo,
    SetlistOrder, SetlistEntry
};
use crate::application::{ApplicationSetlistSource, SourceType};

/// App implementation of setlist source for standalone desktop application
#[derive(Debug, Clone)]
pub struct AppSetlistSource {
    inner: Arc<Mutex<AppSetlistSourceInner>>,
}

#[derive(Debug)]
struct AppSetlistSourceInner {
    /// Current active setlist
    current_setlist: Option<Setlist>,
    /// Setlist order for performance management
    setlist_order: SetlistOrder,
    /// Saved setlists by name
    saved_setlists: HashMap<String, Setlist>,
    /// Application metadata
    metadata: HashMap<String, String>,
    /// Current project file path (for persistence)
    project_file: Option<PathBuf>,
    /// Auto-save enabled
    auto_save: bool,
    /// Modified flag
    is_modified: bool,
}

/// Persistent data structure for saving/loading app state
#[derive(Debug, Clone, Serialize, Deserialize)]
struct AppSetlistData {
    current_setlist: Option<Setlist>,
    setlist_order: SetlistOrder,
    saved_setlists: HashMap<String, Setlist>,
    metadata: HashMap<String, String>,
}

impl AppSetlistSource {
    /// Create a new app setlist source
    pub fn new() -> Self {
        let mut metadata = HashMap::new();
        metadata.insert("created".to_string(), chrono::Utc::now().to_rfc3339());
        metadata.insert("version".to_string(), env!("CARGO_PKG_VERSION").to_string());

        Self {
            inner: Arc::new(Mutex::new(AppSetlistSourceInner {
                current_setlist: None,
                setlist_order: SetlistOrder::new(),
                saved_setlists: HashMap::new(),
                metadata,
                project_file: None,
                auto_save: false,
                is_modified: false,
            })),
        }
    }

    /// Create a new app setlist source with a default setlist
    pub fn with_default_setlist() -> Self {
        let source = Self::new();
        let mut setlist = Setlist::new("New Setlist".to_string()).unwrap();
        setlist.set_metadata("created_by", "FastTrackStudio Desktop");
        setlist.set_metadata("created_at", chrono::Utc::now().to_rfc3339());

        let _ = source.set_current_setlist(setlist);
        source
    }

    /// Create app setlist source with sample data for demo/testing
    pub fn with_sample_data() -> Self {
        let source = Self::new();

        // Create sample setlist
        let mut setlist = Setlist::new("Sample Concert Setlist".to_string()).unwrap();
        setlist.set_metadata("venue", "Demo Venue");
        setlist.set_metadata("date", "2024-01-15");
        setlist.set_metadata("created_by", "FastTrackStudio Desktop");

        // Add sample songs
        let songs = vec![
            Self::create_sample_song("Opening Song", 0.0, 180.0, vec![
                ("Intro", SectionType::Intro, 0.0, 15.0),
                ("Verse 1", SectionType::Verse, 15.0, 45.0),
                ("Chorus 1", SectionType::Chorus, 45.0, 75.0),
                ("Verse 2", SectionType::Verse, 75.0, 105.0),
                ("Chorus 2", SectionType::Chorus, 105.0, 135.0),
                ("Bridge", SectionType::Bridge, 135.0, 150.0),
                ("Final Chorus", SectionType::Chorus, 150.0, 180.0),
            ]),
            Self::create_sample_song("Ballad", 200.0, 320.0, vec![
                ("Verse 1", SectionType::Verse, 200.0, 240.0),
                ("Chorus", SectionType::Chorus, 240.0, 280.0),
                ("Verse 2", SectionType::Verse, 280.0, 300.0),
                ("Outro", SectionType::Outro, 300.0, 320.0),
            ]),
            Self::create_sample_song("Rock Anthem", 340.0, 520.0, vec![
                ("Intro", SectionType::Intro, 340.0, 360.0),
                ("Verse 1", SectionType::Verse, 360.0, 390.0),
                ("Pre-Chorus", SectionType::Pre(Box::new(SectionType::Chorus)), 390.0, 405.0),
                ("Chorus 1", SectionType::Chorus, 405.0, 435.0),
                ("Verse 2", SectionType::Verse, 435.0, 465.0),
                ("Pre-Chorus 2", SectionType::Pre(Box::new(SectionType::Chorus)), 465.0, 480.0),
                ("Chorus 2", SectionType::Chorus, 480.0, 510.0),
                ("Outro", SectionType::Outro, 510.0, 520.0),
            ]),
        ];

        for song in songs {
            setlist.add_song(song).unwrap();
        }

        let _ = source.set_current_setlist(setlist);
        source
    }

    /// Helper to create sample songs
    fn create_sample_song(name: &str, start: f64, end: f64, sections: Vec<(&str, SectionType, f64, f64)>) -> Song {
        let mut song = Song::new(name.to_string()).unwrap();
        song.start_position = Some(Position::from_seconds(start));
        song.end_position = Some(Position::from_seconds(end));
        song.set_metadata("created_by", "sample_data");

        for (section_name, section_type, section_start, section_end) in sections {
            let section = Section::from_seconds(
                section_type,
                section_start,
                section_end,
                section_name.to_string(),
                None,
            ).unwrap();
            song.add_section(section).unwrap();
        }

        song.auto_number_sections();
        song
    }

    /// Set the current active setlist
    pub fn set_current_setlist(&self, setlist: Setlist) -> Result<(), SetlistError> {
        setlist.validate()?;

        let mut inner = self.inner.lock().unwrap();
        inner.current_setlist = Some(setlist);
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Get the current active setlist
    pub fn get_current_setlist(&self) -> Option<Setlist> {
        let inner = self.inner.lock().unwrap();
        inner.current_setlist.clone()
    }

    /// Clear the current setlist
    pub fn clear_current_setlist(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.current_setlist = None;
        inner.is_modified = true;
    }

    /// Save a setlist with a given name
    pub fn save_setlist(&self, name: String, setlist: Setlist) -> Result<(), SetlistError> {
        setlist.validate()?;

        let mut inner = self.inner.lock().unwrap();
        inner.saved_setlists.insert(name, setlist);
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Load a saved setlist by name
    pub fn load_setlist(&self, name: &str) -> Option<Setlist> {
        let inner = self.inner.lock().unwrap();
        inner.saved_setlists.get(name).cloned()
    }

    /// Get list of saved setlist names
    pub fn list_saved_setlists(&self) -> Vec<String> {
        let inner = self.inner.lock().unwrap();
        inner.saved_setlists.keys().cloned().collect()
    }

    /// Delete a saved setlist
    pub fn delete_setlist(&self, name: &str) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();

        if inner.saved_setlists.remove(name).is_none() {
            return Err(SetlistError::not_found("setlist", name));
        }

        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Get the setlist order for performance management
    pub fn get_setlist_order(&self) -> SetlistOrder {
        let inner = self.inner.lock().unwrap();
        inner.setlist_order.clone()
    }

    /// Set the setlist order
    pub fn set_setlist_order(&self, order: SetlistOrder) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();
        inner.setlist_order = order;
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Add an entry to the setlist order
    pub fn add_setlist_entry(&self, entry: SetlistEntry) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();
        inner.setlist_order.entries.push(entry);
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Update setlist order current position
    pub fn set_current_position(&self, position: usize) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();

        if position >= inner.setlist_order.entries.len() {
            return Err(SetlistError::validation_error("Position out of bounds"));
        }

        inner.setlist_order.current_position = position;
        inner.is_modified = true;

        Ok(())
    }

    /// Advance to next song in order
    pub fn advance_setlist(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.setlist_order.advance();
        inner.is_modified = true;
    }

    /// Go to previous song in order
    pub fn retreat_setlist(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.setlist_order.retreat();
        inner.is_modified = true;
    }

    /// Enable or disable auto-save
    pub fn set_auto_save(&self, enabled: bool) {
        let mut inner = self.inner.lock().unwrap();
        inner.auto_save = enabled;
    }

    /// Check if auto-save is enabled
    pub fn is_auto_save_enabled(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.auto_save
    }

    /// Check if data has been modified
    pub fn is_modified(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.is_modified
    }

    /// Mark data as saved (clear modified flag)
    pub fn mark_saved(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.is_modified = false;
    }

    /// Set the project file path for persistence
    pub fn set_project_file<P: AsRef<Path>>(&self, path: P) {
        let mut inner = self.inner.lock().unwrap();
        inner.project_file = Some(path.as_ref().to_path_buf());
    }

    /// Get the project file path
    pub fn get_project_file(&self) -> Option<PathBuf> {
        let inner = self.inner.lock().unwrap();
        inner.project_file.clone()
    }

    /// Save to project file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), SetlistError> {
        let inner = self.inner.lock().unwrap();

        let data = AppSetlistData {
            current_setlist: inner.current_setlist.clone(),
            setlist_order: inner.setlist_order.clone(),
            saved_setlists: inner.saved_setlists.clone(),
            metadata: inner.metadata.clone(),
        };

        let json = serde_json::to_string_pretty(&data)
            .map_err(|e| SetlistError::json_error(e.to_string()))?;

        std::fs::write(path.as_ref(), json)
            .map_err(|e| SetlistError::io_error(format!("Failed to write file: {}", e)))?;

        // Update the inner state
        let mut inner = inner;
        inner.project_file = Some(path.as_ref().to_path_buf());
        inner.is_modified = false;

        Ok(())
    }

    /// Load from project file
    pub fn load_from_file<P: AsRef<Path>>(&self, path: P) -> Result<(), SetlistError> {
        let content = std::fs::read_to_string(path.as_ref())
            .map_err(|e| SetlistError::io_error(format!("Failed to read file: {}", e)))?;

        let data: AppSetlistData = serde_json::from_str(&content)
            .map_err(|e| SetlistError::json_error(format!("Failed to parse JSON: {}", e)))?;

        // Validate loaded data
        if let Some(ref setlist) = data.current_setlist {
            setlist.validate()?;
        }

        for (name, setlist) in &data.saved_setlists {
            setlist.validate().map_err(|e| {
                SetlistError::validation_error(format!("Invalid saved setlist '{}': {}", name, e))
            })?;
        }

        // Update the inner state
        let mut inner = self.inner.lock().unwrap();
        inner.current_setlist = data.current_setlist;
        inner.setlist_order = data.setlist_order;
        inner.saved_setlists = data.saved_setlists;

        // Merge metadata (keep existing keys, add new ones)
        for (k, v) in data.metadata {
            inner.metadata.insert(k, v);
        }

        inner.project_file = Some(path.as_ref().to_path_buf());
        inner.is_modified = false;

        Ok(())
    }

    /// Auto-save to project file if set
    fn auto_save(&self) -> Result<(), SetlistError> {
        let inner = self.inner.lock().unwrap();

        if let Some(ref project_file) = inner.project_file {
            let project_file = project_file.clone();
            drop(inner);
            self.save_to_file(project_file)?;
        }

        Ok(())
    }

    /// Create a new song in the current setlist
    pub fn create_song(&self, name: String) -> Result<uuid::Uuid, SetlistError> {
        let mut inner = self.inner.lock().unwrap();

        let current_setlist = inner.current_setlist.as_mut()
            .ok_or_else(|| SetlistError::validation_error("No current setlist"))?;

        let song_id = uuid::Uuid::new_v4();
        let song = Song::with_id(song_id, name)?;

        current_setlist.add_song(song)?;
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(song_id)
    }

    /// Add a section to a song in the current setlist
    pub fn add_section_to_song(
        &self,
        song_name: &str,
        section: Section
    ) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();

        let current_setlist = inner.current_setlist.as_mut()
            .ok_or_else(|| SetlistError::validation_error("No current setlist"))?;

        let song = current_setlist.get_song_by_name_mut(song_name)
            .ok_or_else(|| SetlistError::not_found("song", song_name))?;

        song.add_section(section)?;
        song.auto_number_sections();
        inner.is_modified = true;

        if inner.auto_save {
            drop(inner);
            self.auto_save()?;
        }

        Ok(())
    }

    /// Get application metadata
    pub fn get_app_metadata(&self, key: &str) -> Option<String> {
        let inner = self.inner.lock().unwrap();
        inner.metadata.get(key).cloned()
    }

    /// Set application metadata
    pub fn set_app_metadata<K: Into<String>, V: Into<String>>(&self, key: K, value: V) {
        let mut inner = self.inner.lock().unwrap();
        inner.metadata.insert(key.into(), value.into());
        inner.is_modified = true;
    }
}

impl Default for AppSetlistSource {
    fn default() -> Self {
        Self::new()
    }
}

impl SetlistSource for AppSetlistSource {
    fn build_setlist(&self) -> Result<Setlist, SetlistError> {
        let inner = self.inner.lock().unwrap();

        match &inner.current_setlist {
            Some(setlist) => Ok(setlist.clone()),
            None => Ok(Setlist::new("Empty Setlist".to_string())?),
        }
    }

    fn source_name(&self) -> &'static str {
        "FastTrackStudio App"
    }

    fn is_available(&self) -> bool {
        true // App source is always available
    }

    fn get_source_info(&self) -> SourceInfo {
        let inner = self.inner.lock().unwrap();

        let mut metadata = HashMap::new();
        metadata.insert("type".to_string(), "app".to_string());
        metadata.insert("has_current_setlist".to_string(), inner.current_setlist.is_some().to_string());
        metadata.insert("saved_setlists_count".to_string(), inner.saved_setlists.len().to_string());
        metadata.insert("setlist_order_length".to_string(), inner.setlist_order.len().to_string());
        metadata.insert("auto_save".to_string(), inner.auto_save.to_string());
        metadata.insert("is_modified".to_string(), inner.is_modified.to_string());

        if let Some(ref project_file) = inner.project_file {
            metadata.insert("project_file".to_string(), project_file.display().to_string());
        }

        // Add custom metadata
        for (k, v) in &inner.metadata {
            metadata.insert(k.clone(), v.clone());
        }

        SourceInfo {
            name: "FastTrackStudio Desktop App".to_string(),
            is_available: true,
            source_type: "app".to_string(),
            metadata,
        }
    }
}

impl ApplicationSetlistSource for AppSetlistSource {
    fn refresh(&mut self) -> Result<(), SetlistError> {
        // For app source, refresh means reloading from project file if available
        let project_file = {
            let inner = self.inner.lock().unwrap();
            inner.project_file.clone()
        };

        if let Some(project_file) = project_file {
            self.load_from_file(project_file)?;
        }

        Ok(())
    }

    fn supports_real_time_updates(&self) -> bool {
        true // App can provide real-time updates
    }

    fn set_real_time_updates(&mut self, enabled: bool) -> Result<(), SetlistError> {
        self.set_app_metadata("real_time_updates", enabled.to_string());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_app_source_creation() {
        let source = AppSetlistSource::new();
        assert_eq!(source.source_name(), "FastTrackStudio App");
        assert!(source.is_available());
        assert!(source.get_current_setlist().is_none());
    }

    #[test]
    fn test_with_default_setlist() {
        let source = AppSetlistSource::with_default_setlist();
        let setlist = source.get_current_setlist().unwrap();
        assert_eq!(setlist.name, "New Setlist");
        assert!(setlist.get_metadata("created_by").is_some());
    }

    #[test]
    fn test_sample_data() {
        let source = AppSetlistSource::with_sample_data();
        let setlist = source.get_current_setlist().unwrap();

        assert_eq!(setlist.name, "Sample Concert Setlist");
        assert_eq!(setlist.song_count(), 3);

        let first_song = setlist.get_song(0).unwrap();
        assert_eq!(first_song.name, "Opening Song");
        assert!(!first_song.sections.is_empty());
    }

    #[test]
    fn test_setlist_management() {
        let source = AppSetlistSource::new();

        let setlist = Setlist::new("Test Setlist".to_string()).unwrap();
        source.set_current_setlist(setlist.clone()).unwrap();

        assert_eq!(source.get_current_setlist().unwrap().name, "Test Setlist");

        // Save the setlist
        source.save_setlist("saved_test".to_string(), setlist).unwrap();
        assert!(source.list_saved_setlists().contains(&"saved_test".to_string()));

        // Load it back
        let loaded = source.load_setlist("saved_test").unwrap();
        assert_eq!(loaded.name, "Test Setlist");
    }

    #[test]
    fn test_setlist_order() {
        let source = AppSetlistSource::new();

        let mut order = SetlistOrder::new();
        let entry = SetlistEntry::new(0);
        order.entries.push(entry);

        source.set_setlist_order(order).unwrap();

        let retrieved_order = source.get_setlist_order();
        assert_eq!(retrieved_order.entries.len(), 1);
        assert_eq!(retrieved_order.current_position, 0);

        // Test navigation
        source.advance_setlist();
        let order_after_advance = source.get_setlist_order();
        assert_eq!(order_after_advance.current_position, 0); // Should wrap to 0 with 1 entry
    }

    #[test]
    fn test_persistence() -> Result<(), Box<dyn std::error::Error>> {
        let temp_file = NamedTempFile::new()?;
        let file_path = temp_file.path().to_path_buf();

        // Create source with data
        let source = AppSetlistSource::with_sample_data();
        source.set_app_metadata("test_key", "test_value");

        // Save to file
        source.save_to_file(&file_path)?;
        assert!(!source.is_modified());

        // Create new source and load
        let new_source = AppSetlistSource::new();
        new_source.load_from_file(&file_path)?;

        let loaded_setlist = new_source.get_current_setlist().unwrap();
        assert_eq!(loaded_setlist.name, "Sample Concert Setlist");
        assert_eq!(loaded_setlist.song_count(), 3);

        assert_eq!(new_source.get_app_metadata("test_key"), Some("test_value".to_string()));

        Ok(())
    }

    #[test]
    fn test_auto_save() -> Result<(), Box<dyn std::error::Error>> {
        let temp_file = NamedTempFile::new()?;
        let file_path = temp_file.path().to_path_buf();

        let source = AppSetlistSource::new();
        source.set_project_file(&file_path);
        source.set_auto_save(true);

        // Make a change - should auto-save
        let setlist = Setlist::new("Auto Save Test".to_string()).unwrap();
        source.set_current_setlist(setlist)?;

        // Load from file to verify it was saved
        let content = std::fs::read_to_string(&file_path)?;
        assert!(content.contains("Auto Save Test"));

        Ok(())
    }

    #[test]
    fn test_song_creation() {
        let source = AppSetlistSource::with_default_setlist();

        let song_id = source.create_song("New Song".to_string()).unwrap();

        let setlist = source.get_current_setlist().unwrap();
        assert_eq!(setlist.song_count(), 1);

        let song = setlist.get_song_by_name("New Song").unwrap();
        assert_eq!(song.id, Some(song_id));
    }

    #[test]
    fn test_modified_flag() {
        let source = AppSetlistSource::new();
        assert!(!source.is_modified());

        let setlist = Setlist::new("Test".to_string()).unwrap();
        source.set_current_setlist(setlist).unwrap();
        assert!(source.is_modified());

        source.mark_saved();
        assert!(!source.is_modified());
    }

    #[test]
    fn test_source_info() {
        let source = AppSetlistSource::with_sample_data();
        let info = source.get_source_info();

        assert_eq!(info.source_type, "app");
        assert!(info.is_available);
        assert_eq!(info.metadata.get("type"), Some(&"app".to_string()));
        assert_eq!(info.metadata.get("has_current_setlist"), Some(&"true".to_string()));
    }
}
