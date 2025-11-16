use crate::{ProjectActions, ProjectError, ProjectProvider};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use transport::{RecordMode, Tempo, Transport, TransportActions, TransportApp, TransportConfig, TransportError};
use primitives::TimeSignature;
use setlist::{AppSetlistSource, Setlist, Song};

/// A concrete DAW project that uses TransportApp for real transport functionality
#[derive(Serialize, Deserialize)]
pub struct DawProject {
    name: String,
    #[serde(skip)]
    transport_app: Option<Arc<RwLock<TransportApp>>>,
    #[serde(skip)]
    setlist_source: Arc<RwLock<AppSetlistSource>>,
}

impl DawProject {
    pub fn new(name: String) -> Self {
        let config = TransportConfig {
            update_rate: 60.0,
            max_subscribers: 50,
            high_resolution: true,
            default_preroll: 0.0,
            default_postroll: 0.0,
        };

        let transport_app = TransportApp::new(config);

        Self {
            name,
            transport_app: Some(Arc::new(RwLock::new(transport_app))),
            setlist_source: Arc::new(RwLock::new(Self::create_initial_setlist())),
        }
    }

    /// Create initial setlist with 3 songs: Praise (fast), Never Gonna Stop (medium), Build My Life (slow)
    fn create_initial_setlist() -> AppSetlistSource {
        use setlist::{Section, SectionType};
        use primitives::{Position, TimePosition};

        let source = AppSetlistSource::new();

        // Create setlist with our 3 songs
        let mut setlist = setlist::Setlist::new("Sunday Morning Worship".to_string()).unwrap();
        setlist.set_metadata("created_by", "FastTrack Studio");
        setlist.set_metadata("venue", "Main Sanctuary");
        setlist.set_metadata("tempo_flow", "Fast → Medium → Slow");

        // Song 1: Praise (Fast - 140 BPM)
        let mut praise = Song::new("Praise".to_string()).unwrap();
        praise.metadata.insert("tempo".to_string(), "140".to_string());
        praise.metadata.insert("key".to_string(), "G".to_string());
        praise.metadata.insert("style".to_string(), "Fast Worship".to_string());

        // Add sections to Praise
        let praise_sections = vec![
            Section::new(
                SectionType::Intro,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 0, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 15, milliseconds: 0 }),
                "Intro".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 15, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 45, milliseconds: 0 }),
                "Verse 1".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 45, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 15, milliseconds: 0 }),
                "Chorus 1".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 15, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 45, milliseconds: 0 }),
                "Verse 2".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 45, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 15, milliseconds: 0 }),
                "Chorus 2".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Bridge,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 15, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 45, milliseconds: 0 }),
                "Bridge".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 45, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 3, seconds: 30, milliseconds: 0 }),
                "Final Chorus".to_string(),
                None
            ).unwrap(),
        ];

        praise.sections = praise_sections;
        setlist.add_song(praise).unwrap();

        // Song 2: Never Gonna Stop (Medium - 120 BPM)
        let mut never_gonna_stop = Song::new("Never Gonna Stop".to_string()).unwrap();
        never_gonna_stop.metadata.insert("tempo".to_string(), "120".to_string());
        never_gonna_stop.metadata.insert("key".to_string(), "D".to_string());
        never_gonna_stop.metadata.insert("style".to_string(), "Medium Worship".to_string());

        let ngs_sections = vec![
            Section::new(
                SectionType::Intro,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 0, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 20, milliseconds: 0 }),
                "Intro".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 20, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 0, milliseconds: 0 }),
                "Verse 1".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 0, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 40, milliseconds: 0 }),
                "Chorus".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 40, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 20, milliseconds: 0 }),
                "Verse 2".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 20, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 3, seconds: 20, milliseconds: 0 }),
                "Chorus x2".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Outro,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 3, seconds: 20, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 4, seconds: 0, milliseconds: 0 }),
                "Outro".to_string(),
                None
            ).unwrap(),
        ];

        never_gonna_stop.sections = ngs_sections;
        setlist.add_song(never_gonna_stop).unwrap();

        // Song 3: Build My Life (Slow - 85 BPM)
        let mut build_my_life = Song::new("Build My Life".to_string()).unwrap();
        build_my_life.metadata.insert("tempo".to_string(), "85".to_string());
        build_my_life.metadata.insert("key".to_string(), "C".to_string());
        build_my_life.metadata.insert("style".to_string(), "Slow Worship".to_string());

        let bml_sections = vec![
            Section::new(
                SectionType::Intro,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 0, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 25, milliseconds: 0 }),
                "Intro".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 0, seconds: 25, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 15, milliseconds: 0 }),
                "Verse 1".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 1, seconds: 15, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 5, milliseconds: 0 }),
                "Chorus".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Verse,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 5, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 55, milliseconds: 0 }),
                "Verse 2".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 2, seconds: 55, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 3, seconds: 45, milliseconds: 0 }),
                "Chorus".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Bridge,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 3, seconds: 45, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 4, seconds: 35, milliseconds: 0 }),
                "Bridge".to_string(),
                None
            ).unwrap(),
            Section::new(
                SectionType::Chorus,
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 4, seconds: 35, milliseconds: 0 }),
                Position::new(primitives::MusicalPosition::default(), TimePosition { minutes: 6, seconds: 15, milliseconds: 0 }),
                "Final Chorus".to_string(),
                None
            ).unwrap(),
        ];

        build_my_life.sections = bml_sections;
        setlist.add_song(build_my_life).unwrap();

        // Set the setlist in the source
        source.set_current_setlist(setlist).unwrap();

        println!("🎵 Created initial worship setlist with 3 songs:");
        println!("   1. Praise (Fast - 140 BPM, G major) - 3:30");
        println!("   2. Never Gonna Stop (Medium - 120 BPM, D major) - 4:00");
        println!("   3. Build My Life (Slow - 85 BPM, C major) - 6:15");
        println!("🎵 Total setlist duration: ~13:45");

        source
    }

    /// Initialize the transport engine (start the update loop)
    pub async fn initialize(&mut self) -> Result<(), TransportError> {
        println!("Initializing transport engine for project: {}", self.name);

        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.start().await?;
            println!("Transport engine started successfully for project: {}", self.name);
        } else {
            eprintln!("No transport app found for project: {}", self.name);
        }
        Ok(())
    }

    /// Get a reference to the transport app for direct access
    pub fn transport_app(&self) -> Option<&Arc<RwLock<TransportApp>>> {
        self.transport_app.as_ref()
    }

    /// Get the setlist source for this project
    pub fn setlist_source(&self) -> &Arc<RwLock<AppSetlistSource>> {
        &self.setlist_source
    }

    /// Get the current setlist for this project
    pub async fn get_setlist(&self) -> Result<Option<Setlist>, ProjectError> {
        let source = self.setlist_source.read().await;
        Ok(source.get_current_setlist())
    }

    /// Set a new setlist for this project
    pub async fn set_setlist(&self, setlist: Setlist) -> Result<(), ProjectError> {
        let source = self.setlist_source.read().await;
        source.set_current_setlist(setlist).map_err(|e| ProjectError::Other(e.to_string()))
    }

    /// Add a song to the current setlist
    pub async fn add_song_to_setlist(&self, song: Song) -> Result<(), ProjectError> {
        let source = self.setlist_source.read().await;
        if let Some(mut setlist) = source.get_current_setlist() {
            setlist.add_song(song).map_err(|e| ProjectError::Other(e.to_string()))?;
            source.set_current_setlist(setlist).map_err(|e| ProjectError::Other(e.to_string()))
        } else {
            // Create new setlist with the song
            let mut new_setlist = Setlist::new("New Setlist".to_string()).map_err(|e| ProjectError::Other(e.to_string()))?;
            new_setlist.add_song(song).map_err(|e| ProjectError::Other(e.to_string()))?;
            source.set_current_setlist(new_setlist).map_err(|e| ProjectError::Other(e.to_string()))
        }
    }

    /// Async play method for use in Tauri commands
    pub async fn async_play(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_play().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async pause method for use in Tauri commands
    pub async fn async_pause(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_pause().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async stop method for use in Tauri commands
    pub async fn async_stop(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_stop().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async play/pause method for use in Tauri commands
    pub async fn async_play_pause(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_play_pause().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async start recording method for use in Tauri commands
    pub async fn async_start_recording(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_start_recording().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async stop recording method for use in Tauri commands
    pub async fn async_stop_recording(&mut self) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_stop_recording().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async set tempo method for use in Tauri commands
    pub async fn async_set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_set_tempo(tempo).await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async set position method for use in Tauri commands
    pub async fn async_set_position(&mut self, position: f64) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_set_position(position).await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async set time signature method for use in Tauri commands
    pub async fn async_set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let mut transport = transport_arc.write().await;
            transport.async_set_time_signature(time_signature).await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async get transport method for use in Tauri commands
    pub async fn async_get_transport(&self) -> Result<Transport, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_get_transport().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async is playing method for use in Tauri commands
    pub async fn async_is_playing(&self) -> Result<bool, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_is_playing().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async is recording method for use in Tauri commands
    pub async fn async_is_recording(&self) -> Result<bool, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_is_recording().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async get tempo method for use in Tauri commands
    pub async fn async_get_tempo(&self) -> Result<Tempo, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_get_tempo().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async get position method for use in Tauri commands
    pub async fn async_get_position(&self) -> Result<f64, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_get_position().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async get time signature method for use in Tauri commands
    pub async fn async_get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_get_time_signature().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    /// Async is ready method for use in Tauri commands
    pub async fn async_is_ready(&self) -> Result<bool, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            let transport = transport_arc.read().await;
            transport.async_is_ready().await
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }
}

// Implement project actions
impl ProjectActions for DawProject {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn set_name(&mut self, name: String) {
        self.name = name;
    }
}

// Implement TransportActions trait for blocking operations (delegates to transport_app)
impl TransportActions for DawProject {
    fn play(&mut self) -> Result<String, TransportError> {
        // This is a blocking operation, so we use a runtime handle
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_play().await
            })
        })
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_pause().await
            })
        })
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_stop().await
            })
        })
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_play_pause().await
            })
        })
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        // Implementation for play/stop toggle
        let is_playing = tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_is_playing().await
            })
        })?;

        if is_playing {
            self.stop()
        } else {
            self.play()
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_start_recording().await
            })
        })
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_stop_recording().await
            })
        })
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        let is_recording = tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_is_recording().await
            })
        })?;

        if is_recording {
            self.stop_recording()
        } else {
            self.start_recording()
        }
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_set_tempo(tempo).await
            })
        })
    }

    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_set_time_signature(time_signature).await
            })
        })
    }

    fn set_record_mode(&mut self, mode: RecordMode) -> Result<String, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            tokio::task::block_in_place(|| {
                tokio::runtime::Handle::current().block_on(async {
                    let mut transport = transport_arc.write().await;
                    transport.set_record_mode(mode)
                })
            })
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    fn set_position(&mut self, position: f64) -> Result<String, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_set_position(position).await
            })
        })
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_get_tempo().await
            })
        })
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_get_time_signature().await
            })
        })
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        if let Some(transport_arc) = &self.transport_app {
            tokio::task::block_in_place(|| {
                tokio::runtime::Handle::current().block_on(async {
                    let transport = transport_arc.read().await;
                    transport.get_record_mode()
                })
            })
        } else {
            Err(TransportError::InvalidState("Transport not initialized".to_string()))
        }
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_get_position().await
            })
        })
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_is_playing().await
            })
        })
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_is_recording().await
            })
        })
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_get_transport().await
            })
        })
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                self.async_is_ready().await
            })
        })
    }
}

/// Desktop application project manager that handles multiple DAW projects
pub struct DesktopProjectManager {
    projects: HashMap<String, DawProject>,
    active_project: Option<String>,
}

impl DesktopProjectManager {
    pub fn new() -> Self {
        Self {
            projects: HashMap::new(),
            active_project: None,
        }
    }

    /// Create and set a default project
    pub async fn create_default_project(&mut self) -> Result<(), ProjectError> {
        let default_name = "Default Project".to_string();

        // Create the project
        let project = self.create_project(default_name.clone())?;

        // Initialize its transport engine
        project.initialize().await
            .map_err(|e| ProjectError::NotFound(format!("Failed to initialize default project: {}", e)))?;

        // Set it as active
        self.set_active_project(&default_name)?;

        println!("✓ Default project created and set as active: {}", default_name);
        println!("✓ {}", self.get_active_project_status());

        // Log setlist information
        if let Ok(project) = self.get_project(&default_name) {
            if let Ok(Some(setlist)) = project.get_setlist().await {
                println!("🎵 Initial setlist '{}' loaded with {} songs", setlist.name, setlist.songs.len());
            }
        }
        Ok(())
    }

    pub fn set_active_project(&mut self, name: &str) -> Result<(), ProjectError> {
        if self.projects.contains_key(name) {
            self.active_project = Some(name.to_string());
            println!("✓ Set active project to: {}", name);
            println!("✓ {}", self.get_active_project_status());
            Ok(())
        } else {
            Err(ProjectError::NotFound(name.to_string()))
        }
    }

    pub fn get_active_project(&self) -> Option<&str> {
        self.active_project.as_deref()
    }

    pub fn get_active_project_mut(&mut self) -> Option<&mut DawProject> {
        let active_name = self.active_project.clone()?;
        self.projects.get_mut(&active_name)
    }

    /// Initialize a project's transport engine
    pub async fn initialize_project(&mut self, name: &str) -> Result<(), ProjectError> {
        println!("Initializing transport engine for project: {}", name);

        let project = self.projects.get_mut(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))?;

        project.initialize().await
            .map_err(|e| {
                eprintln!("Failed to initialize transport engine for project {}: {}", name, e);
                ProjectError::NotFound(e.to_string())
            })?;

        println!("Transport engine initialized successfully for project: {}", name);
        Ok(())
    }

    /// Get the current active project name with status info
    pub fn get_active_project_status(&self) -> String {
        match &self.active_project {
            Some(name) => format!("Active project: '{}' (transport ready: {})",
                name,
                self.projects.get(name).map(|p| p.transport_app().is_some()).unwrap_or(false)
            ),
            None => "No active project set".to_string(),
        }
    }

    /// Get transport statistics for a project
    pub async fn get_transport_stats(&self, name: &str) -> Result<serde_json::Value, ProjectError> {
        let project = self.projects.get(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))?;

        if let Some(transport_arc) = &project.transport_app {
            let transport = transport_arc.read().await;
            let stats = transport.get_stats().await;

            Ok(serde_json::json!({
                "session_id": stats.session_id,
                "uptime_seconds": stats.uptime.as_secs_f64(),
                "subscriber_count": stats.subscriber_count,
                "is_running": stats.is_running,
                "current_position": stats.current_position,
                "effective_bpm": stats.effective_bpm,
            }))
        } else {
            Err(ProjectError::NotFound("Transport not initialized".to_string()))
        }
    }
}

impl ProjectProvider for DesktopProjectManager {
    type Project = DawProject;
    type Error = ProjectError;

    fn get_project(&self, name: &str) -> Result<&Self::Project, Self::Error> {
        self.projects
            .get(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))
    }

    fn get_project_mut(&mut self, name: &str) -> Result<&mut Self::Project, Self::Error> {
        self.projects
            .get_mut(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))
    }

    fn list_projects(&self) -> Vec<String> {
        self.projects.keys().cloned().collect()
    }

    fn create_project(&mut self, name: String) -> Result<&mut Self::Project, Self::Error> {
        if name.trim().is_empty() {
            return Err(ProjectError::InvalidName(name));
        }

        if self.projects.contains_key(&name) {
            return Err(ProjectError::AlreadyExists(name));
        }

        let project = DawProject::new(name.clone());
        self.projects.insert(name.clone(), project);

        // Set as active if it's the first project
        if self.active_project.is_none() {
            self.active_project = Some(name.clone());
            println!("First project created, set as active: {}", name);
        }

        Ok(self.projects.get_mut(&name).unwrap())
    }
}
