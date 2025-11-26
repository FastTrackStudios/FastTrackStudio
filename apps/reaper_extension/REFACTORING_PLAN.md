# REAPER Extension Refactoring Implementation Plan

## Overview

This document provides a step-by-step guide to refactoring the REAPER extension for better maintainability, testability, and clarity.

## Phase 1: Create Application Container

### Step 1.1: Create `app.rs` - Application Container

```rust
// src/app.rs
use std::sync::Arc;
use reaper_high::Reaper;
use reaper_medium::ReaperSession;

/// Application context - holds all services and dependencies
pub struct App {
    // Services
    pub setlist_service: Arc<SetlistService>,
    pub command_service: Arc<CommandService>,
    pub seek_service: Arc<SeekService>,
    
    // Infrastructure
    pub action_registry: Arc<ActionRegistry>,
    pub change_detection: Arc<ChangeDetection>,
    
    // Keep session alive
    _session: Box<ReaperSession>,
}

impl App {
    pub fn new(session: ReaperSession) -> Result<Self, Box<dyn std::error::Error>> {
        // Initialize services
        let setlist_service = Arc::new(SetlistService::new()?);
        let command_service = Arc::new(CommandService::new(setlist_service.clone())?);
        let seek_service = Arc::new(SeekService::new()?);
        
        let action_registry = Arc::new(ActionRegistry::new()?);
        let change_detection = Arc::new(ChangeDetection::new()?);
        
        Ok(Self {
            setlist_service,
            command_service,
            seek_service,
            action_registry,
            change_detection,
            _session: Box::new(session),
        })
    }
    
    pub fn initialize(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Register actions
        self.action_registry.register_all()?;
        
        // Register change detection
        self.change_detection.register()?;
        
        // Start timer
        self.start_timer()?;
        
        Ok(())
    }
    
    fn start_timer(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Timer implementation
        Ok(())
    }
}
```

### Step 1.2: Update `lib.rs` to use App

```rust
// src/lib.rs
mod app;

use app::App;

#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    tracing_config::init_tracing();
    info!("FastTrackStudio REAPER Extension starting...");
    
    // Initialize REAPER APIs
    let _ = Swell::make_available_globally(Swell::load(context));
    HighReaper::load(context).setup().ok();
    
    let session = ReaperSession::load(context);
    
    // Create and initialize app
    let app = App::new(session)?;
    app.initialize()?;
    
    // Leak app to keep it alive
    Box::leak(Box::new(app));
    
    info!("FastTrackStudio REAPER Extension initialized");
    Ok(())
}
```

## Phase 2: Extract Services from `setlist_stream.rs`

### Step 2.1: Create `SetlistService`

```rust
// src/services/setlist_service.rs
use std::sync::{Arc, Mutex};
use setlist::SetlistApi;
use crate::adapters::reaper_setlist::build_setlist_from_open_projects;

pub struct SetlistService {
    current_setlist: Arc<Mutex<Option<SetlistApi>>>,
}

impl SetlistService {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            current_setlist: Arc::new(Mutex::new(None)),
        })
    }
    
    pub fn update_setlist(&self) -> Result<(), Box<dyn std::error::Error>> {
        let setlist = build_setlist_from_open_projects(None)?;
        let mut current = self.current_setlist.lock().unwrap();
        *current = Some(setlist);
        Ok(())
    }
    
    pub fn get_setlist(&self) -> Option<SetlistApi> {
        self.current_setlist.lock().unwrap().clone()
    }
}
```

### Step 2.2: Create `CommandService`

```rust
// src/services/command_service.rs
use std::sync::mpsc;
use crate::services::setlist_service::SetlistService;
use crate::infrastructure::action_registry::ActionRegistry;
use setlist::{TransportCommand, NavigationCommand};

pub struct CommandService {
    setlist_service: Arc<SetlistService>,
    action_registry: Arc<ActionRegistry>,
    command_tx: mpsc::Sender<CommandRequest>,
}

enum CommandRequest {
    Transport(TransportCommand),
    Navigation(NavigationCommand),
    // ... other commands
}

impl CommandService {
    pub fn new(
        setlist_service: Arc<SetlistService>,
        action_registry: Arc<ActionRegistry>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let (tx, _rx) = mpsc::channel();
        Ok(Self {
            setlist_service,
            action_registry,
            command_tx: tx,
        })
    }
    
    pub fn execute_transport_command(&self, cmd: TransportCommand) -> Result<(), String> {
        // Implementation
        Ok(())
    }
    
    pub fn process_pending_commands(&self) {
        // Process commands from channel
    }
}
```

### Step 2.3: Create `SeekService`

```rust
// src/services/seek_service.rs
use std::sync::mpsc;
use crate::live::tracks::actions::go_to_song;

pub struct SeekService {
    seek_tx: mpsc::Sender<SeekRequest>,
}

struct SeekRequest {
    song_index: usize,
    section_index: usize,
    response_tx: mpsc::Sender<Result<(), String>>,
}

impl SeekService {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let (tx, _rx) = mpsc::channel();
        Ok(Self {
            seek_tx: tx,
        })
    }
    
    pub fn seek_to_section(
        &self,
        song_index: usize,
        section_index: usize,
    ) -> Result<(), String> {
        // Implementation
        Ok(())
    }
    
    pub fn process_pending_seeks(&self) {
        // Process seeks from channel
    }
}
```

### Step 2.4: Create `StreamService`

```rust
// src/services/stream_service.rs
use std::sync::Arc;
use setlist::SetlistStreamApi;
use crate::services::setlist_service::SetlistService;
use crate::services::command_service::CommandService;
use crate::services::seek_service::SeekService;

pub struct StreamService {
    setlist_service: Arc<SetlistService>,
    command_service: Arc<CommandService>,
    seek_service: Arc<SeekService>,
}

impl StreamService {
    pub fn new(
        setlist_service: Arc<SetlistService>,
        command_service: Arc<CommandService>,
        seek_service: Arc<SeekService>,
    ) -> Self {
        Self {
            setlist_service,
            command_service,
            seek_service,
        }
    }
    
    pub fn create_stream_api(&self) -> Result<SetlistStreamApi, Box<dyn std::error::Error>> {
        // Create stream API with handlers
        let state_provider = Arc::new(ReaperSetlistStateProvider::new(
            self.setlist_service.clone(),
        ));
        let command_handler = Arc::new(ReaperSetlistCommandHandler::new(
            self.command_service.clone(),
            self.seek_service.clone(),
        ));
        
        let api = SetlistStreamApi::spawn_with_handler(state_provider, Some(command_handler));
        Ok(api)
    }
}
```

## Phase 3: Create Adapter Traits

### Step 3.1: Define Trait Interfaces

```rust
// src/adapters/traits.rs

/// Trait for setlist building from REAPER projects
pub trait SetlistBuilder {
    fn build_setlist(&self, filter: Option<&str>) -> Result<Setlist, SetlistError>;
    fn build_song_from_project(&self, project: &Project) -> Result<Song, SetlistError>;
}

/// Trait for transport operations
pub trait TransportAdapter {
    fn read_transport(&self) -> Result<Transport, TransportError>;
    fn play(&self) -> Result<(), TransportError>;
    fn pause(&self) -> Result<(), TransportError>;
    fn stop(&self) -> Result<(), TransportError>;
}

/// Trait for marker operations
pub trait MarkerAdapter {
    fn read_markers(&self, project: &Project) -> Result<Vec<Marker>, String>;
    fn read_regions(&self, project: &Project) -> Result<Vec<Region>, String>;
}
```

### Step 3.2: Implement Traits for REAPER

```rust
// src/adapters/reaper_setlist.rs
use crate::adapters::traits::SetlistBuilder;

pub struct ReaperSetlistBuilder;

impl SetlistBuilder for ReaperSetlistBuilder {
    fn build_setlist(&self, filter: Option<&str>) -> Result<Setlist, SetlistError> {
        // Implementation using REAPER API
        build_setlist_from_open_projects(filter)
    }
    
    fn build_song_from_project(&self, project: &Project) -> Result<Song, SetlistError> {
        // Implementation
        build_song_from_current_project()
    }
}
```

## Phase 4: Refactor Action Registration

### Step 4.1: Create `ActionRegistry` Service

```rust
// src/infrastructure/action_registry.rs
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use reaper_high::{Reaper, RegisteredAction};
use reaper_medium::CommandId;

pub struct ActionRegistry {
    registered_actions: Mutex<Vec<RegisteredAction>>,
    command_ids: Mutex<HashMap<&'static str, CommandId>>,
}

impl ActionRegistry {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            registered_actions: Mutex::new(Vec::new()),
            command_ids: Mutex::new(HashMap::new()),
        })
    }
    
    pub fn register_action(&self, action: ActionDef) -> Result<(), Box<dyn std::error::Error>> {
        // Registration logic
        Ok(())
    }
    
    pub fn get_command_id(&self, command_id_str: &str) -> Option<CommandId> {
        self.command_ids.lock().unwrap().get(command_id_str).copied()
    }
    
    pub fn register_all(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Register all actions
        Ok(())
    }
}
```

## Phase 5: Update Module Structure

### Step 5.1: Create `services/mod.rs`

```rust
// src/services/mod.rs
pub mod setlist_service;
pub mod command_service;
pub mod seek_service;
pub mod stream_service;

pub use setlist_service::SetlistService;
pub use command_service::CommandService;
pub use seek_service::SeekService;
pub use stream_service::StreamService;
```

### Step 5.2: Create `adapters/mod.rs`

```rust
// src/adapters/mod.rs
pub mod traits;
pub mod reaper_setlist;
pub mod reaper_transport;
pub mod reaper_markers;
pub mod reaper_tracks;
pub mod reaper_project;

pub use traits::*;
```

### Step 5.3: Create `infrastructure/mod.rs`

```rust
// src/infrastructure/mod.rs
pub mod action_registry;
pub mod change_detection;
pub mod menu;
pub mod timer;
pub mod iroh_server;
```

## Migration Strategy

### Step-by-Step Migration

1. **Create new structure** alongside old code
2. **Migrate one service at a time**:
   - Start with `SetlistService`
   - Update callers to use new service
   - Remove old static state
3. **Test after each migration**
4. **Remove old code** once migration complete

### Backward Compatibility

- Keep old functions as wrappers initially
- Gradually migrate callers
- Remove wrappers once all migrated

## Testing Strategy

### Unit Tests

```rust
// src/services/setlist_service.rs
#[cfg(test)]
mod tests {
    use super::*;
    use crate::adapters::traits::SetlistBuilder;
    
    struct MockSetlistBuilder;
    
    impl SetlistBuilder for MockSetlistBuilder {
        fn build_setlist(&self, _filter: Option<&str>) -> Result<Setlist, SetlistError> {
            // Mock implementation
            Ok(Setlist::new())
        }
    }
    
    #[test]
    fn test_setlist_service_update() {
        let service = SetlistService::new().unwrap();
        service.update_setlist().unwrap();
        assert!(service.get_setlist().is_some());
    }
}
```

## Benefits of This Structure

1. **Testability**: Can mock REAPER APIs via traits
2. **Clarity**: Each service has a single responsibility
3. **Maintainability**: Easy to find and modify code
4. **Extensibility**: Easy to add new services
5. **Dependency Management**: Clear dependency graph

