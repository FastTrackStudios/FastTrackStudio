//! SongService implementation

use crate::song_builder::SongBuilder;
use daw_control::Daw;
use roam::session::Context;
use session_proto::{Song, SongService};
use tracing::{debug, warn};

/// Implementation of SongService
#[derive(Clone)]
pub struct SongServiceImpl;

impl SongServiceImpl {
    pub fn new() -> Self {
        Self
    }
}

impl SongService for SongServiceImpl {
    async fn build_from_current_project(&self, _cx: &Context) -> Option<Song> {
        debug!("Building song from current project...");

        let daw = Daw::get();

        // Get current project
        let project = match daw.current_project().await {
            Ok(p) => p,
            Err(e) => {
                warn!("Failed to get current project: {}", e);
                return None;
            }
        };

        debug!("Got current project: {}", project.guid());

        // Build song from project
        match SongBuilder::build(&project).await {
            Ok(song) => {
                debug!("Successfully built song: {}", song.name);
                Some(song)
            }
            Err(e) => {
                warn!("Failed to build song from current project: {}", e);
                None
            }
        }
    }

    async fn get_song(&self, _cx: &Context, project_guid: String) -> Option<Song> {
        debug!("Getting song for project: {}", project_guid);

        let daw = Daw::get();

        // Get specific project
        let project = match daw.project(project_guid.clone()).await {
            Ok(p) => p,
            Err(e) => {
                warn!("Failed to get project {}: {}", project_guid, e);
                return None;
            }
        };

        // Build song from project
        match SongBuilder::build(&project).await {
            Ok(song) => {
                debug!("Successfully built song: {}", song.name);
                Some(song)
            }
            Err(e) => {
                warn!("Failed to build song from project {}: {}", project_guid, e);
                None
            }
        }
    }
}
