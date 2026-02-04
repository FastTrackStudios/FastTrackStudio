//! SetlistBuilder - Build setlists from open DAW projects
//!
//! Scans open projects and combines their song structures into a setlist.

use crate::song_builder::SongBuilder;
use daw_control::Daw;
use session_proto::Setlist;
use tracing::{debug, info, warn};

/// Builder for assembling setlists from open DAW projects
pub struct SetlistBuilder;

impl SetlistBuilder {
    /// Build a setlist from all currently open DAW projects
    ///
    /// Iterates through all open projects, attempts to extract a Song from each,
    /// and combines them into a complete Setlist. Projects that don't contain
    /// valid song structure are skipped with a warning.
    pub async fn build_from_open_projects(daw: &Daw) -> eyre::Result<Setlist> {
        debug!("============================================================");
        debug!("SETLIST BUILDER: Building setlist from open projects...");
        debug!("============================================================");

        // Get all open projects
        let projects = daw.projects().await?;
        debug!("Found {} open projects", projects.len());

        for (i, project) in projects.iter().enumerate() {
            debug!("  Project {}: {}", i, project.guid());
        }

        let mut songs = Vec::new();

        // Extract song from each project
        for (idx, project) in projects.iter().enumerate() {
            debug!("------------------------------------------------------------");
            debug!(
                "Processing project {}/{}: {}",
                idx + 1,
                projects.len(),
                project.guid()
            );

            match SongBuilder::build(project).await {
                Ok(song) => {
                    debug!(
                        "  Song extracted: {} ({} sections)",
                        song.name,
                        song.sections.len()
                    );
                    songs.push(song);
                }
                Err(e) => {
                    warn!(
                        "  ✗ Failed to extract song from project {}: {}",
                        project.guid(),
                        e
                    );
                }
            }
        }

        debug!("Setlist complete: {} songs extracted", songs.len());

        // Sort songs by start position (they should already be in project tab order)
        // songs.sort_by(|a, b| {
        //     a.start_seconds
        //         .partial_cmp(&b.start_seconds)
        //         .unwrap_or(std::cmp::Ordering::Equal)
        // });

        Ok(Setlist {
            id: None,
            name: Self::generate_setlist_name(&songs),
            songs,
        })
    }

    /// Generate a setlist name from the songs
    ///
    /// Uses the current date/time or a meaningful name based on song count.
    fn generate_setlist_name(songs: &[session_proto::Song]) -> String {
        if songs.is_empty() {
            return "Empty Setlist".to_string();
        }

        // Use current date/time
        let now = chrono::Local::now();
        format!("Setlist - {}", now.format("%Y-%m-%d"))
    }
}
