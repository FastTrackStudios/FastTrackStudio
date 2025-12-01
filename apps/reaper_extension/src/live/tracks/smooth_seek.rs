//! Smooth Seek System
//!
//! Provides smooth seeking when clicking sections during playback.
//! When playing a song and clicking another section from the same song,
//! it queues a smooth seek that executes at the next measure start (quantize point).
//!
//! Based on SWS Region Playlist smooth seek behavior.

use reaper_high::{Project, Reaper};
use reaper_medium::{PositionInSeconds, BookmarkRef, BookmarkId, MarkerOrRegionPosition};
use std::sync::{Arc, Mutex, OnceLock};
use tracing::{debug, info, warn};

/// Quantize level for smooth seeking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmoothSeekQuantize {
    /// Quantize to next measure (default)
    Measure,
    /// Quantize to next beat (based on time signature denominator)
    Beat,
    /// Quantize to next half-beat (e.g., quarter notes in 4/4, eighth notes in 6/8)
    HalfBeat,
}

impl Default for SmoothSeekQuantize {
    fn default() -> Self {
        SmoothSeekQuantize::Measure
    }
}

/// A queued smooth seek operation
#[derive(Debug, Clone)]
pub struct QueuedSeek {
    /// Target position to seek to
    pub target_position: PositionInSeconds,
    /// Target measure index where the seek should execute (next measure start)
    pub target_measure_index: i32,
    /// Whether to start playback after seeking
    pub start_playback: bool,
    /// SEEK_DESTINATION marker BookmarkId for cleanup and seeking
    pub seek_destination_bookmark_id: Option<BookmarkId>,
    /// SEEK_TRIGGER marker BookmarkId for cleanup - placed at next measure start
    pub seek_trigger_bookmark_id: Option<BookmarkId>,
}

/// State for smooth seek queuing
#[derive(Debug)]
struct SmoothSeekState {
    /// Currently queued seek (if any)
    queued_seek: Option<QueuedSeek>,
    /// Last known measure index (for detecting measure boundaries)
    last_measure_index: Option<i32>,
    /// Flag to prevent double execution of the same seek
    seek_executed: bool,
    /// Markers to delete on the main thread (queued from background threads)
    /// Stored as BookmarkId for reliable identification
    markers_to_delete: Vec<BookmarkId>,
    /// Target position we're trying to reach (for verification)
    verification_target: Option<PositionInSeconds>,
    /// Number of verification checks performed
    verification_checks: u32,
    /// Quantize level for smooth seeking
    quantize_level: SmoothSeekQuantize,
}

impl SmoothSeekState {
    fn new() -> Self {
        Self {
            queued_seek: None,
            last_measure_index: None,
            seek_executed: false,
            markers_to_delete: Vec::new(),
            verification_target: None,
            verification_checks: 0,
            quantize_level: SmoothSeekQuantize::default(),
        }
    }
    
    /// Set the quantize level for smooth seeking
    pub fn set_quantize_level(&mut self, level: SmoothSeekQuantize) {
        self.quantize_level = level;
    }
    
    /// Get the current quantize level
    pub fn quantize_level(&self) -> SmoothSeekQuantize {
        self.quantize_level
    }
}

/// Smooth seek handler
pub struct SmoothSeekHandler {
    state: Arc<Mutex<SmoothSeekState>>,
}

impl SmoothSeekHandler {
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(SmoothSeekState::new())),
        }
    }
    
    /// Set the quantize level for smooth seeking
    pub fn set_quantize_level(&self, level: SmoothSeekQuantize) {
        let mut state = self.state.lock().unwrap();
        state.set_quantize_level(level);
        info!(quantize_level = ?level, "Set smooth seek quantize level");
    }
    
    /// Get the current quantize level
    pub fn quantize_level(&self) -> SmoothSeekQuantize {
        let state = self.state.lock().unwrap();
        state.quantize_level()
    }

    /// Queue a smooth seek to a specific position
    /// 
    /// The seek will be performed at the next measure start (quantize point).
    pub fn queue_seek(
        &self,
        project: &Project,
        target_position: PositionInSeconds,
        start_playback: bool,
    ) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        // Get current play position using transport adapter
        use crate::implementation::transport::ReaperTransport;
        let transport = ReaperTransport::new(project.clone());
        let current_pos = transport.read_transport()
            .ok()
            .and_then(|t| Some(PositionInSeconds::new(t.playhead_position.time.to_seconds()).ok()?))
            .unwrap_or_else(|| {
                project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO)
            });
        
        // Get current beat info
        let beat_info = medium_reaper.time_map_2_time_to_beats(project_context, current_pos);
        let current_measure = beat_info.measure_index;
        let beats_since_measure = beat_info.beats_since_measure.get();
        let time_sig = beat_info.time_signature;
        
        // Get quantize level from state
        let quantize_level = {
            let state = self.state.lock().unwrap();
            state.quantize_level()
        };
        
        // Calculate target quantize position based on quantize level
        let target_quantize_time = match quantize_level {
            SmoothSeekQuantize::Measure => {
                // Next measure start (current behavior)
        let target_measure_index = current_measure + 1;
                let measure_info = medium_reaper.time_map_get_measure_info(project_context, target_measure_index);
                measure_info.start_time
            }
            SmoothSeekQuantize::Beat => {
                // Next beat boundary
                // beats_since_measure is fractional (e.g., 2.5 means we're halfway through beat 3)
                // Next beat = floor(beats_since_measure) + 1
                let next_beat = beats_since_measure.floor() + 1.0;
                // Get the measure start time
                let measure_info = medium_reaper.time_map_get_measure_info(project_context, current_measure);
                let measure_start = measure_info.start_time.get();
                // Get the next measure start to calculate measure duration
                let next_measure_info = medium_reaper.time_map_get_measure_info(project_context, current_measure + 1);
                let next_measure_start = next_measure_info.start_time.get();
                let measure_duration = next_measure_start - measure_start;
                // Calculate time offset within the measure for the target beat
                // beats_since_measure is 0-based, so next_beat is the beat number (1-based within measure)
                let beat_duration = measure_duration / time_sig.numerator.get() as f64;
                let time_offset = (next_beat - 1.0) * beat_duration;
                PositionInSeconds::new(measure_start + time_offset)
                    .unwrap_or_else(|_| next_measure_info.start_time)
            }
            SmoothSeekQuantize::HalfBeat => {
                // Next half-beat boundary
                // Half-beat = floor(beats_since_measure * 2) / 2 + 0.5
                // This gives us the next 0.5 beat boundary (000 or 500 subdivisions)
                let next_half_beat = (beats_since_measure * 2.0).floor() / 2.0 + 0.5;
                // Get the measure start time
                let measure_info = medium_reaper.time_map_get_measure_info(project_context, current_measure);
                let measure_start = measure_info.start_time.get();
                // Get the next measure start to calculate measure duration
                let next_measure_info = medium_reaper.time_map_get_measure_info(project_context, current_measure + 1);
                let next_measure_start = next_measure_info.start_time.get();
                let measure_duration = next_measure_start - measure_start;
                // Calculate time offset within the measure for the target half-beat
                let beat_duration = measure_duration / time_sig.numerator.get() as f64;
                let time_offset = next_half_beat * beat_duration;
                PositionInSeconds::new(measure_start + time_offset)
                    .unwrap_or_else(|_| next_measure_info.start_time)
            }
        };
        
        // For logging, calculate target measure index (used in old code)
        let target_measure_index = match quantize_level {
            SmoothSeekQuantize::Measure => current_measure + 1,
            _ => {
                // For beat/half-beat, find which measure the target is in
                let target_beat_info = medium_reaper.time_map_2_time_to_beats(project_context, target_quantize_time);
                target_beat_info.measure_index
            }
        };
        
        let mut state = self.state.lock().unwrap();
        
        // Clear any existing queued seek - we only allow one seek at a time
        // BUT: Only delete markers if the old seek hasn't been executed yet
        // If it's already executing (seek_executed = true), the markers are still needed
        // and will be cleaned up when verification completes
        let mut markers_to_delete_now = Vec::new();
        let mut markers_to_queue_for_later = Vec::new();
        let has_active_verification = state.verification_target.is_some();
        let seek_was_executed = state.seek_executed;
        
        // Extract marker BookmarkIds from old seek before mutating state
        if let Some(ref old_seek) = state.queued_seek {
            // Only delete markers if:
            // 1. The seek hasn't been executed yet (not in smooth seek phase)
            // 2. OR there's no active verification (seek completed or never started)
            if !seek_was_executed || !has_active_verification {
                if let Some(old_dest_id) = old_seek.seek_destination_bookmark_id {
                    markers_to_delete_now.push(old_dest_id);
                }
                if let Some(old_trigger_id) = old_seek.seek_trigger_bookmark_id {
                    markers_to_delete_now.push(old_trigger_id);
                }
            } else {
                // Seek is in progress - queue markers for deletion after verification completes
                // They'll be cleaned up in the verification logic
                if let Some(old_dest_id) = old_seek.seek_destination_bookmark_id {
                    markers_to_queue_for_later.push(old_dest_id);
                }
                if let Some(old_trigger_id) = old_seek.seek_trigger_bookmark_id {
                    markers_to_queue_for_later.push(old_trigger_id);
                }
            }
        }
        
        // Now we can mutate state (old_seek borrow is dropped)
        state.markers_to_delete.extend(markers_to_queue_for_later);
        
        // Clear the queued seek state and any active verification
        state.queued_seek = None;
        state.seek_executed = false;
        state.verification_target = None;
        state.verification_checks = 0;
        drop(state); // Release lock before deleting markers and creating new ones
        
        // Delete old markers immediately (on main thread) before creating/moving new ones
        // Only if they weren't in use by an active smooth seek
        for bookmark_id in markers_to_delete_now {
            if let Err(e) = Self::delete_seek_marker_by_id(project, bookmark_id) {
                warn!(error = %e, bookmark_id = bookmark_id.to_raw(), "Failed to delete old seek marker");
            }
        }
        
        // Create new markers - we track them by BookmarkId for reliable deletion
        let seek_destination_name = "SEEK_DESTINATION".to_string();
        let seek_destination_bookmark_id = Self::create_marker(project, &seek_destination_name, target_position)
            .map_err(|e| anyhow::anyhow!("Failed to create SEEK_DESTINATION marker: {}", e))?;
        let seek_trigger_name = "SEEK_TRIGGER".to_string();
        let seek_trigger_bookmark_id = Self::create_marker(project, &seek_trigger_name, target_quantize_time)
            .map_err(|e| anyhow::anyhow!("Failed to create SEEK_TRIGGER marker: {}", e))?;
        
        // CRITICAL: Call UpdateTimeline() AFTER both markers are created
        // This ensures REAPER recognizes the markers for smooth seeking
        // Note: create_marker also calls UpdateTimeline(), but we call it again here
        // to ensure everything is synchronized after both markers are ready
        medium_reaper.update_timeline();
        
        // CRITICAL: Call GoToMarker IMMEDIATELY when queuing the seek
        // REAPER's smooth seek system needs the GoToMarker command to be active
        // BEFORE playback reaches the trigger marker, so it can prepare the smooth transition
        let marker_ref = BookmarkRef::Id(seek_destination_bookmark_id);
        medium_reaper.go_to_marker(project_context, marker_ref);
        
        let mut state = self.state.lock().unwrap();
        
        let queued = QueuedSeek {
            target_position,
            target_measure_index,
            start_playback,
            seek_destination_bookmark_id: Some(seek_destination_bookmark_id),
            seek_trigger_bookmark_id: Some(seek_trigger_bookmark_id),
        };
        
        state.queued_seek = Some(queued.clone());
        // Store the measure we were in when we queued (for boundary detection)
        state.last_measure_index = Some(current_measure);
        state.seek_executed = true; // Mark as executed since we called GoToMarker immediately
        // Set up verification to check if we actually reached the target
        state.verification_target = Some(target_position);
        state.verification_checks = 0;
        
        Ok(())
    }

    /// Clear the queued seek
    pub fn clear_queue(&self) {
        let mut state = self.state.lock().unwrap();
        state.queued_seek = None;
        debug!("Cleared smooth seek queue");
    }
    
    /// Find a marker at the specified position (within tolerance)
    /// Returns Some(BookmarkId) if a marker exists at that position, None otherwise
    fn find_marker_at_position(
        project: &Project,
        position: PositionInSeconds,
        tolerance: f64,
    ) -> Option<BookmarkId> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        let mut idx = 0u32;
        loop {
            let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                result.map(|res| {
                    (res.id, res.position, res.region_end_position.is_some())
                })
            });
            
            match marker_info {
                Some((bookmark_id, marker_pos, is_region)) => {
                    // Check if it's a marker (not region) and at the target position
                    if !is_region {
                        let position_diff = (marker_pos.get() - position.get()).abs();
                        if position_diff < tolerance {
                            return Some(bookmark_id);
                        }
                    }
                    idx += 1;
                }
                None => break,
            }
        }
        
        debug!(
            target_position = position.get(),
            "No marker found at target position"
        );
        None
    }

    /// Find a bookmark by its BookmarkId and return its name
    /// Returns None if the bookmark doesn't exist or is a region
    fn find_bookmark_name_by_id(
        project: &Project,
        bookmark_id: BookmarkId,
    ) -> Option<String> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        let mut idx = 0u32;
        loop {
            let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                result.map(|res| {
                    let name = res.name.to_str().trim_matches('"').trim().to_string();
                    (res.id, name, res.region_end_position.is_some())
                })
            });
            
            match marker_info {
                Some((id, name, is_region)) => {
                    if id == bookmark_id && !is_region {
                        return Some(name);
                    }
                    idx += 1;
                }
                None => break,
            }
        }
        
        None
    }
    
    /// Get or create a marker at the specified position
    /// If a marker already exists at that position, reuse it instead of creating a new one
    /// Only deletes existing markers with our exact name (not position-based) to prevent deleting user markers
    /// Returns BookmarkId for GoToMarker operations
    fn create_marker(
        project: &Project,
        marker_name: &str,
        position: PositionInSeconds,
    ) -> anyhow::Result<BookmarkId> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        // First, check if there's already a marker at this position
        // If so, reuse it instead of creating a new one
        // Use a larger tolerance (0.01 seconds = 10ms) to catch markers that are very close
        let position_tolerance = 0.01; // 10ms tolerance
        if let Some(existing_bookmark_id) = Self::find_marker_at_position(project, position, position_tolerance) {
            if let Some(_actual_name) = Self::find_bookmark_name_by_id(project, existing_bookmark_id) {
                return Ok(existing_bookmark_id);
            }
        }
        
        // No marker exists at this position, so we can create one
        // But first, delete any markers with our exact name (from previous seeks)
        let mut idx = 0u32;
        let mut markers_to_delete: Vec<(BookmarkId, String)> = Vec::new();
        
        loop {
            let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                result.map(|res| {
                    let name = res.name.to_str().trim_matches('"').trim().to_string();
                    (res.id, name, res.region_end_position.is_some())
                })
            });
            
            match marker_info {
                Some((bookmark_id, name, is_region)) => {
                    // Only delete markers (not regions) that have our exact name
                    // This ensures we only delete markers we created, never user markers
                    if !is_region && name == marker_name {
                        markers_to_delete.push((bookmark_id, name));
                    }
                    idx += 1;
                }
                None => {
                    // No more markers
                    break;
                }
            }
        }
        
        // Delete only markers we created (with our exact name), with verification
        for (bookmark_id, expected_name) in markers_to_delete {
            // Verify the marker still has the expected name before deleting
            // Enumerate all bookmarks to find the one with matching BookmarkId
            let mut idx = 0u32;
            loop {
                let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                    result.map(|res| {
                        let name = res.name.to_str().trim_matches('"').trim().to_string();
                        (res.id, name, res.region_end_position.is_some())
                    })
                });
                
                match marker_info {
                    Some((id, actual_name, is_region)) => {
                        if id == bookmark_id && !is_region {
                            if actual_name == expected_name {
                                if let Err(e) = Self::delete_seek_marker_by_id(project, bookmark_id) {
                                    warn!(error = %e, bookmark_id = bookmark_id.to_raw(), "Failed to delete existing marker");
                                }
                    } else {
                                warn!(
                            bookmark_id = bookmark_id.to_raw(),
                                    expected_name = expected_name,
                                    actual_name = actual_name,
                                    "Marker name changed, skipping deletion (safety check)"
                                );
                            }
                            break;
                        }
                        idx += 1;
                    }
                    None => break,
                }
            }
            
            // Marker not found, may have already been deleted (not an error)
        }
        
        // Update timeline after deletions
        medium_reaper.update_timeline();
        
        // Now create a new marker (since no marker exists at this position)
        let _marker_index = medium_reaper.add_project_marker_2(
            project_context,
            MarkerOrRegionPosition::Marker(position),
            marker_name,
            None, // at_index: None means append
            None, // color: None means default
        )
        .map_err(|e| anyhow::anyhow!("Failed to create marker {} at position {}: {:?}", marker_name, position.get(), e))?;
        
        medium_reaper.update_timeline();
        
        // Find the newly created marker by enumerating and matching position and name
        // We can't use marker_index because it may have changed if other markers were deleted
        let mut idx = 0u32;
        loop {
            let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                result.map(|res| {
                    let name = res.name.to_str().trim_matches('"').trim().to_string();
                    (res.id, res.position, name, res.region_end_position.is_some())
                })
            });
            
            match marker_info {
                Some((bookmark_id, marker_pos, actual_name, is_region)) => {
                    if !is_region {
                        let position_diff = (marker_pos.get() - position.get()).abs();
                        // Check if this is our newly created marker (name matches and position is close)
                        if actual_name == marker_name && position_diff < 0.01 {
                            return Ok(bookmark_id);
                        }
                        // CRITICAL: If the marker name doesn't match, REAPER reused an existing marker
                        // This can happen if a marker exists at the exact position but our check missed it
                        // (e.g., due to floating point precision issues). In this case, we should use the
                        // existing marker instead of failing - it will work for smooth seeking anyway.
                        if position_diff < 0.01 && actual_name != marker_name {
                warn!(
                    expected_name = marker_name,
                    actual_name = actual_name,
                    bookmark_id = bookmark_id.to_raw(),
                    position = position.get(),
                                "REAPER reused an existing marker at this position. Using it instead of creating a new one."
                            );
                            // Use the existing marker - it will work fine for smooth seeking
                            // Don't try to delete it as it's a user marker
                            info!(
                    bookmark_id = bookmark_id.to_raw(),
                                actual_name = actual_name,
                    position = position.get(),
                                "Using existing marker for smooth seek"
                );
                            return Ok(bookmark_id);
                        }
                    }
                    idx += 1;
                }
                None => break,
            }
        }
        
        // If we get here, we couldn't find the marker we just created
        // This shouldn't happen, but return an error
        Err(anyhow::anyhow!(
            "Failed to find newly created marker '{}' at position {}",
            marker_name,
            position.get()
        ))
    }
    
    /// Create a "SEEK_DESTINATION" marker and seek to it immediately
    /// This is a helper function for immediate seeks (not queued for measure boundary)
    pub fn seek_to_position_via_marker(
        &self,
        project: &Project,
        target_position: PositionInSeconds,
    ) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        // Create the SEEK_DESTINATION marker and get its BookmarkId
        let marker_name = "SEEK_DESTINATION".to_string();
        let bookmark_id = Self::create_marker(project, &marker_name, target_position)?;
        
        // Use BookmarkRef::Id directly with the BookmarkId we got
        let marker_ref = BookmarkRef::Id(bookmark_id);
        
        // Use GoToMarker for smooth seeking
        medium_reaper.go_to_marker(project_context, marker_ref);
        
        info!(
            bookmark_id = bookmark_id.to_raw(),
            target_position = target_position.get(),
            "Seeked to position using SEEK_DESTINATION marker"
        );
        
        // Queue marker deletion to happen on main thread
        let mut state = self.state.lock().unwrap();
        state.markers_to_delete.push(bookmark_id);
        
        Ok(())
    }
    
    /// Delete a seek marker by BookmarkId
    /// Enumerate all markers and log them for debugging
    /// Uses safe enum_project_markers_3 API
    #[allow(dead_code)]
    fn log_all_markers(project: &Project, context: &str) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        info!("=== Enumerating all markers ({}) ===", context);
        
        let mut idx = 0u32;
        loop {
            let marker_info = medium_reaper.enum_project_markers_3(project_context, idx, |result| {
                result.map(|res| {
                    let name = res.name.to_str().trim_matches('"').trim().to_string();
                    (res.id, name, res.position, res.region_end_position.is_some())
                })
            });
            
            match marker_info {
                Some((bookmark_id, name, position, is_region)) => {
                    if !is_region {
                        // It's a marker (not a region)
                        info!(
                            marker_index = idx,
                            bookmark_id = bookmark_id.to_raw(),
                            marker_name = name,
                            position = position.get(),
                            "Marker found"
                        );
                    }
                    idx += 1;
                }
                None => {
                    // No more markers
                    break;
                }
            }
        }
        
        info!("=== End marker enumeration ({}) ===", context);
    }
    
    /// Delete a seek marker by BookmarkId with verification
    /// Only deletes markers with names "SEEK_DESTINATION" or "SEEK_TRIGGER" as a safety check
    fn delete_seek_marker_by_id(
        project: &Project,
        bookmark_id: BookmarkId,
    ) -> anyhow::Result<()> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        // SAFETY CHECK: Verify the marker has one of our expected names before deleting
        // Use our helper function to find the marker name by BookmarkId
        let marker_name = Self::find_bookmark_name_by_id(project, bookmark_id);
        
        // Only delete if it's one of our markers
        if let Some(name) = marker_name {
            if name != "SEEK_DESTINATION" && name != "SEEK_TRIGGER" {
                warn!(
                    bookmark_id = bookmark_id.to_raw(),
                    marker_name = name,
                    "Refusing to delete marker - not one of our seek markers (safety check)"
                );
                return Err(anyhow::anyhow!(
                    "Refusing to delete marker '{}' - not a seek marker (safety check)",
                    name
                ));
            }
            
        } else {
            // Marker not found, may have already been deleted (not an error)
            return Ok(());
        }
        
        // Delete by BookmarkId using DeleteProjectMarker
        let result = unsafe {
            medium_reaper.low().DeleteProjectMarker(
                project_context.to_raw(),
                bookmark_id.to_raw(), // marker ID
                false, // is_region: false means delete marker, not region
            )
        };
        
        if !result {
            warn!(
                bookmark_id = bookmark_id.to_raw(),
                "Failed to delete marker (DeleteProjectMarker returned false)"
            );
        }
        
        Ok(())
    }

    /// Process playback position and check if we should execute a queued seek
    /// Called from the timer when playback position changes
    /// Returns Ok(true) if a seek was executed, Ok(false) otherwise
    pub fn process_playback_position(
        &self,
        _project: &Project,
        current_pos: PositionInSeconds,
    ) -> anyhow::Result<bool> {
        let mut state = self.state.lock().unwrap();
        
        // Check if we need to verify a previous seek
        if let Some(verification_target) = state.verification_target {
            state.verification_checks += 1;
            let current_time = current_pos.get();
            let target_time = verification_target.get();
            let position_diff = (current_time - target_time).abs();
            
            // Store markers to delete when verification completes
            let mut markers_to_delete_on_completion = Vec::new();
            
            // Check if we've reached the target (within 50ms tolerance for smooth seeks)
            if position_diff < 0.05 {
                // Now that seek has completed, queue markers for deletion
                if let Some(ref completed_seek) = state.queued_seek {
                    if let Some(trigger_id) = completed_seek.seek_trigger_bookmark_id {
                        markers_to_delete_on_completion.push(trigger_id);
                    }
                    if let Some(dest_id) = completed_seek.seek_destination_bookmark_id {
                        markers_to_delete_on_completion.push(dest_id);
                    }
                }
                
                // Clear verification and queued seek
                state.verification_target = None;
                state.verification_checks = 0;
                state.queued_seek = None;
                state.seek_executed = false;
                
                // Queue marker deletions to happen on main thread
                state.markers_to_delete.extend(markers_to_delete_on_completion);
            } else if state.verification_checks > 150 {
                // After 150 checks (about 5 seconds at 30Hz), give up and log failure
                warn!(
                    target_position = target_time,
                    actual_position = current_time,
                    position_diff,
                    verification_checks = state.verification_checks,
                    "❌ Smooth seek verification: Did not reach target position after {} checks (diff: {:.3}s)",
                    state.verification_checks,
                    position_diff
                );
                
                // Even though seek failed, clean up markers
                if let Some(ref failed_seek) = state.queued_seek {
                    if let Some(trigger_id) = failed_seek.seek_trigger_bookmark_id {
                        markers_to_delete_on_completion.push(trigger_id);
                    }
                    if let Some(dest_id) = failed_seek.seek_destination_bookmark_id {
                        markers_to_delete_on_completion.push(dest_id);
                    }
                }
                
                // Clear verification and queued seek
                state.verification_target = None;
                state.verification_checks = 0;
                state.queued_seek = None;
                state.seek_executed = false;
                
                // Queue marker deletions to happen on main thread
                state.markers_to_delete.extend(markers_to_delete_on_completion);
            } else {
                // Still waiting - log progress every 10 checks
                if state.verification_checks % 10 == 0 {
                    debug!(
                        target_position = target_time,
                        actual_position = current_time,
                        position_diff,
                        verification_checks = state.verification_checks,
                        "Smooth seek verification: Still waiting to reach target (check {})",
                        state.verification_checks
                    );
                }
            }
        }
        
        // Note: GoToMarker is now called immediately in queue_seek()
        // We don't need to execute it here - just monitor for verification
        // The verification logic above will handle checking if we reached the target
        
        Ok(false)
    }
    
    /// Process pending marker deletions (called from main thread timer)
    pub fn process_pending_deletions(&self, project: &Project) {
        let mut state = self.state.lock().unwrap();
        
        if state.markers_to_delete.is_empty() {
            return;
        }
        
        // Take all markers to delete
        let markers_to_delete = std::mem::take(&mut state.markers_to_delete);
        drop(state); // Release lock before deleting
        
        for bookmark_id in markers_to_delete {
            if let Err(e) = Self::delete_seek_marker_by_id(project, bookmark_id) {
                warn!(error = %e, bookmark_id = bookmark_id.to_raw(), "Failed to delete marker");
            } else {
                debug!(bookmark_id = bookmark_id.to_raw(), "Deleted marker");
            }
        }
    }
}

impl Default for SmoothSeekHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Global smooth seek handler instance
static SMOOTH_SEEK_HANDLER: OnceLock<SmoothSeekHandler> = OnceLock::new();

/// Get or initialize the global smooth seek handler
pub fn get_smooth_seek_handler() -> &'static SmoothSeekHandler {
    SMOOTH_SEEK_HANDLER.get_or_init(|| SmoothSeekHandler::new())
}

