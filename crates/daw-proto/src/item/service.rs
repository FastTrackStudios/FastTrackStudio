//! Item service trait.
//!
//! `TakeService` retired with the architect::rpc port — see
//! `crate::take::service::Takes`. Item service ports next on the punch
//! list; for now the async `ItemService` stays as-is.

use super::{FadeShape, Item, ItemEvent, ItemRef};
use crate::primitives::{BeatAttachMode, Duration, PositionInSeconds};
use crate::{ProjectContext, TrackRef};
use vox::{Tx, service};

/// Service for managing items on tracks
///
/// Items are media containers that hold one or more takes. They have position,
/// length, and various properties like volume, fades, and timing behavior.
#[service]
pub trait ItemService {
    // =========================================================================
    // Queries
    // =========================================================================

    /// Get all items on a track
    async fn get_items(&self, project: ProjectContext, track: TrackRef) -> Vec<Item>;

    /// Get a specific item
    async fn get_item(&self, project: ProjectContext, item: ItemRef) -> Option<Item>;

    /// Get all items in the project
    async fn get_all_items(&self, project: ProjectContext) -> Vec<Item>;

    /// Get all selected items in the project
    async fn get_selected_items(&self, project: ProjectContext) -> Vec<Item>;

    /// Get the number of items on a track
    async fn item_count(&self, project: ProjectContext, track: TrackRef) -> u32;

    // =========================================================================
    // CRUD Operations
    // =========================================================================

    /// Add a new item to a track
    ///
    /// Returns the GUID of the created item, or None if creation failed.
    async fn add_item(
        &self,
        project: ProjectContext,
        track: TrackRef,
        position: PositionInSeconds,
        length: Duration,
    ) -> Option<String>;

    /// Delete an item
    async fn delete_item(&self, project: ProjectContext, item: ItemRef);

    /// Duplicate an item
    ///
    /// Returns the GUID of the new item, or None if duplication failed.
    async fn duplicate_item(&self, project: ProjectContext, item: ItemRef) -> Option<String>;

    // =========================================================================
    // Position & Length
    // =========================================================================

    /// Set the position of an item
    async fn set_position(
        &self,
        project: ProjectContext,
        item: ItemRef,
        position: PositionInSeconds,
    );

    /// Set the length of an item
    async fn set_length(&self, project: ProjectContext, item: ItemRef, length: Duration);

    /// Move an item to a different track
    async fn move_to_track(&self, project: ProjectContext, item: ItemRef, track: TrackRef);

    /// Set the snap offset
    async fn set_snap_offset(&self, project: ProjectContext, item: ItemRef, offset: Duration);

    // =========================================================================
    // State
    // =========================================================================

    /// Set whether an item is muted
    async fn set_muted(&self, project: ProjectContext, item: ItemRef, muted: bool);

    /// Set whether an item is selected
    async fn set_selected(&self, project: ProjectContext, item: ItemRef, selected: bool);

    /// Set whether an item is locked
    async fn set_locked(&self, project: ProjectContext, item: ItemRef, locked: bool);

    /// Select or deselect all items in the project
    async fn select_all_items(&self, project: ProjectContext, selected: bool);

    // =========================================================================
    // Audio Properties
    // =========================================================================

    /// Set the volume of an item (1.0 = 0dB)
    async fn set_volume(&self, project: ProjectContext, item: ItemRef, volume: f64);

    /// Set the fade in properties
    async fn set_fade_in(
        &self,
        project: ProjectContext,
        item: ItemRef,
        length: Duration,
        shape: FadeShape,
    );

    /// Set the fade out properties
    async fn set_fade_out(
        &self,
        project: ProjectContext,
        item: ItemRef,
        length: Duration,
        shape: FadeShape,
    );

    // =========================================================================
    // Timing Behavior
    // =========================================================================

    /// Set whether the source should loop
    async fn set_loop_source(&self, project: ProjectContext, item: ItemRef, loop_source: bool);

    /// Set how the item attaches to the timeline
    async fn set_beat_attach_mode(
        &self,
        project: ProjectContext,
        item: ItemRef,
        mode: BeatAttachMode,
    );

    /// Set whether the item auto-stretches at tempo changes
    async fn set_auto_stretch(&self, project: ProjectContext, item: ItemRef, auto_stretch: bool);

    // =========================================================================
    // Visual Properties
    // =========================================================================

    /// Set the custom color (None to use default)
    async fn set_color(&self, project: ProjectContext, item: ItemRef, color: Option<u32>);

    /// Set the group ID (None to remove from group)
    async fn set_group_id(&self, project: ProjectContext, item: ItemRef, group_id: Option<u32>);

    // =========================================================================
    // Subscriptions
    // =========================================================================

    /// Subscribe to item change events for a project.
    async fn subscribe_items(&self, project: ProjectContext, tx: Tx<ItemEvent>);
}
