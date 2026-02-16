//! Domain-aware smart views for the signal UI.
//!
//! These components use [`signal::SignalController`] and signal domain types
//! to fetch data, manage state, and compose the dumb [`crate::components`]
//! building blocks into full editor/browser views.

mod block_editor;
mod collection_browser;
mod metadata_display;
mod module_view;
mod rig_preset_canvas;
mod scene_grid;
mod signal_chain_layout;
mod signal_slider;

// New views
mod automation_lane;
mod fx_binding_status;
mod midi_learn;
mod profile_editor;
mod snapshot_panel;
mod song_setlist_editor;

pub use block_editor::{BlockCard, BlockEditor, MiniKnob};
pub use collection_browser::{BrowseLevel, CollectionBrowser};
pub use metadata_display::MetadataDisplay;
pub use module_view::{ModuleView, ModuleViewMode, ParamChange};
pub use rig_preset_canvas::RigPresetCanvas;
pub use scene_grid::RigSceneGrid;
pub use signal_slider::SignalSlider;

// New view re-exports
pub use automation_lane::{
    AutomationLane, AutomationLaneData, AutomationLaneList, AutomationPoint,
};
pub use fx_binding_status::{
    BindingHealth, FxBindingIndicator, FxBindingPanel, FxBindingRow,
};
pub use midi_learn::{LearnState, MidiLearnPanel, MidiMapping};
pub use profile_editor::{
    OverrideEntry, PatchEditor, PatchEntry, ProfileList, ProfileListEntry,
};
pub use snapshot_panel::{CaptureType, SnapshotEntry, SnapshotPanel};
pub use song_setlist_editor::{
    SectionEntry, SetlistEditor, SetlistEntry, SongEditor, SongEntry,
};
