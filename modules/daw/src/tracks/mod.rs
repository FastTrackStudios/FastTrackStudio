//! Track module - contains structures and functionality for managing tracks

pub mod api;
pub mod envelope;
pub mod fx_chain;
pub mod hierarchy_builder;
pub mod item;
pub mod reactive;
pub mod track;
pub mod track_chunk;
pub mod track_list;
pub mod types;

pub use api::automation::AutomationMode;
pub use api::collapse::{ArrangeCollapseState, MixerCollapseState, WiringCollapseState};
pub use api::fixed_lanes::{
    FixedLanesSettings, LaneNameSettings, LaneRecordSettings, LaneSoloSettings,
};
pub use api::folder::{McpFolderState, TcpFolderState};
pub use api::free_mode::FreeMode;
pub use api::hardware::{HardwareOutputSettings, MasterSendSettings, MidiOutputSettings};
pub use api::receive::{ReceiveMode, TrackReceive};
pub use api::record::{MonitorMode, RecordMode, RecordSettings};
pub use api::solo::SoloMode;
pub use envelope::{AutomationItem, Envelope, EnvelopePoint, EnvelopePointShape, ExtensionData};
pub use fx_chain::{FxChain, Plugin};
pub use hierarchy_builder::{TrackHierarchyBuilder, build_hierarchy, build_track_hierarchy};
pub use item::Item;
pub use reactive::irpc::{TrackApi, TrackProtocol, TrackUpdateMessage};
pub use reactive::{
    DefaultTrackReactiveService, EventStreamSubject as TrackEventStreamSubject,
    TrackReactiveService, TrackReactiveState, TrackStreams,
};
pub use track::{
    IntoItems, Track, TrackGroup, TrackStructureBuilder, assert_tracks_equal, display_tracklist,
};
pub use track_chunk::{ParsedTrackChunk, parse_track_chunk};
pub use track_list::{AddChild, IntoTrackVec, PrintTrackTree};
pub use types::{MetadataKey, TrackGuid, TrackName};
