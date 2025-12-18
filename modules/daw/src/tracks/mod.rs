//! Track module - contains structures and functionality for managing tracks

pub mod track;
pub mod track_list;
pub mod api;
pub mod item;
pub mod envelope;
pub mod fx_chain;
pub mod track_chunk;
pub mod reactive;
pub mod hierarchy_builder;
pub mod types;

pub use track::Track;
pub use types::{TrackName, TrackGuid, MetadataKey};
pub use track_list::{AddChild, PrintTrackTree, IntoTrackVec};
pub use hierarchy_builder::{TrackHierarchyBuilder, build_hierarchy, build_track_hierarchy};
pub use track_chunk::{parse_track_chunk, ParsedTrackChunk};
pub use api::folder::{TcpFolderState, McpFolderState};
pub use api::automation::AutomationMode;
pub use api::free_mode::FreeMode;
pub use api::fixed_lanes::{FixedLanesSettings, LaneSoloSettings, LaneRecordSettings, LaneNameSettings};
pub use api::record::{RecordSettings, RecordMode, MonitorMode};
pub use api::receive::{TrackReceive, ReceiveMode};
pub use api::hardware::{HardwareOutputSettings, MidiOutputSettings, MasterSendSettings};
pub use api::solo::SoloMode;
pub use api::collapse::{ArrangeCollapseState, MixerCollapseState, WiringCollapseState};
pub use item::Item;
pub use envelope::{Envelope, EnvelopePoint, EnvelopePointShape, AutomationItem, ExtensionData};
pub use fx_chain::{FxChain, Plugin};
pub use reactive::{
    TrackStreams, TrackReactiveService, DefaultTrackReactiveService,
    TrackReactiveState, EventStreamSubject as TrackEventStreamSubject,
};
pub use reactive::irpc::{TrackProtocol, TrackApi, TrackUpdateMessage};
