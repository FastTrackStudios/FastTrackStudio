//! Track API and supporting types

pub mod automation;
pub mod collapse;
pub mod fixed_lanes;
pub mod folder;
pub mod free_mode;
pub mod hardware;
pub mod midi_note_name;
pub mod quantize;
pub mod receive;
pub mod record;
pub mod send_mode;
pub mod solo;
pub mod timebase;

pub use automation::AutomationMode;
pub use collapse::{
    ArrangeCollapseState, BusCompactSettings, Hidden as CollapseHidden, MixerCollapseState,
    WiringCollapseState,
};
pub use fixed_lanes::{FixedLanesSettings, LaneNameSettings, LaneRecordSettings, LaneSoloSettings};
pub use folder::{FolderDepthChange, McpFolderState, TcpFolderState, TrackDepth};
pub use free_mode::FreeMode;
pub use hardware::{HardwareOutputSettings, MasterSendSettings, MidiOutputSettings};
pub use midi_note_name::MidiNoteName;
pub use quantize::{InputQuantize, QuantizeToPos, RecordPath};
pub use receive::{ReceiveMode, TrackReceive};
pub use record::{Hidden as RecordHidden, MonitorMode, RecordMode, RecordSettings};
pub use send_mode::SendMode;
pub use solo::{Hidden as SoloHidden, SoloMode};
pub use timebase::TrackTimebase;
