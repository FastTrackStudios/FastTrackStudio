//! Track API and supporting types

pub mod folder;
pub mod automation;
pub mod free_mode;
pub mod fixed_lanes;
pub mod record;
pub mod receive;
pub mod hardware;
pub mod solo;
pub mod collapse;
pub mod quantize;
pub mod send_mode;
pub mod timebase;
pub mod midi_note_name;

pub use folder::{TcpFolderState, McpFolderState};
pub use automation::AutomationMode;
pub use free_mode::FreeMode;
pub use fixed_lanes::{FixedLanesSettings, LaneSoloSettings, LaneRecordSettings, LaneNameSettings};
pub use record::{RecordSettings, RecordMode, MonitorMode, Hidden as RecordHidden};
pub use receive::{TrackReceive, ReceiveMode};
pub use hardware::{HardwareOutputSettings, MidiOutputSettings, MasterSendSettings};
pub use solo::{SoloMode, Hidden as SoloHidden};
pub use collapse::{ArrangeCollapseState, MixerCollapseState, WiringCollapseState, BusCompactSettings, Hidden as CollapseHidden};
pub use quantize::{QuantizeToPos, RecordPath, InputQuantize};
pub use timebase::TrackTimebase;
pub use midi_note_name::MidiNoteName;
pub use send_mode::SendMode;
