//! Application services
//!
//! Services coordinate between core implementations and infrastructure.
//! They manage state and provide high-level operations.

pub mod command_service;
pub mod midi_service;
pub mod seek_service;
pub mod setlist_service;
pub mod smooth_seek_service;
pub mod stream_service;

pub use command_service::CommandService;
pub use midi_service::{
    AnalyzedChord, detect_chords_from_midi_track, find_track_by_name, get_first_midi_take,
    get_project_time_signature, read_midi_notes_from_take, MIN_CHORD_DURATION_PPQ,
};
pub use seek_service::SeekService;
pub use setlist_service::SetlistService;
pub use smooth_seek_service::SmoothSeekService;
pub use stream_service::StreamService;
