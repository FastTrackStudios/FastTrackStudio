//! Application services
//!
//! Services coordinate between core implementations and infrastructure.
//! They manage state and provide high-level operations.

pub mod command_service;
pub mod midi_service;
pub mod reaper_transport;
pub mod seek_service;
pub mod setlist_service;
pub mod smooth_seek_service;
pub mod stream_service;

pub use command_service::CommandService;
pub use midi_service::{
    AnalyzedChord, MIN_CHORD_DURATION_PPQ, build_midi_file_from_reaper,
    detect_chords_from_midi_track, find_track_by_name, generate_chart_text_from_reaper,
    get_beats_at_time, get_first_midi_take, get_internal_measure_at_time, get_measure_at_time,
    get_project_time_signature, read_midi_notes_from_take,
};
pub use reaper_transport::ReaperTransport;
pub use seek_service::SeekService;
pub use setlist_service::SetlistService;
pub use smooth_seek_service::SmoothSeekService;
pub use stream_service::StreamService;
