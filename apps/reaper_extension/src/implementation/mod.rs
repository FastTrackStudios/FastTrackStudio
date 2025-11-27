//! REAPER-specific implementations
//!
//! These modules provide REAPER-specific implementations of domain concepts.
//! Code can import directly from submodules (e.g., `use crate::implementation::setlist::build_setlist_from_open_projects`)
//! or use the convenience re-exports below.

pub mod markers;
pub mod project;
pub mod setlist;
pub mod tracks;
pub mod transport;

// Convenience re-exports (code can also import directly from submodules)
#[allow(unused_imports)]
pub use markers::{read_markers_from_project, read_regions_from_project};
#[allow(unused_imports)]
pub use project::create_reaper_project_wrapper;
#[allow(unused_imports)]
pub use setlist::{build_setlist_from_open_projects, build_song_from_current_project};
#[allow(unused_imports)]
pub use tracks::{get_all_tracks, get_track, get_track_summaries, invalidate_track_cache, invalidate_all_track_caches};
#[allow(unused_imports)]
pub use transport::ReaperTransport;

