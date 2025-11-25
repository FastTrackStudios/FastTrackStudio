pub mod layout;
pub mod transport;
pub mod progress;
pub mod song;
pub mod sidebar_items;
pub mod mode_toggle;
pub mod lyrics;
pub mod text_fit;

// Re-export commonly used components
pub use layout::{TopBar, Sidebar, MainContent};
pub use transport::{TransportControlBar, ConnectionStatus};
pub use progress::{SongProgressBar, SectionProgressBar, SegmentedProgressBar, CompactProgressBar, ProgressSection, TempoMarker};
pub use song::{SongTitle, FadedSongTitle};
pub use mode_toggle::ModeToggle;
pub use lyrics::LyricsView;
pub use text_fit::{TextFit, TextFitMode, TextFitProps};

