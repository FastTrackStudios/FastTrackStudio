pub mod layout;
pub mod transport;
pub mod progress;
pub mod song;
pub mod sidebar_items;

// Re-export commonly used components
pub use layout::{TopBar, Sidebar, MainContent};
pub use transport::{TransportControlBar, ConnectionStatus};
pub use progress::{SongProgressBar, SegmentedProgressBar, CompactProgressBar, ProgressSection};
pub use song::SongTitle;

