//! Application services
//!
//! Services coordinate between core implementations and infrastructure.
//! They manage state and provide high-level operations.

pub mod setlist_service;
pub mod command_service;
pub mod seek_service;
pub mod stream_service;
pub mod smooth_seek_service;

pub use setlist_service::SetlistService;
pub use command_service::CommandService;
pub use seek_service::SeekService;
pub use stream_service::StreamService;
pub use smooth_seek_service::SmoothSeekService;

