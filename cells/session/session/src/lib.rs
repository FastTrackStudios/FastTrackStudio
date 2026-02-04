//! Session Services Library
//!
//! This crate provides session management services for FTS Control.
//! Services can be used either:
//! - As a cell (via `main.rs` and `run_cell!` macro)
//! - In-process (by importing and instantiating directly)
//!
//! # In-Process Usage
//!
//! ```rust,ignore
//! use session::{SetlistServiceImpl, SongServiceImpl};
//! use session_proto::{SetlistServiceDispatcher, SongServiceDispatcher};
//!
//! // Create services
//! let setlist = SetlistServiceImpl::new();
//! let song = SongServiceImpl::new();
//!
//! // Create dispatchers for RPC
//! let setlist_dispatcher = SetlistServiceDispatcher::new(setlist);
//! let song_dispatcher = SongServiceDispatcher::new(song);
//! ```

mod setlist_builder;
mod setlist_service;
mod song_builder;
mod song_service;

// Re-export service implementations for library use
pub use setlist_service::SetlistServiceImpl;
pub use song_service::SongServiceImpl;

// Re-export builders for advanced use cases
pub use setlist_builder::SetlistBuilder;
pub use song_builder::SongBuilder;
