//! The chart corpus — the list of songs the analyzer measures against.
//!
//! This crate answers one question: *which records are we studying, and
//! how did each of them chart?* It does not download audio, separate
//! stems, or measure anything; it produces the spine that those stages
//! hang off.
//!
//! ## Shape of the work
//!
//! ```text
//!   Hot 100 JSON archive ──┐
//!                          ├──► ChartEntry ──► dedupe ──► song
//!   genre chart pages ─────┘      (rank,        (norm)      │
//!                                  title,                   │
//!                                  artist)                  ▼
//!                                                     corpus.sqlite
//! ```
//!
//! Two acquisition paths because Billboard publishes the Hot 100 and
//! its genre charts differently — see [`hot100`] and [`billboard`].
//! Both converge on [`chart::ChartEntry`], which [`db::Store`] folds
//! into song identities using [`norm`].
//!
//! ## Why genre comes from charts
//!
//! Genre here is *chart membership*, not a label from a tagging
//! service: a song is country because it charted on Hot Country Songs.
//! That makes the genre split reproducible, and it lets a song carry
//! several genres at once, which is common and true and would be lost
//! by forcing a single label.
//!
//! ## What is derived rather than stored
//!
//! Peak position and weeks-on-chart are computed from the weekly
//! observations by the `song_stats` view rather than recorded at ingest.
//! The Hot 100 archive publishes both, but the genre charts would have
//! to have them scraped positionally out of unlabelled markup — so
//! deriving them keeps every chart reporting the same way, and keeps
//! them correct when a partial ingest is later completed.

pub mod acquire;
pub mod billboard;
pub mod chart;
pub mod db;
pub mod hot100;
pub mod norm;

pub use chart::{Chart, ChartEntry};
pub use db::Store;
