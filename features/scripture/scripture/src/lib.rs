//! `scripture` — read-only Bible spine: USFM ingest + in-memory store.
//!
//! This native crate sits on top of the wasm-clean `scripture_proto`
//! addressing layer (the same split as `locations` over
//! `locations-proto`). It turns USFM source into [`Verse`]s keyed by
//! [`scripture_proto::VerseId`] and serves them from a [`Bible`].
//!
//! Surface:
//! - [`parse_book`] — USFM source → `Vec<Verse>` of clean verse text.
//! - [`Bible`] — an in-memory, ordered store: insert books, then
//!   `get` / iterate verses and chapters by id.
//! - [`Bible::web_sample`] — the bundled WEB Gospel of John (public
//!   domain), the slice-1 proof that a verse resolves end-to-end.
//!
//! The text is never mutated by a user action; later layers (notes,
//! wiki, cross-refs) link in by [`scripture_proto::VerseId`].

#![cfg(not(target_arch = "wasm32"))]

pub mod bible;
pub mod usfm;

pub use bible::Bible;
pub use scripture_proto::{Availability, Book, RefError, Translation, VerseId};
pub use usfm::{UsfmError, Verse, parse_book};
