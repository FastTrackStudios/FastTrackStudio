//! `scripture` — read-only Bible spine: USFM ingest + in-memory store.
//!
//! This native crate sits on top of the wasm-clean `scripture_proto`
//! addressing layer (the same split as `locations` over
//! `locations-proto`). It turns USFM source into [`Verse`]s keyed by
//! [`scripture_proto::VerseId`] and serves them from a [`Bible`].
//!
//! Surface:
//! - [`parse_book`] — USFM source → `Vec<Verse>` of clean verse text.
//! - [`install_usfm_dir`] — normalize a source USFM dir into a clean
//!   translation folder in the resource library.
//! - [`Bible`] — an in-memory, ordered store loaded from a translation
//!   folder ([`Bible::load_dir`]); `get` / iterate verses and chapters.
//!
//! The corpus lives in the resource library on disk (e.g.
//! `<org>/resources/bible/WEB/`), not in the repo. The text is never
//! mutated by a user action; later layers (notes, wiki, cross-refs) link
//! in by [`scripture_proto::VerseId`].

#![cfg(not(target_arch = "wasm32"))]

pub mod bible;
pub mod install;
pub mod usfm;

pub use bible::{Bible, LoadError};
pub use install::{InstallError, install_usfm_dir};
pub use scripture_proto::{Availability, Book, RefError, Translation, VerseId};
pub use usfm::{UsfmError, Verse, parse_book};
