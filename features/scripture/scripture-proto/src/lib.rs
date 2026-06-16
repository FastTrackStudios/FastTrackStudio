//! Wasm-clean verse-addressing keystone for the Bible-study feature.
//!
//! Everything in the feature hangs on a stable way to name a verse, so
//! this proto crate owns that and nothing else. Three pieces:
//!
//! - [`Book`] — the 66-book Protestant canon as a compact ordinal
//!   newtype (`1..=66`), backed by a static table of `USFM` id, `OSIS`
//!   id, English name, and chapter count.
//! - [`VerseId`] — `(book, chapter, verse)` with two canonical keys:
//!   the `OSIS` string (`John.3.16`, the human / interchange form) and
//!   the sortable `BBCCCVVV` integer (`43003016`). Ordering is by the
//!   integer, so verses sort in canonical order for free.
//! - [`Translation`] — metadata + licensing posture for the editions we
//!   ship, encoding the bundle-vs-fetch decision from
//!   `plans/bible-study.md` directly in the type system.
//!
//! The text itself is **read-only**: notes, the wiki, and cross-refs
//! reference verses by [`VerseId`]; nothing edits the scripture spine.
//! The sibling `scripture` crate parses `USFM` into these ids and serves
//! an in-memory store.

pub mod book;
pub mod reference;
pub mod translation;

pub use book::Book;
pub use reference::{RefError, VerseId};
pub use translation::{Availability, Translation};
