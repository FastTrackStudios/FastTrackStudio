//! Names every part of the Files platform agrees on for a File Root's
//! own internals. They live in the proto (rather than in one backend
//! crate) because more than one implementation has to agree on them: the
//! `files` backend hides them from root browsing, and a Storage agent
//! hosting a root's live tree creates [`STORE_DIR`] when it initializes
//! the authoritative repo (issue #262).

/// Marker file at a root's top level recording its stable id (ADR 0001 /
/// glossary "File Root": "identified by a stable id in its entity plus a
/// marker file in the tree").
pub const MARKER_FILE: &str = ".fts-root.json";

/// Directory at a root's top level holding its version-store repo
/// (`task-files-version-store`'s jj repo + CAS chunk store).
pub const STORE_DIR: &str = ".fts-files";
