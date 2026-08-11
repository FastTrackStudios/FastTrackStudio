//! Names [`crate::backend`] and [`crate::scan`] agree on for a File
//! Root's own internals — never surfaced by
//! [`files_proto::FilesService::browse`] (root browsing), but visible
//! through [`files_proto::FilesService::drive_browse`] ("Drive"
//! browsing shows the raw tree, internals included — that's the
//! distinction the glossary draws between the two).

/// Marker file at a root's top level recording its stable id (ADR 0001
/// / glossary "File Root": "identified by a stable id in its entity
/// plus a marker file in the tree").
pub const MARKER_FILE: &str = ".fts-root.json";

/// Directory at a root's top level holding its version-store repo
/// (`task-files-version-store`'s jj repo + CAS chunk store).
pub const STORE_DIR: &str = ".fts-files";
