//! Path-prefix confinement — the grant term with teeth.
//!
//! A Storage grant's path prefix "is the org's own subtree on a shared
//! volume" (glossary "Storage grant"), which is only true if nothing an
//! org supplies can resolve outside it. Two layers, matching the
//! discipline `files`' `FilesBackend` applies to root paths (PR #280
//! review):
//!
//! 1. [`safe_relative`] — a textual gate on the *requested* path, run
//!    before anything is created: no absolute paths (`PathBuf::join`
//!    with an absolute argument silently replaces the base), no `..`,
//!    no root/prefix components, no empty segments.
//! 2. [`confine`] — a canonicalize-then-prefix-check on the *resolved*
//!    path, run after creation, which also resolves symlinks to their
//!    real location. A symlink planted inside the prefix pointing
//!    elsewhere is caught here, not by the textual gate.

use std::path::{Component, Path, PathBuf};

use crate::error::{Error, Result};

/// Validate a caller-supplied relative path and return it normalized.
/// Accepts ordinary nested paths (`clients/acme/mix-session`); rejects
/// anything that could resolve outside the prefix it will be joined to.
pub fn safe_relative(requested: &str) -> Result<PathBuf> {
    let requested = requested.trim();
    if requested.is_empty() {
        return Err(Error::BadRequest("path is empty".into()));
    }
    let path = Path::new(requested);
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(part) => normalized.push(part),
            Component::CurDir => {}
            Component::ParentDir => {
                return Err(Error::BadRequest(format!(
                    "{requested}: `..` may not appear in a path under a grant's prefix"
                )));
            }
            Component::RootDir | Component::Prefix(_) => {
                return Err(Error::BadRequest(format!(
                    "{requested}: must be relative to the grant's path prefix"
                )));
            }
        }
    }
    if normalized.as_os_str().is_empty() {
        return Err(Error::BadRequest(format!(
            "{requested}: resolves to the prefix itself"
        )));
    }
    Ok(normalized)
}

/// Canonicalize `target` and confirm it resolves inside `boundary`
/// (which must already exist and is canonicalized here too). Both paths
/// must exist — call this *after* creating the directory.
pub fn confine(target: &Path, boundary: &Path) -> Result<PathBuf> {
    let boundary = boundary.canonicalize().map_err(|e| {
        Error::Io(std::io::Error::other(format!(
            "{}: {e}",
            boundary.display()
        )))
    })?;
    let canonical = target
        .canonicalize()
        .map_err(|e| Error::Io(std::io::Error::other(format!("{}: {e}", target.display()))))?;
    if canonical != boundary && !canonical.starts_with(&boundary) {
        return Err(Error::BadRequest(format!(
            "{}: outside the grant's path prefix ({})",
            target.display(),
            boundary.display()
        )));
    }
    Ok(canonical)
}

/// A path as a UTF-8 string, or a `BadRequest` — every wire type in the
/// placement layer carries paths as `String`.
pub fn to_utf8(path: &Path) -> Result<String> {
    path.to_str()
        .map(str::to_string)
        .ok_or_else(|| Error::BadRequest(format!("{}: path is not valid UTF-8", path.display())))
}
