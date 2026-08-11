//! The per-root **Ignore set** (issue #273's fourth acceptance criterion,
//! spec: "One Ignore set per root, seeded by flavor, shared verbatim by
//! versioning and selective sync").
//!
//! Two halves, both gitignore-syntax so one matcher serves both:
//!
//! 1. A **flavor seed** — patterns every root of that flavor starts with.
//!    ADR 0001's software-root doctrine is "software roots keep real git
//!    and ignore-pattern stray heavy files", so [`RootFlavor::Software`]'s
//!    seed is build scaffolding plus the heavy media formats that belong in
//!    a media root rather than a git object database. [`RootFlavor::Media`]
//!    seeds empty: a media root's ignore set (`.rpp-bak` storms, peak
//!    caches) is the cadence engine's own ticket (#260), and seeding it
//!    here would change media behavior out from under that work. The seam
//!    it needs is this function.
//! 2. The root's own **`.gitignore` files**, honored on software roots
//!    only ([`honors_gitignore`]). This is what keeps the flavor's promise
//!    that git tooling and Files agree on what is content: a repo's
//!    committed `.gitignore` is already the project's own statement of what
//!    is scaffolding, and Files versioning the files git deliberately
//!    ignores would make `git status` a lie.
//!
//! Matching is jj-lib's [`GitIgnoreFile`] — real gitignore semantics
//! (negation, anchoring, directory patterns, per-directory chaining), the
//! same matcher jj's own working-copy snapshotting uses, so a software
//! root's exclusions are exactly what a git user expects.
//!
//! **Ignored never means deleted.** An ignored path that is *already
//! tracked* in the root's history stays tracked — git's own rule (ignore
//! applies to untracked files) and the only safe one here: a checkpoint
//! must never record a deletion just because a pattern started matching.
//! [`crate::checkpoint`] enforces that; see [`crate::scan::LiveFile`].

use std::path::Path;
use std::sync::Arc;

use files_proto::RootFlavor;
use jj_lib::gitignore::GitIgnoreFile;
use jj_lib::repo_path::RepoPath;

use crate::error::{Error, Result};

/// Software roots: build scaffolding + the heavy formats ADR 0001 says
/// belong in a media root. Deliberately narrow — a repo's own
/// `.gitignore` (honored on top of this) is the authority on anything
/// project-specific, and a seed that guessed too much would silently drop
/// files a developer meant to commit.
const SOFTWARE_SEED: &str = "\
# Files' software-root seed (ADR 0001). A root's own .gitignore is layered
# on top of this and can re-include anything here with a `!` rule.
/.git/
target/
node_modules/
.DS_Store
# Heavy stray media: belongs in a media File Root, not a git object store.
*.wav
*.aif
*.aiff
*.flac
*.mov
*.mp4
*.mkv
*.mxf
*.r3d
*.braw
*.iso
*.dmg
";

/// The flavor's built-in patterns, as an ignore file rooted at the root's
/// top level. Media seeds empty (see the module doc).
pub fn seed(flavor: RootFlavor) -> Result<Arc<GitIgnoreFile>> {
    let empty = GitIgnoreFile::empty();
    match flavor {
        RootFlavor::Media => Ok(empty),
        RootFlavor::Software => empty
            .chain(
                RepoPath::root(),
                Path::new("<software-root-seed>"),
                SOFTWARE_SEED.as_bytes(),
            )
            .map_err(|e| Error::Repo(format!("building the software ignore seed: {e}"))),
    }
}

/// Whether a root of this flavor layers the tree's own `.gitignore` files
/// on top of [`seed`].
#[must_use]
pub fn honors_gitignore(flavor: RootFlavor) -> bool {
    matches!(flavor, RootFlavor::Software)
}
