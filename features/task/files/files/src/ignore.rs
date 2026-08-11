//! A File Root's **Ignore set** (glossary: "the per-root list of
//! patterns that are neither versioned nor synced (backup files, peak
//! caches). Seeded by root flavor, edited per root; versioning and
//! selective sync share it").
//!
//! One set, one file: `<root>/.fts-files/ignore.json`. It lives inside
//! the root's own store directory rather than in the server's registry
//! precisely because selective sync must read the same list on a
//! replica that never saw the server's registry — "shared verbatim" is
//! a storage decision, not just an API one.
//!
//! Matching is deliberately two-sided: every pattern is matched against
//! the full root-relative path *and* against the basename alone, so the
//! everyday case (`*.rpp-bak` — REAPER's backup storm, the El Artisa
//! fixture's shape) catches one at any depth without the author having
//! to write `**/*.rpp-bak`. A pattern ending in `/` is a directory
//! prefix (`peaks/` ignores everything under any `peaks` directory).

use std::path::Path;
use std::sync::LazyLock;

use files_proto::RootFlavor;
use globset::{Glob, GlobSet, GlobSetBuilder};

use crate::error::{Error, Result};

/// Ignore patterns seeded into a new **media** root (ADR 0001's default
/// flavor): DAW backup files and the peak/analysis caches every editor
/// regenerates on demand. Versioning these would fill the store with
/// scaffolding that is, by construction, derivable from the work.
pub const MEDIA_SEED: &[&str] = &[
    // REAPER: `.rpp-bak` on every save (the El Artisa fixture's storm),
    // plus the peak caches it rebuilds from the audio itself.
    "*.rpp-bak",
    "*.RPP-bak",
    "*.reapeaks",
    "*.reapindex",
    // Other DAWs' regenerable analysis/peak sidecars.
    "*.asd",  // Ableton Live
    "*.pkf",  // Pro Tools
    "*.sfk",  // Sound Forge
    "*.wfm",  // Cubase/Nuendo
    "*.peak", // generic
    // Editors' scratch + OS junk.
    "*.tmp",
    "*~",
    ".DS_Store",
    "Thumbs.db",
];

/// Ignore patterns seeded into a new **software** root. Software roots
/// are colocated git (ADR 0001) and are not yet creatable through
/// [`crate::FilesBackend`] — the seed exists so the flavor axis is real
/// in one place rather than invented later at two call sites.
pub const SOFTWARE_SEED: &[&str] = &["target/", "node_modules/", ".DS_Store", "*.tmp"];

/// Filename globs whose write counts as a **project-file save** — the
/// event that marks a save point (glossary "Auto-snapshot"). A project
/// file is the DAW/NLE's own session document: the thing a producer
/// means by "I saved".
pub const MEDIA_PROJECT_FILES: &[&str] = &[
    "*.rpp",
    "*.RPP",
    "*.daw",  // .daw project format (issue #155)
    "*.als",  // Ableton Live
    "*.drp",  // DaVinci Resolve
    "*.ptx",  // Pro Tools
    "*.cpr",  // Cubase
    "*.flp",  // FL Studio
    "*.song", // Studio One / Reason
    "*.logicx",
    "*.dawproject",
];

/// [`MEDIA_PROJECT_FILES`], compiled once. `is_project_file` runs per
/// surviving hint path, and hints arrive per watcher event during
/// exactly the write storms this engine exists for — recompiling
/// eleven globs on each one is work with no purpose.
static MEDIA_PROJECT_GLOBS: LazyLock<GlobSet> = LazyLock::new(|| {
    let mut builder = GlobSetBuilder::new();
    for pattern in MEDIA_PROJECT_FILES {
        builder.add(Glob::new(pattern).expect("project-file patterns are valid globs"));
    }
    builder.build().expect("project-file glob set builds")
});

/// A compiled Ignore set: the patterns as authored (round-tripped over
/// RPC verbatim) plus the [`GlobSet`]s they compile to.
#[derive(Debug, Clone)]
pub struct IgnoreSet {
    patterns: Vec<String>,
    full: GlobSet,
    basename: GlobSet,
    dirs: Vec<String>,
}

impl IgnoreSet {
    /// The seed for a freshly created root of `flavor`.
    pub fn seed(flavor: RootFlavor) -> Self {
        let seed = match flavor {
            RootFlavor::Media => MEDIA_SEED,
            RootFlavor::Software => SOFTWARE_SEED,
        };
        Self::compile(seed.iter().map(|s| (*s).to_string()).collect())
            .expect("seed patterns are valid globs")
    }

    /// An empty set — ignores nothing.
    pub fn empty() -> Self {
        Self::compile(Vec::new()).expect("the empty set compiles")
    }

    /// Compile `patterns`, normalizing them first (trim, drop blanks,
    /// deduplicate, sort) so the stored list is stable no matter what
    /// order a caller sent.
    pub fn compile(patterns: Vec<String>) -> Result<Self> {
        let mut normalized: Vec<String> = patterns
            .into_iter()
            .map(|p| p.trim().to_string())
            .filter(|p| !p.is_empty())
            .collect();
        normalized.sort();
        normalized.dedup();

        let mut full = GlobSetBuilder::new();
        let mut basename = GlobSetBuilder::new();
        let mut dirs = Vec::new();
        for pattern in &normalized {
            if let Some(dir) = pattern.strip_suffix('/') {
                if dir.is_empty() {
                    return Err(Error::BadRequest("\"/\" is not an ignore pattern".into()));
                }
                dirs.push(dir.to_string());
                continue;
            }
            let glob = Glob::new(pattern)
                .map_err(|e| Error::BadRequest(format!("bad ignore pattern {pattern:?}: {e}")))?;
            full.add(glob.clone());
            // A pattern with no separator also matches a bare basename
            // at any depth (see the module doc); one that already spells
            // out a path shape is left to the full-path matcher alone.
            if !pattern.contains('/') {
                basename.add(glob);
            }
        }
        Ok(Self {
            patterns: normalized,
            full: full
                .build()
                .map_err(|e| Error::BadRequest(format!("ignore set: {e}")))?,
            basename: basename
                .build()
                .map_err(|e| Error::BadRequest(format!("ignore set: {e}")))?,
            dirs,
        })
    }

    /// The patterns as stored, normalized.
    #[must_use]
    pub fn patterns(&self) -> &[String] {
        &self.patterns
    }

    /// Is `rel_path` (a root-relative, `/`-separated path) ignored?
    /// Directory patterns match the path's own components, so a
    /// `peaks/` pattern hides everything beneath any `peaks` directory
    /// without needing a second `peaks/**` entry.
    #[must_use]
    pub fn is_ignored(&self, rel_path: &str) -> bool {
        if self.full.is_match(rel_path) {
            return true;
        }
        let mut components: Vec<&str> = rel_path.split('/').collect();
        // The last component is the entry itself, never a parent dir.
        let name = components.pop().unwrap_or(rel_path);
        if self.basename.is_match(name) {
            return true;
        }
        self.dirs.iter().any(|dir| {
            if dir.contains('/') {
                // A spelled-out subtree (`Media/peaks/`): a plain
                // prefix match on the path.
                rel_path.starts_with(&format!("{dir}/"))
            } else {
                components.contains(&dir.as_str())
            }
        })
    }

    /// Is the *directory* at `rel_path` ignored? Same as
    /// [`IgnoreSet::is_ignored`] plus the directory patterns matched
    /// against the directory's own name, so a `peaks/` pattern lets a
    /// scan skip descending into `peaks` at all rather than walking it
    /// and discarding every file inside.
    #[must_use]
    pub fn is_ignored_dir(&self, rel_path: &str) -> bool {
        if self.is_ignored(rel_path) {
            return true;
        }
        self.dirs
            .iter()
            .any(|dir| rel_path == dir || rel_path.ends_with(&format!("/{dir}")))
    }

    /// Does `rel_path` name a project file for `flavor` — i.e. does
    /// writing it count as a save point?
    #[must_use]
    pub fn is_project_file(rel_path: &str, flavor: RootFlavor) -> bool {
        let name = rel_path.rsplit('/').next().unwrap_or(rel_path);
        match flavor {
            RootFlavor::Media => MEDIA_PROJECT_GLOBS.is_match(name),
            // Software roots have no single session document.
            RootFlavor::Software => false,
        }
    }

    /// Load the set stored for the root whose store directory is
    /// `store_dir`, falling back to `flavor`'s seed (and writing it)
    /// when the root predates the Ignore set or has never had one.
    pub fn load_or_seed(store_dir: &Path, flavor: RootFlavor) -> Result<Self> {
        let path = store_dir.join(IGNORE_FILE);
        if path.exists() {
            let bytes = std::fs::read(&path)?;
            let patterns: Vec<String> = serde_json::from_slice(&bytes)?;
            return Self::compile(patterns);
        }
        let set = Self::seed(flavor);
        set.save(store_dir)?;
        Ok(set)
    }

    /// Persist this set into `store_dir`.
    pub fn save(&self, store_dir: &Path) -> Result<()> {
        std::fs::create_dir_all(store_dir)?;
        std::fs::write(
            store_dir.join(IGNORE_FILE),
            serde_json::to_vec_pretty(&self.patterns)?,
        )?;
        Ok(())
    }
}

/// Filename of the per-root Ignore set inside the root's store dir.
pub const IGNORE_FILE: &str = "ignore.json";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn media_seed_catches_reaper_backup_storm_at_any_depth() {
        let set = IgnoreSet::seed(RootFlavor::Media);
        assert!(set.is_ignored("El Artisa.rpp-bak"));
        assert!(set.is_ignored("sessions/El Artisa.rpp-bak"));
        assert!(set.is_ignored("Audio Files/kick.reapeaks"));
        assert!(!set.is_ignored("El Artisa.rpp"));
        assert!(!set.is_ignored("Audio Files/kick.wav"));
    }

    #[test]
    fn directory_patterns_cover_their_whole_subtree() {
        let set = IgnoreSet::compile(vec!["peaks/".into()]).unwrap();
        assert!(set.is_ignored("peaks/kick.dat"));
        assert!(set.is_ignored("stems/peaks/kick.dat"));
        assert!(!set.is_ignored("peaks.wav"));
    }

    #[test]
    fn patterns_are_normalized_and_bad_globs_rejected() {
        let set = IgnoreSet::compile(vec!["  *.tmp ".into(), "*.tmp".into(), "*.bak".into()])
            .expect("valid globs");
        assert_eq!(set.patterns(), ["*.bak", "*.tmp"]);
        assert!(IgnoreSet::compile(vec!["[".into()]).is_err());
    }

    #[test]
    fn project_files_are_flavor_scoped() {
        assert!(IgnoreSet::is_project_file(
            "El Artisa.rpp",
            RootFlavor::Media
        ));
        assert!(IgnoreSet::is_project_file(
            "sessions/Cut 3.drp",
            RootFlavor::Media
        ));
        assert!(!IgnoreSet::is_project_file("mix.wav", RootFlavor::Media));
        assert!(!IgnoreSet::is_project_file(
            "El Artisa.rpp",
            RootFlavor::Software
        ));
    }
}
