//! A browsable, human-readable view of the corpus.
//!
//! Files are stored under numeric ids — `audio/12108/x_XS5q0EUQs.webm`,
//! `stems/12108/vocals.opus` — because an id is a stable key that
//! survives retitling, contains nothing a filesystem objects to, and can
//! never collide. That is right for storage and useless for browsing.
//!
//! ## Why links rather than renaming
//!
//! Renaming the real files would be lossy and risky:
//!
//! - 2.6% of chart titles contain characters a filesystem forbids
//!   (`?`, `"`, `:`, `/`), so those names would be permanently mangled —
//!   and a question mark is often the whole joke of a title.
//! - Every `path` already recorded in the database would have to be
//!   rewritten in the same breath, or the corpus silently loses track of
//!   its own audio.
//! - It moves 14k files to gain something a symlink gives for free.
//!
//! Links cost nothing, break nothing, and can be regenerated with a
//! different scheme whenever the scheme turns out to be wrong. The ids
//! stay the truth; this is a view over them.
//!
//! Every link name carries its `song_id`, so names are unique by
//! construction rather than by luck — the corpus measures no collisions
//! today, but it grows.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

/// Characters a filesystem will not take, plus control codes.
const FORBIDDEN: &[char] = &['/', '\\', ':', '*', '?', '"', '<', '>', '|'];

/// How the browsable tree is grouped above the song directories.
///
/// The corpus spans 35 years and the interesting questions are about
/// change over that span — vocals measurably compress harder now than
/// they did in 1990 — so the default puts the era in the path. A pass
/// that wants "every 1994 vocal" then globs a directory instead of
/// querying, and a listing is already sorted chronologically.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, clap::ValueEnum)]
pub enum GroupBy {
    /// `1994/Artist - Title [id]/`
    #[default]
    Year,
    /// `1990s/Artist - Title [id]/`
    Decade,
    /// No grouping — every song directory at the top level.
    Flat,
}

impl GroupBy {
    /// The directory a song of this year belongs under, if any.
    ///
    /// Grouped by the year it FIRST charted, not by release year: this
    /// corpus is about what was popular when, and a song that re-charts
    /// years later (as Christmas records do every December) still
    /// belongs to the moment it broke.
    pub fn dir_for(self, first_year: i64) -> Option<String> {
        match self {
            GroupBy::Year => Some(first_year.to_string()),
            GroupBy::Decade => Some(format!("{}s", first_year - first_year.rem_euclid(10))),
            GroupBy::Flat => None,
        }
    }
}

/// Fixed filenames inside a song's directory.
///
/// Constant across every song on purpose: a script walking the tree
/// opens `vocals.opus` without consulting the database, and a song whose
/// stems are missing is simply a directory that lacks them. The source
/// keeps its own extension because it varies with what was served.
pub const VOCALS_FILE: &str = "vocals.opus";
pub const INSTRUMENTAL_FILE: &str = "instrumental.opus";

/// A filesystem-safe `Artist - Title [id]` directory for one song.
///
/// One directory per song, holding the source and both stems together,
/// so analysis walks the tree song by song and finds everything for a
/// track in one place.
pub fn song_dir(song_id: i64, title: &str, artist: &str) -> String {
    format!("{} [{song_id}]", sanitize(&format!("{artist} - {title}")))
}

/// A filesystem-safe `Artist - Title [id].ext` for one file.
pub fn link_name(song_id: i64, title: &str, artist: &str, ext: &str) -> String {
    format!("{} [{song_id}].{ext}", sanitize(&format!("{artist} - {title}")))
}

/// Strip what a filesystem will not take, collapse whitespace, and keep
/// the result short enough to sit inside any path limit.
fn sanitize(raw: &str) -> String {
    let mut base = raw.to_string();
    base = base
        .chars()
        .map(|c| {
            if FORBIDDEN.contains(&c) || c.is_control() {
                '_'
            } else {
                c
            }
        })
        .collect();
    base = base.split_whitespace().collect::<Vec<_>>().join(" ");

    // Leave room for the id and extension.
    const MAX_BASE: usize = 120;
    if base.chars().count() > MAX_BASE {
        base = base.chars().take(MAX_BASE).collect();
    }
    let base = base.trim_end_matches(['.', ' ']).to_string();
    if base.is_empty() {
        "untitled".to_string()
    } else {
        base
    }
}

/// The extension of a stored file, defaulting sensibly.
pub fn extension_of(path: &Path) -> String {
    path.extension()
        .and_then(|e| e.to_str())
        .unwrap_or("bin")
        .to_string()
}

/// Point `link` at `target`, replacing any existing link.
///
/// The link is written **relative** to its own directory so the whole
/// corpus can be moved or mounted elsewhere without every link dangling.
pub fn relink(target: &Path, link: &Path) -> Result<()> {
    if let Some(parent) = link.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("creating {}", parent.display()))?;
    }
    // symlink_metadata, not exists(): a dangling link still needs
    // replacing, and `exists()` follows the link and reports false.
    if std::fs::symlink_metadata(link).is_ok() {
        std::fs::remove_file(link).with_context(|| format!("replacing {}", link.display()))?;
    }
    let rel = relative_to(target, link.parent().unwrap_or(Path::new(".")));
    std::os::unix::fs::symlink(&rel, link)
        .with_context(|| format!("linking {} -> {}", link.display(), rel.display()))?;
    Ok(())
}

/// Express `target` relative to `from`.
fn relative_to(target: &Path, from: &Path) -> PathBuf {
    let t: Vec<_> = target.components().collect();
    let f: Vec<_> = from.components().collect();
    let shared = t.iter().zip(&f).take_while(|(a, b)| a == b).count();
    let mut out = PathBuf::new();
    for _ in shared..f.len() {
        out.push("..");
    }
    for c in &t[shared..] {
        out.push(c.as_os_str());
    }
    if out.as_os_str().is_empty() {
        target.to_path_buf()
    } else {
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forbidden_characters_are_replaced_not_dropped() {
        // Real corpus titles. A dropped '?' silently merges two
        // different songs in a listing; a replaced one does not.
        let n = link_name(1, "What Kind Of Man Would I Be?", "Chicago", "opus");
        assert_eq!(n, "Chicago - What Kind Of Man Would I Be_ [1].opus");

        let n = link_name(2, "It Must Have Been Love (From \"Pretty Woman\")", "Roxette", "webm");
        assert!(!n.contains('"'), "{n}");
        assert!(n.contains("(From _Pretty Woman_)"), "{n}");
    }

    #[test]
    fn a_slash_in_a_title_cannot_create_a_directory() {
        let n = link_name(3, "Hot R&B/Hip-Hop", "Someone", "opus");
        assert!(!n.contains('/'), "{n}");
    }

    #[test]
    fn the_id_makes_names_unique_even_when_titles_match() {
        let a = link_name(10, "Hello", "An Artist", "opus");
        let b = link_name(11, "Hello", "An Artist", "opus");
        assert_ne!(a, b);
        assert!(a.contains("[10]") && b.contains("[11]"));
    }

    #[test]
    fn long_titles_are_truncated_within_path_limits() {
        let n = link_name(4, &"x".repeat(400), &"y".repeat(400), "opus");
        assert!(n.len() < 160, "still {} chars", n.len());
        assert!(n.ends_with("].opus"), "{n}");
    }

    #[test]
    fn names_never_end_in_a_dot_or_space() {
        let n = link_name(5, "Title.", "Artist", "opus");
        // The extension is the only thing after the id.
        assert!(n.ends_with("[5].opus"), "{n}");
        assert!(!n.contains(". ["), "{n}");
    }

    #[test]
    fn links_are_relative_so_the_corpus_can_move() {
        let rel = relative_to(
            Path::new("/corpus/stems/12108/vocals.opus"),
            Path::new("/corpus/by-name/Glass Animals - Heat Waves [12108]"),
        );
        assert_eq!(rel, PathBuf::from("../../stems/12108/vocals.opus"));
    }

    #[test]
    fn a_song_directory_has_no_extension() {
        let d = song_dir(12108, "Heat Waves", "Glass Animals");
        assert_eq!(d, "Glass Animals - Heat Waves [12108]");
        assert!(!d.contains('.'), "{d}");
    }

    #[test]
    fn song_directories_are_sanitized_like_filenames() {
        let d = song_dir(1, "Hot R&B/Hip-Hop?", "Someone");
        assert!(!d.contains('/'), "{d}");
        assert!(!d.contains('?'), "{d}");
        assert!(d.ends_with("[1]"), "{d}");
    }

    #[test]
    fn years_group_verbatim_and_decades_round_down() {
        assert_eq!(GroupBy::Year.dir_for(1994).as_deref(), Some("1994"));
        assert_eq!(GroupBy::Decade.dir_for(1994).as_deref(), Some("1990s"));
        assert_eq!(GroupBy::Decade.dir_for(1990).as_deref(), Some("1990s"));
        // A decade boundary must not fall into the previous one.
        assert_eq!(GroupBy::Decade.dir_for(1999).as_deref(), Some("1990s"));
        assert_eq!(GroupBy::Decade.dir_for(2000).as_deref(), Some("2000s"));
        assert_eq!(GroupBy::Decade.dir_for(2025).as_deref(), Some("2020s"));
        assert_eq!(GroupBy::Flat.dir_for(1994), None);
    }

    /// The inner names are a contract with anything walking the tree.
    #[test]
    fn stem_filenames_are_fixed() {
        assert_eq!(VOCALS_FILE, "vocals.opus");
        assert_eq!(INSTRUMENTAL_FILE, "instrumental.opus");
    }

    #[test]
    fn extension_survives_whatever_was_downloaded() {
        assert_eq!(extension_of(Path::new("/a/b.webm")), "webm");
        assert_eq!(extension_of(Path::new("/a/b.m4a")), "m4a");
        assert_eq!(extension_of(Path::new("/a/b")), "bin");
    }
}
