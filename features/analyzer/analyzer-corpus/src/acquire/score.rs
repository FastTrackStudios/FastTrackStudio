//! Choosing which upload is *the hit*.
//!
//! This is the quality-critical decision in the whole corpus, and the
//! easiest place to silently ruin it — so it lives here as a pure
//! function over already-fetched metadata, with the reasoning recorded
//! alongside the verdict.
//!
//! ## What we are looking for
//!
//! The recording that charted: the original studio master. Not the live
//! cut, not the artist's later re-recording, not a remix, not a cover,
//! and emphatically not a reaction video.
//!
//! ## Why this needs to be strict
//!
//! Measured against plain YouTube search for a real 1995 Hot 100 #1,
//! twelve results contained: five reaction videos, a karaoke track, a
//! play-along, a movie clip, a lyric video, a cover, a live version, and
//! a 2022 re-recording — and no original master. The two *plausible*
//! results were both wrong in ways that would pass a naive title match
//! while completely changing the vocal being measured. A re-recording is
//! a different performance through thirty years of different gear; a
//! live cut is a different mix entirely.
//!
//! So the default is to reject. A song with no confident match is worth
//! far more as a gap in the corpus than as a plausible-looking wrong
//! number, because a wrong number is invisible once it is averaged in.

use crate::norm;

/// What we are trying to find.
#[derive(Debug, Clone)]
pub struct Target {
    pub title: String,
    pub artist: String,
    /// The year the song first charted — the original master should be
    /// released around then, which is what separates it from a later
    /// re-recording.
    pub chart_year: i32,
}

/// One upload being considered, as yt-dlp reports it.
#[derive(Debug, Clone, Default, serde::Deserialize)]
pub struct Candidate {
    pub id: String,
    #[serde(default)]
    pub title: String,
    #[serde(default)]
    pub channel: Option<String>,
    #[serde(default)]
    pub duration: Option<f64>,
    /// Populated for YouTube Music "art tracks" — the label's official
    /// audio — and absent for ordinary uploads, which is itself a
    /// useful signal.
    #[serde(default)]
    pub artist: Option<String>,
    #[serde(default)]
    pub track: Option<String>,
    #[serde(default)]
    pub album: Option<String>,
    #[serde(default)]
    pub release_year: Option<i32>,
}

/// The verdict on one candidate.
#[derive(Debug, Clone)]
pub struct Score {
    pub value: f64,
    /// Human-readable trace of what moved the number, stored in the
    /// database so a bad corpus can be audited rather than re-derived.
    pub reason: String,
}

/// Score at or above which a candidate is accepted automatically.
pub const ACCEPT: f64 = 6.0;

/// Minimum share of the charted title's words the candidate must carry.
/// Below this it is a different song, however good everything else looks.
pub const TITLE_FLOOR: f64 = 0.6;

/// Minimum share of the charted artist's words the candidate must carry.
/// Below this it is somebody else's performance — a cover.
pub const ARTIST_FLOOR: f64 = 0.5;

/// Markers that mean "this is not the recording that charted".
///
/// The weights are deliberately large: crossing one of these is not a
/// small demerit, it means the candidate is a different performance.
const VERSION_PENALTIES: &[(&str, f64)] = &[
    ("karaoke", 12.0),
    ("made famous by", 12.0),
    ("backing track", 12.0),
    ("reaction", 12.0),
    ("reacts", 12.0),
    ("first time hearing", 12.0),
    ("play along", 12.0),
    ("tutorial", 10.0),
    ("how to play", 10.0),
    ("nightcore", 12.0),
    ("sped up", 10.0),
    ("slowed", 10.0),
    ("reverb", 8.0),
    ("8d audio", 10.0),
    ("cover", 8.0),
    ("tribute", 8.0),
    ("live", 8.0),
    ("concert", 8.0),
    ("unplugged", 8.0),
    ("instrumental", 8.0),
    ("acapella", 8.0),
    ("a cappella", 8.0),
    // The artist re-recording their own hit decades later. Same singer,
    // completely different vocal chain — and it often outranks the
    // original in search.
    ("classic version", 8.0),
    ("re-record", 8.0),
    ("rerecord", 8.0),
    ("taylor's version", 8.0),
    ("remix", 6.0),
    ("remaster", 2.0),
    ("demo", 6.0),
    ("acoustic", 6.0),
    ("mashup", 8.0),
    ("medley", 8.0),
    ("full album", 8.0),
    ("greatest hits", 6.0),
    ("megamix", 10.0),
];

/// Judge one candidate, or reject it outright.
///
/// Returns `Err` with a reason for candidates that are disqualified
/// regardless of how well the text matches.
pub fn score(target: &Target, cand: &Candidate) -> Result<Score, String> {
    let Some(duration) = cand.duration else {
        return Err("no duration reported".into());
    };

    // A charting single is a song, not a snippet and not a compilation.
    if !(60.0..=900.0).contains(&duration) {
        return Err(format!("duration {duration:.0}s outside 60..900s"));
    }

    // ── text match ───────────────────────────────────────────────────
    // Prefer the structured `track`/`artist` fields when present: they
    // come from the label's own metadata rather than an uploader's
    // free-text title.
    let cand_title = cand.track.clone().unwrap_or_else(|| cand.title.clone());
    let title_sim = token_overlap(&norm::title_key(&target.title), &norm::title_key(&cand_title));

    let cand_artist = cand
        .artist
        .clone()
        .or_else(|| cand.channel.clone())
        .unwrap_or_default();
    let artist_sim = token_overlap(
        &norm::primary_artist(&target.artist),
        &norm::primary_artist(&cand_artist),
    );

    // Title and artist are NECESSARY, not merely additive. Left as
    // bonuses, an artist's *other* song scored 8.0 — right artist,
    // right era, label metadata, zero title overlap — and sailed past
    // the threshold. Likewise a cover with an exact title would pass on
    // title alone. Either would put a completely wrong vocal into the
    // corpus under a real song's name, which is the one failure mode
    // that survives averaging.
    if title_sim < TITLE_FLOOR {
        return Err(format!("title match {title_sim:.2} below {TITLE_FLOOR}"));
    }
    if artist_sim < ARTIST_FLOOR {
        return Err(format!("artist match {artist_sim:.2} below {ARTIST_FLOOR}"));
    }

    let mut value = 0.0;
    let mut why: Vec<String> = Vec::new();

    value += title_sim * 5.0;
    why.push(format!("title {title_sim:.2}"));
    value += artist_sim * 4.0;
    why.push(format!("artist {artist_sim:.2}"));

    // An art track carries label metadata; an ordinary upload does not.
    if cand.artist.is_some() && cand.track.is_some() {
        value += 2.0;
        why.push("art-track".into());
    }

    // ── version markers ──────────────────────────────────────────────
    // Only penalise a marker the target title does not itself contain —
    // some songs really are called "Live Your Life".
    let hay = format!("{} {}", cand.title, cand.album.clone().unwrap_or_default()).to_lowercase();
    let target_hay = target.title.to_lowercase();
    for (marker, penalty) in VERSION_PENALTIES {
        if hay.contains(marker) && !target_hay.contains(marker) {
            value -= penalty;
            why.push(format!("-{marker}"));
        }
    }

    // ── release year ─────────────────────────────────────────────────
    // The original master is released around the time it charted. This
    // is the main thing separating it from a much later re-recording.
    if let Some(year) = cand.release_year {
        let gap = (year - target.chart_year).abs();
        let adj = match gap {
            0..=1 => 2.0,
            2..=3 => 1.0,
            4..=9 => 0.0,
            _ => -3.0,
        };
        value += adj;
        why.push(format!("year {year} ({adj:+.0})"));
    }

    Ok(Score {
        value,
        reason: why.join(", "),
    })
}

/// Fraction of the target's words that appear in the candidate's.
///
/// Asymmetric on purpose: the candidate is allowed extra words (an
/// upload may append an album or a label), but every word of the charted
/// title should be present.
fn token_overlap(target: &str, cand: &str) -> f64 {
    let t: Vec<&str> = target.split_whitespace().collect();
    if t.is_empty() {
        return 0.0;
    }
    let c: Vec<&str> = cand.split_whitespace().collect();
    let hit = t.iter().filter(|w| c.contains(w)).count();
    hit as f64 / t.len() as f64
}

/// Pick the best acceptable candidate, if any.
pub fn best<'a>(target: &Target, cands: &'a [Candidate]) -> Option<(&'a Candidate, Score)> {
    cands
        .iter()
        .filter_map(|c| score(target, c).ok().map(|s| (c, s)))
        .max_by(|a, b| a.1.value.total_cmp(&b.1.value))
        .filter(|(_, s)| s.value >= ACCEPT)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn target() -> Target {
        Target {
            title: "Have You Ever Really Loved A Woman?".into(),
            artist: "Bryan Adams".into(),
            chart_year: 1995,
        }
    }

    fn art_track(title: &str, year: i32, duration: f64) -> Candidate {
        Candidate {
            id: "x".into(),
            title: title.into(),
            channel: Some("Bryan Adams".into()),
            duration: Some(duration),
            artist: Some("Bryan Adams".into()),
            track: Some(title.into()),
            album: None,
            release_year: Some(year),
        }
    }

    #[test]
    fn the_original_master_is_accepted() {
        let c = art_track("Have You Ever Really Loved A Woman?", 1995, 289.0);
        let s = score(&target(), &c).unwrap();
        assert!(s.value >= ACCEPT, "expected accept, got {s:?}");
    }

    /// Every one of these was a real result for this song on YouTube.
    #[test]
    fn the_real_wrong_results_are_all_rejected() {
        let cases = [
            art_track("Have You Ever Really Loved A Woman? (Classic Version)", 2022, 289.0),
            art_track("Have You Ever Really Loved A Woman? (Live)", 1996, 372.0),
            art_track(
                "Have You Ever Really Loved A Woman? (Live At The Royal Albert Hall 2024)",
                2024,
                309.0,
            ),
        ];
        for c in cases {
            let s = score(&target(), &c).unwrap();
            assert!(s.value < ACCEPT, "should have rejected {:?}: {s:?}", c.title);
        }
    }

    #[test]
    fn engagement_content_is_rejected_even_with_a_perfect_title() {
        for junk in [
            "Bryan Adams - Have You Ever Really Loved A Woman REACTION",
            "First time hearing Bryan Adams Have You Ever Really Loved A Woman",
            "Have You Ever Really Loved A Woman - Karaoke",
            "[292] Have You Ever Really Loved A Woman - Bryan Adams play along",
        ] {
            let mut c = art_track(junk, 1995, 289.0);
            c.track = None; // ordinary upload, no label metadata
            c.artist = None;
            let s = score(&target(), &c).unwrap();
            assert!(s.value < ACCEPT, "should have rejected {junk:?}: {s:?}");
        }
    }

    /// The right artist, right era, and label metadata — but a
    /// different song. Scored 8.0 and was accepted while title match
    /// was merely additive.
    #[test]
    fn a_different_song_by_the_right_artist_is_rejected() {
        let c = art_track("18 Til I Die", 1996, 210.0);
        assert!(score(&target(), &c).is_err(), "{:?}", score(&target(), &c));
    }

    /// The exact right title, released the right year, with clean label
    /// metadata — by somebody else entirely.
    #[test]
    fn a_faithful_cover_by_another_artist_is_rejected() {
        let mut c = art_track("Have You Ever Really Loved A Woman?", 1995, 289.0);
        c.artist = Some("Somebody Else Entirely".into());
        c.channel = Some("Somebody Else Entirely".into());
        assert!(score(&target(), &c).is_err(), "{:?}", score(&target(), &c));
    }

    #[test]
    fn snippets_and_compilations_are_disqualified_outright() {
        let mut clip = art_track("Have You Ever Really Loved A Woman?", 1995, 45.0);
        assert!(score(&target(), &clip).is_err());
        clip.duration = Some(3600.0);
        assert!(score(&target(), &clip).is_err());
        clip.duration = None;
        assert!(score(&target(), &clip).is_err());
    }

    #[test]
    fn a_marker_that_is_part_of_the_real_title_is_not_penalised() {
        let t = Target {
            title: "Live Your Life".into(),
            artist: "An Artist".into(),
            chart_year: 2008,
        };
        let c = Candidate {
            id: "x".into(),
            title: "Live Your Life".into(),
            channel: Some("An Artist".into()),
            duration: Some(226.0),
            artist: Some("An Artist".into()),
            track: Some("Live Your Life".into()),
            album: None,
            release_year: Some(2008),
        };
        let s = score(&t, &c).unwrap();
        assert!(s.value >= ACCEPT, "{s:?}");
    }

    #[test]
    fn best_picks_the_original_over_a_rerecording() {
        let cands = vec![
            art_track("Have You Ever Really Loved A Woman? (Classic Version)", 2022, 289.0),
            art_track("Have You Ever Really Loved A Woman?", 1995, 289.0),
            art_track("Have You Ever Really Loved A Woman? (Live)", 1996, 372.0),
        ];
        let (best, _) = best(&target(), &cands).expect("should have found the original");
        assert_eq!(best.release_year, Some(1995));
        assert!(!best.title.contains("Classic"));
    }

    #[test]
    fn best_returns_nothing_when_only_wrong_versions_exist() {
        // A gap in the corpus beats a confident wrong number.
        let cands = vec![
            art_track("Have You Ever Really Loved A Woman? (Live)", 1996, 372.0),
            art_track("18 Til I Die", 1996, 210.0),
        ];
        assert!(best(&target(), &cands).is_none());
    }
}
