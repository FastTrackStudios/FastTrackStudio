//! The Billboard charts the corpus is drawn from, and how each one is
//! reached.
//!
//! Two acquisition paths, because Billboard publishes them differently:
//!
//! - **Hot 100** has a complete public JSON archive going back to 1958
//!   (see [`crate::hot100`]). One 44 MB download, no scraping, exact.
//! - **Every genre chart** has no dump at all, so those come out of the
//!   chart pages themselves ([`crate::billboard`]) at roughly 3 MB of
//!   HTML per chart-week.
//!
//! The genre charts matter because they make genre a *fact about chart
//! membership* rather than a guess from a tagging service — and because
//! a song can sit on several at once, which is real and worth keeping
//! rather than flattening to one label.

use std::fmt;

/// A Billboard chart the corpus ingests.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Chart {
    Hot100,
    Country,
    RnbHipHop,
    Rock,
    Latin,
    Christian,
    DanceElectronic,
}

impl Chart {
    /// Every chart the corpus knows how to ingest.
    pub const ALL: &'static [Chart] = &[
        Chart::Hot100,
        Chart::Country,
        Chart::RnbHipHop,
        Chart::Rock,
        Chart::Latin,
        Chart::Christian,
        Chart::DanceElectronic,
    ];

    /// The genre charts — everything except the Hot 100 spine.
    pub const GENRE: &'static [Chart] = &[
        Chart::Country,
        Chart::RnbHipHop,
        Chart::Rock,
        Chart::Latin,
        Chart::Christian,
        Chart::DanceElectronic,
    ];

    /// The path segment under `billboard.com/charts/`.
    ///
    /// These are the slugs that answer 200 directly. Billboard also
    /// serves `hot-`-prefixed aliases (`hot-country-songs`, etc.) but
    /// those 301 to these, so using the canonical form saves a redirect
    /// on every one of several thousand requests.
    pub fn slug(self) -> &'static str {
        match self {
            Chart::Hot100 => "hot-100",
            Chart::Country => "country-songs",
            Chart::RnbHipHop => "r-b-hip-hop-songs",
            Chart::Rock => "rock-songs",
            Chart::Latin => "latin-songs",
            Chart::Christian => "christian-songs",
            Chart::DanceElectronic => "dance-electronic-songs",
        }
    }

    /// The genre label this chart implies, or `None` for the Hot 100 —
    /// which is all-genre and so says nothing about a song's genre.
    pub fn genre(self) -> Option<&'static str> {
        match self {
            Chart::Hot100 => None,
            Chart::Country => Some("country"),
            Chart::RnbHipHop => Some("r&b/hip-hop"),
            Chart::Rock => Some("rock"),
            Chart::Latin => Some("latin"),
            Chart::Christian => Some("christian"),
            Chart::DanceElectronic => Some("dance/electronic"),
        }
    }

    pub fn from_slug(s: &str) -> Option<Chart> {
        Chart::ALL.iter().copied().find(|c| c.slug() == s)
    }
}

impl fmt::Display for Chart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.slug())
    }
}

impl std::str::FromStr for Chart {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Chart::from_slug(s).ok_or_else(|| {
            let known: Vec<_> = Chart::ALL.iter().map(|c| c.slug()).collect();
            format!("unknown chart {s:?} — known: {}", known.join(", "))
        })
    }
}

/// One song's placing on one chart in one week.
///
/// Deliberately *only* rank, title and artist. Billboard renders
/// last-week / peak / weeks-on-chart into the same anonymous
/// `span.c-label` soup as everything else in the row, so scraping them
/// is guesswork — and unnecessary, because once every week of a chart
/// is ingested those three are derivable from our own time series, and
/// derived values cannot drift when Billboard reskins the page.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChartEntry {
    pub chart: Chart,
    /// ISO `yyyy-mm-dd` of the chart week.
    pub date: String,
    pub rank: i64,
    pub title: String,
    pub artist: String,
}
