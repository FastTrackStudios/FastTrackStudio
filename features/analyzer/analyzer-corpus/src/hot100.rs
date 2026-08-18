//! Hot 100 ingest, from the public JSON archive.
//!
//! The Hot 100 — and only the Hot 100 — has a complete machine-readable
//! archive back to its first chart in August 1958, maintained at
//! <https://github.com/mhollingshead/billboard-hot-100>. It is one 44 MB
//! download for every chart week ever published, which is both far
//! kinder to Billboard and far more trustworthy than scraping ~3,500
//! pages of markup.
//!
//! Every other chart has to be scraped; see [`crate::billboard`].

use anyhow::{Context, Result};
use serde::Deserialize;

use crate::chart::{Chart, ChartEntry};

/// Where the archive lives.
pub const ARCHIVE_URL: &str =
    "https://raw.githubusercontent.com/mhollingshead/billboard-hot-100/main/all.json";

/// One chart week as the archive publishes it.
#[derive(Debug, Deserialize)]
struct ArchiveWeek {
    date: String,
    data: Vec<ArchiveRow>,
}

#[derive(Debug, Deserialize)]
struct ArchiveRow {
    song: String,
    artist: String,
    this_week: i64,
    // last_week / peak_position / weeks_on_chart are published too, but
    // the corpus derives those from its own time series so that every
    // chart — scraped or not — reports them the same way. See db::SCHEMA.
}

/// Download the archive and turn it into chart entries within
/// `[from_year, to_year]` inclusive.
pub async fn fetch(client: &reqwest::Client, from_year: i32, to_year: i32) -> Result<Vec<ChartEntry>> {
    tracing::info!(url = ARCHIVE_URL, "downloading the Hot 100 archive");

    let body = client
        .get(ARCHIVE_URL)
        .send()
        .await
        .context("requesting the Hot 100 archive")?
        .error_for_status()
        .context("the Hot 100 archive returned an error status")?
        .bytes()
        .await
        .context("reading the Hot 100 archive body")?;

    tracing::info!(bytes = body.len(), "archive downloaded, parsing");

    let weeks: Vec<ArchiveWeek> =
        serde_json::from_slice(&body).context("parsing the Hot 100 archive JSON")?;

    Ok(select(&weeks, from_year, to_year))
}

/// Flatten the archive down to the requested year range.
fn select(weeks: &[ArchiveWeek], from_year: i32, to_year: i32) -> Vec<ChartEntry> {
    let mut out = Vec::new();
    for week in weeks {
        let Some(year) = week.date.get(..4).and_then(|y| y.parse::<i32>().ok()) else {
            tracing::warn!(date = %week.date, "skipping archive week with an unparseable date");
            continue;
        };
        if year < from_year || year > to_year {
            continue;
        }
        for row in &week.data {
            out.push(ChartEntry {
                chart: Chart::Hot100,
                date: week.date.clone(),
                rank: row.this_week,
                title: row.song.trim().to_string(),
                artist: row.artist.trim().to_string(),
            });
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn archive() -> Vec<ArchiveWeek> {
        serde_json::from_str(
            r#"[
              {"date":"1989-12-30","data":[
                {"song":"A","artist":"X","this_week":1,"last_week":null,
                 "peak_position":1,"weeks_on_chart":1}]},
              {"date":"1990-01-06","data":[
                {"song":"B","artist":"Y","this_week":1,"last_week":2,
                 "peak_position":1,"weeks_on_chart":3},
                {"song":"C","artist":"Z","this_week":2,"last_week":null,
                 "peak_position":2,"weeks_on_chart":1}]},
              {"date":"2026-01-03","data":[
                {"song":"D","artist":"W","this_week":1,"last_week":null,
                 "peak_position":1,"weeks_on_chart":1}]}
            ]"#,
        )
        .unwrap()
    }

    #[test]
    fn year_range_is_inclusive_and_excludes_outside() {
        let got = select(&archive(), 1990, 2025);
        assert_eq!(got.len(), 2, "only the 1990 week's two rows should survive");
        assert!(got.iter().all(|e| e.date == "1990-01-06"));
        assert_eq!(got[0].rank, 1);
        assert_eq!(got[0].title, "B");
        assert_eq!(got[1].rank, 2);
    }

    #[test]
    fn every_entry_is_attributed_to_the_hot_100() {
        assert!(select(&archive(), 1958, 2030)
            .iter()
            .all(|e| e.chart == Chart::Hot100));
    }
}
