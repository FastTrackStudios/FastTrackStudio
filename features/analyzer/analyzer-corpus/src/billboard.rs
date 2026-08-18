//! Genre-chart ingest, by parsing Billboard's chart pages.
//!
//! No genre chart has a public data dump, so these come out of the
//! rendered page. Billboard serves the modern template for archive
//! dates too, so one parser covers 1990 through today.
//!
//! Only rank, title and artist are read. The row also carries
//! last-week / peak / weeks-on-chart, but they live in the same
//! anonymous `span.c-label` soup as every other number in the row and
//! can only be picked out positionally — so they are derived from our
//! own weekly observations instead (see `db::SCHEMA`'s `song_stats`).
//!
//! ## Failing loudly
//!
//! The one genuinely dangerous outcome for a scraper is silently
//! parsing zero rows and recording that as "this chart was empty that
//! week". [`parse`] therefore separates the two cases: a page with no
//! row containers at all is a [`ScrapeError::NoRows`], which the caller
//! must treat as a bug, while a chart that simply had not launched yet
//! answers 404 and is recorded as a genuinely empty week.

use anyhow::{Context, Result};
use scraper::{Html, Selector};

use crate::chart::{Chart, ChartEntry};

/// A browser UA. Billboard serves a stripped page to obvious bots.
pub const USER_AGENT: &str =
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36";

#[derive(Debug, thiserror::Error)]
pub enum ScrapeError {
    /// The chart did not exist on that date. Expected for dates before
    /// a genre chart launched.
    #[error("chart {chart} has no edition for {date}")]
    NoSuchChart { chart: Chart, date: String },

    /// The page rendered but contained no chart rows — Billboard has
    /// changed its markup and the selectors below need revisiting.
    #[error(
        "chart {chart} for {date} returned {bytes} bytes containing no chart rows — \
         Billboard's markup has probably changed; check the selectors in billboard.rs"
    )]
    NoRows {
        chart: Chart,
        date: String,
        bytes: usize,
    },
}

/// Fetch and parse one chart week.
pub async fn fetch(
    client: &reqwest::Client,
    chart: Chart,
    date: &str,
) -> Result<Vec<ChartEntry>, anyhow::Error> {
    let url = format!("https://www.billboard.com/charts/{}/{date}/", chart.slug());

    let resp = client
        .get(&url)
        .send()
        .await
        .with_context(|| format!("requesting {url}"))?;

    if resp.status() == reqwest::StatusCode::NOT_FOUND {
        return Err(ScrapeError::NoSuchChart {
            chart,
            date: date.to_string(),
        }
        .into());
    }

    let html = resp
        .error_for_status()
        .with_context(|| format!("{url} returned an error status"))?
        .text()
        .await
        .with_context(|| format!("reading the body of {url}"))?;

    parse(&html, chart, date).map_err(Into::into)
}

/// Parse the chart rows out of a Billboard chart page.
pub fn parse(html: &str, chart: Chart, date: &str) -> Result<Vec<ChartEntry>, ScrapeError> {
    // Selectors are constant; a parse failure here is a programming
    // error, not input-dependent.
    let row_sel = Selector::parse("div.o-chart-results-list-row-container").unwrap();
    let title_sel = Selector::parse("h3#title-of-a-story").unwrap();
    let artist_sel = Selector::parse("span.c-label.a-no-trucate").unwrap();
    let label_sel = Selector::parse("span.c-label").unwrap();

    let doc = Html::parse_document(html);
    let mut out = Vec::new();
    let mut containers = 0usize;

    for row in doc.select(&row_sel) {
        containers += 1;

        // The first c-label in the row is the rank. Later ones are the
        // stat columns, so stop at the first that parses.
        let Some(rank) = row
            .select(&label_sel)
            .filter_map(|el| text_of(&el).parse::<i64>().ok())
            .next()
        else {
            tracing::warn!(%chart, date, "chart row with no parseable rank, skipping");
            continue;
        };

        let Some(title) = row.select(&title_sel).map(|el| text_of(&el)).next() else {
            tracing::warn!(%chart, date, rank, "chart row with no title, skipping");
            continue;
        };

        // `a-no-trucate` is Billboard's own misspelling; it is the
        // class that marks the artist credit under the title.
        let artist = row
            .select(&artist_sel)
            .map(|el| text_of(&el))
            .find(|s| !s.is_empty())
            .unwrap_or_default();

        if title.is_empty() || artist.is_empty() {
            tracing::warn!(%chart, date, rank, %title, "chart row missing title or artist, skipping");
            continue;
        }

        out.push(ChartEntry {
            chart,
            date: date.to_string(),
            rank,
            title,
            artist,
        });
    }

    if containers == 0 {
        return Err(ScrapeError::NoRows {
            chart,
            date: date.to_string(),
            bytes: html.len(),
        });
    }

    Ok(out)
}

/// Collapse an element's descendant text into a single trimmed line.
fn text_of(el: &scraper::ElementRef<'_>) -> String {
    let joined: String = el.text().collect::<Vec<_>>().join(" ");
    joined.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A cut-down copy of the real row markup, including the details
    /// that actually broke a naive parser: the artist's `class`
    /// attribute begins with a newline, the artist sits inside an
    /// `<a>`, and several more `c-label` spans follow carrying the stat
    /// columns.
    const ROW: &str = r#"
      <div class="o-chart-results-list-row-container">
        <ul class="o-chart-results-list-row">
          <li><span class="c-label a-font-basic">1</span></li>
          <li>
            <h3 id="title-of-a-story" class="c-title a-font-basic">
                Some Song Title
            </h3>
            <span class="
                c-label a-no-trucate
                a-font-secondary u-font-size-15
            ">
                <a href="https://www.billboard.com/artist/someone/">An Artist</a>
            </span>
          </li>
          <li><span class="c-label">2</span></li>
          <li><span class="c-label">1</span></li>
          <li><span class="c-label">9</span></li>
        </ul>
      </div>"#;

    #[test]
    fn reads_rank_title_and_linked_artist() {
        let got = parse(ROW, Chart::Country, "2020-01-04").unwrap();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].rank, 1);
        assert_eq!(got[0].title, "Some Song Title");
        assert_eq!(got[0].artist, "An Artist");
        assert_eq!(got[0].chart, Chart::Country);
        assert_eq!(got[0].date, "2020-01-04");
    }

    #[test]
    fn reads_an_unlinked_artist() {
        let unlinked = ROW.replace(
            r#"<a href="https://www.billboard.com/artist/someone/">An Artist</a>"#,
            "An Artist",
        );
        let got = parse(&unlinked, Chart::Rock, "2020-01-04").unwrap();
        assert_eq!(got[0].artist, "An Artist");
    }

    #[test]
    fn takes_the_rank_not_a_later_stat_column() {
        // The stat columns are also bare c-label integers; picking the
        // wrong one silently corrupts every rank in the corpus.
        let got = parse(ROW, Chart::Rock, "2020-01-04").unwrap();
        assert_eq!(got[0].rank, 1);
    }

    #[test]
    fn many_rows_all_parse() {
        let page = format!("<html><body>{}</body></html>", ROW.repeat(100));
        assert_eq!(parse(&page, Chart::Latin, "2020-01-04").unwrap().len(), 100);
    }

    #[test]
    fn a_page_with_no_rows_is_an_error_not_an_empty_week() {
        let err = parse("<html><body>nothing here</body></html>", Chart::Rock, "2020-01-04")
            .unwrap_err();
        assert!(matches!(err, ScrapeError::NoRows { .. }));
    }
}
