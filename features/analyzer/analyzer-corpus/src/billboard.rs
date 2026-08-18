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
//! ## Trusting the page, not the URL
//!
//! Asking for a chart *before that chart launched* does not 404. Billboard
//! answers 200, with no redirect, and renders its earliest available
//! chart instead. Requesting Dance/Electronic Songs — which launched in
//! 2013 — for 1990, 1999 and 2009 returns the same January 2013 chart
//! all three times.
//!
//! Taking the URL at its word would therefore file 2013 data under 1990
//! and quietly corrupt every era comparison the corpus exists to make.
//! So the page's own rendered date (`Week of June 10, 1995`) is parsed
//! and treated as the truth:
//!
//! - it is what entries are filed under, not the requested date, and
//! - if it is more than a week away from what was asked for, Billboard
//!   has snapped to a different chart and the week is reported as
//!   [`ScrapeError::NoSuchChart`].
//!
//! ## Failing loudly
//!
//! The other dangerous outcome is silently parsing zero rows and
//! recording that as "this chart was empty that week". A page with no
//! row containers at all is a [`ScrapeError::NoRows`], which the caller
//! must treat as a bug rather than as data.

use anyhow::{Context, Result};
use scraper::{Html, Selector};

use crate::chart::{Chart, ChartEntry};

/// A browser UA. Billboard serves a stripped page to obvious bots.
pub const USER_AGENT: &str =
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36";

#[derive(Debug, thiserror::Error)]
pub enum ScrapeError {
    /// The chart had no edition for that date, so Billboard served a
    /// different week. Expected for dates before a genre chart launched.
    #[error("chart {chart} has no edition for {date} (it served {served} instead)")]
    NoSuchChart {
        chart: Chart,
        date: String,
        /// What Billboard rendered instead, or `"404"`.
        served: String,
    },

    /// The page rendered rows but carried no `Week of ...` date, so
    /// there is no way to tell which chart week it actually is.
    #[error(
        "chart {chart} for {date} has no rendered 'Week of' date — \
         Billboard's markup has probably changed; check rendered_date() in billboard.rs"
    )]
    NoRenderedDate { chart: Chart, date: String },

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
            served: "404".to_string(),
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
    let rows: Vec<_> = doc.select(&row_sel).collect();

    if rows.is_empty() {
        return Err(ScrapeError::NoRows {
            chart,
            date: date.to_string(),
            bytes: html.len(),
        });
    }

    // Billboard answers 200 for a chart that had not launched yet and
    // renders its earliest available week instead, so the requested
    // date cannot be trusted — only the date the page states.
    let Some(rendered) = rendered_date(html) else {
        return Err(ScrapeError::NoRenderedDate {
            chart,
            date: date.to_string(),
        });
    };

    if !within_a_week(&rendered, date) {
        return Err(ScrapeError::NoSuchChart {
            chart,
            date: date.to_string(),
            served: rendered,
        });
    }

    let mut out = Vec::new();

    for row in rows {
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
            // Filed under what the page says the week is, not what was
            // asked for, so a snapped date corrects itself.
            date: rendered.clone(),
            rank,
            title,
            artist,
        });
    }

    Ok(out)
}

/// The chart week the page actually rendered, as ISO `yyyy-mm-dd`.
///
/// Billboard states it in prose — `Week of June 10, 1995` — which is
/// the only place on the page that distinguishes the week you asked for
/// from the week you were given.
pub fn rendered_date(html: &str) -> Option<String> {
    let at = html.find("Week of ")? + "Week of ".len();
    let rest = &html[at..];
    let end = rest.find('<').unwrap_or(rest.len());
    let text = rest[..end].trim();

    // "June 10, 1995"
    let (month_name, rest) = text.split_once(' ')?;
    let (day, year) = rest.split_once(", ")?;

    let month = MONTHS
        .iter()
        .position(|m| m.eq_ignore_ascii_case(month_name.trim()))? as u32
        + 1;
    let day: u32 = day.trim().parse().ok()?;
    let year: i32 = year.trim().parse().ok()?;

    chrono::NaiveDate::from_ymd_opt(year, month, day).map(|d| d.format("%Y-%m-%d").to_string())
}

const MONTHS: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

/// Are these two ISO dates within seven days of each other?
///
/// Charts share one Saturday grid, so a legitimate answer matches
/// exactly; the tolerance only absorbs a chart whose week is dated a
/// day or two differently in some era. Anything further away means
/// Billboard served a different chart entirely.
fn within_a_week(a: &str, b: &str) -> bool {
    let parse = |s: &str| chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d").ok();
    match (parse(a), parse(b)) {
        (Some(a), Some(b)) => (a - b).num_days().abs() <= 7,
        _ => false,
    }
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

    /// A whole page: the `Week of ...` heading Billboard renders, plus
    /// `n` chart rows.
    fn page(week_of: &str, n: usize) -> String {
        format!(
            "<html><body><p class=\"c-tagline\">Week of {week_of}</p>{}</body></html>",
            ROW.repeat(n)
        )
    }

    #[test]
    fn reads_rank_title_and_linked_artist() {
        let got = parse(&page("January 4, 2020", 1), Chart::Country, "2020-01-04").unwrap();
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].rank, 1);
        assert_eq!(got[0].title, "Some Song Title");
        assert_eq!(got[0].artist, "An Artist");
        assert_eq!(got[0].chart, Chart::Country);
        assert_eq!(got[0].date, "2020-01-04");
    }

    #[test]
    fn reads_an_unlinked_artist() {
        let unlinked = page("January 4, 2020", 1).replace(
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
        let got = parse(&page("January 4, 2020", 1), Chart::Rock, "2020-01-04").unwrap();
        assert_eq!(got[0].rank, 1);
    }

    #[test]
    fn many_rows_all_parse() {
        let got = parse(&page("January 4, 2020", 100), Chart::Latin, "2020-01-04").unwrap();
        assert_eq!(got.len(), 100);
    }

    #[test]
    fn a_page_with_no_rows_is_an_error_not_an_empty_week() {
        let err = parse(
            "<html><body>nothing here</body></html>",
            Chart::Rock,
            "2020-01-04",
        )
        .unwrap_err();
        assert!(matches!(err, ScrapeError::NoRows { .. }));
    }

    // ── the pre-launch snap ───────────────────────────────────────────
    //
    // Billboard answers 200 and renders its earliest available chart
    // when asked for a date before that chart existed. Observed live:
    // dance-electronic-songs (launched 2013) returned the same January
    // 2013 chart for 1990, 1999 and 2009. Trusting the URL would file
    // 2013 data under 1990.

    #[test]
    fn a_chart_served_for_the_wrong_week_is_rejected() {
        let err = parse(&page("January 5, 2013", 50), Chart::DanceElectronic, "1990-01-06")
            .unwrap_err();
        match err {
            ScrapeError::NoSuchChart { served, date, .. } => {
                assert_eq!(date, "1990-01-06");
                assert_eq!(served, "2013-01-05");
            }
            other => panic!("expected NoSuchChart, got {other:?}"),
        }
    }

    #[test]
    fn entries_are_filed_under_the_rendered_week_not_the_requested_one() {
        // Within tolerance, so it parses — but the page's own date wins.
        let got = parse(&page("January 4, 2020", 1), Chart::Rock, "2020-01-03").unwrap();
        assert_eq!(got[0].date, "2020-01-04");
    }

    #[test]
    fn a_page_with_rows_but_no_rendered_date_is_an_error() {
        let no_date = format!("<html><body>{}</body></html>", ROW);
        let err = parse(&no_date, Chart::Rock, "2020-01-04").unwrap_err();
        assert!(matches!(err, ScrapeError::NoRenderedDate { .. }));
    }

    #[test]
    fn rendered_date_parses_billboards_prose() {
        assert_eq!(
            rendered_date("<p>Week of June 10, 1995 </p>").as_deref(),
            Some("1995-06-10")
        );
        assert_eq!(
            rendered_date("Week of December 31, 2022<").as_deref(),
            Some("2022-12-31")
        );
        assert_eq!(rendered_date("no date here"), None);
        assert_eq!(rendered_date("Week of Smarch 40, 1995<"), None);
    }

    #[test]
    fn week_tolerance_accepts_neighbours_and_rejects_eras() {
        assert!(within_a_week("2020-01-04", "2020-01-04"));
        assert!(within_a_week("2020-01-04", "2020-01-11"));
        assert!(!within_a_week("2020-01-04", "2020-01-12"));
        assert!(!within_a_week("2013-01-05", "1990-01-06"));
    }
}
