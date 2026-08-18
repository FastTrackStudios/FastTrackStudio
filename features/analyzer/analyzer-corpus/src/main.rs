//! `corpus` — build and inspect the Billboard chart corpus.
//!
//! Typical first run:
//!
//! ```text
//! corpus ingest-hot100 --db corpus.sqlite --from 1990 --to 2025
//! corpus ingest-genre  --db corpus.sqlite
//! corpus status        --db corpus.sqlite
//! corpus export        --db corpus.sqlite --out songs.csv
//! ```
//!
//! `ingest-genre` is resumable: every chart-week it fetches is recorded
//! in the ledger, and a re-run skips what is already there. That matters
//! because it is several thousand page fetches and will be interrupted.

use std::path::PathBuf;
use std::time::Duration;

use analyzer_corpus::billboard::{self, ScrapeError};
use analyzer_corpus::chart::Chart;
use analyzer_corpus::db::Store;
use analyzer_corpus::hot100;
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use sqlx::Row;

#[derive(Parser)]
#[command(name = "corpus", about = "Build the Billboard chart corpus")]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Ingest the Hot 100 from its public JSON archive.
    IngestHot100 {
        #[arg(long, default_value = "corpus.sqlite")]
        db: PathBuf,
        #[arg(long, default_value_t = 1990)]
        from: i32,
        #[arg(long, default_value_t = 2025)]
        to: i32,
    },

    /// Ingest the genre charts by fetching Billboard's chart pages.
    ///
    /// Chart dates come from the Hot 100 already in the database, so
    /// run `ingest-hot100` first — those are Billboard's real chart
    /// Saturdays, which beats guessing at a weekly cadence.
    IngestGenre {
        #[arg(long, default_value = "corpus.sqlite")]
        db: PathBuf,
        /// Charts to fetch; defaults to every genre chart.
        #[arg(long, value_delimiter = ',')]
        charts: Vec<Chart>,
        #[arg(long, default_value_t = 1990)]
        from: i32,
        #[arg(long, default_value_t = 2025)]
        to: i32,
        /// Fetch every Nth chart week. 1 = every week.
        ///
        /// A song sits on a genre chart for many weeks, so a coarser
        /// stride still catches nearly every song at a fraction of the
        /// requests — useful for a first pass over 35 years.
        #[arg(long, default_value_t = 1)]
        stride: usize,
        /// Concurrent requests. Kept low deliberately: this is someone
        /// else's server and each page is ~3 MB.
        #[arg(long, default_value_t = 2)]
        concurrency: usize,
        /// Pause between requests on each worker, in milliseconds.
        #[arg(long, default_value_t = 400)]
        delay_ms: u64,
    },

    /// Show what the corpus currently holds.
    Status {
        #[arg(long, default_value = "corpus.sqlite")]
        db: PathBuf,
    },

    /// Write one row per song to CSV.
    Export {
        #[arg(long, default_value = "corpus.sqlite")]
        db: PathBuf,
        #[arg(long, default_value = "songs.csv")]
        out: PathBuf,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .init();

    match Cli::parse().cmd {
        Cmd::IngestHot100 { db, from, to } => ingest_hot100(db, from, to).await,
        Cmd::IngestGenre {
            db,
            charts,
            from,
            to,
            stride,
            concurrency,
            delay_ms,
        } => ingest_genre(db, charts, from, to, stride, concurrency, delay_ms).await,
        Cmd::Status { db } => status(db).await,
        Cmd::Export { db, out } => export(db, out).await,
    }
}

fn http() -> Result<reqwest::Client> {
    reqwest::Client::builder()
        .user_agent(billboard::USER_AGENT)
        .timeout(Duration::from_secs(60))
        .build()
        .context("building the HTTP client")
}

async fn ingest_hot100(db: PathBuf, from: i32, to: i32) -> Result<()> {
    let store = Store::open(&db).await?;
    let entries = hot100::fetch(&http()?, from, to).await?;

    tracing::info!(entries = entries.len(), from, to, "inserting Hot 100 entries");
    let stats = store.insert_entries(&entries).await?;

    // The ledger is per chart-week, so record each date we covered.
    let mut dates: Vec<&str> = entries.iter().map(|e| e.date.as_str()).collect();
    dates.sort_unstable();
    dates.dedup();
    for date in &dates {
        let n = entries.iter().filter(|e| e.date == **date).count();
        store.mark_week(Chart::Hot100.slug(), date, n).await?;
    }

    println!(
        "hot-100: {} chart weeks, {} new entries, {} songs in corpus",
        dates.len(),
        stats.entries,
        store.song_count().await?
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)]
async fn ingest_genre(
    db: PathBuf,
    charts: Vec<Chart>,
    from: i32,
    to: i32,
    stride: usize,
    concurrency: usize,
    delay_ms: u64,
) -> Result<()> {
    let store = Store::open(&db).await?;
    let client = http()?;

    let charts = if charts.is_empty() {
        Chart::GENRE.to_vec()
    } else {
        charts
    };

    // Billboard's real chart Saturdays, taken from the Hot 100 ingest.
    let dates: Vec<String> = sqlx::query(
        "SELECT DISTINCT chart_date FROM chart_entry
          WHERE chart = 'hot-100'
            AND CAST(substr(chart_date,1,4) AS INTEGER) BETWEEN ? AND ?
          ORDER BY chart_date",
    )
    .bind(from)
    .bind(to)
    .fetch_all(store.pool())
    .await
    .context("reading chart dates from the Hot 100")?
    .iter()
    .map(|r| r.get::<String, _>(0))
    .collect();

    anyhow::ensure!(
        !dates.is_empty(),
        "no Hot 100 chart dates in {} for {from}..{to} — run `corpus ingest-hot100` first",
        db.display()
    );

    let stride = stride.max(1);
    let wanted: Vec<String> = dates.into_iter().step_by(stride).collect();

    for chart in charts {
        let done = store.fetched_weeks(chart.slug()).await?;
        let todo: Vec<String> = wanted
            .iter()
            .filter(|d| !done.contains(d))
            .cloned()
            .collect();

        tracing::info!(
            %chart,
            todo = todo.len(),
            already = wanted.len() - todo.len(),
            "starting genre chart ingest"
        );

        let mut inserted = 0usize;
        let mut absent = 0usize;

        // Chunked rather than a full-width buffer_unordered so the
        // ledger is written as we go — an interrupted run keeps
        // everything it already fetched.
        for chunk in todo.chunks(concurrency.max(1)) {
            let results = futures::future::join_all(chunk.iter().map(|date| {
                let client = client.clone();
                let date = date.clone();
                async move {
                    tokio::time::sleep(Duration::from_millis(delay_ms)).await;
                    let got = billboard::fetch(&client, chart, &date).await;
                    (date, got)
                }
            }))
            .await;

            for (date, got) in results {
                match got {
                    Ok(entries) => {
                        let n = entries.len();
                        let stats = store.insert_entries(&entries).await?;
                        store.mark_week(chart.slug(), &date, n).await?;
                        inserted += stats.entries;
                    }
                    Err(e) if is_absent(&e) => {
                        // Genre charts launched at different times; a
                        // 404 before launch is expected, and recording
                        // it stops us asking again.
                        store.mark_week(chart.slug(), &date, 0).await?;
                        absent += 1;
                    }
                    Err(e) => {
                        // Anything else — a markup change, a network
                        // blip — must not be recorded as an empty week.
                        tracing::error!(%chart, date, error = %e, "chart week failed, not marking");
                    }
                }
            }
        }

        tracing::info!(%chart, inserted, absent, "genre chart ingest finished");
    }

    println!("corpus now holds {} songs", store.song_count().await?);
    Ok(())
}

/// Was this failure "the chart did not exist then" rather than a real
/// problem?
fn is_absent(e: &anyhow::Error) -> bool {
    matches!(
        e.downcast_ref::<ScrapeError>(),
        Some(ScrapeError::NoSuchChart { .. })
    )
}

async fn status(db: PathBuf) -> Result<()> {
    let store = Store::open(&db).await?;

    println!("{:<24} {:>7} {:>8}  {:<12} {:<12}", "chart", "weeks", "songs", "first", "last");
    println!("{}", "-".repeat(68));
    for (chart, weeks, songs, first, last) in store.summary().await? {
        println!("{chart:<24} {weeks:>7} {songs:>8}  {first:<12} {last:<12}");
    }
    println!("{}", "-".repeat(68));
    println!("{:<24} {:>7} {:>8}", "TOTAL (deduped)", "", store.song_count().await?);
    Ok(())
}

async fn export(db: PathBuf, out: PathBuf) -> Result<()> {
    let store = Store::open(&db).await?;

    let rows = sqlx::query(
        "SELECT song_id, title, artist, primary_artist, first_year, first_charted,
                last_charted, best_rank, hot100_peak, hot100_weeks, chart_weeks,
                chart_count, genre_charts
           FROM song_stats
          ORDER BY first_charted, best_rank",
    )
    .fetch_all(store.pool())
    .await
    .context("reading song_stats")?;

    let mut w = csv::Writer::from_path(&out)
        .with_context(|| format!("creating {}", out.display()))?;
    w.write_record([
        "song_id",
        "title",
        "artist",
        "primary_artist",
        "first_year",
        "first_charted",
        "last_charted",
        "best_rank",
        "hot100_peak",
        "hot100_weeks",
        "chart_weeks",
        "chart_count",
        "genre_charts",
    ])?;

    for r in &rows {
        w.write_record([
            r.get::<i64, _>("song_id").to_string(),
            r.get::<String, _>("title"),
            r.get::<String, _>("artist"),
            r.get::<String, _>("primary_artist"),
            r.get::<i64, _>("first_year").to_string(),
            r.get::<String, _>("first_charted"),
            r.get::<String, _>("last_charted"),
            r.get::<i64, _>("best_rank").to_string(),
            opt_i64(r, "hot100_peak"),
            r.get::<i64, _>("hot100_weeks").to_string(),
            r.get::<i64, _>("chart_weeks").to_string(),
            r.get::<i64, _>("chart_count").to_string(),
            r.get::<Option<String>, _>("genre_charts").unwrap_or_default(),
        ])?;
    }
    w.flush()?;

    println!("wrote {} songs to {}", rows.len(), out.display());
    Ok(())
}

fn opt_i64(row: &sqlx::sqlite::SqliteRow, col: &str) -> String {
    row.get::<Option<i64>, _>(col)
        .map(|v| v.to_string())
        .unwrap_or_default()
}
