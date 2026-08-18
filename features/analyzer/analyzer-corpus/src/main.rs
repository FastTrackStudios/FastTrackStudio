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

use analyzer_corpus::acquire;
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

    /// Fetch one audio rendition per song.
    ///
    /// Resumable: a song with any recorded outcome is skipped, so an
    /// interrupted run picks up where it stopped. Works best-charting
    /// songs first, so a partial run still covers the biggest hits.
    Acquire {
        #[arg(long, default_value = "corpus.sqlite")]
        db: PathBuf,
        /// Where audio files are written.
        #[arg(long, default_value = "/run/media/AudioHaven/fts-corpus/audio")]
        audio_root: PathBuf,
        /// How many songs to attempt this run.
        #[arg(long, default_value_t = 50)]
        limit: i64,
        /// Songs attempted concurrently.
        #[arg(long, default_value_t = 4)]
        concurrency: usize,
        /// How many search candidates to score per song.
        #[arg(long, default_value_t = 10)]
        candidates: usize,
        /// Score candidates and report, without downloading anything.
        #[arg(long)]
        dry_run: bool,
        /// Pick songs at random rather than best-charting first.
        ///
        /// The honest way to measure a match rate: the biggest hits are
        /// also the best-catalogued, so working down from #1 flatters
        /// the resolver and hides how it does on the long tail.
        #[arg(long)]
        sample: bool,
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
        Cmd::Acquire {
            db,
            audio_root,
            limit,
            concurrency,
            candidates,
            dry_run,
            sample,
        } => acquire_cmd(db, audio_root, limit, concurrency, candidates, dry_run, sample).await,
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

#[allow(clippy::too_many_arguments)]
async fn acquire_cmd(
    db: PathBuf,
    audio_root: PathBuf,
    limit: i64,
    concurrency: usize,
    candidates: usize,
    dry_run: bool,
    sample: bool,
) -> Result<()> {
    let store = Store::open(&db).await?;
    // Fail here rather than 4,000 songs into an overnight run.
    let tools = acquire::Tools::discover()?;

    let pending = store.songs_needing_audio(limit, sample).await?;
    if pending.is_empty() {
        println!("nothing pending — every song already has a recorded outcome");
        return Ok(());
    }

    tracing::info!(
        songs = pending.len(),
        concurrency,
        dry_run,
        "starting acquisition"
    );

    for chunk in pending.chunks(concurrency.max(1)) {
        let results = futures::future::join_all(
            chunk
                .iter()
                .map(|song| attempt(&tools, song, &audio_root, candidates, dry_run)),
        )
        .await;

        for (song, rec) in chunk.iter().zip(results) {
            let mark = match rec.status.as_str() {
                "ok" => "ok  ",
                "no_match" => "MISS",
                _ => "FAIL",
            };
            println!(
                "{mark} [{:>5}] {} — {}{}",
                song.song_id,
                truncate(&song.title, 38),
                truncate(&song.artist, 26),
                match (&rec.match_score, &rec.error) {
                    (Some(s), _) => format!("  score {s:.1} ({})", rec.match_reason.as_deref().unwrap_or("")),
                    (None, Some(e)) => format!("  {e}"),
                    _ => String::new(),
                }
            );
            if !dry_run {
                store.record_rendition(&rec).await?;
            }
        }
    }

    if !dry_run {
        println!();
        for (status, n) in store.acquisition_summary().await? {
            println!("  {status:<10} {n}");
        }
    }
    Ok(())
}

/// One song's full resolve → download → probe attempt.
///
/// Never returns `Err`: every failure becomes a recorded outcome, so a
/// long run is not derailed by one bad song and the failure stays
/// visible in the corpus.
async fn attempt(
    tools: &acquire::Tools,
    song: &analyzer_corpus::db::PendingSong,
    audio_root: &std::path::Path,
    candidates: usize,
    dry_run: bool,
) -> analyzer_corpus::db::RenditionRecord {
    use analyzer_corpus::db::RenditionRecord;

    let mut rec = RenditionRecord {
        song_id: song.song_id,
        status: "failed".into(),
        source: Some("youtube-music".into()),
        ..Default::default()
    };

    let target = acquire::Target {
        title: song.title.clone(),
        artist: song.artist.clone(),
        chart_year: song.first_year as i32,
    };

    let cands = match acquire::resolve(tools, &target, candidates).await {
        Ok(c) => c,
        Err(e) => {
            rec.error = Some(format!("resolve: {e}"));
            return rec;
        }
    };

    let Some((best, score)) = acquire::score::best(&target, &cands) else {
        rec.status = "no_match".into();
        rec.error = Some(format!("{} candidates, none scored >= {}", cands.len(), acquire::score::ACCEPT));
        return rec;
    };

    rec.video_id = Some(best.id.clone());
    rec.match_score = Some(score.value);
    rec.match_reason = Some(score.reason.clone());
    rec.cand_title = best.track.clone().or_else(|| Some(best.title.clone()));
    rec.cand_artist = best.artist.clone().or_else(|| best.channel.clone());
    rec.cand_year = best.release_year.map(|y| y as i64);

    if dry_run {
        rec.status = "ok".into();
        return rec;
    }

    // One directory per song keeps a re-download from colliding with
    // the file it is replacing.
    let dir = audio_root.join(song.song_id.to_string());
    let path = match acquire::download(tools, &best.id, &dir).await {
        Ok(p) => p,
        Err(e) => {
            rec.error = Some(format!("download: {e}"));
            return rec;
        }
    };

    match acquire::probe(tools, &path).await {
        Ok(p) => {
            // A clip or a half-finished download must never be filed as
            // the record — it would measure as ordinary data.
            if let Err(e) = acquire::check_complete(&p, best.duration) {
                rec.error = Some(format!("incomplete: {e}"));
                let _ = tokio::fs::remove_file(&path).await;
                return rec;
            }
            rec.status = "ok".into();
            rec.path = Some(p.path.to_string_lossy().into_owned());
            rec.bytes = Some(p.bytes as i64);
            rec.duration_s = Some(p.duration_s);
            rec.codec = Some(p.codec);
            rec.sample_rate = Some(p.sample_rate as i64);
            rec.channels = Some(p.channels as i64);
        }
        Err(e) => rec.error = Some(format!("probe: {e}")),
    }
    rec
}

fn truncate(s: &str, n: usize) -> String {
    if s.chars().count() <= n {
        format!("{s:<n$}")
    } else {
        let head: String = s.chars().take(n.saturating_sub(1)).collect();
        format!("{head}…")
    }
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
