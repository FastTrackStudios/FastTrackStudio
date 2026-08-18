//! The corpus store.
//!
//! SQLite, because the corpus is a few hundred thousand rows that wants
//! ad-hoc grouping ("crest factor by genre by decade") far more than it
//! wants throughput, and because a single file is the thing you can
//! hand to someone else or point a website at.
//!
//! The schema keeps chart placings and song identities apart:
//!
//! - [`song`](Store::upsert_song) is one row per *record* — the unit
//!   that gets downloaded, separated and measured.
//! - `chart_entry` is one row per song per chart per week, the raw
//!   observations.
//! - `chart_week` records that a chart-week was *fetched*, which is
//!   what makes ingest resumable: without it there is no way to tell a
//!   week we never asked for from a week that legitimately returned
//!   nothing.
//!
//! Peak position and weeks-on-chart are deliberately not stored. They
//! are computed from `chart_entry` by the `song_stats` view, so they
//! stay correct by construction instead of being scraped out of markup
//! that Billboard is free to change.

use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;

use anyhow::{Context, Result};
use sqlx::sqlite::{SqliteConnectOptions, SqlitePoolOptions};
use sqlx::{Row, SqlitePool};

use crate::chart::ChartEntry;
use crate::norm;

const SCHEMA: &str = r#"
CREATE TABLE IF NOT EXISTS song (
    id             INTEGER PRIMARY KEY,
    title_key      TEXT NOT NULL,
    artist_key     TEXT NOT NULL,
    title          TEXT NOT NULL,
    artist         TEXT NOT NULL,
    primary_artist TEXT NOT NULL,
    UNIQUE (title_key, artist_key)
);

CREATE TABLE IF NOT EXISTS chart_entry (
    chart      TEXT    NOT NULL,
    chart_date TEXT    NOT NULL,
    rank       INTEGER NOT NULL,
    song_id    INTEGER NOT NULL REFERENCES song(id),
    PRIMARY KEY (chart, chart_date, rank)
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS idx_entry_song ON chart_entry (song_id);
CREATE INDEX IF NOT EXISTS idx_entry_date ON chart_entry (chart_date);

-- Fetch ledger. Presence of a row means "we asked"; `entries` may be 0
-- for a week a chart did not publish.
CREATE TABLE IF NOT EXISTS chart_week (
    chart      TEXT    NOT NULL,
    chart_date TEXT    NOT NULL,
    entries    INTEGER NOT NULL,
    fetched_at TEXT    NOT NULL,
    PRIMARY KEY (chart, chart_date)
) WITHOUT ROWID;

-- Everything derivable about a song's chart life, derived rather than
-- stored so it cannot drift from the observations underneath it.
CREATE VIEW IF NOT EXISTS song_stats AS
SELECT
    s.id                                   AS song_id,
    s.title                                AS title,
    s.artist                               AS artist,
    s.primary_artist                       AS primary_artist,
    MIN(e.rank)                            AS best_rank,
    MIN(e.chart_date)                      AS first_charted,
    MAX(e.chart_date)                      AS last_charted,
    CAST(substr(MIN(e.chart_date), 1, 4) AS INTEGER) AS first_year,
    COUNT(*)                               AS chart_weeks,
    COUNT(DISTINCT e.chart)                AS chart_count,
    MIN(CASE WHEN e.chart = 'hot-100' THEN e.rank END) AS hot100_peak,
    SUM(CASE WHEN e.chart = 'hot-100' THEN 1 ELSE 0 END) AS hot100_weeks,
    (SELECT group_concat(DISTINCT g.chart)
       FROM chart_entry g
      WHERE g.song_id = s.id AND g.chart <> 'hot-100') AS genre_charts
FROM song s
JOIN chart_entry e ON e.song_id = s.id
GROUP BY s.id;
"#;

/// A handle on the corpus database.
pub struct Store {
    pool: SqlitePool,
}

impl Store {
    /// Open (creating if absent) the corpus at `path` and apply the
    /// schema.
    pub async fn open(path: &Path) -> Result<Store> {
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                std::fs::create_dir_all(parent)
                    .with_context(|| format!("creating {}", parent.display()))?;
            }
        }

        let opts = SqliteConnectOptions::from_str(&format!("sqlite://{}", path.display()))
            .with_context(|| format!("bad database path {}", path.display()))?
            .create_if_missing(true)
            // Bulk ingest is tens of thousands of small inserts; WAL
            // plus relaxed sync turns that from minutes into seconds,
            // and a torn corpus is re-ingestable rather than precious.
            .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal)
            .synchronous(sqlx::sqlite::SqliteSynchronous::Normal)
            .busy_timeout(std::time::Duration::from_secs(30));

        let pool = SqlitePoolOptions::new()
            .max_connections(4)
            .connect_with(opts)
            .await
            .context("opening the corpus database")?;

        for stmt in SCHEMA.split(";\n") {
            if stmt.trim().is_empty() {
                continue;
            }
            sqlx::query(stmt)
                .execute(&pool)
                .await
                .with_context(|| format!("applying schema statement: {}", stmt.trim()))?;
        }

        Ok(Store { pool })
    }

    pub fn pool(&self) -> &SqlitePool {
        &self.pool
    }

    /// Which `(chart, date)` pairs have already been fetched, so a
    /// re-run can skip them.
    pub async fn fetched_weeks(&self, chart: &str) -> Result<Vec<String>> {
        let rows = sqlx::query("SELECT chart_date FROM chart_week WHERE chart = ?")
            .bind(chart)
            .fetch_all(&self.pool)
            .await
            .context("reading the fetch ledger")?;
        Ok(rows.iter().map(|r| r.get::<String, _>(0)).collect())
    }

    /// Insert a batch of chart entries, creating song rows as needed.
    ///
    /// Runs as one transaction, and memoises the song-id lookup within
    /// the batch — an ingest of the whole Hot 100 archive is ~185,000
    /// entries over ~30,000 distinct songs, so the memo saves the large
    /// majority of the round-trips.
    pub async fn insert_entries(&self, entries: &[ChartEntry]) -> Result<InsertStats> {
        let mut tx = self.pool.begin().await?;
        let mut ids: HashMap<(String, String), i64> = HashMap::new();
        let mut stats = InsertStats::default();

        for entry in entries {
            let tkey = norm::title_key(&entry.title);
            let akey = norm::artist_key(&entry.artist);

            let song_id = match ids.get(&(tkey.clone(), akey.clone())) {
                Some(id) => *id,
                None => {
                    let id = sqlx::query(
                        "INSERT INTO song (title_key, artist_key, title, artist, primary_artist)
                         VALUES (?, ?, ?, ?, ?)
                         ON CONFLICT (title_key, artist_key) DO UPDATE SET title = title
                         RETURNING id",
                    )
                    .bind(&tkey)
                    .bind(&akey)
                    .bind(entry.title.trim())
                    .bind(entry.artist.trim())
                    .bind(norm::primary_artist(&entry.artist))
                    .fetch_one(&mut *tx)
                    .await
                    .with_context(|| format!("upserting song {:?}", entry.title))?
                    .get::<i64, _>(0);
                    ids.insert((tkey, akey), id);
                    id
                }
            };

            let done = sqlx::query(
                "INSERT INTO chart_entry (chart, chart_date, rank, song_id)
                 VALUES (?, ?, ?, ?)
                 ON CONFLICT (chart, chart_date, rank) DO NOTHING",
            )
            .bind(entry.chart.slug())
            .bind(&entry.date)
            .bind(entry.rank)
            .bind(song_id)
            .execute(&mut *tx)
            .await
            .context("inserting chart entry")?;

            if done.rows_affected() > 0 {
                stats.entries += 1;
            }
        }

        stats.songs_seen = ids.len();
        tx.commit().await.context("committing chart entries")?;
        Ok(stats)
    }

    /// Record that a chart-week was fetched.
    pub async fn mark_week(&self, chart: &str, date: &str, entries: usize) -> Result<()> {
        sqlx::query(
            "INSERT INTO chart_week (chart, chart_date, entries, fetched_at)
             VALUES (?, ?, ?, datetime('now'))
             ON CONFLICT (chart, chart_date) DO UPDATE
               SET entries = excluded.entries, fetched_at = excluded.fetched_at",
        )
        .bind(chart)
        .bind(date)
        .bind(entries as i64)
        .execute(&self.pool)
        .await
        .context("marking chart week fetched")?;
        Ok(())
    }

    /// Headline counts for the `status` command.
    pub async fn summary(&self) -> Result<Vec<(String, i64, i64, String, String)>> {
        let rows = sqlx::query(
            "SELECT e.chart,
                    COUNT(DISTINCT e.chart_date) AS weeks,
                    COUNT(DISTINCT e.song_id)    AS songs,
                    MIN(e.chart_date)            AS first,
                    MAX(e.chart_date)            AS last
               FROM chart_entry e
              GROUP BY e.chart
              ORDER BY songs DESC",
        )
        .fetch_all(&self.pool)
        .await
        .context("summarising the corpus")?;

        Ok(rows
            .iter()
            .map(|r| {
                (
                    r.get::<String, _>(0),
                    r.get::<i64, _>(1),
                    r.get::<i64, _>(2),
                    r.get::<String, _>(3),
                    r.get::<String, _>(4),
                )
            })
            .collect())
    }

    pub async fn song_count(&self) -> Result<i64> {
        Ok(sqlx::query("SELECT COUNT(*) FROM song")
            .fetch_one(&self.pool)
            .await?
            .get::<i64, _>(0))
    }
}

/// What one [`Store::insert_entries`] batch actually changed.
#[derive(Debug, Default, Clone, Copy)]
pub struct InsertStats {
    /// Chart entries newly inserted (already-present ones are skipped).
    pub entries: usize,
    /// Distinct songs touched by the batch.
    pub songs_seen: usize,
}
