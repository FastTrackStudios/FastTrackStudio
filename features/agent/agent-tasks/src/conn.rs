//! Connection wrapper. Opens a SQLite file, applies the schema,
//! and turns it into row-oriented helpers the store uses.

use std::path::Path;

use chrono::{DateTime, TimeZone, Utc};
use rusqlite::Connection;

use crate::error::TasksDbError;
use crate::schema::SCHEMA_SQL;

pub(crate) fn open(path: &Path) -> Result<Connection, TasksDbError> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            TasksDbError::Sqlite(rusqlite::Error::ToSqlConversionFailure(Box::new(e)))
        })?;
    }
    let conn = Connection::open(path)?;
    // FK enforcement is opt-in per-connection in SQLite.
    conn.execute_batch("PRAGMA foreign_keys = ON;")?;
    // WAL gives us readers-don't-block-writers, important once
    // the live-update tail reader runs alongside writes.
    conn.execute_batch("PRAGMA journal_mode = WAL;")?;
    conn.execute_batch(SCHEMA_SQL)?;
    Ok(conn)
}

pub(crate) fn now_ms() -> i64 {
    Utc::now().timestamp_millis()
}

pub(crate) fn from_ms(ms: i64) -> DateTime<Utc> {
    Utc.timestamp_millis_opt(ms)
        .single()
        .unwrap_or_else(Utc::now)
}

pub(crate) fn from_ms_opt(ms: Option<i64>) -> Option<DateTime<Utc>> {
    ms.map(from_ms)
}

pub(crate) fn to_ms_opt(ts: Option<DateTime<Utc>>) -> Option<i64> {
    ts.map(|t| t.timestamp_millis())
}
