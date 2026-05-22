//! `SQLite` schema. Disposable: the maildir on disk is canonical;
//! `Store::rebuild_from_disk` reconstructs every row by walking
//! the tree.

pub const SCHEMA_V1: &str = r#"
CREATE TABLE IF NOT EXISTS messages (
    message_id   TEXT PRIMARY KEY,
    folder       TEXT NOT NULL,
    thread_id    TEXT,
    subject      TEXT NOT NULL,
    from_addr    TEXT NOT NULL,
    to_addrs     TEXT NOT NULL,
    cc_addrs     TEXT NOT NULL,
    date_ms      INTEGER NOT NULL,
    flags        TEXT NOT NULL,
    size         INTEGER NOT NULL,
    has_atts     INTEGER NOT NULL,
    snippet      TEXT,
    path         TEXT,
    content_hash TEXT
);

CREATE INDEX IF NOT EXISTS idx_messages_folder_date
    ON messages(folder, date_ms DESC);

CREATE INDEX IF NOT EXISTS idx_messages_thread
    ON messages(thread_id);

-- FTS5 over the queryable text fields. Standard content table
-- (NOT contentless) so DELETE works without the special
-- `INSERT … VALUES('delete', …)` dance. We manage the rowid
-- ourselves via a stable hash of message_id so DELETE +
-- upsert stay symmetric. Cost is ~2x storage on the text
-- columns vs a contentless table — acceptable for a cache.
CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
    subject, from_addr, to_addrs, body_text,
    tokenize='unicode61 remove_diacritics 2'
);

CREATE TABLE IF NOT EXISTS threads (
    thread_id TEXT PRIMARY KEY,
    subject   TEXT NOT NULL,
    last_date INTEGER NOT NULL
);

-- Outbound queue: drafts + flag deltas + moves the sync layer
-- wants to replay when the backend comes back online. Lives in
-- the cache because the maildir doesn't have a natural place to
-- record "this needs to be replayed."
CREATE TABLE IF NOT EXISTS pending_ops (
    id        INTEGER PRIMARY KEY AUTOINCREMENT,
    op_kind   TEXT NOT NULL,
    op_json   TEXT NOT NULL,
    queued_at INTEGER NOT NULL,
    last_err  TEXT
);
"#;

/// Schema version. Bump + add a migration when the layout
/// changes; for now the index is treated as disposable.
pub const SCHEMA_VERSION: i64 = 1;
