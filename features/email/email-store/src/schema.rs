//! SQLite index schema. Disposable: the maildir on disk is
//! canonical; the index can be rebuilt by walking it.

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
    path         TEXT NOT NULL,
    content_hash TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_messages_folder_date
    ON messages(folder, date_ms DESC);

CREATE INDEX IF NOT EXISTS idx_messages_thread
    ON messages(thread_id);

CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
    subject, from_addr, to_addrs, body_text,
    content='', tokenize='unicode61 remove_diacritics 2'
);

CREATE TABLE IF NOT EXISTS threads (
    thread_id TEXT PRIMARY KEY,
    subject   TEXT NOT NULL,
    last_date INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS pending_ops (
    id        INTEGER PRIMARY KEY AUTOINCREMENT,
    op_json   TEXT NOT NULL,
    queued_at INTEGER NOT NULL,
    last_err  TEXT
);
"#;
