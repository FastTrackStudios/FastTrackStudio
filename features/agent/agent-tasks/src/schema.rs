//! Single source of truth for the queue DB schema.
//!
//! Re-applied at every connection open (`CREATE TABLE IF NOT
//! EXISTS`), so adding a column means adding the `ALTER TABLE`
//! here next to the original definition. The DBs are small +
//! single-user, so we don't run a migration framework.

pub const SCHEMA_SQL: &str = r"
CREATE TABLE IF NOT EXISTS queues (
    id           TEXT PRIMARY KEY,
    label        TEXT NOT NULL,
    assignees    TEXT NOT NULL DEFAULT '[]',  -- JSON array
    created_at   INTEGER NOT NULL,            -- unix-ms
    updated_at   INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS agent_tasks (
    id                 TEXT PRIMARY KEY,
    queue_id           TEXT NOT NULL REFERENCES queues(id) ON DELETE CASCADE,
    status             TEXT NOT NULL,         -- triage|ready|running|blocked|done|archived
    title              TEXT NOT NULL,
    description        TEXT NOT NULL DEFAULT '',
    priority           INTEGER NOT NULL DEFAULT 2,
    labels             TEXT NOT NULL DEFAULT '[]',
    source_task        TEXT NOT NULL DEFAULT '',
    project            TEXT NOT NULL DEFAULT '',
    agent_profile      TEXT NOT NULL DEFAULT '',
    claimed_by         TEXT NOT NULL DEFAULT '',
    claim_expires_at   INTEGER,               -- unix-ms; NULL = no expiry
    worker_pid         INTEGER NOT NULL DEFAULT 0,
    result_blob        TEXT NOT NULL DEFAULT '',
    linked_session_id  TEXT NOT NULL DEFAULT '',
    created_at         INTEGER NOT NULL,
    updated_at         INTEGER NOT NULL,
    archived_at        INTEGER                -- unix-ms; NULL until archive_sweep
);

CREATE INDEX IF NOT EXISTS idx_agent_tasks_queue_status
    ON agent_tasks(queue_id, status);
CREATE INDEX IF NOT EXISTS idx_agent_tasks_source_task
    ON agent_tasks(source_task) WHERE source_task != '';

CREATE TABLE IF NOT EXISTS agent_task_comments (
    id                INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_task_id     TEXT NOT NULL REFERENCES agent_tasks(id) ON DELETE CASCADE,
    author            TEXT NOT NULL,
    body              TEXT NOT NULL,
    created_at        INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_comments_task ON agent_task_comments(agent_task_id);

CREATE TABLE IF NOT EXISTS agent_task_links (
    from_task   TEXT NOT NULL REFERENCES agent_tasks(id) ON DELETE CASCADE,
    to_task     TEXT NOT NULL REFERENCES agent_tasks(id) ON DELETE CASCADE,
    kind        INTEGER NOT NULL,                -- AgentTaskLinkKind discriminant
    created_at  INTEGER NOT NULL,
    PRIMARY KEY (from_task, to_task, kind)
);
CREATE INDEX IF NOT EXISTS idx_links_from ON agent_task_links(from_task);
CREATE INDEX IF NOT EXISTS idx_links_to ON agent_task_links(to_task);

-- Append-only event log. Drives the live-update tail clients
-- subscribe to via vox; never trimmed (small, JSON column).
CREATE TABLE IF NOT EXISTS queue_events (
    id            INTEGER PRIMARY KEY AUTOINCREMENT,
    queue_id      TEXT NOT NULL,
    agent_task_id TEXT,                          -- NULL for queue-level events
    kind          TEXT NOT NULL,                 -- created|status|claim|comment|link|done|archived
    payload       TEXT NOT NULL DEFAULT '{}',
    ts            INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_events_queue_id ON queue_events(queue_id, id);
";
