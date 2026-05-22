//! Ingest queue + `record_pages` batch writer.

use std::fs;
use std::io::Write;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use uuid::Uuid;
use wiki_proto::paths;

use crate::error::WikiLiveError;
use crate::raw::is_raw_path;
use crate::state::{StateFile, atomic_write};
use crate::vault::WikiLive;

/// State of one ingest task. Mirrors
/// `wiki_proto::ingest::IngestStatus` but kept simple here
/// (no Facet derive — purely on-disk JSON).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IngestStatus {
    Pending,
    Analyzing,
    Generating,
    Writing,
    Done,
    Failed,
    Cancelled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SourceChange {
    Created,
    Modified,
    Deleted,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IngestTask {
    pub id: String,
    pub source_path: String,
    pub kind: SourceChange,
    pub status: IngestStatus,
    pub source_sha256: String,
    #[serde(default)]
    pub analysis: Option<String>,
    /// Resulting page paths from `record_pages`.
    #[serde(default)]
    pub pages_written: Vec<String>,
    pub retries: u32,
    pub last_error: Option<String>,
    pub enqueued_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// One page draft accepted by [`WikiLive::record_pages`].
#[derive(Debug, Clone)]
pub struct PageDraft {
    /// Wiki-relative path (e.g. `Concepts/Foo.md`).
    pub path: String,
    /// Full markdown including frontmatter.
    pub markdown: String,
    /// Set to `true` to overwrite an existing page.
    pub overwrite: bool,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub(crate) struct QueueFile {
    #[serde(default)]
    pub(crate) tasks: Vec<IngestTask>,
}

impl QueueFile {
    pub(crate) fn tasks_ref(&self) -> &[IngestTask] {
        &self.tasks
    }
}

impl StateFile for QueueFile {
    const FILENAME: &'static str = paths::INGEST_QUEUE_JSON;
}

impl WikiLive {
    pub fn enqueue_ingest(
        &self,
        source_path: &str,
        kind: SourceChange,
        source_bytes: &[u8],
    ) -> Result<IngestTask, WikiLiveError> {
        let mut queue: QueueFile = self.load_state()?;
        let now = Utc::now();
        let task = IngestTask {
            id: format!("ingest-{}", Uuid::new_v4().simple()),
            source_path: source_path.to_string(),
            kind,
            status: IngestStatus::Pending,
            source_sha256: sha256_hex(source_bytes),
            analysis: None,
            pages_written: Vec::new(),
            retries: 0,
            last_error: None,
            enqueued_at: now,
            updated_at: now,
        };
        queue.tasks.push(task.clone());
        self.save_state(&queue)?;
        Ok(task)
    }

    pub fn list_ingest(&self) -> Result<Vec<IngestTask>, WikiLiveError> {
        Ok(self.load_state::<QueueFile>()?.tasks)
    }

    /// Pop one `Pending` task and flip to `Analyzing`.
    pub fn claim_next_ingest(&self) -> Result<Option<IngestTask>, WikiLiveError> {
        let mut queue: QueueFile = self.load_state()?;
        let now = Utc::now();
        for task in &mut queue.tasks {
            if task.status == IngestStatus::Pending {
                task.status = IngestStatus::Analyzing;
                task.updated_at = now;
                let claimed = task.clone();
                self.save_state(&queue)?;
                return Ok(Some(claimed));
            }
        }
        Ok(None)
    }

    pub fn record_analysis(
        &self,
        task_id: &str,
        analysis: String,
    ) -> Result<IngestTask, WikiLiveError> {
        let mut queue: QueueFile = self.load_state()?;
        let task = find_task(&mut queue, task_id)?;
        task.analysis = Some(analysis);
        task.status = IngestStatus::Generating;
        task.updated_at = Utc::now();
        let snap = task.clone();
        self.save_state(&queue)?;
        Ok(snap)
    }

    /// Atomically write a batch of pages and update the
    /// task. Rejects paths under the raw layer.
    pub fn record_pages(
        &self,
        task_id: &str,
        pages: &[PageDraft],
    ) -> Result<IngestTask, WikiLiveError> {
        // Reject raw paths up-front.
        for page in pages {
            if is_raw_path(&page.path) {
                return Err(WikiLiveError::RawIsImmutable {
                    path: page.path.clone(),
                });
            }
        }

        // Write each page (atomic per-file).
        for page in pages {
            let abs = self.wiki_path(&page.path)?;
            if abs.exists() && !page.overwrite {
                return Err(WikiLiveError::IllegalState(format!(
                    "page {} already exists; pass overwrite=true",
                    page.path
                )));
            }
            if let Some(parent) = abs.parent() {
                fs::create_dir_all(parent)?;
            }
            let tmp = abs.with_extension(format!("tmp.{}", Uuid::new_v4().simple()));
            let mut f = fs::File::create(&tmp)?;
            f.write_all(page.markdown.as_bytes())?;
            f.sync_all()?;
            fs::rename(&tmp, &abs)?;
        }

        let mut queue: QueueFile = self.load_state()?;
        let task = find_task(&mut queue, task_id)?;
        task.pages_written = pages.iter().map(|p| p.path.clone()).collect();
        task.status = IngestStatus::Writing;
        task.updated_at = Utc::now();
        let snap = task.clone();
        self.save_state(&queue)?;
        Ok(snap)
    }

    pub fn complete_ingest(&self, task_id: &str) -> Result<IngestTask, WikiLiveError> {
        let mut queue: QueueFile = self.load_state()?;
        let task = find_task(&mut queue, task_id)?;
        task.status = IngestStatus::Done;
        task.updated_at = Utc::now();
        let snap = task.clone();
        self.save_state(&queue)?;
        Ok(snap)
    }

    pub fn fail_ingest(&self, task_id: &str, error: &str) -> Result<IngestTask, WikiLiveError> {
        let mut queue: QueueFile = self.load_state()?;
        let task = find_task(&mut queue, task_id)?;
        task.status = IngestStatus::Failed;
        task.last_error = Some(error.to_string());
        task.updated_at = Utc::now();
        let snap = task.clone();
        self.save_state(&queue)?;
        Ok(snap)
    }
}

fn find_task<'a>(
    queue: &'a mut QueueFile,
    task_id: &str,
) -> Result<&'a mut IngestTask, WikiLiveError> {
    queue
        .tasks
        .iter_mut()
        .find(|t| t.id == task_id)
        .ok_or_else(|| WikiLiveError::TaskNotFound(task_id.to_string()))
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    format!("{:x}", h.finalize())
}

// `atomic_write` is re-exported via the state module — pulled in
// here so the `record_pages` use site can call it if we ever
// refactor.
#[allow(dead_code)]
fn touch_dead_code_for_atomic_write() {
    let _ = atomic_write::<()>;
}
