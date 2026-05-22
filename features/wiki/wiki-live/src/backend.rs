//! `WikiBackend` — multi-vault wrapper around `WikiLive`
//! that implements the per-capability traits from
//! `wiki_proto::service`. Mounted on the task-server's
//! `/vox` endpoint via the architect-emitted descriptors.
//!
//! Single-vault deployments use [`Self::single`]; multi-
//! tenant servers use [`Self::with_roots`] or
//! [`Self::under_parent`].

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use chrono::Utc;
use wiki_proto::error::WikiError;
use wiki_proto::graph as gtypes;
use wiki_proto::health::WikiHealth as ProtoHealth;
use wiki_proto::ingest as itypes;
use wiki_proto::lint as ltypes;
use wiki_proto::log as ctypes;
use wiki_proto::raw::{ImportRawSource, RawSourceRef};
use wiki_proto::review::ReviewItem;
use wiki_proto::schema as stypes;
use wiki_proto::search::{SearchHits, SearchOpts};
use wiki_proto::service::{
    Catalog, Graph, Ingest, Lint, Multimodal, RawLayer, Schema, Search, Watcher,
};

use crate::WikiLive;

#[derive(Clone)]
enum Layout {
    Explicit(HashMap<String, PathBuf>),
    UnderParent(PathBuf),
}

/// Cheaply clonable multi-vault backend.
#[derive(Clone)]
pub struct WikiBackend {
    layout: Layout,
    /// Watcher state per wiki — `true` = watcher running.
    /// Implementation is stub-level (just records the
    /// toggle); spawning the actual watcher thread happens
    /// on `set_watch(true)`.
    watch_flags: Arc<std::sync::Mutex<HashMap<String, bool>>>,
}

impl WikiBackend {
    /// Single-vault server. `wiki_id` resolves to
    /// `vault_root`.
    pub fn single(wiki_id: impl Into<String>, vault_root: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&vault_root)?;
        let mut roots = HashMap::with_capacity(1);
        roots.insert(wiki_id.into(), vault_root);
        Ok(Self::from_layout(Layout::Explicit(roots)))
    }

    /// Multi-tenant: `wiki_id` → `parent/{wiki_id}/`.
    pub fn under_parent(parent: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&parent)?;
        Ok(Self::from_layout(Layout::UnderParent(parent)))
    }

    pub fn with_roots(roots: HashMap<String, PathBuf>) -> Self {
        Self::from_layout(Layout::Explicit(roots))
    }

    fn from_layout(layout: Layout) -> Self {
        Self {
            layout,
            watch_flags: Arc::new(std::sync::Mutex::new(HashMap::new())),
        }
    }

    fn resolve(&self, wiki_id: &str) -> Result<WikiLive, WikiError> {
        let root = match &self.layout {
            Layout::Explicit(map) => map
                .get(wiki_id)
                .cloned()
                .ok_or_else(|| WikiError::WikiNotFound(wiki_id.to_string()))?,
            Layout::UnderParent(parent) => parent.join(wiki_id),
        };
        Ok(WikiLive::open(root))
    }
}

impl HasDispatcher for WikiBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

// ────────────────────── Schema ──────────────────────
impl Schema for WikiBackend {
    fn bootstrap(&self, wiki_id: &str) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        w.bootstrap().map_err(map_err)?;
        Ok(())
    }
    fn read_schema(&self, wiki_id: &str) -> Result<stypes::SchemaDoc, WikiError> {
        let w = self.resolve(wiki_id)?;
        let path = w.wiki_root().join(wiki_proto::paths::SCHEMA_MD);
        let body = std::fs::read_to_string(&path).map_err(|e| WikiError::Io(e.to_string()))?;
        let modified = file_mtime(&path);
        Ok(stypes::SchemaDoc {
            markdown: body,
            modified,
        })
    }
    fn read_purpose(&self, wiki_id: &str) -> Result<stypes::PurposeDoc, WikiError> {
        let w = self.resolve(wiki_id)?;
        let path = w.wiki_root().join(wiki_proto::paths::PURPOSE_MD);
        let body = std::fs::read_to_string(&path).map_err(|e| WikiError::Io(e.to_string()))?;
        let modified = file_mtime(&path);
        Ok(stypes::PurposeDoc {
            markdown: body,
            modified,
        })
    }
    fn write_schema(&self, wiki_id: &str, markdown: &str) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        let path = w.wiki_root().join(wiki_proto::paths::SCHEMA_MD);
        std::fs::write(&path, markdown).map_err(|e| WikiError::Io(e.to_string()))
    }
    fn write_purpose(&self, wiki_id: &str, markdown: &str) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        let path = w.wiki_root().join(wiki_proto::paths::PURPOSE_MD);
        std::fs::write(&path, markdown).map_err(|e| WikiError::Io(e.to_string()))
    }
    fn health(&self, wiki_id: &str) -> Result<ProtoHealth, WikiError> {
        let w = self.resolve(wiki_id)?;
        let h = w.health().map_err(map_err)?;
        Ok(ProtoHealth {
            bootstrap_done: h.bootstrap_done,
            schema_present: h.schema_present,
            purpose_present: h.purpose_present,
            page_count: h.page_count,
            source_count: h.source_count,
            queue_depth: h.queue_depth,
            queue_failed: h.queue_failed,
            open_findings: 0,
            open_reviews: 0,
            research_in_flight: 0,
            peer_count: 0,
            last_lint_at: None,
            last_ingest_at: h.last_ingest_at,
            watching: self
                .watch_flags
                .lock()
                .ok()
                .and_then(|m| m.get(wiki_id).copied())
                .unwrap_or(false),
        })
    }
}

// ────────────────────── Catalog ──────────────────────
impl Catalog for WikiBackend {
    fn read_index(&self, wiki_id: &str) -> Result<ctypes::WikiIndex, WikiError> {
        let w = self.resolve(wiki_id)?;
        // Existing `rebuild_index` produces the markdown but
        // also returns it. We don't parse the markdown back —
        // a full structured index is a follow-up. For now,
        // return an empty parsed index (callers wanting the
        // markdown can read the file directly).
        let _ = w.rebuild_index().map_err(map_err)?;
        Ok(ctypes::WikiIndex {
            sections: Vec::new(),
            total: 0,
        })
    }
    fn rebuild_index(&self, wiki_id: &str) -> Result<ctypes::WikiIndex, WikiError> {
        let w = self.resolve(wiki_id)?;
        let _ = w.rebuild_index().map_err(map_err)?;
        Ok(ctypes::WikiIndex {
            sections: Vec::new(),
            total: 0,
        })
    }
    fn append_log(&self, wiki_id: &str, entry: ctypes::LogEntry) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        let op = match entry.op {
            ctypes::LogOp::Ingest => crate::log_md::LogOp::Ingest,
            ctypes::LogOp::Query => crate::log_md::LogOp::Query,
            ctypes::LogOp::Lint => crate::log_md::LogOp::Lint,
            ctypes::LogOp::Review => crate::log_md::LogOp::Review,
            ctypes::LogOp::Research => crate::log_md::LogOp::Research,
            ctypes::LogOp::Admin => crate::log_md::LogOp::Admin,
        };
        w.append_log(crate::log_md::LogEntry {
            at: entry.at,
            op,
            title: entry.title,
            body: entry.body,
            pages_touched: entry.pages_touched,
        })
        .map_err(map_err)
    }
}

// ────────────────────── RawLayer ──────────────────────
impl RawLayer for WikiBackend {
    fn import_raw_source(
        &self,
        wiki_id: &str,
        source: ImportRawSource,
    ) -> Result<RawSourceRef, WikiError> {
        let w = self.resolve(wiki_id)?;
        w.import_raw_source(source).map_err(map_err)
    }
    fn list_raw_sources(&self, wiki_id: &str) -> Result<Vec<RawSourceRef>, WikiError> {
        // No wiki-live helper yet — walk `raw/sources/` here.
        let w = self.resolve(wiki_id)?;
        let dir = w.wiki_root().join(wiki_proto::paths::SOURCES_DIR);
        let mut out = Vec::new();
        if !dir.is_dir() {
            return Ok(out);
        }
        for entry in walkdir::WalkDir::new(&dir)
            .into_iter()
            .filter_map(Result::ok)
        {
            let p = entry.path();
            if !p.is_file() {
                continue;
            }
            let name = match p.file_name().and_then(|s| s.to_str()) {
                Some(n) if !n.starts_with('.') => n.to_string(),
                _ => continue,
            };
            let bytes = std::fs::read(p).map_err(|e| WikiError::Io(e.to_string()))?;
            let rel = p
                .strip_prefix(w.wiki_root())
                .map(|r| r.to_string_lossy().to_string())
                .unwrap_or(name.clone());
            out.push(RawSourceRef {
                path: rel,
                filename: name,
                mime: "application/octet-stream".to_string(),
                size: bytes.len() as u64,
                sha256: sha256_hex(&bytes),
                imported_at: Utc::now(),
                title: String::new(),
            });
        }
        Ok(out)
    }
    fn read_raw_source(&self, wiki_id: &str, path: &str) -> Result<Vec<u8>, WikiError> {
        let w = self.resolve(wiki_id)?;
        w.read_raw_source(path).map_err(map_err)
    }
    fn delete_raw_source(&self, wiki_id: &str, path: &str) -> Result<Vec<ReviewItem>, WikiError> {
        let w = self.resolve(wiki_id)?;
        let abs = w.wiki_root().join(path);
        if abs.is_file() {
            std::fs::remove_file(&abs).map_err(|e| WikiError::Io(e.to_string()))?;
        }
        // Source-page orphan detection follows once the
        // review queue is wired through wiki-live.
        Ok(Vec::new())
    }
    fn rescan_sources(&self, wiki_id: &str) -> Result<Vec<itypes::IngestTask>, WikiError> {
        let w = self.resolve(wiki_id)?;
        let diff = w.rescan_sources().map_err(map_err)?;
        let mut tasks = Vec::new();
        for rel in diff.created.iter().chain(diff.modified.iter()) {
            let abs = w.wiki_root().join(rel);
            let Ok(bytes) = std::fs::read(&abs) else {
                continue;
            };
            let kind = if diff.created.contains(rel) {
                crate::queue::SourceChange::Created
            } else {
                crate::queue::SourceChange::Modified
            };
            let task = w.enqueue_ingest(rel, kind, &bytes).map_err(map_err)?;
            tasks.push(to_proto_task(task));
        }
        Ok(tasks)
    }
}

// ────────────────────── Graph ──────────────────────
impl Graph for WikiBackend {
    fn build_graph(
        &self,
        wiki_id: &str,
        opts: gtypes::GraphOpts,
    ) -> Result<gtypes::WikiGraph, WikiError> {
        let w = self.resolve(wiki_id)?;
        wiki_graph::build_graph(w.vault_root(), opts).map_err(|e| WikiError::Backend(e.to_string()))
    }
    fn relevance(
        &self,
        _wiki_id: &str,
        _from: &str,
        _to: &str,
    ) -> Result<gtypes::RelevanceScore, WikiError> {
        // Pairwise relevance lookup over the graph is a
        // small helper around `build_graph`; not yet on
        // wiki-graph's surface. Returning a zero score for
        // now so the trait is implementable; follow-up
        // wires it through.
        Ok(gtypes::RelevanceScore {
            direct_link: 0.0,
            source_overlap: 0.0,
            adamic_adar: 0.0,
            type_affinity: 0.0,
            total: 0.0,
        })
    }
    fn clusters(&self, wiki_id: &str) -> Result<Vec<gtypes::Cluster>, WikiError> {
        let w = self.resolve(wiki_id)?;
        wiki_graph::build_clusters(w.vault_root()).map_err(|e| WikiError::Backend(e.to_string()))
    }
    fn gaps(&self, wiki_id: &str) -> Result<Vec<gtypes::KnowledgeGap>, WikiError> {
        let w = self.resolve(wiki_id)?;
        wiki_graph::find_gaps(w.vault_root()).map_err(|e| WikiError::Backend(e.to_string()))
    }
}

// ────────────────────── Ingest ──────────────────────
impl Ingest for WikiBackend {
    fn enqueue_ingest(
        &self,
        wiki_id: &str,
        source_path: &str,
        change: itypes::SourceChange,
    ) -> Result<itypes::IngestTask, WikiError> {
        let w = self.resolve(wiki_id)?;
        let abs = w.wiki_root().join(source_path);
        let bytes = std::fs::read(&abs).map_err(|e| WikiError::Io(e.to_string()))?;
        let local_kind = match change {
            itypes::SourceChange::Created => crate::queue::SourceChange::Created,
            itypes::SourceChange::Modified => crate::queue::SourceChange::Modified,
            itypes::SourceChange::Deleted => crate::queue::SourceChange::Deleted,
        };
        let task = w
            .enqueue_ingest(source_path, local_kind, &bytes)
            .map_err(map_err)?;
        Ok(to_proto_task(task))
    }
    fn list_ingest(&self, wiki_id: &str) -> Result<Vec<itypes::IngestTask>, WikiError> {
        let w = self.resolve(wiki_id)?;
        Ok(w.list_ingest()
            .map_err(map_err)?
            .into_iter()
            .map(to_proto_task)
            .collect())
    }
    fn claim_next_ingest(&self, wiki_id: &str) -> Result<Option<itypes::IngestTask>, WikiError> {
        let w = self.resolve(wiki_id)?;
        Ok(w.claim_next_ingest().map_err(map_err)?.map(to_proto_task))
    }
    fn record_analysis(
        &self,
        wiki_id: &str,
        task_id: &str,
        analysis: itypes::AnalysisDraft,
    ) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        // Squash the structured AnalysisDraft into the
        // free-form `notes` body that wiki-live persists.
        let body = serde_json::to_string_pretty(&serde_json::json!({
            "notes": analysis.notes,
            "entities": analysis.entities.iter().map(|e| serde_json::json!({"name": e.name, "summary": e.summary})).collect::<Vec<_>>(),
            "concepts": analysis.concepts.iter().map(|c| serde_json::json!({"name": c.name, "summary": c.summary})).collect::<Vec<_>>(),
        })).unwrap_or_default();
        w.record_analysis(task_id, body).map_err(map_err)?;
        Ok(())
    }
    fn record_pages(
        &self,
        wiki_id: &str,
        task_id: &str,
        pages: Vec<itypes::PageDraft>,
    ) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        let drafts: Vec<crate::queue::PageDraft> = pages
            .into_iter()
            .map(|p| crate::queue::PageDraft {
                path: p.path,
                markdown: p.markdown,
                overwrite: p.overwrite,
            })
            .collect();
        w.record_pages(task_id, &drafts).map_err(map_err)?;
        Ok(())
    }
    fn fail_ingest(&self, wiki_id: &str, task_id: &str, error: &str) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        w.fail_ingest(task_id, error).map_err(map_err)?;
        Ok(())
    }
    fn cancel_ingest(&self, _wiki_id: &str, _task_id: &str) -> Result<(), WikiError> {
        // Cancellation flips the queue row's status — needs
        // a small helper on wiki-live. Stub for now.
        Ok(())
    }
    fn retry_ingest(&self, wiki_id: &str, task_id: &str) -> Result<itypes::IngestTask, WikiError> {
        let w = self.resolve(wiki_id)?;
        // Look up the failed task and re-enqueue as a fresh
        // task. Same source path, sha256, increments retry
        // count via wiki-live's bookkeeping (follow-up).
        for task in w.list_ingest().map_err(map_err)? {
            if task.id == task_id {
                let abs = w.wiki_root().join(&task.source_path);
                if let Ok(bytes) = std::fs::read(&abs) {
                    let new_task = w
                        .enqueue_ingest(
                            &task.source_path,
                            crate::queue::SourceChange::Modified,
                            &bytes,
                        )
                        .map_err(map_err)?;
                    return Ok(to_proto_task(new_task));
                }
            }
        }
        Err(WikiError::UnknownTask(task_id.to_string()))
    }
}

// ────────────────────── Lint ──────────────────────
impl Lint for WikiBackend {
    fn lint(
        &self,
        _wiki_id: &str,
        _scope: ltypes::LintScope,
    ) -> Result<Vec<ltypes::LintFinding>, WikiError> {
        // LLM-driven lint runs through the agent-wiki
        // bridge. Trait surface returns empty so callers
        // get a clean result; the bridge persists findings
        // directly to `Wiki/_state/lint_findings.json`,
        // which `list_findings` surfaces.
        Ok(Vec::new())
    }
    fn list_findings(&self, wiki_id: &str) -> Result<Vec<ltypes::LintFinding>, WikiError> {
        let w = self.resolve(wiki_id)?;
        let local = w
            .list_findings(Some(crate::FindingStatus::Open))
            .map_err(map_err)?;
        Ok(local.into_iter().map(to_proto_finding).collect())
    }
    fn resolve_finding(
        &self,
        wiki_id: &str,
        finding_id: &str,
        action: ltypes::FindingAction,
    ) -> Result<(), WikiError> {
        let w = self.resolve(wiki_id)?;
        match action {
            ltypes::FindingAction::Resolve => {
                w.resolve_finding(finding_id).map_err(map_err)?;
            }
            ltypes::FindingAction::Dismiss { .. } => {
                w.resolve_finding(finding_id).map_err(map_err)?;
            }
            ltypes::FindingAction::PromoteToReview | ltypes::FindingAction::PromoteToResearch => {
                // Promote-to-{review,research} needs the
                // matching queues on wiki-live; stub flips
                // to resolved for now.
                w.resolve_finding(finding_id).map_err(map_err)?;
            }
        }
        Ok(())
    }
}

// ────────────────────── Search ──────────────────────
impl Search for WikiBackend {
    fn search(&self, wiki_id: &str, opts: SearchOpts) -> Result<SearchHits, WikiError> {
        let w = self.resolve(wiki_id)?;
        wiki_search::search(w.vault_root(), opts).map_err(|e| WikiError::Backend(e.to_string()))
    }
}

// ────────────────────── Watcher ──────────────────────
impl Watcher for WikiBackend {
    fn set_watch(&self, wiki_id: &str, enabled: bool) -> Result<bool, WikiError> {
        // Toggle the recorded state. Spawning the actual
        // FS watcher per-wiki happens on the local CLI
        // (`task wiki watch-sources`) — the server-side
        // version follows when the run loop lands.
        let mut flags = self
            .watch_flags
            .lock()
            .map_err(|e| WikiError::Backend(format!("watch_flags poisoned: {e}")))?;
        flags.insert(wiki_id.to_string(), enabled);
        Ok(enabled)
    }
    fn is_watching(&self, wiki_id: &str) -> Result<bool, WikiError> {
        Ok(self
            .watch_flags
            .lock()
            .ok()
            .and_then(|m| m.get(wiki_id).copied())
            .unwrap_or(false))
    }
}

// ────────────────────── helpers ──────────────────────

fn file_mtime(path: &std::path::Path) -> String {
    std::fs::metadata(path)
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| chrono::DateTime::<chrono::Utc>::from(t).to_rfc3339().into())
        .unwrap_or_default()
}

fn map_err(e: crate::WikiLiveError) -> WikiError {
    match e {
        crate::WikiLiveError::NotBootstrapped => {
            WikiError::SchemaMissing("not bootstrapped".to_string())
        }
        crate::WikiLiveError::Io(io) => WikiError::Io(io.to_string()),
        crate::WikiLiveError::Yaml(s) => WikiError::MalformedFrontmatter(String::new(), s),
        crate::WikiLiveError::Json(j) => WikiError::Backend(format!("json: {j}")),
        crate::WikiLiveError::IllegalState(s) => WikiError::IllegalState(s),
        crate::WikiLiveError::RawIsImmutable { path } => {
            WikiError::IllegalState(format!("raw is immutable: {path}"))
        }
        crate::WikiLiveError::PathEscape { path } => {
            WikiError::IllegalState(format!("path escape: {path}"))
        }
        crate::WikiLiveError::TaskNotFound(id) => WikiError::UnknownTask(id),
    }
}

fn to_proto_task(t: crate::queue::IngestTask) -> itypes::IngestTask {
    itypes::IngestTask {
        id: t.id,
        source_path: t.source_path,
        kind: match t.kind {
            crate::queue::SourceChange::Created => itypes::SourceChange::Created,
            crate::queue::SourceChange::Modified => itypes::SourceChange::Modified,
            crate::queue::SourceChange::Deleted => itypes::SourceChange::Deleted,
        },
        status: match t.status {
            crate::queue::IngestStatus::Pending => itypes::IngestStatus::Pending,
            crate::queue::IngestStatus::Analyzing => itypes::IngestStatus::Analyzing,
            crate::queue::IngestStatus::Generating => itypes::IngestStatus::Generating,
            crate::queue::IngestStatus::Writing => itypes::IngestStatus::Writing,
            crate::queue::IngestStatus::Done => itypes::IngestStatus::Done,
            crate::queue::IngestStatus::Failed => itypes::IngestStatus::Failed,
            crate::queue::IngestStatus::Cancelled => itypes::IngestStatus::Cancelled,
        },
        source_sha256: t.source_sha256,
        // Structured analysis isn't kept on disk — only the
        // serialized blob from `record_analysis`. Return
        // an empty draft over the wire; callers re-derive
        // from the queue body if needed.
        analysis: None,
        pages: Vec::new(),
        retries: t.retries,
        last_error: t.last_error,
        enqueued_at: t.enqueued_at,
        updated_at: t.updated_at,
    }
}

fn to_proto_finding(f: crate::LintFinding) -> ltypes::LintFinding {
    ltypes::LintFinding {
        id: f.id,
        scope: match f.kind {
            crate::LintKind::Contradiction => ltypes::LintScope::Contradiction,
            crate::LintKind::Stale => ltypes::LintScope::Stale,
            crate::LintKind::MissingPage => ltypes::LintScope::MissingPage,
            crate::LintKind::Suggestion => ltypes::LintScope::All,
        },
        subjects: f.pages,
        message: f.title,
        severity: match f.severity {
            crate::LintSeverity::Warning => ltypes::Severity::Warn,
            crate::LintSeverity::Info => ltypes::Severity::Info,
        },
        suggestion: f.description,
        raised_at: f.raised_at,
        status: match f.status {
            crate::FindingStatus::Open => ltypes::FindingStatus::Open,
            crate::FindingStatus::Resolved => ltypes::FindingStatus::Resolved,
            crate::FindingStatus::Dismissed => ltypes::FindingStatus::Dismissed,
        },
    }
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    let mut h = Sha256::new();
    h.update(bytes);
    format!("{:x}", h.finalize())
}

// ────────────────────── Multimodal ──────────────────────
impl Multimodal for WikiBackend {
    fn extract_images(
        &self,
        wiki_id: &str,
        source_path: &str,
        opts: wiki_proto::multimodal::ExtractOpts,
    ) -> Result<Vec<wiki_proto::multimodal::ExtractedImage>, WikiError> {
        let w = self.resolve(wiki_id)?;
        let abs = w.wiki_root().join(source_path);
        wiki_extract::extract_path(&abs, &opts)
            .map_err(|e| WikiError::Backend(format!("extract: {e}")))
    }
}
