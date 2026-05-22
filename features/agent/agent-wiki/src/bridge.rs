//! Orchestration helpers — sequences agent turns with
//! `wiki_live::WikiLive` writes to run whole pipelines.
//!
//! Today: ingest. Lint / dedup / research follow the same
//! pattern once their parser bodies land.
//!
//! Bridge takes a concrete `&CodexBackend` for now;
//! once a second backend (Hermes) lands the signature
//! generalizes to `A: Sessions + TurnDispatch +
//! Subscriptions`.

use std::collections::HashMap;
use std::time::Duration;

use agent_codex::{ChatOpts, CodexBackend};
use agent_proto::event::AgentEvent;
use chrono::Utc;
use futures::StreamExt;
use wiki_live::WikiLive;

use crate::error::AgentWikiError;
use crate::parsers::{IngestBlocks, parse_ingest_blocks};
use crate::prompts::{self, INGEST_ANALYZE_SYSTEM, INGEST_GENERATE_SYSTEM, language_directive};

/// Input to one ingest run.
#[derive(Debug, Clone)]
pub struct IngestRequest {
    /// Filename to record under `Wiki/raw/sources/`.
    pub source_filename: String,
    pub source_mime: String,
    /// Optional title (used in log + analysis prompt).
    pub source_title: String,
    pub source_bytes: Vec<u8>,
    /// Model id (`gpt-5.4-mini`, `o3`, ...). `None` ⇒
    /// daemon default.
    pub model: Option<String>,
    /// Per-turn timeout. Default 5 minutes.
    pub timeout: Duration,
    /// Output language (`English`, `中文`, ...).
    pub language: String,
}

impl IngestRequest {
    pub fn new(filename: impl Into<String>, bytes: Vec<u8>) -> Self {
        Self {
            source_filename: filename.into(),
            source_mime: "text/markdown".to_string(),
            source_title: String::new(),
            source_bytes: bytes,
            model: None,
            timeout: Duration::from_secs(300),
            language: "English".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IngestRunResult {
    pub task_id: String,
    pub raw_source_path: String,
    pub pages_written: Vec<String>,
    pub reviews_raised: Vec<String>,
    pub analysis: String,
}

/// Drive one full ingest pipeline (two-step CoT) against
/// `codex app-server`.
pub async fn run_ingest(
    backend: &CodexBackend,
    wiki: &WikiLive,
    req: IngestRequest,
) -> Result<IngestRunResult, AgentWikiError> {
    wiki.bootstrap()
        .map_err(|e| AgentWikiError::Bridge(format!("bootstrap: {e}")))?;

    let raw_ref = wiki
        .import_raw_source(wiki_proto::raw::ImportRawSource {
            filename: req.source_filename.clone(),
            mime: req.source_mime.clone(),
            title: req.source_title.clone(),
            bytes: req.source_bytes.clone(),
            auto_enqueue: false,
        })
        .map_err(|e| AgentWikiError::Bridge(format!("import_raw_source: {e}")))?;

    let task = wiki
        .enqueue_ingest(
            &raw_ref.path,
            wiki_live::queue::SourceChange::Created,
            &req.source_bytes,
        )
        .map_err(|e| AgentWikiError::Bridge(format!("enqueue_ingest: {e}")))?;

    let claimed = wiki
        .claim_next_ingest()
        .map_err(|e| AgentWikiError::Bridge(format!("claim_next_ingest: {e}")))?
        .ok_or_else(|| AgentWikiError::Bridge("claim_next_ingest returned None".into()))?;
    debug_assert_eq!(claimed.id, task.id);

    let ctx = wiki
        .read_context()
        .map_err(|e| AgentWikiError::Bridge(format!("read_context: {e}")))?;

    // ── Step 1: analyze ────────────────────────────────
    let lang = language_directive(&req.language);
    let mut vars1 = HashMap::new();
    vars1.insert("language_directive", lang.as_str());
    vars1.insert("wiki_purpose", ctx.purpose_markdown.as_str());
    vars1.insert("wiki_index", ctx.index_markdown.as_str());
    let analyze_system = prompts::render(INGEST_ANALYZE_SYSTEM, &vars1);

    let source_text = String::from_utf8_lossy(&req.source_bytes).to_string();
    let user1 = format!(
        "Source filename: {}\n\nSource content:\n\n{source_text}",
        req.source_filename
    );
    let full_msg_1 = format!("{analyze_system}\n\n---\n\n{user1}");
    let analysis = drive_one_turn(backend, wiki, &req, &full_msg_1).await?;

    wiki.record_analysis(&task.id, analysis.clone())
        .map_err(|e| AgentWikiError::Bridge(format!("record_analysis: {e}")))?;

    // ── Step 2: generate ───────────────────────────────
    let source_basename = req
        .source_filename
        .rsplit_once('.')
        .map(|(stem, _)| stem)
        .unwrap_or(req.source_filename.as_str());
    let mut vars2 = HashMap::new();
    vars2.insert("language_directive", lang.as_str());
    vars2.insert("source_filename", req.source_filename.as_str());
    vars2.insert("source_basename", source_basename);
    vars2.insert("wiki_schema", ctx.schema_markdown.as_str());
    vars2.insert("wiki_purpose", ctx.purpose_markdown.as_str());
    vars2.insert("wiki_index", ctx.index_markdown.as_str());
    vars2.insert("wiki_overview", ctx.overview_markdown.as_str());
    let generate_system = prompts::render(INGEST_GENERATE_SYSTEM, &vars2);

    let user2 = format!(
        "Analysis:\n\n{analysis}\n\nNow emit FILE/REVIEW blocks per the rules. \
         Remember: the FIRST character of your response must be `-` (start of `---FILE:`)."
    );
    let full_msg_2 = format!("{generate_system}\n\n---\n\n{user2}");
    let generation = drive_one_turn(backend, wiki, &req, &full_msg_2).await?;

    let blocks: IngestBlocks = parse_ingest_blocks(&generation)?;

    let drafts: Vec<wiki_live::queue::PageDraft> = blocks
        .files
        .iter()
        .map(|fb| wiki_live::queue::PageDraft {
            path: fb.path.clone(),
            markdown: fb.content.clone(),
            overwrite: true,
        })
        .collect();
    wiki.record_pages(&task.id, &drafts)
        .map_err(|e| AgentWikiError::Bridge(format!("record_pages: {e}")))?;

    let title = if req.source_title.is_empty() {
        req.source_filename.clone()
    } else {
        req.source_title.clone()
    };
    wiki.append_log(wiki_live::log_md::LogEntry {
        at: Utc::now(),
        op: wiki_live::log_md::LogOp::Ingest,
        title,
        body: format!(
            "Ingested via agent-wiki bridge.\n\n- Source: `{}`\n- Pages: {}\n- Reviews: {}",
            raw_ref.path,
            blocks.files.len(),
            blocks.reviews.len()
        ),
        pages_touched: blocks.files.iter().map(|f| f.path.clone()).collect(),
    })
    .map_err(|e| AgentWikiError::Bridge(format!("append_log: {e}")))?;

    wiki.rebuild_index()
        .map_err(|e| AgentWikiError::Bridge(format!("rebuild_index: {e}")))?;

    wiki.complete_ingest(&task.id)
        .map_err(|e| AgentWikiError::Bridge(format!("complete_ingest: {e}")))?;

    Ok(IngestRunResult {
        task_id: task.id,
        raw_source_path: raw_ref.path,
        pages_written: blocks.files.into_iter().map(|f| f.path).collect(),
        reviews_raised: blocks.reviews.into_iter().map(|r| r.title).collect(),
        analysis,
    })
}

/// Drive one Codex turn and return the full assistant
/// response (concatenated `MessageDelta`s) when the turn
/// completes.
async fn drive_one_turn(
    backend: &CodexBackend,
    wiki: &WikiLive,
    req: &IngestRequest,
    user_text: &str,
) -> Result<String, AgentWikiError> {
    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model: req.model.clone(),
        effort: None,
        access_mode: Some("current".to_string()),
    };
    let handle = backend
        .chat(wiki.vault_root().to_path_buf(), user_text.to_string(), opts)
        .await
        .map_err(|e| AgentWikiError::Bridge(format!("backend.chat: {e}")))?;
    let mut events = handle.events;
    let mut out = String::new();
    let deadline = tokio::time::Instant::now() + req.timeout;
    loop {
        match tokio::time::timeout_at(deadline, events.next()).await {
            Err(_) => {
                return Err(AgentWikiError::Bridge(format!(
                    "turn timed out after {}s",
                    req.timeout.as_secs()
                )));
            }
            Ok(None) => break,
            Ok(Some(AgentEvent::MessageDelta { content_delta, .. })) => {
                out.push_str(&content_delta);
            }
            Ok(Some(AgentEvent::TurnFinished { .. })) => break,
            Ok(Some(AgentEvent::TurnErrored { kind, message, .. })) => {
                return Err(AgentWikiError::Bridge(format!(
                    "turn errored ({kind}): {message}"
                )));
            }
            Ok(Some(_)) => {}
        }
    }
    Ok(out)
}

// ── Future bridges (lint, research, dedup, sweep) ──

pub fn run_lint(_wiki_id: &str, _session_id: &str) -> Result<u32, AgentWikiError> {
    todo!()
}

pub fn run_propose_research(
    _wiki_id: &str,
    _session_id: &str,
    _gap_id: &str,
) -> Result<crate::parsers::ResearchTopicPlan, AgentWikiError> {
    todo!()
}

pub fn run_sweep_reviews(_wiki_id: &str, _session_id: &str) -> Result<Vec<String>, AgentWikiError> {
    todo!()
}

pub fn run_dedup_detect(
    _wiki_id: &str,
    _session_id: &str,
) -> Result<Vec<crate::parsers::DuplicateGroup>, AgentWikiError> {
    todo!()
}

pub fn run_dedup_merge(
    _wiki_id: &str,
    _session_id: &str,
    _group: &crate::parsers::DuplicateGroup,
) -> Result<String, AgentWikiError> {
    todo!()
}
