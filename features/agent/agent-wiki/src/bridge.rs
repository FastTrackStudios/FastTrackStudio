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

// ────────────────────── Lint ──────────────────────

#[derive(Debug, Clone)]
pub struct LintRequest {
    pub model: Option<String>,
    pub timeout: Duration,
    pub language: String,
}

impl Default for LintRequest {
    fn default() -> Self {
        Self {
            model: None,
            timeout: Duration::from_secs(180),
            language: "English".to_string(),
        }
    }
}

/// Run one semantic-lint pass. Reads page summaries (title
/// + first ~500 chars), drives the LLM through
/// `LINT_SEMANTIC_SYSTEM`, parses the LINT blocks,
/// persists newly-raised findings.
pub async fn run_lint(
    backend: &CodexBackend,
    wiki: &WikiLive,
    req: LintRequest,
) -> Result<Vec<wiki_live::LintFinding>, AgentWikiError> {
    use crate::parsers::{LintBlockKind, LintSeverity, parse_lint_blocks};

    let pages = collect_page_summaries(wiki)?;
    let lang = language_directive(&req.language);
    let mut vars = HashMap::new();
    vars.insert("language_directive", lang.as_str());
    vars.insert("page_summaries", pages.as_str());
    let system = prompts::render(prompts::LINT_SEMANTIC_SYSTEM, &vars);

    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model: req.model.clone(),
        effort: None,
        access_mode: Some("read-only".to_string()),
    };
    let resp = drive_turn_text(backend, wiki, &system, &opts, req.timeout).await?;
    let blocks = parse_lint_blocks(&resp)?;

    let now = chrono::Utc::now();
    let items = blocks.into_iter().map(|b| wiki_live::LintFinding {
        id: String::new(),
        kind: match b.kind {
            LintBlockKind::Contradiction => wiki_live::LintKind::Contradiction,
            LintBlockKind::Stale => wiki_live::LintKind::Stale,
            LintBlockKind::MissingPage => wiki_live::LintKind::MissingPage,
            LintBlockKind::Suggestion => wiki_live::LintKind::Suggestion,
        },
        severity: match b.severity {
            LintSeverity::Warning => wiki_live::LintSeverity::Warning,
            LintSeverity::Info => wiki_live::LintSeverity::Info,
        },
        title: b.title,
        description: b.description,
        pages: b.pages,
        status: wiki_live::FindingStatus::Open,
        raised_at: now,
        resolved_at: None,
    });
    let raised = wiki
        .raise_findings(items)
        .map_err(|e| AgentWikiError::Bridge(format!("raise_findings: {e}")))?;
    wiki.append_log(wiki_live::log_md::LogEntry {
        at: now,
        op: wiki_live::log_md::LogOp::Lint,
        title: format!("Lint pass — {} new finding(s)", raised.len()),
        body: String::new(),
        pages_touched: Vec::new(),
    })
    .map_err(|e| AgentWikiError::Bridge(format!("append_log: {e}")))?;
    Ok(raised)
}

// ────────────────────── Deep research ──────────────────────

/// Propose a research plan for one knowledge gap. Returns
/// the parsed `TOPIC: …` + queries; caller dispatches the
/// actual web search.
pub async fn run_propose_research(
    backend: &CodexBackend,
    wiki: &WikiLive,
    gap_kind: &str,
    gap_title: &str,
    gap_description: &str,
    model: Option<String>,
    timeout: Duration,
    language: &str,
) -> Result<crate::parsers::ResearchTopicPlan, AgentWikiError> {
    let ctx = wiki
        .read_context()
        .map_err(|e| AgentWikiError::Bridge(format!("read_context: {e}")))?;
    let lang = language_directive(language);
    let mut vars = HashMap::new();
    vars.insert("language_directive", lang.as_str());
    vars.insert("wiki_purpose", ctx.purpose_markdown.as_str());
    vars.insert("wiki_overview", ctx.overview_markdown.as_str());
    vars.insert("gap_type", gap_kind);
    vars.insert("gap_title", gap_title);
    vars.insert("gap_description", gap_description);
    let system = prompts::render(prompts::OPTIMIZE_RESEARCH_SYSTEM, &vars);
    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model,
        effort: None,
        access_mode: Some("read-only".to_string()),
    };
    let resp = drive_turn_text(backend, wiki, &system, &opts, timeout).await?;
    crate::parsers::parse_research_plan(&resp)
}

// ────────────────────── Sweep reviews ──────────────────────

#[derive(Debug, Clone)]
pub struct ReviewSummary {
    pub id: String,
    pub kind: String,
    pub title: String,
    pub description: String,
    pub pages: Vec<String>,
}

/// Ask the LLM which review items are now stale given the
/// current wiki state. Returns ids the curator can mark
/// resolved.
pub async fn run_sweep_reviews(
    backend: &CodexBackend,
    wiki: &WikiLive,
    pending: &[ReviewSummary],
    model: Option<String>,
    timeout: Duration,
) -> Result<Vec<String>, AgentWikiError> {
    let pages = collect_page_index_lines(wiki)?;
    let mut items_block = String::new();
    for item in pending {
        items_block.push_str(&format!(
            "- id: {}\n  kind: {}\n  title: {}\n  description: {}\n  pages: {}\n",
            item.id,
            item.kind,
            item.title,
            item.description,
            item.pages.join(", ")
        ));
    }
    let mut vars = HashMap::new();
    vars.insert("page_list", pages.as_str());
    vars.insert("review_items", items_block.as_str());
    let system = prompts::render(prompts::SWEEP_REVIEWS_SYSTEM, &vars);

    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model,
        effort: None,
        access_mode: Some("read-only".to_string()),
    };
    let resp = drive_turn_text(backend, wiki, &system, &opts, timeout).await?;
    crate::parsers::parse_sweep_resolved(&resp)
}

// ────────────────────── Dedup ──────────────────────

pub async fn run_dedup_detect(
    backend: &CodexBackend,
    wiki: &WikiLive,
    model: Option<String>,
    timeout: Duration,
) -> Result<Vec<crate::parsers::DuplicateGroup>, AgentWikiError> {
    let pages = collect_dedup_input(wiki)?;
    let mut vars = HashMap::new();
    vars.insert("pages", pages.as_str());
    let system = prompts::render(prompts::DEDUP_DETECT_SYSTEM, &vars);
    let user_appendix = format!("Page list:\n\n{pages}");

    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model,
        effort: None,
        access_mode: Some("read-only".to_string()),
    };
    let full = format!("{system}\n\n---\n\n{user_appendix}");
    let resp = drive_turn_text(backend, wiki, &full, &opts, timeout).await?;
    crate::parsers::parse_dedup_groups(&resp)
}

/// Ask the LLM to merge a duplicate group into one page.
/// Returns the resulting `(path, markdown)` ready for
/// `record_pages`. Caller does the write so this fn stays
/// pure.
pub async fn run_dedup_merge(
    backend: &CodexBackend,
    wiki: &WikiLive,
    group: &crate::parsers::DuplicateGroup,
    target_path: &str,
    model: Option<String>,
    timeout: Duration,
) -> Result<(String, String), AgentWikiError> {
    let mut inputs = String::new();
    for slug in &group.slugs {
        // Slug → try a few common locations.
        for candidate in [
            format!("Concepts/{slug}.md"),
            format!("concepts/{slug}.md"),
            format!("Entities/{slug}.md"),
            format!("entities/{slug}.md"),
            format!("{slug}.md"),
        ] {
            let abs = wiki.wiki_root().join(&candidate);
            if abs.is_file() {
                let body = std::fs::read_to_string(&abs)
                    .map_err(|e| AgentWikiError::Bridge(format!("read {candidate}: {e}")))?;
                inputs.push_str(&format!("\n### {candidate}\n\n{body}\n"));
                break;
            }
        }
    }
    let system = prompts::render(prompts::DEDUP_MERGE_SYSTEM, &HashMap::new());
    let user = format!(
        "Pages to merge:\n{inputs}\n\nReason for grouping: {}",
        group.reason
    );

    let opts = ChatOpts {
        codex_bin: None,
        codex_args: None,
        codex_home: None,
        model,
        effort: None,
        access_mode: Some("read-only".to_string()),
    };
    let full = format!("{system}\n\n---\n\n{user}");
    let merged = drive_turn_text(backend, wiki, &full, &opts, timeout).await?;
    // First char must be `-` per the prompt contract.
    let trimmed = merged.trim_start();
    if !trimmed.starts_with("---") {
        return Err(AgentWikiError::MalformedResponse(
            "dedup merge response must start with `---`",
            trimmed.chars().take(80).collect(),
        ));
    }
    Ok((target_path.to_string(), trimmed.to_string()))
}

// ────────────────────── Shared helpers ──────────────────────

/// Drive one turn with an LLM and return its full text
/// (concatenated `MessageDelta`s). The session+task it
/// runs under is throwaway — Lint/Dedup/Research don't
/// need persistent session state.
async fn drive_turn_text(
    backend: &CodexBackend,
    wiki: &WikiLive,
    text: &str,
    opts: &ChatOpts,
    timeout: Duration,
) -> Result<String, AgentWikiError> {
    let handle = backend
        .chat(
            wiki.vault_root().to_path_buf(),
            text.to_string(),
            opts.clone(),
        )
        .await
        .map_err(|e| AgentWikiError::Bridge(format!("backend.chat: {e}")))?;
    let mut events = handle.events;
    let mut out = String::new();
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        match tokio::time::timeout_at(deadline, events.next()).await {
            Err(_) => {
                return Err(AgentWikiError::Bridge(format!(
                    "turn timed out after {}s",
                    timeout.as_secs()
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

/// Build a compact "title — first 500 chars" listing for
/// the lint prompt.
fn collect_page_summaries(wiki: &WikiLive) -> Result<String, AgentWikiError> {
    let root = wiki.wiki_root();
    let mut out = String::new();
    const SKIP: &[&str] = &[
        wiki_proto::paths::SCHEMA_MD,
        wiki_proto::paths::PURPOSE_MD,
        wiki_proto::paths::INDEX_MD,
        wiki_proto::paths::LOG_MD,
        wiki_proto::paths::OVERVIEW_MD,
    ];
    for entry in walkdir::WalkDir::new(&root)
        .into_iter()
        .filter_map(Result::ok)
    {
        let p = entry.path();
        if !p.is_file() || p.extension().and_then(|s| s.to_str()) != Some("md") {
            continue;
        }
        let rel = p
            .strip_prefix(&root)
            .map(|r| r.to_string_lossy().to_string())
            .unwrap_or_default();
        if SKIP.contains(&rel.as_str())
            || rel.starts_with("raw/")
            || rel.starts_with("_state/")
            || rel.starts_with("media/")
        {
            continue;
        }
        let body = std::fs::read_to_string(p)
            .map_err(|e| AgentWikiError::Bridge(format!("read {rel}: {e}")))?;
        let snippet: String = body.chars().take(500).collect();
        out.push_str(&format!("\n## {rel}\n\n{snippet}\n"));
    }
    Ok(out)
}

fn collect_page_index_lines(wiki: &WikiLive) -> Result<String, AgentWikiError> {
    let root = wiki.wiki_root();
    let mut out = String::new();
    for entry in walkdir::WalkDir::new(&root)
        .into_iter()
        .filter_map(Result::ok)
    {
        let p = entry.path();
        if !p.is_file() || p.extension().and_then(|s| s.to_str()) != Some("md") {
            continue;
        }
        let rel = p
            .strip_prefix(&root)
            .map(|r| r.to_string_lossy().to_string())
            .unwrap_or_default();
        if rel.starts_with("raw/") || rel.starts_with("_state/") || rel.starts_with("media/") {
            continue;
        }
        out.push_str(&format!("- {rel}\n"));
    }
    Ok(out)
}

fn collect_dedup_input(wiki: &WikiLive) -> Result<String, AgentWikiError> {
    // Same shape as page_index_lines for now — slugs +
    // title. The LLM groups by name similarity.
    collect_page_index_lines(wiki)
}
