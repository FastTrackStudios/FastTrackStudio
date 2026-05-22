//! Output parsers for the wiki-flavored prompts. Each
//! parser is **strict by design** — llm_wiki's pipeline
//! drops responses that don't match the expected format,
//! and the ported parsers preserve that contract.
//!
//! See [`crate::prompts`] for the prompts these consume.
//!
//! ## State of implementation
//!
//! Function signatures + doc comments are committed; the
//! body of each is `todo!()` to be filled in as consuming
//! backends land. The point of this slice is the type
//! surface — concrete parsing logic is small and easily
//! added crate-locally without touching `wiki-proto`.

use crate::error::AgentWikiError;

// ─────────────────────────── Ingest blocks ───────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngestBlocks {
    pub files: Vec<FileBlock>,
    pub reviews: Vec<ReviewBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileBlock {
    /// Vault-relative path (e.g. `wiki/entities/openai.md`).
    pub path: String,
    /// Full markdown including frontmatter.
    pub content: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReviewBlock {
    pub kind: ReviewBlockKind,
    pub title: String,
    pub description: String,
    /// `OPTIONS:` line split on `|`.
    pub options: Vec<String>,
    /// `PAGES:` line split on `,`.
    pub pages: Vec<String>,
    /// `SEARCH:` line split on `|`. Empty when absent.
    pub search_queries: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReviewBlockKind {
    Contradiction,
    Duplicate,
    MissingPage,
    Suggestion,
}

/// Parse an ingest step-2 LLM response into FILE + REVIEW
/// blocks. Returns [`AgentWikiError::MalformedResponse`] if
/// the response doesn't begin with `---FILE:`.
pub fn parse_ingest_blocks(_response: &str) -> Result<IngestBlocks, AgentWikiError> {
    todo!("port from llm_wiki/src/lib/ingest.ts (block parser ~line 1200)")
}

// ─────────────────────────── Lint blocks ───────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintBlock {
    pub kind: LintBlockKind,
    pub severity: LintSeverity,
    pub title: String,
    pub description: String,
    pub pages: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintBlockKind {
    Contradiction,
    Stale,
    MissingPage,
    Suggestion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintSeverity {
    Warning,
    Info,
}

pub fn parse_lint_blocks(_response: &str) -> Result<Vec<LintBlock>, AgentWikiError> {
    todo!("port from llm_wiki/src/lib/lint.ts")
}

// ─────────────────────────── Dedup JSON ───────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateGroup {
    pub slugs: Vec<String>,
    pub reason: String,
    pub confidence: DuplicateConfidence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DuplicateConfidence {
    High,
    Medium,
    Low,
}

pub fn parse_dedup_groups(_response: &str) -> Result<Vec<DuplicateGroup>, AgentWikiError> {
    todo!("expects `{{\"groups\":[...]}}` JSON; see llm_wiki dedup.ts:200+")
}

// ─────────────────────────── Optimize research ───────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResearchTopicPlan {
    pub topic: String,
    /// Exactly 3 queries by contract; parser may return
    /// fewer if the LLM violated the format.
    pub queries: Vec<String>,
}

pub fn parse_research_plan(_response: &str) -> Result<ResearchTopicPlan, AgentWikiError> {
    todo!("expects 4-line response: TOPIC + 3x QUERY")
}

// ─────────────────────────── Sweep reviews JSON ───────────────────────────

pub fn parse_sweep_resolved(_response: &str) -> Result<Vec<String>, AgentWikiError> {
    todo!("expects `{{\"resolved\":[...]}}` JSON")
}
