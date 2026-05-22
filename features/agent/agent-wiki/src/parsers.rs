//! Output parsers for the wiki-flavored prompts.
//!
//! See [`crate::prompts`] for the prompts these consume.
//! Each parser is strict by design — llm_wiki's pipeline
//! drops responses that don't match the expected format
//! and the port preserves that contract.

use crate::error::AgentWikiError;

// ─────────────────────────── Ingest blocks ───────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngestBlocks {
    pub files: Vec<FileBlock>,
    pub reviews: Vec<ReviewBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileBlock {
    /// Vault-relative path (e.g. `Concepts/Spaced repetition.md`
    /// — llm_wiki uses `wiki/...` prefix which we strip).
    pub path: String,
    /// Full markdown including frontmatter.
    pub content: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReviewBlock {
    pub kind: ReviewBlockKind,
    pub title: String,
    pub description: String,
    pub options: Vec<String>,
    pub pages: Vec<String>,
    pub search_queries: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReviewBlockKind {
    Contradiction,
    Duplicate,
    MissingPage,
    Suggestion,
}

impl ReviewBlockKind {
    fn parse(s: &str) -> Result<Self, AgentWikiError> {
        match s.trim().to_lowercase().as_str() {
            "contradiction" => Ok(Self::Contradiction),
            "duplicate" => Ok(Self::Duplicate),
            "missing-page" => Ok(Self::MissingPage),
            "suggestion" => Ok(Self::Suggestion),
            other => Err(AgentWikiError::UnknownReviewKind(other.to_string())),
        }
    }
}

/// Parse an ingest step-2 LLM response into FILE + REVIEW
/// blocks.
///
/// The response must begin with `---FILE:` (after optional
/// leading whitespace). Reviews follow files. Anything
/// between blocks is ignored — common LLM filler like
/// blank lines doesn't break the parse.
///
/// Format reference:
///
/// ```text
/// ---FILE: wiki/path/to/page.md---
/// (markdown body)
/// ---END FILE---
///
/// ---REVIEW: <kind> | <title>---
/// Description.
/// OPTIONS: Create Page | Skip
/// PAGES: a.md, b.md
/// SEARCH: q1 | q2
/// ---END REVIEW---
/// ```
pub fn parse_ingest_blocks(response: &str) -> Result<IngestBlocks, AgentWikiError> {
    let trimmed = response.trim_start();
    if !trimmed.starts_with("---FILE:") && !trimmed.starts_with("---REVIEW:") {
        return Err(AgentWikiError::MalformedResponse(
            "expected the response to start with `---FILE:` (or `---REVIEW:`)",
            response.chars().take(120).collect::<String>(),
        ));
    }

    let mut out = IngestBlocks {
        files: Vec::new(),
        reviews: Vec::new(),
    };

    let mut cursor = 0usize;
    while cursor < response.len() {
        // Advance to the next `---FILE:` / `---REVIEW:` header.
        let after = &response[cursor..];
        if let Some(file_start_rel) = after.find("---FILE:") {
            // Anything before this is filler between blocks.
            let block_start = cursor + file_start_rel;
            let (block, end) = parse_one(response, block_start, "---FILE:", "---END FILE---")?;
            out.files.push(parse_file_block(block)?);
            cursor = end;
        } else if let Some(rev_start_rel) = after.find("---REVIEW:") {
            let block_start = cursor + rev_start_rel;
            let (block, end) = parse_one(response, block_start, "---REVIEW:", "---END REVIEW---")?;
            out.reviews.push(parse_review_block(block)?);
            cursor = end;
        } else {
            break;
        }
    }

    Ok(out)
}

/// Return the slice between the open header at `start`
/// and the matching close marker. Returns the slice
/// (header + body + close) and the absolute end index.
fn parse_one<'a>(
    src: &'a str,
    start: usize,
    open: &str,
    close: &str,
) -> Result<(&'a str, usize), AgentWikiError> {
    debug_assert!(src[start..].starts_with(open));
    let close_rel = src[start..].find(close).ok_or_else(|| {
        AgentWikiError::MalformedResponse(
            "missing close marker",
            format!("{}…", &src[start..src.len().min(start + 80)]),
        )
    })?;
    let end = start + close_rel + close.len();
    Ok((&src[start..end], end))
}

fn parse_file_block(raw: &str) -> Result<FileBlock, AgentWikiError> {
    // Strip the header line.
    let rest = raw
        .strip_prefix("---FILE:")
        .ok_or(AgentWikiError::MalformedResponse(
            "expected ---FILE: prefix",
            raw.chars().take(60).collect(),
        ))?;
    let (header, body) = rest
        .split_once('\n')
        .ok_or(AgentWikiError::MalformedResponse(
            "FILE block missing newline after header",
            raw.chars().take(60).collect(),
        ))?;
    // Header is `<path>---` — strip the trailing `---`.
    let path_raw = header.trim().trim_end_matches('-').trim();
    if path_raw.is_empty() {
        return Err(AgentWikiError::InvalidFileTarget(
            path_raw.to_string(),
            "empty path",
        ));
    }
    // Strip an optional `wiki/` prefix (llm_wiki's
    // convention) so our paths are Wiki/-relative.
    let path = path_raw
        .strip_prefix("wiki/")
        .or_else(|| path_raw.strip_prefix("Wiki/"))
        .unwrap_or(path_raw)
        .to_string();
    if path.is_empty() || path.contains("..") {
        return Err(AgentWikiError::InvalidFileTarget(
            path,
            "must be wiki-relative, no `..` allowed",
        ));
    }

    // Strip the closing `---END FILE---` from the body.
    let content = body
        .strip_suffix("---END FILE---")
        .or_else(|| body.strip_suffix("---END FILE---\n"))
        .unwrap_or_else(|| {
            body.trim_end_matches('\n')
                .strip_suffix("---END FILE---")
                .unwrap_or(body)
        });
    Ok(FileBlock {
        path,
        content: content.trim_end_matches('\n').to_string() + "\n",
    })
}

fn parse_review_block(raw: &str) -> Result<ReviewBlock, AgentWikiError> {
    let rest = raw
        .strip_prefix("---REVIEW:")
        .ok_or(AgentWikiError::MalformedResponse(
            "expected ---REVIEW: prefix",
            raw.chars().take(60).collect(),
        ))?;
    let (header, body) = rest
        .split_once('\n')
        .ok_or(AgentWikiError::MalformedResponse(
            "REVIEW block missing newline after header",
            raw.chars().take(60).collect(),
        ))?;
    // Header is `<kind> | <title>---`.
    let header = header.trim().trim_end_matches('-').trim();
    let (kind_str, title) = header
        .split_once('|')
        .ok_or(AgentWikiError::MalformedResponse(
            "REVIEW header must be `<kind> | <title>`",
            header.to_string(),
        ))?;
    let kind = ReviewBlockKind::parse(kind_str)?;
    let title = title.trim().to_string();

    let body = body
        .strip_suffix("---END REVIEW---")
        .or_else(|| body.strip_suffix("---END REVIEW---\n"))
        .unwrap_or(body);

    let mut description = String::new();
    let mut options = Vec::new();
    let mut pages = Vec::new();
    let mut search_queries = Vec::new();
    for line in body.lines() {
        let l = line.trim_end();
        if let Some(rest) = l.strip_prefix("OPTIONS:") {
            options = rest
                .split('|')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        } else if let Some(rest) = l.strip_prefix("PAGES:") {
            pages = rest
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        } else if let Some(rest) = l.strip_prefix("SEARCH:") {
            search_queries = rest
                .split('|')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        } else {
            description.push_str(l);
            description.push('\n');
        }
    }

    Ok(ReviewBlock {
        kind,
        title,
        description: description.trim().to_string(),
        options,
        pages,
        search_queries,
    })
}

// ─────────────────────────── Lint, dedup, research ───────────────────────────

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

impl LintBlockKind {
    fn parse(s: &str) -> Result<Self, AgentWikiError> {
        match s.trim().to_lowercase().as_str() {
            "contradiction" => Ok(Self::Contradiction),
            "stale" => Ok(Self::Stale),
            "missing-page" => Ok(Self::MissingPage),
            "suggestion" => Ok(Self::Suggestion),
            other => Err(AgentWikiError::UnknownLintKind(other.to_string())),
        }
    }
}

impl LintSeverity {
    fn parse(s: &str) -> LintSeverity {
        match s.trim().to_lowercase().as_str() {
            "info" => Self::Info,
            _ => Self::Warning,
        }
    }
}

/// Parse `---LINT: <kind> | <severity> | <title>---` /
/// `---END LINT---` blocks. Matches llm_wiki's lint
/// emit format.
pub fn parse_lint_blocks(response: &str) -> Result<Vec<LintBlock>, AgentWikiError> {
    let mut out = Vec::new();
    let mut cursor = 0usize;
    while cursor < response.len() {
        let after = &response[cursor..];
        let Some(rel) = after.find("---LINT:") else {
            break;
        };
        let block_start = cursor + rel;
        let (block, end) = parse_one(response, block_start, "---LINT:", "---END LINT---")?;
        out.push(parse_lint_block(block)?);
        cursor = end;
    }
    Ok(out)
}

fn parse_lint_block(raw: &str) -> Result<LintBlock, AgentWikiError> {
    let rest = raw
        .strip_prefix("---LINT:")
        .ok_or(AgentWikiError::MalformedResponse(
            "expected ---LINT: prefix",
            raw.chars().take(60).collect(),
        ))?;
    let (header, body) = rest
        .split_once('\n')
        .ok_or(AgentWikiError::MalformedResponse(
            "LINT block missing newline after header",
            raw.chars().take(60).collect(),
        ))?;
    let header = header.trim().trim_end_matches('-').trim();
    let mut parts = header.splitn(3, '|');
    let kind_str = parts.next().unwrap_or("").trim();
    let sev_str = parts.next().unwrap_or("warning").trim();
    let title = parts.next().unwrap_or("").trim().to_string();
    let kind = LintBlockKind::parse(kind_str)?;
    let severity = LintSeverity::parse(sev_str);

    let body = body
        .strip_suffix("---END LINT---")
        .or_else(|| body.strip_suffix("---END LINT---\n"))
        .unwrap_or(body);

    let mut description = String::new();
    let mut pages = Vec::new();
    for line in body.lines() {
        let l = line.trim_end();
        if let Some(rest) = l.strip_prefix("PAGES:") {
            pages = rest
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        } else {
            description.push_str(l);
            description.push('\n');
        }
    }

    Ok(LintBlock {
        kind,
        severity,
        title,
        description: description.trim().to_string(),
        pages,
    })
}

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

/// Parse `{"groups":[{"slugs":[...],"reason":"…",
/// "confidence":"high"}]}` JSON.
pub fn parse_dedup_groups(response: &str) -> Result<Vec<DuplicateGroup>, AgentWikiError> {
    // Trim markdown fences in case the LLM wrapped despite
    // instructions.
    let body = strip_json_fence(response);
    let v: serde_json::Value = serde_json::from_str(body).map_err(|e| {
        AgentWikiError::MalformedResponse(
            "expected JSON object",
            format!("{e}: {}", body.chars().take(80).collect::<String>()),
        )
    })?;
    let groups =
        v.get("groups")
            .and_then(|g| g.as_array())
            .ok_or(AgentWikiError::MalformedResponse(
                "missing `groups` array",
                body.chars().take(80).collect(),
            ))?;
    let mut out = Vec::with_capacity(groups.len());
    for g in groups {
        let slugs: Vec<String> = g
            .get("slugs")
            .and_then(|s| s.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default();
        if slugs.len() < 2 {
            continue;
        }
        let reason = g
            .get("reason")
            .and_then(|v| v.as_str())
            .unwrap_or_default()
            .to_string();
        let confidence = match g.get("confidence").and_then(|v| v.as_str()) {
            Some("high") => DuplicateConfidence::High,
            Some("medium") => DuplicateConfidence::Medium,
            Some("low") => DuplicateConfidence::Low,
            _ => DuplicateConfidence::Medium,
        };
        out.push(DuplicateGroup {
            slugs,
            reason,
            confidence,
        });
    }
    Ok(out)
}

fn strip_json_fence(s: &str) -> &str {
    let t = s.trim();
    if let Some(rest) = t.strip_prefix("```json") {
        rest.trim_start_matches('\n')
            .trim_end()
            .strip_suffix("```")
            .unwrap_or(rest)
            .trim()
    } else if let Some(rest) = t.strip_prefix("```") {
        rest.trim_start_matches('\n')
            .trim_end()
            .strip_suffix("```")
            .unwrap_or(rest)
            .trim()
    } else {
        t
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResearchTopicPlan {
    pub topic: String,
    pub queries: Vec<String>,
}

/// Parse the 4-line `TOPIC:` + 3× `QUERY:` response from
/// `OPTIMIZE_RESEARCH_SYSTEM`. Tolerant: accepts < 3
/// queries, but `TOPIC:` is required.
pub fn parse_research_plan(response: &str) -> Result<ResearchTopicPlan, AgentWikiError> {
    let mut topic = String::new();
    let mut queries = Vec::new();
    for line in response.lines() {
        let l = line.trim();
        if let Some(rest) = l.strip_prefix("TOPIC:") {
            topic = rest.trim().to_string();
        } else if let Some(rest) = l.strip_prefix("QUERY:") {
            let q = rest.trim().to_string();
            if !q.is_empty() {
                queries.push(q);
            }
        }
    }
    if topic.is_empty() {
        return Err(AgentWikiError::MalformedResponse(
            "missing TOPIC: line",
            response.chars().take(120).collect(),
        ));
    }
    Ok(ResearchTopicPlan { topic, queries })
}

/// Parse `{"resolved":["id1","id2"]}` from
/// `SWEEP_REVIEWS_SYSTEM`.
pub fn parse_sweep_resolved(response: &str) -> Result<Vec<String>, AgentWikiError> {
    let body = strip_json_fence(response);
    let v: serde_json::Value = serde_json::from_str(body).map_err(|e| {
        AgentWikiError::MalformedResponse(
            "expected JSON object",
            format!("{e}: {}", body.chars().take(80).collect::<String>()),
        )
    })?;
    let arr =
        v.get("resolved")
            .and_then(|r| r.as_array())
            .ok_or(AgentWikiError::MalformedResponse(
                "missing `resolved` array",
                body.chars().take(80).collect(),
            ))?;
    Ok(arr
        .iter()
        .filter_map(|v| v.as_str().map(|s| s.to_string()))
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_one_file_block() {
        let resp = r#"---FILE: wiki/Concepts/Foo.md---
---
type: concept
title: Foo
---

# Foo

Body.
---END FILE---"#;
        let parsed = parse_ingest_blocks(resp).expect("parse");
        assert_eq!(parsed.files.len(), 1);
        assert_eq!(parsed.files[0].path, "Concepts/Foo.md");
        assert!(parsed.files[0].content.starts_with("---\n"));
        assert!(parsed.files[0].content.contains("# Foo"));
    }

    #[test]
    fn parses_file_then_review() {
        let resp = "---FILE: wiki/Entities/X.md---\n---\ntype: entity\ntitle: X\n---\n\n# X\n---END FILE---\n\n---REVIEW: contradiction | Foo conflicts with bar---\nDescription line.\nOPTIONS: Create Page | Skip\nPAGES: Concepts/Foo.md, Concepts/Bar.md\n---END REVIEW---\n";
        let parsed = parse_ingest_blocks(resp).expect("parse");
        assert_eq!(parsed.files.len(), 1);
        assert_eq!(parsed.reviews.len(), 1);
        assert_eq!(parsed.reviews[0].kind, ReviewBlockKind::Contradiction);
        assert_eq!(parsed.reviews[0].title, "Foo conflicts with bar");
        assert_eq!(parsed.reviews[0].options, vec!["Create Page", "Skip"]);
        assert_eq!(parsed.reviews[0].pages.len(), 2);
    }

    #[test]
    fn rejects_non_block_start() {
        let resp = "Sure! Here are the files:\n\n---FILE: wiki/foo.md---\nbody\n---END FILE---";
        assert!(parse_ingest_blocks(resp).is_err());
    }
}
