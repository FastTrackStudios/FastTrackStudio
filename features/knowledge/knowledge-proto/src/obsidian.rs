//! Pure Obsidian-compat helpers. No Loro, no Repo, no async — just
//! string-in / string-out (plus the shaped intermediate structs).
//!
//! Why hand-rolled, not `pulldown-cmark`? Because we need byte-range
//! fidelity for round-trip stability, and we need Obsidian-specific
//! constructs (callouts, `^block-id` anchors, `((id))` block refs,
//! `[[entity://kind/uuid]]` typed refs) that the standard markdown
//! grammar doesn't model.
//!
//! Module exports the regex source strings as `pub const` so other
//! crates can build their own compiled `Regex` if they want; we
//! compile inside the helper fns via `once_cell`-free `OnceLock`.

use std::sync::OnceLock;

use indexmap::IndexMap;
use regex::Regex;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::refs::{BlockRef, EmbedRef, EntityRef, LinkRef, Ref, TagRef};
use crate::{Block, Page, Vault};

// ── Regex sources ───────────────────────────────────────────────────

/// `[[Page]]` / `[[Page#Heading]]` / `[[Page#^block-id]]` / `[[Page|alias]]`.
/// Doesn't match `![[…]]` (handled by `EMBED_REGEX` separately and
/// the link scanner explicitly skips a preceding `!`).
pub const LINK_REGEX: &str =
    r"\[\[([^\]\|#\^\r\n]+)(?:#\^([a-zA-Z0-9\-]+)|#([^\]\|\r\n]+))?(?:\|([^\]\r\n]+))?\]\]";

/// `![[…]]` — same shape as the link regex but with the leading `!`.
pub const EMBED_REGEX: &str =
    r"!\[\[([^\]\|#\^\r\n]+)(?:#\^([a-zA-Z0-9\-]+)|#([^\]\|\r\n]+))?(?:\|([^\]\r\n]+))?\]\]";

/// Trailing `^id` anchor — must be preceded by whitespace or be at
/// the start of the line, and run to end-of-line.
pub const BLOCK_ID_REGEX: &str = r"(?:^|\s)\^([a-zA-Z0-9\-]+)\s*$";

/// `#nested/tag`. Tags must start with a letter (Obsidian doesn't
/// recognize all-digit tags). The leading `#` must be at start of
/// line or preceded by whitespace.
pub const TAG_REGEX: &str = r"(?:^|\s)#([\p{L}][\p{L}\p{N}_\-/]*)";

/// Our extension: typed entity reference. The display alias is
/// optional just like a normal wikilink.
pub const ENTITY_LINK_REGEX: &str =
    r"\[\[entity://([a-z][a-z0-9_-]*)/([0-9a-f-]{36})(?:\|([^\]\r\n]+))?\]\]";

/// Logseq-style `key:: value` line at the top of a block.
pub const LOGSEQ_PROP_REGEX: &str = r"^([A-Za-z][A-Za-z0-9_-]*)::\s*(.*)$";

/// Logseq-style `((block-uuid))` reference.
pub const LOGSEQ_BLOCK_REF_REGEX: &str =
    r"\(\(([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\)\)";

/// `((uuid))` direct block reference. Synonym of `LOGSEQ_BLOCK_REF_REGEX`
/// re-exported under the v1.5 editor's preferred name.
pub const BLOCK_REF_REGEX: &str = LOGSEQ_BLOCK_REF_REGEX;

// ── Compiled regex accessors (lazy, leaked-once) ────────────────────

fn link_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(LINK_REGEX).expect("LINK_REGEX is valid"))
}

fn embed_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(EMBED_REGEX).expect("EMBED_REGEX is valid"))
}

fn block_id_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(BLOCK_ID_REGEX).expect("BLOCK_ID_REGEX is valid"))
}

fn tag_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(TAG_REGEX).expect("TAG_REGEX is valid"))
}

fn entity_link_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(ENTITY_LINK_REGEX).expect("ENTITY_LINK_REGEX is valid"))
}

fn logseq_prop_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(LOGSEQ_PROP_REGEX).expect("LOGSEQ_PROP_REGEX is valid"))
}

fn logseq_block_ref_re() -> &'static Regex {
    static R: OnceLock<Regex> = OnceLock::new();
    R.get_or_init(|| Regex::new(LOGSEQ_BLOCK_REF_REGEX).expect("LOGSEQ_BLOCK_REF_REGEX is valid"))
}

// ── Intermediate value types ───────────────────────────────────────

/// One frontmatter `key: value` pair. Value is JSON-shaped so we
/// preserve scalar / list / map distinctions on round-trip.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FrontmatterEntry {
    pub key: String,
    pub value: serde_json::Value,
}

/// A page parsed out of markdown source, prior to assigning vault /
/// page UUIDs.
#[derive(Clone, Debug, PartialEq)]
pub struct ParsedPage {
    pub frontmatter: Vec<FrontmatterEntry>,
    pub blocks: Vec<ParsedBlock>,
}

/// A block parsed out of markdown source. Lacks `vault_id` /
/// `page_id` because those are assigned by the consumer when
/// inserting into a CRDT.
#[derive(Clone, Debug, PartialEq)]
pub struct ParsedBlock {
    pub kind: String,
    pub content: String,
    pub heading_level: Option<i32>,
    pub list_ordered: bool,
    pub list_task: Option<String>,
    pub code_lang: Option<String>,
    pub callout_kind: Option<String>,
    pub callout_foldable: bool,
    /// Outliner indent depth (0 = top level). Tracked per list item;
    /// always 0 for non-list kinds.
    pub indent_depth: u32,
    pub obsidian_block_id: Option<String>,
    pub refs: Vec<Ref>,
    pub properties: Vec<FrontmatterEntry>,
}

impl Default for ParsedBlock {
    fn default() -> Self {
        Self {
            kind: "paragraph".into(),
            content: String::new(),
            heading_level: None,
            list_ordered: false,
            list_task: None,
            code_lang: None,
            callout_kind: None,
            callout_foldable: false,
            indent_depth: 0,
            obsidian_block_id: None,
            refs: Vec::new(),
            properties: Vec::new(),
        }
    }
}

// ── Frontmatter ─────────────────────────────────────────────────────

/// Parse the leading YAML frontmatter block (`---` ... `---`).
/// Returns the parsed entries and the byte offset where the body
/// begins. If no frontmatter is present, returns `(vec![], 0)`.
///
/// Edge cases:
/// - Empty `---\n---\n` returns `(vec![], 8)`.
/// - Frontmatter without trailing `---` is treated as no frontmatter
///   (consistent with Obsidian, which renders the raw `---` line).
/// - YAML parse failures fall back to empty frontmatter to keep the
///   helper infallible.
pub fn parse_frontmatter(src: &str) -> (Vec<FrontmatterEntry>, usize) {
    if !src.starts_with("---\n") && !src.starts_with("---\r\n") {
        return (Vec::new(), 0);
    }
    let after_open = if src.starts_with("---\r\n") { 5 } else { 4 };
    let rest = &src[after_open..];
    // Find the closing `---` on its own line.
    let mut search_start = 0usize;
    let close = loop {
        let Some(idx) = rest[search_start..].find("---") else {
            return (Vec::new(), 0);
        };
        let abs = search_start + idx;
        // Must be at start of line.
        let at_line_start = abs == 0 || rest.as_bytes()[abs - 1] == b'\n';
        // Must be followed by `\n`, `\r\n`, or EOF.
        let after_close = abs + 3;
        let followed_ok = after_close >= rest.len()
            || rest.as_bytes()[after_close] == b'\n'
            || (rest.as_bytes()[after_close] == b'\r'
                && after_close + 1 < rest.len()
                && rest.as_bytes()[after_close + 1] == b'\n');
        if at_line_start && followed_ok {
            break abs;
        }
        search_start = abs + 3;
    };
    let yaml = &rest[..close];
    let body_offset = after_open + close + 3;
    // Skip the newline after the closing `---`.
    let body_offset = if body_offset < src.len() && src.as_bytes()[body_offset] == b'\r' {
        body_offset + 2
    } else if body_offset < src.len() && src.as_bytes()[body_offset] == b'\n' {
        body_offset + 1
    } else {
        body_offset
    };

    let parsed: serde_yaml::Value = match serde_yaml::from_str(yaml) {
        Ok(v) => v,
        Err(_) => return (Vec::new(), body_offset),
    };
    let mut out = Vec::new();
    if let serde_yaml::Value::Mapping(map) = parsed {
        for (k, v) in map {
            let key = match k {
                serde_yaml::Value::String(s) => s,
                other => format!("{other:?}"),
            };
            let value = yaml_to_json(v);
            out.push(FrontmatterEntry { key, value });
        }
    }
    (out, body_offset)
}

/// Serialize frontmatter back to YAML, preserving key order. Returns
/// the empty string when `fm` is empty (so the parent serializer can
/// skip the `---` fences entirely).
pub fn serialize_frontmatter(fm: &[FrontmatterEntry]) -> String {
    if fm.is_empty() {
        return String::new();
    }
    let mut map: IndexMap<String, serde_yaml::Value> = IndexMap::new();
    for e in fm {
        map.insert(e.key.clone(), json_to_yaml(&e.value));
    }
    // serde_yaml::to_string preserves IndexMap order.
    let body = serde_yaml::to_string(&map).unwrap_or_default();
    format!("---\n{}---\n", body)
}

fn yaml_to_json(v: serde_yaml::Value) -> serde_json::Value {
    match v {
        serde_yaml::Value::Null => serde_json::Value::Null,
        serde_yaml::Value::Bool(b) => serde_json::Value::Bool(b),
        serde_yaml::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                serde_json::Value::Number(i.into())
            } else if let Some(f) = n.as_f64() {
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null)
            } else {
                serde_json::Value::Null
            }
        }
        serde_yaml::Value::String(s) => serde_json::Value::String(s),
        serde_yaml::Value::Sequence(seq) => {
            serde_json::Value::Array(seq.into_iter().map(yaml_to_json).collect())
        }
        serde_yaml::Value::Mapping(map) => {
            let mut out = serde_json::Map::new();
            for (k, v) in map {
                let key = match k {
                    serde_yaml::Value::String(s) => s,
                    other => format!("{other:?}"),
                };
                out.insert(key, yaml_to_json(v));
            }
            serde_json::Value::Object(out)
        }
        serde_yaml::Value::Tagged(t) => yaml_to_json(t.value),
    }
}

fn json_to_yaml(v: &serde_json::Value) -> serde_yaml::Value {
    match v {
        serde_json::Value::Null => serde_yaml::Value::Null,
        serde_json::Value::Bool(b) => serde_yaml::Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                serde_yaml::Value::Number(i.into())
            } else if let Some(f) = n.as_f64() {
                serde_yaml::Value::Number(serde_yaml::Number::from(f))
            } else {
                serde_yaml::Value::Null
            }
        }
        serde_json::Value::String(s) => serde_yaml::Value::String(s.clone()),
        serde_json::Value::Array(a) => {
            serde_yaml::Value::Sequence(a.iter().map(json_to_yaml).collect())
        }
        serde_json::Value::Object(o) => {
            let mut m = serde_yaml::Mapping::new();
            for (k, v) in o {
                m.insert(serde_yaml::Value::String(k.clone()), json_to_yaml(v));
            }
            serde_yaml::Value::Mapping(m)
        }
    }
}

// ── Block-id stripping ──────────────────────────────────────────────

/// Strip a trailing `^block-id` anchor off a single line. Returns
/// the cleaned content plus the (textual) id if present.
///
/// Edge cases:
/// - `"text ^abc"` → `("text", Some("abc"))`
/// - `"^abc"` on its own (no preceding whitespace) → `("", Some("abc"))`
/// - `"text ^"` (no id chars) → `("text ^", None)` (no match)
/// - `"text ^abc trailing"` → no match, id must be at end-of-line
pub fn parse_block_id(line: &str) -> (String, Option<String>) {
    let re = block_id_re();
    if let Some(caps) = re.captures(line) {
        let m = caps.get(0).unwrap();
        let id = caps.get(1).unwrap().as_str().to_string();
        let cleaned = line[..m.start()].trim_end().to_string();
        (cleaned, Some(id))
    } else {
        (line.to_string(), None)
    }
}

// ── Inline ref extraction ───────────────────────────────────────────

/// Pull every `[[…]]` / `![[…]]` / `#tag` / `[[entity://…]]` out of
/// the given content. Order matches their byte position in the source
/// so consumers can re-attach span data later.
pub fn extract_refs(content: &str) -> Vec<Ref> {
    let mut out: Vec<(usize, Ref)> = Vec::new();

    // Entity links go first since they're a strict subset of the
    // wikilink shape; we record their byte ranges to avoid emitting
    // both an EntityRef and a LinkRef for the same span.
    let mut entity_spans: Vec<(usize, usize)> = Vec::new();
    for caps in entity_link_re().captures_iter(content) {
        let m = caps.get(0).unwrap();
        let kind = caps.get(1).unwrap().as_str().to_string();
        let id_str = caps.get(2).unwrap().as_str();
        let display = caps.get(3).map(|m| m.as_str().to_string());
        if let Ok(id) = Uuid::parse_str(id_str) {
            entity_spans.push((m.start(), m.end()));
            out.push((m.start(), Ref::Entity(EntityRef { kind, id, display })));
        }
    }

    let in_entity_span =
        |start: usize, end: usize| entity_spans.iter().any(|(s, e)| start >= *s && end <= *e);

    // Embeds before links because `![[X]]` would otherwise match
    // `[[X]]` as a link with a stray `!` before it.
    let mut embed_spans: Vec<(usize, usize)> = Vec::new();
    for caps in embed_re().captures_iter(content) {
        let m = caps.get(0).unwrap();
        embed_spans.push((m.start(), m.end()));
        let target = caps.get(1).unwrap().as_str().to_string();
        let block_id = caps.get(2).map(|m| m.as_str().to_string());
        let heading = caps.get(3).map(|m| m.as_str().to_string());
        let alias = caps.get(4).map(|m| m.as_str().to_string());
        out.push((
            m.start(),
            Ref::Embed(EmbedRef {
                target_linkpath: target,
                heading,
                block_id,
                alias,
                original: m.as_str().to_string(),
            }),
        ));
    }

    for caps in link_re().captures_iter(content) {
        let m = caps.get(0).unwrap();
        // Skip if this is the tail of a `!`-prefixed embed.
        let preceded_by_bang = m.start() > 0 && &content[m.start() - 1..m.start()] == "!";
        if preceded_by_bang {
            continue;
        }
        // Skip if this span is entirely inside an entity-link match.
        if in_entity_span(m.start(), m.end()) {
            continue;
        }
        // Skip if this span overlaps an embed match.
        if embed_spans
            .iter()
            .any(|(s, e)| m.start() >= *s && m.end() <= *e)
        {
            continue;
        }
        let target = caps.get(1).unwrap().as_str().to_string();
        // Treat the `entity://…` linkpath as already handled above.
        if target.starts_with("entity://") {
            continue;
        }
        let block_id = caps.get(2).map(|m| m.as_str().to_string());
        let heading = caps.get(3).map(|m| m.as_str().to_string());
        let alias = caps.get(4).map(|m| m.as_str().to_string());
        out.push((
            m.start(),
            Ref::Link(LinkRef {
                target_linkpath: target,
                heading,
                block_id,
                alias,
                original: m.as_str().to_string(),
            }),
        ));
    }

    // `((uuid))` direct block references.
    for caps in logseq_block_ref_re().captures_iter(content) {
        let m = caps.get(0).unwrap();
        let uuid_str = caps.get(1).unwrap().as_str();
        if let Ok(target_block_id) = Uuid::parse_str(uuid_str) {
            out.push((
                m.start(),
                Ref::BlockRef(BlockRef {
                    target_block_id,
                    alias: None,
                    original: m.as_str().to_string(),
                }),
            ));
        }
    }

    for caps in tag_re().captures_iter(content) {
        let m = caps.get(0).unwrap();
        // Skip tags inside link / embed spans (Obsidian doesn't
        // index `#tag` inside `[[Page#tag]]`).
        if embed_spans
            .iter()
            .chain(entity_spans.iter())
            .any(|(s, e)| m.start() >= *s && m.end() <= *e)
        {
            continue;
        }
        let raw = caps.get(1).unwrap().as_str();
        let path: Vec<String> = raw.split('/').map(|s| s.to_string()).collect();
        out.push((
            m.start(),
            Ref::Tag(TagRef {
                path,
                original: format!("#{raw}"),
            }),
        ));
    }

    out.sort_by_key(|(start, _)| *start);
    out.into_iter().map(|(_, r)| r).collect()
}

// ── Linkpath resolution ─────────────────────────────────────────────

/// Resolve a wikilink target to a concrete page UUID.
///
/// Obsidian's algorithm (paraphrased):
/// 1. If `linkpath` contains `/` → path-based: try as-is, then with
///    `.md` appended, then relative to `source_path`'s folder.
/// 2. Else basename lookup: case-insensitive match against `Page.basename`.
///    If multiple match, pick the one with the shortest path
///    (lexicographic tie-break).
/// 3. Return `None` when nothing matches.
///
/// Paths are case-sensitive; basename lookups normalize to lowercase
/// because filesystems vary and Obsidian itself is case-insensitive
/// at the basename level.
pub fn resolve_linkpath(linkpath: &str, source_path: &str, pages: &[Page]) -> Option<Uuid> {
    if linkpath.contains('/') {
        // Try exact path.
        for p in pages {
            if p.path == linkpath {
                return Some(p.id);
            }
        }
        // Try with `.md` appended.
        let with_ext = format!("{linkpath}.md");
        for p in pages {
            if p.path == with_ext {
                return Some(p.id);
            }
        }
        // Try relative to source folder.
        if let Some(source_folder) = source_path.rsplit_once('/').map(|(d, _)| d) {
            let combined = if source_folder.is_empty() {
                linkpath.to_string()
            } else {
                format!("{source_folder}/{linkpath}")
            };
            for p in pages {
                if p.path == combined || p.path == format!("{combined}.md") {
                    return Some(p.id);
                }
            }
        }
        return None;
    }

    let target_lc = linkpath.to_lowercase();
    let mut candidates: Vec<&Page> = pages
        .iter()
        .filter(|p| p.basename.to_lowercase() == target_lc)
        .collect();
    if candidates.is_empty() {
        return None;
    }
    if candidates.len() == 1 {
        return Some(candidates[0].id);
    }
    // Shortest path, lexicographic tie-break.
    candidates.sort_by(|a, b| {
        a.path
            .len()
            .cmp(&b.path.len())
            .then_with(|| a.path.cmp(&b.path))
    });
    Some(candidates[0].id)
}

// ── Page-level parse ───────────────────────────────────────────────

/// Walk markdown source line-by-line and produce a `ParsedPage`.
///
/// State machine highlights:
/// - YAML frontmatter is stripped via `parse_frontmatter`.
/// - Code fences (``` / ~~~) suppress all other parsing until closed.
/// - Headings (`^#{1,6}\s`) emit a fresh `heading` block.
/// - Thematic breaks (`***` / `---` / `___` alone on a line).
/// - Callouts (`^> \[!kind\][+-]?`) start a `callout` block; foldable
///   if the trailing modifier is `+` or `-`.
/// - Plain blockquotes (`^>`) accumulate in a `blockquote` block.
/// - List items (`^\s*([-*+]|\d+\.)\s`) emit `list_item` blocks. The
///   indent depth is derived from leading whitespace (2-space and
///   4-space both parse as one level; we use a depth-stack rather
///   than a fixed divisor).
/// - Tables (`^\|.*\|`) — v1 stuffs all rows into one block.
/// - Blank lines close the current block.
/// - Everything else accumulates into a `paragraph`.
///
/// v1 limitations (flagged inline):
/// - Nested callouts collapse into the outer callout's content.
/// - HTML blocks are emitted as `html` kind but with no further
///   tag-balancing logic.
pub fn parse_page(src: &str) -> ParsedPage {
    let (frontmatter, body_offset) = parse_frontmatter(src);
    let body = &src[body_offset..];

    let mut blocks: Vec<ParsedBlock> = Vec::new();
    let mut cur: Option<ParsedBlock> = None;
    let mut in_code = false;
    let mut code_fence: Option<String> = None;
    let mut indent_stack: Vec<usize> = Vec::new(); // columns

    let push_cur = |blocks: &mut Vec<ParsedBlock>, mut b: Option<ParsedBlock>| {
        if let Some(mut blk) = b.take() {
            // Strip trailing block-id off the final line of content.
            let (cleaned, id) = parse_block_id(&blk.content);
            blk.content = cleaned;
            if id.is_some() {
                blk.obsidian_block_id = id;
            }
            // Run logseq prop parse + ref extraction.
            let (content_without_props, props) = extract_logseq_props(&blk.content);
            blk.content = content_without_props;
            blk.properties = props;
            blk.refs = extract_refs(&blk.content);
            blocks.push(blk);
        }
    };

    for line in body.split_inclusive('\n') {
        let trimmed = line.trim_end_matches(['\r', '\n']);

        if in_code {
            // Look for matching fence.
            let fence = code_fence.as_deref().unwrap_or("```");
            if trimmed.trim_start().starts_with(fence)
                && trimmed
                    .trim_start()
                    .chars()
                    .all(|c| c == fence.chars().next().unwrap() || c.is_whitespace())
            {
                in_code = false;
                code_fence = None;
                push_cur(&mut blocks, cur.take());
            } else if let Some(b) = cur.as_mut() {
                b.content.push_str(trimmed);
                b.content.push('\n');
            }
            continue;
        }

        // Detect code fence opener.
        let ts = trimmed.trim_start();
        if (ts.starts_with("```") || ts.starts_with("~~~")) && cur.is_none() {
            let fence_char = if ts.starts_with("```") { "```" } else { "~~~" };
            let lang = ts[3..].trim().to_string();
            let mut b = ParsedBlock {
                kind: "code".into(),
                code_lang: if lang.is_empty() { None } else { Some(lang) },
                ..ParsedBlock::default()
            };
            b.content.clear();
            cur = Some(b);
            in_code = true;
            code_fence = Some(fence_char.into());
            continue;
        }

        // Blank line — close current block.
        if trimmed.trim().is_empty() {
            push_cur(&mut blocks, cur.take());
            indent_stack.clear();
            continue;
        }

        // Thematic break.
        if matches!(trimmed.trim(), "***" | "---" | "___") {
            push_cur(&mut blocks, cur.take());
            blocks.push(ParsedBlock {
                kind: "thematic_break".into(),
                ..ParsedBlock::default()
            });
            continue;
        }

        // Heading.
        if let Some(level_caps) = parse_heading(trimmed) {
            push_cur(&mut blocks, cur.take());
            blocks.push(ParsedBlock {
                kind: "heading".into(),
                heading_level: Some(level_caps.0),
                content: level_caps.1,
                ..ParsedBlock::default()
            });
            continue;
        }

        // Callout opener (must be checked before blockquote).
        if let Some(callout) = parse_callout_opener(trimmed) {
            push_cur(&mut blocks, cur.take());
            cur = Some(ParsedBlock {
                kind: "callout".into(),
                callout_kind: Some(callout.kind),
                callout_foldable: callout.foldable,
                content: callout.rest,
                ..ParsedBlock::default()
            });
            continue;
        }

        // Continuation of an open callout (`>` lines).
        if let Some(b) = cur.as_mut() {
            if b.kind == "callout" && trimmed.starts_with('>') {
                let rest = trimmed[1..].trim_start_matches(' ');
                if !b.content.is_empty() {
                    b.content.push('\n');
                }
                b.content.push_str(rest);
                continue;
            }
        }

        // Plain blockquote.
        if trimmed.starts_with('>') {
            let rest = trimmed[1..].trim_start_matches(' ');
            match cur.as_mut() {
                Some(b) if b.kind == "blockquote" => {
                    b.content.push('\n');
                    b.content.push_str(rest);
                }
                _ => {
                    push_cur(&mut blocks, cur.take());
                    cur = Some(ParsedBlock {
                        kind: "blockquote".into(),
                        content: rest.to_string(),
                        ..ParsedBlock::default()
                    });
                }
            }
            continue;
        }

        // List item.
        if let Some(li) = parse_list_item(trimmed) {
            push_cur(&mut blocks, cur.take());
            // Manage indent stack to compute depth.
            while let Some(&top) = indent_stack.last() {
                if li.indent_cols <= top {
                    indent_stack.pop();
                } else {
                    break;
                }
            }
            indent_stack.push(li.indent_cols);
            let depth = (indent_stack.len() as u32).saturating_sub(1);
            cur = Some(ParsedBlock {
                kind: "list_item".into(),
                list_ordered: li.ordered,
                list_task: li.task_mark,
                indent_depth: depth,
                content: li.text,
                ..ParsedBlock::default()
            });
            continue;
        }

        // Table row.
        if trimmed.starts_with('|') && trimmed.ends_with('|') {
            match cur.as_mut() {
                Some(b) if b.kind == "table" => {
                    b.content.push('\n');
                    b.content.push_str(trimmed);
                }
                _ => {
                    push_cur(&mut blocks, cur.take());
                    cur = Some(ParsedBlock {
                        kind: "table".into(),
                        content: trimmed.to_string(),
                        ..ParsedBlock::default()
                    });
                }
            }
            continue;
        }

        // HTML block.
        if trimmed.starts_with('<') && cur.is_none() {
            cur = Some(ParsedBlock {
                kind: "html".into(),
                content: trimmed.to_string(),
                ..ParsedBlock::default()
            });
            continue;
        }

        // Paragraph (or continuation of one).
        match cur.as_mut() {
            Some(b) if b.kind == "paragraph" => {
                b.content.push('\n');
                b.content.push_str(trimmed);
            }
            Some(b) if b.kind == "list_item" => {
                // List-item soft continuation: indented text under a
                // bullet keeps appending to the item's content.
                b.content.push('\n');
                b.content.push_str(trimmed.trim_start());
            }
            _ => {
                push_cur(&mut blocks, cur.take());
                cur = Some(ParsedBlock {
                    kind: "paragraph".into(),
                    content: trimmed.to_string(),
                    ..ParsedBlock::default()
                });
            }
        }
    }

    push_cur(&mut blocks, cur.take());

    ParsedPage {
        frontmatter,
        blocks,
    }
}

fn parse_heading(line: &str) -> Option<(i32, String)> {
    let trimmed = line.trim_start();
    let hashes: usize = trimmed.chars().take_while(|c| *c == '#').count();
    if hashes == 0 || hashes > 6 {
        return None;
    }
    let rest = &trimmed[hashes..];
    if !rest.starts_with(' ') {
        return None;
    }
    Some((hashes as i32, rest.trim_start().to_string()))
}

struct CalloutOpener {
    kind: String,
    foldable: bool,
    rest: String,
}

fn parse_callout_opener(line: &str) -> Option<CalloutOpener> {
    let rest = line.strip_prefix("> ").or_else(|| line.strip_prefix(">"))?;
    let rest = rest.strip_prefix("[!")?;
    let close = rest.find(']')?;
    let kind = rest[..close].to_string();
    let after = &rest[close + 1..];
    let (foldable, body) = if let Some(b) = after.strip_prefix('+') {
        (true, b)
    } else if let Some(b) = after.strip_prefix('-') {
        (true, b)
    } else {
        (false, after)
    };
    Some(CalloutOpener {
        kind,
        foldable,
        rest: body.trim_start().to_string(),
    })
}

struct ListItem {
    indent_cols: usize,
    ordered: bool,
    task_mark: Option<String>,
    text: String,
}

fn parse_list_item(line: &str) -> Option<ListItem> {
    let indent_cols = line.chars().take_while(|c| *c == ' ' || *c == '\t').count();
    let rest = &line[indent_cols..];
    // Tab counts as 4 cols for depth purposes — matches Obsidian
    // default ("Use tabs" = 1 tab per level).
    let indent_cols = line[..indent_cols]
        .chars()
        .map(|c| if c == '\t' { 4 } else { 1 })
        .sum();

    let (ordered, after_marker) =
        if rest.starts_with("- ") || rest.starts_with("* ") || rest.starts_with("+ ") {
            (false, &rest[2..])
        } else {
            // Try ordered: `\d+. `.
            let dot = rest.find('.')?;
            if dot == 0 || !rest[..dot].chars().all(|c| c.is_ascii_digit()) {
                return None;
            }
            let after = &rest[dot + 1..];
            if !after.starts_with(' ') {
                return None;
            }
            (true, &after[1..])
        };

    // Task mark: `[x] `.
    let (task_mark, text) = if let Some(after_open) = after_marker.strip_prefix('[') {
        if after_open.len() >= 2 && after_open.as_bytes()[1] == b']' {
            let mark = &after_open[..1];
            let rest = after_open[2..].trim_start_matches(' ');
            (Some(mark.to_string()), rest.to_string())
        } else {
            (None, after_marker.to_string())
        }
    } else {
        (None, after_marker.to_string())
    };

    Some(ListItem {
        indent_cols,
        ordered,
        task_mark,
        text,
    })
}

fn extract_logseq_props(content: &str) -> (String, Vec<FrontmatterEntry>) {
    let mut props = Vec::new();
    let mut kept: Vec<&str> = Vec::new();
    let re = logseq_prop_re();
    let mut still_in_prop_prefix = true;
    for line in content.lines() {
        if still_in_prop_prefix {
            if let Some(caps) = re.captures(line) {
                let key = caps.get(1).unwrap().as_str().to_string();
                let raw_val = caps.get(2).unwrap().as_str().to_string();
                props.push(FrontmatterEntry {
                    key,
                    value: serde_json::Value::String(raw_val),
                });
                continue;
            }
            still_in_prop_prefix = false;
        }
        kept.push(line);
    }
    (kept.join("\n"), props)
}

// ── Page-level serialize ───────────────────────────────────────────

/// Reverse of `parse_page`. The contract is: feeding the output of
/// this fn back into `parse_page` produces an equivalent block tree
/// (modulo whitespace inside paragraph continuations).
///
/// Frontmatter is taken from the `Page.frontmatter_json` field
/// (consumer parses it into a `Vec<FrontmatterEntry>`); we accept
/// it parallel to `blocks` to avoid forcing the caller to round-trip
/// through JSON inside this fn.
pub fn serialize_page(page: &Page, blocks: &[Block]) -> String {
    let fm: Vec<FrontmatterEntry> =
        serde_json::from_str(&page.frontmatter_json).unwrap_or_default();
    let mut out = String::new();
    out.push_str(&serialize_frontmatter(&fm));

    // Caller is responsible for handing us blocks in sort_key order;
    // we double-check by sorting here too (cheap and avoids surprise).
    let mut ordered: Vec<&Block> = blocks.iter().collect();
    ordered.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));

    for (i, b) in ordered.iter().enumerate() {
        if i > 0 && !out.ends_with("\n\n") {
            if out.ends_with('\n') {
                out.push('\n');
            } else {
                out.push_str("\n\n");
            }
        }
        out.push_str(&serialize_block(b));
    }
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn serialize_block(b: &Block) -> String {
    let mut out = String::new();
    match b.kind.as_str() {
        "heading" => {
            let level = b.heading_level.unwrap_or(1).clamp(1, 6) as usize;
            out.push_str(&"#".repeat(level));
            out.push(' ');
            out.push_str(&b.content);
        }
        "thematic_break" => {
            out.push_str("***");
        }
        "callout" => {
            let kind = b.callout_kind.clone().unwrap_or_else(|| "note".into());
            let fold = if b.callout_foldable { "-" } else { "" };
            out.push_str(&format!("> [!{}]{}", kind, fold));
            for line in b.content.lines() {
                out.push('\n');
                out.push_str("> ");
                out.push_str(line);
            }
        }
        "blockquote" => {
            for (i, line) in b.content.lines().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                out.push_str("> ");
                out.push_str(line);
            }
        }
        "code" => {
            out.push_str("```");
            if let Some(lang) = &b.code_lang {
                out.push_str(lang);
            }
            out.push('\n');
            out.push_str(&b.content);
            if !b.content.ends_with('\n') {
                out.push('\n');
            }
            out.push_str("```");
        }
        "list_item" => {
            // v1 indent depth is not stored on Block (it's derived
            // by walking `parent_block_id`); the serializer assumes
            // depth-0 here and the caller (KnowledgeService) is
            // responsible for indenting nested children.
            let marker = if b.list_ordered { "1." } else { "-" };
            out.push_str(marker);
            out.push(' ');
            if let Some(mark) = &b.list_task {
                out.push_str(&format!("[{}] ", mark));
            }
            out.push_str(&b.content);
        }
        "table" => {
            out.push_str(&b.content);
        }
        "html" => {
            out.push_str(&b.content);
        }
        _ => {
            // paragraph + everything else
            out.push_str(&b.content);
        }
    }
    // Append logseq properties.
    let props: Vec<FrontmatterEntry> = serde_json::from_str(&b.properties_json).unwrap_or_default();
    for p in props {
        out.push('\n');
        let v = match &p.value {
            serde_json::Value::String(s) => s.clone(),
            other => other.to_string(),
        };
        out.push_str(&format!("{}:: {}", p.key, v));
    }
    if let Some(id) = &b.obsidian_block_id {
        // For block kinds where the id customarily lives at end of
        // the content line (list items), append it inline; for
        // multi-line kinds (callouts, blockquotes, code) put it on
        // its own line below.
        match b.kind.as_str() {
            "list_item" | "heading" | "paragraph" => {
                out.push(' ');
                out.push_str(&format!("^{id}"));
            }
            _ => {
                out.push('\n');
                out.push_str(&format!("^{id}"));
            }
        }
    }
    out
}

// ── Logseq block-ref translation ───────────────────────────────────

/// Translate Logseq-style `((uuid))` block references into Obsidian
/// `[[Page#^short-id]]` form. For each matched UUID, finds the
/// target block via `blocks` and rewrites the span. If the target
/// block doesn't yet have an `obsidian_block_id`, this function
/// returns a `(rewritten_src, assigned_ids)` map so the caller can
/// persist the new ids back.
///
/// Phase A simplification: the rewriter doesn't have access to the
/// `Page` (only `blocks`) and so emits `^short-id` without a
/// resolvable page name. The full implementation lives in the
/// import crate (Phase C, separate `knowledge-import` crate).
pub fn translate_logseq_block_refs(src: &str, blocks: &[Block]) -> String {
    let re = logseq_block_ref_re();
    re.replace_all(src, |caps: &regex::Captures| {
        let uuid_str = caps.get(1).unwrap().as_str();
        let target_id = match Uuid::parse_str(uuid_str) {
            Ok(id) => id,
            Err(_) => return caps.get(0).unwrap().as_str().to_string(),
        };
        match blocks.iter().find(|b| b.id == target_id) {
            Some(b) => {
                let short = b
                    .obsidian_block_id
                    .clone()
                    .unwrap_or_else(|| uuid_str[..8].to_string());
                format!("[[#^{short}]]")
            }
            None => caps.get(0).unwrap().as_str().to_string(),
        }
    })
    .to_string()
}

// ── Link formatting ────────────────────────────────────────────────

/// Build the canonical `[[…]]` form for a link to `target` from
/// `source_path`, respecting the vault's `new_link_format`.
///
/// - `"shortest"`: emit just the basename unless ambiguous (caller
///   handles ambiguity by passing a longer form).
/// - `"relative"`: emit a path relative to `source_path`'s folder.
/// - `"absolute"`: emit a vault-absolute path (without leading `/`).
pub fn format_link(
    target: &Page,
    source_path: &str,
    subpath: Option<&str>,
    alias: Option<&str>,
    vault: &Vault,
) -> String {
    let body = match vault.new_link_format.as_str() {
        "absolute" => strip_md(&target.path).to_string(),
        "relative" => relative_path(&target.path, source_path),
        _ => target.basename.clone(),
    };
    let sub = subpath.map(|s| format!("#{}", s)).unwrap_or_default();
    let al = alias.map(|s| format!("|{}", s)).unwrap_or_default();
    format!("[[{body}{sub}{al}]]")
}

fn strip_md(p: &str) -> &str {
    p.strip_suffix(".md").unwrap_or(p)
}

fn relative_path(target: &str, source: &str) -> String {
    let source_folder = source.rsplit_once('/').map(|(d, _)| d).unwrap_or("");
    if source_folder.is_empty() {
        return strip_md(target).to_string();
    }
    let target_no_ext = strip_md(target);
    // Walk back up from source to common prefix, then forward to target.
    let source_parts: Vec<&str> = source_folder.split('/').collect();
    let target_parts: Vec<&str> = target_no_ext.split('/').collect();
    let common = source_parts
        .iter()
        .zip(target_parts.iter())
        .take_while(|(a, b)| a == b)
        .count();
    let ups = source_parts.len() - common;
    let mut out = String::new();
    for _ in 0..ups {
        out.push_str("../");
    }
    for (i, part) in target_parts.iter().skip(common).enumerate() {
        if i > 0 {
            out.push('/');
        }
        out.push_str(part);
    }
    out
}
