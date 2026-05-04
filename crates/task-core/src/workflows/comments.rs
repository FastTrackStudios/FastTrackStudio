//! Universal comment system — entity-agnostic, Nextcloud-compatible.
//!
//! Comments attach to any entity (task, output, session, project).
//!
//! ## Time references
//! Audio/video comments support point and range timecodes:
//! - Point: `[2:34]` — a single moment
//! - Range: `[2:30–2:36]` — a span (en-dash or hyphen)
//!
//! Internally stored as seconds (f64) for precise seeking.
//!
//! ## Spatial references
//! Stage plots and designs support x,y position references.
//!
//! ## Markdown format
//! ```markdown
//! ## Comments
//!
//! **@cody** (2026-04-11) [2:30–2:36]: Kick needs more low end in this section
//! > **@amy** (2026-04-11): Agreed, try boosting 60Hz by 2dB
//!
//! **@carter** (2026-04-11) [0:45]: Love the snare sound here ✅
//! ```
//!
//! ## Nextcloud sync
//! Round-trips to Nextcloud Deck card comments. The format is preserved.

use chrono::{NaiveDate, NaiveDateTime};
use facet::Facet;

// ── Time reference ──────────────────────────────────────────────────────────

/// A time reference — either a single point or a range.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct TimeRef {
    /// Start time in seconds.
    pub start: f64,
    /// End time in seconds. If None, this is a point reference.
    pub end: Option<f64>,
}

impl TimeRef {
    /// Create a point reference.
    pub fn point(seconds: f64) -> Self {
        Self {
            start: seconds,
            end: None,
        }
    }

    /// Create a range reference.
    pub fn range(start: f64, end: f64) -> Self {
        Self {
            start,
            end: Some(end),
        }
    }

    /// Whether this is a range (not a point).
    pub fn is_range(&self) -> bool {
        self.end.is_some()
    }

    /// Duration of the range, or 0 for a point.
    pub fn duration(&self) -> f64 {
        self.end.map_or(0.0, |e| e - self.start)
    }

    /// Whether a given time (in seconds) falls within this reference.
    pub fn contains(&self, t: f64) -> bool {
        match self.end {
            Some(end) => t >= self.start && t <= end,
            None => (t - self.start).abs() < 0.5, // within 0.5s for point refs
        }
    }

    /// Format as human-readable string: "2:34" or "2:30–2:36".
    pub fn display(&self) -> String {
        let fmt = |s: f64| -> String {
            let total = s as u64;
            let hours = total / 3600;
            let minutes = (total % 3600) / 60;
            let secs = total % 60;
            if hours > 0 {
                format!("{hours}:{minutes:02}:{secs:02}")
            } else {
                format!("{minutes}:{secs:02}")
            }
        };
        match self.end {
            Some(end) => format!("{}–{}", fmt(self.start), fmt(end)),
            None => fmt(self.start),
        }
    }
}

/// Parse a timecode string like "2:34" or "2:30–2:36" or "1:23:45–1:24:00".
pub fn parse_timecode(s: &str) -> Option<TimeRef> {
    // Split on en-dash, em-dash, or hyphen (but not if it looks like negative number)
    let parts: Vec<&str> = s
        .splitn(2, |c: char| c == '–' || c == '—' || c == '-')
        .map(|p| p.trim())
        .collect();

    let start = parse_time_str(parts[0])?;

    if parts.len() == 2 && !parts[1].is_empty() {
        let end = parse_time_str(parts[1])?;
        if end > start {
            return Some(TimeRef::range(start, end));
        }
    }

    Some(TimeRef::point(start))
}

/// Parse "M:SS" or "H:MM:SS" to seconds.
fn parse_time_str(s: &str) -> Option<f64> {
    let parts: Vec<&str> = s.split(':').collect();
    match parts.len() {
        2 => {
            let m: f64 = parts[0].parse().ok()?;
            let s: f64 = parts[1].parse().ok()?;
            Some(m * 60.0 + s)
        }
        3 => {
            let h: f64 = parts[0].parse().ok()?;
            let m: f64 = parts[1].parse().ok()?;
            let s: f64 = parts[2].parse().ok()?;
            Some(h * 3600.0 + m * 60.0 + s)
        }
        _ => None,
    }
}

// ── Spatial reference ───────────────────────────────────────────────────────

/// A 2D position reference for spatial comments (stage plots, designs).
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct SpatialRef {
    pub x: f64,
    pub y: f64,
    /// Optional label for the reference point.
    pub label: Option<String>,
}

// ── Comment ─────────────────────────────────────────────────────────────────

/// A single comment on any entity.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Comment {
    /// Stable unique ID (auto-generated from author+timestamp hash).
    pub id: String,
    /// Who wrote the comment.
    pub author: String,
    /// Comment text (may contain markdown).
    pub body: String,
    /// When the comment was created.
    pub created_at: Option<NaiveDateTime>,
    /// Time reference for audio/video (point or range).
    pub time_ref: Option<TimeRef>,
    /// Position reference for spatial contexts.
    pub spatial_ref: Option<SpatialRef>,
    /// Whether this comment has been addressed/resolved.
    pub resolved: bool,
    /// Who resolved it.
    pub resolved_by: Option<String>,
    /// ID of the parent comment. None = top-level. Supports nested replies.
    pub reply_to: Option<String>,
    /// @mentions extracted from the body.
    #[facet(default)]
    pub mentions: Vec<String>,
    /// External ID for Nextcloud Deck comment sync.
    pub external_id: Option<String>,
}

impl Comment {
    /// Generate a stable ID from author + timestamp + body prefix.
    pub fn generate_id(author: &str, created_at: Option<NaiveDateTime>, body: &str) -> String {
        let ts = created_at
            .map(|d| d.format("%Y%m%d%H%M%S").to_string())
            .unwrap_or_default();
        let prefix: String = body
            .chars()
            .take(16)
            .filter(|c| c.is_alphanumeric())
            .collect();
        let hash: u32 = format!("{author}{ts}{prefix}")
            .bytes()
            .fold(5381u32, |h, b| h.wrapping_mul(33).wrapping_add(b as u32));
        format!("c-{hash:08x}")
    }
}

impl Comment {
    /// Extract @mentions from the body text.
    pub fn extract_mentions(body: &str) -> Vec<String> {
        body.split_whitespace()
            .filter_map(|w| w.strip_prefix('@'))
            .map(|m| m.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_' && c != '-'))
            .filter(|m| !m.is_empty())
            .map(|m| m.to_string())
            .collect()
    }

    /// Get the thread depth of this comment (0 = top-level).
    pub fn depth(&self, all: &[Comment]) -> usize {
        let mut d = 0;
        let mut current = self.reply_to.as_deref();
        while let Some(parent_id) = current {
            d += 1;
            current = all
                .iter()
                .find(|c| c.id == parent_id)
                .and_then(|c| c.reply_to.as_deref());
            if d > 20 {
                break;
            } // safety
        }
        d
    }
}

// ── Thread helpers ──────────────────────────────────────────────────────────

/// Get all top-level comments (not replies).
pub fn top_level_comments(comments: &[Comment]) -> Vec<&Comment> {
    comments.iter().filter(|c| c.reply_to.is_none()).collect()
}

/// Get all direct replies to a specific comment by ID.
pub fn replies_to<'a>(comments: &'a [Comment], parent_id: &str) -> Vec<&'a Comment> {
    comments
        .iter()
        .filter(|c| c.reply_to.as_deref() == Some(parent_id))
        .collect()
}

/// Get a comment by ID.
pub fn find_comment<'a>(comments: &'a [Comment], id: &str) -> Option<&'a Comment> {
    comments.iter().find(|c| c.id == id)
}

/// Get the full thread starting from a comment (the comment + all nested replies).
pub fn thread_from<'a>(comments: &'a [Comment], root_id: &str) -> Vec<&'a Comment> {
    let mut result = Vec::new();
    if let Some(root) = find_comment(comments, root_id) {
        result.push(root);
        collect_replies(comments, root_id, &mut result);
    }
    result
}

fn collect_replies<'a>(comments: &'a [Comment], parent_id: &str, out: &mut Vec<&'a Comment>) {
    for reply in replies_to(comments, parent_id) {
        out.push(reply);
        collect_replies(comments, &reply.id, out);
    }
}

/// Get all comments that reference a given time (within tolerance).
pub fn comments_at_time(comments: &[Comment], time_seconds: f64) -> Vec<&Comment> {
    comments
        .iter()
        .filter(|c| {
            c.time_ref
                .as_ref()
                .map_or(false, |tr| tr.contains(time_seconds))
        })
        .collect()
}

/// Get all unresolved top-level comments.
pub fn unresolved_comments(comments: &[Comment]) -> Vec<&Comment> {
    comments
        .iter()
        .filter(|c| !c.resolved && c.reply_to.is_none())
        .collect()
}

// ── Markdown parse/render ───────────────────────────────────────────────────

/// Parse comments from a markdown body's `## Comments` section.
pub fn parse_comments(body: &str) -> Vec<Comment> {
    let mut comments: Vec<Comment> = Vec::new();
    let mut in_comments = false;

    for line in body.lines() {
        let trimmed = line.trim();
        if trimmed == "## Comments" {
            in_comments = true;
            continue;
        }
        if in_comments && trimmed.starts_with("## ") {
            break;
        }
        if !in_comments || trimmed.is_empty() {
            continue;
        }

        let is_reply = trimmed.starts_with('>');
        let content = if is_reply {
            trimmed.trim_start_matches('>').trim()
        } else {
            trimmed
        };

        if let Some(rest) = content.strip_prefix("**@") {
            if let Some(author_end) = rest.find("**") {
                let author = rest[..author_end].to_string();
                let after_author = rest[author_end + 2..].trim();

                let mut date = None;
                let mut time_ref = None;
                let mut cursor = after_author;

                // Parse (date)
                if let Some(d) = cursor.strip_prefix('(') {
                    if let Some(paren_end) = d.find(')') {
                        let date_str = &d[..paren_end];
                        date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
                            .ok()
                            .map(|d| d.and_hms_opt(0, 0, 0).unwrap());
                        cursor = d[paren_end + 1..].trim();
                    }
                }

                // Parse [timecode] or [start–end]
                if let Some(t) = cursor.strip_prefix('[') {
                    if let Some(bracket_end) = t.find(']') {
                        let tc_str = &t[..bracket_end];
                        time_ref = parse_timecode(tc_str);
                        cursor = t[bracket_end + 1..].trim();
                    }
                }

                let body_text = cursor.trim_start_matches(':').trim().to_string();
                let resolved = body_text.ends_with('✅') || body_text.contains(" ✅");
                let mentions = Comment::extract_mentions(&body_text);
                let id = Comment::generate_id(&author, date, &body_text);

                // Replies attach to the most recent top-level comment
                let reply_to = if is_reply {
                    comments
                        .iter()
                        .rev()
                        .find(|c| c.reply_to.is_none())
                        .map(|c| c.id.clone())
                } else {
                    None
                };

                comments.push(Comment {
                    id,
                    author,
                    body: body_text,
                    created_at: date,
                    time_ref,
                    resolved,
                    mentions,
                    reply_to,
                    ..Default::default()
                });
            }
        }
    }
    comments
}

/// Render comments back to markdown for the `## Comments` section.
pub fn render_comments(comments: &[Comment]) -> String {
    let mut out = String::from("## Comments\n\n");
    for comment in comments {
        let prefix = if comment.reply_to.is_some() { "> " } else { "" };
        let date = comment
            .created_at
            .map(|d| format!(" ({})", d.format("%Y-%m-%d")))
            .unwrap_or_default();
        let tc = comment
            .time_ref
            .as_ref()
            .map(|t| format!(" [{}]", t.display()))
            .unwrap_or_default();
        let resolved = if comment.resolved { " ✅" } else { "" };
        out.push_str(&format!(
            "{prefix}**@{}**{date}{tc}: {}{resolved}\n",
            comment.author, comment.body
        ));
        if comment.reply_to.is_none() {
            out.push('\n');
        }
    }
    out
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_point_timecode() {
        let tr = parse_timecode("2:34").unwrap();
        assert_eq!(tr.start, 154.0);
        assert!(tr.end.is_none());
        assert_eq!(tr.display(), "2:34");
    }

    #[test]
    fn parse_range_timecode() {
        let tr = parse_timecode("2:30–2:36").unwrap();
        assert_eq!(tr.start, 150.0);
        assert_eq!(tr.end, Some(156.0));
        assert!(tr.is_range());
        assert_eq!(tr.duration(), 6.0);
        assert_eq!(tr.display(), "2:30–2:36");
    }

    #[test]
    fn parse_range_with_hyphen() {
        let tr = parse_timecode("1:00-1:30").unwrap();
        assert_eq!(tr.start, 60.0);
        assert_eq!(tr.end, Some(90.0));
    }

    #[test]
    fn parse_hms_timecode() {
        let tr = parse_timecode("1:23:45").unwrap();
        assert_eq!(tr.start, 5025.0);
        assert_eq!(tr.display(), "1:23:45");
    }

    #[test]
    fn parse_hms_range() {
        let tr = parse_timecode("1:23:45–1:24:00").unwrap();
        assert_eq!(tr.start, 5025.0);
        assert_eq!(tr.end, Some(5040.0));
    }

    #[test]
    fn time_ref_contains() {
        let range = TimeRef::range(150.0, 156.0);
        assert!(range.contains(153.0));
        assert!(range.contains(150.0));
        assert!(range.contains(156.0));
        assert!(!range.contains(149.0));
        assert!(!range.contains(157.0));

        let point = TimeRef::point(100.0);
        assert!(point.contains(100.0));
        assert!(point.contains(100.3));
        assert!(!point.contains(101.0));
    }

    #[test]
    fn parse_render_roundtrip() {
        let md = r#"## Comments

**@cody** (2026-04-11) [2:30–2:36]: Kick needs more low end in this section
> **@amy** (2026-04-11): Agreed, try boosting 60Hz by 2dB

**@carter** (2026-04-11) [0:45]: Love the snare sound here ✅
"#;

        let comments = parse_comments(md);
        assert_eq!(comments.len(), 3);

        // First: ranged timecode
        assert_eq!(comments[0].author, "cody");
        assert!(comments[0].time_ref.as_ref().unwrap().is_range());
        assert_eq!(comments[0].time_ref.as_ref().unwrap().start, 150.0);
        assert_eq!(comments[0].time_ref.as_ref().unwrap().end, Some(156.0));
        assert!(comments[0].reply_to.is_none());

        // Second: reply
        assert_eq!(comments[1].author, "amy");
        assert!(comments[1].reply_to.is_some());
        assert!(comments[1].time_ref.is_none());

        // Third: point timecode, resolved
        assert_eq!(comments[2].author, "carter");
        assert!(comments[2].resolved);
        assert_eq!(comments[2].time_ref.as_ref().unwrap().start, 45.0);
        assert!(!comments[2].time_ref.as_ref().unwrap().is_range());

        // Render back
        let rendered = render_comments(&comments);
        assert!(rendered.contains("[2:30–2:36]"));
        assert!(rendered.contains("[0:45]"));
        assert!(rendered.contains("✅"));
    }

    #[test]
    fn extract_mentions() {
        let mentions = Comment::extract_mentions("Hey @cody and @amy check this out @carter!");
        assert_eq!(mentions, vec!["cody", "amy", "carter"]);
    }

    #[test]
    fn comments_at_time_query() {
        let comments = vec![
            Comment {
                author: "cody".into(),
                body: "too loud".into(),
                time_ref: Some(TimeRef::range(150.0, 156.0)),
                ..Default::default()
            },
            Comment {
                author: "amy".into(),
                body: "nice".into(),
                time_ref: Some(TimeRef::point(153.0)),
                ..Default::default()
            },
            Comment {
                author: "carter".into(),
                body: "general".into(),
                ..Default::default()
            },
        ];

        // At 153s: both cody's range and amy's point match
        let at_153 = comments_at_time(&comments, 153.0);
        assert_eq!(at_153.len(), 2);

        // At 200s: none match
        let at_200 = comments_at_time(&comments, 200.0);
        assert_eq!(at_200.len(), 0);
    }

    #[test]
    fn threading_with_ids() {
        let root = Comment {
            id: "c-root".into(),
            author: "cody".into(),
            body: "Thoughts on this section?".into(),
            ..Default::default()
        };
        let reply1 = Comment {
            id: "c-reply1".into(),
            author: "amy".into(),
            body: "Sounds great".into(),
            reply_to: Some("c-root".into()),
            ..Default::default()
        };
        let nested = Comment {
            id: "c-nested".into(),
            author: "carter".into(),
            body: "Agree with @amy".into(),
            reply_to: Some("c-reply1".into()),
            ..Default::default()
        };
        let other_root = Comment {
            id: "c-other".into(),
            author: "tom".into(),
            body: "Unrelated comment".into(),
            ..Default::default()
        };

        let all = vec![root, reply1, nested, other_root];

        // Top level
        let top = top_level_comments(&all);
        assert_eq!(top.len(), 2);
        assert_eq!(top[0].id, "c-root");
        assert_eq!(top[1].id, "c-other");

        // Replies to root
        let root_replies = replies_to(&all, "c-root");
        assert_eq!(root_replies.len(), 1);
        assert_eq!(root_replies[0].id, "c-reply1");

        // Nested reply
        let nested_replies = replies_to(&all, "c-reply1");
        assert_eq!(nested_replies.len(), 1);
        assert_eq!(nested_replies[0].id, "c-nested");

        // Full thread from root
        let thread = thread_from(&all, "c-root");
        assert_eq!(thread.len(), 3); // root + reply1 + nested

        // Depth
        assert_eq!(all[0].depth(&all), 0); // root
        assert_eq!(all[1].depth(&all), 1); // reply1
        assert_eq!(all[2].depth(&all), 2); // nested reply
    }

    #[test]
    fn parse_assigns_stable_ids() {
        let md = "## Comments\n\n**@cody** (2026-04-11): First comment\n\n**@amy** (2026-04-11): Second comment\n";
        let comments = parse_comments(md);
        assert_eq!(comments.len(), 2);
        // IDs should be non-empty and different
        assert!(!comments[0].id.is_empty());
        assert!(!comments[1].id.is_empty());
        assert_ne!(comments[0].id, comments[1].id);
        // Same input should produce same IDs (deterministic)
        let comments2 = parse_comments(md);
        assert_eq!(comments[0].id, comments2[0].id);
    }

    #[test]
    fn reply_links_to_parent_id() {
        let md = "## Comments\n\n**@cody** (2026-04-11) [1:00–1:05]: Check this range\n> **@amy** (2026-04-11): On it\n";
        let comments = parse_comments(md);
        assert_eq!(comments.len(), 2);
        assert!(comments[0].reply_to.is_none());
        assert_eq!(
            comments[1].reply_to.as_deref(),
            Some(comments[0].id.as_str())
        );
    }
}
