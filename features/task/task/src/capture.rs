//! Minimal natural-language task capture.
//!
//! Input: `"Buy milk tomorrow #errands @shopping !high"`.
//! Output: a `TaskInfo` with the extractions applied + the
//! remaining text as `title`.
//!
//! Extraction rules (v1 — kept small on purpose):
//! - `#tag` → push to `tags`. The literal `#task` is
//!   auto-added so the page passes the
//!   `looks_like_task` discriminator.
//! - `@context`     → push to `contexts`.
//! - `[[Wikilink]]` → push to `projects`.
//! - `!low|normal|high|critical` → set `priority`.
//! - Bare date tokens (case-insensitive):
//!   - `today` → today (`due`)
//!   - `tomorrow` → today + 1
//!   - `next monday` … → next Mon…Sun
//!   - `mon` / `tue` / … → next occurrence of that weekday
//!   - `YYYY-MM-DD` → that date
//!
//! Whatever's left after extraction becomes the `title`. NLP
//! expansion (priority words "asap"/"urgent", deadline phrases
//! "by Friday", recurring "every Monday", duration "for 30m")
//! lives in a later slice — port the rules from
//! `tasknotes-nlp-core` when we need them.

use chrono::{Datelike, Duration, Local, NaiveDate, Weekday};

use crate::model::TaskInfo;

/// Parse an input line into a `TaskInfo`. The returned task
/// has `path = ""` — call [`crate::write::default_task_path`]
/// to compute one, then write via [`crate::write::write_task`].
pub fn capture(input: &str) -> TaskInfo {
    let mut tags: Vec<String> = Vec::new();
    let mut contexts: Vec<String> = Vec::new();
    let mut projects: Vec<String> = Vec::new();
    let mut priority: Option<String> = None;
    let mut due: Option<String> = None;

    let today = Local::now().date_naive();

    // Tokenize on whitespace, but preserve `[[multi word]]` as
    // a single token. We scan the source once with a small state
    // machine so the link bracket can hold spaces.
    let tokens = tokenize(input);
    // Multi-token date phrase pass first ("next monday" swallows
    // both tokens), so the single-token weekday rule below
    // doesn't grab "monday" before we see "next".
    let (tokens, phrase_due) = consume_date_phrase(tokens, today);
    let mut title_parts: Vec<String> = Vec::new();
    for tok in tokens {
        if let Some(tag) = tok.strip_prefix('#') {
            if !tag.is_empty() {
                tags.push(tag.to_string());
            }
            continue;
        }
        if let Some(ctx) = tok.strip_prefix('@') {
            if !ctx.is_empty() {
                contexts.push(format!("@{ctx}"));
            }
            continue;
        }
        if let Some(rest) = tok.strip_prefix("[[").and_then(|s| s.strip_suffix("]]")) {
            if !rest.is_empty() {
                projects.push(format!("[[{rest}]]"));
            }
            continue;
        }
        if let Some(prio) = tok.strip_prefix('!').and_then(canonical_priority) {
            priority = Some(prio.to_string());
            continue;
        }
        if let Some(date) = parse_date_token(&tok, today) {
            due = Some(date.format("%Y-%m-%d").to_string());
            continue;
        }
        title_parts.push(tok);
    }
    let due = due.or(phrase_due);

    // Ensure `task` is in tags for the discriminator.
    if !tags.iter().any(|t| t == "task") {
        tags.push("task".into());
    }

    let title = title_parts.join(" ").trim().to_string();
    TaskInfo {
        id: uuid::Uuid::new_v4(),
        path: String::new(),
        title: if title.is_empty() {
            "Untitled task".into()
        } else {
            title
        },
        status: "open".into(),
        priority: priority.unwrap_or_else(|| "normal".into()),
        due,
        scheduled: None,
        tags: crate::model::StringList(tags),
        contexts: crate::model::StringList(contexts),
        projects: crate::model::StringList(projects),
        project_id: None,
        milestone_id: None,
        time_estimate: None,
        time_entries: crate::model::TimeEntries::default(),
        recurrence: None,
        recurrence_anchor: None,
        complete_instances: crate::model::StringList::default(),
        completed_date: None,
        agent_profile: String::new(),
        dispatched_agent_tasks: crate::model::StringList::default(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    }
}

fn tokenize(src: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut in_link = false;
    for ch in src.chars() {
        if in_link {
            cur.push(ch);
            if cur.ends_with("]]") {
                out.push(std::mem::take(&mut cur));
                in_link = false;
            }
            continue;
        }
        if ch == '[' && cur.is_empty() {
            cur.push(ch);
            // Look-ahead is annoying; let the chunk close itself
            // on `]]`. Mark in_link as soon as we see the
            // opening `[`; a stray `[` becomes a one-char token.
            in_link = true;
            continue;
        }
        if ch.is_whitespace() {
            if !cur.is_empty() {
                out.push(std::mem::take(&mut cur));
            }
            continue;
        }
        cur.push(ch);
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}

fn canonical_priority(p: &str) -> Option<&'static str> {
    match p.to_ascii_lowercase().as_str() {
        "none" => Some("none"),
        "low" => Some("low"),
        "normal" | "medium" | "med" => Some("normal"),
        "high" => Some("high"),
        "critical" | "urgent" => Some("critical"),
        _ => None,
    }
}

fn parse_date_token(tok: &str, today: NaiveDate) -> Option<NaiveDate> {
    let lc = tok.to_ascii_lowercase();
    match lc.as_str() {
        "today" => Some(today),
        "tomorrow" | "tmrw" => Some(today + Duration::days(1)),
        // Single weekday → next occurrence (or today if it IS today).
        "mon" | "monday" => Some(next_weekday(today, Weekday::Mon)),
        "tue" | "tues" | "tuesday" => Some(next_weekday(today, Weekday::Tue)),
        "wed" | "weds" | "wednesday" => Some(next_weekday(today, Weekday::Wed)),
        "thu" | "thur" | "thurs" | "thursday" => Some(next_weekday(today, Weekday::Thu)),
        "fri" | "friday" => Some(next_weekday(today, Weekday::Fri)),
        "sat" | "saturday" => Some(next_weekday(today, Weekday::Sat)),
        "sun" | "sunday" => Some(next_weekday(today, Weekday::Sun)),
        _ => NaiveDate::parse_from_str(tok, "%Y-%m-%d").ok(),
    }
}

fn consume_date_phrase(tokens: Vec<String>, today: NaiveDate) -> (Vec<String>, Option<String>) {
    let mut out = Vec::with_capacity(tokens.len());
    let mut due: Option<String> = None;
    let mut i = 0;
    while i < tokens.len() {
        if due.is_none() && i + 1 < tokens.len() && tokens[i].eq_ignore_ascii_case("next") {
            if let Some(date) = parse_date_token(&tokens[i + 1], today) {
                due = Some(date.format("%Y-%m-%d").to_string());
                i += 2;
                continue;
            }
        }
        out.push(tokens[i].clone());
        i += 1;
    }
    (out, due)
}

fn next_weekday(today: NaiveDate, target: Weekday) -> NaiveDate {
    let today_num = i64::from(today.weekday().num_days_from_monday());
    let target_num = i64::from(target.num_days_from_monday());
    let delta = (target_num - today_num).rem_euclid(7);
    // "next mon" when it IS monday: 7 days out (next-next-monday).
    let delta = if delta == 0 { 7 } else { delta };
    today + Duration::days(delta)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_tags_contexts_projects() {
        let t = capture("Buy milk #errands #urgent @shopping [[Groceries]]");
        assert_eq!(t.title, "Buy milk");
        assert!(t.tags.0.contains(&"errands".into()));
        assert!(t.tags.0.contains(&"urgent".into()));
        assert!(t.tags.0.contains(&"task".into())); // auto-added
        assert_eq!(t.contexts.0, vec!["@shopping"]);
        assert_eq!(t.projects.0, vec!["[[Groceries]]"]);
    }

    #[test]
    fn extracts_priority() {
        let t = capture("Fix bug !critical");
        assert_eq!(t.title, "Fix bug");
        assert_eq!(t.priority, "critical");
    }

    #[test]
    fn extracts_today_tomorrow() {
        let today = Local::now().date_naive();
        let t = capture("Buy milk tomorrow");
        assert_eq!(
            t.due.as_deref(),
            Some(
                today
                    .succ_opt()
                    .unwrap()
                    .format("%Y-%m-%d")
                    .to_string()
                    .as_str()
            )
        );
        assert_eq!(t.title, "Buy milk");
    }

    #[test]
    fn extracts_next_weekday() {
        let t = capture("Standup next monday");
        assert!(t.due.is_some());
        assert_eq!(t.title, "Standup");
    }

    #[test]
    fn empty_input_gets_placeholder_title() {
        let t = capture("");
        assert_eq!(t.title, "Untitled task");
    }
}
