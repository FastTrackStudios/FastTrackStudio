// r[impl capture.nlp]
//! NLP capture parser — turns a free-form text string into a partial task.
//!
//! Syntax recognised:
//!   `!priority`        — sets priority (low / normal / high / urgent)
//!   `#tag`             — adds a tag
//!   `@context`         — adds a context
//!   `[[Project Name]]` — adds a project WikiLink (may contain spaces)
//!   date expression    — see below
//!   remaining text     — becomes the title
//!
//! Date expressions recognised (case-insensitive):
//!   • Explicit: today, tomorrow, yesterday
//!   • Relative: next/last/this {weekday}, in N days/weeks
//!   • Plain weekday name: "Friday" → nearest future Friday
//!   • ISO date: YYYY-MM-DD
//!   • fuzzydate expressions starting with a date-starter word:
//!       "last Friday", "this week", "January 15", "a week from now",
//!       "end of month", "3 days ago", etc.
//!
//! The date scanner tries every contiguous word window (longest first) from
//! each position. Explicit patterns are tried before fuzzydate to avoid
//! false positives (e.g. bare "3" parsing as 3am = today).

use chrono::{Datelike as _, Duration, NaiveDate, NaiveDateTime, Weekday};

use crate::task::{Priority, WikiLink};

/// The result of parsing a capture string.
#[derive(Debug, Clone, Default)]
pub struct CaptureInput {
    pub title: String,
    pub priority: Option<Priority>,
    pub tags: Vec<String>,
    pub contexts: Vec<String>,
    pub projects: Vec<WikiLink>,
    pub due: Option<NaiveDate>,
}

/// Parse a free-form capture string into a `CaptureInput`.
// r[impl capture.nlp]
pub fn parse_capture(input: &str) -> CaptureInput {
    let now: NaiveDateTime = chrono::Local::now().naive_local();
    let today = now.date();
    let mut result = CaptureInput::default();

    // ── 1. Extract [[WikiLinks]] (may contain spaces) ────────────────────────
    let mut remaining = String::new();
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '[' && chars.peek() == Some(&'[') {
            chars.next(); // consume second '['
            let mut link = String::new();
            loop {
                match chars.next() {
                    None => break,
                    Some(']') if chars.peek() == Some(&']') => {
                        chars.next(); // consume second ']'
                        break;
                    }
                    Some(c) => link.push(c),
                }
            }
            let name = link.trim().to_string();
            if !name.is_empty() {
                result.projects.push(WikiLink(name));
            }
            remaining.push(' '); // placeholder preserves word boundaries
        } else {
            remaining.push(ch);
        }
    }

    // ── 2. Extract !, #, @ tokens ────────────────────────────────────────────
    let tokens: Vec<&str> = remaining.split_whitespace().collect();
    let mut rest: Vec<&str> = Vec::new();

    for tok in &tokens {
        if let Some(prio_str) = tok.strip_prefix('!') {
            if let Some(p) = parse_priority(prio_str) {
                result.priority = Some(p);
                continue;
            }
        }
        if let Some(tag) = tok.strip_prefix('#') {
            if !tag.is_empty() {
                result.tags.push(tag.to_string());
                continue;
            }
        }
        if let Some(ctx) = tok.strip_prefix('@') {
            if !ctx.is_empty() {
                result.contexts.push(ctx.to_string());
                continue;
            }
        }
        rest.push(tok);
    }

    // ── 3. Find date expression via sliding window ───────────────────────────
    // Try every contiguous subsequence of words, longest first from each start.
    // Explicit patterns are tried before fuzzydate to avoid false positives.
    let mut date_range: Option<(usize, usize)> = None;

    'outer: for start in 0..rest.len() {
        for end in (start + 1..=rest.len()).rev() {
            let window = &rest[start..end];
            let candidate = window.join(" ");

            // ISO date (single token only)
            if end - start == 1 {
                if let Ok(d) = candidate.parse::<NaiveDate>() {
                    result.due = Some(d);
                    date_range = Some((start, end));
                    break 'outer;
                }
            }

            // Explicit patterns (today, tomorrow, yesterday, in N days/weeks,
            // next/last/this {weekday}, plain {weekday})
            if let Some(d) = try_explicit_date(window, today) {
                result.due = Some(d);
                date_range = Some((start, end));
                break 'outer;
            }

            // fuzzydate — only for windows whose first word looks like a date
            // starter. This prevents title words like "Fix" or bare numbers
            // from false-positive matching (e.g. "3" → today as 3am).
            if is_date_starter(window[0]) {
                if let Ok(dt) = fuzzydate::parse_relative_to(candidate, now) {
                    result.due = Some(dt.date());
                    date_range = Some((start, end));
                    break 'outer;
                }
            }
        }
    }

    // ── 4. Build title from non-date words ───────────────────────────────────
    let title_words: Vec<&str> = rest
        .iter()
        .enumerate()
        .filter(|(i, _)| match date_range {
            Some((s, e)) => *i < s || *i >= e,
            None => true,
        })
        .map(|(_, w)| *w)
        .collect();

    result.title = title_words.join(" ");
    result
}

// ── Explicit date patterns ────────────────────────────────────────────────────

/// Patterns fuzzydate doesn't cover or where we want guaranteed behaviour.
/// Each pattern is strict about token count so long windows don't accidentally
/// consume words that follow the date token.
fn try_explicit_date(tokens: &[&str], today: NaiveDate) -> Option<NaiveDate> {
    let first = tokens[0].to_lowercase();

    // Single-token keywords
    if tokens.len() == 1 {
        match first.as_str() {
            "today" => return Some(today),
            "tomorrow" => return Some(today + Duration::days(1)),
            "yesterday" => return Some(today - Duration::days(1)),
            _ => {}
        }
        if let Some(wd) = parse_weekday(&first) {
            return Some(next_weekday(today, wd));
        }
    }

    // Two-token: "next/last/this {weekday}"
    if tokens.len() == 2 && (first == "next" || first == "last" || first == "this") {
        if let Some(wd) = parse_weekday(tokens[1]) {
            return Some(match first.as_str() {
                "last" => prev_weekday(today, wd),
                _ => next_weekday(today, wd),
            });
        }
    }

    // Three-token: "in N days" | "in N weeks"
    if tokens.len() == 3 && first == "in" {
        if let Ok(n) = tokens[1].parse::<i64>() {
            let unit = tokens[2].to_lowercase();
            let unit = unit.trim_end_matches('s');
            match unit {
                "day" => return Some(today + Duration::days(n)),
                "week" => return Some(today + Duration::weeks(n)),
                _ => {}
            }
        }
    }

    None
}

// ── fuzzydate guard ───────────────────────────────────────────────────────────

/// Returns true if this word could plausibly start a date expression that
/// fuzzydate handles but `try_explicit_date` does not — e.g. "last", "January",
/// "a", "end". Prevents ordinary title words from triggering fuzzydate.
fn is_date_starter(word: &str) -> bool {
    matches!(
        word.to_lowercase().as_str(),
        // relative anchors
        |"last"| "this" | "a" | "an" | "end" | "start" | "beginning"
        // month names (full + 3-letter abbrev)
        | "january" | "jan" | "february" | "feb" | "march" | "mar"
        | "april"   | "apr" | "may"      | "june" | "jun"  | "july" | "jul"
        | "august"  | "aug" | "september"| "sep"  | "october" | "oct"
        | "november"| "nov" | "december" | "dec"
        // weekday names (covered by explicit parser but harmless to include)
        | "monday" | "mon" | "tuesday"  | "tue" | "wednesday" | "wed"
        | "thursday"| "thu"| "friday"   | "fri" | "saturday"  | "sat"
        | "sunday"  | "sun" // ordinal numbers ("1st", "2nd" etc.)
    ) || is_ordinal(word)
}

fn is_ordinal(s: &str) -> bool {
    let s = s.to_lowercase();
    s.ends_with("st") || s.ends_with("nd") || s.ends_with("rd") || s.ends_with("th")
}

// ── Weekday helpers ───────────────────────────────────────────────────────────

fn parse_weekday(s: &str) -> Option<Weekday> {
    match s.to_lowercase().as_str() {
        "monday" | "mon" => Some(Weekday::Mon),
        "tuesday" | "tue" => Some(Weekday::Tue),
        "wednesday" | "wed" => Some(Weekday::Wed),
        "thursday" | "thu" => Some(Weekday::Thu),
        "friday" | "fri" => Some(Weekday::Fri),
        "saturday" | "sat" => Some(Weekday::Sat),
        "sunday" | "sun" => Some(Weekday::Sun),
        _ => None,
    }
}

/// Nearest future occurrence of `target` (strictly after `from`).
fn next_weekday(from: NaiveDate, target: Weekday) -> NaiveDate {
    let days = (target.num_days_from_monday() as i64
        - from.weekday().num_days_from_monday() as i64)
        .rem_euclid(7);
    from + Duration::days(if days == 0 { 7 } else { days })
}

/// Nearest past occurrence of `target` (strictly before `from`).
fn prev_weekday(from: NaiveDate, target: Weekday) -> NaiveDate {
    let days = (from.weekday().num_days_from_monday() as i64
        - target.num_days_from_monday() as i64)
        .rem_euclid(7);
    from - Duration::days(if days == 0 { 7 } else { days })
}

// ── Priority helper ───────────────────────────────────────────────────────────

fn parse_priority(s: &str) -> Option<Priority> {
    match s.to_lowercase().as_str() {
        "low" => Some(Priority::Low),
        "normal" | "medium" => Some(Priority::Normal),
        "high" => Some(Priority::High),
        "urgent" | "critical" => Some(Priority::Urgent),
        _ => None,
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn today() -> NaiveDate {
        chrono::Local::now().date_naive()
    }

    #[test]
    fn test_basic_title() {
        let r = parse_capture("Fix the login bug");
        assert_eq!(r.title, "Fix the login bug");
        assert!(r.priority.is_none());
        assert!(r.due.is_none());
    }

    #[test]
    fn test_priority_token() {
        let r = parse_capture("Fix auth !high");
        assert_eq!(r.title, "Fix auth");
        assert_eq!(r.priority, Some(Priority::High));
    }

    #[test]
    fn test_tags_and_contexts() {
        let r = parse_capture("Review PR #backend @dev");
        assert_eq!(r.title, "Review PR");
        assert_eq!(r.tags, vec!["backend"]);
        assert_eq!(r.contexts, vec!["dev"]);
    }

    #[test]
    fn test_wikilink_project() {
        let r = parse_capture("Write docs [[My Project]]");
        assert_eq!(r.title, "Write docs");
        assert_eq!(r.projects, vec![WikiLink("My Project".to_string())]);
    }

    #[test]
    fn test_date_today() {
        let r = parse_capture("Do thing today");
        assert_eq!(r.due, Some(today()));
        assert_eq!(r.title, "Do thing");
    }

    #[test]
    fn test_date_tomorrow() {
        let r = parse_capture("Do thing tomorrow");
        assert_eq!(r.due, Some(today() + Duration::days(1)));
        assert_eq!(r.title, "Do thing");
    }

    #[test]
    fn test_date_yesterday() {
        let r = parse_capture("Follow up yesterday");
        assert_eq!(r.due, Some(today() - Duration::days(1)));
        assert_eq!(r.title, "Follow up");
    }

    #[test]
    fn test_date_in_n_days() {
        let r = parse_capture("Follow up in 3 days");
        assert_eq!(r.due, Some(today() + Duration::days(3)));
        assert_eq!(r.title, "Follow up");
    }

    #[test]
    fn test_date_in_n_weeks() {
        let r = parse_capture("Review contract in 2 weeks");
        assert_eq!(r.due, Some(today() + Duration::weeks(2)));
        assert_eq!(r.title, "Review contract");
    }

    #[test]
    fn test_date_next_weekday() {
        let r = parse_capture("Record guitar next Monday");
        let due = r.due.unwrap();
        assert!(due > today());
        assert_eq!(due.weekday(), Weekday::Mon);
        assert_eq!(r.title, "Record guitar");
    }

    #[test]
    fn test_date_last_weekday() {
        let r = parse_capture("Update notes last Friday");
        let due = r.due.unwrap();
        assert!(due < today());
        assert_eq!(due.weekday(), Weekday::Fri);
        assert_eq!(r.title, "Update notes");
    }

    #[test]
    fn test_date_plain_weekday() {
        let r = parse_capture("Call dentist Friday");
        let due = r.due.unwrap();
        assert!(due > today());
        assert_eq!(due.weekday(), Weekday::Fri);
        assert_eq!(r.title, "Call dentist");
    }

    #[test]
    fn test_date_iso() {
        let r = parse_capture("Submit report 2026-05-01");
        assert_eq!(r.due, Some(NaiveDate::from_ymd_opt(2026, 5, 1).unwrap()));
        assert_eq!(r.title, "Submit report");
    }

    #[test]
    fn test_date_month_day_fuzzydate() {
        // fuzzydate handles "January 15"
        let r = parse_capture("File taxes January 15");
        assert!(r.due.is_some());
        let due = r.due.unwrap();
        assert_eq!(due.month(), 1);
        assert_eq!(due.day(), 15);
        assert_eq!(r.title, "File taxes");
    }

    #[test]
    fn test_date_mid_sentence() {
        let r = parse_capture("Call John tomorrow about the contract");
        assert_eq!(r.due, Some(today() + Duration::days(1)));
        assert_eq!(r.title, "Call John about the contract");
    }

    #[test]
    fn test_no_false_positive_number() {
        // bare "3" must not be parsed as a date
        let r = parse_capture("Pick up 3 items");
        assert!(r.due.is_none());
        assert_eq!(r.title, "Pick up 3 items");
    }

    #[test]
    fn test_combined() {
        let r = parse_capture("Fix auth bug !high #backend @frontend [[Task]] tomorrow");
        assert_eq!(r.title, "Fix auth bug");
        assert_eq!(r.priority, Some(Priority::High));
        assert_eq!(r.tags, vec!["backend"]);
        assert_eq!(r.contexts, vec!["frontend"]);
        assert_eq!(r.projects, vec![WikiLink("Task".to_string())]);
        assert_eq!(r.due, Some(today() + Duration::days(1)));
    }
}
