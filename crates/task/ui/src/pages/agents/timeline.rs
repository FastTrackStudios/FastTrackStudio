//! The transcript's pure row model — t3code's
//! `MessagesTimeline.logic.ts` as Rust: a discriminated union of
//! row kinds derived by plain functions, so the view is one
//! `match` and the folding/grouping behavior is unit-tested.

use agent_proto::message::{Message, Role};

/// Tone of one activity line (tool event / warning), driving the
/// status glyph: `Running` ▸, `Ok` ✓, `Fail` ✗, `Note` ·.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ToolTone {
    Running,
    Ok,
    Fail,
    Note,
}

/// One line in a turn's activity log.
#[derive(Clone, PartialEq, Debug)]
pub struct ActivityLine {
    pub tone: ToolTone,
    pub text: String,
    /// Tool-call id when known — lets `ToolFinished` settle the
    /// matching `Running` line instead of appending a duplicate.
    pub tool_id: String,
}

/// The retained work log of one completed turn, anchored to the
/// assistant message that concluded it.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct TurnLog {
    pub lines: Vec<ActivityLine>,
    pub reasoning: String,
    pub duration_secs: i64,
}

/// One transcript row. `Message` carries an index into the
/// caller's message list; `TurnFold` anchors a completed turn's
/// work log *before* its assistant message (t3code's turn-fold).
#[derive(Clone, PartialEq, Debug)]
pub enum Row {
    Message(usize),
    TurnFold {
        /// Anchor assistant-message id (the `TurnLog` key).
        anchor: String,
        tool_count: usize,
        duration_secs: i64,
    },
}

/// Derive the settled transcript rows: each assistant message that
/// has a retained `TurnLog` gets a fold row directly above it.
pub fn build_rows(
    messages: &[Message],
    turns: &std::collections::HashMap<String, TurnLog>,
) -> Vec<Row> {
    let mut rows = Vec::with_capacity(messages.len() + turns.len());
    for (i, m) in messages.iter().enumerate() {
        if matches!(m.role, Role::Assistant) {
            if let Some(log) = turns.get(&m.id) {
                if !log.lines.is_empty() || !log.reasoning.is_empty() {
                    rows.push(Row::TurnFold {
                        anchor: m.id.clone(),
                        tool_count: log.lines.len(),
                        duration_secs: log.duration_secs,
                    });
                }
            }
        }
        rows.push(Row::Message(i));
    }
    rows
}

/// Append a live activity line, deduping immediate repeats and
/// capping the list (drops oldest).
pub fn push_line(lines: &mut Vec<ActivityLine>, line: ActivityLine) {
    if lines.last().is_some_and(|l| l.text == line.text && l.tone == line.tone) {
        return;
    }
    lines.push(line);
    let overflow = lines.len().saturating_sub(200);
    if overflow > 0 {
        lines.drain(..overflow);
    }
}

/// Settle a `Running` line when its tool finishes: match by tool id
/// (or, failing that, the most recent `Running` line), set the tone
/// and append the duration. Appends a fresh line when no running
/// line matches (finish-without-start).
pub fn settle_tool(lines: &mut Vec<ActivityLine>, tool_id: &str, text: String, ok: bool, duration_ms: u32) {
    let tone = if ok { ToolTone::Ok } else { ToolTone::Fail };
    let suffix = if duration_ms > 0 {
        format!(" · {:.1}s", f64::from(duration_ms) / 1000.0)
    } else {
        String::new()
    };
    let target = lines
        .iter()
        .rposition(|l| l.tone == ToolTone::Running && !tool_id.is_empty() && l.tool_id == tool_id)
        .or_else(|| lines.iter().rposition(|l| l.tone == ToolTone::Running));
    match target {
        Some(i) => {
            lines[i].tone = tone;
            lines[i].text = format!("{text}{suffix}");
        }
        None => push_line(
            lines,
            ActivityLine {
                tone,
                text: format!("{text}{suffix}"),
                tool_id: tool_id.to_string(),
            },
        ),
    }
}

/// The one-line fold summary: "3 tool calls · 0:42".
pub fn fold_summary(tool_count: usize, duration_secs: i64) -> String {
    let noun = if tool_count == 1 { "tool call" } else { "tool calls" };
    if duration_secs > 0 {
        format!(
            "{tool_count} {noun} · {}",
            super::logic::fmt_elapsed(duration_secs)
        )
    } else {
        format!("{tool_count} {noun}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use agent_proto::message::ContentBlock;
    use chrono::Utc;

    fn msg(id: &str, role: Role) -> Message {
        Message {
            id: id.into(),
            session_id: "s".into(),
            role,
            content: vec![ContentBlock::Text { text: "t".into() }],
            partial: false,
            errored: false,
            error_text: String::new(),
            reasoning: None,
            created_at: Utc::now(),
        }
    }

    fn line(tone: ToolTone, text: &str, id: &str) -> ActivityLine {
        ActivityLine {
            tone,
            text: text.into(),
            tool_id: id.into(),
        }
    }

    #[test]
    fn fold_lands_before_its_assistant_message_only() {
        let messages = vec![
            msg("u1", Role::User),
            msg("a1", Role::Assistant),
            msg("u2", Role::User),
            msg("a2", Role::Assistant),
        ];
        let mut turns = std::collections::HashMap::new();
        turns.insert(
            "a2".to_string(),
            TurnLog {
                lines: vec![line(ToolTone::Ok, "grep", "t1")],
                reasoning: String::new(),
                duration_secs: 42,
            },
        );
        let rows = build_rows(&messages, &turns);
        assert_eq!(
            rows,
            vec![
                Row::Message(0),
                Row::Message(1),
                Row::Message(2),
                Row::TurnFold {
                    anchor: "a2".into(),
                    tool_count: 1,
                    duration_secs: 42
                },
                Row::Message(3),
            ]
        );
    }

    #[test]
    fn empty_turn_logs_produce_no_fold() {
        let messages = vec![msg("a1", Role::Assistant)];
        let mut turns = std::collections::HashMap::new();
        turns.insert("a1".to_string(), TurnLog::default());
        assert_eq!(build_rows(&messages, &turns), vec![Row::Message(0)]);
    }

    #[test]
    fn push_line_dedupes_and_caps() {
        let mut lines = Vec::new();
        push_line(&mut lines, line(ToolTone::Note, "x", ""));
        push_line(&mut lines, line(ToolTone::Note, "x", ""));
        assert_eq!(lines.len(), 1);
        for i in 0..250 {
            push_line(&mut lines, line(ToolTone::Note, &format!("l{i}"), ""));
        }
        assert_eq!(lines.len(), 200);
    }

    #[test]
    fn settle_matches_by_id_then_falls_back_to_last_running() {
        let mut lines = vec![
            line(ToolTone::Running, "run a", "a"),
            line(ToolTone::Running, "run b", "b"),
        ];
        settle_tool(&mut lines, "a", "ran a".into(), true, 1500);
        assert_eq!(lines[0].tone, ToolTone::Ok);
        assert_eq!(lines[0].text, "ran a · 1.5s");
        settle_tool(&mut lines, "", "ran b".into(), false, 0);
        assert_eq!(lines[1].tone, ToolTone::Fail);
        // Finish-without-start appends.
        settle_tool(&mut lines, "c", "ran c".into(), true, 0);
        assert_eq!(lines.len(), 3);
    }

    #[test]
    fn fold_summary_pluralizes() {
        assert_eq!(fold_summary(1, 0), "1 tool call");
        assert_eq!(fold_summary(3, 42), "3 tool calls · 0:42");
    }
}
