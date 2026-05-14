//! `InlineDiff` — minimal line-by-line plus/minus diff for tool-call
//! file edits (`Edit`, `Write`, `NotebookEdit`).
//!
//! Pure split-and-zip implementation: if either side is empty, every
//! line of the other side renders as add/remove. Otherwise we align
//! by line index — sufficient for the spec's "render an inline diff"
//! requirement without pulling in a full diff algorithm. The proper
//! Myers / patience diff is a future polish step (see issue #24).

use dioxus::prelude::*;
use fts_ui::prelude::*;

#[component]
pub fn InlineDiff(before: String, after: String) -> Element {
    let lines = compute_lines(&before, &after);
    rsx! {
        div { class: "rounded-md border border-border bg-muted/20 font-mono text-xs overflow-x-auto",
            for (i, line) in lines.iter().enumerate() {
                DiffLine {
                    key: "{i}",
                    line: line.clone(),
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DiffKind {
    Context,
    Add,
    Remove,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiffLineEntry {
    pub kind: DiffKind,
    pub text: String,
}

pub fn compute_lines(before: &str, after: &str) -> Vec<DiffLineEntry> {
    let before_lines: Vec<&str> = before.lines().collect();
    let after_lines: Vec<&str> = after.lines().collect();

    // Common prefix of identical lines.
    let mut prefix = 0;
    while prefix < before_lines.len()
        && prefix < after_lines.len()
        && before_lines[prefix] == after_lines[prefix]
    {
        prefix += 1;
    }
    // Common suffix.
    let mut suffix = 0;
    while suffix < before_lines.len() - prefix
        && suffix < after_lines.len() - prefix
        && before_lines[before_lines.len() - 1 - suffix]
            == after_lines[after_lines.len() - 1 - suffix]
    {
        suffix += 1;
    }

    let mut out = Vec::new();
    for line in &before_lines[..prefix] {
        out.push(DiffLineEntry {
            kind: DiffKind::Context,
            text: (*line).to_string(),
        });
    }
    for line in &before_lines[prefix..before_lines.len() - suffix] {
        out.push(DiffLineEntry {
            kind: DiffKind::Remove,
            text: (*line).to_string(),
        });
    }
    for line in &after_lines[prefix..after_lines.len() - suffix] {
        out.push(DiffLineEntry {
            kind: DiffKind::Add,
            text: (*line).to_string(),
        });
    }
    for line in &before_lines[before_lines.len() - suffix..] {
        out.push(DiffLineEntry {
            kind: DiffKind::Context,
            text: (*line).to_string(),
        });
    }
    out
}

#[component]
fn DiffLine(line: DiffLineEntry) -> Element {
    let (sigil, bg) = match line.kind {
        DiffKind::Add => ("+ ", "bg-emerald-500/10 text-emerald-600"),
        DiffKind::Remove => ("- ", "bg-rose-500/10 text-rose-600"),
        DiffKind::Context => ("  ", "text-muted-foreground"),
    };
    let class = format!("flex gap-2 px-2 py-0.5 {bg}");
    rsx! {
        div { class,
            span { class: "select-none", "{sigil}" }
            span { class: "whitespace-pre", "{line.text}" }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pure_addition() {
        let lines = compute_lines("", "new line");
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].kind, DiffKind::Add);
        assert_eq!(lines[0].text, "new line");
    }

    #[test]
    fn pure_removal() {
        let lines = compute_lines("dead line", "");
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].kind, DiffKind::Remove);
    }

    #[test]
    fn prefix_and_suffix_preserved() {
        let before = "a\nb\nc";
        let after = "a\nB\nc";
        let lines = compute_lines(before, after);
        let kinds: Vec<_> = lines.iter().map(|l| l.kind.clone()).collect();
        assert_eq!(
            kinds,
            vec![
                DiffKind::Context,
                DiffKind::Remove,
                DiffKind::Add,
                DiffKind::Context
            ]
        );
    }

    #[test]
    fn identical_is_all_context() {
        let lines = compute_lines("same\nfile\n", "same\nfile\n");
        assert!(lines.iter().all(|l| l.kind == DiffKind::Context));
    }
}
