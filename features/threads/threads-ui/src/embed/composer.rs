//! Composer — author input + body Textarea + kind picker chip +
//! mention autocomplete + attachment paperclip stub + Promote-to-task.
//!
//! The composer emits a [`ComposerSubmit`] on send; the caller (usually
//! [`ThreadEmbed`]) wraps it in a `CommentCreate`. The composer does
//! NOT call the repo and owns no global state.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Paperclip, Send};
use fts_ui::prelude::*;
use threads_proto::THREAD_KINDS;

#[derive(Clone, PartialEq, Debug)]
pub struct ComposerSubmit {
    pub author: String,
    pub body: String,
    pub kind: String,
    pub mentions: Vec<String>,
}

#[derive(Props, Clone, PartialEq)]
pub struct ThreadComposerProps {
    /// Pool of usernames the mention autocomplete pulls from.
    #[props(default)]
    pub mention_pool: Vec<String>,
    pub on_submit: EventHandler<ComposerSubmit>,
    /// Fires only when the current draft kind is `"action"` and the user
    /// clicks Promote-to-task. Body is the current draft body.
    pub on_promote_to_task: EventHandler<String>,
}

#[component]
pub fn ThreadComposer(props: ThreadComposerProps) -> Element {
    let mut author = use_signal(String::new);
    let mut body = use_signal(String::new);
    let mut kind = use_signal(|| "discussion".to_string());

    // Mention autocomplete: when body ends with `@<token>` we surface
    // matching mention pool entries. Mentions submitted with the comment
    // are simply any usernames found via `extract_mentions` at send time.
    let body_str = body.read().clone();
    let suggestions = mention_suggestions(&body_str, &props.mention_pool);

    let send = move |_| {
        let a = author.read().trim().to_string();
        let b = body.read().trim().to_string();
        if a.is_empty() || b.is_empty() {
            return;
        }
        let mentions = extract_mentions(&b, &props.mention_pool);
        props.on_submit.call(ComposerSubmit {
            author: a,
            body: b,
            kind: kind.read().clone(),
            mentions,
        });
        author.set(String::new());
        body.set(String::new());
        kind.set("discussion".into());
    };

    let is_action = kind.read().clone() == "action";

    rsx! {
        div { class: "flex flex-col gap-2 border-t border-border pt-2",
            div { class: "flex gap-2 items-center",
                Input {
                    placeholder: "author".to_string(),
                    value: author,
                }
                // Kind picker — a small chip-style select.
                select {
                    class: "px-2 py-1 rounded-md border border-border bg-background text-xs",
                    value: "{kind}",
                    onchange: move |evt: FormEvent| kind.set(evt.value()),
                    for k in THREAD_KINDS.iter() {
                        option { value: "{k}", "{k}" }
                    }
                }
            }
            Textarea {
                placeholder: "Write a comment\u{2026} use @username to mention".to_string(),
                value: body,
            }
            if !suggestions.is_empty() {
                div { class: "flex flex-wrap gap-1",
                    for name in suggestions.iter().cloned() {
                        button {
                            class: "px-2 py-0.5 rounded-full text-xs bg-muted hover:bg-accent",
                            r#type: "button",
                            onclick: move |_| {
                                let current = body.read().clone();
                                if let Some(at_idx) = current.rfind('@') {
                                    let mut next = current[..at_idx].to_string();
                                    next.push('@');
                                    next.push_str(&name);
                                    next.push(' ');
                                    body.set(next);
                                }
                            },
                            "@{name}"
                        }
                    }
                }
            }
            div { class: "flex gap-2 items-center justify-between",
                div { class: "flex gap-1",
                    // Attachment paperclip stub — fires no event yet;
                    // Phase C wires upload.
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        
                        on_click: move |_| {},
                        Paperclip { size: 14 }
                    }
                }
                div { class: "flex gap-2",
                    if is_action {
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: move |_| {
                                let b = body.read().trim().to_string();
                                if !b.is_empty() {
                                    props.on_promote_to_task.call(b);
                                }
                            },
                            "Promote to task"
                        }
                    }
                    Button {
                        size: ButtonSize::Small,
                        on_click: send,
                        Send { size: 14 }
                        " Send"
                    }
                }
            }
        }
    }
}

/// Returns up to 5 mention pool entries that start with the token after
/// the last `@` in `body`. Empty if there's no in-progress `@token` or
/// no matches.
fn mention_suggestions(body: &str, pool: &[String]) -> Vec<String> {
    let Some(at_idx) = body.rfind('@') else {
        return vec![];
    };
    let tail = &body[at_idx + 1..];
    if tail.is_empty() || tail.contains(char::is_whitespace) {
        return vec![];
    }
    let token = tail.to_lowercase();
    pool.iter()
        .filter(|p| p.to_lowercase().starts_with(&token))
        .take(5)
        .cloned()
        .collect()
}

/// Returns the subset of pool whose `@<name>` literal appears in `body`.
fn extract_mentions(body: &str, pool: &[String]) -> Vec<String> {
    pool.iter()
        .filter(|name| body.contains(&format!("@{name}")))
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mention_suggestions_picks_prefix_matches() {
        let pool = vec![
            "alice".to_string(),
            "alan".to_string(),
            "bob".to_string(),
        ];
        let s = mention_suggestions("Hello @al", &pool);
        assert_eq!(s, vec!["alice", "alan"]);
    }

    #[test]
    fn mention_suggestions_empty_when_no_token() {
        let pool = vec!["alice".to_string()];
        assert!(mention_suggestions("Hello world", &pool).is_empty());
        assert!(mention_suggestions("Hello @ ", &pool).is_empty());
    }

    #[test]
    fn extract_mentions_finds_referenced_users() {
        let pool = vec!["alice".to_string(), "bob".to_string()];
        let m = extract_mentions("ping @alice and ignore @nonpool", &pool);
        assert_eq!(m, vec!["alice"]);
    }
}
