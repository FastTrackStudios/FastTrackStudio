//! `TaskGitPanel` — Git tab inside `TaskDetailSheet`.
//!
//! Owned by `project-ui` because the data lives on `Task` itself
//! (branch_name, pr_urls, commit_refs). Dispatch & cancel — which need
//! the agent integration plugin — live in `agent-ui::hermes_kit`.

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Copy as CopyIcon, ExternalLink, GitBranch, GitCommitVertical as GitCommit, GitPullRequest,
    RefreshCw,
};
use fts_ui::prelude::*;
use project_proto::Task;

#[component]
pub fn TaskGitPanel(
    task: Task,
    on_set_branch_name: EventHandler<String>,
    on_clear_branch_name: EventHandler<()>,
) -> Element {
    let branch_name = task.branch_name.clone();
    let pr_urls = task.pr_urls.clone();
    let mut commit_refs = task.commit_refs.clone();
    commit_refs.sort_by_key(|c| std::cmp::Reverse(c.authored_at));
    let total_commits = commit_refs.len();
    let take = total_commits.min(10);
    let extra = total_commits.saturating_sub(take);
    let commits_to_show: Vec<_> = commit_refs.into_iter().take(take).collect();

    // Generate a fresh slug for the regenerate button. We use a
    // placeholder handle/project_key here because v1 doesn't yet
    // propagate org handle through the sheet; the route layer will
    // do the actual generation via `branch_slug` and write through
    // `on_set_branch_name`. We pass an empty string to signal
    // "regenerate, parent decides the slug".
    let mut confirm_regen = use_signal(|| false);
    let now = Utc::now();

    rsx! {
        VStack { class: "gap-4 py-2",
            // ── Branch ─────────────────────────────────────────────────
            VStack { class: "gap-2",
                HStack { class: "items-center gap-2",
                    GitBranch { size: 14 }
                    span { class: "text-xs font-semibold uppercase tracking-widest text-foreground", "Branch" }
                }
                if let Some(branch) = branch_name.clone() {
                    HStack { class: "items-center gap-2 flex-wrap",
                        Kbd { "{branch}" }
                        {
                            let b_for_copy = branch.clone();
                            let b_for_checkout = branch.clone();
                            let b_for_pr = branch.clone();
                            rsx! {
                                button {
                                    r#type: "button",
                                    class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                    title: "Copy branch name",
                                    onclick: move |_| copy_to_clipboard(&b_for_copy),
                                    CopyIcon { size: 12 }
                                }
                                button {
                                    r#type: "button",
                                    class: "inline-flex items-center gap-1 rounded border border-border px-2 py-0.5 text-[10px] text-muted-foreground hover:text-foreground",
                                    title: "Copy git checkout command",
                                    onclick: move |_| {
                                        copy_to_clipboard(&format!("git checkout -b {b_for_checkout}"));
                                    },
                                    CopyIcon { size: 10 }
                                    "checkout"
                                }
                                button {
                                    r#type: "button",
                                    class: "inline-flex items-center gap-1 rounded border border-border px-2 py-0.5 text-[10px] text-muted-foreground hover:text-foreground",
                                    title: "Copy gh pr create command",
                                    onclick: move |_| {
                                        copy_to_clipboard(&format!("gh pr create --head {b_for_pr}"));
                                    },
                                    CopyIcon { size: 10 }
                                    "gh pr create"
                                }
                                button {
                                    r#type: "button",
                                    class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                    title: "Regenerate branch name",
                                    onclick: move |_| confirm_regen.set(true),
                                    RefreshCw { size: 12 }
                                }
                            }
                        }
                    }
                } else {
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            // Empty string signals "parent: please derive a slug
                            // from the task + handle". Parent route is the only
                            // place with access to the org handle / project key.
                            on_set_branch_name.call(String::new());
                        },
                        "+ Generate branch name"
                    }
                }
            }

            Divider {}

            // ── Pull requests ──────────────────────────────────────────
            VStack { class: "gap-2",
                HStack { class: "items-center gap-2",
                    GitPullRequest { size: 14 }
                    span { class: "text-xs font-semibold uppercase tracking-widest text-foreground", "Pull requests" }
                }
                if pr_urls.is_empty() {
                    Text { variant: TextVariant::Muted, "No PRs from this task yet." }
                } else {
                    div { class: "flex flex-col gap-1",
                        for url in pr_urls.into_iter() {
                            {
                                let u_for_copy = url.clone();
                                let u_for_open = url.clone();
                                let label = url.clone();
                                rsx! {
                                    Item { key: "{url}",
                                        ItemContent {
                                            ItemTitle {
                                                span { class: "font-mono text-[11px] truncate", "{label}" }
                                            }
                                        }
                                        ItemActions { class: "gap-1",
                                            StatusBadge {
                                                variant: StatusBadgeVariant::Neutral,
                                                label: "open",
                                            }
                                            button {
                                                r#type: "button",
                                                class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                                title: "Copy URL",
                                                onclick: move |_| copy_to_clipboard(&u_for_copy),
                                                CopyIcon { size: 12 }
                                            }
                                            a {
                                                href: "{u_for_open}",
                                                target: "_blank",
                                                rel: "noreferrer",
                                                class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                                title: "Open",
                                                ExternalLink { size: 12 }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Divider {}

            // ── Commits ────────────────────────────────────────────────
            VStack { class: "gap-2",
                HStack { class: "items-center gap-2",
                    GitCommit { size: 14 }
                    span { class: "text-xs font-semibold uppercase tracking-widest text-foreground", "Commits" }
                }
                if commits_to_show.is_empty() {
                    Text { variant: TextVariant::Muted, "No commits from this task yet." }
                } else {
                    div { class: "flex flex-col gap-1",
                        for c in commits_to_show.into_iter() {
                            {
                                let short = c.sha.chars().take(7).collect::<String>();
                                let rel = super::common::relative_time(c.authored_at, now);
                                let url = c.url.clone();
                                rsx! {
                                    Item { key: "{c.sha}",
                                        ItemContent {
                                            ItemTitle {
                                                HStack { class: "items-center gap-2",
                                                    Kbd { "{short}" }
                                                    span { class: "truncate text-xs", "{c.message}" }
                                                }
                                            }
                                            ItemDescription {
                                                span { class: "text-[10px] text-muted-foreground", "{rel}" }
                                            }
                                        }
                                        if let Some(u) = url {
                                            ItemActions {
                                                a {
                                                    href: "{u}",
                                                    target: "_blank",
                                                    rel: "noreferrer",
                                                    class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                                    ExternalLink { size: 12 }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if extra > 0 {
                        Text { variant: TextVariant::Muted, "+{extra} more" }
                    }
                }
            }

            // Regenerate confirm
            AlertDialog {
                open: Some(*confirm_regen.read()),
                on_open_change: move |o: bool| confirm_regen.set(o),
                AlertDialogHeader {
                    AlertDialogTitle { "Regenerate branch name?" }
                    AlertDialogDescription {
                        "The previously generated branch name will be overwritten. Open PRs pointing at the old branch are not renamed."
                    }
                }
                AlertDialogFooter {
                    AlertDialogCancel {
                        on_click: move |_| confirm_regen.set(false),
                        "Cancel"
                    }
                    AlertDialogAction {
                        on_click: move |_| {
                            confirm_regen.set(false);
                            // Parent re-derives — see comment above.
                            on_clear_branch_name.call(());
                            on_set_branch_name.call(String::new());
                        },
                        "Regenerate"
                    }
                }
            }
        }
    }
}

fn copy_to_clipboard(text: &str) {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(win) = web_sys::window() {
            let _ = win.navigator().clipboard().write_text(text);
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = text;
    }
}
