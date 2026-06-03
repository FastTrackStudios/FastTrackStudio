//! `/repos` — the forge view.
//!
//! Lists the repos the selected org's forge backend can address
//! (`RepoCatalog::list_repos`) and, under each, that repo's issues
//! (`IssueTracker::list_issues`). It's the read slice over the git
//! feature's two core services — the same surface the CLI's
//! `task issue pull/sync` drives, rendered in the app.
//!
//! When the org's forge is unconfigured (no `TASK_FORGEJO_TOKEN`)
//! the backend returns an auth/forge error; the page tolerates that
//! and shows an empty state rather than blanking.
//!
//! The proto DTOs (`git_proto::Repo` / `issues::Issue`) don't derive
//! `PartialEq`, so child components take the primitive fields they
//! render rather than the whole struct (Dioxus props must be
//! `PartialEq`). `RepoId` itself does derive `PartialEq`/`Hash`, so
//! it's fine to thread through for the per-repo issue fetch.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use git_proto::{Repo, RepoId};

use crate::orgs::{OrgMeta, OrgSelection};

/// Map a forge issue state onto a status-badge variant.
fn issue_variant(state: git_proto::IssueState) -> StatusBadgeVariant {
    match state {
        git_proto::IssueState::Open => StatusBadgeVariant::Success,
        git_proto::IssueState::Closed => StatusBadgeVariant::Neutral,
    }
}

#[component]
pub fn ReposView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org whose forge we list (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let repos = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_repos(&s).await,
            None => Ok(Vec::new()),
        }
    });

    let (rows, load_err): (Vec<Repo>, Option<String>) = match &*repos.read() {
        Some(Ok(all)) => (all.clone(), None),
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Repos" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{rows.len()} repos" }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Forge repositories this org can address, with their issues.",
            }

            if let Some(err) = load_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load repos: {err}"
                }
            }

            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text {
                        variant: TextVariant::Muted,
                        "No repos — connect a forge token (TASK_FORGEJO_TOKEN) to populate this view.",
                    }
                }
            } else {
                div { class: "flex flex-col gap-3",
                    for repo in rows {
                        RepoCard {
                            key: "{repo.id.owner}/{repo.id.repo}",
                            slug: slug().unwrap_or_default(),
                            repo_id: repo.id.clone(),
                            owner: repo.id.owner.clone(),
                            name: repo.id.repo.clone(),
                            description: repo.description.clone().unwrap_or_default(),
                            private: repo.private,
                            archived: repo.archived,
                        }
                    }
                }
            }
        }
    }
}

/// One repo: its slug + description, with its issues listed below.
/// Takes scalar fields (not the `Repo` struct) so the props satisfy
/// Dioxus's `PartialEq` bound; `RepoId` derives it and drives the
/// per-repo issue fetch.
#[component]
fn RepoCard(
    slug: String,
    repo_id: RepoId,
    owner: String,
    name: String,
    description: String,
    private: bool,
    archived: bool,
) -> Element {
    let full_name = format!("{owner}/{name}");

    // Fetch this repo's issues. Keyed on the repo id so each card
    // loads independently. Returns `(number, title, state)` tuples —
    // all `PartialEq`-clean — so the rendered rows don't need the
    // non-`PartialEq` `Issue` struct.
    let issues = use_resource(move || {
        let s = slug.clone();
        let rid = repo_id.clone();
        async move {
            if s.is_empty() {
                Ok(Vec::new())
            } else {
                crate::feeds::fetch_issues(&s, rid).await.map(|issues| {
                    issues
                        .into_iter()
                        .map(|i| (i.id.0, i.title, i.state))
                        .collect::<Vec<(u64, String, git_proto::IssueState)>>()
                })
            }
        }
    });

    let (issue_rows, issue_err): (Vec<(u64, String, git_proto::IssueState)>, Option<String>) =
        match &*issues.read() {
            Some(Ok(all)) => (all.clone(), None),
            Some(Err(e)) => (Vec::new(), Some(e.clone())),
            None => (Vec::new(), None),
        };
    let loading = issues.read().is_none();

    rsx! {
        div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-3",
            div { class: "flex flex-wrap items-center gap-2",
                Text { class: "text-sm font-medium", "{full_name}" }
                if private {
                    span { class: "rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "private" }
                }
                if archived {
                    span { class: "rounded bg-muted px-1.5 py-px text-[11px] text-muted-foreground", "archived" }
                }
            }
            if !description.is_empty() {
                Text { variant: TextVariant::Muted, class: "text-xs", "{description}" }
            }

            // ── Issues ─────────────────────────────────────────────
            if let Some(err) = issue_err {
                Text { variant: TextVariant::Muted, class: "text-xs", "issues: {err}" }
            } else if loading {
                Text { variant: TextVariant::Muted, class: "text-xs", "Loading issues…" }
            } else if issue_rows.is_empty() {
                Text { variant: TextVariant::Muted, class: "text-xs", "No issues." }
            } else {
                div { class: "mt-1 flex flex-col gap-1",
                    for (number , title , state) in issue_rows {
                        div {
                            key: "{number}",
                            class: "flex items-center gap-2 rounded-lg border border-border bg-background/40 px-2 py-1",
                            Text { variant: TextVariant::Muted, class: "shrink-0 text-[11px] font-mono", "#{number}" }
                            Text { class: "min-w-0 flex-1 truncate text-xs", "{title}" }
                            StatusBadge {
                                variant: issue_variant(state),
                                label: if state == git_proto::IssueState::Open { "open".to_string() } else { "closed".to_string() },
                            }
                        }
                    }
                }
            }
        }
    }
}
