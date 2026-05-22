//! `IssueTracker` impl. `list_issues` + `get_issue` are wired
//! end-to-end; the rest are `todo!()` stubs.

use crate::{Backend, map_err};
use git_proto::issues::{Comment, Issue, IssueFilter, IssueTracker, IssueUpdate};
use git_proto::{GitError, GitEvent, IssueId, IssueState, Label, RepoId, User};
use vox::Tx;

impl IssueTracker for Backend {
    fn list_issues(&self, repo: &RepoId, filter: IssueFilter) -> Result<Vec<Issue>, GitError> {
        Backend::check_forge(repo)?;
        let backend = self.clone();
        let repo = repo.clone();
        self.runtime().block_on(async move {
            let handler = backend.octo().issues(&repo.owner, &repo.repo);
            let mut req = handler.list();
            if let Some(state) = filter.state {
                req = req.state(match state {
                    IssueState::Open => octocrab::params::State::Open,
                    IssueState::Closed => octocrab::params::State::Closed,
                });
            }
            if !filter.labels.is_empty() {
                req = req.labels(&filter.labels);
            }
            if let Some(ref assignee) = filter.assignee {
                req = req.assignee(assignee.as_str());
            }
            if let Some(ref creator) = filter.author {
                req = req.creator(creator.as_str());
            }
            if let Some(milestone) = filter.milestone {
                req = req.milestone(milestone);
            }
            let page = req.send().await.map_err(map_err)?;
            Ok(page
                .items
                .into_iter()
                .map(|i| translate_issue(&repo, i))
                .collect())
        })
    }

    fn get_issue(&self, repo: &RepoId, issue: IssueId) -> Result<Issue, GitError> {
        Backend::check_forge(repo)?;
        let backend = self.clone();
        let repo = repo.clone();
        self.runtime().block_on(async move {
            let raw = backend
                .octo()
                .issues(&repo.owner, &repo.repo)
                .get(issue.0)
                .await
                .map_err(map_err)?;
            Ok(translate_issue(&repo, raw))
        })
    }

    fn create_issue(
        &self,
        repo: &RepoId,
        _title: String,
        _body: String,
    ) -> Result<Issue, GitError> {
        Backend::check_forge(repo)?;
        todo!("create_issue: octo.issues(o, r).create(title).body(body).send()")
    }

    fn update_issue(
        &self,
        repo: &RepoId,
        _issue: IssueId,
        _update: IssueUpdate,
    ) -> Result<Issue, GitError> {
        Backend::check_forge(repo)?;
        todo!(
            "update_issue: octo.issues(o, r).update(n).{{title,body,state,labels,assignees,milestone}}()"
        )
    }

    fn list_comments(&self, repo: &RepoId, _issue: IssueId) -> Result<Vec<Comment>, GitError> {
        Backend::check_forge(repo)?;
        todo!("list_comments: octo.issues(o, r).list_comments(n).send()")
    }

    fn add_comment(
        &self,
        repo: &RepoId,
        _issue: IssueId,
        _body: String,
    ) -> Result<Comment, GitError> {
        Backend::check_forge(repo)?;
        todo!("add_comment: octo.issues(o, r).create_comment(n, body)")
    }

    async fn subscribe(&self, _repo: RepoId, _tx: Tx<GitEvent>) {
        // First pass: no event source. Phase 2 polls via
        // octocrab.repos().events() or attaches a webhook
        // receiver. The trait permits dropping `tx` to signal
        // shutdown; doing nothing here keeps it open until the
        // caller drops.
    }
}

/// Translate octocrab's issue model into the proto shape.
fn translate_issue(repo: &RepoId, raw: octocrab::models::issues::Issue) -> Issue {
    Issue {
        id: IssueId(raw.number),
        repo: repo.clone(),
        title: raw.title,
        body: raw.body.unwrap_or_default(),
        state: match raw.state {
            octocrab::models::IssueState::Open => IssueState::Open,
            octocrab::models::IssueState::Closed => IssueState::Closed,
            _ => IssueState::Closed,
        },
        author: User {
            login: raw.user.login.clone(),
            display_name: raw.user.name.clone(),
        },
        labels: raw
            .labels
            .into_iter()
            .map(|l| Label {
                name: l.name,
                color: Some(l.color),
            })
            .collect(),
        assignees: raw
            .assignees
            .into_iter()
            .map(|a| User {
                login: a.login,
                display_name: a.name,
            })
            .collect(),
        milestone: raw.milestone.map(|m| git_proto::Milestone {
            title: m.title,
            number: m.number as u64,
        }),
    }
}
