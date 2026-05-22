//! `ReviewSurface` impl. All `todo!()` stubs in the first pass
//! — `IssueTracker` lands first; PR methods follow once the
//! binding layer needs them.

use crate::Backend;
use git_proto::reviews::{MergeMethod, NewPullRequest, PullRequestUpdate, Review, ReviewSurface};
use git_proto::{GitError, GitEvent, PullRequest, PullRequestId, RepoId, Reviewer};
use vox::Tx;

impl ReviewSurface for Backend {
    fn list_pull_requests(&self, repo: &RepoId) -> Result<Vec<PullRequest>, GitError> {
        Backend::check_forge(repo)?;
        todo!("list_pull_requests: octo.pulls(o, r).list().send()")
    }

    fn get_pull_request(&self, repo: &RepoId, _pr: PullRequestId) -> Result<PullRequest, GitError> {
        Backend::check_forge(repo)?;
        todo!("get_pull_request: octo.pulls(o, r).get(n)")
    }

    fn create_pull_request(
        &self,
        repo: &RepoId,
        _new: NewPullRequest,
    ) -> Result<PullRequest, GitError> {
        Backend::check_forge(repo)?;
        todo!(
            "create_pull_request: octo.pulls(o, r).create(title, head, base).body(body).draft(draft).send()"
        )
    }

    fn update_pull_request(
        &self,
        repo: &RepoId,
        _pr: PullRequestId,
        _update: PullRequestUpdate,
    ) -> Result<PullRequest, GitError> {
        Backend::check_forge(repo)?;
        todo!("update_pull_request: octo.pulls(o, r).update(n).{{title,body,state}}()")
    }

    fn list_reviews(&self, repo: &RepoId, _pr: PullRequestId) -> Result<Vec<Review>, GitError> {
        Backend::check_forge(repo)?;
        todo!("list_reviews: octo.pulls(o, r).list_reviews(n).send()")
    }

    fn request_reviewers(
        &self,
        repo: &RepoId,
        _pr: PullRequestId,
        _reviewers: Vec<Reviewer>,
    ) -> Result<(), GitError> {
        Backend::check_forge(repo)?;
        todo!("request_reviewers: POST /repos/:o/:r/pulls/:n/requested_reviewers")
    }

    fn merge_pull_request(
        &self,
        repo: &RepoId,
        _pr: PullRequestId,
        _method: MergeMethod,
    ) -> Result<Option<String>, GitError> {
        Backend::check_forge(repo)?;
        todo!("merge_pull_request: octo.pulls(o, r).merge(n).method(method).send()")
    }

    async fn subscribe(&self, _repo: RepoId, _tx: Tx<GitEvent>) {
        // First pass: no event source. See issues::subscribe.
    }
}
