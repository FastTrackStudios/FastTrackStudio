//! `WikiService` — the canonical sync trait, decorated with
//! `#[architect::rpc]`.
//!
//! Single end-to-end spec: bootstrap → ingest → graph → lint
//! → review → research → federation. Backends impl this
//! directly (zero-cost in-process call sites); remote callers
//! reach the same surface via the auto-emitted
//! [`WikiServiceClient`] over vox. See `vault-proto::VaultSync`
//! for the precedent and `architect/DESIGN.md` for the macro
//! mechanics.
//!
//! `wiki_id` identifies which wiki on a multi-tenant server.
//! For single-vault deployments it's just `"default"` or the
//! vault name.

use crate::{
    error::WikiError,
    event::WikiEvent,
    federation::{CrossWikiPageRef, PeerPullResult, PeerWiki},
    graph::{Cluster, GraphOpts, KnowledgeGap, RelevanceScore, WikiGraph},
    ingest::{AnalysisDraft, IngestTask, PageDraft, SourceChange},
    lint::{FindingAction, LintFinding, LintScope},
    log::{LogEntry, WikiIndex},
    research::{RawSource, ResearchPlan, ResearchSourceKind, ResearchStatus},
    review::{ReviewAction, ReviewItem},
    schema::{PurposeDoc, SchemaDoc},
};
use chrono::{DateTime, Utc};
use vox::Tx;

#[architect::rpc]
pub trait WikiService {
    // ───────────────────────── Bootstrap / schema ─────────────────────────

    /// Initialize `Wiki/` in the named vault if it doesn't
    /// already exist. Writes `schema.md` + `purpose.md` from
    /// [`crate::schema::default_schema_doc`] +
    /// [`crate::schema::default_purpose_doc`], creates empty
    /// `index.md` + `log.md`, scaffolds `sources/`, `media/`,
    /// and `_state/`. Idempotent: returns
    /// [`WikiError::IllegalState`] only if something exists
    /// but is malformed.
    fn bootstrap(&self, wiki_id: &str) -> Result<(), WikiError>;

    /// Read the current `schema.md` content.
    fn read_schema(&self, wiki_id: &str) -> Result<SchemaDoc, WikiError>;

    /// Read the current `purpose.md` content.
    fn read_purpose(&self, wiki_id: &str) -> Result<PurposeDoc, WikiError>;

    /// Overwrite `schema.md`. Curator-only; the LLM agent
    /// never calls this.
    fn write_schema(&self, wiki_id: &str, markdown: &str) -> Result<(), WikiError>;

    /// Overwrite `purpose.md`. Curator-only.
    fn write_purpose(&self, wiki_id: &str, markdown: &str) -> Result<(), WikiError>;

    // ───────────────────────── Index + log ─────────────────────────

    /// Read the parsed `index.md`. Backends regenerate from
    /// disk on each call (cheap — the catalog is small).
    fn read_index(&self, wiki_id: &str) -> Result<WikiIndex, WikiError>;

    /// Rebuild `index.md` from the current wiki pages.
    /// Idempotent.
    fn rebuild_index(&self, wiki_id: &str) -> Result<WikiIndex, WikiError>;

    /// Append one entry to `log.md`. Backends format the
    /// header (`## [YYYY-MM-DD] <op> | <title>`) so callers
    /// only supply the body fields.
    fn append_log(&self, wiki_id: &str, entry: LogEntry) -> Result<(), WikiError>;

    // ───────────────────────── Graph ─────────────────────────

    /// Build the 4-signal weighted graph over the wiki.
    fn build_graph(&self, wiki_id: &str, opts: GraphOpts) -> Result<WikiGraph, WikiError>;

    /// Compute the relevance score between two pages — useful
    /// for "why are these connected?" tooltips.
    fn relevance(&self, wiki_id: &str, from: &str, to: &str) -> Result<RelevanceScore, WikiError>;

    /// Run Louvain community detection. Backends cache the
    /// result and invalidate on page-write events.
    fn clusters(&self, wiki_id: &str) -> Result<Vec<Cluster>, WikiError>;

    /// Surface knowledge gaps (orphans, sparse clusters,
    /// bridge nodes, missing pages). Each gap can be promoted
    /// to a research plan via [`Self::propose_research`].
    fn gaps(&self, wiki_id: &str) -> Result<Vec<KnowledgeGap>, WikiError>;

    // ───────────────────────── Ingest pipeline ─────────────────────────

    /// Register a raw source for ingestion. Returns the
    /// freshly-created task in `Pending` state.
    fn enqueue_ingest(
        &self,
        wiki_id: &str,
        source_path: &str,
        change: SourceChange,
    ) -> Result<IngestTask, WikiError>;

    /// List the queue (any status).
    fn list_ingest(&self, wiki_id: &str) -> Result<Vec<IngestTask>, WikiError>;

    /// Pop the next `Pending` task and transition it to
    /// `Analyzing`. Agents call this to claim work. Returns
    /// `None` if the queue is empty.
    fn claim_next_ingest(&self, wiki_id: &str) -> Result<Option<IngestTask>, WikiError>;

    /// Record the step-1 analysis output. Transitions the
    /// task from `Analyzing` → `Generating`.
    fn record_analysis(
        &self,
        wiki_id: &str,
        task_id: &str,
        analysis: AnalysisDraft,
    ) -> Result<(), WikiError>;

    /// Record the step-2 page drafts. Backend atomically
    /// writes them to disk, updates `index.md`, appends to
    /// `log.md`. Transitions `Generating` → `Writing` →
    /// `Done`.
    fn record_pages(
        &self,
        wiki_id: &str,
        task_id: &str,
        pages: Vec<PageDraft>,
    ) -> Result<(), WikiError>;

    /// Mark a task failed with a recorded error. Backends
    /// auto-retry up to `max_retries`; this is the manual
    /// equivalent.
    fn fail_ingest(&self, wiki_id: &str, task_id: &str, error: &str) -> Result<(), WikiError>;

    /// Curator-cancelled a task.
    fn cancel_ingest(&self, wiki_id: &str, task_id: &str) -> Result<(), WikiError>;

    // ───────────────────────── Lint ─────────────────────────

    /// Run the named lint scope and persist any findings.
    /// Returns the findings newly raised on this pass.
    fn lint(&self, wiki_id: &str, scope: LintScope) -> Result<Vec<LintFinding>, WikiError>;

    /// List all open findings.
    fn list_findings(&self, wiki_id: &str) -> Result<Vec<LintFinding>, WikiError>;

    /// Apply an action to a finding.
    fn resolve_finding(
        &self,
        wiki_id: &str,
        finding_id: &str,
        action: FindingAction,
    ) -> Result<(), WikiError>;

    // ───────────────────────── Review queue ─────────────────────────

    /// Enqueue a review item. Agents call this when they hit
    /// something that needs curator judgment.
    fn enqueue_review(&self, wiki_id: &str, item: ReviewItem) -> Result<(), WikiError>;

    /// List open review items.
    fn list_review(&self, wiki_id: &str) -> Result<Vec<ReviewItem>, WikiError>;

    /// Apply a curator's choice. Backends fan out into the
    /// other surfaces — e.g. `Research` action → spawn a
    /// research plan.
    fn apply_review(
        &self,
        wiki_id: &str,
        item_id: &str,
        action: ReviewAction,
    ) -> Result<(), WikiError>;

    // ───────────────────────── Deep research ─────────────────────────

    /// Propose a research plan for the given source. Agent
    /// fills in the queries; backend persists in `Proposed`.
    fn propose_research(
        &self,
        wiki_id: &str,
        source_kind: ResearchSourceKind,
        source_id: &str,
    ) -> Result<ResearchPlan, WikiError>;

    /// List research plans (any status).
    fn list_research(&self, wiki_id: &str) -> Result<Vec<ResearchPlan>, WikiError>;

    /// Transition a plan's status. Agents drive
    /// `Proposed` → `Running` → `Awaiting` themselves.
    fn set_research_status(
        &self,
        wiki_id: &str,
        plan_id: &str,
        status: ResearchStatus,
    ) -> Result<(), WikiError>;

    /// Submit research results. Backend persists each
    /// `RawSource` under `Wiki/sources/` and spawns a
    /// corresponding `IngestTask`. Returns the new tasks.
    fn submit_research_result(
        &self,
        wiki_id: &str,
        plan_id: &str,
        sources: Vec<RawSource>,
    ) -> Result<Vec<IngestTask>, WikiError>;

    // ───────────────────────── Federation ─────────────────────────

    /// Register a peer wiki.
    fn add_peer(&self, wiki_id: &str, peer: PeerWiki) -> Result<(), WikiError>;

    /// Remove a peer. Pulled mirror files under
    /// `Wiki/sources/<peer-id>/` are left in place by default
    /// (becomes orphan data the next lint pass will surface).
    fn remove_peer(&self, wiki_id: &str, peer_id: &str) -> Result<(), WikiError>;

    /// List registered peers.
    fn list_peers(&self, wiki_id: &str) -> Result<Vec<PeerWiki>, WikiError>;

    /// Pull changes from a peer since the given timestamp.
    /// `None` ⇒ since the peer's `last_pulled_at`.
    fn pull_from_peer(
        &self,
        wiki_id: &str,
        peer_id: &str,
        since: Option<DateTime<Utc>>,
    ) -> Result<PeerPullResult, WikiError>;

    /// Resolve a federated wikilink (`[[peer-id:Page title]]`).
    /// Returns `None` if no peer matches or the peer doesn't
    /// have that page.
    fn resolve_cross_wiki_link(
        &self,
        wiki_id: &str,
        link: &str,
    ) -> Result<Option<CrossWikiPageRef>, WikiError>;

    // ───────────────────────── Live events ─────────────────────────

    /// Subscribe to live change events. The server keeps
    /// sending until the caller drops `tx`. On broadcast lag
    /// the server sends [`WikiEvent::Resync`] and continues —
    /// clients re-pull state in response.
    async fn subscribe(&self, wiki_id: String, tx: Tx<WikiEvent>);
}
