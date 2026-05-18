//! Index-aware repo wrappers — thin shims over the macro-emitted
//! `PageRepoLoro` that hook the Tier-2 reindex pipeline into
//! create / update / delete.
//!
//! UI / sync code should construct these instead of the raw
//! `PageRepoLoro` so `PagePropEdge` rows trail page writes
//! automatically. Direct `PageRepoLoro` use is still supported
//! (and the canonical store is `Page.frontmatter_json`); this
//! wrapper just keeps the materialized index in sync.

use architect::{Filter, Page as PageWindow, RepoError, Sort};
use crdt::CrdtDoc;
use knowledge_proto::{Page, PageCreate, PageList, PageRepo, PageUpdate};
use uuid::Uuid;

use crate::PageRepoLoro;
use crate::reindex;

/// Page repo that auto-reindexes the `PagePropEdge` table on
/// every write.
#[derive(Clone)]
pub struct IndexedPageRepo {
    inner: PageRepoLoro,
    doc: CrdtDoc,
}

impl IndexedPageRepo {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self {
            inner: PageRepoLoro::new(doc),
            doc: doc.clone(),
        }
    }

    pub fn inner(&self) -> &PageRepoLoro {
        &self.inner
    }
}

impl PageRepo for IndexedPageRepo {
    async fn get(&self, id: Uuid) -> Result<Page, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: PageWindow,
        sort: Option<Sort>,
        filter: Option<Filter>,
    ) -> Result<PageList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: PageCreate) -> Result<Page, RepoError> {
        let page = self.inner.create(input).await?;
        if let Err(e) = reindex::reindex_page(&self.doc, page.id).await {
            tracing::warn!(?e, %page.id, "page-prop reindex failed (post-create)");
        }
        // Tier 3: any previously-unresolved [[NewPage]] refs now
        // get their target_uuid stamped automatically. Aliases
        // also count as resolution targets.
        if let Err(e) =
            reindex::resolve_page_refs(&self.doc, page.id, &page.basename, &page.aliases).await
        {
            tracing::warn!(?e, %page.id, "ref auto-resolve failed (post-create)");
        }
        Ok(page)
    }
    async fn update(&self, id: Uuid, input: PageUpdate) -> Result<Page, RepoError> {
        // Capture the old basename + aliases BEFORE the update so
        // we can run rename / alias cascades on what changed.
        let old = self.inner.get(id).await.ok();
        let page = self.inner.update(id, input).await?;
        if let Err(e) = reindex::reindex_page(&self.doc, page.id).await {
            tracing::warn!(?e, %page.id, "page-prop reindex failed (post-update)");
        }
        if let Some(old) = old {
            if old.basename != page.basename {
                if let Err(e) = reindex::cascade_rename_page_refs(
                    &self.doc,
                    page.id,
                    &old.basename,
                    &page.basename,
                )
                .await
                {
                    tracing::warn!(?e, %page.id, "page-rename cascade failed");
                }
            }
            // Aliases added in this update? Resolve any dangling
            // refs targeting them. (Removed aliases don't break
            // existing edges — those were already resolved to
            // page.id and the basename + remaining aliases still
            // match.)
            if old.aliases != page.aliases {
                if let Err(e) =
                    reindex::resolve_page_refs(&self.doc, page.id, &page.basename, &page.aliases)
                        .await
                {
                    tracing::warn!(?e, %page.id, "alias re-resolve failed");
                }
            }
        }
        Ok(page)
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await?;
        if let Err(e) = reindex::cascade_delete_page_edges(&self.doc, id).await {
            tracing::warn!(?e, %id, "page-prop edge cascade-delete failed");
        }
        // Tier 3: any block-ref edges that pointed at this page
        // become broken links — clear the resolved UUID.
        if let Err(e) = reindex::unresolve_page_refs(&self.doc, id).await {
            tracing::warn!(?e, %id, "ref unresolve failed (post-delete)");
        }
        Ok(())
    }
}
