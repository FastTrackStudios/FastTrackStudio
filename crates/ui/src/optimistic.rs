//! `OptimisticList<T, K>` — the canonical optimistic, write-through list
//! primitive shared by the `/…` route pages.
//!
//! The route holds the authoritative `Vec<T>` in this primitive's `rows`
//! signal. A mutation lands in the list **first** (instant UI), then a
//! best-effort write-through (`feeds::create_x(..)` etc.) is fired on a
//! background task. Because our CRUD services are request/response — the
//! create future *returns the persisted entity* — reconciliation is keyed
//! by the provisional row's id (held in the spawn's own closure), so we
//! just replace that row in place when the canonical entity comes back.
//! On failure the row stays visible and `failed`; the next load reconciles.
//!
//! Contrast the refresh-counter pages this replaces (`refresh += 1` →
//! `use_resource` refetch), which block the UI on two round-trips per
//! create. See `plans/optimistic-ui-pattern.md`.
//!
//! The id key type `K` is generic: most vault entities key on
//! `uuid::Uuid`, but some (inbox items, recipes, scheduling) key on a
//! `String`. `K` is inferred from the `id_of` accessor, so call sites
//! rarely name it: `use_optimistic_list::<Location>(|l| l.id)` infers
//! `K = Uuid`; `use_optimistic_list::<InboxItem>(|i| i.id.clone())`
//! infers `K = String`.
//!
//! **Native** (`not(target_arch = "wasm32")`) has no vox client: the
//! optimistic mutation still applies to the in-memory list, but the write
//! future is dropped and `pending`/`failed` stay empty — no false-pending
//! rows on desktop. Mirrors `task_wiring`'s native no-op.

use std::collections::HashSet;
use std::future::Future;
use std::hash::Hash;

use dioxus::prelude::*;

/// Per-row write-through state, surfaced to the view for styling. A page
/// may ignore this and still get instant inserts; honoring it adds the
/// pending / failed affordance.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RowState {
    /// Persisted (or freshly loaded from the server).
    Settled,
    /// Optimistically applied, write-through in flight.
    Pending,
    /// Write-through errored; the row is kept and flagged.
    Failed,
}

/// An optimistic, write-through collection bound to a vox service.
///
/// `Copy` (every field is a `Signal` handle or a fn pointer), so it
/// threads into event-handler closures like a bare `Signal`. Construct
/// with [`use_optimistic_list`]. `K` is the id key type (e.g. `Uuid` or
/// `String`), inferred from the `id_of` accessor.
pub struct OptimisticList<T: Clone + 'static, K: Eq + Hash + Clone + 'static> {
    rows: Signal<Vec<T>>,
    pending: Signal<HashSet<K>>,
    failed: Signal<HashSet<K>>,
    /// Stable id accessor — a non-capturing closure coerces to this.
    id_of: fn(&T) -> K,
}

// Manual `Copy`/`Clone` — every field is a `Signal` handle (Copy
// regardless of `T`/`K`) or a fn pointer, so the list is Copy even when
// `T` is not. The derive would wrongly demand `T: Copy`.
#[allow(clippy::expl_impl_clone_on_copy)] // manual to drop the derive's `T: Copy` bound
impl<T: Clone + 'static, K: Eq + Hash + Clone + 'static> Clone for OptimisticList<T, K> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: Clone + 'static, K: Eq + Hash + Clone + 'static> Copy for OptimisticList<T, K> {}

// Manual `PartialEq` so the list can be a Dioxus component prop (the
// `#[component]` macro derives `PartialEq` on the props struct). Compares
// the `Signal` handles + the id accessor — never the row values, so no
// `T: PartialEq` bound. Reactivity still flows through signal reads, not
// prop equality, so handle identity is the correct prop comparison.
impl<T: Clone + 'static, K: Eq + Hash + Clone + 'static> PartialEq for OptimisticList<T, K> {
    fn eq(&self, other: &Self) -> bool {
        self.rows == other.rows
            && self.pending == other.pending
            && self.failed == other.failed
            && std::ptr::fn_addr_eq(self.id_of, other.id_of)
    }
}

/// Hook: a fresh [`OptimisticList`] backed by component-local signals.
/// `id_of` is a non-capturing closure, e.g. `|l: &Location| l.id` or
/// `|i: &InboxItem| i.id.clone()`.
pub fn use_optimistic_list<T: Clone + 'static, K: Eq + Hash + Clone + 'static>(
    id_of: fn(&T) -> K,
) -> OptimisticList<T, K> {
    OptimisticList {
        rows: use_signal(Vec::new),
        pending: use_signal(HashSet::new),
        failed: use_signal(HashSet::new),
        id_of,
    }
}

impl<T: Clone + 'static, K: Eq + Hash + Clone + 'static> OptimisticList<T, K> {
    /// The backing rows signal, for rendering: `list.items().read().iter()`.
    pub fn items(&self) -> Signal<Vec<T>> {
        self.rows
    }

    /// Per-row state for styling. `Failed` wins over `Pending`.
    pub fn state(&self, id: K) -> RowState {
        if self.failed.read().contains(&id) {
            RowState::Failed
        } else if self.pending.read().contains(&id) {
            RowState::Pending
        } else {
            RowState::Settled
        }
    }

    /// Replace the whole list from a server snapshot (initial load /
    /// org-switch refetch). Server truth wins — clears pending + failed.
    ///
    /// Takes `&self` (the `Signal` fields are `Copy` handles with interior
    /// mutability) so the list needs no `mut` binding and can be shared
    /// across event-handler closures / drilled as a prop.
    pub fn set(&self, items: Vec<T>) {
        let (mut rows, mut pending, mut failed) = (self.rows, self.pending, self.failed);
        rows.set(items);
        pending.write().clear();
        failed.write().clear();
    }

    /// Optimistically append `provisional` (caller mints its id), then
    /// write through. On success the provisional row is replaced in place
    /// by the canonical entity the future returns (id swap is free — whole
    /// row is replaced). On failure the row stays, flagged `Failed`.
    pub fn create<F>(&self, provisional: T, write: F)
    where
        F: Future<Output = Result<T, String>> + 'static,
    {
        let id = (self.id_of)(&provisional);
        let mut rows = self.rows;
        rows.write().push(provisional);
        let id_of = self.id_of;
        #[cfg(target_arch = "wasm32")]
        {
            let (mut pending, mut failed) = (self.pending, self.failed);
            pending.write().insert(id.clone());
            failed.write().remove(&id);
            spawn(async move {
                match write.await {
                    Ok(real) => {
                        rows.write().iter_mut().for_each(|r| {
                            if id_of(r) == id {
                                *r = real.clone();
                            }
                        });
                        pending.write().remove(&id);
                    }
                    Err(e) => {
                        tracing::warn!("optimistic create write-through failed: {e}");
                        pending.write().remove(&id);
                        failed.write().insert(id);
                    }
                }
            });
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = (id, id_of, &write); // native: no client; nothing spawned
        }
    }

    /// Optimistically replace the row whose id matches `next`, then write
    /// through. On failure the previous row is restored.
    pub fn update<F>(&self, next: T, write: F)
    where
        F: Future<Output = Result<T, String>> + 'static,
    {
        let id_of = self.id_of;
        let id = id_of(&next);
        let mut rows = self.rows;
        let prev = rows
            .write()
            .iter_mut()
            .find(|r| id_of(r) == id)
            .map(|slot| std::mem::replace(slot, next));
        #[cfg(target_arch = "wasm32")]
        {
            let (mut pending, mut failed) = (self.pending, self.failed);
            pending.write().insert(id.clone());
            failed.write().remove(&id);
            spawn(async move {
                match write.await {
                    Ok(real) => {
                        rows.write().iter_mut().for_each(|r| {
                            if id_of(r) == id {
                                *r = real.clone();
                            }
                        });
                        pending.write().remove(&id);
                    }
                    Err(e) => {
                        tracing::warn!("optimistic update write-through failed: {e}");
                        if let Some(prev) = prev {
                            rows.write().iter_mut().for_each(|r| {
                                if id_of(r) == id {
                                    *r = prev.clone();
                                }
                            });
                        }
                        pending.write().remove(&id);
                        failed.write().insert(id);
                    }
                }
            });
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = (&write, prev); // native: no client; nothing spawned
        }
    }

    /// Optimistically remove the row, then write through. On failure the
    /// row is re-inserted at its original index (clamped to current len).
    pub fn delete<F>(&self, id: K, write: F)
    where
        F: Future<Output = Result<(), String>> + 'static,
    {
        let id_of = self.id_of;
        let mut rows_sig = self.rows;
        let removed = {
            let mut rows = rows_sig.write();
            rows.iter()
                .position(|r| id_of(r) == id)
                .map(|i| (i, rows.remove(i)))
        };
        #[cfg(target_arch = "wasm32")]
        {
            let (mut rows, mut failed) = (self.rows, self.failed);
            spawn(async move {
                if let Err(e) = write.await {
                    tracing::warn!("optimistic delete write-through failed: {e}");
                    if let Some((i, row)) = removed {
                        let mut rows = rows.write();
                        let i = i.min(rows.len());
                        rows.insert(i, row);
                        drop(rows);
                        failed.write().insert(id);
                    }
                }
            });
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = (&write, removed); // native: no client; nothing spawned
        }
    }
}
