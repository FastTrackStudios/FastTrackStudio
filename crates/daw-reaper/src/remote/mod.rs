//! `Send + Sync` REAPER implementation of the `daw_proto::sync` traits.
//!
//! [`ReaperRemote`] wraps every trait call in `main_thread::query` and blocks
//! the calling thread on a tokio runtime until the closure has executed on
//! REAPER's main thread. This is the replacement for the old
//! `daw-control-sync` crate (a blocking wrapper over the now-retired async
//! `Daw` API).
//!
//! Each trait method delegates into a fresh [`crate::sync::ReaperMainThread`]
//! constructed inside the dispatched closure — the `!Send` main-thread handle
//! never crosses the closure boundary, so [`ReaperRemote`] itself is `Send +
//! Sync` and safe to share between worker threads / audio callbacks.
//!
//! Sub-handles ([`RemoteProject`], [`RemoteTransport`], etc.) borrow
//! `&'a ReaperRemote` and inherit its `Send + Sync` discipline via the borrow.

pub mod daw;
// ext_state ported to architect::rpc — see `crate::ext_state`.
// fx_chains ported to architect::rpc — see `crate::fx_chains`.
// fx_params ported to architect::rpc — see `crate::fx_params`.
// items ported to architect::rpc — see `crate::item`.
// markers ported to the singleton `Reaper` backend — see
// `crate::marker`. The remote/marshal-to-main-thread wrapper is no
// longer needed; mounting via `marker::serve(Reaper)` handles
// threading through `HasDispatcher`.
pub mod project;
// regions ported to architect::rpc — see `crate::region`.
// routing ported to architect::rpc — see `crate::routing_sync`.
// takes ported to architect::rpc — see `crate::take`.
// tempo_map ported to architect::rpc — see `crate::tempo_map`.
// tracks ported to architect::rpc — `marker::serve(Reaper)` style
// mounting handles threading via `HasDispatcher`.
// transport ported to architect::rpc — see `crate::transport`.

pub use daw::ReaperRemote;
pub use project::RemoteProject;

use daw_proto::{DawError, DawResult};

/// Dispatch a fallible mutation onto REAPER's main thread and block until it
/// returns. The outer `None` from `main_thread::query` (TaskSupport not yet
/// initialized) is flattened into [`DawError::MainThreadUnavailable`].
pub(crate) fn dispatch<F, R>(remote: &ReaperRemote, f: F) -> DawResult<R>
where
    F: FnOnce() -> DawResult<R> + Send + 'static,
    R: Send + 'static,
{
    remote.runtime.block_on(async move {
        crate::main_thread::query(f)
            .await
            .ok_or(DawError::MainThreadUnavailable)?
    })
}

/// Dispatch an infallible read onto REAPER's main thread, falling back to
/// `R::default()` if TaskSupport is unavailable. Matches the
/// "reads silently degrade" convention documented on the sync traits.
pub(crate) fn dispatch_read<F, R>(remote: &ReaperRemote, f: F) -> R
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static + Default,
{
    remote
        .runtime
        .block_on(async move { crate::main_thread::query(f).await.unwrap_or_default() })
}

/// Construct a `ReaperMainThread` inside the dispatched closure or surface
/// `DawError::MainThreadUnavailable` if the high-level REAPER API has not been
/// initialized yet.
pub(crate) fn main_thread() -> DawResult<crate::sync::ReaperMainThread> {
    crate::sync::ReaperMainThread::try_new().ok_or(DawError::MainThreadUnavailable)
}
