//! Live event subscriptions.

use crate::event::WikiEvent;
use vox::Tx;

#[architect::rpc]
pub trait Events {
    /// Subscribe to live `WikiEvent`s for one wiki. Stream
    /// closes when caller drops `tx`. Broadcast lag → the
    /// server sends `WikiEvent::Resync`.
    async fn subscribe(&self, wiki_id: String, tx: Tx<WikiEvent>);
}
