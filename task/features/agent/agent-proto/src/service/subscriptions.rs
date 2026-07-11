//! Live event subscriptions. Async because the underlying
//! transport is streaming.

use crate::event::AgentEvent;
use vox::Tx;

#[architect::rpc]
pub trait Subscriptions {
    /// Subscribe to live events scoped to one session.
    /// The server keeps sending until the caller drops
    /// `tx`. On broadcast lag the server sends
    /// `AgentEvent::Resync` — clients re-pull state.
    async fn subscribe_session(&self, session_id: String, tx: Tx<AgentEvent>);

    /// Subscribe to board-scoped events.
    async fn subscribe_board(&self, board_id: String, tx: Tx<AgentEvent>);

    /// Cross-session firehose — list-changed events,
    /// session created / archived / pinned. Used by
    /// sidebar UIs that need to stay live.
    async fn subscribe_global(&self, tx: Tx<AgentEvent>);
}
