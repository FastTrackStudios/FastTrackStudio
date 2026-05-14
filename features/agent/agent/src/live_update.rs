//! Live update transport — three event kinds (`RunStateChanged`,
//! `LogAppended`, `ToolCallChanged`) carried over a
//! `tokio::sync::broadcast` channel. Two subscription modes:
//!
//! - **Per-run**: filter on `run_id`, forward matching events as
//!   they arrive. Tight feedback for a focused run detail view.
//! - **Workspace-scope**: batch events within a 50ms window so a
//!   high-volume dashboard stays smooth. Receivers see
//!   `Vec<AgentEvent>` batches keyed on the batch boundary.
//!
//! The bus is in-process — every AgentServiceImpl instance owns one,
//! and the server's transport layer (vox SSE / WS) bridges the
//! broadcast receivers to client sockets. No cross-process fanout
//! lives here.

use std::time::Duration;

use agent_proto::AgentEvent;
use tokio::sync::broadcast;
use tokio::time::interval;
use uuid::Uuid;

/// Broadcast channel capacity. Each `subscribe_*` receiver allocates
/// its own buffer of this size; slow consumers see `RecvError::Lagged`
/// and skip ahead rather than blocking publishers.
const BUS_CAPACITY: usize = 1024;

/// Workspace-scope batch window. 50ms per spec
/// `r[agent.live-update.workspace-scope]`.
const BATCH_WINDOW: Duration = Duration::from_millis(50);

#[derive(Clone)]
pub struct LiveUpdateBus {
    tx: broadcast::Sender<AgentEvent>,
}

impl Default for LiveUpdateBus {
    fn default() -> Self {
        Self::new()
    }
}

impl LiveUpdateBus {
    pub fn new() -> Self {
        let (tx, _) = broadcast::channel(BUS_CAPACITY);
        Self { tx }
    }

    /// Publish one event. Cheap — pushes onto the broadcast channel.
    /// Returns the count of currently-registered receivers (useful
    /// for diagnostic logging).
    pub fn publish(&self, event: AgentEvent) -> usize {
        // `broadcast::Sender::send` returns Err only when there are
        // zero active receivers; that's not a publish failure in our
        // model — events are fire-and-forget.
        let _ = self.tx.send(event);
        self.tx.receiver_count()
    }

    /// Subscribe to all events for one run. Returns a stream that
    /// yields only events whose `run_id` matches. Other runs' events
    /// are filtered out before reaching the receiver's await.
    pub fn subscribe_run(&self, run_id: Uuid) -> RunSubscription {
        RunSubscription {
            rx: self.tx.subscribe(),
            run_id,
        }
    }

    /// Subscribe with workspace-scope batching. Events flow through a
    /// 50ms accumulator and are delivered as `Vec<AgentEvent>` chunks
    /// so dashboards rendering many runs don't redraw on every
    /// keystroke from one. The returned stream yields one batch per
    /// window (empty windows yield nothing).
    pub fn subscribe_workspace(&self) -> WorkspaceSubscription {
        WorkspaceSubscription {
            rx: self.tx.subscribe(),
        }
    }
}

pub struct RunSubscription {
    rx: broadcast::Receiver<AgentEvent>,
    run_id: Uuid,
}

impl RunSubscription {
    /// Pull the next event matching this subscription's `run_id`.
    /// Lagged events (when the consumer fell behind the buffer) are
    /// surfaced as `Err`; the consumer can either reconnect-from-db
    /// or pump until caught up.
    pub async fn recv(&mut self) -> Result<AgentEvent, broadcast::error::RecvError> {
        loop {
            let event = self.rx.recv().await?;
            if event_run_id(&event) == self.run_id {
                return Ok(event);
            }
        }
    }
}

pub struct WorkspaceSubscription {
    rx: broadcast::Receiver<AgentEvent>,
}

impl WorkspaceSubscription {
    /// Pull the next batch. Blocks until at least one event arrives,
    /// then waits up to `BATCH_WINDOW` collecting more before
    /// returning. The batch never crosses a window boundary.
    pub async fn recv_batch(&mut self) -> Result<Vec<AgentEvent>, broadcast::error::RecvError> {
        let first = self.rx.recv().await?;
        let mut batch = vec![first];

        let mut ticker = interval(BATCH_WINDOW);
        ticker.tick().await; // consume the immediate first tick

        loop {
            tokio::select! {
                _ = ticker.tick() => return Ok(batch),
                next = self.rx.recv() => match next {
                    Ok(ev) => batch.push(ev),
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(e) => return Err(e),
                }
            }
        }
    }
}

fn event_run_id(e: &AgentEvent) -> Uuid {
    match e {
        AgentEvent::RunStateChanged { run_id, .. } => *run_id,
        AgentEvent::LogAppended { run_id, .. } => *run_id,
        AgentEvent::ToolCallChanged { run_id, .. } => *run_id,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration as StdDuration;
    use tokio::time::sleep;

    fn run_event(run_id: Uuid, status: &str) -> AgentEvent {
        AgentEvent::RunStateChanged {
            run_id,
            new_status: status.into(),
        }
    }

    fn log_event(run_id: Uuid) -> AgentEvent {
        AgentEvent::LogAppended {
            run_id,
            log_id: Uuid::new_v4(),
            seq: 1,
        }
    }

    #[tokio::test]
    async fn per_run_subscriber_only_sees_its_events() {
        let bus = LiveUpdateBus::new();
        let r1 = Uuid::new_v4();
        let r2 = Uuid::new_v4();
        let mut sub1 = bus.subscribe_run(r1);
        let mut sub2 = bus.subscribe_run(r2);

        bus.publish(run_event(r1, "running"));
        bus.publish(run_event(r2, "starting"));
        bus.publish(log_event(r1));

        let e1 = sub1.recv().await.unwrap();
        assert!(
            matches!(e1, AgentEvent::RunStateChanged { ref new_status, .. } if new_status == "running")
        );
        let e2 = sub1.recv().await.unwrap();
        assert!(matches!(e2, AgentEvent::LogAppended { .. }));

        let e_other = sub2.recv().await.unwrap();
        assert!(
            matches!(e_other, AgentEvent::RunStateChanged { ref new_status, .. } if new_status == "starting")
        );
    }

    #[tokio::test]
    async fn workspace_subscriber_batches_within_window() {
        let bus = LiveUpdateBus::new();
        let mut sub = bus.subscribe_workspace();
        let r1 = Uuid::new_v4();
        let r2 = Uuid::new_v4();

        // Spawn the recv in a task so we can publish from this one.
        let handle = tokio::spawn(async move { sub.recv_batch().await });

        // Yield so the subscriber registers.
        tokio::task::yield_now().await;

        // Publish 3 events in quick succession (within the 50ms window).
        bus.publish(run_event(r1, "running"));
        bus.publish(run_event(r2, "running"));
        bus.publish(log_event(r1));

        // Sleep past the batch window so the subscriber's flush ticks.
        sleep(StdDuration::from_millis(80)).await;

        let batch = handle.await.unwrap().unwrap();
        assert!(
            batch.len() >= 3,
            "expected batch to contain all 3 events, got {}: {:?}",
            batch.len(),
            batch
        );
    }

    #[tokio::test]
    async fn publish_with_no_subscribers_is_ok() {
        let bus = LiveUpdateBus::new();
        let count = bus.publish(run_event(Uuid::new_v4(), "running"));
        assert_eq!(count, 0);
    }

    #[tokio::test]
    async fn two_subscribers_both_receive_event() {
        let bus = LiveUpdateBus::new();
        let r = Uuid::new_v4();
        let mut sub_a = bus.subscribe_run(r);
        let mut sub_b = bus.subscribe_run(r);

        bus.publish(run_event(r, "completed"));

        let a = sub_a.recv().await.unwrap();
        let b = sub_b.recv().await.unwrap();
        match (a, b) {
            (
                AgentEvent::RunStateChanged { new_status: sa, .. },
                AgentEvent::RunStateChanged { new_status: sb, .. },
            ) => {
                assert_eq!(sa, "completed");
                assert_eq!(sb, "completed");
            }
            other => panic!("unexpected: {other:?}"),
        }
    }
}
