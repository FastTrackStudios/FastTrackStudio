//! `impl EventBus for Standalone` — stub. Standalone has no event source.

use crate::Standalone;
use daw_proto::event_bus::{BusFilter, DawEvent, EventBus};
use vox::Tx;

impl EventBus for Standalone {
    async fn subscribe(&self, _filter: BusFilter, _tx: Tx<DawEvent>) {}
}
