//! Track sends, receives, and hardware outputs.
//!
//! Sends are scoped to source-track guid + send index. Adding a send takes the
//! destination track guid and returns the new send index on the source.

use crate::{DawResult, TrackRoute};

pub trait Routing {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute>;
    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute>;
    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute>;

    fn send_count(&self, track_guid: &str) -> u32;
    fn receive_count(&self, track_guid: &str) -> u32;

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32>;
    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()>;

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()>;
    fn set_send_pan(&self, track_guid: &str, send_idx: u32, pan: f64) -> DawResult<()>;
    fn set_send_muted(&self, track_guid: &str, send_idx: u32, muted: bool) -> DawResult<()>;
    fn is_send_muted(&self, track_guid: &str, send_idx: u32) -> bool;
}
