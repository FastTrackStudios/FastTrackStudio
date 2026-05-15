//! `impl Routing for Standalone` — stub. The pre-port impl operated
//! on the old plain-string trait surface; the new trait uses
//! `ProjectContext` + `TrackRef` + `RouteLocation` and has ~14
//! additional methods (set_mono/set_phase/parent_send/etc.). Full
//! port pending — stubbed with `todo!()` to keep the workspace green.

use daw_proto::{
    DawResult, ProjectContext, RouteLocation, RouteRef, Routing, SendMode, TrackRef, TrackRoute,
};

use crate::sync::Standalone;

impl Routing for Standalone {
    fn sends(&self, _project: ProjectContext, _track: TrackRef) -> Vec<TrackRoute> {
        Vec::new()
    }
    fn receives(&self, _project: ProjectContext, _track: TrackRef) -> Vec<TrackRoute> {
        Vec::new()
    }
    fn hardware_outputs(&self, _project: ProjectContext, _track: TrackRef) -> Vec<TrackRoute> {
        Vec::new()
    }
    fn get_route(&self, _project: ProjectContext, _location: RouteLocation) -> Option<TrackRoute> {
        None
    }
    fn send_count(&self, _project: ProjectContext, _track: TrackRef) -> u32 {
        0
    }
    fn receive_count(&self, _project: ProjectContext, _track: TrackRef) -> u32 {
        0
    }
    fn add_send(
        &self,
        _project: ProjectContext,
        _source: TrackRef,
        _dest: TrackRef,
    ) -> Option<u32> {
        todo!()
    }
    fn add_hardware_output(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _hw_output: u32,
    ) -> Option<u32> {
        todo!()
    }
    fn remove_route(&self, _project: ProjectContext, _location: RouteLocation) -> DawResult<()> {
        todo!()
    }
    fn set_volume(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _volume: f64,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_pan(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _pan: f64,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_muted(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _muted: bool,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_mono(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _mono: bool,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_phase(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _inverted: bool,
    ) -> DawResult<()> {
        todo!()
    }
    fn is_muted(&self, _project: ProjectContext, _location: RouteLocation) -> bool {
        false
    }
    fn set_send_mode(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _route: RouteRef,
        _mode: SendMode,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_source_channels(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _start: u32,
        _num: u32,
    ) -> DawResult<()> {
        todo!()
    }
    fn set_dest_channels(
        &self,
        _project: ProjectContext,
        _location: RouteLocation,
        _start: u32,
        _num: u32,
    ) -> DawResult<()> {
        todo!()
    }
    fn parent_send_enabled(&self, _project: ProjectContext, _track: TrackRef) -> bool {
        false
    }
    fn set_parent_send_enabled(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _enabled: bool,
    ) -> DawResult<()> {
        todo!()
    }
}
