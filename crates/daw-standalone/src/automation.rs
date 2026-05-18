//! `impl Automation for Standalone` — every method panics with
//! `todo!("standalone: …")`. Envelopes / points / automation modes
//! aren't modeled in `ProjectState` yet.

use daw_proto::TrackRef;
use daw_proto::automation::{
    AddPointParams, Automation, Envelope, EnvelopeLocation, EnvelopePoint, SetPointParams,
    TimeRangeParams,
};
use daw_proto::primitives::AutomationMode;
use daw_proto::primitives::PositionInSeconds;
use daw_proto::project::ProjectContext;

use crate::sync::Standalone;

impl Automation for Standalone {
    fn envelopes(&self, _project: ProjectContext, _track: TrackRef) -> Vec<Envelope> {
        todo!("standalone: Automation::envelopes — automation lanes not yet modeled")
    }
    fn envelope(&self, _project: ProjectContext, _location: EnvelopeLocation) -> Option<Envelope> {
        todo!("standalone: Automation::envelope")
    }
    fn set_visible(&self, _project: ProjectContext, _location: EnvelopeLocation, _visible: bool) {
        todo!("standalone: Automation::set_visible")
    }
    fn set_armed(&self, _project: ProjectContext, _location: EnvelopeLocation, _armed: bool) {
        todo!("standalone: Automation::set_armed")
    }
    fn set_automation_mode(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _mode: AutomationMode,
    ) {
        todo!("standalone: Automation::set_automation_mode")
    }
    fn points(&self, _project: ProjectContext, _location: EnvelopeLocation) -> Vec<EnvelopePoint> {
        todo!("standalone: Automation::points")
    }
    fn points_in_range(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _range: TimeRangeParams,
    ) -> Vec<EnvelopePoint> {
        todo!("standalone: Automation::points_in_range")
    }
    fn value_at(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _time: PositionInSeconds,
    ) -> f64 {
        todo!("standalone: Automation::value_at")
    }
    fn add_point(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _params: AddPointParams,
    ) -> u32 {
        todo!("standalone: Automation::add_point")
    }
    fn delete_point(&self, _project: ProjectContext, _location: EnvelopeLocation, _index: u32) {
        todo!("standalone: Automation::delete_point")
    }
    fn set_point(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _params: SetPointParams,
    ) {
        todo!("standalone: Automation::set_point")
    }
    fn delete_points_in_range(
        &self,
        _project: ProjectContext,
        _location: EnvelopeLocation,
        _range: TimeRangeParams,
    ) {
        todo!("standalone: Automation::delete_points_in_range")
    }
    fn global_automation_override(&self, _project: ProjectContext) -> Option<AutomationMode> {
        todo!("standalone: Automation::global_automation_override")
    }
    fn set_global_automation_override(
        &self,
        _project: ProjectContext,
        _mode: Option<AutomationMode>,
    ) {
        todo!("standalone: Automation::set_global_automation_override")
    }
}
