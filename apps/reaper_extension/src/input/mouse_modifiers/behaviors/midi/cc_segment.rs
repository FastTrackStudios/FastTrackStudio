//! MIDI CC Segment behaviors

use crate::input::mouse_modifiers::behaviors::shared::traits::{BehaviorId, BehaviorDisplay};
#[allow(unused_imports)]
use crate::define_behavior_enum;

/// MIDI CC Segment left drag behaviors
define_behavior_enum! {
    pub enum MidiCcSegmentLeftDragBehavior {
        NoAction => (0, "No action"),
        MoveCcSegmentIgnoringTimeSelection => (1, "Move CC segment ignoring time selection"),
        InsertCcEvent => (2, "Insert CC event"),
        InsertCcEventIgnoringSnap => (3, "Insert CC event ignoring snap"),
        DrawEditCcEventsIgnoringSelection => (4, "Draw/edit CC events ignoring selection"),
        DrawEditCcEventsIgnoringSnapAndSelection => (5, "Draw/edit CC events ignoring snap and selection"),
        EditCcSegmentCurvature => (7, "Edit CC segment curvature"),
    }
}

/// MIDI CC Segment double click behaviors
define_behavior_enum! {
    pub enum MidiCcSegmentDoubleClickBehavior {
        NoAction => (0, "No action"),
        ResetCcSegmentCurvature => (1, "Reset CC segment curvature"),
        AddCcEvent => (2, "Add CC event"),
        AddCcEventIgnoringSnap => (3, "Add CC event ignoring snap"),
    }
}
