//! Envelope mouse modifier behaviors

#[allow(unused_imports)]
use crate::define_behavior_enum;
use crate::input::mouse_modifiers::behaviors::shared::traits::{BehaviorDisplay, BehaviorId};

/// Envelope control panel double click behaviors
define_behavior_enum! {
    pub enum EnvelopeControlPanelDoubleClickBehavior {
        NoAction => (0, "No action"),
        SelectUnselectAllEnvelopePoints => (1, "Select/unselect all envelope points"),
    }
}

/// Envelope lane left drag behaviors
define_behavior_enum! {
    pub enum EnvelopeLaneLeftDragBehavior {
        NoAction => (0, "No action"),
        DeselectAllEnvelopePoints => (1, "Deselect all envelope points"),
        DeselectAllEnvelopePointsAndMoveEditCursor => (2, "Deselect all envelope points and move edit cursor"),
    }
}

/// Envelope point left drag behaviors
define_behavior_enum! {
    pub enum EnvelopePointLeftDragBehavior {
        NoAction => (0, "No action"),
        MoveEnvelopePoint => (1, "Move envelope point"),
        MoveEnvelopePointIgnoringSnap => (2, "Move envelope point ignoring snap"),
        FreehandDrawEnvelope => (3, "Freehand draw envelope"),
        DeleteEnvelopePoint => (4, "Delete envelope point"),
        MoveEnvelopePointVerticallyFine => (5, "Move envelope point vertically (fine)"),
        MoveEnvelopePointOnOneAxisOnly => (6, "Move envelope point on one axis only"),
        MoveEnvelopePointOnOneAxisOnlyIgnoringSnap => (7, "Move envelope point on one axis only ignoring snap"),
        CopyEnvelopePoint => (8, "Copy envelope point"),
        CopyEnvelopePointIgnoringSnap => (9, "Copy envelope point ignoring snap"),
        MoveEnvelopePointHorizontally => (10, "Move envelope point horizontally"),
        MoveEnvelopePointHorizontallyIgnoringSnap => (11, "Move envelope point horizontally ignoring snap"),
        MoveEnvelopePointVertically => (12, "Move envelope point vertically"),
    }
}

/// Envelope point double click behaviors
define_behavior_enum! {
    pub enum EnvelopePointDoubleClickBehavior {
        NoAction => (0, "No action"),
        ResetPointToDefaultValue => (1, "Reset point to default value"),
        OpenEnvelopePointEditor => (2, "Open envelope point editor"),
    }
}

/// Envelope segment left drag behaviors
define_behavior_enum! {
    pub enum EnvelopeSegmentLeftDragBehavior {
        NoAction => (0, "No action"),
        MoveEnvelopeSegmentIgnoringTimeSelection => (1, "Move envelope segment ignoring time selection"),
        InsertEnvelopePointDragToMove => (2, "Insert envelope point, drag to move"),
        FreehandDrawEnvelope => (3, "Freehand draw envelope"),
        MoveEnvelopeSegmentFine => (4, "Move envelope segment (fine)"),
        EditEnvelopeSegmentCurvature => (5, "Edit envelope segment curvature"),
        MoveEnvelopeSegment => (6, "Move envelope segment"),
        MoveEnvelopeSegmentPreservingEdgePoints => (7, "Move envelope segment preserving edge points"),
        EditEnvelopeSegmentCurvatureGangSelectedPoints => (8, "Edit envelope segment curvature (gang selected points)"),
    }
}

/// Envelope segment double click behaviors
define_behavior_enum! {
    pub enum EnvelopeSegmentDoubleClickBehavior {
        NoAction => (0, "No action"),
        ResetEnvelopeSegmentCurvature => (1, "Reset envelope segment curvature"),
        AddEnvelopePoint => (2, "Add envelope point"),
    }
}
