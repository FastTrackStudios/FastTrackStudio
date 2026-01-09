//! MIDI Marker Lanes behaviors

#[allow(unused_imports)]
use crate::define_behavior_enum;
use crate::input::mouse_modifiers::behaviors::shared::traits::{BehaviorDisplay, BehaviorId};

/// MIDI Marker Lanes behaviors
define_behavior_enum! {
    pub enum MidiMarkerLanesBehavior {
        NoAction => (0, "No action"),
        HandScroll => (1, "Hand scroll"),
        HandScrollAndHorizontalZoom => (2, "Hand scroll and horizontal zoom"),
        HorizontalZoom => (4, "Horizontal zoom"),
        SetEditCursorAndHorizontalZoom => (6, "Set edit cursor and horizontal zoom"),
        SetEditCursorHandScrollAndHorizontalZoom => (8, "Set edit cursor, hand scroll and horizontal zoom"),
    }
}
