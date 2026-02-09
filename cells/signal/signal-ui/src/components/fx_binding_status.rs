//! FX Binding Status indicator component.
//!
//! Shows whether the rig is bound to a live DAW FX chain and displays
//! the binding status (track name, module count, unassigned count).

use crate::prelude::*;
use crate::signals::{RIG_FX_BINDING, RIG_FX_BINDING_STATUS};

/// Displays the current FX binding status.
///
/// Shows "Not bound" when no DAW track is connected, or
/// "Bound: TrackName (N modules, M unassigned)" when bound.
#[component]
pub fn FxBindingStatus() -> Element {
    let status = RIG_FX_BINDING_STATUS.read();
    let is_bound = RIG_FX_BINDING.read().is_some();

    let color = if is_bound {
        "color: #4ade80;" // green
    } else {
        "color: #9ca3af;" // gray
    };

    let dot = if is_bound { "\u{25CF}" } else { "\u{25CB}" }; // filled vs hollow circle

    rsx! {
        div {
            class: "fx-binding-status",
            style: "display: flex; align-items: center; gap: 6px; font-size: 12px; padding: 4px 8px;",
            span { style: "{color} font-size: 10px;", "{dot}" }
            span { style: "color: #d1d5db; font-size: 11px;", "{status}" }
        }
    }
}
