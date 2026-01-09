//! Infrastructure for chords/chart integration with Dioxus
//!
//! Provides global signals for chart state that can be accessed from UI components

#[cfg(feature = "dioxus")]
use dioxus::prelude::*;
use keyflow::Chart;
use std::collections::HashMap;

/// Global signal for chart state (project_name -> Chart)
/// This is updated by the chart_connection module in the desktop app
#[cfg(feature = "dioxus")]
pub static CHART_STATE: GlobalSignal<HashMap<String, Chart>> = Signal::global(|| HashMap::new());
